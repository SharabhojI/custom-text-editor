/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <ctype.h>
#include <errno.h>
#include <termios.h>
#include <time.h>

/*** defines ***/

#define BERRYTEXT_VERSION "0.0.1"
#define TAB_STOP 8
#define CTRL_KEY(k) ((k) & 0x1f)
#define ABUF_INIT {NULL, 0} // Empty buffer -> constructor.

// Enumerate escape keys.
enum editorKey {
	ARROW_LEFT = 1000,
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN
};

/*** data ***/

// Data type for storing a row of text.
typedef struct eRow {
	int size;
	int r_size;
	char *chars; // Pointer for a line of text.
	char *render; // Pointer for rendering characters for a row/line.
} eRow;

// Global structure for storing editor state.
struct editorConfig {
	int curs_x, curs_y; // Integers to hold cursor position in file.
	int rend_x;
	int row_offset;
	int col_offset;
	int screen_rows;
	int screen_cols;
	int num_rows;
	eRow *row; // Dynamic array (pointer) used to hold a row of chars.
	char *file_name;
	char status_msg[80];
	time_t status_msg_time;
	struct termios orig_termios;
};
struct editorConfig E;

/*** terminal ***/

void die(const char *s) {
	// Escape seq, J -> erase in display, 2 -> entire screen cleared.
	write(STDOUT_FILENO, "\x1b[2J", 4);
	// Escape sequence, H -> cursor position (defaults to pos 1;1H)
	write(STDOUT_FILENO, "\x1b[H", 3);
	
	perror(s); // Print descriptive error message.
	exit(1);
}

void disableRawMode() {
	// Revert attribute to saved attribute orig_termios.
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) 
		die("tcsetattr");
}

void enableRawMode() {
	// Read terminal STDIN attribute into orig_termios.
	// Revert terminal attributes when exiting.
	if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
	atexit(disableRawMode); 
	
	// Read orig_termios into clone raw.
	struct termios raw = E.orig_termios;
	// Disable carriage return new line and flow control. Disable output
	// processing. Disable echo, signals and canonical mode.
	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
	raw.c_oflag &= ~(OPOST); // c_lflag -> local flags.
	raw.c_cflag |= (CS8);
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
	
	// Timeout to ensure read() returns after inactivity. 
	// VMIN -> min num of bytes before read() can return.
	// VTIME -> max time to wait before return. 1 -> 1/10s -> 100ms.
	raw.c_cc[VMIN] = 0;
	raw.c_cc[VTIME] = 1;
	
	// Apply modified attribute, TCSAFLUSH -> makes changes after 
	// waiting for queued output. Discards queued inputs.
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) 
		die("tcsetattr");
}

int editorReadKey() {
	int nread;
	char c;
	// Read 1 byte from STDIN into char c.
	while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
		// Kill editor if nread is -1.
		if (nread == -1 && errno != EAGAIN) die("read");
	}
	
	if(c == '\x1b') { // Check if escape character.
		char seq[3];
		if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
		if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
		
		if (seq[0] == '[') { // Check for command modifier.
			if (seq[1] >= '0' && seq[1] <= '9') {
				if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
				if (seq[2] == '~') { // If final char is ~.
					switch (seq[1]) {
						case '1': return HOME_KEY;
						case '3': return DEL_KEY;
						case '4': return END_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				}
			} else {
				switch (seq[1]) { // Switch case depending on arg.
					case 'A': return ARROW_UP;
					case 'B': return ARROW_DOWN;
					case 'C': return ARROW_RIGHT;
					case 'D': return ARROW_LEFT;
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;
				}
			}
		} else if (seq[0] == '0') { // Else check if first char is 0.
			switch (seq[1]) { // Switch based on second char.
				case 'H': return HOME_KEY;
				case 'F': return END_KEY;
			}
		}
		
		return '\x1b'; // Return escape character.
	} else {
		return c;
	}
}

int getCursorPosition(int *rows, int *cols) {
	char buf[32];
	unsigned int i = 0;
	
	if (write(STDOUT_FILENO, "\x1b[6b", 4) != 4) return -1;	
	while (i < sizeof(buf) - 1) {
		if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
		if (buf[i] == 'R') break;
		i++;
	}
	buf[i] = '\0';
	
	// Don't want to print \x1b -> skip first char in buf (esc seq).
	if (buf[0] != '\x1b' || buf[1] != '[') return -1;
	if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;	
	return 0;
}

int getWindowSize(int *rows, int *cols) {
	struct winsize ws;
	// TIOCGWINSZ -> Terminal Input Output Control Get Window Size.
	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
		// Move cursor to right then down, cursor in bottom right.
		if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) 
			return -1;
		return getCursorPosition(rows, cols);
	} else {
		*rows = ws.ws_row;
		*cols = ws.ws_col;
		return 0; 
	}
}

/*** row operations ***/

// Convert char index in row to render index.
int editorRowCxToRx(eRow *row, int cx) {
	int rx = 0;
	for (int j = 0; j < cx; j++) {
		if (row->chars[j] == '\t') // If char is a tab.
			// (rx % TAB_STOP) Find out how many cols to the right of 
			// the last tab stop. Subtract from (TAB_STOP - 1) to find
			// how many cols to the left of the next tab stop.
			// Adding this to rx gets us just to left of next tab stop. 
			rx += (TAB_STOP - 1) - (rx % TAB_STOP);
		rx++; // Move to right of next tab stop.
	}
	return rx;
}

void editorUpdateRow(eRow *row) {
	int tabs = 0;
	for (int j = 0; j < row->size; j++)
		if (row->chars[j] == '\t') tabs++; // Increment tabs when found.
	
	free(row->render); // Free memory allocated to (*row).render.
	// Allocate memory to render to accomodate size of row and tabs.
	row->render = malloc(row->size + tabs*(TAB_STOP - 1) + 1);
	
	int index = 0; // Num of chars to copy into (*row).render.
	for (int j = 0; j < row->size; j++) {
		if (row->chars[j] == '\t') { // If character is a tab...
			row->render[index++] = ' '; // Add spaces equiv to tab len.
			while (index % TAB_STOP != 0) row->render[index++] = ' ';
		} else {
			row->render[index++] = row->chars[j]; // Copy char to render.
		}
	}
	row->render[index] = '\0'; // Null terminate (*row).render[index].
	row->r_size = index; // Set (*row).r_size to index.
}

void editorAppendRow(char *s, size_t len) {
	// Allocate bytes based on sizeof eRow and number of rows.
	E.row = realloc(E.row, sizeof(eRow) * (E.num_rows + 1));
	
	int at = E.num_rows; // Index of the new row to init.
	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1); // Allocate memory to chars.	
	// Memory copy line to chars (-> allocated memory).
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';
	
	E.row[at].r_size = 0; 
	E.row[at].render = NULL;
	editorUpdateRow(&E.row[at]);
	E.num_rows++; // Indicate there is a line to be displayed.
}

/*** file I/O ***/

void editorOpen(char *file_name) {
	free(E.file_name);
	// Make copy of file name, then allocate required memory.
	// strdup() assumes that you will free the memory.
	E.file_name = strdup(file_name);
	
	FILE *fp = fopen(file_name, "r"); // Read file with fopen.
	if (!fp) die("fopen"); // If no file, kill open process.
	
	char *line = NULL;
	size_t line_cap = 0; // Line capacity, default 0.
	ssize_t line_len; // Define line length as signed size_t.
	// Dynamically allocate memory and read line.
	line_len = getline(&line, &line_cap, fp);
	while ((line_len = getline(&line, &line_cap, fp)) != -1) {
		while (line_len > 0 && (line[line_len - 1] == '\n' ||
								line[line_len - 1] == '\r'))
			line_len--;
		editorAppendRow(line, line_len);
	}
	free(line);
	fclose(fp);
}

/*** append buffer ***/

// Append buffer with pointer to buffer in memory and the length.
struct aBuf {
	char *b;
	int len;
};

void aBufAppend(struct aBuf *ab, const char *s, int len) {
	// Reallocate memory to accomodate string s.
	char *new = realloc(ab->b, ab->len + len);
	
	if (new == NULL) return;
	// Copy string s after end of current data in buffer.
	memcpy(&new[ab->len], s, len);
	//update pointer and length of abuf.
	ab->b = new;
	ab->len += len;
}

// Destructor, dynamically reallocates memory used by abuf.
void aBufFree(struct aBuf *ab) {
	free(ab->b);
}

/*** output ***/

void editorScroll() {
	E.rend_x = 0;
	if (E.curs_y < E.num_rows) {
		E.rend_x = editorRowCxToRx(&E.row[E.curs_y], E.curs_x);
	}
	// If cursor is above visible window, scroll to it.
	if (E.curs_y < E.row_offset) {
		E.row_offset = E.curs_y;
	}
	// If cursor is below visible window, scroll to it.
	if (E.curs_y >= E.row_offset + E.screen_rows) {
		E.row_offset = E.curs_y - E.screen_rows + 1;
	}
	if (E.rend_x < E.col_offset) {
		E.col_offset = E.rend_x;
	}
	if (E.rend_x >= E.col_offset + E.screen_cols) {
		E.col_offset = E.rend_x - E.screen_cols + 1;
	}
}

// Draws each row of the buffer of text being handled.
void editorDrawRows(struct aBuf *ab) {
	for (int y = 0; y < E.screen_rows; y++) {
		int file_row = y + E.row_offset;
		if (file_row >= E.num_rows) {
			if (E.num_rows == 0 && y == E.screen_rows / 3) {
				char welcome[80];
				// Interpolate Version No. into welcome message.
				int welcome_len = snprintf(welcome, sizeof(welcome),
					"BerryText Editor -- version %s", BERRYTEXT_VERSION);
				if (welcome_len > E.screen_cols) welcome_len = E.screen_cols;
				
				// Find screen center.
				int padding = (E.screen_cols - welcome_len) / 2;
				if (padding) {
					aBufAppend(ab, "~", 1);
					padding--;
				}
				while (padding--) aBufAppend(ab, " ", 1);
				aBufAppend(ab, welcome, welcome_len);
			} else {
				aBufAppend(ab, "~", 1); // Draw tilde for rows
			}
		} else {
			int len = E.row[file_row].r_size - E.col_offset;
			if (len < 0) len = 0; 
			// If the len is greater than screen cols, reduce it.
			if (len > E.screen_cols) len = E.screen_cols;
			// Append row and length.
			aBufAppend(ab, &E.row[file_row].render[E.col_offset], len);
		}
		
		// Escape sequence, erases part of line to the right of cursor.
		aBufAppend(ab, "\x1b[K", 3);	
		aBufAppend(ab, "\r\n", 2); // Draw tilde for last row.
	}
}

// Creates a status bar for the last rows.
void editorDrawStatusBar(struct aBuf *ab) {
	aBufAppend(ab, "\x1b[7m", 4); // Escape sequence to invert colors.
	char status[80], rend_status[80];
	
	// Display up to 20 chars of the file name and the number of lines
	// in the file. If no file name, display [No Name].
	int len = snprintf(status, sizeof(status), "%.20s - %d lines",
		E.file_name ? E.file_name : "[No Name]", E.num_rows);
	int rend_len = snprintf(rend_status, sizeof(rend_status), "%d/%d",
		E.curs_y + 1, E.num_rows);
	
	// Cut status string short when it does not fit screen width.
	if (len > E.screen_cols) len = E.screen_cols;
	aBufAppend(ab, status, len); // First status string.	
	while (len < E.screen_cols) {
		// Print spaces until the second status msg will appear at end.
		if (E.screen_cols - len == rend_len) {
			aBufAppend(ab, rend_status, rend_len);
			break; // Break loop after printing second status message.
		} else {
			aBufAppend(ab, " ", 1); // Append space/blank chr to buffer.
			len++;
		}
	}
	aBufAppend(ab, "\x1b[m", 3); // Switch back to normal formatting.
	aBufAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct aBuf *ab) {
	aBufAppend(ab, "\x1b[K", 3); // Clear message bar -> \x1b{K
	int msg_len = strlen(E.status_msg);
	// Cut message string short when it does not fit screen width.
	if (msg_len > E.screen_cols) msg_len = E.screen_cols;
	// If (message) AND age is less than 5 secs...
	if (msg_len && time(NULL) - E.status_msg_time < 5)
		aBufAppend(ab, E.status_msg, msg_len); // Display message.
}

void editorRefreshScreen() {
	editorScroll();
	struct aBuf ab = ABUF_INIT;
	
	// Escape sequence, hides cursor.
	aBufAppend(&ab, "\x1b[?25l", 6);
	// Escape sequence, H -> cursor position (defaults to pos 1;1H,
	// the first row and first column)
	aBufAppend(&ab, "\x1b[H", 3);
	
	// Draw rows, status bar, and message bar to screen.
	editorDrawRows(&ab);
	editorDrawStatusBar(&ab);
	editorDrawMessageBar(&ab);
	
	char buf[32];	
	snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.curs_y - E.row_offset) + 1, 
											  (E.rend_x - E.col_offset) + 1);
	aBufAppend(&ab, buf, strlen(buf));
	aBufAppend(&ab, "\x1b[?25h", 6); // Escape seq to show cursor.	
	write(STDOUT_FILENO, ab.b, ab.len); // Write buffer to STDOUT.
	aBufFree(&ab); // Free memory allocated to buffer.
}

// Variadic function -> any number of arguments.
void editorSetStatusMessage(const char *fmt, ...) {
	va_list ap;
	// Pass last argument before the ... (fmt) so that the address of
	// the next arguments are known.
	va_start(ap, fmt);
	// Pass fmt and ap to vsnprintf() which will read the format string
	// and call va_arg() on each argument.
	vsnprintf(E.status_msg, sizeof(E.status_msg), fmt, ap);
	va_end(ap);
	// Returns number of seconds that have passed since Jan 1st, 1970.
	E.status_msg_time = time(NULL);
}

/*** input ***/

void editorMoveCursor(int key) {
	// If the cursor y is greater than num rows, row is null, else
	// the row is at cursor y.
	eRow *row = (E.curs_y >= E.num_rows) ? NULL : &E.row[E.curs_y];
	// Switch based upon input key, move along relevant axis.
	switch (key) {
		case ARROW_LEFT:
			if (E.curs_x != 0) { // If cursor not on first col.
				E.curs_x--;
			} else if (E.curs_y > 0) {
				E.curs_y--;
				E.curs_x = E.row[E.curs_y].size;
			} 
			break;
		case ARROW_RIGHT:
			// If row and cursor x are less than (*row).size.
			if (row && E.curs_x < row->size) {
				E.curs_x++;
			// Else if they are equal to (*row).size.	
			} else if (row && E.curs_x == row->size) {
				E.curs_y++;
				E.curs_x = 0;
			}
			break;
		case ARROW_UP:
			if (E.curs_y != 0) { // If cursor not on first row.
				E.curs_y--;
			}
			break;
		case ARROW_DOWN:
			if (E.curs_y < E.num_rows) { // If not on last row.
				E.curs_y++;
			}
			break;
	}
	
	// Block to snap cursor to end of a line.
	row = (E.curs_y >= E.num_rows) ? NULL : &E.row[E.curs_y];
	int row_len = row ? row->size : 0; // If row, row_len = row, else 0.
	if (E.curs_x > row_len) {
		E.curs_x = row_len;
	}	
}

void editorProcessKeypress() {
	int c = editorReadKey(); // Read keypress into c.
	switch (c) {
		case CTRL_KEY('q'): // If char combination is ctrl + q.
			// Esc seq, J -> erase display, 2 -> entire screen cleared.
			write(STDOUT_FILENO, "\x1b[2J", 4);
			// Escape sequence, H -> cursor position (defaults to 1;1H).
			write(STDOUT_FILENO, "\x1b[H", 3);
			exit(0);
			break;
		
		// If keypress is home, send cursor to first col.	
		case HOME_KEY:
			E.curs_x = 0;
			break;
		//If keypress is end, send cursor to the last column.
		case END_KEY:
			if (E.curs_y < E.num_rows)
				E.curs_x = E.row[E.curs_y].size;
			break;
		
		// If keypress is page up or page down.
		case PAGE_UP:
		case PAGE_DOWN:
			{
				if (c == PAGE_UP) {
					E.curs_y = E.row_offset;
				} else if (c == PAGE_DOWN) {
					E.curs_y = E.row_offset = E.screen_rows - 1;
					if (E.curs_y > E.num_rows) E.curs_y = E.num_rows;
				}
				
				int times = E.screen_rows; // Find num rows to traverse.
				while (times--) // Decrement and move up or down.
					editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
			}
			break;	
		
		// If keypress is any of the arrow keys.
		case ARROW_UP:
		case ARROW_DOWN:
		case ARROW_LEFT:
		case ARROW_RIGHT:
			editorMoveCursor(c); // Pass keypress to editorMoveCursor.
			break;
	}
}

/*** init ***/

void initEditor() {
	// Initialize all fields in struct E.
	E.curs_x = 0;
	E.curs_y = 0;
	E.rend_x = 0;
	E.row_offset = 0;
	E.col_offset = 0;
	E.num_rows = 0;
	E.row = NULL;
	E.file_name = NULL;
	E.status_msg[0] = '\0'; // Init with null termination.
	E.status_msg_time = 0; // Timestamp for status message.
	
	// If invalid window size, end process
	if (getWindowSize(&E.screen_rows, &E.screen_cols) == -1)
		die("getWindowSize");
	E.screen_rows -= 2; // Make room for status and message bar.
}

int main(int argc, char *argv[]) {
	enableRawMode();
	initEditor();
	if (argc >= 2) {
		editorOpen(argv[1]);
	}
	editorSetStatusMessage("HELP: Ctrl-Q = Quit");
	
	while (1) {
		editorRefreshScreen();
		editorProcessKeypress(); // Waits for and then handles input.
	}
	return 0;
}
