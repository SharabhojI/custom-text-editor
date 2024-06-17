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
#include <fcntl.h>

/*** defines ***/

#define BERRYTEXT_VERSION "0.0.1"
#define TAB_STOP 8
#define BERRYTEXT_FORCE_QUIT 3
#define CTRL_KEY(k) ((k) & 0x1f)
#define ABUF_INIT {NULL, 0} // Empty buffer -> constructor.

// Enumerate escape keys.
enum editorKey {
	BACKSPACE = 127, // Assign ASCII value of 127.
	ARROW_LEFT = 1000, // Assign ASCII value of 1000++ for following.
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
	int rend_x; // Int to hold line render position.
	int row_offset;
	int col_offset;
	int screen_rows; // Number of rows on screen.
	int screen_cols; // Number of columns on screen.
	int num_rows;
	int dirty; // Flag to indicate if data has been changed.
	eRow *row; // Dynamic array (pointer) used to hold a row of chars.
	char *file_name;
	char status_msg[80];
	time_t status_msg_time; // Timer for status message display.
	struct termios orig_termios;
};
struct editorConfig E;

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

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

// Editor function to read a keypress.
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

// Function to get cursor position in the editor.
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

// Function to get window size of editor (i.e rows, cols).
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

// Converts render index into a character index.
int editorRowRxToCx(eRow *row, int rx) {
	int cur_rx = 0;
	int cx;
	for (cx = 0; cx < row->size; cx++) { // Loop through chars string.
		if (row->chars[cx] == '\t') 
			cur_rx += (BERRYTEXT_FORCE_QUIT	- 1) - (cur_rx % BERRYTEXT_FORCE_QUIT);
		cur_rx++;
		
		if (cur_rx > rx) return cx;
	}
	return cx;
}

// Function to update a row using a temporary render row.
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

// Function to insert a new line into the file at index at.
void editorInsertRow(int at, char *s, size_t len) {
	if (at < 0 || at > E.num_rows) return; // Validate index.
	
	// Allocate bytes based on sizeof eRow and number of rows.
	E.row = realloc(E.row, sizeof(eRow) * (E.num_rows + 1));
	memmove(&E.row[at + 1], &E.row[at], sizeof(eRow) * (E.num_rows - at));
	
	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1); // Allocate memory to chars.	
	// Memory copy line to chars (-> allocated memory).
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';
	
	E.row[at].r_size = 0; 
	E.row[at].render = NULL;
	editorUpdateRow(&E.row[at]);
	E.num_rows++; // Indicate there is a line to be displayed.
	E.dirty++; // Indicate there is change to be saved.
}

// Function to free a row of characters.
void editorFreeRow(eRow *row) {
	free(row->render);
	free(row->chars);
}

// Function to delete a row.
void editorDeleteRow(int at) {
	if (at < 0 || at >= E.num_rows) return; // Validate the at index.
	editorFreeRow(&E.row[at]);
	// Use memmove to overwrite the deleted row with the remaining rows.
	memmove(&E.row[at], &E.row[at + 1], sizeof(eRow) * (E.num_rows - at - 1));
	E.num_rows--; // Decrement the number of rows.
	E.dirty++; // Indicate change to be saved.
} 

// Function to insert a single character into a row.
void editorRowInsertChar(eRow *row, int at, int c) {
	// If at != *row.size, set it to *row.size.
	if (at < 0 || at > row->size) at = row->size;
	// Allocate bytes for char c and null byte.
	row->chars = realloc(row->chars, row->size + 2);
	// Use memmove to make room for new char.
	memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
	row->size++;
	row->chars[at] = c; // Assign char to its array position.
	editorUpdateRow(row); // Update row fields.
	E.dirty++;
}

// Function to append a strink to the end of a row.
void editorRowAppendString(eRow *row, char *s, size_t len) {
	// Reallocate *row.chars size to  *row.size + len + 1. Includes null byte.
	row->chars = realloc(row->chars, row->size + len + 1);
	// Memcpy the string to the end of *row.chars.
	memcpy(&row->chars[row->size], s, len);
	row->size += len; // Update row size.
	row->chars[row->size] = '\0';
	editorUpdateRow(row);
	E.dirty++; // Indicate there is a change to save.
}

// Function to delete a single character in a row.
void editorRowDeleteChar(eRow *row, int at) {
	if (at < 0 || at >= row->size) return;
	// Use memmove to overwrite the deleted character with the characters
	// that follow it. The null byte at end of row is included.
	memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
	row->size--; // Decrement row size.
	editorUpdateRow(row);
	E.dirty++; // Indicate change to be saved.
}

/*** editor operations ***/

// Function to insert a character.
void editorInsertChar(int c) {
	if (E.curs_y == E.num_rows) { // If on the last row.
		editorInsertRow(E.num_rows, "", 0); // Insert new row.
	}
	// Place character at row pos y, and col pos x.
	editorRowInsertChar(&E.row[E.curs_y], E.curs_x, c);
	E.curs_x++;
}

// Function to insert a new line.
void editorInsertNewline() {
	if (E.curs_x == 0) { // If at the start of a line.
		editorInsertRow(E.curs_y, "", 0); // Insert blank row.
	} else {
		eRow *row = &E.row[E.curs_y];
		// Pass characters on current row that are to right of cursor.
		editorInsertRow(E.curs_y + 1, &row->chars[E.curs_x], row->size - E.curs_x);
		// Reassign pointer as editorInsertRow uses realloc().
		row = &E.row[E.curs_y];
		row->size = E.curs_x; // Truncate row by setting size to cursor pos.
		row->chars[row->size] = '\0';
		editorUpdateRow(row);
	}
	E.curs_y++;
	E.curs_x = 0; // Increment and move cursor to start of the new row.
}

// Function to delete a character.
void editorDeleteChar() {
	// If cursor is past end of the file, return immediately.
	// If the cursor position is at the start of the file, return.
	if (E.curs_y == E.num_rows) return; 
	if (E.curs_x == 0 && E.curs_y == 0) return;
	
	eRow *row = &E.row[E.curs_y]; // Get eRow cursor is on.
	if (E.curs_x > 0) { // If there is a char to left of cursor...
		editorRowDeleteChar(row, E.curs_x - 1); // Delete the char.
		E.curs_x--; // Move cursor to the left.
	} else {
		// Set cursor to end of previous line.
		E.curs_x = E.row[E.curs_y - 1].size;
		// Append *row.chars to the previous row.
		editorRowAppendString(&E.row[E.curs_y - 1], row->chars, row->size);
		editorDeleteRow(E.curs_y); // Delete row y-cursor is on.
		E.curs_y--;
	}
}

/*** file I/O ***/

// Function to convert the rows of the file into a single string.
char *editorRowsToString(int *buf_len) {
	int total_len = 0;
	// Find the length of each row + 1 for newline char.
	for (int i = 0; i < E.num_rows; i++)
		total_len += E.row[i].size + 1;
	*buf_len = total_len; // Save total length into buf_len.
	
	char *buf = malloc(total_len); // Allocate required memory.
	char *p = buf;
	// Loop through the rows and memcpy the contents to the end of
	// the buffer. Append newline after each row.
	for(int i = 0; i < E.num_rows; i++) {
		memcpy(p, E.row[i].chars, E.row[i].size);
		p += E.row[i].size;
		*p = '\n';
		p++;
	}
	return buf; // Return buf, expect caller to free memory.
}

// Function to open and read from given file name.
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
		editorInsertRow(E.num_rows, line, line_len);
	}
	free(line);
	fclose(fp);
	E.dirty = 0; // Reset dirty flag as editorAppendRow increments it.
}

// Function to save changes made in editor.
void editorSave() {
	if (E.file_name == NULL) { // If no file name, prompt for it.
		E.file_name = editorPrompt("Save as: %s (ESC to cancel)", NULL);
		if (E.file_name == NULL) {
			editorSetStatusMessage("Save Aborted");
			return;
		}
	}	
	int len;
	char *buf = editorRowsToString(&len); // Copy file to buf.
	
	// Tell open() to open file for Read and Write, if it does not 
	// exist, create it. 0644 -> standard permissions for txt file.
	int fd = open(E.file_name, O_RDWR | O_CREAT, 0644);
	if (fd != -1) {		 
		// Cut file size down to specified length. If it is larger, then
		// cut off data at end of the file to make it specified length.
		if (ftruncate(fd, len) != -1) {
			// Write to path in E.file_name.			 
			if (write(fd, buf, len) == len) { 
				close(fd);
				free(buf);
				E.dirty = 0; // Reset dirty flag after saving changes.
				editorSetStatusMessage("%d bytes written to disk", len);
				return;
			}
		}
		close(fd); // Close file regardless of any errors.
	}
	free(buf); // Free memory assigned to buf.
	editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/
void editorFindCallback(char *query, int key) {
	static int last_match = -1;
	static int direction = 1;
		
	if (key == '\r' || key == '\x1b') {
		last_match = -1;
		direction = 1;
		return; // Abort search operation
	} else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
		direction = 1;
	} else if (key == ARROW_LEFT || key == ARROW_UP) {
		direction = -1;
	} else {
		last_match = -1;
		direction = 1;
	}
	
	if (last_match == -1) direction = 1;
	int current = last_match;
	int i;
	for (i = 0; i < E.num_rows; i++) { // Loop through all file rows.
		current += direction;
		if (current == -1) current = E.num_rows - 1;
		else if (current == E.num_rows) current = 0;
		
		eRow *row = &E.row[current];
		// Check if query is a substring of the current row
		char *match = strstr(row->render, query);
		if (match) {
			last_match = current;
			E.curs_y = current;
			E.curs_x = editorRowRxToCx(row, match - row->render);
			E.row_offset = E.num_rows;
			break;
		}
	}	
}

void editorFind() {
	int saved_cx = E.curs_x;
	int saved_cy = E.curs_y;
	int saved_col_off = E.col_offset;
	int saved_row_off = E.row_offset;
	
	char *query = editorPrompt("Search: %s (use ESC/Arrows/Enter)", 
		editorFindCallback);
	
	if (query) {
		free(query);
	} else {
		E.curs_x = saved_cx;
		E.curs_y = saved_cy;
		E.col_offset = saved_col_off;
		E.row_offset = saved_row_off;
	}
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

// Function to handle editor scrolling.
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

// Function to create a status bar for the last rows.
void editorDrawStatusBar(struct aBuf *ab) {
	aBufAppend(ab, "\x1b[7m", 4); // Escape sequence to invert colors.
	char status[80], rend_status[80];
	
	// Display up to 20 chars of the file name and the number of lines
	// in the file. If no file name, display [No Name]. If dirty flag,
	// is not 0 display (modified), else display nothing.
	int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
		E.file_name ? E.file_name : "[No Name]", E.num_rows, 
		E.dirty ? "(modified)" : "");
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

// Function to draw a message bar at the bottom.
void editorDrawMessageBar(struct aBuf *ab) {
	aBufAppend(ab, "\x1b[K", 3); // Clear message bar -> \x1b{K
	int msg_len = strlen(E.status_msg);
	// Cut message string short when it does not fit screen width.
	if (msg_len > E.screen_cols) msg_len = E.screen_cols;
	// If (message) AND age is less than 5 secs...
	if (msg_len && time(NULL) - E.status_msg_time < 5)
		aBufAppend(ab, E.status_msg, msg_len); // Display message.
}

// Function to refresh editor screen after change.
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

// Function to prompt the user for input.
char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
	size_t buf_size = 128;
	char *buf = malloc(buf_size); // Dynamically allocated string.
	
	size_t buf_len = 0;
	buf[0] = '\0'; // Initialize with empty string.
	
	while (1) { // Infinite loop.
		// Set status message and refresh screen.
		editorSetStatusMessage(prompt, buf);
		editorRefreshScreen();
		
		int c = editorReadKey();
		if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
			if (buf_len != 0) buf[--buf_len] = '\0';
		} else if (c == '\x1b') { // If user pressed esc key.
			editorSetStatusMessage("");
			if (callback) callback(buf, c);
			free(buf); // Clear message, prompt and free memory.
			return NULL;
		} else if (c == '\r') { // If character is enter key.
			if (buf_len != 0) { // If input is not empty.
				editorSetStatusMessage("");
				if (callback) callback(buf, c);
				return buf; // Clear status meesage and return input.
			}
		  // Else if input is printable character.
		} else if (!iscntrl(c) && c < 128) {
			if (buf_len == buf_size - 1) { // If max capacity reached.
				buf_size *= 2; 
				buf = realloc(buf, buf_size); // Double and reallocate capacity,
			}
			buf[buf_len++] = c;
			buf[buf_len] = '\0'; // End with \0 to indicate end of string.
		}
		if (callback) callback(buf, c);
	}
}

// Function to move cursor based on input keypress.
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

// Function for the editor to process a read keypress.
void editorProcessKeypress() {
	static int force_quit_ctr = BERRYTEXT_FORCE_QUIT;
	
	int c = editorReadKey(); // Read keypress into c.
	switch (c) {
		case '\r': // If char is enter key.
			editorInsertNewline(); // Insert newline.
			break;
		
		case CTRL_KEY('q'): // If char combination is ctrl + q.
			// If unsaved changes and an attempt to quit. Make the user 
			// Ctrl + Q two more times to quit without saving.
			if (E.dirty && force_quit_ctr > 0) {
				editorSetStatusMessage("WARNING: File has unsaved changes. "
					"Press Ctrl-Q %d more times to quit.", force_quit_ctr);
				force_quit_ctr--;
				return;
			}
			// Esc seq, J -> erase display, 2 -> entire screen cleared.
			write(STDOUT_FILENO, "\x1b[2J", 4);
			// Escape sequence, H -> cursor position (defaults to 1;1H).
			write(STDOUT_FILENO, "\x1b[H", 3);
			exit(0);
			break;
		
		// If combination is ctrl + s, then save the file.
		case CTRL_KEY('s'):
			editorSave();
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
			
		case CTRL_KEY('f'):
			editorFind();
			break;
		
		// If char is backspace/ctrl(h) or delete key.
		case BACKSPACE:
		case CTRL_KEY('h'):
		case DEL_KEY:
			// If char is delete key, move cursor to right.
			if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
			editorDeleteChar(); // Delete key to the left.
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
			
		// If char is ctrl(l) or esc, do nothing.	
		case CTRL_KEY('l'):
		case '\x1b':
			break;
			
		default: // If not a command, insert char at cursor.
			editorInsertChar(c);
			break;
	}
	// When any other key is pressed, reset the first quit counter.
	force_quit_ctr = BERRYTEXT_FORCE_QUIT;
}

/*** init ***/

// Function to initalize fields in the editor struct.
void initEditor() {
	// Initialize all fields in struct E.
	E.curs_x = 0;
	E.curs_y = 0;
	E.rend_x = 0;
	E.row_offset = 0;
	E.col_offset = 0;
	E.num_rows = 0;
	E.dirty = 0; // Set dirty flag to zero -> no changes to save.
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
	if (argc >= 2) { // If two or more arguments passed.
		editorOpen(argv[1]); // Open second arg.
	}
	editorSetStatusMessage(
		"HELP: Ctrl-S = Save | Ctrl-Q = Quit | CTRL-F = Find");
	
	while (1) {
		editorRefreshScreen();
		editorProcessKeypress(); // Waits for and then handles input.
	}
	return 0;
}
