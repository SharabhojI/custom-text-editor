/*** includes ***/

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <errno.h>
#include <termios.h>

/*** defines ***/

#define BERRYTEXT_VERSION "0.0.1"
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

// Global structure for editor state.
struct editorConfig {
	int curs_x, curs_y;
	int screen_rows;
	int screen_cols;
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
				switch (seq[1]) { // Switch case depending on arg after [
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

// Draws each row of the buffer of text being handled.
void editorDrawRows(struct aBuf *ab) {
	for (int y = 0; y < E.screen_rows; y++) {
		if (y == E.screen_rows / 3) {
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
		
		// Escape sequence, erases part of line to the right of cursor.
		aBufAppend(ab, "\x1b[K", 3);	
		if (y < E.screen_rows - 1) {
			aBufAppend(ab, "\r\n", 2); // Draw tilde for last row.
		}
	}
}

void editorRefreshScreen() {
	struct aBuf ab = ABUF_INIT;
	
	// Escape sequence, hides cursor.
	aBufAppend(&ab, "\x1b[?25l", 6);
	// Escape sequence, H -> cursor position (defaults to pos 1;1H,
	// the first row and first column)
	aBufAppend(&ab, "\x1b[H", 3);
	
	editorDrawRows(&ab);
	char buf[32];	
	snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.curs_y + 1, E.curs_x + 1);
	aBufAppend(&ab, buf, strlen(buf));
	aBufAppend(&ab, "\x1b[?25h", 6); // Escape seq to show cursor.
	
	write(STDOUT_FILENO, ab.b, ab.len); // Write buffer to STDOUT.
	aBufFree(&ab); // Free memory allocated to buffer.
}

/*** input ***/

void editorMoveCursor(int key) {
	// Switch based upon input key, move along relevant axis.
	switch (key) {
		case ARROW_LEFT:
			if (E.curs_x != 0) { // If cursor not on first col.
				E.curs_x--;
			}
			break;
		case ARROW_RIGHT:
			if (E.curs_x != E.screen_cols - 1) { // If not on last col.
				E.curs_x++;
			}
			break;
		case ARROW_UP:
			if (E.curs_y != 0) { // If cursor not on first row.
				E.curs_y--;
			}
			break;
		case ARROW_DOWN:
			if (E.curs_y != E.screen_rows - 1) { // If not on last row.
				E.curs_y++;
			}
			break;
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
			E.curs_x = E.screen_cols - 1;
			break;
		
		// If keypress is page up or page down.
		case PAGE_UP:
		case PAGE_DOWN:
			{
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
	
	if (getWindowSize(&E.screen_rows, &E.screen_cols) == -1)
		die("getWindowSize");
}

int main() {
	enableRawMode();
	initEditor();
	
	while (1) {
		editorRefreshScreen();
		editorProcessKeypress(); // Waits for and then handles input.
	}
	return 0;
}
