/*** includes ***/

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

//#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE
#define BERRYTEXT_VERSION "0.0.1"
#define TAB_STOP 8
#define BERRYTEXT_FORCE_QUIT 3
#define MAX_CHANGES 1000
#define CTRL_KEY(k) ((k) & 0x1f)
#define ABUF_INIT {NULL, 0} // Empty buffer -> constructor.
#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** enumerations ***/

// Enumerate function keys.
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
	PAGE_DOWN,
	CTRL_ARROW_UP,
	CTRL_ARROW_DOWN,
	CTRL_ARROW_LEFT,
	CTRL_ARROW_RIGHT
};

// Enumeration for the syntax highlight.
enum editorHighlight {
	HL_NORMAL = 0,
	HL_COMMENT,
	HL_MLCOMMENT, // Highlight for Multi-Line Comments.
	HL_KEYWORD1,
	HL_KEYWORD2,
	HL_STRING,
	HL_NUMBER,
	HL_MATCH
};

// Enumerations for the change struct.
enum ChangeType {
	INSERT,
	DELETE,
	NEWLINE,
	PASTE
};

/*** data ***/

// Encapsulates syntax highlighting rules for file_type
struct editorSyntax {
	char *file_type;
	char **file_match;
	char **keywords;
	char *singleline_comment_start;
	char *multiline_comment_start;
	char *multiline_comment_end;
	int flags;
};

// Data type for storing a row of text.
typedef struct eRow {
	int idx;
	int size; // Stores the raw character length of the row.
	int r_size; // Stores the character length of the rendered row.
	char *chars; // Pointer for a line of text.
	char *render; // Pointer for rendering characters for a row/line.
	unsigned char *hlight; // Highlight.
	int hl_open_comment;
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

	int cleanup_performed; // Flag to indicate if cleanup (free()) has already been performed.
	int dirty; // Flag to indicate if data has been changed.
	eRow *row; // Dynamic array (pointer) used to hold a row of chars.
	char *file_name;
	char status_msg[80];
	time_t status_msg_time; // Timer for status message display.

	char *clipboard; // Buffer to hold copied text.
	int clipboard_len; // Length of the copied text.
	int mark_row;
	int mark_col; // Start column of selection.
	int select_row;
	int select_col; // End column of text selection.
	int in_selection; // Flag to indicate a selection.

	struct editorSyntax *syntax;
	struct termios orig_termios;
};
struct editorConfig E;

// Struct for storing changes in the file
typedef struct {
	enum ChangeType type; // the type of change made (e.g. deletion, insertion, newline, etc.)
    int start_row;
    int start_col;
    int end_row;
    int end_col;
	char *content; // pointer to hold content that was inserted/deleted
	int content_len; // Length of the content
} Change;

// stack structure to hold file changes
typedef struct {
	Change changes[MAX_CHANGES]; // array for storing change structs
	int count; // number of changes in the stack
	int current; // position in stack
} ChangeStack;

// Global variables for undo and redo stacks
ChangeStack undo_stack;
ChangeStack redo_stack;

/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL }; // Define extensions for C
char *C_HL_keywords[] = { // Define keywords for C
	"switch", "if", "while", "for", "break", "continue", "return", "else",
	"struct", "union", "typedef", "static", "enum", "class", "case",
	"int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
	"void|", NULL
};

// Highlight Database.
struct editorSyntax HLDB[] = {
	{
		"c",
		C_HL_extensions,
		C_HL_keywords,
		"//", "/*", "*/",
		HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
	},
};
#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** terminal ***/

void editorCleanup() {
    if (E.cleanup_performed) return;
    E.cleanup_performed = 1;

    // Free the clipboard
    free(E.clipboard);

    // Free each row
    for (int i = 0; i < E.num_rows; i++) {
        free(E.row[i].chars);
        free(E.row[i].render);
        free(E.row[i].hlight);
    }

    // Free the row array itself
    free(E.row);

    // Free the filename
    free(E.file_name);
}

// Function to terminate process after clearing screen (prints error message)
void die(const char *s) {
	// Escape seq, J -> erase in display, 2 -> entire screen cleared.
	write(STDOUT_FILENO, "\x1b[2J", 4);
	// Escape sequence, H -> cursor position (defaults to pos 1;1H)
	write(STDOUT_FILENO, "\x1b[H", 3);
	perror(s); // Print descriptive error message.
	editorCleanup();
	exit(1);
}

// Function to restore terminal settings
void disableRawMode() {
	// Revert attribute to saved attribute orig_termios.
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) 
		die("tcsetattr");
}

// function to free the content of a Change
void freeChange(Change *change) {
    free(change->content);
}

// Function to set terminal into raw mode (inpts are read char by char without buffering / processing special chars)
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
		char seq[5];
		if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
		if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
		
		if (seq[0] == '[') { // Check for command modifier.
			if (seq[1] >= '0' && seq[1] <= '9') {
				if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
				if (seq[2] == '~') { // If third char is ~.
					switch (seq[1]) {
						case '1': return HOME_KEY;
						case '3': return DEL_KEY;
						case '4': return END_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				} else if (seq[2] == ';') { // If third char is ;.
					if (read(STDIN_FILENO, &seq[3], 1) != 1) return '\x1b';
					if (read(STDIN_FILENO, &seq[4], 1) != 1) return '\x1b';
					if (seq[3] == '5') { // Check if fourth character is Ctrl key
						switch(seq[4]) {
							case 'A': return CTRL_ARROW_UP;
							case 'B': return CTRL_ARROW_DOWN;
							case 'C': return CTRL_ARROW_RIGHT;
							case 'D': return CTRL_ARROW_LEFT;
						}
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

/*** syntax hgihlighting ***/

// Determines if a character is a seperator (e.g. whitespace, null char, punctuation / operator)
int is_seperator(int c) {
	return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(eRow *row) {
    row->hlight = realloc(row->hlight, row->r_size);
    memset(row->hlight, HL_NORMAL, row->r_size);
    if (E.syntax == NULL) return;

    char **keywords = E.syntax->keywords;
    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row->r_size) {
        char c = row->render[i];
        unsigned char prev_hlight = (i > 0) ? row->hlight[i - 1] : HL_NORMAL;

        // Handle control characters
        if (iscntrl(c)) {
            row->hlight[i] = HL_NORMAL;
            i++;
            continue;
        }

        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hlight[i], HL_COMMENT, row->r_size - i);
                break;
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hlight[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hlight[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hlight[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

		if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
			if (in_string) {
				row->hlight[i] = HL_STRING;
				if (c == '\\' && i + 1 < row->r_size) {
					row->hlight[i + 1] = HL_STRING;
					i += 2;
					continue;
				}
				if (c == in_string) in_string = 0;
				i++;
				prev_sep = 1;
				continue;
			} else {
				if (c == '"' || c == '\'') {
					in_string = c;
					row->hlight[i] = HL_STRING;
					i++;
					continue;
				}
			}
		}

		if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
			if ((isdigit(c) && (prev_sep || prev_hlight == HL_NUMBER)) ||
			(c == '.' && prev_hlight == HL_NUMBER)) {
				row->hlight[i] = HL_NUMBER;
				i++;
				prev_sep = 0;
				continue;
			}
		}

		if (prev_sep) {
			int j;
			for (j = 0; keywords[j]; j++) {
				int klen = strlen(keywords[j]);
				int kw2 = keywords[j][klen - 1] == '|';
				if (kw2) klen--;

				if (!strncmp(&row->render[i], keywords[j], klen) &&
				is_seperator(row->render[i + klen])) {
					memset(&row->hlight[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
					i += klen;
					break;
				}
			}
			if (keywords[j] != NULL) {
				prev_sep = 0;
				continue;
			}
		}

		prev_sep = is_seperator(c);
		i++;
	}

	int changed = (row->hl_open_comment != in_comment);
	row->hl_open_comment = in_comment;
	if (changed && row->idx + 1 < E.num_rows)
		editorUpdateSyntax(&E.row[row->idx + 1]);
}

// Function to map syntax highlight to its corresponding color
int editorSyntaxToColor(int hlight) {
	switch (hlight) {
		case HL_COMMENT:
		case HL_MLCOMMENT: return 36;
		case HL_KEYWORD1: return 33;
		case HL_KEYWORD2: return 32;
		case HL_STRING: return 35;
		case HL_NUMBER: return 31;
		case HL_MATCH: return 34;
		default: return 37;
	}
}

void editorSelectSyntaxHighlight() {
	E.syntax = NULL;
	if (E.file_name == NULL) return;

	char *ext = strrchr(E.file_name, '.');
	for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
		struct editorSyntax *s = &HLDB[j];
		unsigned int i = 0;
		while (s->file_match[i]) {
			int is_ext = (s->file_match[i][0] == '.');
			if ((is_ext && ext && !strcmp(ext, s->file_match[i])) ||
			(!is_ext && strstr(E.file_name, s->file_match[i]))) {
				E.syntax = s;

				for (int file_row = 0; file_row < E.num_rows; file_row++) {
					editorUpdateSyntax(&E.row[file_row]);
				}
				return;
			}
			i++;
		}
	}
}

/*** stack operations ***/

// Function to initialize a ChangeStack
void initChangeStack(ChangeStack *stack) {
	stack->count = 0;
	stack->current = -1; // Current = -1 for empty stack
}

void pushChange(ChangeStack *stack, Change change) {
	if (stack->count > MAX_CHANGES) { // if max number of changes has been reached
		freeChange(&stack->changes[0]);
		// remove the oldest change
		memmove(&stack->changes[0], &stack->changes[1], sizeof(Change) * (MAX_CHANGES - 1));
		stack->count--;
	}
	stack->changes[stack->count++] = change; // add change as last element
	stack->current = stack->count - 1; // update count
}

// Function to retrieve and remove (pop) the top of the stack (last element to be added)
Change popChange(ChangeStack *stack) {
	if (stack->current >= 0) { // if there are changes available in the stack
		return stack->changes[stack->current--]; // return change and decrement current
	}
	Change empty = {0};
	return empty; // else return an empty change
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
	editorUpdateSyntax(row);
}

// Function to insert a new line into the file at index at.
void editorInsertRow(int at, char *s, size_t len) {
	if (at < 0 || at > E.num_rows) return; // Validate index.
	
	// Allocate bytes based on sizeof eRow and number of rows.
	E.row = realloc(E.row, sizeof(eRow) * (E.num_rows + 1));
	memmove(&E.row[at + 1], &E.row[at], sizeof(eRow) * (E.num_rows - at));
	for (int j = at + 1; j <= E.num_rows; j++) E.row[j].idx++;
	
	E.row[at].idx = at;
	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1); // Allocate memory to chars.	
	// Memory copy line to chars (-> allocated memory).
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';
	
	E.row[at].r_size = 0; 
	E.row[at].render = NULL;
	E.row[at].hlight = NULL;
	E.row[at].hl_open_comment = 0;
	editorUpdateRow(&E.row[at]);
	E.num_rows++; // Indicate there is a line to be displayed.
	E.dirty++; // Indicate there is change to be saved.
}

// Function to free a row of characters (and all its elements).
void editorFreeRow(eRow *row) {
	free(row->render);
	free(row->chars);
	free(row->hlight);
}

// Function to delete a row.
void editorDeleteRow(int at) {
	if (at < 0 || at >= E.num_rows) return; // Validate the at index.
	editorFreeRow(&E.row[at]);
	// Use memmove to overwrite the deleted row with the remaining rows.
	memmove(&E.row[at], &E.row[at + 1], sizeof(eRow) * (E.num_rows - at - 1));
	for (int j = at; j < E.num_rows - 1; j++) E.row[j].idx--;
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

// Function to store/record changes made to file
void editorRecordChange(enum ChangeType type, int start_row, int start_col, int end_row, int end_col, const char *content, int content_len) {
    Change change;
    change.type = type;
    change.start_row = start_row;
    change.start_col = start_col;
    change.end_row = end_row;
    change.end_col = end_col;
    change.content = malloc(content_len + 1);
    memcpy(change.content, content, content_len);
    change.content[content_len] = '\0';
    change.content_len = content_len;
    pushChange(&undo_stack, change);
    initChangeStack(&redo_stack);
}

// Function to delete a selection of text
void editorDeleteSelection() {
    if (!E.in_selection) return; // break out if not in selection mode

	// get the start and end positions for the text selection
    int start_row = E.mark_row < E.select_row ? E.mark_row : E.select_row;
    int end_row = E.mark_row > E.select_row ? E.mark_row : E.select_row;
    int start_col = E.mark_row < E.select_row ? E.mark_col : E.select_col;
    int end_col = E.mark_row > E.select_row ? E.mark_col : E.select_col;

    for (int row = start_row; row <= end_row; row++) {
        int row_start = (row == start_row) ? start_col : 0;
        int row_end = (row == end_row) ? end_col : E.row[row].size;
        int delete_len = row_end - row_start;

        memmove(&E.row[row].chars[row_start], &E.row[row].chars[row_end], E.row[row].size - row_end);
        E.row[row].size -= delete_len;
        E.row[row].chars[E.row[row].size] = '\0';
        editorUpdateRow(&E.row[row]);
    }

    if (start_row != end_row) {
        for (int row = start_row + 1; row <= end_row; row++) {
            editorDeleteRow(start_row + 1);
        }
    }

	// reset cursor position to starting row, col
    E.curs_y = start_row;
    E.curs_x = start_col;
    E.in_selection = 0;
    E.dirty++; // indicate change to be saved
}

// Function to insert a character.
void editorInsertChar(int c) {
    if (E.in_selection) {
        editorDeleteSelection();
    }
    if (E.curs_y == E.num_rows) {
        editorInsertRow(E.num_rows, "", 0);
    }

    editorRowInsertChar(&E.row[E.curs_y], E.curs_x, c);
    E.curs_x++;

    char content[2] = {c, '\0'};
    editorRecordChange(INSERT, E.curs_y, E.curs_x - 1, E.curs_y, E.curs_x, content, 1);
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
	if (E.in_selection) {
		editorDeleteSelection();
		return;
	}
	// If cursor is past end of the file, return immediately.
	// If the cursor position is at the start of the file, return.
	if (E.curs_y == E.num_rows) return; 
	if (E.curs_x == 0 && E.curs_y == 0) return;

	eRow *row = &E.row[E.curs_y]; // Get eRow cursor is on.
	if (E.curs_x > 0) { // If there is a char to left of cursor...
		char deleted = row->chars[E.curs_x - 1]; // copy char to be deleted
		editorRowDeleteChar(row, E.curs_x - 1); // Delete the char.
		E.curs_x--; // Move cursor to the left.
        editorRecordChange(DELETE, E.curs_y, E.curs_x, E.curs_y, E.curs_x + 1, &deleted, 1);
	} else {
		// Set cursor to end of previous line.
		E.curs_x = E.row[E.curs_y - 1].size;
		char *content = strdup(row->chars);
		int content_len = row->size;
		// Append *row.chars to the previous row.
		editorRowAppendString(&E.row[E.curs_y - 1], row->chars, row->size);
		editorDeleteRow(E.curs_y); // Delete row y-cursor is on.
		E.curs_y--;
        editorRecordChange(DELETE, E.curs_y, E.curs_x, E.curs_y + 1, 0, content, content_len);
        free(content);
	}
}

// Function to copy a selection of text
void editorCopySelection() {
    if (!E.in_selection) return;

    int start_row = E.mark_row < E.select_row ? E.mark_row : E.select_row;
    int end_row = E.mark_row > E.select_row ? E.mark_row : E.select_row;
    int start_col = E.mark_row < E.select_row ? E.mark_col : E.select_col;
    int end_col = E.mark_row > E.select_row ? E.mark_col : E.select_col;

    // Swap start and end columns if they're in the wrong order
    if (start_row == end_row && start_col > end_col) {
        int temp = start_col;
        start_col = end_col;
        end_col = temp;
    }

	// Reset clipboard status
    free(E.clipboard);
    E.clipboard = NULL;
    E.clipboard_len = 0;

    for (int row = start_row; row <= end_row; row++) {
        int row_start = (row == start_row) ? start_col : 0;
        int row_end = (row == end_row) ? end_col : E.row[row].size;
        int len = row_end - row_start;

        if (len < 0) len = 0;  // Ensure we don't have negative length

        E.clipboard = realloc(E.clipboard, E.clipboard_len + len + 2);  // +2 for newline and null terminator
        if (E.clipboard == NULL) {
            // Handle memory allocation failure
            E.clipboard_len = 0;
            return;
        }

        memcpy(E.clipboard + E.clipboard_len, &E.row[row].chars[row_start], len);
        E.clipboard_len += len;

        if (row != end_row) {
            E.clipboard[E.clipboard_len] = '\n';
            E.clipboard_len++;
        }
    }

    E.clipboard[E.clipboard_len] = '\0';
}

// Paste text from clipboard at the cursor position
void editorPasteClipboard() {
    if (E.clipboard == NULL || E.clipboard_len == 0) return;

    int start_row = E.curs_y;
    int start_col = E.curs_x;

    int line_start = 0;
    for (int i = 0; i <= E.clipboard_len; i++) {
        if (i == E.clipboard_len || E.clipboard[i] == '\n') {
            // Process the line
            for (int j = line_start; j < i; j++) {
                if (E.clipboard[j] == '\t') {
                    int tab_size = TAB_STOP - (E.curs_x % TAB_STOP);
                    for (int k = 0; k < tab_size; k++) {
                        editorInsertChar(' ');
                    }
                } else if (!iscntrl(E.clipboard[j])) {
                    editorInsertChar(E.clipboard[j]);
                }
            }

            if (i < E.clipboard_len) {
                editorInsertNewline();
            }
            line_start = i + 1;
        }
    }

    editorRecordChange(PASTE, start_row, start_col, E.curs_y, E.curs_x, E.clipboard, E.clipboard_len);

    // Update syntax highlighting for affected rows
    for (int row = start_row; row <= E.curs_y; row++) {
        editorUpdateSyntax(&E.row[row]);
    }
}

// Function for editor to undo recently made changes
void editorUndo() {
    Change change = popChange(&undo_stack);
    if (change.type == 0) return;

    switch (change.type) {
        case INSERT:
        case PASTE:
            E.curs_y = change.end_row;
            E.curs_x = change.end_col;
            for (int i = 0; i < change.content_len; i++) {
                editorDeleteChar();
            }
            E.curs_y = change.start_row;
            E.curs_x = change.start_col;
            break;
        case DELETE:
            E.curs_y = change.start_row;
            E.curs_x = change.start_col;
            for (int i = 0; i < change.content_len; i++) {
                if (change.content[i] == '\n') {
                    editorInsertNewline();
                } else {
                    editorInsertChar(change.content[i]);
                }
            }
            break;
        case NEWLINE:
            E.curs_y = change.start_row;
            E.curs_x = change.start_col;
            editorDeleteChar();
            break;
    }

    // Update syntax highlighting for the affected rows
    for (int row = change.start_row; row <= change.end_row; row++) {
        editorUpdateSyntax(&E.row[row]);
    }

    pushChange(&redo_stack, change);
}

// Function for editor to redo recently made changes
void editorRedo() {
    Change change = popChange(&redo_stack);
    if (change.type == 0) return;

    switch (change.type) {
        case INSERT:
            E.curs_y = change.start_row;
            E.curs_x = change.start_col;
            for (int i = 0; i < change.content_len; i++) {
                editorInsertChar(change.content[i]);
            }
            break;
        case DELETE:
            E.curs_y = change.end_row;
            E.curs_x = change.end_col;
            for (int i = 0; i < change.content_len; i++) {
                editorDeleteChar();
            }
            break;
        case NEWLINE:
            E.curs_y = change.start_row;
            E.curs_x = change.start_col;
            editorInsertNewline();
            break;
        case PASTE:
            E.curs_y = change.start_row;
            E.curs_x = change.start_col;
            for (int i = 0; i < change.content_len; i++) {
                if (change.content[i] == '\n') {
                    editorInsertNewline();
                } else {
                    editorInsertChar(change.content[i]);
                }
            }
            break;
    }

    // Update syntax highlighting for the affected rows
    for (int row = change.start_row; row <= change.end_row; row++) {
        editorUpdateSyntax(&E.row[row]);
    }

    pushChange(&undo_stack, change);
}

// Function to calculate how many columns needed to display line numbering
int editorLineNumberWidth() {
    int max_rows = E.num_rows > 0 ? E.num_rows : 1;
    int digits = 1;
    while (max_rows >= 10) {
        digits++;
        max_rows /= 10;
    }
    return digits + 1; // +1 for the space after the number
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
	editorSelectSyntaxHighlight();
	
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
		editorSelectSyntaxHighlight();
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
	static int saved_hlight_line;
	static char *saved_hlight = NULL;

	if (saved_hlight) {
		memcpy(E.row[saved_hlight_line].hlight, saved_hlight, E.row[saved_hlight_line].r_size);
		free(saved_hlight);
		saved_hlight = NULL;
	}
		
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

			saved_hlight_line = current;
			saved_hlight = malloc(row->r_size);
			memcpy(saved_hlight, row->hlight, row->r_size);
			memset(&row->hlight[match - row->render], HL_MATCH, strlen(query));
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
    int line_number_width = editorLineNumberWidth();
    for (int y = 0; y < E.screen_rows; y++) {
        int file_row = y + E.row_offset;

        // Line number section with different background color
        aBufAppend(ab, "\x1b[44m", 5); // Blue background
        if (file_row < E.num_rows) {
            char line_num[16];
            int num_len = snprintf(line_num, sizeof(line_num), "%*d ", line_number_width - 1, file_row + 1);
            aBufAppend(ab, line_num, num_len);
        } else {
            // For empty lines, just add spaces
            for (int i = 0; i < line_number_width; i++) {
                aBufAppend(ab, " ", 1);
            }
        }
        aBufAppend(ab, "\x1b[m", 3); // Reset formatting

        // Text content
        if (file_row < E.num_rows) {
            int len = E.row[file_row].r_size - E.col_offset;
            if (len < 0) len = 0;
            if (len > E.screen_cols - line_number_width) len = E.screen_cols - line_number_width;
            char *c = &E.row[file_row].render[E.col_offset];
            unsigned char *hlight = &E.row[file_row].hlight[E.col_offset];
            int current_color = -1;
            for (int j = 0; j < len; j++) {
                // Determine if the current position is within the selection range
                int selected = 0;
                if (E.in_selection) {
                    int sel_start_row = E.mark_row < E.select_row ? E.mark_row : E.select_row;
                    int sel_end_row = E.mark_row > E.select_row ? E.mark_row : E.select_row;
                    int sel_start_col = (E.mark_row == sel_start_row) ? E.mark_col : E.select_col;
                    int sel_end_col = (E.mark_row == sel_end_row) ? E.mark_col : E.select_col;

                    if ((file_row > sel_start_row && file_row < sel_end_row) ||
                        (file_row == sel_start_row && file_row == sel_end_row && j + E.col_offset >= sel_start_col && j + E.col_offset < sel_end_col) ||
                        (file_row == sel_start_row && file_row != sel_end_row && j + E.col_offset >= sel_start_col) ||
                        (file_row == sel_end_row && file_row != sel_start_row && j + E.col_offset < sel_end_col)) {
                        selected = 1;
                    }
                }

                if (selected) {
                    aBufAppend(ab, "\x1b[7m", 4); // Invert colors for selected text
                }

                if (iscntrl(c[j])) {
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    aBufAppend(ab, "\x1b[7m", 4);
                    aBufAppend(ab, &sym, 1);
                    aBufAppend(ab, "\x1b[m", 3);
                    if (current_color != -1) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        aBufAppend(ab, buf, clen);
                    }
                } else if (hlight[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        aBufAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    aBufAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hlight[j]);
                    if (color != current_color) {
                        current_color = color;
                        char buf[16];
                        int c_len = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        aBufAppend(ab, buf, c_len);
                    }
                    aBufAppend(ab, &c[j], 1);
                }

                if (selected) {
                    aBufAppend(ab, "\x1b[m", 3); // Reset colors after selected text
                }
            }
            aBufAppend(ab, "\x1b[39m", 5);
        } else if (E.num_rows == 0 && y == E.screen_rows / 3) {
            char welcome[80];
            int welcome_len = snprintf(welcome, sizeof(welcome),
                                       "BerryText Editor -- Version %s", BERRYTEXT_VERSION);
            if (welcome_len > E.screen_cols) welcome_len = E.screen_cols;
            int padding = (E.screen_cols - welcome_len) / 2;
            if (padding) {
                aBufAppend(ab, "~", 1);
                padding--;
            }
            while (padding--) aBufAppend(ab, " ", 1);
            aBufAppend(ab, welcome, welcome_len);
        } else {
            //aBufAppend(ab, "~", 1); // Append tilde for empty row.
        }

        aBufAppend(ab, "\x1b[K", 3);
        aBufAppend(ab, "\r\n", 2);
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
	int rend_len = snprintf(rend_status, sizeof(rend_status), "%s | %d/%d",
		E.syntax ? E.syntax->file_type : "No File Type", E.curs_y + 1, E.num_rows);
	
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
    int line_number_width = editorLineNumberWidth();
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.curs_y - E.row_offset) + 1,
             (E.rend_x - E.col_offset) + 1 + line_number_width);
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
void editorMoveCursor(int key, int ctrl) {
    int line_number_width = editorLineNumberWidth();
    eRow *row = (E.curs_y >= E.num_rows) ? NULL : &E.row[E.curs_y];

    if (ctrl && !E.in_selection) {
        E.mark_row = E.curs_y;
        E.mark_col = E.curs_x - line_number_width;
        E.in_selection = 1;
    } else if (!ctrl && E.in_selection) {
        E.in_selection = 0;
    }

    switch (key) {
        case ARROW_LEFT:
        case CTRL_ARROW_LEFT:
            if (E.curs_x > line_number_width) {
                E.curs_x--;
            } else if (E.curs_y > 0) {
                E.curs_y--;
                E.curs_x = E.row[E.curs_y].size + line_number_width;
            }
            break;
        case ARROW_RIGHT:
        case CTRL_ARROW_RIGHT:
            if (row && E.curs_x < row->size + line_number_width) {
                E.curs_x++;
            } else if (E.curs_y < E.num_rows - 1) {
                E.curs_y++;
                E.curs_x = line_number_width;
            }
            break;
        case ARROW_UP:
        case CTRL_ARROW_UP:
            if (E.curs_y > 0) {
                E.curs_y--;
            }
            break;
        case ARROW_DOWN:
        case CTRL_ARROW_DOWN:
            if (E.curs_y < E.num_rows - 1) {
                E.curs_y++;
            }
            break;
    }

    row = (E.curs_y >= E.num_rows) ? NULL : &E.row[E.curs_y];
    int row_len = row ? row->size + line_number_width : line_number_width;
    if (E.curs_x > row_len) {
        E.curs_x = row_len;
    }

    if (ctrl) {
        E.select_row = E.curs_y;
        E.select_col = E.curs_x - line_number_width;
    }

    editorRefreshScreen();  // Refresh the screen after each cursor movement
}

// Function for the editor to process a read keypress.
void editorProcessKeypress() {
	static int force_quit_ctr = BERRYTEXT_FORCE_QUIT;

	int c = editorReadKey(); // Read keypress into c.
	int ctrl = 0; // Flag for control keypress.

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
			//editorCleanup();
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

		// If combination is ctrl + c, then copy text selection.
		case CTRL_KEY('c'):
			editorCopySelection();
			break;

		// If combination is ctrl + v, then paste clipboard.
		case CTRL_KEY('v'):
			editorPasteClipboard();
			break;

		case CTRL_KEY('z'):
			editorUndo();
			break;

		case CTRL_KEY('y'):
			editorRedo();
			break;

		// If combination is ctrl + a, then select the entire text.
		case CTRL_KEY('a'):
			E.mark_row = 0;
			E.mark_col = 0; // Set mark to first row and column
			E.select_row = E.num_rows - 1;
			E.select_col = E.row[E.num_rows - 1].size; // Set select to last row and column
			E.in_selection = 1;
			editorRefreshScreen();
			break;

		// If combination is ctrl + f, initiate find function.
		case CTRL_KEY('f'):
			editorFind();
			break;

		// If char is backspace/ctrl(h) or delete key.
		case BACKSPACE:
		case CTRL_KEY('h'):
		case DEL_KEY:
			// If char is delete key, move cursor to right.
			if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT, 0);
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
					editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN, 0);
			}
			break;

		case CTRL_ARROW_UP:
		case CTRL_ARROW_DOWN:
		case CTRL_ARROW_LEFT:
		case CTRL_ARROW_RIGHT:
			editorMoveCursor(c, 1);  // Pass 1 for ctrl flag
			break;

		// If keypress is any of the arrow keys.
		case ARROW_UP:
		case ARROW_DOWN:
		case ARROW_LEFT:
		case ARROW_RIGHT:
			editorMoveCursor(c, 0); // Pass keypress to editorMoveCursor.
			break;

		// If char is ctrl(l), ctrl(w) or ctrl(d) do nothing.
		case CTRL_KEY('l'):
		case CTRL_KEY('w'):
		case CTRL_KEY('d'):
			break;

		case '\x1b': // Escape sequence
			{
				char seq[5];
				if (read(STDIN_FILENO, &seq[0], 1) != 1) return;
				if (read(STDIN_FILENO, &seq[1], 1) != 1) return;

				if (seq[0] == '[') {
					if (seq[1] >= '0' && seq[1] <= '9') {
						if (read(STDIN_FILENO, &seq[2], 1) != 1) return;
						if (seq[2] == '~') {
							// Handle other special keys
						} else if (seq[2] == ';') {
							if (read(STDIN_FILENO, &seq[3], 1) != 1) return;
							if (read(STDIN_FILENO, &seq[4], 1) != 1) return;
							if (seq[3] == '5') { // This indicates Ctrl key
								ctrl = 1;
								switch (seq[4]) {
									case 'A': editorMoveCursor(ARROW_UP, ctrl); break;
									case 'B': editorMoveCursor(ARROW_DOWN, ctrl); break;
									case 'C': editorMoveCursor(ARROW_RIGHT, ctrl); break;
									case 'D': editorMoveCursor(ARROW_LEFT, ctrl); break;
								}
							}
						}
					} else {
						switch (seq[1]) {
							case 'A': editorMoveCursor(ARROW_UP, 0); break;
							case 'B': editorMoveCursor(ARROW_DOWN, 0); break;
							case 'C': editorMoveCursor(ARROW_RIGHT, 0); break;
							case 'D': editorMoveCursor(ARROW_LEFT, 0); break;
						}
					}
				}
			}
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
	E.cleanup_performed = 0;
	E.dirty = 0; // Set dirty flag to zero -> no changes to save.
	E.row = NULL;
	E.file_name = NULL;
	E.status_msg[0] = '\0'; // Init with null termination.
	E.status_msg_time = 0; // Timestamp for status message.
	E.syntax = NULL;
	E.clipboard = NULL;
	E.clipboard_len = 0;

	// Initialize undo and redo stacks
	initChangeStack(&undo_stack);
	initChangeStack(&redo_stack);

	// If invalid window size, end process
	if (getWindowSize(&E.screen_rows, &E.screen_cols) == -1)
		die("getWindowSize");
	E.screen_rows -= 2; // Make room for status and message bar.
}

int main(int argc, char *argv[]) {
	enableRawMode();
	initEditor();
	atexit(editorCleanup);
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
