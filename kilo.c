
/*** ------------------------- includes ------------------------- ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <string.h>
#include <time.h>
#include <stdarg.h>
#include <fcntl.h>

/*** ------------------------- defines ------------------------- ***/

#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 4
#define KILO_QUIT_TIMES 3

#define CTRL_KEY(k) ((k) & 0x1f)

enum editorKey {
    BACKSPACE = 127,
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

enum editorHighlight {
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_NUMBER,
    HL_MATCH,
    HL_STRING
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** ------------------------- data ------------------------- ***/

// Syntax for coloring keywords and other file type specific information
struct editorSyntax {
    char *filetype;
    char **filematch;
    char **keywords;
    int flags;
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
};

// Row information
typedef struct erow {
    int idx;
    int size;
    int rsize;
    char *chars;
    char *render;
    unsigned char *hl;
    int hl_open_comment;
} erow;

// Struct containing status of editor
struct editorConfig {
    int cx, cy;
    int lx;
    int rx;
    int rowoff;
    int coloff;
    struct termios orig_termios;
    int numrows;
    erow *row;
    char *filename;
    char statusmsg[80];
    time_t statusmsg_time;
    int screenrows;
    int screencols;
    int dirty;
    struct editorSyntax *syntax;
};

struct editorConfig E;

/*** ------------------------- filetypes ------------------------- ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case",
    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
};

char *JAVA_HL_extensions[] = { ".java", NULL };
char *JAVA_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "class", "interface", "enum", "public|", "private|", "protected|",
    "static|", "final|", "void|", "int|", "long|", "double|", "float|",
    "char|", "boolean|", NULL
};

char *PYTHON_HL_extensions[] = { ".py", NULL };
char *PYTHON_HL_keywords[] = {
    "def", "class", "if", "elif", "else", "while", "for", "break", "continue",
    "return", "import", "from", "as", "try", "except", "finally",
    "with", "lambda", "yield", NULL
};

char *RUST_HL_extensions[] = { ".rs", NULL };
char *RUST_HL_keywords[] = {
    "let", "mut", "fn", "impl", "struct", "enum", "match", "if", "else",
    "while", "for", "break", "continue", "return", "use", "pub|",
    "crate|", "mod|", "self|", "super|", NULL
};

char *HTML_HL_extensions[] = { ".html", ".htm", NULL };
char *HTML_HL_keywords[] = {
    "html", "head", "body", "title", "meta", "link", "script", "style",
    "div", "span", "p", "a", "img", "ul", "ol", "li",
    "table", "tr", "td", "th", NULL
};

char *JS_HL_extensions[] = { ".js", NULL };
char *JS_HL_keywords[] = {
    "function", "var", "let", "const", "if", "else", "switch", "case",
    "break", "continue", "return", "for", "while", "do", "try",
    "catch", "finally", "import", "export", "class", "extends",
    "super|", NULL
};

char *CPP_HL_extensions[] = { ".cpp", ".hpp", ".cxx", ".hxx", NULL };
char *CPP_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "class", "struct", "union", "typedef", "static", "enum",
    "public|", "private|", "protected|", "virtual|",
    "int|", "long|", "double|", "float|", "char|",
    "unsigned|", "signed|", NULL
};

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        "//", "/*", "*/"
    },
    {
        "java",
        JAVA_HL_extensions,
        JAVA_HL_keywords,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        "//", "/*", "*/"
    },
    {
        "python",
        PYTHON_HL_extensions,
        PYTHON_HL_keywords,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        "#", NULL, NULL
    },
    {
        "rust",
        RUST_HL_extensions,
        RUST_HL_keywords,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        "//", "/*", "*/"
    },
    {
        "html",
        HTML_HL_extensions,
        HTML_HL_keywords,
        HL_HIGHLIGHT_STRINGS,
        NULL, "<!--", "-->"
    },
    {
        "javascript",
        JS_HL_extensions,
        JS_HL_keywords,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        "//", "/*", "*/"
    },
    {
        "cpp",
        CPP_HL_extensions,
        CPP_HL_keywords,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        "//", "/*", "*/"
    }
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** ------------------------- prototypes ------------------------- ***/

void editorSetStatusMessage(const char *fmt, ...);   
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** ------------------------- terminal ------------------------- ***/

/**
 * call to exit program (CTRL Q or error)
 */
void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);
    perror(s);
    exit(1);
}

/**
 * Disables raw mode
 * 
 */
void disableRawMode() {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
        die("tcsetattr");
}

/**
 * Sets flags for terminal to enter correct mode
 */
void enableRawMode() {
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
        die("tcgetattr");
    atexit(disableRawMode);

    struct termios raw = E.orig_termios;

    raw.c_iflag &= ~(ICRNL | IXON);
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
        die("tcsetattr");
}

/**
 * Reads key into seq
 * @return editorKey value of input key
 */
int editorReadKey() {
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
      if (nread == -1 && errno != EAGAIN) die("read");
    }
    if (c == '\x1b') {
        char seq[3];
        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if (seq[2] == '~') {
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
                switch (seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
            switch (seq[1])
            {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;           // Last handle for End and home keys
            }
        }

        return '\x1b';      // Returns escape character
    } else {
        return c;
    }
}

/**
 * Return cursor position to given pointers
 * 
 * @param rows returning row value of cursor
 * @param cols returning column value of cursor
 * @return 0 on success, -1 on failure
 */
int getCursorPosition(int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;
    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;
    while (i < sizeof(buf) - 1) {
      if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
      if (buf[i] == 'R') break;
      i++;
    }
    buf[i] = '\0';
    if (buf[0] != '\x1b' || buf[1] != '[') return -1;
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
    return 0;
  }

/**
 * Return number of rows and columns in user's window
 * 
 * @param rows returning row number value
 * @param cols returning column number value
 * @return 0 on success, -1 on failure
 */
int getWindowSize(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

/*** ------------------------- syntax highlighting ------------------------- ***/

/** 
 * Helper function to see if character is a seperator
 * 
 * @param c character to check
 * @return 0 if not seperator, 1 otherwise
 */
int isseparator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

/**
 * Go through characters of erow and highlight them
 * 
 * @param row erow to be updated
 */
void editorUpdateSyntax(erow *row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

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
    while (i < row->rsize) {
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_COMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i+=mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                }
                else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if (c == in_string) in_string = 0;
                i++;
                prev_sep = 0;
                continue;
            } else {
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) || 
                            (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
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
                if (!strncmp(&row->render[i], keywords[j], klen) && isseparator(row->render[i + klen])) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
          }

        prev_sep = isseparator(c);
        i++;
    }

    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.numrows)
        editorUpdateSyntax(&E.row[row->idx + 1]);
}

/**
 * Match input hl to actual colors
 * Order here is priority for coloring
 * 
 * @param hl input hl enum value
 * @return integer corresponding with needed color
 */
int editorSyntaxToColor(int hl) {
    switch(hl) {
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

/**
 * Match file name to field in HLDB, set syntax to that
 */
void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) return;
    char *ext = strrchr(E.filename, '.');
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
                (!is_ext && strstr(E.filename, s->filematch[i]))) {
                E.syntax = s;
                int filerow;
                for (filerow = 0; filerow < E.numrows; filerow++) {
                    editorUpdateSyntax(&E.row[filerow]);
                }

                return;
            }
            i++;
        }
    }
}

/*** ------------------------- row operations ------------------------- ***/

/**
 * 
 * @param row erow containing current row being examined
 * @param cx current cx value
 * @return new rx value
 */
int editorRowCxToRx(erow *row, int cx) {
    int rx = 0;
    int j;
    for (j=0; j<cx; j++) {
        if (row->chars[j] == '\t')
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        rx++;
    }
    return rx;
}

/**
 * @param row erow containing current row being examined
 * @param rx current rx value
 * @return new cx value
 */
int editorRowRxToCx(erow *row, int rx) {
    int cur_rx = 0;
    int cx;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t')
            cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
        cur_rx++;
        if (cur_rx > rx) return cx;
    }
    return cx;
}

/**
 * Update row after changing something
 * 
 * @param row row to update
 */
void editorUpdateRow(erow *row) {
    int tabs = 0;
    int j;
    for (j = 0; j < row->size; j++)
      if (row->chars[j] == '\t') tabs++;
    free(row->render);
    row->render = malloc(row->size + tabs*(KILO_TAB_STOP-1) + 1);
    
    int idx = 0;
    for (j = 0; j < row->size; j++) {
      if (row->chars[j] == '\t') {
        row->render[idx++] = ' ';
        while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
      } else {
        row->render[idx++] = row->chars[j];
      }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}

/**
 * Insert row after given row
 * 
 * @param at row index to insert after
 * @param s string to fill line with
 * @param len length of s
 */
void editorInsertRow(int at, char *s, size_t len) {
    if (at < 0 || at > E.numrows) return;

    E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
    for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

    E.row[at].idx = at;

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].rsize = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    editorUpdateRow(&E.row[at]);

    E.numrows++;
    E.dirty++;
}

void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

/**
 * Delete row at given row index
 * 
 * @param at delete \r\n and row at index
 */
void editorDelRow(int at) {
    if (at < 0 || at >= E.numrows) return;
    editorFreeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
    for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
    E.numrows--;
    E.dirty++;
}

/**
 * Insert character at given index of given row
 * 
 * @param row erow of row being modified
 * @param at index to insert character
 * @param c character to insert
 */
void editorRowInsertChar(erow *row, int at, int c) {
    if (at < 0 || at > row->size) at = row->size;
    row->chars = realloc(row->chars, row->size + 2);
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}

/**
 * Appends string to end of existing row
 * 
 * @param row row to be appended
 * @param s character string to append to end of row
 * @param len length of s
 */
void editorRowAppendString(erow *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}

/**
 * Delete character at given index
 * 
 * @param row erow being modified
 * @param at index to delete character
 */
void editorRowDeleteChar(erow *row, int at) {
    if (at < 0 || at >= row->size) return;
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editorUpdateRow(row);
    E.dirty++;
}

/*** ------------------------- editor operations ------------------------- */

/**
 * Insert character c at current location
 * 
 * @param c character to be inserted
 */
void editorInsertChar(int c) {
    if (E.cy == E.numrows) {
        editorInsertRow(E.numrows, "", 0);         // Append new row if on last row
    }
    editorRowInsertChar(&E.row[E.cy], E.cx, c);
    E.cx++;
    E.lx = E.cx;
}

/**
 * Inserts new line after cursor position
 */
void editorInsertNewline(){
    if (E.cx == 0) {
        editorInsertRow(E.cy, "", 0);
    } else {
        erow *row = &E.row[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editorUpdateRow(row);
    }
    E.cy++;
    E.cx = 0;
    E.lx = E.cx;
}

/**
 * Handler for delete key
 */
void editorDelChar() {
    if (E.cy == E.numrows) return;
    if (E.cx == 0 && E.cy == 0) return;

    erow *row = &E.row[E.cy];
    if (E.cx > 0) {
        editorRowDeleteChar(row, E.cx - 1);
        E.cx--;
    } else {
        E.cx = E.row[E.cy - 1].size;
        editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
        editorDelRow(E.cy);
        E.cy--;
    }
    E.lx = E.cx;
} 

/*** ------------------------- file i/o ------------------------- ***/

/**
 * @return character pointer to string version of document
 * @param buflen pointer to length of document, updated
 */
char *editorRowsToString(int *buflen) {
    int totlen = 0;
    int j;
    for (j = 0; j < E.numrows; j++)
      totlen += E.row[j].size + 1;
    *buflen = totlen;
    char *buf = malloc(totlen);
    char *p = buf;
    for (j = 0; j < E.numrows; j++) {
      memcpy(p, E.row[j].chars, E.row[j].size);
      p += E.row[j].size;
      *p = '\n';
      p++;
    }
    return buf;
}

/**
 * Open and read file from disk
 */
void editorOpen(char *filename) {
    free(E.filename);
    E.filename = strdup(filename);

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp) die("fopen");
    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
            while (linelen > 0 && (line[linelen - 1] == '\n' ||
                                   line[linelen - 1] == '\r'))
            linelen--;
        editorInsertRow(E.numrows, line, linelen);
    }
    free(line);
    fclose(fp);
    E.dirty = 0;
}

/**
 * Export current state of file to user file system, prompt if file does not exist
 */
void editorSave() {
    if (E.filename == NULL) {
        E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
        if (E.filename == NULL) {
          editorSetStatusMessage("Save aborted");
          return;
        }
    }
    int len;
    char *buf = editorRowsToString(&len);
    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            if (write(fd, buf, len) == len) {
                close(fd);
                free(buf);
                E.dirty = 0;
                editorSetStatusMessage("%d bytes written to disk", len);
                return;
            }
        }
        close(fd);
    }
    free(buf);
    editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));

    editorSelectSyntaxHighlight();
}

/*** ------------------------- find ------------------------- ***/

/**
 * Callback function for incremental search
 * 
 * @param query input query to search for
 * @param key last input key
 */
void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;

    // Restore highlighting
    static int saved_hl_line;
    static char *saved_hl = NULL;
    if (saved_hl) {
      memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
      free(saved_hl);
      saved_hl = NULL;
    }

    // Navigation keys
    if (key == '\r' || key == '\x1b') {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_UP) {
        direction = -1;
    } else if (key == ARROW_DOWN) {
        direction = 1;
    } else {
        last_match = -1;
        direction = 1;
    }

    // Finding match
    if (last_match == -1) direction = 1;
    int current = last_match;
    int i;
    for (i = 0; i < E.numrows; i++) {
        current += direction;
        if (current == -1) current = E.numrows - 1;
        else if (current == E.numrows) current = 0;
        erow *row = &E.row[current];
        char *match = strstr(row->render, query);
        if (match) {
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render);
            E.lx = E.cx;
            E.rowoff = E.numrows;


            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

/**
 * Query user, search document for input string
 */
void editorFind() {
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char *query = editorPrompt("Search: %s (ESC to cancel, Enter to finish, Arrows to move)", editorFindCallback);

    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}

/*** ------------------------- append buffer ------------------------- ***/

struct abuf {
    char *b;
    int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);
    if (new == NULL) return;
    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf *ab) {
    free(ab->b);
}

/*** ------------------------- output ------------------------- ***/

/**
 * Update rowoff and coloff with current scroll value
 */
void editorScroll() {
    E.rx = E.cx;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
    }

    if (E.cy < E.rowoff) {
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        E.rowoff = E.cy - E.screenrows + 1;
    }
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}

/**
 * Draws rows according to buffer
 * 
 * @param ab Buffer struct to be drawn from
 */
void editorDrawRows(struct abuf *ab) {
    int y;
    for (y = 0; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;
        if (filerow >= E.numrows) {
            if (E.numrows == 0 && y == E.screenrows / 3) {
            char welcome[80];
            int welcomelen = snprintf(welcome, sizeof(welcome),
                "Kilo editor -- version %s", KILO_VERSION);
            if (welcomelen > E.screencols) welcomelen = E.screencols;
            int padding = (E.screencols - welcomelen) / 2;
            if (padding) {
                abAppend(ab, "~", 1);
                padding--;
            }
            while (padding--) abAppend(ab, " ", 1);
            abAppend(ab, welcome, welcomelen);
            } else {
                abAppend(ab, "~", 1);
            }
        } else {
            int len = E.row[filerow].rsize - E.coloff;
            if (len < 0) len = 0;
            if (len > E.screencols) len = E.screencols;
            char *c = &E.row[filerow].render[E.coloff];
            unsigned char *hl = &E.row[filerow].hl[E.coloff];
            int curcolor = -1;
            int j;
            for (j = 0; j < len; j++) {
                if (iscntrl(c[j])) {
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    abAppend(ab, "\x1b[7m", 4);
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);
                    if (curcolor != -1) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", curcolor);
                        abAppend(ab, buf, clen);
                    }
                }
                else if (hl[j] == HL_NORMAL) {
                    if (curcolor != -1) {
                        abAppend(ab, "\x1b[39m", 5);
                        curcolor = -1;
                    }
                    abAppend(ab, &c[j], 1);
                }
                else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != curcolor) {
                        curcolor = color;
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
        }
        abAppend(ab, "\x1b[K", 3);
        abAppend(ab, "\r\n", 2);
    }
}

/**
 * Draw status bar on second to bottom line of terminal
 * 
 * @param ab buffer struct to be drawn from
 */
void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4);

    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
        E.filename ? E.filename : "[No Name]", E.numrows,
        E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
        E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);

    if (len > E.screencols) len = E.screencols;
    abAppend(ab, status, len);
    
    while(len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        }
        else {
            abAppend(ab, " ", 1);
            len++;
        }
    }
    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}

/**
 * Draw message bar on bottom line of terminal
 * 
 * @param ab buffer to be drawn from
 */
void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3);

    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) msglen = E.screencols;
    if (msglen && (time(NULL) - E.statusmsg_time < 5)) {
        abAppend(ab, E.statusmsg, msglen);
    }
}

/**
 * Clear and refresh screen and buffer
 */
void editorRefreshScreen() {
    editorScroll();

    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6);
    abAppend(&ab, "\x1b[H", 3);
    
    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);
    
    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 1);
    abAppend(&ab, buf, strlen(buf));
    
    abAppend(&ab, "\x1b[?25h", 6);
    
    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

/**
 * Set status message to given string
 * Supports usual c string printing format
 * 
 * @param fmt String to set status message to
 */
void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);      // Get parameters for printing string
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

/*** ------------------------- input ------------------------- ***/

/**
 * Prompt the user with given prompt
 * 
 * @param prompt prompt to give the user
 * @param callback callback function to use
 * @return user input to given prompt
 */
char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0) buf[--buflen] = '\0';
        } else if (c == '\x1b') {
            editorSetStatusMessage("");
            if (callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen != 0) {
                editorSetStatusMessage("");
                if (callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
              bufsize *= 2;
              buf = realloc(buf, bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }
        if (callback) callback(buf, c);
    }
}

/**
 * Handles moving cursor with arrow keys
 * 
 * @param key editorKey value of key pressed
 */
void editorMoveCursor(int key) {
    erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];

    switch (key) {
        case ARROW_LEFT:
            if (E.cx != 0){
                E.cx--;
            } else if (E.cy > 0) {
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            E.lx = E.cx;
            break;
        case ARROW_RIGHT:
            if (row && E.cx < row->size) {
                E.cx++;
            } else if (row && E.cx == row->size) {
                E.cy++;
                E.cx = 0;
            }
            E.lx = E.cx;
            break;
        case ARROW_UP:
            if (E.cy != 0) {
                E.cy--;
                E.cx = E.lx;
            }
            break;
        case ARROW_DOWN:
            if (E.cy < E.numrows) {
                E.cy++;
                E.cx = E.lx;
            }
            break;
    }

    row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    int rowlen = row ? row->size : 0;
    if (E.cx > rowlen) {
        E.cx = rowlen;
    }
}

/**
 * Keypress handler
 */
void editorProcessKeypress() {
    static int quit_times = KILO_QUIT_TIMES;
    int c = editorReadKey();

    switch(c) {
        case '\r':
            editorInsertNewline();
            break;
        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0) {
                editorSetStatusMessage("WARNING: File contains unsaved changes. Press ctrl-Q %d more times to quit", quit_times);
                quit_times--;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;

        case HOME_KEY:
            E.cx = 0;
            E.lx = 0;
            break;
        case END_KEY: 
            if (E.cy < E.numrows) {
                E.cx = E.row[E.cy].size;
                E.lx = E.cx;
            }
            break;

        case CTRL_KEY('s'):
            editorSave();
            break;

        case CTRL_KEY('f'):
            editorFind();
            break;

        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if (c==DEL_KEY) editorMoveCursor(ARROW_RIGHT);
            editorDelChar();
            break;
        
        case PAGE_UP:
        case PAGE_DOWN:
            {
                if (c == PAGE_UP) {
                    E.cy = E.rowoff;
                } else if (c == PAGE_DOWN) {
                    E.cy = E.rowoff + E.screenrows - 1;
                    if (E.cy > E.numrows) E.cy = E.numrows;
                }

                int times = E.screenrows;
                while (times--)
                    editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }
            break;

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;

        case CTRL_KEY('l'):
        case '\x1b':
            break;

        default:
            editorInsertChar(c);
            break;
    }
    quit_times = KILO_QUIT_TIMES;
}

/*** ------------------------- init ------------------------- ***/

/**
 * Initialize editorConfig struct
 */
void initEditor() {
    E.cx = 0;
    E.cy = 0;
    E.lx = 0;
    E.rx = 0;
    E.numrows = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.dirty = 0;
    E.row = NULL;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;

    if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
    E.screenrows -= 2;
}

int main(int argc, char *argv[]){
    enableRawMode();
    initEditor();

    if (argc >= 2) {
        editorOpen(argv[1]);
    }

    editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (1){
        editorRefreshScreen();
        editorProcessKeypress();
    }
    return 0;
}
