#include "common.h"
#include "lexer.h"

#include <stdio.h>
#include <string.h>

typedef struct {
    const char* start;
    const char* current;
    int line;
} Lexer;

Lexer lexer;

void initLexer(const char* source) {
    lexer.start = source;
    lexer.current = source;
    lexer.line = 1;
}

static bool isAtEnd();
static Token makeToken(TokenType);
static Token errorToken(const char*);
static bool match(char);
static void skipWhitespace();
static Token string();
static bool isDigit(char);
static Token number();
static bool isAlpha(char);
static Token identifier();

#define ADVANCE() (*lexer.current++)
#define PEEKN(n) (*lexer.current+n-1)
#define PEEK() PEEKN(1)
Token scanToken() {
    skipWhitespace();
    lexer.start = lexer.current;
    char c = ADVANCE();
    if (c == '\0') {
        // printf("Lexer reached end of input\n");
        lexer.current--;
        return makeToken(TOKEN_EOF);
    }
    // printf("Next character to lex: 0x%x\n", c);
    if (isDigit(c)) {
        return number();
    }
    if (isAlpha(c)) {
        return identifier();
    }
    switch (c) {
        // One-character symbols
        case '(': return makeToken(TOKEN_LEFT_PAREN);
        case ')': return makeToken(TOKEN_RIGHT_PAREN);
        case '{': return makeToken(TOKEN_LEFT_BRACE);
        case '}': return makeToken(TOKEN_RIGHT_BRACE);
        case ';': return makeToken(TOKEN_SEMICOLON);
        case ',': return makeToken(TOKEN_COMMA);
        case '.': return makeToken(TOKEN_DOT);
        case '-': return makeToken(TOKEN_MINUS);
        case '+': return makeToken(TOKEN_PLUS);
        case '/': return makeToken(TOKEN_SLASH);
        case '*': return makeToken(TOKEN_STAR);
        // One- or Two-character symbols
        case '!': return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>': return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        // Literals
        case '"': return string();
    }
    return errorToken("Unexpected character.");
}

static void skipWhitespace() {
    for (;;) {
        switch (PEEK()) {
            case '\n':
                lexer.line++;
            case ' ':
            case '\t':
            case '\r':
                ADVANCE();
                break;
            // Skip comments
            case '/':
                if (PEEKN(2) == '/') {
                    while (PEEK() != '\n' && !isAtEnd()) {
                        ADVANCE();
                    }
                } else {
                    return;
                }
            default: return;
        }
    }
}

static Token string() {
    while (PEEK() != '"' && !isAtEnd()) {
        if (PEEK() == '\n') {
            lexer.line++;
        }
        ADVANCE();
    }
    if (isAtEnd()) {
        return errorToken("Unterminated string.");
    }
    ADVANCE();
    return makeToken(TOKEN_STRING);
}

static Token number() {
    while (isDigit(PEEK())) {
        ADVANCE();
    }
    if (PEEK() == '.' && isDigit(PEEKN(2))) {
        ADVANCE();
    }
    while (isDigit(PEEK())) {
        ADVANCE();
    }
    return makeToken(TOKEN_NUMBER);
}

static TokenType checkKeyword(int start, int length, const char* tail, TokenType type) {
    if (lexer.current - lexer.start == start + length && memcmp(lexer.start + start, tail, length) == 0) {
        return type;
    } else {
        return TOKEN_IDENTIFIER;
    }
}

static TokenType identifierType() {
    // fprintf(stderr, "Making a token from \"%*s\"\n", (int)(lexer.current - lexer.start), lexer.start);
    switch (*lexer.start) {
        // Single keyword letters
        case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND);
        case 'c': return checkKeyword(1, 4, "lass", TOKEN_CLASS);
        case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE);
        case 'f':
            if (lexer.current - lexer.start > 1) {
                switch (lexer.start[1]) {
                    case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
                    case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
                    case 'u': return checkKeyword(2, 1, "n", TOKEN_FUN);
                }
            }
            break;
        case 'i': return checkKeyword(1, 1, "f", TOKEN_IF);
        case 'n': return checkKeyword(1, 2, "il", TOKEN_NIL);
        case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
        case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT);
        case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
        case 's': return checkKeyword(1, 4, "uper", TOKEN_SUPER);
        case 't':
            if (lexer.current - lexer.start > 1) {
                switch (lexer.start[1]) {
                    case 'h': return checkKeyword(2, 2, "is", TOKEN_THIS);
                    case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE);
                }
            }
            break;
        case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR);
        case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE);
    }
    return TOKEN_IDENTIFIER;
}

static Token identifier() {
    while (isAlpha(PEEK()) || isDigit(PEEK())) {
        ADVANCE();
    }
    return makeToken(identifierType());
}
#undef PEEK
#undef PEEKN
#undef ADVANCE

static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || c == '_';
}

// Conditionally consume a character if and only if it is the requested
// one. Returns true if a character was consumed.
static bool match(char c) {
    if (isAtEnd()) {
        return false;
    }
    if (*lexer.current != c) {
        return false;
    }
    lexer.current++;
    return true;
}

static bool isAtEnd() {
    return *lexer.current == '\0';
}

static Token makeToken(TokenType type) {
    Token token;
    token.type = type;
    token.start = lexer.start;
    token.length = (int)(lexer.current - lexer.start);
    token.line = lexer.line;
    return token;
}

static Token errorToken(const char* message) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = lexer.line;
    return token;
}
