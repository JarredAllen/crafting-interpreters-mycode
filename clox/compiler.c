#include "common.h"
#include "chunk.h"
#include "compiler.h"
#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;
Parser parser;

Chunk* targetChunk;

static void advance();
static void consume();
static void endCompiler();
static void expression();
static void initParser();

bool compile(const char* source, Chunk* chunk) {
    initLexer(source);
    initParser();
    targetChunk = chunk;
    advance();
    expression();
    consume(TOKEN_EOF, "Expected end of expression");
    endCompiler();
    return !parser.hadError;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) {
        return;
    }
    fprintf(stderr, "[line %d] Error", token->line);
    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        fprintf(stderr, " in lexing");
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }
    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
    parser.panicMode = true;
}
static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}
static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void advance() {
    parser.previous = parser.current;
    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) {
            break;
        }
        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
    } else {
        errorAtCurrent(message);
    }
}

static void emitByte(uint8_t byte) {
    writeChunk(targetChunk, byte, parser.previous.line);
}
static void emitConstant(Value constant) {
    writeConstant(targetChunk, constant, parser.previous.line);
}

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY,
} Precedence;

typedef void (*ParseFn)();
typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static ParseRule* getRule(TokenType type);

static void parsePrecedence(Precedence precedence);

static void number() {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}
static void literal() {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: fprintf(stderr, "Unreachable line reached in <literal>"); return;
    }
}
static void grouping() {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after expression");
}
static void unary() {
    TokenType operatorType = parser.previous.type;
    parsePrecedence(PREC_UNARY);
    switch (operatorType) {
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        case TOKEN_BANG: emitByte(OP_NOT); break;
        default: fprintf(stderr, "Unreachable line reached in <unary>");
    }
}
static void binary() {
    TokenType operatorType = parser.previous.type;

    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence+1));
    switch (operatorType) {
        case TOKEN_PLUS:  emitByte(OP_ADD); break;
        case TOKEN_MINUS: emitByte(OP_SUB); break;
        case TOKEN_STAR:  emitByte(OP_MUL); break;
        case TOKEN_SLASH: emitByte(OP_DIV); break;
        case TOKEN_EQUAL_EQUAL: emitByte(OP_EQ); break;
        case TOKEN_BANG_EQUAL: emitByte(OP_EQ); emitByte(OP_NOT); break;
        case TOKEN_LESS: emitByte(OP_LT); break;
        case TOKEN_LESS_EQUAL: emitByte(OP_LE); break;
        case TOKEN_GREATER: emitByte(OP_GT); break;
        case TOKEN_GREATER_EQUAL: emitByte(OP_GE); break;
        default:          fprintf(stderr, "Unreachable line reached in <binary>"); return;
    }
}
static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expected expression");
        return;
    }
    prefixRule();
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule();
    }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = { grouping, NULL,   PREC_NONE },
    [TOKEN_RIGHT_PAREN]   = { NULL,     NULL,   PREC_NONE },
    [TOKEN_LEFT_BRACE]    = { NULL,     NULL,   PREC_NONE }, 
    [TOKEN_RIGHT_BRACE]   = { NULL,     NULL,   PREC_NONE },
    [TOKEN_COMMA]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_DOT]           = { NULL,     NULL,   PREC_NONE },
    [TOKEN_MINUS]         = { unary,    binary, PREC_TERM },
    [TOKEN_PLUS]          = { NULL,     binary, PREC_TERM },
    [TOKEN_SEMICOLON]     = { NULL,     NULL,   PREC_NONE },
    [TOKEN_SLASH]         = { NULL,     binary, PREC_FACTOR },
    [TOKEN_STAR]          = { NULL,     binary, PREC_FACTOR },
    [TOKEN_BANG]          = { unary,    NULL,   PREC_NONE },
    [TOKEN_BANG_EQUAL]    = { NULL,     binary, PREC_EQUALITY },
    [TOKEN_EQUAL]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_EQUAL_EQUAL]   = { NULL,     binary, PREC_EQUALITY },
    [TOKEN_GREATER]       = { NULL,     binary, PREC_COMPARISON },
    [TOKEN_GREATER_EQUAL] = { NULL,     binary, PREC_COMPARISON },
    [TOKEN_LESS]          = { NULL,     binary, PREC_COMPARISON },
    [TOKEN_LESS_EQUAL]    = { NULL,     binary, PREC_COMPARISON },
    [TOKEN_IDENTIFIER]    = { NULL,     NULL,   PREC_NONE },
    [TOKEN_STRING]        = { NULL,     NULL,   PREC_NONE },
    [TOKEN_NUMBER]        = { number,   NULL,   PREC_NONE },
    [TOKEN_AND]           = { NULL,     NULL,   PREC_NONE },
    [TOKEN_CLASS]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_ELSE]          = { NULL,     NULL,   PREC_NONE },
    [TOKEN_FALSE]         = { literal,  NULL,   PREC_NONE },
    [TOKEN_FOR]           = { NULL,     NULL,   PREC_NONE },
    [TOKEN_FUN]           = { NULL,     NULL,   PREC_NONE },
    [TOKEN_IF]            = { NULL,     NULL,   PREC_NONE },
    [TOKEN_NIL]           = { literal,  NULL,   PREC_NONE },
    [TOKEN_OR]            = { NULL,     NULL,   PREC_NONE },
    [TOKEN_PRINT]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_RETURN]        = { NULL,     NULL,   PREC_NONE },
    [TOKEN_SUPER]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_THIS]          = { NULL,     NULL,   PREC_NONE },
    [TOKEN_TRUE]          = { literal,  NULL,   PREC_NONE },
    [TOKEN_VAR]           = { NULL,     NULL,   PREC_NONE },
    [TOKEN_WHILE]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_ERROR]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_EOF]           = { NULL,     NULL,   PREC_NONE },
};
static ParseRule* getRule(TokenType type) {
    return rules+type;
}

static void endCompiler() {
    emitByte(OP_RETURN);
#ifdef DEBUG_PRINT_CODE
    disassembleChunk(targetChunk, "code");
#endif
}

static void initParser() {
    parser.hadError = false;
    parser.panicMode = false;
}
