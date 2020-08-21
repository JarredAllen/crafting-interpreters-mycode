#include "common.h"
#include "chunk.h"
#include "compiler.h"
#include "lexer.h"
#include "object.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token name;
    int depth;
} Local;

#define MAX_LOCALS 65536

typedef struct {
    Local locals[MAX_LOCALS];
    int localCount;
    int scopeDepth;
} Compiler;
Compiler* currentCompiler = NULL;

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
static void declaration();
static void endCompiler();
static void expression();
static void initCompiler(Compiler* compiler);
static void initParser();
static bool match(TokenType);
static void statement();

bool compile(const char* source, Chunk* chunk) {
    initLexer(source);
    initParser();
    Compiler compiler;
    initCompiler(&compiler);
    targetChunk = chunk;
    advance();
    while (!match(TOKEN_EOF)) {
        declaration();
    }
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

static bool check(TokenType type) {
    return parser.current.type == type;
}
static bool match(TokenType type) {
    if (!check(type)) {
        return false;
    }
    advance();
    return true;
}
static void consume(TokenType type, const char* message) {
    if (!match(type)) {
        errorAtCurrent(message);
    }
}

static void emitByte(uint8_t byte) {
    writeChunk(targetChunk, byte, parser.previous.line);
}
static void emitConstant(Value constant) {
    writeConstant(targetChunk, constant, parser.previous.line);
}

static void synchronize() {
    parser.panicMode = false;
    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) {
            return;
        }
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default: // Do nothing
                ;
        }
        advance();
    }
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) {
        return false;
    }
    return memcmp(a->start, b->start, a->length) == 0;
}
static void addLocal(Token name) {
    if (currentCompiler->localCount >= MAX_LOCALS) {
        error("Too many local variables in function.");
        return;
    }
    for (int i=currentCompiler->localCount-1; i >= 0; i--) {
        Local* local = currentCompiler->locals+i;
        if (local->depth != -1 && local->depth < currentCompiler->scopeDepth) {
            break;
        }
        if (identifiersEqual(&name, &local->name)) {
            error("Variable with this name already declared in this scope.");
        }
    }
    Local* local = &currentCompiler->locals[currentCompiler->localCount++];
    local->name = name;
    local->depth = -1;
}
static void declareVariable() {
    if (currentCompiler->scopeDepth == 0) {
        return;
    }
    addLocal(parser.previous);
}
static uint64_t identifierConstant(Token* name) {
    return addConstant(targetChunk, OBJ_VAL(copyString(name->start, name->length)));
}
// Make a new variable (or error if the next token isn't an identifier)
// and point to its location in the constants table
static uint64_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (currentCompiler->scopeDepth > 0) {
        return 0;
    }
    return identifierConstant(&parser.previous);
}
static void markInitialized() {
    currentCompiler->locals[currentCompiler->localCount-1].depth = currentCompiler->scopeDepth;
}
// Make an instruction to define the variable whose name is at the given
// index of the constants table (if we're not in a scope) or into the
// stack (if we're in a scope).
// Will error if the index is too large
static void defineVariable(uint64_t index) {
    if (currentCompiler->scopeDepth > 0) {
        markInitialized();
        return;
    }
    if (index <= 0xFF) {
        emitByte(OP_DEFINE_GLOBAL);
        emitByte((uint8_t)index);
    } else if (index <= 0xFFFF) {
        emitByte(OP_DEFINE_GLOBAL_LONG);
        emitByte((uint8_t)(index & 0xFF));
        emitByte((uint8_t)((index >> 8) & 0xFF));
    } else {
        fprintf(stderr, "Global variable names must be in the first 65536 entries of the constants table");
        exit(-1);
    }
}
static void varDeclaration() {
    uint64_t global = parseVariable("Expected a variable name");
    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration");
    defineVariable(global);
}

static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }
    if (parser.panicMode) {
        synchronize();
    }
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expected ';' at end of print statement");
    emitByte(OP_PRINT);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expected ';' at the end of expression statement");
    emitByte(OP_POP);
}

static void beginScope() {
    currentCompiler->scopeDepth++;
}
static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }
    consume(TOKEN_RIGHT_BRACE, "Expected '}' after block");
}
static void endScope() {
    currentCompiler->scopeDepth--;
    while (currentCompiler->localCount > 0 && currentCompiler->locals[currentCompiler->localCount-1].depth > currentCompiler->scopeDepth) {
        emitByte(OP_POP);
        currentCompiler->localCount--;
    }
}

// Emit a jump with a placeholder length (0xFFFF) and return the
// index of the jump instruction's argument in the chunk
static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xFF);
    emitByte(0xFF);
    return targetChunk->length - 2;
}
static void emitKnownJump(uint8_t instruction, int destination) {
    int jumpLength = destination - targetChunk->length - 3;
    emitByte(instruction);
    emitByte(jumpLength & 0xFF);
    emitByte((jumpLength >> 8) & 0xFF);
}
static void patchJump(int offset) {
    int jumpLength = targetChunk->length - offset - 2;
    if (jumpLength > 0x7FFF) {
        error("Jumping over too much code");
    }
    targetChunk->code[offset] = jumpLength & 0xFF;
    targetChunk->code[offset+1] = (jumpLength >> 8) & 0xFF;
}
static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expected '(' after 'if'");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after condition");
    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    int elseJump = emitJump(OP_JUMP);
    patchJump(thenJump);
    emitByte(OP_POP);
    if (match(TOKEN_ELSE)) {
        statement();
    }
    patchJump(elseJump);
}
static void whileStatement() {
    consume(TOKEN_LEFT_PAREN, "Expected '(' after 'while'");
    int loopStart = targetChunk->length;
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after condition");
    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    emitKnownJump(OP_JUMP, loopStart);
    patchJump(exitJump);
    emitByte(OP_POP);
}
static void forStatement() {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expected '(' after 'for'");
    if (match(TOKEN_SEMICOLON)) {
        // No initializer, so we do nothing
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }
    int loopStart = targetChunk->length;
    int exitJump = -1;
    if (match(TOKEN_SEMICOLON)) {
        // No condition, so we do nothing
    } else {
        expression();
        consume(TOKEN_SEMICOLON, "Expected ';' between for clauses");
        exitJump = emitJump(OP_JUMP_IF_FALSE);
    }
    if (match(TOKEN_RIGHT_PAREN)) {
        // No increment, so we do nothing
    } else {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = targetChunk->length;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expected ')' after for clauses");
        emitKnownJump(OP_JUMP, loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }
    statement();
    emitKnownJump(OP_JUMP, loopStart);
    endScope();
    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);
    }
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else {
        expressionStatement();
    }
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

typedef void (*ParseFn)(bool canAssign);
typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static ParseRule* getRule(TokenType type);

static void parsePrecedence(Precedence precedence);

static void number(__attribute__((unused)) bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}
static void string(__attribute__((unused)) bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length-2)));
}
static int64_t resolveLocal(Compiler* compiler, Token* name) {
    for (int i=compiler->localCount-1; i >= 0; i--) {
        if (compiler->locals[i].depth != -1 && identifiersEqual(name, &compiler->locals[i].name)) {
            return i;
        }
    }
    return -1;
}
static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(currentCompiler, &name);
    uint64_t index;
    if (arg != -1) {
        index = (uint64_t)arg;
        if (index <= 0xFF) {
            getOp = OP_GET_LOCAL;
            setOp = OP_SET_LOCAL;
        } else if (index <= 0xFFFF) {
            getOp = OP_GET_LOCAL_LONG;
            setOp = OP_SET_LOCAL_LONG;
        } else {
            fprintf(stderr, "Can have no more than 65536 local variables in scope at a time");
            exit(-1);
        }
    } else {
        index = identifierConstant(&name);
        if (index <= 0xFF) {
            getOp = OP_GET_GLOBAL;
            setOp = OP_SET_GLOBAL;
        } else if (index <= 0xFFFF) {
            getOp = OP_GET_GLOBAL_LONG;
            setOp = OP_SET_GLOBAL_LONG;
        } else {
            fprintf(stderr, "Global variable names must be in the first 65536 entries of the constants table");
            exit(-1);
        }
    }
    bool assign = canAssign && match(TOKEN_EQUAL);
    if (assign) {
        expression();
        emitByte(setOp);
    } else {
        emitByte(getOp);
    }
    if (index <= 0xFF) {
        emitByte((uint8_t) index);
    } else if (index <= 0xFFFF) {
        emitByte((uint8_t)(index & 0xFF));
        emitByte((uint8_t)((index >> 8) & 0xFF));
    } else {
        fprintf(stderr, "Global variable names must be in the first 65536 entries of the constants table");
        exit(-1);
    }
}
static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}
static void literal(__attribute__((unused)) bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: fprintf(stderr, "Unreachable line reached in <literal>"); return;
    }
}
static void grouping(__attribute__((unused)) bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after expression");
}
static void unary(__attribute__((unused)) bool canAssign) {
    TokenType operatorType = parser.previous.type;
    parsePrecedence(PREC_UNARY);
    switch (operatorType) {
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        case TOKEN_BANG: emitByte(OP_NOT); break;
        default: fprintf(stderr, "Unreachable line reached in <unary>");
    }
}
static void binary(__attribute__((unused)) bool canAssign) {
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
static void and_(__attribute__((unused)) bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    parsePrecedence(PREC_AND);
    patchJump(endJump);
}
static void or_(__attribute__((unused)) bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_TRUE);
    emitByte(OP_POP);
    parsePrecedence(PREC_OR);
    patchJump(endJump);
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
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }
    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target");
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
    [TOKEN_IDENTIFIER]    = { variable, NULL,   PREC_NONE },
    [TOKEN_STRING]        = { string,   NULL,   PREC_NONE },
    [TOKEN_NUMBER]        = { number,   NULL,   PREC_NONE },
    [TOKEN_AND]           = { NULL,     and_,   PREC_AND  },
    [TOKEN_CLASS]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_ELSE]          = { NULL,     NULL,   PREC_NONE },
    [TOKEN_FALSE]         = { literal,  NULL,   PREC_NONE },
    [TOKEN_FOR]           = { NULL,     NULL,   PREC_NONE },
    [TOKEN_FUN]           = { NULL,     NULL,   PREC_NONE },
    [TOKEN_IF]            = { NULL,     NULL,   PREC_NONE },
    [TOKEN_NIL]           = { literal,  NULL,   PREC_NONE },
    [TOKEN_OR]            = { NULL,     or_,    PREC_OR   },
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
#ifdef DEBUG_PRINT_CODE
    disassembleChunk(targetChunk, "code");
#endif
}

static void initParser() {
    parser.hadError = false;
    parser.panicMode = false;
}

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    currentCompiler = compiler;
}
