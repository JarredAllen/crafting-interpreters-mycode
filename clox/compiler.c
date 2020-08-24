#include "common.h"
#include "chunk.h"
#include "compiler.h"
#include "lexer.h"
#include "memory.h"
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
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

#define MAX_LOCALS 65536
#define MAX_UPVALUES 256

typedef enum {
    TYPE_FUNCTION,
    TYPE_METHOD,
    TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;
    int localCount;
    int scopeDepth;
    Local* locals;
    Upvalue* upvalues;
} Compiler;
Compiler* currentCompiler = NULL;

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;
Parser parser;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    Token name;
} ClassCompiler;
ClassCompiler* currentClass = NULL;

Chunk* targetChunk;

static void advance();
static void consume();
static void declaration();
static ObjFunction* endCompiler();
static void expression();
static void initCompiler(Compiler*, FunctionType);
static void initParser();
static bool match(TokenType);
static void statement();

ObjFunction* compile(const char* source) {
    initLexer(source);
    initParser();
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);
    advance();
    while (!match(TOKEN_EOF)) {
        declaration();
    }
    consume(TOKEN_EOF, "Expected end of expression");
    ObjFunction* function = endCompiler();
    free(compiler.upvalues);
    return parser.hadError ? NULL : function;
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
    writeChunk(&currentCompiler->function->chunk, byte, parser.previous.line);
}
static void emitConstant(Value constant) {
    writeConstant(&currentCompiler->function->chunk, constant, parser.previous.line);
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
    return addConstant(&currentCompiler->function->chunk, OBJ_VAL(copyString(name->start, name->length)));
}
// Make a new variable (or error if the next token isn't an identifier)
// and point to its location in the constants table (or return 0 if the
// variable is being placed in local slots).
static uint64_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (currentCompiler->scopeDepth > 0) {
        return 0;
    }
    return identifierConstant(&parser.previous);
}
static void markInitialized() {
    if (currentCompiler->scopeDepth == 0) {
        return;
    }
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

static void beginScope() {
    currentCompiler->scopeDepth++;
}
static void endScope() {
    currentCompiler->scopeDepth--;
    while (currentCompiler->localCount > 0 && currentCompiler->locals[currentCompiler->localCount-1].depth > currentCompiler->scopeDepth) {
        if (currentCompiler->locals[currentCompiler->localCount-1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        currentCompiler->localCount--;
    }
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }
    consume(TOKEN_RIGHT_BRACE, "Expected '}' after block");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();
    // parameter list
    consume(TOKEN_LEFT_PAREN, "Expected '(' after function name");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            currentCompiler->function->arity++;
            if (currentCompiler->function->arity > 255) {
                errorAtCurrent("Cannot have more than 255 parameters");
            }
            uint64_t paramConstant = parseVariable("Expected parameter name");
            defineVariable(paramConstant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after function parameter list");
    // body
    consume(TOKEN_LEFT_BRACE, "Expected '{' before function body");
    block();
    ObjFunction* function = endCompiler();
    uint64_t function_index = addConstant(&currentCompiler->function->chunk, OBJ_VAL(function));
    if (function_index <= 0xFF) {
        emitByte(OP_CLOSURE);
        emitByte(function_index);
    } else if (function_index <= 0xFFFF) {
        emitByte(OP_CLOSURE_LONG);
        emitByte(function_index & 0xFF);
        emitByte(function_index >> 8);
    } else {
        fprintf(stderr, "Cannot have function past index 65536 in constants array");
        exit(-1);
    }
    for (int i=0; i<function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
    free(compiler.upvalues);
}
static void funDeclaration() {
    uint64_t global = parseVariable("Expected function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expected method name");
    uint64_t constant = identifierConstant(&parser.previous);
    FunctionType type = TYPE_METHOD;
    function(type);
    if (constant <= 0xFF) {
        emitByte(OP_METHOD);
        emitByte(constant);
    } else if (constant <= 0xFFFF) {
        emitByte(OP_METHOD_LONG);
        emitByte(constant & 0xFF);
        emitByte((constant >> 8) & 0xFF);
    } else {
        fprintf(stderr, "Cannot have method be after 65536 constants.\n");
        exit(-1);
    }
}
static void namedVariable(Token name, bool canAssign);
static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expected name of class");
    Token className = parser.previous;
    uint64_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();
    if (nameConstant < 0xFF) {
        emitByte(OP_CLASS);
        emitByte(nameConstant);
    } else if (nameConstant < 0xFFFF) {
        emitByte(OP_CLASS_LONG);
        emitByte(nameConstant & 0xFF);
        emitByte((nameConstant >> 8) & 0xFF);
    } else {
        fprintf(stderr, "Cannot have a class after 65536 other constants");
        exit(-1);
    }
    defineVariable(nameConstant);
    ClassCompiler classCompiler;
    classCompiler.name = parser.previous;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;
    namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while(!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    currentClass = currentClass->enclosing;
    currentClass = currentClass->enclosing;
}

static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_CLASS)) {
        classDeclaration();
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

static void returnStatement() {
    if (currentCompiler->type == TYPE_SCRIPT) {
        error("Cannot return outside of function/method");
    }
    if (check(TOKEN_SEMICOLON)) {
        emitByte(OP_NIL);
    } else {
        expression();
    }
    consume(TOKEN_SEMICOLON, "Expected ';' after return value");
    emitByte(OP_RETURN);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expected ';' at the end of expression statement");
    emitByte(OP_POP);
}

// Emit a jump with a placeholder length (0xFFFF) and return the
// index of the jump instruction's argument in the chunk
static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xFF);
    emitByte(0xFF);
    return currentCompiler->function->chunk.length - 2;
}
static void emitKnownJump(uint8_t instruction, int destination) {
    int jumpLength = destination - currentCompiler->function->chunk.length - 3;
    emitByte(instruction);
    emitByte(jumpLength & 0xFF);
    emitByte((jumpLength >> 8) & 0xFF);
}
static void patchJump(int offset) {
    int jumpLength = currentCompiler->function->chunk.length - offset - 2;
    if (jumpLength > 0x7FFF) {
        error("Jumping over too much code");
    }
    currentCompiler->function->chunk.code[offset] = jumpLength & 0xFF;
    currentCompiler->function->chunk.code[offset+1] = (jumpLength >> 8) & 0xFF;
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
    int loopStart = currentCompiler->function->chunk.length;
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
    int loopStart = currentCompiler->function->chunk.length;
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
        int incrementStart = currentCompiler->function->chunk.length;
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
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
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
    } else if (match(TOKEN_SEMICOLON)) {
        // Convert empty statements to nops
        emitByte(OP_NOP);
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
static int64_t addUpvalue(Compiler* compiler, uint64_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;
    for (int i=0; i<upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }
    if (index >= 0xFF) {
        error("Cannot capture as closure a variable not in the first 256 slots");
        return -1;
    }
    if (upvalueCount == MAX_UPVALUES) {
        error("Too many closure variables in function");
        return -1;
    }
    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}
static int64_t resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) {
        return -1;
    }
    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint64_t)local, true);
    }
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint64_t)local, false);
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
    } else if ((arg = resolveUpvalue(currentCompiler, &name)) != -1) {
        if (arg <= 0xFF) {
            getOp = OP_GET_UPVALUE;
            setOp = OP_SET_UPVALUE;
            index = arg;
        } else {
            fprintf(stderr, "Functions can not close over more than 256 values");
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
        fprintf(stderr, "Variable names must be in the first 65536 entries of the constants table");
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

static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Cannot have more than 255 arguments");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after argument list");
    return argCount;
}
static void call(__attribute__((unused))bool canAssign) {
    uint8_t argCount = argumentList();
    emitByte(OP_CALL);
    emitByte(argCount);
}
static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expected propery name after '.'");
    uint64_t name = identifierConstant(&parser.previous);
    if (name <= 0xFF) {
        if (canAssign && match(TOKEN_EQUAL)) {
            expression();
            emitByte(OP_SET_PROPERTY);
        } else {
            emitByte(OP_GET_PROPERTY);
        }
        emitByte(name);
    } else if (name <= 0xFFFF) {
        if (canAssign && match(TOKEN_EQUAL)) {
            expression();
            emitByte(OP_SET_PROPERTY_LONG);
        } else {
            emitByte(OP_GET_PROPERTY_LONG);
        }
        emitByte(name & 0xFF);
        emitByte((name >> 8) & 0xFF);
    } else {
        fprintf(stderr, "Field names must be in the first 65536 constants");
        exit(-1);
    }
}

static void this(__attribute__((unused)) bool canAssign) {
    if (currentClass == NULL) {
        error("Cannot use 'this' outside of a class.");
        return;
    }
    variable(false);
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = { grouping, call,   PREC_CALL },
    [TOKEN_RIGHT_PAREN]   = { NULL,     NULL,   PREC_NONE },
    [TOKEN_LEFT_BRACE]    = { NULL,     NULL,   PREC_NONE }, 
    [TOKEN_RIGHT_BRACE]   = { NULL,     NULL,   PREC_NONE },
    [TOKEN_COMMA]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_DOT]           = { NULL,     dot,    PREC_CALL },
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
    [TOKEN_THIS]          = { this,     NULL,   PREC_NONE },
    [TOKEN_TRUE]          = { literal,  NULL,   PREC_NONE },
    [TOKEN_VAR]           = { NULL,     NULL,   PREC_NONE },
    [TOKEN_WHILE]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_ERROR]         = { NULL,     NULL,   PREC_NONE },
    [TOKEN_EOF]           = { NULL,     NULL,   PREC_NONE },
};
static ParseRule* getRule(TokenType type) {
    return rules+type;
}

static ObjFunction* endCompiler() {
    emitByte(OP_NIL);
    emitByte(OP_RETURN);
    ObjFunction* function = currentCompiler->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(&function->chunk, function->name == NULL ? "<script>" : function->name->chars);
    }
#endif
    free(currentCompiler->locals);
    currentCompiler = currentCompiler->enclosing;
    return function;
}

static void initParser() {
    parser.hadError = false;
    parser.panicMode = false;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = currentCompiler;
    currentCompiler = compiler;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    compiler->locals = (Local*)malloc(sizeof(Local)*MAX_LOCALS);
    compiler->upvalues = (Upvalue*)malloc(sizeof(Upvalue)*MAX_UPVALUES);
    if (type != TYPE_SCRIPT) {
        currentCompiler->function->name = copyString(parser.previous.start, parser.previous.length);
    }
    // Stack slot 0 is reserved by the compiler for internal use. It
    // stores the function currently being evaluated, if in a function,
    // or the receiver of the method call, if in a method call.
    Local* local = &currentCompiler->locals[currentCompiler->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type == TYPE_FUNCTION || type == TYPE_SCRIPT) {
        local->name.start = "";
        local->name.length = 0;
    } else {
        local->name.start = "this";
        local->name.length = 4;
    }
}

void markCompilerRoots() {
    Compiler* compiler = currentCompiler;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
