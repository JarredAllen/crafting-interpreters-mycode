#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "lexer.h"
#include "table.h"
#include "value.h"

typedef enum {
    OP_NOP,
    // Direct stack manipulation
    OP_CONSTANT,
    OP_CONSTANT_LONG,
    // OP_CONSTANT_LONG_LONG, // Include this if 65,536 constants aren't enough for code
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    // Arithmetic
    OP_NEGATE,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    // Comparisons
    OP_EQ,
    OP_LT,
    OP_LE,
    OP_GT,
    OP_GE,
    // Boolean logic
    OP_NOT,
    // Functions
    OP_RETURN,
    OP_CALL,
    OP_CLOSURE,
    OP_CLOSURE_LONG,
    // Control flow
    OP_JUMP,
    OP_JUMP_IF_TRUE,
    OP_JUMP_IF_FALSE,
    // Input/outpuit
    OP_PRINT,
    // Global variable manipulation
    OP_DEFINE_GLOBAL,
    OP_DEFINE_GLOBAL_LONG,
    OP_GET_GLOBAL,
    OP_GET_GLOBAL_LONG,
    OP_SET_GLOBAL,
    OP_SET_GLOBAL_LONG,
    // Local variable manipulation
    OP_GET_LOCAL,
    OP_GET_LOCAL_LONG,
    OP_SET_LOCAL,
    OP_SET_LOCAL_LONG,
    // Nonlocal variable manipulation
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_CLOSE_UPVALUE,
    // Object Stuff
    OP_METHOD,
    OP_METHOD_LONG,
    OP_GET_PROPERTY,
    OP_GET_PROPERTY_LONG,
    OP_SET_PROPERTY,
    OP_SET_PROPERTY_LONG,
    OP_INVOKE,
    OP_INVOKE_LONG,
    // OOP Stuff
    OP_CLASS,
    OP_CLASS_LONG,
    OP_INHERIT,
    OP_GET_SUPER,
    OP_GET_SUPER_LONG,
    OP_SUPER_INVOKE,
    OP_SUPER_INVOKE_LONG,
} OpCode;

typedef struct {
    Table stringConstants;
    uint8_t* code;
    uint* lines;
    int length;
    int capacity;
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, uint line);
void freeChunk(Chunk* chunk);
// Create a new constant and return its index in the vector
uint64_t addConstant(Chunk* chunk, Value value);
// Create a new constant and write the appropriate retrieval in the code
void writeConstant(Chunk* chunk, Value value, int line);
void fetchConstant(Chunk* chunk, uint64_t index, int line);
void findStringConstant(Chunk* chunk, Token* name);

#endif
