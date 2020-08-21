#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "lexer.h"
#include "table.h"
#include "value.h"

typedef enum {
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
    // Control flow
    OP_RETURN,
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
