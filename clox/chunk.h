#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_CONSTANT,
    OP_CONSTANT_LONG,
    // OP_CONSTANT_LONG_LONG, // Include this if 65,536 constants aren't enough for stuff
    OP_NEGATE,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_RETURN,
} OpCode;

typedef struct {
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

#endif
