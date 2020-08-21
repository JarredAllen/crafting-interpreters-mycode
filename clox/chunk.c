#include "chunk.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include <stdio.h>

void initChunk(Chunk* chunk) {
    chunk->length = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
    initTable(&chunk->stringConstants);
}

void writeChunk(Chunk* chunk, uint8_t byte, uint line) {
    if (chunk->capacity < chunk->length+1) {
        // reallocate
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(uint, chunk->lines, oldCapacity, chunk->capacity);
    }
    chunk->code[chunk->length] = byte;
    chunk->lines[chunk->length] = line;
    chunk->length++;
}

void freeChunk(Chunk* chunk) {
    free(chunk->code);
    free(chunk->lines);
    freeValueArray(&chunk->constants);
    freeTable(&chunk->stringConstants);
    initChunk(chunk);
}

uint64_t addConstant(Chunk*chunk, Value value) {
    // TODO Expand this to also check for other constants in the constants array
    if (IS_STRING(value)) {
        Value index;
        if (tableGet(&chunk->stringConstants, AS_STRING(value), &index)) {
            return (uint64_t) index.as.integer;
        } else {
            writeValueArray(&chunk->constants, value);
            uint64_t index = chunk->constants.length - 1;
            tableSet(&chunk->stringConstants, AS_STRING(value), INTEGER_VAL(index));
            return index;
        }
    } else {
        writeValueArray(&chunk->constants, value);
        return chunk->constants.length - 1;
    }
}

void writeConstant(Chunk* chunk, Value value, int line) {
    int index = addConstant(chunk, value);
    fetchConstant(chunk, index, line);
}
void fetchConstant(Chunk* chunk, uint64_t index, int line) {
    if (index <= 0xFF) {
        writeChunk(chunk, OP_CONSTANT, line);
        writeChunk(chunk, index & 0xFF, line);
    } else if (index <= 0xFFFF) {
        writeChunk(chunk, OP_CONSTANT, line);
        writeChunk(chunk, index & 0xFF, line);
        writeChunk(chunk, (index >> 8) & 0xFF, line);
    } else {
        fprintf(stderr, "Constant too large: 0x%lx", index);
        exit(65);
    }
}
