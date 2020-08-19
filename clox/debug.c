#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "value.h"

#include <stdio.h>

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset+1];
    printf("%-16s 0x%04x '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}
static int longConstantInstruction(const char* name, Chunk* chunk, int offset) {
    int constant = chunk->code[offset+1] + (chunk->code[offset+2] << 8);
    printf("%-16s 0x%04x '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 3;
}

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);
    for (int offset = 0; offset < chunk->length;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("0x%04x ", offset);
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset-1]) {
        printf("   | ");
    } else {
        printf("%4d ", chunk->lines[offset]);
    }
    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
    case OP_RETURN:
        return simpleInstruction("OP_RETURN", offset);
    case OP_CONSTANT:
        return constantInstruction("OP_CONSTANT", chunk, offset);
    case OP_CONSTANT_LONG:
        return longConstantInstruction("OP_CONSTANT_LONG", chunk, offset);
    case OP_NIL:
        return simpleInstruction("OP_NIL", offset);
    case OP_TRUE:
        return simpleInstruction("OP_TRUE", offset);
    case OP_FALSE:
        return simpleInstruction("OP_FALSE", offset);
    case OP_NEGATE:
        return simpleInstruction("OP_NEGATE", offset);
    case OP_NOT:
        return simpleInstruction("OP_NOT", offset);
    case OP_ADD:
        return simpleInstruction("OP_ADD", offset);
    case OP_SUB:
        return simpleInstruction("OP_SUB", offset);
    case OP_MUL:
        return simpleInstruction("OP_MUL", offset);
    case OP_DIV:
        return simpleInstruction("OP_DIV", offset);
    case OP_EQ:
        return simpleInstruction("OP_EQ", offset);
    case OP_LT:
        return simpleInstruction("OP_LT", offset);
    case OP_LE:
        return simpleInstruction("OP_LE", offset);
    case OP_GT:
        return simpleInstruction("OP_GT", offset);
    case OP_GE:
        return simpleInstruction("OP_GE", offset);
    default:
        printf("Unknown opcode: 0x%x\n", instruction);
        return offset + 1;
    }
}
