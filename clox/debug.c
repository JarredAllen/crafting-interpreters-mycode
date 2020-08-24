#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "object.h"
#include "value.h"

#include <stdio.h>

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset+1];
    printf("%-21s 0x%04x '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}
static int localVariableInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset+1];
    printf("%-21s slot %d\n", name, slot);
    return offset + 2;
}
static int parameterInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t parameter = chunk->code[offset+1];
    printf("%-21s 0x%04x\n", name, parameter);
    return offset + 2;
}
static int longConstantInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t constant = (uint16_t)(chunk->code[offset+1]) + (uint16_t)(chunk->code[offset+2] << 8);
    printf("%-21s 0x%04x '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 3;
}
static int jumpInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t argument = (uint16_t)(chunk->code[offset+1]) + (uint16_t)(chunk->code[offset+2] << 8);
    printf("%-21s 0x%04x -> 0x%04x", name, argument, offset + (int16_t)argument + 3);
    printf("\n");
    return offset + 3;
}
static int longLocalVariableInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t slot = (uint16_t)(chunk->code[offset+1]) + (uint16_t)(chunk->code[offset+2] << 8);
    printf("%-21s slot %d\n", name, slot);
    return offset + 2;
}

static int closureInstruction(Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset+1];
    printf("%-21s 0x%04x '", "OP_CLOSURE", constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    ObjFunction* function = (ObjFunction*)chunk->constants.values[constant].as.obj;
    for (int j=0; j < function->upvalueCount; j++) {
        int isLocal = chunk->code[offset++];
        int index = chunk->code[offset++];
        printf("0x%04x    |                       %s %d\n", offset-2, isLocal ? "local" : "upvalue", index);
    }
    return offset + 2;
}
static int longClosureInstruction(Chunk* chunk, int offset) {
    uint16_t constant = (uint16_t)chunk->code[offset+1] + ((uint16_t)chunk->code[offset+2] << 8);
    printf("%-21s 0x%04x '", "OP_CLOSURE_LONG", constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    ObjFunction* function = (ObjFunction*)chunk->constants.values[constant].as.obj;
    for (int j=0; j < function->upvalueCount; j++) {
        int isLocal = chunk->code[offset++];
        int index = chunk->code[offset++];
        printf("0x%04x    |                       %s %d\n", offset-2, isLocal ? "local" : "upvalue", index);
    }
    return offset + 2;
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
        case OP_PRINT:
            return simpleInstruction("OP_PRINT", offset);
        case OP_POP:
            return simpleInstruction("OP_POP", offset);
        case OP_DEFINE_GLOBAL:
            return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
        case OP_DEFINE_GLOBAL_LONG:
            return longConstantInstruction("OP_DEFINE_GLOBAL_LONG", chunk, offset);
        case OP_GET_GLOBAL:
            return constantInstruction("OP_GET_GLOBAL", chunk, offset);
        case OP_GET_GLOBAL_LONG:
            return longConstantInstruction("OP_GET_GLOBAL_LONG", chunk, offset);
        case OP_SET_GLOBAL:
            return constantInstruction("OP_SET_GLOBAL", chunk, offset);
        case OP_SET_GLOBAL_LONG:
            return longConstantInstruction("OP_SET_GLOBAL_LONG", chunk, offset);
        case OP_GET_LOCAL:
            return localVariableInstruction("OP_GET_LOCAL", chunk, offset);
        case OP_GET_LOCAL_LONG:
            return longLocalVariableInstruction("OP_GET_LOCAL_LONG", chunk, offset);
        case OP_SET_LOCAL:
            return localVariableInstruction("OP_SET_LOCAL", chunk, offset);
        case OP_SET_LOCAL_LONG:
            return longLocalVariableInstruction("OP_SET_LOCAL_LONG", chunk, offset);
        case OP_JUMP:
            return jumpInstruction("OP_JUMP", chunk, offset);
        case OP_JUMP_IF_TRUE:
            return jumpInstruction("OP_JUMP_IF_TRUE", chunk, offset);
        case OP_JUMP_IF_FALSE:
            return jumpInstruction("OP_JUMP_IF_FALSE", chunk, offset);
        case OP_CALL:
            return parameterInstruction("OP_CALL", chunk, offset);
        case OP_NOP:
            return simpleInstruction("OP_NOP", offset);
        case OP_CLOSURE:
            return closureInstruction(chunk, offset);
        case OP_CLOSURE_LONG:
            return longClosureInstruction(chunk, offset);
        case OP_GET_UPVALUE:
            return parameterInstruction("OP_GET_UPVALUE", chunk, offset);
        case OP_SET_UPVALUE:
            return parameterInstruction("OP_SET_UPVALUE", chunk, offset);
        case OP_CLOSE_UPVALUE:
            return simpleInstruction("OP_CLOSE_UPVALUE", offset);
        case OP_CLASS:
            return constantInstruction("OP_CLASS", chunk, offset);
        case OP_CLASS_LONG:
            return longConstantInstruction("OP_CLASS_LONG", chunk, offset);
        case OP_GET_PROPERTY:
            return constantInstruction("OP_GET_PROPERTY", chunk, offset);
        case OP_GET_PROPERTY_LONG:
            return longConstantInstruction("OP_GET_PROPERTY_LONG", chunk, offset);
        case OP_SET_PROPERTY:
            return constantInstruction("OP_SET_PROPERTY", chunk, offset);
        case OP_SET_PROPERTY_LONG:
            return longConstantInstruction("OP_SET_PROPERTY_LONG", chunk, offset);
        default:
            printf("Unknown opcode: 0x%x\n", instruction);
            return offset + 1;
    }
}
