#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "value.h"
#include "vm.h"
#include <stdio.h>

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
}

void initVM() {
    resetStack();
}

void freeVM() {
}

static void stackPush(Value value) {
    *vm.stackTop++ = value;
}
static Value stackPop() {
    return *--vm.stackTop;
}

static InterpretResult run();

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);
    if (!compile(source, &chunk)) {
        return INTERPRET_COMPILE_ERROR;
    }
    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;
    freeChunk(vm.chunk);
    vm.chunk = NULL;
    return run();
}

#define DEBUG_TRACE_EXECUTION

// Define some aliases for common functions used in the run function
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (vm.chunk->constants.values[READ_BYTE() | (READ_BYTE() << 8)])
#define BINARY_OP(op) \
    do { \
        double b = stackPop(); \
        double a = stackPop(); \
        stackPush(a op b); \
    } while(false)
static InterpretResult run() {
    uint8_t instruction;
    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
        switch (instruction = READ_BYTE()) {
            case OP_RETURN: {
                printValue(stackPop());
                printf("\n");
                return INTERPRET_OK;
            }
            case OP_CONSTANT: {
                stackPush(READ_CONSTANT());
                break;
            }
            case OP_CONSTANT_LONG: {
                stackPush(READ_CONSTANT_LONG());
                break;
            }
            case OP_NEGATE: {
                stackPush(-stackPop());
                break;
            }
            case OP_ADD: BINARY_OP(+); break;
            case OP_SUB: BINARY_OP(-); break;
            case OP_MUL: BINARY_OP(*); break;
            case OP_DIV: BINARY_OP(/); break;
        }
    }
}
// Undefine them to be a good C programmer
#undef BINARY_OP
#undef READ_CONSTANT_LONG
#undef READ_CONSTANT
#undef READ_BYTE
