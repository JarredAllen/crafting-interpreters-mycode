#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "value.h"
#include "vm.h"
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
}

void initVM() {
    resetStack();
    vm.objects = NULL;
}

void freeVM() {
    freeObjects();
}

static void stackPush(Value value) {
    *vm.stackTop++ = value;
}
static Value stackPop() {
    return *--vm.stackTop;
}

static Value stackPeek(int distance) {
    return vm.stackTop[-1 - distance];
}

static InterpretResult run();

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);
    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }
    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;
    InterpretResult result = run();
    freeChunk(vm.chunk);
    vm.chunk = NULL;
    return result;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);
    size_t instruction = vm.ip - vm.chunk->code - 1;
    int line = vm.chunk->lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
}

// Define some aliases for common functions used in the run function
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (vm.chunk->constants.values[READ_BYTE() | (READ_BYTE() << 8)])
#define BINARY_OP(valueType, op) \
    do { \
        if (!IS_NUMBER(stackPeek(0)) || !IS_NUMBER(stackPeek(1))) { \
            runtimeError("Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = stackPop().as.number; \
        double a = stackPop().as.number; \
        stackPush(valueType(a op b)); \
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
            case OP_NIL: {
                stackPush(NIL_VAL());
                break;
            }
            case OP_TRUE: {
                stackPush(BOOL_VAL(true));
                break;
            }
            case OP_FALSE: {
                stackPush(BOOL_VAL(false));
                break;
            }
            case OP_NEGATE: {
                if (!IS_NUMBER(stackPeek(0))) {
                    runtimeError("Unary negation operand must be a number");
                    return INTERPRET_RUNTIME_ERROR;
                }
                stackPush(NUMBER_VAL(-stackPop().as.number));
                break;
            }
            case OP_NOT: {
                stackPush(BOOL_VAL(!truthy(stackPop())));
                break;
            }
            case OP_ADD: {
                if (IS_STRING(stackPeek(0)) && IS_STRING(stackPeek(1))) {
                    ObjString* b = AS_STRING(stackPop());
                    ObjString* a = AS_STRING(stackPop());
                    int length = a->length + b->length;
                    char* chars = ALLOCATE(char, length+1);
                    memcpy(chars, a->chars, a->length);
                    memcpy(chars+a->length, b->chars, b->length);
                    chars[length] = '\0';
                    ObjString* result = takeString(chars, length);
                    stackPush(OBJ_VAL(result));
                } else if (IS_NUMBER(stackPeek(0))  && IS_NUMBER(stackPeek(1))) {
                    Value b = stackPop();
                    Value a = stackPop();
                    stackPush(NUMBER_VAL(a.as.number + b.as.number));
                } else {
                    runtimeError("Addition operands must be either two strings or two numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
             }
            case OP_SUB: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MUL: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIV: BINARY_OP(NUMBER_VAL, /); break;
            case OP_EQ: {
                Value b = stackPop();
                Value a = stackPop();
                stackPush(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_LT: BINARY_OP(BOOL_VAL, <); break;
            case OP_LE: BINARY_OP(BOOL_VAL, <=); break;
            case OP_GT: BINARY_OP(BOOL_VAL, >); break;
            case OP_GE: BINARY_OP(BOOL_VAL, >=); break;
            default: runtimeError("Reached unknown bytecode: 0x%x", instruction); return INTERPRET_RUNTIME_ERROR;
        }
    }
}
// Undefine them to be a good C programmer
#undef BINARY_OP
#undef READ_CONSTANT_LONG
#undef READ_CONSTANT
#undef READ_BYTE
