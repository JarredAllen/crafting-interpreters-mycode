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
    vm.frameCount = 0;
}

void initVM() {
    resetStack();
    initTable(&vm.strings);
    initTable(&vm.globals);
    vm.objects = NULL;
}

void freeVM() {
    freeTable(&vm.strings);
    freeTable(&vm.globals);
    freeObjects();
}

static void runtimeError(const char* format, ...);
static void stackPush(Value value) {
    *vm.stackTop++ = value;
#ifdef DEBUG_RUNTIME_CHECKS
    if (vm.stackTop >= vm.stack+STACK_MAX) {
        runtimeError("Stack overflow");
    }
#endif
}
static Value stackPop() {
#ifdef DEBUG_RUNTIME_CHECKS
        if (vm.stackTop <= vm.stack) {
            fprintf(stderr, "Stack underflow\n");
            exit(-1);
        }
#endif
    return *--vm.stackTop;
}

static Value stackPeek(int distance) {
    return vm.stackTop[-1 - distance];
}

static bool call(ObjFunction* function, int argCount) {
    if (argCount != function->arity) {
        runtimeError("Expected %d arguments but got %d", function->arity, argCount);
        return false;
    }
    if (vm.frameCount >= FRAMES_MAX) {
        runtimeError("Stack overflow");
        return false;
    }
    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->function = function;
    frame->ip = function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}
static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_FUNCTION: return call((ObjFunction*)callee.as.obj, argCount);
            default:           break;
        }
    }
    runtimeError("Can only call functions and classes");
    return false;
}

static InterpretResult run();

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (function == NULL) {
        return INTERPRET_COMPILE_ERROR;
    }
    stackPush(OBJ_VAL(function));
    callValue(OBJ_VAL(function), 0);
    return run();
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);
    for (int i=vm.frameCount-1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }
    resetStack();
}

// Define some aliases for common functions used in the run function
#define READ_BYTE() (*frame->ip++)
#define READ_TWO_BYTES() ((uint16_t)READ_BYTE() + (uint16_t)(READ_BYTE() << 8))
#define READ_CONSTANT() (frame->function->chunk.constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (frame->function->chunk.constants.values[READ_TWO_BYTES()])
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
    CallFrame* frame = &vm.frames[vm.frameCount-1];
    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(&frame->function->chunk, (int)(frame->ip - frame->function->chunk.code));
#endif
        switch (instruction = READ_BYTE()) {
            case OP_RETURN: {
                Value result = stackPop();
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    stackPop();
                    return INTERPRET_OK;
                } else {
                    vm.stackTop = frame->slots;
                    stackPush(result);
                    frame = &vm.frames[vm.frameCount - 1];
                }
                break;
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
            case OP_PRINT: {
                printValue(stackPop());
                printf("\n");
                break;
            }
            case OP_POP: stackPop(); break;
            case OP_DEFINE_GLOBAL: {
                ObjString* name = AS_STRING(READ_CONSTANT());
                tableSet(&vm.globals, name, stackPeek(0));
                stackPop();
                break;
            }
            case OP_DEFINE_GLOBAL_LONG: {
                ObjString* name = AS_STRING(READ_CONSTANT_LONG());
                tableSet(&vm.globals, name, stackPeek(0));
                stackPop();
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = AS_STRING(READ_CONSTANT());
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                stackPush(value);
                break;
            }
            case OP_GET_GLOBAL_LONG: {
                ObjString* name = AS_STRING(READ_CONSTANT_LONG());
                tableSet(&vm.globals, name, stackPeek(0));
                stackPop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = AS_STRING(READ_CONSTANT());
                if (tableSet(&vm.globals, name, stackPeek(0))) {
                    runtimeError("Assignment to undefined variable '%s'", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SET_GLOBAL_LONG: {
                ObjString* name = AS_STRING(READ_CONSTANT_LONG());
                if (tableSet(&vm.globals, name, stackPeek(0))) {
                    runtimeError("Assignment to undefined variable '%s'", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                stackPush(frame->slots[slot]);
                break;
            }
            case OP_GET_LOCAL_LONG: {
                uint16_t slot = READ_TWO_BYTES();
                stackPush(frame->slots[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = stackPeek(0);
                break;
            }
            case OP_SET_LOCAL_LONG: {
                uint16_t slot = READ_TWO_BYTES();
                frame->slots[slot] = stackPeek(0);
                break;
            }
            case OP_JUMP: {
                frame->ip += (int16_t)READ_TWO_BYTES();
                break;
            }
            case OP_JUMP_IF_TRUE: {
                int16_t target = (int16_t)READ_TWO_BYTES();
                if (truthy(stackPeek(0))) {
                    frame->ip += target;
                }
                break;
            }
            case OP_JUMP_IF_FALSE: {
                int16_t target = (int16_t)READ_TWO_BYTES();
                if (!truthy(stackPeek(0))) {
                    frame->ip += target;
                }
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(stackPeek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount-1];
                break;
            }
            default: runtimeError("Reached unknown bytecode: 0x%x", instruction); return INTERPRET_RUNTIME_ERROR;
        }
    }
}
// Undefine them to be a good C programmer
#undef BINARY_OP
#undef READ_CONSTANT_LONG
#undef READ_CONSTANT
#undef READ_TWO_BYTES
#undef READ_BYTE
