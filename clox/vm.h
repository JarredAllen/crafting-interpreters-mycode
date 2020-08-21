#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "table.h"
#include "value.h"

#define STACK_MAX 65536

typedef struct {
  Chunk* chunk;
  uint8_t* ip;
  Value* stackTop;
  Table globals;
  Table strings;
  Obj* objects;
  Value stack[STACK_MAX];
} VM;

void initVM();
void freeVM();

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;
InterpretResult interpret(const char* source);

extern VM vm;

#endif
