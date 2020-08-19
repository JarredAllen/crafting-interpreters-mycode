#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include "memory.h"

typedef double Value;

typedef struct {
    Value* values;
    int length;
    int capacity;
} ValueArray;

void initValueArray(ValueArray* values);
void writeValueArray(ValueArray* values, Value value);
void freeValueArray(ValueArray* values);

void printValue(Value value);

#endif
