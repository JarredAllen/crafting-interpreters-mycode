#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include "memory.h"

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
    } as;
} Value;

typedef struct {
    Value* values;
    int length;
    int capacity;
} ValueArray;

void initValueArray(ValueArray* values);
void writeValueArray(ValueArray* values, Value value);
void freeValueArray(ValueArray* values);

bool truthy(Value);

bool valuesEqual(Value, Value);

void printValue(Value value);

#define BOOL_VAL(value) ((Value){ VAL_BOOL, { .boolean = value } })
#define NIL_VAL() ((Value){ VAL_NIL, { .number = 0 } })
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, { .number = value } })

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

#endif
