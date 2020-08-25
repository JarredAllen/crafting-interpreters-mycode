#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include "memory.h"

#ifdef NAN_BOXING
#include <string.h>
#endif

typedef struct sObj Obj;
typedef struct sObjString ObjString;

#ifdef NAN_BOXING

#define QNAN ((uint64_t)0x7ffc000000000000)
#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define TAG_INTEGER 1
#define TAG_NIL 0
#define TAG_TRUE 2
#define TAG_FALSE 3

typedef uint64_t Value;

static inline Value numToValue(double num) {
    Value value;
    memcpy(&value, &num, sizeof(double));
    return value;
}
#define NUMBER_VAL(num) numToValue(num)
#define NIL_VAL() ((Value)(uint64_t)(QNAN | TAG_NIL))
#define FALSE_VAL() ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL() ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define BOOL_VAL(boolean) ((boolean) ? TRUE_VAL() : FALSE_VAL())
#define INTEGER_VAL(integer) ((Value)(uint64_t)(QNAN | TAG_INTEGER | ((uint64_t)integer << 2)))
#define OBJ_VAL(obj) ((Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj)))

#define IS_NUMBER(value) (((value) & QNAN) != QNAN)
#define IS_NIL(value) ((value) == NIL_VAL())
#define IS_BOOL(value) (((value) | 1) == FALSE_VAL())
#define IS_INTEGER(value) (((value) & (QNAN | TAG_INTEGER)) == (QNAN | TAG_INTEGER))
#define IS_OBJ(value) (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

static inline double valueToNum(Value value) {
    double num;
    memcpy(&num, &value, sizeof(Value));
    return num;
}
#define AS_NUMBER(value) valueToNum(value)
#define AS_BOOL(value) ((value) == TRUE_VAL())
#define AS_INTEGER(value) ((uint32_t)(((value) & ~(QNAN | TAG_INTEGER)) >> 2))
#define AS_OBJECT(value) ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

#else
typedef enum {
    VAL_NIL,
    VAL_BOOL,
    VAL_NUMBER,
    VAL_INTEGER,
    VAL_OBJ,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        int32_t integer;
        Obj* obj;
    } as;
} Value;

#define BOOL_VAL(value) ((Value){ VAL_BOOL, { .boolean = value } })
#define NIL_VAL() ((Value){ VAL_NIL, { .number = 0 } })
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, { .number = value } })
#define INTEGER_VAL(value) ((Value){ VAL_NUMBER, { .integer = value } })
#define OBJ_VAL(object) ((Value){ VAL_OBJ, { .obj = (Obj*)(object) } })

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_INTEGER(value) ((value).type == VAL_INTEGER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_INTEGER(value) ((value).as.integer)
#define AS_OBJECT(value) ((value).as.obj)

#endif

#define AS_STRING(value) ((ObjString*)AS_OBJECT(value))
#define AS_CSTRING(value) (AS_STRING(value)->chars)

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

#endif
