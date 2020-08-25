#include "common.h"
#include "object.h"
#include "value.h"
#include <stdio.h>

void initValueArray(ValueArray* values) {
    values->values = NULL;
    values->capacity = 0;
    values->length = 0;
}

void writeValueArray(ValueArray* values, Value value) {
    if (values->capacity < values->length+1) {
        // reallocate
        int oldCapacity = values->capacity;
        values->capacity = GROW_CAPACITY(oldCapacity);
        values->values = GROW_ARRAY(Value, values->values, oldCapacity, values->capacity);
    }
    values->values[values->length] = value;
    values->length++;
}

void freeValueArray(ValueArray* values) {
    free(values->values);
    initValueArray(values);
}

void printValue(Value value) {
    if (IS_NUMBER(value)) {
        printf("%g", AS_NUMBER(value));
    } else if (IS_INTEGER(value)) {
        printf("%d", AS_INTEGER(value));
    } else if (IS_BOOL(value)) {
        printf(AS_BOOL(value) ? "true" : "false");
    } else if (IS_NIL(value)) {
        printf("nil");
    } else if (IS_OBJ(value)) {
        printObject(AS_OBJECT(value));
    }
}

bool truthy(Value value) {
    if (IS_NIL(value)) {
        return false;
    } else if (IS_BOOL(value)) {
        return AS_BOOL(value);
    } else {
        return true;
    }
}

bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
    // Note: this says NaN == NaN is true, which goes against IEE 754
    // But the performance savings are worth it
    return a == b;
#else
    if (a.type != b.type) {
        return false;
    }
    switch (a.type) {
        case VAL_NIL:    return true;
        case VAL_BOOL:   return a.as.boolean == b.as.boolean;
        case VAL_NUMBER: return a.as.number == b.as.number;
        case VAL_INTEGER: return a.as.integer == b.as.integer;
        case VAL_OBJ:    return a.as.obj == b.as.obj;
    }
#endif
}
