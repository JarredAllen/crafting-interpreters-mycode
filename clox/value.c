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
    switch (value.type) {
        case VAL_NUMBER: printf("%g", value.as.number); break;
        case VAL_INTEGER: printf("%ld", value.as.integer); break;
        case VAL_BOOL: printf(value.as.boolean ? "true" : "false"); break;
        case VAL_NIL: printf("nil"); break;
        case VAL_OBJ: printObject(value.as.obj); break;
    }
}

bool truthy(Value value) {
    switch (value.type) {
        case VAL_NIL:    return false;
        case VAL_BOOL:   return value.as.boolean;
        case VAL_NUMBER:
        case VAL_INTEGER:
        case VAL_OBJ:   
            return true;
    }
    return false;
}

bool valuesEqual(Value a, Value b) {
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
    return false;
}
