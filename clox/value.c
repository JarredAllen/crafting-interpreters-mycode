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
    printf("%g", value);
}
