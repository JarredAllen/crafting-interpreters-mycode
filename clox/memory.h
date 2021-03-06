#ifndef clox_memory_h
#define clox_memory_h

#include <stdlib.h>

#include "common.h"

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)realloc(pointer, sizeof(type) * (newCount))

#define ALLOCATE(type, count) ((type*)allocate(sizeof(type)*count))

void freeObjects();
void markObject();
void collectGarbage();

void* allocate(int bytes);
#endif
