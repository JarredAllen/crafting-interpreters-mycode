#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"
#include <stdio.h>
#include <string.h>

#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)
static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)malloc(size);
    object->type = type;
    object->next = vm.objects;
    vm.objects = object;
    return object;
}

static uint32_t hashString(const char* string, int length) {
    uint32_t hash = 2166136261u;
    for (int i=0; i<length; i++) {
        hash ^= string[i];
        hash *= 16777619;
    }
    return hash;
}

static ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);
ObjString* allocateString(char* chars, int length, uint32_t hash) {
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
#ifdef DEBUG_STRING_INTERNING
        printf("Used interned string \"%s\"\n", chars);
#endif
        free(chars);
        return interned;
    }
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

#ifdef DEBUG_STRING_INTERNING
    printf("Made new string \"%s\"\n", chars);
#endif
    tableSet(&vm.strings, string, NIL_VAL());
    return string;
}

ObjString* copyString(const char* chars, int length) {
    char* heapChars = ALLOCATE(char, length+1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';

    return allocateString(heapChars, length, hashString(heapChars, length));
}

void printObject(Obj* object) {
    switch (object->type) {
        case OBJ_STRING:
            printf("%s", ((ObjString*)object)->chars);
            break;
    }
}

bool objectsEqual(Obj* a, Obj* b) {
    if (a->type != b->type) {
        return false;
    }
    switch (a->type) {
        case OBJ_STRING: {
            char* aStr = ((ObjString*)a)->chars;
            char* bStr = ((ObjString*)b)->chars;
            return strcmp(aStr, bStr) == 0;
        }
    }
}

ObjString* takeString(char* chars, int length) {
    return allocateString(chars, length, hashString(chars, length));
}

static ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) {
        return NULL;
    }
    uint32_t startIndex = hash % table->capacity;
    for (uint32_t offset = 0;; offset++) {
        Entry* entry = table->entries + (startIndex + offset*offset) % table->capacity;
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // Actually empty entry (not tombstone)
                return NULL;
            }
        } else if (entry->key->hash == hash
                && entry->key->length == length
                && memcmp(entry->key->chars, chars, length) == 0
        ) {
            return entry->key;
        }
        if (offset > table->capacity) {
            fprintf(stderr, "Hash table findEntry failed");
            exit(-1);
        }
    }
}
