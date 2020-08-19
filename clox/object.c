#include "memory.h"
#include "object.h"
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

ObjString* allocateString(char* chars, int length) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;

    return string;
}

ObjString* copyString(const char* chars, int length) {
    char* heapChars = ALLOCATE(char, length+1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';

    return allocateString(heapChars, length);
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
    return allocateString(chars, length);
}
