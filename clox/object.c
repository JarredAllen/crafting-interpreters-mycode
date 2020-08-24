#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"
#include <stdio.h>
#include <string.h>

#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)
static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)ALLOCATE(uint8_t, size);
    object->type = type;
    object->next = vm.objects;
    object->isMarked = false;
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

static void printFunction(ObjFunction* function) {
    if (function->name) {
        printf("<fn %s>", function->name->chars);
    } else {
        printf("<script>");
    }
}

void printObject(Obj* object) {
    switch (object->type) {
        case OBJ_STRING: {
            printf("%s", ((ObjString*)object)->chars);
            break;
        }
        case OBJ_FUNCTION: {
            printFunction((ObjFunction*)object);
            break;
        }
        case OBJ_CLASS: {
            printf("%s", ((ObjClass*)object)->name->chars);
            break;
        }
        case OBJ_INSTANCE: {
            printf("%s instance", ((ObjClass*)((ObjInstance*)object)->class)->name->chars);
            break;
        }
        case OBJ_NATIVE: {
            printf("<native fn>");
            break;
        }
        case OBJ_CLOSURE: {
            printFunction(((ObjClosure*)object)->function);
            break;
        }
        case OBJ_UPVALUE: {
            printf("upvalue: ");
            printValue(*((ObjUpvalue*)object)->location);
            break;
        }
        case OBJ_BOUND_METHOD: {
            printFunction(((ObjBoundMethod*)object)->method->function);
            break;
        }
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
        case OBJ_NATIVE:
        case OBJ_CLOSURE:
        case OBJ_UPVALUE:
        case OBJ_CLASS:
        case OBJ_INSTANCE:
        case OBJ_BOUND_METHOD:
        case OBJ_FUNCTION: {
            return a == b;
        }
    }
    return false;
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

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->name = NULL;
    function->upvalueCount = 0;
    initChunk(&function->chunk);
    return function;
}

ObjNative* newNative(NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

ObjClosure* newClosure(ObjFunction* function) {
    ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
    for (int i=0; i<function->upvalueCount; i++) {
        upvalues[i] = NULL;
    }
    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;
    return closure;
}
ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->next = NULL;
    upvalue->closed = NIL_VAL();
    return upvalue;
}
ObjClass* newClass(ObjString* name) {
    ObjClass* class = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    class->name = name;
    initTable(&class->methods);
    return class;
}
ObjInstance* newInstance(ObjClass* class) {
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->class = class;
    initTable(&instance->fields);
    return instance;
}
ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}
