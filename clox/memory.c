#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include "debug.h"
#include <stdio.h>
#endif

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif
    switch (object->type) {
        case OBJ_STRING: {
            free(((ObjString*)object)->chars);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            break;
        }
        case OBJ_NATIVE: break;
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            free(closure->upvalues);
            break;
        }
        case OBJ_UPVALUE: break;
        case OBJ_CLASS: {
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            freeTable(&instance->fields);
            break;
        }
    }
    free(object);
}

void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
}

static void markRoots();
static void traceReferences();
static void sweep();

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin --\nMarking roots...\n");
#endif
    markRoots();
#ifdef DEBUG_LOG_GC
    printf("Marking roots done!\nTracing references...\n");
#endif
    traceReferences();
#ifdef DEBUG_LOG_GC
    printf("Tracing references done!\nSweeping...\n");
#endif
    tableRemoveWhite(&vm.strings);
    sweep();
    // Run the GC every 16MB of allocations
    vm.nextGC = vm.bytesAllocated + (1 << 20);
#ifdef DEBUG_LOG_GC
    printf("Sweeping done!\n--  gc end  --\n");
#endif
}

void markObject(Obj* object) {
    if (object == NULL || object->isMarked) {
        return;
    }
#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif
    object->isMarked = true;
    if (vm.grayCapacity < vm.grayCount+1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack = realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);
    }
    vm.grayStack[vm.grayCount++] = object;
}
static void markValue(Value value) {
    if (!IS_OBJ(value)) {
        return;
    }
    markObject(value.as.obj);
}
static void markTable(Table* table) {
    for (uint i=0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}
static void markRoots() {
    // Mark the stack and globals
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }
    markTable(&vm.globals);
    markTable(&vm.strings);
    // Mark any open upvalues (closed ones are caught inductively from the stack)
    for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = (ObjUpvalue*)upvalue->next) {
        markObject((Obj*)upvalue);
    }
    markCompilerRoots();
}

static void markArray(ValueArray* array) {
    for (int i=0; i < array->length; i++) {
        markValue(array->values[i]);
    }
}
static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif
    switch (object->type) {
        case OBJ_NATIVE:
        case OBJ_STRING:
            break;

        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object)->closed);
            break;

        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            markObject((Obj*)function->name);
            markArray(&function->chunk.constants);
            break;
        }

        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);
            for (int i=0; i < closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }

        case OBJ_CLASS: {
            ObjClass* class = (ObjClass*)object;
            markObject((Obj*)class->name);
            break;
        }

        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            markTable(&instance->fields);
            markObject((Obj*)instance->class);
            break;
        }
    }
}
static void traceReferences() {
    while (vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

static void sweep() {
    Obj* previous = NULL;
    Obj* object = vm.objects;
    while (object) {
        if (object->isMarked) {
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            Obj* unreached = object;
            object = object->next;
            if (previous) {
                previous->next = object;
            } else {
                vm.objects = object;
            }
            freeObject(unreached);
        }
    }
}

void* allocate(int bytes) {
    // CraftingInterpreters subtracts the old size, but we don't have it
    // because I didn't want to write a reallocate function
    vm.bytesAllocated += bytes;
#ifdef DEBUG_STRESS_GC
    collectGarbage();
#else
    if (vm.bytesAllocated > vm.nextGC) {
        collectGarbage();
    }
#endif
    return malloc(bytes);
}
