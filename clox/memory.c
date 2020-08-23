#include "common.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

static void freeObject(Obj* object) {
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
