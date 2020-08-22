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
