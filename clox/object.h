#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

typedef enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_NATIVE,
} ObjType;

struct sObj {
  ObjType type;
  struct sObj* next;
};

typedef struct {
    Obj obj;
    int arity;
    Chunk chunk;
    ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);
typedef struct {
    Obj obj;
    NativeFn function;
} ObjNative;

struct sObjString {
    Obj obj;
    uint32_t hash;
    int length;
    char* chars;
};

ObjString* copyString(const char* chars, int length);

ObjFunction* newFunction();

ObjNative* newNative(NativeFn);

#define OBJ_TYPE(value) (value.as.obj->type)
#define IS_STRING(value) isObjType(value, OBJ_STRING)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && value.as.obj->type == type;
}

void printObject(Obj*);

bool objectsEqual(Obj*, Obj*);

ObjString* takeString(char* chars, int length);

#endif
