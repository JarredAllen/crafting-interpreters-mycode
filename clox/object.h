#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

typedef enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_CLOSURE,
    OBJ_UPVALUE,
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
    int upvalueCount;
} ObjFunction;

typedef struct {
    Obj obj;
    Value* location;
    Value closed;
    struct ObjUpvalue* next;
} ObjUpvalue;

typedef struct {
    Obj obj;
    ObjFunction* function;
    ObjUpvalue** upvalues;
    int upvalueCount;
} ObjClosure;

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
ObjClosure* newClosure(ObjFunction*);
ObjUpvalue* newUpvalue(Value* slot);

#define OBJ_TYPE(value) (value.as.obj->type)
#define IS_STRING(value) isObjType(value, OBJ_STRING)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_UPVALUE(value) isObjType(value, OBJ_UPVALUE)
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && value.as.obj->type == type;
}

void printObject(Obj*);

bool objectsEqual(Obj*, Obj*);

ObjString* takeString(char* chars, int length);

#endif
