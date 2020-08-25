#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

typedef enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_CLOSURE,
    OBJ_UPVALUE,
    OBJ_CLASS,
    OBJ_INSTANCE,
    OBJ_BOUND_METHOD,
} ObjType;

struct sObj {
  struct sObj* next;
  ObjType type;
  bool isMarked;
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

typedef struct {
    Obj obj;
    ObjString* name;
    Table methods;
} ObjClass;

typedef struct {
    Obj obj;
    ObjClass* class;
    Table fields;
} ObjInstance;

typedef struct {
    Obj obj;
    Value receiver;
    ObjClosure* method;
} ObjBoundMethod;

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
ObjClass* newClass(ObjString* name);
ObjInstance* newInstance(ObjClass* class);
ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method);

#define OBJ_TYPE(value) (AS_OBJECT(value)->type)
#define IS_STRING(value) isObjType(value, OBJ_STRING)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_UPVALUE(value) isObjType(value, OBJ_UPVALUE)
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && OBJ_TYPE(value) == type;
}

#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJECT(value))
#define AS_NATIVE(value) ((ObjNative*)AS_OBJECT(value))
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJECT(value))
#define AS_UPVALUE(value) ((ObjUpvalue*)AS_OBJECT(value))
#define AS_CLASS(value) ((ObjClass*)AS_OBJECT(value))
#define AS_INSTANCE(value) ((ObjInstance*)AS_OBJECT(value))
#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJECT(value))

void printObject(Obj*);

bool objectsEqual(Obj*, Obj*);

ObjString* takeString(char* chars, int length);

#endif
