#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

typedef struct {
    ObjString* key;
    Value value;
} Entry;

typedef struct {
    uint count;
    uint capacity;
    Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);

bool tableSet(Table* table, ObjString* key, Value value);
void copyTable(Table* from, Table* to);

bool tableGet(Table* table, ObjString* key, Value* value);

bool tableDelete(Table* table, ObjString* key);
void tableRemoveWhite(Table* table);

#endif
