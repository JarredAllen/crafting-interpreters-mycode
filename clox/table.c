#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Entry* findEntry(Entry* enties, uint capacity, ObjString* key);
static void adjustCapacity(Table* table, uint capacity);

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 8;
    table->entries = (Entry*)malloc(sizeof(Entry)*table->capacity);
    for (uint i=0; i < table->capacity; i++) {
        table->entries[i].key = NULL;
        table->entries[i].value = NIL_VAL();
    }
}
void freeTable(Table* table) {
    if (table->entries) {
        free(table->entries);
    }
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

#define TABLE_MAX_LOAD 0.3
bool tableSet(Table* table, ObjString* key, Value value) {
    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = (entry->key == NULL);
    if (isNewKey) {
        table->count++;
    }
    entry->key = key;
    entry->value = value;
    if (table->count > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }
    return isNewKey;
}

void copyTable(Table* from, Table* to) {
    for (Entry* entry = from->entries; entry < from->entries+from->capacity; entry++) {
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->capacity == 0 || table->entries == NULL) {
        return false;
    }
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key) {
        *value = entry->value;
        return true;
    } else {
        return false;
    }
}

bool tableDelete(Table* table, ObjString* key) {
    if (table->capacity == 0 || table->entries == NULL) {
        return false;
    }
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key) {
        table->count--;
        entry->key = NULL;
        entry->value = BOOL_VAL(true);
        return true;
    } else {
        return false;
    }
}

// Warning: this function is unsafe if capacity is zero or if entries is null
static Entry* findEntry(Entry* entries, uint capacity, ObjString* key) {
    uint32_t startIndex = key->hash % capacity;
    Entry* tombstone = NULL;
    for (uint32_t offset = 0;; offset++) {
        Entry* entry = entries + (startIndex + offset*offset) % capacity;
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // Actually empty entry
                return tombstone == NULL ? entry : tombstone;
            } else {
                // Tombstone
                if (tombstone == NULL) {
                    tombstone = entry;
                }
            }
        } else if (entry->key == key) {
            return entry;
        }
        if (offset > capacity) {
            if (tombstone != NULL) {
                return tombstone;
            }
            fprintf(stderr, "Hash table findEntry failed");
            exit(-1);
        }
    }
}

static void adjustCapacity(Table* table, uint capacity) {
    if (capacity == 0) {
        freeTable(table);
        return;
    }
    Entry* newEntries = ALLOCATE(Entry, capacity);
    for (uint i=0; i < capacity; i++) {
        newEntries[i].key = NULL;
        newEntries[i].value = NIL_VAL();
    }
    for (uint i=0; i < table->capacity; i++) {
        Entry* src = table->entries + i;
        if (src->key != NULL) {
            Entry* dst = findEntry(newEntries, capacity, src->key);
            *dst = *src;
        }
    }
    free(table->entries);
    table->entries = newEntries;
    table->capacity = capacity;
}

void tableRemoveWhite(Table* table) {
    for (uint i=0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key && !entry->key->obj.isMarked) {
            tableDelete(table, entry->key);
        }
    }
}
