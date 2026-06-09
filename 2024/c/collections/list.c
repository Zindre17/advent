#include <stdio.h>
#include <stdlib.h>
#include "list.h"

typedef struct list_t {
    size_t capacity;
    size_t count;
    void **entries;
} list_t;

const size_t default_capacity = 1 << 4;

static void expand(list_t *list) {
    size_t newCapacity = list->capacity << 1;
    list->entries = realloc(list->entries, newCapacity * sizeof(void*));
    list->capacity = newCapacity;
}

list_t *list_create() {
    list_t *list = malloc(sizeof(list_t));
    list->count = 0;
    list->capacity = default_capacity;
    void **buffer = calloc(list->capacity, sizeof(void*));
    list->entries = buffer;
    return list;
}

void list_destroy(list_t **list) {
    free((*list)->entries);
    free(*list);
    *list = NULL;
}

void list_destroy_deep(list_t **list) {
    for (size_t i = 0; i < list_count(*list); i++) {
        free(list_get_at(*list, i));
    }
    list_destroy(list);
}

void *list_items(list_t *list) {
    return list->entries;
}

void list_add(list_t *list, void *any) {
    if (list->count >= list->capacity) {
        expand(list);
    }
    list->entries[list->count] = any;
    list->count++;
}

size_t list_count(list_t *list) {
    return list->count;
}

void *list_get_at(list_t *list, size_t index) {
    return list->entries[index];
}

void list_insert_after(list_t *list, size_t index, void *item) {
    if (list->count >= list->capacity) {
        expand(list);
    }

    void *new_value,*old_value;
    size_t idx = index + 1;
    new_value = item;
    while(idx <= list->count) {
        old_value = list->entries[idx];
        list->entries[idx++] = new_value;
        new_value = old_value;
    }
    list->count++;
}

