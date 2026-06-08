#include <stdio.h>
#include <stdlib.h>
#include "list.h"

typedef struct list_t {
    size_t capacity;
    size_t count;
    void **entries;
} list_t;

const size_t default_capacity = 1 << 4;

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

void *list_items(list_t *list) {
    return list->entries;
}

void list_add(list_t *list, void *any) {
    if (list->count >= list->capacity) {
        size_t newCapacity = list->capacity << 1;
        void **temp = realloc(list->entries, newCapacity * sizeof(void*));
        if (temp) {
            list->entries = temp;
        } else {
            printf("Not able to realloc");
        }
        list->capacity = newCapacity;
    }
    list->entries[list->count] = any;
    list->count++;
}

size_t list_count(list_t *list) {
    return list->count;
}

