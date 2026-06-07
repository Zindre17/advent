#ifndef _LIST_H
#define _LIST_H

#include <stddef.h>

typedef struct list_t list_t;

list_t* list_create();
void list_destroy(list_t** list);

size_t list_count(list_t* list);
void* list_items(list_t* list);
void list_add(list_t* list, void* item);

#endif
