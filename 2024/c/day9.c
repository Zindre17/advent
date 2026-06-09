#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "collections/list.h"

typedef struct {
    size_t id;
    bool free;
} block_t;

block_t *block_create(bool free, size_t id) {
    block_t *block = malloc(sizeof(block_t));
    block->free = free;
    block->id = id;
    return block;
}

void add_file_blocks(list_t *blocks, size_t count, size_t id) {
    for (size_t i = 0; i < count; i++) {
        block_t *block = block_create(false, id);
        list_add(blocks, block);
    }
}

void add_free_blocks(list_t *blocks, size_t count) {
    for (size_t i = 0; i < count; i++) {
        block_t *block = block_create(true, 0);
        list_add(blocks, block);
    }
}

typedef enum {
    File,
    Free
} State;

list_t *parse_disk_map(char *filename) {
    list_t *blocks = list_create();

    FILE *fd = fopen(filename, "r");
    State state = File;
    size_t id = 0;
    while(!feof(fd)) {
        char c = fgetc(fd);
        if (!isdigit(c)) {
            break;
        }
        size_t block_count = c - '0';
        if(state == File) {
            add_file_blocks(blocks, block_count, id++);
            state = Free;
        } else {
            add_free_blocks(blocks, block_count);
            state = File;
        }
    }
    fclose(fd);

    return blocks;
}

size_t find_first_free_block_index(list_t *blocks, size_t from) {
    block_t *current_block;
    for (size_t i = from; i < list_count(blocks); i++) {
        current_block = list_get_at(blocks, i);
        if (current_block->free) {
            return i;
        }
    }
    return SIZE_MAX;
}

void swap_blocks(block_t *a, block_t *b) {
    block_t temp = { a->id, a->free };
    a->free = b->free;
    a->id = b->id;
    b->free = temp.free;
    b->id = temp.id;
}

void compact_disk(list_t *blocks) {
    size_t index = list_count(blocks) - 1;
    size_t first_free_index = find_first_free_block_index(blocks, 0);
    block_t *current_block, *first_free_block;

    while(index > first_free_index) {
        current_block = list_get_at(blocks, index--);
        if(!current_block->free){
            swap_blocks(current_block, list_get_at(blocks, first_free_index));
            first_free_index = find_first_free_block_index(blocks, first_free_index);
        }
    }
}

unsigned long long int checksum_disk(list_t *blocks) {
    unsigned long long int sum = 0;
    block_t *current_block;
    for (size_t i = 0; i < list_count(blocks); i++) {
        current_block = list_get_at(blocks, i);
        if (current_block->free) break;
        sum += (i * current_block->id);
    }
    return sum;
}

void print_blocks(list_t *blocks) {
    for (size_t i = 0; i < list_count(blocks); i++) {
        block_t *block = list_get_at(blocks, i);
        printf("%c", block->free? '.' : '0' + (block->id % 10));
    }
    printf("\n");
}

typedef struct {
    size_t id;
    size_t size;
    bool free;
} multi_block_t;

multi_block_t *multi_block_create(size_t id, size_t size, bool free) {
    multi_block_t *block = malloc(sizeof(multi_block_t));
    block->id = id;
    block->size = size;
    block->free = free;
    return block;
}

size_t find_free_slot(list_t *blocks, size_t size) {
    for (size_t i = 0; i < list_count(blocks); i++) {
        multi_block_t *block = list_get_at(blocks, i);
        if(block->free && block->size >= size) {
            return i;
        }
    }
    return SIZE_MAX;
}

list_t *parse_disk_part2(const char *filename) {
    list_t *blocks = list_create();

    FILE *fd = fopen(filename, "r");
    bool reading_file = true;
    size_t id = 0;
    while(!feof(fd)) {
        char c = fgetc(fd);
        if(!isdigit(c)) {
            break;
        }
        size_t block_count = c - '0';
        multi_block_t *block;
        if(reading_file) {
            block = multi_block_create(id++, block_count, false);
        } else {
            block = multi_block_create(0, block_count, true);
        }
        list_add(blocks, block);
        reading_file = !reading_file;
    }
    fclose(fd);

    return blocks;
}

bool move_file(list_t *blocks, size_t file_index, size_t free_slot_index) {
    multi_block_t *file = list_get_at(blocks, file_index);
    multi_block_t *slot = list_get_at(blocks, free_slot_index);
    bool added_new_block = false;
    if (file->size < slot->size) {
        size_t remaining = slot->size - file->size;
        multi_block_t *new_block = multi_block_create(0, remaining, true);
        list_insert_after(blocks, free_slot_index, new_block);
        added_new_block = true;
    }
    multi_block_t temp = { file->id, file->size, file->free };
    file->free = true;
    slot->free = temp.free;
    slot->id = temp.id;
    slot->size = temp.size;
    return added_new_block;
}

void print_disk_part2(list_t *blocks) {
    multi_block_t *block;
    printf("\n\e[1F");
    for (size_t i = 0; i < list_count(blocks); i++) {
        block = list_get_at(blocks, i);
        size_t blocks_left = block->size;
        while (blocks_left) {
            char below;
            if (block->size == 1) {
                below = '.';
            } else if (block->size == 2) {
                below = blocks_left == 2 ? '\\' : '/';
            } else {
                below = blocks_left == block->size || blocks_left == 1 ? '|' : '_';
            }
            if(block->size == 1) below = '.';
            if(block->free){
                printf(".\e[1B\e[1D%c\e[1A", below);
            } else {
                printf("%c\e[1B\e[1D%c\e[1A", '0' + (block->id % 10), below);
            }
            blocks_left--;
        }
    }
    printf("\e[1B\n");
}

void compact_disk_part2(list_t *blocks) {
    for (size_t i = list_count(blocks); i-- > 0;) {
        multi_block_t *current_file = list_get_at(blocks, i);
        if (current_file->free) continue;
        size_t available_slot = find_free_slot(blocks, current_file->size);
        if (available_slot < i) {
            if (move_file(blocks, i, available_slot)) {
                i++;
            }
        }
    }
}

unsigned long long int checksum_disk_part2(list_t *blocks) {
    size_t index = 0;
    multi_block_t *block;
    unsigned long long int sum = 0;
    for (size_t i = 0; i < list_count(blocks); i++) {
        block = list_get_at(blocks, i);
        if (block->free) {
            index += block->size;
            continue;
        }
        size_t blocks_left = block->size;
        while(blocks_left) {
            sum += index * block->id;
            blocks_left--;
            index++;
        }
    }
    return sum;
}

void main(void) {
    list_t *blocks = parse_disk_map("day9-sample.txt");
    compact_disk(blocks);
    unsigned long long int sum = checksum_disk(blocks);
    printf("Checksum(part 1 - sample):%llu\n", sum);
    list_destroy_deep(&blocks);

    blocks = parse_disk_map("day9.txt");
    compact_disk(blocks);
    sum = checksum_disk(blocks);
    printf("Checksum(part 1):%llu\n", sum);
    list_destroy_deep(&blocks);

    blocks = parse_disk_part2("day9-sample.txt");
    compact_disk_part2(blocks);
    sum = checksum_disk_part2(blocks);
    printf("Checksum(part 2 - sample):%llu\n", sum);
    list_destroy_deep(&blocks);

    blocks = parse_disk_part2("day9.txt");
    compact_disk_part2(blocks);
    sum = checksum_disk_part2(blocks);
    printf("Checksum(part 2):%llu\n", sum);
    list_destroy_deep(&blocks);
}
