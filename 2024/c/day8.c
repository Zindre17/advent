#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "collections/list.h"

typedef size_t row_t;
typedef size_t col_t;

typedef struct {
    row_t row;
    col_t col;
} position_t;

typedef struct {
    char frequency;
    position_t position;
} antenna_t;

typedef antenna_t antinode_t;

typedef struct {
    col_t cols;
    row_t rows;
    list_t *antennas;
    list_t *antinodes;
    list_t *antinodes_p2;
} map_t;

map_t *map_create() {
    map_t *map = malloc(sizeof(map_t));
    map->cols = 0;
    map->rows = 0;
    map->antennas = list_create();
    map->antinodes = list_create();
    map->antinodes_p2 = list_create();
    return map;
}

void map_destroy(map_t **map) {
    list_destroy(&(*map)->antennas);
    list_destroy(&(*map)->antinodes);
    free(*map);
    *map = NULL;
}

antenna_t *antenna_create(char frequency, row_t row, col_t col) {
    antenna_t *antenna = malloc(sizeof(antenna_t));
    antenna->frequency = frequency;
    antenna->position.row = row;
    antenna->position.col = col;
    return antenna;
}

antinode_t *antinode_create(char frequency, row_t row, col_t col) {
    return antenna_create(frequency, row, col);
}

col_t count_columns(FILE *file_ptr) {
    char c = fgetc(file_ptr);
    col_t cols = 0;

    while(c != '\n') {
        cols++;
        c = fgetc(file_ptr);
    }

    rewind(file_ptr);
    return cols;
}

row_t count_rows(FILE *file_ptr, col_t cols) {
    fseek(file_ptr, 0, SEEK_END);
    int charcount = ftell(file_ptr);
    row_t rows = charcount / (cols + 1);
    rewind(file_ptr);
    return rows;
}

void map_add_antenna(map_t *map, char frequency, row_t row, col_t col) {
    antenna_t *antenna = antenna_create(frequency, row, col);
    list_add(map->antennas, antenna);
}

void identify_antennas(FILE *fd, map_t *map) {
    row_t row = 0;
    col_t col = 0;
    while(!feof(fd)) {
        char c = fgetc(fd);
        if (isalnum(c)) {
            map_add_antenna(map, c, row, col);
        }
        col++;
        if(c == '\n') {
            col = 0;
            row++;
        }
    }
}

map_t *read_map(const char *filename) {
    map_t *map = map_create();

    FILE *fd = fopen(filename, "r");
        map->cols = count_columns(fd);
        map->rows = count_rows(fd, map->cols);
        identify_antennas(fd, map);
    fclose(fd);

    return map;
}

void print_antenna(antenna_t *antenna) {
    printf("%c: %i,%i", antenna->frequency, antenna->position.row, antenna->position.col);
}

void print_map(map_t *map) {
    printf("Map:\n\trows:%i\n\tcols:%i\n\tantennas:%i\n", map->rows, map->cols, list_count(map->antennas));
    antenna_t **a = list_items(map->antennas);
    for(size_t i = 0; i < list_count(map->antennas); i++) {
        antenna_t *current = a[i];
        printf("\t\t");
        print_antenna(current);
        printf("\n");
    }
    printf("\tantinodes:%i\n", list_count(map->antinodes));
    antinode_t **n = (antinode_t**)list_items(map->antinodes);
    for(size_t i = 0; i < list_count(map->antinodes); i++) {
        antinode_t *current = n[i];
        printf("\t\t");
        print_antenna((antenna_t*)current);
        printf("\n");
    }
}
typedef struct {
    void *a;
    void *b;
} pair_t;

list_t *permutations_pair(list_t *list) {
    list_t *pairs = list_create();
    void **entries = list_items(list);
    for(size_t a = 0; a < list_count(list) - 1; a++) {
        for (size_t b = a+1; b < list_count(list); b++) {
            pair_t *pair = malloc(sizeof(pair_t));
            pair->a = entries[a];
            pair->b = entries[b];
            list_add(pairs, pair);
        }
    }
    return pairs;
}

typedef struct {
    int dx;
    int dy;
} distance_t;

distance_t antenna_calculate_distance(antenna_t *a, antenna_t *b) {
    distance_t result = { b->position.col - a->position.col, b->position.row - a->position.row };
    return result;
}

bool map_within_bounds(map_t *map, antinode_t *node) {
    position_t p = node->position;
    return p.row >= 0 && p.row < map->rows
        && p.col >= 0 && p.col < map->cols;
}

void create_antinode_pair(map_t *map, antenna_t *a, antenna_t *b) {
    distance_t distance = antenna_calculate_distance(a, b);
    distance_t inverse = { -distance.dx, -distance.dy };
    antinode_t *node = antinode_create(a->frequency, a->position.row + inverse.dy, a->position.col + inverse.dx);
    if (map_within_bounds(map, node)) {
        list_add(map->antinodes, node);
    }
    node = antinode_create(b->frequency, b->position.row + distance.dy, b->position.col + distance.dx);
    if (map_within_bounds(map, node)) {
        list_add(map->antinodes, node);
    }
}

void create_repeating_antinodes(map_t *map, antenna_t *a, antenna_t *b) {
    distance_t distance = antenna_calculate_distance(a, b);
    antinode_t *node = antinode_create(a->frequency, a->position.row + distance.dy, a->position.col + distance.dx);
    while(map_within_bounds(map, node)) {
        list_add(map->antinodes_p2, node);
        node = antinode_create(node->frequency, node->position.row + distance.dy, node->position.col + distance.dx);
    }
    node = antinode_create(b->frequency, b->position.row - distance.dy, b->position.col - distance.dx);
    while(map_within_bounds(map, node)) {
        list_add(map->antinodes_p2, node);
        node = antinode_create(node->frequency, node->position.row - distance.dy, node->position.col - distance.dx);
    }
}

void find_antinodes(map_t *map) {
    list_t *pair_list = permutations_pair(map->antennas);
    pair_t **pairs = list_items(pair_list);
    for(size_t i = 0; i < list_count(pair_list); i++) {
        pair_t *pair = pairs[i];
        antenna_t *a = pair->a;
        antenna_t *b = pair->b;
        if (a->frequency == b->frequency) {
            create_antinode_pair(map, a, b);
            create_repeating_antinodes(map, a, b);
        }
    }
}

list_t *find_unique_antinode_positions(list_t *antinode_list) {
    list_t *unique_position_list = list_create();
    antinode_t **antinodes = list_items(antinode_list);
    for (size_t i = 0; i < list_count(antinode_list); i++) {
        antinode_t *nodeToConsider = antinodes[i];
        position_t cp = nodeToConsider->position;
        bool contains = false;
        position_t **unique_positions = list_items(unique_position_list);
        for (size_t j = 0; j < list_count(unique_position_list); j++) {
            position_t up = *unique_positions[j];
            if (up.row == cp.row && up.col == cp.col) {
                contains = true;
                break;
            }
        }

        if (!contains) {
            list_add(unique_position_list, &nodeToConsider->position);
        }
    }
    return unique_position_list;
}

void main() {
    map_t *map = read_map("day8.txt");
    find_antinodes(map);
    list_t *unique_positions = find_unique_antinode_positions(map->antinodes);
    list_t *unique_positions_p2 = find_unique_antinode_positions(map->antinodes_p2);
    printf("Part 1: Unique positions:%i\n", list_count(unique_positions));
    printf("Part 2: Unique positions:%i\n", list_count(unique_positions_p2));
    map_destroy(&map);
}
