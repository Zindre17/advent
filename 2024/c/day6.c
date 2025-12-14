#include <stdio.h>
#include <stdlib.h>

typedef struct{
    int row;
    int column;
} position;

typedef struct{
    int height;
    int width;
} size;

typedef struct{
    int x;
    int y;
} vector;

int isguard(char c){ return c == '^';}
int isobstacle(char c){ return c == '#';}

int getlinelength(char* line, int length){
    int i = 0;
    while(line[i++] != '\n' && i < length){}
    return i -1 ;
}

size getMapSize(FILE* file){
    const int maxLength = 10000;
    char line[maxLength];
    int missingWidth = 1;
    int width;
    int height = 0;
    while(fgets(line, maxLength, file)){
        if(missingWidth){
            missingWidth = 0;
            width = getlinelength(line, maxLength);
        }
        height++;
    }
    rewind(file);
    return (size){height, width};
}

void fillMap(size mapSize, char** map, FILE* file){
    char line[mapSize.width*2];
    for(int row = 0; row < mapSize.height; row++){
        fgets(line, mapSize.width *2 , file);
        for(int column = 0; column < mapSize.width; column++){
            map[row][column] = line[column];
        }
    }
    rewind(file);
}

void printMap(size mapSize, char** map){
    for(int row = 0; row < mapSize.height; row++){
        for(int column = 0; column < mapSize.width; column++){
            printf("%c",map[row][column]);
        }
        printf("\n");
    }
}

position findGuard(size mapSize, char** map){
    for(int row = 0; row < mapSize.height; row++){
        for(int column = 0; column < mapSize.width; column++){
            if(map[row][column] == '^'){
                return (position){row, column};
            }
        }
    }
}

vector up(){return (vector){0, -1};}
vector down(){return (vector){0, 1};}
vector left(){return (vector){-1, 0};}
vector right(){return (vector){1, 0};}

position move(position actor, vector velocity){
    return (position){actor.row + velocity.y, actor.column + velocity.x};
}

vector rotateRight90(vector direction){
    // 0 -1 -> 1 0
    // 1 0 -> 0 1
    // 0 1 -> -1 0
    // -1 0 -> 0 -1
    int x,y;
    if(direction.x == 0){
        x = -direction.y;
        y = 0;
    }else{
        x = 0;
        y = direction.x;
    }
    return (vector){x,y};
}

int withinBounds(size mapSize, position guard){
    return guard.row >= 0 && guard.row < mapSize.height && guard.column >= 0 && guard.column < mapSize.width;
}

char** allocateCharMap(size mapSize){
    char** map = (char**)malloc(mapSize.height * sizeof(char*));
    for(int row = 0; row < mapSize.height; row++){
        map[row] = (char*) malloc(mapSize.width * sizeof(char));
    }
    return map;
}

int** allocateIntMap(size mapSize){
    int** map = (int**)malloc(mapSize.height * sizeof(int*));
    for(int row = 0; row < mapSize.height; row++){
        map[row] = (int*) malloc(mapSize.width * sizeof(int));
    }
    return map;
}

int canMove(position actor, vector velocity, size mapSize, char** map){
    position newPosition = move(actor, velocity);
    return !withinBounds(mapSize, newPosition) || '#' != map[newPosition.row][newPosition.column];
}

int countOccurances(size mapSize, char** map, char target){
    int count = 0;
    for(int row = 0; row < mapSize.height; row++){
        for(int column = 0; column < mapSize.width;column++){
            if(map[row][column] == target){
                count++;
            }
        }
    }
    return count;
}

void main(){
    FILE* file = fopen("day6.txt", "r");
    
    size mapSize = getMapSize(file);
    printf("Map dimensions (h x w): %d x %d\n", mapSize.height, mapSize.width);
    
    
    char** map = allocateCharMap(mapSize);
    fillMap(mapSize, map, file);
    position start, guard; start = guard = findGuard(mapSize, map);
    vector direction = up(); // up
    while(withinBounds(mapSize, guard)){
        map[guard.row][guard.column] = 'X';
        if(!canMove(guard, direction, mapSize, map)){
            vector oldDirection = direction;
            direction = rotateRight90(direction);
            // printMap(mapSize, map);
            // printf("Rotating from %d,%d to %d,%d\n\n", oldDirection.x, oldDirection.y, direction.x, direction.y);
        }else{
            guard = move(guard, direction);
        }
    }
    // printMap(mapSize, map);
    
    int distinctLocations = countOccurances(mapSize, map, 'X');
    printf("Total visited locations: %d\n", distinctLocations);
    
    fillMap(mapSize, map, file);
    
    int possibilities = 0;
    char** mapCopy = allocateCharMap(mapSize);
    fillMap(mapSize, mapCopy, file);
    for(int row = 0; row < mapSize.height; row++){
        for(int column = 0; column < mapSize.width; column++){
            guard = start;
            direction = up();
            fillMap(mapSize, map, file);
            char orignialValue = map[row][column];
            map[row][column] = '#';
            while(withinBounds(mapSize, guard)){
                if(!canMove(guard, direction, mapSize, map)){
                    if(direction.y != 0){
                        if('+' == map[guard.row][guard.column] || '|' == map[guard.row][guard.column]){
                            mapCopy[guard.row][guard.column] = 'O';
                            possibilities++;
                            break;
                        }else if('-' == map[guard.row][guard.column]){
                            map[guard.row][guard.column] = '+';
                        }
                        else{
                            map[guard.row][guard.column] = '|';
                        }
                    }else{
                        if('+' == map[guard.row][guard.column] || '-' == map[guard.row][guard.column]){
                            mapCopy[row][column] = 'O';
                            possibilities++;
                            break;
                        }else if('|' == map[guard.row][guard.column])
                        {
                            map[guard.row][guard.column] = '+';
                        }else{
                            map[guard.row][guard.column] = '-';
                        }
                    }
                    direction = rotateRight90(direction);
                }else{
                    guard = move(guard, direction);
                }
            }
            map[row][column] = orignialValue;
        }
    }
    // printMap(mapSize, mapCopy);
    printf("Possible obstacle locations: %d\n\n", possibilities);
    
    free(map);
    fclose(file);
}
