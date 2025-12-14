#include <stdio.h>
#include <stdlib.h>

typedef struct{
    int columns;
    int rows;
} size;

typedef struct{
    int x;
    int y;
} vector;

size getsize(FILE* file){
    const int linebufferlength = 1000;
    char line[linebufferlength];
    
    int columns = 0;
    int rows = 0;
    while(fgets(line, linebufferlength, file)){
        if(columns == 0){
            while(line[columns] != '\n'){
                columns++;
            }
        }
        rows++;
    }
    rewind(file);
    return (size){columns, rows};
}

void loaddata(FILE* file, char** data, size s){
    const int linebufferlength = 1000;
    char line[linebufferlength];
    int row = 0;
    
    while(fgets(line, linebufferlength, file)){
        for(int column = 0; column < s.columns; column++){
            data[row][column] = line[column];
        }
        row++;
    }
}

vector add(vector a, vector b){
    return (vector){a.x + b.x, a.y + b.y};
}

int isOutOfBounds(size s, vector position){
    return position.x < 0 || position.x >= s.columns || position.y < 0 || position.y >= s.rows;
}

int tryFind(size s, char** data, vector position,char target){
    if(isOutOfBounds(s, position)){
        return 0;
    }
    return target == data[position.y][position.x];
}

const vector right = (vector){1, 0};
const vector left = (vector){-1, 0};
const vector down = (vector){0, 1};
const vector up = (vector){0, -1};

char* dirstrings[8] = {"right", "downright", "down", "downleft", "left", "upleft", "up", "upright"};
int tryfindxmas(char** data, int row, int column, size s){
    char targets[3] = { 'M', 'A', 'S'};
    
    vector directions[8] = {right, add(down,right), down, add(down,left), left, add(up,left), up, add(up,right)};
    int xmases = 0;
    for(int dir = 0; dir < 8 ; dir ++){
        int fullmatch = 1;
        vector position = (vector){column, row};
        for(int t = 0; t < 3; t++){
            position = add(position, directions[dir]);
            if(!tryFind(s, data, position, targets[t])){
                fullmatch = 0;
                break;
            }
        }
        if(fullmatch){
            // printf("Found xmas %s from [%d,%d]\n", dirstrings[dir], row, column);
        }
        xmases += fullmatch;
    }
    
    return xmases;
}

char** allocateData(size s){
    char** data = malloc(s.rows * sizeof(char*));
    for(int row = 0; row < s.rows; row++){
        data[row] = malloc(s.columns * sizeof(char));
    }
    return data;
}

void main(){
    FILE* file = fopen("day4.txt", "r");
    size s = getsize(file);
    
    char** data = allocateData(s);
    loaddata(file, data, s);
    
    int sumxmas = 0;
    
    for(int row = 0; row < s.rows; row++){
        for(int column = 0; column < s.columns; column++){
            
            if(data[row][column] == 'X'){
                int result = tryfindxmas(data, row, column, s);
                sumxmas += result;
            }
        }
    }
    int xes = 0;
    for(int row = 0; row < s.rows; row++){
        for(int column = 0; column < s.columns; column++){
            vector position = (vector){column, row};
            if(data[row][column]== 'A'){
                vector topRightPos = add(position, add(up,right));
                vector botLeftPos = add(position, add(down,left));
                int diag1 = 0;
                int diag2 = 0;
                if(!isOutOfBounds(s,topRightPos) && !isOutOfBounds(s, botLeftPos))
                {
                    char topRight = data[topRightPos.y][topRightPos.x];
                    char botLeft = data[botLeftPos.y][botLeftPos.x];
                    diag1 = ('M' == botLeft && 'S' == topRight)||('M' == topRight && 'S' == botLeft);
                }
                vector topLeftPos = add(position, add(up,left));
                vector botRightPos = add(position, add(down,right));
                if(!isOutOfBounds(s, topLeftPos) && !isOutOfBounds(s, botRightPos)){
                    char topLeft = data[topLeftPos.y][topLeftPos.x];
                    char botRight = data[botRightPos.y][botRightPos.x];
                    diag2 = ('M' == botRight && 'S' == topLeft)||('M' == topLeft && 'S' == botRight);
                }
                if(diag1 && diag2){
                    xes++;
                }
            }
        }
    }
    
    printf("XMAS count: %d\n", sumxmas);
    printf("X-MASes: %d\n", xes);
    fclose(file);
}
