#include <stdio.h>
#include <stdlib.h>

int words(char* line, int* buffer){
    buffer[0] = 0;
    int count = 1;
    int i = 0;
    while( *(line + i ) != '\0'){
        if(*(line + i) == ' '){
            buffer[count++] = i;
        }
        i++;
    }
    return count;
}

int isDecreasingSafe(int a, int b){
    return a > b && a - b <= 3;
}

int isIncreasingSafe(int a, int b){
    return a < b && b - a <= 3;
}

void main(){
    FILE* file = fopen("day2.txt", "r");
    char line[100];
    int safe = 0;
    while(fgets(line, 100, file) != NULL){
        int s[100];
        int nums = words(line, s);
        int canIncrease = 1;
        int canDecrease = 1;
        for(int i = 0; i < nums - 1; i++){
            int value = atoi(line + s[i]);
            int next = atoi(line + s[i + 1]);
            if(canIncrease && !isIncreasingSafe(value, next)){
                canIncrease = 0;
            }
            if(canDecrease && !isDecreasingSafe(value, next)){
                canDecrease = 0;
            }
        }
        if(canIncrease || canDecrease){
            safe++;
        }
        else{
            for(int skip = 0; skip < nums; skip++)
            {
                canIncrease = 1;
                canDecrease = 1;
                for(int i = 0; i < nums - 1; i++){
                    int firstIndex = i;
                    if(skip == i) { 
                        firstIndex++; 
                    }
                    int secondIndex = firstIndex + 1;
                    if(firstIndex + 1 == skip){ 
                        secondIndex++; 
                    }
                    if(secondIndex >= nums){
                        break;
                    }
                    int value = atoi(line + s[firstIndex]);
                    int next = atoi(line + s[secondIndex]);
                    if(canIncrease && !isIncreasingSafe(value, next)){
                        canIncrease = 0;
                    }
                    if(canDecrease && !isDecreasingSafe(value, next)){
                        canDecrease = 0;
                    }
                }    
                if(canIncrease || canDecrease){
                    // printf("%s : Safe when skipping %d(%d)\n", line, atoi(line + s[skip]), skip);
                    safe++;
                    break;
                }
            }
            if(!(canIncrease || canDecrease)){
                printf("%s : Not safe\n", line);
            }
        }
    }
    printf("Safe: %d\n", safe);
    fclose(file);
}
