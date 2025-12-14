#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

const int contentLength = 100000;

int comp(const void* a, const void* b){
    return (*(int*)a - *(int*)b);
}

void sort(int* array, int length){
    qsort(array, length, sizeof(int), comp);  
}


int main(){
    
    FILE* file = fopen("day1.txt", "r");

    char content[contentLength];
    
    int leftNumbers[contentLength];
    int rightNumbers[contentLength];
    
    int i = 0;
    while(fgets(content, contentLength, file)!= NULL){
        leftNumbers[i] = atoi(content);
        rightNumbers[i++] = atoi(content+5);
    }
    
    sort(leftNumbers, i);
    sort(rightNumbers, i);
    
    int sum = 0;
    int similarity = 0;
    for(int j = 0; j < i; j++){
        int distance = abs(leftNumbers[j] - rightNumbers[j]);
        
        for(int k = 0; k < i; k++){
            if(leftNumbers[j] == rightNumbers[k]){
                similarity+= leftNumbers[j];
            }
        }
        sum += distance;
    }
    printf("Part 1\n");
    printf("Sum: %d\n", sum);
    printf("Part 2\n");
    printf("Similarity: %d\n", similarity);

    fclose(file);
}

