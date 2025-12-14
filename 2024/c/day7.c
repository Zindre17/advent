#include <stdio.h>
#include <stdlib.h>

typedef struct{
    long long result;
    int start;
    int ops;
    int* items;
} test;

int indexOf(char* line, char target){
    int i = 0;
    while(line[i++] != target){}
    return i;
}

int count(char* line, char target) {
    int count = 0;
    int i =0;
    while(line[i] != '\n'){
        if(target == line[i++]){
            count++;
        }
    }
    return count;
}

test parseTest(char* line){
    long long result = atoll(line);
    line = line + indexOf(line, ':') + 1;
    int start = atoi(line);
    int elements = count(line, ' ');
    int* items = malloc(elements * sizeof(int));
    for(int i = 0; i < elements; i++){
        line = line + indexOf(line, ' ');
        items[i] = atoi(line);
    }
    
    // printf("%lld - %d (%d)\n", result, start, elements);
    // for(int i = 0; i < elements; i++){
    //     printf("\t%d", items[i]);
    // }
    // printf("\n");
    return (test){result, start, elements, items};
}

int testPermutations(test test){
    // printf("Ops: %d -> Permutations: %d\n", test.ops, (2 << test.ops));
    for(int permutation = 0; permutation < (2 << test.ops); permutation++){
        long long current = test.start;
        for(int position = 0; position < test.ops; position++){
            int multiplication = (permutation >> position) & 1;
            if(multiplication){
                current *= test.items[position];
            }else{
                current += test.items[position];
            }
        }
        
        if(current == test.result){
            // printf("%lld Test OK: %d\n", test.result, permutation);
            return 1;
        }
    }
    return 0;
}

long long concat(long long a, int b){
    int c = b;
    while(c){
        a *= 10;
        c /= 10;
    }
    return a + b;
}

int testPermutations2(test test, int index, long long current){
    if(index == test.ops){
        return current == test.result? 1 : 0;
    }
    if(testPermutations2(test, index + 1, current + test.items[index])){
        return 1;
    }else if(testPermutations2(test, index + 1, current * test.items[index])){
        return 1;
    }else{
        return testPermutations2(test, index + 1, concat(current, test.items[index]));
    }
}

void main(){
    FILE* file = fopen("day7.txt", "r");
    int maxLength = 10000;
    char line[maxLength];
    long long calibrationResult = 0;
    long long newCalibrationResult = 0;
    while(fgets(line, maxLength, file)){
        test test = parseTest(line);
        if(testPermutations(test)){
            calibrationResult += test.result;
        }
        if(testPermutations2(test, 0, test.start)){
            newCalibrationResult += test.result;
        }
    }
    printf("Total calibration result: %lld\n", calibrationResult);
    printf("Total calibration result: %lld\n", newCalibrationResult);
}
