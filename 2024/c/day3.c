#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct{
    int a;
    int b;
    int enabled;
    int nextStart;
} mulmatch;

int lengthOfMatch(regmatch_t match){
    return match.rm_eo - match.rm_so;
}

void getGroup(regmatch_t match, char* source, char* destination){
    strncpy(destination, source + match.rm_so, lengthOfMatch(match));
    destination[lengthOfMatch(match)] = '\0';
}

mulmatch nextMatch(regex_t* exp, char* string){
    int groups = exp->re_nsub + 1;
    regmatch_t matches[groups];
    int match = regexec(exp, string, groups, matches, 0); 
    if(match == 0){
        if(lengthOfMatch(matches[0]) == 4){
            return (mulmatch){-1, -1, 1, matches[0].rm_eo};
        }if(lengthOfMatch(matches[0]) == 7){
            return (mulmatch){-1, -1, 0, matches[0].rm_eo};
        }
        char number[4];
        getGroup(matches[2], string, number);
        int a = atoi(number);
        getGroup(matches[3], string, number);
        int b = atoi(number);
        return (mulmatch){a, b, -1, matches[0].rm_eo};
    }
    return (mulmatch){-1, -1, -1, strlen(string)};
}

void main(){
    FILE* file = fopen("day3.txt", "r");
    regex_t exp;
    int value = regcomp(&exp, "(mul\\(([[:digit:]]{1,3}),([[:digit:]]{1,3})\\)|do\\(\\)|don't\\(\\))", REG_EXTENDED);
    if(value != 0){
        printf("Error compiling regex\n");
        return;
    }
    
    const int lineLength = 10000;
    char line[lineLength];
    int acc = 0;
    int enabledAcc = 0;
    int mulEnabled = 1;
    mulmatch match;
    while(fgets(line, lineLength, file) != NULL){
        int offset = 0;
        while(offset < strlen(line)){
            match = nextMatch(&exp, line + offset);
            if(match.a != -1){
                acc += match.a * match.b;
                if(mulEnabled){
                    enabledAcc += match.a * match.b;
                }
            }
            if(match.enabled != -1){
                mulEnabled = match.enabled;
            }
            
            offset += match.nextStart;
        }
    }
    
    printf("Acc: %d\n", acc);
    printf("Enabled acc: %d\n", enabledAcc);
    
    regfree(&exp);
    
    fclose(file);
}
