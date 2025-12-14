#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct{
    int before;
    int after;
} rule;

typedef struct{
    int count;
    int* pages;
} update;

typedef struct{
    int rules;
    int updates;
} meta;

meta countrulesandupdates(FILE* file){
    const int linelength = 100;
    char line[linelength];
    
    int rules = 0, updates = 0;
    int section = 0;
    while(fgets(line, linelength, file)){
        if(line[0] == '\n')
        {
            section = 1;
            continue;
        }
        if(0 == section){
            rules++;
        }else{
            updates++;   
        }
    }
    
    rewind(file);
    return (meta){rules, updates};
}

void loaddata(FILE* file, rule* rules, update* updates)
{
    const int linelength = 100;
    char line[linelength];
    
    int isrule = 1;
    int index = 0;
    while(fgets(line, linelength, file)){
        if(line[0] == '\n')
        {
            isrule = 0;
            index = 0;
            continue;
        }
        if(isrule)
        {
            rules[index++] = (rule){atoi(line), atoi(line+3)};
            // printf("%d must come before %d\n", rules[index -1].before, rules[index -1].after);
        }else{
            int pages = 1;
            for(int i = 0; i < strlen(line); i++){
                if(line[i] == ',') {
                    pages++;
                }
            }
            int* pagenums = malloc(sizeof(int)* pages);
            pagenums[0] = atoi(line);
            int page = 1;
            for(int i = 0; i < strlen(line); i++){
                if(line[i] == ',') {
                    pagenums[page++] = atoi(line+i+1);
                }
            }
            // printf("updated pages: ");
            // for(int i = 0; i < pages; i++){
            //     printf("%d\t",pagenums[i]); 
            // }
            // printf("\n");
            updates[index++] = (update){pages,pagenums};
        }
    }
}

int verifybefore(meta m, rule* rules, update u, int index){
    int current = u.pages[index];
    
    for(int r = 0; r < m.rules; r++){
        if(rules[r].before == current){
            for(int i = 0; i < index; i++){
                if(rules[r].after == u.pages[i]){
                    return 0;
                }
            }
        }
    }
    
    return 1;
}

int findmiddleelement(update u){
    int middle = (u.count / 2);
    return u.pages[middle];
}

void order(meta m, rule* rules, update u){
    for(int i = 0; i < u.count; i++){
        for(int r = 0; r < m.rules; r++){
            if(rules[r].after == u.pages[i]){
                for(int o = u.count - 1; o >= 0; o--){
                    if(rules[r].before == u.pages[o] && i < o){
                        int temp = u.pages[i];
                        u.pages[i] = u.pages[o];
                        u.pages[o] = temp;
                    }
                }
            }
        }
    }
}

void main(){
    FILE* file = fopen("day5.txt", "r");
    
    meta m = countrulesandupdates(file);
    printf("Meta - rules: %d\tupdates: %d\n", m.rules, m.updates);
    
    rule rules[m.rules];
    update updates[m.updates];
    
    loaddata(file, rules, updates);
    
    for(int r = 0; r < m.rules; r++){
        printf("%d must come before %d\n", rules[r].before, rules[r].after);
    }
    
    int sum =0;
    int sumordered = 0;
    for(int u = 0; u < m.updates; u++){
        printf("updated pages: ");
        int allverified = 1;
        for(int i = 0; i < updates[u].count; i++){
            printf("%d\t",updates[u].pages[i] ); 
            if(!verifybefore(m, rules, updates[u], i)){
                allverified = 0;
                break;
            }
        }
        if(allverified){
            int middle = findmiddleelement(updates[u]); 
            printf("Middle: %d\n", middle);
            sum += middle;
        }else{
            int rulesort(const void* a, const void* b){ 
                for(int r = 0; r < m.rules; r++){
                    if(rules[r].before == *(int*)a && rules[r].after == *(int*)b){
                        return 1;
                    }
                }
                return -1;
            }
            qsort(updates[u].pages, updates[u].count, sizeof(int), rulesort);  
            sumordered += findmiddleelement(updates[u]);
        }
        
        printf("\n");
    }
    
    for(int u = 0; u < m.updates; u++){
        free(updates[u].pages);
    }
    
    printf("Sum: %d\n", sum);
    printf("Sum ordered: %d\n", sumordered);
    
    fclose(file);
}
