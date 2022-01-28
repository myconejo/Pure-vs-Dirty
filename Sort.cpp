#include <stdio.h>
#include <stdlib.h>
#include <string.h>  

#define VERBOSE 0

/* array - array constructor with size */
int* array(int size) {
    int* arr = (int *)malloc(size * sizeof(int));
    for (int i = 0; i < size; i++) {
        // random integer generator
        *(arr+i) = i*2 - (size*i + 4)%13 + i%5 + (((int)arr+(int)(&i))>>4 + 7*i)%19;
    }
    return arr;
}

int* myarr(int size) {
    int* arr = (int *)malloc(size * sizeof(int));
    for (int i = 0; i < size; i++) {
        printf(">> enter the [%d]-th integer: ", i);
        scanf("%d", arr+i);
    }
    return arr;
}


int max(int arr[], int size) {
    if (VERBOSE) printf(">> array is at %p\n", arr);
    if (VERBOSE) printf("length of the given array is %d\n", size);
    int max = *arr;
    for (int i = 1; i < size; i++) {
        if (arr[i] > max) max = arr[i];
    }
    return max;
}

int min(int *arr, int size) {
    int min = *arr;
    for (int i = 1; i < size; i++) {
        if (*(arr+i) < min) min = *(arr+i);
    }
    return min;
}

/* traverse - traversing the list elements */
void traverse(int* arr, int size) {
    for (int i = 0; i < size; i++) {
        printf("%d ", *(arr+i));
    }
    printf("\n");
}

/* selSort - selection sort with time complexity O(n^2)  */
void selSort(int* arr, int size, int verb) {
    for (int i = 0; i < size; i++) {
        int min = *(arr+i);
        int tem = min;
        int ind = i;
        for (int j = i+1; j < size; j++) {
            if (*(arr+j) < min) {
                min = *(arr+j);
                ind = j;
            }
        }
        if (verb) printf("found minimum %d at index %d\n", min, ind);
        *(arr+i) = *(arr+ind);
        *(arr+ind) = tem;
        if (verb) traverse(arr, size);
    }
}

/* mergesort - function to sort the subsection a[i .. j] of the array a[] */
void mergesort(int lo, int hi, int* arr, int* aux) {
    if (hi <= lo) {
        return;    
    }
    int mid = (lo + hi) / 2;
    mergesort(lo, mid, arr, aux);          
    mergesort(mid + 1, hi, arr, aux);      
    int ptr_low = lo;               
    int ptr_high = mid + 1;        
    int k;                             
    for (k = lo; k <= hi; k++) {
        if (ptr_low == mid + 1) {            
            *(aux + k) = *(arr + ptr_high);
            ptr_high++;
        } else if (ptr_high == hi + 1) {        
            *(aux + k) = *(arr + ptr_low);
            ptr_low++;
        } else if (*(arr + ptr_low) < *(arr + ptr_high)) {
            *(aux + k) = *(arr + ptr_low);
            ptr_low++;
        } else {        
            *(aux + k) = *(arr + ptr_high);
            ptr_high++;
        }
    }
    for (k = lo; k <= hi; k++) {     
        *(arr + k) = *(aux + k);
    }
}

/* insSort - insertion sort with time complexity O(n^2) */
void insSort(int *arr, int size, int verb) {
    int temp = 0;
    for (int i = 1; i < size; i++) {
        for (int j = 0; j < i; j++) {
            if (*(arr+i) < *(arr+j)) {
                temp = *(arr+i);
                *(arr+i) = *(arr+j);
                *(arr+j) = temp;
            }
        }
        if (verb) traverse(arr, size);
    }
    if (verb) traverse(arr, size);
}

void line() {
    printf("+----------------------------------------------------------------------------------------+\n");
}

int main() {
    // array allocation
    int stat = 1;
    int verb = 0;
    int length = 0;
    line();
    printf(">> Sorting Algorithms in C++\n");
    int* arr = NULL;
    int* aux = NULL;
    while(stat) {
        printf(">> enter a command: ");
        char cmd[20];
        scanf("%19s", cmd);
        if (!strcmp(cmd, "exit")) {
            free(arr);
            stat = 0;
        }
        else if (!strcmp(cmd, "new")) {
            for (int i=0; i<length; i++) {
                free(arr + i);
            }
            printf(">> enter the size of the array: ");
            scanf("%d", &length);
            if (length < 1) printf("please enter an integer between 1~100\n");
            else {
                if (length > 100) length = 100;
                arr = array(length);
                printf(">> created an random array with random elements\n>> ");
                traverse(arr, length);
            }
        }
        else if (!strcmp(cmd, "make")) {
            for (int i=0; i<length; i++) {
                free(arr+i);
            }
            printf("enter the size of the array: ");
            scanf("%d", &length);
            if (length < 1) printf("please enter an integer between 1~30\n");
            else {
                if (length > 30) length = 30;
                arr = myarr(length);
                printf(">> created an array\n>> ");
                traverse(arr, length);
            }
        }
        else if (!strcmp(cmd, "sort") || !strcmp(cmd, "S")) {
            char condition[20];
            if (length < 1) {
                printf(">> create a new array using <new> before calling sort!\n");
            }
            else {
                printf(">> insertion[I]/selection[S]/merge[M]?: ");
                scanf("%19s", condition);
                if (!strcmp(condition, "selection") || !strcmp(condition, "S")) {
                    selSort(arr, length, verb);
                    printf(">> final result of selection sort:\n");
                    traverse(arr, length);
                }
                else if (!strcmp(condition, "insertion") || !strcmp(condition, "I")) {
                    insSort(arr, length, verb);
                    printf(">> final result of insertion sort:\n");
                    traverse(arr, length);
                }
                else if (!strcmp(condition, "merge") || !strcmp(condition, "M")) {
                    aux = (int *)malloc(length * sizeof(int));
                    for (int i = 0; i < length; i++) {
                    // aux has all zero
                        *(aux+i)=0;
                    }
                    mergesort(0, length-1, arr, aux);
                    for (int i = 0; i < length; i++) {
                        free(aux + i);
                    }
                    printf(">> final result of merge sort\n");
                    traverse(arr, length);
                } 
                else {
                    printf(">> invalid sort!\n");
                }
            }
            
        }
        else if (!strcmp(cmd, "traverse") || !strcmp(cmd, "T")) {
            if (length < 1) {
                printf(">> create a new array using <new> before calling traverse!\n");
            }
            else {
                printf(">> traversing the list: ");
                traverse(arr, length);
            }
        }
        else if (!strcmp(cmd, "reset")) {
            printf(">> resetting the list...\n");
            for (int i=0; i<length; i++) {
                free(arr + i);
            }
            length = 0;
            int* arr = NULL;
        }
        else if (!strcmp(cmd, "V") || !strcmp(cmd, "verbose")) {
            if (!verb) {
                printf(">> making it more verbose...\n");
                verb = 1;
            }
            else {
                printf(">> making it less verbose...\n");
                verb = 0;
            }
        }
        else if (!strcmp(cmd, "--help")) {
            line();
            printf("|   new      - create new list of random integers with length between 1~100              |\n");
            printf("|   make     - make your own list of random integers with length between 1~30            |\n");
            printf("|   reset    - reset the list                                                            |\n");
            printf("|   sort     - sort the current array (must create a new array before calling sort)        |\n");
            printf("|   verbose  - make program more/less verbose                                            |\n");
            printf("|   traverse - traverse the current array                                                |\n");
            line();
        }
        else {
            printf(">> invalid command <%s>\n", cmd);
            printf(">> use --help to see the list of commands\n");
        }
    }
    return 0;
}