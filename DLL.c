#include <stdio.h>
#include <stdlib.h>
#include <string.h>  

struct Node {
    int val;
    struct Node* prev;
    struct Node* next;
};

struct List {
    int len;
    struct Node* head;
    struct Node* tail;
};

struct List* construct() {
    struct List* newList = (struct List*)malloc(sizeof(struct List));
    newList->len = 0;
    newList->head = NULL;
    newList->tail = NULL;
    return newList;
}

void destruct(struct List* list) {
    free(list);
}

void append(struct List* list, int v) {
    struct Node* node = (struct Node*)malloc(sizeof(struct Node));
    node->next = NULL;
    node->val = v;
    node->prev = list->tail;
    if (list->tail != NULL) {
        list->tail->next = node;
    }
    else list->head = node;
    list->tail = node;
    (list->len)++;
}

void enqueue(struct List* list, int v) {
    struct Node* node = (struct Node*)malloc(sizeof(struct Node));
    node->prev = NULL;
    node->val = v;
    node->next = list->head;
    if (list->head != NULL) {
        list->head->prev = node;
    }
    else list->tail = node;
    list->head = node;
    (list->len)++;
}

int pop(struct List* list) {
    if (list->head == NULL) return 1<<31;   // error code
    int v = list->tail->val;
    struct Node* ptail = list->tail->prev;
    free(list->tail);
    if (ptail != NULL) {
        list->tail = ptail;
        ptail->next = NULL;
    }
    else {
        list->head = NULL;
        list->tail = NULL;
    }
    list->len -= 1;
    return v;
}

int dequeue(struct List* list) {
    if (list->head == NULL) return 1<<31;
    int v = list->head->val;
    struct Node* nhead = list->head->next;
    free(list->head);
    if (nhead != NULL) {
        list->head = nhead;
        nhead->prev = NULL;
    }
    else {
        list->head = NULL;
        list->tail = NULL;
    }
    list->len -= 1;
    return v;
}

void replaceN(struct List* list, int index, int v) {
    if (index >= list->len) return;     // out of index
    else { 
        struct Node* walk = list->head;
        for (int i=0; i<index; i++) {
            walk = walk->next;
        }
        walk->val = v;
    }
}

void insertN(struct List* list, int index, int v) {
    if (index == 0) {
        enqueue(list, v);
        return;
    }
    if (index == (list->len-1)) {
        append(list, v);
        return;
    } 
    if (index >= list->len) return;     // out of index
    else {
        struct Node* walk = list->head;     
        struct Node* node = (struct Node*)malloc(sizeof(struct Node));
        node->val = v;
        for (int i=0; i<index; i++) {
            walk = walk->next;
        }
        walk->prev->next = node;
        node->next = walk;
        node->prev = walk->prev;
        walk->prev = node;
        list->len += 1;
    }
}

void deleteN (struct List* list, int index) {
    if (index == 0) {
        dequeue(list);
        return;
    }
    if (index == list->len-1) {
        pop(list);
        return;
    }
    if (index >= list->len) return;     // out of index
    else {
        struct Node* walk = list->head;
        for (int i=0; i<index; i++) {
            walk = walk->next;
        }
        walk->prev->next = walk->next;
        walk->next->prev = walk->prev;
        free(walk);
        list->len -= 1;
    }
}

void insSort(int *arr, int size) {
    int temp = 0;
    for (int i = 1; i < size; i++) {
        for (int j = 0; j < i; j++) {
            if (*(arr+i) < *(arr+j)) {
                temp = *(arr+i);
                *(arr+i) = *(arr+j);
                *(arr+j) = temp;
            }
        }
    }
}

void sort(struct List* list) {
    int l = list->len;
    int* array = (int*)malloc(l * sizeof(int));
    struct Node* walk = list->head;
    for (int i=0; i<l; i++) {
        *(array + i) = walk->val;
        walk = walk -> next;
    }
    insSort(array, l);
    struct Node* temp = list->head;
    for (int i=0; i<l; i++) {
        temp->val = *(array + i);
        temp = temp->next;
    }
    free(array);
}

void traverse(struct List* list) {
    struct Node* walk = list->head;
    while(walk != NULL) {
        printf("%d ", walk->val);
        walk = walk->next;
    }
    printf("\n");
}

int main() {
    struct List* list1 = construct();
    printf("--------------------------------------------------------------------\n");
    printf("Dynamic List in C\n");
    int status = 1;
    while (status) {
        char cmd[20];
        printf(">> enter a command: ");
        scanf("%19s", cmd);
        if (!strcmp(cmd, "exit")) {
            destruct(list1);
            status = 0;
        }
        else {
            if (!strcmp(cmd, "traverse")) {
                traverse(list1);
            }
            int val = 0;
            int ind = 0;
            int stt = 1;
            if (!strcmp(cmd, "append") || !strcmp(cmd, "A")) {
                printf(">> enter an integer to append: ");
                scanf("%d", &val);
                append(list1, val);
            }
            else if (!strcmp(cmd, "pop") || !strcmp(cmd, "P")) {
                printf(">> last element popped\n");
                pop(list1);
            }
            else if (!strcmp(cmd, "enqueue") || !strcmp(cmd, "E")) {
                printf(">> enter an integer to enqueue: ");
                scanf("%d", &val);
                enqueue(list1, val);
            }
            else if (!strcmp(cmd, "dequeue") || !strcmp(cmd, "D")) {
                printf(">> first element dequeued\n");
                dequeue(list1);
            }
            else if (!strcmp(cmd, "replace") || !strcmp(cmd, "R")) {
                printf(">> enter the index to replace an element: ");
                scanf("%d", &ind);
                printf(">> enter the integer to replace at postion [%d]: ", ind);
                scanf("%d", &val);
                replaceN(list1, ind, val);
            }
            else if (!strcmp(cmd, "insert") || !strcmp(cmd, "I")) {
                printf(">> enter the index to insert an element: ");
                scanf("%d", &ind);
                printf(">> enter the integer to insert at postion [%d]: ", ind);
                scanf("%d", &val);
                insertN(list1, ind, val);
            }
            else if (!strcmp(cmd, "delete") || !strcmp(cmd, "X")) {
                printf(">> enter the index to delete an element: ");
                scanf("%d", &ind);
                deleteN(list1, ind);
            }
            else if (!strcmp(cmd, "sort") || !strcmp(cmd, "S")) {
                printf(">> sorting the list...\n");
                sort(list1);
            }
            else if (!strcmp(cmd, "reset")) {
                printf(">> resetting the list...\n");
                free(list1);
                struct List* list1 = construct();
            }
            else if (!strcmp(cmd, "--help")) {
                printf("--------------------------------------------------------------------\n");
                printf("    exit                - exit the list editing\n");
                printf("    traverse            - traverse the current list\n");
                printf("    reset               - reset the current list to an empty list\n");
                printf("    append  [A]         - add an integer at the last of the list\n");
                printf("    pop     [P]         - delete the last element of the list\n");
                printf("    enqueue [E]         - add an integer at the first of the list\n");
                printf("    dequeue [D]         - delete the first element of the list\n");
                printf("    replace [R]         - replace an element in the list\n");
                printf("    insert  [I]         - insert an element at the desired index\n");
                printf("    delete  [X]         - delete an element at the desired index\n");
                printf("    sort    [S]         - sorts the list in ascending order\n");
                printf("--------------------------------------------------------------------\n");
                stt = 0;
            }
            else {
                stt = 0;
                printf(">> no such command <%s>\n", cmd);
                printf(">> use --help to see the list of the commands\n");
            }
            if (stt) traverse(list1);
        }
    }
    printf("--------------------------------------------------------------------\n");
    return 0;
}
