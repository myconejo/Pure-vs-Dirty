#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VERBOSE 1
#define VERBOSE_2 1

/*------------------------------------------HASH CODES------------------------------------------*/
int hashcode(int key, int size) {
    if (size <= 1) return 0;
    return key%size;
}

/*----------------------------------SEPARATE CHAINING NAMESPACE---------------------------------*/
namespace sepChain {
    
    /*----------------------------------------NODE CLASS----------------------------------------*/
    class Node {
        int val;
        int key;
        Node* next;
        Node* prev;
    public:
        Node(int k, int v);
        void setVal(int v);
        void setNext(Node* n);
        void setPrev(Node* p);
        int getKey();
        int getVal();
        Node* getNext();
        Node* getPrev();
    };

    // default constructor
    Node::Node(int k, int v) {
        this->key = k;
        this->val = v;
        this->next = NULL;
        this->prev = NULL;
    }

    void Node::setVal(int v) {
        this->val = v;
    }

    void Node::setNext(Node* n) {
        this->next = n;
    }

    void Node::setPrev(Node* p) {
        this->prev  = p;
    }

    int Node::getKey() {
        return this->key;
    }

    int Node::getVal() {
        return this->val;
    }

    Node* Node::getNext() {
        return this->next;
    }

    Node* Node::getPrev() {
        return this->prev;
    }

    /*----------------------------------------LIST CLASS----------------------------------------*/
    class List {
        Node* head;
        Node* tail;
        int size;
    public:
        List();
        void addLast(int k, int v);
        void addFirst(int k, int v);
        int delLast();
        int delFirst();
        Node* findKey(int key);
        void delKey(int key);
        void traverse();
        int getSize();
    };

    List::List() {
        this->head = NULL;
        this->tail = NULL;
        this->size = 0;
    }

    void List::addFirst(int k, int v) {
        //Node *node = new Node(v, NULL, this->tail);
        Node* newNode = new Node(k ,v);
        newNode->setNext(this->head);
        if (this->head != NULL) head->setPrev(newNode);
        else this->tail = newNode;
        this->head = newNode;
        this->size++;
    }

    void List::addLast(int k, int v) {
        Node* newNode = new Node(k, v);
        newNode->setPrev(this->tail);
        if (this->head != NULL) tail->setNext(newNode);
        else this->head = newNode;
        this->tail = newNode;
        this->size++;
        newNode = tail;
    }

    int List::delFirst() {
        if (this->head == NULL) return 1<<31;
        int v = this->head->getVal();
        if (this->head->getNext() != NULL) {
            this->head = this->head->getNext();
            this->head->setPrev(NULL);
        }
        else {
            this->head = NULL;
            this->tail = NULL;
        }
        this->size--;
        return v;
    }

    int List::delLast() {
        if (this->head == NULL) return 1<<31;
        int v = this->tail->getVal();
        if (this->tail->getPrev() != NULL) {
            this->tail = this->tail->getPrev();
            this->tail->setNext(NULL);
        }
        else {
            this->head = NULL;
            this->tail = NULL;
        }
        this->size--;
        return v;
    }

    Node* List::findKey(int key) {
        Node* walk = this->head;
        while(walk->getKey() != key) {
            if (walk == this->tail) break;
            walk = walk -> getNext();
        }
        if (walk->getKey() != key) return NULL;
        return walk;
    }

    void List::delKey(int key) {
        Node* found = this->findKey(key);
        if (found == NULL) {
            return;
        }
        if (found->getNext() != NULL) {
            found->getNext()->setPrev(found->getPrev());
        }
        else {
            this->tail = found->getPrev();
            this->tail->setNext(NULL);
        }
        if (found->getPrev() != NULL) {
            found->getPrev()->setNext(found->getNext());
        }
        else {
            this->head = found->getNext();
            this->head->setPrev(NULL);
        }
        delete found;
        this->size--;
        return;
    }

    void List::traverse() {
        int s = this->size;
        Node* walk = this->head;
        if (walk == NULL) printf("empty!");
        for (int i=0; i<s; i++) {
            printf("%d", walk->getKey());
            printf(":(%d)", walk->getVal());
            if (i < s-1) {
                printf(" >> ");
            }
            walk = walk->getNext();
        }
    }

    int List::getSize() {
        return this->size;
    }
    /*--------------------------------------END LIST CLASS---------------------------------------*/

    /*----------------------------------------ARRAY CLASS----------------------------------------*/
    class Array {
        int size;
    public:
        List *arr;
        Array(int size);
        void put(int key, int val);
        int pop(int key);
        int search(int key);
        void trv();
        void reset();
        ~Array();
    };

    Array::Array(int s) {
        this->size = s;
        arr = new List[s];
        for (int i = 0; i<s; i++) {
            *(arr+i) = List();
        }
    }

    void Array::put(int key, int val) {
        int s = this->size;
        int index = hashcode(key, s);
        this->arr[index].addFirst(key, val);
    }

    int Array::pop(int key) {
        int s = this->size;
        int index = hashcode(key, s);
        this->arr[index].delKey(key);
    }

    int Array::search(int key) {
        int index = hashcode(key, this->size);
        Node* found = this->arr[index].findKey(key);
        if (found == NULL) return 1<<31;
        else return found->getVal();
    }

    void Array::trv() {
        int s = this->size;
        for (int i = 0; i<s; i++) {
            printf("Arr[%d] - [", i);
            (this->arr[i]).traverse();
            printf("]\n");
        }
    }

    void Array::reset() {
        if (VERBOSE) printf(">> resetting the array!\n");
        for (int i=0; i<this->size; i++) {
            if (VERBOSE) printf("------------ loop %d go -------------\n", i);
            if (VERBOSE) this->arr[i].traverse();
            if (VERBOSE) printf("\n");
            while(this->arr[i].getSize() != 0) {
                this->arr[i].delLast();
                if (VERBOSE) this->arr[i].traverse();
                if (VERBOSE) printf("\n");
            }
            if (VERBOSE) printf("------------ loop %d end ------------\n", i);
        }
        if (VERBOSE) printf(">> reset done!\n");
    }

    Array::~Array() {
        if (VERBOSE) printf(">> destructing the array!\n");
        this->reset();
        delete[] arr;
    }
    /*-----------------------------------END ARRAY CLASS----------------------------------------*/
}
/*---------------------------------END SEPARATE CHAINING NAMESPACE------------------------------*/

/*-------------------------------------DOUBLE HASH NAMESPACE------------------------------------*/
namespace doubleHash {
    
    /*----------------------------------------PAIR CLASS----------------------------------------*/
    class Pair {
    public:
        int key;
        int val;
        Pair();
        Pair(int k, int v);
        void setKey(int k);
        void setVal(int v);
    };

    Pair::Pair(){
        this->key = 0;
        this->val = 0;
    }

    Pair::Pair(int k, int v) {
        this->key = k;
        this->val = v;
    }

    void Pair::setKey(int k) {
        this->key = k;
    }

    void Pair::setVal(int v) {
        this->key = v;
    }
    /*---------------------------------------END PAIR CLASS----------------------------------------*/

    /*----------------------------------------DYNAMIC CLASS----------------------------------------*/
    class Dynamic {
        int size;
        int weight;
        Pair** list;
    public:
        Dynamic(int s);
        void put(int k, int v);
        int pop(int k);
        void extend();
        void diminish();
        int search(int k);
        void traverse();
        void reset();
        ~Dynamic();
    };

    Dynamic::Dynamic(int s) {
        this->weight = 0;
        this->size = s;
        list = new Pair*[s];
        for (int i=0; i<s; i++) {
            list[i] = NULL;
        }
    }

    void Dynamic::put(int k, int v) 
    {
        int trial = 0;
        int h;
        while(1) 
        {
            h = (hashcode(k, this->size) + trial*hashcode(k, (h/2 + 7)))%(this->size);
            if (VERBOSE_2) printf(">> try index %d ", h);
            if (this->list[h] == NULL) 
            {
                if (VERBOSE_2) printf("$ added a Pair(%d, %d) at index %d\n", k ,v, h);
                list[h] = new Pair(k, v);
                this->weight++;
                break;
            }
            else 
            {
                if (VERBOSE_2) printf("$ index %d is occupied\n", h);
                if (list[h]->key == k) {
                    if (VERBOSE_2) printf("$ no duplicate keys allowed\n\n");
                    return;
                }
                trial++;
                if (trial == 4*this->size) {
                    if (VERBOSE_2) printf("$ failed to put Pair(%d, %d)\n\n", k, v);
                    return;
                }
            }
        }
        if (VERBOSE_2) printf(">> checking the weight\n  $ current size = %d\n  $ current weight = %d\n\n", this->size, this->weight);
        if (this->weight > (this->size)/2) this->extend();
        return;
    }
    
    int Dynamic::pop(int k) {
        int trial = 0;
        int h;
        int v = 1<<31;
        while(1) {
            h = (hashcode(k, this->size) + trial*hashcode(k, (h/2 + 7)))%(this->size);
            if (VERBOSE_2) printf(">> try index %d ", h);
            if (this->list[h] != NULL) {
                if (this->list[h]->key == k) {
                    if (VERBOSE_2) printf("$ found the key %d at index %d\n",k , h);
                    v = this->list[h]->val;
                    delete this->list[h];
                    this->list[h] = NULL;
                    this->weight--;
                    break;
                }
                else {
                    if (VERBOSE_2) printf("$ key does not match...\n");
                    trial++;
                }
            }
            else {
                if (VERBOSE_2) printf("\n  $ no such key!\n\n");
                return v;
            }
        }
        if (VERBOSE_2) printf(">> checking the weight\n  $ current size = %d\n  $ current weight = %d\n\n", this->size, this->weight);
        if (weight <= this->size/4) {
            if (this->size > 7) {
                this->diminish();
            }
        }
        return v;
    }

    void Dynamic::extend() {
        if (VERBOSE_2) printf("+--------------------------------------+\n");
        printf(">> extending the list...\n");
        int s = this->size;
        int k;
        int v;
        Pair** newList = new Pair*[s];
        this->weight = 0;
        for (int i=0; i<s; i++) {
            newList[i] = this->list[i];
        }
        delete[] this->list;
        this->list = new Pair*[2*s];
        for (int i=0; i<2*s; i++) {
            this->list[i] = NULL;
        }
        this->size = 2*s;
        for (int i=0; i<s; i++) {
            if (newList[i] != NULL) {
                k = newList[i]->key;
                v = newList[i]->val;
                this->put(k, v);
            }
        }
        delete[] newList;
        if (VERBOSE_2) printf(">> extension finished\n");
        if (VERBOSE_2) this->traverse();
        if (VERBOSE_2) printf("+--------------------------------------+\n\n");
    }

    void Dynamic::diminish() {
        if (VERBOSE_2) printf("+--------------------------------------+\n");
        printf(">> diminishing the list...\n");

        int s = this->size;
        int k;
        int v;
        Pair** newList = new Pair*[s];
        this->weight = 0;
        for (int i=0; i<s; i++) {
            newList[i] = this->list[i];
        } 
        delete[] this->list;
        this->list = new Pair*[s/2];
        for (int i=0; i<s/2; i++) {
            this->list[i] = NULL;
        }
        this->size = s/2;
        for (int i=0; i<s; i++) {
            if (newList[i] != NULL) {
                k = newList[i]->key;
                v = newList[i]->val;
                this->put(k, v);
            }
        }
        delete[] newList;

        if (VERBOSE_2) printf(">> diminish finished\n");
        if (VERBOSE_2) this->traverse();
        if (VERBOSE_2) printf("+--------------------------------------+\n\n");
    }

    void Dynamic::traverse() {
        for (int i=0; i<this->size; i++) {
            if (this->list[i] != NULL) printf("[%d:%d] ", this->list[i]->key, this->list[i]->val);
            else printf("[_:_] ");
        }
        printf("\n");
    }

    int Dynamic::search(int k) {
        if (VERBOSE_2) printf(">> searching the key %d\n", k);
        int trial = 0;
        int h;
        int v = 1<<31;
        while(1) {
            h = (hashcode(k, this->size) + trial*hashcode(k, (h/2 + 7)))%(this->size);
            if (VERBOSE_2) printf(">> try index %d ", h);
            if (this->list[h] != NULL) {
                if (this->list[h]->key == k) {
                    if (VERBOSE_2) printf("$ found the key %d at index %d\n",k , h);
                    v = this->list[h]->val;
                    printf("  $ key %d's value is %d\n\n", k, v);
                    return v;
                }
                else {
                    if (VERBOSE_2) printf("$ key does not match...\n");
                    trial++;
                }
            }
            else {
                if (VERBOSE_2) printf("\n  $ no such key!\n\n");
                return v;
            }
        }
    }

    void Dynamic::reset() {
        if (VERBOSE_2) printf(">> resetting the list\n");
        for (int i=0; i<this->size; i++) {
            if (this->list[i] != NULL) {
                delete this->list[i];
                this->list[i] = NULL;
            }
        }
    }

    Dynamic::~Dynamic() {
        this->reset();
        delete[] this->list;
    }
    /*----------------------------------END DYNAMIC CLASS--------------------------------------*/
}
/*-----------------------------------END DOUBLE HASH NAMESPACE---------------------------------*/

int main() 
{
    using namespace sepChain;
    using namespace doubleHash;
    return 0;
}
