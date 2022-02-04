#include <stdio.h>
#include <stdlib.h>
#include <string.h>  

#define VERBOSE 1

namespace tree {

    class Node {
        int key;
        int value;
        Node* left;
        Node* right;
    public:
        Node(int k, int v);
        int getVal();
        int getKey();
        Node* getLeft();
        Node* getRight();
        void put(int k, int v);
        Node* min();
        Node* pop(int k);
        void traverse();
        void reset();
    };

    Node::Node(int k, int v) {
        this->key = k;
        this->value = v;
        this->left = NULL;
        this->right = NULL;
    }

    int Node::getVal() {
        return this->value;
    }

    int Node::getKey() {
        return this->key;
    }

    Node* Node::getLeft() {
        return this->left;
    }

    Node* Node::getRight() {
        return this->right;
    }

    void Node::put(int k, int v) {
        if (k == this->key) {
            this->value = v;
            return;
        }
        if (k < this->key) {
            if (this->left == NULL) {
                this->left = new Node(k, v);
                return;
            }
            this->left->put(k, v);
        }
        else {
            if (this->right == NULL) {
                this->right = new Node(k, v);
                return;
            }
            this->right->put(k, v);
        }
    }

    Node* Node::min() {
        if (this == NULL) return NULL;
        if (this->left == NULL) return this;
        else this->left->min();
    }

    /* 
     * pop(k) - remove the node with key = k
     * Example) (1-3-4)-6-(7-8-9)
     *       6
     *    3     8
     *   1 4   7 9
     * 
     * root = root->pop(8)  
     *
     * this[6].pop(8)
     *  (k = 8 > this[6]->key) => this[6]->right = this[6]->right->pop(8) = (2) (== [8]->pop(8))
     * 
     *      (k = 8 == this[8]->key)
     *      (this[8]->left  != NULL)
     *      (this[8]->right != NULL)     
     *      tem1            = [8]->right (== [9])
     *      this[8]->key    = [9]->key (== 9)
     *      this[9']->right = this[9']->right->pop(tem1->right (== 9)) = (1)
     * 
     *          (k = 9 == [9]-> key)
     *          ([9]->left  == NULL)
     *          tem2    = [9]->right = NULL
     *          delete [9]
     *          return NULL (1)
     * 
     *      this[9']->right = (1) = NULL
     *      return this[9'] = (2)
     *  
     *  this[6]->right = (2) = [9']
     *  return this[6]
     * 
     * root = [6]
     * 
     * Result: 
     *       6
     *    3     9'
     *   1 4   7 -
     */
    Node* Node::pop(int k) {
        if (this == NULL) return this;
        if (k < this->key) this->left = this->left->pop(k); 
        else if (k > this->key) this->right = this->right->pop(k);
        else {
            if (this->left == NULL) {
                Node* temp = this->right;
                delete this;
                return temp;
            }
            else if (this->right == NULL) {
                Node* temp = this->left;
                delete this;
                return temp;
            }
            else {
                Node* temp = this->right->min();        // successor
                this->key = temp->key;
                this->right = this->right->pop(temp->key);
            }
        }
        return this;
    }

    void Node::traverse() {
        if (this == NULL) {
            printf(" empty tree ");
            return;
        }
        if (this->left != NULL) {
            printf("(");
            this->left->traverse();
            printf(")-");
        }
        printf("%d", this->key);
        if (this->right != NULL) {
            printf("-(");
            this->right->traverse();
            printf(")");
        }
    }

    void Node::reset() {
        if (this == NULL) {
            delete this;
        }
        if (this->left != NULL) {
            this->left->reset();
        }
        if (this->right != NULL) {
            this->right->reset();
        }
        delete this;
    }
}

void line() {
    printf("--------------------------------------------------------------------\n");
}

int main() {
    line();
    printf("Binary Search Tree in C++\n");
    using namespace tree;
    Node* root;
    int status = 1;
    int init = 1;
    while (status) {
        char cmd[20];
        int key = 0;
        printf(">> enter a command: ");
        scanf("%19s", cmd);
        if (!strcmp(cmd, "exit")) 
        {
            root->reset();
            delete root;
            status = 0;
        }
        else if (!strcmp(cmd, "put")) {
            printf(">> enter an integer to put: ");
            scanf("%d", &key);
            if (init == 1) {
                root = new Node(key, key);
                init = 0;
            }
            else
            {
                root->put(key, key);
            }
            printf(">> ");
            root->traverse();
            printf(" \n");
        }
        else if (!strcmp(cmd, "pop")) {
            printf(">> enter the key to pop: ");
            scanf("%d", &key);
            root = root->pop(key);
            printf(">> ");
            root->traverse();
            printf(" \n");
        }
        else if (!strcmp(cmd, "traverse") || !(strcmp(cmd, "trv")))
        {
            printf(">> traversing the tree\n");
            printf("[");
            root->traverse();
            printf("]\n");
        }
        else if (!strcmp(cmd, "reset"))
        {
            printf(">> resetting the tree\n");
            root->reset();
            root = NULL;
            init = 1;
        }
        else if (!strcmp(cmd, "--help"))
        {
            printf("--------------------------------------------------------------------\n");
            printf("    exit                - exit the tree editing\n");
            printf("    traverse            - inorder traversal of the tree\n");
            printf("    reset               - reset the list\n");
            printf("    put                 - put a key into a tree\n");
            printf("    pop                 - pop a given key from the tree\n");
            printf("--------------------------------------------------------------------\n");
        }
        else
        {
            printf(">> no such command <%s>\n", cmd);
            printf(">> use --help to see the list of commands\n");
        }
    }
    return 0;
}
