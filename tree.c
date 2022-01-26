#include <stdio.h>
#include <stdlib.h>
#include <string.h>  

#define VERBOSE 0

struct Node {
    int val;
    int key;
    struct Node* parent;
    struct Node* left;
    struct Node* right;
};

struct Tree {
    struct Node* root;
};

struct Tree* construct() {
    struct Tree* tree = (struct Tree*)malloc(sizeof(struct Tree));
    tree->root = NULL;
}

void destruct(struct Tree* tree) {
    free(tree);
}

void put(struct Tree* t, int k, int v) {
    if (k < 0) 
    {
        if (VERBOSE) printf("%d: invalid key\n", k);
        return;
    }
    if (VERBOSE) printf("putting a node (%d) with value [%d]\n", k, v);
    struct Node* node = (struct Node*)malloc(sizeof(struct Node));
    node->left = NULL;
    node->right = NULL;
    node->parent = NULL;
    node->val = v;
    node->key = k;
    if (t->root == NULL) 
    {
        if (VERBOSE) printf("-> putting key (%d) as a root\n\n", k);
        t->root = node;
        return;
    }
    struct Node* walk = t->root;
    while (1) 
    {
        if (walk->key == k) 
        {
            if (VERBOSE) printf("-> same key found: replacing the value %d to the new value %d\n\n", walk->val, v);
            free(node);
            walk->val = v;
            return;
        }
        else if (walk->key > k) 
        {
            if (walk->left == NULL) 
            {
                if (VERBOSE) printf("-> putting key (%d) as a left child of parent (%d)\n", k, walk->key);
                walk->left = node;
                node->parent = walk;
                if (VERBOSE) printf("\n");
                break;
            }
            if (VERBOSE) printf("-> key (%d): move to left\n", k);
            walk = walk->left;
        }
        else 
        {
            if (walk->right == NULL) 
            {
                if (VERBOSE) printf("-> putting key (%d) as a right child of parent %d\n", k, walk->key);
                walk->right = node;
                node->parent = walk;
                if (VERBOSE) printf("\n");
                break;
            }
            if (VERBOSE) printf("-> key (%d): move to right\n", k);
            walk = walk->right;
        }
    }
    return;
}

struct Node* find(struct Node* n, int k) {
    struct Node* walk = n;
    while(1) {
        if (walk->key == k) 
        {
            return walk;
        }
        else 
        {
            if (walk->key > k) 
            {
                if (walk->left == NULL) return NULL;
                walk = walk->left;
            }
            else 
            {
                if (walk->right == NULL) return NULL;
                walk = walk->right;
            }
        }
    }
}

struct Node* succ(struct Node* n) {
    struct Node* r = n->right;
    if (r == NULL) return NULL;
    while(1) 
    {
        if (r->left == NULL) 
        {
            return r;
        }
        r = r->left;
    }
}

int pop(struct Node* n, int k) {
    if (VERBOSE) printf("remove node (%d)\n", k);
    if (n == NULL)
    {
        return -1;
    }
    struct Node* f = find(n, k);    // node <k> is located at f
    if (f == NULL) 
    {
        if (VERBOSE) printf("-> node (%d) not found\n\n", k);
        return -1;
    }
    struct Node* s = succ(f);       // successor of k;
    int v;
    // case 1: no successor, thus there is no right chidld to the node
    if (s == NULL)                  
    {
        if (VERBOSE) printf("no successor\n-> removing the node (%d)\n\n", k);
        v = f->val;
        // case 1-1 <f> is the left child
        if (f->parent->key > f->key) 
        {
            // case 1-1-1: <f> is the left child and it has no children
            if (f->left == NULL) 
            {
                f->parent->left = NULL;
                free(f);
                return v;
            }
            f->parent->left = f->left;
            f->left->parent = f->parent;
        }
        // case 1-2: <f> is the right child
        else                         
        {
            // case 1-2-1: <f> is the right child and it has no children
            if (f->left == NULL)
            {
                f->parent->right = NULL;
                free(f);
                return v;
            }
            f->parent->right = f->left;
            f->left->parent = f->parent;
        }
        free(f);
        return v;
    }
    else                            // there exists the successor!
    {
        // case 2: there is a successor, but successor has no child
        if (s->right == NULL)       
        {
            if (VERBOSE) printf("successor (%d) is a leaf\n-> replace the node (%d) to node (%d)\n\n", s->key, f->key, s->key);
            v = f->val;
            f->key = s->key;        // swap the key and the value
            f->val = s->val;
            if (s->key < s->parent->key)    // left
            {
                s->parent->left = NULL;
            }
            else 
            {
                s->parent->right = NULL;
            }
            free(s);                // free the succ node, as values are already passed to f
            return v;
        }
        else
        // case 3: successor has a right child                      
        {
            if (VERBOSE) printf("successor (%d) has a child\n-> connect the child of the succ (%d) to the parent of the succ (%d)\n", s->key, s->right->key, s->parent->key);
            struct Node* sparent = s->parent;
            s->right->parent = sparent;
            if (sparent->key > s->right->key)
            {
                sparent->left = s->right;
            }
            else 
            {
                sparent->right = s->right;
            } // connected the succ's child to succ's parent.
            if (VERBOSE) printf("-> replace the node (%d) to node (%d)\n\n", f->key, s->key);
            v = f->val;
            f->key = s->key;
            f->val = s->val;
            free(s);
            return v;
        }
    }
}

void traverse(struct Node* n) {
    if (n == NULL) {
        printf("empty tree ");
        return;
    }
    if (n->left != NULL) 
    {
        traverse(n->left);
    }
    printf("%d ", n->key, n->val);
    if (n->right != NULL)
    {
        traverse(n->right);
    }
    return;
}

int getVal(struct Node* n, int k) {
    struct Node* f = find(n, k);
    if (f == NULL) return 1<<31;
    else return f->val;
}

void setVal(struct Node* n, int k, int v) {
    struct Node* f = find(n, k);
    if (f == NULL) return;
    else f->val = v;
}

int getRoot(struct Tree* t) {
   if (t->root != NULL) return t->root->key;
   return -1; 
}

void line() {
    printf("--------------------------------------------------------------------\n");
}

int main() {
    line();
    printf("Binary Search Tree in C\n");
    struct Tree* tree = construct();
    int status = 1;
    while (status)
    {
        char cmd[20];
        int key = 0;
        printf(">> enter a command: ");
        scanf("%19s", cmd);
        if (!strcmp(cmd, "exit")) 
        {
            destruct(tree);
            status = 0;
        }
        else if (!strcmp(cmd, "put"))
        {
            printf(">> enter an integer to put: ");
            scanf("%d", &key);
            put(tree, key, key);
            printf(">> ");
            traverse(tree->root);
            printf(" \n");
        }
        else if (!strcmp(cmd, "pop"))
        {
            printf(">> enter the key to pop: ");
            scanf("%d", &key);
            pop(tree->root, key);
            printf(">> ");
            traverse(tree->root);
            printf(" \n");
        }
        else if (!strcmp(cmd, "root"))
        {
            if (tree->root == NULL) printf(">> the key of the root is (null)\n");
            else printf(">> the key of the root is %d\n", tree->root->key);
        }
        else if (!strcmp(cmd, "traverse") || !(strcmp(cmd, "trv")))
        {
            printf(">> traversing the tree\n");
            printf("[ ");
            traverse(tree->root);
            printf("]\n");
        }
        else if (!strcmp(cmd, "reset"))
        {
            printf(">> resetting the tree\n");
            destruct(tree);
            struct Tree* tree = construct();
        }
        else if (!strcmp(cmd, "--help"))
        {
            printf("--------------------------------------------------------------------\n");
            printf("    exit                - exit the tree editing\n");
            printf("    traverse            - inorder traversal of the tree\n");
            printf("    reset               - reset the list\n");
            printf("    root                - get the key of the root\n");
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
