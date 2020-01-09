#include <stdio.h>
#include <stdlib.h>
#include "linkedlist.h"

struct linkedlist
{
    struct linkedlist_node *first;
    // TODO: define linked list metadata
    
};

struct linkedlist_node
{
    // TODO: define the linked list node
    
    int key;
    int value;
    struct linkedlist_node *next;
    
};
typedef struct linkedlist_node linkedlist_node_t;

linkedlist_t *ll_init()
{
    // TODO: create a new linked list

    // We have done this TODO for you as an example of how to use malloc().
    // You might want to read more about malloc() from Linux Manual page.
    // Usually free() should be used together with malloc(). For example, 
    // the linkedlist you are currently implementing usually have free() in the
    // ll_delete() function. Since we are not asking you to implement
    // the ll_delete() function, you will not be using free() in this case.

    // First, you use C's sizeof function to determine
    // the size of the linkedlist_t data type in bytes.
    // Then, you use malloc to set aside that amount of space in memory.
    // malloc returns a void pointer to the memory you just allocated.
    // We want a linkedlist_t pointer, however, so we cast it. Done!
    linkedlist_t *list = (linkedlist_t *)malloc(sizeof(linkedlist_t));

    // TODO: set metadata for your new list and return the new list
    
    return list;
    
}

void ll_add(linkedlist_t *list, int key, int value)
{
    // TODO: create a new node and add to the front of the linked list if a
    // node with the key does not already exist.
    // Otherwise, replace the existing value with the new value.
    
    struct linkedlist_node *current_node = list->first;
    struct linkedlist_node **current_node_ptr = &list ->first;
    
    //check if a key already exists
    while (current_node != NULL) {
        if (current_node->key == key) {
                current_node->value = value;
                return;
            }
        current_node_ptr = &current_node->next;
        current_node = current_node->next;
    }
    
    //add a new node to the front of the linked list
    struct linkedlist_node *new_node;
    new_node = (struct linkedlist_node *) malloc(sizeof(struct linkedlist_node));
    new_node->key = key;
    new_node->value = value;
    new_node->next = NULL;
    *current_node_ptr = new_node;
    
}

int ll_get(linkedlist_t *list, int key)
{
    // TODO: go through each node in the linked list and return the value of
    // the node with a matching key.
    // If it does not exist, return 0.
    
    struct linkedlist_node *current_node = list->first;
    
    while (current_node != NULL) {
        if (current_node->key == key) {
                return current_node->value;
            }
        current_node = current_node->next;
    }
    return 0;
}

int ll_size(linkedlist_t *list)
{
    // TODO: return the number of nodes in this linked list
    int size = 0;
    struct linkedlist_node *current_node = list->first;
    
    while (current_node != NULL) {
        size++;
        current_node = current_node->next;
    }
    return size;
}
