#include <stdio.h>
#include <stdlib.h>
#include "linkedlist.h"
#include "hashtable.h"
struct hashtable {
    // TODO: define hashtable struct to use linkedlist as buckets
    struct linkedlist **buckets;
	int num_buckets;
};

/**
 * Hash function to hash a key into the range [0, max_range)
 */
static int hash(int key, int max_range) {
    // TODO: feel free to write your own hash function (NOT REQUIRED)
    key = (key > 0) ? key : -key;
    return key % max_range;
}

hashtable_t *ht_init(int num_buckets) {
    // TODO: create a new hashtable
	hashtable_t *table = (hashtable_t *) malloc(sizeof(hashtable_t));
	table->buckets = (linkedlist_t **) malloc(sizeof(linkedlist_t*) * num_buckets);
	table->num_buckets = num_buckets;
	for(int i = 0; i < num_buckets; i++) {
		table->buckets[i] = NULL;
	}	
	return table;
}

void ht_add(hashtable_t *table, int key, int value) {
    // TODO: create a new mapping from key -> value.
    // If the key already exists, replace the value.
	if(table != NULL) {
		int k = hash(key, sizeof(table));
		struct linkedlist* ll = table->buckets[k];
		if(ll == NULL) {
			table->buckets[k] = ll_init();
			ll = table->buckets[k];
		}
		ll_add(ll,key,value);	
	}
}

int ht_get(hashtable_t *table, int key) {
    // TODO: retrieve the value mapped to the given key.
    // If it does not exist, return 0
	if(table != NULL) {
		int k = hash(key, sizeof(table));
		linkedlist_t* ll = table->buckets[k];
		if(ll != NULL) { 
			return ll_get(ll,key);
		}
	}
	return 0;
}

int ht_size(hashtable_t *table) {
    // TODO: return the number of mappings in this hashtable
	int size = 0;
	if(table != NULL) {
		for(int i = 0; i < table->num_buckets; i++) {
			linkedlist_t* ll = table->buckets[i];
			if(ll != NULL) {
				size += ll_size(ll);
			}
		}
	}
	return size;
}
