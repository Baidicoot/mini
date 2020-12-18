#include<stdlib.h>

struct arena {
    void* allocation;
    size_t allocation_size;
    void* allocation_ptr;
    struct arena* next;
};

struct arena* alloc_arena(size_t size);
void* alloc_in_arena(struct arena* arena, size_t size);
void free_arena(struct arena* arena);

void gc_check_lim(struct arena* arena, size_t size);

void* reg_gpr[100];
void* reg_arith;

struct arena* heap;

struct arena* alloc_arena(size_t size) {
    //printf("\nallocated arena of size %i\n", size);
    struct arena* arena = malloc(size);
    arena->allocation = (void*)(arena)+sizeof(struct arena);
    arena->allocation_size = size-sizeof(struct arena);
    arena->allocation_ptr = arena->allocation;
    arena->next = NULL;
    return arena;
}

void free_arena(struct arena* arena) {
    if (arena->next != NULL) {
        free_arena(arena->next);
    }
    free(arena);
}

void* alloc_in_arena(struct arena* arena, size_t size) {
    while (arena->allocation_size-(size_t)(arena->allocation_ptr-arena->allocation) < size) {
        if (arena->next == NULL) {
            struct arena* next = alloc_arena((arena->allocation_size+sizeof(struct arena))*2);
            arena->next = next;
            arena = next;
        } else {
            arena = arena->next;
        }
    }
    void* loc = arena->allocation_ptr;
    arena->allocation_ptr += size;
    return loc;
}