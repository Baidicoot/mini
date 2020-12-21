#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>

#define NREGS 100

#define MIN_SIZE 2048
#define MAJ_SIZE 2048

#define UNTAG(val) ((uintptr_t)val - 1) >> 1
#define TAG(i) ((i << 1) | 1)

struct arena {
    uintptr_t major;
    size_t major_size;
    uintptr_t major_ptr;
    uintptr_t minor;
    size_t minor_size;
    uintptr_t minor_ptr;
    struct arena* next;
};

struct arena* alloc_arena(size_t maj_size, size_t min_size);
uintptr_t alloc_in_arena(struct arena** arena, size_t size);
void free_arena(struct arena* arena);

uintptr_t reg_gpr[NREGS];
uintptr_t reg_arith;

struct arena* heap;

struct arena* alloc_arena(size_t maj_size, size_t min_size) {
    struct arena* arena = malloc(sizeof(struct arena)+maj_size+min_size);
    arena->major = ((uintptr_t)arena)+sizeof(struct arena);
    arena->minor = ((uintptr_t)arena)+sizeof(struct arena);
    arena->major_ptr = arena->major;
    arena->minor_ptr = arena->minor;
    arena->major_size = maj_size;
    arena->minor_size = min_size;
    arena->next = NULL;
    return arena;
}

void free_arena(struct arena* arena) {
    if (arena->next != NULL) {
        free_arena(arena->next);
    }
    free(arena);
}

#define IS_PTR(val) !((uintptr_t)val & 0x1)

#define COPY_RECORD(root,next) \
uintptr_t* pnext = (uintptr_t*)next; \
for (int j = 0; j < TO_INT(root[0]); j++) { \
    *((uintptr_t*)next++) = root[j]; \
} \
root = pnext;

#define FORWARD(root,src_start,src_end,dst_start,dst_end,next) \
if (root >= src_start && root < src_end) { \
    if (IS_PTR(root[0]) && (root[0] < dst_start || root[0] >= dst_end)) { \
        COPY_RECORD(root[0],next); \
    } \
}
/*
uintptr_t* alloc_in_arena(struct arena** arena, size_t size) {
    if (size > (*arena)->minor_size-((*arena)->minor_ptr-(*arena)->minor)) {
        // if there is not enough space, trigger a minor collection
        uintptr_t* scan = (*arena)->major_ptr;
        uintptr_t* next = scan;
        for (int i = 0; i < NREGS; i++) {
            FORWARD(((uintptr_t**)reg_gpr[i]),
                (*arena)->minor,(*arena)->minor+(*arena)->minor_size,
                (*arena)->major, (*arena)->major+(*arena)->major_size,(*arena)->major_ptr)
        }
        while (scan < next) {
            if (IS_PTR(*scan)) {
                FORWARD(((uintptr_t**)*scan),
                    (*arena)->minor,(*arena)->minor+(*arena)->minor_size,
                    (*arena)->major, (*arena)->major+(*arena)->major_size,(*arena)->major_ptr)
            }
        }
        (*arena)->minor_ptr = (*arena)->minor;
    }
    uintptr_t* loc = (*arena)->minor_ptr;
    (*arena)->minor_ptr += size;
    return loc;
}
*/
/*
uintptr_t* alloc_in_arena(struct arena** arena, size_t size) {
    return malloc(size);
}
*/

uintptr_t alloc_in_arena(struct arena** arena, size_t size) {
    if ((*arena)->minor_size-((*arena)->minor_ptr-(*arena)->minor) < size) {
        struct arena* next = alloc_arena(MAJ_SIZE,MIN_SIZE);
        next->next = *arena;
        *arena = next;
    }
    uintptr_t loc = (*arena)->minor_ptr;
    (*arena)->minor_ptr += size;
    return loc;
}
