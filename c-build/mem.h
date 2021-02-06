#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>

#define NREGS 100

#define MIN_SIZE 2048
#define MAJ_SIZE 2048

#define UNTAG(val) ((uintptr_t)val - 1) >> 1
#define TAG(i) ((i << 1) | 1)

uintptr_t reg_gpr[NREGS];
uintptr_t reg_arith;