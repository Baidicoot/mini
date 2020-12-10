char* reg_gpr[100];
char* reg_arith;
char* data_ptr;
char* data_lim;
int main() {
goto start;
/* split_k1 c4:r1 c3:r2 */
split_k1: ;
printf("entered split_k1\n");
reg_gpr[0] = reg_gpr[1];
goto k1;
/* k1 v2:r0 */
k1: ;
printf("entered k1\n");
goto k0;
/* k0 Comb.mod:r0 */
k0: ;
printf("entered k0\n");
return 0;
/* start */
start: ;
printf("entered start\n");
/* let c5 = {<split_k1>} */
reg_gpr[0] = malloc(sizeof(void*)*1);
((void**)(reg_gpr[0]))[0] = &&split_k1;
printf("allocated 1 in r0\n");
reg_gpr[2] = reg_gpr[0];
reg_gpr[1] = 0;
goto Comb_main;
/* split_k43 c126:r1 c125:r2 */
Comb_split_k43: ;
printf("entered split_k43\n");
/* let c127 = c125[1] */
reg_gpr[0] = ((void**)(reg_gpr[2]))[1];
goto Comb_k43;
/* split_k41 c120:r1 c119:r2 */
Comb_split_k41: ;
printf("entered split_k41\n");
/* let c121 = c119[1] */
reg_gpr[0] = ((void**)(reg_gpr[2]))[1];
goto Comb_k41;
/* split_f46 c107:r1 c108:r2 c106:r3 */
Comb_split_f46: ;
printf("entered split_f46\n");
/* let c109 = c106[1] */
reg_gpr[0] = ((void**)(reg_gpr[3]))[1];
reg_arith = reg_gpr[1];
reg_gpr[1] = reg_gpr[2];
reg_gpr[2] = reg_arith;
goto Comb_f46;
/* split_k_6 c112:r1 c113:r2 c111:r3 */
Comb_split_k_6: ;
printf("entered split_k_6\n");
reg_gpr[0] = reg_gpr[2];
goto Comb_k_6;
/* split_k57 c79:r1 c78:r2 */
Comb_split_k57: ;
printf("entered split_k57\n");
/* let c80 = c78[1] */
reg_gpr[0] = ((void**)(reg_gpr[2]))[1];
/* let c81 = c78[2] */
reg_gpr[2] = ((void**)(reg_gpr[2]))[2];
reg_arith = reg_gpr[1];
reg_gpr[1] = reg_gpr[0];
reg_gpr[0] = reg_gpr[2];
reg_gpr[2] = reg_arith;
goto Comb_k57;
/* split_k55 c84:r1 c83:r2 */
Comb_split_k55: ;
printf("entered split_k55\n");
/* let c85 = c83[1] */
reg_gpr[0] = ((void**)(reg_gpr[2]))[1];
/* let c86 = c83[2] */
reg_gpr[3] = ((void**)(reg_gpr[2]))[2];
/* let c87 = c83[3] */
reg_gpr[2] = ((void**)(reg_gpr[2]))[3];
reg_arith = reg_gpr[1];
reg_gpr[1] = reg_gpr[3];
reg_gpr[3] = reg_arith;
goto Comb_k55;
/* split_k53 c68:r1 c67:r2 */
Comb_split_k53: ;
printf("entered split_k53\n");
/* let c69 = c67[1] */
reg_gpr[0] = ((void**)(reg_gpr[2]))[1];
goto Comb_k53;
/* split_f51 c90:r1 c91:r2 c89:r3 */
Comb_split_f51: ;
printf("entered split_f51\n");
/* let c92 = c89[1] */
reg_gpr[0] = ((void**)(reg_gpr[3]))[1];
/* let c93 = c89[2] */
reg_gpr[3] = ((void**)(reg_gpr[3]))[2];
reg_arith = reg_gpr[1];
reg_gpr[1] = reg_gpr[0];
reg_gpr[0] = reg_gpr[3];
reg_gpr[3] = reg_arith;
goto Comb_f51;
/* split_f49 c96:r1 c97:r2 c95:r3 */
Comb_split_f49: ;
printf("entered split_f49\n");
/* let c98 = c95[1] */
reg_gpr[0] = ((void**)(reg_gpr[3]))[1];
reg_arith = reg_gpr[1];
reg_gpr[1] = reg_gpr[2];
reg_gpr[2] = reg_arith;
goto Comb_f49;
/* split_s_3 c101:r1 c102:r2 c100:r3 */
Comb_split_s_3: ;
printf("entered split_s_3\n");
reg_gpr[0] = reg_gpr[2];
goto Comb_s_3;
/* k43 v44:r1 c122:r0 */
Comb_k43: ;
printf("entered k43\n");
/* let c123 = v44[0] */
reg_gpr[2] = ((void**)(reg_gpr[1]))[0];
/* let c124 = {<split_k41>,c122} */
reg_gpr[3] = malloc(sizeof(void*)*2);
((void**)(reg_gpr[3]))[0] = &&Comb_split_k41;
((void**)(reg_gpr[3]))[1] = reg_gpr[0];
printf("allocated 2 in r3\n");
reg_gpr[0] = reg_gpr[2];
reg_gpr[2] = reg_gpr[3];
reg_gpr[3] = reg_gpr[1];
reg_gpr[1] = 1;
/* c123 #1 c124 v44 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* k41 v42:r1 c118:r0 */
Comb_k41: ;
printf("entered k41\n");
goto Comb_k39;
/* k_6 a_4:r1 k45:r0 */
Comb_k_6: ;
printf("entered k_6\n");
/* let c103 = k45[0] */
reg_gpr[2] = ((void**)(reg_gpr[0]))[0];
/* let c110 = {<split_f46>,a_4} */
reg_gpr[3] = malloc(sizeof(void*)*2);
((void**)(reg_gpr[3]))[0] = &&Comb_split_f46;
((void**)(reg_gpr[3]))[1] = reg_gpr[1];
printf("allocated 2 in r3\n");
reg_gpr[1] = reg_gpr[3];
reg_arith = reg_gpr[2];
reg_gpr[2] = reg_gpr[0];
reg_gpr[0] = reg_arith;
/* c103 c110 k45 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* f46 b_5:r2 k47:r1 c104:r0 */
Comb_f46: ;
printf("entered f46\n");
/* let c105 = k47[0] */
reg_gpr[2] = ((void**)(reg_gpr[1]))[0];
reg_arith = reg_gpr[2];
reg_gpr[2] = reg_gpr[1];
reg_gpr[1] = reg_gpr[0];
reg_gpr[0] = reg_arith;
/* c105 c104 k47 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* s_3 x_0:r1 k48:r0 */
Comb_s_3: ;
printf("entered s_3\n");
/* let c59 = k48[0] */
reg_gpr[2] = ((void**)(reg_gpr[0]))[0];
/* let c99 = {<split_f49>,x_0} */
reg_gpr[3] = malloc(sizeof(void*)*2);
((void**)(reg_gpr[3]))[0] = &&Comb_split_f49;
((void**)(reg_gpr[3]))[1] = reg_gpr[1];
printf("allocated 2 in r3\n");
reg_gpr[1] = reg_gpr[3];
reg_arith = reg_gpr[2];
reg_gpr[2] = reg_gpr[0];
reg_gpr[0] = reg_arith;
/* c59 c99 k48 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* f49 y_1:r2 k50:r1 c60:r0 */
Comb_f49: ;
printf("entered f49\n");
/* let c61 = k50[0] */
reg_gpr[3] = ((void**)(reg_gpr[1]))[0];
/* let c94 = {<split_f51>,c60,y_1} */
reg_gpr[4] = malloc(sizeof(void*)*3);
((void**)(reg_gpr[4]))[0] = &&Comb_split_f51;
((void**)(reg_gpr[4]))[1] = reg_gpr[0];
((void**)(reg_gpr[4]))[2] = reg_gpr[2];
printf("allocated 3 in r4\n");
reg_gpr[0] = reg_gpr[3];
reg_gpr[2] = reg_gpr[1];
reg_gpr[1] = reg_gpr[4];
/* c61 c94 k50 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* f51 z_2:r3 k52:r2 c62:r1 c63:r0 */
Comb_f51: ;
printf("entered f51\n");
/* let c64 = c62[0] */
reg_gpr[4] = ((void**)(reg_gpr[1]))[0];
/* let c88 = {<split_k55>,k52,c63,z_2} */
reg_gpr[5] = malloc(sizeof(void*)*4);
((void**)(reg_gpr[5]))[0] = &&Comb_split_k55;
((void**)(reg_gpr[5]))[1] = reg_gpr[2];
((void**)(reg_gpr[5]))[2] = reg_gpr[0];
((void**)(reg_gpr[5]))[3] = reg_gpr[3];
printf("allocated 4 in r5\n");
reg_gpr[0] = reg_gpr[4];
reg_gpr[2] = reg_gpr[5];
reg_arith = reg_gpr[3];
reg_gpr[3] = reg_gpr[1];
reg_gpr[1] = reg_arith;
/* c64 z_2 c88 c62 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* k55 v56:r3 c70:r2 c71:r1 c72:r0 */
Comb_k55: ;
printf("entered k55\n");
/* let c73 = c71[0] */
reg_gpr[4] = ((void**)(reg_gpr[1]))[0];
/* let c82 = {<split_k57>,c70,v56} */
reg_gpr[5] = malloc(sizeof(void*)*3);
((void**)(reg_gpr[5]))[0] = &&Comb_split_k57;
((void**)(reg_gpr[5]))[1] = reg_gpr[2];
((void**)(reg_gpr[5]))[2] = reg_gpr[3];
printf("allocated 3 in r5\n");
reg_gpr[2] = reg_gpr[5];
reg_gpr[3] = reg_gpr[1];
reg_gpr[1] = reg_gpr[0];
reg_gpr[0] = reg_gpr[4];
/* c73 c72 c82 c71 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* k57 v58:r2 c74:r1 c75:r0 */
Comb_k57: ;
printf("entered k57\n");
/* let c76 = c75[0] */
reg_gpr[3] = ((void**)(reg_gpr[0]))[0];
/* let c77 = {<split_k53>,c74} */
reg_gpr[4] = malloc(sizeof(void*)*2);
((void**)(reg_gpr[4]))[0] = &&Comb_split_k53;
((void**)(reg_gpr[4]))[1] = reg_gpr[1];
printf("allocated 2 in r4\n");
reg_gpr[1] = reg_gpr[2];
reg_gpr[2] = reg_gpr[4];
reg_arith = reg_gpr[3];
reg_gpr[3] = reg_gpr[0];
reg_gpr[0] = reg_arith;
/* c76 v58 c77 c75 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* k53 v54:r1 c65:r0 */
Comb_k53: ;
printf("entered k53\n");
/* let c66 = c65[0] */
reg_gpr[2] = ((void**)(reg_gpr[0]))[0];
reg_arith = reg_gpr[2];
reg_gpr[2] = reg_gpr[0];
reg_gpr[0] = reg_arith;
/* c66 v54 c65 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* k39 x_7:r1 c114:r0 */
Comb_k39: ;
printf("entered k39\n");
/* let c115 = c114[0] */
reg_gpr[2] = ((void**)(reg_gpr[0]))[0];
/* let c116 = {<split_k_6>} */
reg_gpr[3] = malloc(sizeof(void*)*1);
((void**)(reg_gpr[3]))[0] = &&Comb_split_k_6;
printf("allocated 1 in r3\n");
/* let c117 = {<split_s_3>} */
reg_gpr[4] = malloc(sizeof(void*)*1);
((void**)(reg_gpr[4]))[0] = &&Comb_split_s_3;
printf("allocated 1 in r4\n");
/* let v40 = {x_7,c116,c117} */
reg_gpr[5] = malloc(sizeof(void*)*3);
((void**)(reg_gpr[5]))[0] = reg_gpr[1];
((void**)(reg_gpr[5]))[1] = reg_gpr[3];
((void**)(reg_gpr[5]))[2] = reg_gpr[4];
printf("allocated 3 in r5\n");
reg_gpr[1] = reg_gpr[5];
reg_arith = reg_gpr[2];
reg_gpr[2] = reg_gpr[0];
reg_gpr[0] = reg_arith;
/* c115 v40 c114 */
reg_arith = reg_gpr[0];
goto *reg_arith;
/* Comb.main i37:r1 k38:r2 */
Comb_main: ;
printf("entered Comb.main\n");
/* let c128 = {<split_k43>,k38} */
reg_gpr[0] = malloc(sizeof(void*)*2);
((void**)(reg_gpr[0]))[0] = &&Comb_split_k43;
((void**)(reg_gpr[0]))[1] = reg_gpr[2];
printf("allocated 2 in r0\n");
reg_gpr[1] = 0;
goto Comb_k_6;
}
