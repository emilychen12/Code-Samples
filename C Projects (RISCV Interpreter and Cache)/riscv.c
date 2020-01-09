#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "linkedlist.h"
#include "hashtable.h"
#include "riscv.h"

/************** BEGIN HELPER FUNCTIONS PROVIDED FOR CONVENIENCE ***************/
const int R_TYPE = 0;
const int I_TYPE = 1;
const int MEM_TYPE = 2;
const int U_TYPE = 3;
const int UNKNOWN_TYPE = 4;

/**
 * Return the type of instruction for the given operation
 * Available options are R_TYPE, I_TYPE, MEM_TYPE, UNKNOWN_TYPE
 */
static int get_op_type(char *op)
{
    const char *r_type_op[] = {"add", "sub", "and", "or", "xor", "nor", "slt", "sll", "sra"};
    const char *i_type_op[] = {"addi", "andi", "ori", "xori", "slti"};
    const char *mem_type_op[] = {"lw", "lb", "sw", "sb"};
    const char *u_type_op[] = {"lui"};
    for (int i = 0; i < (int)(sizeof(r_type_op) / sizeof(char *)); i++)
    {
        if (strcmp(r_type_op[i], op) == 0)
        {
            return R_TYPE;
        }
    }
    for (int i = 0; i < (int)(sizeof(i_type_op) / sizeof(char *)); i++)
    {
        if (strcmp(i_type_op[i], op) == 0)
        {
            return I_TYPE;
        }
    }
    for (int i = 0; i < (int)(sizeof(mem_type_op) / sizeof(char *)); i++)
    {
        if (strcmp(mem_type_op[i], op) == 0)
        {
            return MEM_TYPE;
        }
    }
    for (int i = 0; i < (int)(sizeof(u_type_op) / sizeof(char *)); i++)
    {
        if (strcmp(u_type_op[i], op) == 0)
        {
            return U_TYPE;
        }
    }
    return UNKNOWN_TYPE;
}
/*************** END HELPER FUNCTIONS PROVIDED FOR CONVENIENCE ****************/

registers_t *registers;
// TODO: create any additional variables to store the state of the interpreter
hashtable_t *memory;

void init(registers_t *starting_registers)
{
    registers = starting_registers;
    // TODO: initialize any additional variables needed for state
    //initialize memory
    memory = ht_init(512);
}

// TODO: create any necessary helper functions

//Removes spaces from a string
char* strip(char *string) {
    int count = 0;
    char *new_str = string;
    for(int i = 0; string[i]; i++) {
        if(string[i] != ' ') {
            new_str[count++] = string[i];
        }
    }
    new_str[count] = '\0';
    return new_str;
}

//Returns the integer value from a register string i.e. ("x4" -> 4)
int get_reg_idx(char **string, const char *sep) {
    if ((string != NULL) && (*string != NULL)) {
        char *inp = (sep != NULL) ? strsep(string,sep) : *string;
        if ((inp != NULL) && (inp[0] == 'x')) {
            return atoi(++inp);
        }
    }
    return -1;
}

//Returns the imm; converts imm to decimal if in hex.
int get_imm(char **string, const char *sep) {
    if ((string != NULL) && (*string != NULL)) {
        char *inp = (sep != NULL) ? strsep(string,sep) : *string;
        if (inp != NULL) {
            if ((inp[0] == '0') && (inp[1] == 'x')) {
                return (int) strtol(&inp[2], NULL, 16);
            }
            return atoi(inp);
        }
    }
    return 0;
}


void step(char *instruction) {
    char *next_ins;
    // Extracts and returns the substring before the first space character,
    // by replacing the space character with a null-terminator.
    // `instruction` now points to the next character after the space
    // See `man strsep` for how this library function works
    instruction = strdup(instruction);
    char *op = strsep(&instruction, " ");
    // Uses the provided helper function to determine the type of instruction
    int op_type = get_op_type(op);
    // Skip this instruction if it is not in our supported set of instructions
    if (op_type == UNKNOWN_TYPE)
    {
        return;
    }
    
    //strip the rest of the instruction
    next_ins = strip(instruction);
    int arr0 = get_reg_idx(&next_ins,",");
    if(arr0 < 0) {
        return;
    }
    
    if(op_type == R_TYPE) {
        int arr1 = get_reg_idx(&next_ins,",");
        int arr2 = get_reg_idx(&next_ins,NULL);
        if((arr1 < 0) || (arr2 < 0)) {
            return;
        }
        int r1_val = registers->r[arr1];
        int r2_val = registers->r[arr2];
        
        if(strcmp(op,"add") == 0) {
            registers->r[arr0] = r1_val + r2_val;   
        }

        else if(strcmp(op,"sub") == 0) {
            registers->r[arr0] = r1_val - r2_val;
        }

        else if(strcmp(op,"and") == 0) {
            registers->r[arr0] = r1_val & r2_val;
        }

        else if(strcmp(op,"or") == 0) {
            registers->r[arr0] = r1_val | r2_val;
        }

        else if(strcmp(op,"xor") == 0) {
            registers->r[arr0] = r1_val ^ r2_val; 
        }

        else if(strcmp(op,"nor") == 0) {
            registers->r[arr0] = ~(r1_val | r2_val);
        }

        else if(strcmp(op,"slt") == 0) {
            if(r1_val < r2_val) {
                registers->r[arr0] = 1;
            } else {
                registers->r[arr0] = 0;
            }   
        }

        else if(strcmp(op,"sll") == 0) {
            registers->r[arr0] = r1_val << (31 & r2_val);
        }

        else if(strcmp(op,"sra") == 0) {
            registers->r[arr0] = r1_val >> (31 & r2_val);
        }
    }

    if(op_type == I_TYPE) {
        int arr1 = get_reg_idx(&next_ins,",");
        if(arr1 < 0) {
            return;
        }
        int r1_val = registers->r[arr1];
        int r2_val = get_imm(&next_ins,NULL);
        
        if(strcmp(op,"addi") == 0) {
            registers->r[arr0] = r1_val + r2_val;
        }

        else if(strcmp(op,"andi") == 0) {
            registers->r[arr0] = r1_val & r2_val;
        }

        else if(strcmp(op,"ori") == 0) {
            registers->r[arr0] = r1_val | r2_val;
        }

        else if(strcmp(op,"xori") == 0) {
            registers->r[arr0] = r1_val ^ r2_val;
        }

        else if(strcmp(op,"slti") == 0) {
            if(r1_val < r2_val) {
                registers->r[arr0] = 1;
            } else {
                registers->r[arr0] = 0;
            }
        }
    }

    if(op_type == MEM_TYPE) {
        int r1_ind = arr0;
        int offset = get_imm(&next_ins,"(");
        int r2_ind = get_reg_idx(&next_ins,")");
        if(r2_ind < 0) {
		    return;
	    }
        int mem_loc = registers->r[r2_ind] + offset;
        
        if(strcmp(op,"lw") == 0) {
            int word = 0;
            word += ht_get(memory, mem_loc); 
            word += ht_get(memory, mem_loc + 1) << 8; 
            word += ht_get(memory, mem_loc + 2) << 16; 
            word += ht_get(memory, mem_loc + 3) << 24; 
            registers->r[r1_ind] = word; 	      	
        }
        
        else if(strcmp(op,"lb") == 0) {
            int low8 = ht_get(memory, mem_loc);
            int cur = registers->r[r1_ind];
		    registers->r[r1_ind] = (cur ^ (cur & 255)) + low8; 
        }
        
        else if(strcmp(op,"sw") == 0) {
        	int word = registers->r[r1_ind];
            ht_add(memory, mem_loc, word & 255);
            ht_add(memory, mem_loc + 1, (word >> 8) & 255);
            ht_add(memory, mem_loc + 2, (word >> 16) & 255);
            ht_add(memory, mem_loc + 3, (word >> 24) & 255);
        }
        
        else if(strcmp(op,"sb") == 0) {
        	ht_add(memory, mem_loc, registers->r[r2_ind] & 255);
        }
        return;
    }

    if(op_type == U_TYPE) {
        int rs1 = get_imm(&next_ins,NULL);
        if(strcmp(op,"lui") == 0) {
            registers->r[arr0] = rs1 << 12;
        }
        
    }
}
