/*
File name : [buffer.h]
Purpose : Declare functions and structure of the buffer for more organization in h file.
Function list : b_allocate();
b_addc();
b_free();
b_isfull();
b_limit();
b_capacity();
b_mark();
b_mode();
b_incfactor();
b_load();
b_isempty();
b_getc();
b_eob();
b_print();
b_compact();
b_rflag();
b_retract();
b_reset();
b_getcoffset();
b_rewind();
b_location();*/

#pragma once
/*
* File Name: buffer.h
* Version: 1.01.0
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

							/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

														   /* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

														   /* constant definitions */
#define RT_FAIL_1 -1         /* fail return value */
#define RT_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 128       /* default increment factor */

														   /* You should add your own constant definitions here */

#define MODE_FIXED 0	/* fixed-size mode */
#define MODE_ADD 1		/* additive self-incrementing mode */
#define MODE_MULT -1	/* multiplicative self-incrementing mode */		
#define INC_FACTOR_0 0
#define INC_FACTOR_1 1
#define INC_FACTOR_100 100
#define INC_FACTOR_255 255

														   /* Enter your bit-masks constant definitions here */
#define DEFAULT_FALGS 0xFFFC /*default flags value*/
#define SET_EOB 0x0002              /*set eob mask*/
#define RESET_EOB 0x0000            /*reset eob mask*/
#define CHECK_EOB 0x0001            /*check eob mask*/
#define SET_R_FLAG 0x0000           /*set r_flag mask*/
#define RESET_R_FLAG 0x0000         /*reset r_flag mask*/
#define CHECK_R_FLAG 0x0001         /*check r_flag mask*/

														   /* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  mode;       /* operational mode indicator*/
	unsigned short flags; /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/
Buffer * b_allocate(short, char, char);
pBuffer b_addc(pBuffer const, char);
void b_free(Buffer * const);
int b_isfull(Buffer * const);
short b_limit(Buffer * const);
short b_capacity(Buffer * const);
short b_mark(pBuffer const, short mark);
int b_mode(Buffer * const);
size_t b_incfactor(Buffer * const);
int b_load(FILE * const, Buffer * const);
int b_isempty(Buffer * const);
char b_getc(Buffer * const);
int b_eob(Buffer * const);
int b_print(Buffer * const);
Buffer * b_compact(Buffer * const, char);
char b_rflag(Buffer * const);
short b_retract(Buffer * const);
short b_reset(Buffer * const);
short b_getcoffset(Buffer * const);
int b_rewind(Buffer * const);
char * b_location(Buffer * const, short);

#endif

