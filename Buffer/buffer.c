/*
File name : [buffer.c]
	Purpose : Create a buffer to add symbols into the heap for the character Buffer under three modes, additive, fixed, and multiplicative
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

#include <stdlib.h>
#include "buffer.h"
/******************
Function name: Buffer * b_allocate
Purpose: Creates a buffer on the heap.
Version: 1.20
Called functions: calloc(), malloc(), free()
Function In parameters: short init_capacity is the initial capacity for one dynamic character buffer
						char int_factor is used in incrementing capacity for growing the buffer 
						char o_mode sets the mode to fixed, additive, or multiplicative on the condition that the inc_factor is also met
Function Out parameters NULL if there is a runtime error such as unable to create a working buffer, or constraint violations
						Buffer structure if successful
Algorithm:	Check init_capacity is a valid value
			allocate memory the size of 1 byte for the Buffer
			check Buffer exists on the heap
			allocate memory the size of the initial capacity for the character pointer cb_head
			check cb_head valid
			check mode f, or a, or m valid, and inc_factor valid
			increase capacity of cb_head, and change flag to default
			free memory allocated if something went wrong, then return NULL
			return Buffer

******************/
Buffer * b_allocate(short initial_capacity, char increment_factor, char o_mode) {
	/* Declaration of a pointer to the Buffer struct */
	Buffer * pBuffer;

	/* check init_capacity is valid, return NULL if init_capacity is not between 0 and MAX VALUE-1 inclusive */
	if (DEFAULT_INIT_CAPACITY < 0 && DEFAULT_INIT_CAPACITY > SHRT_MAX) {
		return NULL;
	}
	
	/* buffer definition, allocate memory for 1 Buffer structure */
	pBuffer = (Buffer *)calloc(1, sizeof(Buffer));

	/* else ... allocate memory for one dynamic character(*cb_head) buffer with init_capacity */
	pBuffer->cb_head = (char *)malloc(DEFAULT_INIT_CAPACITY);

	if (!(pBuffer->cb_head))
		return NULL;

	/* fixed value when inc_factor has a fixed value of 0 */
	if(DEFAULT_INC_FACTOR == 0) {
		/* set buffer mode to 0 (FIXED) */
		pBuffer->mode = MODE_FIXED;
		/* increment to capacity is not going to happen when it is FIXED, in this case at 0 */
		pBuffer->inc_factor = 0;
	}
	/* cannot be 0 and between 255 */
	else if (DEFAULT_INC_FACTOR > INC_FACTOR_0 && DEFAULT_INC_FACTOR <= INC_FACTOR_255) {
		switch (o_mode) {
		case 'f':
			/* inc_factor is checked earlier to not be 0, so it is FIXED automatically from o_mode = 'f' */
			pBuffer->mode = MODE_FIXED;
			pBuffer->inc_factor = 0;
			break;
		case 'a':
			/* ADDITIVE mode only if inc_factor between 1 and 255 inclusive*/
			if (DEFAULT_INC_FACTOR >= INC_FACTOR_1 && DEFAULT_INC_FACTOR <= INC_FACTOR_255) {
				/* set to 1 (ADDITIVE) */
				pBuffer->mode = MODE_ADD;
				/* buffer increment by inc_factor*/
				pBuffer->inc_factor = DEFAULT_INC_FACTOR;
			}
			/* not between 1 and 255 inclusive */
			else {
				/* case o_mode does exist, clear memory created on the heap for pBuffer->cb_head, return NULL to indicate error */
				free(pBuffer->cb_head);
				free(pBuffer);

				return NULL;
			}
			break;
		case 'm':
			/* MULTIPLICATIVE if inc_factor is between 1 and 100 inclusive */
			if (DEFAULT_INC_FACTOR >= INC_FACTOR_1 && DEFAULT_INC_FACTOR <= INC_FACTOR_100) {
				/* set to -1 (MULTIPLICATIVE) */
				pBuffer->mode = MODE_MULT;
				/* buffer increment by inc_factor */
				pBuffer->inc_factor = DEFAULT_INC_FACTOR;
			}
			/* greater than 100, less than 1 */
			else {
				/* case o_mode does exist, clear memory created on the heap for pBuffer->cb_head, return NULL to indicate error */
				free(pBuffer->cb_head);
				free(pBuffer);

				return NULL;
			}
			break;
		default:
			/* case o_mode does exist, clear memory created on the heap for pBuffer->cb_head, return NULL to indicate error */
			free(pBuffer->cb_head);
			free(pBuffer);

			return NULL;
			break;
		}
	}
	/* case does exist, clear memory created on the heap for pBuffer->cb_head, return NULL to indicate error */
	else {
		free(pBuffer->cb_head);
		free(pBuffer);

		return NULL;
	}/* if-else finished */
	/* copy initial capacity to the Buffer's capacity */
	pBuffer->capacity = DEFAULT_INIT_CAPACITY;
	/* default init hexadecimal flag 0xFFFC*/
	pBuffer->flags = DEFAULT_FALGS;

	/* success! return pointer to Buffer struct*/
	return pBuffer;
}

/******************
Function name: pBuffer b_addc
Purpose: Tries to increase capacity based on increment mode
Version: 1.20
Called functions: realloc()
Function In parameters: pBuffer const pBD is a pointer to the Buffer, 
						char symbol is a character that is meant to be placed in the buffer on the heap
Function Out parameters NULL if there is a runtime error such as unable to create a working buffer, or constraint violations
						Buffer structure if successful
Algorithm:	check buffer exists
			check addc_offset less than capacity
			increase addc_offset after placing a symbol at the end of the character pointer 
			increase capacity if Buffer full
			insert symbol in Buffer character array
			return Buffer

******************/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	/* size of addc_offset in bytes */
	short buffer_addc_offset;
	/* buffer's current capacity */
	short buffer_capacity;
	/* buffer mode */
	char buffer_mode;
	/* holds new size for capacity */
	short buffer_new_capacity;
	/* holds multiplicative increment factor */
	short buffer_inc_mult;
	/* buffer increment factor */
	short buffer_inc_factor;
	/* temporary character pointer buffer */
	char *buffer_temp;

	/* check pBuffer exists / is on the heap to not waste time defining the above declarations */
	if (!pBD)
		return NULL;
	
	/* more efficient by calling pBD->xxx only once */
	buffer_addc_offset = (short)(pBD->addc_offset * sizeof(char));
	buffer_capacity = pBD->capacity;
	/* reset r_flag to DEFAULT FALGS */
	pBD->flags = DEFAULT_FALGS;
	/****** 
		check capacity validity, store new capacity,
		check functionality of buffer
	*******/
	/* check length from beginning of array to the end of the next available space is less than the capacity that holds it */
	if (buffer_addc_offset < buffer_capacity) {
		pBD->cb_head[buffer_addc_offset] = symbol;
		/* increase buffer addc_offset (character length) by 1 */
		pBD->addc_offset++;
		return pBD;
	}
	/* FULL !! increase capacity */
	else if (buffer_addc_offset == buffer_capacity) {
		buffer_mode = pBD->mode;
		buffer_inc_factor = pBD->inc_factor;
		switch (buffer_mode) {
		case MODE_FIXED:
			/* Buffer mode of increment is FIXED. Nothing happens */
			return NULL;
			break;
		case MODE_ADD:
			/* OVERFLOW check */
			if (buffer_capacity == SHRT_MAX)
				return NULL;
			/* increase Buffer capacity by adding current capacity size to a number of chars (1 char to 255 chars) indicated in the increment factor */
			buffer_new_capacity = buffer_capacity + (unsigned char)(buffer_inc_factor)*(sizeof(char));
			/* check data is positive and not exceeding max -1  (SHRT_MAX - 1), also it cannot be negative*/
			if (buffer_new_capacity <= 0 || buffer_new_capacity > (SHRT_MAX - 1))
				return NULL;
			/* assign MAX-1 to capacity if new capacity exceeds MAX-1 */
			if (buffer_new_capacity > (SHRT_MAX - 1))
				buffer_new_capacity = SHRT_MAX - 1;
			break;
		case MODE_MULT:
			/* OVERFLOW check, cannot increase capacity if it is at its MAXIMUM value datatype (SHRT_MAX) */
			if (buffer_capacity == SHRT_MAX)
				return NULL;
			/* valid capacity, but may loop to negative after calculations */
			else {
				/* given equation ((max buffer capacity - current capacity)*inc_factor) / 100 */
				buffer_inc_mult = ((SHRT_MAX - buffer_capacity)*buffer_inc_factor) / 100;
				/* check buffer capacity is less than SHRT_MAX, as in the new increment is  not negative from calculations */
				if (buffer_inc_mult <= 0)
					/* reset increment to 1 when negative or fixed */
					buffer_new_capacity = SHRT_MAX;
				/* calculate new capacity by given equation (current capacity + new increment) */
				else
				buffer_new_capacity = buffer_capacity + buffer_inc_mult -1;

			}
			break;
		/* something went wrong */
		default:
			return NULL;
			break;
		}
	}
	/* some condition that does not satify the above, return NULL for error */
	else {
		return NULL;
	} /* end of if, else-if, else */
	
	/******
		check character buffer can be expanded before changing the original character Buffer (cb_head)
		then expand if valid
	******/
	if (!buffer_new_capacity)
		return NULL;
	buffer_temp = (char *)realloc(pBD->cb_head, buffer_new_capacity);
	/* return NULL if temporary buffer is not valid */
	if (!buffer_temp) {
		/* memory of character buffer has not been changed, set flag to 0 */
		/*pBD->flags = 0;*/
		return NULL;
	}

	/* buffer was able to reallocate memory location because buffer character locations do not equal each other */
	if (pBD->cb_head != buffer_temp)
		/* set flag to 1 */
		pBD->flags = (pBD->flags) | SET_R_FLAG;

	/* original buffer has the new allocated memory location for the character pointer */
	pBD->cb_head = buffer_temp;
	/* add symbol to character pointer Buffer at the index addc_offset*/
	pBD->cb_head[pBD->addc_offset] = symbol;
	/* addc_offset grew by 1 character */
	pBD->addc_offset++;
	/* insert new capacity directly to Buffer */
	pBD->capacity = buffer_new_capacity;

	return pBD;
} /* end of function */

/******************
Function name: int b_clear
Purpose: Clears the Buffer by starting from the beginning (not clearing data) and in the future overwriting content
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters -1 if there is an error
						0 if it is successful
Algorithm:	set capacity to 0
			set offset pointing to the beginning of character array

******************/
int b_clear(Buffer * const pBD) {
	/* check Buffer exists before reinitializing */
	if (!pBD)
		/* return -1 to notify the calling function of the failure */
		return -1;
	/* capacity appears empty */
	pBD->capacity = 0;
	/* length from first character to last index of the list of characters in the buffer */
	pBD->addc_offset = 0;
	/* length from first character to a index indicated by b_getc() call */
	pBD->getc_offset = 0;
	/* length from first character to a desired index */
	pBD->markc_offset = 0;
	/* reset end of buffer flag */
	pBD->flags = RESET_EOB;
	return 0;
}

/******************
Function name: void b_free
Purpose: Frees the character Buffer and the Buffer
Version: 1.0
Called functions: free()
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters none
Algorithm:	Free memory for the Buffer character pointer cb_head
			Free memory for the buffer

******************/
void b_free(Buffer * const pBD) {
	/* make sure the Buffer is not NULL or invalid before freeing memory */
	if (pBD) {
		free(pBD->cb_head);
		free(pBD);
	}
	return;
}

/******************
Function name: int b_isfull
Purpose: Checks to see if the capcity is full, 
Version: 1.05
Called functions: sizeof()
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on error
						return 1 on indication Buffer capacity is full
						return 0 if not full
Algorithm:	check for valid Buffer pointer
			get addc_offset and capacity and compare to check capacity is full

******************/
int b_isfull(Buffer * const pBD) {
	/* length of character Buffer in bytes */
	short buffer_addc_offset;
	/* capacity of Buffer*/
	short buffer_capacity;

	/* if runtime error, NULL Buffer */
	if (!pBD)
		return -1;
	/* store value of character length in bytes */
	buffer_addc_offset = (short)(pBD->addc_offset * sizeof(char));
	/* store capacity */
	buffer_capacity = pBD->capacity;
	/* check if Buffer capacity is full */
	if (buffer_addc_offset == buffer_capacity)
		return 1;
	return 0;
}

/******************
Function name: short b_limit
Purpose: returns the value of addc_offset which descirbes the number of characters taken up so far
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on error
						return addc_offset when valid Buffer exists
Algorithm:	check runtime error
			return addc_offset (limit of the buffer)

******************/
short b_limit(Buffer * const pBD) {
	/* return -1 if pBD gives runtime error */
	if (!pBD)
		return -1;
	/* return addc_offset value of the Buffer */
	return pBD->addc_offset;
}

/******************
Function name: short b_capacity
Purpose: returns the value of capacity which descirbes the space the Buffer has currently
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on error
						return capacity when valid Buffer exists
Algorithm:	return capacity

******************/
short b_capacity(Buffer * const pBD) {
	if (!pBD)
		return -1;
	return pBD->capacity;
}

/******************
Function name: short b_mark
Purpose: sets the Buffer's markc_offset to mark. Mark cannot exceed the current character length given by addc_offset otherwise the program will point to random memory

Function In parameters: pBuffer const pBD is a pointer to the Buffer
						mark is the index in which to stop at in the character Buffer
Function Out parameters return -1 on error
						return markc_offset when valid Buffer exists, where markc_offset begins from the first character in the Buffer (cb_head)
Algorithm:	check mark is a valid value in an existing range between 0 and addc_offsset
			set mark
			return mark
******************/
short b_mark(pBuffer const pBD, short mark) {
	/* check if runtime error, return -1*/
	if (!pBD)
		return -1;
	/* mark must be greater or equal to 0 and less than the length of the existing characters of the buffer denoted by addc_pffset */
	if (mark >= 0 && mark <= pBD->addc_offset) {
		/* set Buffer's markc_offset to indicated mark */
		pBD->markc_offset = mark;
		/* return the current markc_offset starting from the first character in the Buffer */
		return pBD->markc_offset;
	}
	/* else above is false */
	return -1;
}

/******************
Function name: int b_mode
Purpose: return the mode of the Buffer
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on error
						return mode of the Buffer
Algorithm:	

******************/
int b_mode(Buffer * const pBD) {
	/* check pBD for runtime error and notify calling function about failure */
	if (!pBD)
		return -1;
	/* return Buffer mode */
	return pBD->mode;
}

/******************
Function name: size_t b_incfactor
Purpose: return the 0 or positive value of the Buffer's incfactor
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return 0x100 on error, which is past the limit of all modes (1 to 100 inclusive, or 1 to 255 inclusive)
						return the incremental factor of the Buffer
Algorithm:	return inc_factor

******************/
size_t b_incfactor(Buffer * const pBD) {
	/* check for runtime error */
	if (!pBD)
		return 0x100;
	return (unsigned char)(pBD->inc_factor);
}

/******************
Function name: size_t b_incfactor
Purpose: return the 0 or positive value of the Buffer's incfactor
Version: 1.15
Called functions: fget(), feof(), b_addc(), ungetc(), printf(), fclose()
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return 0x100 on error, which is past the limit of all modes (1 to 100 inclusive, or 1 to 255 inclusive)
Algorithm:	read file if file is valid
			store symbol and check validity
			add symbol to the character buffer
			if symbol is invalid, return the character, print the symbol and number and return LOAD_fail
			return number of symbols added to the character buffer if everythign is valid
			close file reader


******************/
int b_load(FILE * const fi, Buffer * const pBD) {
	/* holds 1 symbol with size of 1 byte */
	char symbol;
	/* holds number of symbols added to the buffer */
	short numOfSymbols = 0;

	/* check runtime error */
	if (!pBD || !fi)
		return -1;

	/* loop through every symbol in the file, and break out of loop at the end of file */
	for (;;) {
		/* store 1 symbol */
		symbol = (char)fgetc(fi);
		/* check file end of file */
		if (feof(fi))
			break;
		/* check return value of b_addc, either returns NULL or valid Buffer. return -2 if NULL, otherwise symbol has been added to Buffer */
		if (!b_addc(pBD, symbol)) {
			/* return character to file stream */
			ungetc(symbol, fi);
			/* print symbol as character and integer */
			printf("Last character read from input file is: %c %d\n", symbol,symbol);
			/* return -2 on error */
			return LOAD_FAIL;
		}
		/* increase number of symbols added to the buffer */
		numOfSymbols++;
	}
	/* close finished file */
	fclose(fi);
	
	return numOfSymbols;
}

/******************
Function name: int b_isempty
Purpose: Check if the buffer's add character offset is empty (length of characters from first character to last)
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return 1 if empty
						return 0 if not empty
Algorithm:	check if the length of the character Buffer is 0

******************/
int b_isempty(Buffer * const pBD) {
	/* check if valid */
	if (!pBD)
		return -1;

	/* return 1 if empty */
	if (pBD->addc_offset == 0)
		return 1;
	return 0;
}

/******************
Function name: char b_getc
Purpose: Check if EOB reached, otherwise return the symbol of the character pointer array (cb_head)
Version: 1.3
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -2 if error occurs
						return 0 if EOB reached
						return symbol indicated by getc_offset
Algorithm:	check if Buffer and Buffer character is valid
			EOB set if end of buffer for character Buffer
			otherwise reset EOB and increase getc_offset
			return character at getc_offset-1

******************/
char b_getc(Buffer * const pBD) {
	/* check that Buffer is not NULL, and because we are reading/getting a character check the character pointer cb_head is valid */
	if (!pBD || !pBD->cb_head)
		return -2;
	/*  check if end of buffer has been reached during read operation */
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->flags = DEFAULT_FALGS | SET_EOB;
		return 0;
	}
	/* end of buffer not reached yet, EOB flag is set to 0 to denote this */
	pBD->flags = RESET_EOB;

	/* increment character offset for the next function call */
	pBD->getc_offset++;
	/* return symbol from the character array at index getc_offset*/
	return *(pBD->cb_head + (pBD->getc_offset-1));
}

/******************
Function name: int b_eob
Purpose: returns the EOB flag
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Version: 1.0
Called functions: None
Function Out parameters return -1 if runtime error occurs
						return EOB flag
Algorithm:	return flags

******************/
int b_eob(Buffer * const pBD) {
	/* check for error (if Buffer is valid) */
	if (!pBD)
		return -1;
	/* return EOB flag */
	return pBD->flags;
}

/******************
Function name: int b_print
Purpose: print out each symbol in the character Buffer, and return the number of symbols there are in the character Buffer cb_head
Version: 1.4
Called functions: printf(), b_getc(), b_eob()
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on runtime error
						return number of symbols
						return nothing because buffer is empty
Algorithm:	check buffer is empty, print empty
			loop through the buffer character and print the symbols stored
			count number of symbols printed and return number of symbols if end of buffer

******************/
int b_print(Buffer * const pBD) {
	short numOfSymbols = 0;
	char symbol;

	if (!pBD)
		return -1;

	/* check addc_offset is empty, addc_offset is the length of symbols in the character Buffer */
	if (pBD->addc_offset == 0) {
		return 0;
	}
	for (;;) {
		/* get symbol from cb_head AND set EOB flag */
		symbol = b_getc(pBD);
		/* check end of buffer flag determined in b_getc by pBD->getc_offset == pBD->addc_offset */
		if (b_eob(pBD))
			/* break if EOB reached */
			break;
		/* print character if EOB not reached */
		printf("%c", symbol);
		/* increase number of symbols printed out */
		numOfSymbols++;
	}
	printf("\n");
	return numOfSymbols;
}

/******************
Function name: int b_compact
Purpose: shrink the buffer or expand the buffer to a new capacity for all operation modes
Version: 1.20
Called functions: sizeof(), realloc(), 
Function In parameters: pBuffer const pBD is a pointer to the Buffer
						synbol is a character added to the end of the Buffer character pointer cb_head
Function Out parameters return NULL on runtime error
						return pBD original Buffer structure after alterations to Buffer
Algorithm:	check valid Buffer and Buffer character
			alter capacity by 1 to reallocate capacity for cb_head
			cb_head cannot have the same memory location as the temporary buffer
			set new character Buffer and capacity
			add symbol to the end of the character buffer

******************/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	/* store characters temporarily to be checked later */
	char * buffer_temp;
	/* holds new size for capacity */
	short buffer_new_capacity;
	/* check for runtime error, and for cb_head because it is going to be altered */
	if (!pBD || !pBD->cb_head)
		return NULL;
	/* new capacity measured in bytes */
	buffer_new_capacity = (pBD->addc_offset + 1) * sizeof(char);
	/* check calculation for UNDERFLOW */
	if (buffer_new_capacity <= 0)
		return NULL;
	/* using realloc to adjust new capacity calculated by the current limit + 1 in bytes */
	buffer_temp = (char *)realloc(pBD->cb_head, (unsigned short)(buffer_new_capacity));
	/* check if valid */
	if (!buffer_temp)
		return NULL;
	/* character array location changed from reallocation? return 1 */
	if (buffer_temp != pBD->cb_head)
		pBD->flags = SET_R_FLAG;
	/* character array has not changed */
	else
		pBD->flags = (pBD->flags) | SET_R_FLAG;
	/* update all necessary members of the buffer descriptor */
	pBD->cb_head = buffer_temp;
	pBD->capacity = buffer_new_capacity;

	/* add symbol to the end of character buffer */
	pBD->cb_head[pBD->addc_offset++] = symbol;

	return pBD;
}

/******************
Function name: char b_rflag
Purpose: return flags
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return NULL on runtime error
						return r_flag
Algorithm: check runtime error
			return flag

******************/
char b_rflag(Buffer * const pBD) {
	if (!pBD)
		return -1;
	return (unsigned char)pBD->flags;
}

/******************
Function name: int b_retract
Purpose: return 1 index of the character array pointer of the Buffer
Version:
Called functions:
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on runtime error
						return getc_offset 1 index back
Algorithm:	check runtime error
			return the index of the previous getc_offset

******************/
short b_retract(Buffer * const pBD) {
	if (!pBD)
		return -1;
	return pBD->getc_offset--;
}

/******************
Function name: int b_reset
Purpose: reset Buffer getc_offset
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on runtime error
						return reset getc_offset


******************/
short b_reset(Buffer * const pBD) {
	if (!pBD)
		return -1;
	/* reset getc_offset by storing markc_offset */
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

/******************
Function name: int b_getcoffset
Purpose: reset Buffer getc_offset
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on runtime error
						returngetc_offset
Algorithm:	return character offset

******************/
short b_getcoffset(Buffer * const pBD) {
	if (!pBD)
		return -1;
	return pBD->getc_offset;
}

/******************
Function name: int b_rewind
Purpose: allow Buffer to be reread by setting getc and markc offset to 0
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
Function Out parameters return -1 on runtime error
						return 0 on success
Algorithm:	point character offset and character mark to the beginning, denoted by 0

******************/
int b_rewind(Buffer * const pBD) {
	if (!pBD)
		return -1;
	/* set to 0 so Buffer can be re-read */
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	return 0;
}

/******************
Function name: char * b_location
Purpose: return pointer to a location of the character buffer indicated by loc_offset
Version: 1.0
Called functions: None
Function In parameters: pBuffer const pBD is a pointer to the Buffer
						short loc_offset is the length from the beginning of the character array (cb_head)
Function Out parameters return -1 on runtime error
						return 
Algorithm:	check runtime exception
			return pointer Buffer at a specified location (loc_offset)

******************/
char * b_location(Buffer * const pBD, short loc_offset) {
	if (!pBD)
		return NULL;
	/* return POINTER + loc_offset, NOT THE CHARACTER */
	return pBD->cb_head + loc_offset;
}