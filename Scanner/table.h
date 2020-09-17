/*
* File Name: table.h
*/


#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

 /*   Source end-of-file (SEOF) sentinel symbol
  *    '\0' or one of 255,0xFF,EOF
  */

  /*  Special case tokens processed separately one by one
   *  in the token-driven part of the scanner
   *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
   *  white space
   *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
   *  .AND., .OR. , SEOF, 'illegal symbol',
   */

#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

   /* State transition table definition */

#define TABLE_COLUMNS 6
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */ { 1, 6, 4, ES, ES, ES },		/* NOAS */
	/* State 1 */ { 1, 1, 1, ER, 3, 2 },		/* NOAS */
	/* State 2 */ { IS, IS, IS, IS, IS, IS },	/* Retract */
	/* State 3 */ { IS, IS, IS, IS, IS, IS },	/* No Retract */
	/* State 4 */ { ER, 6, ER, 7, ER, 5 },		/* NOAS */
	/* State 5 */ { IS, IS, IS, IS, IS, IS },	/* Retract */
	/* State 6 */ { ER, 6, ER, 7, ER, 5 },		/* NOAS */
	/* State 7 */ { ER, 7, 7, ER, 8, 8 },		/* NOAS */
	/* State 8 */ { IS, IS, IS, IS, IS, IS },	/* Retract */
	/* State 9 */  { 9, 9, 9, 9, 9, 9 },		/* NOAS */
	/* State 10 */ { IS, IS, IS, IS, IS, IS },	/* No Retract */
	/* State 11 */ { IS, IS, IS, IS, IS, IS },	/* No Retract */
	/* State 12 */ { IS, IS, IS, IS, IS, IS }	/* Retract */
};

/* Accepting state table definition */
#define ASWR 14  /* accepting state with retract */
#define ASNR 15  /* accepting state with no retract */
#define NOAS 16  /* not accepting state */

int as_table[] = {
	/* State 0 */ NOAS,
	/* State 1 */ NOAS,
	/* State 2 */ ASWR,
	/* State 3 */ ASNR,
	/* State 4 */ NOAS,
	/* State 5 */ ASWR,
	/* State 6 */ NOAS,
	/* State 7 */ NOAS,
	/* State 8 */ ASWR,
	/* State 9 */ NOAS,
	/* State 10 */ ASNR,
	/* State 11 */ ASNR,
	/* State 12 */ ASWR
};

/* Accepting action function declarations */
Token aa_func02(char *lexeme); /* AVID/KWASW	Retract */
Token aa_func03(char *lexeme); /* SVID  AS		No Retract */
Token aa_func05(char *lexeme); /* DIL ASW		Retract */
Token aa_func08(char *lexeme); /* FPL ASW		Retract */
Token aa_func10(char *lexeme); /* SL AS			No Retract */
Token aa_func11(char *lexeme); /* ES AS			No Retract */
Token aa_func12(char *lexeme); /* ES ASW		Retract */

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/

typedef Token(*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	aa_func10,
	aa_func11,
	aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table[] =
{
"ELSE",
"FALSE",
"IF",
"PLATYPUS",
"READ",
"REPEAT",
"THEN",
"TRUE",
"WHILE",
"WRITE"
};

#endif
