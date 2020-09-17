/*
* Function List: scanner_init(), char_class(), get_next_state(), iskeyword(),malar_next_token(), aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func11(), aa_func12()
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */

/*
Purpose: Initialize scaner
Version:
Called function: b_isempty(), b_rewind(), b_clear()
Parameters:  Buffer * psc_buf
Return value: EXIT_SUCCESS, EXIT_FAILURE
*/
/*Initializes scanner */
int scanner_init(Buffer * psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
Purpose: Pattern token recognition from sc_buf
Version: 2.0
Called function: b_getc(), b_retract(), b_mark(), b_getcoffset(), b_reset(), isalpha(), isdigit(), get_next_state(), b_allocate, b_free, b_addc()
Parameters:
Return value:
*/
Token malar_next_token(void) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */

  /*  DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED   */

	while (1) { /* endless loop broken by token returns it will generate a warning */

   /* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */

		c = b_getc(sc_buf);


		/* Part 1: Implementation of token driven scanner */
		/* every token is possessed by its own dedicated code */
		/*
		WRITE YOUR CODE FOR PROCESSING THE SPECIAL - CASE TOKENS HERE.
			COMMENTS ARE PROCESSED HERE ALSO.

			WHAT FOLLOWS IS A PSEUDO CODE.YOU CAN USE switch STATEMENT
			INSTEAD OF if - else TO PROCESS THE SPECIAL CASES
			DO NOT FORGET TO COUNT THE PROGRAM LINES

			NOTE :
		IF LEXICAL ERROR OR ILLEGAL CHARACTER ARE FOUND THE SCANNER MUST RETURN AN ERROR TOKEN.
			ILLEGAL CHARACTER IS ONE THAT IS NOT DEFINED IN THE LANGUAGE SPECIFICATION
			OR IT IS OUT OF CONTEXT.
			THE ILLEGAL CHAR IS THE ATTRIBUTE OF THE ERROR TOKEN
			THE ILLEGAL CHARACTERS ARE PROCESSED BY THE TRANSITION TABLE.
			SOME OF THE LEXICAL ERRORS ARE ALSO PROCESSED BY THE TRANSITION TABLE.

			MUST BE THE STRING "RUN TIME ERROR: "

			IF(c == SOME CHARACTER)
			...
			SKIP CHARACTER(FOR EXAMPLE SPACE)
			continue;
		OR SET TOKEN(SET TOKEN CODE AND TOKEN ATTRIBUTE(IF AVAILABLE))
			return t;
	EXAMPLE:
		*/
		/* new line or spacing characters */
		if (c == '\t') continue;
		if (c == '\n') { line++; continue; }
		if (c == '\r') continue;
		if (c == '\f') continue;
		if (c == '\v') continue;
		if (c == ' ') continue;
		/* left brace */
		if (c == '{') { t.code = LBR_T; /*no attribute */ return t; }
		/* right brace */
		if (c == '}') { t.code = RBR_T; /*no attribute */ return t; }
		/* left parenthesis */
		if (c == '(') { t.code = LPR_T; /*no attribute */ return t; }
		/* right parenthesis */
		if (c == ')') { t.code = RPR_T; /*no attribute */ return t; }

		/* ARITHMETIC OPERATOR */
		/* add */
		if (c == '+') {
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		}

		/* subtract */
		if (c == '-') {
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		}
		/* multiplication */
		if (c == '*') {
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		}
		/* division */
		if (c == '/') {
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		}
		/* RELATIONAL OPERATORS */
		/* assignment = / relational == */
		if (c == '=') {
			/* check next character for == */
			c = b_getc(sc_buf);
			/* relational == */
			if (c == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;
		}
		/* relational <> / concatenation << */
		if (c == '<') {
			/* check next character for << or >*/
			c = b_getc(sc_buf);
			if (c == '<') { /* string concatenation */
				t.code = SCC_OP_T;
				return t;
			}
			else if (c == '>') { /* relational */
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			else {
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				b_retract(sc_buf);
				return t;
			}
		}
		/* relational */
		if (c == '>') {
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		}

		/* LOGARITHMETIC OPERATOR */
		if (c == '.') {
			/* set marker incase error to reset */
			b_mark(sc_buf, b_getcoffset(sc_buf));
			/* check next character for AND or OR or errors */
			c = b_getc(sc_buf);

			if (c == 'A') {
				/* check for N */
				c = b_getc(sc_buf);
				if (c == 'N') {
					/* check for D */
					c = b_getc(sc_buf);
					if (c == 'D') {
						/* check for . */
						c = b_getc(sc_buf);
						if (c == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
			}
			else if (c == 'O') {
				/* check for R */
				c = b_getc(sc_buf);
				if (c == 'R') {
					/* check for . */
					c = b_getc(sc_buf);
					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			}

			/* run if unsucessful return t; above */
			/* back to b_mark */
			b_reset(sc_buf);
			t.code = ERR_T;
			t.attribute.err_lex[0] = '.';
			t.attribute.err_lex[1] = c;
			t.attribute.err_lex[2] = '\0';
			return t;
		}

		/* comments */
		if (c == '!') {
			/* check for error */
			c = b_getc(sc_buf);
			if (c != '!') {
				b_retract(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';
				/* go through comment until end of line */
				while (c != '\n' && c != '\r' && c != '\0' && c != 255 || c != SEOF_EOF) {
					c = b_getc(sc_buf);
				}
				return t;
			}
			else {
				/* is a comment */
				while (c != '\n' && c != '\r' && c != '\0') {
					c = b_getc(sc_buf);
				}
				line++;

				continue;
			}
		}

		if (c == '\0') {
			t.attribute.seof = SEOF_0;
			t.code = SEOF_T;
			return t;
		}

		/* Part 2: Implementation of Finite State Machine (DFA)
				   or Transition Table driven Scanner
				   Note: Part 2 must follow Part 1 to catch the illegal symbols
		*/
		if (isalpha(c) || isdigit(c)) {
			/* FSM0.Begin with state = 0 and the input character c */
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
			/* FSM1. Get the next state from the transition table calling   */
			state = get_next_state(state, c, &accept);
			/* FSM2. Get the next character */
			// c = b_getc(sc_buf);

			/* FSM3. If the state is not accepting (accept == NOAS), go to step FSM1 */
			//if (accept == NOAS) {
			while (accept == NOAS) {
				/* FSM2. Get the next character */
				c = b_getc(sc_buf);
				/* FSM3. If the state is not accepting (accept == NOAS), go to step FSM1 */
				state = get_next_state(state, c, &accept);
			}

			/* If the step is accepting, token is found, leave the machine and
			call an accepting function as described below.     */
			if (accept == ASWR) {
				/* RETRACT  getc_offset IF THE FINAL STATE IS A RETRACTING FINAL STATE */
				b_retract(sc_buf);
			}
			/* SET lexend TO getc_offset USING AN APPROPRIATE BUFFER FUNCTION */
			lexend = b_getcoffset(sc_buf);

			return t;
		}

		/* CREATE  A TEMPORRARY LEXEME BUFFER HERE; */
		lex_buf = b_allocate(lexend - lexstart, 0, 'f');
		/* RETRACT getc_offset to the MARK SET PREVIOUSLY AT THE BEGINNING OF THE LEXEME AND
		*/
		/*
		IN A CASE OF RUNTIME ERROR, THE FUNCTION MUST STORE
		A NON - NEGATIVE NUMBER INTO THE GLOBAL VARIABLE scerrnum
		AND RETURN A RUN TIME ERROR TOKEN.THE RUN TIME ERROR TOKEN ATTRIBUTE
		*/
		if (!lex_buf) {
			scerrnum = 1;
			t.code = RTE_T;
			return t;
		}

		/* . USING b_getc() COPY THE LEXEME BETWEEN lexstart AND lexend FROM THE INPUT BUFFER INTO lex_buf USING b_addc(...), */
		for (int i = lexstart; i < lexend; i++) {
			b_addc(lex_buf, b_getc(sc_buf));
		}
		/*
		 . WHEN VID (KEYWORDS INCLUDED), FPL OR IL IS RECOGNIZED
	. YOU MUST CALL THE ACCEPTING FUNCTION USING THE ARRAY aa_table ,WHICH
	. CONTAINS POINTERS TO FUNCTIONS. THE ARRAY INDEX OF THE FUNCTION TO BE
	. CALLED IS STORED IN THE VARIABLE state.
	. YOU ARE NOT ALLOWED TO CALL ANY OF THE ACCEPTING FUNCTIONS BY NAME.
	. THE ARGUMENT TO THE FUNCTION IS THE STRING STORED IN lex_buf char array.
		*/
		aa_table[state](lex_buf->cb_head);
		b_free(lex_buf);
		return t;
	}//end while(1)
}

/*
Purpose: Get next state in scanner of the character
Version:
Called function: char_class()
Parameters: int state, char c, int *accept
Return value: int next
*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*
Purpose: Return column number of the transition table for char c
Version: 1.x
Called function: isalpha(), isdigit()
Parameters: char c
Return value: int val
*/
int char_class(char c)
{
	int val;
	/*
THIS FUNCTION RETURNS THE COLUMN NUMBER IN THE TRANSITION
TABLE st_table FOR THE INPUT CHARACTER c.
SOME COLUMNS MAY REPRESENT A CHARACTER CLASS .
FOR EXAMPLE IF COLUMN 2 REPRESENTS [A-Za-z]
THE FUNCTION RETURNS 2 EVERY TIME c IS ONE
OF THE LETTERS A,B,...,Z,a,b...z.
PAY ATTENTION THAT THE FIRST COLOMN IN THE TT IS 0 (has index 0)
*/
	if (isalpha(c)) {
		return 1;
	}
	else if (isdigit(c)) {
		return 2;
	}
	else if (c == '0') {
		return 3;
	}
	else if (c == '.') {
		return 4;
	}
	else if (c == '@') {
		return 5;
	}
	else {
		return 6;
	}

	return val;
}


/*
ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords (VID - AVID/KW)
*/

Token aa_func02(char lexeme[]) {
	Token t;
	/* 1. CHECK IF THE LEXEME IS A KEYWORD. */
	int check = iskeyword(lexeme);

	/*
		  IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
   FOR THE KEYWORD. THE ATTRIBUTE CODE FOR THE KEYWORD
   IS ITS INDEX IN THE KEYWORD LOOKUP TABLE (kw_table in table.h).
	*/
	if (check != -1) {
		t.code = KW_T;
		return t;
	}
	/* IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2. */
	/*
2. SET a AVID TOKEN.
   IF THE lexeme IS LONGER than VID_LEN (see token.h) CHARACTERS,
   ONLY FIRST VID_LEN CHARACTERS ARE STORED
   INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
   ADD \0 AT THE END TO MAKE A C-type STRING.
	*/
	if (strlen(lexeme) > VID_LEN) {
		for (int i = 0; i < VID_LEN; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
	}
	return t;
}

/*
Purpose: Accepting function fur SVID
Version: 1.3
Called function: strlen()
Parameters: char lexeme[]
Return value: Token
*/
/* ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID) */
Token aa_func03(char lexeme[]) {
	Token t;
	int i = 0;
	unsigned int j = 0;
	/*1. SET a SVID TOKEN. */
	t.code = SVID_T;

	/* IF THE lexeme IS LONGER than VID_LEN characters,
		ONLY FIRST VID_LEN - 1 CHARACTERS ARE STORED
		INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
		AND THEN THE @ CHARACTER IS APPENDED TO THE NAME.
		ADD \0 AT THE END TO MAKE A C - type STRING.
		*/
	if (strlen(lexeme) > VID_LEN) {
		for (i; i < VID_LEN - 1; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[i] = '@';
		t.attribute.vid_lex[i++] = '\0';
		return t;
	}

	/* IF lexeme SHORT THAN VID_LEN STILL SVID*/
	for (j; j < strlen(lexeme); j++) {
		t.attribute.vid_lex[j] = lexeme[j];
	}
	t.attribute.vid_lex[j] = '@';
	t.attribute.vid_lex[j++] = '\0';
	return t;
}

/*
Purpose: Accepting function for FPL
Version: 1.1
Called function: atof(), aa_func12()
Parameters: char lexeme[]
Return value: Token
*/
/* ACCEPTING FUNCTION FOR THE floating - point literal (FPL) */
Token aa_func08(char lexeme[]) {
	Token t;
	/* THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
		WHICH IS THE ATTRIBUTE FOR THE TOKEN. */
	double f = atof(lexeme);

	/* THE VALUE MUST BE IN THE SAME RANGE AS the value of 4 - byte float in C. */
	if ((f > 0 && (f < FLT_MIN || f > FLT_MAX))) {
		/*
		IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
		THE ERROR TOKEN ATTRIBUTE IS  lexeme.

		IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
		STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C - type string.
		*/
		return aa_func12(lexeme);
	}
	/*BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	t.code = FPL_T;
	t.attribute.flt_value = (float)f;
	return t;
}

/*
Purpose: Acception function for IL - DIL
Version: 1.1
Called function: atol(), aa_func12()
Parameters: char lexeme[]
Return value: Token
*/
/* ACCEPTING FUNCTION FOR THE integer literal(IL)-decimal constant(DIL) */
Token aa_func05(char lexeme[]) {
	Token t;
	/*THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
		TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.*/
	long l = atol(lexeme);

	/*THE VALUE MUST BE IN THE SAME RANGE AS the value of 2 - byte integer in C.*/
	if (l > SHRT_MAX) {
		/*IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
		THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
		STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C - type string.*/
		return aa_func12(lexeme);
	}
	/*BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	t.code = INL_T;
	t.attribute.int_value = (int)l;
	return t;
}

/*
Purpose: Accepting function for SL
Version: 1.1
Called function: b_addc(), strlen()
Parameters: char lexeme[]
Return value: Token
*/
/* ACCEPTING FUNCTION FOR THE string literal(SL) */
Token aa_func10(char lexeme[]) {
	Token t;
	line++;
	/*THE FUNCTION MUST STORE THE lexeme PARAMETER CONTENT INTO THE STRING LITERAL TABLE(str_LTBL) */
	/* FIRST THE ATTRIBUTE FOR THE TOKEN MUST BE SET.
	THE ATTRIBUTE OF THE STRING TOKEN IS THE OFFSET FROM
	THE BEGINNING OF THE str_LTBL char buffer TO THE LOCATION
	WHERE THE FIRST CHAR OF THE lexeme CONTENT WILL BE ADDED TO THE BUFFER.
	*/
	t.attribute.str_offset = b_limit(str_LTBL);
	/*
	USING b_addc(..)COPY THE lexeme content INTO str_LTBL.
	THE OPENING AND CLOSING " MUST BE IGNORED DURING THE COPING PROCESS. */
	for (unsigned int i = 1; i < strlen(lexeme) - 1; i++)
		b_addc(str_LTBL, lexeme[i]);
	/*
		ADD '\0' AT THE END MAKE THE STRING C - type string
		IF THE STING lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
		*/
	b_addc(str_LTBL, '\0');
	/*SET THE STRING TOKEN CODE.*/
	t.code = STR_T;
	return t;
}

/*
Purpose: Display error token with message depending on ERR_LEN exceeded or not
Version: 1.0
Called function: strlen()
Parameters: char lexeme[]
Return value: Token
*/
/* ACCEPTING FUNCTION FOR THE ERROR TOKEN */
Token aa_func11(char lexeme[]) {
	Token t;
	int lexLen = strlen(lexeme);

	/*
	lexeme[] CONTAINS THE ERROR
	THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
	AND IT MUST BE STORED in err_lex.IF THE ERROR lexeme IS LONGER
	than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
	STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
	err_lex C - type string.
	*/
	if (lexLen > ERR_LEN) {
		for (int i = 0; i < ERR_LEN - 3; i++) {
			t.attribute.err_lex[i] = lexeme[i];
			/*
			IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
			*/=
			if (lexeme[i] == '\n') line++;

		}
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else {
		for (int i = 0; i < lexLen; i++) {
			t.attribute.err_lex[i] = lexeme[i];
			/*
			IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
			*/
			if (lexeme[i] == '\n') line++;
			if (lexeme[i] == '\r') line++;
		}
		t.attribute.err_lex[lexLen] = '\0';
	}
	/* THE FUNCTION SETS THE ERROR TOKEN. */
	/*BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	t.code = ERR_T;
	return t;
}

/*
Purpose: Display error token with message depending on ERR_LEN exceeded or notf
Version: 1.0
Called function: strlen()
Parameters: char lexeme[]
Return value: Token
*/
/* ACCEPTING FUNCTION FOR THE ERROR TOKEN */
Token aa_func12(char lexeme[]) {
	Token t;
	int lexLen = strlen(lexeme);

	/*
	lexeme[] CONTAINS THE ERROR
		THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
		AND IT MUST BE STORED in err_lex.IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
		STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C - type string.
		*/
	if (lexLen > ERR_LEN) {
		for (int i = 0; i < ERR_LEN - 3; i++) {
			t.attribute.err_lex[i] = lexeme[i];
			/*
			IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
			*/
			if (lexeme[i] == '\0') line++;
			if (lexeme[i] == '\n') line++;
			if (lexeme[i] == '\r') line++;

		}
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else {
		for (int i = 0; i < lexLen; i++) {
			t.attribute.err_lex[i] = lexeme[i];
			/*
			IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
			*/
			if (lexeme[i] == '\0') line++;
			if (lexeme[i] == '\n') line++;
			if (lexeme[i] == '\r') line++;
		}
		t.attribute.err_lex[lexLen] = '\0';
	}
	/* THE FUNCTION SETS THE ERROR TOKEN. */
	/*BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	t.code = ERR_T;
	return t;
}

/*
	Purpose: Compare character from kw_table and kw_lexeme, then return				index where comparison matched to an existing lexeme
	Version: 1.1
	Called function:
	Parameters: char kw_lexeme
	Return value:	-1 when no match found
					1 when match found
*/
int iskeyword(char * kw_lexeme) {
	if (kw_lexeme == NULL) {
		return -1;
	}
	for (int i = 0; i < KWT_SIZE; i++) {
		/* compare table with keyword lexeme */
		if (strcmp(kw_table[i], kw_lexeme) == 0) {
			return i;
		}
	}
	return -1;
}