#ifndef PARSER_H_
#define PARSER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

/* Define Constants */
#define NO_ATTR -1
#define ELSE 0	
#define FALSE 1 
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

#define AND 0
#define OR 1

#define EQ 0
#define NE 1
#define GT 2
#define LT 3

#define PLUS 0
#define MINUS 1 
#define MULT 2 
#define DIV 3

#define SCC_OP_T 7

static Token lookahead;

void parser(void);
void match(int, int);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char*);

void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_p(void);
void assignment_statement(void);
void assignment_expression(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void selection_statement(void);
void conditional_expression(void);
void logical_or_expression(void);
void logical_or_expression_p(void);
void logical_and_expression(void);
void logical_and_expression_p(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_a_relational_expression_p(void);
void primary_s_relational_expression(void);
void primary_s_relational_expression_p(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);

extern Token malar_next_token();
extern Buffer * str_LTBL;
extern char * kw_table[];
extern int line;
int synerrno = 0;

#endif