/*
Description: This program is meant to parse tokens and validate each token before compiling the program. On error the parser goes into panic mode to try and fix the issue. An error message is sent on error, and the line of error is displayed.
Functions:  parser(), match(int, int), syn_eh(int sync_token_code), syn_printe(), gen_incode(char*), program(), opt_statements(), statements(), statement(), statements_p(), assignment_statement(), assignment_expression(), arithmetic_expression(), unary_arithmetic_expression(), additive_arithmetic_expression(), additive_arithmetic_expression_p(), multiplicative_arithmetic_expression(), multiplicative_arithmetic_expression_p(), primary_arithmetic_expression(), string_expression(), string_expression_p(), primary_string_expression(), selection_statement(), conditional_expression(), logical_or_expression(), logical_or_expression_p(), logical_and_expression(), logical_and_expression_p(), relational_expression(), primary_a_relational_expression(), primary_a_relational_expression_p(), primary_s_relational_expression(), primary_s_relational_expression_p(), iteration_statement(), input_statement(), variable_list(), variable_list_p(), variable_identifier(), output_statement(), output_list(), 
*/

#include "parser.h"


/*
* Purpose: Begin program and match PLATYPUS
* Called Functions: program(), match(), gen_incode()
* Parameters: None
* Return value: None
* Algorithm:
*/
void parser(void) {
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*
* Purpose: Match toke based on code and attribute
* Called Functions: syn_eh()
* Parameters: None
* Return value: None
* Algorithm:
*/
void match(int pr_token_code, int pr_token_attribute) {
	/* check token */
	switch (pr_token_code) {
	case KW_T:
		if (pr_token_attribute != lookahead.attribute.kwt_idx) {
			/* wrong token */
			syn_eh(pr_token_code);
		}
		break;
	case LOG_OP_T:
		if (pr_token_attribute != lookahead.attribute.log_op) {
			syn_eh(pr_token_code);
		}
		break;
	case ART_OP_T:
		if (pr_token_attribute != lookahead.attribute.arr_op) {
			syn_eh(pr_token_code);
		}
		break;
	case  REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.rel_op) {
			syn_eh(pr_token_code);
		}
		break;
	}

	/* check if successfull and SEOF_T /*/
	if (lookahead.code == SEOF_T)
		return;
	else {
		lookahead = malar_next_token();
		if (lookahead.code == ERR_T) {
			syn_printe();
			malar_next_token();
			synerrno++;
			return;
		}
		else {
			syn_eh(pr_token_code);
		}
	}
}

/*
* Purpose: Panic mode on parser error
* Called Functions: syn_printe(), malar_next_token(), exit(),
* Parameters: sync_token_code
* Return value: None
* Algorithm:
*/
void syn_eh(int sync_token_code) {
	/* print msg */
	syn_printe();
	/* error counter ++ */
	synerrno++;

	/* panic mode begins */
	do {
		/* advance token until token code matching the one required my parser */
		lookahead = malar_next_token();

		/* Check for SEOF as the token, or else error */
		if (sync_token_code == SEOF_T) {
			exit(synerrno);
			return;
		}
		/* match token */
		if (sync_token_code == lookahead.code) {
			/* next token */
			lookahead = malar_next_token();
			return;
		}
	} while (sync_token_code != lookahead.code);
}

/*
* Purpose: Display string
* Called Functions: None
* Parameters: None
* Return value: None
* Algorithm:
*/ 
void gen_incode(char* s) {
	printf("%s\n", s);
}

/*
* Purpose: Contains PLATYPUS program to start matching
* Called Functions: match(), opt_statements(), gen_incode()
* Parameters: None
* Return value: None
* Algorithm:
*/
void program(void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
* Called Functions: statements(), gen_incode()
*/
/* FIRST(<opt_statements>) = {AVID_T,SVID_T,KW_T,e}*/
void opt_statements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF ||
			lookahead.attribute.get_int == WHILE ||
			lookahead.attribute.get_int == READ ||
			lookahead.attribute.get_int == WRITE)
		{
			statements();
			break;
		}
	default: /*empty string - optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*
* Called Functions: statement(), statement_p()
*/
/* FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}*/
void statements(void) {
	statement();
	statement_p();
}

/*
* Called Functions: statement(), statements_p()
*/
/* FIRST(<statements_p>) -> {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e} */
void statements_p(void) {
	if (lookahead.code == AVID_T || lookahead.code == SVID_T) {
		statement();
		statements_p();
	}
	else if (lookahead.code == KW_T) {
		if (
			lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != ELSE
			) 
		{
			statements();
		}
	}
}

/*
* Called Functions: assignent_statement(), selection_statement(), input_statement(), iteration_statement(), selection_statement(), syn_printe()
*/
/* FIRST(<statement>)  = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)} */
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		if (lookahead.attribute.get_int == IF)
			selection_statement();
		else if (lookahead.attribute.get_int == WHILE)
			iteration_statement();
		else if (lookahead.attribute.get_int == READ)
			input_statement();
		else if (lookahead.attribute.get_int == WRITE)
	default:
		syn_printe();
	}
}

/*
* Called Functions: assignment_expression(), match(), gen_incode()
*/
/* FIRST(<assignment statement>) -> {AVID_T, SVID_T} */
void assignment_statement(void) {
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
* Called Functions: match()
*/
/* FIRST(<assignment expression>) -> {AVID_T, SVID_T} */
void assignment_expression(void) {
	if (lookahead.code == AVID_T) {
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
	}
	else if (lookahead.code == SVID_T) {
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
	}
	else {
		syn_printe();
	}
}
/* FIRST(<arithmetic expression) -> {+,-, e, AVID_T, FPL_T, INL_T, (} */
void arithmetic_expression(void) {
	if (lookahead.code == ART_OP_T) {
		if (
			lookahead.attribute.arr_op == PLUS ||
			lookahead.attribute.arr_op == MINUS
			)
		{
			unary_arithmetic_expression();
		}
		else {
			syn_printe();
		}
	}
	else if (
		lookahead.code == AVID_T ||
		lookahead.code == FPL_T ||
		lookahead.code == INL_T ||
		lookahead.code == LPR_T
		)
	{
		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
	}
	else {
		syn_printe();
	}
}
/* FIRST(<unary arithmetic expression> -> {-,+} */
void unary_arithmetic_expression(void) {
	if (lookahead.code == ART_OP_T) {
		switch (lookahead.attribute.arr_op) {
		case MINUS:
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
			break;
		case PLUS:
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
			break;
		default:
			syn_printe();
			break;
		}
	}
	else {
		gen_incode("PLATY: Unary arithmetic expression parsed");
	}
}
/* FIRST(<additive arithmetic expression>) -> {+,-, e, AVID_T, FPL_T, INL_T, (}  */
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}
/* FIRST(<additive arithmetic expression_p>) -> {+,-, e} */
void additive_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T) {
		switch (lookahead.attribute.arr_op) {
		case MINUS:
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		case PLUS:
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
			syn_printe();
			break;
		}
	}
}
/* FIRST(<multiplicative arithmetic expression>) -> {AVID_T, FPL_T, INL_T, (, *, /, e} */
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}
/* FIRST(<multiplicative arithmetic expression_p>) -> {*,/,e} */
void multiplicative_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T) {
		switch (lookahead.attribute.arr_op) {
		case DIV:
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;

		case MULT:
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	}
}
/* FIRST(<primary arithmetic expression>) -> {AVID_T, FPL_T, INL_T, (} */
void primary_arithmetic_expression(void) {
	if (
		lookahead.code == AVID_T ||
		lookahead.code == FPL_T ||
		lookahead.code == INL_T
		)
	{
		match(lookahead.code, NO_ATTR);
	}
	else if (lookahead.code == LPR_T) {
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}
/* FIRST(<string expression>) -> { SVID_T, STR_T, <<, e} */
void string_expression(void) {
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed");
}
/* FIRST(<string expression_p> ->  {<<, e } */
void string_expression_p(void) {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
	}
}
/* FIRST(<primary string expression> ->  {SVID_T, STR_T} */
void primary_string_expression(void) {
	if (lookahead.code == SVID_T) {
		match(SVID_T, NO_ATTR);
	}
	else if (lookahead.code == STR_T) {
		match(STR_T, NO_ATTR);
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/* FIRST(<selection statement>) = KW_T(IF) */
void selection_statement(void) {
	match(KW_T, IF);
	match(KW_T, TRUE);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/* FIRST(<conditional expression>) -> {AVID_T, FPL_T, INL_T, SVID_T} */
void conditional_expression(void) {
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/* FIRST(<logical OR expression>) -> {AVID_T, FPL_T, INL_T, SVID_T} */
void logical_or_expression(void) {
	logical_and_expression();
	logical_or_expression_p();
}

/* FIRST(<logical OR expression_p)> -> {.OR., e } */
void logical_or_expression_p(void) {
	if (lookahead.code == LOG_OP_T) {
		if (lookahead.attribute.log_op == OR) {
			match(LOG_OP_T, OR);
			logical_and_expression();
			logical_or_expression_p();
			gen_incode("PLATY: Logical OR expression parsed");
		}
	}
}

/*  FIRST(<logical AND expression>) ->  { AVID_T, FPL_T, INL_T, SVID_T} */
void logical_and_expression(void) {
	relational_expression();
	logical_and_expression_p();
}

/* FIRST(<logical AND expression_p>) -> {.AND., e } */
void logical_and_expression_p(void) {
	if (lookahead.code == LOG_OP_T) {
		if (lookahead.attribute.log_op == AND) {
			match(LOG_OP_T, AND);
			relational_expression();
			logical_and_expression_p();
			gen_incode("PLATY: Logical AND expression parsed");
		}
	}
}

/* FIRST(<relational expression>) -> {AVID_T, FPL_T, INL_T, STR_T, SVID_T} */
void relational_expression(void) {
	if
		(
			lookahead.code == AVID_T ||
			lookahead.code == FPL_T ||
			lookahead.code == INL_T
			)
	{
		primary_a_relational_expression();
		primary_a_relational_expression_p();
	}
	else if
		(
			lookahead.code == SVID_T ||
			lookahead.code == STR_T
			)
	{
		primary_s_relational_expression();
		primary_s_relational_expression_p();
	}
	else {
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}

/* FIRST(<primary a_relational expression) -> {AVID_T, FPL_T, INL_T} */
void primary_a_relational_expression(void) {
	if
		(
			lookahead.code == AVID_T ||
			lookahead.code == FPL_T ||
			lookahead.code == INL_T
			)
	{
		match(lookahead.code, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
	}
	else if
		(
			lookahead.code == SVID_T ||
			lookahead.code == STR_T
			)
	{
		syn_printe();
		gen_incode("PLATY: Primary a_relational expression parsed");
	}
	else {
		syn_printe();
	}
}

/* FIRST(<primary a_relational expression_p) -> {==, <>, >, <} */
void primary_a_relational_expression_p(void) {
	if (lookahead.code == REL_OP_T) {
		if
			(
				lookahead.attribute.rel_op == EQ ||
				lookahead.attribute.rel_op == NE ||
				lookahead.attribute.rel_op == GT ||
				lookahead.attribute.rel_op == LT
				)
		{
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_a_relational_expression();
		}
		else {
			syn_printe();
		}
	}
}

/* FIRST(<primary s_relational expression) -> {STR_T, SVID_T} */
void primary_s_relational_expression(void) {
	if (
		lookahead.code == STR_T ||
		lookahead.code == SVID_T
		)
	{
		primary_string_expression();
		gen_incode("PLATY: Primary s_relational expression parsed");
	}
	else if
		(
			lookahead.code == AVID_T ||
			lookahead.code == FPL_T ||
			lookahead.code == INL_T
			)
	{
		syn_printe();
		gen_incode("PLATY: Primary s_relational expression parsed");
	}
}

/* FIRST(<primary s_relational expression_p) -> {==, <>, >, <} */
void primary_s_relational_expression_p(void) {
	if (lookahead.code == REL_OP_T) {
		if
			(
				lookahead.attribute.rel_op == EQ ||
				lookahead.attribute.rel_op == NE ||
				lookahead.attribute.rel_op == GT ||
				lookahead.attribute.rel_op == LT
				)
		{
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_s_relational_expression();
		}
		else {
			syn_printe();
		}
	}
}

/* FIRST(<iteration statement>) = KW_T(WHILE) */
void iteration_statement(void) {
	match(KW_T, WHILE);
	match(KW_T, TRUE);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/* FIRST(<iteration statement>) = KW_T(WHILE) */
void input_statement(void) {
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/* FIRST(<variable list>) = {AVID_T, SVID_T} */
void variable_list(void) {
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}

/* FIRST(<variable list_p>) = {AVID_T, SVID_T,COM_T , e} */
void variable_list_p(void) {
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
	}
}

/* FIRST(<variable identifier>) =  {AVID_T, SVID_T}  */
void variable_identifier(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/* FIRST(<output statement>) = KW_T(WRITE) */
void output_statement(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/* FIRST(<output list>) = {AVID_T, SVID_T, STR_T, e} */
void output_list(void) {
	if (
		lookahead.code == AVID_T ||
		lookahead.code == SVID_T
		)
	{
		variable_list();
	}
	else if (lookahead.code == STR_T) {
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
	}
	else {
		gen_incode("PLATY: Output list (empty) parsed");
	}
}


/*
* Purpose: Error messages are displayed through t.code
* Called Functions: None
* Parameters: None
* Return value: None
*/
/* error printing function for Assignment 3 (Parser), W19 */
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/