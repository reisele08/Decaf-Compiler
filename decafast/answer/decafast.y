%{
#include <iostream>
#include <ostream>
#include <string>
#include <cstdlib>
#include "default-defs.h"

int yylex(void);
int yyerror(char *); 

// print AST?
bool printAST = true;

#include "decafast.cc"

using namespace std;

%}

%define parse.error verbose

%union{
    class decafAST *ast;
    std::string *sval;
 }

%token T_FUNC
%token T_INTTYPE
%token T_PACKAGE
%token T_BOOLTYPE
%token T_BREAK
%token T_CONTINUE
%token T_STRINGTYPE
%token T_TRUE
%token T_VAR
%token T_VOID
%token T_WHILE
%token T_ELSE
%token T_FALSE
%token T_FOR
%token T_RETURN
%token T_NULL
%token T_IF
%token T_EXTERN
%token T_INTCONSTANT
%token T_LCB
%token T_RCB
%token T_LPAREN
%token T_RPAREN
%token <sval> T_ID
%token T_WHITESPACE
%token T_ASSIGN
%token <sval> T_CHARCONSTANT
%token T_COMMA
%token T_COMMENT
%token T_DOT
%token T_LSB
%token T_MINUS
%token T_PLUS
%token T_MOD
%token T_NOT
%token T_RSB
%token T_SEMICOLON
%token T_EQ
%token <sval> T_STRINGCONSTANT
%token T_NEQ
%token T_LEQ
%token T_LT
%token T_GT
%token T_GEQ
%token T_AND
%token T_OR
%token T_MULT
%token T_DIV
%token T_LEFTSHIFT
%token T_RIGHTSHIFT

 /*precedence bottom to top */
%left T_AND
%left T_OR
%left T_EQ T_NEQ T_LT T_LEQ T_GT T_GEQ
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%left T_UNOT
%left T_UMINUS

%type <ast> extern_list decafpackage

%%

start: program

program: extern_list decafpackage
    { 
        ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2); 
		if (printAST) {
			cout << getString(prog) << endl;
		}
        delete prog;
    }

extern_list: /* extern_list can be empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

decafpackage: T_PACKAGE T_ID T_LCB T_RCB
    { $$ = new PackageAST(*$2, new decafStmtList(), new decafStmtList()); delete $2; }
    ;

%%

int main() {
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}

