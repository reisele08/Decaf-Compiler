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
%type <ast> decaf_type method_type field_list field_decl method_list method_decl typed_symbol
%type <ast> method_block var_decl_list statement_list assign extern_type expr method_arg_list
%type <ast> method_call if_stmt while_stmt for_stmt return_stmt break_stmt continue_stmt block method_arg
%type <ast> bool_constant bool assign_list statement
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

decaf_type: T_INTTYPE
    {
        $$ = new string("IntType");
    }
    | T_BOOLTYPE
    {
        $$ = new string("BoolType");
    }
    ;

method_type: T_VOID
    {
        $$ = new string("VoidType");
    }
    | decaf_type
    {
        $$ = $1;
    }
    ;

extern_type: T_STRINGTYPE
    {
        $$ = new string("StringType");
    }
    | decaf_type
    {
        $$ = $1;
    }
    ;

package: T_PACKAGE T_ID T_LCB field_list method_list T_RCB
    {
        /* stub */
    }
    ;

field_list:
    {
        /* stub */
    }
    ;

field_decl:
    {
        /* stub */
    }
    ;
field_size:
    {
        /* stub */
    }
    ;

method_list:
    {
        /* stub */
    }
    ;
method_decl:
    {
        /* stub */
    }
    ;
typed_symbol: T_ID
    {
        $$ = new string("identifier name");
    }
    | decaf_type
    {
        $$ = $1;
    }
    ;

method_block: var_decl_list statement_list
    {
        /* stub */
    }
    ;

block: var_decl_list statement_list
    {
        /* stub */
    }
    ;

statement: assign
    {
       $$ = $1; 
    }
    | method_call
    {
        $$ = $1;
    }
    | if_stmt
    {
        $$ = $1;
    }
    | while_stmt
    {
        $$ = $1;
    }
    | for_stmt
    {
        $$ = $1;
    }
    | return_stmt
    {
        $$ = $1;
    }
    | break_stmt
    {
        $$ = $1;
    }
    | continue_stmt
    {
        $$ = $1;
    }
    | block
    {
        $$ = $1;
    }
    ;

if_stmt: T_IF T_LPAREN expr T_RPAREN block
    {
        /* stub */
    }
    | T_IF T_LPAREN expr T_RPAREN block T_ELSE block
    {
        /* stub */
    }
    ;
while_stmt: T_WHILE T_LPAREN expr T_RPAREN block
    {
        /* stub */
    }
    ;
for_stmt: T_FOR T_LPAREN assign_list T_SEMICOLON expr T_SEMICOLON assign_list T_RPAREN block
    {
        /* stub */
    }
    ;
return_stmt: T_RETURN T_SEMICOLON
    {
        /* stub */
    }
    ;
break_stmt: T_BREAK T_SEMICOLON
    {
        /* stub */
    }
    ;
continue_stmt: T_CONTINUE T_SEMICOLON
    {
        /* stub */
    }
    ;

assign: T_ID T_ASSIGN expr
    {
        /* stub */
    }
assign_list: assign assign_list
    {
        /* stub */
    }
    | assign
    {
        /* stub */
    }
    ;
method_call: T_ID T_LPAREN T_RPAREN
    {
        /* stub */
    }
    | T_ID T_LPAREN method_arg_list T_RPAREN
    {
        /* stub */
    }
    ;
method_arg: T_STRINGCONSTANT
    {
        /* stub */
    }
    | expr
    {
        $$ = $1;
    }
    ;

rvalue: T_ID T_LSB expr T_RSB
    {
        /* stub */
    }
    | T_ID
    {
        /* stub */
    }
    ;
expr: rvalue
    {
        /* stub */
    }
    ;
constant: T_INTCONSTANT
    {
        /* stub */
    }
    | T_CHARCONSTANT
    {
        /* stub */
    }
    | bool_constant
    {
        /* stub */
    }
    ;
bool: T_TRUE
    {
        $$ = new string("True");
    }
    | T_FALSE
    {
        $$ = new string("False");
    }
    ;

bool_constant: T_TRUE
    {
        /* stub */
    }
    ;

method_arg_list: 
    {
        /* stub */
    }
    ;

statement_list: 
    {
        $$ = NULL;
    }
    | statement statement_list
    {
        /* stub */
    }
    ;

var_decl_list:
    {
        $$ = NULL;
    }
    | var_decl var_decl_list
    {
        /* stub */
    }
    ;
var_decl: decaf_type T_ID T_SEMICOLON
    {
        /* stub */
    }
    ;
%%

int main() {
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}

