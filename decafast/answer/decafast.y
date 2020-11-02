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
    std::vector<string> *ptr;
 }

%token T_FUNC
%token T_INTTYPE
%token T_PACKAGE
%token T_BOOLTYPE
%token T_BREAK
%token T_CONTINUE
%token T_STRINGTYPE
%token <sval> T_TRUE
%token T_VAR
%token T_VOID
%token T_WHILE
%token T_ELSE
%token <sval> T_FALSE
%token T_FOR
%token T_RETURN
%token T_NULL
%token T_IF
%token T_EXTERN
%token T_SEMICOLON
%token T_LCB
%token T_RCB
%token T_LPAREN
%token T_RPAREN
%token T_WHITESPACE
%token T_ASSIGN
%token T_COMMA
%token T_COMMENT
%token T_DOT
%token T_LSB
%token T_RSB

%token <sval> T_NOT
%token <sval> T_MINUS
%token <sval> T_MOD
%token <sval> T_CHARCONSTANT
%token <sval> T_ID
%token <sval> T_INTCONSTANT
%token <sval> T_PLUS
%token <sval> T_EQ
%token <sval> T_STRINGCONSTANT
%token <sval> T_NEQ
%token <sval> T_LEQ
%token <sval> T_LT
%token <sval> T_GT
%token <sval> T_GEQ
%token <sval> T_AND
%token <sval> T_OR
%token <sval> T_MULT
%token <sval> T_DIV
%token <sval> T_LEFTSHIFT
%token <sval> T_RIGHTSHIFT

 /*precedence bottom to top */
%left T_OR
%left T_AND
%left T_EQ T_NEQ T_LT T_LEQ T_GT T_GEQ
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%left T_UNOT
%left T_UMINUS

%type <ast> extern_defn extern_type_list
%type <ptr> id_list
%type <ast> extern_list decafpackage var_decl id_type_list rvalue method_args
%type <ast> field_list field_decl method_list method_decl typed_symbol
%type <ast> method_block var_decl_list statement_list assign expr method_arg_list
%type <ast> method_call if_stmt while_stmt for_stmt return_stmt break_stmt continue_stmt block method_arg
%type <ast> assign_list statement constant param_list expr_opt
%type <sval> decaf_type method_type bool_constant extern_type

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
    ;
extern_list:
    {
        $$ = NULL;
    }
    | extern_defn extern_list
    {
        decafStmtList *slist;
        if($2 != NULL){
            slist = (decafStmtList*)$2;
        }
        else if($2 == NULL){
            slist = new decafStmtList();
        }
        slist->push_front($1);
        $$ = slist;
    }
    ;

extern_defn: T_EXTERN T_FUNC T_ID T_LPAREN extern_type_list T_RPAREN method_type T_SEMICOLON
    {           
        $$ = new externAST(*$3, *$7,(decafStmtList*)$5);
        delete $3, $7;
    }
    ;
    /* NEEDS 'EMPTY' DEF */
extern_type_list: extern_type T_COMMA extern_type_list
    {
        decafStmtList* slist = (decafStmtList*)$3;
        varDefAST* varDef = new varDefAST(string("extern"), *$1);
        slist->push_front(varDef);
        $$ = slist;
        delete $1;
    } 
    | extern_type
    {
        decafStmtList* slist = new decafStmtList();
        varDefAST* varDef = new varDefAST(string("extern"),*$1);
        slist->push_front(varDef);
        $$ = slist;
        delete $1;     
	}
    ;

decafpackage: T_PACKAGE T_ID T_LCB field_list method_list T_RCB
    { 
        //$$ = new PackageAST(*$2, new decafStmtList(), new decafStmtList()); 
        $$ = new PackageAST(*$2,(decafStmtList*)$4,(decafStmtList*)$5); 
        delete $2; 
    }
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


extern_type: T_STRINGTYPE
    {
        $$ = new string("StringType");
    }
    | decaf_type
    {
        $$ = $1;
    }
    ;

field_list:
    {
        $$ = NULL;
    }
    | field_decl field_list
    {
        decafStmtList* list;
        if($2){
            list = (decafStmtList*)$2;
        }
        else if($2 == NULL){
            list = new decafStmtList();
        }
        list->push_front($1);
        $$ = list;
    }
    ;

    /* missing some defns */
field_decl: T_VAR id_list decaf_type T_SEMICOLON
    {
        decafStmtList* slist = new decafStmtList();
        
        for(int i = 0; i < $2->size(); i++ ){
            fieldAST* field = new fieldAST((*$2)[i], *$3, "Scalar");
            slist->push_front(field);
        }
        delete $2, $3;
        $$ = slist;
    }
    ;

    /* PROBABLY GOOD */
id_list: T_ID T_COMMA id_list
    {
        vector<string>* idList = $3;
        idList->push_back(*$1);
        delete $1;
        $$ = idList;
    }
    | T_ID
    {
        vector<string>* idList = new vector<string>; 
        idList->push_back(*$1);
        delete $1;
        $$ = idList;
    }
    ;

    /* PROBABLY GOOD */
method_decl: T_FUNC T_ID T_LPAREN param_list T_RPAREN method_type method_block
    {
        methodAST* method;
        string Type = *$6;
        decafStmtList* slist = new decafStmtList();
        method = new methodAST((*$2), *$6, (decafStmtList*)$4, (methodBlockAST*)$7);
        slist->push_back(method);
        delete $2, $6;
        $$ = slist;
     }
     ; 

    /* PROBABLY GOOD */
method_list:
    {
        $$ = NULL;
    }
    | method_decl method_list
    {
        decafStmtList* slist;
        if($2 != NULL){
            slist = (decafStmtList*)$2;
        }
        else if($2 == NULL){
            slist = new decafStmtList();
        }
        slist->push_front($1);
        $$ = slist;
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

    /* PROBABLY GOOD */
id_type_list: T_ID decaf_type T_COMMA id_type_list
    {
        varDefAST* varDef = new varDefAST(*$1, *$2);
        ((decafStmtList*)$4)->push_front(varDef);
        $$ = $4;
        delete $1, $2;
    }
    | T_ID decaf_type
    {
        decafStmtList* slist = new decafStmtList();
        varDefAST* varDef = new varDefAST(*$1, *$2);
        slist->push_front(varDef);

        delete $1, $2;
        $$ = slist;
    }   
    ;

param_list:
    {
        $$ = NULL;
    }
    | id_type_list
    {
        $$ = $1;
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

method_block: T_LCB var_decl_list statement_list T_RCB
    {
        $$ = new methodBlockAST((decafStmtList*)$2, (decafStmtList*)$3);
    }
    ;

block: T_LCB var_decl_list statement_list T_RCB
    {
        $$ = new blockAST((decafStmtList*)$2, (decafStmtList*)$3);
    }
    ;

statement: assign T_SEMICOLON
    {
       $$ = $1; 
    }
    | method_call T_SEMICOLON
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
    | return_stmt T_SEMICOLON
    {
        $$ = $1;
    }
    | break_stmt T_SEMICOLON
    {
        $$ = $1;
    }
    | continue_stmt T_SEMICOLON
    {
        $$ = $1;
    }
    | block
    {
        $$ = $1;
    }
    ;
    /* if, while, for, ret, brk, cont PROBABLY GOOD */
if_stmt: T_IF T_LPAREN expr T_RPAREN block
    {
        $$ = new ifStmtAST($3, (blockAST*)$5, NULL);
    }
    | T_IF T_LPAREN expr T_RPAREN block T_ELSE block
    {
        $$ = new ifStmtAST($3, (blockAST*)$5, (blockAST*)$7);
    }
    ;
while_stmt: T_WHILE T_LPAREN expr T_RPAREN block
    {
        $$ = new whileStmtAST($3, (blockAST*)$5);
    }
    ;
for_stmt: T_FOR T_LPAREN assign_list T_SEMICOLON expr T_SEMICOLON assign_list T_RPAREN block
    {
        $$ = new forStmtAST((assignVarAST*)$3, $5, (assignVarAST*)$7, (blockAST*)$9);
    }
    ; 

return_stmt: T_RETURN
    {
        $$ = new returnStmtAST(NULL);
    }
    | T_RETURN T_LPAREN expr_opt T_RPAREN
    {
        $$ = new returnStmtAST($3);
    }
    ;
expr_opt:
    { 
        $$ = NULL; 
    } 
    | expr
    { 
        $$ = $1;
    } 
    ;
break_stmt: T_BREAK
    {
        $$ = new breakStmtAST();
    }
    ;
continue_stmt: T_CONTINUE
    {
        $$ = new continueStmtAST();
    }
    ;

    /* PROBABLY GOOD */
assign: T_ID T_ASSIGN expr
    {
        $$ = new assignVarAST(*$1, $3);
        delete $1;
    }
    | T_ID T_LSB expr T_RSB T_ASSIGN expr
    {
        $$ = new assignArrayLocAST(*$1, $3, $6);
        delete $1;
    }
    ;
assign_list: assign assign_list
    {
        decafStmtList* slist = (decafStmtList*)$2;
        slist->push_front($1);
        $$ = slist;
    }
    | assign
    {
        decafStmtList* slist = new decafStmtList();
        slist->push_front($1);
        $$ = slist;
    }
    ;
    /* PROBABLY GOOD (method calls,args, etc)*/
method_call: T_ID T_LPAREN method_args T_RPAREN
    {
        $$ = new methodCallAST(*$1, $3);
        delete $1;
    }
    ;
method_arg: T_STRINGCONSTANT
    {
        $$ = new numberExprAST("str",*$1);
        delete $1;
    }
    | expr
    {
        $$ = $1;
    }
    ;
method_args:
    { 
        $$ = NULL;
    } 
    | method_arg_list
    { 
        $$ = $1; 
    }
    ;
method_arg_list: method_arg T_COMMA method_arg_list
    {
        decafStmtList* slist = (decafStmtList*)$3;
        slist -> push_front($1);
        $$ = slist;
    }
    | method_arg
    {
        decafStmtList* slist = new decafStmtList();
        slist->push_front($1);
        $$ = slist;
    }
    ;


expr: rvalue
    {
        $$ = $1;
    }
    | method_call
    {
        $$ = $1;
    }
    | constant
    {
        $$ = $1;
    }
    | expr T_PLUS expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_MINUS expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_MULT expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_DIV expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_RIGHTSHIFT expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_LEFTSHIFT expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_EQ expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_NEQ expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_LEQ expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_MOD expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_LT expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_GT expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_AND expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_GEQ expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | expr T_OR expr
    {
        $$ = new binaryExprAST(*$2, (decafStmtList*)$1, (decafStmtList*)$3);
        delete $2;
    }
    | T_NOT expr %prec T_UNOT
    {
        $$ = new unaryExprAST(*$1, (decafStmtList*)$2);
        delete $1;
    }
    | T_MINUS expr %prec T_UMINUS
    {
        $$ = new unaryExprAST(string("UnaryMinus"), (decafStmtList*)$2);
    }
    | T_LPAREN expr T_RPAREN
    {
        $$ = $2;
    }
    ;

rvalue: T_ID T_LSB expr T_RSB
    {
        $$ = new arrayLocExprAST(*$1, (decafStmtList*) $3);
        delete $1;
    }
    | T_ID
    {
        $$ = new variableExprAST(*$1);
        delete $1;
    }
    ; 

constant: T_INTCONSTANT
    {
        $$ = new numberExprAST(string("int"), *$1);
        delete $1;
    }
    | T_CHARCONSTANT
    {
        $$ = new numberExprAST(string("int"), convertASCII(*$1));
        delete $1;

    }   
    | bool_constant
    {
        $$ = new boolExprAST(*$1);
        delete $1;
    }
    ; 

bool_constant: T_TRUE
    {
        $$ = new string("True");
    }
    | T_FALSE
    {
        $$ = new string("False");
    }
    ;

statement_list: 
    {
        $$ = NULL;
    }
    | statement statement_list
    {
        decafStmtList* slist;
        if($2 != NULL){
            slist = (decafStmtList*)$2;
        }
        else if($2 == NULL){
            slist = new decafStmtList();
        }
        slist->push_front($1);
        $$ = slist;
    }
    ;

    /* PROBABLY GOOD */
var_decl_list:
    {
        $$ = NULL;
    }
    | var_decl var_decl_list
    {
        decafStmtList* slist;
        if($2 != NULL){
            slist = (decafStmtList*)$2;
        }
        else if($2 == NULL){
            slist = new decafStmtList();
        }
        slist->push_front($1);
        $$ = slist;
    }
    ;

var_decl: T_VAR id_list decaf_type T_SEMICOLON
    {
        decafStmtList* slist = new decafStmtList();
        
        for(int i = 0; i < $2->size(); i++){
            varDefAST* varDef = new varDefAST((*$2)[i], *$3);
            slist->push_front(varDef);
        }
        delete $2, $3;
        $$ = slist;
    }
    ;

%%

int main() {
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}

