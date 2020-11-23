%{
#include <iostream>
#include <ostream>
#include <string>
#include <cstdlib>
#include "default-defs.h"

int yylex(void);
int yyerror(char *); 

// print AST?
bool printAST = false;

using namespace std;

// this global variable contains all the generated code
static llvm::Module *TheModule;

// this is the method used to construct the LLVM intermediate code (IR)
static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
// the calls to TheContext in the init above and in the
// following code ensures that we are incrementally generating
// instructions in the right order

// dummy main function
// WARNING: this is not how you should implement code generation
// for the main function!
// You should write the codegen for the main method as 
// part of the codegen for method declarations (MethodDecl)
// static llvm::Function *TheFunction = 0;

// // we have to create a main function 
// llvm::Function *gen_main_def() {
//   // create the top-level definition for main
//   llvm::FunctionType *FT = llvm::FunctionType::get(llvm::IntegerType::get(TheContext, 32), false);
//   llvm::Function *TheFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", TheModule);
//   if (TheFunction == 0) {
//     throw runtime_error("empty function block"); 
//   }
//   // Create a new basic block which contains a sequence of LLVM instructions
//   llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
//   // All subsequent calls to IRBuilder will place instructions in this location
//   Builder.SetInsertPoint(BB);
//   return TheFunction;
// }

#include "decafcomp.cc"

%}

%union{
    class decafAST *ast;
    std::string *sval;
    std::vector<string> *ptr;
    arrayinfo info;
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

%type <ptr> id_list
%type <ast> extern_defn extern_type_list
%type <ast> extern_list decafpackage var_decl id_type_list rvalue method_args
%type <ast> field_list field_decl method_list method_decl typed_symbol
%type <ast> method_block var_decl_list statement_list assign expr method_arg_list
%type <ast> method_call if_stmt while_stmt for_stmt return_stmt break_stmt continue_stmt block method_arg
%type <ast> assign_list statement constant param_list expr_opt
%type <sval> decaf_type method_type bool_constant extern_type
%type <info> arraytype

%%

start: program

program: extern_list decafpackage
    { 
    ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2); 
    if (printAST) {
      cout << getString(prog) << endl;
    }
        try {
            prog->Codegen();
        } 
        catch (std::runtime_error &e) {
            cout << "semantic error: " << e.what() << endl;
            //cout << prog->str() << endl; 
            exit(EXIT_FAILURE);
        }
        delete prog;
    }
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

extern_type_list: extern_type T_COMMA extern_type_list
    {
        decafStmtList* slist = (decafStmtList*)$3;
        varDefAST* varDef = new varDefAST(true, string("extern"), *$1);
        slist->push_front(varDef);
        $$ = slist;
        delete $1;
    } 
    | extern_type
    {
        decafStmtList* slist = new decafStmtList();
        varDefAST* varDef = new varDefAST(true, string("extern"),*$1);
        slist->push_front(varDef);
        $$ = slist;
        delete $1;     
  }
    | 
    {
        decafStmtList* slist = new decafStmtList();
        varDefAST* varDef = new varDefAST(true, string("extern"), string("extern"));
        slist->push_front(varDef);
        $$ = slist;
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

ignore: ignore T_ID
    | ignore T_LCB
    | ignore T_RCB
  | T_ID
  | T_LCB
  | T_RCB
  ;
var_decl: T_VAR id_list decaf_type T_SEMICOLON
    {
        decafStmtList* slist = new decafStmtList();
        
        for(int i = 0; i < $2->size(); i++){
            varDefAST* varDef = new varDefAST(false, (*$2)[i], *$3);
            slist->push_front(varDef);
        }
        delete $2, $3;
        $$ = slist;
    }
    ;

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

begin_block: T_LCB
    {
    symtbl.push_front(symbol_table());
    };

end_block: T_RCB
    {
    symbol_table symb_table = symtbl.front();
    //freeDesc(symb_table);
    symtbl.pop_front();
    };

decafpackage: T_PACKAGE T_ID begin_block field_list method_list end_block
    { 
        $$ = new PackageAST(*$2,(decafStmtList*)$4,(decafStmtList*)$5); 
        delete $2; 
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

method_type: T_VOID
    {
       $$ = new string("VoidType");
    }
    | decaf_type
    {
        $$ = $1;
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
method_block: T_LCB var_decl_list statement_list T_RCB
    {
        $$ = new methodBlockAST((decafStmtList*)$2, (decafStmtList*)$3);
    }
    ;
block: begin_block var_decl_list statement_list end_block
    {
        $$ = new blockAST((decafStmtList*)$2, (decafStmtList*)$3);
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
method_decl: T_FUNC T_ID T_LPAREN param_list T_RPAREN method_type method_block
    {
        methodAST* method;
        string Type; 
        Type = *$6;
        decafStmtList* slist = new decafStmtList();
        method = new methodAST((*$2), Type, (decafStmtList*)$4, (methodBlockAST*)$7);
        delete $2;
        delete $6;
        $$ = (decafAST*)method;
     }
     ; 

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
    | T_VAR id_list decaf_type T_ASSIGN expr T_SEMICOLON
    {
        decafStmtList* slist = new decafStmtList();
        
        if($2->size() > 1){
            return 1;
        }

        assignGlobalVarAST* global = new assignGlobalVarAST((*$2)[0],*$3, $5);
        slist->push_front(global);

        delete $2, $3;
        $$ = slist;
    }
    | T_VAR id_list arraytype T_SEMICOLON
    {
        decafStmtList *slist = new decafStmtList();

        string type = *($3.array_type);
        string size = string("Array(") + *($3.array_size) + ")";

        for(int i = 0; i < $2->size(); i++){
            fieldAST *field = new fieldAST((*$2)[i], type, size);
            slist->push_front(field);
        }

        delete $2, $3.array_size, $3.array_type;
        $$ = slist;
    }
    ;
arraytype: T_LSB T_INTCONSTANT T_RSB decaf_type
    {
        arrayinfo info;
        info.array_size = $2;
        info.array_type = $4;
        $$ = info;
    }
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

method_call: T_ID T_LPAREN method_args T_RPAREN
    {
        $$ = new methodCallAST(*$1, (decafStmtList*)$3);
        delete $1;
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
        // $$ = new arrayLocExprAST(*$1, (decafStmtList*) $3);
        // delete $1;
        $$ = new arrayLocExprAST(*$1, (decafStmtList*) $3);
        delete $1;

    }
    | T_ID
    {
        // $$ = new variableExprAST(*$1);
        // delete $1;
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
        // $$ = new numberExprAST(string("int"), convertASCII(*$1));
        // delete $1;
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
id_type_list: T_ID decaf_type T_COMMA id_type_list
    {
        varDefAST* varDef = new varDefAST(true, *$1, *$2);
        ((decafStmtList*)$4)->push_front(varDef);
        $$ = $4;
        delete $1, $2;
    }
    | T_ID decaf_type
    {
        decafStmtList* slist = new decafStmtList();
        varDefAST* varDef = new varDefAST(true, *$1, *$2);
        slist->push_front(varDef);

        delete $1, $2;
        $$ = slist;
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

%%

int main() {
  // initialize LLVM
  llvm::LLVMContext &Context = TheContext;
  // Make the module, which holds all the code.
  TheModule = new llvm::Module("Test", Context);
  // set up symbol table
  symtbl.push_front(symbol_table());
  // set up dummy main function
  //TheFunction = gen_main_def();

  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  // remove symbol table
  // Finish off the main function. (see the WARNING above)
  // return 0 from main, which is EXIT_SUCCESS
  //Builder.CreateRet(llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0)));
  // Validate the generated code, checking for consistency.
  //verifyFunction(*TheFunction);

  symbol_table sym_table = symtbl.front();
  //freeDesc(sym_table);
  symtbl.pop_front(); 

  // Print out all of the generated code to stderr
  TheModule->print(llvm::errs(), nullptr);
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}
