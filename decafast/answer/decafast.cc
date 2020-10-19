
#include "default-defs.h"
#include <list>
#include <ostream>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <list>

#ifndef YYTOKENTYPE
#include "decafast.tab.h"
#endif

using namespace std;

/// decafAST - Base class for all abstract syntax tree nodes.
class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
};

string getString(decafAST *d) {
	if (d != NULL) {
		return d->str();
	} else {
		return string("None");
	}
}

template <class T>
string commaList(list<T> vec) {
    string s("");
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) { 
        s = s + (s.empty() ? string("") : string(",")) + (*i)->str(); 
    }   
    if (s.empty()) {
        s = string("None");
    }   
    return s;
}

/// decafStmtList - List of Decaf statements
class decafStmtList : public decafAST {
	list<decafAST *> stmts;
public:
	decafStmtList() {}
	~decafStmtList() {
		for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) { 
			delete *i;
		}
	}
	int size() { return stmts.size(); }
	void push_front(decafAST *e) { stmts.push_front(e); }
	void push_back(decafAST *e) { stmts.push_back(e); }
	string str() { return commaList<class decafAST *>(stmts); }
};

class PackageAST : public decafAST {
	string Name;
	decafStmtList *FieldDeclList;
	decafStmtList *MethodDeclList;
public:
	PackageAST(string name, decafStmtList *fieldlist, decafStmtList *methodlist) 
		: Name(name), FieldDeclList(fieldlist), MethodDeclList(methodlist) {}
	~PackageAST() { 
		if (FieldDeclList != NULL) { delete FieldDeclList; }
		if (MethodDeclList != NULL) { delete MethodDeclList; }
	}
	string str() { 
		return string("Package") + "(" + Name + "," + getString(FieldDeclList) + "," + getString(MethodDeclList) + ")";
	}
};

class blockAST : public decafAST{
	decafStmtList* Var_Decl_List;
	decafStmtList* Statement_List;
public:
	blockAST(decafStmtList *var_decl_list, decafStmtList *statement_list)
		: Var_Decl_List(var_decl_list), Statement_List(statement_list){}
	~blockAST() { 
		if (Var_Decl_List != NULL) { delete Var_Decl_List; }
		if (Statement_List != NULL) { delete Statement_List; }
	}
	string str(){
		return string("Block") + "(" + getString(Var_Decl_List) + "," + getString(Statement_List) + ")";
	}
};

class methodBlockAST : public decafAST{
	decafStmtList* Var_Decl_List;
	decafStmtList* Statement_List;
public:
	methodBlockAST(decafStmtList *var_decl_list, decafStmtList *statement_list)
		: Var_Decl_List(var_decl_list), Statement_List(statement_list){}
	~methodBlockAST() { 
		if (Var_Decl_List != NULL) { delete Var_Decl_List; }
		if (Statement_List != NULL) { delete Statement_List; }
	}
	string str(){
		return string("MethodBlock") + "(" + getString(Var_Decl_List) + "," + getString(Statement_List) + ")";
	}
};

class ifStmtAST : public decafAST{
	decafAST *Condition;
	blockAST *If_Block;
	blockAST *Else_Block;
public:
	ifStmtAST(decafAST *condition, blockAST *if_block, blockAST *else_block)
		: Condition(condition), If_Block(if_block), Else_Block(else_block){}
	~ifStmtAST() { 
		if (Condition != NULL) { delete Condition; }
		if (If_Block != NULL) { delete If_Block; }
		if (Else_Block != NULL) { delete Else_Block; }
	}
	string str(){
		return string("IfStmt") + "(" + getString(Condition) + "," + getString(If_Block) + "," + getString(Else_Block) + ")";
	}
};

class whileStmtAST : public decafAST{
	decafAST *Condition;
	blockAST *While_Block;
public:
	whileStmtAST(decafAST *condition, blockAST *while_block)
		: Condition(condition), While_Block(while_block){}
	~whileStmtAST() { 
		if (Condition != NULL) { delete Condition; }
		if (While_Block != NULL) { delete While_Block; }
	}
	string str(){
		return string("WhileStmt") + "(" + getString(Condition) + "," + getString(While_Block) + ")";
	}
};

class returnStmtAST : public decafAST{
	decafAST *Return_Value;
public:
	returnStmtAST(decafAST *return_value)
		: Return_Value(return_value){}
	~returnStmtAST() {
		if (Return_Value != NULL) { delete Return_Value; }
	}
	string str(){
		return string("ReturnStmt") + "(" + getString(Return_Value) +")";
	}	
};
class fieldAST : public decafAST{
	string Name;
	string Type;
	string Expression;
public:
	fieldAST(string name, string type, string expression)
		: Name(name), Type(type), Expression(expression){}
	string str(){
		return string("FieldDecl") + "(" + Name + "," + Type + "," + Expression + ")";
	}	
};
class assignGlobalVarAST : public decafAST{
	string Name;
	string Type;
	string Value;
public:
	assignGlobalVarAST(string name, string type, string value)
		: Name(name), Type(type), Value(value){}
	string str(){
		return string("AssignGlobalVar") + "(" + Name + "," + Type + "," + Value +")";
	}		
};

class externAST : public decafAST{
	string Name;
	string Return_Type;
	decafStmtList* Typelist;
public:
	externAST(string name, string return_type, decafStmtList *typelist)
		: Name(name), Return_Type(return_type), Typelist(typelist){}
	~externAST(){
		if (Typelist != NULL) { delete Typelist; }
	}
	string str(){
	return string("ExternFunction") + "(" + Name + "," + Return_Type + "," + getString(Typelist) +")";
	}

};

class varDefAST : public decafAST{
	string Name;
	string Type;
public:
	varDefAST(string name, string type)
		: Name(name), Type(type){}
	string str(){
		//for externs
		if(Name == "extern"){
			return string("VarDef") + "(" + Type + ")";
		}
		else{
	return string("VarDef") + "(" + Name + "," + Type + ")";
		}
	}
};
	/* for escaped chars */
string convertASCII(string s) {

	stringstream stringStream;
	int ASCII;

	if(s[1] != '\\'){
		ASCII = int(s[1]);
	}
	else if(s[2] == 'a'){
		ASCII = 7;
  }
	else if(s[2] == 'b'){
		ASCII = 8;
  }
	else if(s[2] == 't'){
		ASCII = 9;
  }
	else if(s[2] == 'n'){
		ASCII = 10;
  }
	else if(s[2] == 'v'){
		ASCII = 11;
  }
    else if(s[2] == 'f'){
		ASCII = 12;
  }
    else if(s[2] == 'r'){
		ASCII = 13;
  }
    else if(s[2] == '\\'){
		ASCII = 92;
  }
    else if(s[2] == '\''){
		ASCII = 39;
  }
  	else if(s[2] == '\"'){
		ASCII = 34;
  }
	stringStream << ASCII;
	return string(stringStream.str());
}
class numberExprAST : public decafAST{
	string Type;
	string Value;
public:
	numberExprAST(string type, string value)
		: Type(type), Value(value){}
	string str(){
		string Name;
		if(Type == string ("int")){
			Name = "NumberExpr";
		}
		else if(Type == string("str")){
			Name = "StringConstant";
		}
	return Name + "(" + Value + ")";
	}
};

class boolExprAST : public decafAST{
	string Value;
public:
	boolExprAST(string value)
		: Value(value){}
	string str(){
	return string("BoolExpr") + "(" + Value + ")";
	}
};

class methodAST : public decafAST{
	string Name;
	string Return_Type;
	decafStmtList *Param_List;
	methodBlockAST *Block;
public:
	methodAST(string name, string return_type, decafStmtList* param_list, methodBlockAST* block)
		: Name(name), Return_Type(return_type), Param_List(param_list), Block(block){}
	~methodAST(){
		if (Param_List != NULL) { delete Param_List; } 
		if (Block != NULL) { delete Block; }
  	}
  	string str(){
	return string("Method") + "(" + Name + "," + Return_Type + "," + getString(Param_List) + "," + getString(Block) + ")";
	}
};
//VALUE AST
class variableExprAST : public decafAST{
	string Name;
public:
	variableExprAST(string name)
		: Name(name){}
	string getName() { return Name; }
	string str(){
	return string("VariableExpr") + "(" + Name + ")";
	}
};
//VALUE AST
class arrayLocExprAST : public decafAST{
	string Name;
	decafStmtList* Index;
public:
	arrayLocExprAST(string name, decafStmtList* index)
		: Name(name), Index(index){}
	string getName() { return Name; }
	decafStmtList* getIndex() { return Index; }
	string str(){
	return string("ArrayLocExpr") + "(" + Name + "," + getString(Index) + ")";
	}
};


//ASSIGN AST
class assignVarAST : public decafAST{
	string Name;
	decafAST* Value;
public:
	assignVarAST(string name, decafAST* value)
		: Name(name), Value(value){}
	~assignVarAST(){
		if (Value != NULL) { delete Value; } 
		//if (Expression != NULL) { delete Expression; }
	}
	string str(){
	return string("AssignVar") + "(" + Name + "," + getString(Value) + ")";
	}
	
};
//ASSIGN AST
class assignArrayLocAST : public decafAST{
	string Name;
	decafAST* Index;
	decafAST* Value;
public:
	assignArrayLocAST(string name, decafAST *index, decafAST *value)
		: Name(name), Index(index), Value(value){}
	~assignArrayLocAST(){
		if (Value != NULL) { delete Value; } 
		if (Index != NULL) { delete Index; }
	}
	string str(){
	return string("AssignArrayLoc") + "(" + Name + "," + getString(Index) + "," + getString(Value) + ")";
	}
};

class binaryExprAST : public decafAST{
	string Op;
	decafStmtList* Left_Value;
	decafStmtList* Right_Value;
public:
  	binaryExprAST(string op, decafStmtList* left_value, decafStmtList* right_value) 
        : Op(op), Left_Value(left_value), Right_Value(right_value){}
  	~binaryExprAST(){
     if(Left_Value != NULL)  { delete Left_Value; }
     if(Right_Value != NULL) { delete Right_Value; }
   }
   	string str(){
	return string("BinaryExpr") + "(" + Op + "," + getString(Left_Value) + "," + getString(Right_Value) + ")";
	}
};

class unaryExprAST : public decafAST{
	string Op;
	decafStmtList* Value;
public: 
  	unaryExprAST(string op,  decafStmtList* value) 
		: Op(op), Value(value){}
  	~unaryExprAST(){
     if(Value != NULL) { delete Value; }
   }
  string str(){
    return string("UnaryExpr") + "(" + Op + "," + getString(Value) + ")";
  }
};

class breakStmtAST : public decafAST{
  string str(){
    return string("BreakStmt");
  }
};

class continueStmtAST : public decafAST{
  string str(){
    return string("ContinueStmt");
  }
};

class forStmtAST : public decafAST{
  assignVarAST* Pre_Assign_List;
  decafAST*  Condition;
  assignVarAST* Loop_Assign_List;
  blockAST*  For_Block;
    
public:
  forStmtAST(assignVarAST* pre_assign_list, decafAST* condition, assignVarAST* loop_assign_list, blockAST* for_block)
           : Pre_Assign_List(pre_assign_list), Condition(condition), Loop_Assign_List(loop_assign_list), For_Block(for_block){}  
  ~forStmtAST(){
	if(For_Block != NULL) { delete For_Block; }
    if(Condition != NULL) { delete Condition; }
    if(Loop_Assign_List != NULL) { delete Loop_Assign_List; }
	if(Pre_Assign_List  != NULL) { delete Pre_Assign_List; }
  }
  string str(){ 
    return string("ForStmt") + "(" + getString(Pre_Assign_List) + "," + getString(Condition) + "," + getString(Loop_Assign_List) + "," + getString(For_Block) + ")";
  }   
};

class methodCallAST : public decafAST{
	string Name;
	decafAST* Method_Arg_List;
public: 
	methodCallAST(string name, decafAST* method_arg_list)
	: Name(name), Method_Arg_List(method_arg_list){}
	~methodCallAST(){
		if(Method_Arg_List != NULL) { delete Method_Arg_List; }
	}
	string str(){
		return string("MethodCall") + "(" + Name + "," + getString(Method_Arg_List) + ")";
	}
};

/// ProgramAST - the decaf program
class ProgramAST : public decafAST {
	decafStmtList *ExternList;
	PackageAST *PackageDef;
public:
	ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), PackageDef(c) {}
	~ProgramAST() { 
		if (ExternList != NULL) { delete ExternList; } 
		if (PackageDef != NULL) { delete PackageDef; }
	}
	string str() { return string("Program") + "(" + getString(ExternList) + "," + getString(PackageDef) + ")"; }
};

