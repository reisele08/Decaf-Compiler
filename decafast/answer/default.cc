
#include "default-defs.h"
#include <list>
#include <ostream>
#include <iostream>
#include <sstream>

#ifndef YYTOKENTYPE
#include "default.tab.h"
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

class fieldAST : public decafAST{
	string Name;
	string Type;
	int Size;
public:
	fieldAST(string name, string type, int size)
		: Name(name), Type(type), Size(size){}
	string str(){
		return string("FieldDecl") + "(" + Name + "," + FieldType + "," + Size + ")";
	}
};

class blockAST : public decafAST{
	decafStmtList Var_Decl_List;
	decafStmtList Statement_List;
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

}

class methodBlockAST : public decafAST{
	decafStmtList Var_Decl_List;
	decafStmtList Statement_List;
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

}

class ifStmtAST : public decafAST{
	decafAST *Condition;
	blockAST *If_Block;
	blockAST *Else_Block;
public:
	ifStmtAST(decafAST *condition, blockAST *if_block, blockAST else_block)
		: Condition(condition), If_Block(if_block), Else_Block(else_block){}
	~ifStmtAST() { 
		if (Condition != NULL) { delete Condition; }
		if (If_Block != NULL) { delete If_Block; }
		if (Else_Block != NULL) { delete Else_Block; }
	}
	string str(){
		return string("IfStmt") + "(" + getString(Condition) + "," + getString(If_Block) + "," + getString(Else_Block) + ")";
	}
}

class whileStmtAST : public decafAST{
	decafAST *Condition;
	blockAST *While_Block;
public:
	whileStmtAST(decafAST *condition, blockAST while_block)
		: Condition(condition), While_Block(while_block){}
	~whileStmtAST() { 
		if (Condition != NULL) { delete Condition; }
		if (While_Block != NULL) { delete While_Block; }
	}
	string str(){
		return string("WhileStmt") + "(" + getString(Condition) + "," + getString(While_Block) + ")";
	}
}

class forStmtAST : public decafAST{
	decafStmtList *Pre_Assign_List;
	decafAST *Condition;
	decafStmtList *Loop_Assign_List;
	blockAST *For_Block;
public:
	forStmtAST(decafStmtList *pre_assign_list, decafAST *condition, decafStmtList *loop_assign_list, blockAST *for_block)
		: Pre_Assign_List(pre_assign_list), Condition(condition), Loop_Assign_List(loop_assign_list), For_Block(for_block){}
	~forStmtAST() { 
		if (Pre_Assign_List != NULL) { delete Pre_Assign_List; }
		if (Condition != NULL) { delete Condition; }
		if (Loop_Assign_List != NULL) { delete Loop_Assign_List; }
		if (For_Block != NULL) { delete For_Block; }
	}
	string str(){
		return string("ForStmt") + "(" + getString(Pre_Assign_List) + "," + getString(Condition) + "," + getString(Loop_Assign_List) +"," + getString(For_Block) +")";
	}	
}

class returnStmtAST : public decafAST{
	decafAST *Return_Value;
public:
	returnStmtAST(decafAST *return_value)
		:Return_Value(return_value){}
	~returnStmtAST() {
		if (Return_Value != NULL) { delete Return_Value; }
	}
	string str(){
		return string("ReturnStmt") + "(" + getString(Return_Value) +")";
	}	
}

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


