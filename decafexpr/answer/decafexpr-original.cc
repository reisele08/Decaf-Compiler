
#include "default-defs.h"
#include <list>
#include <ostream>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <list>
#include <map>

#ifndef YYTOKENTYPE
#include "decafexpr.tab.h"
#endif

using namespace std;

symbol_table_list symtbl;
bool debug = true;
llvm::Value* retVal;
bool assignment = false;
llvm::Type* assignment_type;

// descriptor* access_symtbl(string ident) {
//     for (auto i : symtbl) {
//         auto find_ident = i.find(ident);
//         if (find_ident != i.end()) {
//             return find_ident->second;
//         }
//     }
//     return NULL;
// }

////////////
descriptor* access_symtbl(string id)
{
  for(symbol_table_list::iterator i = symtbl.begin(); i != symtbl.end(); ++i)
  {
    symbol_table::iterator find_id;
    if((find_id = i->find(id)) != i->end())
    {
      return find_id->second; 
    }
  } 
  return NULL;
}
/////////////
void freeDesc(symbol_table symbl_table){
  for(symbol_table::iterator i = symbl_table.begin(); i != symbl_table.end(); i++){
    delete(i->second);
  } 
} 

void print_debug(bool debug, string out){
	if(debug){
		cout << out << endl;
	}
}

static llvm::AllocaInst* CreateEntryBlockAlloca(llvm::Function *TheFunction, llvm::Type* varType, const std::string &varName){
	llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(varType, NULL, varName.c_str());
}

llvm::Type* getType(string t){
	llvm::Type* LType;
	if(t == "StringType"){
		LType = Builder.getInt8PtrTy();
	}
	else if(t == "VoidType"){
		LType = Builder.getVoidTy();
	}
	else if(t == "IntType"){
		LType = Builder.getInt32Ty();
	}
	else if(t == "BoolType"){
		LType = Builder.getInt1Ty();
	}
}
	//FIX FIX FIX
int getOp(string op){
	if(op == "Plus"){ 
		return T_PLUS;  
	}
	if(op == "Minus"){
		return T_MINUS; 
	}
	if(op == "Mult"){ return T_MULT;  
	}
	if(op == "Div")        { return T_DIV;   }
	if(op == "Leftshift")  { return T_LEFTSHIFT;   }
	if(op == "Rightshift") { return T_RIGHTSHIFT;  }
	if(op == "Mod")        { return T_MOD;   } 
	if(op == "Eq")         { return T_EQ;    }
	if(op == "Neq")        { return T_NEQ ;  }
	if(op == "Lt")         { return T_LT;    }
	if(op == "Leq")        { return T_LEQ;   }
	if(op == "Gt")         { return T_GT;    }
	if(op == "Geq")        { return T_GEQ;   }
	if(op == "And")        { return T_AND;   }
	if(op == "Or")         { return T_OR;}
	if(op == "Not")        { return T_NOT;} 
	if(op == "UnaryMinus") { return T_MINUS;}

	return -1;
}

/// decafAST - Base class for all abstract syntax tree nodes.
class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
  virtual llvm::Value *Codegen() = 0;
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

template <class T>
llvm::Value *listCodegen(list<T> vec) {
	llvm::Value *val = NULL;
	for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) { 
		llvm::Value *j = (*i)->Codegen();
		if (j != NULL) { val = j; }
	}	
	return val;
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
	list<decafAST*> retList(){return stmts;}
	string str() { return commaList<class decafAST *>(stmts); }
	llvm::Value *Codegen(){
		return listCodegen<decafAST*>(stmts);
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
	llvm::Value *Codegen(){
		symtbl.push_front(symbol_table());
		if(Var_Decl_List != NULL) {Var_Decl_List->Codegen();}
		if(Statement_List != NULL) {Statement_List->Codegen();}

		symbol_table symbl_table = symtbl.front();
		for(symbol_table::iterator i = symbl_table.begin(); i != symbl_table.end(); i++){
			delete(i->second);
		}
		symtbl.pop_front();
		return NULL;
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
	llvm::Value *Codegen(){
		// symtbl.push_front(symbol_table());
		// if(Var_Decl_List != NULL) {Var_Decl_List->Codegen();}
		// if(Statement_List != NULL) {Statement_List->Codegen();}

		// symbol_table symbl_table = symtbl.front();
		// for(symbol_table::iterator i = symbl_table.begin(); i != symbl_table.end(); i++){
		// 	delete(i->second);
		// }
		// symtbl.pop_front();
		// return NULL;

		////////////////////////
		llvm::Value *val = NULL;
		if(NULL != Var_Decl_List){
			val = Var_Decl_List->Codegen();
		}
		if(NULL != Statement_List){
			val = Statement_List->Codegen();
		}
		return val;
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
	llvm::Value *Codegen(){return NULL;}//d
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
	llvm::Value *Codegen(){return NULL;}//d 
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
	llvm::Value *Codegen(){return NULL;}	
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
	llvm::Value *Codegen(){
		print_debug(debug, "fieldAST");
		llvm::Constant* Initializer;
		llvm::Type* type = getType(Type);

		if(type->isIntegerTy(32)){
			Initializer = Builder.getInt32(0);
		}
		else if(type->isVoidTy()){
			Initializer = NULL;
		}
		else if(type->isIntegerTy(1)){
			Initializer = Builder.getInt1(0);
		}

		llvm::GlobalVariable *GV = new llvm::GlobalVariable(*TheModule, type, false, llvm::GlobalValue::InternalLinkage, Initializer, Name);

		descriptor* desc = new descriptor;
		desc->type = Type;
		desc->globvar = GV;
		desc->lineno = lineno;

		(symtbl.front())[Name] = desc;
		print_debug(debug, "fieldAST_END");
		return GV;

	}	
};
class assignGlobalVarAST : public decafAST{
	string Name;
	string Type;
	//string Value;
	decafAST* Value;
public:
	assignGlobalVarAST(string name, string type, decafAST* value)
		: Name(name), Type(type), Value(value){}
	~assignGlobalVarAST(){
		if(Value != NULL) {delete Value;}
	}
	string str(){
		return string("AssignGlobalVar") + "(" + Name + "," + Type + "," + getString(Value) +")";
	}
	llvm::Value *Codegen(){
		print_debug(debug, "assignGlob");
		llvm::Constant* Initializer;
		llvm::Type* type = getType(Type);

		Initializer = (llvm::Constant*)Value->Codegen();

		llvm::GlobalVariable *GV = new llvm::GlobalVariable(*TheModule, type, false, llvm::GlobalValue::InternalLinkage, Initializer, Name);

		descriptor* desc = new descriptor;
		desc->type = Type;
		desc->globvar = GV;
		desc->lineno = lineno;

		(symtbl.front())[Name] = desc;
		print_debug(debug, "assignGlob_END");
		return GV;
	}		
};


class varDefAST : public decafAST{
	bool Params;
	string Name;
	string Type;
public:
	varDefAST(bool params, string name, string type)
		: Params(params), Name(name), Type(type){}
	string str(){
		//for externs
		if(Name == "extern"){
			return string("VarDef") + "(" + Type + ")";
		}
		else{
	return string("VarDef") + "(" + Name + "," + Type + ")";
		}
	}
	//getters
	string getName(){
		return Name;
	}
	string getVar(){
		return Type;
	}

	llvm::Value *Codegen(){
		print_debug(debug, "vardef_START");

		if(Name.empty()){
			return NULL;
		}

		llvm::Type *LType = getType(Type);
		llvm::AllocaInst *Alloca = NULL;

		if(Params == false){
			Alloca = Builder.CreateAlloca(LType, NULL, Name);
		}

		descriptor* desc = new descriptor;
		desc->lineno = lineno;
		desc->type = Type;
		desc->alloc = Alloca;

		(symtbl.front())[Name] = desc;
		print_debug(debug, "vardef_END");
		return (llvm::Value*)Alloca;
	}
};

	//FIX FIX FIX
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
	llvm::Value *Codegen(){
		print_debug(debug, "extern_START");
		
		llvm::Value *value = NULL;
		llvm::Type* returnTy = getType(Return_Type);

		vector<llvm::Type*> arguments;
		if(NULL != Typelist){
			list<decafAST*> stmts = Typelist->retList();
			llvm::Type* Return_Type;
			for(list<decafAST*>::iterator i = stmts.begin(); i != stmts.end(); i++){
				string type =  ((varDefAST*)(*i))->getVar();
				if(type.empty()){arguments.clear(); break;}
				else{Return_Type = getType(type);}

				arguments.push_back(Return_Type);
			}
		}
		llvm::Function *func = llvm::Function::Create(llvm::FunctionType::get(returnTy,arguments,false), llvm::Function::ExternalLinkage, Name, TheModule);

		verifyFunction(*func);
		value = (llvm::Value*)func;

		descriptor* desc = new descriptor;
		desc->lineno = lineno;
		desc->type = Return_Type;
		desc->funct = func;
		desc->arg_types = arguments;

		(symtbl.front())[Name] = desc;
		print_debug(debug, "extern_END");
		return value;

	}
};

//////////////////////////
// class externAST : public decafAST
// {
//   string Name;
//   decafStmtList* ExternTypeList;
//   string MethodType; 

// public: 
//   externAST(string name, string mtype, decafStmtList* elist) 
//   : Name(name), MethodType(mtype),ExternTypeList(elist){}  
//   ~externAST()  
//   {
//     if(ExternTypeList != NULL) { delete ExternTypeList; }
//   }

//   string str()
//   {
//     return string("ExternFunction") + "(" + Name + "," + MethodType + "," + getString(ExternTypeList)+ ")";
//   }
//   llvm::Value *Codegen() 
//   {
//     print_debug(debug,"...Extern Codegen Begins...");

//     llvm::Value *val = NULL;

//     llvm::Type* returnTy = getType(MethodType);

//     vector<llvm::Type*> args;
//     if (NULL != ExternTypeList) 
//     {
//       list<decafAST*> stmts = ExternTypeList->retList();  
//       llvm::Type* ArgType;
//       for (list<decafAST*>::iterator i = stmts.begin(); i != stmts.end(); i++)
//       { 
        
//         string type =  ((varDefAST*)(*i))->getVar();
//         if(type.empty())
// 	{
//           args.clear();
//           break;      
//         }
//         else
//         {
//           ArgType = getType(type); 
// 	}
//         args.push_back(ArgType);    
//       }        
//     }
  
//     llvm::Function *func = llvm::Function::Create(llvm::FunctionType::get(returnTy,args,false),
//                                                   llvm::Function::ExternalLinkage,
//                                                   Name,
//                                                   TheModule
//  	 					 );
//     verifyFunction(*func);
//     val = (llvm::Value*)func;
//     descriptor* d = new descriptor;
//     d->type       = MethodType;
//     d->lineno     = lineno;
//     d->funct   = func;
//     d->arg_types  = args;
//     (symtbl.front())[Name] = d; 
//     print_debug(debug,"...Extern Codegen Ends...");
//     return val; 
//   }
// };
////////////////////////////////////////
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

int convertInt(string s){
	int result;
	stringstream stringStream;
	if(s.find("x") != string::npos){
		stringStream << hex << s;
	}
	else{
		stringStream << s;
	}
	stringStream >> result;
	return result;

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
	llvm::Value *Codegen(){
		llvm::Constant *Constnt;
		if(Type == "int"){
			Constnt = Builder.getInt32(convertInt(Value));
		}
		else if(Type == "str"){
			llvm::GlobalVariable *GV = Builder.CreateGlobalString(Value.c_str(), "globalstring");
			return Builder.CreateConstGEP2_32(GV->getValueType(), GV, 0, 0, "cast");
		}
		return (llvm::Value*)Constnt;
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
	llvm::Value *Codegen(){
		llvm::Constant *Constnt;

		if(Value == "True") {Constnt = Builder.getInt1(1);}
		if(Value == "False") {Constnt = Builder.getInt1(0);}

		return (llvm::Value*)Constnt;
	}
};

	//FIX FIX FIX FIX
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

	// llvm::Function* funct(){
		
	// 	llvm::Function *func;
	// 	llvm::Type *returnTy;
	// 	vector<string> arg_names;
	// 	vector<llvm::Type*> arg_types;
	// 	list<decafAST*> stmnts;

	// 	returnTy = getType(Return_Type);

	// 	if(Param_List != NULL){
	// 		stmnts = Param_List->retList();
	// 		Param_List->Codegen();
	// 	}

	// 	for(list<decafAST*>::iterator i = stmnts.begin(); i != stmnts.end(); i++){
	// 		varDefAST* varDef = (varDefAST*)(*i);
    //   		llvm::Type* type = getType(varDef->getVar());
    //   		string name = varDef->getName(); 

    //   		arg_types.push_back(type);  
    //   		arg_names.push_back(name);			
	// 	}

	// 	func = llvm::Function::Create(llvm::FunctionType::get(returnTy, arg_types, false), llvm::Function::ExternalLinkage, Name, TheModule);

	// 	descriptor* descrptr = new descriptor;
	// 	descrptr->arg_names = arg_names;
	// 	descrptr->arg_types = arg_types;
	// 	descrptr->funct = func;
	// 	descrptr->lineno = lineno;
	// 	descrptr->type = Return_Type;

	// 	(symtbl.front())[Name] = descrptr;

	// 	return func;
	// } 
	llvm::Value *Codegen(){

		print_debug(debug, "method codegen");

		// llvm::Type *returnTy = getType(Return_Type);
		// list<decafAST*> stmnts;

		// descriptor* descrptr = access_symtbl(Name);

		// if(returnTy->isIntegerTy(32)){
		// 	retVal = Builder.getInt32(0);
		// }
		// else{
		// 	retVal = Builder.getInt1(1) ;
		// }

		// if(Param_List != NULL){
		// 	stmnts = Param_List->retList();
		// 	Param_List->Codegen();
		// }

		// vector<llvm::Type*> arg_types;
		// vector<string> arg_names;

		// for(list<decafAST*>:: iterator i = stmnts.begin(); i != stmnts.end(); i++){
		// 	varDefAST* varDef = (varDefAST*)(*i);
		// 	llvm::Type* type = getType(varDef->getVar());
		// 	string name = varDef->getName();
		// 	arg_names.push_back(name);
		// 	arg_types.push_back(type);
		// }
		// llvm::Function *func;

		// if(descrptr == NULL){
		// 	func = llvm::Function::Create(llvm::FunctionType::get(returnTy, arg_types, false), llvm::Function::ExternalLinkage, Name, TheModule);

		// 	descriptor* descrptr = new descriptor;
		// 	descrptr->arg_names = arg_names;
		// 	descrptr->arg_types = arg_types;
		// 	descrptr->funct = func;
		// 	descrptr->lineno = lineno;
		// 	descrptr->type = Return_Type;

		// 	(symtbl.front())[Name] = descrptr;
		// }
		// else{
		// 	func = descrptr->funct;
		// }

		// llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", func);

		// Builder.SetInsertPoint(BB);

		// unsigned int idx = 0;

		// for(llvm::Function::arg_iterator i = func->arg_begin(); i != func->arg_end(); i++, idx++){
		// 	descriptor* descrptr = access_symtbl(arg_names[idx]);
		// 	llvm::AllocaInst* Alloca = descrptr->alloc;

		// 	Alloca = CreateEntryBlockAlloca(func, arg_types[idx], arg_names[idx]);

		// 	Builder.CreateStore(&(*i), Alloca);

		// 	descrptr->alloc = Alloca;
		// }
		
		// if(Block != NULL){
		// 	Block->Codegen();
		// }
		// if(returnTy->isVoidTy()){
		// 	Builder.CreateRet(NULL);
		// }
		// else{
		// 	Builder.CreateRet(retVal);
		// }

		// verifyFunction(*func);
		// print_debug(debug, "method end");

		// return (llvm::Value*)func;
		print_debug(debug, "method end");
	} 
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
	llvm::Value *Codegen() { 
		print_debug(debug, "package_START");
		// llvm::Value *val = NULL;
		// TheModule->setModuleIdentifier(llvm::StringRef(Name)); 
		// if (NULL != FieldDeclList) {
		// 	val = FieldDeclList->Codegen();
		// }
		// if (NULL != MethodDeclList) {
		// 	list<decafAST*> stmts = MethodDeclList->retList();
		// 	for(list<decafAST*>::iterator i = stmts.begin(); i != stmts.end(); i++){
		// 		methodAST* method = (methodAST*)(*i);

		// 		method->funct();
		// 	}
		// 	val = MethodDeclList->Codegen();
		// } 
		llvm::Value *val = NULL;
		TheModule->setModuleIdentifier(llvm::StringRef(Name)); 
		if (NULL != FieldDeclList) {
			val = FieldDeclList->Codegen();
		}
		if (NULL != MethodDeclList) {
			val = MethodDeclList->Codegen();
		} 
		// Q: should we enter the class name into the symbol table?
		print_debug(debug, "package_END");
		return val; 
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
	llvm::Value *Codegen(){
		descriptor* descrptr = access_symtbl(Name);
		return Builder.CreateLoad(descrptr->alloc);
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
	llvm::Value *Codegen(){
		descriptor* descrptr = access_symtbl(Name);
		return Builder.CreateLoad(descrptr->alloc);
	} 
};

//FIX FIX FIX
//ASSIGN AST
class assignVarAST : public decafAST{
	string Name;
	decafAST* Value;
public:
	assignVarAST(string name, decafAST* value)
		: Name(name), Value(value){}
	~assignVarAST(){
		if (Value != NULL) { delete Value; } 
	}
	string str(){
	return string("AssignVar") + "(" + Name + "," + getString(Value) + ")";
	}
	llvm::Value *Codegen(){
		print_debug(debug, "assignVar_START");

		llvm::AllocaInst *Alloca;
		llvm::Value* value = NULL;
		llvm::Value *RValue;

		descriptor* desc;

		desc = access_symtbl(Name);
		Alloca = desc->alloc;
		assignment = true;
		assignment_type = Alloca->getType();

		RValue = Value->Codegen();

		if((RValue->getType()->isIntegerTy(1)  == true) && (Alloca->getType()->isIntegerTy(32) == true)){
			RValue = Builder.CreateZExt(RValue, Builder.getInt32Ty(), "zexttmp");
		}
		const llvm::PointerType *pointer_type = RValue->getType()->getPointerTo();

		if(Alloca->getType() == pointer_type){
			value = Builder.CreateStore(RValue, Alloca);
		}

		assignment = false;

		print_debug(debug, "assignvar_END");
		return value;
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
	llvm::Value *Codegen(){
		print_debug(debug, "assignArray_START");

		llvm::AllocaInst *Alloca;
		llvm::Value* value = NULL;
		llvm::Value *RValue;

		descriptor* desc;

		desc = access_symtbl(Name);
		Alloca = desc->alloc;
		assignment = true;
		assignment_type = Alloca->getType();

		RValue = Value->Codegen();

		if((RValue->getType()->isIntegerTy(1)  == true) && (Alloca->getType()->isIntegerTy(32) == true)){
			RValue = Builder.CreateZExt(RValue, Builder.getInt32Ty(), "zexttmp");
		}
		const llvm::PointerType *pointer_type = RValue->getType()->getPointerTo();

		if(Alloca->getType() == pointer_type){
			value = Builder.CreateStore(RValue, Alloca);
		}

		assignment = false;

		print_debug(debug, "assignArray_END");
		return value;		
	} 
};
	//FIX FIX FIX
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
	llvm::Value *Codegen(){
		print_debug(debug, "binaryExpr_START");

		llvm::Value* LValue = Left_Value->Codegen();
		llvm::Value* val = nullptr;
		llvm::Value* RValue = Right_Value->Codegen();

    switch(getOp(Op))
    {   
      case T_PLUS: 
      {  
        val = Builder.CreateAdd(LValue,RValue,"addtmp");
        break;
      }
      case T_MINUS: 
      {  
        val = Builder.CreateSub(LValue,RValue,"subtmp");
        break;
      }
      case T_MULT: 
      {  
        val = Builder.CreateMul(LValue,RValue,"multmp");
        break;
      }
      case T_DIV: 
      {  
        val = Builder.CreateSDiv(LValue,RValue,"divtmp");
        break;
      }
      case T_LEFTSHIFT:
      {
        val = Builder.CreateShl(LValue, RValue,"lstmp");
        break;
      }
      case T_RIGHTSHIFT:
      {
        val = Builder.CreateLShr(LValue, RValue,"rstmp");
        break;
      }
      case T_MOD:
      {
        val = Builder.CreateSRem(LValue, RValue,"modtmp");
        break;
      }
      case T_EQ:
      {
        val = Builder.CreateICmpEQ(LValue, RValue,"eqtmp");
        break;
      }
      case T_NEQ:
      {
        val = Builder.CreateICmpNE(LValue, RValue,"neqtmp");
        break;
      }
      case T_LT:
      {
        val = Builder.CreateICmpSLT(LValue, RValue,"lttmp");        
        break;
      }
      case T_GT:
      {
        val = Builder.CreateICmpSGT(LValue, RValue,"gttmp");
        break;
      }
      case T_LEQ:
      {
        val = Builder.CreateICmpSLE(LValue, RValue,"leqtmp");
        break;
      }
      case T_GEQ:
      {
        val = Builder.CreateICmpSGE(LValue, RValue,"geqtmp");
        break;
      }
      case T_AND: 
      {  
        val = Builder.CreateAnd(LValue,RValue,"andtmp");
        break;
      }
      case T_OR:
      {
        val = Builder.CreateOr(LValue, RValue,"ortmp");
        break;
      }
      default: 
	  break;
    	}
		print_debug(debug, "binaryExpr_END");
		return val;
	} 
};
	//FIX FIX FIX
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
  llvm::Value *Codegen(){
	  print_debug(debug, "unary_START");

	  llvm::Value* RValue = Value->Codegen();
	  llvm::Value* val = NULL;

	switch(getOp(Op))
    {
      case T_NOT:
      {
        val = Builder.CreateNot(RValue,"unottmp");
        break;
      }
      case T_MINUS:
      {
        val = Builder.CreateNeg(RValue,"unegtmp");
        break;
      }
      default: 
	  break;
    }
	print_debug(debug, "unary_END");
	return val;
  }
};

class breakStmtAST : public decafAST{
  string str(){
    return string("BreakStmt");
  }
  llvm::Value *Codegen(){}//d
};

class continueStmtAST : public decafAST{
  string str(){
    return string("ContinueStmt");
  }
  llvm::Value *Codegen(){}//d
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
  llvm::Value *Codegen(){}//d
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
	llvm::Value *Codegen(){}//d
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
	llvm::Value *Codegen() { 
		llvm::Value *val = NULL;
		if (NULL != ExternList) {
			val = ExternList->Codegen();
		}
		if (NULL != PackageDef) {
			val = PackageDef->Codegen();
		} else {
			throw runtime_error("no package definition in decaf program");
		}
		return val; 
	}
};

