
#include "default-defs.h"
#include <list>
#include <ostream>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <list>
#include <map>

#ifndef YYTOKENTYPE
#include "decafcomp.tab.h"
#endif

using namespace std;

symbol_table_list symtbl;
bool debug = false;
llvm::Value* retVal;
bool assignment = false;
llvm::Type* assignment_type;

llvm::Value* access_symtbl(string ident) {
    for (auto i : symtbl) {
        auto find_ident = i.find(ident);
        if (find_ident != i.end()) {
            return find_ident->second;
        }
    }
    return NULL;
}

void print_debug(bool debug, string out){
	if(debug){
		cout << out << endl;
	}
}

llvm::Type *getType(string t){
	print_debug(debug, "getType_START");
	if(t == "StringType"){
		print_debug(debug, "getType_StringType");
		return Builder.getInt8PtrTy();
	}
	else if(t == "VoidType"){
		print_debug(debug, "getType_VoidType");
		return Builder.getVoidTy();
	}
	else if(t == "IntType"){
		print_debug(debug, "getType_IntType");
		return Builder.getInt32Ty();
	}
	else if(t == "BoolType"){
		print_debug(debug, "getType_BoolType");
		return Builder.getInt1Ty();
	}
	print_debug(debug, "getType_END");
}

static llvm::AllocaInst* CreateEntryBlockAlloca(llvm::Function *TheFunction, llvm::Type* varType, const std::string &varName){
	llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(varType, NULL, varName.c_str());
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
	list<decafAST *>::iterator begin() { return stmts.begin(); }
	list<decafAST *>::iterator end() { return stmts.end(); }
	list<decafAST*> retList(){return stmts;}
	vector<llvm::Value *> argList(){
		vector<llvm::Value *> args;
		for (list<class decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++){
			args.push_back((*i)->Codegen());
		}
		return args;
	}
	string str() { return commaList<class decafAST *>(stmts); }
	llvm::Value *Codegen() { 
		return listCodegen<decafAST *>(stmts); 
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

		// descriptor* desc = new descriptor;
		// desc->type = Type;
		// desc->globvar = GV;
		// desc->lineno = lineno;

		//(symtbl.front())[Name] = desc;

		(symtbl.front())[Name] = (llvm::Value*) GV;
		print_debug(debug, "fieldAST_END");
		return GV;

	}	
};

class assignGlobalVarAST : public decafAST{
	string Name;
	string Type;
	decafAST* Expr;
public:
	assignGlobalVarAST(string name, string type, decafAST* arg)
		: Name(name), Type(type), Expr(arg){}
	string str(){
		return string("AssignGlobalVar") + "(" + Name + "," + Type + "," + getString(Expr) +")";
	}
	llvm::Value *Codegen(){
		print_debug(debug, "assignGlobalVarAST_START");
		llvm::Constant* Initializer;
		llvm::Type* type = getType(Type);

		Initializer = (llvm::Constant*)Expr->Codegen();

		llvm::GlobalVariable *GV = new llvm::GlobalVariable(*TheModule, type, false, llvm::GlobalValue::InternalLinkage, Initializer, Name);

		// descriptor* desc = new descriptor;
		// desc->type = Type;
		// desc->globvar = GV;
		// desc->lineno = lineno;

		//(symtbl.front())[Name] = desc;
		(symtbl.front())[Name] = (llvm::Value*) GV;

		print_debug(debug, "assignGlobalVarAST_END");
		return GV;

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
	}
	string str(){
	return string("AssignVar") + "(" + Name + "," + getString(Value) + ")";
	}
	llvm::Value *Codegen(){
		print_debug(debug, "assignVar_START");

		//descriptor* desc;

		//desc = access_symtbl(Name);
		//llvm::AllocaInst *Alloca = desc->alloc;

		//llvm::Value *val = Value->Codegen();

		llvm::Value *val = NULL; 
		llvm::Value *r_val;
    	llvm::Value *l_val;

		l_val = access_symtbl(Name);
		r_val = Value->Codegen(); 
		
		if((r_val->getType()->isIntegerTy(1)  == true) && (l_val->getType()->isIntegerTy(32) == true)){
			r_val = Builder.CreateZExt(val, Builder.getInt32Ty(), "zexttmp");
		}

		if(l_val->getType() == r_val->getType()->getPointerTo()){
			return Builder.CreateStore(r_val, l_val);
		}
		print_debug(debug, "assignvar_END");
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

		llvm::Value *val = NULL; 
		llvm::Value *r_val;
    	llvm::Value *l_val;	

		llvm::GlobalVariable *GV = (llvm::GlobalVariable*)l_val;
    	llvm::ArrayType *arrayi32 = (llvm::ArrayType*)GV->getType();

    	llvm::Value *ArrayLoc = Builder.CreateStructGEP(arrayi32, GV, 0, "arrayloc");
    	llvm::Value *indexValue = (Index)->Codegen();
    	llvm::Value *ArrayIndex = Builder.CreateGEP(arrayi32->getElementType(), ArrayLoc, indexValue, "arrayindex");
    	l_val = ArrayIndex;

		r_val = Index->Codegen();

		if((l_val->getType()->isIntegerTy(32) == true) && (r_val->getType()->isIntegerTy(1)  == true)){  
      		r_val = Builder.CreateZExt(r_val, Builder.getInt32Ty(), "zexttmp");    
   		}

    const llvm::PointerType *ptrTy = r_val->getType()->getPointerTo();
    
    	if(l_val->getType() == ptrTy){
      	val = Builder.CreateStore(r_val, l_val);
    	}    
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
		//descriptor* descrptr = access_symtbl(Name);
		//return Builder.CreateLoad(descrptr->alloc);
		llvm::Value* val = access_symtbl(Name);
		return Builder.CreateLoad(val, Name);
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
		//descriptor* descrptr = access_symtbl(Name);
		//return Builder.CreateLoad(descrptr->alloc);
		print_debug(debug, "arrayLocExpr_START");
		llvm::Value* val = access_symtbl(Name);

		llvm::GlobalVariable *GV = (llvm::GlobalVariable*)val;
    	llvm::ArrayType *arrayi32 = (llvm::ArrayType*)GV->getType();
    	llvm::Value *ArrayLoc = Builder.CreateStructGEP(arrayi32, GV, 0, "arrayloc");
    	llvm::Value *In = Index->Codegen();
    	llvm::Value *ArrayIndex = Builder.CreateGEP(arrayi32->getElementType(), ArrayLoc, In, "arrayindex");

		print_debug(debug, "arrayLocExpr_END");
		return Builder.CreateLoad(ArrayIndex, "loadtmp");

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
		print_debug(debug, "blockAST_START");

		symtbl.push_front(symbol_table());
		if(Var_Decl_List != NULL) {Var_Decl_List->Codegen();}
		if(Statement_List != NULL) {Statement_List->Codegen();}

		symbol_table sym_table = symtbl.front();
		symtbl.pop_front();

		print_debug(debug, "blockAST_END");
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
		print_debug(debug, "methodBlockAST_START");

		symtbl.push_front(symbol_table());

		llvm::BasicBlock* CurBB = Builder.GetInsertBlock();
    	llvm::Function* func = CurBB->getParent();
		llvm::StringRef func_name = func->getName();
    	llvm::AllocaInst* Alloca;

		unsigned int idx = 0;
		string arg_name;

		for(llvm::Function::arg_iterator i = func->arg_begin(); i != func->arg_end(); ++i){
			arg_name = (*i).getName();

			Alloca = Builder.CreateAlloca((*i).getType() , NULL, (*i).getName());
			Builder.CreateStore(&(*i), Alloca);
			(symtbl.front())[arg_name] = (llvm::Value*)Alloca;
		}

		if(Var_Decl_List != NULL) {Var_Decl_List->Codegen();}
		if(Statement_List != NULL) {Statement_List->Codegen();}

		symbol_table sym_table = symtbl.front();
		symtbl.pop_front();
		print_debug(debug, "methodBlockAST_END");
		return NULL;
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
			(symtbl.front())[Name] = Alloca;
		}

		// descriptor* desc = new descriptor;
		// desc->lineno = lineno;
		// desc->type = Type;
		// desc->alloc = Alloca;

		// (symtbl.front())[Name] = desc;

		print_debug(debug, "vardef_END");
		return (llvm::Value*)Alloca;
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
	llvm::Value *Codegen(){
		print_debug(debug, "binaryExpr_START");

		llvm::Value* L_Value = Left_Value->Codegen();
		llvm::Value* R_Value = Right_Value->Codegen();
 
		if(Op == "Plus") { return Builder.CreateAdd(L_Value,R_Value,"addtmp"); }
		if(Op == "Minus") { return Builder.CreateSub(L_Value,R_Value,"subtmp"); }
		if(Op == "Mult") { return Builder.CreateMul(L_Value,R_Value,"multmp"); }
		if(Op == "Div") { return Builder.CreateSDiv(L_Value,R_Value,"divtmp"); }
		if(Op == "Mod") { return Builder.CreateSRem(L_Value, R_Value,"modtmp"); }
		if(Op == "Eq") { return Builder.CreateICmpEQ(L_Value, R_Value,"eqtmp"); }
		if(Op == "Neq") { return Builder.CreateICmpNE(L_Value, R_Value,"neqtmp");}
		if(Op == "Lt") { return Builder.CreateICmpSLT(L_Value, R_Value,"lttmp"); }
		if(Op == "Gt") { return Builder.CreateICmpSGT(L_Value, R_Value,"gttmp"); }
		if(Op == "Rightshift") { return Builder.CreateLShr(L_Value, R_Value,"rstmp"); }
		if(Op == "Leftshift") { return Builder.CreateShl(L_Value, R_Value,"lstmp"); }
		if(Op == "Leq") { return Builder.CreateICmpSLE(L_Value, R_Value,"leqtmp"); }
		if(Op == "Geq") { return Builder.CreateICmpSGE(L_Value, R_Value,"geqtmp"); }
		if(Op == "And") { return Builder.CreateAnd(L_Value,R_Value,"andtmp"); }
		if(Op == "Or") { return Builder.CreateOr(L_Value, R_Value,"ortmp"); }
		return NULL;

		print_debug(debug, "binaryExpr_END");
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
			string s = string("");

			for(int i = 1; i < Value.length()-1; i++){
				if(Value[i] != '\\'){
					s.push_back(Value[i]);
				}
				else{
					switch(Value[i+1]){
						case 'a':  s.push_back('\a'); break;
      					case 'b':  s.push_back('\b'); break;
      					case 't':  s.push_back('\t'); break;
						case 'n':  s.push_back('\n'); break;
						case 'v':  s.push_back('\v'); break;
						case 'f':  s.push_back('\f'); break;
						case 'r':  s.push_back('\r'); break;
						case '\\': s.push_back('\\'); break;
						case '\'': s.push_back('\''); break;
						case '\"': s.push_back('\"'); break;
					}
					i++;
				}
			}

			llvm::GlobalVariable *GV = Builder.CreateGlobalString(s.c_str(), "globalstring");
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

class methodCallAST : public decafAST{
	string Name;
	decafStmtList* Method_Arg_List;
public: 
	methodCallAST(string name, decafStmtList* method_arg_list)
	: Name(name), Method_Arg_List(method_arg_list){}
	~methodCallAST(){
		if(Method_Arg_List != NULL) { delete Method_Arg_List; }
	}
	string str(){
		return string("MethodCall") + "(" + Name + "," + getString(Method_Arg_List) + ")";
	}

	llvm::Value *Codegen(){
		llvm::Function *func = TheModule->getFunction(Name);
		
        assert(func != NULL);
        std::vector<llvm::Value *> args;
        for (auto i = Method_Arg_List->begin(); i != Method_Arg_List->end(); i++){
            args.push_back((*i)->Codegen());
            if (!args.back()){
                return NULL;
            }
        }

        int count = 0;
        for (auto i = func->arg_begin(); i != func->arg_end(); i++){
            if (i->getType()->isIntegerTy(32) && args[count]->getType()->isIntegerTy(1)){
                args[count] = Builder.CreateIntCast(args[count], Builder.getInt32Ty(), false);
            }
            count++;
        }
        if (func->getReturnType()->isVoidTy()){
            return Builder.CreateCall(func, args);
        }
        return Builder.CreateCall(func, args, "calltmp");
		return NULL;
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

	llvm::Function* funct(){
	print_debug(debug, "methodAST_funct_START");
    llvm::Function *func;
	list<decafAST*> stmts;
    llvm::Type *returnTy;

    returnTy = getType(Return_Type);

    if(Param_List != NULL){
      stmts = Param_List->retList(); 
      Param_List->Codegen();
    }
   
    vector<llvm::Type *> arg_types;
    vector<string>arg_names;

	print_debug(debug, "methodAST_funct_FORLOOP");
    for (list<decafAST*>::iterator i = stmts.begin(); i != stmts.end(); i++){  
      varDefAST* varDef = (varDefAST*)(*i);
      llvm::Type* arg_type = getType(varDef->getVar());
      string arg_name = varDef->getName(); 

      arg_types.push_back(arg_type);  
      arg_names.push_back(arg_name);  
    }   

    func = llvm::Function::Create(llvm::FunctionType::get(returnTy, arg_types, false), llvm::Function::ExternalLinkage, Name, TheModule);
  
    unsigned int idx = 0;
	print_debug(debug, "methodAST_funct_FORLOOPTWO");
    for(auto &Arg : func->args()){
      Arg.setName(arg_names[idx++]);
    }

    (symtbl.front())[Name] = (llvm::Value*) func;
	print_debug(debug, "methodAST_funct_END");
    return func;
	} 

	llvm::Value *Codegen(){
	print_debug(debug, "methodAST_START");
    llvm::Function *func = (llvm::Function*)access_symtbl(Name);
    llvm::Type *returnTy = getType(Return_Type);

    list<decafAST*> stmts;
 
    if(Param_List != NULL){
      stmts = Param_List->retList(); 
      Param_List->Codegen();
    }
    
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", func);

	print_debug(debug, "methodAST_INSERTPT");
    Builder.SetInsertPoint(BB);
    
    if(Block != NULL){
      Block->Codegen(); 
    }

    if(retVal == NULL){
      if(returnTy->isVoidTy()){ 
        Builder.CreateRet(NULL);
      }
      else{
        if(returnTy->isIntegerTy(32)){ 
			retVal = Builder.getInt32(0); 
		}
        else{ 
			retVal = Builder.getInt1(1) ; 
		} 
        Builder.CreateRet(retVal);
        retVal = NULL;
      }
    }

    verifyFunction(*func);
	print_debug(debug, "methodAST_END");
    return (llvm::Value*)func;
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
		llvm::Value *val = NULL;
		TheModule->setModuleIdentifier(llvm::StringRef(Name)); 
		if (NULL != FieldDeclList) {
			val = FieldDeclList->Codegen();
		}
		if (NULL != MethodDeclList) {
			list<decafAST*> stmts = MethodDeclList->retList();
			for(list<decafAST*>::iterator i = stmts.begin(); i != stmts.end(); i++){
				methodAST* method = (methodAST*)(*i);

				method->funct();
			}
			val = MethodDeclList->Codegen();
		}
		print_debug(debug, "package_END");
		return val; 
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
	llvm::Value *Codegen(){
		print_debug(debug, "extern_START");
		
		llvm::Value *value = NULL;

		print_debug(debug, "extern_getType");
		llvm::Type *returnTy = getType(Return_Type);
		print_debug(debug, "extern_EXIT_getType");

		print_debug(debug, "extern_ENTER_VECTOR");
		std::vector<llvm::Type*> arguments;

		print_debug(debug, "extern_ENTER_IFstmt");
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
		print_debug(debug, "extern_EXIT_IFstmt");

		print_debug(debug, "extern_CREATE_FUNCTION");
		llvm::Function *func = llvm::Function::Create(llvm::FunctionType::get(returnTy,arguments,false), llvm::Function::ExternalLinkage, Name, TheModule);

		value = (llvm::Value*)func;

		print_debug(debug, "extern_symtbl");
		(symtbl.front())[Name] = value;
		print_debug(debug, "extern_END");
		return value;
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
	llvm::Value *Codegen(){
		
		llvm::Function *func = Builder.GetInsertBlock()->getParent();
	
		llvm::BasicBlock* IfBB = llvm::BasicBlock::Create(TheContext, "if", func);
		llvm::BasicBlock* IfTrueBB = llvm::BasicBlock::Create(TheContext, "iftrue", func);
		llvm::BasicBlock* IfFalseBB = llvm::BasicBlock::Create(TheContext, "iffalse", func);
		llvm::BasicBlock* EndBB = llvm::BasicBlock::Create(TheContext, "end", func);     

		Builder.CreateBr(IfBB);
		Builder.SetInsertPoint(IfBB);

		llvm::Value* Cond = Condition->Codegen();   

		Builder.CreateCondBr(Cond, IfTrueBB, IfFalseBB);
		Builder.SetInsertPoint(IfTrueBB);

		If_Block->Codegen();

		Builder.CreateBr(EndBB);
		Builder.SetInsertPoint(IfFalseBB);

		if(Else_Block != NULL){
		Else_Block->Codegen();
		}

		Builder.CreateBr(EndBB);
		Builder.SetInsertPoint(EndBB);  

		return NULL; 
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
	llvm::Value *Codegen(){

		llvm::Function *func = Builder.GetInsertBlock()->getParent();
	
		llvm::BasicBlock* WhileBB = llvm::BasicBlock::Create(TheContext, "while", func);
		llvm::BasicBlock* WhileTrueBB = llvm::BasicBlock::Create(TheContext, "whiletrue", func);
		llvm::BasicBlock* WhileEndBB = llvm::BasicBlock::Create(TheContext, "whileend", func);     
		
		Builder.CreateBr(WhileBB);
		Builder.SetInsertPoint(WhileBB);

		llvm::Value* Cond = Condition->Codegen(); 

		Builder.CreateCondBr(Cond, WhileTrueBB, WhileEndBB);
		Builder.SetInsertPoint(WhileTrueBB);

		While_Block->Codegen(); 

		Builder.CreateBr(WhileBB);   
		Builder.SetInsertPoint(WhileEndBB);
   
    	return NULL; 
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
	llvm::Value *Codegen(){

		llvm::Value* val;

		if(Return_Value != NULL){ 
		val = Return_Value->Codegen();
		retVal = val;
		Builder.CreateRet(retVal);
		retVal = NULL;
			}

		return val;
	}	
};

class breakStmtAST : public decafAST{
  string str(){
    return string("BreakStmt");
  }
  llvm::Value *Codegen(){

	llvm::BasicBlock* BreakBB = (llvm::BasicBlock*)(access_symtbl("break")); 
    if(BreakBB != NULL){
      Builder.CreateBr(BreakBB);
    }  

  }
};

class continueStmtAST : public decafAST{
  string str(){
    return string("ContinueStmt");
  }
  llvm::Value *Codegen(){
	  
	llvm::BasicBlock* ContinueBB = (llvm::BasicBlock*)(access_symtbl("continue")); 
    if(ContinueBB != NULL){
      Builder.CreateBr(ContinueBB);
    }

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
  llvm::Value *Codegen(){

	llvm::Function *func = Builder.GetInsertBlock()->getParent();
    
    llvm::BasicBlock* ForBB = llvm::BasicBlock::Create(TheContext, "for", func);
    llvm::BasicBlock* ForBodyBB = llvm::BasicBlock::Create(TheContext, "forbody", func);
    llvm::BasicBlock* ForAssignBB = llvm::BasicBlock::Create(TheContext, "forassign", func);
    llvm::BasicBlock* ForEndBB = llvm::BasicBlock::Create(TheContext, "forend", func);     

    Pre_Assign_List->Codegen();
    
    Builder.CreateBr(ForBB);
    Builder.SetInsertPoint(ForBB);

    llvm::Value* Cond = Condition->Codegen();

    Builder.CreateCondBr(Cond, ForBodyBB, ForEndBB);
    Builder.SetInsertPoint(ForBodyBB);

    For_Block->Codegen();

    Builder.CreateBr(ForAssignBB);
    Builder.SetInsertPoint(ForAssignBB); 

    Loop_Assign_List->Codegen();
    Builder.CreateBr(ForBB);

    Builder.SetInsertPoint(ForEndBB);
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
  llvm::Value *Codegen(){
	  print_debug(debug, "unary_START");

	  llvm::Value* R_Value = Value->Codegen();

	if(Op == "Not"){
		return Builder.CreateNot(R_Value,"unottmp");
	}
	if(Op == "UnaryMinus"){
		return Builder.CreateNeg(R_Value,"unegtmp");
	}
	print_debug(debug, "unary_END");
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

