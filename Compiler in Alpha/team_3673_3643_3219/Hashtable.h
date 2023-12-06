#include <unordered_map>
#include <map>
#include <vector>
#include <typeinfo>
#include "alpha_token.h"

typedef std::unordered_multimap<int, std::vector<alpha_token_t>> SymbolTable;

class Scopes{
    private:
        int ScopeID;
        bool isActive;
    public:
        void SetScopeActive();
        void SetScopeInactive();
        void SetScopeID(int a);
        int GetScopeID();
};


int alpha_token_t::GetLine(){
    return lineno;
}

void alpha_token_t::SetLine(int l){
    lineno = l;
}


int Function::GetArgc(){
    return argc;
}
void Function::UpdateArgc(){
    argc = argv.size();
}

std::vector<std::string> Function::GetArgv(){
    return argv;
}
void Function::AddArgument(std::string argname){
    Function::GetArgv().push_back(argname);
}


void Scopes::SetScopeActive(){
    isActive = true;
}

void Scopes::SetScopeInactive(){
    isActive = false;
}

void Scopes::SetScopeID(int a){
    ScopeID = a;
}

int Scopes::GetScopeID(){
    return ScopeID;
}


void alpha_token_t::SetType(SymbolType T){
    alpha_token_t::type = T;
}

void alpha_token_t::SetUnionType(UnionType u){
    alpha_token_t::uT = u;
}

std::string alpha_token_t::GetName(){
    return name;
}
void alpha_token_t::SetName(std::string n){
    name.assign(n);
}
UnionType alpha_token_t::GetUnionType(){
    return uT;
}

SymbolType alpha_token_t::GetSymbolType(){
    return type;
}
Function alpha_token_t::GetFunction(){
    return f;
}
Variable alpha_token_t::GetVariable(){
    return v;
}

Function createFunction( std::vector<std::string> arv){
    Function fu;
    for (auto i : arv){
        fu.AddArgument(i);
    }
    return fu;
}
Function createFunction(){
    Function fu;
    return fu;
}

void FunctionAsToken(std::string name, Function f, SymbolType Type, int line){
    alpha_token_t FaT;
    FaT.SetValue(f);
    FaT.SetUnionType(FUNCTION);
    FaT.SetType(Type);
    FaT.SetLine(line);

}


bool LookupInScope(int ScopeNum, std::string name, SymbolTable ST){
    auto its = ST.equal_range(ScopeNum);
    for (auto it = its.first; it != its.second; ++it){
        for (unsigned i = 0; i < it->second.size(); ++i){
            if(it->second.at(i).GetName().compare(name)==0){
                return true;
            }else{
                return false;
            }
        }
    }
}

bool LookupInScope(int ScopeNum, std::string name, UnionType T, SymbolTable ST){
    auto its = ST.equal_range(ScopeNum);
    for (auto it = its.first; it != its.second; ++it){
        for (unsigned i = 0; i < it->second.size(); ++i){
            if((it->second.at(i).GetName().compare(name)==0) && (it->second.at(i).GetUnionType()==T)){
                return true;
            }else{
                return false;
            }
        }
    }
}

bool CheckIfFunction(int ScopeNum, std::string name, SymbolTable ST){
    auto its = ST.equal_range(ScopeNum);
    for (auto it = its.first; it != its.second; ++it){
        for (unsigned i = 0; i < it->second.size(); ++i){
            if((it->second.at(i).GetName().compare(name)==0) && (it->second.at(i).GetUnionType()==FUNCTION)){
                return true;
            }else{
                return false;
            }
        }
    }
}
bool CheckIfLibFunction(int ScopeNum, std::string name, SymbolTable ST){
    auto its = ST.equal_range(ScopeNum);
    for (auto it = its.first; it != its.second; ++it){
        for (unsigned i = 0; i < it->second.size(); ++i){
            if((it->second.at(i).GetName().compare(name)==0) && (it->second.at(i).GetSymbolType()==LIBFUNC)){
                return true;
            }else{
                return false;
            }
        }
    }
}


void AddToTokenList(int ScopeNum, std::string name, SymbolType T, int lineno, SymbolTable ST){
    alpha_token_t Token;
    Token.SetName(name);
    Token.SetType(T);
    Token.SetLine(lineno);
    auto its = ST.equal_range(ScopeNum);
    for(auto it = its.first; it!=its.second; ++it){
        it->second.push_back(std::move(Token));
    }
}










