
#include <string>
#include <list>
#include <vector>

int token = 0;
int line = 0;
char *tmp;

enum UnionType
{
    FUNCTION,
    VARIABLE
};
enum SymbolType
{
    GLOBAL,
    LOCAL,
    FORMAL,
    USERFUNC,
    LIBFUNC
};

class Variable
{
private:
    int value;
public:
    int GetValue();
    void SetValue(int s);
};
class Argument
{
public:
    std::string name;
};

class Function
{
private:
    std::vector<std::string> argv;
    int argc=0;
public:
    int GetArgc();
    void UpdateArgc();
    std::vector<std::string> GetArgv();    
    void AddArgument(std::string argname);
    
};

class alpha_token_t
{
private:
    std::string name;
    SymbolType type;
    UnionType uT;
    int lineno;
    Function f;
    Variable v;

public:
    alpha_token_t() {}
    ~alpha_token_t(){};
    std::string GetName();
    void SetName(std::string n);
    void SetValue(Variable v);
    void SetValue(Function f);
    void SetType(SymbolType T);
    void SetUnionType(UnionType u);
    Function GetFunction();
    Variable GetVariable();
    UnionType GetUnionType();
    SymbolType GetSymbolType();
    void SetLine(int s);
    int GetLine();
};
