%{
    #include "Hashtable.h"

    int yyerror(std::string yaccProvidedMessage);
    int yylex(void);

    extern int yylineno;
    extern char* yytext;
    extern char* yyval;
    extern FILE* yyin;
    extern FILE* yyout;


    int scopecounter = 0;
    int index = 0;
    int FunctionFlag = 0;
    int ArrayFlag = 0;

    char default_func_name[3] = "_";
    Scopes ScopeArray[15];
    SymbolTable ST = new SymbolTable();

    AddToTokenList(0, "print",  LIBFUNC , ST);
    AddToTokenList(0, "input", LIBFUNC, ST);
    AddToTokenList(0,"objectmemberkeys",  LIBFUNC, ST);
    AddToTokenList(0, "objecttotalmembers", LIBFUNC, ST);
    AddToTokenList(0, "objectcopy", LIBFUNC, ST);
    AddToTokenList(0, "totalarguments", LIBFUNC, ST);
    AddToTokenList(0, "argument", LIBFUNC, ST);
    AddToTokenList(0, "typeof", LIBFUNC, ST);
    AddToTokenList(0, "strtonum",LIBFUNC,, ST);
    AddToTokenList(0, "sqrt", LIBFUNC, ST);
    AddToTokenList(0, "cos", LIBFUNC, ST);
    AddToTokenList(0, "sin", LIBFUNC, ST);

%}




%start program

%token IF
%token ELSE
%token WHILE
%token FOR
%token BREAK 
%token CONTINUE
%token FUNCTION
%token RETURN
%token AND
%token OR
%token NOT
%token LOCAL
%token TRUE
%token FALSE
%token NIL 


%token ASSIGN
%token PLUS 
%token MINUS
%token MULTIPLY
%token DIVIDE
%token MODULO
%token EQUAL
%token NOT_EQUAL
%token PLUS_PLUS
%token MINUS_MINUS 
%token GREATER_THAN
%token LESS_THAN
%token GREATER_OR_EQUAL
%token LESS_OR_EQUAL 

%token DECIMAL
%token FLOAT

%token OPEN_BRACKET
%token CLOSE_BRACKET 
%token OPEN_SQUARE_BRACKET
%token CLOSE_SQUARE_BRACKET
%token OPEN_PARENTHESIS
%token CLOSE_PARENTHESIS
%token SEMICOLON
%token COLON
%token DOUBLE_COLON
%token COMMA
%token DOT
%token DOUBLE_DOT

%token ID 


%token COMMENT
%token BLOCK_COMMENT

%token STRING
%token OTHER
%token WHITESPACE [ \t\r\v]+

%right ASSIGN
%left AND OR
%nonassoc EQUAL NOT_EQUAL
%nonassoc GREATER_THAN GREATER_OR_EQUAL LESS_THAN LESS_OR_EQUAL
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO
%right NOT PLUS_PLUS MINUS_MINUS UMINUS
%left DOT DOUBLE_DOT
%left OPEN_BRACKET CLOSE_BRACKET 
%left OPEN_SQUARE_BRACKET CLOSE_SQUARE_BRACKET 
%left OPEN_PARENTHESIS CLOSE_PARENTHESIS


%nonassoc IF
%nonassoc ELSE

%union{
    std::string strval;
    int intval;
}

%%


program:        statement statement         {std::cout<< "Program --> Statements" << endl;}
                | statement                 {std::cout<< "Program --> Statements" << endl;}
                ;

statement:      expression SEMICOLON        {std::cout << "Statement --> expression;"<< endl;}
                | ifstatement               {std::cout << "Statement --> ifstatement;"<< endl;}   
                | whilestatement            {std::cout << "Statement --> whilestatement;"<< endl;}
                | forstatement              {std::cout << "Statement --> forstatement;"<< endl;}
                | returnstatement           {std::cout << "Statement --> returnstatement;"<< endl;}
                | BREAK SEMICOLON           {std::cout << "Statement --> break;"<< endl;}
                | CONTINUE SEMICOLON        {std::cout << "Statement --> continue;"<< endl;}
                | block                     {std::cout << "Statement --> block;"<< endl;}
                | FunctionDefinition        {std::cout << "Statement --> FunctionDefinition;"<< endl;}
                | SEMICOLON                 {std::cout << "Statement --> semicolon;"<< endl;}
                ;

expression:     assignexpression            {std::cout << "expression --> assignexpression"<< endl;}
                | expression op expression  {std::cout << "expression --> expr op expr"<< endl;}
                | term                      {std::cout << "expression --> term"<< endl;}
                ;

op:             PLUS                        {std::cout << "operator --> +"<< endl;}
                | MINUS                     {std::cout << "operator --> -"<< endl;}
                | MULTIPLY                  {std::cout << "operator --> *"<< endl;}
                | DIVIDE                    {std::cout << "operator --> /"<< endl;}
                | MODULO                    {std::cout << "operator --> %"<< endl;}
                | GREATER_OR_EQUAL          {std::cout << "operator --> >="<< endl;}
                | GREATER_THAN              {std::cout << "operator --> >"<< endl;}
                | LESS_OR_EQUAL             {std::cout << "operator --> <="<< endl;}
                | LESS_THAN                 {std::cout << "operator --> <"<< endl;}
                | EQUAL                     {std::cout << "operator --> =="<< endl;}
                | NOT_EQUAL                 {std::cout << "operator --> !="<< endl;}
                | AND                       {std::cout << "operator --> and"<< endl;}
                | OR                        {std::cout << "operator --> or"<< endl;}
                ;

term:           OPEN_PARENTHESIS expression CLOSE_PARENTHESIS   {std::cout << "term-->(expr)" << std::endl;}
                | NOT expression                                {std::cout << "term-->!expr" << std::endl;}
                | MINUS expression %prec UMINUS                 {std::cout << "term-->-expr" << std::endl;}
                | PLUS_PLUS lvalue          {
                                                for (int i = scopecounter; i>-1; --i){
                                                    try{
                                                        if(CheckIfFunction(i, yyval.stringValue, ST){
                                                            throw e1;
                                                        }
                                                    }catch(e1){
                                                        std::cerr << "Error! line: "<< yylineno << " " << yyval.stringValue << " is a Function."
                                                    }
                                                    std::cout << "term --> ++lvalue" << std::endl;
                                                }
                                            }
                | lvalue PLUS_PLUS          {
                                               for (auto i = scopecounter; i>-1; --i){
                                                    try{
                                                        if(CheckIfFunction(i, yyval.stringValue, ST){
                                                            throw e1;
                                                        }
                                                    }catch(e1){
                                                        std::cerr << "Error! line: "<< yylineno << " " << yyval.stringValue << " is a Function."
                                                    }
                                                    std::cout << "term --> lvalue++" << std::endl;
                                                }
                                            }
                | MINUS_MINUS lvalue        {
                                                for (auto i = scopecounter; i>-1; --i){
                                                    try{
                                                        if(CheckIfFunction(i, yyval.stringValue, ST){
                                                            throw e1;
                                                        }
                                                    }catch(e1){
                                                        std::cerr << "Error! line: "<< yylineno << " " << yyval.stringValue << " is a Function."
                                                    }
                                                    std::cout << "term --> --lvalue" << std::endl; 
                                                }
                                            }
                | lvalue MINUS_MINUS        {
                                              for (auto i = scopecounter; i>-1; --i){
                                                    try{
                                                        if(CheckIfFunction(i, yyval.stringValue, ST){
                                                            throw e1;
                                                        }
                                                    }catch(e1){
                                                        std::cerr << "Error! line: "<< yylineno << " " << yyval.stringValue << " is a Function."
                                                    }
                                                    std::cout << "term --> lvalue--" << std::endl; 
                                                } 
                                            }
                | primary                   {std::cout << "term --> primary" << std::endl;}
                ;

assignexpression:   lvalue {
                                for (auto i = scopecounter; i>-1; --i){
                                    try{
                                        if(CheckIfFunction(i, yyval.stringValue, ST){
                                            throw e1;
                                        }
                                    }catch(e1){
                                        std::cerr << "Error! line: "<< yylineno << " " << yyval.stringValue << " is a Function."
                                    }
                                }
                            }ASSIGN expression {std::cout <<"assignexpression --> lvalue = expression" << std::endl;}
                    ;

primary:            lvalue          {std::cout << "primary --> lvalue" << std::endl;}
                    | call          {std::cout << "primary --> lvalue" << std::endl;}
                    | objectdef     {std::cout << "primary --> objectdef" << std::endl;}
                    | OPEN_PARENTHESIS funcdef CLOSE_PARENTHESIS{
                                    std::cout << "primary --> (funcdef)" << std::endl;
                    }
                    | const          {std::cout << "primary --> const" << std::endl;}
                    ;

lvalue:             ID  {
                            std::cout << "lvalue --> id" << std::endl;
                            try{
                                for (auto i = scopecounter; i>-1; --i){
                                    if(LookupInScope(i, yyval.stringValue, ST))
                                        break;
                                }
                                switch(i){
                                    case(-1):
                                        /*isws prepei na peta3ei error ws mh dhlwmenh*/
                                    case(0):
                                        AddToTokenList(scopecounter,yyval.stringValue, GLOBAL, yylineno, ST );
                                        break;
                                    default:
                                        AddToTokenList(scopecounter,yyval.stringValue, LOCAL, yylineno, ST );
                                        break;
                                }
                            }catch(ex){
                                std::cerr << "Error! lvalue with ID: " << yyval.stringValue << " not found" << std::endl;
                            }
                        }
                    |LOCAL ID{
                                std::cout << "lvalue --> local id" << std::endl;
                                if(!LookupInScope(scopecounter, YYText(), ST)){
                                    if(scopecounter){
                                        AddToTokenList(scopecounter, yyval.stringValue, LOCAL, yylineno, ST);
                                    }else{
                                        AddToTokenList(scopecounter, yyval.stringValue, GLOBAL, yylineno, ST);
                                    }
                                }
                    }
                    |DOUBLE_COLON ID {
                                        std::cout << "lvalue -->::id" << std::endl;
                                        try{
                                            if(!LookupInScope(scopecounter,YYText(),ST))
                                                throw ex;
                                        }catch(ex){
                                            std::cerr << "Error! Line: " << yylineno << " name: " << yyval.stringValue << " No such member in global scope." << std::endl;
                                        }
                    }
                    |member         {std::cout<< "lvalue --> member" << std::endl;}
                    ;

member:             lvalue DOT ID   {std::cout << "member-->lvalue.id" << std::endl;}
                    | lvalue OPEN_SQUARE_BRACKET expression CLOSE_SQUARE_BRACKET {
                        std::cout << "member-->lvalue[expr]" << std::endl;
                    }
                    | call DOT ID   {std::cout << "member-->call.id" << std::endl;}
                    | call OPEN_SQUARE_BRACKET expression CLOSE_SQUARE_BRACKET{
                        std::cout << "member-->call[expr]" << std::endl;
                    }
                    ;
call:               call OPEN_PARENTHESIS elist CLOSE_PARENTHESIS{
                        std::cout << "call--(elist)" << std::endl;
                    }
                    | lvalue callsuffix {
                        std::cout << "call-->lvalue callsuffix" << std::endl;
                    }
                    | OPEN_PARENTHESIS FunctionDefinition CLOSE_PARENTHESIS OPEN_PARENTHESIS elist CLOSE_PARENTHESIS{
                        std::cout << "call-->(funcdef)(elist)" << std::endl;
                    }
                    ;

callsuffix:         normcall        {std::cout << "callsuffix-->normcall" << std::endl;}
                    | methodcall    {std::cout << "callsuffix-->methodcall" << std::endl;}
                    ;
normcall:           OPEN_PARENTHESIS elist CLOSE_PARENTHESIS {
                            std::cout << "normcall-->(elist)" << std::endl;
                    }
                    ;
methodcall:         DOUBLE_DOT ID OPEN_PARENTHESIS elist CLOSE_PARENTHESIS {
                        std::cout << "methodcall-->..id(elist)" << std::endl;
                    }
                    ;
elist:              expression COMMA expression {std::cout << "elist-->expr , expr" << std::endl;}
                    | expression                {std::cout << "elist-->expr" << std::endl;}
                    | %empty                    {std::cout << "elist-->empty" << std::endl;}
                    ;
objectdef:          OPEN_SQUARE_BRACKET elist CLOSE_SQUARE_BRACKET {
                        std::cout << "objectdef-->elist" << std::endl;r
                    }
                    | OPEN_SQUARE_BRACKET indexed CLOSE_SQUARE_BRACKET {
                        std::cout << "objectdef-->indexed" << std::endl;
                    }
                    |    std::cout << "objectdef-->empty" << std::endl;
                    ;

indexed:            indexedelem COMMA indexedelem {std::cout << "indexed-->indexedelem , indexedelem" << std::endl;}
                    |%empty                       {std::cout << "indexed-->empty" << std::endl;}
                    ;

indexedelem:        OPEN_BRACKET expression COLON expression CLOSE_BRACKET{
                        std::cout << "indexedelem-->{elem:elem}" << std::endl;
                    }
                    ;
block:              OPEN_BRACKET CLOSE_BRACKET {std::cout << "block --> {}"<< endl;}
                    | OPEN_BRACKET {scopecounter++; ScopeArray[scopecounter].SetActive();} statement statements CLOSE_BRACKET{
                        std::cout<< "block --> {statement*}" << endl;
                        ScopeArray[scopecounter].SetInactive();
                        scopecounter--;
                    }
                    ;
FunctionDefinition: FUNCTION { 
                        AddToTokenList(scopecounter, "_", USERFUNC, yylineno, ST);
                    } OPEN_PARENTHESIS {scopecounter++;} idlist CLOSE_PARENTHESIS{scopecounter--;} block {
                        std::cout << "funcdef --> function(idlist){}" << std:: endl;
                    }
                    | FUNCTION ID{
                            try{
                                if(LookupInScope(scopecounter, yytext, ST)){
                                    throw ex2;
                                }else{
                                    AddToTokenList(scopecounter, yytext, USERFUNC, yylineno, ST);
                                }
                            }catch(ex2){
                                std::cerr << "Error! Redeclaration of function: " << yyval.stringValue << " Line : " << yylineno <<std::endl;                         
                            }
                        }OPEN_PARENTHESIS {scopecounter++;} idlist CLOSE_PARENTHESIS{scopecounter--;} block{
                          std::cout << "funcdef --> function ID(idlist){}" << std:: endl;

                        }
                    ;

const:              DECIMAL     {std::cout << "const-->int" << std::endl;}
                    | FLOAT     {std::cout << "const-->float" << std::endl;}
                    | STRING    {std::cout << "const-->string" << std::endl;}
                    | NIL       {std::cout << "const-->nil" << std::endl;}
                    | TRUE      {std::cout << "const-->true" << std::endl;}
                    | FALSE     {std::cout << "const-->false" << std::endl;}
                    ;

idlist:             ID COMMA ID {
                        try{
                            if(LookupInScope(scopecounter, yytext, ST)){
                                throw ex3;
                            }else if(CheckIfLibFunction(scopecounter, yytext, ST)){
                                throw ex4;
                            }else{
                                alpha_token_t Token;
                                Token.SetName(YYText);
                                Token.SetType(FORMAL);
                                Token.SetLine(yylineno);
                                auto its = ST.equal_range(scopecounter);
                                for(auto it = its.first; it!=its.second; ++it){
                                    it->second.push_back(std::move(Token));
                                    it->second.back().GetFunction().AddArgument(yytext);
                                }
                            }
                        }catch(ex3){
                            std::cerr << "Error! Line: " << yylineno << "redefinition of formal " << yylval.stringValue; << std::endl;
                        }catch(ex4){
                            std::cerr << "Error! Line: " << yylineno << " " << yylval.stringValue << " shadows a Library Function" << std::endl;
                        }
                        std::cout << "idlist-->ID , ID" << std::endl;
                    }
                    | ID        {
                            try{
                                if(LookupInScope(scopecounter, yytext, ST)){
                                    throw ex3;
                                }else if(CheckIfLibFunction(scopecounter, yytext, ST)){
                                    throw ex4;
                                }else{
                                    alpha_token_t Token;
                                    Token.SetName(YYText);
                                    Token.SetType(FORMAL);
                                    Token.SetLine(yylineno);
                                    auto its = ST.equal_range(scopecounter);
                                    for(auto it = its.first; it!=its.second; ++it){
                                        it->second.push_back(std::move(Token));
                                        it->second.back().GetFunction().AddArgument(yytext);
                                    }
                                }
                            }catch(ex3){
                                std::cerr << "Error! Line: " << yylineno << "redefinition of formal " << yylval.stringValue; << std::endl;
                            }catch(ex4){
                                std::cerr << "Error! Line: " << yylineno << " " << yylval.stringValue << " shadows a Library Function" << std::endl;
                            }
                            std::cout << "idlist-->ID" << std::endl;
                    }
                    |      {std::cout << "idlist-->empty" << std::endl;}
                    ;

ifstatement:        IF OPEN_PARENTHESIS expression CLOSE_PARENTHESIS statement {
                        std::cout << "ifstmt-->if (expr) stmt" << std::endl;}
                    |IF OPEN_PARENTHESIS expression CLOSE_PARENTHESIS statement ELSE statement {
                        std::cout << "ifstmt-->if (expr) stmt else stmt " << std::endl;}
                    ;
whilestatement:     WHILE OPEN_PARENTHESIS expression CLOSE_PARENTHESIS statement{
                        std::cout << "whilestmt-->if (expr) stmt" << std::endl;
                    }
                    ;
forstatement:       FOR OPEN_PARENTHESIS elist SEMICOLON expression SEMICOLON elist CLOSE_PARENTHESIS statement {
                        std::cout << "forstmt-->for (elist; expr; elist) stmt" << std::endl;
                    }
                    ;
returnstatement:    RETURN expression   {std::cout << "return-->return expr" << std::endl;}
                    |%empty             {std::cout << "return-->return"<< std::endl;}
                    ;

%%