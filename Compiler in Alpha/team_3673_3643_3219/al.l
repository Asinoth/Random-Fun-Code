%{
    #include "alpha_token.h"
	#include "Hashtable.h"
	#include <fstream>
	#include <cstdio>
	#ifdef __cplusplus
		static int yyinput (void );
	#else
		static int input (void );
	#endif

	using namespace std;
	#define YY_DECL int alpha_yylex (void* yylval)
	#if defined(WIN32)
	#define YY_NO_UNISTD_H
	static int isatty (int i){return 0;}
	#elif defined(_WIN32_WCE)
	#define YY_NO_UNISTD_H
	static int isatty (void *i){return 0;}
	#endif

	int lineNo=0;
	


%}

IF "if"
ELSE "else"
WHILE "while"
FOR "for"
BREAK "break"
CONTINUE "continue"
FUNCTION "function"
RETURN "return"
AND "and"
OR "or"
NOT "not"
LOCAL "local"
TRUE "true",
FALSE "false"
NIL "nil"


ASSIGN "="
PLUS "+"
MINUS "-"
MULTIPLY "*"
DIVIDE "/"
MODULO "%"
EQUAL "=="
NOT_EQUAL "!="
PLUS_PLUS "++"
MINUS_MINUS "--"
GREATER_THAN ">"
LESS_THAN "<"
GREATER_OR_EQUAL ">="
LESS_OR_EQUAL "<="

DECIMAL (0[xX][0-9A-Fa-f]+)|([0-9]+)
FLOAT ([0-9]*\.[0-9](e|E)?[0-9]*)

OPEN_BRACKET \{
CLOSE_BRACKET \}
OPEN_SQUARE_BRACKET \[
CLOSE_SQUARE_BRACKET \]
OPEN_PARENTHESIS \(
CLOSE_PARENTHESIS \)
SEMICOLON \;
COLON \:
DOUBLE_COLON \::
COMMA \,
DOT \.
DOUBLE_DOT \..

ID [a-zA-Z][a-zA-Z_0-9]*


COMMENT "//".*
BLOCK_COMMENT "/*"

STRING \"
OTHER .
WHITESPACE [ \t\r\v]+

%option noyywrap 
%option yylineno

%%

"\n" {lineNo++;}
{IF}  {token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return IF; }
{ELSE}  {token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return ELSE;  }
{WHILE} {token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return WHILE; }
{FOR}  {token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl;  return FOR;}
{FUNCTION}      	{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return FUNCTION; } 
{RETURN} 			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return RETURN; }	
{BREAK}         	{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return BREAK; }  
{CONTINUE}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return CONTINUE; }
{TRUE}	 			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return TRUE; }
{FALSE}    			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return FALSE; }
{LOCAL}				{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return LOCAL; }
{NIL}				{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return NIL; }
{AND}	      		{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return AND; }
{NOT} 	        	{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return NOT; }
{OR}				{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "KEYWORD" <<std::endl; return OR; }
{STRING}          	{
						token++; 
						char character;
						int flag = 1;
						std::string str;
						while(character=yyinput() != EOF){
							if(character == '\\'){
								character  = yyinput();
								switch (character){
									case 'n':
										str += '\n';
										break;
									case 't':
										str += '\t';
										break;
									/*case 'r':
										str+= '/r';
										break;*/
									default:
										str += '\\';
										str += character;
								}
								unput(character);
							}else if(character == '\"'){
								std::cout << yylineno << ": #" << token << " " << "STRING" << " <-char" << "\n";
								flag = 0;
							}else{
								str += character;
							}
						}
						if(flag){
							std::cout << yylineno << "Error! No matching double quote.  "  << "STRING"<<str << " <-char" << "\n";
						}
						return STRING;
					}
{BLOCK_COMMENT}        {token++;
							char character;
							int opened_flag = 1;
							int nest = 0;
							while ((character = yyinput() !=EOF) && opened_flag){
								if (character == '*'){
									character = yyinput();
									switch (character){
										case '/':
											opened_flag--;
											break;
										default: 
											unput(character);
									}
								}else if(character == '/'){
									character = yyinput();
									switch (character){
										case '*':
											opened_flag++;
											nest++;
											break;
										default: 
											unput(character);
									}
									unput(character);
								}
							}

							if (opened_flag && nest){
								std::cout << " " << "BLOCK_COMMENT " << yylineno << " " << token << " " << "Error! Not properly closed nested comment " << yytext << "\n";
							}else if(opened_flag && !nest){
								std::cout << " " << "BLOCK_COMMENT " << yylineno << " " << token << " " << "Error! Not properly closed comment " << yytext << "\n";
							}else if(!opened_flag && nest) {
								std::cout << " " << "BLOCK_COMMENT " << yylineno << " " << token << " " << "Properly closed nested comment " << yytext << "\n";
							}else{
								std::cout << " " << "BLOCK_COMMENT " << yylineno << " " << token << " " << "Improperly closed block comment " << yytext << "\n";

							}
							return BLOCK_COMMENT;
						}
{DECIMAL}	   		{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "INTCONST" <<std::endl; return DECIMAL;  } 
{FLOAT}       		{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "REALNUMCONST" <<std::endl; return FLOAT; } 
{ASSIGN}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return ASSIGN; }
{PLUS}		    	{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return PLUS; }
{MINUS}		 		{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return MINUS; }
{MULTIPLY}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return MULTIPLY; }
{DIVIDE}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return DIVIDE; }
{MODULO}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return MODULO; }
{EQUAL}				{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return EQUAL; }
{NOT_EQUAL}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return NOT_EQUAL; }
{PLUS_PLUS}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return PLUS_PLUS; }
{MINUS_MINUS}    	{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return MINUS_MINUS; }
{GREATER_THAN}		{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return GREATER_THAN; }
{LESS_THAN}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return LESS_THAN; }
{GREATER_OR_EQUAL} 	{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return GREATER_OR_EQUAL;}
{LESS_OR_EQUAL}		{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OPERATOR" <<std::endl; return LESS_OR_EQUAL;}
{OPEN_BRACKET}		{token++; std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return OPEN_BRACKET; }
{CLOSE_BRACKET}		{token++; std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return CLOSE_BRACKET;}
{OPEN_SQUARE_BRACKET}	{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return  OPEN_SQUARE_BRACKET; }
{CLOSE_SQUARE_BRACKET}	{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return CLOSE_SQUARE_BRACKET; }
{OPEN_PARENTHESIS}		{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return OPEN_PARENTHESIS; }
{CLOSE_PARENTHESIS}		{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return CLOSE_PARENTHESIS;}
{SEMICOLON}				{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return SEMICOLON; }
{COMMA}					{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return COMMA; }
{COLON}					{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return COLON; }
{DOUBLE_COLON}			{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return DOUBLE_COLON;}
{DOT}					{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return DOT; }
{DOUBLE_DOT}	        {token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "PUNCTUATION MARK" <<std::endl; return DOUBLE_DOT; }
{ID} 					{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "ID" <<std::endl; return ID; }
{OTHER} 				{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "OTHER" <<std::endl; return OTHER;  }
{COMMENT} 				{token++;  std::cout <<yylineno<< ": #" <<token<<" "<<yytext<<" "<< "COMMENT" <<std::endl; return COMMENT; }					


{WHITESPACE} {}

%%

int main(int argc, char *argv[])
{
  	if (argc!=2){
		std::cout << "Wrong number of arguments!\n";
		return 1;
  	}else{
		FILE *F, *f_out; //input/output file handlers.
		yyin =F;
		if(!(yyin = fopen(argv[1], "r"))){
			std::cerr << "Cannot read file: " << argv[1] << "\n";
		}
		/*if(argc>=3){
			yyout=fopen(argv[2],"w+"); 
			//f_out=fopen(argv[2],"w+");
			//yyout=f_out;
		}
		else yyout=stdout;*/
		alpha_yylex(NULL);
		fclose(F);
		//fclose(f_out);
  	}

return 0;
}
