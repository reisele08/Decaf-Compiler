
%{
#include "default-defs.h"
#include "decafast.tab.h"
#include <cstring>
#include <string>
#include <iostream>
#include <cstdlib>

using namespace std;

int lineno = 1;
int tokenpos = 1;

%}
whitespace [\r|\t|\v|\f|\n| ]
char [\a|\b|\t|\v|\f|\r -\!\#-\[\]-~]
escaped_char \\(n|r|t|v|f|a|b|\\|\'|\")
char_lit_chars [\a|\b|\t|\n|\v|\f|\r -/&/(-\[\]-~]
char_lit  ({char_lit_chars}|{escaped_char})
char_no_n1 [\a|\b|\t|\v|\f|\r| -~]+
hex_digit [0-9A-Fa-f]
decimal_lit [0-9]
hex_lit (0(x|X){hex_digit}+) 
string_lit  ({char}|{escaped_char})*
error_unknown_escape_seq \"{string_lit}\\[^{escaped_char}]
error_newline_in_str \"{string_lit}\n{string_lit}\"
%%
  /*
    Pattern definitions for all tokens
  */
func                           { return T_FUNC; }
int                            { return T_INTTYPE; }
package                        { return T_PACKAGE; }
bool                           { return T_BOOLTYPE; }
break                          { return T_BREAK; }
continue                       { return T_CONTINUE; }
string                         { return T_STRINGTYPE; }
true                           { return T_TRUE; }
var                            { return T_VAR; }
void                           { return T_VOID; }
while                          { return T_WHILE; }
else                           { return T_ELSE; }
false                          { return T_FALSE; }
for                            { return T_FOR; }
return                         { return T_RETURN; }
null                           { return T_NULL; }
if                             { return T_IF; }
extern                         { return T_EXTERN; }
({decimal_lit}+)|({hex_lit})   { return T_INTCONSTANT; }
\{                             { return T_LCB; }
\}                             { return T_RCB; }
\(                             { return T_LPAREN; }
\)                             { return T_RPAREN; }
[a-zA-Z\_][a-zA-Z\_0-9]*       { return T_ID; }
{whitespace}+                  { return T_WHITESPACE; }
\&\&                           { return T_AND; }
\=                             { return T_ASSIGN; }
\'{char_lit}\'                 { return T_CHARCONSTANT; }
\,                             { return T_COMMA; }
\/\/{char_no_n1}\n             { return T_COMMENT; }
\/                             { return T_DIV; }
\.                             { return T_DOT; }
\==                            { return T_EQ; }
\>=                            { return T_GEQ; }
\>                             { return T_GT; }
\<<                            { return T_LEFTSHIFT; }
\<=                            { return T_LEQ; }
\[                             { return T_LSB; }
\<                             { return T_LT; }
\-                             { return T_MINUS; }
\%                             { return T_MOD; }
\*                             { return T_MULT; }
\!\=                           { return T_NEQ; }
\!                             { return T_NOT; }
\|\|                           { return T_OR; }
\+                             { return T_PLUS; }
\>\>                           { return T_RIGHTSHIFT; }
\]                             { return T_RSB; }
\;                             { return T_SEMICOLON; }
\"{string_lit}\"               { return T_STRINGCONSTANT; }
  /* Error Messages */  
  /*
{error_unknown_escape_seq}                        { return 52; }
{error_newline_in_str}                            { return 53; }
\"                                                { return 54; }
\'{char_lit}(\\['\\(n|r|t|v|f|a|b]|[^'\\\n])+\'   { return 55; }
\'{char_lit}                                      { return 56; }
\'\'                                              { return 57; }
^{char}                                           { return 58; }
  */

%%

int yyerror(const char *s) {
  cerr << lineno << ": " << s << " at char " << tokenpos << endl;
  return 1;
}

  /*
int main () {
  int token;
  int line_count = 1;
  int pos_count = 0;
  string lexeme;
  while ((token = yylex())) {
    if (token > 0) {
      lexeme.assign(yytext);
      switch(token) {
        case 1: cout << "T_FUNC " << lexeme << endl; break;
        case 3: cout << "T_PACKAGE " << lexeme << endl; break;
        case 4: cout << "T_LCB " << lexeme << endl; break;
        case 5: cout << "T_RCB " << lexeme << endl; break;
        case 6: cout << "T_LPAREN " << lexeme << endl; break;
        case 7: cout << "T_RPAREN " << lexeme << endl; break;
        case 8: cout << "T_ID " << lexeme << endl; break;
        case 9:
          cout << "T_WHITESPACE ";
          for(int i = 0; i < lexeme.size(); ++i){
              if(yytext[i] == '\n'){
                cout << "\\n";
                line_count++;
                pos_count = 0;
              }
              else{
                pos_count++;
                cout << yytext[i];
              }
          }
          cout << endl; break;
        case 11: cout << "T_AND " << lexeme << endl; break;
        case 12: cout << "T_ASSIGN " << lexeme << endl; break;
        case 13: cout << "T_BOOLTYPE " << lexeme << endl; break;
        case 14: cout << "T_BREAK " << lexeme << endl; break;
        case 15: cout << "T_CHARCONSTANT " << lexeme << endl; break;
        case 16: cout << "T_COMMA " << lexeme << endl; break;
        case 17:
         cout << "T_COMMENT ";
         for(int i = 0; i < lexeme.size(); ++i){
           if(yytext[i] != '\n'){
             cout << yytext[i];
           }
         }
          line_count++;
          cout << "\\n" << endl; break;
        case 18: cout << "T_CONTINUE " << lexeme << endl; break;
        case 19: cout << "T_DIV " << lexeme << endl; break;
        case 20: cout << "T_DOT " << lexeme << endl; break;
        case 21: cout << "T_ELSE " << lexeme << endl; break;
        case 22: cout << "T_EQ " << lexeme << endl; break;
        case 23: cout << "T_FALSE " << lexeme << endl; break;
        case 24: cout << "T_FOR " << lexeme << endl; break;
        case 25: cout << "T_GEQ " << lexeme << endl; break;
        case 26: cout << "T_GT " << lexeme << endl; break;
        case 27: cout << "T_IF " << lexeme << endl; break;
        case 28: cout << "T_INTCONSTANT " << lexeme << endl; break;
        case 29: cout << "T_EXTERN " << lexeme << endl; break;
        case 2: cout << "T_INTTYPE " << lexeme << endl; break;
        case 30: cout << "T_LEFTSHIFT " << lexeme << endl; break;
        case 31: cout << "T_LEQ " << lexeme << endl; break;
        case 32: cout << "T_LSB " << lexeme << endl; break;
        case 33: cout << "T_LT " << lexeme << endl; break;
        case 34: cout << "T_MINUS " << lexeme << endl; break;
        case 35: cout << "T_MOD " << lexeme << endl; break;
        case 36: cout << "T_MULT " << lexeme << endl; break;
        case 37: cout << "T_NEQ " << lexeme << endl; break;
        case 38: cout << "T_NOT " << lexeme << endl; break;
        case 39: cout << "T_NULL " << lexeme << endl; break;
        case 40: cout << "T_OR " << lexeme << endl; break;
        case 41: cout << "T_PLUS " << lexeme << endl; break;
        case 42: cout << "T_RETURN " << lexeme << endl; break;
        case 43: cout << "T_RIGHTSHIFT " << lexeme << endl; break;
        case 44: cout << "T_RSB " << lexeme << endl; break;
        case 45: cout << "T_SEMICOLON " << lexeme << endl; break;
        case 46: cout << "T_STRINGCONSTANT " << lexeme << endl; break;
        case 47: cout << "T_STRINGTYPE " << lexeme << endl; break;
        case 48: cout << "T_TRUE " << lexeme << endl; break;
        case 49: cout << "T_VAR " << lexeme << endl; break;
        case 50: cout << "T_VOID " << lexeme << endl; break;
        case 51: cout << "T_WHILE " << lexeme << endl; break;
        case 52: cerr << "Error: unknown escape sequence in string constant " << endl;
        printf("Lexical error: line %d", line_count); printf(", position: %d", yyleng); exit(EXIT_FAILURE);

        case 53: cerr << "Error: newline in string constant " << endl;
        printf("Lexical error: line %d", line_count); printf(", position: %d", yyleng); exit(EXIT_FAILURE);

        case 54: cerr << "Error: string constant is missing closing delimiter " << endl;
        printf("Lexical error: line %d", line_count); printf(", position: %d", yyleng); exit(EXIT_FAILURE);

        case 55: cerr << "Error: char constant length is greater than one " << endl;
        printf("Lexical error: line %d", line_count); printf(", position: %d", yyleng); exit(EXIT_FAILURE);

        case 56: cerr << "Error: unterminated char constant " << endl;
        printf("Lexical error: line %d", line_count); printf(", position: %d", yyleng); exit(EXIT_FAILURE);

        case 57: cerr << "Error: char constant has zero width " << endl;
        printf("Lexical error: line %d", line_count); printf(", position: %d", yyleng); exit(EXIT_FAILURE);

        case 58: cerr << "Error: unexpected character in input " << endl;
        printf("Lexical error: line %d", line_count); printf(", position: %d", yyleng); exit(EXIT_FAILURE);

        default: exit(EXIT_FAILURE);
      }
    } else {
      if (token < 0) {
        exit(EXIT_FAILURE);
      }
    }
  }
  exit(EXIT_SUCCESS);
}
  */