
%{

#include <iostream>
#include <cstdlib>

using namespace std;

%}
whitespace [\r|\t|\v|\f|\n| ]
char [\a|\b|\t|\v|\f|\r -\!\#-\[\]-~]
escaped_char \\(n|r|t|v|f|a|b|\\|\'|\")
char_lit_chars [\a|\b|\t|\n|\v|\f|\r -/&/(-\[\]-~]
char_lit  {char_lit_chars}|{escaped_char}
char_no_n1 [\a|\b|\t|\v|\f|\r| -~]+
hex_digit [0-9A-Fa-f]
decimal_lit [0-9]
hex_lit (0(x|X){hex_digit}+) 
string_lit  ({char}|{escaped_char})*
%%
  /*
    Pattern definitions for all tokens
  */
func                           { return 1; }
int                            { return 2; }
package                        { return 3; }
bool                           { return 13; }
break                          { return 14; }
continue                       { return 18; }
string                         { return 47; }
true                           { return 48; }
var                            { return 49; }
void                           { return 50; }
while                          { return 51; }
else                           { return 21; }
false                          { return 23; }
for                            { return 24; }
return                         { return 42; }
null                           { return 39; }
if                             { return 27; }
extern                         { return 29; }
({decimal_lit}+)|({hex_lit})   { return 28; }
\{                             { return 4; }
\}                             { return 5; }
\(                             { return 6; }
\)                             { return 7; }
[a-zA-Z\_][a-zA-Z\_0-9]*       { return 8; }
{whitespace}+                 { return 9; }
\&\&                             { return 11; }
\=                              { return 12; }
\'{char_lit}\'                 { return 15; }
\,                             { return 16; }
\/\/{char_no_n1}\n             { return 17; }
\/                             { return 19; }
\.                             { return 20; }
\==                            { return 22; }
\>=                            { return 25; }
\>                             { return 26; }
\<<                            { return 30; }
\<=                            { return 31; }
\[                             { return 32; }
\<                             { return 33; }
\-                             { return 34; }
\%                             { return 35; }
\*                             { return 36; }
\!\=                            { return 37; }
\!                             { return 38; }
\|\|                           { return 40; }
\+                             { return 41; }
\>\>                           { return 43; }
\]                             { return 44; }
\;                             { return 45; }
\"{string_lit}\"               { return 46; }
.                              { cerr << "Error: unexpected character in input" << endl; return -1; }

%%
int main () {
  int token;
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
              }
              else{
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
