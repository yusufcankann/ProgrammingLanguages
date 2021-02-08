%{
   #include "gpp_interpreter.h"
   #include <stdio.h>
   #include <stdlib.h>
	#include <string.h>
   int quote=0;
%}

%option caseless

%%
and     {return KW_AND;}
or      {return KW_OR;}
not     {return KW_NOT;}
equal   {return KW_EQUAL;}
less    {return KW_LESS;}
nil     {return KW_NIL;}
list    {return KW_LIST;}
append  {return KW_APPEND;}
concat  {return KW_CONCAT;}
set     {return KW_SET;}
deffun  {return KW_DEFFUN;}
for     {return KW_FOR;}
while     {return KW_WHILE;}
if      {return KW_IF;}
exit    {return KW_EXIT;}
defvar    {return KW_DEFVAR;}
load    {return KW_LOAD;}
disp    {return KW_DISP;}
true    {return KW_TRUE;}
false   {return KW_FALSE;}

[']   {return OP_APOS;}

[+]     {return OP_PLUS;}
[-]     {return OP_MINUS;}
[/]     {return OP_DIV;}
[*]     {return OP_MULT;}
[(]     {return OP_OP;}
[)]     {return OP_CP;}
[*][*]    {return OP_DBLMULT;}
["]     {      quote += 1;
               if(quote % 2 == 1) return OP_OC;
                  else return OP_CC;}

[,]     {return OP_COMMA;}
[;][;][ a-zA-Z0-9\/.+-]+	{return COMMENT;}


[0]|[1-9][0-9]* {
   yylval.number=atoi(yytext);
                    return VALUE;}

[a-zA-Z_][a-zA-Z0-9_]* {    strcpy(yylval.str, yytext);
                            return IDENTIFIER;}

[ \t\r]*    {;}
[\n]    {;}

[,;]+[a-zA-Z1-9]+.* {printf("Token ERROR\n"); exit(EXIT_FAILURE);}
[0-9][a-zA-Z]+.* {printf("Token ERROR\n"); exit(EXIT_FAILURE);}
[a-zA-Z0-9]+[*/+,;-]+.* {printf("Token ERROR\n"); exit(EXIT_FAILURE);}
[0-9]+.[^0-9\t\r\n)"[[:space:]]] {printf("Token ERROR\n");}
[a-zA-Z_]+.[^a-zA-Z0-9_)\t\r\n [[:space:]]]+ {printf("Token ERROR\n"); exit(EXIT_FAILURE);}

.|[0][a-zA-Z0-9]+|[*]{3,}|[+]{2,}|[-]{2,}|[/]{2,}|["]{3,}|[,]{2,}|[,]{3,}* {printf("Token ERROR\n"); exit(EXIT_FAILURE);}

%% 




