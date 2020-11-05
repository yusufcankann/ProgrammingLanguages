%{
    #define ERROR -1
    #define RETURN 5
    #define NEW_LINE 1
    #define IDENTIFIER 2
    #define WHITESPACE 3
    #define QUOTES 4
    #include <stdio.h>
    #include <stdlib.h>

    int quote=0;
%}

%option caseless

%%
and     {printf("KW_AND\n"); return RETURN;}
or      {printf("KW_OR\n"); return RETURN;}
not     {printf("KW_NOT\n"); return RETURN;}
equal   {printf("KW_EQUAL\n"); return RETURN;}
less    {printf("KW_LESS\n"); return RETURN;}
nil     {printf("KW_NIL\n"); return RETURN;}
list    {printf("KW_LIST\n"); return RETURN;}
append  {printf("KW_APPEND\n"); return RETURN;}
concat  {printf("KW_CONCAT\n"); return RETURN;}
set     {printf("KW_SET\n"); return RETURN;}
deffun  {printf("KW_DEFFUN\n"); return RETURN;}
for     {printf("KW_FOR\n"); return RETURN;}
if      {printf("KW_IF\n"); return RETURN;}
exit    {printf("KW_EXIT\n"); return RETURN;}
load    {printf("KW_LOAD\n"); return RETURN;}
disp    {printf("KW_DISP\n"); return RETURN;}
true    {printf("KW_TRUE\n"); return RETURN;}
false   {printf("KW_FALSE\n"); return RETURN;}

[+]     {printf("OP_PLUS\n"); return RETURN;}
[-]     {printf("OP_MINUS\n"); return RETURN;}
[/]     {printf("OP_DIV\n"); return RETURN;}
[*]     {printf("OP_MULT\n"); return RETURN;}
[(]     {printf("OP_OP\n"); return RETURN;}
[)]     {printf("OP_CP\n"); return RETURN;}
[*][*]    {printf("OP_DBLMULT\n"); return RETURN;}
["]     {return QUOTES;}
[,]     {printf("OP_COMMA\n"); return RETURN;}
[;][;]      {printf("COMMENT\n");
                int r;
                r=yylex();
                while(r!= NEW_LINE){
                    r=yylex();
                }
                return RETURN;
            }

[a-zA-Z_][a-zA-Z0-9_]* {return IDENTIFIER;}

[0]|[1-9][0-9]* {printf("VALUE\n"); return RETURN;}

[ \t\r]*    {return WHITESPACE;}

[\n]    {return NEW_LINE;}

.|[0][a-zA-Z0-9]+|[*]{3,}|[+]{2,}|[-]{2,}|[/]{2,}|["]{3,}|[,]{2,}|[;]{3,} {return ERROR;}

%%

int yywrap(){}

/*Token control.*/
int lex_control(int lexreturn,int file){
    if(lexreturn==ERROR){
        printf("SYNTAX_ERROR %s cannot be tokenized.\n",yytext);
        return -1;
    }
    else if(lexreturn==IDENTIFIER) printf("IDENTIFIER\n");
    else if(lexreturn==QUOTES){
        if(quote==0){
            quote++;
            printf("OP_OC\n");
        }
        else{
            printf("OP_CC\n");
            quote=0;  
        } 
    }
    else
        if(lexreturn==NEW_LINE){
            if(file!=1) printf("> ");
            quote=0;
        }
    
    return 0;
}
int main(int argc, char **argv){
    int lexreturn=0;


    /*READ FILE PART*/
    if (argc>1){
        yyin= fopen(argv[1],"r");
    
        while((lexreturn=yylex())!=0){
            lex_control(lexreturn,1);
        }

        fclose(yyin);
        yyin=stdin;
        yyrestart(yyin);
    }

    /*INTERPRETER PART.*/
    printf("> ");
    lexreturn=yylex();
    while(1){
        if(lex_control(lexreturn,0)==-1){
            return 0;
        }
        lexreturn=yylex();
    }

    return 0;
}