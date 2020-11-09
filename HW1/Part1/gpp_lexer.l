%{
    #define ERROR -1
    #define KW_AND 1
    #define KW_OR 2
    #define KW_NOT 3
    #define KW_EQUAL 4
    #define KW_LESS 5
    #define KW_NIL 6
    #define KW_LIST 7
    #define KW_APPEND 8
    #define KW_CONCAT 9
    #define KW_SET 10
    #define KW_DEFFUN 11
    #define KW_FOR 12
    #define KW_IF 13
    #define KW_EXIT 14
    #define KW_LOAD 15
    #define KW_DISP 16
    #define KW_TRUE 17
    #define KW_FALSE 18
    #define OP_PLUS 19
    #define OP_MINUS 20
    #define OP_DIV 21
    #define OP_MULT 22
    #define OP_OP 23
    #define OP_CP 24
    #define OP_DBLMULT 25
    #define OP_COMMA 26
    #define COMMENT 27
    #define NEW_LINE 28
    #define IDENTIFIER 29
    #define WHITESPACE 30
    #define VALUE 31
    #define QUOTES 32

    #include <stdio.h>
    #include <stdlib.h>

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
if      {return KW_IF;}
exit    {return KW_EXIT;}
load    {return KW_LOAD;}
disp    {return KW_DISP;}
true    {return KW_TRUE;}
false   {return KW_FALSE;}

[+]     {return OP_PLUS;}
[-]     {return OP_MINUS;}
[/]     {return OP_DIV;}
[*]     {return OP_MULT;}
[(]     {return OP_OP;}
[)]     {return OP_CP;}
[*][*]    {return OP_DBLMULT;}
["]     {return QUOTES;}
[,]     {return OP_COMMA;}
[;][;]      {   int r;
                r=yylex();
                while(r!= NEW_LINE){
                    r=yylex();
                }
                return COMMENT;
            }

[0]|[1-9][0-9]* {return VALUE;}
[a-zA-Z_][a-zA-Z0-9_]* {return IDENTIFIER;}
[ \t\r]*    {return WHITESPACE;}
[\n]    {return NEW_LINE;}


[a-zA-Z0-9]+[*/+,;-]+ {return ERROR;}
[0-9]+.[^0-9\t\r\n)"[[:space:]]] {return ERROR;}
[a-zA-Z_]+.[^a-zA-Z0-9_)\t\r\n [[:space:]]]+ {return ERROR;}
.|[0][a-zA-Z0-9]+|[*]{3,}|[+]{2,}|[-]{2,}|[/]{2,}|["]{3,}|[,]{2,}|[;]{3,} {return ERROR;}

%%

int yywrap(){}

/*Token control.*/
int lex_control(int lexreturn,FILE **fp){
    if(lexreturn==ERROR){
        fprintf(*fp,"SYNTAX_ERROR %s cannot be tokenized.\n",yytext);
        return -1;
    }
    else if(lexreturn==KW_AND) fprintf(*fp,"KW_AND\n");
    else if(lexreturn==KW_OR) fprintf(*fp,"KW_OR\n");
    else if(lexreturn==KW_NOT) fprintf(*fp,"KW_NOT\n");
    else if(lexreturn==KW_EQUAL) fprintf(*fp,"KW_EQUAL\n");
    else if(lexreturn==KW_LESS) fprintf(*fp,"KW_LESS\n");
    else if(lexreturn==KW_NIL) fprintf(*fp,"KW_NIL\n");
    else if(lexreturn==KW_LIST) fprintf(*fp,"KW_LIST\n");
    else if(lexreturn==KW_APPEND) fprintf(*fp,"KW_APPEND\n");
    else if(lexreturn==KW_CONCAT) fprintf(*fp,"KW_CONCAT\n");
    else if(lexreturn==KW_SET) fprintf(*fp,"KW_SET\n");
    else if(lexreturn==KW_DEFFUN) fprintf(*fp,"KW_DEFFUN\n");
    else if(lexreturn==KW_FOR) fprintf(*fp,"KW_FOR\n");
    else if(lexreturn==KW_IF) fprintf(*fp,"KW_IF\n");
    else if(lexreturn==KW_EXIT) fprintf(*fp,"KW_EXIT\n");
    else if(lexreturn==KW_LOAD) fprintf(*fp,"KW_LOAD\n");
    else if(lexreturn==KW_DISP) fprintf(*fp,"KW_DISP\n");
    else if(lexreturn==KW_TRUE) fprintf(*fp,"KW_TRUE\n");
    else if(lexreturn==KW_FALSE) fprintf(*fp,"KW_FALSE\n");
    else if(lexreturn==OP_PLUS) fprintf(*fp,"KW_PLUS\n");
    else if(lexreturn==OP_MINUS) fprintf(*fp,"KW_MINUS\n");
    else if(lexreturn==OP_DIV) fprintf(*fp,"KW_DIV\n");
    else if(lexreturn==OP_MULT) fprintf(*fp,"OP_MULT\n");
    else if(lexreturn==OP_OP) fprintf(*fp,"OP_OP\n");
    else if(lexreturn==OP_CP) fprintf(*fp,"OP_CP\n");
    else if(lexreturn==OP_DBLMULT) fprintf(*fp,"OP_DBLMULT\n");
    else if(lexreturn==OP_COMMA) fprintf(*fp,"OP_COMMA\n");
    else if(lexreturn==COMMENT) fprintf(*fp,"COMMENT\n");
    else if(lexreturn==IDENTIFIER) fprintf(*fp,"IDENTIFIER\n");
    else if(lexreturn==VALUE) fprintf(*fp,"VALUE\n");
    else if(lexreturn==QUOTES){
        if(quote==0){
            quote++;
            fprintf(*fp,"OP_OC\n");
        }
        else{
            fprintf(*fp,"OP_CC\n");
            quote=0;  
        } 
    }
    else
        if(lexreturn==NEW_LINE)
            quote=0;
    
    return 0;
}
int main(int argc, char **argv){
    int lexreturn=0;
    int new_line_flag=0;
    FILE *fp;
    fp = fopen("parsed_cpp.txt", "w");

    /*READ FILE PART*/
    if (argc>1){
        yyin= fopen(argv[1],"r");
        while((lexreturn=yylex())!=0){
            if(lex_control(lexreturn,&fp)==-1) break;
        }

        fclose(yyin);
        yyin=stdin;
        yyrestart(yyin);
    }
    else{
        /*INTERPRETER PART.*/
        lexreturn=yylex();
        while(1){
            /*Empty string, break loop case.*/
            if(lexreturn==28){
                new_line_flag++;
                if(new_line_flag==2) break;
            }else new_line_flag=0;

            lex_control(lexreturn,&fp);
            lexreturn=yylex();
        }
    }



    fclose(fp);
    return 0;
}