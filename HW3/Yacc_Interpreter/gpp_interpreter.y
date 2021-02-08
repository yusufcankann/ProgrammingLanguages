%{
   #include <stdio.h>
   #include <string.h>
   #include <stdlib.h>
   #include "helperfunctions.h"


      
   extern FILE *yyin;
   
   int yylex();
   void yyerror(const char *s);
   void yyrestart (FILE *input_file);

   struct variables v={0,0,0};
   int line=2;

   FILE *outputFile;

%}

%union { int number; char str[256]; int *arr; }

%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_TRUE
%token KW_FALSE
%token KW_DISP
%token OP_OP
%token OP_CP
%token KW_LESS
%token KW_NIL
%token KW_SET
%token KW_DEFVAR
%token KW_LIST
%token KW_APPEND
%token KW_CONCAT
%token NEW_LINE
%token KW_DEFFUN
%token KW_FOR
%token KW_IF
%token KW_WHILE
%token KW_EXIT
%token KW_LOAD
%token OP_OC
%token OP_CC
%token OP_COMMA
%token OP_APOS
%token COMMENT
%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_DBLMULT
%token <number>VALUE
%token <str>IDENTIFIER
%type <str>IDLIST
%type <number>EXPB
%type <number>EXPI
%type <arr>VALUES
%type INPUT
%type LISTVALUE

%start START

%%
START: | START INPUT {};

INPUT:
   EXPI { if($<arr>1 != NULL){
         for(int i=2;i < ($<arr>1)[0] + 2 ;i++) 
            fprintf(outputFile,"%d ",($<arr>1)[i]); fprintf(outputFile,"\n"); free($<arr>1);} }|
   EXPB { if($1==1) fprintf(outputFile,"True\n"); else fprintf(outputFile,"False\n"); }
   |EXPLISTI {      
                     if($<arr>1==NULL){ fprintf(outputFile,"() \n");}
                     else{
                        for(int i=2;i < ($<arr>1)[0] + 2 ;i++) 
                           fprintf(outputFile,"%d ",($<arr>1)[i]); 
                        free($<arr>1);
                        fprintf(outputFile,"\n");}
                        }
   |NEW_LINE{}|
   COMMENT{}|
   OP_OP KW_EXIT OP_CP {exit(0);};

LISTVALUE:
   OP_APOS OP_OP VALUES OP_CP {$<arr>$=$<arr>3;}|
   OP_APOS OP_OP OP_CP {$<arr>$=NULL;}|
   OP_OP KW_LIST VALUES OP_CP {$<arr>$=$<arr>3;}|
   OP_OP KW_LIST OP_CP {$<arr>$=NULL;}|
   KW_NIL {$<arr>$=NULL;}|
   OP_OP KW_DISP LISTVALUE OP_CP { $<arr>$=$<arr>3;};


VALUES: 
   EXPI {$$=$<arr>1;}|VALUES EXPI{$$=addArr($1,$<arr>2[2]);};

EXPI:
   VALUE {$<arr>$ = createArr($1);}|
   IDENTIFIER {$<arr>$ = createArr(get_identifier(&v,$1));}|
   OP_OP OP_PLUS EXPI EXPI OP_CP {$<arr>$=createArr(($<arr>3[2]+$<arr>4[2]));}|
   OP_OP OP_MINUS EXPI EXPI OP_CP {$<arr>$=createArr($<arr>3[2]-$<arr>4[2]);}|
   OP_OP OP_DIV EXPI EXPI OP_CP {$<arr>$=createArr($<arr>3[2]/$<arr>4[2]);}|
   OP_OP OP_MULT EXPI EXPI OP_CP {$<arr>$=createArr($<arr>3[2]*$<arr>4[2]);}|
   OP_OP OP_DBLMULT EXPI EXPI OP_CP {
         $<number>$=1;
         for(int i=0;i<$<arr>4[2];i++){
            $<number>$*=$<arr>3[2];
         }
      $<arr>$=createArr($<number>$);
   }|
   OP_OP KW_SET IDENTIFIER EXPI OP_CP {$<arr>$=createArr(set_identifier(&v,$3,$<arr>4[2]));}|
   OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP { $<arr>$=createArr(set_identifier(&v,$3,$<arr>4[2]));}|
   OP_OP IDENTIFIER EXPLISTI OP_CP { $<arr>$ = createArr($<arr>3[$<arr>3[0]+1]); free($<arr>3); $<arr>3=NULL;}|
   OP_OP KW_DEFFUN IDLIST EXPLISTI OP_CP { $<arr>$ = createArr(0); set_identifier(&v,$<str>3,$<arr>4[$<arr>4[0]+1]); free($<arr>4); $<arr>4=NULL;}|
   OP_OP KW_DEFFUN IDLIST OP_OP IDLIST OP_CP EXPI OP_CP { $<arr>$ = createArr(0); set_identifier(&v,$<str>3,$<arr>7[$<arr>7[0]+1]); free($<arr>7); $<arr>7=NULL;}|
   OP_OP KW_DEFFUN IDLIST OP_OP IDLIST OP_CP EXPLISTI OP_CP { $<arr>$ = createArr(0); set_identifier(&v,$<str>3,$<arr>7[$<arr>7[0]+1]); free($<arr>7); $<arr>7=NULL;}|
   OP_OP KW_IF EXPB EXPLISTI OP_CP{ if($3==1){ $<arr>$ = createArr($<arr>4[$<arr>4[0]+1]); free($<arr>4);} else $<arr>$=NULL; }|
   OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP{ $<arr>$ = createArr($<arr>8[$<arr>8[0]+1]); }|
   OP_OP KW_DISP EXPI OP_CP {$<arr>$=$<arr>3;};

EXPB:
   OP_OP KW_AND EXPB EXPB OP_CP {  $$ = ($3 && $4);}|
   OP_OP KW_OR EXPB EXPB OP_CP { $$ = ($3 || $4);} |
   OP_OP KW_NOT EXPB OP_CP {if($3==1) $$=0; else $$=1;} |
   OP_OP KW_EQUAL EXPB EXPB OP_CP {if($3 == $4) $$=1; else $$=0;}|
   KW_TRUE {$$ = 1;} | KW_FALSE {$$ = 0;}|
   OP_OP KW_DISP EXPB OP_CP { $$=$3;} |
   OP_OP KW_EQUAL EXPI EXPI OP_CP {if($<arr>3[2] == $<arr>4[2]) $$=1; else $$=0;} |
   OP_OP KW_LESS EXPI EXPI OP_CP {if($<arr>3[2] < $<arr>4[2]) $$=1; else $$=0;};
   
EXPLISTI: 
   OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {$<arr>$=append($<arr>3,$<arr>4);}|
   OP_OP KW_APPEND VALUES EXPLISTI {$<arr>$=append($<arr>3,$<arr>4);}| 
   LISTVALUE {$<arr>$=$<arr>1;};

IDLIST:
   IDLIST IDENTIFIER{      strcpy($<str>$,$<str>1);
                                 strcat($<str>$, " ");;
                                 strcat($<str>$, $1);}|
   IDENTIFIER{ strcpy($<str>$,$1);};
%%
void yyerror(const char *s){
   fprintf(outputFile, "Error:%s\n", s);
   exit(0); 
}

int yywrap(){
   return 1;
} 



int main(int argc, char **argv){
   FILE *fp;
   outputFile = fopen("parsed_cpp.txt", "w+");
   if(1 < argc){
      yyin = fopen(argv[1], "r");
      yyrestart(yyin);
      if(1 == yyparse()){
         fclose(yyin);
         return 0;
      }
      fclose(yyin);
   }
   yyin = stdin;
   yyrestart(yyin);
   while(1) {
      if(1 == yyparse()) {
         return 0;
      }
   }
   return 0;
}
