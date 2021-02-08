all:
	lex lexer.l
	yacc -d gpp_interpreter.y -o gpp_interpreter.c
	gcc helperfunctions.c lex.yy.c gpp_interpreter.c -o gpp_interpreter.out 

clean:
	rm gpp_interpreter.out  parsed_cpp.txt
