% rules


operator(X,Y,Z):- Z=X+Y. %Set Z, X+Y
operator(X,Y,Z):- Z=X-Y. %Set Z, X-Y
operator(X,Y,Z):- Z=X*Y. %Set Z, X*Y
operator(X,Y,Z):- Y=\=0 ,Z=X/Y.   % avoid division by zero and set Z = X/Y

%Adds operators into given lists.
addOperator([Result],Result). %If there is only 1 element, set that element to Result.
addOperator(List,Result):- divide(List,Left,Right), %divide number list into 2 part for
                        %left and right side of the equation
    %Create right side of equations
    addOperator(Right,RightSide),       
    %Create left and right side of equations
    addOperator(Left,LeftSide),
    operator(LeftSide,RightSide,Result). %add arithmetic expression between left and right
                                    % and set result to it.

%divide result into 2 part                                   
divide(I,X,Y) :- 
append(X,Y,I), %append indicates 
                        %that List = concatenate(List1,List2).
X=[_|_],Y=[_|_]. %set list that it dies not matter left and 
        %right part v\%set list that it dies not matter left and 
                %right part


calculate(List,X,Y):-
    divide(List,Left,Right), %divide number list into 2 part for
                                %left and right side of the equation
    addOperator(Right,X), %this part adds a arithmetic operatin betweend 
                                    %the elements of list nd constructs the 
                                    %right side of the equation.
    addOperator(Left,Y),  %left side
    % calculare created left and right side of expression and assign it.      
    LeftResult is Y, RightResult is X, LeftResult =:= RightResult. 
                                                        %if two side is equal, 
                                                        %it means we find our expresion.


submain(List):-calculate(List,X,Y),
    open("output.txt",append,Wr), %WRITE FILE
    swritef(Strr,'%w = %w\n',[X,Y]),
    write(Wr,Strr),
    close(Wr),
    fail.

submain(_).

:- use_module(library(dcg/basics), except([eos/2])).

main(File) :-
    open("output.txt",write,Wr), %CLEARS THE OUTPUT FILE.
    close(Wr),
    DCG = read_list(NumberList),
    phrase_from_file(DCG,File), !,
    NumberList = [Numbers],
    submain(Numbers).

%WRITE FILE PART.
eos([], []).

read_list([]) --> call(eos).
read_list([List|Lists]) -->
    read(List),
    (
        "\n"
    |
        []
    ),
    read_list(Lists).
read_list([]) --> [].

read([Item|Items]) -->
    number(Item),
    whites,
    read(Items).
read([]) --> [].

?- main("input.txt").