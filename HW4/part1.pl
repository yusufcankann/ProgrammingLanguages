% knowledge base
flight(edirne,edremit).
flight(edremit,edirne).
flight(edremit,erzincan).
flight(erzincan,edremit).
flight(burdur,isparta).
flight(isparta,burdur).
flight(isparta,izmir).
flight(izmir,isparta).
flight(izmir,istanbul).
flight(istanbul,izmir).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).
flight(antalya,gaziantep).
flight(antalya,konya).
flight(antalya,istanbul).
flight(gaziantep,antalya).
flight(gaziantep,istanbul).
flight(konya,antalya).
flight(konya,ankara).
flight(ankara,konya).
flight(ankara,van).
flight(ankara,istanbul).
flight(van,ankara).
flight(van,rize).
flight(van,istanbul).
flight(rize,van).
flight(rize,istanbul).

% rules..



route(X, Y) :- routefind(X, Y, []).
routefind(X,Y,Visited) :- flight(X,Z), %finds every path from city X
						not(member(Z, Visited)), % looks finding city is visited previous recusive calls
						(Y = Z ;routefind(Z,Y,[X|Visited])). % Y=Z => finded node is Y 
															 % Find new routes (conected cities) recursivly.

