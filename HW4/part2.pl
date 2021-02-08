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

distance(edirne,edremit,235).
distance(edremit,edirne,235).
distance(edremit,erzincan,1066).
distance(erzincan,edremit,1066).
distance(burdur,isparta,25).
distance(isparta,burdur,25).
distance(isparta,izmir,309).
distance(izmir,isparta,309).
distance(izmir,istanbul,329).
distance(istanbul,izmir,329).
distance(istanbul,antalya,483).
distance(istanbul,gaziantep,847).
distance(istanbul,ankara,352).
distance(istanbul,van,1262).
distance(istanbul,rize,967).
distance(antalya,gaziantep,592).
distance(antalya,konya,192).
distance(antalya,istanbul,483).
distance(gaziantep,antalya,592).
distance(gaziantep,istanbul,847).
distance(konya,antalya,192).
distance(konya,ankara,227).
distance(ankara,konya,227).
distance(ankara,istanbul,352).
distance(ankara,van,920).
distance(van,ankara,920).
distance(van,rize,373).
distance(van,istanbul,1262).
distance(rize,van,373).
distance(rize,istanbul,967).

% rules..


route(X, Y) :- routefind(X, Y, []). %wrapper
routefind(X,Y,Visited) :- flight(X,Z), %finds every path from city X
						not(member(Z, Visited)), % looks finding city is visited previous recusive calls
						(Y = Z ;routefind(Z,Y,[X|Visited])). % Y=Z => finded node is Y 
															 % Find new routes (conected cities) recursivly.



                    
%calculates all the alternative path costs
distanceList(X, Y, Distances) :- findall(D,routeDistance(X,Y,[],D), Distances).

%finds path cost from X to Y.
routeDistance(X,Y,Visited,Dist) :- 
    distance(X,Z,D), not(member(Z, Visited)),(Y = Z, Dist is D;
    routeDistance(Z,Y,[X|Visited],K),Dist is K+D). 
                                        
sroute(X,Y,D) :- route(X,Y),distanceList(X,Y,List),min_list(List,D).