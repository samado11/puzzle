move([R1,R2,R3,R4,R5,R6,R7,R8],W,CHILD):-
	permutation(W,CHILD).
printlist([]):-!.
printlist([H|T]):-
write('  '),write(H),write('  '),printlist(T).
printlisttill(_,-1,Stream):-close(Stream),!.
printlisttill([H|T],X,Stream):-
    write(Stream, H),nl(Stream),
    X1 is X - 1,printlisttill(T,X1,Stream).



goal([_,R2H1,_,R4H1,_,R6H1,_,R8H1,_],  %H1
	 [_,R2H2,_,R4H2,_,R6H2,_,R8H2,_],  %H2
	 [_,R2H3,_,R4H3,_,R6H3,_,R8H3,_],  %H3
	 [_,R2H4,_,R4H4,_,R6H4,_,R8H4,_],  %H4
	 [_,R2H1,_,R2H2,_,R2H3,_,R2H4,_],  %V1
	 [_,R4H1,_,R4H2,_,R4H3,_,R4H4,_],  %V2
	 [_,R6H1,_,R6H2,_,R6H3,_,R6H4,_],  %V3
	 [_,R8H1,_,R8H2,_,R8H3,_,R8H4,_]). %V4
go:-
	read(W1),
	read(W2),
	read(W3),
	read(W4),
	read(W5),
	read(W6),
	read(W7),
	read(W8),
	path([[[-1,-1,-1,-1,-1,-1,-1,-1],null]],[],[W1,W2,W3,W4,W5,W6,W7,W8],X),write(X),nl,write('Steps : '),nl , bagof(Y,move([W1,W2,W3,W4,W5,W6,W7,W8],[W1,W2,W3,W4,W5,W6,W7,W8],Y),List),nth0(Index1,List,X),open('steps.txt',write, Stream),printlisttill(List,Index1,Stream).

path([],_,_):-
	write('No solution'),nl,!.

path([[[W1,W2,W3,W4,W5,W6,W7,W8],Parent] | _], Closed, _,[W1,W2,W3,W4,W5,W6,W7,W8]):-
		atom_chars(W1,WW1),atom_chars(W2,WW2),atom_chars(W3,WW3),atom_chars(W4,WW4),atom_chars(W5,WW5),atom_chars(W6,WW6),atom_chars(W7,WW7),atom_chars(W8,WW8),
		goal(WW1,WW2,WW3,WW4,WW5,WW6,WW7,WW8),
		write('A solution is found'), nl ,
		write('          v1  '),write('      v2  '),write('      v3  '),write('      v4  '),nl,write('          '),nth0(0,WW5,X),write(X),write('  '),write('       '),nth0(0,WW6,Y),write(Y),write('  '),write('       '),nth0(0,WW7,Z),write(Z),write('  '),write('       '),nth0(0,WW8,U),write(U),write('  '),nl,
		write('h1 '),printlist(WW1)
		,nl,write('          '),nth0(2,WW5,A),write(A),write('         '),nth0(2,WW6,A1),write(A1),write('         '),nth0(2,WW7,A2),write(A2),write('         '),nth0(2,WW8,A3),write(A3),nl,
		write('h2 '),printlist(WW2),nl,
		write('          '),nth0(4,WW5,A4),write(A4),write('         '),nth0(4,WW6,A5),write(A5),write('         '),nth0(4,WW7,A6),write(A6),write('         '),nth0(4,WW8,A7),write(A7),nl,
		write('h3 '),printlist(WW3),nl,
		write('          '),nth0(6,WW5,A8),write(A8),write('         '),nth0(6,WW6,A9),write(A9),write('         '),nth0(6,WW7,AX),write(AX),write('         '),nth0(6,WW8,AY),write(AY),nl,
		write('h4 '),printlist(WW4),nl,
		write('          '),nth0(8,WW5,AW),write(AW),write('         '),nth0(8,WW6,AZ),write(AZ),write('         '),nth0(8,WW7,AU),write(AU),write('         '),nth0(8,WW8,AO),write(AO),nl
		,!.
path(Open, Closed, [],X):-!,
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		path(RestOfOpen, [[State, Parent] | Closed], [],X).
		
path(Open, Closed, Words,X):-
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		getchildren(State, Open, Closed,Words, Children),
		append(Children,RestOfOpen , NewOpen),
		path(NewOpen, [[State, Parent] | Closed], [],X).


%gets Children of State that arent in Open or Close
getchildren(State, Open ,Closed, Words, Children):-
		bagof(X, moves( State, Open, Closed,Words, X), 	Children), ! .
getchildren(_,_,_, []).

%adds children to open list (without head child) to form new open list
%here it is like append i.e.Breadth First
addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).

%gets head of open list to get its children later
removeFromOpen([State|RestOpen], State, RestOpen).

	
%gets next state given the current state
moves( State, Open, Closed,Words,[Next,State]):-
		move(State,Words,Next),
		\+ member([Next,_],Open),
		\+ member([Next,_],Closed).

%prints the path from start state to goal state
printsolution([State, null],_):-
		write(State),nl.
printsolution([State, Parent], Closed):-
		member([Parent, GrandParent], Closed),
		printsolution([Parent, GrandParent], Closed),
		write(State), nl.
% i have made a GUI ^_^ 