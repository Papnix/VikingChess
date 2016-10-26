%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Technicals Predicats %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% - Affichage Plateau ----------------------------------------------------------------------------------------------------------------------------- %

printList([]).
printList([H|T]) :- write(H), write(' '), printList(T).
printBoard([]).
printBoard([H|T]) :- printList(H), writeln(''), printBoard(T).

displayBoard :- board(Board), printBoard(Board), writeln('\n\n').
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% - Placement pièces sur plateau ------------------------------------------------------------------------------------------------------------------ %
	
getCaseOnBoard(X, Y, E) :- board(Board), nth0(Y, Board, L), nth0(X, L, E).
setCaseOnBoard(X, Y, E) :- board(Board), nth0(Y, Board, L), replace(L, X, E, NewL), replace(Board, Y, NewL, NewBoard), applyIt(NewBoard). 

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

update(_,[],[],_).
update(OldValue, [A|B],[X|Y], NewValue) :- A = OldValue, X = NewValue, update(OldValue,B,Y, NewValue).
update(OldValue, [A|B],[X|Y], NewValue) :- A = X, update(OldValue,B,Y, NewValue).

remove(Indice, List, NewList):- nth0(Indice,List,Elmt), delete(List, Elmt, NewList).

changePlayer:- currentPlayer(Player),resetPlayer, Player = 'A' -> assert(currentPlayer('D'));Player = 'D' -> assert(currentPlayer('A')).
applyIt(NewBoard) :- resetBoard, assert(board(NewBoard)).
resetBoard :- findall(_,retract(board(_)),_).
resetSize :- findall(_,retract(size(_)),_).
resetPlayer :- findall(_, retract(currentPlayer(_)),_).
reset :- resetBoard, resetSize, resetPlayer, resetAttackers, resetDefenders.