%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Pr�dicats techniques %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Contient des pr�dicats "g�n�riques", qui se veulent simples et pratiques


% - Affichage Plateau ----------------------------------------------------------------------------------------------------------------------------- %

% Affiche tous les �l�ments d'une liste sur une ligne
printList([]):- writeln('').
printList([H|T]) :- write(H), write(' '), printList(T).

% Affiche une liste de listes
printBoard([]).
printBoard([H|T]) :- printList(H), writeln(''), printBoard(T).

% Affiche le plateau de jeu
displayBoard :- board(Board), printBoard(Board), writeln('\n\n').
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% - Placement pi�ces sur plateau ------------------------------------------------------------------------------------------------------------------ %

% Renvoie le contenu 'Elem' de la case de coordonn�es (X,Y)
getCaseOnBoard(X, Y, Elem) :- board(Board), nth0(Y, Board, L), nth0(X, L, Elem).

% D�finit le contenu 'Elem' de la case de coordonn�es (X,Y)
setCaseOnBoard(X, Y, Elem) :- board(Board), nth0(Y, Board, L), replace(L, X, Elem, NewL), replace(Board, Y, NewL, NewBoard), applyIt(NewBoard). 

% Modifie un �l�ment d'une liste � partir de son indice dans la liste.
% S'appelle comme suit : replace(AncienneListe, IndiceElemAChanger, NouvelElem, NouvelleListe)
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% Modifie un �l�ment d'une liste � partir de la valeur de l'ancien �l�ment.
% S'appelle comme suit : update(AncienElem, AncienneListe, NouvelleListe, NouvelElem)
update(_,[],[],_).
update(OldValue, [A|B],[X|Y], NewValue) :- A = OldValue, X = NewValue, update(OldValue,B,Y, NewValue).
update(OldValue, [A|B],[X|Y], NewValue) :- A = X, update(OldValue,B,Y, NewValue).

% Supprime un �l�ment d'une liste � partir de son indice
remove(Indice, List, NewList):- nth0(Indice,List,Elmt), delete(List, Elmt, NewList).

% Modifie le joueur actuellement en train de jouer
changePlayer:- currentPlayer(Player),resetPlayer, (Player = 'A',assert(currentPlayer('D'))) ; (Player = 'D',assert(currentPlayer('A'))).

% Enregistre le nouveau plateau de jeu comme �tant le plateau actuellement utilis�
applyIt(NewBoard) :- resetBoard, assert(board(NewBoard)).

% Efface toute assertion correspondant au nom du pr�dicat
resetBoard :- findall(_,retract(board(_)),_).
resetSize :- findall(_,retract(size(_)),_).
resetPlayer :- findall(_, retract(currentPlayer(_)),_).
resetIA_Defence :- findall(_, retract(prefered_Vertical_Direction(_)),_), findall(_, retract(prefered_Horizontal_Direction(_)),_).

% Efface toutes les assertions
reset :- resetBoard, resetSize, resetPlayer, resetAttackers, resetDefenders, resetIA_Defence.

abs(X,Y,Result):-X<Y-> Result is Y-X; Result is X-Y.


