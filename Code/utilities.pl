%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Prédicats techniques %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Contient des prédicats "génériques", qui se veulent simples et pratiques


% - Affichage Plateau ----------------------------------------------------------------------------------------------------------------------------- %

% Affiche tous les éléments d'une liste sur une ligne
printList([]):- writeln('').
printList([H|T]) :- write(H), write(' '), printList(T).

% Affiche une liste de listes
printBoard([]).
printBoard([H|T]) :- printList(H), writeln(''), printBoard(T).

% Affiche le plateau de jeu
displayBoard :- board(Board), printBoard(Board), writeln('\n\n').
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% - Placement pièces sur plateau ------------------------------------------------------------------------------------------------------------------ %

% Renvoie le contenu 'Elem' de la case de coordonnées (X,Y)
getCaseOnBoard(X, Y, Elem) :- board(Board), nth0(Y, Board, L), nth0(X, L, Elem).

% Définit le contenu 'Elem' de la case de coordonnées (X,Y)
setCaseOnBoard(X, Y, Elem) :- board(Board), nth0(Y, Board, L), replace(L, X, Elem, NewL), replace(Board, Y, NewL, NewBoard), applyIt(NewBoard). 

% Modifie un élément d'une liste à partir de son indice dans la liste.
% S'appelle comme suit : replace(AncienneListe, IndiceElemAChanger, NouvelElem, NouvelleListe)
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% Modifie un élément d'une liste à partir de la valeur de l'ancien élément.
% S'appelle comme suit : update(AncienElem, AncienneListe, NouvelleListe, NouvelElem)
update(_,[],[],_).
update(OldValue, [A|B],[X|Y], NewValue) :- A = OldValue, X = NewValue, update(OldValue,B,Y, NewValue).
update(OldValue, [A|B],[X|Y], NewValue) :- A = X, update(OldValue,B,Y, NewValue).

% Supprime un élément d'une liste à partir de son indice
remove(Indice, List, NewList):- nth0(Indice,List,Elmt), delete(List, Elmt, NewList).

% Modifie le joueur actuellement en train de jouer
changePlayer:- currentPlayer(Player),resetPlayer, (Player = 'A',assert(currentPlayer('D'))) ; (Player = 'D',assert(currentPlayer('A'))).

% Enregistre le nouveau plateau de jeu comme étant le plateau actuellement utilisé
applyIt(NewBoard) :- resetBoard, assert(board(NewBoard)).

% Efface toute assertion correspondant au nom du prédicat
resetBoard :- findall(_,retract(board(_)),_).
resetSize :- findall(_,retract(size(_)),_).
resetPlayer :- findall(_, retract(currentPlayer(_)),_).
resetIA_Defence :- findall(_, retract(prefered_Vertical_Direction(_)),_), findall(_, retract(prefered_Horizontal_Direction(_)),_).

% Efface toutes les assertions
reset :- resetBoard, resetSize, resetPlayer, resetAttackers, resetDefenders, resetIA_Defence.

abs(X,Y,Result):-X<Y-> Result is Y-X; Result is X-Y.


