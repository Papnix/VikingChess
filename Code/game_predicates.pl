%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Prédicats du jeu %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Contient des prédicats concernant les mécaniques du jeu

% - Game Over ------------------------------------------------------------------------------------------------------------------------------------- %
	
displayWinA :- writeln('LES ATTAQUANTS ONT GAGNE ! \n\n').
displayWinD :- writeln('LES DEFENSEURS ONT GAGNE ! \n\n').
	
checkKingCastle(0, 0).
checkKingCastle(0, Y) :- size(Size), Ywin is Size-1, Y = Ywin.
checkKingCastle(X, 0) :- size(Size), Xwin is Size-1, X = Xwin.
checkKingCastle(X, Y) :- size(Size), Win is Size-1, X = Win, Y = Win.

checkAttackers([]).
checkAttackers([[X|[Y|_]]|T]) :- getCaseOnBoard(X, Y, E), E = '_A_', checkAttackers(T).

checkKingDead(X, Y) :- around(X, Y, List), checkAttackers(List).

checkKingWin :- getPieceInDefenders(0,List), getCoord(List, X, Y), checkKingCastle(X, Y), displayWinD.	
checkKingLose :- getPieceInDefenders(0,List), getCoord(List, X, Y), write(X),write(' '), writeln(Y), checkKingDead(X, Y), displayWinA.

checkAttackersDead :- attackers(A), A = [], displayWinD.
checkDefendersDead :- defenders(D), D = [], displayWinA.

checkForVictory:- checkKingWin;	checkKingLose;checkAttackersDead;checkDefendersDead.
% ------------------------------------------------------------------------------------------------------------------------------------------------- %


% - Combats pièces -------------------------------------------------------------------------------------------------------------------------------- %

% Renvoie 2 listes : List1 contient les coordonnées des cases adjacentes à la case (X,Y).
% List2 contient les coordonnées des cases orthogonales à une distance de 2 de la case (X,Y).
around(X, Y, List1, List2) :- 
    X1 is X+1, 
    X2 is X-1, 
    Y1 is Y+1,
    Y2 is Y-1,
    X1bis is X+2,
    X2bis is X-2,
    Y1bis is Y+2,
    Y2bis is Y-2,
    List1 = [[X, Y1], [X, Y2], [X1, Y], [X2, Y]],
    List2 = [[X, Y1bis], [X, Y2bis], [X1bis, Y], [X2bis, Y]].
	
% Renvoie List1 : contient les coordonnées des cases adjacentes à la case (X,Y).
around(X, Y, List1) :- 
    X1 is X+1, 
    X2 is X-1, 
    Y1 is Y+1,
    Y2 is Y-1,
    List1 = [[X, Y1], [X, Y2], [X1, Y], [X2, Y]].
	
% Convertit des coordonnées sous forme de liste en variables séparées. Ex : [X,Y] => X, Y
getCoord([H|T], X, Y) :- X = H, [H1|_] = T, Y = H1.

% S'appelle comme suit : deadlyConfiguration (Pion1, Pion2, Pion3).
% Permet de savoir si Pion2 est entouré par 2 ennemis (true) ou non (false).
% (anciennement nommé execute)
deadlyConfiguration('_A_', '_D_', '_A_').
deadlyConfiguration('_X_', '_D_', '_A_').
deadlyConfiguration('_A_', '_D_', '_X_').

deadlyConfiguration('_D_', '_A_', '_D_').
deadlyConfiguration('_X_', '_A_', '_D_').
deadlyConfiguration('_D_', '_A_', '_X_').
deadlyConfiguration('_R_', '_A_', '_D_').
deadlyConfiguration('_D_', '_A_', '_R_').
deadlyConfiguration('_R_', '_A_', '_X_').
deadlyConfiguration('_X_', '_A_', '_R_').

% Retire les pièces ennemies du plateau qui sont entre le pion de coordonnées (X,Y) et un allié à proximité
% Uniquement utilisé par applyKillNextTo. Pour effectuer un kill, utiliser applyKillNextTo.
doKill([], [], _).
doKill([H_Enemy|T_Enemy], [H_Ally|T_Ally], Pawn) :- 
    getCoord(H_Enemy, X_Enemy, Y_Enemy),
    getCoord(H_Ally, X_Ally, Y_Ally),
    ((getCaseOnBoard(X_Enemy, Y_Enemy, Enemy),
    getCaseOnBoard(X_Ally, Y_Ally, Ally),
    ((deadlyConfiguration(Pawn, Enemy, Ally), removePieceOnBoard(X_Enemy, Y_Enemy));true));true),
    doKill(T_Enemy, T_Ally, Pawn).

% Récupère les infos nécessaires et retire les pièces ennemies qui sont entre le pion de coordonnées (X,Y) et un allié à proximité
% (anciennement nommé kill)
applyKillNextTo(X, Y) :- 
    getCaseOnBoard(X, Y, Pawn),
    around(X, Y, Enemy, Ally),
    doKill(Enemy, Ally, Pawn).


% - Collisions pièces ----------------------------------------------------------------------------------------------------------------------------- %

% Vérifie que les cases du plateau de la case (X,Y) à la case (NewX, Y) sont toutes libres (true). Si une case ne l'est pas, renvoie false.
% Vérifie également si la case (NewX, Y) n'est pas une citadelle dans le cas où 'Pawn' est un attaquant.
% A noter qu'une citadelle peut être "sautée" par un attaquant, c'est-à-dire que le pion peut aller à une case
% au-delà de la citadelle, tant qu'il ne s'arrête pas dessus.
% X, Y et NewX doivent être des coordonnées valides.
reachableX(NewX, Y, NewX, Pawn):-
	getCaseOnBoard(NewX,Y,E),
	(E = '___' ; (E = '_X_', not(Pawn = '_A_'))).
	
reachableX(X, Y, NewX, Pawn):-
	Delta is NewX - X,
	(Delta > 0 -> Xinc is X + 1;
	Delta < 0 -> Xinc is X - 1),
	getCaseOnBoard(Xinc,Y,E),
	(E = '___' ; E = '_X_'),
	reachableX(Xinc, Y, NewX, Pawn).

% Voir commentaire de reachableX. Le fonctionnement est le même
reachableY(X, NewY, NewY, Pawn):-
	getCaseOnBoard(X,NewY,E),
	(E = '___' ; (E = '_X_', not(Pawn = '_A_'))).

reachableY(X, Y, NewY, Pawn):-
	Delta is NewY - Y,
	(Delta > 0 -> Yinc is Y + 1; Delta < 0 -> Yinc is Y - 1),
	getCaseOnBoard(X,Yinc,E),
	(E = '___' ; E = '_X_'),
	reachableY(X, Yinc, NewY, Pawn).


% - Mouvements pièces ----------------------------------------------------------------------------------------------------------------------------- %

% Déplace la pièce 'Pawn' de coordonnées (X,Y) de 'NbCase' cases vers le Nord. Les collisions sont vérifiées.
% (X,Y) doit impérativement correspondre aux coordonnées d'une pièce.
% Ce prédicat est uniquement utilisé par move. Pour déplacer une pièce, utiliser move.
moveN(Pawn, X, Y, NbCase):-
	NewY is Y - NbCase,
	NewY >= 0,
	reachableY(X, Y, NewY, Pawn),
	updatePieceOnBoard([X, Y], [X, NewY]), 
	applyKillNextTo(X, NewY).
	
% Voir commentaire de moveN, le fonctionnement est le même
moveS(Pawn, X, Y, NbCase):-
	NewY is Y + NbCase,
	size(Size),
	NewY < Size,
	reachableY(X, Y, NewY, Pawn),
	updatePieceOnBoard([X, Y], [X, NewY]), 
	applyKillNextTo(X, NewY).
	
% Voir commentaire de moveN, le fonctionnement est le même
moveE(Pawn, X, Y, NbCase):-
	NewX is X + NbCase,
	size(Size),
	NewX < Size,
	reachableX(X, Y, NewX, Pawn), 
	updatePieceOnBoard([X, Y], [NewX, Y]),
	applyKillNextTo(NewX, Y).
	
% Voir commentaire de moveN, le fonctionnement est le même
moveO(Pawn, X, Y, NbCase):-
	NewX is X - NbCase,
	NewX >= 0, 
	reachableX(X, Y, NewX, Pawn),
	updatePieceOnBoard([X, Y], [NewX, Y]),
	applyKillNextTo(NewX, Y).

% Déplace la pièce de coordonnées (X,Y) de 'NbCase' cases dans la direction 'Dir'. Les collisions sont vérifiées.
% 'Dir' peut prendre les valeurs 'N', 'S', 'E' ou 'O'.
move(X, Y, Dir, NbCase):-
	getCaseOnBoard(X, Y, Pawn),
	not(Pawn = '___'),
	(Dir = 'N' -> moveN(Pawn, X, Y, NbCase);
	Dir = 'S' -> moveS(Pawn, X, Y, NbCase);
	Dir = 'E' -> moveE(Pawn, X, Y, NbCase);
	Dir = 'O' -> moveO(Pawn, X, Y, NbCase)).

moveKing(X, Y, Dir, NbCase):-
	NbCase > 0,
	getCaseOnBoard(X, Y, E),
	not(E = '___'),
	(
		Dir = 'N',
		NewY is Y - NbCase,
		updatePieceOnBoard([X, Y], [X, NewY]),
		applyKillNextTo(X, NewY)
	)
	;
	(
		Dir = 'S',
		NewY is Y + NbCase,
		updatePieceOnBoard([X, Y], [X, NewY]),
		applyKillNextTo(X, NewY)
	)
	;
	(
		Dir = 'E',
		NewX is X + NbCase,
		updatePieceOnBoard([X, Y], [NewX, Y]),
		applyKillNextTo(NewX, Y)
	)
	;	
	(
		Dir = 'O',
		NewX is X - NbCase,
		updatePieceOnBoard([X, Y], [NewX, Y]),
		applyKillNextTo(NewX, Y)
	).
	