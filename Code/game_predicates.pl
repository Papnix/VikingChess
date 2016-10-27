%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Pr�dicats du jeu %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Contient des pr�dicats concernant les m�caniques du jeu


% - Combats pi�ces -------------------------------------------------------------------------------------------------------------------------------- %

% Renvoie 2 listes : List1 contient les coordonn�es des cases adjacentes � la case (X,Y).
% List2 contient les coordonn�es des cases orthogonales � une distance de 2 de la case (X,Y).
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

% Convertit des coordonn�es sous forme de liste en variables s�par�es. Ex : [X,Y] => X, Y
getCoord([H|T], X, Y) :- X = H, [H1|_] = T, Y = H1.

% S'appelle comme suit : deadlyConfiguration (Pion1, Pion2, Pion3).
% Permet de savoir si Pion2 est entour� par 2 ennemis (true) ou non (false).
% (anciennement nomm� execute)
deadlyConfiguration('_A_', '_D_', '_A_').
deadlyConfiguration('_D_', '_A_', '_D_').
deadlyConfiguration('_R_', '_A_', '_D_').
deadlyConfiguration('_D_', '_A_', '_R_').

% Retire les pi�ces ennemies du plateau qui sont entre le pion de coordonn�es (X,Y) et un alli� � proximit�
% Uniquement utilis� par applyKillNextTo. Pour effectuer un kill, utiliser applyKillNextTo.
doKill([], [], _).
doKill([H_Enemy|T_Enemy], [H_Ally|T_Ally], Pawn) :- 
    getCoord(H_Enemy, X_Enemy, Y_Enemy),
    getCoord(H_Ally, X_Ally, Y_Ally),
    ((getCaseOnBoard(X_Enemy, Y_Enemy, Enemy),
    getCaseOnBoard(X_Ally, Y_Ally, Ally),
    ((deadlyConfiguration(Pawn, Enemy, Ally), removePieceOnBoard(X_Enemy, Y_Enemy));true));true),
    doKill(T_Enemy, T_Ally, Pawn).

% R�cup�re les infos n�cessaires et retire les pi�ces ennemies qui sont entre le pion de coordonn�es (X,Y) et un alli� � proximit�
% (anciennement nomm� kill)
applyKillNextTo(X, Y) :- 
    getCaseOnBoard(X, Y, Pawn),
    around(X, Y, Enemy, Ally),
    doKill(Enemy, Ally, Pawn).


% - Collisions pi�ces ----------------------------------------------------------------------------------------------------------------------------- %

% V�rifie que les cases du plateau de la case (X,Y) � la case (NewX, Y) sont toutes libres (true). Si une case ne l'est pas, renvoie false.
% X, Y et NewX doivent �tre des coordonn�es valides.
collisionX(NewX, Y, NewX):-
	getCaseOnBoard(NewX,Y,E),
	E = '___'.
	
collisionX(X, Y, NewX):-
	Delta is NewX - X,
	(Delta > 0 -> Xinc is X + 1;
	Delta < 0 -> Xinc is X - 1),
	getCaseOnBoard(Xinc,Y,E),
	E = '___',
	collisionX(Xinc, Y,  NewX).

% Voir commentaire de collisionX. Le fonctionnement est le m�me
collisionY(X, NewY, NewY):-
	getCaseOnBoard(X,NewY,E),
	E = '___'.

collisionY(X, Y, NewY):-
	Delta is NewY - Y,
	(Delta > 0 -> Yinc is Y + 1; Delta < 0 -> Yinc is Y - 1),
	getCaseOnBoard(X,Yinc,E),
	E = '___',
	collisionY(X, Yinc,  NewY).


% - Mouvements pi�ces ----------------------------------------------------------------------------------------------------------------------------- %

% D�place la pi�ce de coordonn�es (X,Y) de 'NbCase' cases vers le Nord. Les collisions sont v�rifi�es.
% (X,Y) doit imp�rativement correspondre aux coordonn�es d'une pi�ce.
% Ce pr�dicat est uniquement utilis� par move. Pour d�placer une pi�ce, utiliser move.
moveN(X, Y, NbCase):-
	NewY is Y - NbCase,
	NewY >= 0,
	collisionY(X, Y, NewY),
	updatePieceOnBoard([X, Y], [X, NewY]), 
	applyKillNextTo(X, NewY).
	
% Voir commentaire de moveN, le fonctionnement est le m�me
moveS(X, Y, NbCase):-
	NewY is Y + NbCase,
	size(Size),
	NewY < Size,
	collisionY(X, Y, NewY),
	updatePieceOnBoard([X, Y], [X, NewY]), 
	applyKillNextTo(X, NewY).
	
% Voir commentaire de moveN, le fonctionnement est le m�me
moveE(X, Y, NbCase):-
	NewX is X + NbCase,
	size(Size),
	NewX < Size,
	collisionX(X, Y, NewX), 
	updatePieceOnBoard([X, Y], [NewX, Y]),
	applyKillNextTo(NewX, Y).
	
% Voir commentaire de moveN, le fonctionnement est le m�me
moveO(X, Y, NbCase):-
	NewX is X - NbCase,
	NewX >= 0, 
	collisionX(X, Y, NewX),
	updatePieceOnBoard([X, Y], [NewX, Y]),
	applyKillNextTo(NewX, Y).

% D�place la pi�ce de coordonn�es (X,Y) de 'NbCase' cases dans la direction 'Dir'. Les collisions sont v�rifi�es.
% 'Dir' peut prendre les valeurs 'N', 'S', 'E' ou 'O'.
move(X, Y, Dir, NbCase):-
	NbCase > 0,
	getCaseOnBoard(X, Y, E),
	not(E = '___'),
	(Dir = 'N' -> moveN(X, Y, NbCase);
	Dir = 'S' -> moveS(X, Y, NbCase);
	Dir = 'E' -> moveE(X, Y, NbCase);
	Dir = 'O' -> moveO(X, Y, NbCase)).