%%%%% Game Predicats %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% - Combats pièces -------------------------------------------------------------------------------------------------------------------------------- %
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

getCoord([H|T], X, Y) :- X = H, [H1|_] = T, Y = H1.

execute('_A_', '_D_', '_A_').
execute('_D_', '_A_', '_D_').
execute('_R_', '_A_', '_D_').
execute('_D_', '_A_', '_R_').

doKill([], [], _).
doKill([HE|TE], [HA|TA], Pawn) :- 
    getCoord(HE, XE, YE),
    getCoord(HA, XA, YA),
    ((getElmt(XE, YE, E),
    getElmt(XA, YA, A),
    ((execute(Pawn, E, A), setElmt(XE, YE, '___'));true));true),
    doKill(TE, TA, Pawn).

kill(X, Y) :- 
    getElmt(X, Y, Pawn),
    around(X, Y, Enemy, Ally),
    doKill(Enemy, Ally, Pawn).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% - Collisions pièces ----------------------------------------------------------------------------------------------------------------------------- %

collisionX(NewX, Y, NewX):-
	getElmt(NewX,Y,E),
	E = '___'.
	
collisionX(X, Y, NewX):-
	Delta is NewX - X,
	(Delta > 0 -> Xinc is X + 1;
	Delta < 0 -> Xinc is X - 1),
	getElmt(Xinc,Y,E),
	E = '___',
	collisionX(Xinc, Y,  NewX).

collisionY(X, NewY, NewY):-
	getElmt(X,NewY,E),
	E = '___'.

collisionY(X, Y, NewY):-
	Delta is NewY - Y,
	(Delta > 0 -> Yinc is Y + 1; Delta < 0 -> Yinc is Y - 1),
	getElmt(X,Yinc,E),
	E = '___',
	collisionY(X, Yinc,  NewY).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% - Mouvements pièces ----------------------------------------------------------------------------------------------------------------------------- %

moveN(X, Y, NbCase):-
	NewY is Y - NbCase,
	NewY >= 0,
	collisionY(X, Y, NewY),
	getElmt(X, Y, E),
	setElmt(X, Y, '___'),
	setElmt(X, NewY, E), kill(X, NewY).
	
moveS(X, Y, NbCase):-
	NewY is Y + NbCase,
	size(Size),
	NewY < Size,
	collisionY(X, Y, NewY),
	getElmt(X, Y, E),
	setElmt(X, Y, '___'),
	setElmt(X, NewY, E), kill(X, NewY).
	
moveE(X, Y, NbCase):-
	NewX is X + NbCase,
	size(Size),
	NewX < Size,
	collisionX(X, Y, NewX), 
	getElmt(X, Y, E),
	setElmt(X, Y, '___'),
	setElmt(NewX, Y, E), kill(NewX, Y).
	
moveO(X, Y, NbCase):-
	NewX is X - NbCase,
	NewX >= 0, 
	collisionX(X, Y, NewX),
	getElmt(X, Y, E), 
	setElmt(X, Y, '___'), 
	setElmt(NewX, Y, E), kill(NewX, Y).

move(X, Y,Dir, NbCase):-
	NbCase > 0,
	getElmt(X, Y, E),
	not(E = '___'),
	(Dir = 'N' -> moveN(X, Y, NbCase);
	Dir = 'S' -> moveS(X, Y, NbCase);
	Dir = 'E' -> moveE(X, Y, NbCase);
	Dir = 'O' -> moveO(X, Y, NbCase)).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %