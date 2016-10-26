:- dynamic board/1. % permet l'assertion et le retrait de faits board/1
:- dynamic size/1.
:- dynamic attackers/1.
:- dynamic defenders/1.
:- dynamic currentPlayer/1.

%%%%% init game %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Size) :- reset, assert(size(Size)), createBoard(Size), displayBoard.
initGame(Size) :- reset, assert(size(Size)), createAndSetupBoard(Size), displayBoard.

%%%%% Game Predicats %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% - Mise en place plateau ------------------------------------------------------------------------------------------------------------------------- %
createAndSetupBoard(Size) :- createBoard(Size), setupBoard(Size).

setupBoard(Size) :- setCitadels(Size), setPieces(Size).

setCitadels(Size) :-
	setElmt(0,0,'_X_'),
	Var is Size - 1,
	Middle is Var / 2,
	setElmt(0,Var,'_X_'),
	setElmt(Middle,Middle,'_X_'),
	setElmt(Var,0,'_X_'),
	setElmt(Var,Var,'_X_').

setPieces(Size) :-
    Width is Size - 1,
    Middle is Width / 2,
    Offset is Size - 2,
    
    P2 is Middle - 1,
    P3 is Middle,
    P4 is Middle + 1,
    
    length(ListAtt,16),
    length(ListDef,9),
    
    assert(attackers(ListAtt)),
    assert(defenders(ListDef)),
    
    setElmt(0,P2,'_A_'),
    setPieceInAttackers(0,[0,P2]),
    setElmt(0,P3,'_A_'),
    setPieceInAttackers(1,[0,P3]),
    setElmt(1,P3,'_A_'),
    setPieceInAttackers(2,[1,P3]),
    setElmt(0,P4,'_A_'),
    setPieceInAttackers(3,[0,P4]),
	
	setElmt(P2,0,'_A_'),
    setPieceInAttackers(4,[P2,0]),
    setElmt(P3,0,'_A_'),
    setPieceInAttackers(5,[P3,0]),
    setElmt(P3,1,'_A_'),
    setPieceInAttackers(6,[P3,1]),
    setElmt(P4,0,'_A_'),
    setPieceInAttackers(7,[P4,0]),
	
	setElmt(Width,P2,'_A_'),
    setPieceInAttackers(8,[Width,P2]),
    setElmt(Width,P3,'_A_'),
    setPieceInAttackers(9,[Width,P3]),
    setElmt(Offset,P3,'_A_'),
    setPieceInAttackers(10,[Offset,P3]),
    setElmt(Width,P4,'_A_'),
    setPieceInAttackers(11,[Width,P4]),
	
	setElmt(P2,Width,'_A_'),
    setPieceInAttackers(12,[P2,Width]),
    setElmt(P3,Width,'_A_'),
    setPieceInAttackers(13,[P3,Width]),
    setElmt(P3,Offset,'_A_'),
    setPieceInAttackers(14,[P3,Offset]),
    setElmt(P4,Width,'_A_'),
    setPieceInAttackers(15,[P4,Width]),
    
    Pos_D_1 is Middle + 1,
    Pos_D_2 is Middle + 2,
    Neg_D_1 is Middle - 1,
    Neg_D_2 is Middle - 2,
	
	setElmt(Middle,Middle,'_R_'),
    setPieceInDefenders(0,[Middle,Middle]),
    setElmt(Pos_D_1,Middle,'_D_'),
    setPieceInDefenders(1,[Pos_D_1,Middle]),
    setElmt(Pos_D_2,Middle,'_D_'),
    setPieceInDefenders(2,[Pos_D_2,Middle]),
    setElmt(Neg_D_1,Middle,'_D_'),
    setPieceInDefenders(3,[Neg_D_1,Middle]),
    setElmt(Neg_D_2,Middle,'_D_'),
    setPieceInDefenders(4,[Neg_D_2,Middle]),
	
	setElmt(Middle,Pos_D_1,'_D_'),
    setPieceInDefenders(5,[Middle,Pos_D_1]),
    setElmt(Middle,Pos_D_2,'_D_'),
    setPieceInDefenders(6,[Middle,Pos_D_2]),
    setElmt(Middle,Neg_D_1,'_D_'),
    setPieceInDefenders(7,[Middle,Neg_D_1]),
    setElmt(Middle,Neg_D_2,'_D_'),
    setPieceInDefenders(8,[Middle,Neg_D_2]).
	
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

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

getElmt(X, Y, E) :- board(Board), nth0(Y, Board, L), nth0(X, L, E).
setElmt(X, Y, E) :- board(Board), nth0(Y, Board, L), replace(L, X, E, NewL), replace(Board, Y, NewL, NewBoard), applyIt(NewBoard). 
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% - Creation plateau ------------------------------------------------------------------------------------------------------------------------------ %

populateList(([])).
populateList([H|T]) :- H = '___', populateList(T).

createGrid(([]),_).
createGrid([H|T],Size) :- length(H,Size) , populateList(H), createGrid(T,Size).
createBoard(Size) :- length(Board,Size), createGrid(Board,Size),assert(board(Board)).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

% - Gestion Listes pions -------------------------------------------------------------------------------------------------------------------------- %

setPieceInAttackers(Indice,Piece) :- attackers(List), replace(List,Indice,Piece,NewList), updateAttackers(NewList).
getPieceInAttackers(Indice,Piece) :- attackers(List), nth0(Indice, List, Piece).

setPieceInDefenders(Indice,Piece) :- defenders(List), replace(List,Indice,Piece,NewList), updateDefenders(NewList).
getPieceInDefenders(Indice,Piece) :- defenders(List), nth0(Indice, List, Piece).

removeAttacker(Indice) :- attackers(Att), remove(Indice,Att,NewAtt), updateAttackers(NewAtt).
removeDefenders(Indice):- defenders(Def), remove(Indice,Def,NewDef), updateDefenders(NewDef). 

updateAttackers(List):- resetAttackers, assert(attackers(List)).
updateDefenders(List):- resetDefenders, assert(defenders(List)).

resetAttackers :- findall(_,retract(attackers(_)),_).
resetDefenders :- findall(_,retract(defenders(_)),_).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %

remove(Indice, List, NewList):- nth0(Indice,List,Elmt), delete(List, Elmt, NewList).

changePlayer:- currentPlayer(Player),resetPlayer, Player = 'A' -> assert(currentPlayer('D'));Player = 'D' -> assert(currentPlayer('A')).
applyIt(NewBoard) :- resetBoard, assert(board(NewBoard)).
resetBoard :- findall(_,retract(board(_)),_).
resetSize :- findall(_,retract(size(_)),_).
resetPlayer :- findall(_, retract(currentPlayer(_)),_).
reset :- resetBoard, resetSize, resetPlayer, resetAttackers, resetDefenders.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% IA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Tests Unitaires & autres %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :- 
	remove(2,[0,1,2,3,4,5],A),
	printList(A).

testMove :-
	init(13),
	setElmt(6,6, '_R_'), 
	setElmt(6,5, '_D_'), 
	displayBoard, 
	move(6,6, 'S', 2), 
	displayBoard, 
	move(6,5, 'N', 3), 
	displayBoard, 
	move(6,2, 'O', 2), 
	displayBoard, 
	move(6,8, 'E', 3), 
	displayBoard.

testCollision:-
	init(13),
	setElmt(6,6, '_R_'), 
	setElmt(6,5, '_D_'),
	displayBoard, 
	move(6,5, 'S', 3), 
	displayBoard, 
	move(6,6, 'S', 5),
	displayBoard, 
	move(6,5, 'S', 1), 
	displayBoard. 

testChange :-
	init(9),
	setElmt(2,3,'_R_'), 
	displayBoard.

testCombat :- 
	init(9),
    setElmt(3, 3, '_D_'), 
    setElmt(3, 4, '_A_'),
    setElmt(2, 3, '_A_'),
    setElmt(4, 3, '_A_'),
    setElmt(1, 3, '_D_'),
    setElmt(5, 3, '_D_'),
    setElmt(2, 5, '_D_'),
    displayBoard,
    move(2,5, 'E', 1),
    displayBoard.

testPlayerChange:- assert(currentPlayer('D')), changePlayer, currentPlayer(Player), write(Player).

testListAttDef(List) :-
    assert(attackers(List)),
    printList(List),
    writeln('\n\n'),
    setPieceInAttackers(1,[9,9]),
    getPieceInAttackers(2,X),
    printList(X), writeln('\n\n'),
    getPieceInAttackers(1,Y),
    printList(Y),writeln('\n\n'),
    attackers(Att),
    printList(Att).
    
testCreationList :-
    initGame(13),
    attackers(Att),
    defenders(Def),
    printList(Att),
    writeln('\n'),
    printList(Def).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
