:- dynamic board/1. % permet l'assertion et le retrait de faits board/1
:- dynamic size/1.
:- dynamic currentPlayer/1.

:- consult(board_manager).
:- consult(utilities).
:- consult(game_predicates).

%%%%% init game %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Size) :- reset, assert(size(Size)), createBoard(Size), displayBoard.
initGame(Size) :- reset, assert(size(Size)), createAndSetupBoard(Size), displayBoard.

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
	
testUpdatePiecesAtt :-
    initGame(13),
    attackers(Att),
    printList(Att),
    writeln('\n'),
	writeln('\n Time to update ! \n'),
	getPieceInAttackers(2,X),
	updatePieceInAttackers(X,[3,3]),
	displayBoard,
	writeln('\n'),
	attackers(NewAtt),
    printList(NewAtt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
