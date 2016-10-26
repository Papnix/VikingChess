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

testMove :-
	init(13),
	initListAttDef,
	setCaseOnBoard(6,6,'_R_'),
    setPieceInDefenders(0,[6,6]),
    setCaseOnBoard(6,5,'_D_'),
    setPieceInDefenders(1,[6,5]),
	displayBoard, 
	move(6,6, 'S', 2), 
	displayBoard, 
	move(6,5, 'N', 3), 
	displayBoard, 
	move(6,2, 'O', 2), 
	displayBoard, 
	move(6,8, 'E', 3), 
	displayBoard.

testCollision:- % Doit renvoyer false après 3 affichages.
	init(13),
	initListAttDef,
	setCaseOnBoard(6,6,'_R_'),
    setPieceInDefenders(0,[6,6]),
    setCaseOnBoard(6,5,'_D_'),
    setPieceInDefenders(1,[6,5]),
	displayBoard, 
	move(6,5, 'S', 3), 
	displayBoard, 
	move(6,6, 'S', 5),
	displayBoard, 
	move(6,5, 'S', 1), 
	displayBoard,
	writeln('Test failure : expected false').
testCollision:- writeln('Test success : expected prolog return false').

testChange :-
	init(9),
	setCaseOnBoard(2,3,'_R_'), 
	displayBoard.

testCombat :- 
	init(9),
	initListAttDef,
    setCaseOnBoard(3, 3, '_D_'),
		setPieceInDefenders(0,[3,3]),	
    setCaseOnBoard(3, 4, '_A_'),
		setPieceInDefenders(0,[3,3]),
    setCaseOnBoard(2, 3, '_A_'),
		setPieceInDefenders(0,[3,3]),
    setCaseOnBoard(4, 3, '_A_'),
		setPieceInDefenders(0,[3,3]),
    setCaseOnBoard(1, 3, '_D_'),
		setPieceInDefenders(0,[3,3]),
    setCaseOnBoard(5, 3, '_D_'),
		setPieceInDefenders(0,[3,3]),
    setCaseOnBoard(2, 5, '_D_'),
		setPieceInDefenders(0,[3,3]),
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
	updatePieceOnBoard(X,[3,3]),
	displayBoard,
	writeln('\n'),
	attackers(NewAtt),
    printList(NewAtt).
	
launchAllTests :-
	writeln('=== testMove'),testMove,
	writeln('=== testChange'),testChange,
	writeln('=== testCombat'),testCombat,
	writeln('=== testPlayerChange'),testPlayerChange,
	writeln('=== testListAttDef'),testListAttDef([100,100,100,100]),
	writeln('=== testCreationList'),testCreationList,
	writeln('=== testUpdatePiecesAtt'),testUpdatePiecesAtt,
	writeln('=== testCollision'),testCollision.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
