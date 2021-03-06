:- dynamic board/1. % permet l'assertion et le retrait de faits board/1
:- dynamic size/1.
:- dynamic currentPlayer/1.

:- consult(board_manager).
:- consult(utilities).
:- consult(game_predicates).
:- consult(ia_Defence).
:- consult(ia_Play).


%%%%% init game %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	init : Permet de créer un plateau vide
%	@param: Size -> Longueur & Largeur du plateau

init(Size) :- reset, assert(size(Size)), createBoard(Size), displayBoard.

%	initGame : Permet de créer un plateau et de le préparer au jeu, disposition des pions.
%	@param: Size -> Longueur & Largeur du plateau

initGame(Size) :- reset, assert(size(Size)), createAndSetupBoard(Size), displayBoard.
			
play:-
	initGame(9),
	assert(currentPlayer('A')),
	gameloop.	

gameloop:- 
	currentPlayer(Player),
	write('New turn for:'),	writeln(Player),
    callAI, % appel à l'IA du Player 
	sleep(2),
    displayBoard,
	changePlayer,
	(checkForVictory;gameloop).
		
gameloop:- writeln('- Fin du jeu -').

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Appel des IA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	callAI:-
		currentPlayer(Player),
		(Player = 'A',(iaPhase1Agg; iaPhase2));
		runAI_Defence.


% ---- Test des IA ---- %

testTestIA_Attack:-initGame(13), assert(currentPlayer('A')), move(0,5,'E', 5), displayBoard, playTestIA_Attack.

playTestIA_Attack:-(not(iaPhase1Agg)->iaPhase2; !), notrace, displayBoard, changePlayer, sleep(2), playTestIA_Attack.

testPseudoRandomPlay:-initGame(13), assert(currentPlayer('A')), pseudoRandomPlay.

pseudoRandomPlay:-iaPhase2Agg, displayBoard,  sleep(2), pseudoRandomPlay.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Tests Unitaires & autres %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%	testMove : Test les déplacements des pions	
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


%	testCollision : Doit renvoyer false après 3 affichages.	
testCollision:- 
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
	writeln('Test failure : expected false after 3 printboard').


%	testRemovePiece : 
%		Doit enlever le roi, un attaquant et un défenseur
%		L'affichage des listes montre cette différence comme la visualisation du plateau
testRemovePiece:-
	initGame(13),
	attackers(PreAtt),
	defenders(PreDef),
	removePieceOnBoard(6,6),
	displayBoard,
	removePieceOnBoard(5,6),
	displayBoard,
	removePieceOnBoard(0,6),
	displayBoard,
	attackers(PostAtt),
	defenders(PostDef),
	printList(PreAtt),
	printList(PostAtt),writeln(''),
	printList(PreDef),
	printList(PostDef).


%	testCombat : 
%		Doit tuer la pièce 'attaquant' cernée.
testCombat :- 
	init(9),
	initListAttDef,
	setPieceOnBoard(0,[3,3],'_D_'),
	setPieceOnBoard(1,[3,4],'_A_'),
	setPieceOnBoard(2,[2,3],'_A_'),
	setPieceOnBoard(1,[4,3],'_A_'),	
	setPieceOnBoard(2,[1,3],'_D_'),
	setPieceOnBoard(3,[5,3],'_D_'),
	setPieceOnBoard(4,[2,5],'_D_'),	
    displayBoard,
    move(2,5,'E', 1),
    displayBoard.

%	testPlayerChange : 
%		Fait changer le joueur en train de jouer
testPlayerChange:- 
	assert(currentPlayer('D')), 
	changePlayer, 
	currentPlayer(Player), 
	write(Player).


%	testListAttDef : 
%		Doit renvoyer des listes de variables non instanciée excepté pour l'index 1 dans les attaquants et 3 dans les defenseurs
testListAttDef:-
	initListAttDef,
    setPieceInAttackers(1,[9,9]),
    getPieceInAttackers(1,X),
    printList(X),
    attackers(Att),
    printList(Att),
	setPieceInDefenders(3,[6,6]),
    getPieceInDefenders(3,Y),
    printList(Y),
    defenders(Def),
    printList(Def).
  
%	testCreationList : 
%		Vérifie que les listes sont bien instanciées  
testCreationList :-
    initGame(13),
    attackers(Att),
    defenders(Def),
    printList(Att),
    printList(Def).


%	testUpdatePiecesAtt : 
%		Test la mise à jour de pièce et la synchro data + affichage	
testUpdatePiecesAtt :-
    initGame(13),
    attackers(Att),
    printList(Att),
	write('\n Time to update ! \n'),
	getPieceInAttackers(2,X),
	updatePieceOnBoard(X,[3,3]),
	displayBoard,
	attackers(NewAtt),
    printList(NewAtt).

%	testKingDead : 
%			
testKingDead :- 
	init(9),
	length(ListDef,1),
	assert(defenders(ListDef)),
	length(ListAtk,4),
	assert(attackers(ListAtk)),
    setPieceOnBoard(0,[3,3], '_R_'), 
    setPieceOnBoard(0,[2,3], '_A_'),
    setPieceOnBoard(1,[3,2], '_A_'),
    setPieceOnBoard(2,[4,3], '_A_'),
    setPieceOnBoard(3,[3,4], '_A_'),
    displayBoard,
    checkKingLose.

%	testKingCastle : 
%	
testKingCastle :- 
	init(9),
	length(ListDef,1),
	assert(defenders(ListDef)),
	setPieceOnBoard(0,[0,8],'_R_'),
	displayBoard,
    checkKingWin.

%	testAttackersDead : 
%		
testAttackersDead :- 
	init(9),
	length(ListAtk,0),
	assert(attackers(ListAtk)),
    checkAttackersDead.	


%%%%%% Test IA Defence
% Doit retourner la plus grande distance de déplacement possible (N = 3)
testMoveKing:-
	initGame(9),
	removePieceOnBoard(4,1),
	removePieceOnBoard(4,2),
	removePieceOnBoard(4,3),
	moveKing(DirectionToPlay,NbCase),
	write('Direction du mouvement (attendu = N) : '), writeln(DirectionToPlay),
	write('Nombre de case de deplacement (attendu = 3): '), writeln(NbCase),
	DirectionToPlay = 'N',
	NbCase = 3 .

testChooseCaseToMoveOn:-
	chooseCaseToMoveOn([4,4],[[4,5],[4,6],[4,7],[4,7]], MaxNbCase),
	write('Nombre max attendu : '), writeln('3'),
	write('Nombre max calcule : '), writeln(MaxNbCase),
	MaxNbCase = 3 .

launchAllTests :-

	writeln('=== testMove'),testMove,
	writeln('=== testCombat'),testCombat,
	writeln('=== testRemovePiece'),testRemovePiece,
	writeln('=== testPlayerChange'),testPlayerChange,
	writeln('=== testListAttDef'),testListAttDef,
	writeln('=== testCreationList'),testCreationList,
	writeln('=== testUpdatePiecesAtt'),testUpdatePiecesAtt,
	writeln('=== testKingDead'),testKingDead,
	writeln('=== testKingCastle'),testKingCastle,
	writeln('=== testAttackersDead'),testAttackersDead,
	writeln('=== testChooseCaseToMoveOn'),testChooseCaseToMoveOn,
	writeln('=== testMoveKing'),testMoveKing.
    