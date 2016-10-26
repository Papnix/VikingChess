:- dynamic attackers/1.
:- dynamic defenders/1.

% - Creation plateau ------------------------------------------------------------------------------------------------------------------------------ %

populateList(([])).
populateList([H|T]) :- H = '___', populateList(T).

createGrid(([]),_).
createGrid([H|T],Size) :- length(H,Size) , populateList(H), createGrid(T,Size).
createBoard(Size) :- length(Board,Size), createGrid(Board,Size),assert(board(Board)).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %
% - Mise en place plateau ------------------------------------------------------------------------------------------------------------------------- %
createAndSetupBoard(Size) :- createBoard(Size), setupBoard(Size).

setupBoard(Size) :- setCitadels(Size), setPieces(Size).

setCitadels(Size) :-
	setCaseOnBoard(0,0,'_X_'),
	Var is Size - 1,
	Middle is Var / 2,
	setCaseOnBoard(0,Var,'_X_'),
	setCaseOnBoard(Middle,Middle,'_X_'),
	setCaseOnBoard(Var,0,'_X_'),
	setCaseOnBoard(Var,Var,'_X_').
	
initListAttDef:-
	resetAttackers,
	resetDefenders,
    length(ListAtt,16),
    length(ListDef,9),
    assert(attackers(ListAtt)),
    assert(defenders(ListDef)).
	
setPieces(Size) :-
    Width is Size - 1,
    Middle is Width / 2,
    Offset is Size - 2,
    
    P2 is Middle - 1,
    P3 is Middle,
    P4 is Middle + 1,
    
	initListAttDef,
    
    setPieceOnBoard(0,[0,P2],'_A_'),
    setPieceOnBoard(1,[0,P3],'_A_'),
    setPieceOnBoard(2,[1,P3],'_A_'),
    setPieceOnBoard(3,[0,P4],'_A_'),
    setPieceOnBoard(4,[P2,0],'_A_'),
    setPieceOnBoard(5,[P3,0],'_A_'),
    setPieceOnBoard(6,[P3,1],'_A_'),
    setPieceOnBoard(7,[P4,0],'_A_'),
    setPieceOnBoard(8,[Width,P2],'_A_'),
    setPieceOnBoard(9,[Width,P3],'_A_'),
    setPieceOnBoard(10,[Offset,P3],'_A_'),
    setPieceOnBoard(11,[Width,P4],'_A_'),
    setPieceOnBoard(12,[P2,Width],'_A_'),
    setPieceOnBoard(13,[P3,Width],'_A_'),
    setPieceOnBoard(14,[P3,Offset],'_A_'),
    setPieceOnBoard(15,[P4,Width],'_A_'),
    
    Pos_D_1 is Middle + 1,
    Pos_D_2 is Middle + 2,
    Neg_D_1 is Middle - 1,
    Neg_D_2 is Middle - 2,
	
    setPieceOnBoard(0,[Middle,Middle],'_R_'),
    setPieceOnBoard(1,[Pos_D_1,Middle],'_D_'),
    setPieceOnBoard(2,[Pos_D_2,Middle],'_D_'),
    setPieceOnBoard(3,[Neg_D_1,Middle],'_D_'),
    setPieceOnBoard(4,[Neg_D_2,Middle],'_D_'),
    setPieceOnBoard(5,[Middle,Pos_D_1],'_D_'),
    setPieceOnBoard(6,[Middle,Pos_D_2],'_D_'),
    setPieceOnBoard(7,[Middle,Neg_D_1],'_D_'),
    setPieceOnBoard(8,[Middle,Neg_D_2],'_D_').
	
% ------------------------------------------------------------------------------------------------------------------------------------------------- %
% - Gestion Listes pions -------------------------------------------------------------------------------------------------------------------------- %

% A utiliser pour bouger les pièces
updatePieceOnBoard([Old_X,Old_Y],[X,Y]):-
	getCaseOnBoard(Old_X,Old_Y,Case),
	(Case = '_A_' -> updatePieceInAttackers([Old_X,Old_Y],[X,Y]),setCaseOnBoard(X,Y,'_A_');
	 Case = '_D_' -> updatePieceInDefenders([Old_X,Old_Y],[X,Y]),setCaseOnBoard(X,Y,'_D_');
	 Case = '_R_' -> updatePieceInDefenders([Old_X,Old_Y],[X,Y]),setCaseOnBoard(X,Y,'_R_')),
	setCaseOnBoard(Old_X,Old_Y,'___').

setPieceOnBoard(Index,[X,Y],CharacterToDisplay):-
	(CharacterToDisplay = '_A_' -> setPieceInAttackers(Index,[X,Y]),setCaseOnBoard(X,Y,CharacterToDisplay);
	 CharacterToDisplay = '_D_' -> setPieceInDefenders(Index,[X,Y]),setCaseOnBoard(X,Y,CharacterToDisplay);
	 CharacterToDisplay = '_R_' -> setPieceInDefenders(Index,[X,Y]),setCaseOnBoard(X,Y,CharacterToDisplay)).
	
% -----
setPieceInAttackers(Indice,Piece) :- 
	attackers(List), 
	replace(List,Indice,Piece,NewList), 
	updateAttackers(NewList).
	
updatePieceInAttackers([Old_X,Old_Y],[X,Y]):-
	attackers(List), 
	update([Old_X,Old_Y],List, NewList, [X|Y]), 
	updateAttackers(NewList).
	
getPieceInAttackers(Indice,Piece) :- 
	attackers(List), 
	nth0(Indice, List, Piece).
	
% -----	
setPieceInDefenders(Indice,Piece) :- 
	defenders(List), 
	replace(List,Indice,Piece,NewList), 
	updateDefenders(NewList).
	
updatePieceInDefenders([Old_X|Old_Y],[X|Y]):- 
	defenders(List), 
	update(OldPiece, List, NewList, NewPiece), 
	updateDefenders(NewList).
	
getPieceInDefenders(Indice,Piece) :-
	defenders(List),
	nth0(Indice, List, Piece).
% -----

removePiece(X,Y) :- 
	getCaseOnBoard(X,Y,E),
	E = '_A_' , attackers(Att),
	nth0(Indice,Def,[X|Y]),
	remove(Indice,Att,NewAtt),
	updateAttackers(NewAtt));
removePiece(X,Y) :- 
	getCaseOnBoard(X,Y,E)
	(E = '_D_' ; E = '_R_'),
	defenders(Def),
	nth0(Indice,Def,[X|Y]),
	remove(Indice,Def,NewAtt),
	updateAttackers(NewAtt)).
removePiece(X,Y).	

updateAttackers(List):- resetAttackers, assert(attackers(List)).
updateDefenders(List):- resetDefenders, assert(defenders(List)).

resetAttackers :- findall(_,retract(attackers(_)),_).
resetDefenders :- findall(_,retract(defenders(_)),_).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %