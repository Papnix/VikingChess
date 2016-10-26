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
% - Gestion Listes pions -------------------------------------------------------------------------------------------------------------------------- %

setPieceInAttackers(Indice,Piece) :- 
	attackers(List), 
	replace(List,Indice,Piece,NewList), 
	updateAttackers(NewList).
	
updatePieceInAttackers([Old_X,Old_Y],[X,Y]):-
	attackers(List), 
	update([Old_X,Old_Y],List, NewList, [X,Y]), 
	updateAttackers(NewList),
	setElmt(Old_X,Old_Y,'___'),
	setElmt(X,Y,'_A_').
	
getPieceInAttackers(Indice,Piece) :- 
	attackers(List), 
	nth0(Indice, List, Piece).
	
% -----	
setPieceInDefenders(Indice,Piece) :- 
	defenders(List), 
	replace(List,Indice,Piece,NewList), 
	updateDefenders(NewList).
	
updatePieceInDefenders([Old_X,Old_Y],[X,Y]):- 
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
	nth0(Indice,Def,[X,Y]),
	remove(Indice,Att,NewAtt),
	updateAttackers(NewAtt));
removePiece(X,Y) :- 
	getCaseOnBoard(X,Y,E)
	(E = '_D_' ; E = '_R_'),
	defenders(Def),
	nth0(Indice,Def,[X,Y]),
	remove(Indice,Def,NewAtt),
	updateAttackers(NewAtt)).
removePiece(X,Y).	

updateAttackers(List):- resetAttackers, assert(attackers(List)).
updateDefenders(List):- resetDefenders, assert(defenders(List)).

resetAttackers :- findall(_,retract(attackers(_)),_).
resetDefenders :- findall(_,retract(defenders(_)),_).
% ------------------------------------------------------------------------------------------------------------------------------------------------- %