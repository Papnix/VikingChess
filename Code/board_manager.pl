% Listes de liste de la forme [[x1, y1],[x2,y2]] représentant les coordonnées des pions
:- dynamic attackers/1.
:- dynamic defenders/1. % Le roi se trouve en tête de liste

% - Creation plateau ------------------------------------------------------------------------------------------------------------------------------ %

% populateList : Permet de remplir le plateau de jeu avec des cases vides représentées par '___'
populateList(([])).
populateList([H|T]) :- H = '___', populateList(T).


% createGrid : Rempli une liste de longueur Size avec des listes de longueur Size, créant ainsi une grille
% @param : Size la longueur de la liste et des listes à y ajouter.
createGrid(([]),_).
createGrid([H|T],Size) :- length(H,Size) , populateList(H), createGrid(T,Size).


% createBoard : Créer le plateau de jeu, le rempli de case vide et le sauvegarde
% @param : Size la longueur et la largeur du plateau
createBoard(Size) :- length(Board,Size), createGrid(Board,Size),assert(board(Board)).

% ------------------------------------------------------------------------------------------------------------------------------------------------- %
% - Mise en place plateau ------------------------------------------------------------------------------------------------------------------------- %


% createAndSetupBoard : Créer le plateau et place les pions
createAndSetupBoard(Size) :- createBoard(Size), setupBoard(Size).

 
% setupBoard : place les pions
setupBoard(Size) :- setCitadels(Size), setGamePieces(Size).


% setCitadels : place les cases de types 'citadelle'
setCitadels(Size) :-
	setCaseOnBoard(0,0,'_X_'),
	Var is Size - 1,
	Middle is Var / 2,
	setCaseOnBoard(0,Var,'_X_'),
	setCaseOnBoard(Middle,Middle,'_X_'),
	setCaseOnBoard(Var,0,'_X_'),
	setCaseOnBoard(Var,Var,'_X_').

 
%  initListAttDef : Créer et sauvegarde les liste de pions d'attaquants et de défenseurs	
initListAttDef:-
	resetAttackers,
	resetDefenders,
    length(ListAtt,16),
    length(ListDef,9),
    assert(attackers(ListAtt)),
    assert(defenders(ListDef)).

%  setGamePieces : place les pions sur le plateau et dans les listes appropriées.
% @param : la taille du plateau	
setGamePieces(Size) :-
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
 
%	updatePieceOnBoard : Utiliser pour faire bouger une pièce. Synchronise les données des tableaux avec l'affichage
%	@param : [Old_X,Old_Y] -> anciennes coordonnées de la pièce
%          [X,Y] 		 -> nouvelles coordonnées de la pièce
updatePieceOnBoard([Old_X,Old_Y],[X,Y]):-
	getCaseOnBoard(Old_X,Old_Y,Case),
	(Case = '_A_' -> updatePieceInAttackers([Old_X,Old_Y],[X,Y]),setCaseOnBoard(X,Y,'_A_');
	 Case = '_D_' -> updatePieceInDefenders([Old_X,Old_Y],[X,Y]),setCaseOnBoard(X,Y,'_D_');
	 Case = '_R_' -> updatePieceInDefenders([Old_X,Old_Y],[X,Y]),setCaseOnBoard(X,Y,'_R_')),
	setCaseOnBoard(Old_X,Old_Y,'___').


%	setPieceOnBoard : Uniquement appellé pendant l'initialisation du jeu. Créer les pions dans les listes et les place sur le plateau
%	@param: Index -> index auquel sera rangé la pièce dans le tableau correspondant à son type Att.Def
%			[X,Y] -> coordonnées de la pièce
%			CharacterToDisplay -> symbole représentant la pièce sur le plateau
setPieceOnBoard(Index,[X,Y],CharacterToDisplay):-
	(CharacterToDisplay = '_A_' -> setPieceInAttackers(Index,[X,Y]),setCaseOnBoard(X,Y,CharacterToDisplay);
	 CharacterToDisplay = '_D_' -> setPieceInDefenders(Index,[X,Y]),setCaseOnBoard(X,Y,CharacterToDisplay);
	 CharacterToDisplay = '_R_' -> setPieceInDefenders(Index,[X,Y]),setCaseOnBoard(X,Y,CharacterToDisplay)).
	
% -----

%	setPieceInAttackers : Uniquement appellé à l'initialisation Place un pion dans la liste des attaquants
%	@param: Indice -> index auquel sera rangé la pièce dans le tableau des attaquants
%			Piece  -> coordonnées de la pièce
setPieceInAttackers(Indice,Piece) :- 
	attackers(List), 
	replace(List,Indice,Piece,NewList), 
	updateAttackers(NewList).

%	updatePieceInAttackers : Uniquement appellé à l'initialisation Place un pion dans la liste des attaquants
%	@param: Indice -> index auquel sera rangé la pièce dans le tableau des attaquants
%			Piece  -> coordonnées de la pièce	
updatePieceInAttackers([Old_X,Old_Y],[X,Y]):-
	attackers(List), 
	update([Old_X,Old_Y],List, NewList, [X,Y]), 
	updateAttackers(NewList).


%	getPieceInAttackers : Renvoie un pion de la liste des attaquants
%	@param: Indice -> index de la pièce dont ont veut connaitre les coordonnées.
%			Piece  -> coordonnées de la pièce (retour)	
getPieceInAttackers(Indice,Piece) :- 
	attackers(List), 
	nth0(Indice, List, Piece).
	
% -----	

% Equivalent aux prédicats pour les attaquants (voir plus haut)
setPieceInDefenders(Indice,Piece) :- 
	defenders(List), 
	replace(List,Indice,Piece,NewList), 
	updateDefenders(NewList).
	
updatePieceInDefenders([Old_X,Old_Y],[X,Y]):- 
	defenders(List), 
	update([Old_X,Old_Y], List, NewList, [X,Y]), 
	updateDefenders(NewList).
	
getPieceInDefenders(Indice,Piece) :-
	defenders(List),
	nth0(Indice, List, Piece).
	
% -----


%	removePieceOnBoard : Enlève une pièce du plateau (graphique) et de la liste à laquelle il appartient
%	@param: X,Y -> coordonnées de la pièce
removePieceOnBoard(X,Y):-
	getCaseOnBoard(X,Y,Case),
	(Case = '_A_' -> getPieceInAttackers(Index,[X,Y]),removeAttacker(Index);
	 Case = '_D_' -> getPieceInDefenders(Index,[X,Y]),removeDefender(Index);
	 Case = '_R_' -> removeDefender(0)),
	setCaseOnBoard(X,Y,'___').

%	removeAttacker : Enlève une pièce à l'indice 'Index" à la liste des attaquants
%	@param: Index -> Index ou est rangé la pièce dans le tableau
removeAttacker(Index):-
	attackers(Att),
	remove(Index,Att,NewAtt),
	updateAttackers(NewAtt).
	
%	removeDefender : Enlève une pièce à l'indice 'Index" à la liste des defenseurs
%	@param: Index -> Index ou est rangé la pièce dans le tableau
removeDefender(Index):-
	defenders(Def),
	remove(Index,Def,NewDef),
	updateDefenders(NewDef).	

%	updateAttackers & updateDefenders : Permet de mettre à jours les listes en sauvegardant les nouvelles versions.
%	@param: List -> Liste qui doit être sauvegardé.
updateAttackers(List):- resetAttackers, assert(attackers(List)).
updateDefenders(List):- resetDefenders, assert(defenders(List)).


%	resetAttackers & resetDefenders : Supprime toutes les sauvegardes des listes.
%	@param: List -> Liste qui doit être sauvegardé.
resetAttackers :- findall(_,retract(attackers(_)),_).
resetDefenders :- findall(_,retract(defenders(_)),_).