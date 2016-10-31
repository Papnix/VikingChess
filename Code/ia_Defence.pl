:- dynamic prefered_Vertical_Direction/1.
:- dynamic prefered_Horizontal_Direction/1.

% ----- IA de défense -----

% Méthode à appeller pour utiliser l'IA
runAI_Defence:- 
	(
		getPieceInDefenders(0,[X,Y]),
		decide(D,N),
		write('Mouvement prévu : '), write(N),write(' vers '),writeln(D),
		moveKing(X,Y,D,N)
	)
	; 
	(
		read(X),read(Y),read(D),read(N),moveKing(X,Y,D,N)
	).
		
decide(DirectionToPlay,NbCase):- 
	getPieceInDefenders(0,[PosX,PosY]),
	(
		% On check les directions privilégiées
		(
			%writeln('Pref vertical'),
			prefered_Vertical_Direction(Dir),
			getAllWalkablePath([PosX,PosY], Dir, ListCase),
			not(ListCase == []),
			DirectionToPlay = Dir
		)
		;
		(
			%writeln('Pref horizontal'),
			prefered_Horizontal_Direction(Dir),
			getAllWalkablePath([PosX,PosY], Dir, ListCase),
			not(ListCase == []),
			DirectionToPlay = Dir
		)
		;
		(	% Si aucune direction n'est privilégié, alors on va checker les chemins
			(	
				%writeln('Bloc horizontal'),
				% On ne calcul pas de chemin si les conditions d'avant on échouées sur un "not(ListCase == [])" 	
				not(prefered_Horizontal_Direction(Dir)),
			
				% On est maintenant sûr qu'il n'y a pas de direction horizontale, on cherche si une direction peut être prise (not(ListCase == []))
				(getAllWalkablePath([PosX,PosY], 'E', ListCase), not(ListCase == []), updatePrefered_Horizontal_Direction('E'),DirectionToPlay = 'E');
				(getAllWalkablePath([PosX,PosY], 'O', ListCase), not(ListCase == []), updatePrefered_Horizontal_Direction('O'),DirectionToPlay = 'O')
			)
			;
			(
				%writeln('Bloc vertical'),
				% De même pour la verticali
				not(prefered_Vertical_Direction(Dir)),
							
				(getAllWalkablePath([PosX,PosY], 'N', ListCase), not(ListCase == []), updatePrefered_Vertical_Direction('N'),DirectionToPlay = 'N');
				(getAllWalkablePath([PosX,PosY], 'S', ListCase), not(ListCase == []), updatePrefered_Vertical_Direction('S'),DirectionToPlay = 'S')	
			)
		)
	),
	%writeln('Fin decide'),
	chooseCaseToMoveOn([PosX,PosY],ListCase,NbCase).
	
	
% Détermine la case la plus loin qui peut être jouée sans risque.			
chooseCaseToMoveOn([PosX,PosY],ListCase, MaxNbCase):-
	findall(NbCase,isPlayableCase([PosX,PosY],ListCase,NbCase), ListNbCase),
	max_list(ListNbCase,MaxNbCase).

isPlayableCase([X,Y],ListCase,NbCase):-
	nth0(_,ListCase,[Targeted_X,Targeted_Y]),
	%   checkDanger([Targeted_X,Targeted_Y]),
	calculNbCase([X,Y],[Targeted_X,Targeted_Y],NbCase).

% Calcul le déplacement en nombre de case entre deux positions			
calculNbCase([X,Y],[ToX,ToY],NbCase):-
	(abs(ToX,X,Result), Result > 0, NbCase = Result);
	(abs(ToY,Y,Result), Result > 0, NbCase = Result).				

% Renvoie les coordonnées de la case si elle est accessible (pas d'interet seul, appeller getAllWalkablePath)
getWalkablePath([PosX,PosY],[X,Y],Direction):-
		(Direction = 'N',
			checkNorthPath([PosX,PosY],[X,Y])
		);		
		(Direction = 'S',
			checkSouthPath([PosX,PosY],[X,Y])
		);
		(Direction = 'E',
			checkEastPath([PosX,PosY],[X,Y])
		);
		(Direction = 'O',
			checkWestPath([PosX,PosY],[X,Y])
		).
		
checkNorthPath([PosX,PosY],[X,Y]):-
	board(Board), reverse(Board,BoardReversed),
	size(Size), 
	Reverse_PosY is Size - 1 - PosY,
	nth0(Reverse_Y, BoardReversed, L),
	nth0(X, L, Elem),
	(((Elem == '___';Elem == '_X_'),X == PosX, Reverse_Y > Reverse_PosY);
	(not((Elem == '___';Elem == '_X_')),X == PosX, Reverse_Y > Reverse_PosY , !, fail)),
	Y is Size - 1 - Reverse_Y.
	
checkSouthPath([PosX,PosY],[X,Y]):-
	getCaseOnBoard(X,Y,Elem),
	(((Elem == '___';Elem == '_X_'),X == PosX, Y > PosY); (not((Elem == '___';Elem == '_X_')),X == PosX, Y > PosY ,!, fail)).
		
checkEastPath([PosX,PosY],[X,Y]):-
	getCaseOnBoard(X,Y,Elem),
	(((Elem == '___';Elem == '_X_'),X > PosX, Y == PosY); (not((Elem == '___';Elem == '_X_')),X > PosX, Y == PosY ,!, fail)).

checkWestPath([PosX,PosY],[X,Y]):-
	board(Board), nth0(Y, Board, L), reverse(L,LReverse),
	size(Size), 
	Reverse_PosX is Size - 1 - PosX,
	nth0(Reverse_X, LReverse, Elem),		
	(((Elem == '___';Elem == '_X_'),Reverse_X > Reverse_PosX, Y == PosY);
	(not((Elem == '___';Elem == '_X_')),Reverse_X > Reverse_PosX, Y == PosY , !, fail)),
	X is Size - 1 - Reverse_X.	
		
% Trouve toutes les cases libres et atteignables dans une direction
getAllWalkablePath([PosX,PosY],Direction, L) :-
	findall([X,Y], getWalkablePath([PosX,PosY], [X,Y],Direction), L).

% Permet de changer la direction direction préférée sauvegardée.
updatePrefered_Vertical_Direction(NewDirection):-
	findall(_, retract(prefered_Vertical_Direction(_)),_), assert(prefered_Vertical_Direction(NewDirection)).
	
updatePrefered_Horizontal_Direction(NewDirection):-
	findall(_, retract(prefered_Horizontal_Direction(_)),_), assert(prefered_Horizontal_Direction(NewDirection)).

