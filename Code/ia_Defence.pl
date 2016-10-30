:- dynamic prefered_Vertical_Direction/1.
:- dynamic prefered_Horizontal_Direction/1.

% ----- IA de défense -----

% Méthode à appeller pour utiliser l'IA
runAI_Defence:- 
	(decide(X,Y,D,N), move(X,Y,D,N) ; randomMove('D')).

		
decide(X,Y,D,N):-
	(moveKing(D,N),getPieceInDefenders(0,[X,Y])).
	
moveKing(DirectionToPlay,NbCase):- 
	getPieceInDefenders(0,[X,Y]),
	(
		(prefered_Vertical_Direction(Dir), getAllWalkablePath([X,Y], Dir, ListCase), DirectionToPlay = Dir);
		(getAllWalkablePath([X,Y], 'N', ListCase), length(ListCase,Size), Size > 0, updatePrefered_Vertical_Direction('N'),DirectionToPlay = 'N');
		(getAllWalkablePath([X,Y], 'S', ListCase), length(ListCase,Size), Size > 0, updatePrefered_Vertical_Direction('S'),DirectionToPlay = 'S');
		(prefered_Horizontal_Direction(Dir), getAllWalkablePath([X,Y], Dir, ListCase), DirectionToPlay = Dir);
		(getAllWalkablePath([X,Y], 'E', ListCase), length(ListCase,Size), Size > 0, updatePrefered_Horizontal_Direction('E'),DirectionToPlay = 'E');
		(getAllWalkablePath([X,Y], 'O', ListCase), length(ListCase,Size), Size > 0, updatePrefered_Horizontal_Direction('O'),DirectionToPlay = 'O')
	),
	length(ListCase,Size), Size > 0,
	chooseCaseToMoveOn([X,Y],ListCase,NbCase).

% Détermine la case la plus loin qui peut être jouée sans risque.			
chooseCaseToMoveOn([X,Y],ListCase, MaxNbCase):-
	findall(NbCase,isPlayableCase([X,Y],ListCase,NbCase), ListNbCase),
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
	getCaseOnBoard(X,Y,Elem),
	(
		(Direction == 'N' ,((Elem == '___',X == PosX, Y < PosY); (not(Elem == '___'),X == PosX, Y > PosY ,!, fail)));		
		(Direction == 'S' ,((Elem == '___',X == PosX, Y > PosY); (not(Elem == '___'),X == PosX, Y < PosY ,!, fail)));
		(Direction == 'E' ,((Elem == '___',X > PosX, Y == PosY); (not(Elem == '___'),X < PosX, Y == PosY ,!, fail)));
		(Direction == 'O' ,((Elem == '___',X < PosX, Y == PosY); (not(Elem == '___'),X > PosX, Y == PosY ,!, fail)))
	).

% Trouve toutes les cases libres et atteignables dans une direction
getAllWalkablePath([PosX,PosY],Direction, L) :- findall([X,Y], getWalkablePath([PosX,PosY], [X,Y],Direction), L).

% Permet de changer la direction direction préférée sauvegardée.
updatePrefered_Vertical_Direction(NewDirection):-
	findall(_, retract(prefered_Vertical_Direction(_)),_), assert(prefered_Vertical_Direction(NewDirection)).
	
updatePrefered_Horizontal_Direction(NewDirection):-
	findall(_, retract(prefered_Horizontal_Direction(_)),_), assert(prefered_Horizontal_Direction(NewDirection)).

