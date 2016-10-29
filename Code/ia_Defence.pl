% ----- IA de défense -----

% Méthode à appeller pour utiliser l'IA
runAI_Defence:- 
		(decide(X,Y,D,N), move(X,Y,D,N) ; randomMove('D')).

		
decide(X,Y,D,N):-
		(moveKing(X,Y,D,N)).
	
moveKing(XtoPlay,YtoPlay,DirectionToPlay,NbCase):- !.

% Renvoie les coordonnées de la case si elle est accessible (pas d'interet seul, appeller getAllWalkablePath)
getWalkablePath([PosX,PosY],[X,Y],Direction):-
		board(Board), getCaseObBoard(X,Y,Elem),
       	(
			(Direction == 'N' ,((Elem == '___',X == PosX, Y < PosY); (not(Elem == '___'),X == PosX, Y < PosY ,!, fail)));		
			(Direction == 'S' ,((Elem == '___',X == PosX, Y > PosY); (not(Elem == '___'),X == PosX, Y > PosY ,!, fail)));
			(Direction == 'E' ,((Elem == '___',X > PosX, Y == PosY); (not(Elem == '___'),X > PosX, Y == PosY ,!, fail)));
			(Direction == 'O' ,((Elem == '___',X < PosX, Y == PosY); (not(Elem == '___'),X < PosX, Y == PosY ,!, fail)))
        ).

% Trouve toutes les cases libres et atteignables dans une direction
getAllWalkablePath([PosX,PosY],Direction, L) :- findall([X,Y], getWalkablePath([PosX,PosY], [X,Y],Direction), L).


