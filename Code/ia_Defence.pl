% ----- IA de défense -----

% Méthode à appeller pour utiliser l'IA
runAI_Defence:- 
		(decide(X,Y,D,N), move(X,Y,D,N) ; randomMove('D')).

		
decide(X,Y,D,N):-
		(moveKing(X,Y,D,N)).
	
moveKing(XtoPlay,YtoPlay,DirectionToPlay,NbCase):- !.
	
getWalkablePath([PosX,PosY],[X,Y]):-
		board(Board), nth0(X, Board, Column),nth0(Y, Column, Elem),
		((X == PosX, Y > PosY, Elem == '___')).