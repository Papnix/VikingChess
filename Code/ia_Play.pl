%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% IA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getOtherPiece(PieceList):-currentPlayer(Player), (Player = 'D' -> attackers(PieceList); Player = 'A' -> defenders(PieceList)).

getOwnPiece(PieceList):- currentPlayer(Player), (Player = 'A' -> attackers(PieceList); Player = 'D' -> defenders(PieceList)).

iaPhase1Agg:- getOtherPiece(PieceList), checkTarget(PieceList). 

checkTarget([]):-!.
checkTarget([[X, Y|_]|List]):- around(X,Y, Neighbors, _), not(checkDanger(X, Y, Neighbors)->checkTarget(List)); !.

checkDanger(_,_,[]).
checkDanger(X, Y, [[Xbis, Ybis|_]|List]):- currentPlayer(Player), getCaseOnBoard(Xbis, Ybis, E), ((Player = 'A',E = '_A_');
	 (Player = 'D', (E = '_D_'; E = '_R_')))->opPosition(X, Y, Xbis, Ybis); checkDanger(X, Y, List).


opPosition(X, Y, X1, Y1):-NewX is 2*X-X1, NewY is 2*Y-Y1, getOwnPiece(PieceList), pieceOp(NewX, NewY, PieceList). 

pieceOp(X,Y, []):-!.
pieceOp(X, Y, [[Xbis, Ybis|_]|List]):- not(X = Xbis ->(abs(Y,Ybis,ResultY), (Y-Ybis>0 -> move(Xbis, Ybis, 'S', ResultY);move(Xbis, Ybis, 'N', ResultY)));
	 Y=Ybis ->(abs(X, Xbis, ResultX), (X-Xbis>0 -> move(Xbis, Ybis, 'E', ResultX);move(Xbis, Ybis, 'O', ResultX))))-> pieceOp(X, Y, List); !. 

% IA - Aleatoire. 
iaPhase2:-currentPlayer(Player), randomMove(Player).

% choisi aleatoirement une piece dans la liste des pieces du joueur qui doit jouer.
% si le premier mouvement echoue (a cause de collision ou autre) relance une procedure de choix).
randomMove(Player):-(Player = 'A' ->attackers(PieceList);
	      Player = 'D' ->defenders(PieceList)), 
	      choosePiece(PieceList, X, Y), chooseDir(Dir), 
	      NbCase is random(3)+1, 
	      not(move(X, Y, Dir, NbCase)) -> randomMove(Player); !.

% choisi aleatoirement la direction (N, S, E, O) du mouvement. 
chooseDir(Dir):- DirNum is random(4), (DirNum = 0 -> Dir = 'N';DirNum = 1 -> Dir = 'S';DirNum = 2 -> Dir = 'E';DirNum = 3 -> Dir = 'O').

% determine le nombre de case dont la piece ce deplacera.
choosePiece(ListPiece, X, Y):-length(ListPiece, NbOfPieces), PieceNum is random(NbOfPieces), nth0(PieceNum, ListPiece, [X,Y|_]). 

