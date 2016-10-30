%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% IA - Aggressive %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getOtherPiece(PieceList):-currentPlayer(Player), (Player = 'D' -> attackers(PieceList); Player = 'A' -> defenders(PieceList)).

getOwnPiece(PieceList):- currentPlayer(Player), (Player = 'A' -> attackers(PieceList); Player = 'D' -> defenders(PieceList)).

iaPhase1Agg:- currentPlayer(Player), write(Player), writeln(" - IA aggressive"), getOtherPiece(PieceList), checkTarget(PieceList). 

checkTarget([]):-!, fail.
checkTarget([[X, Y|_]|List]):- around(X,Y, Neighbors, _), not(checkKillable(X, Y, Neighbors)),checkTarget(List).

checkKillable(_,_,[]):- !, fail.
checkKillable(X, Y, [[Xbis, Ybis|_]|List]):- currentPlayer(Player), getCaseOnBoard(Xbis, Ybis, E), ((Player = 'A',E = '_A_');
	 (Player = 'D', (E = '_D_'; E = '_R_')),((opPosition(X, Y, Xbis, Ybis); !)); checkKillable(X, Y, List)); checkKillable(X, Y, List).


opPosition(X, Y, X1, Y1):-NewX is 2*X-X1, NewY is 2*Y-Y1, getOwnPiece(PieceList), pieceOp(NewX, NewY, PieceList).


pieceOp(X,Y, []):-!, fail.
pieceOp(X, Y, [[Xbis, Ybis|_]|List]):- not(X = Xbis ,((abs(Y,Ybis,ResultY), (Y-Ybis>0, (move(Xbis, Ybis, 'S', ResultY);move(Xbis, Ybis, 'N', ResultY))));Y=Ybis,(abs(X,Xbis,ResultX), (X-Xbis>0, (move(Xbis, Ybis, 'E', ResultX);move(Xbis, Ybis, 'O', ResultX)))))), pieceOp(X, Y, List). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% IA - Totalement Aleatoire%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
iaPhase2:-currentPlayer(Player), write(Player), writeln(" - IA aleatoire"), randomMove(Player).

% choisi aleatoirement une piece dans la liste des pieces du joueur qui doit jouer.
% si le premier mouvement echoue (a cause de collision ou autre) relance une procedure de choix).
randomMove(Player):-(Player = 'A' ->attackers(PieceList);
	      Player = 'D' ->defenders(PieceList)), 
	      choosePiece(PieceList, X, Y), chooseDir(Dir), 
	      NbCase is random(4)+1, 
	      not(move(X, Y, Dir, NbCase)) -> randomMove(Player); !.

% choisi aleatoirement la direction (N, S, E, O) du mouvement. 
chooseDir(Dir):- DirNum is random(4), (DirNum = 0 -> Dir = 'N';DirNum = 1 -> Dir = 'S';DirNum = 2 -> Dir = 'E';DirNum = 3 -> Dir = 'O').

% determine la piece qui ce deplacera.
choosePiece(ListPiece, X, Y):-length(ListPiece, NbOfPieces), PieceNum is random(NbOfPieces), nth0(PieceNum, ListPiece, [X,Y|_]). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% IA - pseudo Aleatoire - Attaquant %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- permet la convergence des pions de l'attaquant vers le roi adverse. -- %

iaPhase2Agg:-currentPlayer(Player), Player = 'A', attackers(PieceList),
	      choosePiece(PieceList, X, Y), selectKing(Xroi,Yroi, 0),  
	      (chooseDirAtt(X,Y,Xroi,Yroi);iaPhase2Agg).

% retrouve les coordonnées X et Y du roi
selectKing(X,Y, Pos):-getPieceInDefenders(Pos,[Xpiece,Ypiece|_]), getCaseOnBoard(Xpiece,Ypiece, Elmt), (Elmt='_R_', X=Xpiece,Y=Ypiece, !);(NewPos is Pos+1, selectKing(X,Y, NewPos)).

% Permet de verifier dans quelle direction (horizontale ou verticale) faire le deplacement. priorité au mouvement ayant la plus grande amplitude.
chooseDirAtt(X,Y,Xroi,Yroi):-abs(Xroi, X,ResultX), abs(Yroi, Y,ResultY), ((ResultX<ResultY, (vMove(X,Y,Xroi,Yroi, ResultY);(!,fail)));(hMove(X,Y,Xroi,Yroi, ResultX);vMove(X,Y,Xroi,Yroi, ResultY))).

% mouvement de convergence vers la position du roi (horizontalement).
hMove(_,_,_,_, 0):-!, fail.
hMove(X,Y,Xroi,Yroi, NbCase):- (X<Xroi,((move(X,Y, 'E', NbCase), !);( NCase is NbCase-1,hMove(X,Y,Xroi,Yroi, NCase);(!,fail))));((move(X,Y, 'O', NbCase),!);( NCase is NbCase-1, hMove(X,Y,Xroi,Yroi, NCase);(!,fail))).

% mouvement de convergence vers la position du roi (verticalement).
vMove(_,_,_,_, 0):-!, fail.
vMove(X,Y,Xroi,Yroi, NbCase):- (Y<Yroi,((move(X,Y, 'S', NbCase), !);( NCase is NbCase-1,vMove(X,Y,Xroi,Yroi, NCase);(!,fail))));((move(X,Y, 'N', NbCase),!);(NCase is NbCase-1,vMove(X,Y,Xroi,Yroi, NCase);(!,fail))). 
