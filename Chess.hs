module Chess where

  -- TODO: AI!

  import qualified MapManyToOne
  import MapManyToOne((!))
  import qualified Data.Map as Map
  import Data.Maybe
 

  newtype Pos = Pos (Int, Int)
      deriving (Eq, Ord)

  type Move = Pos -> Pos

  data Player = White | Black
              deriving (Show, Eq, Ord, Enum, Bounded)
  data Piece = King | Queen | Rook | Knight | Bishop | Pawn
               deriving (Eq, Ord, Show)

  otherPlayer :: Player -> Player
  otherPlayer White = Black
  otherPlayer Black = White
                     
  type Army = MapManyToOne.MapManyToOne Pos Piece
  data MoveHistory = MoveHistory {kingMoved :: Bool,
                                  leftRookMoved :: Bool,
                                  rightRookMoved :: Bool}
                 deriving Show

  data TieState = TieState {tsDullCounter :: Int,
                            tsSeen :: Map.Map (Army, Army) Int,
                            tsMaxSeen :: Int}

  data Board = Board {bMyArmy :: Army,
                      bHisArmy :: Army,
                      bPlayer :: Player,
                      bHisJumpedPawns :: Army,
                      bMyMoveHistory :: MoveHistory,
                      bHisMoveHistory :: MoveHistory,
                      bTieState :: TieState}

  type PieceMovement = (Pos, Pos, Maybe Pos, Piece) -- from, to, eaten, becomes
  type BoardChange = (Board, [PieceMovement])

  emptyTieState :: (Army, Army) -> TieState
  emptyTieState boardState =
      TieState { tsDullCounter = 0,
                 tsSeen = Map.singleton boardState 1,
                 tsMaxSeen = 1 }
      
  newTieState :: TieState -> (Army, Army) -> Bool -> TieState
  newTieState tieState boardState isDull =
      if not isDull
        then
            emptyTieState boardState
        else
            TieState { tsDullCounter = 1 + tsDullCounter tieState,
                       tsSeen = newSeen,
                       tsMaxSeen = max (tsMaxSeen tieState) (newSeen Map.! boardState) }
             where
               newSeen = Map.alter updateEntry boardState $ tsSeen tieState
               updateEntry Nothing = Just 1
               updateEntry (Just x) = Just (x+1)
                                      
  upDir :: Board -> Int
  upDir board = case (bPlayer board) of
                  White -> -1
                  Black -> 1

  emptyMoveHistory :: MoveHistory
  emptyMoveHistory = MoveHistory {kingMoved=False,
                                  leftRookMoved=False,
                                  rightRookMoved=False}

  inRange :: Ord a => a -> a -> a -> Bool
  inRange low high value = low <= value && value < high

  pawnJumpAllowed, pawnReachedEnd :: Board -> Pos -> Bool
  pawnJumpAllowed board (Pos (_, y)) =
      case bPlayer board of
        Black -> (1 == y)
        White -> (6 == y)
  pawnReachedEnd board (Pos (_, y)) =
      case bPlayer board of
        Black -> (7 == y)
        White -> (0 == y)

  posInBoard :: Pos -> Bool
  posInBoard (Pos (x, y)) = (inRange 0 8 x &&
                             inRange 0 8 y)

  orMoveHistories :: MoveHistory -> MoveHistory -> MoveHistory
  orMoveHistories a b =
      MoveHistory {kingMoved=(kingMoved a) || (kingMoved b),
                   leftRookMoved=(leftRookMoved a) || (leftRookMoved b),
                   rightRookMoved=(rightRookMoved a) || (rightRookMoved b)}

  withMoves :: [PieceMovement] -> Army -> Army -> (Army, Army)
  withMoves [] myArmy hisArmy = (myArmy, hisArmy)
  withMoves ((from, to, eaten, becomes):rest) myArmy hisArmy =
      withMoves rest newMyArmy (newHisArmy eaten)
      where newMyArmy = MapManyToOne.insert to becomes (MapManyToOne.delete from myArmy)
            newHisArmy (Just eatenPos) = MapManyToOne.delete eatenPos hisArmy
            newHisArmy Nothing = hisArmy

  makeMove2 :: Board -> MoveHistory -> Army -> [PieceMovement] -> Bool -> BoardChange
  makeMove2 board historyChanges jumpedPawns pieceMovements isDull =
      (Board {bMyArmy = newHisArmy,
              bHisArmy = newMyArmy,
              bPlayer = otherPlayer $ bPlayer board,
              bHisJumpedPawns = jumpedPawns,
              bMyMoveHistory = bHisMoveHistory board,
              bHisMoveHistory = orMoveHistories historyChanges (bMyMoveHistory board),
              bTieState = newTieState (bTieState board) boardState isDull},
       pieceMovements)
      where (newMyArmy, newHisArmy) = withMoves pieceMovements (bMyArmy board) (bHisArmy board)
            boardState = case bPlayer board of
                           White -> (newHisArmy, newMyArmy)
                           Black -> (newMyArmy, newHisArmy)

  makeMove0 :: Board -> [PieceMovement] -> Bool -> BoardChange
  makeMove0 board = makeMove2 board emptyMoveHistory (MapManyToOne.fromList [])

  makeMove1 :: Board -> MoveHistory -> [PieceMovement] -> Bool -> BoardChange
  makeMove1 board moveHistory = makeMove2 board moveHistory (MapManyToOne.fromList [])

  -- Useful to look at the board from the enemy's perspective
  reverseBoard :: Board -> Board
  reverseBoard board = newBoard
      where (newBoard, _) = makeMove0 board [] False

  longMovesToward :: Piece -> Pos -> Pos -> Board -> MoveHistory -> Move -> [BoardChange]
  longMovesToward piece origPos pos board historyChanges direction
      | not (posInBoard nextPos) = []
      | MapManyToOne.member nextPos (bMyArmy board) = []
      | MapManyToOne.member nextPos (bHisArmy board) = [nextMove (Just nextPos) False]
      | otherwise = nextMove Nothing True : longMovesToward piece origPos nextPos board historyChanges direction
      where nextPos = (direction pos)
            nextMove eaten = (makeMove1 board historyChanges [(origPos, nextPos, eaten, piece)])

  longMoves :: Piece -> [Move] -> Pos -> Board -> MoveHistory -> [BoardChange]
  longMoves piece directions pos board historyChanges = concatMap (longMovesToward piece pos pos board historyChanges) directions


  notMyPiece :: Board -> Pos -> Bool
  notMyPiece board pos = (posInBoard pos && (not $ MapManyToOne.member pos $ bMyArmy board))

  nobodyIn :: Board -> Pos -> Bool
  nobodyIn board pos = (posInBoard pos && and [(not $ MapManyToOne.member pos $ army board) |
                                               army <- [bMyArmy, bHisArmy]])

  shortMoves :: Piece -> [Move] -> Pos -> Board -> MoveHistory -> [BoardChange]
  shortMoves piece moves pos board historyChanges =
      [let (eatPos, isDull) = if MapManyToOne.member nextPos (bHisArmy board)
                                  then (Just nextPos, False)
                                  else (Nothing, True)
       in makeMove1 board historyChanges [(pos, nextPos, eatPos, piece)] isDull
       | nextPos <- filter (notMyPiece board) $ map ($pos) moves]

  move :: (Int, Int) -> Move
  move (deltax, deltay) (Pos (x, y)) = Pos (x+deltax, y+deltay)
  allDirections, diagonals, straight, knightwise :: [Move]
  allDirections = map move [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x/=0) || (y/=0)]
  diagonals = map move [(x, y) | x <- [-1, 1], y <- [-1, 1]]
  straight = map move [(-1, 0), ( 1, 0),
                       ( 0,-1), ( 0, 1)]
  knightwise = map move [(func1 a, func2 b) |
                         func1 <- [id, negate],
                         func2 <- [id, negate],
                         (a, b) <- [(1, 2), (2, 1)]]

  allPos :: [Pos]
  allPos = [Pos (x, y) | x <- [0..7], y <- [0..7]]

  piecePlayerAt :: Board -> Pos -> Maybe (Piece, Player)
  piecePlayerAt board pos =
      case ((MapManyToOne.member pos (bMyArmy board)),
            (MapManyToOne.member pos (bHisArmy board))) of
        (True, True) -> error "Invalid board state"
        (True, False) -> Just (bMyArmy board ! pos, bPlayer board)
        (False, True) -> Just (bHisArmy board ! pos, otherPlayer $ bPlayer board)
        (False, False) -> Nothing

  myArmyInCheck :: Board -> Bool
  myArmyInCheck board = isThreatened board $ MapManyToOne.uniqueKeyFor King (bMyArmy board)

  isThreatened :: Board -> Pos -> Bool
  isThreatened board pos = elem pos [to |
                                     (_, pieceMovements) <- allAttackMoves (reverseBoard board),
                                     (_, to, _, _) <- pieceMovements]

  isLegalMove :: BoardChange -> Bool
  isLegalMove (board, _) = not $ myArmyInCheck $ reverseBoard board

  filterLegalMoves :: Board -> [BoardChange] -> [BoardChange]
  filterLegalMoves board moves =
      if isTie board
        then []
        else filter isLegalMove moves

  legalMoves, movesFrom :: Board -> Pos -> [BoardChange]
  legalMoves board pos = filterLegalMoves board $ movesFrom board pos

  movesFrom board pos = if pos `MapManyToOne.member` bMyArmy board
                        then
                            pieceMoves board pos ((bMyArmy board) ! pos)
                        else
                            []

  attackMoves :: Board -> Pos -> Piece -> [BoardChange]
  attackMoves board pos Queen = longMoves Queen allDirections pos board emptyMoveHistory

  attackMoves board pos@(Pos (x, _)) Rook = longMoves Rook straight pos board $ case x of
                                                                                0 -> emptyMoveHistory{leftRookMoved=True}
                                                                                7 -> emptyMoveHistory{rightRookMoved=True}
                                                                                _ -> emptyMoveHistory

  attackMoves board pos Bishop = longMoves Bishop diagonals pos board emptyMoveHistory

  attackMoves board pos King = shortMoves King allDirections pos board newHistory
          where newHistory = emptyMoveHistory{kingMoved=True}

  attackMoves board pos Knight = shortMoves Knight knightwise pos board emptyMoveHistory

  attackMoves board pos Pawn =
      (if nobodyIn board up
       then [(makeMove0 board [(pos, up, Nothing, x)] False) | x <- becomes]
       else []) ++

      (if pawnJumpAllowed board pos &&
          nobodyIn board up &&
          nobodyIn board jump
       then [makeMove2 board emptyMoveHistory (MapManyToOne.fromList [(jump, Pawn)]) [(pos, jump, Nothing, Pawn)] False]
       else []) ++

      [makeMove0 board [(pos, eatPos, Just eatPos, x)] False |
       (eatPos, _) <- diagonalMoves,
       MapManyToOne.member eatPos (bHisArmy board),
       x <- becomes] ++

      [makeMove0 board [(pos, eatPos, Just jumpPos, Pawn)] False |
       (eatPos, jumpPos) <- diagonalMoves,
       -- If he just jumped, then nobody can
       -- possibly be in the eat pos (in which
       -- case we would eat 2 at one move!)
       MapManyToOne.member jumpPos (bHisJumpedPawns board)]
      where up = move (0, upDir board) pos
            jump = move (0, 2 * upDir board) pos
            diagonalMoves = [(move (x, upDir board) pos,
                              move (x, 0) pos)            | x <- [-1, 1]]
            becomes = if (pawnReachedEnd board up)
                      then [Queen, Rook, Bishop, Knight]
                      else [Pawn]

  castling :: Pos -> Board -> (MoveHistory -> Bool) -> [Int] -> [Int] -> [BoardChange] -> [BoardChange]
  castling kingPos board movePredicate freeSpots unthreatenedSpots castlingMoves
      | castlingAllowed = castlingMoves
      | otherwise       = []
      where history = bMyMoveHistory board
            castlingAllowed = not (movePredicate history || kingMoved history) &&
                              and [nobodyIn board $ move (i, 0) kingPos | i <- freeSpots] &&
                              and [not $ isThreatened board $ (move (i, 0) kingPos) | i <- unthreatenedSpots]

  pieceMoves :: Board -> Pos -> Piece -> [BoardChange]
  pieceMoves board pos King = attackMoves board pos King ++
                              -- Rook allowed to be threatened before castling
                              (castling pos board leftRookMoved [-3, -2, -1] [-3..0]
                                            [(afterMove [(pos, move (-2, 0) pos, Nothing, King),
                                                         (move (-4, 0) pos, move (-1, 0) pos, Nothing, Rook)] True)] ++
                               castling pos board rightRookMoved [1, 2] [0..2]
                                            [(afterMove [(pos, move (2, 0) pos, Nothing, King),
                                                         (move (3, 0) pos, move ( 1, 0) pos, Nothing, Rook)] True)])
      where newHistory = emptyMoveHistory{kingMoved=True}
            afterMove = makeMove1 board newHistory
  pieceMoves board pos other = attackMoves board pos other

  allMovesBy :: (Board -> Pos -> Piece -> [BoardChange]) -> Board -> [BoardChange]
  allMovesBy func board = concatMap (\pos -> (func board pos ((bMyArmy board)!pos))) (MapManyToOne.keys $ bMyArmy board)

  allAttackMoves, allLegalMoves :: Board -> [BoardChange]
  allAttackMoves = allMovesBy attackMoves
  allLegalMoves board = filterLegalMoves board $ allMovesBy pieceMoves board

  _allLegalMoves :: Board -> [BoardChange]
  _allLegalMoves board = filter isLegalMove $ allMovesBy pieceMoves board

  isLose :: Board -> Bool
  isLose board = (null $ _allLegalMoves board) && myArmyInCheck board

  isTie :: Board -> Bool
  isTie board = ((null $ _allLegalMoves board) && (not $ myArmyInCheck board)) ||
                ((tsDullCounter $ bTieState board) >= 50) ||
                ((tsMaxSeen $ bTieState board) >= 3)

  initialArmy :: (Int->Int) -> MapManyToOne.MapManyToOne Pos Piece
  initialArmy fixY = MapManyToOne.fromList
                     ([(Pos (i, pawnline), Pawn) | i <- [0..7]] ++
                      [(Pos (i, mainline), Rook) | i <- [0, 7]] ++
                      [(Pos (i, mainline), Knight) | i <- [1, 6]] ++
                      [(Pos (i, mainline), Bishop) | i <- [2, 5]] ++
                      [(Pos (3, mainline), Queen),
                       (Pos (4, mainline), King)])
      where pawnline = fixY 1
            mainline = fixY 0

  initialBoard :: Board
  initialBoard = Board {
                   bMyArmy = whiteArmy,
                   bHisArmy = blackArmy,
                   bPlayer = White,
                   bHisJumpedPawns = MapManyToOne.fromList [],
                   bMyMoveHistory = emptyMoveHistory,
                   bHisMoveHistory = emptyMoveHistory,
                   bTieState = emptyTieState (blackArmy, whiteArmy)
                 }
      where
        whiteArmy = initialArmy (7-)
        blackArmy = initialArmy id