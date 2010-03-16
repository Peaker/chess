module Main
where

  import Chess
  import Control.Monad(forM_)
  import Data.Char(toUpper, toLower, ord, chr, digitToInt)
  import MapManyToOne((!))
  import qualified MapManyToOne

  instance Show Pos where
            show (Pos (x, y)) = chr (ord 'A' + x) : show y
  instance Read Pos where
            readsPrec _ (x:y:str) = [(Pos (ord x - ord 'A', digitToInt y), str)]

  pieceLetter :: Piece -> Char
  pieceLetter King = 'k'
  pieceLetter Queen = 'q'
  pieceLetter Rook = 'r'
  pieceLetter Knight = 'h'
  pieceLetter Bishop = 'b'
  pieceLetter Pawn = 'p'

  prettyPrint :: Board -> IO ()
  prettyPrint board = do
    putStrLn "  ABCDEFGH"
    forM_ [0..7] $ \y -> do
      putStr $ (show y) ++ " "
      forM_ [0..7] $ \x ->
          putStr $ [case piecePlayerAt board $ Pos (x, y) of
            Nothing -> '-'
            Just (piece, owner) ->
                (if owner == (bPlayer board) then toUpper else toLower) $ pieceLetter piece
          ]
      putStr $ " " ++ (show y)
      putStr "\n"
    putStrLn "  ABCDEFGH"

  menu :: Pos -> [BoardChange] -> IO Board
  menu orig [] = do
    ioError $ userError "Can't move that piece!"
  menu orig options = do
    putStrLn "Your options are:"
    forM_ (zip [0..] options) $ \(index, (board, pieceMovements)) ->
        do
          putStr $ show index ++ ": "
          forM_ pieceMovements $ \(from, to, eaten, becomes) -> do
              if orig /= from
                then
                  putStr $ show from
                else
                  return ()
              putStr $ show to ++ "(" ++ show becomes ++ ")"
              putStr $ case eaten of
                Nothing -> ""
                Just pos -> "*"
                ++ "\n  "
          putStrLn ""
    x <- readLn
    if inRange 0 (length options) x then let (board, pieceMovements) = options!!x in return board else menu orig options

  play :: Board -> IO ()
  play board = do
        prettyPrint board
        putStr "My army in check "
        print $ myArmyInCheck board
        putStr "Number of moves possible: "
        let legalMoveCount = sum [(length $ legalMoves board pos) |
                                  (pos, piece) <- MapManyToOne.toList $ bMyArmy board]
        if legalMoveCount == 0
          then do
            if myArmyInCheck board
              then
                print $ ("Game lost by", (upDir board))
              else
                print $ "Tie!"
          else do
            print $ legalMoveCount
            putStrLn "Enter a position"
            let code = do
                  pos <- readLn
                  if not $ MapManyToOne.member pos (bMyArmy board)
                    then do
                      print "Invalid position"
                      play board
                    else do
                      newBoard <- menu pos $ legalMoves board pos
                      play newBoard
            let errHandler err = do
                               print err
                               play board
            catch code errHandler
  main = play initialBoard
