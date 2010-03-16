module Main
where

  import Data.Maybe
  import Control.Monad(forM_)
  import Data.Char(digitToInt, ord, chr, toUpper, toLower)
  import qualified Chess

  type BoardChangeValue = Int
      
  instance Show Chess.Pos where
            show (Chess.Pos (x, y)) = chr (ord 'A' + x) : show y
  instance Read Chess.Pos where
            readsPrec _ (x:y:str) = [(Chess.Pos (ord x - ord 'A', digitToInt y), str)]

  pieceLetter :: Chess.Piece -> Char
  pieceLetter Chess.King = 'k'
  pieceLetter Chess.Queen = 'q'
  pieceLetter Chess.Rook = 'r'
  pieceLetter Chess.Knight = 'h'
  pieceLetter Chess.Bishop = 'b'
  pieceLetter Chess.Pawn = 'p'

  prettyPrint :: Chess.Board -> IO ()
  prettyPrint board = do
    putStrLn "  ABCDEFGH"
    forM_ [0..7] $ \y -> do
      putStr $ (show y) ++ " "
      forM_ [0..7] $ \x ->
          putStr $ [case Chess.piecePlayerAt board $ Chess.Pos (x, y) of
            Nothing -> '-'
            Just (piece, owner) ->
                (if owner == (Chess.bPlayer board) then toUpper else toLower) $ pieceLetter piece
          ]
      putStr $ " " ++ (show y)
      putStr "\n"
    putStrLn "  ABCDEFGH"

  computeBestMove :: Board -> BoardChange
  computeBestMove board =
      getBestBoardChange minBound maxBound (Chess.allLegalMoves board) 3

  getBestBoardChange :: BoardChangeValue -> BoardChangeValue -> Int -> [BoardChange] -> BoardChange
  getBestBoardChange tooBad tooGood depthLeft ~((board, _):rest) =
      if Chess.isLose board then 
       boardChange@(board, _)

  main = do
    let (board, _) = computeBestMove Chess.initialBoard
    prettyPrint $ board
