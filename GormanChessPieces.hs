module GormanChessPieces
where

  import qualified Graphics.UI.SDL.Image as Image
  import qualified Graphics.UI.SDL as SDL
  import System.FilePath
  import Chess

  graphicsPath :: FilePath -> FilePath
  graphicsPath x = joinPath $ ["gorman_chess_pieces", x]
  -- Size of the MEDIUM pieces
  size :: Integral a => (a, a)
  size = (60, 60)

  playerName :: Player -> String
  playerName White = "w"
  playerName Black = "b"

  pieceName :: Piece -> String
  pieceName King = "k"
  pieceName Queen = "q"
  pieceName Rook = "r"
  pieceName Knight = "n"
  pieceName Bishop = "b"
  pieceName Pawn = "p"

  squareImageName :: Piece -> Player -> String
  squareImageName piece player = playerName player ++ pieceName piece

  loadImage :: Piece -> Player -> IO SDL.Surface
  loadImage piece player = Image.load $ graphicsPath $ (squareImageName piece player) ++ ".png"
