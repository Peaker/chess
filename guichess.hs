{-# LANGUAGE DeriveDataTypeable #-}

module Main
where
  import Data.Maybe
  import Data.Typeable(Typeable)
  import Data.IORef
  import Data.List
  import qualified Chess
  import qualified Graphics.UI.SDL as SDL
  import qualified MySDL
  import qualified Control.Monad as Monad
  import qualified Control.Exception as Exception
  import qualified GormanChessPieces

  data Quit = Quit
    deriving (Show, Typeable)
  instance Exception.Exception Quit where
      toException Quit = Exception.SomeException Quit
      fromException = undefined

  data BoardState = BoardState {
        bsBoard :: Chess.Board,
        bsLegalMoves :: [Chess.BoardChange],
        bsPrevBoardState :: Maybe BoardState
  }

  data GameState = GameState {
        gsBoardState :: BoardState,
        gsDraggingData :: Maybe (Chess.Pos, (Int, Int), Chess.Piece, [Chess.BoardChange]),
        gsCurrentIteration :: Int
  }

  gsBoard :: GameState -> Chess.Board
  gsBoard = bsBoard . gsBoardState

  handleDragStart :: Integral a => (a, a) -> IORef GameState -> IO ()
  handleDragStart screenPos gameState = do
    case posOfScreenPos screenPos of
      Nothing -> return ()
      Just (pos, hotSpot) -> do
        currentGameState <- readIORef gameState
        let board = gsBoard currentGameState
        case Chess.piecePlayerAt board pos of
          Nothing -> return ()
          Just (piece, _) -> do
            let legalMoves = Chess.legalMoves board pos
            if not $ null legalMoves
              then
                  writeIORef gameState currentGameState{gsDraggingData = Just (pos, hotSpot, piece, legalMoves)}
              else
                  return ()

  legalMovesTo :: [Chess.BoardChange] -> Chess.Pos -> [Chess.BoardChange]
  legalMovesTo legalMoves destPos = [(board, pieceMovements) |
                                     (board, pieceMovements) <- legalMoves,
                                     (_, to, _, _) <- pieceMovements,
                                     to == destPos]

  posOfDragPos :: (Int, Int) -> (Int, Int) -> Maybe (Chess.Pos, (Int, Int))
  posOfDragPos (x, y) (hx, hy) =
      let (width, height) = GormanChessPieces.size
      in posOfScreenPos (x-hx + width `div` 2, y-hy + height `div` 2)

  chooseBoardChange :: (Int, Int) -> (Int, Int) -> [Chess.BoardChange] -> Maybe Chess.BoardChange
  chooseBoardChange screenPos hotSpot legalMoves =
      case posOfDragPos screenPos hotSpot of
        Nothing -> Nothing
        Just (destPos, (hx, _)) ->
          let moves = legalMovesTo legalMoves destPos
              (width, _) = GormanChessPieces.size
          in case moves of
            [] -> Nothing
            _ -> Just (moves !! ((hx * length moves `div` width)))

  handleDragDrop :: (Int, Int) -> IORef GameState -> IO ()
  handleDragDrop screenPos gameState = do
    currentGameState <- readIORef gameState
    let draggingData = gsDraggingData currentGameState
        newGameState = currentGameState{gsDraggingData = Nothing}
    writeIORef gameState newGameState
    case draggingData of
      Nothing -> return ()
      Just (_, hotSpot, _, legalMoves) -> do
        case chooseBoardChange screenPos hotSpot legalMoves of
          Nothing -> return ()
          Just (newBoard, _) ->
              let newBoardState = BoardState { bsBoard = newBoard,
                                               bsLegalMoves = Chess.allLegalMoves newBoard,
                                               bsPrevBoardState = Just $ gsBoardState newGameState }
              in writeIORef gameState newGameState{gsBoardState = newBoardState}

  handleUndo :: IORef GameState -> IO ()
  handleUndo gameState = do
    currentGameState <- readIORef gameState
    case bsPrevBoardState $ gsBoardState currentGameState of
      Nothing -> return ()
      (Just prevBoardState) -> writeIORef gameState currentGameState{gsBoardState = prevBoardState}

  handleEvents :: [SDL.Event] -> IORef GameState -> IO ()
  handleEvents events gameState = do
    Monad.forM_ events $ \event ->
        case event of
          SDL.Quit -> (Exception.throw Quit)
          SDL.MouseButtonDown x y b ->
              if b == SDL.ButtonLeft
                then handleDragStart (x, y) gameState
                else return ()
          SDL.MouseButtonUp x y b ->
              if b == SDL.ButtonLeft
                then handleDragDrop (fromIntegral x, fromIntegral y) gameState
                else return ()
          SDL.KeyDown SDL.Keysym {SDL.symKey=symKey,
                                  SDL.symModifiers=symModifiers} ->
              case (symModifiers, symKey) of
                ([SDL.KeyModLeftCtrl], SDL.SDLK_z) -> handleUndo gameState
                ([SDL.KeyModRightCtrl], SDL.SDLK_z) -> handleUndo gameState
                _ -> return ()
          _ -> return ()

  isOddSquare :: Chess.Pos -> Bool
  isOddSquare (Chess.Pos (x, y)) = (x `mod` 2) /= (y `mod` 2)

  drawPiece :: SDL.Surface -> Chess.Player -> Chess.Piece -> SDL.Rect -> IO ()
  drawPiece display player piece rect = do
    image <- GormanChessPieces.loadImage piece player
    SDL.blitSurface image Nothing display $ Just rect
    return ()

  whiteColor, blackColor :: Bool -> MySDL.Color

  whiteColor True = (255, 200, 100)
  whiteColor False = (200, 150, 50)
  blackColor True = (70, 30, 00)
  blackColor False = (100, 50, 0)

  drawBackground :: SDL.Surface -> MySDL.Color -> SDL.Rect -> IO ()
  drawBackground display color rect = do
    pixel <- MySDL.sdlPixel color display
    SDL.fillRect display (Just rect) pixel
    return ()

  screenPosOfPos :: Chess.Pos -> (Int, Int)
  screenPosOfPos (Chess.Pos (x, y)) = let (width, height) = GormanChessPieces.size
                                      in (x*width, y*height)

  posOfScreenPos :: Integral a => (a, a) -> Maybe (Chess.Pos, (Int, Int))
  posOfScreenPos (x, y) = let (width, height) = GormanChessPieces.size
                              pos = Chess.Pos (fromIntegral $ x `div` width,
                                               fromIntegral $ y `div` height)
                              hotSpot = (fromIntegral $ x `mod` width,
                                         fromIntegral $ y `mod` height)
                          in if Chess.posInBoard pos then Just (pos, hotSpot)
                                                     else Nothing

  screenRectOfPos :: Chess.Pos -> SDL.Rect
  screenRectOfPos pos = let (x, y) = (screenPosOfPos pos)
                            (width, height) = GormanChessPieces.size
                        in (SDL.Rect x y width height)

  interestingSquares :: GameState -> [Chess.Pos]
  interestingSquares currentGameState =
      let (moves, component) =
              case (gsDraggingData currentGameState) of
                Nothing -> (bsLegalMoves $ gsBoardState currentGameState, \(from, _, _, _) -> from)
                (Just (_, _, _, legalMoves)) -> (legalMoves, \(_, to, _, _) -> to)
      in nub [component pieceMovement |
              (_, pieceMovements) <- moves,
              pieceMovement <- pieceMovements]

  drawChessBoard :: IORef GameState -> SDL.Surface -> IO ()
  drawChessBoard gameState display = do
    currentGameState <- readIORef gameState
    let board = gsBoard currentGameState
        squares = interestingSquares currentGameState
    (x, y, _) <- SDL.getMouseState
    Monad.forM_ Chess.allPos $ \pos -> do
      let color = (if isOddSquare pos then whiteColor else blackColor) (pos `elem` squares)
      drawBackground display color $ screenRectOfPos pos
      case Chess.piecePlayerAt board pos of
        Nothing -> return ()
        Just (piece, player) -> drawPiece display player piece $ screenRectOfPos pos
    case gsDraggingData currentGameState of
      Nothing -> return ()
      Just (_, (hx, hy), draggedPiece, legalMoves) -> do
        let piece = case chooseBoardChange (x, y) (hx, hy) legalMoves of
                      Nothing -> draggedPiece
                      Just (_, ((_, _, _, becomes):_)) -> becomes
                      Just (_, []) -> error "A move without piece movements is invalid"
        drawPiece display (Chess.bPlayer board) piece $ SDL.Rect (x-hx) (y-hy) 0 0
        return ()

  mainLoop :: SDL.Surface -> IO ()
  mainLoop display = do
    black <- MySDL.sdlPixel (0, 0, 0) display

    gameState <- newIORef GameState {gsBoardState = BoardState {
                                                      bsBoard = Chess.initialBoard,
                                                      bsLegalMoves = Chess.allLegalMoves Chess.initialBoard,
                                                      bsPrevBoardState = Nothing
                                                    },
                                     gsDraggingData = Nothing,
                                     gsCurrentIteration = 0}
    font <- MySDL.defaultFont 40

    flip Exception.catch (\Quit -> return ()) $ Monad.forever $ do
        SDL.fillRect display Nothing black
        drawChessBoard gameState display

        currentGameState <- readIORef gameState
        writeIORef gameState currentGameState{gsCurrentIteration = 1 + gsCurrentIteration currentGameState}
        let board = bsBoard $ gsBoardState currentGameState
            textColor = MySDL.sdlColor (255, 0, 0)
            text = if Chess.isTie board
                     then
                         "Draw!"
                     else
                         if Chess.isLose board
                           then
                               show (Chess.otherPlayer $ Chess.bPlayer board) ++ " wins!"
                           else
                               show (Chess.bPlayer board) ++ "'s turn"

        textSurface <- MySDL.renderTextSolid font text textColor
        SDL.blitSurface textSurface Nothing display $ Just (SDL.Rect 0 550 0 0)
        SDL.flip display
        events <- MySDL.getEvents
        handleEvents events gameState
        SDL.delay 20


  main :: IO ()
  main = MySDL.withSDL $ do
           display <- SDL.setVideoMode 800 600 16 [SDL.DoubleBuf]
           mainLoop display
           return ()
