{-# OPTIONS -Wall -O2 #-}

module MySDL where

import TakeWhile(takeWhileM)
import Data.Word(Word8)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified Control.Exception as E

renderTextSolid :: TTF.Font -> String -> SDL.Color -> IO SDL.Surface
renderTextSolid = TTF.renderTextSolid

defaultFont :: Int -> IO TTF.Font
defaultFont = TTF.openFont "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

withSDL :: IO () -> IO ()
withSDL code = SDL.withInit [SDL.InitEverything] (E.bracket TTF.init (const TTF.quit) (const code))

type Color = (Word8, Word8, Word8)
sdlColor :: Color -> SDL.Color
sdlColor (r, g, b) = (SDL.Color r g b)
sdlPixel :: Color -> SDL.Surface -> IO SDL.Pixel
sdlPixel (r, g, b) surface = SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b

getEvents :: IO [SDL.Event]
getEvents = takeWhileM (return . (/=SDL.NoEvent)) SDL.pollEvent

surfaceGetSize :: SDL.Surface -> (Int, Int)
surfaceGetSize surface = (fromIntegral (SDL.surfaceGetWidth surface),
                          fromIntegral (SDL.surfaceGetHeight surface))
