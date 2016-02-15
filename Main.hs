{-# LANGUAGE OverloadedStrings #-}

module Main where

import Linear (V4(..), V2(..))
import Control.Monad (unless)
import SDL
import Linear.Affine 
import Foreign.C.Types


p1 :: Point V2 Foreign.C.Types.CInt
p1 = P (V2 291 97)
rect :: Rectangle CInt
rect = Rectangle p1 (V2 26 46)

data SizedTex = SizedTex SDL.Texture (V2 CInt)

loadTexture :: SDL.Renderer -> FilePath -> IO SizedTex
loadTexture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 255 255 255 255
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return $ SizedTex t size

unsized :: SizedTex -> SDL.Texture
unsized (SizedTex texture _) = texture

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- loadTexture renderer "npcs.bmp"
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  copy renderer (unsized texture) (Just rect) (Just rect)
  present renderer
  appLoop renderer (unsized texture)

data Action = Quit | Up | Down | MoveLeft | MoveRight

buttonPressed :: Event -> Maybe Action
buttonPressed event =
-- TODO do notation
        case eventPayload event of
            KeyboardEvent keyboardEvent ->
                case keyboardEventKeyMotion keyboardEvent of 
                    Pressed ->
                         case keysymKeycode (keyboardEventKeysym keyboardEvent) of
                             KeycodeQ -> Just Quit
                             KeycodeW -> Just Up
                             KeycodeS -> Just Down
                             KeycodeA -> Just MoveLeft 
                             KeycodeD -> Just MoveRight
                             _        -> Nothing

                    _ -> Nothing
            _ -> Nothing



appLoop :: Renderer -> Texture -> IO ()
appLoop renderer texture = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = not (null (filter eventIsQPress events))
  unless qPressed (appLoop renderer texture)

