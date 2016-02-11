{-# LANGUAGE OverloadedStrings #-}

module Main where

import Linear (V4(..), V2(..))
import Control.Monad (unless)
import SDL
import Linear.Affine 
import Foreign.C.Types
p1 :: Point V2 Foreign.C.Types.CInt
p1 = P (V2 10 10)
rect :: Rectangle CInt
rect = Rectangle p1 (V2 20 20)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createTexture renderer 
                           RGB24 
                           TextureAccessTarget 
                           (V2 255 255)
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  copy renderer texture (Just rect) (Just rect)
  present renderer
  appLoop renderer texture

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

