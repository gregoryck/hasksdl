{-# LANGUAGE OverloadedStrings #-}

module Main where

import Linear (V4(..), V2(..))
{-import Control.Monad (unless)-}
import SDL
-- import Linear.Affine 
import Foreign.C.Types

p1 :: Point V2 Foreign.C.Types.CInt
p1 = P (V2 291 97)

type Location = Rectangle CInt

rect :: Location
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
  spriteSheet <- loadTexture renderer "npcs.bmp"
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  appLoop renderer spriteSheet [girl]

render :: Renderer -> SizedTex -> Character -> IO ()
render renderer spriteSheet (Character spriteSheetLoc gameLoc) = do
  copy renderer (unsized spriteSheet) (Just spriteSheetLoc) (Just gameLoc)
  present renderer
  

data Action = Quit | Up | Down | MoveLeft | MoveRight


pressedMotion :: KeyboardEventData -> Maybe Action
pressedMotion keyboardEvent =
      case keyboardEventKeyMotion keyboardEvent of 
            Pressed -> case keysymKeycode (keyboardEventKeysym keyboardEvent) of
                             KeycodeQ -> Just Quit
                             KeycodeW -> Just Up
                             KeycodeS -> Just Down
                             KeycodeA -> Just MoveLeft 
                             KeycodeD -> Just MoveRight
                             _        -> Nothing
            _       -> Nothing

keyboardPayload :: EventPayload -> Maybe KeyboardEventData
keyboardPayload (KeyboardEvent keyboardEvent) = Just keyboardEvent
keyboardPayload _                             = Nothing

actionPressed :: Event -> Maybe Action
actionPressed event = do
   keyboardEvent <- keyboardPayload $ eventPayload event
   pressedMotion keyboardEvent
   

data Character = Character Location Location
girl :: Character
girl = Character rect rect

moveRight :: Character -> Character
moveRight (Character sourceLoc (
          Rectangle (P (V2 x1 y1)) (V2 x2 y2))) = 
            Character sourceLoc $ Rectangle (P (V2 (x1+10) y1)) (V2 x2 y2)

appLoop :: Renderer -> SizedTex -> [Character] -> IO ()
appLoop renderer spriteSheet [girl'] = do
  render renderer spriteSheet girl'
  event <- waitEvent
  let action = actionPressed event
  case action of
      Just Quit      -> return ()
      Just MoveRight -> appLoop renderer spriteSheet [moveRight girl']
      _              -> appLoop renderer spriteSheet [girl']

appLoop renderer spriteSheet [] = do
  event <- waitEvent
  let action = actionPressed event
  case action of
      Just Quit      -> return ()
      _              -> appLoop renderer spriteSheet []


appLoop renderer spriteSheet (girl':_unImplementedCharacters) = 
  appLoop renderer spriteSheet [girl']




