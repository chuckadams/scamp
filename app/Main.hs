{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL.TTF as TTF
import qualified SDL as SDL
import SDL.Raw (Color(..))

import Control.Monad.RWS.Strict

import Foreign.C.Types (CInt)

import Linear.V2
import Linear.Affine (Point(..))

data SDLEnvironment = SDLEnv {
  envWindow   :: SDL.Window,
  envRenderer :: SDL.Renderer,
  envTexture  :: SDL.Texture
}

data GameState = GameState {
  pos :: (CInt, CInt)
}

type ScampState = RWST SDLEnvironment () GameState IO ()

arial :: String
arial = "src/arial.ttf"

main :: IO ()
main = do
    _ <- SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Test" SDL.defaultWindow
    renderer <- SDL.createRenderer window 0 SDL.defaultRenderer

    TTF.withInit $ do
      inited <- TTF.wasInit
      if not inited then error "[Bug] Font system not initialized" else return ()
      font <- TTF.openFont arial 150
      textSurface <- TTF.renderUTF8Solid font "@" (Color 255 255 255 0)
      textTexture <- SDL.createTextureFromSurface renderer textSurface
      SDL.freeSurface textSurface

      let env = SDLEnv {
            envWindow = window,
            envRenderer = renderer,
            envTexture = textTexture
      } 

      let state = GameState { pos = (100,100) }

      execRWST loop env state 

      TTF.closeFont font
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      SDL.quit


loop :: ScampState
loop = do
    env <- ask
    (GameState (x,y)) <- get
    let loc = SDL.Rectangle (P $ V2 x y) (V2 50 50)
    let renderer = envRenderer env
    let tex = envTexture env
    liftIO $ do
      SDL.clear renderer
      SDL.copy renderer tex Nothing (Just loc)
      SDL.present renderer
    handleEvents 

handleEvents :: ScampState
handleEvents = do
  mbEvent <- SDL.pollEvent
  case mbEvent of
    Just (SDL.Event _ SDL.QuitEvent) -> return ()
    Just (SDL.Event _ (SDL.KeyboardEvent dat)) -> handleKeyEvent dat
    _ -> loop

handleKeyEvent :: SDL.KeyboardEventData -> ScampState
handleKeyEvent ev =
  do
    let sym = SDL.keyboardEventKeysym ev
        code = SDL.keysymKeycode sym
    state@(GameState (x,y)) <- get
    case code of
      SDL.KeycodeUp -> put (GameState (x, y - 10))
      SDL.KeycodeDown -> put (GameState (x, y + 10))
      SDL.KeycodeLeft -> put (GameState (x - 10, y))
      SDL.KeycodeRight -> put (GameState (x + 10, y))
      _ -> return ()
    loop

