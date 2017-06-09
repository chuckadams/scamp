{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL.TTF as TTF
import qualified SDL as SDL
import SDL.Raw (Color(..))

import Control.Monad.RWS.Strict

import Linear.V2
import Linear.Affine (Point(..))

data SDLEnvironment = SDLEnv {
  envWindow   :: SDL.Window,
  envRenderer :: SDL.Renderer,
  envTexture  :: SDL.Texture
}

data GameState = GameState {
  pos :: (Int, Int)
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

      let state = GameState { pos = (50,50) }

      execRWST loop env state 

      TTF.closeFont font
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      SDL.quit


loop :: ScampState
loop = do
    env <- ask
    let loc = SDL.Rectangle (P $ V2 320 240) (V2 50 50)
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
    _ -> loop
