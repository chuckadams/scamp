{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL.TTF as TTF
import qualified SDL as SDL
import SDL.Raw (Color(..))

import Control.Monad.Reader

import Linear.V2
import Linear.Affine (Point(..))

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

      runReaderT loop env

      TTF.closeFont font
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      SDL.quit

data SDLEnvironment = SDLEnv {
  envWindow   :: SDL.Window,
  envRenderer :: SDL.Renderer,
  envTexture  :: SDL.Texture
}

data GameState = GameState {
  pos :: (Int, Int)
}

loop :: ReaderT SDLEnvironment IO ()
loop = do
    let loc = SDL.Rectangle (P $ V2 320 240) (V2 50 50)
    env <- ask
    let renderer = envRenderer env
    let tex = envTexture env
    liftIO $ SDL.clear renderer
    liftIO $ SDL.copy renderer tex Nothing (Just loc)
    liftIO $  SDL.present renderer
    handleEvents 

handleEvents :: ReaderT SDLEnvironment IO ()
handleEvents = do
  mbEvent <- SDL.pollEvent
  case mbEvent of
    Just (SDL.Event _ SDL.QuitEvent) -> return ()
    _ -> loop
