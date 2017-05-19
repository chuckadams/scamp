{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL.TTF as TTF
import qualified SDL as SDL
import SDL.Raw (Color(..))

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
      loop window renderer textTexture

      TTF.closeFont font
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      SDL.quit

loop :: t -> SDL.Renderer -> SDL.Texture -> IO ()
loop window renderer textTexture = do
    let loc = SDL.Rectangle (P $ V2 320 240) (V2 50 50)
    SDL.clear renderer
    SDL.copy renderer textTexture Nothing (Just loc)
    SDL.present renderer
    handleEvents window renderer textTexture

handleEvents :: t -> SDL.Renderer -> SDL.Texture -> IO ()
handleEvents window renderer textTexture = do
  mbEvent <- SDL.pollEvent
  case mbEvent of
    Just (SDL.Event _ SDL.QuitEvent) -> return ()
    _ -> loop window renderer textTexture
