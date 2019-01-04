{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding ((.), id)

import qualified SDL as SDL
import Control.Monad as CM (when)
import Control.Wire
import FRP.Netwire

challenge1 :: (HasTime t s, Monad m) => Wire s e m a Double
challenge1 = integral 20 . pure 50

appLoop :: HasTime t s => Session IO s -> Wire s Double IO () Double -> SDL.Renderer -> IO ()
appLoop session wire renderer = do
  (ds, session') <- stepSession session
  (x, wire') <- stepWire wire ds (Right ())
  let xpos = either id id x
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
  SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P (SDL.V2 (truncate xpos) 50)) (SDL.V2 50 50))
  SDL.present renderer
  CM.when (not qPressed) (appLoop session' wire' renderer)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Tufe" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  appLoop clockSession_ challenge1 renderer
