{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding ((.), id)

import qualified SDL as SDL
import qualified Control.Monad as CM (when)
import Control.Wire
import FRP.Netwire
import qualified Data.Set as S

challenge2 :: (HasTime t s, Monad m) => Wire s () m (S.Set SDL.Keycode) Double
challenge2 = integral 20 . velocity

velocity :: (Monad m, Monoid e) => Wire s e m (S.Set SDL.Keycode) Double
velocity =  pure 50 . when (S.member SDL.KeycodeRight)
        <|> pure (-50) . when (S.member SDL.KeycodeLeft)
        <|> pure 0

handleKey event ks =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      case SDL.keyboardEventKeyMotion keyboardEvent of
        SDL.Pressed ->
          S.insert (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)) ks
        SDL.Released ->
          S.delete (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)) ks
    _ -> ks

-- appLoop :: (HasTime t s) => S.Set SDL.Keycode -> Session IO s -> Wire s e IO (S.Set SDL.Keycode) Double -> SDL.Renderer -> IO ()
appLoop keysDown session wire renderer = do
  (ds, session') <- stepSession session
  events <- SDL.pollEvents
  let qPressed = S.member SDL.KeycodeQ keysDown'
      keysDown' = foldr handleKey keysDown events
  (x, wire') <- stepWire wire ds (Right keysDown')
  let xpos = either (const 0) id x
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
  SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P (SDL.V2 (truncate (xpos :: Double)) 50)) (SDL.V2 50 50))
  SDL.present renderer
  CM.when (not qPressed) (appLoop keysDown' session' wire' renderer)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Tufe" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  appLoop S.empty clockSession_ challenge2 renderer
