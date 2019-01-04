{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding ((.), id)

import qualified SDL as SDL
import qualified Control.Monad as CM (when)
import Control.Monad.Fix (MonadFix)
import Control.Wire
import FRP.Netwire
import qualified Data.Set as S

acceleration :: (HasTime t s, Monad m, Monoid e) => Wire s e m (S.Set SDL.Keycode) Double
acceleration =  pure 50 . when (S.member SDL.KeycodeRight)
            <|> pure (-50) . when (S.member SDL.KeycodeLeft)
            <|> pure 0

velocity :: HasTime t s => Wire s e m (Double, Bool) Double
velocity = integralWith (\collided a -> if collided then (- a) else a) 0

position :: (HasTime t s, Monad m, Monoid e) => Wire s e m Double (Double, Bool)
position = go 0
  where
    go x =
      mkPure $ \ds v ->
        let dt = realToFrac (dtime ds)
            x' = x + v * dt
            coll = x < 0 || x > 800 - 50
            x'' = if coll then max 1 (min (799 - 50) x') else x'
        in (Right (x'', coll), go x'')

challenge3 :: (HasTime t s, MonadFix m) => Wire s () m (S.Set SDL.Keycode) Double
challenge3 = proc keysDown -> do
  accel <- acceleration -< keysDown
  rec (pos, coll) <- position -< vel
      vel <- velocity -< (accel, coll)
  returnA -< pos

handleKey event ks =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      case SDL.keyboardEventKeyMotion keyboardEvent of
        SDL.Pressed ->
          S.insert (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)) ks
        SDL.Released ->
          S.delete (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)) ks
    _ -> ks

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
  appLoop S.empty clockSession_ challenge3 renderer
