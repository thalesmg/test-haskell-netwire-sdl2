{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Prelude hiding ((.), id)

import qualified SDL as SDL
import qualified Control.Monad as CM (when, unless)
import Control.Concurrent (threadDelay)
import Control.Monad.Fix (MonadFix)
import Control.Wire
import FRP.Netwire
import qualified Data.Set as S
import GHC.Stack

type Vec = (Double, Double)

baseAcc = 400
dragConst = 0.33

acceleration :: (HasTime t s, Monad m, Monoid e) => Wire s e m (S.Set SDL.Keycode, Vec) Vec
acceleration = proc (keysDown, ~(vx, vy)) -> do
  let xSign = if | S.member SDL.KeycodeRight keysDown -> 1
                 | S.member SDL.KeycodeLeft keysDown -> - 1
                 | otherwise -> 0
  let ySign = if | S.member SDL.KeycodeUp keysDown -> - 1
                 | S.member SDL.KeycodeDown keysDown -> 1
                 | otherwise -> 0
  returnA -< (xSign * baseAcc - vx * dragConst, ySign * baseAcc - vy * dragConst)

reflectV :: HasTime t s => Wire s e m (Double, Bool) Double
reflectV = integralWith (\collided a -> if collided then (- a) else a) 0

velocity :: (HasTime t s, MonadFix m) => Wire s e m (Vec, (Bool, Bool)) Vec
velocity = proc ((aX, aY), (collidedX, collidedY)) -> do
  aX' <- reflectV -< (aX, collidedX)
  aY' <- reflectV -< (aY, collidedY)
  returnA -< (aX', aY')

position :: (HasTime t s, Monad m, Monoid e) => Wire s e m Vec (Vec, (Bool, Bool))
position = go (0, 0)
  where
    go (x, y) =
      mkPure $ \ds (vX, vY) ->
        let dt = realToFrac (dtime ds)
            x' = x + vX * dt
            y' = y + vY * dt
            collX = x < 0 || x > 800 - 50
            collY = y < 0 || y > 600 - 50
            x'' = if collX then max 1 (min (799 - 50) x') else x'
            y'' = if collY then max 1 (min (599 - 50) y') else y'
        in (Right ((x'', y''), (collX, collY)), go (x'', y''))

challenge3 :: (HasTime t s, MonadFix m) => Wire s () m (S.Set SDL.Keycode) Vec
challenge3 = proc keysDown -> do
  rec (accelX, accelY) <- acceleration -< (keysDown, (velX, velY))
      (pos, (collX, collY)) <- position -< (velX, velY)
      -- vel <- velocity -< (accel, coll)
      velX <- reflectV -< (accelX, collX)
      velY <- reflectV -< (accelY, collY)
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
  threadDelay (2 * 10^4)
  (ds, session') <- stepSession session
  events <- SDL.pollEvents
  let qPressed = S.member SDL.KeycodeQ keysDown'
      keysDown' = foldr handleKey keysDown events
  (pos, wire') <- stepWire wire ds (Right keysDown')
  let (xpos, ypos) = either (const (0, 0)) id pos
  -- let (xpos, ypos) = (50, 50)
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
  SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P (SDL.V2 (truncate (xpos :: Double)) (truncate (ypos :: Double)))) (SDL.V2 50 50))
  SDL.present renderer
  CM.unless qPressed (appLoop keysDown' session' wire' renderer)
  -- CM.unless qPressed (appLoop keysDown' session' wire renderer)

main :: HasCallStack => IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Tufe" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  appLoop S.empty clockSession_ challenge3 renderer
  -- appLoop S.empty clockSession_ undefined renderer
