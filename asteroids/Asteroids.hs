{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, until)
import qualified Prelude
import Control.Lens
import Control.Monad.Trans.State
import Control.Wire
import Data.Foldable (asum)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid)
import Linear.Matrix
import Linear.V2
import qualified Graphics.UI.SDL as SDL
import qualified Data.Set as Set

deriving instance Ord SDL.Keysym

--------------------------------------------------------------------------------
keyDown :: (Foldable f, Monoid e) => SDL.SDLKey -> Event e m (f SDL.Keysym)
keyDown k = when (keyDown' k)


--------------------------------------------------------------------------------
keyDown' :: Foldable f => SDL.SDLKey -> f SDL.Keysym -> Bool
keyDown' k = elemOf (folded . to SDL.symKey) k


--------------------------------------------------------------------------------
acceleration
  :: (Foldable f, Monad m, Monoid e)
  => Wire e m (f SDL.Keysym) (V2 Double)
acceleration = rotateVector <$> thrust <*> r

 where
  thrust = 10 . keyDown SDL.SDLK_UP <|> 0

  r = integral_ 0 . (asum [ 1 . keyDown SDL.SDLK_LEFT
                          , (-1) . keyDown SDL.SDLK_RIGHT
                          , 0
                          ])


rotateVector :: Double -> Double -> V2 Double
rotateVector mag r = v *! rotation
 where
  v = V2 0 (negate mag)
  rotation = V2 (V2 (cos r) (-(sin r))) (V2 (sin r) (cos r))

--------------------------------------------------------------------------------
velocity :: Wire e m (V2 Time) (V2 Time)
velocity = accumT (\dt a v -> min 50 $ a + v * pure dt) 0


--------------------------------------------------------------------------------
shipRadius :: Double
shipRadius = 50


--------------------------------------------------------------------------------
wrappedPosition :: V2 Double -> V2 Double -> Wire e m (V2 Double) (V2 Double)
wrappedPosition (V2 w h) = accumT wrap

 where

  wrap dt p v =
    let f b = Prelude.until (>= 0) (+ b) . Prelude.until (<= b) (\a -> a - b)
        (V2 x y) = p + (pure dt * v)
    in V2 (f (w + 50) (x + 50) - 50) (f (h + 50) (y + 50) - 50)

--------------------------------------------------------------------------------
data Frame = Frame { shipPosition :: V2 Double
                   , asteroidPosition :: V2 Double
                   }

--------------------------------------------------------------------------------
gameWire
  :: (Foldable f, RandomGen g)
  => V2 Double -> g
  -> Wire () IO (f SDL.Keysym) Frame
gameWire bounds@(V2 w h) g = proc keysDown -> do
  asteroid <- asteroid bounds g -< keysDown
  ship <- wrappedPosition bounds shipStart . velocity . acceleration -< keysDown

  returnA -< Frame { shipPosition = ship
                   , asteroidPosition = asteroid
                   }

 where shipStart = V2 (w / 2 - 25) (h / 2 - 25)


--------------------------------------------------------------------------------
asteroid :: RandomGen g => V2 Double -> g -> Wire e IO a (V2 Double)
asteroid bounds@(V2 w h) g =
  wrappedPosition bounds start . pure (rotateVector mag r)

 where

  (start, mag, r) = flip evalState g $ do
                      x <- state (randomR (0, w))
                      y <- state (randomR (0, h))
                      mag <- state (randomR (10, 40))
                      r <- state (randomR (0, 2 * pi))
                      return (V2 x y, mag, r)


--------------------------------------------------------------------------------
-- This shooting wire allows one shot per 1s. The user can only shoot after
-- 0.1s have elapsed, and they have released the spacebar during this cooldown.
isShooting :: (Foldable f, Monad m, Monoid e) => Event e m (f SDL.Keysym)
isShooting =
  asSoonAs (keyDown' SDL.SDLK_SPACE) >>> (once --> coolDown >>> isShooting)

 where

  coolDown =
    arr head .  multicast [ after 0.4, asSoonAs (not . keyDown' SDL.SDLK_SPACE) ]


--------------------------------------------------------------------------------
main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 640 480 32 [SDL.SWSurface]

  g <- getStdGen
  go screen (Set.empty) clockSession (gameWire (V2 640 480) g)

 where

  go screen keysDown s w = do
    keysDown' <- parseEvents keysDown
    (r, w', s') <- stepSession w s keysDown'

    case r of
      Right f -> do
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 0 0 >>=
            SDL.fillRect screen Nothing

        drawSquareAt screen (shipPosition f)
        drawSquareAt screen (asteroidPosition f)

        SDL.flip screen

        go screen keysDown' s' w'

      Left () -> return ()

  parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
      SDL.NoEvent -> return keysDown
      SDL.KeyDown k -> parseEvents (Set.insert k keysDown)
      SDL.KeyUp k -> parseEvents (Set.delete k keysDown)
      _ -> parseEvents keysDown

  drawSquareAt screen p =
    let (V2 x y) = p
    in (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
         SDL.fillRect screen (Just $ SDL.Rect (round x) (round y) 50 50)

