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
import Linear hiding (rotate)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
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
  => Wire e m (f SDL.Keysym, M22 Double) (V2 Double)
acceleration = uncurry (*!) <$> (thrust *** id)

 where

  thrust = ((V2 0 1) ^*) <$> (150 . keyDown SDL.SDLK_UP <|> 0)


--------------------------------------------------------------------------------
rotate
  :: (Foldable f, Monad m, Monoid e)
  => Wire e m (f SDL.Keysym) (M22 Double)
rotate = rotationMatrix . (+ pi) <$>
  (integral_ 0 .  (asum [ pi . keyDown SDL.SDLK_LEFT
                        , (negate pi) . keyDown SDL.SDLK_RIGHT
                        , 0
                        ]))

  where

    rotationMatrix r = V2 (V2 (cos r) (-(sin r)))
                          (V2 (sin r) (  cos r) )

--------------------------------------------------------------------------------
velocity :: Wire e m (V2 Time) (V2 Time)
velocity = accumT (\dt v a -> let v' = v + a ^* dt
                              in normalize v' ^* (min 100 (norm v'))) 0


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
data Frame = Frame { frameShip :: !Object
                   --, asteroid :: !Object
                   }

data Object = Object { objPos :: !(V2 Double), objRotation :: !(M22 Double) }


--------------------------------------------------------------------------------
ship :: (Foldable f, Monoid e, Monad m) => V2 Double -> Wire e m (f SDL.Keysym) Object
ship bounds@(V2 w h) = proc keysDown -> do
  rot <- rotate -< keysDown
  position <- wrappedPosition bounds shipStart . velocity . acceleration -< (keysDown, rot)
  returnA -< Object position rot

 where

  shipStart = V2 (w / 2 - 25) (h / 2 - 25)

--------------------------------------------------------------------------------
gameWire
  :: (Monoid e, Foldable f, RandomGen g)
  => V2 Double -> g
  -> Wire e IO (f SDL.Keysym) Frame
gameWire bounds g = proc keysDown -> do
  --asteroid <- asteroid bounds g -< keysDown
  ship <- ship bounds -< keysDown

  returnA -< Frame { frameShip = ship
                   --, asteroidPosition = asteroid
                   }

--------------------------------------------------------------------------------
{-asteroid :: RandomGen g => V2 Double -> g -> Wire e IO a (V2 Double)-}
{-asteroid bounds@(V2 w h) g =-}
  {-wrappedPosition bounds start . pure (rotateVector mag r)-}

 {-where-}

  {-(start, mag, r) = flip evalState g $ do-}
                      {-x <- state (randomR (0, w))-}
                      {-y <- state (randomR (0, h))-}
                      {-mag <- state (randomR (10, 40))-}
                      {-r <- state (randomR (0, 2 * pi))-}
                      {-return (V2 x y, mag, r)-}


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

        drawObject screen (frameShip f)
        --drawVectorAt screen (asteroidPosition f)

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

  drawObject screen (Object pos@(V2 x y) rot) = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.circle screen (round x) (round y) 50 pixel

    let (V2 x' y') = ((V2 0 50) *! rot) + pos
    SDL.line screen (round x) (round y) (round x') (round y') pixel



