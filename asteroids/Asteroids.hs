{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, until)
import qualified Prelude
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Control.Wire
import Data.Foldable (Foldable)
import Data.Foldable (asum)
import Data.Monoid (Monoid, mempty)
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

rotationMatrix :: Floating a => a -> M22 a
rotationMatrix r = V2 (V2 (cos r) (-(sin r)))
                      (V2 (sin r) (  cos r) )

--------------------------------------------------------------------------------
velocity :: Wire e m (V2 Time) (V2 Time)
velocity = accumT (\dt v a -> let v' = v + a ^* dt
                              in normalize v' ^* (min 100 (norm v'))) 0


--------------------------------------------------------------------------------
wrappedPosition :: V2 Double -> V2 Double -> Wire e m (V2 Double) (V2 Double)
wrappedPosition bounds = accumT wrap

 where

  wrap dt p v =
    let (V2 w h) = bounds + 50
        f b = Prelude.until (>= 0) (+ b) . Prelude.until (<= b) (\a -> a - b)
        (V2 x y) = p + (pure dt * v)
    in V2 (f w (x + 50) - 50) (f h (y + 50) - 50)


--------------------------------------------------------------------------------
data Frame = Frame { frameShip :: !Object
                   , frameAsteroids :: ![Object]
                   , frameBullet :: [Object]
                   }

data Object = Object { objPos :: !(V2 Double), objRotation :: !(M22 Double) }

data AutoObject d e m = AutoObject { aoObj :: d, aoWire :: Wire e m () d }


--------------------------------------------------------------------------------
ship
  :: (Foldable f, Monoid e, Monad m)
  => V2 Double -> Wire e m (f SDL.Keysym) Object
ship bounds@(V2 w h) = proc keysDown -> do
  rot <- rotate -< keysDown
  position <- wrappedPosition bounds shipStart . velocity . acceleration -< (keysDown, rot)
  returnA -< Object position rot

 where

  shipStart = V2 (w / 2 - 25) (h / 2 - 25)


bullet :: (Monoid e, Monad m) => Object -> AutoObject Object e m
bullet parent = let
  wire = proc _ -> do
    let rot = objRotation parent
    let vel = (V2 0 300) *! rot
    pos <- withinBounds (V2 640 480) .
            wrappedPosition (V2 640 480) (objPos parent) -< vel
    returnA -< Object pos rot
  in AutoObject parent wire

withinBounds :: (Monoid e, Monad m, Num a, Ord a) => V2 a -> Wire e m (V2 a) (V2 a)
withinBounds b@(V2 w h) = mkPure $ \_ a@(V2 x y) ->
  if x < 0 || x > w || y < 0 || y > h
    then (Left mempty, empty) else (Right a, withinBounds b)

--------------------------------------------------------------------------------
gameWire
  :: (Foldable f, Monoid e, RandomGen g)
  => V2 Double -> g
  -> Wire e IO (f SDL.Keysym) Frame
gameWire bounds g = proc keysDown -> do
  currentShip <- ship bounds -< keysDown
  newBullets <- arr fst . (arr (pure.bullet) *** isShooting) <|> pure [] -< (currentShip, keysDown)

  rec

    asteroids <- step . delay (makeAsteroids g) -< asteroids'
    bullets <- step . delay [] -< newBullets ++ bullets'

    (asteroids', bullets') <- collideBullets -< (asteroids, bullets)

    when id -< not $ any (colliding currentShip) (map aoObj asteroids')


  returnA -< Frame { frameShip = currentShip
                   , frameAsteroids = map aoObj asteroids
                   , frameBullet = map aoObj bullets
                   }

 where

  makeAsteroids g = flip evalState g $ replicateM 5 (state $ asteroid bounds)

  collideBullets = mkFix $ \_ (asteroids, bullets) ->
    Right ( filter (\asteroid -> not $ any (colliding (aoObj asteroid) . aoObj) bullets) asteroids
          , filter (\bullet ->   not $ any (colliding (aoObj bullet) . aoObj) asteroids) bullets
          )

  colliding a b = norm (objPos a - objPos b) < 40



--------------------------------------------------------------------------------
asteroid
  :: (Monad m, Monoid e, RandomGen g)
  => V2 Double -> g -> (AutoObject Object e m, g)
asteroid bounds@(V2 w h) g = (AutoObject { aoObj = Object { objPos = pos, objRotation = rotation }
                                         , aoWire = wire
                                         }, g')
 where

  wire = proc _ -> do
    position <- wrappedPosition bounds pos . pure (V2 0 speed *! rotation) -< ()
    returnA -< Object position rotation

  ((pos, speed, rotation), g') = flip runState g $ do
    randomPosition <-
      V2 <$> state (randomR (0, w))
         <*> state (randomR (0, h))
    randomRotation <- rotationMatrix <$> state (randomR (0, 2 * pi))
    randomSpeed <- state (randomR (10, 30))
    return (randomPosition, randomSpeed, randomRotation)


--------------------------------------------------------------------------------
-- This shooting wire allows one shot per 1s. The user can only shoot after
-- 0.1s have elapsed, and they have released the spacebar during this cooldown.
isShooting :: (Foldable f, Monad m, Monoid e) => Event e m (f SDL.Keysym)
isShooting =
  asSoonAs (keyDown' SDL.SDLK_SPACE) >>> (once --> coolDown >>> isShooting)

 where

  coolDown =
    arr head .  multicast [ after 0.05, asSoonAs (not . keyDown' SDL.SDLK_SPACE) ]


--------------------------------------------------------------------------------
main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 640 480 32 [SDL.SWSurface]

  g <- getStdGen
  go screen (Set.empty) clockSession (gameWire (V2 640 480) g)

 where

  go screen keysDown s w = do
    keysDown' <- parseEvents keysDown
    (f, w', s') <- stepSession_ w s keysDown'

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 0 0 >>=
        SDL.fillRect screen Nothing

    drawObject screen 15 (frameShip f)
    mapM_ (drawObject screen 40) (frameAsteroids f)
    mapM_ (drawPixel screen) (frameBullet f)

    SDL.flip screen

    go screen keysDown' s' w'

  parseEvents keysDown = do
    e <- SDL.pollEvent
    case e of
      SDL.NoEvent -> return keysDown
      SDL.KeyDown k -> parseEvents (Set.insert k keysDown)
      SDL.KeyUp k -> parseEvents (Set.delete k keysDown)
      _ -> parseEvents keysDown

  drawObject screen r (Object pos@(V2 x y) rot) = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.circle screen (round x) (round y) (round r) pixel

    let (V2 x' y') = ((V2 0 r) *! rot) + pos
    SDL.line screen (round x) (round y) (round x') (round y') pixel

  drawPixel screen (Object (V2 x y) _) = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.pixel screen (round x) (round y) pixel


--------------------------------------------------------------------------------
step :: Monad m => Wire e m [AutoObject d e m] [AutoObject d e m]
step = mkGen $ \dt objects -> do
  stepped <- mapM (\o -> stepWire (aoWire o) dt ()) objects
  return (Right [ AutoObject o w' | (Right o, w') <- stepped ], step)

