{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Asteroids where

import Prelude hiding ((.), id, until, mapM_, any, concatMap)
import qualified Prelude
import Control.Concurrent (threadDelay)
import Control.Monad (liftM2, replicateM, void)
import Control.Lens
import Control.Monad.Fix (MonadFix)
import Control.Wire
import Data.Foldable
import Data.Monoid
import Linear hiding ((*!))
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL

--------------------------------------------------------------------------------
deriving instance Ord SDL.Keysym

--------------------------------------------------------------------------------
class Physical p where
  bounds :: p -> Bounds

data Bounds = Circle (V2 Double) Double | Point (V2 Double)

intersecting :: Bounds -> Bounds -> Bool
intersecting (Circle x i) (Circle y j)  = norm (x - y) < (i + j)
intersecting c@(Circle _ _) (Point p)   = intersecting c (Circle p 0)
intersecting p@(Point _) c@(Circle _ _) = intersecting c p
intersecting (Point _) (Point _)        = False

--------------------------------------------------------------------------------
data Asteroid = Asteroid { astPos :: V2 Double
                         , astGeneration :: Int
                         , astSize :: Double
                         , astVelocity :: V2 Double
                         }

instance Physical Asteroid where
  bounds Asteroid{..} = Circle astPos astSize

--------------------------------------------------------------------------------
data Bullet = Bullet { bulletPos :: V2 Double }

instance Physical Bullet where
  bounds Bullet{..} = Point bulletPos

--------------------------------------------------------------------------------
data Ship = Ship { shipPos :: V2 Double, shipRotation :: M22 Double }

instance Physical Ship where
  bounds Ship{..} = Circle shipPos 20

--------------------------------------------------------------------------------
data Frame = Frame { fShip :: Ship
                   , fAsteroids :: [Asteroid]
                   , fBullets :: [Bullet]
                   , fScore :: Int
                   }

--------------------------------------------------------------------------------
render :: SDL.Surface -> Frame -> IO ()
render screen Frame{..} = do
  void $ (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 0 0 >>=
    SDL.fillRect screen Nothing

  let renderObject = void . renderBounds . bounds
      renderShip s@Ship{..} = do
        renderObject s
        renderLine shipPos
          (shipPos + normalize (shipRotation !* (V2 0 (-1))) ^* 20)

  mapM_ renderObject fAsteroids
  mapM_ renderObject fBullets
  renderShip fShip

  SDL.flip screen

 where

  renderBounds (Circle (V2 x y) r) = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.circle screen (round x) (round y) (round r) pixel

  renderBounds (Point (V2 x y)) = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.pixel screen (round x) (round y) pixel

  renderLine (V2 x y) (V2 x' y') = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.line screen (round x) (round y) (round x') (round y') pixel

--------------------------------------------------------------------------------
main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 650 480 32 [SDL.SWSurface]
  g <- getStdGen
  go screen (Set.empty) clockSession asteroids

 where

  go screen keysDown s w = do
    keysDown' <- parseEvents keysDown
    (frame, w', s') <- stepSession_ w s keysDown'

    render screen frame

    go screen keysDown' s' w'

  parseEvents keysDown = do
    e <- SDL.pollEvent
    case e of
      SDL.NoEvent -> return keysDown
      SDL.KeyDown k -> parseEvents (Set.insert k keysDown)
      SDL.KeyUp k -> parseEvents (Set.delete k keysDown)
      _ -> parseEvents keysDown

--------------------------------------------------------------------------------
keyDown :: Foldable f => SDL.SDLKey -> f SDL.Keysym -> Bool
keyDown k = elemOf (folded . to SDL.symKey) k

--------------------------------------------------------------------------------
asteroids
  :: (Applicative m, Monoid e, Monad m, MonadFix m, MonadRandom m)
  => Wire e m (Set.Set SDL.Keysym) Frame
asteroids = proc keysDown -> do
  p <- player -< keysDown
  newBulletWires <- fire -< (p, keysDown)

  rec
    newAsteroids <- arr (concatMap splitAsteroid) . delay [] -<
      map fst removedAsteroids

    bulletAutos <- stepWires . delay [] -< newBulletWires ++ map snd activeBullets
    asteroidAutos <- stepWires . initialAsteroids -<
      newAsteroids ++ map snd activeAsteroids

    (activeBullets, activeAsteroids, removedAsteroids) <-
      collide -< (bulletAutos, asteroidAutos)

  points <- countFrom 0 -< sumOf (folded._1.to score) removedAsteroids

  let frame = Frame { fShip = p
                    , fAsteroids = map fst asteroidAutos
                    , fBullets = map fst bulletAutos
                    , fScore = points
                    }
  returnA -< frame

 where

  score Asteroid{..}
    | astGeneration == 1 = 10
    | astGeneration == 2 = 50
    | astGeneration == 3 = 100
    | otherwise          = 0

  initialAsteroids = mkGen $ \dt a -> do
    n <- getRandomR (3, 6)
    wires <- replicateM n $
      asteroid 1 <$> getRandomR (30, 60)
                 <*> (V2 <$> getRandomR (0, 640) <*> getRandomR (0, 480))
                 <*> (V2 <$> getRandomR (-20, 20) <*> getRandomR (-20, 20))
    stepWire (delay wires) dt a

  collide = mkFix $ \_ (bullets, asteroids) ->
    let colliding others this =
          any (intersecting (bounds (fst this))) . map (bounds.fst) $
            others
        activeBullets = filter (not . colliding asteroids) bullets
        activeAsteroids = filter (not . colliding bullets) asteroids
        destroyedAsteroids = filter (colliding bullets) asteroids
    in Right (activeBullets, activeAsteroids, destroyedAsteroids)

  fire = let tryShoot = proc (p, keysDown) -> do
               isShooting -< keysDown
               returnA -< [ bulletWire p ]
         in tryShoot <|> pure []

  bulletWire parent =
      Bullet <$> integrateVector (shipPos parent) . pure bulletVelocity
    where bulletVelocity = shipRotation parent !* (V2 0 (-300))

  splitAsteroid Asteroid{..}
    | astGeneration < 3 =
        let (V2 x y) = astVelocity
            mkAsteroid vel =
              asteroid (succ astGeneration) (astSize / 2) astPos (vel ^* 3)
        in [ mkAsteroid (rotationMatrix (pi / 2) !* astVelocity)
           , mkAsteroid (rotationMatrix ((negate pi) / 2) !* astVelocity)
           ]
    | otherwise         = []

--------------------------------------------------------------------------------
player :: (Monoid e, Monad m) => Wire e m (Set.Set SDL.Keysym) Ship
player = proc keysDown -> do
  rotation <- rotationMatrix <$> (integral_ 0 . inputRotation) -< keysDown
  accel <- uncurry (!*) <$> (id *** inputAcceleration) -< (rotation, keysDown)

  pos <- integrateVector (V2 (640 / 2) (380 / 2)) . integrateVector 0 -< accel

  let s = Ship pos rotation
  returnA -< s

 where
  inputAcceleration  =  pure (V2 0 (-150)) . when (keyDown SDL.SDLK_UP)
                    <|> 0

  inputRotation  =  (negate pi) . when (keyDown SDL.SDLK_LEFT)
                <|> pi . when (keyDown SDL.SDLK_RIGHT)
                <|> pure (0 :: Double)

--------------------------------------------------------------------------------
integrateVector
  :: (Functor f, Num (f Time)) => f Double -> Wire e m (f Double) (f Double)
integrateVector c = accumT step c where step dt a b = a + dt *^ b


--------------------------------------------------------------------------------
rotationMatrix :: Floating a => a -> M22 a
rotationMatrix r = V2 (V2 (cos r) (-(sin r)))
                      (V2 (sin r) (  cos r) )

--------------------------------------------------------------------------------
asteroid
  :: Monad m => Int -> Double -> V2 Double -> V2 Double -> Wire e m a Asteroid
asteroid generation size initialPosition velocity = proc _ -> do
  pos <- integrateVector initialPosition . pure velocity -< ()
  returnA -< Asteroid pos generation size velocity


--------------------------------------------------------------------------------
isShooting :: (Foldable f, Monad m, Monoid e) => Event e m (f SDL.Keysym)
isShooting =
  asSoonAs (keyDown SDL.SDLK_SPACE) >>> (once --> coolDown >>> isShooting)

 where

  coolDown =
    arr head .  multicast [ after 0.05, asSoonAs (not . keyDown SDL.SDLK_SPACE) ]

--------------------------------------------------------------------------------
stepWires :: Monad m => Wire e m [Wire e m () b] [(b, Wire e m () b)]
stepWires = mkFixM $ \dt objects -> do
  stepped <- mapM (\o -> stepWire o dt ()) objects
  return $ Right [ (o, w') | (Right o, w') <- stepped ]

--------------------------------------------------------------------------------
randomVector :: (Monad m, MonadRandom m) => m (V2 Double)
randomVector = liftM2 V2 (getRandomR (-1, 1)) (getRandomR (-1, 1))
