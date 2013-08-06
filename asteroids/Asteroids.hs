{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Prelude hiding ((.), id, mapM_, any, concatMap, concat)
import qualified Prelude
import Control.Monad (replicateM, void)
import Control.Lens hiding (at, perform, wrapped)
import Control.Monad.Fix (MonadFix)
import Control.Wire hiding (until)
import Data.Foldable
import Data.Monoid
import Linear hiding ((*!))
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.TTF as SDLTTF

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

colliding :: (Physical a, Physical b) => [a] -> b -> Bool
colliding others this =
  any (intersecting (bounds this)) . map bounds $ others

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

shipRadius :: Double
shipRadius = 10

instance Physical Ship where
  bounds Ship{..} = Circle shipPos shipRadius

--------------------------------------------------------------------------------
data Frame = Frame { fShip :: Either [V2 Double] Ship
                   , fAsteroids :: [Asteroid]
                   , fBullets :: [Bullet]
                   , fScore :: Int
                   , fParticles :: [V2 Double]
                   }

--------------------------------------------------------------------------------
render :: SDL.Surface -> SDLTTF.Font -> Frame -> IO ()
render screen font Frame{..} = do
  void $ (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 0 0 >>=
    SDL.fillRect screen Nothing

  let renderObject = void . renderBounds . bounds
      renderShip (Left particles) = mapM_ renderPoint particles
      renderShip (Right s@Ship{..}) = do
        renderObject s
        renderLine shipPos
          (shipPos + normalize (shipRotation !* (V2 0 (-1))) ^* shipRadius)
        return ()

  mapM_ renderObject fAsteroids
  mapM_ renderObject fBullets
  mapM_ renderPoint fParticles
  renderShip fShip

  scoreS <-
    SDLTTF.renderTextSolid font ("SCORE: " ++ show fScore)
      (SDL.Color 255 255 255)

  SDL.blitSurface scoreS Nothing screen Nothing

  SDL.flip screen

 where

  renderBounds (Circle (V2 x y) r) = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.circle screen (round x) (round y) (round r) pixel

  renderBounds (Point p) = renderPoint p

  renderPoint (V2 x y) = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.pixel screen (round x) (round y) pixel

  renderLine (V2 x y) (V2 x' y') = do
    pixel <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255
    SDL.line screen (round x) (round y) (round x') (round y') pixel

--------------------------------------------------------------------------------
main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 650 480 32 [SDL.SWSurface]

  SDLTTF.init
  ka1 <- SDLTTF.openFont "game_over.ttf" 20

  go screen ka1 (Set.empty) clockSession playForever

 where

  playForever = switchBy progression asteroidsRound
   where progression Cleared = playForever
         progression Crashed = playForever

  go screen font keysDown s w = do
    keysDown' <- parseEvents keysDown
    (r, w', s') <- stepSession w s keysDown'

    case r of
      Right frame -> do
        render screen font frame
        go screen font keysDown' s' w'

      Left _ -> return ()


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
data LevelOver = Cleared | Crashed
  deriving (Show)

instance Monoid LevelOver where
  mempty = Crashed
  mappend Crashed Crashed = Crashed
  mappend _ _ = Cleared

asteroidsRound
  :: (Applicative m, Monad m, MonadFix m, MonadRandom m)
  => Wire LevelOver m (Set.Set SDL.Keysym) Frame
asteroidsRound = proc keysDown -> do
  rec
    bulletAutos <- stepWires . delay [] -< activeBullets
    asteroidAutos <- stepWires . initialAsteroids -< activeAsteroids

    (remainingBullets, remainingAsteroids, removedAsteroids) <-
      collide -< (bulletAutos, asteroidAutos)

    newAsteroids <- splitAsteroids -< map fst removedAsteroids

    (p, newBulletWires) <- player -< (keysDown, map fst remainingAsteroids)

    activeBullets <- returnA -< newBulletWires ++ map snd remainingBullets
    activeAsteroids <- returnA -< newAsteroids ++ map snd remainingAsteroids

    unless (== 0) <!> Cleared -< length activeAsteroids

  let asteroidExplosions = removedAsteroids ^.. folded . _1 . to astPos
  particles <- particleSystems -< asteroidExplosions

  points <- countFrom 0 -< sumOf (folded._1.to score) removedAsteroids

  returnA -< Frame { fShip = p
                   , fAsteroids = map fst asteroidAutos
                   , fBullets = map fst bulletAutos
                   , fScore = points
                   , fParticles = particles
                   }

 where

  score Asteroid{..}
    | astGeneration == 1 = 10
    | astGeneration == 2 = 50
    | astGeneration == 3 = 100
    | otherwise          = 0

  initialAsteroids = mkGen $ \dt a -> do
    wires <- replicateM 4 $
      asteroid 1 <$> getRandomR (20, 40)
                 <*> (V2 <$> getRandomR (0, 640) <*> getRandomR (0, 480))
                 <*> randomVelocity (10, 20)
    stepWire (delay wires) dt a

  collide = mkFix $ \_ (bullets, asteroids) ->
    let activeBullets = filter (not . colliding (map fst asteroids) . fst) bullets
        activeAsteroids = filter (not . colliding (map fst bullets) . fst) asteroids
        destroyedAsteroids = filter (colliding (map fst bullets) . fst) asteroids
    in Right (activeBullets, activeAsteroids, destroyedAsteroids)

  splitAsteroids = mkFixM $ \_ asteroids ->
    (Right . concat) <$> mapM splitAsteroid asteroids

  splitAsteroid Asteroid{..}
    | astGeneration < 3 = do
        let mkAsteroid vel =
              asteroid (succ astGeneration) (astSize / 2) astPos (vel ^* 3)
            mag = ( fromIntegral $ astGeneration * 10
                  , fromIntegral $ (astGeneration + 1) * 10)
        replicateM 2 (mkAsteroid <$> randomVelocity mag)
    | otherwise         = return []

--------------------------------------------------------------------------------
randomVelocity
  :: (Applicative m, MonadRandom m) => (Double, Double) -> m (V2 Double)
randomVelocity magRange = do
  v <- V2 <$> getRandomR (-1, 1) <*> getRandomR (-1, 1)
  mag <- getRandomR magRange
  return (normalize v ^* mag)

--------------------------------------------------------------------------------
particleSystems
  :: (Applicative m, MonadRandom m) => Wire e m [V2 Double] [V2 Double]
particleSystems = go []
 where
  go systems = mkGen $ \dt newSystemLocations -> do
    stepped <- mapM (\w -> stepWire w dt ()) systems

    let alive = [ (r, w) | (Right r, w) <- stepped ]
    spawned <- concat <$> mapM spawnParticles newSystemLocations

    return (Right (map fst alive), go $ map snd alive ++ spawned)

  spawnParticles at = do
    n <- getRandomR (4, 8)
    replicateM n $ do
      velocity <- randomVelocity (5, 10)
      life <- getRandomR (1, 3)
      return ((for life <!> ()). integrateVector at . pure velocity)

--------------------------------------------------------------------------------
player
  :: (Monoid e, Monad m, MonadRandom m, Applicative m)
  => Wire LevelOver m
       (Set.Set SDL.Keysym, [Asteroid])
       (Either [V2 Double] Ship, [Wire e m () Bullet])
player = proc (keysDown, activeAsteroids) -> do
  ship <- fly -< keysDown
  arr snd . (notColliding *** aliveShip) --> arr fst . first explode
    -< ((ship, activeAsteroids), (ship, keysDown))

 where
  fly = proc keysDown -> do
    rotation <- rotationMatrix <$> (integral_ 0 . inputRotation) -< keysDown
    thrust <- inputAcceleration -< keysDown

    pos <- wrapped .
           integrateVector (V2 (640 / 2) (380 / 2)) .
           integrateVector 0 -< rotation !* thrust

    returnA -< Ship pos rotation

  notColliding = mkFix $ \_ a@(ship, asteroids) ->
    if colliding asteroids ship
        then Left mempty
        else Right a

  explode = proc (ship, _) -> do
    particles <- particleSystems . (once <|> pure []) -< [shipPos ship]
    for 3 . returnA -< (Left particles, [])

  aliveShip = proc (ship, keysDown) -> do
    newBulletWires <- fire -< (ship, keysDown)
    returnA -< (Right ship, newBulletWires)

  inputAcceleration  =  pure (V2 0 (-150)) . when (keyDown SDL.SDLK_UP)
                    <|> 0

  inputRotation  =  (negate pi) . when (keyDown SDL.SDLK_LEFT)
                <|> pi . when (keyDown SDL.SDLK_RIGHT)
                <|> pure (0 :: Double)

  bulletWire parent = for 2 . aBullet
    where
      aBullet = Bullet <$> wrapped .
                           integrateVector (shipPos parent) .
                           pure bulletVelocity
      bulletVelocity = shipRotation parent !* (V2 0 (-300))

  fire = let tryShoot = proc (p, keysDown) -> do
               isShooting -< keysDown
               returnA -< [ bulletWire p ]
         in tryShoot <|> pure []

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
  pos <- wrapped . integrateVector initialPosition . pure velocity -< ()
  returnA -< Asteroid pos generation size velocity

--------------------------------------------------------------------------------
wrapped :: Wire e m (V2 Double) (V2 Double)
wrapped = mkFix $ \_ (V2 x y) ->
  let x' = until (>= 0) (+ 640) $ until (<= 640) (subtract 640) $ x
      y' = until (>= 0) (+ 480) $ until (<= 480) (subtract 480) $ y
  in Right (V2 x' y')

--------------------------------------------------------------------------------
isShooting :: (Foldable f, Monad m, Monoid e) => Event e m (f SDL.Keysym)
isShooting =
  asSoonAs (keyDown SDL.SDLK_SPACE) >>> (once --> coolDown >>> isShooting)

 where

  coolDown =
    arr head . multicast [ after 0.05, asSoonAs (not . keyDown SDL.SDLK_SPACE) ]

--------------------------------------------------------------------------------
stepWires :: Monad m => Wire e m [Wire e m () b] [(b, Wire e m () b)]
stepWires = mkFixM $ \dt objects -> do
  stepped <- mapM (\o -> stepWire o dt ()) objects
  return $ Right [ (o, w') | (Right o, w') <- stepped ]
