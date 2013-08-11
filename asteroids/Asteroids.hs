{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import qualified Sounds

import Prelude hiding ((.), id, mapM_, any, concatMap, concat)
import qualified Prelude
import Control.Monad (replicateM, void)
import Data.Bits
import Data.Ix (inRange)
import Data.Word (Word8)
import Control.Lens hiding (at, perform, wrapped)
import Control.Wire hiding (until)
import Data.Foldable
import Data.Monoid
import Linear hiding ((*!))
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as SDL
import qualified Graphics.UI.SDL.Framerate as Framerate
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
                         , astSpikes :: [V2 Double]
                         }

instance Physical Asteroid where
  bounds Asteroid{..} = Circle astPos astSize

class Positioned p where
  position :: Functor f => (V2 Double -> f (V2 Double)) -> p -> f p

instance Positioned Asteroid where
  position f x =  f (astPos x) <&> \b -> x { astPos = b }

--------------------------------------------------------------------------------
data Bullet = Bullet { bulletPos :: V2 Double }

instance Physical Bullet where
  bounds Bullet{..} = Point bulletPos

instance Positioned Bullet where
  position f x =  f (bulletPos x) <&> \b -> x { bulletPos = b }

--------------------------------------------------------------------------------
data Ship = Ship { shipPos :: V2 Double, shipRotation :: M22 Double }

shipRadius :: Double
shipRadius = 10

instance Physical Ship where
  bounds Ship{..} = Circle shipPos shipRadius

instance Positioned Ship where
  position f x =  f (shipPos x) <&> \b -> x { shipPos = b }

--------------------------------------------------------------------------------
data Frame = Frame { fShip :: Either [V2 Double] Ship
                   , fAsteroids :: [Asteroid]
                   , fBullets :: [Bullet]
                   , fScore :: Int
                   , fParticles :: [V2 Double]
                   , fUfo :: [UFO]
                   }

data UFO = UFO { ufoPos :: V2 Double, ufoSize :: UFOSize }

data UFOSize = Small | Large

instance Positioned UFO where
  position f x =  f (ufoPos x) <&> \b -> x { ufoPos = b }

instance Physical UFO where
  bounds UFO{..} = Circle ufoPos $ case ufoSize of
                                     Small -> 10
                                     Large -> 30

--------------------------------------------------------------------------------
render :: SDL.Surface -> SDLTTF.Font -> Frame -> IO ()
render screen font Frame{..} = do
  void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
    SDL.fillRect screen Nothing

  mapM_ renderAsteroid fAsteroids
  mapM_ (renderBounds . bounds) fBullets
  mapM_ renderPoint fParticles
  mapM_ renderUfo fUfo
  renderShip fShip

  scoreS <-
    SDLTTF.renderTextSolid font ("SCORE: " ++ show fScore)
      (SDL.Color 255 255 255)

  SDL.blitSurface scoreS Nothing screen (Just $ SDL.Rect 20 20 100 50)

  SDL.flip screen

 where

  renderBounds (Circle (V2 x y) r) =
    SDL.circle screen (round x) (round y) (round r) white

  renderBounds (Point p) = renderPoint p

  renderPoint (V2 x y) =
    SDL.pixel screen (round x) (round y) white

  renderLine (V2 x y) (V2 x' y') =
    SDL.line screen (round x) (round y) (round x') (round y') white

  renderAsteroid Asteroid{..} = renderPolygon $ map (+ astPos) astSpikes

  renderPolygon points =
    mapM_ (uncurry renderLine) (zip points (tail points ++ points))

  renderShip (Left particles) = mapM_ renderPoint particles

  renderShip (Right Ship{..}) =
    let v1 = V2 (-0.5) 1
        v2 = V2 0 0.5
        v3 = V2 0.5 1
        v4 = V2 0 (-1)
    in renderPolygon $
      map ((+ shipPos) . (shipRotation !*). (^* shipRadius)) [v1, v2, v3, v4]

  renderUfo = renderBounds . bounds

  white = rgbColor 255 255 255

rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fi r) 24 .|.
                            shiftL (fi g) 16 .|.
                            shiftL (fi b) 8  .|.
                            255)
  where fi = fromIntegral

--------------------------------------------------------------------------------
main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 800 600 0 [SDL.SWSurface]--], SDL.Fullscreen]

  SDLTTF.init
  ka1 <- SDLTTF.openFont "ka1.ttf" 10

  frameRate <- Framerate.new
  Framerate.init frameRate
  Framerate.set frameRate 60

  let audioSampleRate = 44100 -- 22050
  [explosionChunk, pew, deathSound] <- mapM (Sounds.render audioSampleRate)
    [Sounds.explosion, Sounds.pew, Sounds.death]
  SDL.openAudio audioSampleRate SDL.AudioU8 2 512

  go screen ka1 Set.empty clockSession
    (playForever explosionChunk pew deathSound)
    frameRate

 where

  playForever c d e = game 4 0
    where
      game n score =
        let progress (LevelOver Cleared score) = game (min 12 $ succ n) score
            progress (LevelOver Crashed _) = game 4 0
        in switchBy progress (asteroidsRound n c d e score)

  go screen font keysDown s w frameRate = do
    keysDown' <- parseEvents keysDown
    (r, w', s') <- stepSession w s keysDown'

    case r of
      Right frame -> do
        render screen font frame
        Framerate.delay frameRate

        go screen font keysDown' s' w' frameRate

      Left _ -> return ()


  parseEvents keysDown = do
    e <- SDL.pollEvent
    case e of
      SDL.NoEvent -> return keysDown
      SDL.KeyDown k ->
        let keyset = Set.insert k keysDown
        in if keyDown (SDL.SDLK_q) keyset then undefined
             else parseEvents keyset
      SDL.KeyUp k -> parseEvents (Set.delete k keysDown)
      _ -> parseEvents keysDown

--------------------------------------------------------------------------------
keyDown :: Foldable f => SDL.SDLKey -> f SDL.Keysym -> Bool
keyDown = elemOf (folded . to SDL.symKey)

--------------------------------------------------------------------------------
data EndReason = Cleared | Crashed

data LevelOver = LevelOver EndReason Int

instance Monoid LevelOver where
  mempty = LevelOver Crashed 0
  mappend (LevelOver Crashed n) (LevelOver Crashed m) = LevelOver Crashed (max n m)
  mappend (LevelOver _ n) (LevelOver _ m) = LevelOver Cleared (max n m)

asteroidsRound
  :: Int
  -> SDL.Chunk -> SDL.Chunk -> SDL.Chunk
  -> Int
  -> Wire LevelOver IO (Set.Set SDL.Keysym) Frame
asteroidsRound nAsteroids c d e initialScore = proc keysDown -> do
  -- Ship, asteroids, bullets
  rec
    bulletAutos <- stepWires . delay [] -< activeBullets
    ufoAutos <- stepWires . delay [] -< activeUfos
    asteroidAutos <- stepWires . initialAsteroids -< activeAsteroids

    (remainingBullets, remainingAsteroids, removedAsteroids) <-
      collide -< (bulletAutos, asteroidAutos)

    newAsteroids <- splitAsteroids -< map fst removedAsteroids

    (p, newBulletWires) <- player e -< (keysDown, map fst remainingAsteroids)
    newUfoWires <- pure [ largeUfo ] . ufoSpawned <|> pure [] -< ()

    activeBullets <- returnA -< newBulletWires ++ map snd remainingBullets
                                 ++ concatMap (snd . fst) ufoAutos
    activeAsteroids <- returnA -< newAsteroids ++ map snd remainingAsteroids
    activeUfos <- returnA -< newUfoWires ++ map snd ufoAutos

  -- Points/explosions
  let asteroidExplosions = removedAsteroids ^.. folded . _1 . position
  particles <- particleSystems -< asteroidExplosions
  points <- countFrom initialScore -< sumOf (folded._1.to score) removedAsteroids

  -- Sound effects
  playForList c 0 -< asteroidExplosions
  playForList d 1 -< newBulletWires

  -- End game semantics
  first (unless (== 0)) --> for 2 --> second clearLevel -<
    (length activeAsteroids, points)

  returnA -< Frame { fShip = p
                   , fAsteroids = map fst asteroidAutos
                   , fBullets = map fst bulletAutos
                   , fScore = points
                   , fParticles = particles
                   , fUfo = ufoAutos ^.. traverse . _1 . _1
                   }

 where

  playForList c channel = once . playChunk channel c . edge (not . null) <|> id

  clearLevel = mkFix $ \_ points -> Left $ LevelOver Cleared points

  score Asteroid{..}
    | astGeneration == 1 = 10
    | astGeneration == 2 = 50
    | astGeneration == 3 = 100
    | otherwise          = 0

  initialAsteroids = mkGen $ \dt a -> do
    wires <- replicateM nAsteroids $
      asteroid 1
        <$> getRandomR (20, 40)
        <*> (V2 <$> getRandomR (0, 800) <*> getRandomR (0, 600))
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
            mag = ( fromIntegral $ astGeneration * 20
                  , fromIntegral $ (astGeneration + 1) * 20)
        replicateM 2 (mkAsteroid <$> randomVelocity mag)
    | otherwise         = return []

  ufoSpawned = once --  . wackelkontaktM (1 / 500) . after 60

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
  :: (Monoid e, Monad m)
  => SDL.Chunk
  -> Wire LevelOver IO
       (Set.Set SDL.Keysym, [Asteroid])
       (Either [V2 Double] Ship, [Wire e m () Bullet])
player deathSound = proc (keysDown, activeAsteroids) -> do
  ship <- fly -< keysDown
  arr snd . (notColliding *** aliveShip) --> arr fst . first explode
    -< ((ship, activeAsteroids), (ship, keysDown))

 where
  fly = proc keysDown -> do
    rotation <- rotationMatrix <$> (integral_ 0 . inputRotation) -< keysDown
    thrust <- inputAcceleration -< keysDown

    pos <- wrapped .
           integrateVector (V2 (800 / 2) (600 / 2)) .
           integrateVectorUpTo 0 100 -< rotation !* thrust

    returnA -< Ship pos rotation

  notColliding = mkFix $ \_ a@(ship, asteroids) ->
    if colliding asteroids ship
        then Left mempty
        else Right a

  explode = proc (ship, _) -> do
    (playChunk 2 deathSound . once) <|> id -< ()
    particles <- particleSystems . (once --> pure []) -< [ship ^. position]
    for 3 . returnA -< (Left particles, [])

  aliveShip = proc (ship, keysDown) -> do
    newBulletWires <- fire -< (ship, keysDown)
    returnA -< (Right ship, newBulletWires)

  inputAcceleration  =  pure (V2 0 (-75)) . when (keyDown SDL.SDLK_UP)
                    <|> 0

  inputRotation  =  negate pi . when (keyDown SDL.SDLK_LEFT)
                <|> pi . when (keyDown SDL.SDLK_RIGHT)
                <|> pure (0 :: Double)

  bulletWire ship = bullet (shipPos ship) (shipRotation ship !* V2 0 (-300))

  fire = let tryShoot = proc (p, keysDown) -> do
               isShooting -< keysDown
               returnA -< [ bulletWire p ]
         in tryShoot <|> pure []

--------------------------------------------------------------------------------
bullet :: (Monad m, Monoid e) => V2 Double -> V2 Double -> Wire e m a Bullet
bullet initialPosition bulletVelocity = for 1.5 . aBullet
  where
    aBullet = Bullet <$> wrapped . integrateVector initialPosition .
                         pure bulletVelocity

--------------------------------------------------------------------------------
playChunk :: SDL.Channel -> SDL.Chunk -> Wire e IO a a
playChunk channel chunk = mkFixM $ \_ a -> do
  SDL.haltChannel channel
  SDL.playChannel channel chunk 0
  return (Right a)

--------------------------------------------------------------------------------
integrateVector
  :: (Functor f, Num (f Time), Metric f)
  => f Double -> Wire e m (f Double) (f Double)
integrateVector = accumT step where step dt a b = a + dt *^ b

--------------------------------------------------------------------------------
integrateVectorUpTo
  :: (Functor f, Num (f Time), Metric f)
  => f Double -> Double -> Wire e m (f Double) (f Double)
integrateVectorUpTo c m = accumT step c
  where step dt a b = let v' = a + dt *^ b
                          n = norm v'
                      in if n > m then normalize v' ^* m else v'

--------------------------------------------------------------------------------
rotationMatrix :: Floating a => a -> M22 a
rotationMatrix r = V2 (V2 (cos r) (-(sin r)))
                      (V2 (sin r) (  cos r) )

--------------------------------------------------------------------------------
asteroid
  :: (Monad m, MonadRandom m)
  => Int -> Double -> V2 Double -> V2 Double -> Wire e m a Asteroid
asteroid generation size initialPosition velocity = proc _ -> do
  spikes <- randomSpikes -< size
  pos <- wrapped . integrateVector initialPosition . pure velocity -< ()
  returnA -< Asteroid pos generation size velocity spikes

 where

  randomSpikes = mkGen $ \_ size -> do
    let mkSpike i = do
          mag <- getRandomR (size / 2, size)
          theta <- getRandomR (i * (2 * pi) / 7, (i + 1) * (2 * pi) / 7)
          return $ rotationMatrix theta !* V2 0 mag
    spikes <- mapM mkSpike [0..6]
    return (Right spikes, pure spikes)

--------------------------------------------------------------------------------
wrapped :: Wire e m (V2 Double) (V2 Double)
wrapped = mkFix $ \_ (V2 x y) ->
  let x' = until (>= 0) (+ 800) $ until (<= 800) (subtract 800) x
      y' = until (>= 0) (+ 600) $ until (<= 600) (subtract 600) y
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

--------------------------------------------------------------------------------
largeUfo
  :: (Monad m, MonadRandom m, Monoid e)
  => Wire e m a (UFO, [Wire e m a Bullet])
largeUfo = proc _ -> do
  position <- require onScreen . pos -< ()
  shotsFired <- shoot <|> pure [] -< position

  returnA -< (UFO position Small, shotsFired)

 where

  shoot = proc pos -> do
    periodically 1 -< ()
    velocity <- (!* (V2 0 300)) . rotationMatrix <$> noiseRM . pure (0, 2 * pi) -< ()
    returnA -< [bullet pos velocity]

  pos = let x = integrateVector (V2 0 0) . pure (V2 80 0)
            y = (V2 0) . (+ 100) . (5 *) . sin . (* 10) <$> time
        in (+) <$> x <*> y

  onScreen (V2 x y) = x >= 0 && x < 800

  randomConstant range = mkGen $ \_ _ -> do
    x <- getRandomR range
    return (Right x, pure x)
