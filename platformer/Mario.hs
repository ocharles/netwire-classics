{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
import Prelude hiding ((.), id, until)

import Control.Lens
import Control.Monad (liftM, msum, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Loops (unfoldM)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, gets, modify)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, withReaderT)
import Control.Monad.Trans.State (evalStateT, StateT)
import Control.Wire
import Data.Array (Array, (!))
import Data.Foldable (Foldable, asum, minimumBy)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Time (getCurrentTime, diffUTCTime)
import FRP.Netwire
import Linear
import Linear.Affine

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Wire.Unsafe.Event as Unsafe
import qualified Data.Array as Array
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keysym as Scancode

import Debug.Trace
traceId x = traceShow x x

--------------------------------------------------------------------------------
type SceneElement = SDL.Renderer -> IO ()

data MarioInput = MarioInput
  { marioJumping :: Event ()
  , marioXVelocity :: Double
  }

--------------------------------------------------------------------------------
gravity :: V2 Double
gravity = V2 0 800

terminalSpeed, jumpSpeed :: Num a => a
terminalSpeed = 345
jumpSpeed = 400

tileSize :: V2 Double
tileSize = V2 16 16

--------------------------------------------------------------------------------
correctVelocity
  :: (MonadReader (V2 (Maybe Double)) m, MonadState (V2 Double) m)
  => m (V2 Double)
correctVelocity = do
  ask >>= modify . flip (liftI2 fromMaybe)
  _y %= min terminalSpeed
  get

--------------------------------------------------------------------------------
correctPosition
  :: ( MonadReader (Array (Point V2 Int) Bool) m
     , MonadState (Point V2 Double) m)
  => m (Point V2 Double, V2 Double)
correctPosition = do
  world <- ask

  collisions <- unfoldM $ runMaybeT $ do
    currentPosition <- get

    let d = qd currentPosition
        adjacencies = take 3 $
          sortBy (comparing d) $
            filter ((< (32^2 * 2)) . d) $
              map ((* 32) . fmap fromIntegral) $
                filter (world !) $
                  Array.indices world

    msum $ map ?? adjacencies $ \tileCenter -> do
      let delta = currentPosition .-. tileCenter
          gap = fmap abs delta - (2 * tileSize)

          correct axis =
            let onAxis = gap * axis
                direction = if dot delta axis < 0 then -1 else 1
            in [ onAxis * direction | dot onAxis axis < 0 ]

      correction <- liftM (minimumBy (comparing quadrance))
                          (mapM correct basis)

      _xy -= correction
      return correction

  gets $ \relocated -> (relocated, Foldable.foldl' (+) 0 collisions)

--------------------------------------------------------------------------------
hitHead, onFloor :: (Num a, Ord a, Fractional a) => V2 a -> Bool
hitHead collisions = dot collisions (V2 0 1) < 0
onFloor collisions = dot collisions (V2 0 1) > 0.5

--------------------------------------------------------------------------------
mario
  :: ( Fractional t, HasTime t s, Monoid e, Monad m, MonadFix m
     , MonadState cache m, At cache, IxValue cache ~ SDL.Texture
     , MonadReader SDL.Renderer m, MonadIO m
     , Index cache ~ FilePath)
  => Wire s e m (MarioInput, Array (Point V2 Int) Bool) SceneElement
mario = proc (m, world) -> do
  rec
    collisions <- delay 0 -< collisions'

    let velocityCorrections =
          V2 (return (marioXVelocity m))
             (asum [ [ negate jumpSpeed | onFloor collisions
                                        , Unsafe.occurred (marioJumping m) ]
                   , [ 0 | hitHead collisions ]
                   , [ gravity ^. _y | onFloor collisions ]
                   ])

    velocity <-
      integralWith correctVelocity gravity -< (gravity, velocityCorrections)

    (position, collisions') <-
      integralWith correctPosition (P (V2 0 0)) -< (P velocity, world)

  rec
    lastFlip <- delay [] -< flip
    let flip = case signum (marioXVelocity m) of
                 0 -> lastFlip
                 1 -> []
                 -1 -> [SDL.Horizontal]

  yVelocity <- derivative <|> 0 -< position ^. _y

  frame <-
    if abs yVelocity > 0
      then pure (SDL.Rect 68 26 17 26) -< ()
      else
        if abs (velocity ^. _x) > 0
          then cycleFrames runFrames -< ()
          else pure idleFrame -<()

  tile "mario.bmp" -< (position, frame, flip)

 where

    runFrames = cycle [ SDL.Rect 115 26 16 28, SDL.Rect 139 26 17 26 ]

    idleFrame = SDL.Rect 10 26 15 27

    cycleFrames (a:as) = pure a . for (1 / 10) --> cycleFrames as

--------------------------------------------------------------------------------
tile
  :: (Monoid s, MonadIO m, MonadState cache m, At cache, IxValue cache ~ SDL.Texture, Index cache ~ FilePath, MonadReader SDL.Renderer m)
  => FilePath -> Wire s e m (Point V2 Double, SDL.Rect, [SDL.Flip]) SceneElement
tile fileName = mkGen $ \d a -> do
  renderer <- ask

  texture <- cacheLookup (Lens.at fileName) $ liftIO $ do
    surface <- SDL.loadBMP fileName
    surfaceFormat <- SDL.surfaceFormat surface
    key <- SDL.mapRGBA surfaceFormat 255 255 255 255
    SDL.setColorKey surface True key
    SDL.createTextureFromSurface renderer surface

  let move =
        mkPureN $ \(P pos, bounds, flipSprite) ->
          let draw r = void $ do
                liftIO $ SDL.renderCopyEx
                  renderer texture
                  (Just bounds)
                  (Just $ SDL.Rect 0 0 (SDL.rectW bounds) (SDL.rectH bounds)
                        & rectLoc +~ fmap round pos
                        & rectLoc -~ (V2 16 16))
                  0 Nothing
                  flipSprite

          in (Right draw, move)
  stepWire move d (Right a)

 where

  cacheLookup cacheKey action =
    use (cloneIndexedLens cacheKey) >>=
      maybe ((cloneIndexedLens cacheKey <?=) =<< action) return

--------------------------------------------------------------------------------
main :: IO ()
main =
  SDL.withInit [SDL.InitEverything] $
  SDL.withWindow "Mario" (SDL.Position 0 0) (SDL.Size 800 600) [] $ \window ->
  SDL.withRenderer window SDL.FirstSupported [SDL.Accelerated] $ \renderer -> do

  Right (JP.ImageRGB8 level) <- JP.readImage "level.png"
  let worldBounds = (0, P (V2 (JP.imageWidth level - 1) (JP.imageHeight level - 1)))
      world = Array.listArray worldBounds $
                map (\(P (V2 x y)) -> JP.computeLuma (JP.pixelAt level x y) == 0)
                  (Array.range worldBounds)

  getCurrentTime >>=
    flip evalStateT (Map.empty) .
    flip runReaderT renderer .
      reactimate (return $ const $ return ())
                 world
                 (game world)
                 (countSession_ timestep) 0

 where

  timestep = 1 / 200

  game
    :: Array (Point V2 Int) Bool
    -> Wire
         (Timed NominalDiffTime ())
         ()
         (ReaderT ([SDL.Event], SDL.Renderer) (StateT (Map.Map FilePath SDL.Texture) IO))
         a SceneElement
  game w = proc _ -> do
    input <- mapWire (withReaderT (view _1))
               (MarioInput
                  <$> keyDown Scancode.Space
                  <*> asum [ 75 . whileKeyHeld Scancode.Right
                           , -75 . whileKeyHeld Scancode.Left
                           , 0 ]) -< ()
    mapWire (withReaderT (view _2)) mario -< (input, w)

  reactimate r' world w' s' leftOver' previousTime = do
    currentTime <- liftIO getCurrentTime
    let delta = leftOver' + currentTime `diffUTCTime` previousTime
        steps = floor (delta / timestep) :: Int
        leftOver = delta - (fromIntegral steps * timestep)

    (r, w, s) <- stepNTimes r' w' s' (min 25 steps)

    case r of
      Left () -> return ()
      Right a -> do
        renderer <- ask
        liftIO $ do
          SDL.setRenderDrawColor renderer 255 255 255 255
          SDL.renderClear renderer
          renderLevel world renderer
          a renderer
          SDL.renderPresent renderer

        reactimate r world w s leftOver currentTime

  stepNTimes r' w' s' steps =
    if steps <= 0
        then return (r', w', s')
        else do
          events <- liftIO (unfoldM SDL.pollEvent)
          (ds, s) <- liftIO (stepSession s')
          (r, w) <-  withReaderT (\r -> (events, r)) (stepWire w' ds (Right ()))
          stepNTimes r w s (steps - 1)

  renderLevel world renderer = mapM_ ?? Array.assocs world $
    \(P (V2 x y), t) -> Monad.when t $ do
      SDL.setRenderDrawColor renderer 0 0 128 255
      SDL.renderFillRect renderer (SDL.Rect (x * 32 - 16) (y * 32 - 16) 32 32)


--------------------------------------------------------------------------------
-- Various events into the SDL event system
eventData :: Lens' SDL.Event SDL.EventData
eventData f e = f (SDL.eventData e) <&> \dat -> e { SDL.eventData = dat }

keysym :: Traversal' SDL.EventData SDL.Keysym
keysym f (SDL.Keyboard m w r s) = SDL.Keyboard m w r <$> f s
keysym _ e = pure e

keyMovement :: Traversal' SDL.EventData SDL.KeyMovement
keyMovement f (SDL.Keyboard m w r s) =
  SDL.Keyboard <$> f m <*> pure w <*> pure r <*> pure s
keyMovement _ e = pure e

scancode :: Lens' SDL.Keysym SDL.Scancode
scancode f k = f (Scancode.keyScancode k) <&> \s -> k { Scancode.keyScancode = s }

--------------------------------------------------------------------------------
whileKeyHeld
  :: (Functor m, Foldable f, Monoid e, MonadReader (f SDL.Event) m)
  => Scancode.Scancode -> Wire s e m t t
whileKeyHeld s = proc a -> do
  down <- keyDown s -< ()
  up <- keyUp s -< ()
  between -< (a, down, up)

--------------------------------------------------------------------------------
keyDown
  :: (Monad m, Functor m, Foldable f, MonadReader (f SDL.Event) m)
  => Scancode.Scancode -> Wire s e m a (Event a)
keyDown s = keyEvent s [ has $ keyMovement.only SDL.KeyDown ]

keyUp
  :: (Monad m, Functor m, Foldable f, MonadReader (f SDL.Event) m)
  => Scancode.Scancode -> Wire s e m a (Event a)
keyUp s = keyEvent s [ has $ keyMovement.only SDL.KeyUp ]

keyEvent
  :: (Monad m, Functor m, Foldable f, MonadReader (f SDL.Event) m)
  => Scancode.Scancode -> [SDL.EventData -> Bool] -> Wire s e m a (Event a)
keyEvent s conditions = mkGen_ $ \a -> do
  occuring <- hasEvent (has (keysym.scancode.only s) : conditions)
  return . Right $ if occuring then Unsafe.Event a else Unsafe.NoEvent

hasEvent
  :: (Monad m, Functor m, Foldable f, MonadReader (f SDL.Event) m)
  => [SDL.EventData -> Bool] -> m Bool
hasEvent conditions =
  has (folded . eventData . filtered (and <$> sequence conditions)) <$> ask

--------------------------------------------------------------------------------
rectLoc :: Functor f => (V2 Int -> f (V2 Int)) -> SDL.Rect -> f SDL.Rect
rectLoc f (SDL.Rect x y w h) =
  f (V2 x y) <&> \(V2 x' y') -> SDL.Rect x' y' w h
