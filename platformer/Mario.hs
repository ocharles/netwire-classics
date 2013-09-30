{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
import Prelude hiding ((.), id, until)

import Control.Lens
import Control.Monad (liftM, msum)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Loops (unfoldM)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, gets, modify)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (evalStateT)
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
hitHead, onFloor :: (Num a, Ord a) => V2 a -> Bool
hitHead collisions = dot collisions (V2 0 1) < 0
onFloor collisions = dot collisions (V2 0 1) > 0

--------------------------------------------------------------------------------
mario
  :: ( Fractional t, HasTime t s, Monoid e, Monad m, MonadFix m
     , MonadState cache m, At cache, IxValue cache ~ SDL.Texture
     , Index cache ~ FilePath)
  => Wire s e m (MarioInput, Array (Point V2 Int) Bool) SceneElement
mario = proc (m, world) -> do
  rec
    -- Collisions that had to be corrected in the previous instant
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

  rec bearing <- case signum (marioXVelocity m) of
                   0 -> delay (V2 1 0)     -< bearing
                   b -> arr (\b -> V2 b 0) -< b

  returnA -< \r -> do
    SDL.setRenderDrawColor r 0 0 0 255
    let (x, y) = ( round (position ^. _x - 16)
                 , round (position ^. _y - 16)
                 )
        (x', y') = ( round (position ^. _x + bearing ^. _x * 32)
                   , round $ position ^. _y - 16)
    SDL.renderDrawRect r (SDL.Rect x y 32 32)
    SDL.renderDrawLine r (fromIntegral x + 16) (fromIntegral y + 16)
                         (x' + 16) (y' + 16)

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
    flip evalStateT Map.empty .
      reactimate (return $ const $ return ())
                 renderer world
                 (game world)
                 (countSession_ timestep) 0

 where

  timestep = 1 / 200

  game w = proc _ -> do
    input <- MarioInput
               <$> keyDown Scancode.Space
               <*> asum [ 75 . whileKeyHeld Scancode.Right
                        , -75 . whileKeyHeld Scancode.Left
                        , 0 ] -< ()
    mario -< (input, w)

  reactimate r' renderer world w' s' leftOver' previousTime = do
    currentTime <- liftIO getCurrentTime
    let delta = leftOver' + currentTime `diffUTCTime` previousTime
        steps = floor (delta / timestep) :: Int
        leftOver = delta - (fromIntegral steps * timestep)

    (r, w, s) <- stepNTimes r' w' s' (min 25 steps)

    case r of
      Left () -> return ()
      Right a -> do
        liftIO $ do
          SDL.setRenderDrawColor renderer 255 255 255 255
          SDL.renderClear renderer
          renderLevel world renderer
          a renderer
          SDL.renderPresent renderer

        reactimate r renderer world w s leftOver currentTime

  stepNTimes r' w' s' steps =
    if steps <= 0
        then return (r', w', s')
        else do
          events <- liftIO (unfoldM SDL.pollEvent)
          (ds, s) <- liftIO (stepSession s')
          (r, w) <- runReaderT (stepWire w' ds (Right ())) events
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
