{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
import Prelude hiding ((.), id, until)
import Data.Set (Set)
import Control.Lens
import Control.Monad.Loops
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Wire
import Control.Wire.FRP
import Data.Bits
import Data.Foldable
import Data.Word
import Linear
import Linear.Affine

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL

type Pill = Point V2 Double
type Pacman = Point V2 Double

data Scene = Scene { pills :: [Pill]
                   , pacman :: Pacman
                   }

data Assets = Assets { pillSurface :: SDL.Surface }

_Pixel :: Iso' (Word8, Word8, Word8) SDL.Pixel
_Pixel = iso to fro
  where
    to (r, g, b) = SDL.Pixel (shiftL (fi r) 24 .|.
                              shiftL (fi g) 16 .|.
                              shiftL (fi b) 8  .|.
                              255)
    fi = fromIntegral
    fro = undefined

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 800 600 0 [SDL.SWSurface]

  assets <- Assets
    <$> (SDL.createRGBSurface [] 16 16 32 0 0 0 0 >>= \s ->
         SDL.filledCircle s 8 8 7 ((255, 255, 0) ^. _Pixel) >>
         return s)

  flip evalStateT allKeysUp $ reactimateM
    (handleEvents >> get)
    (either handleInhibition (lift . handleResult screen assets))
    lift
    clockSession_
    pacmanGame

 where

  handleInhibition () = return Quit

  handleResult screen assets scene = do
    forM_ (pills scene) $ \pill ->
      SDL.blitSurface (pillSurface assets) Nothing
        screen (Just (pillBounds & rLoc +~ (round <$> pill ^. _xy)))

    SDL.flip screen

    return NoQuit

  rLoc f (SDL.Rect x y w h) = f (V2 x y) <&> \(V2 x y) -> SDL.Rect x y w h

  pillBounds = SDL.Rect 0 0 16 16

  allKeysUp :: Set SDL.SDLKey
  allKeysUp = mempty

  handleEvents = unfoldM_ $ do
    e <- liftIO SDL.pollEvent

    case e of
      SDL.NoEvent -> pure empty
      _ -> Just <$> case e of
        SDL.KeyDown k -> contains (SDL.symKey k) .= True
        SDL.KeyUp k   -> contains (SDL.symKey k) .= False
        _             -> pure ()

  pacmanGame = pure (Scene [P (V2 50 50)] 0)
