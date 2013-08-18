module Sounds (render, explosion, pew, death, ufo) where

import Prelude hiding ((.), id, length, sin)
import qualified Prelude

import Data.Bits
import Data.ByteString hiding (map, take, putStrLn)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign
import Graphics.UI.SDL.Mixer
import ForeignChunk
import Control.Wire hiding (empty, force, noise, sample)

import Data.Monoid

staticNoise :: [Double]
staticNoise = map (subtract 1 . (/ 16384) . fromIntegral) $ noiseFunc 2

 where

  noiseFunc :: Word16 -> [Word16]
  noiseFunc n = n : noiseFunc (nextN n)

  nextN n' = (n' `shiftR` 1) + ((n' .&. 1) `xor` ((n' .&. 2) `shiftR` 1) `shiftL` 14)

--------------------------------------------------------------------------------
-- | Noise within +/- 1
noise :: (Monoid e, Monad m) => Wire e m a Double
noise =  cycleW staticNoise--(noiseRM . pure (-1, 1))

--------------------------------------------------------------------------------
-- | Quantize a signal. Left input is the signal, right input is the
-- quantization factor (0 - 1).
quantize :: Wire e m (Double, Double) Double
quantize = mkFix $ \_ (s, ratio) ->
  Right $ ratio * fromIntegral (floor (s / ratio) :: Int)

--------------------------------------------------------------------------------
-- | Reduce sampling frequency. Left input is the signal, right input is
-- required sample rate.
rateReduce :: (Monad m, Monoid e) => Wire e m (Double, Int) Double
rateReduce = mkPure $ \_ (s, rate) ->
  (Right s, (keep . arr fst >>> for (recip $ fromIntegral rate)) --> rateReduce)

--------------------------------------------------------------------------------
-- | Decay a single over a period of time, inhibiting after.
decay :: (Monoid e, Monad m) => Time -> Wire e m Double Double
decay duration = id * (require (>0) . integral_ 1 . pure ((-1) / duration))

--------------------------------------------------------------------------------
-- | Gate a signal to only pass above a certain threshold. Left input is the
-- signal to gate, right input is the gate threshold.
gate :: (Num a, Ord a) => Wire e m (a, a) a
gate = mkFix $ \_ (s, threshold) -> Right $ if s >= threshold then s else 0

--------------------------------------------------------------------------------
explosion :: (Monoid e, Monad m, MonadRandom m) => Wire e m a Double
explosion = decay 2 . gate . (rateReduce &&& 0.4) . (quantize &&& 500) . (noise &&& 0.2)

--------------------------------------------------------------------------------
ufo :: (Monoid e, Monad m, MonadRandom m) => Wire e m a Double
ufo = for (1 / 3) . sin . ((sin . 3) * 200 + 100)

--------------------------------------------------------------------------------
death :: (Monoid e, Monad m, MonadRandom m) => Wire e m a Double
death = decay 5 . gate . (rateReduce &&& 0.3) . (quantize &&& 3000) . (noise &&& 0.2)

--------------------------------------------------------------------------------
clamp :: Wire e m Double Double
clamp = mkFix $ \_ x -> Right $ min 1 (max x (-1))

--------------------------------------------------------------------------------
pew :: (Monoid e, Monad m) => Wire e m a Double
pew = decay 0.5 . rateReduce . (((clamp . (sinDrop * 1000)) / 2) &&& 5000)
  where sinDrop = sin . ((+ 100) <$> decay 0.5 . 600)

--------------------------------------------------------------------------------
sin :: Monad m => Wire e m Double Double
sin = go 0

 where

  go theta' = mkPure $ \dt frequency ->
    let theta = theta' + 2 * pi * frequency * dt
    in (Right (Prelude.sin theta), go theta)

--------------------------------------------------------------------------------
render :: Int -> WireM IO () Double -> IO Chunk
render sampleRate wire = do
  let session = counterSession (recip $ fromIntegral sampleRate)
  buffer <- pack <$> go wire session
  unsafeUseAsCString buffer $ \buf -> do
    sdlBuf <- mallocBytes (length buffer)
    copyBytes sdlBuf buf (length buffer)
    p <- new (FChunk 1 (castPtr sdlBuf) (fromIntegral $ length buffer) 128)
    mkFinalizedChunk (castPtr p)

 where

  go :: Wire e IO () Double -> Session IO -> IO [Word8]
  go w' s' = do
    (r, w, s) <- stepSession w' s' ()
    case r of
      Left _ -> return []
      Right sample ->
        let digitized = round $ (sample + 1) * fromIntegral ((maxBound :: Word8) `div` 2)
        in digitized `seq` (digitized :) <$> go w s
