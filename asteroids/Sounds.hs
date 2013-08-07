module Sounds (render, explosion) where

import Prelude hiding ((.), id, length)
import Data.ByteString hiding (map, take, putStrLn)
import Foreign
import Graphics.UI.SDL.Mixer
import ForeignChunk
import Control.Wire hiding (empty, force, noise, sample)

import Data.Monoid

--------------------------------------------------------------------------------
-- | Noise within +/- 1
noise :: (Monad m, MonadRandom m) => Wire e m a Double
noise = noiseRM . pure (-1, 1)

--------------------------------------------------------------------------------
-- | Quantize a signal. Left input is the signal, right input is the
-- quantization factor (0 - 1).
quantize :: Wire e m (Double, Double) Double
quantize = mkFix $ \_ (s, ratio) ->
  Right $ ratio * (fromIntegral (floor (s / ratio) :: Int))

--------------------------------------------------------------------------------
-- | Reduce sampling frequency. Left input is the signal, right input is
-- required sample rate.
rateReduce :: (Monad m, Monoid e) => Wire e m (Double, Int) Double
rateReduce = mkPure $ \_ (s, rate) ->
  (Right s, (keep . arr fst >>> for (recip $ fromIntegral rate)) --> rateReduce)

--------------------------------------------------------------------------------
-- | Decay a single over a period of time, inhibiting after.
decay :: (Monoid e, Monad m) => Time -> Wire e m Double Double
decay duration = for duration . decayer
  where decayer = (*) <$> integral_ 1 . pure ((-1) / duration) <*> id

--------------------------------------------------------------------------------
-- | Gate a signal to only pass above a certain threshold. Left input is the
-- signal to gate, right input is the gate threshold.
gate :: (Num a, Ord a) => Wire e m (a, a) a
gate = mkFix $ \_ (s, threshold) -> Right $ if s >= threshold then s else 0

--------------------------------------------------------------------------------
explosion :: (Monoid e, Monad m, MonadRandom m) => Wire e m a Double
explosion = decay 2 . gate . (rateReduce &&& 0.4) . (quantize &&& 500) . (noise &&& 0.2)

--------------------------------------------------------------------------------
render :: Int -> WireM IO () Double -> IO Chunk
render sampleRate wire = do
  let session = counterSession (recip $ fromIntegral sampleRate)
  buffer <- pack <$> go wire session
  useAsCString buffer $ \buf -> do
  alloca $ \p -> do
    poke p (FChunk 1 (castPtr buf) (fromIntegral $ length buffer) 128)
    mkFinalizedChunk (castPtr p)

 where

  go :: Wire e IO () Double -> Session IO -> IO [Word8]
  go w' s' = do
    (r, w, s) <- stepSession w' s' ()
    case r of
      Left _ -> return []
      Right sample ->
        let digitized = round $ (sample + 1) * (fromIntegral $ (maxBound :: Word8) `div` 2)
        in digitized `seq` (digitized :) <$> go w s

{----------------------------------------------------------------------------------}
{-main :: IO ()-}
{-main = do-}
  {-let sampleRate = 44100-}
  {-chunk <- render sampleRate explosion-}
  {-openAudio sampleRate AudioU8 2 512-}
  {-tryPlayChannel 0 chunk 0 >>= print-}
  {-threadDelay 10000000-}
