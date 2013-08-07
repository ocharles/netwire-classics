{-# LINE 1 "Chunk.hsc" #-}
module ForeignChunk where
{-# LINE 2 "Chunk.hsc" #-}


{-# LINE 4 "Chunk.hsc" #-}

{-# LINE 5 "Chunk.hsc" #-}

import Data.Word
import Foreign
import Foreign.C.Types

data FChunk = FChunk { chunkAllocated :: CInt, chunkBuf :: Ptr Word8, chunkLen :: Word32, chunkVol :: Word8 }

instance Storable FChunk where
  sizeOf _ = ((24))
{-# LINE 15 "Chunk.hsc" #-}
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    allocated <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 18 "Chunk.hsc" #-}
    abuf <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 19 "Chunk.hsc" #-}
    alen <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 20 "Chunk.hsc" #-}
    volume <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 21 "Chunk.hsc" #-}
    return $ FChunk allocated abuf alen volume
  poke ptr (FChunk al buf len vol) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr al
{-# LINE 24 "Chunk.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr buf
{-# LINE 25 "Chunk.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr len
{-# LINE 26 "Chunk.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr vol
{-# LINE 27 "Chunk.hsc" #-}


