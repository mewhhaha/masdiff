module MSDF.Binary
  ( ByteBuffer(..)
  , readByteBuffer
  , slice
  , readU8
  , readU16BE
  , readS16BE
  , readU32BE
  , readS32BE
  , readTag
  ) where

import Data.Bits ((.|.), shiftL)
import Data.Char (chr)
import Data.Int (Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import System.IO (Handle, IOMode(ReadMode), withBinaryFile, hFileSize, hGetBuf)
import System.IO.Unsafe (unsafePerformIO)

-- | A view into a byte buffer.
data ByteBuffer = ByteBuffer
  { bbPtr :: ForeignPtr Word8
  , bbLen :: Int
  , bbOff :: Int
  }

readByteBuffer :: FilePath -> IO ByteBuffer
readByteBuffer path = withBinaryFile path ReadMode $ \h -> do
  size <- hFileSize h
  let len = fromIntegral size
  fptr <- mallocForeignPtrBytes len
  withForeignPtr fptr $ \p -> do
    readAll h p len
    pure (ByteBuffer fptr len 0)

slice :: ByteBuffer -> Int -> Int -> ByteBuffer
slice bb off len
  | off < 0 || len < 0 || off + len > bbLen bb = error "slice: out of bounds"
  | otherwise =
      let off' = bbOff bb + off
      in ByteBuffer (bbPtr bb) len off'

readU8 :: ByteBuffer -> Int -> Word8
readU8 bb i
  | i < 0 || i >= bbLen bb = error "readU8: out of bounds"
  | otherwise = unsafePerformIO $ withForeignPtr (bbPtr bb) $ \p ->
      peekByteOff p (bbOff bb + i)
{-# NOINLINE readU8 #-}

readAll :: Handle -> Ptr a -> Int -> IO ()
readAll h p len = go 0
  where
    go off
      | off >= len = pure ()
      | otherwise = do
          n <- hGetBuf h (p `plusPtr` off) (len - off)
          if n <= 0
            then error "readByteBuffer: short read"
            else go (off + n)

readU16BE :: ByteBuffer -> Int -> Word16
readU16BE bb i =
  let b0 = fromIntegral (readU8 bb i) :: Word16
      b1 = fromIntegral (readU8 bb (i + 1)) :: Word16
  in (b0 `shiftL` 8) .|. b1

readS16BE :: ByteBuffer -> Int -> Int16
readS16BE bb i = fromIntegral (readU16BE bb i)

readU32BE :: ByteBuffer -> Int -> Word32
readU32BE bb i =
  let b0 = fromIntegral (readU8 bb i) :: Word32
      b1 = fromIntegral (readU8 bb (i + 1)) :: Word32
      b2 = fromIntegral (readU8 bb (i + 2)) :: Word32
      b3 = fromIntegral (readU8 bb (i + 3)) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3

readS32BE :: ByteBuffer -> Int -> Int32
readS32BE bb i = fromIntegral (readU32BE bb i)

readTag :: ByteBuffer -> Int -> String
readTag bb off =
  let b0 = readU8 bb off
      b1 = readU8 bb (off + 1)
      b2 = readU8 bb (off + 2)
      b3 = readU8 bb (off + 3)
  in map (chr . fromIntegral) [b0, b1, b2, b3]
