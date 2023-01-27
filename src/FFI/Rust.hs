module FFI.Rust (
    simpleMovingAverage,
    mean,
) where

import Data.Vector.Storable
import Data.Word
import Foreign
import System.IO.Unsafe

foreign import ccall "mean" rust_mean :: Ptr Double -> Word32 -> IO Double

foreign import ccall "simple_moving_average"
    rust_simple_moving_average :: Word32 -> Ptr Double -> Word32 -> Ptr Word32 -> IO (Ptr Double)

mean :: Vector Double -> Double
mean v =
    let (vFPtr, len) = unsafeToForeignPtr0 v
     in unsafePerformIO $
            withForeignPtr vFPtr $
                \vPtr -> rust_mean vPtr (fromIntegral len)

simpleMovingAverage :: Word32 -> Vector Double -> Vector Double
simpleMovingAverage k v =
    let (vFPtr, len) = unsafeToForeignPtr0 v
     in unsafePerformIO $
            alloca $ \lenPtr -> do
                withForeignPtr vFPtr $
                    \vPtr -> do
                        resPtr <- rust_simple_moving_average k vPtr (fromIntegral len) lenPtr
                        resLen <- fromIntegral <$> peek lenPtr
                        vecResPtr <- newForeignPtr finalizerFree resPtr
                        pure $ unsafeFromForeignPtr0 vecResPtr resLen
