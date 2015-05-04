
module Termbox.Internal.Buffer (
    Buffer(..)
  , newEmptyBuffer
  , append
  ) where

import Data.Vector.Unboxed.Mutable as V
import Data.IORef
import Data.Word

-------------------------------------------------------------------------------
data Buffer a = Buffer {
      buf_next_free :: !(IORef Int)
    , buf_content :: !(V.IOVector a)
    }

-------------------------------------------------------------------------------
newEmptyBuffer :: Int -> IO (Buffer Word8)
newEmptyBuffer capacity = do
    idx <- newIORef 0
    nV <- V.replicate capacity 0
    return $ Buffer idx nV

-------------------------------------------------------------------------------
append :: Unbox a => a -> Buffer a -> IO ()
append content (Buffer ref buff) = do
    writeIdx <- readIORef ref
    V.write buff writeIdx content
    writeIORef ref (writeIdx + 1)

