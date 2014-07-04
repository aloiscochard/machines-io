{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE Rank2Types #-}
module System.IO.Machine where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IOData (IOData, hGetLine, hPutStrLn)
import Data.Machine
import System.IO (Handle, IOMode(ReadMode), hClose, hIsEOF, openBinaryFile)

-- TODO Test coverage
-- alternatively, one can just use System.IO.withFile
-- TODO Is it correct to use openBinaryFIle in sourceFileWith?

type IOSink k = forall a. ProcessT IO k a
type IOSource a = forall k. MachineT IO k a

sourceIO :: IO r -> (r -> IO Bool) -> (r -> IO a) -> IOSource a
sourceIO acquire release read = MachineT $ do
  r         <- acquire
  released  <- release r
  if released then
    return Stop
  else do
    x <- read r
    return . Yield x $ sourceIO acquire release read

sourceHandleWith :: (Handle -> IO a) -> Handle -> IOSource a
sourceHandleWith f h = sourceIO (return h) hIsEOF f

sourceHandleByLine :: IOData a => Handle -> IOSource a
sourceHandleByLine = sourceHandleWith hGetLine

sourceFileWith :: (Handle -> IO a) -> FilePath -> IOSource a
sourceFileWith f fp = sourceIO acquire release f where
  acquire = openBinaryFile fp ReadMode
  release h = do
    isEOF <- hIsEOF h
    when isEOF $ hClose h
    return isEOF

sourceFileByLine :: IOData a => FilePath -> IOSource a
sourceFileByLine fp = sourceFileWith hGetLine fp

sinkHandleWith :: IOData a => (Handle -> a -> IO ()) -> Handle -> IOSink a
sinkHandleWith f h = repeatedly $ await >>= \x -> liftIO $ f h x

sinkHandleByLine :: IOData a => Handle -> IOSink a
sinkHandleByLine = sinkHandleWith hPutStrLn
