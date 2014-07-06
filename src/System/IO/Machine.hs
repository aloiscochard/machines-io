{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
module System.IO.Machine where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IOData (IOData, hGetLine, hPutStrLn)
import Data.Machine
import System.IO (Handle, IOMode(ReadMode), hClose, hIsEOF, openBinaryFile)

data IODataMode a = IODataMode (Handle -> IO a) (Handle -> a -> IO ())

type IOSink k = forall a. ProcessT IO k a
type IOSource a = forall k. MachineT IO k a

byLine :: IOData a => IODataMode a
byLine = IODataMode hGetLine hPutStrLn

sourceIO :: MonadIO m => m r -> (r -> m Bool) -> (r -> m a) -> forall k. MachineT m k a
sourceIO acquire release read = MachineT $ do
  r         <- acquire
  released  <- release r
  if released then
    return Stop
  else do
    x <- read r
    return . Yield x $ sourceIO acquire release read

sourceHandle :: IODataMode a -> Handle -> IOSource a
sourceHandle (IODataMode r _) = sourceHandleWith r

sinkHandle :: IOData a => IODataMode a -> Handle -> IOSink a
sinkHandle (IODataMode _ w) h = repeatedly $ await >>= \x -> liftIO $ w h x

sourceHandleWith :: (Handle -> IO a) -> Handle -> IOSource a
sourceHandleWith f h = sourceIO (return h) hIsEOF f

sinkHandleWith :: IOData a => (Handle -> a -> IO ()) -> Handle -> IOSink a
sinkHandleWith f h = repeatedly $ await >>= \x -> liftIO $ f h x
