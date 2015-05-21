{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
module System.IO.Machine where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IOData (IOData, hGetLine, hPutStrLn)
import Data.Machine
import Data.Word (Word8)
import System.IO (Handle, hIsEOF)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8

type DataModeIO m a = MonadIO m => ((Handle -> m a), (Handle -> a -> m ()))
type SinkIO m k = MonadIO m => forall a. ProcessT m k a
type SourceIO m a = MonadIO m => forall k. MachineT m k a

type IODataMode a = ((Handle -> IO a), (Handle -> a -> IO ()))
type IOSink k = forall a. ProcessT IO k a
type IOSource a = forall k. MachineT IO k a

byChar :: IODataMode Char
byChar = (\h -> BSC8.head <$> BSC8.hGet h 1, \h w -> BSC8.hPut h $ BSC8.pack [w])

byWord8 :: IODataMode Word8
byWord8 = (\h -> BS.head <$> BS.hGet h 1, \h w -> BS.hPut h $ BS.pack [w])

byLine :: IOData a => DataModeIO m a
byLine = (hGetLine, hPutStrLn)

sourceIO :: IO a -> SourceIO m a
sourceIO f = repeatedly $ liftIO f >>= yield

sourceHandle :: DataModeIO m a -> Handle -> SourceIO m a
sourceHandle (r, _) = sourceHandleWith r

sourceIOWith :: m r -> (r -> m Bool) -> (r -> m a) -> SourceIO m a
sourceIOWith acquire release read' = MachineT $ do
  r         <- acquire
  released  <- release r
  if released then
    return Stop
  else do
    x <- read' r
    return . Yield x $ sourceIOWith acquire release read'

sourceHandleWith :: (Handle -> m a) -> Handle -> SourceIO m a
sourceHandleWith f h = sourceIOWith (return h) (liftIO . hIsEOF) f

sinkIO :: (a -> IO ()) -> SinkIO m a
sinkIO f = repeatedly $ await >>= liftIO . f

sinkHandle :: IOData a => IODataMode a -> Handle -> SinkIO m a
sinkHandle (_, w) h = repeatedly $ await >>= liftIO . w h

sinkHandleWith :: IOData a => (Handle -> a -> IO ()) -> Handle -> SinkIO m a
sinkHandleWith f h = repeatedly $ await >>= liftIO . f h

filteredIO :: (a -> IO Bool) -> ProcessT IO a a
filteredIO p = repeatedly $ do
  i <- await
  x <- liftIO $ p i
  if x then yield i else return ()

printer :: Show a => SinkIO m a
printer = sinkIO $ liftIO . print
