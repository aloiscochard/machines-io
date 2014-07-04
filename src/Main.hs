import Data.ByteString (ByteString)
import Data.Machine
import Data.Machine.IO
import System.IO (IOMode(WriteMode), stdin, stdout, withBinaryFile)

import qualified Data.ByteString as BS

echoInOut :: IODataMode a -> MachineT IO k ()
echoInOut m = sinkHandle m stdout <~ (sourceHandle m stdin :: IOSource ByteString)

copyFile :: IODataMode a -> FilePath -> FilePath -> IO ()
copyFile m src dst = withBinaryFile dst WriteMode $ \hDst -> do
  runMachineT $ sinkHandle m hDst <~ sourceFile m src
  return ()

main :: IO ()
main = do
  runMachineT $ echoInOut (byLine :: IODataMode ByteString)
  putStrLn "Finished!"
