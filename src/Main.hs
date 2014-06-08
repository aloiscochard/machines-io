import Data.ByteString (ByteString)
import Data.Machine
import Data.Machine.IO
import System.IO (IOMode(WriteMode), stdin, stdout, withBinaryFile)

import qualified Data.ByteString as BS

echoInOut :: MachineT IO k ()
echoInOut = sinkHandleByLine stdout <~ (sourceHandleByLine stdin :: IOSource ByteString)

-- TODO Don't use `byLine`
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = withBinaryFile dst WriteMode $ \hDst -> do
  runMachineT $ sinkHandleByLine hDst <~ (sourceFileByLine src :: IOSource ByteString)
  return ()

main :: IO ()
main = do
  runMachineT echoInOut
  putStrLn "Finished!"
