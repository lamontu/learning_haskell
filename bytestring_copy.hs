import System.Environment
import qualified Data.ByteString.Lazy as L

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- L.readFile source
    L.writeFile dest contents
