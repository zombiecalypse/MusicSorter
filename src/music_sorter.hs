import Sound.TagLib
import System.Environment (getArgs)
import System.Console.GetOpt 
import Control.Monad 
import Control.Monad.Error
import Control.Monad.Reader
import Data.Maybe (fromJust)

import MuSo.PathFormat
import MuSo.CopyFiles
import qualified MuSo.Config as Config

data Options = Options {
	library :: FilePath
} deriving (Eq, Show)

options = [
	Option "l" ["library"] (ReqArg (\ d o -> return o { library = d }) "DIR") "Directory in which to place the files" ]

defaultOptions = Options { library = Config.library Config.default_config }

parseArgs = do
	args <- getArgs
	let ( actions, nonOpts, messages) = getOpt Permute options args
	options <- foldl (>>=) (return defaultOptions) actions
	return (options, nonOpts, messages)


notifyErrors [] = return ()
notifyErrors msgs = do 
	forM_ msgs putStr
	putStr $ usageInfo header options
	fail "Errors occured"
				where header = "USAGE: music-sorter [OPTIONS] FILE..."
main = do
	(opts, toRead, msgs) <- parseArgs
	notifyErrors msgs
	runReaderT (copyFilesToLibrary toRead) (Config.default_config {Config.library = library opts})
  {-
	 -tags <- mapM (\s -> tagOf s >>= title ) toRead
	 -forM_ tags putStrLn
   -}
