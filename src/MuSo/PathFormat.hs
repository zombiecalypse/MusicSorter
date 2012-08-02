module MuSo.PathFormat (pathOf) where
import Control.Monad.Reader
import Control.Monad.Maybe
import Data.Maybe
import Sound.TagLib
import System.FilePath
import Text.Printf (printf)

import MuSo.Config

tagOf :: String -> MaybeT (ReaderT Config IO) Tag
tagOf s = do
	tagfile <- MaybeT $ lift $ open s
	MaybeT $ lift $ tag tagfile

pathOf :: FilePath -> MaybeT (ReaderT Config IO) FilePath
pathOf s = do
	let extension = takeExtension s
	format <- asks pathFormat
	tag <- tagOf s
	let specFormat =  lift $ mapM (translateDirective tag) format
	inlib <- lift $ liftM concat specFormat
	lib <- asks library
	let libsplit = splitDirectories lib
	return $ normalise $ lib </> (inlib ++ extension)

translateDirective :: Tag -> Directive -> IO FilePath
translateDirective tag (Literal s) = return s
translateDirective tag Artist = artist tag
translateDirective tag Album = album tag
translateDirective tag Title = title tag
translateDirective tag Number = liftM (printf "%02d") $ track tag
translateDirective tag Folder = return [pathSeparator]

example = Artist:Folder:Album:Folder:Number:Literal " - ":Title:ext
