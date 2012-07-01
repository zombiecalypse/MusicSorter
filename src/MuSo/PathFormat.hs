module MuSo.PathFormat (pathOf) where
import Control.Monad.Reader
import Data.Maybe
import Sound.TagLib
import System.FilePath
import Text.Printf (printf)

import MuSo.Config

tagOf :: String -> IO Tag
tagOf s = do
	tagfile <- open s
	tag_ <- tag (fromJust tagfile)
	return $ fromJust tag_

pathOf :: FilePath -> ReaderT Config IO FilePath
pathOf s = do
	let extension = takeExtension s
	format <- asks pathFormat
	tag <- lift $ tagOf s
	let specFormat =  mapM (translateDirective tag) format
	inlib <- lift $ (liftM concat) specFormat
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
