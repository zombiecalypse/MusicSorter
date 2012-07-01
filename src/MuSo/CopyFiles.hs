module MuSo.CopyFiles (copyFilesToLibrary) where
import MuSo.PathFormat
import MuSo.Config
import System.Directory
import System.FilePath
import Control.Monad.Reader

discover :: FilePath -> IO [FilePath]
discover f = do
	file <- doesFileExist f
	if file 
		then return [f]
		else do
						contents <- getDirectoryContents f
						walked <- mapM discover $ map (f </>) $ filter (not . flip elem [".",".."]) contents
						return $ concat walked
		
copyFilesToLibrary :: [FilePath] -> ReaderT Config IO ()
copyFilesToLibrary l = do
	read <- lift $ mapM discover l
	forM_ (concat read) copyFileToLibrary

mkdir_p = createDirectoryIfMissing True 

copyFileToLibrary s = do
			path <- pathOf s
			lift $ putStrLn $ s ++ " >>>> " ++ path
			let dirname = takeDirectory path
			lift $ mkdir_p dirname
			lift $ copyFile s path
