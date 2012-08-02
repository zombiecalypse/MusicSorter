module MuSo.CopyFiles (copyFilesToLibrary) where
import MuSo.PathFormat
import MuSo.Config
import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.Maybe

discover :: FilePath -> IO [FilePath]
discover f = do
	file <- doesFileExist f
	dir  <- doesDirectoryExist  f
	if file 
		then return [f]
		else if dir 
			then do
						contents <- getDirectoryContents f
						walked <- mapM discover $ map (f </>) $ filter (not . flip elem [".",".."]) contents
						return $ concat walked
			else do
					putStrLn $ "No such file..." ++ f 
					return []
		
copyFilesToLibrary :: [FilePath] -> ReaderT Config IO ()
copyFilesToLibrary l = do
	read <- lift $ mapM discover l
	forM_ (concat read) copyFileToLibrary

mkdir_p = createDirectoryIfMissing True 

copyFileToLibrary :: FilePath -> ReaderT Config IO ()
copyFileToLibrary s = do
			path <- runMaybeT $ pathOf s
			case path of 
					Nothing -> putNoSuchFile
					Just a -> copyExistingFile a 
			where
				putNoSuchFile = lift $ putStrLn $ s++ " has no valid tag!"
				copyExistingFile path = do
							lift $ putStrLn $ s ++ " >>>> " ++ path
							let dirname = takeDirectory path
							lift $ mkdir_p dirname
							lift $ copyFile s path
