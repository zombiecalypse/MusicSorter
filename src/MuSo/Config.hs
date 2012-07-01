module MuSo.Config where

type LibraryPath = String

type PathFormat = [Directive]

data Directive = Artist | Album | Title | Number | Literal String | Folder

ext :: [Directive]
ext = []

data Config = Config {
		library :: LibraryPath,
		pathFormat :: PathFormat
	}

default_config = Config {
	library = "/mount/multimedia/music/",
	pathFormat = Artist:Folder:Album:Folder:Number:Literal " - ":Title:ext
}
