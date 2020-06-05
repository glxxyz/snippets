#!c:/Program Files/Hugs98/runhugs -98

module Main (main) where

import Directory
import System
import Monad
import Trace
import IOExtensions

-- --------------------------------------------------------------------------------                                          
-- Configuration variables

rootDirectory = "c:/Program Files/Apache Group/Apache2/htdocs/photos"
rootUrl = "/photos"
ignoreDirectories = [".", "..", "pages", ".xvpics", "images", "originals", "thumbnails"]
ignoreFiles = []
imageExtensions = [".jpeg", ".jpg", ".gif", ".JPEG", ".JPG", ".GIF"]
photosPerLine = 4 :: Int
thumbnailDimensions = "150x150"
imageDimensions = "640x400"

-- --------------------------------------------------------------------------------                                          
-- Windows Specific stuff - change for different OS

winDozeIfy :: String -> String
winDozeIfy = replaceChar '/' '\\'

makeThumbnail :: FilePath -> IO ()
makeThumbnail path = do system ("mogrify -resize " ++ thumbnailDimensions++ " \"" ++ (winDozeIfy path) ++ "\"")
                        return ()

makeImage :: FilePath -> IO ()
makeImage path = do system ("mogrify -resize " ++ imageDimensions ++ " \"" ++ (winDozeIfy path) ++ "\"")
                    return ()
                           
copyFile :: FilePath -> FilePath -> IO ()                     
-- ugly, but quiet
copyFile source dest = do b <- readBinaryFile source
                          writeBinaryFile dest b
-- copyFile source dest = do system ("copy \"" ++ (winDozeIfy source) ++ "\" \"" ++ (winDozeIfy dest) ++ "\"")
--                          return ()

moveFile :: FilePath -> FilePath -> IO ()                     
moveFile source dest = do system ("move \"" ++ (winDozeIfy source) ++ "\" \"" ++ (winDozeIfy dest) ++ "\"")
                          return ()

-- --------------------------------------------------------------------------------                                          
-- Main
main :: IO ()
main = do queryString <- safeGetEnv "QUERY_STRING" ""
          runMain queryString

runMain queryString = 
       do putStr "Content-Type:text/html\n\n"
          let query = parseQuery queryString
              photoName = valFromQuery "photo" query
              albumName = valFromQuery "album" query
              in if photoName /= ""
                 then do photo <- getValidPhoto albumName photoName
                         album <- getValidAlbum albumName
                         outputPage (buildXmlPhoto album photo)
                 else do album <- getValidAlbum albumName
                         outputPage (buildXmlAlbum album)

outputPage xml = putStr (render xml)

-- --------------------------------------------------------------------------------
-- Build XML

buildXmlPhoto album photo =
       Xml "html" [] [head, body]
       where head = Xml "head" [] [title]
             title = Xml "title" [] [Text ("GoatPhoto - " ++ (photodesc photo))]
             body = Xml "body" [] (buildPhotoBody album photo)

buildPhotoBody album photo = [xml_hr] ++ albumNavigation ++ [xml_hr]
                         ++ photoDisplay ++ [xml_hr] ++ [xml_emailLink]
                        where albumNavigation = [buildAlbumNavigation album]
                              photoDisplay = [Xml "table" [] rows]
                              rows = [Xml "tr" [] cols]
                              cols = [Xml "td" [("valign", "top")] [Text "Prev: "],
                                      Xml "td" [("valign", "top")] [prevLink],
                                      Xml "td" [("valign", "top")] [buildImage photo],
                                      Xml "td" [("valign", "top")] [Text "Next: "],
                                      Xml "td" [("valign", "top")] [nextLink]]
                              prevLink = buildMaybeThumb (prevPhoto photo album)
                              nextLink = buildMaybeThumb (nextPhoto photo album)

buildMaybeThumb :: Maybe Photo -> XML
buildMaybeThumb Nothing      = Text "[none]"
buildMaybeThumb (Just photo) = buildThumbnail photo

buildXmlAlbum album =
       Xml "html" [] [head, body]
       where head = Xml "head" [] [title]
             title = Xml "title" [] [Text ("GoatPhoto - " ++ (albumdesc album))]
             body = Xml "body" [] (buildAlbumBody album)
             
buildAlbumBody album =     [xml_hr] ++ albumNavigation ++ [xml_hr] 
                        ++ photoDisplay ++ [xml_hr] ++ [xml_emailLink]
                       where albumNavigation = [buildAlbumNavigation album]
                             photoDisplay = buildThumbnailDisp (photos album)
            
            
           
buildAlbumNavigation album = navigateParentAlbums album (parentalbums album) 
navigateParentAlbums album [] = Xml "ul" [] [listContents]
                                where listContents = Xml "li" [] ([thisAlbum] ++ subAlbums)
                                      thisAlbum = Xml "b" [] [albumLink album]
                                      subAlbums = subAlbumList (subalbums album)
navigateParentAlbums album (x:xs) = Xml "ul" [] [listContents]
                                    where listContents = Xml "li" [] [albumLink x, subAlbums]
                                          subAlbums = navigateParentAlbums album xs                                          
subAlbumList []     = []
subAlbumList albums = [Xml "ul" [] (navigateSubAlbums albums)]
navigateSubAlbums []     = []
navigateSubAlbums (x:xs) = [Xml "li" [] [albumLink x]] ++ (navigateSubAlbums xs)

buildThumbnailDisp []     = []
buildThumbnailDisp photos = [Xml "table" [] (buildThumbnailRows photos)]
buildThumbnailRows []     = []
buildThumbnailRows photos = [Xml "tr" [] (buildThumbnailCols (take photosPerLine photos))] ++ (buildThumbnailRows (drop photosPerLine photos))
buildThumbnailCols []     = []
buildThumbnailCols (x:xs) = [Xml "td" [("valign","top")] ([buildThumbnail x] ++ (buildThumbnailCols xs))]

buildThumbnail photo      = Xml "a" [("href", imagelink)] [image, xml_br, text]
                            where imagelink = "?album=" ++ (parentalbumname photo) ++ "&photo=" ++ (photoname photo)
                                  image = Xml "img" [("src", thumburl photo)] []
                                  text = Text (photodesc photo)

buildImage photo          = Xml "a" [("href", imagelink)] [image, xml_br, text]
                            where imagelink = originalurl photo
                                  image = Xml "img" [("src", imageurl photo)] []
                                  text = Text (photodesc photo)

albumLink :: Album -> XML
albumLink album@Album{albumname=""} = Xml "a" [("href", "?")] [Text "album"]
albumLink album                     = Xml "a" [("href", albumLink)] [Text (albumdesc album)]
                                      where albumLink = "?album=" ++ (albumname album)

xml_hr = Xml "hr" [] []
xml_br = Xml "br" [] []
xml_emailLink = Xml "a" [("href", "mailto:alan@goatpunch.com")] [Text "alan@goatpunch.com"]

-- --------------------------------------------------------------------------------
-- Main Data Strucuture

data Photo = Photo{photodesc   :: String,
                   photoname   :: String,
                   parentname  :: String,
                   parentalbumname   :: String,
                   originalurl :: String,
                   imageurl    :: String,
                   thumburl    :: String,
                   caption     :: String,
                   nextphoto   :: String,
                   prevphoto   :: String}
             deriving (Show)

data Album = Album{albumdesc    :: String,
                   albumname    :: String,
                   subalbums    :: [Album],
                   parentalbums :: [Album],
                   photos       :: [Photo]}
             deriving (Show)
             

getValidPhoto :: String -> String -> IO Photo             
getValidPhoto albumName photoName = do ensureOriginalValid albumName photoName
                                       ensureThumbValid albumName photoName
                                       ensureImageValid albumName photoName
                                       return Photo{photodesc=prettyName (removeExtension photoName), -- for now
                                                    photoname=photoName,
                                                    parentname=albumName,
                                                    parentalbumname=albumName,
                                                    originalurl=originalUrl,
                                                    imageurl=imageUrl,
                                                    thumburl=thumbUrl,
                                                    caption="", -- for now
                                                    nextphoto = "", --for now
                                                    prevphoto = ""} -- for now
                                    where originalUrl  = dirConcat [rootUrl, albumName, "originals", photoName]
                                          imageUrl     = dirConcat [rootUrl, albumName, "images", photoName]
                                          thumbUrl     = dirConcat [rootUrl, albumName, "thumbnails", photoName]

getValidAlbum :: String -> IO Album
getValidAlbum albumName = do ensureAlbumValid albumName
                             dirContentsDir <- safeGetDirectoryContents albumPathDir
                             dirContentsFiles <- safeGetDirectoryContents albumPathFiles
                             files <- filterFiles albumPathFiles dirContentsFiles
                             directories <- filterDirectories albumPathDir dirContentsDir
                             albums <- mapM getShellAlbum (map (\x -> dirConcat [albumName, x]) (stripOutIgnoreDirectories directories))
                             photos <- mapM (getValidPhoto albumName) (stripOutIgnoreFiles files)
                             parentAlbums <- getParentAlbums albumName
                             return Album{albumdesc=prettyName (getLastSubdir albumName), -- for now
                                          albumname=albumName,
                                          subalbums=albums,
                                          parentalbums = parentAlbums,
                                          photos=photos}
                          where albumPathDir = dirConcat [rootDirectory, albumName]
                                albumPathFiles = dirConcat [rootDirectory, albumName, "images"]
                                         
getShellAlbum :: String -> IO Album
getShellAlbum albumName = do return Album{albumdesc=prettyName (getLastSubdir albumName), -- for now
                                          albumname=albumName,
                                          subalbums=[],
                                          parentalbums = [],
                                          photos=[]}
                          where albumPath = dirConcat [rootDirectory, albumName]
       
getParentAlbums :: String -> IO [Album]                   
getParentAlbums [] = return []
getParentAlbums albumName = getParentAlbums' (trimLastSubdir albumName)
    where getParentAlbums' []        = do thisAlbum <- getShellAlbum ""
                                          return [thisAlbum]
          getParentAlbums' albumName = do parentAlbums <- getParentAlbums' (trimLastSubdir albumName)
                                          thisAlbum <- getShellAlbum albumName
                                          return (parentAlbums ++ [thisAlbum])
                            
ensureAlbumValid albumName = do sourceDirContents <- safeGetDirectoryContents sourcePath
                                sourceFiles <- filterFiles sourcePath sourceDirContents
                                mapM (ensureOriginalValid albumName) (stripOutIgnoreFiles sourceFiles)
                                originalDirContents <- safeGetDirectoryContents originalPath
                                originalFiles <- filterFiles originalPath originalDirContents
                                originalValidFiles <- return (stripOutIgnoreFiles originalFiles)
                                mapM (ensureThumbValid albumName) originalValidFiles
                                mapM (ensureImageValid albumName) originalValidFiles
                                return ()
                             where sourcePath = dirConcat [rootDirectory, albumName]
                                   originalPath = dirConcat [rootDirectory, albumName, "originals"]
                                         
ensureOriginalValid albumName photoName = do moveExists <- doesFileExist movePath
                                             originalExists <- doesFileExist originalPath
                                             if moveExists && not originalExists
                                                then do ensureDirectory originalDir
                                                        moveFile movePath originalPath
                                                        return ()
                                                else return ()
                                          where movePath     = dirConcat [rootDirectory, albumName, photoName]
                                                originalDir  = dirConcat [rootDirectory, albumName, "originals"]
                                                originalPath = dirConcat [originalDir, photoName]
                                                                                          
                                              
ensureThumbValid albumName photoName =    do thumbExists <- doesFileExist thumbPath
                                             if not thumbExists
                                                then do ensureDirectory thumbDir
                                                        copyFile originalPath thumbPath
                                                        makeThumbnail thumbPath
                                                        return ()
                                                else return ()
                                          where originalDir  = dirConcat [rootDirectory, albumName, "originals"]
                                                originalPath = dirConcat [originalDir, photoName]
                                                thumbDir  = dirConcat [rootDirectory, albumName, "thumbnails"]
                                                thumbPath = dirConcat [thumbDir, photoName]
                                          
ensureImageValid albumName photoName    = do imageExists <- doesFileExist imagePath
                                             if not imageExists
                                                then do ensureDirectory imageDir
                                                        copyFile originalPath imagePath
                                                        makeImage imagePath
                                                        return ()
                                                else return ()
                                          where originalDir  = dirConcat [rootDirectory, albumName, "originals"]
                                                originalPath = dirConcat [originalDir, photoName]
                                                imageDir  = dirConcat [rootDirectory, albumName, "images"]
                                                imagePath = dirConcat [imageDir, photoName]

stripOutIgnoreDirectories [] = []                                                
stripOutIgnoreDirectories (x:xs) = if elem x ignoreDirectories
                                      then stripOutIgnoreDirectories xs
                                      else x : stripOutIgnoreDirectories xs
                                   
stripOutIgnoreFiles [] = []                                                
stripOutIgnoreFiles (x:xs) = if (elem x ignoreFiles) || not (stringMatchesOneOf x imageExtensions)
                                      then stripOutIgnoreFiles xs
                                      else x : stripOutIgnoreFiles xs

nextPhoto :: Photo -> Album -> Maybe Photo
nextPhoto photo album = afterMatchingPhoto photo (photos album)

prevPhoto :: Photo -> Album -> Maybe Photo
prevPhoto photo album = afterMatchingPhoto photo (reverse (photos album))

afterMatchingPhoto :: Photo -> [Photo] -> Maybe Photo
afterMatchingPhoto _     []       = Nothing
afterMatchingPhoto _     (x:[])   = Nothing
afterMatchingPhoto photo (x:xs) = if (photoname photo) == (photoname x)
                                      then Just (head xs)
                                      else afterMatchingPhoto photo xs

-- --------------------------------------------------------------------------------                                          
-- xml output
data XML =   Xml { tag :: String, attributes :: [(String,String)], contents :: [XML] }
           | Text{ text :: String }
           | Comment{text :: String}
           deriving (Show, Eq)
           
render :: XML -> String
render xml = render' 0 xml
    where render' indent xml@Xml{contents=[]} = (openTag indent xml) ++ " />\n"
          render' indent xml@Xml{contents}    = (openTag indent xml) ++ ">\n" ++ (rContents indent contents) ++ (closeTag indent xml)
          render' indent Text{text} = (spaces indent) ++ text ++ "\n"
          render' indent Comment{text} = (spaces indent) ++ "<!-- " ++ text ++ " -->\n"
          openTag indent xml = (spaces indent) ++ "<" ++ (tag xml) ++ (rAttributes (attributes xml))
          closeTag indent xml = (spaces indent) ++ "</" ++ (tag xml) ++ ">\n"
          rContents indent contents = foldl (\x y -> x ++ (render' (indent+4) y)) "" contents
          rAttributes []            = []
          rAttributes ((xn, xv):xs) = " " ++ xn ++ "=\"" ++ xv ++ "\"" ++ rAttributes xs
          spaces indent = take indent (repeat ' ')
          
-- --------------------------------------------------------------------------------
-- html examples
           
page = Xml "html" [] [head, body]
       where head = Xml "head" [] [title]
             title = Xml "title" [] [titletext]
             titletext = Text "Alan's test page"
             body = Xml "body" [] [heading, hr, bodytext, hr, emaillink]
             hr = Xml "hr" [] []
             heading = Xml "h2" [] [Text "Alan's test"]
             bodytext = Xml "p" [] [Text "This is a test. Yo mama."]
             emaillink = Xml "a" [("href", "mailto:alan@goatpunch.com"), ("dummy", "test")] [Text "mail me"]
             
page2 = Xml "allalbums" [] [albumList, albumContents]
        where albumList = Xml "albuminfo" [("id", "my_album"), ("name", "My Album")] []
              albumContents = Xml "albumcontents" [] [comment, photo1, photo2]
              comment = Comment "Here it is!"
              photo1 = Xml "photo" [("name", "myphoto1"), ("data", "test")] [Text "My Photo 1"]
              photo2 = Xml "photo" [("name", "myphoto2")] [Text "My Photo 2"]

-- --------------------------------------------------------------------------------
-- CGI Stuff

-- Makes code cleaner if getenv returns default if variable doesn't exist
safeGetEnv :: String -> String -> IO String
safeGetEnv varname defaultval = catch (getEnv varname) (const (return defaultval))

-- Case INSENSITIVE!
valFromQuery :: String -> [(String, String)] -> String
valFromQuery _ []     = ""
valFromQuery p (x:xs) = if map toUpper (fst x) == map toUpper p
                        then snd x
                        else valFromQuery p xs

parseQuery :: String -> [(String, String)]
parseQuery ""     = []
parseQuery input  = splitOnEquals(x) : parseQuery xs
                    where (x, xs) = splitOnAmpersand input
                    
-- --------------------------------------------------------------------------------
-- String helper

stringContains :: String -> String -> Bool
stringContains test pat = stringContains' test pat pat
                          where
                            stringContains' :: String -> String -> String -> Bool
                            stringContains' _      []     _       = True
                            stringContains' []     _      _       = False
                            stringContains' (t:ts) (p:ps) pattern = (   (t == p) && (stringContains' ts ps pattern)
                                                                     || stringContains' ts pattern pattern)
                                                                     
stringMatchesOneOf :: String -> [String] -> Bool
stringMatchesOneOf _    []     = False
stringMatchesOneOf test (x:xs) = (stringContains test x) || (stringMatchesOneOf test xs)

splitOnEquals :: String -> (String, String)
splitOnEquals = splitOnChar '='
                      
splitOnAmpersand :: String -> (String, String)
splitOnAmpersand = splitOnChar '&'
                      
splitOnSlash :: String -> (String, String)
splitOnSlash = splitOnChar '/'
                      
splitOnChar :: Char -> String -> (String, String)
splitOnChar char input = (x, snd(splitAt 1 xs))
                         where (x, xs) = break (\a -> a == char) input
                         
splitOnChars :: String -> String -> (String, String)
splitOnChars test input = (x, snd(splitAt 1 xs))
                          where (x, xs) = break (\a -> elem a test) input
                          
replaceChar :: Char -> Char -> String -> String
replaceChar oldChar newChar = map (\x -> if x == oldChar then newChar else x)

prettyName = replaceChar '_' ' '

-- --------------------------------------------------------------------------------
-- File manipulation

safeGetDirectoryContents :: FilePath -> IO [FilePath]
safeGetDirectoryContents pathName = do dirExists <- doesDirectoryExist pathName
                                       if dirExists
                                          then getDirectoryContents pathName
                                          else return []
ensureDirectory :: FilePath -> IO ()
ensureDirectory pathName = do dirExists <- doesDirectoryExist pathName
                              if dirExists
                                 then return ()
                                 else do createDirectory pathName
                                         return ()                       

filterFiles :: FilePath -> [FilePath] -> IO [FilePath]
filterFiles dir xs = filterM (\x -> doesFileExist (dirConcat [dir, x])) xs

filterDirectories :: FilePath -> [FilePath] -> IO [FilePath]
filterDirectories dir xs = filterM (\x -> doesDirectoryExist (dirConcat [dir, x])) xs

removeTrailingSlash :: String -> String
removeTrailingSlash [] = ""
removeTrailingSlash path =
            case last path of
                 '/'  -> removeTrailingSlash (take ((length path)-1) path)
                 _    -> path
                 
addTrailingSlash :: String -> String
addTrailingSlash path = (removeTrailingSlash path) ++ "/"

dirConcat :: [String] -> String
dirConcat []      = []
dirConcat ("":xs) = dirConcat xs
dirConcat (x:xs)  = removeTrailingSlash((addTrailingSlash x) ++ (dirConcat xs))

getLastSubdir :: String -> String
getLastSubdir s = let reversed = reverse s
                      reversedSub = fst (splitOnSlash reversed)
                  in reverse reversedSub

trimLastSubdir :: String -> String
trimLastSubdir s = let reversed = reverse s
                       reversedSub = snd (splitOnSlash reversed)
                   in reverse reversedSub
                   
removeExtension :: String -> String
removeExtension s = let reversed = reverse s
                        reversedSub = snd (splitOnChar '.' reversed)
                    in reverse reversedSub

                          

                    
                    
