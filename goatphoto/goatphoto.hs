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
ignoreDirectories = [".", "..", "pages", ".xvpics", "images", "originals", "thumbnails", "notused"]
ignoreFiles = []
albumsPerLine = 5 :: Int
imageExtensions = [".jpeg", ".jpg", ".gif", ".JPEG", ".JPG", ".GIF"]
imageDimensions = "640x400"

-- --------------------------------------------------------------------------------                                          
-- Windows Specific stuff - change for different OS

winDozeIfy :: String -> String
winDozeIfy = replaceChar '/' '\\'

-- This sequence creates an image whose minimum dimensions are at least 300x300,
-- crops out the middle 300x300, and resizes to 125x125
makeThumbnail :: FilePath -> IO ()
makeThumbnail path = do system ("mogrify -resize 300x300 " ++ imageName)
                        system ("mogrify -resize \"300x<\" " ++ imageName)
                        system ("mogrify -resize \"x300<\" " ++ imageName)
                        system ("mogrify -gravity Center -crop 300x300+0+0 " ++ imageName)
                        system ("mogrify -resize 125x125 " ++ imageName)
                        return ()
                     where imageName = "\"" ++ (winDozeIfy path) ++ "\""
                        

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
              unsafePhotoName = valFromQuery "photo" query
              unsafeAlbumName = valFromQuery "album" query
              albumName = if stringContains unsafeAlbumName ".."
                             then ""
                             else unsafeAlbumName
              photoName = if stringContains unsafePhotoName ".."
                             then ""
                             else unsafePhotoName
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
             title = Xml "title" [] [Text ("Goatpunch Photo Album - " ++ (albumdesc album) ++ " / " ++ (photodesc photo))]
             body = Xml "body" [] [buildPhotoBody album photo]

buildPhotoBody album photo = Xml "center" [] [maintable, xml_br, xml_emailLink]
                        where maintable = Xml "table" [("border", "1"), ("cellspacing", "0"), ("cellpadding", "10"), ("width", "100%")] rows
                              rows = [Xml "tr" [] cols,
                                      Xml "tr" [] [Xml "td" [("valign", "top"), ("align", "middle")] prevLink],
                                      Xml "tr" [] [Xml "td" [("valign", "top"), ("align", "middle")] nextLink]]
                              cols = [Xml "td" [("valign", "top")] albumNavigation,
                                      Xml "td" [("valign", "top"), ("rowspan", "3")] [buildImage photo]]
                              albumNavigation = [buildAlbumNavigation album (Just photo)]
                              prevLink = [Text "Previous: ", xml_br, xml_br, buildMaybeThumb (prevPhoto photo album)]
                              nextLink = [Text "Next: ", xml_br, xml_br, buildMaybeThumb (nextPhoto photo album)]

buildMaybeThumb :: Maybe Photo -> XML
buildMaybeThumb Nothing      = Text ""
buildMaybeThumb (Just photo) = buildThumbnail photo

buildXmlAlbum album =
       Xml "html" [] [head, body]
       where head = Xml "head" [] [title]
             title = Xml "title" [] [Text ("Goatpunch Photo Album - " ++ (albumdesc album))]
             body = Xml "body" [] [buildAlbumBody album]
             
buildAlbumBody album  = Xml "center" [] [maintable, xml_br, xml_emailLink]
                        where maintable = Xml "table" [("border", "1"), ("cellspacing", "0"), ("cellpadding", "10"), ("width", "100%")] rows
                              rows = [Xml "tr" [] cols]
                              cols = [Xml "td" [("valign", "top")] albumNavigation,
                                      Xml "td" [] ([Text (albumcaption album)] ++ albumCaptionIfHr ++ importAlbums ++ importAlbumsIfHr ++ photoDisplay)]
                              albumCaptionIfHr = if (albumcaption album) == "" || importAlbums == [] || photoDisplay == []
                                                    then []
                                                    else [xml_hr]
                              albumNavigation = [buildAlbumNavigation album Nothing]
                              importAlbumsIfHr = if importAlbums == [] || photoDisplay == []
                                                      then []
                                                      else [xml_hr]
                              importAlbums = buildImportAlbumDisp (importalbums album)
                              photoDisplay = buildThumbnailDisp (photos album)
            
           
buildAlbumNavigation album maybePhoto = navigateParentAlbums album (parentalbums album) maybePhoto True
navigateParentAlbums album [] maybePhoto isFirst
                              = if isFirst
                                   then Xml "dl" [] [Xml "dt" [] ([thisAlbum] ++ subAlbums)]
                                   else Xml "ul" [] [Xml "li" [] ([thisAlbum] ++ subAlbums)]
                                where thisAlbum = Xml "b" [] [albumLink album]
                                      subAlbums = subAlbumList (subalbums album) maybePhoto
navigateParentAlbums album (x:xs) maybePhoto isFirst
                              = if isFirst
                                   then Xml "dl" [] [Xml "dt" [] ([albumLink x, subAlbums])]
                                   else Xml "ul" [] [Xml "li" [] ([albumLink x, subAlbums])]
                                where subAlbums = navigateParentAlbums album xs maybePhoto False
subAlbumList _      (Just photo) = [Xml "ul" [] [Xml "li" [] [Xml "i" [] [buildTextImageLink photo]]]]
subAlbumList []     _            = []
subAlbumList albums _            = [Xml "ul" [] (navigateSubAlbums albums)]

navigateSubAlbums []     = []
navigateSubAlbums (x:xs) = [Xml "li" [] [albumLink x]] ++ (navigateSubAlbums xs)


buildThumbnailDisp []     = []
buildThumbnailDisp (x:xs) = xml_nbsp : (buildThumbnail x) : (buildThumbnailDisp xs)

buildImportAlbumDisp []     = []
buildImportAlbumDisp albums = [Xml "table" [("cellpadding", "5")] (buildImportAlbumRows albums)]
buildImportAlbumRows []     = []
buildImportAlbumRows albums = [Xml "tr" [] (buildImportAlbumCols (take albumsPerLine albums))] ++ (buildImportAlbumRows (drop albumsPerLine albums))
buildImportAlbumCols []     = []
buildImportAlbumCols (x:xs) = [Xml "td" [("valign","top"), ("align","middle")] ([buildImport x] ++ (buildImportAlbumCols xs))]

buildTextImageLink photo  = Xml "a" [("href", imagelink)] [Text (photodesc photo)]
                            where imagelink = "?album=" ++ (parentalbumname photo) ++ "&photo=" ++ (photoname photo)

buildThumbnail photo      = Xml "a" [("href", imagelink)] [image]
                            where imagelink = "?album=" ++ (parentalbumname photo) ++ "&photo=" ++ (photoname photo)
                                  image = Xml "img" [("src", thumburl photo), ("alt", photodesc photo), ("border", "0")] []

buildImage photo          = Xml "center" [] (photoCaptionWithHr ++ [link])
                            where photoCaptionWithHr = if photocaption photo == ""
                                                          then []
                                                          else [Xml "p" [] [Text (photocaption photo)], xml_hr, xml_br]
                                  link = Xml "a" [("href", imagelink)] [image, xml_br, xml_br, text]
                                  imagelink = originalurl photo
                                  image = Xml "img" [("src", imageurl photo), ("border", "0"), ("alt", "Click to view photo " ++ (photodesc photo) ++ " full-size")] []
                                  text = Text (photodesc photo)
                                  
                                  
buildImport album         = Xml "a" [("href", imagelink)] [image, xml_br, xml_br, text]
                            where imagelink = "?album=" ++ (albumname album)
                                  image = Xml "img" [("src", thumburl photo), ("border", "0"), ("alt", "Click to view album " ++ (albumdesc album))] []
                                  photo = head (photos album)
                                  text = Text (albumdesc album)

albumLink :: Album -> XML
albumLink album@Album{albumname=""} = Xml "a" [("href", "?")] [Text "Goatpunch Photo Album"]
albumLink album                     = Xml "a" [("href", albumLink)] [Text (albumdesc album)]
                                      where albumLink = "?album=" ++ (albumname album)

xml_hr = Xml "hr" [] []
xml_br = Xml "br" [] []
xml_nbsp = Text "&nbsp;"
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
                   photocaption     :: String}
             deriving (Read, Show)

data Album = Album{albumdesc    :: String,
                   albumname    :: String,
                   subalbums    :: [Album],
                   parentalbums :: [Album],
                   importalbums :: [Album],
                   photos       :: [Photo],
                   albumcaption :: String}
             deriving (Read, Show)
             
getValidPhoto :: String -> String -> IO Photo             
getValidPhoto albumName photoName = do ensureOriginalValid albumName photoName
                                       ensureThumbValid albumName photoName
                                       ensureImageValid albumName photoName
                                       realName <- getRealName albumName photoName
                                       caption <- getCaption albumName photoName
                                       return Photo{photodesc=realName,
                                                    photoname=photoName,
                                                    parentname=albumName,
                                                    parentalbumname=albumName,
                                                    originalurl=originalUrl,
                                                    imageurl=imageUrl,
                                                    thumburl=thumbUrl,
                                                    photocaption=caption}
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
                             realName <- getRealName (trimLastSubdir albumName) (getLastSubdir albumName)
                             caption <- getCaption albumName "."
                             importAlbums <- getImportAlbums albumName
                             return Album{albumdesc=realName,
                                          albumname=albumName,
                                          subalbums=albums,
                                          parentalbums = parentAlbums,
                                          importalbums = importAlbums,
                                          photos=photos,
                                          albumcaption=caption}
                          where albumPathDir = dirConcat [rootDirectory, albumName]
                                albumPathFiles = dirConcat [rootDirectory, albumName, "originals"]
                                         
getShellAlbum :: String -> IO Album
getShellAlbum albumName = do realName <- getRealName (trimLastSubdir albumName) (getLastSubdir albumName)
                             return Album{albumdesc=realName,
                                          albumname=albumName,
                                          subalbums=[],
                                          parentalbums = [],
                                          importalbums = [],
                                          photos=[],
                                          albumcaption=""}
       
getImportAlbums :: String -> IO [Album]
getImportAlbums albumName = do imports <- getImports albumName
                               mapM (getImportAlbum albumName) imports

getImportAlbum :: String -> (String, String) -> IO Album
getImportAlbum parentName (albumPath, photoPath) =
                           do realName <- getRealName albumDir subAlbumName
                              photo <- getValidPhoto photoDir photoName
                              caption <- getCaption albumName "."
                              return Album{albumdesc=realName,
                                           albumname=albumName,
                                           subalbums=[],
                                           parentalbums = [],
                                           importalbums = [],
                                           photos=[photo],
                                           albumcaption=caption}
                           where albumDir = if head albumPath == '/'
                                               then trimLastSubdir (removeLeadingSlash albumPath)
                                               else dirConcat [parentName, trimLastSubdir albumPath]
                                 subAlbumName = getLastSubdir albumPath
                                 albumName = if head albumPath == '/'
                                               then removeLeadingSlash albumPath
                                               else dirConcat[parentName, albumPath]
                                 photoDir = if head photoPath == '/'
                                               then trimLastSubdir (removeLeadingSlash photoPath)
                                               else dirConcat [parentName, trimLastSubdir photoPath]
                                 photoName = getLastSubdir photoPath

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
                                -- No real need for this is there? mapM (ensureImageValid albumName) originalValidFiles
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
render xml = render' xml
    where render' xml@Xml{contents=[]} = (openTag xml) ++ " />"
          render' xml@Xml{contents}    = (openTag xml) ++ ">" ++ (rContents contents) ++ (closeTag xml)
          render' Text{text} = text
          render' Comment{text} = "<!-- " ++ text ++ " -->\n"
          openTag xml = "<" ++ (tag xml) ++ (rAttributes (attributes xml))
          closeTag xml = "</" ++ (tag xml) ++ ">\n"
          rContents contents = foldl (\x y -> x ++ (render' y)) "" contents
          rAttributes []            = []
          rAttributes ((xn, xv):xs) = " " ++ xn ++ "=\"" ++ xv ++ "\"" ++ rAttributes xs
          
renderPretty xml = render' 0 xml
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

-- Ultralight parser
               
          
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
parseQuery input  = map splitOnEquals (splitToListOnChar '&' input)
                    
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
                      
splitOnSlash :: String -> (String, String)
splitOnSlash = splitOnChar '/'
                      
splitOnChar :: Char -> String -> (String, String)
splitOnChar char input = (x, snd(splitAt 1 xs))
                         where (x, xs) = break (\a -> a == char) input
                         
splitToListOnChar :: Char -> String -> [String]
splitToListOnChar _    []    = []
splitToListOnChar char input = x : (splitToListOnChar char xs)
                               where (x, xs) = splitOnChar char input

                         
splitOnChars :: String -> String -> (String, String)
splitOnChars test input = (x, snd(splitAt 1 xs))
                          where (x, xs) = break (\a -> elem a test) input
                          
replaceChar :: Char -> Char -> String -> String
replaceChar oldChar newChar = map (\x -> if x == oldChar then newChar else x)

prettyName = replaceChar '_' ' '

trimLeadingWhitespace [] = []
trimLeadingWhitespace (' ':xs) = trimTrailingWhitespace xs
trimLeadingWhitespace xs = xs
trimTrailingWhitespace xs = reverse (trimLeadingWhitespace (reverse xs))
trimAllWhitespace xs = trimTrailingWhitespace (trimLeadingWhitespace xs)

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
                 
removeLeadingSlash ('/':xs) = removeLeadingSlash xs
removeLeadingSlash xs       = xs
                 
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
                        in if reversedSub == []
                              then s
                              else reverse reversedSub
           
getRealName :: String -> String -> IO String         
getRealName albumName subName = do realNames <- getRealNames albumName
                                   let res = lookup subName realNames
                                       in case res of
                                               Nothing -> return (prettyName (removeExtension subName)) -- best we can do
                                               Just name -> return name

getCaption :: String -> String -> IO String         
getCaption albumName subName = do captions <- getCaptions albumName
                                  let caption = lookup subName captions
                                      in case caption of
                                              Nothing -> do return []
                                              Just caption -> return caption

getRealNames :: String -> IO [(String, String)]
getRealNames albumName = readDBFile (dirConcat [rootDirectory, albumName, "desc.txt"])
                                   
getCaptions :: String -> IO [(String, String)]
getCaptions albumName = readDBFile (dirConcat [rootDirectory, albumName, "caption.txt"])
                                   
getImports :: String -> IO [(String, String)]
getImports albumName = readDBFile (dirConcat [rootDirectory, albumName, "import.txt"])
                                   
readDBFile :: String -> IO [(String, String)]
readDBFile fileName = do fileExists <- doesFileExist fileName
                         if fileExists
                            then do contents <- readFile fileName
                                    return (map splitOnEquals (splitToListOnChar '\n' contents))
                               else return []
