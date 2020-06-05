#!c:/Program Files/Hugs98/runhugs

module Main (main) where

import CGI
import Directory
import System
import Monad
import Trace

-- Configuration variables

rootDirectory = "c:/Program Files/Apache Group/Apache2/htdocs/photos"
baseUrl = "/photos"
thumbnailDirectories = ["thumbnails"]
imageDirectories = ["images"]
ignoreDirectories = [".", "..", "pages"] ++ imageDirectories ++ thumbnailDirectories
imageExtensions = [".jpeg", ".jpg", ".gif"]

-- Main Wrapper

main :: IO ()
main = do queryString <- safeGetEnv "QUERY_STRING" ""
          let query = parseQuery queryString
              directoryName = if length queryString > 0 then fst (head query) else ""              
              actualDirName = dirConcat [rootDirectory, directoryName]
              scriptName = valFromQuery "SCRIPT_NAME" query
              in do album <- albumFromDirectory ""
                    wrapper (\env -> do return (Content{mime = buildGoatPhoto env
                                                                              directoryName
                                                                              album
                                                                              scriptName}) )
          
-- --------------------------------------------------------------------------------
-- buildGoatPhoto main html creation function
buildGoatPhoto env dirName album scriptName=
    page ("GoatPhoto - " ++ displayName)
         []
         (   [h3 ("Current Album: " ++ displayName)]
          ++ [table "" [] [[   [ul [albumTree scriptName dirName album]  ],
                               (albumContents scriptName dirName [album] )  ]]   ]
         )
    where displayName = if length dirName > 0 then dirName else "/"
    
albumTree :: String -> String -> Album -> [HTML]
albumTree scriptName dirName album  = [href (scriptName ++ "?" ++ (subdirname album)) [prose displayName]]
                                      ++ [ul (map (albumTree scriptName dirName) (subalbums album))]
                                      where displayName = if length (albumname album) > 0 then (albumname album) else "/"

                                      
albumContents :: String -> String -> [Album] -> [HTML]
albumContents _          _       []     = []
albumContents scriptName dirName (x:xs) = if (subdirname x) == dirName
                                             then outputImages scriptName (images x)
                                             else (albumContents scriptName dirName xs)
                                                  ++ (albumContents scriptName dirName (subalbums x))

outputImages :: String -> [Image] -> [HTML]
outputImages _          []     = []
outputImages scriptName (x:xs) =   (href (dirConcat [baseUrl, imagepath x]) [prose (imagename x)])
                                 : br : (outputImages scriptName xs)
 


            
-- --------------------------------------------------------------------------------
-- Main Data Strucuture

data Image = Image{imagename :: String,
                   imagepath :: String,
                   thumbpath :: String,
                   caption   :: String}
             deriving (Show)

data Album = Album{albumname   :: String,
                   subdirname  :: String,
                   subalbums   :: [Album],
                   images      :: [Image]}
             deriving (Show)
                    
albumFromDirectory :: String -> IO Album
albumFromDirectory subDirName = do dirContents <- getDirectoryContents (dirConcat [rootDirectory, subDirName])
                                   subAlbums <- processDirectories subDirName dirContents
                                   images <- processImages subDirName dirContents
                                   return (Album (getLastSubdir subDirName)
                                                 subDirName
                                                 subAlbums
                                                 images)
                                  
processDirectories :: String -> [String] -> IO [Album]
processDirectories _          []     = return []
processDirectories subDirName (x:xs) = do dirExists <- doesDirectoryExist (dirConcat [rootDirectory, subDirName, x])
                                          if dirExists && (False == elem x ignoreDirectories)
                                             then do thisDir <- albumFromDirectory (dirConcat [subDirName, x])
                                                     otherDirs <- processDirectories subDirName xs
                                                     return (thisDir : otherDirs)
                                             else processDirectories subDirName xs
                                                
processImages :: String -> [String] -> IO [Image]
processImages _          []     = return []
processImages subDirName (x:xs) = do fileExists <- doesFileExist (dirConcat [rootDirectory, subDirName, x])
                                     dirExists <- doesDirectoryExist (dirConcat [rootDirectory, subDirName, x])
                                     if fileExists && (stringMatchesOneOf x imageExtensions)
                                        then do thisImage <- imageFromFile (dirConcat [subDirName, x])
                                                otherImages <- processImages subDirName xs
                                                return (thisImage : otherImages)
                                        else if dirExists && (elem x imageDirectories)
                                                then do dirContents <- getDirectoryContents (dirConcat [rootDirectory, subDirName, x])
                                                        subDirImages <- processImages (dirConcat [subDirName, x]) dirContents
                                                        otherImages <- processImages subDirName xs
                                                        return (subDirImages ++ otherImages)
                                                else processImages subDirName xs
                                            
imageFromFile :: String -> IO Image
imageFromFile path = return (Image (getLastSubdir path)
                                   path
                                   "" -- no thumb path yet
                                   "") -- no caption yet
                                     

-- --------------------------------------------------------------------------------
-- File manipulation

extractFiles :: FilePath -> [FilePath] -> IO [FilePath]
extractFiles dir xs = filterM (\x -> doesFileExist (dirConcat [dir, x])) xs

extractDirectories :: FilePath -> [FilePath] -> IO [FilePath]
extractDirectories dir xs = filterM (\x -> doesDirectoryExist (dirConcat [dir, x])) xs

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
                    
