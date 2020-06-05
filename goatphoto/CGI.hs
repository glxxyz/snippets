-- %
-- % Original Version by Erik Meijer <erik@cs.ruu.nl>
-- % Further hacked on by Sven Panne <Sven.Panne@informatik.uni-muenchen.de>
-- % De-literated and further hacked by Alan Davies <alan@goatpunch.com>
-- %
-- \section[CGI]{Haskell binding for CGI/HTTP/HTML}
-- 
-- 

module CGI (

        -- Interface HTML --------------------------------------------------
        Name, Value, URL, Tag, HTML, TextPlain,
        prose,                     -- :: String -> HTML
        attributedElement,         -- :: Tag -> [(Name,Value)] -> [HTML] -> HTML
        unclosedAttributedElement, -- :: Tag -> [(Name,Value)]           -> HTML
        element,                   -- :: Tag                   -> [HTML] -> HTML
        unclosedElement,           -- :: Tag                             -> HTML
        set,                       -- :: [(Name,Value)] -> (HTML -> HTML)

        -- Interface HTMLWizard --------------------------------------------
        Color, Face, Size, Widget, Wrap, CLSID,
        page,                 -- :: String -> [(Name,Value)] -> [HTML] -> HTML
        vanillaPage,          -- :: [HTML] -> HTML
        backgroundPage,       -- :: URL -> String -> [HTML] -> HTML
        format,               -- :: Tag -> String -> HTML
        h,                    -- :: Int -> String -> HTML
        h1,h2,h3,h4,h5,h6,h7, -- :: String -> HTML
        p,                    -- :: [HTML] -> HTML
        font,                 -- :: Color -> [Face] -> Size -> [HTML] -> HTML
        href,                 -- :: URL -> [HTML] -> HTML
        name,                 -- :: String -> [HTML] -> HTML
        img,                  -- :: String -> URL -> HTML
        ul, ol,               -- :: [[HTML]] -> HTML
        dl,                   -- :: [(String,[HTML])] -> HTML
        table,                -- :: String -> [String] -> [[ [HTML] ]] -> HTML
        widget,               -- :: Widget -> Name -> HTML
        checkbox,             -- :: Name -> Value -> HTML
        hidden,               -- :: Name -> Value -> HTML
        password,             -- :: Name -> HTML
        radio,                -- :: Name -> Value -> HTML
        reset,                -- :: Name -> Value -> HTML
        submit,               -- :: Name -> Value -> HTML
        textfield,            -- :: Name -> HTML
        file,                 -- :: Name -> HTML
        clickmap,             -- :: Name -> HTML
        group,                -- :: (Name -> Value -> HTML) -> Name -> [Value] -> HTML
        menu,                 -- :: Name -> [Value] -> HTML
        textarea,             -- :: Name -> Int -> Int -> Wrap -> Value -> HTML
        gui,                  -- :: URL -> [HTML] -> HTML
        object,               -- :: CLSID -> String -> [(Name,Value)] -> HTML
        br,                   -- :: HTML
        hr,                   -- :: HTML

        StatusCode, Reason, CgiOut(..),

        simpleWrapper,        -- :: Show a => ([(Name,Value)] -> a) -> IO ()
        wrapper               -- :: Show a => ([(Name,Value)] -> IO a) -> IO ()
       ) where

import Char ( ord, chr, toUpper, isDigit, isAlphaNum, isHexDigit )
import System ( getEnv )
import qualified Pretty ( (<+>), (<>), nest, sep, text, Doc )
import Monad(MonadPlus(..), guard)

infixr 5 +++

-- 
-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-Parser]{Yet another combinator parser library}
-- %*                                                                         *
-- %***************************************************************************
-- 
-- NOTE: This is all a little bit of a sledgehammer here for the simple task
-- at hand...
-- 
-- The parser monad
-- 

newtype Parser a = Parser (String -> [(a,String)])

instance Functor Parser where
   -- map :: (a -> b) -> (Parser a -> Parser b)
   fmap f (Parser p) = Parser (\inp -> [(f v, out) | (v, out) <- p inp])

instance Monad Parser where
   -- return :: a -> Parser a
   return v = Parser (\inp -> [(v,inp)])

   -- >>= :: Parser a -> (a -> Parser b) -> Parser b
   (Parser p) >>= f = Parser (\inp -> concat [papply (f v) out
                                             | (v,out) <- p inp])

instance MonadPlus Parser where
   -- zero :: Parser a
   mzero = Parser (\_ -> [])
   -- (++) :: Parser a -> Parser a -> Parser a
   (Parser p) `mplus` (Parser q) = Parser (\inp -> (p inp ++ q inp))

-- 
-- Other primitive parser combinators
-- 

item :: Parser Char
item = Parser (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

force :: Parser a -> Parser a
force (Parser p) = Parser (\inp -> let x = p inp in
                             (fst (head x), snd (head x)) : tail x)

first :: Parser a -> Parser a
first (Parser p) = Parser (\inp -> case p inp of
                            []    -> []
                            (x:_) -> [x])

papply :: Parser a -> String -> [(a,String)]
papply (Parser p) inp = p inp

-- 
-- Derived combinators
-- 

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p `mplus` q)

sat :: (Char -> Bool) -> Parser Char
sat p = do {x <- item; guard (p x); return x}

many :: Parser a -> Parser [a]
many p = force (many1 p +++ return [])

many1 :: Parser a -> Parser [a]
many1 p = do {x <- p; xs <- many p; return (x:xs)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do x  <- p
                    xs <- many (do {sep; p})
                    return(x:xs)

char :: Char -> Parser Char
char x = sat (x==)

alphanum :: Parser Char
alphanum = sat isAlphaNum

string :: String -> Parser String
string ""     = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)

hexdigit :: Parser Char
hexdigit = sat isHexDigit

-- 
-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-MIME]{MIME types}
-- %*                                                                         *
-- %***************************************************************************
-- 
-- A type a is in class Mime whenever it can be parsed and unparsed
-- and moreover it has a method mimeType for printing its MIME type.
-- 
-- This is a VERY naive view of MIME types. Read the documentation for
-- more excitement: http://andrew2.andrew.cmu.edu/rfc/rfc1521.html.
-- 
-- However, all PERL libraries I have seen so far are even more
-- primitive. They just dump raw data on stdout.
-- 

class (Show a, Read a) => Mime a where
   mimeType :: a -> String

-- 
-- Defining a new MIME type instance usual takes the following steps:
-- \begin{itemize}
-- \item Define a newtype synonym or algebraic data type that describes
-- the decoded form of the type.
-- \item Make that type instance of Show by providing an unparser, and
-- an instance of Read by providing a parser. For all but the most
-- simple types you cannot use deriving.
-- \item Make the type an instance of Mime by telling what the MIME type
-- is.
-- \end{itemize}
-- 
-- Here is the most simple one: text/plain.
-- 
-- It is a shame that we cannot make String an instance of Mime
-- directly. We have to define a type synonym :-( I want Gofer!
-- 

newtype TextPlain = TextPlain String

instance Read TextPlain where
   readsPrec _ s = [(TextPlain s,"")]

instance Show TextPlain where
   showsPrec _ (TextPlain s) = showString s

instance Mime TextPlain where
   mimeType _ = "text/plain"

-- 
-- Common type declarations
-- 

type Name  = String
type Value = String
type URL   = String

-- 
-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-HTML]{HTML abstract syntax}
-- %*                                                                         *
-- %***************************************************************************
-- 

type Tag = String

data HTML =
     Element { tag :: Tag, attributes :: [(Name,Value)], html :: [HTML] }
   | Text{ text :: String }
   | UnclosedElement { tag :: Tag, attributes :: [(Name,Value)] }

-- 
-- element tag html
--  = <tag>html</tag>
-- 

element :: Tag -> [HTML] -> HTML
element tag html = attributedElement tag [] html

-- 
-- attributedElement tag [(name1,value1),...,(namen,valuen)] html
--  = <tag
--       name1 = value1
--       ...
--       namen = valuen
--    >
--      html
--    </tag>
-- 

attributedElement :: Tag -> [(Name,Value)] -> [HTML] -> HTML
attributedElement tag attributes html = Element{tag=tag, attributes=attributes, html=html}

-- 
-- TODO!!! <- done by Alan
-- 

unclosedElement :: Tag -> HTML
unclosedElement tag = unclosedAttributedElement tag []

unclosedAttributedElement :: Tag -> [(Name,Value)] -> HTML
unclosedAttributedElement tag attributes = UnclosedElement{tag=tag, attributes=attributes}

-- 
-- prose text
--  = text
-- 

prose :: String -> HTML
prose text = Text{text=text}

-- 
-- 

set :: [(Name,Value)] -> (HTML -> HTML)
set _             e@Text{} = e
set newAttributes e@Element{attributes=attributes} =
   e{ attributes = newAttributes ++ oldAttributes }
      where oldAttributes = [ (n,v) | (n,v) <- attributes, not (elem n ns) ]
            ns            = [ t | (t, _) <- newAttributes ]

-- 
-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-HTM-Util]{HTML utility combinators}
-- %*                                                                         *
-- %***************************************************************************
-- 

type Color = String
type Face  = String
type Size  = String

type Widget = String
type Wrap   = String
type CLSID  = String

-- 
-- page title attributes html
--  = <HTML>
--      <HEAD>
--        <TITLE>title</TITLE>
--      </HEAD>
--      <BODY attributes>
--        html
--      </BODY>
--    </HTML>
-- 

page :: String -> [(Name,Value)] -> [HTML] -> HTML
page title attributes html =
   element "HTML"
           [ element "HEAD" [element "TITLE" [prose title]]
           , attributedElement "BODY" attributes html
           ]

vanillaPage :: [HTML] -> HTML
vanillaPage = page "" []

backgroundPage :: URL -> String -> [HTML] -> HTML
backgroundPage url title body = page title [("BACKGROUND",url)] body

-- 
-- format tag text
--  = <tag>text</tag>
-- 

format :: Tag -> String -> HTML
format tag text = element tag [prose text]

-- 
-- h i heading
--  = <Hi>heading</Hi>
-- 

h :: Int -> String -> HTML
h n heading = element ("H" ++ show n) [prose heading]

h1,h2,h3,h4,h5,h6,h7 :: String -> HTML
h1 = h 1
h2 = h 2
h3 = h 3
h4 = h 4
h5 = h 5
h6 = h 6
h7 = h 7

-- 
-- p html
--  = <P>html</P>
-- 

p :: [HTML] -> HTML
p = element "P"

-- 
-- font color [face1,...,facen] size html
--  = <FONT
--       COLOR = color
--       FACE  = face1,...,facen
--       SIZE  = size
--    >
--      html
--    </FONT>
-- 

font :: Color -> [Face] -> Size -> [HTML] -> HTML
font color face size html =
   attributedElement "FONT"
                     [ ("COLOR", color)
                     , ("FACE" ,(tail . init . show) face)
                     , ("SIZE" , size)
                     ] html

-- 
-- href url html
--  = <A HREF = url>
--      html
--    </A>
-- 

href :: URL -> [HTML] -> HTML
href url html = attributedElement "A" [("HREF",url)] html

-- 
-- name label html
--  = <A HREF = name>
--      html
--    </A>
-- 

name :: String -> [HTML] -> HTML
name label html = attributedElement "A" [("NAME",label)] html

-- 
-- image alt src
--  = <IMG
--       SRC = src
--       ALT = alt
--    >
-- 

img :: String -> URL -> HTML
img alt src = unclosedAttributedElement "IMG" [("SRC",src), ("ALT",alt)]

-- 
-- ul [item1,...,itemn]
--  = <UL>
--      <LI>item1</LI>
--      ...
--      <LI>itemn</LI>
--    </UL>
-- 

ul :: [[HTML]] -> HTML
ul is = element "UL" [ element "LI" i | i <- is ]

-- 
-- ol [item1,...,itemn]
--  = <OL>
--      <LI>item1</LI>
--      ...
--      <LI>itemn</LI>
--    </OL>
-- 

ol :: [[HTML]] -> HTML
ol is = element "OL" [ element "LI" i | i <- is ]

-- 
-- dl [(label1,item1),...(labeln,itemn)]
--  = <DL>
--      <DT><B>label1</B></DT>
--      <DD>item1</DD>
--      ...
--      <DT><B>labeln</B></DT>
--      <DD>itemn</DD>
--    </DL>
-- 

dl :: [(String,[HTML])] -> HTML
dl tds = element "DL"
                 [ e | (dt,dd) <- tds,
                       e <- [ element "DT" [format "B" dt] ,
                              element "DD" dd ] ]

-- 
-- table caption [th1,..., thn]
--              [[td11,...,td1n]
--              ,...
--              ,[tdm1,...,tdmn]
--              ]
--  = <TABLE BORDER>
--      <CAPTION>caption</CAPTION>
--      <TR>
--        <TH>th1</TH>...<TH>thn</TH>
--      </TR>
--      <TR>
--        <TD>td11</TD>...<TD>td1n</TD>
--      </TR>
--      ...
--      <TR>
--        <TD>tdm1</TD>...<TD>tdmn</TD>
--      </TR>
--    </TABLE>
-- 

table :: String -> [String] -> [[ [HTML] ]] -> HTML
table caption header rows = 
   attributedElement "TABLE"
                     [("BORDER","")]
     ( element "CAPTION" [prose caption]
     :   element "TR" [ element "TH" [prose th] | th <- header ]
     : [ element "TR" [ element "TD" td         | td <- row    ] | row <- rows ]
     )

-- 
-- widget w name
--  = <INPUT
--       TYPE = w
--       NAME = name
--    >
--    </INPUT>
-- 

widget :: Widget -> Name -> HTML
widget w name = unclosedAttributedElement "INPUT" [("TYPE",w), ("NAME",name)]

checkbox :: Name -> Value -> HTML
checkbox name value = set [("VALUE", value)] (widget "CHECKBOX" name)

hidden :: Name -> Value -> HTML
hidden name value = set [("VALUE",value)] (widget "HIDDEN" name)

password :: Name -> HTML
password name = widget "PASSWORD" name

radio :: Name -> Value -> HTML
radio name value = set [("VALUE", value)] (widget "RADIO" name)

reset :: Name -> Value -> HTML
reset name value = set [("VALUE",value)] (widget "RESET" name)

submit :: Name -> Value -> HTML
submit name value = set [("VALUE",value)] (widget "SUBMIT" name)

textfield :: Name -> HTML
textfield name = widget "TEXT" name

file :: Name -> HTML
file name = widget "FILE" name

clickmap :: Name -> HTML
clickmap name = widget "IMAGE" name

group :: (Name -> Value -> HTML) -> Name -> [Value] -> HTML
group w name values = table name [] buttons
   where buttons = [ [ [w name value], [prose value] ] | value <- values ]

-- 
-- menu name [choice1,...,choicen]
--  = <SELECT NAME=name>
--      <OPTION>choice1</OPTION>
--      ...
--      ,OPTION>choicen</OPTION>
--    </SELECT>
-- 

menu :: Name -> [Value] -> HTML
menu name choices =
   attributedElement "SELECT" [("NAME",name)]
      [ element "OPTION" [prose choice] | choice <- choices ]

-- 
-- textarea name rows cols wrap text
--  = <TEXTAREA
--       NAME = name
--       ROWS = rows
--       COLS = cols
--       WRAP = wrap
--    >
--      text
--    </TEXTAREA>
-- 

textarea :: Name -> Int -> Int -> Wrap -> Value -> HTML
textarea name rows cols wrap text =
   attributedElement "TEXTAREA"
     [ ("NAME",name), ("ROWS", show rows), ("COLS", show cols), ("WRAP", wrap)]
     [prose text]

-- 
-- gui action html
--  = <FORM
--       ACTION = action
--       METHOD = "POST"
--    >
--      html
--    </FORM>
-- 

gui :: URL -> [HTML] -> HTML
gui action html =
   attributedElement "FORM" [ ("ACTION",action), ("METHOD","POST") ] html

-- 
-- object classid id [(name1,value1),...,(namen,valuen)]
--  = <OBJECT
--       CLASSID = "clsid:"++classid
--       ID      = id
--    >
--      <PARAM name1=value1></PARAM>
--      ...
--      <PARAM namen = valuen></PARAM>
--    </OBJECT>
-- 

object :: CLSID -> String -> [(Name,Value)] -> HTML
object classid id params =
   attributedElement "OBJECT"
      [ ("CLASSID", "clsid:"++classid), ("ID", id) ]
      [ attributedElement "PARAM" [("NAME",n),("VALUE",v)] [] | (n,v) <- params ]
      
-- br
--  = <BR />

br :: HTML
br = unclosedElement "BR"    

-- hr
--  = <HR />

hr :: HTML
hr = unclosedElement "HR"

-- 
-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-HTML-Pretty]{Pretty printing HTML}
-- %*                                                                         *
-- %***************************************************************************
-- 

(<>),(<+>) :: Pretty.Doc -> Pretty.Doc -> Pretty.Doc
(<>)  = (Pretty.<>)
(<+>) = (Pretty.<+>)

nest :: Int -> Pretty.Doc -> Pretty.Doc
nest  = Pretty.nest

sep :: [Pretty.Doc] -> Pretty.Doc
sep   = Pretty.sep

txt :: String -> Pretty.Doc
txt   = Pretty.text

htmlify :: String -> String
htmlify cs = [ c2 | c <- cs, c2 <- htmlifyChar c ]
   where htmlifyChar '<' = "&lt;"
         htmlifyChar '>' = "&gt;"
         htmlifyChar '&' = "&amp;"
         htmlifyChar '"' = "&quot;"
         htmlifyChar 'ä' = "&auml;"
         htmlifyChar 'ö' = "&ouml;"
         htmlifyChar 'ü' = "&uuml;"
         htmlifyChar 'Ä' = "&Auml;"
         htmlifyChar 'Ö' = "&Ouml;"
         htmlifyChar 'Ü' = "&Uuml;"
         htmlifyChar 'ß' = "&szlig;"
         htmlifyChar c   = [c]

instance Show HTML where
   showsPrec _ html = shows (ppHTML html)

instance Read HTML where
   readsPrec _ = error "Reading HTML not yet implemented"

instance Mime HTML where
   mimeType _ = "text/html"

ppHTML :: HTML -> Pretty.Doc
ppHTML Element{tag=tag, attributes=attributes, html=html} = ppElement tag attributes html
ppHTML UnclosedElement{tag=tag, attributes=attributes} = ppUnclosedElement tag attributes
ppHTML Text{text=text} = txt (htmlify text)

ppElement :: Tag -> [(Name,Value)] -> [HTML] -> Pretty.Doc
ppElement tag attributes [] = ppOpenTag tag attributes <> ppCloseTag tag
ppElement tag attributes html
 =  sep [ ppOpenTag tag attributes
        , nest indent (sep (map ppHTML html))
        , ppCloseTag tag
        ]

ppOpenTag :: Tag -> [(Name,Value)] -> Pretty.Doc
ppOpenTag tag [] = txt "<" <> txt (htmlify tag) <> txt ">"
ppOpenTag tag attributes =
   sep [ txt "<" <> txt (htmlify tag)
       , nest indent (sep [ txt (htmlify name) <> txt "=\"" <> txt (htmlify value) <> txt "\""
                             | (name, value) <- attributes ])
       , txt ">" ]

ppCloseTag :: Tag -> Pretty.Doc
ppCloseTag tag = txt "</" <> txt (htmlify tag) <> txt ">"

ppUnclosedElement :: Tag -> [(Name,Value)] -> Pretty.Doc
ppUnclosedElement tag [] = txt "<" <> txt (htmlify tag) <> txt " />"
ppUnclosedElement tag attributes =
   sep [ txt "<" <> txt (htmlify tag)
       , nest indent (sep [ txt (htmlify name) <> txt "=\"" <> txt (htmlify value) <> txt "\""
                             | (name, value) <- attributes ])
       , txt " />" ]

indent :: Int
indent = 2

-- 
-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-HTTP-Resp]{HTTP Responses}
-- %*                                                                         *
-- %***************************************************************************
-- 
-- The output of a CGI script is a HTTP response (part of, the server
-- adds additional noise), which is either some MIME content, a
-- redirection to some other location, or an error status.
-- 
-- We make CgiOut an instance of the Show class so that we don't have to
-- worry about the exact format of the response that is required by the
-- HTTP definition.
-- 
-- I think what we really want is to use an existential type here!
-- 
-- data CgiOut = Mime a => Content{content :: a}
--             | ....
-- 
-- We are not interested in the content type, we only need to know it is
-- in the Mime class.
-- 
-- What is missing is a table of (StatusCode,Reason)-pairs such as
-- (503, "server busy") and (200, "Transaction ok") etc.
-- 

type StatusCode = Int
type Reason = String

data Mime a => CgiOut a =
     Content{mime :: a}
   | Location{url :: URL}
   | Status{status :: StatusCode, reason :: Reason}

instance Mime a => Show (CgiOut a) where
   showsPrec _ Content{mime=mime}
    = showString "Content-type: " . showString (mimeType mime)
    . showString "\n\n"
    . shows mime

   showsPrec _ Location{url=url}
    = showString "Location: " . showString url
    . showString "\n\n"

   showsPrec _ Status{status=status,reason=reason}
    = showString "Status: "
    . shows status . showString " " . showString reason
    . showString "\n\n"

-- 
-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-Decode]{Decoding application/x-www-form-urlencoded data}
-- %*                                                                         *
-- %***************************************************************************
-- 
-- The MIME type application/x-www-form-urlencoded is used to encode
-- tables of (name, value)-pairs that are transmitted from the client to
-- the server.
-- 

newtype ApplicationX_Www_Form_UrlEncoded
   = URLEncoded [(String,String)]

instance Read ApplicationX_Www_Form_UrlEncoded where
   readsPrec _ e = papply (do{f <- env; return (URLEncoded f)}) e

instance Show ApplicationX_Www_Form_UrlEncoded where
   showsPrec r env = showsPrec r (ppEnv env)

instance Mime ApplicationX_Www_Form_UrlEncoded where
   mimeType _ = "application/x-www-form-urlencoded"

-- 
-- An URL encoded value consist of a sequence of
-- zero or more name "=" value pairs separated by "&"
-- 
-- Env ::= [Name "=" Value {"&" Name "=" Value}]
-- 
-- Names and values are URL-encoded,
-- according to the following table
-- 
--    character | encoding
--    ----------|---------
--     ' '      | '+'
--     '<'      | "%XX"
--      c       | "%"hexval(ord c)
-- 

urlDecode :: String -> [(Name,Value)]
urlDecode s = case readsPrec 0 s of
                 [] -> []
                 ((URLEncoded e,_):_) -> e

env :: Parser [(Name,Value)]
env = (do n <- urlEncoded
          string "="
          v <- urlEncoded
          return (n,v)) `sepby` (string "&")

urlEncoded :: Parser String
urlEncoded
 = many ( alphanum `mplus` extra `mplus` safe
         `mplus` do{ char '+' ; return ' '}
         `mplus` do{ char '%'
		   ; d <- hexadecimal
		   ; return $ chr (hex2int d)
		   }
         )

extra :: Parser Char
extra = sat (`elem` "!*'(),")

safe :: Parser Char
safe = sat (`elem` "$-_.")

hexadecimal :: Parser HexString
hexadecimal = do d1 <- hexdigit
                 d2 <- hexdigit
                 return [d1,d2]

type HexString = String

hex2int :: HexString -> Int
hex2int ds = foldl (\n d -> n*16+d) 0 (map (toInt . toUpper) ds)
   where toInt d | isDigit d    =  ord d - ord '0'
         toInt d | isHexDigit d = (ord d - ord 'A') + 10
         toInt d                = error ("hex2int: illegal hex digit " ++ [d])

-- 
-- Pretty printing URL encoded name=value pairs
-- 

ppEnv :: ApplicationX_Www_Form_UrlEncoded -> Pretty.Doc
ppEnv (URLEncoded [])     = txt ""
ppEnv (URLEncoded (e:es)) =
    sep (ppAssoc e : [ txt "&" <+> ppAssoc f | f <- es ])
       where ppAssoc (n, v) = txt n <+> txt "=" <+> txt v

-- 
-- A function to do URL encoding and proving its correctness might be a
-- nice exercise for the book.
-- 
-- We don't usually need it for CGI scripts though. The browser does the
-- encoding and the CGI script does the decoding.
-- 
-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-Hide]{Hide the CGI protocol from the programmer}
-- %*                                                                         *
-- %***************************************************************************
-- 

simpleWrapper :: Show a => ([(Name,Value)] -> a) -> IO ()
simpleWrapper f = wrapper (return . f)

wrapper :: Show a => ([(Name,Value)] -> IO a) -> IO ()
wrapper f = do qs      <- getQueryString
               cgiVars <- getCgiVars
               a       <- f (cgiVars ++ urlDecode qs)
               putStr (show a)

myGetEnv :: String -> IO String
myGetEnv v = catch (getEnv v) (const (return ""))
                      
getQueryString :: IO String
getQueryString = do
   method <- myGetEnv "REQUEST_METHOD"
   case method of
      "POST" -> do len <- myGetEnv "CONTENT_LENGTH"
                   inp <- getContents
                   return (take (read len) inp)
      _      -> myGetEnv "QUERY_STRING"

getCgiVars :: IO [(Name,Value)]
getCgiVars = do vals <- mapM myGetEnv cgiVarNames
                return (zip cgiVarNames vals)

cgiVarNames :: [String]
cgiVarNames =
   [ "DOCUMENT_ROOT"
   , "AUTH_TYPE"
   , "GATEWAY_INTERFACE"
   , "SERVER_SOFTWARE"
   , "SERVER_NAME"
   , "REQUEST_METHOD"
   , "SERVER_ADMIN"
   , "SERVER_PORT"
   , "QUERY_STRING"
   , "CONTENT_LENGTH"
   , "CONTENT_TYPE"
   , "REMOTE_USER"
   , "REMOTE_IDENT"
   , "REMOTE_ADDR"
   , "REMOTE_HOST"
   , "TZ"
   , "PATH"
   , "PATH_INFO"
   , "PATH_TRANSLATED"
   , "SCRIPT_NAME"
   , "SCRIPT_FILENAME"
   , "HTTP_CONNECTION"
   , "HTTP_ACCEPT_LANGUAGE"
   , "HTTP_ACCEPT"
   , "HTTP_HOST"
   , "HTTP_UA_COLOR"
   , "HTTP_UA_CPU"
   , "HTTP_UA_OS"
   , "HTTP_UA_PIXELS"
   , "HTTP_USER_AGENT"
   ]

