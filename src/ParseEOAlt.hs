{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DatatypeContexts #-}

module ParseEOAlt (tProgram, printTree, Position(..)) where

import           Control.Applicative        (Alternative ((<|>)), optional)
import           Control.Monad.Identity
import           Data.Char                  (digitToInt)
import qualified Data.List                  as DL
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (lookAhead, takeWhile1P),
                                             Parsec, SourcePos (SourcePos),
                                             choice, count, getSourcePos, many,
                                             manyTill, some, try,
                                             unPos, (<?>))
import           Text.Megaparsec.Char       (alphaNumChar, char, eol,
                                             hexDigitChar, letterChar,
                                             lowerChar, numberChar, printChar,
                                             string)
import           Text.Megaparsec.Char.Lexer (charLiteral, decimal, scientific,
                                             signed)
import           Text.Megaparsec.Debug      (dbg)
import qualified Text.Megaparsec.Error
import           Text.Megaparsec.Internal   (ParsecT)
import qualified Text.Megaparsec.Stream
import           Text.Printf                (printf, PrintfType, IsChar)
import Data.Data (Data(toConstr))

type Parser = Parsec Void Text

-- constant symbols

cARROW :: Text
cARROW = ">"

cAT :: Text
cAT = "@"

cCOLON :: Text
cCOLON = ":"

cCONST :: Text
cCONST = "!"

cCOPY :: Text
cCOPY = "\'"

cDOT :: Text
cDOT = "."

cHASH :: Text
cHASH = "#"

cINDENT :: Text
cINDENT = "  "

cLB :: Text
cLB = "("

cLSQ :: Text
cLSQ = "["

cMINUS :: Text
cMINUS = "-"

cPLUS :: Text
cPLUS = "+"

cQUESTION :: Text
cQUESTION = "?"

cRB :: Text
cRB = ")"

cRHO :: Text
cRHO = "^"

cROOT :: Text
cROOT = "Q"

cRSQ :: Text
cRSQ = "]"

cSIGMA :: Text
cSIGMA = "&"

cSLASH :: Text
cSLASH = "/"

cSPACE :: Text
cSPACE = " "

cSTAR :: Text
cSTAR = "*"

cVERTEX :: Text
cVERTEX = "<"

cXI:: Text
cXI = "$"

cTEXT_MARK :: Text
cTEXT_MARK = "\"\"\""

cDOTS :: Text
cDOTS = "..."

cNEWLINE :: Text
cNEWLINE = "\n"

cCARET_RETURN :: Text
cCARET_RETURN = "\r"

cEMPTY_BYTES :: Text
cEMPTY_BYTES = "--"

cEMPTY_TEXT :: Text
cEMPTY_TEXT = ""

cTRUE :: Text
cTRUE = "TRUE"

cFALSE :: Text
cFALSE = "FALSE"







-- Node definition
instance Show Position where
  show (Position r c) = printf "%d:%d" r c
instance Show Segment where
  show Segment {..} = printf "[%s..%s]" (show start) (show end)

data Position = Position
  { row    :: Int,
    column :: Int
  } deriving (Data)
type I a b = Node a b
data Segment = Segment {start::Position, end::Position} deriving (Data)
data Node a b = Node {pos::Segment, node::a b, load::b} deriving (Data)
-- type K a b = Node a b
data Node1 a b = Node1 {node::a b, load::b}
newtype (Data b) => PLicense b = PLicense {cs::Node PComment b} deriving (Data)
newtype (Data b) => PComment b = PComment {c::Text} deriving (Data)


-- Node traversals
-- instance Functor (Node PLicense) where
--   fmap f n@Node{load = load, node = cs} = n {load = f load, node = fmap f cs}

-- instance Functor (Node PComment) where
--   fmap f n@Node{load = load} = n {load = f load}
-- instance Functor (Node TMetas) where
--   fmap f n@Node{load = load} = n {load = f load}

-- instance Functor (Node TProgram) where
--   fmap f n@Node {node = TProgram {..}, load = load} = n {
--     node = TProgram {
--       l = l', 
--       m = m', 
--       o = f o
--       }, 
--     load = f load
--     }
--     where 
--       l' =
--         case l of
--           Just p -> Just (f <$> p)
--           Nothing -> Nothing
--       m' = 
--         case m of
--           Just p -> Just (f <$> p)
--           Nothing -> Nothing
--       o' = f <$> o

-- instance (Foldable (Node TProgram), Functor (Node TProgram)) => Traversable (Node TProgram) where
--   traverse f x = undefined 

-- Node printing



-- Helpers
tab :: String
tab = "|  "

type TabNumber = Int

tabs :: Int -> String
tabs n = DL.intercalate "" (replicate n tab)

cName :: Data a => a -> String
cName n = show $ toConstr n

-- showHead :: (PrintfType t, Data a) => Int -> I a -> t
showHead n Node {..} = printf "%s%s %s" (tabs n) (cName node) (show pos)
nothing :: String
nothing = "Nothing"

-- printTree :: I TProgram a -> String
printTree p = printProgram 0 p

-- | for non-leaf nodes
-- printNonLeaf :: (Data a, Foldable t, Functor t) =>
--   Int -> I a b -> t (Int -> [Char]) -> [Char]
printNonLeaf n i l = (showHead n i) <> "\n" <> (foldl (<>) [] (($ n + 1) <$> l))

-- | for leaf nodes
-- printLeaf :: (Show t, Data a) =>
--   Int -> I a -> t -> [Char]
printLeaf n i d = showHead n i <>  " " <> (show d) <> "\n"


-- Node printers

-- printProgram :: TabNumber -> I TProgram a -> String
printProgram n i@Node {node = TProgram {..}} = printNonLeaf n i [l', m', o']
  where
    l' k = maybe nothing (printLicense k) l
    m' k = maybe nothing (printMetas k) m
    o' k = printObjects k o

-- printLicense :: Int -> I TLicense a -> String
printLicense n i@Node {node = TLicense {..}} = printNonLeaf n i [cs']
  where
    cs' k = foldl (<>) [] (printComment k <$> cs)

-- printComment :: Int -> I TComment a -> String
printComment n i@Node {node = TComment {..}} = printLeaf n i c

-- printMetas :: Int -> I TMetas a -> String
printMetas n i@Node {node = TMetas {..}} = printNonLeaf n i [cs']
  where
    cs' k = foldl (<>) [] (printMeta k <$> ms)

-- printNothing :: Int -> p -> String
printNothing n d = tabs n <> nothing

-- printMeta :: Int -> I TMeta a -> String
printMeta n i@Node {node = TMeta {..}} = printNonLeaf n i [name']
  where
    name' k = printName k name
    suff' = maybe nothing (printNothing n) suff


-- printName :: Int -> I TName a -> String
printName m i@Node {node = TName {..}} = printLeaf m i n

-- printObjects :: Int -> I TObjects a -> String
printObjects n i@Node {node = TObjects {..}} = printNonLeaf n i  [os']
  where
    os' k = foldl (<>) [] (printObject k <$> os)

-- printObject :: Int -> I TObject a -> String
printObject n i = printNothing n i <> "\n"



-- Parsing helpers

getPos :: Parser Position
getPos = do
  SourcePos _ r c <- getSourcePos
  return (Position (unPos r) (unPos c))

tHexDigit :: Char -> Char -> Parser Int
tHexDigit l1 l2 = do
  c <- hexDigitChar
  guard (c `elem` ['0' .. '9'] <> [l1 .. l2])
  let c' = digitToInt c
  return c'

pHexDigitUpper :: Parser Integer
pHexDigitUpper = toInteger <$> tHexDigit 'A' 'F'

pHexDigitLower :: Parser Integer
pHexDigitLower = toInteger <$> tHexDigit 'a' 'f'

pEmpty :: Parser ()
pEmpty = return ()

hexToInt :: [Integer] -> Integer
hexToInt = foldl (\x acc -> (acc * 16) + x) 0

debugFlag :: Bool
debugFlag = True

data DebugMode = On | Off

debug :: (Text.Megaparsec.Stream.VisualStream s, Text.Megaparsec.Error.ShowErrorComponent e, Show a) => String -> ParsecT e s m a -> ParsecT e s m a
debug label parser
  | debugFlag = dbg label parser
  | otherwise = parser <?> label

manyTry :: MonadParsec e s m => m a -> m [a]
manyTry p = try $ many (try p)

someTry :: MonadParsec e s m => m a -> m [a]
someTry p = try $ some (try p)

choiceTry :: MonadParsec e s m => [m a] -> m a
choiceTry p = try $ choice (map try p)

enter :: Show a => a -> ParsecT Void Text Identity ()
enter name = do
  pos <- getPos
  debug (show pos <> ": Enter " <> show name) pEmpty
  return ()

tEOL :: Parser ()
tEOL = try $ do
  _ <- eol *> optional (try eol)
  return ()

leave :: (Show a1, Show a2) => a1 -> a2 -> ParsecT Void Text Identity ()
leave name node = do
  pos <- getPos
  let l = printf "%s: Leave %s" (show pos) (show name)
  debug l pEmpty
  return ()

noIndent :: Int
noIndent = 0

indentAdd :: Int
indentAdd = 1

getIndent :: I TIndent a -> Int
getIndent Node {node = TIndent {..}} = n

-- | does common actions for all nodes
-- dec::Text -> Parser (a b) -> Parser (I a b)
dec :: p
  -> Parser (a b)
  -> Parser (b -> Node a b)
dec t p = do
  p1 <- getPos
  -- enter t
  p' <- p
  p2 <- getPos
  let ans = Node (Segment p1 p2) p'
  -- leave t ans
  return ans

-- I a -> (b -> Node a b)

-- ***************************************************

-- Parsers | Parsers | Parsers | Parsers | Parsers

-- ***************************************************

-- type K a b = a b-> (I a b)

data Options2 a b = Opt2A a | Opt2B b deriving (Data)
data Options3 a b c = Opt3A a | Opt3B b | Opt3C c deriving (Data)

data TProgram a = TProgram {l::Maybe (I TLicense a), m::Maybe (I TMetas a), o::I TObjects a} deriving (Data)
tProgram :: Parser (I TProgram a)
tProgram = dec "Program" $ do
  l <- optional $ try ({-debug "program:license"-} tLicense)
  m <- optional $ try ({-debug "program:metas"-} tMetas)
  o <- {-debug "program:objects"-} tObjects
  return TProgram {l = l, m = m, o = o}

data TLicense a = TLicense {cs::[I TComment a]} deriving (Data)
tLicense :: Parser (I TLicense a)
tLicense = dec "License" $ do
  cs <- someTry (tComment <* tEOL)
  return TLicense {cs = cs}

data TMetas a = TMetas {ms::[I TMeta a]} deriving (Data)
tMetas :: Parser (I TMetas a)
tMetas = dec "Metas" $ do
  ms <- someTry (tMeta <* tEOL)
  return TMetas {ms = ms}

data TObjects a = TObjects {os::[I TObject a]} deriving (Data)
tObjects :: Parser (I TObjects a)
tObjects = dec "Objects" $ do
  os <- someTry $ tObject noIndent <* tEOL
  return TObjects {os = os}

data TObject b = TObject {cs::[I TComment b],
  a::Options2 (I TAbstraction b) (I TApplication b),
  t::Maybe (I TTail b),
  s::[(I TMethod b, Maybe (I THtail b), Maybe (I TSuffix b), Maybe (I TTail b))]} deriving (Data)

tObject :: Int -> Parser (I TObject a)
tObject ind = dec "Object" $ do
  comments <- manyTry $ do
    c <- {-debug "object:comment"-} tComment
    e <- tEOLTabMany
    guard $ getIndent e == ind
    return c
  a <-
    choiceTry
      [ Opt2A <$>{-debug "object:abstraction"-} tAbstraction,
        Opt2B <$> {-debug "object:application"-} tApplication
      ]
  let newIndent = ind + indentAdd
  -- list of attributes
  t <- optional $ try ({-debug "object:tail"-} tTail newIndent)
  let g = do
        e <- tEOLTabMany
        guard $ getIndent e == ind
        method <- {-debug "object:method"-} tMethod
        h <- optional $ try ({-debug "object:htail"-} tHtail)
        suffix <- optional $ try ({-debug "object:suffix"-} tSuffix)
        p <- optional $ try ({-debug "object:tail"-} tTail (ind + indentAdd))
        return (method, h, suffix, p)
  s <- manyTry $ ({-debug "object:after tail"-} g)
  let ans = TObject comments a t s
  return ans


data TAbstraction a = TAbstraction {as::I TAttributes a, t::Maybe (I TAbstractionTail a)} deriving (Data)
tAbstraction ::Parser (I TAbstraction a)
tAbstraction = dec "Abstraction" $ do
  attrs <- tAttributes
  t <- optional $ try tAbstractionTail
  return TAbstraction {as = attrs, t = t}

data TAbstractionTail a = TAbstractionTail {e::Options2 (I TSuffix a, Maybe (Options2 (I TName a) (I TTerminal a))) (I THtail a)} deriving (Data)
tAbstractionTail :: Parser (I TAbstractionTail a)
tAbstractionTail = dec "Abstraction tail" $ do
  let a = do
      suff <- tSuffix
      o <- optional $ try (
        string cSPACE
        *> string cSLASH
        *> choiceTry
          [ Opt2A <$> {-debug "abstraction:name"-} tName,
            Opt2B <$> tTerminal cQUESTION Question
          ])
      return (suff, o)
  let b = tHtail
  e <- choiceTry [Opt2A <$> a, Opt2B <$> b]
  return TAbstractionTail {e = e}

-- data Load = Load {start::Position, end::Position} deriving (Data)
-- initLoad = Load {start = Position 0 0, end = Position 0 0}

-- | contains list of arguments
--
-- If no arguments are provided, the list is empty
-- This is the same as making the part between [] optional
data TAttributes a = TAttributes {as::[I TLabel a]} deriving (Data)
tAttributes :: Parser (I TAttributes a)
tAttributes = dec "Attributes" $ do
  _ <- string cLSQ
  attrs <- choiceTry
        [ do
            a <- {-debug "attributes:attribute1"-} tLabel
            as <- manyTry (string cSPACE *> {-debug "attributes:attribute2"-} tLabel)
            return (a : as),
          [] <$ pEmpty
        ]
  _ <- string cRSQ
  return TAttributes {as = attrs}

data TLabel a = TLabel {l::Options2 (I TTerminal a) (I TName a, Maybe (I TTerminal a))} deriving (Data)
tLabel :: Parser (I TLabel a)
tLabel = dec "Label" $ do
  l <-
    choiceTry
      [ Opt2A <$> {-debug "label:@"-} (tTerminal cAT At),
        Opt2B <$> (do
          name <- {-debug "label:name"-} tName
          -- TODO move dots to abstraction end (before csq)
          dots <- optional ({-debug "label:..."-} (tTerminal cDOTS Dots))
          return (name, dots))
      ]
  return TLabel {l = l}

data TTail a = TTail {os::[I TObject a]} deriving (Data)
tTail :: Int -> Parser (I TTail a)
tTail ind = dec "Tail" $ do
  let tObj = do
        e <- {-debug "tail:eol"-} tEOLTabMany
        let ind1 = getIndent e
        guard $ ind1 == ind
        tObject ind1
  objects <- someTry tObj
  return TTail {os = objects}

data TSuffix a = TSuffix {l::I TLabel a, c::Maybe (I TTerminal a)} deriving (Data)
tSuffix :: Parser (I TSuffix a)
tSuffix = dec "Suffix" $ do
  label <- string cSPACE *> string cARROW *> string cSPACE *> {-debug "suffix:label"-} tLabel
  c <- optional ({-debug "suffix:const"-} (tTerminal cCONST Const))
  return TSuffix {l = label, c = c}

data TMethod a = TMethod {m::Options2 (I TName a) (I TTerminal a)} deriving (Data)
tMethod :: Parser (I TMethod a)
tMethod = dec "Method" $ do
  method <-
    string cDOT
      *> choiceTry
        [ Opt2A <$> {-debug "method:name"-} tName,
          Opt2B <$> {-debug "method:^"-} (tTerminal cRHO Rho),
          Opt2B <$> {-debug "method:@"-} (tTerminal cAT At),
          Opt2B <$> {-debug "method:<"-} (tTerminal cVERTEX Vertex)
        ]
  return TMethod {m = method}

data TApplication a = TApplication {s::Options2 (I THead a) (I TApplication a), h::Maybe (I THtail a), a1::I TApplication1 a} deriving (Data)
tApplication :: Parser (I TApplication a)
tApplication = dec "Application" $ do
  s <-
    choiceTry
      [ Opt2A <$>{-debug "application:head"-} tHead,
        Opt2B <$> (string cLB *> {-debug "application:application"-} tApplication <* string cRB)
      ]
  h <- optional $ try ({-debug "application:htail"-} tHtail)
  a1 <- {-debug "application:application1"-} tApplication1
  return TApplication {s = s, h = h, a1 = a1}

data TApplication1 a = TApplication1 {c::Maybe (I TApplication1Elem a)} deriving (Data)
tApplication1 :: Parser (I TApplication1 a)
tApplication1 = dec "Application1" $ do
  c <- optional $ try tApplication1Elem
  return TApplication1 {c = c}

data TApplication1Elem a = TApplication1Elem {c1::Options3 (I TMethod a) (I THas a) (I TSuffix a), ht::Maybe (I THtail a), a::I TApplication1 a} deriving (Data)
tApplication1Elem :: Parser (I TApplication1Elem a)
tApplication1Elem = dec "Application1 Element" $ do
  c1 <-
    choiceTry
      [ Opt3A <$> {-debug "application1:method"-} tMethod,
        Opt3B <$> {-debug "application1:has"-} tHas,
        Opt3C <$> {-debug "application1:suffix"-} tSuffix
      ]
  ht <- optional $ try ({-debug "application1:htail"-} tHtail)
  a <- {-debug "application1:application1"-} tApplication1
  return TApplication1Elem {c1 = c1, ht = ht, a = a}


data THtail a = THtail {t::[Options3 (I THead a) (I TApplication a) (I TAbstraction a)]} deriving (Data)
tHtail :: Parser (I THtail a)
tHtail = dec "Htail" $ do
  let op = choiceTry
            [ Opt3A <$> {-debug "htail:head"-} tHead,
              Opt3B <$> (string cLB *> {-debug "htail:application1"-} tApplication <* string cRB),
              Opt3C <$> {-debug "htail:abstraction"-} tAbstraction
            ]
  t <- someTry (string cSPACE *> op)
  return THtail {t = t}

data Options8 a b c d e f g h =
    Options8A a
  | Options8B b
  | Options8C c
  | Options8D d
  | Options8E e
  | Options8F f
  | Options8G g
  | Options8H h
  deriving (Data)

data TTerminal a =
    Root
  | Xi
  | Sigma
  | Dot
  | Copy
  | Star
  | At
  | Rho
  | Vertex
  | EmptyBytes
  | Question
  | Dots
  | Const
  deriving Data

tTerminal :: Text -> TTerminal a -> Parser (I TTerminal a)
tTerminal s t = dec s $ do
  p1 <- getPos
  void (string s)
  p2 <- getPos
  return t

data THead a = THead {dots::Maybe (I TTerminal a), t::Options3 (I TTerminal a) (I THeadName a) (I TData a)} deriving (Data)
tHead :: Parser (I THead a)
tHead = dec "Head" $ do
  dots <- optional $ tTerminal cDOTS Dots
  t <- choiceTry
      [ Opt3A <$>{-debug "head:root"-}  (tTerminal cROOT Root),
        Opt3A <$>{-debug "head:at"-}  (tTerminal cAT At),
        Opt3A <$>{-debug "head:rho"-}  (tTerminal cRHO Rho),
        Opt3A <$>{-debug "head:xi"-}   (tTerminal cXI Xi),
        Opt3A <$>{-debug "head:sigma"-}   (tTerminal cSIGMA Sigma),
        Opt3A <$>{-debug "head:star"-}   (tTerminal cSTAR Star),
        Opt3B <$> tHeadName,
        Opt3C <$>{-debug "head:data"-} (tData)
      ]
  return THead {dots = dots, t = t}

-- TODO lookahead EOL
data THeadName a = THeadName {name::I TName a, c::Options2 (I TTerminal a) (Maybe (I TTerminal a))} deriving (Data)
tHeadName :: Parser (I THeadName a)
tHeadName = dec "Head name" $ do
  name <- tName
  c <- choiceTry [
        Opt2A <$> tTerminal cDOT Dot <* lookAhead (string cSPACE)
      , Opt2B <$> optional (tTerminal cCOPY Copy)
      ]
  return THeadName {name = name, c = c}


data THas a = THas {n::I TName a} deriving (Data)
tHas :: Parser (I THas a)
tHas = dec "Has" $ do
  _ <- string cCOLON
  n <- {-debug "has:name"-} tName
  return THas {n = n}


data Options9 a b c d e f g h i =
    Opt9A a
  | Opt9B b
  | Opt9C c
  | Opt9D d
  | Opt9E e
  | Opt9F f
  | Opt9G g
  | Opt9H h
  | Opt9I i
  deriving (Data)

data TData a = TData {d::Options9 (I TBool a) (I TText a) (I THex a) (I TString a) (I TFloat a) (I TInt a) (I TBytes a) (I TChar a) (I TRegex a)} deriving (Data)
tData :: Parser (I TData a)
tData = dec "DATA" $ do
  d <-
    choiceTry
      [ {-debug "data:bool"-}Opt9A <$> tBool,
        {-debug "data:text"-} Opt9B <$>tText,
        {-debug "data:hex"-} Opt9C <$>tHex,
        {-debug "data:string"-} Opt9D <$>tString,
        {-debug "data:float"-} Opt9E <$>tFloat,
        {-debug "data:int"-} Opt9F <$>tInt,
        {-debug "data:bytes"-} Opt9G <$>tBytes,
        {-debug "data:char"-} Opt9H <$>tChar,
        {-debug "data:regex"-} Opt9I <$>tRegex
      ]
  return TData {d = d}

data TComment a = TComment {c::Text} deriving (Data)
tComment :: Parser (I TComment a)
tComment = dec "COMMENT" $ do
  _ <- string cHASH
  content <- pack <$> many printChar
  return TComment {c = content}


data TMeta a = TMeta {name::I TName a, suff::Maybe Text} deriving (Data)
tMeta :: Parser (I TMeta a)
tMeta = dec "META" $ do
  _ <- string cPLUS
  name <- {-debug "meta:name"-} tName
  suffix <- {-debug "meta:suffix"-} (optional . try $ pack <$> (string cSPACE *> some printChar))
  return TMeta {name = name, suff = suffix}


data TRegex a = TRegex {r :: Text, suff :: Text} deriving (Data)
tRegex :: Parser (I TRegex a)
tRegex = dec "REGEX" $ do
  _ <- string cSLASH
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- pack <$> many alphaNumChar
  return (TRegex {r = r, suff = suffix})


data TIndent a = TIndent {n::Int} deriving (Data)
tEOLTabMany :: Parser (I TIndent a)
tEOLTabMany = dec "EOL_TAB_MANY" $ do
  _ <- {-debug "eol:eol"-} (try (eol *> optional eol))
  indents <- T.concat <$> many (string cINDENT)
  let nIndents = T.length indents `div` 2
  return TIndent {n = nIndents}

data TByte a = TByte {b :: Integer} deriving (Data)
tByte :: Parser (I TByte a)
tByte = dec "BYTE" $ do
  b <- hexToInt <$> count 2 pHexDigitUpper
  return TByte {b = b}

data TLineBytes a = TLineBytes {bs::[I TByte a]} deriving (Data)
tLineBytes :: Parser (I TLineBytes a)
tLineBytes = dec "LINE_BYTES" $ do
  byte <- {-debug "line_bytes:byte"-} tByte
  bytes <- {-debug "line_bytes:bytes"-} (someTry (string cMINUS *> tByte))
  return TLineBytes {bs = byte : bytes}

data TBytes a = TBytes {bs::Options3 (I TTerminal a) (I TByte a) [I TLineBytes a]} deriving (Data)
tBytes :: Parser (I TBytes a)
tBytes = dec "BYTES" $ do
  bytes <-
    choiceTry
      [ parser1,
        parser3,
        parser2
      ]
  return TBytes {bs = bytes}
  where
    parser1 = do
      s <- tTerminal cEMPTY_BYTES EmptyBytes
      return (Opt3A s)
    parser2 = do
      byte <- tByte
      _ <- string cMINUS
      return (Opt3B byte)
    parser4 = do
      _ <- string cMINUS
      -- TODO guard indentation
      e <- tEOLTabMany
      lb <- tLineBytes
      return lb
    parser3 = do
      lb <- tLineBytes
      lbs <- manyTry parser4
      return (Opt3C (lb : lbs))

data TBool a = TBool {b::Bool} deriving (Data)
tBool :: Parser (I TBool a)
tBool = dec "BOOL" $ do
  b <-
    choiceTry
      [ True <$ string cTRUE,
        False <$ string cFALSE
      ]
  return TBool {b = b}

data TChar a = TChar {c::Char} deriving (Data)
-- | slightly differs from grammar: doesn't allow u Byte Byte
tChar :: Parser (I TChar a)
tChar = dec "CHAR" $ do
  c <- char '\'' *> charLiteral <* char '\''
  return TChar {c = c}


data TString a = TString {s::Text} deriving (Data)
-- | slightly differs from grammar: doesn't allow u Byte Byte
tString :: Parser (I TString a)
tString = dec "STRING" $ do
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  return TString {s = s}


data TInt a = TInt {s::Integer} deriving (Data)
tInt :: Parser (I TInt a)
tInt = dec "INT" $ do
  s <- signed pEmpty decimal
  return TInt {s = s}


data TFloat a = TFloat {f::Scientific} deriving (Data)
tFloat :: Parser (I TFloat a)
tFloat = dec "FLOAT" $ do
  f <- signed pEmpty scientific
  return (TFloat {f = f})

data THex a = THex {h::Integer} deriving (Data)
tHex :: Parser (I THex a)
tHex = dec "HEX" $ do
  s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
  return THex {h = s}


data TName a = TName {n::Text} deriving (Data)
tName :: Parser (I TName a)
tName = dec "NAME" $ do
  l1 <- {-debug "name: first letter"-} lowerChar
  l2 <- {-debug "name: other letters"-} (manyTry (letterChar <|> numberChar <|> char '_' <|> char '-'))
  return TName {n = pack (l1:l2)}

-- IDK maybe need to allow indentation after eol
data TText a = TText {t::Text} deriving (Data)
tText :: Parser (I TText a)
tText = dec "TEXT" $ do
  t <- try $ pack <$> (string cTEXT_MARK *> eol *> manyTill charLiteral (string cTEXT_MARK))
  return TText {t = t}

-- TODO function to combine pos