{- 
---
fulltitle: "In class exercise: XML parsing and printing"
date: March 4, 2025
---

In today's exercise you will use the definitions from the `Parsers` lecture to
build a simple parser for `XML` data.

This exercise is based on definitions from the `Parsers` lecture, summarized
by the module `ParserCombinators`. You may modify the import statement below
to bring more functions into scope, but you should *not* modify the
`ParserCombinators` library.

Note that the import below makes the listed functions available, in addition
to the instances for `Parser` for the `Functor`, `Applicative` and
`Alternative` classes. However, it does *not* import the `P` data constructor
so you should think of `Parser a` as an abstract type.

You may import more operators from the `ParserCombinators` and
 `Control.Applicative` libraries if it is helpful for you. However, you should
 not modify the `ParserCombinators` module itself.
-}

module Xml where

import Control.Applicative (Alternative(..))
import System.IO
import Prelude hiding (filter)

import Text.PrettyPrint ( (<+>), Doc )
import qualified Text.PrettyPrint as PP


import ParserCombinators (Parser, doParse, satisfy, char, string, filter)

{- 
Your goal: produce this structured data from a string
-}

-- | A simplified datatype for storing XML
data SimpleXML =
          PCDATA  String
        | Element ElementName [SimpleXML]
      deriving Show

type ElementName = String

{- 
First: the characters `/`, `<`, and `>` are not allowed to appear in tags or
PCDATA. Let's define a function that recognizes them.
-}

reserved :: Char -> Bool
reserved c = c `elem` ['/', '<', '>']

{- 
Use this definition to parse a maximal nonempty sequence of nonreserved characters:
(HINT: check out operations related to [the `Alternative` type class](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Applicative.html#g:2).)
-}

text :: Parser String
text = some (satisfy (not . reserved))


-- >>> doParse text "skhdjf"
-- Just ("skhdjf","")

-- >>> doParse text "akj<skdfsdhf"
-- Just ("akj","<skdfsdhf")

-- >>> doParse text ""
-- Nothing


{- 
Now use this definition to parse nonreserved characters into XML.
-}

pcdata :: Parser SimpleXML
pcdata = PCDATA <$> text

-- you can directly fmap the constructor onto other functors. 


-- >>> doParse pcdata "akj<skdfsdhf"
-- Just (PCDATA "akj","<skdfsdhf")


{- 
Next, parse an empty element, like `"<br/>"`
-}

emptyContainer :: Parser SimpleXML
emptyContainer = (\en -> Element en []) <$> (char '<' *> text <* string "/>")


-- >>> doParse emptyContainer "<br/>sdfsdf"


{- 
Parse a container element: this consists of an open tag, a potentially empty
 sequence of content parsed by `p`, and a closing tag.  For example,
 `container pcdata` should recognize `<br></br>` or `<title>A midsummer
 night's dream</title>` (and more examples below).  You do NOT need to make
 sure that the closing tag matches the open tag.
-}

container :: Parser SimpleXML -> Parser SimpleXML
container p = (\o ct c -> Element o ct) <$> openP <*> contentP <*> closeP where
    openP = char '<' *> text <* char '>'
    contentP = many p
    closeP = string "</" *> text <* char '>'

-- > :t many emptyContainer    
-- > many emptyContainer :: Parser [SimpleXML]



-- >>> doParse (container pcdata) "<br></br>"
-- Just (Element "br" [],"")

-- >>> doParse (container pcdata) "<title>A midsummer night's dream</title>"
-- Just (Element "title" [PCDATA "A midsummer night's dream"],"")


-- >>> doParse (container emptyContainer) "<text><br/><br/></text>"
-- Just (Element "text" [Element "br" [],Element "br" []],"")


-- This should also work, even though the tag is wrong
-- >>> doParse (container pcdata) "<title>A midsummer night's dream</br>"
-- Just (Element "title" [PCDATA "A midsummer night's dream"],"")


{- 
Now put the above together to construct a parser for simple XML data:
-}

xml :: Parser SimpleXML
xml = container xml <|> emptyContainer <|> pcdata


-- >>> doParse xml "<body>a</body>"
-- Just (Element "body" [PCDATA "a"],"")

-- >>> doParse xml "<body><h1>A Midsummer Night's Dream</h1><h2>Dramatis Personae</h2>THESEUS, Duke of Athens.<br/>EGEUS, father to Hermia.<br/></body>"
-- Just (Element "body" [Element "h1" [PCDATA "A Midsummer Night's Dream"],Element "h2" [PCDATA "Dramatis Personae"],PCDATA "THESEUS, Duke of Athens.",Element "br" [],PCDATA "EGEUS, father to Hermia.",Element "br" []],"")


-- >>> doParse xml "cis552"
-- Just (PCDATA "cis552","")

-- >>> doParse xml "<br/>"
-- Just (Element "br" [],"")


{- 
Now let's try it on something a little bigger. How about [`dream.html`](../../hw/hw02/dream.html) from hw02?
-}

-- | Run a parser on a particular input file
parseFromFile :: Parser a -> String -> IO (Maybe (a,String))
parseFromFile parser filename = do
  handle <- openFile filename ReadMode
  str    <- hGetContents handle
  return $ doParse parser str

{- 
Run this test in a terminal, the output is large so do not try to run as an inline doctest.

~~~~~{.haskell}
Xml> parseFromFile xml "dream.html"
~~~~~

Challenge: rewrite container so that it only succeeds when the closing tag matches the opening tag. If you had a `Monad` instance for `Parser`, this 
challenge would be easier to do. However, there is a solution that uses 
`filter` instead of `(>>=)`.
-}

container2 :: Parser SimpleXML -> Parser SimpleXML
container2 p = 
    fmap (\(o, ct, c) -> Element o ct)
    (filter (\(o,ct,c) -> o==c)
    ((,,) <$> openP <*> contentP <*> closeP)) where
    openP = char '<' *> text <* char '>'
    contentP = many p
    closeP = string "</" *> text <* char '>'


-- >>> doParse (container2 pcdata) "<title>A midsummer night's dream</title>"
-- Just (Element "title" [PCDATA "A midsummer night's dream"],"")

-- >>> doParse (container2 pcdata) "<title>A midsummer night's dream</br>"
-- Nothing

{- | Great! We have parsed XML into syntax!
Let's now briefly focus on the opposite problem: pretty printing.

The derived `Show` instances for datatypes can be pretty hard to
read, especially when the data gets long (as they will in your
MiniDafny homework! 

A *pretty printer* is a function that formats an abstract syntax tree into a
readable representation of the concrete syntax. 

The `pretty` library, imported above as `PP`, provides the following to assist
in the development of pretty printers:

-- import Text.PrettyPrint ( (<+>), Doc )
-- import qualified Text.PrettyPrint as PP

   * An abstract type `Doc` of "pretty documents" that know how to lay
     themselves out prettily. We can use this type to define a class of of types
     that support pretty printing---those that define a function mapping any
     value of that type to a suitable `Doc`.
-} 

class PP a where
  pp :: a -> Doc

{- |

   * Operations for rendering, or converting a `Doc` to text at the
     top level.  The rendering functions are parameterized over display
     options, such as the maximum line length, so that they can figure out
     how to best display the text. 
-}

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: PP a => a -> String
pretty = PP.render . pp

{- | Let's write an instance for our SimpleXML datatype: -}

instance PP String where
    pp s = PP.text s

instance PP SimpleXML where
  pp (PCDATA s) = PP.text s
  pp (Element n []) = PP.text "<" <> pp n <> PP.text "/>"
  pp (Element n xs) = 
    PP.vcat(    [PP.text "<" <> pp n <> PP.text "/>"] 
            ++  [PP.nest 2 (PP.vcat (map pp xs))]
            ++  [PP.text "</" <> pp n <> PP.text ">"])