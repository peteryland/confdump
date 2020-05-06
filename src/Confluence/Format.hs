{-# LANGUAGE OverloadedStrings #-}

module Confluence.Format(getHeaders, confluenceToPandoc) where

import Debug.Trace

import Confluence.Utils(toIdent, safeHead)
import System.FilePath (replaceFileName)
import Data.Char(isSpace, isAlphaNum)
import Data.Maybe(mapMaybe)
import Data.List(intersperse, sort, isPrefixOf, isInfixOf, isSuffixOf, (\\)) --, intercalate)
import Data.Text(pack)
import Data.Text.Lazy(unpack)
import Data.Text.Lazy.Builder(toLazyText)
import HTMLEntities.Decoder(htmlEncodedText)
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

data Attr = Attr String String deriving (Show, Eq, Ord)
data Elem = Tag String [Attr] [Elem] | Text String deriving (Show, Eq, Ord)
type Doc = [Elem]

type Style = [StyleElem]
data StyleValue = RGB Int Int Int | RGBA Int Int Int Int | Value String
data StyleElem = StyleElem { styleName :: String, styleValue :: StyleValue }

instance Show StyleElem where
  show (StyleElem { styleName=n, styleValue=v }) = n ++ ": " ++ show v ++ ";"
instance Show StyleValue where
  show (RGB r g b) = "rgb(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"
  show (RGBA r g b a) = "rgb(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ", " ++ show a ++ ")"
  show (Value s) = s

showStyle :: Style -> String
showStyle s = unwords $ map show s

data ParseContext = PC { spaceKey :: String
                       , pageName :: String
                       , pagemap :: M.Map (String, String) String
                       , anchormap :: M.Map String String
                       , spacekeys :: [String]
                       , inLink :: Bool
                       , inCode :: Bool
                       , inEm :: Bool
                       , inStrong :: Bool
                       , inULine :: Bool
                       , inTable :: Bool
                       , inSimpleTable :: Bool
                       , inTableBody :: Bool
                       , prevChar :: Char
                       , tabsetId :: Int
                       } deriving (Show, Eq, Ord)

safeLast :: Char -> String -> Char
safeLast d [] = d
safeLast _ s = last s

-- replace :: Eq a => a -> a -> [a] -> [a]
-- replace a b = map $ \c -> if c == a then b else c

lookupWithDefault :: Eq a => b -> a -> [(a, b)] -> b
lookupWithDefault def k kvs = maybe def id $ lookup k kvs

confToStandardEmoticon :: String -> String
confToStandardEmoticon s = case s of
  "information" -> ":information_source:" -- ℹ️
  "thumbs-up" -> ":thumbsup:" -- 👍
  "red-star" -> ":star:" -- ⭐
  "tick" -> ":heavy_check_mark:" -- ✔️
  "cross" -> ":x:" -- ❌ -- alternative would be :heavy_multiplication_x:
  "minus" -> ":heavy_minus_sign:" -- ➖
  "plus" -> ":heavy_plus_sign:" -- ➕
  "warning" -> ":warning:" -- ⚠️
  "smile" -> ":smile:" -- TODO check these three
  "light-on" -> ":light-on:"
  "yellow-star" -> ":yellow-star:"
  _ -> ":" ++ (trace ("WARNING: Unkown emoticon: " ++ s) s) ++ ":"

showElems :: ParseContext -> [Elem] -> String
showElems _ [] = ""
showElems pc (Tag "ac:structured-macro" (Attr "ac:name" "mgnl-f":_) _:e:es) = "[" ++ showElem pc e ++ "]{.f}" ++ showElems pc { prevChar = '}', tabsetId = 1 + tabsetId pc } es
showElems pc (Tag "ac:structured-macro" (Attr "ac:name" "mgnl-l":_) _:e:es) = "[" ++ showElem pc e ++ "]{.l}" ++ showElems pc { prevChar = '}', tabsetId = 1 + tabsetId pc } es
showElems pc (Tag "ac:structured-macro" (Attr "ac:name" "mgnl-n":_) _:e:es) = "[" ++ showElem pc e ++ "]{.n}" ++ showElems pc { prevChar = '}', tabsetId = 1 + tabsetId pc } es
showElems pc (Tag "ac:structured-macro" (Attr "ac:name" "mgnl-p":_) _:e:es) = "[" ++ showElem pc e ++ "]{.p}" ++ showElems pc { prevChar = '}', tabsetId = 1 + tabsetId pc } es
showElems pc (Tag "ac:structured-macro" (Attr "ac:name" "mgnl-wp":_) _:e:es) = "[" ++ showElem pc e ++ "]{.wp}" ++ showElems pc { prevChar = '}', tabsetId = 1 + tabsetId pc } es
showElems pc (e:es) = let s = showElem pc e
                      in  s ++ showElems pc { prevChar = safeLast '\n' s, tabsetId = 1 + tabsetId pc } es

showElemsBrackets :: ParseContext -> [Elem] -> String -> String
showElemsBrackets pc es ss = let es' = if inCode pc then [Tag "code" [] es] else es
                                 s = showElems pc { prevChar = '\n', inCode = False } es'
                             in  (if " " `isPrefixOf` s then " " else "") ++
                                 (if inCode pc then "`" else "") ++
                                 "[" ++ trim s ++ "]" ++ ss ++
                                 (if " " `isSuffixOf` s then " " else "") ++
                                 (if inCode pc then "`" else "")

showElemsBrackets' :: ParseContext -> [Elem] -> String -> String
showElemsBrackets' pc es ss = let s = showElems pc es
                              in  case trim s of
                                    "" -> ""
                                    _  -> showElemsBrackets pc es ss

showElemsBlock :: ParseContext -> [Elem] -> String
showElemsBlock pc es = case showElems pc es of
  "" -> "\n"
  " " -> "\n"
  "  " -> "\n"
  s -> '\n':s ++ "\n"

showElemsBlock' :: ParseContext -> [Elem] -> String -> String -> String
showElemsBlock' pc es pre post = case trim $ showElems pc es of
  "" -> "\n"
  "\\" -> "\n"
  s -> '\n':pre ++ '\n':'\n':s ++ '\n':'\n':post ++ "\n"

indentWith :: String -> String -> String
indentWith s = unlines . map' (s ++) . lines
  where
    map' _ [] = []
    map' f (x:xs) = (if isCodeOrDiv x then x else if isList x then x else f x) : (if isCodeOrDiv x then map'' else map') f xs
    map'' _ [] = []
    map'' f (x:xs) = x : (if isCodeOrDiv x then map' else map'') f xs
    isCodeOrDiv s' = "```" `isInfixOf` s' || ":::" `isInfixOf` s'
    isList ('*':' ':_) = True
    isList _           = False

indentWith' :: String -> String -> String
indentWith' s = unlines . map (s ++) . lines -- let (ss1, ss2) = span (\x -> not ("```" `isInfixOf` x)) (lines s) in intercalate ('\n':i) ss1 ++ unlines (map' (i ++) ss2)
  -- where
    -- map' _ [] = []
    -- map' f (x:xs) = if isCode x then x : map'' f xs else f x : map' f xs
    -- map'' _ [] = []
    -- map'' f (x:xs) = x : (if isCode x then map' else map'') f xs
    -- isCode s' = "```" `isPrefixOf` s'

indent :: String -> String
indent = indentWith "  "

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

trim' :: String -> String -> String
trim' b s = case (trim s, safeHead s, safeLast '.' s) of
  ("", _, _) -> " "
  (s', Just h, l) -> (if isSpace h then h:"" else "") ++ b ++ s' ++ b ++ (if isSpace l then l:"" else "")
  (_, _, _) -> ""

indent' :: String -> String
indent' = indentWith' "  "

uriEncode :: String -> String -- just convert spaces and parens, Pandoc deals with everything else
uriEncode = concatMap uriEncode'
  where
    uriEncode' ' ' = "+"
    uriEncode' '(' = "%28"
    uriEncode' ')' = "%29"
    uriEncode' c = c:""

uriDecode :: String -> String
uriDecode ('+':s) = ' ':uriDecode s
uriDecode ('%':'2':'8':s) = '(':uriDecode s
uriDecode ('%':'2':'9':s) = ')':uriDecode s
uriDecode ('%':'2':'F':s) = '/':uriDecode s
uriDecode ('&':'a':'m':'p':';':s) = '&':uriDecode s
uriDecode ('&':'q':'u':'o':'t':';':s) = '\'':uriDecode s
uriDecode ('\\':'\\':s) = '\\':uriDecode s
uriDecode (c:s) = c:uriDecode s
uriDecode "" = ""

findAnchor :: ParseContext -> String -> String
findAnchor _  ('a':'n':'c':'-':la) = la
findAnchor pc la = M.findWithDefault (uriEncode la) la $ anchormap pc

dropanc :: String -> String
dropanc ('a':'n':'c':'-':la) = la
dropanc la = la

findLink :: ParseContext -> String -> String
findLink pc ('h':'t':'t':'p':'s':':':'/':'/':'d':'o':'c':'u':'m':'e':'n':'t':'a':'t':'i':'o':'n':'.':'m':'a':'g':'n':'o':'l':'i':'a':'-':'c':'m':'s':'.':'c':'o':'m'
             :'/':'d':'i':'s':'p':'l':'a':'y':'/':'D':'O':'C':'S':'/':s) = findLink pc $ uriDecode s
findLink pc ('h':'t':'t':'p':'s':':':'/':'/':'d':'o':'c':'u':'m':'e':'n':'t':'a':'t':'i':'o':'n':'.':'m':'a':'g':'n':'o':'l':'i':'a':'-':'c':'m':'s':'.':'c':'o':'m'
             :'/':'d':'i':'s':'p':'l':'a':'y':'/':s) = findLink pc $ uriDecode s
findLink pc ('h':'t':'t':'p':'s':':':'/':'/':'d':'o':'c':'u':'m':'e':'n':'t':'a':'t':'i':'o':'n':'.':'m':'a':'g':'n':'o':'l':'i':'a':'-':'c':'m':'s':'.':'c':'o':'m'
             :'/':s) = findLink pc $ uriDecode s
findLink _  l@('h':'t':'t':'p':':':_) = uriEncode l
findLink _  l@('h':'t':'t':'p':'s':':':_) = uriEncode l
findLink _  l@('f':'t':'p':':':_) = uriEncode l
findLink _  l@('m':'a':'i':'l':'t':'o':':':_) = uriEncode l
-- findLink pc ('_':l) = findLink pc ('I':l)
findLink pc l = findLink' pc [spaceKey pc] l

externWikiLink :: ParseContext -> String -> String -> String
externWikiLink pc space' l | space' `elem` spacekeys pc = findLink' pc [space',spaceKey pc] l
externWikiLink _ k l = '/' : k ++ '/' : uriEncode l

findLink' :: ParseContext -> [String] -> String -> [Char]
-- findLink' pc keys ('_':l) = findLink' pc keys ('I':l)
findLink' pc keys l = case findBest (keys ++ (spacekeys pc \\ keys)) l (pagemap pc) of
  Just ('/':'_':'I':'n':'c':'l':'u':'s':'i':'o':'n':'s':' ':'l':'i':'b':'r':'a':'r':'y':'/':l') -> "/_i/" ++ uriEncode l'
  Just l' -> uriEncode l'
  Nothing -> '/':uriEncode l

findBest :: [String] -> String -> M.Map (String, String) String -> Maybe String
findBest keys linkname pm = safeHead $ mapMaybe (\k -> M.lookup (k, linkname) pm) keys

decode :: ParseContext -> String -> String
decode pc = escapeDollars . decode' . unpack . toLazyText . htmlEncodedText . pack
  where
    escapeDollars line | not (inCode pc) && length (filter (=='$') line) > 1 = concatMap (\c -> if c == '$' then "\\$" else c:"") line
    escapeDollars line = line

    decode' "" = ""
    decode' ('\x00a0':s) = ' ':decode' s
    decode' ('\x2028':s) = '\n':decode' s
    decode' ('<':s) | not (inCode pc) = '&':'l':'t':';':decode' s
    decode' ('>':s) | not (inCode pc) = '&':'g':'t':';':decode' s
    decode' ('&':s) | not (inCode pc) = '&':'a':'m':'p':';':decode' s
    decode' ('*':s) | not (inCode pc) = '\\':'*':decode' s
    decode' ('\\':s) | not (inCode pc) = '\\':'\\':decode' s
    decode' ('`':s) | not (inCode pc) = '\\':'`':decode' s
    decode' ('_':s) | not (inCode pc) = '\\':'_':decode' s
    decode' (':':':':':':s) | not (inCode pc) = '\\':':':'\\':':':'\\':':':decode' s
    decode' (c:s) = c:decode' s

showIndentTrim :: ParseContext -> [Elem] -> String
showIndentTrim _  es | isEmpty es = ""
showIndentTrim pc es = indent $ trimlines $ showElems pc es

showIndentTrimTable :: ParseContext -> [Elem] -> String
showIndentTrimTable _  es | isEmpty es = ""
showIndentTrimTable pc es = trimlines $ showElems pc es
-- showIndentTrimTable pc es = indent $ trimlines $ showElems pc es

showIndentTrim' :: ParseContext -> String -> [Elem] -> String
showIndentTrim' _  _ es | isEmpty es = ""
showIndentTrim' pc s es = indentWith' s $ trimlines $ showElems pc es

showElem :: ParseContext -> Elem -> String
showElem pc  (Text s) = decode pc s
showElem pc' (Tag tagname attrs elems) = showElem' pc' tagname (sort $ removeSomeStyle attrs) elems
  where
    removeSomeStyle :: [Attr] -> [Attr]
    removeSomeStyle [] = []
    removeSomeStyle ((Attr "colspan" "1"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "ac:alt" _):as) = removeSomeStyle as
    removeSomeStyle ((Attr "ac:align" _):as) = removeSomeStyle as
    removeSomeStyle ((Attr "ac:title" _):as) = removeSomeStyle as
    removeSomeStyle ((Attr "title" ""):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" ('_':_)):as) = removeSomeStyle as -- ignore any class starting with '_'
    removeSomeStyle ((Attr "class" "p1"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "p2"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "s1"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "s2"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "r"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "inline-comment-marker"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "confluence-link"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "with-breadcrumbs"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "auto-cursor-target"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "toc-item-body"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "font-color-normal"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "editable-field inactive"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "wiki-content"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "topictitle"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "title" "Click to edit"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "title" "Follow link"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "caption"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "pm-h4"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "nolink"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "c-link"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "c-message__body"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "v-table-cell-wrapper"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "text"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" ('s':'y':'n':'t':'a':'x':'h':'i':'g':'h':'l':'i':'g':'h':'t':'e':'r':' ':_)):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "page"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "title" "Page 1"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "line number1 index0 alt2"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "external-link"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "external"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "external external-icon"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "paragraph"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" ('h':'i':'g':'h':'l':'i':'g':'h':'t':'-':_)):as) = removeSomeStyle as
    removeSomeStyle ((Attr "title" ('B':'a':'c':'k':'g':'r':'o':'u':'n':'d':' ':'c':'o':'l':'o':'u':'r':' ':_)):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "wrapped"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "text-image__text--heading h3 "):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "container"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "glossary-term"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "MsoNormal"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "line862"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "in-cell-link"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "v-radiobutton v-select-option"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" ('c':'o':'n':'f':'l':'u':'e':'n':'c':'e':'-':'t':'h':'u':'m':'b':'n':'a':'i':'l':'-':'l':'i':'n':'k':' ':_)):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "image-wrap"):as) = removeSomeStyle as

    removeSomeStyle ((Attr "data-highlight-colour" _):as) = removeSomeStyle as
    removeSomeStyle ((Attr "rel" "nofollow"):as) = removeSomeStyle as -- drop the rel="nofollow" since this is not user-generated content
    removeSomeStyle ((Attr "style" ""):as) = removeSomeStyle as
    removeSomeStyle (a@(Attr "style" s):as) = case parse style "" s of
                                            Left _   -> a:removeSomeStyle as
                                            Right ss -> case removeStyleElems ss of
                                              [] -> removeSomeStyle as
                                              s' -> (Attr "style" $ showStyle s'):removeSomeStyle as
    removeSomeStyle (a:as) = a:removeSomeStyle as
    removeStyleElems :: Style -> Style
    removeStyleElems [] = []
    removeStyleElems (s@(StyleElem { styleName="color", styleValue=(RGB r g b) }):ss) = if all (<100) [r,g,b] then removeStyleElems ss else s:removeStyleElems ss
    removeStyleElems (StyleElem { styleName="letter-spacing" }:ss) = removeStyleElems ss
    -- removeStyleElems (StyleElem { styleName="text-align", styleValue=Value "left" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="text-align" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="white-space" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="text-decoration", styleValue=Value "none" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="font-family", styleValue=Value "DINWebPro , Arial , sans-serif" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="font-family", styleValue=Value "Arial , sans-serif" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="font-size" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="width" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="line-height" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="background-color", styleValue=Value "transparent" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="background-image", styleValue=Value "none" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="margin-left", styleValue=Value v }:ss) = StyleElem "margin-left" (Value $ removeDotZero v) : removeStyleElems ss
    removeStyleElems (s:ss) = s:removeStyleElems ss
    removeDotZero ".0px" = "px"
    removeDotZero ".00px" = "px"
    removeDotZero ".000px" = "px"
    removeDotZero (s:ss)  = s:removeDotZero ss
    removeDotZero "" = ""

    maximum' [] = 0
    maximum' xs = maximum xs
    numcols = maximum' . map (length . filter (=='|')) . lines

    splitTableHeader (Tag "thead" _ es:es') = splitTableHeader (es ++ es')
    splitTableHeader (Tag "tbody" _ es:es') = splitTableHeader (es ++ es')
    splitTableHeader (Tag "colgroup" _ _:es) = splitTableHeader es
    splitTableHeader b@(Tag "tr" _ (Tag "td" _ _:_):_) = ([], b)
    splitTableHeader (e:es) = let (h, b) = splitTableHeader es in (e:h, b)
    splitTableHeader [] = ([], [])

    tryTable pc es = let (h, b) = splitTableHeader es
                         (h', b') = (trimlines $ showElems pc { inTable = True, inSimpleTable = True } h, trimlines $ showElems pc { inTable = True, inSimpleTable = True } b)
                         t = h' ++ b'
                         n = numcols t
                         t' = showIndentTrimTable pc { inTable = True, inSimpleTable = False } es
                         notsimple = '\0' `elem` t
                     in  if notsimple
                           then "<table>\n" ++ t' ++ "</table>\n"
                           else if n == 0
                                  then "" -- contentless table
                                  else "\n" ++ (if null h' then "||\n" else h') ++ (intersperse '-' $ replicate n '|') ++ "\n" ++ b' ++ "\n"
                       -- in  case (notsimple, s) of
      -- (True, Nothing)  -> "<table>\n" ++ t' ++ "</table>\n"
      -- (True, Just s')  -> "<table class=\"" ++ s' ++ "\">\n" ++ t' ++ "</table>\n"
      -- (False, Nothing) -> "\n||\n|-|-|-|-|-|\n" ++ t ++ "\n"
      -- (False, Just s') -> "::: " ++ s' ++ "\n  ||||||\n  |-|-|-|-|-|\n" ++ indent t ++ "\n:::\n"

    showElem' pc "p" [] es = ('\n':) $ trimlines $ showElemsBlock pc es
    showElem' pc "p" [Attr "style" s] es = "\n" ++ (showElemsBrackets' pc es $ "{style=\"" ++ s ++ "\"}") ++ "\n"
    showElem' pc "p" as es = case lookup "class" (map (\(Attr n v) -> (n, v)) as) of
      Just ('e':'m':'o':'t':'i':'c':'o':'n':' ':'e':'m':'o':'t':'i':'c':'o':'n':'-':s) -> "\n" ++ confToStandardEmoticon s ++ " " ++ showElems pc es ++ "\n"
      Just _ -> ('\n':) $ trimlines $ showElemsBlock pc es
      _ -> traceShow ("ERROR"::String, pageName pc, as, es) ""
    showElem' pc "span" [] es = showElems pc es
    showElem' pc "span" [Attr "title" ""] es = showElems pc es
    showElem' pc "span" [Attr "style" s] es = showElemsBrackets' pc es $ "{style=\"" ++ s ++ "\"}"
    showElem' pc "span" as es = case lookup "class" (map (\(Attr n v) -> (n, v)) as) of
      Just ('e':'m':'o':'t':'i':'c':'o':'n':' ':'e':'m':'o':'t':'i':'c':'o':'n':'-':s) -> confToStandardEmoticon s ++ showElems pc es
      Just s -> showElemsBrackets pc es $ "{." ++ s ++ "}"
      _ -> traceShow ("ERROR"::String, pageName pc, as, es) ""
    showElem' _  "hr" [] [] = "---\n"
    showElem' pc "br" _ [] | inSimpleTable pc = ""
    showElem' pc "br" _ [] | inLink pc = ""
    showElem' _  "br" _ [] = "\\\n"
    showElem' pc "table" _ es = tryTable pc es
    -- showElem' pc "table" (Attr "class" s:_) es = tryTable pc es (Just s)
    -- showElem' pc "table" [] es = tryTable pc es Nothing
    showElem' _  "colgroup" [] _ = ""
    showElem' pc "thead" [] es = showElems pc es
    showElem' pc "tbody" [] es = showElems pc es
    showElem' pc "tr" [] es | inSimpleTable pc = let t = trim $ trimlines $ showElems pc es
                                                 in  if '\n' `elem` t then "\0" else t ++ "|\n"
    showElem' pc "tr" [] es = "<tr>\n" ++ showIndentTrimTable pc es ++ "</tr>\n"
    showElem' pc "tr" [Attr "style" _] _ | inSimpleTable pc = "\0" -- FIXME: maybe try to pass through left-margins instead
    showElem' pc "tr" [Attr "style" s] es = "<tr style=\"" ++ s ++ "\">\n" ++ showIndentTrimTable pc es ++ "</tr>\n"
    showElem' pc "td" [Attr "colspan" _] _ | inSimpleTable pc = "\0"
    showElem' pc "td" [Attr "rowspan" _] _ | inSimpleTable pc = "\0"
    showElem' pc "td" _ es | inSimpleTable pc = "|" ++ (trim $ trimlines $ showElems pc es) -- FIXME: make sure left-margin styles on TRs and TDs are supported by simple tables
    showElem' pc "td" [] es = "<td>\n" ++ showIndentTrimTable pc es ++ "</td>\n"
    showElem' pc "td" [Attr "colspan" cs] es = "<td colspan=\"" ++ cs ++ "\">\n" ++ showIndentTrimTable pc es ++ "</td>\n"
    showElem' pc "td" [Attr "rowspan" cs] es = "<td rowspan=\"" ++ cs ++ "\">\n" ++ showIndentTrimTable pc es ++ "</td>\n"
    showElem' pc "td" [Attr "style" s] es = "<td style=\"" ++ s ++ "\">\n" ++ showIndentTrimTable pc es ++ "</td>\n"
    showElem' pc "th" [Attr "colspan" _] _ | inSimpleTable pc = "\0"
    showElem' pc "th" [Attr "rowspan" _] _ | inSimpleTable pc = "\0"
    showElem' pc "th" _ es | inSimpleTable pc = "|" ++ (trim $ trimlines $ showElems pc es) -- FIXME: header needs to come first somehow
    showElem' pc "th" [Attr "colspan" cs] es = "<th colspan=\"" ++ cs ++ "\">\n" ++ showIndentTrimTable pc es ++ "</th>\n"
    showElem' pc "th" [Attr "rowspan" cs] es = "<th rowspan=\"" ++ cs ++ "\">\n" ++ showIndentTrimTable pc es ++ "</th>\n"
    showElem' pc "th" [] es = "<th>\n" ++ showIndentTrimTable pc es ++ "</th>\n"
    showElem' pc "div" [Attr "class" "content-wrapper", Attr "style" s] es = "[" ++ (trim $ trimlines $ showElems pc es) ++ "]{style=\"" ++ s ++ "\"}"
    showElem' pc "div" [Attr "class" "tablesorter-header-inner"] es = (trim $ trimlines $ showElems pc es)
    showElem' pc "div" [Attr "class" "content-wrapper"] es = (trim $ trimlines $ showElems pc es)
    showElem' pc "div" [Attr "class" "layoutArea"] es = showElemsBlock' pc es "::: columns" ":::"
    showElem' pc "div" [Attr "class" "column"] es = showElemsBlock' pc es "::: column" ":::"
    -- showElem' pc "div" [] es = "\n::: {}\n" ++ showIndentTrim pc es ++ ":::\n"
    showElem' pc "div" [Attr "class" c] es = "\n::: " ++ c ++ showElemsBlock pc es ++ ":::\n"
    showElem' pc "div" [] es = showElemsBlock pc es
    showElem' pc "h1" _ es = "\n# " ++ showElems pc es ++ "\n"
    showElem' pc "h2" _ es = "\n## " ++ showElems pc es ++ "\n"
    showElem' pc "h3" _ es = "\n### " ++ showElems pc es ++ "\n"
    showElem' pc "h4" _ es = "\n#### " ++ showElems pc es ++ "\n"
    showElem' pc "h5" _ es = "\n##### " ++ showElems pc es ++ "\n"
    showElem' pc "h6" _ es = "\n###### " ++ showElems pc es ++ "\n"
    showElem' pc "label" [] es = showElems pc es
    showElem' pc "a" [] es = showElems pc es
    showElem' pc "a" [Attr "href" l] es = showElemsBrackets pc { inLink = True } es $ "(" ++ findLink pc l ++ ")"
    showElem' pc "a" [Attr "class" c, Attr "href" l, Attr "title" t] es = showElemsBrackets pc { inLink = True } es $ "(" ++ findLink pc l ++ "){" ++ allclasses c ++ " title=\"" ++ t ++ "\"}"
    showElem' pc "a" [Attr "href" l, Attr "title" t] es = showElemsBrackets pc { inLink = True } es $ "(" ++ findLink pc l ++ "){title=\"" ++ t ++ "\"}"
    showElem' pc "a" [Attr "href" l, Attr "style" s] es = showElemsBrackets pc { inLink = True } es $ "(" ++ findLink pc l ++ "){style=\"" ++ s ++ "\"}"
    showElem' _  "ac:emoticon" [Attr "ac:name" s] [] = confToStandardEmoticon s
    showElem' pc "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []] | inCode pc = "`[`" ++ l ++ "`](" ++ findLink pc l ++ ")`"
    showElem' pc "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []] = "[" ++ l ++ "](" ++ findLink pc l ++ ")"
    showElem' pc "ac:link" [] (Tag "ri:page" [Attr "ri:content-title" l] []:es) = showElemsBrackets pc { inLink = True } es $ "(" ++ findLink pc l ++ ")"
    showElem' pc "ac:link" [] (Tag "ri:space" [Attr "ri:space-key" k] []:es) = showElemsBrackets pc { inLink = True } es $ "(" ++ externWikiLink pc k "" ++ ")"
    showElem' pc "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" k, Attr "ri:content-title" l] []] = "[" ++ l ++ "](" ++ externWikiLink pc k l ++ ")"
    showElem' pc "ac:link" [] (Tag "ri:page" [Attr "ri:space-key" k, Attr "ri:content-title" l] []:es) = showElemsBrackets pc { inLink = True } es $ "(" ++ externWikiLink pc k l ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] (Tag "ri:page" [Attr "ri:space-key" k, Attr "ri:content-title" l] []:es) = showElemsBrackets pc { inLink = True } es $ "(" ++ externWikiLink pc k l ++ "#" ++ findAnchor pc la ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] (Tag "ri:page" [Attr "ri:content-title" l] []:es) = showElemsBrackets pc { inLink = True } es $ "(" ++ findLink pc l ++ "#" ++ findAnchor pc la ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] [] | inCode pc = "`[`" ++ la ++ "`](#" ++ findAnchor pc la ++ ")`"
    showElem' pc "ac:link" [Attr "ac:anchor" la] [] = "[" ++ la ++ "](#" ++ findAnchor pc la ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] es = showElemsBrackets pc { inLink = True } es $ "(#" ++ findAnchor pc la ++ ")"
    showElem' pc "ac:link" [] (Tag "ri:attachment" [Attr "ri:filename" t] []:[]) = "[" ++ t ++ "](/_showraw" ++ (replaceFileName (findLink pc $ pageName pc) t) ++ "){download=\"" ++ t ++ "\"}"
    showElem' pc "ac:link" [] (Tag "ri:attachment" [Attr "ri:filename" t] []:es) = showElemsBrackets pc { inLink = True } es $ "(/_showraw" ++ (replaceFileName (findLink pc $ pageName pc) t) ++ "){download=\"" ++ t ++ "\"}"
    showElem' _  "ac:link" [] [Tag "ri:user" _ []] = "[\\@Unknown User]{.user title=\"User linking not supported.\"}"
    showElem' pc "ac:link" _ es = showElems pc es -- empty link, ignore
    showElem' pc "ac:image" [] [Tag "ri:attachment" [Attr "ri:filename" f] [Tag "ri:page" [Attr "ri:content-title" p] es]] = "!" ++ (showElemsBrackets pc es $ "(" ++ (replaceFileName (findLink pc p) f) ++ ")")
    showElem' pc "ac:image" [] [Tag "ri:attachment" [Attr "ri:filename" f] [Tag "ri:page" [Attr "ri:space-key" k, Attr "ri:content-title" p] es]] = "!" ++ (showElemsBrackets pc es $ "(" ++ externWikiLink pc k p ++ "/" ++ f ++ ")")
    showElem' pc "ac:image" [] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ ")")
    showElem' pc "ac:image" [Attr "ac:thumbnail" "true"] [Tag "ri:attachment" [Attr "ri:filename" f] [Tag "ri:page" [Attr "ri:content-title" p] es]] = "!" ++ (showElemsBrackets pc es $ "(" ++ (replaceFileName (findLink pc p) f) ++ "){.inline}")
    showElem' pc "ac:image" [Attr "ac:thumbnail" "true"] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){.inline}")
    showElem' pc "ac:image" [Attr "ac:height" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){" ++ (if (read val :: Int) > 50 then "" else ".inline ") ++ "height=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:height" val, Attr "ac:thumbnail" "true"] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){.inline height=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] [Tag "ri:page" [Attr "ri:content-title" p] es]] = "!" ++ (showElemsBrackets pc es $ "(" ++ (replaceFileName (findLink pc p) f) ++ "){width=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){" ++ (if (read val :: Int) > 50 then "" else ".inline ") ++ "width=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:thumbnail" "true", Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] [Tag "ri:page" [Attr "ri:content-title" p] es]] = "!" ++ (showElemsBrackets pc es $ "(" ++ (replaceFileName (findLink pc p) f) ++ "){.inline width=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:thumbnail" "true", Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){.inline width=" ++ val ++ "}")
    showElem' pc "ac:inline-comment-marker" _ es = showElems pc es
    showElem' _  "ac:parameter" [Attr "ac:name" "atlassian-macro-output-type"] [Text _] = "" -- macros are either always "inline" or always "block", so this parameter is actually unnecessary
    showElem' pc "ac:link-body" _ es = showElems pc { inLink = True } es
    showElem' pc "ac:plain-text-link-body" _ es = showElems pc { inLink = True } es
    showElem' pc "ac:plain-text-body" _ es = showElems pc es
    showElem' pc "ac:rich-text-body" _ es = showElems pc es
    showElem' pc "ac:structured-macro" (_:Attr "ac:name" macroName:_) es = processMacro pc macroName es
    showElem' pc "code" _ es | inCode pc = trim $ showElems pc es
    showElem' _  "code" _ es | isEmpty es = ""
    showElem' pc "code" _ es = trim' "`" $ showElems pc { inCode = True } es
    showElem' pc "pre" _ es | inCode pc = trim $ showElems pc es
    showElem' _  "pre" _ es | isEmpty es = ""
    showElem' pc "pre" _ es = trim' "`" $ showElems pc { inCode = True } es
    showElem' pc "em" _ es | inEm pc = trim $ showElems pc es
    showElem' pc "em" _ es | inCode pc = "`" ++ showElem' pc { inCode = False } "em" [] [Tag "code" [] es] ++ "`"
    showElem' _  "em" _ es | isEmpty es = ""
    showElem' pc "em" _ es = trim' "*" $ showElems pc { inEm = True } es
    showElem' pc "strong" _ es | inStrong pc = trim $ showElems pc es
    showElem' pc "strong" _ es | inCode pc = "`" ++ showElem' pc { inCode = False } "strong" [] [Tag "code" [] es] ++ "`"
    showElem' _  "strong" _ es | isEmpty es = ""
    showElem' pc "strong" _ es = trim' "**" $ showElems pc { inStrong = True } es
    showElem' pc "u" _ es | inULine pc = trim $ showElems pc es
    showElem' pc "u" _ es | inCode pc = "`" ++ showElem' pc { inCode = False } "u" [] [Tag "code" [] es] ++ "`"
    showElem' _  "u" _ es | isEmpty es = ""
    showElem' pc "u" _ es = trim' "__" $ showElems pc { inULine = True } es
    showElem' pc "sup" _ es = "^" ++ showElems pc es ++ "^"
    showElem' pc "ul" _ _ | inSimpleTable pc = "\0"
    showElem' pc "ul" _ es | all (not . isli) es = showElems pc es -- ul with no lis
    showElem' pc "ul" _ es = "\n" ++ concatMap (showul pc) es
    showElem' pc "ol" _ _ | inSimpleTable pc = "\0"
    showElem' pc "ol" _ es | all (not . isli) es = showElems pc es -- ol with no lis
    showElem' pc "ol" _ es = "\n" ++ concatMap (showol pc) es
    showElem' pc "blockquote" _ _ | inSimpleTable pc = "\0"
    showElem' pc "blockquote" [] es = "\n" ++ indentWith "> " (unlines . dropWhile (=="") . lines $ showElems pc es) ++ "\n"
    --showElem' pc "ac:layout" [] es = "\n::: layout" ++ showElemsBlock pc es ++ ":::\n" -- FIXME: use markdown .columns (https://pandoc.org/MANUAL.html#columns - maybe just for slides)
    --showElem' pc "ac:layout-section" [Attr "ac:type" t] es = "\n::: {.layout-section ." ++ t ++ "}" ++ showElemsBlock pc es ++ ":::\n"
    --showElem' pc "ac:layout-cell" [] es = "\n::: cell" ++ showElemsBlock pc es ++ ":::\n"
    showElem' pc "ac:layout" _ es = showElemsBlock pc es
    showElem' pc "ac:layout-section" _ es = showElemsBlock' pc es "::: columns" ":::"
    showElem' pc "ac:layout-cell" _ es = showElemsBlock' pc es "::: column" ":::"
    showElem' pc  "img" as es = case lookup "class" (map (\(Attr n v) -> (n, v)) as) of
      Just ('e':'m':'o':'t':'i':'c':'o':'n':' ':'e':'m':'o':'t':'i':'c':'o':'n':'-':s) -> confToStandardEmoticon s
      _ -> traceShow ("ERROR"::String, pageName pc, as, es) ""
    showElem' pc "ac:task-list" _ es = showElemsBlock pc es
    showElem' _  "ac:task" _ [Text _, Tag "ac:task-id" _ _, Text _, Tag "ac:task-status" _ [Text taskstatus], Text _, Tag "ac:task-body" _ [Text taskbody], Text _] = "\n- [" ++ (if trim taskstatus == "complete" then "x" else " ") ++ "]" ++ if not $ null $ trim taskbody then ' ':trim taskbody else "" ++ "\n"
    showElem' pc t as es = traceShowId $ pageName pc ++ ": <" ++ t ++ show as ++ ">" ++ showElemsBlock pc es ++ "</" ++ t ++ ">"

isli :: Elem -> Bool
isli (Tag "li" _ _) = True
isli _ = False

isEmpty :: [Elem] -> Bool
isEmpty [] = True
isEmpty (Text t:es) | all isSpace t = isEmpty es
isEmpty (Tag "br" _ []:es) = isEmpty es
isEmpty _ = False

    -- optNL (PC{prevChar = '\n'}) = ""
    -- optNL _ = "\n"

processMacroParams :: [Elem] -> ([(String, String)], [Elem])
processMacroParams es = let (ps, es') = processMacroParams' es in (sort ps, es')
  where
    processMacroParams' (Tag "ac:parameter" [Attr "ac:name" n] [Text v]:es'') = let (ps, es''') = processMacroParams' es'' in filterParams n v ps es'''
    processMacroParams' (e:es'') = let (ps, es''') = processMacroParams' es'' in (ps, e:es''')
    processMacroParams' [] = ([], [])
    filterParams "atlassian-macro-output-type" "BLOCK" ps restes = (ps, restes)
    filterParams "atlassian-macro-output-type" "INLINE" ps restes = (ps, restes)
    filterParams "enableHeadingAttributes" "false" ps restes = (ps, restes)
    filterParams "enableHighlighting" "false" ps restes = (ps, restes)
    filterParams "enableSorting" "false" ps restes = (ps, restes)
    filterParams "multiple" "false" ps restes = (ps, restes)
    filterParams "heading" "0" ps restes = (ps, restes)
    filterParams "all" "true" ps restes = (ps, restes)
    filterParams "highlightColor" "@default" ps restes = (ps, restes)
    filterParams n v ps restes = ((n, v) : ps, restes)

allclasses :: String -> String
allclasses = unwords . map ('.':) . words

trimlines :: String -> String
trimlines = unlines . trimlines1 . lines
  where
    trimlines1 s = dropWhile null' $ reverse $ dropWhile null' $ reverse s
    null' = all isSpace

processMacro :: ParseContext -> String -> [Elem] -> String
processMacro pc _ es | (Tag "ac:parameter" [Attr "ac:name" "hidden"] [Text "true"]) `elem` es = "<!-- " ++ (trim $ trimlines $ showElems pc $ filter isNotParam es) ++ " -->\n"
  where
    isNotParam (Tag "ac:parameter" _ _) = False
    isNotParam _ = True
processMacro pc "aui-button" es = let (_, es') = processMacroParams es
                                  in  showElems pc es'
processMacro _  "mgnl-get" [] = "[GET]{.get}"
processMacro _  "mgnl-put" [] = "[PUT]{.put}"
processMacro _  "mgnl-post" [] = "[POST]{.post}"
processMacro _  "mgnl-delete" [] = "[DELETE]{.delete}"
processMacro pc "list-children" es = processMacro pc "children" es
processMacro pc "children" es = let (_, es') = processMacroParams es
                                in  case es' of
                                      [Tag "ac:parameter" [Attr "ac:name" "page"] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]] -> "[" ++ findLink pc l ++ "](!children)"
                                      _ -> "[](!children)"
processMacro pc "anchor" [Tag "ac:parameter" [Attr "ac:name" ""] name] | inCode pc = "`[]{#" ++ (dropanc $ dropWhile (=='#') $ showElems pc name) ++ "}`"
processMacro pc "anchor" [Tag "ac:parameter" [Attr "ac:name" ""] name] = "[]{#" ++ (dropanc $ dropWhile (=='#') $ showElems pc name) ++ "}"

processMacro pc "mgnl-app-lauchner" es = let (pss, es') = processMacroParams es -- TODO
                                         in  "(mgnl-app-launcher: " ++ showElems pc es' ++ show pss ++ ")"

processMacro pc "localtab" es = processMacro pc "localtabgroup" [Tag "ac:structured-macro" [Attr "ac:name" "localtab"] (Tag "ac:parameter" [Attr "ac:name" "active"] [Text "true"]:es)] -- put isolated localtab into its own tabgroup
processMacro pc "localtabgroup" es = "\n::: tabset\n\n" ++ showTabGroup (processTabGroupMacro es) ++ "\n:::\n"
  where
    processTabGroupMacro [] = []
    processTabGroupMacro (Tag "p" [Attr "class" "auto-cursor-target"] [Tag "br" [] []]:rest) = processTabGroupMacro rest
    processTabGroupMacro (Tag "ac:rich-text-body" _ es':rest) = processTabGroupMacro es' ++ processTabGroupMacro rest
    processTabGroupMacro (Tag "ac:structured-macro" (Attr "ac:name" "localtab":_) es':rest) = let (pss, es'') = processMacroParams es'
                                                                                                  title = lookupWithDefault "" "title" pss
                                                                                                  checked = ("active", "true") `elem` pss
                                                                                              in  (title, showElems pc es'', checked):processTabGroupMacro rest
    processTabGroupMacro es' = trace ("WARNING: " ++ pageName pc ++ ": unknown structure for processTabGroupMacro: " ++ show es') []
    showTabGroup ts = concatMap showRadio ts ++ "\n::: tab-panels\n\n" ++ concatMap showTab ts ++ "\n\n:::\n\n"
    showRadio (title, _, checked) = let title' = idEncode title ++ tabsetId'
                                        checked' = if checked then " checked" else ""
                                    in  "<input type=\"radio\" name=\"tabset" ++ tabsetId' ++ "\" id=\"" ++ title' ++ "-button\"" ++ checked'
                                        ++ "><label for=\"" ++ title' ++ "-button\">" ++ title ++ "</label>\n"
    showTab (title, tabtext, _) = "\n::: {.tab-panel #" ++ idEncode title ++ tabsetId' ++ "}\n\n" ++ tabtext ++ "\n\n:::\n\n"
    idEncode "" = ""
    idEncode (c:s) | isAlphaNum c = c:idEncode s
    idEncode (_:s) = idEncode s
    tabsetId' = show (tabsetId pc)

processMacro pc "javadoc" es = let (pss, es') = processMacroParams es
                                   classname = maybe (lookup "" pss) Just $ maybe (lookup "0" pss) Just $ lookup "className" pss
                               in  case classname of
                                     Nothing -> trace ("ERROR: " ++ pageName pc ++ ": no classname set in: (javadoc: " ++ showElems pc es' ++ show pss ++ ")") ""
                                     Just classname' -> "[" ++ classname' ++ "](!javadoc)"
processMacro pc "javadoc-resource-link-macro" es = processMacro pc "javadoc" es

processMacro pc "artifact-maven-dependencies-snippet-macro" es = let (pss, es') = processMacroParams es -- TODO
                                                                 -- in  traceId $ "(artifact-maven-dependencies-snippet-macro: " ++ showElems pc es' ++ show pss ++ ")"
                                                                 in  "(artifact-maven-dependencies-snippet-macro: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "artifact-guess-resource-macro" es = let (pss, es') = processMacroParams es -- TODO
                                                     -- in  traceId $ "(artifact-guess-resource-macro: " ++ showElems pc es' ++ show pss ++ ")"
                                                     in  "(artifact-guess-resource-macro: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "artifact-resource-macro" es = let (pss, es') = processMacroParams es -- TODO
                                                   label' = lookupWithDefault "(no label)" "label" pss
                                               -- in  traceId $ "[" ++ label' ++ "](" ++ showElems pc es' ++ show pss ++ ")"
                                               in  "[" ++ label' ++ "](" ++ showElems pc es' ++ show pss ++ ")"

processMacro pc "jira" es = let (pss, es') = processMacroParams es
                                jql = maybe (("key="++) <$> lookup "key" pss) Just $ lookup "jqlQuery" pss
                            in  case jql of
                                  Nothing -> trace ("ERROR: " ++ pageName pc ++ ": no jira jql: " ++ showElems pc es' ++ show pss) ""
                                  Just jql' -> "[" ++ jql' ++ "](!jira)"
processMacro pc "hide-macro" es = let (pss, es') = processMacroParams es
                                  in  "<!-- " ++ showElems pc es' ++ show pss ++ " -->"
processMacro pc "code" es = showCodeMacro pc False Nothing [] [] Nothing es
processMacro pc "code-pro" es = showCodeMacro pc False Nothing [] [] Nothing es
processMacro pc "noformat" es = showCodeMacro pc False Nothing [] [] Nothing es
processMacro pc "mgnl-mini-code-snippet" es = showCodeMacro pc False Nothing [] [] Nothing es
processMacro pc "details" es = showElems pc es
processMacro pc "excerpt" es = let (_, es') = processMacroParams es
                               in  "\n::: excerpt\n" ++ (trimlines $ showElems pc es') ++ ":::\n"
processMacro pc "excerpt-include" es = let (_, es') = processMacroParams es
                                           pan = id -- if lookup "nopanel" pss == Just "true" then id else \x -> "\n::: excerpt-panel\n  " ++ x ++ "\n:::\n"
                                           link = case es' of
                                                   [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]] -> findLink pc l
                                                   [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" s, Attr "ri:content-title" l] []]]] -> externWikiLink pc s l
                                                   _ -> trace ("ERROR: " ++ pageName pc ++ ": Unkown link type in excerpt-include") ""
                                       in  pan $ "[" ++ link ++ "](!excerpt-include)"
processMacro pc "multiexcerpt" es = let (pss, es') = processMacroParams es
                                        name = case lookup "MultiExcerptName" pss of
                                                 Just name' -> "{.excerpt #" ++ toIdent name' ++ "}"
                                                 Nothing    -> "excerpt"
                                    in  "\n::: " ++ name ++ "\n" ++ (trimlines $ showElems pc es') ++ ":::\n"
processMacro pc "multiexcerpt-include" es = let (pss, es') = processMacroParams es
                                                pan = id -- if lookup "nopanel" pss == Just "true" then id else \x -> "\n::: excerpt-panel\n  " ++ x ++ "\n:::\n"
                                                name = case lookup "MultiExcerptName" pss of
                                                         Just name' -> '#':toIdent name'
                                                         Nothing -> ""
                                                link = case es' of
                                                        [Tag "ac:parameter" [Attr "ac:name" "PageWithExcerpt"] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]] -> findLink pc l
                                                        [Tag "ac:parameter" [Attr "ac:name" "SpaceWithExcerpt"] [Tag "ri:space" [Attr "ri:space-key" s] []],
                                                         Tag "ac:parameter" [Attr "ac:name" "PageWithExcerpt"] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]] -> externWikiLink pc s l
                                                        [Tag "ac:parameter" [Attr "ac:name" "PageWithExcerpt"] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" s, Attr "ri:content-title" l] []]]] -> externWikiLink pc s l
                                                        [Tag "ac:parameter" [Attr "ac:name" "SpaceWithExcerpt"] [Tag "ri:space" [Attr "ri:space-key" s] []], -- TODO check that space keys match?
                                                         Tag "ac:parameter" [Attr "ac:name" "PageWithExcerpt"] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" _, Attr "ri:content-title" l] []]]] -> externWikiLink pc s l
                                                        _ -> trace ("ERROR: " ++ pageName pc ++ ": Unkown link type in excerpt-include") ""
                                            in  pan $ "[" ++ link ++ name ++ "](!excerpt-include)"
processMacro pc "expand" es = let (_, es') = processMacroParams es -- TODO
                              in  showElems pc es'
processMacro _  "livesearch" _ = "<!-- live search used to be here -->\n" -- TODO
processMacro _  "youtube-macro" es = let (ps, _) = processMacroParams es
                                         videoId = maybe "" id $ lookup "videoId" ps
                                         width = maybe "" (\w -> " width=\"" ++ w ++ "\"") $ lookup "width" ps
                                         height = maybe "" (\h -> " height=\"" ++ h ++ "\"") $ lookup "height" ps
                                     in "[" ++ videoId ++ width ++ height ++ "](!youtube)"
--                                      in "`<iframe" ++ width ++ height ++ " src=\"https://www.youtube.com/embed/" ++ videoId
--                                                   ++ "?rel=0\" frameborder=\"0\" allowfullscreen id=\"player" ++ show (tabsetId pc) ++ "\"></iframe>`{=html}"
processMacro pc "html-wrap" es = let (ps, es') = processMacroParams es
                                     (ps', _) = processMacroParamsS' es
                                     myclass = lookup "class" ps
                                 in case myclass of
                                      Just "menu" -> "\n::: infobox\n" ++ (trimlines $ showElems pc es') ++ ":::\n"
                                      Just "thumbnail" -> showElems pc es'
                                      Just myclass' -> trace ("WARNING:" ++ pageName pc ++ ": other class: " ++ myclass') $ "\n::: {" ++ ps' ++ "}\n" ++ (trimlines $ showElems pc es') ++ ":::\n"
                                      Nothing -> "\n::: {" ++ ps' ++ "}\n" ++ (trimlines $ showElems pc es') ++ ":::\n"
  where
    processMacroParamsS' es' = let (ps, es'') = processMacroParams es' in (unwords $ mapMaybe processMacroParamS ps, es'')
    processMacroParamS ("class", v) = Just $ allclasses v
    processMacroParamS ("align", "right") = Just "style=\"overflow: hidden; float: right;\""
    processMacroParamS ("float", "right") = Just "style=\"overflow: hidden; float: right;\""
    processMacroParamS (n, v) = Just $ n ++ "=\"" ++ v ++ "\""
processMacro pc "include" [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" s, Attr "ri:content-title" l] []]]] = "[" ++ externWikiLink pc s l ++ "](!include)"
processMacro pc "include" [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]] = "[" ++ findLink pc l ++ "](!include)"
processMacro pc "bestpractice" es = mkBoxDiv pc "bestpractice" es
processMacro pc "info" es = mkBoxDiv pc "info" es
processMacro pc "note" es = mkBoxDiv pc "note" es
processMacro pc "tip" es = mkBoxDiv pc "tip" es
processMacro pc "warning" es = mkBoxDiv pc "warning" es
processMacro pc "list-items-wrapper-macro" es = showElems pc es
processMacro pc "status" es = let (_, es') = processMacroParams es -- FIXME: take params into account
                              in  showElemsBrackets' pc es' $ "{.status}"
processMacro pc "table-plus" es = let (pss, es') = processMacroParams es
                                      ps = unwords $ "table" : (mapMaybe filterAttrs pss)
                                      filterAttrs ("columnAttributes", _) = Nothing
                                      filterAttrs (n, v) = Just $ n ++ "=\"" ++ v ++ "\""
                                      output [] = ""
                                      output (e1@(Tag t _ _):restes) | t /= "table" = showElems pc [e1] ++ output restes
                                      output [Tag "table" _ myes] = "\n<" ++ ps ++ ">\n" ++ showIndentTrimTable pc myes ++ "</table>\n"
                                      output (t1@(Tag "table" _ _):e2:restes) = output [t1] ++ output (e2:restes)
                                      output myes = error $ "Unhandled table-plus macro\n!!!table\n{" ++ ps ++ "}\n" ++ showElems pc myes ++ "!!!end table\n"
                                  in output es'
processMacro _  "toc" _ = "[](!toc)"
processMacro pc "section" es = let (_, es') = processMacroParams es -- ignore parameters
                               in  showElemsBlock' pc es' "::: columns" ":::"
processMacro pc "column" es = let (_, es') = processMacroParams es -- ignore width parameter
                              in  showElemsBlock' pc es' "::: column" ":::"
processMacro _  "comment" _ = ""
processMacro _  "recently-updated" _ = "<!-- recently-updated macro not supported -->"
processMacro pc "mgnl-f" es = "[" ++ showElems pc es ++ "]{.f}"
processMacro pc "mgnl-l" es = "[" ++ showElems pc es ++ "]{.l}"
processMacro pc "mgnl-n" es = "[" ++ showElems pc es ++ "]{.n}"
processMacro pc "mgnl-p" es = "[" ++ showElems pc es ++ "]{.p}"
processMacro pc "mgnl-wp" es = "[" ++ showElems pc es ++ "]{.wp}"
processMacro pc "div" es = showElems pc es
processMacro _  "contentbylabel" es = let (ps, _) = processMacroParams es
                                          labellist = unwords $ maybe [] getLabels $ lookup "cql" ps
                                      in  "[" ++ labellist ++ "](!label)"
  where
    getLabels ('&':'q':'u':'o':'t':';':s) = getLabels' "" s
    getLabels (_:s) = getLabels s
    getLabels "" = []
    getLabels' acc ('&':'q':'u':'o':'t':';':s) = acc:getLabels s
    getLabels' acc (c:s) = getLabels' (acc ++ [c]) s
    getLabels' _ "" = []
-- processMacro _ _ _ = "" -- unknown macro, ignore for demo purposes
processMacro pc name es = let (ps, es') = processMacroParamsS es in trace ("\n" ++ pageName pc ++ ": !" ++ name ++ ps ++ show es') (if null es' then "" else showElemsBlock pc es')
  where
    processMacroParamsS es' = let (ps, es'') = processMacroParams es' in (concatMap processMacroParamS ps, es'')
    processMacroParamS (n, v) = "(" ++ n ++ "=\"" ++ v ++ "\")"

mkBoxDiv :: ParseContext -> String -> [Elem] -> String
mkBoxDiv pc boxtype es = let (pss, es') = processMacroParams es
                             title = maybe "" (\t -> "  **" ++ t ++ "**\n\n") $ lookup "title" pss
                         in  "\n::: " ++ boxtype ++ "\n" ++ title ++ showIndentTrim pc es' ++ ":::\n"

chooseLang :: String -> String -- translate language string to class attribute
chooseLang "html/xml" = ".xml"
chooseLang s = '.':s

showCodeMacro :: ParseContext -> Bool -> Maybe String -> [String] -> [String] -> Maybe String -> [Elem] -> String
showCodeMacro pc _        title attrs sections url (Tag "ac:parameter" [Attr "ac:name" ""] [Text t]:es) = showCodeMacro pc True title (chooseLang t:attrs) sections url es
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "language"] [Text "text"]:es) = showCodeMacro pc haveLang title attrs sections url es
showCodeMacro pc _        title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "language"] [Text t]:es) = showCodeMacro pc True title (chooseLang t:attrs) sections url es
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "firstline"] [Text "1"]:es) = showCodeMacro pc haveLang title attrs sections url es
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "firstline"] [Text t]:es) = showCodeMacro pc haveLang title (("startFrom=\"" ++ t ++ "\""):attrs) sections url es
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "linenumbers"] [Text "true"]:es) = showCodeMacro pc haveLang title (".numberLines":attrs) sections url es
showCodeMacro pc haveLang _     attrs sections url (Tag "ac:parameter" [Attr "ac:name" "title"] [Text t]:es) = showCodeMacro pc haveLang (Just t) attrs sections url es
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "collapse"] [Text _]:es) = showCodeMacro pc haveLang title attrs sections url es
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "sections"] [Text t]:es) = showCodeMacro pc haveLang title attrs (t:sections) url es
showCodeMacro pc haveLang title attrs sections _   (Tag "ac:parameter" [Attr "ac:name" "url"] [Text t]:es) = showCodeMacro pc haveLang title attrs sections (Just t) es
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "theme"] [Text _]:es) = showCodeMacro pc haveLang title attrs sections url es -- ignore
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "atlassian-macro-output-type"] [Text _]:es) = showCodeMacro pc haveLang title attrs sections url es -- ignore
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "profile"] [Text _]:es) = showCodeMacro pc haveLang title attrs sections url es -- ignore
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "Style"] [Text _]:es) = showCodeMacro pc haveLang title attrs sections url es -- ignore
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "user"] [Text _]:es) = showCodeMacro pc haveLang title attrs sections url es -- ignore
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "password"] [Text _]:es) = showCodeMacro pc haveLang title attrs sections url es -- ignore
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" "collapseType"] [Text _]:es) = showCodeMacro pc haveLang title attrs sections url es -- ignore
showCodeMacro pc haveLang title attrs sections url (Tag "ac:parameter" [Attr "ac:name" n] [Text v]:es) = trace ("WARNING: " ++ pageName pc ++ ": Unknown attribute in code macro: " ++ n ++ " = " ++ v) $ showCodeMacro pc haveLang title attrs sections url es
showCodeMacro pc haveLang title attrs sections url es = "\n" ++ (withTitle title $ case url of
                                                                                     Nothing   -> "```" ++ t ++ "\n" ++ s ++ "\n```\n"
                                                                                     Just url' -> "[" ++ uriDecode url' ++ "](!include-raw" ++ sections' ++ ")" ++ t)
  where
    s = showElems pc { inCode = True } es
    sections' = concatMap (\sec -> " \"" ++ uriDecode sec ++ "\"") sections
    t = if null a then "" else "{" ++ unwords a ++ "}"
    a = reverse $ if haveLang then attrs else
          case dropWhile isSpace s of
            '<':_ -> ".xml":attrs
            '-':_ -> ".yaml":attrs
            _     -> attrs
    withTitle Nothing c = "::: codebox\n\n" ++ (indent $ trimlines $ c) ++ ":::\n"
    withTitle (Just title') c = "::: codebox\n\n" ++ (indent $ trimlines $ title' ++ "\n\n\n" ++ c) ++ ":::\n"

showul :: ParseContext -> Elem -> String
showul pc (Tag "li" _ [Tag "ul" _ es]) = concatMap (showul pc) es
showul pc (Tag "li" _ es) = "* " ++ (indent' $ trimlines $ showElems pc es) -- FIXME: don't ignore attributes
showul pc e = showIndentTrim' pc "  " [e]

showol :: ParseContext -> Elem -> String
showol pc (Tag "li" _ es) = "#. " ++ (indentWith' "   " $ trimlines $ showElems pc es) -- FIXME: don't ignore attributes
showol pc e = showIndentTrim' pc "   " [e]

asciiletter :: GenParser Char st Char
asciiletter = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

idenletter :: GenParser Char st Char
idenletter = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-:"

iden :: GenParser Char st String
iden = do
  a <- asciiletter
  r <- many $ idenletter
  return $ a:r

styleValueP :: GenParser Char st StyleValue
styleValueP = do
  spaces
  value <- try colorVal <|> Value <$> (many $ noneOf ";")
  return value
  where
    colorVal = do
      _ <- string "rgb("
      spaces
      r <- read <$> (many $ oneOf ['0' .. '9'])
      spaces
      _ <- char ','
      spaces
      g <- read <$> (many $ oneOf ['0' .. '9'])
      spaces
      _ <- char ','
      spaces
      b <- read <$> (many $ oneOf ['0' .. '9'])
      spaces
      _ <- char ')'
      spaces
      return $ RGB r g b

styleElem :: GenParser Char st StyleElem
styleElem = do
  name <- many1 $ oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "-"
  spaces
  _ <- char ':'
  spaces
  value <- styleValueP
  lookAhead eof <|> const () <$> (many1 $ spaces >> char ';' >> spaces)
  return $ StyleElem { styleName=name, styleValue=value }

style :: GenParser Char st Style
style = do
  elems <- many1 styleElem
  return $ elems

emptytag :: GenParser Char st Elem
emptytag = do
  _ <- string "<"
  t <- iden
  a <- many $ try attr
  spaces
  _ <- string "/>"
  return $ Tag t a []

attr :: GenParser Char st Attr
attr = do
  spaces
  n <- iden
  _ <- string "=\""
  v <- manyTill anyChar (char '"')
  return $ Attr n v

tag :: GenParser Char st Elem
tag = do
  _ <- string "<"
  t <- iden
  a <- many $ try attr
  _ <- string ">"
  e <- manyTill (try tag <|> try emptytag <|> try cdata <|> text) (try $ string $ "</" ++ t ++ ">")
  return $ Tag t a e

text :: GenParser Char st Elem
text = do
  s <- many1 (noneOf "<")
  return $ Text s

cdata :: GenParser Char st Elem
cdata = do
  _ <- string "<![CDATA["
  s <- manyTill anyChar (try $ string "]]>")
  return $ Text s

doc :: GenParser Char st Doc
doc = many (try tag <|> try emptytag <|> try cdata <|> text)

confluence :: GenParser Char st Doc
confluence = do
  e <- doc
  eof
  return e

sanitize :: String -> String
sanitize = sanitize' . unlines . map removeTrailingSpace . lines . sanitize' . unlines . map sanitizeLine . lines
  where
    sanitize' "" = ""
    sanitize' ('\n':s) = sanitize' s
    sanitize' ss = sanitize'' ss
    sanitize'' "" = ""
    sanitize'' ('\\':'\n':'\n':s) = sanitize'' $ '\n':'\n':s -- br followed by p  == p
    sanitize'' ('\n':'\n':'\\':s) = sanitize'' $ '\n':'\n':s -- p  followed by br == p
    sanitize'' ('\n':'\n':'\n':s) = sanitize'' $ '\n':'\n':s -- p  followed by p  == p
    sanitize'' (s:ss) = s:sanitize'' ss
    sanitizeLine = fixWordBreaks . removeDoubleSpaces . removeTrailingSpace
    removeTrailingSpace = reverse . dropWhile isSpace . reverse
    fixWordBreaks ('`':'`':y:s) | y /= '`' = fixWordBreaks' $ y:s
    -- fixWordBreaks ('|':s) = fixWordBreaks' $ s
    fixWordBreaks s = fixWordBreaks' s
    -- fixWordBreaks' (x:'%':'#':'&':y:s) = fixWordBreaks' $ check x y ++ s
    -- fixWordBreaks' [x,'%','#','&'] = [x]
    -- fixWordBreaks' ('%':'#':'&':y:s) = fixWordBreaks' $ y:s
    fixWordBreaks' "" = ""
    -- fixWordBreaks' (' ':'|':s) = fixWordBreaks' $ ' ':s
    -- fixWordBreaks' (',':y:s) | not (isSpace y) = ',':' ':fixWordBreaks' (y:s)
    fixWordBreaks' ('*':',':'*':s) = fixWordBreaks' $ '*':',':' ':'*':s -- special case for a common pattern in Magnolia docs
    fixWordBreaks' (d:' ':x:"")  | d `elem` ("`*})" :: [Char]) && x `elem` (".,)" :: [Char]) = d:x:""
    fixWordBreaks' (d:' ':x:y:s) | d `elem` ("`*})" :: [Char]) && x `elem` (".,)" :: [Char]) && not (isAlphaNum y || y `elem` ("`*" :: [Char])) = fixWordBreaks' $ d:x:y:s
    fixWordBreaks' (x:' ':d:s)   | d `elem` ("`*"   :: [Char]) && x `elem` ("("   :: [Char]) = fixWordBreaks' $ x:d:s
    fixWordBreaks' (x:'`':'`':y:s) | x /= '`' && y /= '`' = fixWordBreaks' $ x:y:s
    fixWordBreaks' (x:'`':'`':[]) | x /= '`' = fixWordBreaks' $ [x]
    -- fixWordBreaks (' ':',':s) = fixWordBreaks' $ ',':s
    fixWordBreaks' (s:ss) = s:fixWordBreaks' ss
    -- check x y = if not (isAlphaNum x) || not (isAlphaNum y) || (x == '*' && y == '`') || (x == '`' && y == '*' ) then x:y:"" else x:' ':y:""
    -- check x y = if or [ isSpace x, isSpace y, y `elem` (",.:)]}/'\"\\" :: String), x `elem` ("([{'\"/\\" :: String), x == '*' && y == '`', x == '`' && y == '*' ] then x:y:"" else x:' ':y:""
    removeDoubleSpaces s = let (s1, s2) = span isSpace s
                           in  s1 ++ removeDoubleSpaces' s2
    removeDoubleSpaces' "" = ""
    removeDoubleSpaces' (x:y:s) | isSpace x && isSpace y = removeDoubleSpaces' $ ' ':s
    removeDoubleSpaces' (s:ss) = s:removeDoubleSpaces' ss

getHeaders :: String -> [String]
getHeaders s = case parse confluence "" s of
  Left _   -> []
  Right s' -> concatMap getHeaders' s'
  where
    getHeaders' :: Elem -> [String]
    getHeaders' (Tag h _ es) | h `elem` ["h1", "h2", "h3", "h4", "h5"] = [concatMap showElemFlat es]
    getHeaders' (Tag _ _ es) = concatMap getHeaders' es
    getHeaders' _ = []
    showElemFlat (Tag "ac:parameter" _ _) = ""
    showElemFlat (Tag _ _ es) = concatMap showElemFlat es
    showElemFlat (Text t) = t

confluenceToPandoc :: String -> String -> M.Map (String, String) String -> M.Map String String -> [String] -> String -> String
confluenceToPandoc spacekey title pm am keys s = case parse confluence "" s of
  Left e   -> "Error: " ++ show e ++ "\n" ++ s
  Right s' -> sanitize (showElems (PC spacekey title pm am keys False False False False False False False False '\n' 0) s') ++ "\n\n\n<!-- Original Confluence content:\n\n" ++ s ++ "\n\n-->\n"
