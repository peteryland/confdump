{-# LANGUAGE OverloadedStrings #-}

module Confluence.Format(confluenceToPandoc) where

import Debug.Trace

import Data.Char(isSpace, isAlphaNum)
import Data.Maybe(mapMaybe)
import Data.List(intersperse, sort, isPrefixOf, isInfixOf, isSuffixOf) -- , intercalate
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

data ParseContext = PC { pageName :: String
                       , pagemap :: M.Map String String
                       , spacekeys :: [String]
                       , links :: [String] -- ??
                       , inCode :: Bool
                       , inEm :: Bool
                       , inStrong :: Bool
                       , inTable :: Bool
                       , inSimpleTable :: Bool
                       , inTableBody :: Bool
                       , prevChar :: Char
                       , tabsetId :: Int
                       } deriving (Show, Eq, Ord)

safeLast :: Char -> String -> Char
safeLast d [] = d
safeLast _ s = last s

showElems :: ParseContext -> [Elem] -> String
showElems _ [] = ""
showElems pc (Tag "ac:structured-macro" (Attr "ac:name" "mgnl-f":_) []:e:es) = "[" ++ showElem pc e ++ "]{.f}" ++ showElems pc { prevChar = '}', tabsetId = 1 + tabsetId pc } es
showElems pc (Tag "ac:structured-macro" (Attr "ac:name" "mgnl-n":_) []:e:es) = "[" ++ showElem pc e ++ "]{.n}" ++ showElems pc { prevChar = '}', tabsetId = 1 + tabsetId pc } es
showElems pc (Tag "ac:structured-macro" (Attr "ac:name" "mgnl-p":_) []:e:es) = "[" ++ showElem pc e ++ "]{.p}" ++ showElems pc { prevChar = '}', tabsetId = 1 + tabsetId pc } es
showElems pc (e:es) = let s = showElem pc e
                      in  s ++ showElems pc { prevChar = safeLast '\n' s, tabsetId = 1 + tabsetId pc } es

showElemsBrackets :: ParseContext -> [Elem] -> String -> String
showElemsBrackets pc es ss = let s = showElems pc{prevChar = '\n'} es
                             in  (if " " `isPrefixOf` s then " [" else "[") ++
                                 trim s ++ "]" ++ ss ++
                                 (if " " `isSuffixOf` s then " " else "")

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
  s -> '\n' : s ++ "\n"

indentWith :: String -> String -> String
indentWith s = unlines . map' (s ++) . lines
  where
    map' _ [] = []
    map' f (x:xs) = (if isCodeOrDiv x then x else f x) : (if isCodeOrDiv x then map'' else map') f xs
    map'' _ [] = []
    map'' f (x:xs) = x : (if isCodeOrDiv x then map' else map'') f xs
    isCodeOrDiv s' = "```" `isInfixOf` s' || ":::" `isInfixOf` s'

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
trim' b s = case trim s of
  "" -> ""
  -- s' -> "%#&" ++ b ++ s' ++ b ++ "%#&"
  s' -> b ++ s' ++ b

indent' :: String -> String
indent' = indentWith' "  "

uriEncode :: String -> String -- just convert parens, Pandoc deals with everything else
uriEncode = concatMap uriEncode'
  where
    uriEncode' ' ' = "+"
    uriEncode' '(' = "%28"
    uriEncode' ')' = "%29"
    uriEncode' c = c:""

findAnchor :: String -> String
findAnchor la = uriEncode la

findLink :: ParseContext -> String -> String
findLink _ l@('h':'t':'t':'p':':':_) = uriEncode l
findLink _ l@('h':'t':'t':'p':'s':':':_) = uriEncode l
findLink _ l@('f':'t':'p':':':_) = uriEncode l
findLink _ l@('m':'a':'i':'l':'t':'o':':':_) = uriEncode l
findLink pc ('_':l) = findLink pc l
findLink pc l = case M.lookup l (pagemap pc) of
  Just ('/':'I':'n':'c':'l':'u':'s':'i':'o':'n':'s':' ':'l':'i':'b':'r':'a':'r':'y':'/':l') -> "/_i/" ++ uriEncode l'  -- specific fix for Magnolia includes
  Just l' -> uriEncode l'
  Nothing -> '/':uriEncode l

externWikiLink :: ParseContext -> String -> String -> String
externWikiLink pc "INCL" l = findLink pc l -- FIXME: should come from list of spaces given on command line
externWikiLink _ k l = '/' : k ++ '/' : uriEncode l

decode :: ParseContext -> String -> String
decode pc = escapeDollars . decode' . unpack . toLazyText . htmlEncodedText . pack
  where
    escapeDollars line | not (inCode pc) && length (filter (=='$') line) > 1 = concatMap (\c -> if c == '$' then "\\$" else c:"") line
    escapeDollars line = line

    decode' "" = ""
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

-- Should do a first parse to find the anchors to match against anchor links (need to put a hyphen to replace spaces)
-- getAnchors :: Elem -> [String]
-- getAnchors _ = []

showIndentTrim :: ParseContext -> [Elem] -> String
showIndentTrim _  es | isEmpty es = ""
showIndentTrim pc es = indent $ trimlines $ showElems pc es

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
    removeSomeStyle ((Attr "class" "with-breadcrumbs"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "auto-cursor-target"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" ('h':'i':'g':'h':'l':'i':'g':'h':'t':'-':_)):as) = removeSomeStyle as
    removeSomeStyle ((Attr "title" ('B':'a':'c':'k':'g':'r':'o':'u':'n':'d':' ':'c':'o':'l':'o':'u':'r':' ':_)):as) = removeSomeStyle as
    removeSomeStyle ((Attr "class" "wrapped"):as) = removeSomeStyle as
    removeSomeStyle ((Attr "data-highlight-colour" _):as) = removeSomeStyle as
    removeSomeStyle ((Attr "rel" "nofollow"):as) = removeSomeStyle as -- drop the rel="nofollow" since this is not user-generated content
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
    removeDotZero "" = ""
    removeDotZero ('.':s) = removeDotZero' s
    removeDotZero (s:ss)  = s:removeDotZero ss
    removeDotZero' "" = ""
    removeDotZero' ('0':s) = removeDotZero' s
    removeDotZero' ss = ss

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
                         t' = showIndentTrim pc { inTable = True, inSimpleTable = False } es
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
    showElem' pc "span" [] es = showElems pc es
    showElem' pc "span" [Attr "title" ""] es = showElems pc es
    showElem' pc "span" [Attr "style" s] es = showElemsBrackets' pc es $ "{style=\"" ++ s ++ "\"}"
    showElem' pc "span" as es = case lookup "class" (map (\(Attr n v) -> (n, v)) as) of
      Just "nolink" -> showElems pc es
      Just "external-link" -> showElems pc es
      Just "confluence-link" -> showElems pc es
      Just ('e':'m':'o':'t':'i':'c':'o':'n':' ':'e':'m':'o':'t':'i':'c':'o':'n':'-':s) -> case s of
        "information" -> ":information_source: " ++ showElems pc es
        "thumbs-up" -> ":thumbsup: " ++ showElems pc es
        _ -> ":" ++ s ++ ": " ++ showElems pc es
      Just s -> showElemsBrackets' pc es $ "{." ++ s ++ "}"
      _ -> traceShow (as, es) ""
    showElem' _  "hr" [] [] = "---\n"
    showElem' pc "br" _ [] | inSimpleTable pc = ""
    showElem' _  "br" _ [] = "\\\n"
    showElem' pc "table" _ es = tryTable pc es
    -- showElem' pc "table" (Attr "class" s:_) es = tryTable pc es (Just s)
    -- showElem' pc "table" [] es = tryTable pc es Nothing
    showElem' _  "colgroup" [] _ = ""
    showElem' pc "thead" [] es = showElems pc es
    showElem' pc "tbody" [] es = showElems pc es
    showElem' pc "tr" [] es | inSimpleTable pc = let t = trim $ trimlines $ showElems pc es
                                                 in  if '\n' `elem` t then "\0" else t ++ "|\n"
    showElem' pc "tr" [] es = "<tr>\n" ++ showIndentTrim pc es ++ "</tr>\n"
    showElem' pc "tr" [Attr "style" _] _ | inSimpleTable pc = "\0" -- FIXME: maybe try to pass through left-margins instead
    showElem' pc "tr" [Attr "style" s] es = "<tr style=\"" ++ s ++ "\">\n" ++ showIndentTrim pc es ++ "</tr>\n"
    showElem' pc "td" [Attr "colspan" _] _ | inSimpleTable pc = "\0"
    showElem' pc "td" _ es | inSimpleTable pc = "|" ++ (trim $ trimlines $ showElems pc es) -- FIXME: make sure left-margin styles on TRs and TDs are supported by simple tables
    showElem' pc "td" [] es = "<td>\n" ++ showIndentTrim pc es ++ "</td>\n"
    showElem' pc "td" [Attr "colspan" cs] es = "<td colspan=\"" ++ cs ++ "\">\n" ++ showIndentTrim pc es ++ "</td>\n"
    showElem' pc "td" [Attr "style" s] es = "<td style=\"" ++ s ++ "\">\n" ++ showIndentTrim pc es ++ "</td>\n"
    showElem' pc "th" [Attr "colspan" _] _ | inSimpleTable pc = "\0"
    showElem' pc "th" _ es | inSimpleTable pc = "|" ++ (trim $ trimlines $ showElems pc es) -- FIXME: header needs to come first somehow
    showElem' pc "th" [Attr "colspan" cs] es = "<th colspan=\"" ++ cs ++ "\">\n" ++ showIndentTrim pc es ++ "</th>\n"
    showElem' pc "th" [] es = "<th>\n" ++ showIndentTrim pc es ++ "</th>\n"
    showElem' pc "div" [Attr "class" "content-wrapper", Attr "style" s] es = "[" ++ (trim $ trimlines $ showElems pc es) ++ "]{style=\"" ++ s ++ "\"}"
    showElem' pc "div" [Attr "class" "content-wrapper"] es = (trim $ trimlines $ showElems pc es)
    showElem' pc "div" [Attr "class" "paragraph"] es = showElem' pc "p" [] es
    showElem' pc "div" [Attr "class" "syntaxhighlighter nogutter html"] es = showElemsBlock pc es
    -- showElem' pc "div" [] es = "\n::: {}\n" ++ showIndentTrim pc es ++ ":::\n"
    showElem' pc "div" [] es = showElemsBlock pc es
    showElem' pc "h1" [] es = "\n# " ++ showElems pc es ++ "\n"
    showElem' pc "h2" [] es = "\n## " ++ showElems pc es ++ "\n"
    showElem' pc "h3" [] es = "\n### " ++ showElems pc es ++ "\n"
    showElem' pc "h4" [] es = "\n#### " ++ showElems pc es ++ "\n"
    showElem' pc "h5" [] es = "\n##### " ++ showElems pc es ++ "\n"
    showElem' pc "h6" [] es = "\n###### " ++ showElems pc es ++ "\n"
    showElem' pc "a" [Attr "href" l] es | inCode pc = "`" ++ (showElemsBrackets pc { inCode = False } [Tag "code" [] es] $ "(" ++ findLink pc l ++ ")`")
    showElem' pc "a" [Attr "href" l] es = showElemsBrackets pc es $ "(" ++ findLink pc l ++ ")"
    showElem' pc "a" [Attr "class" c, Attr "href" l, Attr "title" t] es | inCode pc = "`" ++ (showElemsBrackets pc { inCode = False } [Tag "code" [] es] $ "(" ++ findLink pc l ++ "){" ++ allclasses c ++ " title=\"" ++ t ++ "\"}`")
    showElem' pc "a" [Attr "class" c, Attr "href" l, Attr "title" t] es = showElemsBrackets pc es $ "(" ++ findLink pc l ++ "){" ++ allclasses c ++ " title=\"" ++ t ++ "\"}"
    showElem' pc "a" [Attr "class" "external-link", Attr "href" l] es | inCode pc = "`" ++ (showElemsBrackets pc { inCode = False } [Tag "code" [] es] $ "(" ++ l ++ ")`")
    showElem' pc "a" [Attr "class" "external-link", Attr "href" l] es = showElemsBrackets pc es $ "(" ++ l ++ ")"
    showElem' pc "a" [Attr "class" "external-link", Attr "href" l, Attr "style" s] es | inCode pc = "`" ++ (showElemsBrackets pc { inCode = False } [Tag "code" [] es] $ "(" ++ findLink pc l ++ "){style=\"" ++ s ++ "\"}`")
    showElem' pc "a" [Attr "class" "external-link", Attr "href" l, Attr "style" s] es = showElemsBrackets pc es $ "(" ++ findLink pc l ++ "){style=\"" ++ s ++ "\"}"
    showElem' pc "a" [Attr "class" "external", Attr "href" l] es | inCode pc = "`" ++ (showElemsBrackets pc { inCode = False } [Tag "code" [] es] $ "(" ++ l ++ ")`")
    showElem' pc "a" [Attr "class" "external", Attr "href" l] es = showElemsBrackets pc es $ "(" ++ l ++ ")"
    showElem' pc "a" [Attr "class" "external", Attr "href" l, Attr "style" s] es | inCode pc = "`" ++ (showElemsBrackets pc { inCode = False } [Tag "code" [] es] $ "(" ++ findLink pc l ++ "){style=\"" ++ s ++ "\"}`")
    showElem' pc "a" [Attr "class" "external", Attr "href" l, Attr "style" s] es = showElemsBrackets pc es $ "(" ++ findLink pc l ++ "){style=\"" ++ s ++ "\"}"
    showElem' _  "ac:emoticon" [Attr "ac:name" "warning"] [] = ":warning:" -- ⚠️
    showElem' _  "ac:emoticon" [Attr "ac:name" "information"] [] = ":information_source:" -- ℹ️
    showElem' _  "ac:emoticon" [Attr "ac:name" "thumbs-up"] [] = ":thumbsup:" -- 👍
    showElem' _  "ac:emoticon" [Attr "ac:name" "red-star"] [] = ":star:" -- ⭐
    showElem' _  "ac:emoticon" [Attr "ac:name" "tick"] [] = ":heavy_check_mark:" -- ✔️
    showElem' _  "ac:emoticon" [Attr "ac:name" "cross"] [] = ":x:" -- ❌ -- alternative would be :heavy_multiplication_x:
    showElem' _  "ac:emoticon" [Attr "ac:name" "minus"] [] = ":heavy_minus_sign:" -- ➖
    showElem' _  "ac:emoticon" [Attr "ac:name" "plus"] [] = ":heavy_plus_sign:" -- ➕
    showElem' pc "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []] | inCode pc = "`[`" ++ l ++ "`](" ++ findLink pc l ++ ")`"
    showElem' pc "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []] = "[" ++ l ++ "](" ++ findLink pc l ++ ")"
    showElem' pc "ac:link" [] (Tag "ri:page" [Attr "ri:content-title" l] []:es) | inCode pc = "`" ++ (showElemsBrackets pc { inCode = False } [Tag "code" [] es] $ "(" ++ findLink pc l ++ ")`")
    showElem' pc "ac:link" [] (Tag "ri:page" [Attr "ri:content-title" l] []:es) = showElemsBrackets pc es $ "(" ++ findLink pc l ++ ")"
    showElem' pc "ac:link" [] (Tag "ri:space" [Attr "ri:space-key" k] []:es) = showElemsBrackets pc es $ "(" ++ externWikiLink pc k "" ++ ")"
    showElem' pc "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" k, Attr "ri:content-title" l] []] = "[" ++ l ++ "](" ++ externWikiLink pc k l ++ ")"
    showElem' pc "ac:link" [] (Tag "ri:page" [Attr "ri:space-key" k, Attr "ri:content-title" l] []:es) = showElemsBrackets pc es $ "(" ++ externWikiLink pc k l ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] (Tag "ri:page" [Attr "ri:space-key" k, Attr "ri:content-title" l] []:es) = showElemsBrackets pc es $ "(" ++ externWikiLink pc k l ++ "#" ++ findAnchor la ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] (Tag "ri:page" [Attr "ri:content-title" l] []:es) = showElemsBrackets pc es $ "(" ++ findLink pc l ++ "#" ++ findAnchor la ++ ")"
    showElem' _  "ac:link" [Attr "ac:anchor" la] [] = "[" ++ la ++ "](#" ++ findAnchor la ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] es = showElemsBrackets pc es $ "(#" ++ findAnchor la ++ ")"
    showElem' pc "ac:image" [] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){.inline}")
    showElem' pc "ac:image" [Attr "ac:thumbnail" "true"] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){.inline}")
    showElem' pc "ac:image" [Attr "ac:height" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){" ++ (if (read val :: Int) > 50 then "" else ".inline ") ++ "height=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:height" val, Attr "ac:thumbnail" "true"] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){.inline height=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){" ++ (if (read val :: Int) > 50 then "" else ".inline ") ++ "width=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:thumbnail" "true", Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){.inline width=" ++ val ++ "}")
    showElem' pc "ac:inline-comment-marker" _ es = showElems pc es
    showElem' _  "ac:parameter" [Attr "ac:name" "atlassian-macro-output-type"] [Text _] = "" -- macros are either always "inline" or always "block", so this parameter is actually unnecessary
    showElem' pc "ac:link-body" _ es = showElems pc es
    showElem' pc "ac:plain-text-link-body" _ es = showElems pc es
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
    showElem' _  "em" _ es | isEmpty es = ""
    showElem' pc "em" _ es = trim' "*" $ showElems pc { inEm = True } es
    showElem' pc "strong" _ es | inStrong pc = trim $ showElems pc es
    showElem' _  "strong" _ es | isEmpty es = ""
    showElem' pc "strong" _ es = trim' "**" $ showElems pc { inStrong = True } es
    showElem' pc "sup" _ es = "^" ++ showElems pc es ++ "^"
    showElem' pc "ul" _ _ | inSimpleTable pc = "\0"
    showElem' pc "ul" _ es = "\n" ++ concatMap (showul pc) es
    showElem' pc "ol" _ _ | inSimpleTable pc = "\0"
    showElem' pc "ol" _ es = "\n" ++ concatMap (showol pc) es
    showElem' pc "blockquote" _ _ | inSimpleTable pc = "\0"
    showElem' pc "blockquote" [] es = "\n" ++ indentWith "> " (unlines . dropWhile (=="") . lines $ showElems pc es) ++ "\n"
    showElem' pc "ac:layout" [] es = "\n::: layout" ++ showElemsBlock pc es ++ ":::\n" -- FIXME: use markdown .columns (https://pandoc.org/MANUAL.html#columns - maybe just for slides)
    showElem' pc "ac:layout-section" [Attr "ac:type" t] es = "\n::: {.layout-section ." ++ t ++ "}" ++ showElemsBlock pc es ++ ":::\n"
    showElem' pc "ac:layout-cell" [] es = "\n::: cell" ++ showElemsBlock pc es ++ ":::\n"
    showElem' _  "img" as es = case lookup "class" (map (\(Attr n v) -> (n, v)) as) of
      Just ('e':'m':'o':'t':'i':'c':'o':'n':' ':'e':'m':'o':'t':'i':'c':'o':'n':'-':s) -> case s of
        "information" -> ":information_source:"
        "thumbs-up" -> ":thumbsup:"
        _ -> ":" ++ s ++ ":"
      _ -> traceShow (as, es) ""
    showElem' pc t as es = "<" ++ t ++ show as ++ ">" ++ showElemsBlock pc es ++ "</" ++ t ++ ">"

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

processMacroParamsS' :: [Elem] -> (String, [Elem])
processMacroParamsS' es = let (ps, es') = processMacroParams es in (unwords $ mapMaybe processMacroParamS ps, es')
  where
    processMacroParamS ("class", v) = Just $ allclasses v
    processMacroParamS ("align", "right") = Just "style=\"overflow: hidden; float: right;\""
    processMacroParamS (n, v) = Just $ n ++ "=\"" ++ v ++ "\""

allclasses :: String -> String
allclasses = unwords . map ('.':) . words

trimlines :: String -> String
trimlines = unlines . trimlines1 . lines
  where
    trimlines1 s = dropWhile null' $ reverse $ dropWhile null' $ reverse s
    null' = all isSpace

processMacro :: ParseContext -> String -> [Elem] -> String
processMacro pc "aui-button" es = let (_, es') = processMacroParams es
                                  in  showElems pc es'
processMacro _  "mgnl-get" [] = "[GET]{.get}"
processMacro _  "mgnl-put" [] = "[PUT]{.put}"
processMacro _  "mgnl-post" [] = "[POST]{.post}"
processMacro pc "list-children" es = processMacro pc "children" es
processMacro pc "children" es = let (_, es') = processMacroParams es
                                in  case es' of
                                      [Tag "ac:parameter" [Attr "ac:name" "page"] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]] -> "[" ++ findLink pc l ++ "](!children)"
                                      _ -> "[](!children)"
processMacro pc "anchor" [Tag "ac:parameter" [Attr "ac:name" ""] name] = "[]{#" ++ showElems pc name ++ "}"
processMacro pc "javadoc-resource-link-macro" es = let (pss, es') = processMacroParams es
                                                   in  "(javadoc-resource-link-macro: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "mgnl-app-lauchner" es = let (pss, es') = processMacroParams es
                                         in  "(mgnl-app-launcher: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "artifact-maven-dependencies-snippet-macro" es = let (pss, es') = processMacroParams es
                                                                 in  "(artifact-maven-dependencies-snippet-macro: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "artifact-guess-resource-macro" es = let (pss, es') = processMacroParams es
                                                     in  "(artifact-guess-resource-macro: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "artifact-resource-macro" es = let (pss, es') = processMacroParams es
                                                   label' = lookup "label" pss
                                               in  "[" ++ maybe "(no label)" id label' ++ "](" ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "localtab" es = processMacro pc "localtabgroup" [Tag "ac:structured-macro" [Attr "ac:name" "localtab"] (Tag "ac:parameter" [Attr "ac:name" "active"] [Text "true"]:es)] -- put isolated localtab into its own tabgroup
processMacro pc "localtabgroup" es = "\n::: tabset\n\n" ++ showTabGroup (processTabGroupMacro es) ++ "\n:::\n"
  where
    processTabGroupMacro [] = []
    processTabGroupMacro (Tag "p" [Attr "class" "auto-cursor-target"] [Tag "br" [] []]:rest) = processTabGroupMacro rest
    processTabGroupMacro (Tag "ac:rich-text-body" _ es':rest) = processTabGroupMacro es' ++ processTabGroupMacro rest
    processTabGroupMacro (Tag "ac:structured-macro" (Attr "ac:name" "localtab":_) es':rest) = let (pss, es'') = processMacroParams es'
                                                                                                  title = maybe "" id $ lookup "title" pss
                                                                                                  checked = ("active", "true") `elem` pss
                                                                                              in  (title, showElems pc es'', checked):processTabGroupMacro rest
    processTabGroupMacro es' = trace ("processTabGroupMacro: " ++ show es') []
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
processMacro pc "jira" es = let (pss, es') = processMacroParams es
                            in  "(jira: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "javadoc" es = let (pss, es') = processMacroParams es
                               in  "(javadoc: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "multiexcerpt" es = let (pss, es') = processMacroParams es
                                    in  "(multiexcerpt: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "multiexcerpt-include" es = let (pss, es') = processMacroParams es
                                            in  "(multiexcerpt-include: " ++ showElems pc es' ++ show pss ++ ")"
processMacro pc "hide-macro" es = let (pss, es') = processMacroParams es
                                  in  "<!-- " ++ showElems pc es' ++ show pss ++ " -->"
processMacro pc "code" es = showCodeMacro pc False Nothing [] es
processMacro pc "code-pro" es = showCodeMacro pc False Nothing [] es
processMacro pc "details" es = showElems pc es
processMacro pc "excerpt" ((Tag "ac:parameter" [Attr "ac:name" "hidden"] [Text "true"]):es) = "<!-- " ++ (indent' $ trimlines $ showElems pc es) ++ " -->\n"
processMacro pc "expand" es = let (_, es') = processMacroParams es
                              in  showElems pc es'
processMacro _  "livesearch" _ = "<!-- live search used to be here -->\n"
processMacro pc "html-wrap" es = let (ps, es') = processMacroParamsS' es
                                     output myes = "\n::: {" ++ ps ++ "}\n" ++ trimlines (showElems pc myes) ++ ":::\n"
                                 in output es'
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
                                      output [Tag "table" _ myes] = "\n<" ++ ps ++ ">\n" ++ showIndentTrim pc myes ++ "</table>\n"
                                      output (t1@(Tag "table" _ _):e2:restes) = output [t1] ++ output (e2:restes)
                                      output myes = error $ "Unhandled table-plus macro\n!!!table\n{" ++ ps ++ "}\n" ++ showElems pc myes ++ "!!!end table\n"
                                  in output es'
-- processMacro pc "toc" es = let (ps, es') = processMacroParamsS' es in "#toc" ++ ps ++ "\n" ++ showElems pc es'
processMacro _  "toc" _ = "[](!toc)"
-- processMacro _ _ _ = "" -- unknown macro, ignore for demo purposes
processMacro pc name es = let (ps, es') = processMacroParamsS es in trace ("\n!" ++ name ++ ps) (if null es' then "" else showElemsBlock pc es')
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

showCodeMacro :: ParseContext -> Bool -> Maybe String -> [String] -> [Elem] -> String
showCodeMacro pc _        title attrs (Tag "ac:parameter" [Attr "ac:name" ""] [Text t]:es) = showCodeMacro pc True title (chooseLang t:attrs) es
showCodeMacro pc _        title attrs (Tag "ac:parameter" [Attr "ac:name" "language"] [Text t]:es) = showCodeMacro pc True title (chooseLang t:attrs) es
showCodeMacro pc haveLang title attrs (Tag "ac:parameter" [Attr "ac:name" "firstline"] [Text "1"]:es) = showCodeMacro pc haveLang title attrs es
showCodeMacro pc haveLang title attrs (Tag "ac:parameter" [Attr "ac:name" "firstline"] [Text t]:es) = showCodeMacro pc haveLang title (("startFrom=\"" ++ t ++ "\""):attrs) es
showCodeMacro pc haveLang title attrs (Tag "ac:parameter" [Attr "ac:name" "linenumbers"] [Text "true"]:es) = showCodeMacro pc haveLang title (".numberLines":attrs) es
showCodeMacro pc haveLang _     attrs (Tag "ac:parameter" [Attr "ac:name" "title"] [Text t]:es) = showCodeMacro pc haveLang (Just t) attrs es
showCodeMacro pc haveLang title attrs (Tag "ac:parameter" [Attr "ac:name" "collapse"] [Text _]:es) = showCodeMacro pc haveLang title attrs es
showCodeMacro pc haveLang title attrs es = "\n" ++ (withTitle title $ "```" ++ t ++ "\n" ++ s ++ "\n```\n")
  where
    s = showElems pc { inCode = True } es
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
showul pc (Tag "li" _ es) = "* " ++ (indent' $ trimlines $ showElems pc es) ++ "\n" -- FIXME: don't ignore attributes
showul pc e = showIndentTrim pc [e]

showol :: ParseContext -> Elem -> String
showol pc (Tag "li" _ es) = "#. " ++ (indentWith' "   " $ trimlines $ showElems pc es) ++ "\n" -- FIXME: don't ignore attributes
showol pc e = showIndentTrim pc [e]

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

confluenceToPandoc :: String -> M.Map String String -> [String] -> String -> String
confluenceToPandoc title pm keys s = case parse confluence "" s of
  Left e   -> "Error: " ++ show e ++ "\n" ++ s
  Right s' -> sanitize (showElems (PC title pm keys [] False False False False False False '\n' 0) s') ++ "\n\n\n<!-- Original Confluence content:\n\n" ++ s ++ "\n\n-->\n"
