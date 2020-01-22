{-# LANGUAGE OverloadedStrings #-}

module Confluence.Format(confluenceToPandoc) where

import Data.Char(isSpace, isAlphaNum)
import Data.Maybe(mapMaybe)
import Data.List(sort, isPrefixOf, isInfixOf, isSuffixOf, intercalate)
import Data.Text(pack)
import Data.Text.Lazy(unpack)
import Data.Text.Lazy.Builder(toLazyText)
import HTMLEntities.Decoder(htmlEncodedText)
import Text.ParserCombinators.Parsec

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

data ParseContext = PC { pages :: [String] -- ??
                       , links :: [String] -- ??
                       , inCode :: Bool
                       , inEm :: Bool
                       , inStrong :: Bool
                       , prevChar :: Char
                       } deriving (Show, Eq, Ord)

firstParse :: [Elem] -> ParseContext
firstParse _ = PC [] [] False False False '\n'

safeLast :: Char -> String -> Char
safeLast d [] = d
safeLast _ s = last s

showElems :: ParseContext -> [Elem] -> String
showElems _ [] = ""
showElems pc (e:es) = let s = showElem pc e
                      in  s ++ showElems pc{prevChar = safeLast '\n' s} es

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

map', map'' :: (String -> String) -> [String] -> [String]
map' _ [] = []
map' f (x:xs) = (if "```" `isPrefixOf` x then x else f x) : (if "```" `isInfixOf` x then map'' else map') f xs
map'' _ [] = []
map'' f (x:xs) = x : (if "```" `isInfixOf` x then map' else map'') f xs

indentWith' :: String -> String -> String
indentWith' i s = let (ss1, ss2) = span (\x -> not ("```" `isInfixOf` x)) (lines s) in intercalate ('\n':i) ss1 ++ unlines (map' (i ++) ss2)

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

findLink :: String -> String
findLink l@('h':'t':'t':'p':':':_) = uriEncode l
findLink l@('h':'t':'t':'p':'s':':':_) = uriEncode l
findLink l@('f':'t':'p':':':_) = uriEncode l
findLink l@('m':'a':'i':'l':'t':'o':':':_) = uriEncode l
findLink l = '/':uriEncode l

externWikiLink :: String -> String -> String
externWikiLink k l = '/' : k ++ '/' : uriEncode l

decode :: String -> String
decode = unpack . toLazyText . htmlEncodedText . pack

-- Should do a first parse to find the anchors to match against anchor links (need to put a hyphen to replace spaces)
-- getAnchors :: Elem -> [String]
-- getAnchors _ = []

showElem :: ParseContext -> Elem -> String
showElem _  (Text s) = decode s
showElem context (Tag tagname attrs elems) = showElem' context tagname (removeSomeStyle $ sort attrs) elems
  where
    removeSomeStyle :: [Attr] -> [Attr]
    removeSomeStyle [] = []
    removeSomeStyle (a@(Attr "style" s):as) = case parse style "" s of
                                            Left _   -> a:removeSomeStyle as
                                            Right ss -> case removeStyleElems ss of
                                              [] -> removeSomeStyle as
                                              s' -> (Attr "style" $ showStyle s'):removeSomeStyle as
    removeSomeStyle (a:as) = a:removeSomeStyle as
    removeStyleElems :: Style -> Style
    removeStyleElems [] = []
    removeStyleElems (s@(StyleElem { styleName="color", styleValue=(RGB r g b) }):ss) = if all (<100) [r,g,b] then removeStyleElems ss else s:removeStyleElems ss
    removeStyleElems (StyleElem { styleName="letter-spacing", styleValue=_ }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="text-align", styleValue=Value "left" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="text-decoration", styleValue=Value "none" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="font-family", styleValue=Value "DINWebPro , Arial , sans-serif" }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="font-size", styleValue=_ }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="line-height", styleValue=_ }:ss) = removeStyleElems ss
    removeStyleElems (StyleElem { styleName="background-color", styleValue=Value "transparent" }:ss) = removeStyleElems ss
    removeStyleElems (s:ss) = s:removeStyleElems ss

    showElem' pc "p" [] es = ('\n':) $ trimlines $ showElemsBlock pc es
    showElem' pc "p" [Attr "class" "auto-cursor-target"] es = trimlines $ showElemsBlock pc es
    showElem' pc "p" [Attr "style" s] es = "\n" ++ (showElemsBrackets' pc es $ "{style=\"" ++ s ++ "\"}") ++ "\n"
    showElem' pc "span" [Attr "style" s] es = showElemsBrackets' pc es $ "{style=\"" ++ s ++ "\"}"
    showElem' pc "span" [Attr "class" "nolink"] es = showElems pc es
    showElem' pc "span" [Attr "class" "external-link"] es = showElems pc es
    showElem' pc "span" [Attr "class" "confluence-link"] es = showElems pc es
    showElem' pc "span" [Attr "class" s] es = showElemsBrackets' pc es $ "{." ++ s ++ "}"
    showElem' _  "hr" [] [] = "---\n"
    showElem' _  "br" [] [] = "\n"
    showElem' pc "table" [Attr "class" "wrapped"] es = "<table>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</table>\n"
    showElem' pc "table" [Attr "class" "relative-table wrapped", Attr "style" _] es = "<table>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</table>\n"
    showElem' pc "table" [] es = "<table>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</table>\n"
    showElem' _  "colgroup" [] _ = ""
    showElem' pc "tbody" [] es = showElems pc es
    showElem' pc "thead" [] es = showElems pc es
    showElem' pc "tr" [] es = "<tr>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</tr>\n"
    showElem' pc "td" [Attr "colspan" "1"] [Tag "div" [Attr "class" "content-wrapper"] es] = "<td>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</td>\n"
    showElem' pc "td" [] [Tag "div" [Attr "class" "content-wrapper"] es] = "<td>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</td>\n"
    showElem' pc "td" [Attr "colspan" "1"] es = "<td>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</td>\n"
    showElem' pc "td" [] es = "<td>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</td>\n"
    showElem' pc "th" [Attr "colspan" "1"] es = "<th>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</th>\n"
    showElem' pc "th" [] es = "<th>\n" ++ (indent $ trimlines $ showElems pc es) ++ "</th>\n"
    showElem' pc "div" [] es = "\n::: {}\n" ++ (indent $ trimlines $ showElems pc es) ++ ":::\n"
    showElem' pc "h1" [] es = "\n# " ++ showElems pc es ++ "\n"
    showElem' pc "h2" [] es = "\n## " ++ showElems pc es ++ "\n"
    showElem' pc "h3" [] es = "\n### " ++ showElems pc es ++ "\n"
    showElem' pc "h4" [] es = "\n#### " ++ showElems pc es ++ "\n"
    showElem' pc "h5" [] es = "\n##### " ++ showElems pc es ++ "\n"
    showElem' pc "h6" [] es = "\n###### " ++ showElems pc es ++ "\n"
    showElem' pc "a" [Attr "href" l] es = showElemsBrackets pc es $ "(" ++ findLink l ++ ")"
    showElem' pc "a" [Attr "class" c, Attr "href" l, Attr "title" t] es = showElemsBrackets pc es $ "(" ++ findLink l ++ "){" ++ allclasses c ++ " title=\"" ++ t ++ "\"}"
    showElem' pc "a" [Attr "class" "external-link", Attr "href" l, Attr "rel" "nofollow"] es = showElemsBrackets pc es $ "(" ++ l ++ "){.external-link}" -- drop the rel="nofollow" since this is not user-generated content
    showElem' pc "a" [Attr "class" "external-link", Attr "href" l, Attr "rel" "nofollow", Attr "style" s] es = showElemsBrackets pc es $ "(" ++ findLink l ++ "){.external-link style=\"" ++ s ++ "\"}" -- drop the rel="nofollow" since this is not user-generated content
    showElem' _  "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []] = "[" ++ l ++ "](" ++ findLink l ++ ")"
    showElem' _  "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" k, Attr "ri:content-title" l] []] = "[" ++ l ++ "](" ++ externWikiLink k l ++ ")"
    showElem' pc "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] [], Tag "ac:plain-text-link-body" [] es] = showElemsBrackets pc es $ "(" ++ findLink l ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] [Tag "ri:page" [Attr "ri:content-title" l] [], Tag "ac:plain-text-link-body" [] es] = showElemsBrackets pc es $ "(" ++ findLink l ++ "#" ++ findAnchor la ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] [Tag "ri:page" [Attr "ri:content-title" l] [], Tag "ac:link-body" [] es] = showElemsBrackets pc es $ "(" ++ findLink l ++ "#" ++ findAnchor la ++ ")"
    showElem' pc "ac:link" [Attr "ac:anchor" la] [Tag "ac:plain-text-link-body" [] es] = showElemsBrackets pc es $ "(#" ++ findAnchor la ++ ")"
    showElem' _  "ac:link" [Attr "ac:anchor" la] [] = "[" ++ la ++ "](#" ++ findAnchor la ++ ")"
    showElem' pc "ac:image" [] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ ")")
    showElem' pc "ac:image" [Attr "ac:height" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){height=" ++ val ++ "}")
    showElem' pc "ac:image" [Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "!" ++ (showElemsBrackets pc es $ "(" ++ f ++ "){width=" ++ val ++ "}")
    showElem' pc "ac:inline-comment-marker" _ es = showElems pc es
    showElem' _  "ac:parameter" [Attr "ac:name" "atlassian-macro-output-type"] [Text _] = "" -- macros are either always "inline" or always "block", so this parameter is actually unnecessary
    showElem' pc "ac:rich-text-body" _ es = showElems pc es
    showElem' pc "ac:structured-macro" (_:Attr "ac:name" macroName:_) es = processMacro pc macroName es
    showElem' pc "span" [] es = showElems pc es
    showElem' pc "code" _ es | inCode pc = trim $ showElems pc es
    showElem' pc "code" _ es = trim' "`" $ showElems pc { inCode = True } es
    showElem' pc "em" [] es | inEm pc = trim $ showElems pc es
    showElem' pc "em" [] es = trim' "*" $ showElems pc { inEm = True } es
    showElem' pc "strong" [] es | inStrong pc = trim $ showElems pc es
    showElem' pc "strong" [] es = trim' "**" $ showElems pc { inStrong = True } es
    showElem' pc "ul" _ es = "\n" ++ concatMap (showul pc) es
    showElem' pc "ol" _ es = "\n" ++ concatMap (showol pc) es
    showElem' pc "blockquote" [] es = "\n" ++ indentWith "> " (unlines . dropWhile (=="") . lines $ showElems pc es) ++ "\n"
    showElem' pc "ac:layout" [] es = "\n::: contentLayout2" ++ showElemsBlock pc es ++ ":::\n"
    showElem' pc "ac:layout-section" [Attr "ac:type" t] es = "\n::: {.columnLayout ." ++ t ++ " data-layout=\"" ++ t ++ "\"}" ++ showElemsBlock pc es ++ ":::\n"
    showElem' pc "ac:layout-cell" [] es = "\n::: {.cell .normal data-type=\"normal\"}\n::: innerCell" ++ showElemsBlock pc es ++ ":::\n:::\n"
    showElem' pc t as es = "<" ++ t ++ show as ++ ">" ++ showElemsBlock pc es ++ "</" ++ t ++ ">"

    -- optNL (PC{prevChar = '\n'}) = ""
    -- optNL _ = "\n"

processMacroParams :: [Elem] -> ([(String, String)], [Elem])
processMacroParams es = let (ps, es') = processMacroParams' es in (sort ps, es')
  where
    processMacroParams' (Tag "ac:parameter" [Attr "ac:name" n] [Text v]:es') = let (ps, es'') = processMacroParams' es' in filterParams n v ps es''
    processMacroParams' (e:es') = let (ps, es'') = processMacroParams' es' in (ps, e:es'')
    processMacroParams' [] = ([], [])
    filterParams "atlassian-macro-output-type" "BLOCK" ps restes = (ps, restes)
    filterParams "enableHeadingAttributes" "false" ps restes = (ps, restes)
    filterParams "enableHighlighting" "false" ps restes = (ps, restes)
    filterParams "enableSorting" "false" ps restes = (ps, restes)
    filterParams "multiple" "false" ps restes = (ps, restes)
    filterParams "heading" "0" ps restes = (ps, restes)
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
processMacro _  "mgnl-f" [] = "[]{.m5-icon .icon-folder-l}"
processMacro _  "mgnl-n" [] = "[]{.m5-icon .icon-node-content}"
processMacro _  "mgnl-p" [] = "[]{.m5-icon .icon-node-data}"
processMacro pc "anchor" [Tag "ac:parameter" [Attr "ac:name" ""] name] = "{#" ++ showElems pc name ++ "}"
processMacro pc "code" es = showCodeMacro pc False [] es
processMacro pc "bestpractice" es = "\n::: bestpractice\n" ++ (indent $ trimlines $ showElemsBlock pc es) ++ ":::\n" -- don't actually inline, just use a div
processMacro pc "excerpt" ((Tag "ac:parameter" [Attr "ac:name" "hidden"] [Text "true"]):es) = "<!-- " ++ (indent' $ trimlines $ showElems pc es) ++ " -->\n"
processMacro pc "html-wrap" es = let (ps, es') = processMacroParamsS' es
                                     output myes = "\n::: {" ++ ps ++ "}\n" ++ trimlines (showElems pc myes) ++ ":::\n"
                                 in case es' of
                                   [Tag "ac:rich-text-body" [] es''] -> output es''
                                   _ -> output es'
processMacro _  "include" [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" s, Attr "ri:content-title" l] []]]] = "#include \"" ++ externWikiLink s l ++ "\""
processMacro _  "include" [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]] = "#include \"" ++ findLink l ++ "\""
processMacro pc "info" [Tag "ac:rich-text-body" [] es] = "\n::: info\n" ++ (indent $ trimlines $ showElems pc es) ++ ":::\n"
processMacro pc "tip" [Tag "ac:rich-text-body" [] es] = "\n::: tip\n" ++ (indent $ trimlines $ showElems pc es) ++ ":::\n"
processMacro pc "warning" [Tag "ac:rich-text-body" [] es] = "\n::: warning\n" ++ (indent $ trimlines $ showElems pc es) ++ ":::\n"
processMacro pc "list-items-wrapper-macro" [Tag "ac:rich-text-body" [] es] = showElems pc es
processMacro pc "table-plus" es = let (pss, es') = processMacroParams es
                                      ps = unwords $ "table" : (mapMaybe filterAttrs pss)
                                      filterAttrs ("columnAttributes", _) = Nothing
                                      filterAttrs (n, v) = Just $ n ++ "=\"" ++ v ++ "\""
                                      output [] = ""
                                      output (e1@(Tag t _ _):restes) | t /= "table" = showElems pc [e1] ++ output restes
                                      output [Tag "table" _ myes] = "\n<" ++ ps ++ ">\n" ++ (indent $ trimlines $ showElems pc myes) ++ "</table>\n"
                                      output (t1@(Tag "table" _ _):e2:restes) = output [t1] ++ output (e2:restes)
                                      output myes = error $ "Unhandled table-plus macro\n!!!table\n{" ++ ps ++ "}\n" ++ showElems pc myes ++ "!!!end table\n"
                                  in case es' of
                                    [Tag "ac:rich-text-body" [] es''] -> output es''
                                    _ -> output es'
processMacro pc "toc" es = let (ps, es') = processMacroParamsS' es in "#toc" ++ ps ++ "\n" ++ showElems pc es'
processMacro pc name es = let (ps, es') = processMacroParamsS es in "\n!" ++ name ++ ps ++ if null es' then "" else "\n~~~~" ++ showElemsBlock pc es' ++ "~~~~\n"
  where
    processMacroParamsS es' = let (ps, es'') = processMacroParams es' in (concatMap processMacroParamS ps, es'')
    processMacroParamS (n, v) = "(" ++ n ++ "=\"" ++ v ++ "\")"

chooseLang :: String -> String -- translate language string to class attribute
chooseLang "html/xml" = ".xml"
chooseLang s = '.':s

showCodeMacro :: ParseContext -> Bool -> [String] -> [Elem] -> String
showCodeMacro pc _        attrs (Tag "ac:parameter" [Attr "ac:name" ""] [Text t]:es) = showCodeMacro pc True (chooseLang t:attrs) es
showCodeMacro pc _        attrs (Tag "ac:parameter" [Attr "ac:name" "language"] [Text t]:es) = showCodeMacro pc True (chooseLang t:attrs) es
showCodeMacro pc haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "firstline"] [Text t]:es) = showCodeMacro pc haveLang (".numberLines":("startFrom=\"" ++ t ++ "\""):attrs) es
showCodeMacro pc haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "linenumbers"] [Text "true"]:es) = showCodeMacro pc haveLang (".numberLines":attrs) es
showCodeMacro pc haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "title"] [Text t]:es) = showCodeMacro pc haveLang (("title=\"" ++ t ++ "\""):attrs) es
showCodeMacro pc haveLang attrs [Tag "ac:plain-text-body" [] es] = "\n```" ++ t ++ "\n" ++ s ++ "\n```\n"
  where
    s = showElems pc es
    t = if null a then "" else " {" ++ unwords a ++ "}"
    a = reverse $ if haveLang then attrs else
          case dropWhile isSpace s of
            '<':_ -> ".xml":attrs
            '-':_ -> ".yaml":attrs
            _     -> attrs
showCodeMacro pc haveLang attrs es = showCodeMacro pc haveLang attrs [Tag "ac:plain-text-body" [] es]

showul :: ParseContext -> Elem -> String
showul pc (Tag "li" [] es) = "* " ++ (indent' $ trimlines $ showElems pc es) ++ "\n"
showul pc (Tag "li" [Attr "style" "list-style-type: disc;"] es) = "* " ++ (indent' $ trimlines $ showElems pc es) ++ "\n"
showul pc e = indent $ trimlines $ showElem pc e

showol :: ParseContext -> Elem -> String
showol pc (Tag "li" [] es) = "#. " ++ (indentWith' "   " $ trimlines $ showElems pc es) ++ "\n"
showol pc e = indent $ trimlines $ showElem pc e

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
sanitize = unlines . map sanitizeLine . lines
  where
    sanitizeLine = fixWordBreaks . removeDoubleSpaces . removeTrailingSpace
    removeTrailingSpace = reverse . dropWhile isSpace . reverse
    -- fixWordBreaks (x:'%':'#':'&':y:s) = fixWordBreaks $ check x y ++ s
    -- fixWordBreaks [x,'%','#','&'] = [x]
    -- fixWordBreaks ('%':'#':'&':y:s) = fixWordBreaks $ y:s
    fixWordBreaks "" = ""
    fixWordBreaks (d:' ':x:"")  | d `elem` ("`*})" :: [Char]) && x `elem` (".,)" :: [Char]) = d:x:""
    fixWordBreaks (d:' ':x:y:s) | d `elem` ("`*})" :: [Char]) && x `elem` (".,)" :: [Char]) && not (isAlphaNum y || y `elem` ("`*" :: [Char])) = fixWordBreaks $ d:x:y:s
    fixWordBreaks (x:' ':d:s)   | d `elem` ("`*"   :: [Char]) && x `elem` ("("   :: [Char]) = fixWordBreaks $ x:d:s
    -- fixWordBreaks (' ':',':s) = fixWordBreaks $ ',':s
    fixWordBreaks (s:ss) = s:fixWordBreaks ss
    -- check x y = if not (isAlphaNum x) || not (isAlphaNum y) || (x == '*' && y == '`') || (x == '`' && y == '*' ) then x:y:"" else x:' ':y:""
    -- check x y = if or [ isSpace x, isSpace y, y `elem` (",.:)]}/'\"\\" :: String), x `elem` ("([{'\"/\\" :: String), x == '*' && y == '`', x == '`' && y == '*' ] then x:y:"" else x:' ':y:""
    removeDoubleSpaces s = let (s1, s2) = span isSpace s
                           in  s1 ++ removeDoubleSpaces' s2
    removeDoubleSpaces' "" = ""
    removeDoubleSpaces' (x:y:s) | isSpace x && isSpace y = removeDoubleSpaces' $ ' ':s
    removeDoubleSpaces' (s:ss) = s:removeDoubleSpaces' ss

confluenceToPandoc :: String -> String
confluenceToPandoc s = case parse confluence "" s of
  Left e   -> "Error: " ++ show e ++ "\n" ++ s
  Right s' -> sanitize (showElems (firstParse s') s') ++ "\n\n\n<!-- Original content:\n\n" ++ s ++ "\n\n-->\n"
