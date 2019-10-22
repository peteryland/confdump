{-# LANGUAGE OverloadedStrings #-}

module Confluence.Format(confluenceToPandoc) where

import Data.Char(isSpace)
import Data.Maybe(mapMaybe)
import Data.List(intercalate, sort)
import Data.Text(pack, unpack)
import Data.Text.Lazy(toStrict)
import Data.Text.Lazy.Builder(toLazyText)
import HTMLEntities.Decoder(htmlEncodedText)
import Text.ParserCombinators.Parsec

data Attr = Attr String String deriving (Show, Eq, Ord)
data Elem = Tag String [Attr] [Elem] | Text String deriving (Show, Eq, Ord)
type Doc = [Elem]

  -- t <- choice $ map string ["p", "em", "h2", "li", "ul", "a", "code", "table", "tbody", "tr", "th", "td"]

showElems :: [String] -> [Elem] -> String
showElems fs es = concatMap (showElem fs) es

indentWith :: String -> String -> String
indentWith s = unlines . map (s ++) . lines

indentWith' :: String -> String -> String
indentWith' s = intercalate ('\n' : s) . lines

indent :: String -> String
indent = indentWith "  "

indent' :: String -> String
indent' = indentWith' "  "

uriEncode :: String -> String -- just convert parens, Pandoc deals with everything else
uriEncode = concatMap uriEncode'
  where
    uriEncode' '(' = "%28"
    uriEncode' ')' = "%29"
    uriEncode' c = c:""

findAnchor :: String -> String
findAnchor la = la

findLink :: String -> String
findLink l@('h':'t':'t':'p':':':_) = uriEncode l
findLink l@('h':'t':'t':'p':'s':':':_) = uriEncode l
findLink l@('f':'t':'p':':':_) = uriEncode l
findLink l@('m':'a':'i':'l':'t':'o':':':_) = uriEncode l
findLink l = '/':uriEncode l

findLink' :: String -> String -> String
findLink' s l = "/" ++ uriEncode s ++ "-" ++ uriEncode l

decode :: String -> String
decode = unpack . toStrict . toLazyText . htmlEncodedText . pack

-- Should do a first parse to find the anchors to match against anchor links (need to put a hyphen to replace spaces)
-- getAnchors :: Elem -> [String]
-- getAnchors _ = []

showElem :: [String] -> Elem -> String
showElem _  (Text s) = decode s
showElem context (Tag tagname attrs elems) = showElem' context tagname (sort attrs) elems
  where
    showElem' fs "p" [] es = "\n" ++ showElems fs es ++ "\n"
    showElem' fs "p" [Attr "class" "auto-cursor-target"] es = "\n" ++ showElems fs es ++ "\n"
    showElem' fs "p" [Attr "style" s] es = "\n[" ++ showElems fs es ++ "]{style=\"" ++ s ++ "\"}\n"
    showElem' fs "span" [Attr "style" s] es = "[" ++ showElems fs es ++ "]{style=\"" ++ s ++ "\"}"
    showElem' _  "hr" [] [] = "---\n"
    showElem' _  "br" [] [] = "\n"
    showElem' fs "div" [] es = "\n:::\n" ++ trimlines (showElems fs es) ++ ":::\n"
    showElem' fs "h1" [] es = "\n# " ++ showElems fs es ++ "\n"
    showElem' fs "h2" [] es = "\n## " ++ showElems fs es ++ "\n"
    showElem' fs "h3" [] es = "\n### " ++ showElems fs es ++ "\n"
    showElem' fs "h4" [] es = "\n#### " ++ showElems fs es ++ "\n"
    showElem' fs "h5" [] es = "\n##### " ++ showElems fs es ++ "\n"
    showElem' fs "h6" [] es = "\n###### " ++ showElems fs es ++ "\n"
    showElem' fs "a" [Attr "href" l] es = "[" ++ showElems fs es ++ "](" ++ findLink l ++ ")"
    showElem' fs "a" [Attr "class" c, Attr "href" l, Attr "title" t] es = "[" ++ showElems fs es ++ "](" ++ findLink l ++ "){" ++ allclasses c ++ " title=\"" ++ t ++ "\"}"
    showElem' fs "a" [Attr "class" "external-link", Attr "href" l, Attr "rel" "nofollow"] es = "[" ++ showElems fs es ++ "](" ++ l ++ "){.external-link}" -- drop the rel="nofollow" since this is not user-generated content
    showElem' fs "a" [Attr "class" "external-link", Attr "href" l, Attr "rel" "nofollow", Attr "style" s] es = "[" ++ showElems fs es ++ "](" ++ findLink l ++ "){.external-link style=\"" ++ s ++ "\"}" -- drop the rel="nofollow" since this is not user-generated content
    showElem' _  "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []] = "[" ++ l ++ "](" ++ findLink l ++ ")"
    showElem' fs "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] [], Tag "ac:plain-text-link-body" [] es] = "[" ++ showElems fs es ++ "](" ++ findLink l ++ ")"
    showElem' fs "ac:link" [Attr "ac:anchor" la] [Tag "ri:page" [Attr "ri:content-title" l] [], Tag "ac:plain-text-link-body" [] es] = "[" ++ showElems fs es ++ "](" ++ findLink l ++ "#" ++ findAnchor la ++ ")"
    showElem' fs "ac:link" [Attr "ac:anchor" la] [Tag "ac:plain-text-link-body" [] es] = "[" ++ showElems fs es ++ "](#" ++ findAnchor la ++ ")"
    showElem' fs "ac:image" [] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "![" ++ showElems fs es ++ "](" ++ f ++ ")"
    showElem' fs "ac:image" [Attr "ac:height" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "![" ++ showElems fs es ++ "](" ++ f ++ "){height=" ++ val ++ "}"
    showElem' fs "ac:image" [Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] es] = "![" ++ showElems fs es ++ "](" ++ f ++ "){width=" ++ val ++ "}"
    showElem' fs "ac:structured-macro" (_:Attr "ac:name" macroName:_) es = processMacro fs macroName es
    showElem' fs "ac:inline-comment-marker" _ es = showElems fs es
    showElem' fs "span" [] es = showElems fs es
    showElem' fs "code" [] es = "`" ++ showElems fs es ++ "`"
    showElem' fs "code" [Attr "style" "font-size: 21.546px;"] es = "`" ++ showElems fs es ++ "`"
    showElem' fs "em" [] es = "*" ++ showElems fs es ++ "*"
    showElem' fs "strong" [] es = "**" ++ showElems fs es ++ "**"
    showElem' fs "ul" _ es = concatMap (showul fs) es
    showElem' fs "ol" _ es = concatMap (showol fs) es
    showElem' fs "blockquote" [] es = "\n" ++ indentWith "> " (unlines . dropWhile (=="") . lines $ showElems fs es) ++ "\n"
    showElem' fs "ac:layout" [] es = "::: contentLayout2\n" ++ showElems fs es ++ "\n:::\n"
    showElem' fs "ac:layout-section" [Attr "ac:type" t] es = "::: {.columnLayout ." ++ t ++ " data-layout=\"" ++ t ++ "\"}\n" ++ showElems fs es ++ "\n:::\n"
    showElem' fs "ac:layout-cell" [] es = "::: {.cell .normal data-type=\"normal\"}\n::: innerCell\n" ++ showElems fs es ++ "\n:::\n:::\n"
    showElem' fs t as es = "<" ++ t ++ show as ++ ">\n" ++ showElems fs es ++ "\n</" ++ t ++ ">"

processMacroParams :: [Elem] -> ([(String, String)], [Elem])
processMacroParams es = let (ps, es') = processMacroParams' es in (sort ps, es')
  where
    processMacroParams' (Tag "ac:parameter" [Attr "ac:name" n] [Text v]:es') = let (ps, es'') = processMacroParams' es' in ((n, v) : ps, es'')
    processMacroParams' (e:es') = let (ps, es'') = processMacroParams' es' in (ps, e:es'')
    processMacroParams' [] = ([], [])

processMacroParamsS :: [Elem] -> (String, [Elem])
processMacroParamsS es = let (ps, es') = processMacroParams es in (unwords $ map processMacroParamS ps, es')
  where
    processMacroParamS (n, v) = n ++ "=\"" ++ v ++ "\""

processMacroParamsS' :: [Elem] -> (String, [Elem])
processMacroParamsS' es = let (ps, es') = processMacroParams es in (unwords $ mapMaybe processMacroParamS ps, es')
  where
    processMacroParamS ("class", v) = Just $ allclasses v
    processMacroParamS ("align", "right") = Just "style=\"overflow: hidden; float: right;\""
    processMacroParamS ("atlassian-macro-output-type", "BLOCK") = Nothing
    processMacroParamS (n, v) = Just $ n ++ "=\"" ++ v ++ "\""

allclasses :: String -> String
allclasses = unwords . map ('.':) . words

trimlines :: String -> String
trimlines = unlines . trimlines' . lines
  where
    trimlines' s = dropWhile null $ reverse $ dropWhile null $ reverse s

processMacro :: [String] -> String -> [Elem] -> String
processMacro _  "mgnl-f" [] = "[]{.m5-icon .icon-folder-l}"
processMacro _  "mgnl-n" [] = "[]{.m5-icon .icon-node-content}"
processMacro _  "mgnl-p" [] = "[]{.m5-icon .icon-node-data}"
processMacro fs "anchor" [Tag "ac:parameter" [Attr "ac:name" ""] name] = "{#" ++ showElems fs name ++ "}"
processMacro fs "code" es = showCodeMacro fs False [] es
processMacro fs "bestpractice" [Tag "ac:parameter" [Attr "ac:name" "atlassian-macro-output-type"] [Text "INLINE"], Tag "ac:rich-text-body" [] es] = "::: bestpractice\n" ++ showElems fs es ++ "\n:::\n" -- don't actually inline, just use a div
processMacro fs "warning" [Tag "ac:rich-text-body" [] es] = "::: warning\n" ++ trimlines (showElems fs es) ++ ":::\n"
processMacro fs "info" [Tag "ac:rich-text-body" [] es] = "::: info\n" ++ trimlines (showElems fs es) ++ ":::\n"
processMacro fs "tip" [Tag "ac:rich-text-body" [] es] = "::: tip\n" ++ trimlines (showElems fs es) ++ ":::\n"
processMacro _  "include" [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" s, Attr "ri:content-title" l] []]]] = "#include \"" ++ findLink' s l ++ "\""
processMacro _  "include" [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]] = "#include \"" ++ findLink l ++ "\""
processMacro fs "list-items-wrapper-macro" [Tag "ac:rich-text-body" [] es] = showElems fs es
processMacro fs "html-wrap" es = let (ps, es') = processMacroParamsS' es in case es' of
                                   [Tag "ac:rich-text-body" [] es''] -> "\n::: {" ++ ps ++ "}\n" ++ trimlines (showElems fs es'') ++ ":::\n"
                                   _ -> "\n::: {" ++ ps ++ "}\n" ++ trimlines (showElems fs es') ++ ":::\n"
processMacro fs name es = let (ps, es') = processMacroParamsS es in "\n#macro " ++ name ++ (case ps of "" -> ""; _ -> " " ++ ps) ++ "\n" ++ indentWith ">>> " (showElems fs es')

showCodeMacro :: [String] -> Bool -> [String] -> [Elem] -> String
showCodeMacro fs _        attrs (Tag "ac:parameter" [Attr "ac:name" "language"] [Text t]:es) = showCodeMacro fs True (('.':t):attrs) es
showCodeMacro fs haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "firstline"] [Text t]:es) = showCodeMacro fs haveLang (".numberLines":("startFrom=\"" ++ t ++ "\""):attrs) es
showCodeMacro fs haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "linenumbers"] [Text "true"]:es) = showCodeMacro fs haveLang (".numberLines":attrs) es
showCodeMacro fs haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "title"] [Text t]:es) = showCodeMacro fs haveLang (("title=\"" ++ t ++ "\""):attrs) es
showCodeMacro fs haveLang attrs [Tag "ac:plain-text-body" [] es] = "```" ++ t ++ "\n" ++ s ++ "\n```\n"
  where
    s = showElems fs es
    t = if null a then "" else " {" ++ unwords a ++ "}"
    a = reverse $ if haveLang then attrs else
          case dropWhile isSpace s of
            '<':_ -> ".xml":attrs
            '-':_ -> ".yaml":attrs
            _     -> attrs
showCodeMacro fs haveLang attrs es = showCodeMacro fs haveLang attrs [Tag "ac:plain-text-body" [] es]

showul :: [String] -> Elem -> String
showul fs (Tag "li" [] es) = "* " ++ indent' (showElems fs es) ++ "\n"
showul fs (Tag "li" [Attr "style" "list-style-type: disc;"] es) = "* " ++ indent' (showElems fs es) ++ "\n"
showul fs e = indent $ showElem fs e

showol :: [String] -> Elem -> String
showol fs (Tag "li" [] es) = "# " ++ indent' (showElems fs es) ++ "\n"
showol fs e = indent $ showElem fs e

asciiletter :: GenParser Char st Char
asciiletter = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

idenletter :: GenParser Char st Char
idenletter = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-:"

iden :: GenParser Char st String
iden = do
  a <- asciiletter
  r <- many $ idenletter
  return $ a:r

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

confluenceParser :: GenParser Char st Doc
confluenceParser = do
  e <- doc
  eof
  return e

confluenceToPandoc :: [String] -> String -> String
confluenceToPandoc fs s = case parse confluenceParser "" s of
  Left e   -> "Error: " ++ show e ++ "\n" ++ s
  Right s' -> concatMap (showElem fs) s' -- ++ "\n\n\nOriginal:\n\n" ++ s
