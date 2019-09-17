{-# LANGUAGE OverloadedStrings #-}

module Confluence.Format(confluenceToPandoc) where

import Data.Char(isSpace)
import Data.List(intercalate)
import Data.Text(pack, unpack)
import Data.Text.Lazy(toStrict)
import Data.Text.Lazy.Builder(toLazyText)
import HTMLEntities.Decoder(htmlEncodedText)
import Text.ParserCombinators.Parsec

data Attr = Attr String String deriving Show
data Elem = Tag String [Attr] [Elem] | Text String deriving Show
type Doc = [Elem]

  -- t <- choice $ map string ["p", "em", "h2", "li", "ul", "a", "code", "table", "tbody", "tr", "th", "td"]

show'' :: [String] -> [Elem] -> String
show'' fs es = concatMap (show' fs) es

indentWith :: String -> String -> String
indentWith s = unlines . map (s ++) . lines

indentWith' :: String -> String -> String
indentWith' s = intercalate ('\n' : s) . lines

indent :: String -> String
indent = indentWith "  "

indent' :: String -> String
indent' = indentWith' "  "

findLink :: String -> String
findLink l@('h':'t':'t':'p':':':_) = l
findLink l@('h':'t':'t':'p':'s':':':_) = l
findLink l@('f':'t':'p':':':_) = l
findLink l = '/':l

findLink' :: String -> String -> String
findLink' s l = "/" ++ s ++ "-" ++ l

decode :: String -> String
decode = unpack . toStrict . toLazyText . htmlEncodedText . pack

show' :: [String] -> Elem -> String
show' fs (Tag "p" [] es) = "\n" ++ show'' fs es ++ "\n"
show' fs (Tag "p" [Attr "class" "auto-cursor-target"] es) = "\n" ++ show'' fs es ++ "\n"
show' fs (Tag "p" [Attr "style" s] es) = "\n[" ++ show'' fs es ++ "]{style=\"" ++ s ++ "\"}\n"
show' fs (Tag "span" [Attr "style" s] es) = "[" ++ show'' fs es ++ "]{style=\"" ++ s ++ "\"}"
show' _  (Tag "hr" [] []) = "---\n"
show' _  (Tag "br" [] []) = "\n"
show' fs (Tag "div" [] es) = "\n:::\n" ++ show'' fs es ++ "\n:::\n"
show' fs (Tag "h1" [] es) = "\n# " ++ show'' fs es ++ "\n"
show' fs (Tag "h2" [] es) = "\n## " ++ show'' fs es ++ "\n"
show' fs (Tag "h3" [] es) = "\n### " ++ show'' fs es ++ "\n"
show' fs (Tag "h4" [] es) = "\n#### " ++ show'' fs es ++ "\n"
show' fs (Tag "h5" [] es) = "\n##### " ++ show'' fs es ++ "\n"
show' fs (Tag "h6" [] es) = "\n###### " ++ show'' fs es ++ "\n"
show' fs (Tag "a" [Attr "href" l] es) = "[" ++ show'' fs es ++ "](" ++ findLink l ++ ")"
show' fs (Tag "a" [Attr "class" "external-link", Attr "href" l, Attr "rel" "nofollow"] es) = "[" ++ show'' fs es ++ "](" ++ l ++ "){.external-link}" -- drop the rel="nofollow"
show' _  (Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]) = "[" ++ l ++ "](" ++ findLink l ++ ")"
show' fs (Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] [], Tag "ac:plain-text-link-body" [] es]) = "[" ++ show'' fs es ++ "](" ++ findLink l ++ ")"
show' fs (Tag "ac:link" [Attr "ac:anchor" la] [Tag "ri:page" [Attr "ri:content-title" l] [], Tag "ac:plain-text-link-body" [] es]) = "[" ++ show'' fs es ++ "](" ++ findLink l ++ "#" ++ la ++ ")"
show' fs (Tag "ac:link" [Attr "ac:anchor" la] [Tag "ac:plain-text-link-body" [] es]) = "[" ++ show'' fs es ++ "](#" ++ la ++ ")"
show' fs (Tag "ac:image" [] [Tag "ri:attachment" [Attr "ri:filename" f] es]) = "![" ++ show'' fs es ++ "](" ++ f ++ ")"
show' fs (Tag "ac:image" [Attr "ac:height" val] [Tag "ri:attachment" [Attr "ri:filename" f] es]) = "![" ++ show'' fs es ++ "](" ++ f ++ "){height=" ++ val ++ "}"
show' fs (Tag "ac:image" [Attr "ac:width" val] [Tag "ri:attachment" [Attr "ri:filename" f] es]) = "![" ++ show'' fs es ++ "](" ++ f ++ "){width=" ++ val ++ "}"
show' fs (Tag "ac:inline-comment-marker" _ es) = show'' fs es
show' fs (Tag "span" [] es) = show'' fs es
show' fs (Tag "code" [] es) = "`" ++ show'' fs es ++ "`"
show' fs (Tag "code" [Attr "style" "font-size: 21.546px;"] es) = "`" ++ show'' fs es ++ "`"
show' fs (Tag "ac:structured-macro" (Attr "ac:name" "anchor":_) [Tag "ac:parameter" [Attr "ac:name" ""] name]) = "{#" ++ show'' fs name ++ "}"
show' fs (Tag "ac:structured-macro" (Attr "ac:name" "code":_) es) = showCodeMacro fs False [] es
show' fs (Tag "ac:structured-macro" (Attr "ac:name" "bestpractice":_) [Tag "ac:parameter" [Attr "ac:name" "atlassian-macro-output-type"] [Text "INLINE"], Tag "ac:rich-text-body" [] es]) = "::: {.bestpractice}\n" ++ show'' fs es ++ "\n:::\n" -- don't actually inline, just use a div
show' fs (Tag "ac:structured-macro" (Attr "ac:name" "warning":_) [Tag "ac:rich-text-body" [] es]) = "::: {.warning}\n" ++ show'' fs es ++ "\n:::\n"
show' fs (Tag "ac:structured-macro" (Attr "ac:name" "info":_) [Tag "ac:rich-text-body" [] es]) = "::: {.info}\n" ++ show'' fs es ++ "\n:::\n"
show' fs (Tag "ac:structured-macro" (Attr "ac:name" "tip":_) [Tag "ac:rich-text-body" [] es]) = "::: {.tip}\n" ++ show'' fs es ++ "\n:::\n"
show' _  (Tag "ac:structured-macro" (Attr "ac:name" "include":_) [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:space-key" s, Attr "ri:content-title" l] []]]]) = "#include \"" ++ findLink' s l ++ "\""
show' _  (Tag "ac:structured-macro" (Attr "ac:name" "include":_) [Tag "ac:parameter" [Attr "ac:name" ""] [Tag "ac:link" [] [Tag "ri:page" [Attr "ri:content-title" l] []]]]) = "#include \"" ++ findLink l ++ "\""
show' fs (Tag "em" [] es) = "*" ++ show'' fs es ++ "*"
show' fs (Tag "strong" [] es) = "**" ++ show'' fs es ++ "**"
show' fs (Tag "ul" _ es) = concatMap (showul fs) es
show' fs (Tag "ol" _ es) = concatMap (showol fs) es
show' fs (Tag "blockquote" [] es) = "\n" ++ indentWith "> " (unlines . dropWhile (=="") . lines $ show'' fs es) ++ "\n"
show' fs (Tag t as es) = "<" ++ t ++ show as ++ ">\n" ++ show'' fs es ++ "\n</" ++ t ++ ">"
show' _  (Text s) = decode s

showCodeMacro :: [String] -> Bool -> [String] -> [Elem] -> String
showCodeMacro fs _        attrs (Tag "ac:parameter" [Attr "ac:name" "language"] [Text t]:es) = showCodeMacro fs True (('.':t):attrs) es
showCodeMacro fs haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "firstline"] [Text t]:es) = showCodeMacro fs haveLang (".numberLines":("startFrom=\"" ++ t ++ "\""):attrs) es
showCodeMacro fs haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "linenumbers"] [Text "true"]:es) = showCodeMacro fs haveLang (".numberLines":attrs) es
showCodeMacro fs haveLang attrs (Tag "ac:parameter" [Attr "ac:name" "title"] [Text t]:es) = showCodeMacro fs haveLang (("title=\"" ++ t ++ "\""):attrs) es
showCodeMacro fs haveLang attrs [Tag "ac:plain-text-body" [] es] = "```" ++ t ++ "\n" ++ s ++ "\n```\n"
  where
    s = show'' fs es
    t = if null a then "" else " {" ++ intercalate " " a ++ "}"
    a = reverse $ if haveLang then attrs else
          case dropWhile isSpace s of
            '<':_ -> ".xml":attrs
            '-':_ -> ".yaml":attrs
            _     -> attrs
showCodeMacro fs haveLang attrs es = showCodeMacro fs haveLang attrs [Tag "ac:plain-text-body" [] es]

showul :: [String] -> Elem -> String
showul fs (Tag "li" [] es) = "* " ++ indent' (show'' fs es) ++ "\n"
showul fs (Tag "li" [Attr "style" "list-style-type: disc;"] es) = "* " ++ indent' (show'' fs es) ++ "\n"
showul fs e = indent $ show' fs e

showol :: [String] -> Elem -> String
showol fs (Tag "li" [] es) = "# " ++ indent' (show'' fs es) ++ "\n"
showol fs e = indent $ show' fs e

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
  Right s' -> concatMap (show' fs) s' ++ "\n\n\nOriginal:\n\n" ++ s
