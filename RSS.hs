module RSS (replaceFeedItem) where
import           Data.Char (isSpace)
import           Data.Maybe
import qualified Data.Text as T
import           Text.Feed.Query
import           Text.Feed.Types
import           Text.ParserCombinators.Parsec

data XMLAST
  = Element Name
            [Attribute]
            [XMLAST]
  | Body String
  deriving (Show)
   
type Name = String
type Attribute = (Key, Value)
type Key = String
type Value = String

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

strip :: String -> String
strip = dropWhile isSpace

feedItem :: GenParser Char st [XMLAST]
feedItem = many innerXML

innerXML :: GenParser Char st XMLAST
innerXML = xmlParser <|> parseBody

parseBody :: GenParser Char st XMLAST
parseBody =
  fmap Body $
  many1 $
  noneOf "<>"

xmlParser :: GenParser Char st XMLAST
xmlParser = try selfClosingTag <|> fullTag

selfClosingTag :: GenParser Char st XMLAST
selfClosingTag = do try $ char '<' >> notFollowedBy (char '/')
                    tag <- many $ letter <|> digit
                    spaces
                    a <- try $ many keyValue
                    _ <- string "/>"
                    return $ Element tag a []

fullTag :: GenParser Char st XMLAST
fullTag =
  do (name,attr) <- openTag
     inner <- many innerXML
     closeTag name
     return $
       Element name attr inner
  
openTag :: GenParser Char st (String, [Attribute])
openTag = do
  try $ char '<' >> notFollowedBy (char '/')
  tag <- many (letter <|> digit)
  spaces
  a <- try $ many keyValue
  _ <- char '>'
  return (tag, a)
  
keyValue :: GenParser Char st Attribute
keyValue =
  do key <- many1 $ letter <|> digit <|>
            char '-'
     spaces
     _ <- char '='
     spaces
     value <- quotedString
     spaces
     return (key,value)

quotedString :: GenParser Char st String
quotedString =
  do _ <- char '"'
     value <- many $
              noneOf "\""
     _ <- char '"'
     return value

closeTag :: String -> GenParser Char st ()
closeTag name =
  do _ <- string "</"
     spaces
     _ <- string name
     spaces
     _ <- char '>'
     return ()

replace :: String -> [XMLAST] -> T.Text
replace absolute x =
  T.pack $ strip . rstrip $
  replace' absolute x

replace' :: String -> [XMLAST] -> String
replace' absolute =
  foldr ((++) . replaceTag absolute) ""

replaceTag :: String -> XMLAST -> String
replaceTag _ (Body x) = x
replaceTag absolute (Element name attr body)
  | name == "img" = replaceImg absolute attr
  | name == "ul" = replaceList absolute body
  | name == "p" =
    replace' absolute body ++
    "\n\n"
  | name == "a" = replaceLink absolute attr body
  | name == "br" = "<br />"
  | name `elem`
      ["h1","h2","h3","h4"] =
    "\n## " ++
    replace' absolute body ++
    "\n"
  | name `elem`
      ["h5","h6"] =
    "\n### " ++
    replace' absolute body ++
    "\n"
  | name == "fieldset" = ""
  | otherwise = replace' absolute body

replaceList :: String -> [XMLAST] -> String
replaceList absolute items =
  '\n' :
  foldr (replaceListItem absolute) "" items

replaceListItem :: String -> XMLAST -> String -> String
replaceListItem absolute (Element "li" _ body) acc =
  "* " ++
  replaceListBody absolute body ++
  "\n" ++
  acc
replaceListItem _ _ acc = acc

replaceListBody :: String -> [XMLAST] -> String
replaceListBody _ [] = ""
replaceListBody absolute (Body body:rest) =
  body ++
  replaceListBody absolute rest
replaceListBody absolute (Element "a" attr body:rest) =
  replaceLink absolute attr body ++
  replaceListBody absolute rest
replaceListBody absolute (_:rest) =
  replaceListBody absolute rest

replaceLink :: String -> [Attribute] -> [XMLAST] -> String
replaceLink absolute attr body =
  '[' :
  replace' absolute body ++
  "]" ++
  "(" ++
  url ++
  ")"
  where src =
          fromMaybe " " $
          lookup "href" attr
        url
          | head src == '/' = absolute ++ src
          | otherwise = src                                                            

replaceImg :: String -> [Attribute] -> String
replaceImg absolute attr = "![" ++ alt ++ "](" ++ url ++ " \"" ++ title ++ "\")"
  where alt =
          fromMaybe "" $
          lookup "alt" attr
        src =
          fromMaybe " " $
          lookup "src" attr
        url
          | head src == '/' = absolute ++ src
          | otherwise = src
        title =
          fromMaybe "" $
          lookup "title" attr


replaceFeedItem :: Text.Feed.Types.Item -> String -> [String] -> Maybe T.Text
replaceFeedItem item absolute tags =
  do title <- getItemTitle item
     let link =
           fromMaybe "" $
           getItemLink item
     desc <- getItemDescription item
     parsed <- safeFromRight $
               parse feedItem "" .
               concat .
               lines $
               desc
     let replaced = replace absolute parsed
     return $
       T.concat [T.pack "## ["
                ,T.pack title
                ,T.pack "]("
                ,T.pack link
                ,T.pack ")\n"
                ,replaced
                ,T.pack "\n\n"
                ,T.pack $ unwords tags]

safeFromRight :: Either b a -> Maybe a
safeFromRight (Left _) = Nothing
safeFromRight (Right a) = Just a
