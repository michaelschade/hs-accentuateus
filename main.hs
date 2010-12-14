import Text.JSON
import Network.HTTP
import Network.URI

type Lang = String
type Locale = String

data Param
    = PCall String
    | PCode String
    | PText String
    | PLang String
    | PLocale String
    | PVersion String
    deriving (Show)

toQuery :: [Param] -> [(String, String)]
toQuery = map toQuery' where
    toQuery' p = case p of
        PCall c     -> ("call", "charlifter." ++ c)
        PCode c     -> ("code", c)
        PText t     -> ("text", t)
        PLang l     -> ("lang", l)
        PLocale  l  -> ("locale",   l)
        PVersion v  -> ("version",  v)

headers :: String -> [Header]
headers s = [(Header HdrContentType "application/json; charset=utf-8")
    , (Header HdrUserAgent "Accentuate.us/0.9 haskell")
    , (Header HdrContentLength cl)
    ] where cl = show . length $ s

url :: String -> URI
url lang = URI "http:" uriAuth "/" "" ""
    where
        uriAuth = Just (URIAuth "" host ":8080")
        base    = "api.accentuate.us"
        host    = (if lang /= "" then (lang ++ ".") else lang) ++ base

post :: [Param] -> Request String
post ps = Request (url "ht") POST (headers body) body
    where body = encode . toJSObject . toQuery $ ps

--langs :: String -> Request String
--langs l = post [PCall "langs", PLocale l]

-- Accentuate text
accentuate :: Lang -> Locale -> String -> Request String
accentuate la lo text =
    post [PCall "lift", PLang la, PLocale lo, PText text]

-- Get body or error from response
unpack rsp = case rsp of
    Left  e -> show e
    Right r -> rspBody r

main = do
    rsp <- simpleHTTP $ accentuate "ht" "en-US" "le la we andey"
    putStrLn . unpack $ rsp
    --rsp <- simpleHTTP (langs "es")
    --putStrLn . unpack $ rsp
    return ""
