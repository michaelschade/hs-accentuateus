import Text.JSON
import Network.HTTP
import Network.URI

type Lang   = String
type Locale = String

headers :: String -> [Header]
headers s = [(Header HdrContentType "application/json; charset=UTF8")
    , (Header HdrUserAgent "Accentuate.us/0.9 haskell")
    , (Header HdrContentLength cl)
    ] where cl = show . length $ s

url :: Lang -> URI
url lang = URI "http:" uriAuth "/" "" ""
    where
        uriAuth = Just (URIAuth "" host ":8080")
        base    = "api.accentuate.us"
        host    = (if lang /= "" then (lang ++ ".") else lang) ++ base

post :: String -> Request String
post s = Request (url "ht") POST (headers s) s

langs :: Locale -> Request String
langs l = post . encode $ params
    where params = toJSObject [("call", "charlifter.langs"), ("locale", l)]

accentuate :: Lang -> Locale -> String -> Request String
accentuate lang locale text = post . encode $ params
    where params = toJSObject [
              ("call",  "charlifter.lift")
            , ("lang",  lang)
            , ("locale",locale)
            , ("text",  text)
            ]

unpack rsp = case rsp of
    Left  e -> show e
    Right r -> rspBody r

main = do
    rsp <- simpleHTTP (accentuate "ht" "en-US" "le la we andey")
    putStrLn . unpack $ rsp
    rsp <- simpleHTTP (langs "es")
    putStrLn . unpack $ rsp
    return ""
