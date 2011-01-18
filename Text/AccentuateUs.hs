module Text.AccentuateUs
    ( AUSResponse(..)
    , LangsStatus(..)
    , langs
    , accentuate
    , feedback
    ) where

import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Network.HTTP (Header(Header), HeaderName(..), Request(Request)
    , RequestMethod(POST), getResponseBody, simpleHTTP)
import Network.URI (URI(URI), URIAuth(URIAuth))
import Text.JSON (JSON(..), decode, encode, JSValue(..), resultToEither,
    toJSObject, valFromObj)

type Lang = String
type Locale = String

-- | Get langs and their localized names
langs :: Maybe Locale -> Int -> IO (Either String AUSResponse)
langs l v = liftM eitherDecode $
    post [PCall "langs", PLocale (mbLocale l), PVersion v]

-- | For a given language, and optionally a locale, accentuates text
accentuate :: Lang -> Maybe Locale -> String -> IO (Either String AUSResponse)
accentuate la lo t = liftM eitherDecode $
    post [PCall "lift", PLang la, PLocale (mbLocale lo), PText t]

-- | Submits corrected text as feedback to Accentuate.us
feedback :: Lang -> Maybe Locale -> String -> IO (Either String AUSResponse)
feedback la lo t = liftM eitherDecode $
    post [PCall "feedback", PLang la, PLocale (mbLocale lo), PText t]

-- | Encapsulates various properties of an Accentuate.us API call
data Param
    = PCall String
    | PCode Integer
    | PText String
    | PLang Lang
    | PLocale Locale
    | PVersion Int
    deriving (Show)

-- | Represents responses for the three Accentuate.us calls
data AUSResponse
    = Langs { status    :: LangsStatus
            , version   :: Int
            , languages :: [(String, Lang)] -- ^ [(ISO-639, Localized Language)]
            }
    | Lift  { text :: String }
    | Feedback
    deriving Show

-- | Represents languages response status
data LangsStatus = OutOfDate -- ^ Given version number  < server's
                 | UpToDate  -- ^ Given version number == server's
                 | OverDate  -- ^ Given version number  > server's
                 deriving (Show, Eq)

instance JSON AUSResponse where
    readJSON (JSObject rsp) = do
        call <- valFromObj "call" rsp
        code <- valFromObj "code" rsp
        case call of
            "charlifter.langs" -> do
                code' <- mbCode (codeToStatus code)
                vers  <- valFromObj "version" rsp
                pairs <- pairs' code'
                return Langs { status    = code'
                             , version   = read vers
                             , languages = pairs
                             }
                where   pairs' UpToDate = return []
                        pairs' _        = liftM (map splitPair . lines) txt
                        txt             = valFromObj "text" rsp
            "charlifter.lift" ->
                case code::Int of
                    200 -> liftM Lift (valFromObj "text" rsp)
                    400 -> fail'
                    _   -> failCode
            "charlifter.feedback" ->
                case code::Int of
                    100 -> return Feedback
                    400 -> fail'
                    _   -> failCode
            c -> fail ("Unknown Accentuate.us call " ++ c)
            where   fail'    = (valFromObj "text" rsp) >>= \e -> fail e
                    failCode = fail "Unknown Accentuate.us response code"
                    mbCode (Just c) = return c
                    mbCode Nothing  = failCode
    readJSON _ = undefined
    showJSON   = undefined

-- | Converts integer response code into data type LangsStatus
codeToStatus :: Int -> Maybe LangsStatus
codeToStatus c = case c of
    100 -> Just OutOfDate
    200 -> Just UpToDate
    400 -> Just OverDate
    _   -> Nothing

-- | Splits a string pair (separated by :) into a tuple, removing separator
splitPair :: String -> (String, String)
splitPair s = removeSep $ break (== ':') s
    where removeSep (a,b) = (a, tail b)

-- | Sends response to server
post :: [Param] -> IO String
post ps = (simpleHTTP . prepRequest $ ps) >>= \r -> getResponseBody r

-- | Create request
prepRequest :: [Param] -> Request String
prepRequest params = Request (url lang) POST (headers body) body
    where   ps   = toQuery params
            body = encode . toJSObject $ ps
            lang = maybe "" id $ "lang" `lookup` ps

-- | Map parameters to call-appropriate tuples
toQuery :: [Param] -> [(String, String)]
toQuery = map toQuery' where
    toQuery' p = case p of
        PCall c     -> ("call", "charlifter." ++ c)
        PCode c     -> ("code", show c)
        PText t     -> ("text", t)
        PLang l     -> ("lang", l)
        PLocale  l  -> ("locale",  l)
        PVersion v  -> ("version", show v)

-- | Produces locale from Maybe
mbLocale :: Maybe Locale -> Locale
mbLocale = maybe "" (\l -> l)

-- | Common response parsing
eitherDecode :: (JSON a) => String -> Either String a
eitherDecode  = resultToEither . decode

-- | Generate appropriate headers
headers :: String -> [Header]
headers s = [(Header HdrContentType "application/json; charset=utf-8")
    , (Header HdrUserAgent "Accentuate.us/0.9 haskell")
    , (Header HdrContentLength cl)
    ] where cl = show . length $ s

-- | Generate language-specific URL
url :: Lang -> URI
url lang = URI "http:" uriAuth "/" "" ""
    where   uriAuth = Just (URIAuth "" host ":8080")
            base    = "api.accentuate.us"
            host    = (if lang /= "" then (lang ++ ".") else lang) ++ base
