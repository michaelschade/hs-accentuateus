{-# LANGUAGE OverloadedStrings #-}

module Text.AccentuateUs
    ( Lang
    , Locale
    , AUSResponse(..)
    , LangsStatus(..)
    , langs
    , accentuate
    , feedback
    ) where

import Control.Monad        ( liftM )
import Data.Maybe           ( fromMaybe )
import Data.Text.Encoding   ( decodeUtf8, encodeUtf8 )
import Text.JSON            ( JSON(..), JSValue(..), Result(Ok, Error), decode
                            , encode, toJSObject, valFromObj
                            )
import Network.HTTP         ( Header(Header), HeaderName(..), Request(Request)
                            , RequestMethod(POST), getResponseBody, simpleHTTP
                            , catchIO
                            )
import Network.URI          ( URI(URI), URIAuth(URIAuth) )
import qualified Data.ByteString.Char8  as C8
import qualified Data.Text              as T

type Lang   = C8.ByteString
type Locale = C8.ByteString

-- | Get langs and their localized names
langs :: Maybe Locale -> Int -> IO (Either C8.ByteString AUSResponse)
langs l v = catchIO (liftM eitherDecode call) (\_ -> err)
    where
        call = post [PCall "langs", PLocale (mbString l), PVersion v]
        err  = return . Left $ "Network error. Unable to retrieve languages."

-- | For a given language, and optionally a locale, accentuates text. E.g.,
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Text.AccentuateUs   (accentuate, text)
-- > import Control.Monad       (liftM)
-- > import Data.Either         (either)
-- > import Data.Text.Encoding  (decodeUtf8)
-- > import qualified Data.Text.IO as TIO
-- >
-- > TIO.putStrLn =<< liftM (either decodeUtf8 text)
-- >    (accentuate "vie" (Just "en") "My tu bo ke hoach la chan ten lua")
accentuate  :: Lang -> Maybe Locale -> T.Text
            -> IO (Either C8.ByteString AUSResponse)
accentuate la lo t = catchIO (liftM eitherDecode call) (\_ -> err)
    where
        call = post [PCall "lift", PLang la, PLocale (mbString lo), PText t]
        err  = return . Left $ C8.append
            "Network error. Unable to accentuate text for language " la

-- | Submits corrected text as feedback to Accentuate.us
feedback :: Lang -> Maybe Locale -> T.Text
         -> IO (Either C8.ByteString AUSResponse)
feedback la lo t = catchIO (liftM eitherDecode call) (\_ -> err)
    where
        call = post [PCall "feedback", PLang la, PLocale (mbString lo), PText t]
        err  = return . Left $ "Network error. Unable to submit feedback."

-- | Encapsulates various properties of an Accentuate.us API call
data Param
    = PCall     C8.ByteString
    | PCode     Integer
    | PText     T.Text
    | PLang     Lang
    | PLocale   Locale
    | PVersion  Int
    deriving (Show)

-- | Represents responses for the three Accentuate.us calls
data AUSResponse
    = Langs { status    :: LangsStatus
            , version   :: Int
            , languages :: [(Lang, T.Text)] -- ^ [(ISO-639, Localized Language)]
            }
    | Lift  { text :: T.Text }
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
                        pairs' _        = liftM (map splitPair . C8.lines) txt
                        txt             = valFromObj "text" rsp
            "charlifter.lift" ->
                case code::Int of
                    200 -> liftM (Lift . decodeUtf8) (valFromObj "text" rsp)
                    400 -> fail'
                    _   -> failCode
            "charlifter.feedback" ->
                case code::Int of
                    100 -> return Feedback
                    400 -> fail'
                    _   -> failCode
            c -> fail ("Unknown Accentuate.us call " ++ c)
            where   fail'    = valFromObj "text" rsp >>= \e -> fail e
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
splitPair :: C8.ByteString -> (C8.ByteString, T.Text)
splitPair s = removeSep $ C8.break (== ':') s
    where removeSep (a, b) = (a, decodeUtf8 . C8.tail $ b)

-- | Sends response to server
post :: [Param] -> IO C8.ByteString
post ps = (simpleHTTP . prepRequest $ ps) >>= \r -> getResponseBody r

-- | Create request
prepRequest :: [Param] -> Request C8.ByteString
prepRequest params  = Request (url lang) POST (headers body) body
    where   ps      = toQuery params
            body    = C8.pack . encode . toJSObject $ ps
            lang    = mbString ("lang" `lookup` ps)

-- | Map parameters to call-appropriate tuples
toQuery :: [Param] -> [(String, C8.ByteString)]
toQuery = map toQuery' where
    toQuery' p = case p of
        PCall c     -> ("call",     "charlifter." `C8.append` c)
        PCode c     -> ("code",     C8.pack . show $ c)
        PText t     -> ("text",     encodeUtf8 t)
        PLang l     -> ("lang",     l)
        PLocale  l  -> ("locale",   l)
        PVersion v  -> ("version",  C8.pack . show $ v)

-- | Common response parsing
eitherDecode :: (JSON a) => C8.ByteString -> Either C8.ByteString a
eitherDecode  = resultToEither' . decode . C8.unpack
    where   resultToEither' (Ok a)       = Right a
            resultToEither' (Error s)    = Left . C8.pack $ s

-- | Conversion from optional parameter to (empty) string.
mbString :: Maybe C8.ByteString -> C8.ByteString
mbString  = fromMaybe ""

-- | Generate appropriate headers
headers :: C8.ByteString -> [Header]
headers s =
    [ Header HdrContentType     "application/json; charset=utf-8"
    , Header HdrUserAgent       "Accentuate.us/0.9 haskell"
    , Header HdrContentLength   (show . length . C8.unpack $ s)
    ]

-- | Generate language-specific URL
url :: Lang -> URI
url lang = URI "http:" uriAuth "/" "" ""
    where   uriAuth = Just (URIAuth "" host ":8080")
            base    = "api.accentuate.us"
            host    = (if lang' /= "" then lang' ++ "." else lang') ++ base
            lang'   = C8.unpack lang
