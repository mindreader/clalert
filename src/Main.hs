{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, TemplateHaskell #-}

module Main where


import BasicPrelude as BP

import Network.Mail.Mime

import Pipes.Prelude as PP (fold)
import Pipes.HTTP

import Network.HTTP.Base (urlEncode)

import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Builder as BB (byteString, toLazyByteString)
import qualified Data.Text.Lazy.Builder as TLB (toLazyText, Builder, fromText)
import qualified Data.Text as T (pack)
import Data.Text.Lazy.Encoding (decodeLatin1)

import Text.XML as XML

import Text.Xml.Lens
import Control.Lens

import Data.Time (LocalTime, parseTime)

import Data.Time.Zones (utcToLocalTimeTZ)
import Data.Time.Zones.TH (includeTZFromDB)

import System.Locale (defaultTimeLocale)

import Options.Applicative


data EmailInfo = EmailInfo {
  eTo :: Text,
  eFrom :: Text
} deriving Show

newtype Alert = Alert [Search]
newtype Search = Search Request

newtype Subject = Subject { unSubject :: Text } deriving Show
newtype Title = Title { unTitle :: Text } deriving Show
newtype URL = URL { unURL :: Text } deriving Show

data Result = Result {
  rTitle :: Maybe Title,
  rURL:: Maybe URL,
  rEntries :: [Entry]
}

data Entry = Entry {
  eUrl :: URL,
  eSubject :: Subject,
  eDate :: Maybe LocalTime
} deriving Show


-- testcldate :: Text
-- testcldate = "2014-08-21T22:38:56-04:00"

cldateToLocal :: Text -> Maybe LocalTime
cldateToLocal datestr = utcToLocalTimeTZ tzLouisville <$> utc
  where
    datefmt = "%FT%X%z"
    tzLouisville = $(includeTZFromDB "America/Louisville")
    utc = parseTime defaultTimeLocale datefmt . textToString $ datestr

htmlize :: (Monoid m, IsString m) => m -> m
htmlize body = "<html><head></head><body>" <> body <> "</body></html>"

fetchSearches :: Alert -> (LText -> Result) -> (Result -> TLB.Builder) -> IO LText
fetchSearches (Alert searches) toResult rend = fmap (TLB.toLazyText . htmlize . mconcat . intersperse "\n\n") $
  withManager defaultManagerSettings $ \m ->
    forM searches $ \(Search req) -> do
      withHTTP req m $ \resp -> do
        -- BP.print req
        rend . toResult . decodeLatin1 <$>
          PP.fold (\x y -> x <> BB.byteString y) mempty BB.toLazyByteString (responseBody resp) :: IO TLB.Builder


renderResult :: Result -> TLB.Builder
renderResult (Result title url entries) =
  mconcat $ "Search: " <> renderSearch url title <> "<ul>\n" :
     fmap (("<li>" <>) . (<> "</li>\n") . renderEntry ) entries
    <> ["</ul><br>\n"]
  where
    renderSearch :: Maybe URL -> Maybe Title -> TLB.Builder
    renderSearch Nothing t = renderTitle t
    renderSearch (Just u) t = "<a href=\"" <> (TLB.fromText . unURL $ u) <> "\"> " <> renderTitle t <> "</a>"

    renderTitle :: Maybe Title -> TLB.Builder
    renderTitle Nothing = "No title"
    renderTitle (Just t) = TLB.fromText . unTitle $ t
    
    renderEntry :: Entry -> TLB.Builder
    renderEntry (Entry (URL u) (Subject subject) date) =
        TLB.fromText (maybe "" show date) <>
        "&nbsp;<a href=\"" <> TLB.fromText subject <> "\">" <>
        TLB.fromText u <> "</a>"

channeln,titlen, linkn, itemn, sourcen, daten :: Traversal' Element Element
channeln = node "{http://purl.org/rss/1.0/}channel" 
titlen   = node "{http://purl.org/rss/1.0/}title"
linkn    = node "{http://purl.org/rss/1.0/}link"
itemn    = node "{http://purl.org/rss/1.0/}item"
sourcen  = node "{http://purl.org/dc/elements/1.1/}source"
daten    = node "{http://purl.org/dc/elements/1.1/}date"

parseResult :: AsXmlDocument a => a -> Result
parseResult txt = Result title link entries
  where
    title :: Maybe Title
    link :: Maybe URL
    (title, link) = maybe (Nothing, Nothing) id $
      (txt ^.. xml . channeln & over mapped titleandlink) ^? ix 0
      where
        titleandlink :: Element -> (Maybe Title, Maybe URL)
        titleandlink x =
          (x ^. titlen . text & Just . Title,
           x ^. linkn . text & Just . URL)


    entries :: [Entry]
    entries = txt ^.. xml . itemn & over each entry
      where
        entry :: Element -> Entry
        entry x = Entry
                (x ^. titlen . text & URL)
                (x ^. sourcen . text & Subject)
                (x ^. daten . text & cldateToLocal)


mail :: EmailInfo -> LText -> Mail
mail (EmailInfo eto efrom) body =
  (emptyMail $ Address Nothing efrom) {
    mailTo = [Address Nothing eto],
    mailHeaders = [("Subject", "Craigslist Alert")]
  } & addPart [htmlPart body]

search :: Text -> Text -> Search
search sub criteria = Search def {
            host = "louisville.craigslist.org",
            path = "/search/" <> encodeUtf8 sub,
            queryString = "?format=rss&hasPic=1&query=" <> (B.pack $ urlEncode $ textToString criteria)
          }

defAlert :: Alert
defAlert = Alert [motoSearch "(victory | triumph) -suzuki -yamaha -kafe -lexington"]
  where
    motoSearch :: Text -> Search
    motoSearch = search "mca"
    -- fitSearch = search "sss"


test :: IO LText
test = TLB.toLazyText . renderResult . parseResult <$> XML.readFile def "files/cl.xml"

main :: IO ()
main = do
  emails <- execParser opts
  emailTxt <- fetchSearches defAlert parseResult renderResult
  BP.putStrLn $ show emails
  renderSendMail $ mail emails emailTxt

  where
    opts = info (helper <*> emailParser) $
            fullDesc <>
            progDesc "Simply set the to and from and you will receive an email about the latest and greatest \
                     \in hard coded motorcyle deals on craigslist in your area which is Louisville." <>
            header "clalert - alerts you to deals on craigslist"

    emailParser :: Parser EmailInfo
    emailParser = EmailInfo <$>
      (T.pack <$> strOption (short 't' <> long "to" <> metavar "EMAIL" <> help "Set to address")) <*>
      (T.pack <$> strOption (short 'f' <> long "from" <> metavar "EMAIL" <> help "Set from address"))

