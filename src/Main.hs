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

data Entry = Entry {
  url :: Text,
  subject :: Text,
  date :: Maybe LocalTime
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

fetchSearches :: Alert -> IO LText
fetchSearches (Alert searches) = fmap (TLB.toLazyText . htmlize . mconcat . intersperse "\n\n") $
  withManager defaultManagerSettings $ \m ->
    forM searches $ \(Search req) -> do
      withHTTP req m $ \resp -> do
        -- BP.print req
        txt <- decodeLatin1 <$>
          PP.fold (\x y -> x <> BB.byteString y) mempty BB.toLazyByteString (responseBody resp)

        let
          title :: TLB.Builder
          title = TLB.fromText $ txt ^. xml . deep (node "{http://purl.org/rss/1.0/}title") . text

          entries :: [Entry]
          entries = txt ^.. xml . node "{http://purl.org/rss/1.0/}item" & over each entry
            where
              entry :: Element -> Entry
              entry x = Entry
                      (x ^. node "{http://purl.org/rss/1.0/}title" . text)
                      (x ^. node "{http://purl.org/dc/elements/1.1/}source" . text)
                      (x ^. node "{http://purl.org/dc/elements/1.1/}date" . text & cldateToLocal)



        return $ mconcat $ "Search: " <> title <> "\n<ul>" : fmap (("<li>" <>) . (<> "</li>\n") . renderEntry ) entries <> ["</ul><br>\n"]
  where
    renderEntry :: Entry -> TLB.Builder
    renderEntry entry =
        TLB.fromText (maybe "" show $ date entry) <>
        "&nbsp;<a href=\"" <> TLB.fromText (subject entry) <> "\">" <>
        TLB.fromText (url entry) <> "</a>"

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
-- defAlert = Alert [motoSearch "(victory | triumph) -suzuki -yamaha", fitSearch "(dumbbell* | dumbell* | \"dumb bell*\" | powerblock* | \"power block*\" | selecttech* | \"select tech*\") -tires"]
defAlert = Alert [motoSearch "(victory | triumph) -suzuki -yamaha -kafe -lexington"]
  where
    motoSearch :: Text -> Search
    motoSearch = search "mca"
    -- fitSearch = search "sss"

main :: IO ()
main = do
  emails <- execParser opts
  emailTxt <- fetchSearches defAlert
  BP.putStrLn $ show emails
  renderSendMail $ mail emails emailTxt

  where
    opts = info (helper <*> emailParser) $
            fullDesc <> progDesc "Simply set the to and from and you will receive an email about the latest and greatest in hard coded motorcyle deals on craigslist in your area which is Louisville." <> header "clalert - alerts you to deals on craigslist"

    emailParser :: Parser EmailInfo
    emailParser = EmailInfo <$>
      (T.pack <$> strOption (short 't' <> long "to" <> metavar "EMAIL" <> help "Set to address")) <*>
      (T.pack <$> strOption (short 'f' <> long "from" <> metavar "EMAIL" <> help "Set from address"))

