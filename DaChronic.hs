{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}


module DaChronic where


import Control.Applicative
import Data.Aeson.Encode.Pretty             (encodePretty)
import Data.Aeson.TH                        (deriveJSON)
import Data.List                            (sort, nub)
import Data.Ord                             (comparing)
import Data.Maybe                           (fromMaybe)
import Data.Monoid                          (mconcat)
import Data.List.Split                      (splitOn)
import Data.Text.Lazy                       (Text)
import Network                              (withSocketsDo)
import Network.HTTP.Conduit                 (simpleHttp)
import Text.HTML.TagSoup                    (Tag(..), parseTags, fromAttrib,
                                             innerText)
import System.IO                            (withFile, IOMode(..))

import qualified Data.ByteString.Lazy       as B
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as TE
import qualified Text.HTML.TagSoup.Match    as M


main :: IO ()
main = withSocketsDo $ withFile outFileName WriteMode $ \h ->
         B.hPutStr h =<< encodePretty <$> fetchAll
  where
    outFileName = "chronics.json"
    fetchAll    = sequence [fetchNRK, fetchAftenposten, fetchDagbladet]


fetchNRK :: IO Publication
fetchNRK = simpleFetch "NRK"
                       baseURIs
                       aTagFilter
                       titleFinder
                       authorFinder
                       dateFinder
  where
    aTagFilter   = "a" `ofClass` "autonomous"
    titleFinder  = maybeInnerText $ "h1"      `ofClass` "title"
    authorFinder = maybeInnerText $ "strong"  `ofClass` "fn"
    dateFinder _ doc = (y, m, d)
      where [y, m, d] = map read $ splitOn "-" $ T.unpack rawDate
            rawDate   = T.take (T.length "xxxx-xx-xx") rawDate'
            rawDate'  = fromAttrib "datetime" $
                          head $ filter (M.tagOpenAttrNameLit "time"
                                         "class" ("relative"==))
                                        doc

    baseURIs = [T.concat
        ["http://www.nrk.no/serum/api/render/1.8266709?"
        ,"size=512&arrangement.offset=0&arrangement.quantity=258"
        ,"&arrangement.repetition=PATTERN"
        ]
      ]


fetchAftenposten :: IO Publication
fetchAftenposten = simpleFetch "Aftenposten"
                               ["http://www.aftenposten.no/meninger/kommentarer"
                               ,"http://www.aftenposten.no/meninger/debatt"
                               ]
                               aTagFilter
                               titleFinder
                               authorFinder
                               dateFinder
  where
    aTagFilter   = simpleATagFilter True ["/meninger"]
    titleFinder  = maybeInnerText $ "h1" `ofClass` "articleTitle"
    authorFinder = maybeInnerText $ "p"  `ofClass` "author"
    dateFinder _ doc = (y, m, d)
      where [y, m, d] = map read $ splitOn "-" $ T.unpack rawDate
            rawDate   = T.take (T.length "xxxx-xx-xx") rawDate'
            rawDate'  = fromAttrib "datetime" $
                          head $ filter (M.tagOpenAttrNameLit "time"
                                         "pubdate" ("pubdate"==))
                                        doc


fetchDagbladet :: IO Publication
fetchDagbladet = simpleFetch "Dagbladet"
                             ["http://www.dagbladet.no/meninger/kommentarer/"]
                             aTagFilter
                             titleFinder
                             authorFinder
                             dateFinder
  where
    aTagFilter   = simpleATagFilter False ["/kommentar/"]
    titleFinder  = maybeInnerText $ "h2" `ofClass` "main-article-title"
    authorFinder = maybeInnerText $ "p"  `ofClass` "article-byline-name"
    dateFinder uri _ = (y, m, d)
      where [y, m, d] = map read $ splitOn "/" $ T.unpack rawDate
            rawDate = T.take (T.length "xxxx/xx/xx")
                             (T.drop (T.length "http://www.dagbladet.no/") uri)


simpleFetch :: Text
            -> [Text]
            -> (Tag Text -> Bool)
            -> ([Tag Text] -> Maybe Text)
            -> ([Tag Text] -> Maybe Text)
            -> (Text -> [Tag Text] -> (Int, Int, Int))
            -> IO Publication
simpleFetch name baseURIs aTagFilter titleFinder authorFinder dateFinder =
    putStrLn ("READING OPINIONS AT " ++ T.unpack name ++ "...") >>
    Publication name <$> opinions
  where
    opinions = do allTags         <- concat <$> mapM readAndParseURI baseURIs
                  let aTags       = filter aTagFilter allTags
                  let opinionURIs = nub $ sort $ map (fromAttrib "href") aTags
                  reverse <$> nub <$> sort <$> mapM fetchOpinion opinionURIs

    fetchOpinion uri = readAndParseURI uri >>= \doc ->
        let opt = Opinion title author uri month day
            title  = "(ingen tittel)" `ifNot` titleFinder doc
            author = name `ifNot` authorFinder doc
            (_, month, day) = dateFinder uri doc
        in
          putStrLn ("...read " ++ take 90 (show opt)) >>
          return opt


simpleATagFilter :: Bool -> [Text] -> Tag Text -> Bool
simpleATagFilter forceHTMLSuffix validSegs t =
    M.tagOpenLit "a" (const True) t &&
    any (`T.isInfixOf` href) validSegs &&
    not forceHTMLSuffix || ".html" `T.isSuffixOf` href
  where
    href = fromAttrib "href" t


readURI :: Text -> IO Text
readURI url = TE.decodeUtf8With handleErr <$> simpleHttp (T.unpack url)
  where handleErr _ _ = Just '#'


readAndParseURI :: Text -> IO [Tag Text]
readAndParseURI = fmap parseTags . readURI


maybeInnerText :: (Tag Text -> Bool) -> [Tag Text] -> Maybe Text
maybeInnerText c d = innerText <$> getSection c d


getSection :: (Tag Text -> Bool) -> [Tag Text] -> Maybe [Tag Text]
getSection cond doc = scan $ filter cond doc
  where scan [t@(TagOpen n _)] = Just $ takeWhile (TagClose n /=) $
                                        dropWhile (t /=) doc
        scan (t1:_)  = scan [t1]      -- TODO!
        scan []       = Nothing


ofClass :: Text -> Text -> Tag Text -> Bool
tagName `ofClass` className =
  M.tagOpenAttrNameLit tagName "class" (className `T.isInfixOf`)


ifNot :: Text -> Maybe Text -> Text
a `ifNot` m = fromMaybe a m


data Publication = Publication { _name     :: Text
                               , _opinions :: [Opinion]
                               }


data Opinion = Opinion { _title  :: Text
                       , _author :: Text
                       , _uri    :: Text
                       , _month  :: Int
                       , _day    :: Int
                       }
                       deriving (Eq)

instance Ord Opinion where
  compare a b = mconcat $ map (\c -> c a b)
                  [comparing _month
                  ,comparing _day
                  ,comparing _title
                  ,comparing _author
                  ,comparing _uri
                  ]

instance Show Opinion where
  show (Opinion t a _ m d) = T.unpack $ T.concat
                               ["opinion("
                               ,T.take 30 t
                               ,", "
                               ,T.take 20 a
                               ,", m="
                               ,T.pack (show m)
                               ,", d="
                               ,T.pack (show d)
                               ,")"
                               ]


$(deriveJSON (drop 1) ''Publication)
$(deriveJSON (drop 1) ''Opinion)
