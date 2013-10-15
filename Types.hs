{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}


module Types where


import Data.Aeson.TH                        (deriveJSON)
import Data.Monoid                          (mconcat)
import Data.Ord                             (comparing)
import Data.Text.Lazy                       (Text)

import qualified Data.Text.Lazy             as T


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
