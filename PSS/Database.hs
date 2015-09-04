{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PSS.Database where

import Database.Persist.MySQL
import Data.Maybe
import Data.Text (Text)

import Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Book sql=books
    name Text
    author Text
    count Int Maybe
    spann Double
    acti Double
    lineart Double
    emotion Double
    realit Double
    einfuehl Double
    horror Double
    gutboes Double
    tief Double
    zeit Double
    nachv Double
    humor Double
    ende Double
    BookSpec name author
    deriving Show
|]

-- | Merges the given opinion into the existing set of opinions.
-- `given -> existing -> result`
mergeOpinion :: Book -> Book -> Book
mergeOpinion b bs = Book
    (bookName b)
    (bookAuthor b)
    (Just $ cg + 1)
    (mer bookSpann)
    (mer bookActi)
    (mer bookLineart)
    (mer bookEmotion)
    (mer bookRealit)
    (mer bookEinfuehl)
    (mer bookHorror)
    (mer bookGutboes)
    (mer bookTief)
    (mer bookZeit)
    (mer bookNachv)
    (mer bookHumor)
    (mer bookEnde)
    where
        cg = fromMaybe 1 (bookCount bs)
        mer f = (f b + f bs * fromIntegral cg) / fromIntegral (cg + 1)

type BookSpec = Unique Book

-- | Gets the unique constraint from a book.
specFromBook :: Book -> BookSpec
specFromBook b = BookSpec (bookName b) (bookAuthor b)

-- | The default connection info to connect with the database.
connInfo :: ConnectInfo
connInfo = defaultConnectInfo {connectDatabase = "bookadvice"}
