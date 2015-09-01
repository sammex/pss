{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PSS.Database where

import Database.Persist.MySQL
import Data.Text (Text)

import Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Book
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

-- | Gets the unique constraint from a book.
specFromBook :: Book -> Unique Book
specFromBook b = BookSpec (bookName b) (bookAuthor b)

-- | The default connection info to connect with the database.
connInfo :: ConnectInfo
connInfo = defaultConnectInfo {connectDatabase = "bookadvice"}
