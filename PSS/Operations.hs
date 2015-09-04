{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PSS.Operations where

import Data.Monoid
import Database.Persist.MySQL

import Yesod

import PSS.Database

-- | Adds a book to the database.
addBookOperation :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Book -> YesodDB site ()
addBookOperation book = do
    mEntity <- getBy $ specFromBook book
    case mEntity of {
        -- book does not exist in db yet
        Nothing -> insert book >> liftIO (return ());
        -- book does exist
        Just (Entity bookId preBook) -> replace bookId (mergeOpinion book preBook);
    }

-- | Searchs a book in the database. It takes the specification of the book and
-- the offset and limit values. For example, if you want to have results number
-- 10 to 20, you use `searchBookOperation spec 10 20`. This can be used for
-- pagination.
searchBookOperation :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => BookSpec -> Int -> Int -> YesodDB site [BookSpec]
searchBookOperation (BookSpec na au) offs limi = do {
        let {query = "SELECT ?? FROM books WHERE name LIKE ? AND author LIKE ? LIMIT ? OFFSET ?;"};
        results <- rawSql query [
            PersistText ("%" <> na <> "%"), -- name spec
            PersistText ("%" <> au <> "%"), -- author spec
            PersistInt64 $ fromIntegral limi, -- limit spec
            PersistInt64 $ fromIntegral offs -- offset spec
        ];
        return $ map (specFromBook . entityVal) results;
    }
