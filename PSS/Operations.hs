{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PSS.Operations where

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
