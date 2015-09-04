{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PSS.Foundation where

import Control.Applicative

import qualified Data.ByteString.Lazy.Char8 as BS
import Database.Persist.MySQL
import Data.Maybe
import Data.Text (Text)

import Text.Hamlet
import Text.Jasmine

import Yesod
import Yesod.Default.Util
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery
import Yesod.Static

import PSS.Database
import PSS.Operations

-- | The foundation value for Yesod.
data App = App {
    -- | The pool of connections to the database we will use.
    cpool :: ConnectionPool,
    -- | The static resource of this site.
    getStatic :: Static }

mkYesod "App" [parseRoutes|
/ FrameR GET
/st StaticR Static getStatic
/co/home HomeR GET
/co/about AboutR GET
/co/links LinksR GET
/dy/search SearchR GET POST
/dy/add AddR GET POST
|]

instance Yesod App where
    -- | A default implementation of `addStaticContent`, minifying javascript, saving static files
    -- into a folder named "static", and giving static files a name based on their hashed file
    -- contents.
    addStaticContent = addStaticContentExternal
        minifym
        (\bs -> "ag-" ++ base64md5 bs)
        "static"
        (StaticR . flip StaticRoute [])

-- | A layout that creates only the content of a website (content of the `<body>`-tag).
contentLayout :: WidgetT App IO () -> HandlerT App IO Html
contentLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent contents
    mmsg <- getMessage
    withUrlRenderer [hamlet|
$maybe msg <- mmsg
    <div #message>#{msg}
<h1>#{title}
<hr />
^{bodyTags}
|]

messageLayout :: HandlerT App IO Html
messageLayout = do
    mmsg <- getMessage
    withUrlRenderer [hamlet|
$maybe msg <- mmsg
    <div .widget-message>#{msg}
|]

-- | The file type of a static file: Either Stylesheet or Script.
data StaticFileType = Stylesheet | Script

-- | Gets the file type and name of a static file and saves that into the `static` folder.
addStaticWidget :: StaticFileType -> FilePath -> WidgetT App IO ()
addStaticWidget ft fn = do
    content <- liftIO $ BS.readFile fn
    mer <- handlerToWidget $ case ft of {
        Stylesheet -> addStaticContent ".css" "text/css" content;
        Script -> addStaticContent ".js" "text/javascript" content;
    }
    let er = fromJust mer
    case ft of {
        Stylesheet -> either addStylesheetRemote (addStylesheet . fst) er;
        Script -> either addScriptRemote (addScript . fst) er;
    }

-- | A shortcut for creating a bootstrap alert message.
alertMsg :: Text -> Html -> Html
alertMsg typ inn = [shamlet|
<div class="alert alert-#{typ}" role="alert">#{inn}
|]

alertSuccess :: Html -> Html
alertSuccess = alertMsg "success"

alertInfo :: Html -> Html
alertInfo = alertMsg "info"

alertWarning :: Html -> Html
alertWarning = alertMsg "warning"

alertDanger :: Html -> Html
alertDanger = alertMsg "danger"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    -- | Uses the connection pool of the foundation type to run actions.
    runDB action = do
        app <- getYesod
        runSqlPool action (cpool app)

-- | A field for a double value between 0 and 10.
bValField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Double
bValField = checkBool (\x -> x <= 10.0 && 0 <= x) ("Wert liegt nicht zwischen 0 und 10!" :: Text) doubleField

-- | The input form to add a book.
addBookForm :: FormInput (HandlerT App IO) Book
addBookForm = Book
    <$> ireq textField "name"
    <*> ireq textField "author"
    <*> pure Nothing
    <*> ireq bValField "spann"
    <*> ireq bValField "acti"
    <*> ireq bValField "lineart"
    <*> ireq bValField "emotion"
    <*> ireq bValField "realit"
    <*> ireq bValField "einfuehl"
    <*> ireq bValField "horror"
    <*> ireq bValField "gutboes"
    <*> ireq bValField "tief"
    <*> ireq bValField "zeit"
    <*> ireq bValField "nachv"
    <*> ireq bValField "humor"
    <*> ireq bValField "ende"

-- | Gets the frame resource of the website, which loads all other content pages.
getFrameR :: Handler Html
getFrameR = defaultLayout $ do
    setTitle "Bücherkarten"
    addStaticWidget Stylesheet "pre/css/bootstrap.css"
    addStaticWidget Stylesheet "pre/css/frame.css"
    addStaticWidget Script "pre/js/bootstrap.js"
    addStaticWidget Script "pre/js/frame.js"
    [whamlet|<h1>Hey there!|]

-- | Gets the homepage (content format).
getHomeR :: Handler Html
getHomeR = contentLayout $(whamletFile "pre/html/home.hamlet")

-- | Gets the "about" page (content format).
getAboutR :: Handler Html
getAboutR = contentLayout $(whamletFile "pre/html/about.hamlet")

-- | Gets the "links" page (content format).
getLinksR :: Handler Html
getLinksR = contentLayout $(whamletFile "pre/html/links.hamlet")

-- | The dynamic version of the search page, accepting the search request (content format).
postSearchR :: Handler Html
postSearchR = do {
        -- mi now is a Maybe (name information, author information)
        mi <- do {n <- lookupPostParam "name"; a <- lookupPostParam "author"; return (n, a)};
        maybe
            (setMessage "Bitte beide Felder ausfüllen!")
            (\(n, a) -> runDB $ searchBookOperation (BookSpec n a) 0 10)
            mi;
        messageLayout;
    }

-- | Gets the static version of the search page (content format).
getSearchR :: Handler Html
getSearchR = contentLayout $(whamletFile "pre/html/search.hamlet")

-- | The dynamic version of the add page, accepting the add form (content format).
postAddR :: Handler Html
postAddR = do
    setMessage "OK, Buch hinzugefügt" -- We assume that everything went fine
    book <- runInputPost addBookForm
    runDB $ addBookOperation book
    messageLayout

-- | The static version of the add page (content format).
getAddR :: Handler Html
getAddR = contentLayout $(whamletFile "pre/html/add.hamlet")
