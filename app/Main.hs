{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty (scotty, get, html, param, redirect, post, ActionM)
import System.Environment (setEnv, getEnvironment)
import Data.Map (fromList, findWithDefault)
import Database.SQLite.Simple (Connection)
import System.Directory (doesFileExist, doesPathExist)
import Lib
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Mustache
import Data.Text.Internal.Lazy (Text)
import qualified Data.Text as Data.Internal
import qualified Data.Text.Internal.Lazy as Data.Internal.Lazy
import qualified GHC.TypeError as Data.Internal

data Env = Env { 
    conn :: Connection 
    , temp :: String
}


templateH1 :: String -> TL.Text
templateH1 tx = TL.pack  $ "<h1>" <> tx <> "</h1>"

temp' :: Template -> String -> Data.Internal.Text
temp' t s = substitute t (toMustache s)

conv :: Data.Internal.Text -> Data.Internal.Lazy.Text
conv t = TL.pack (show t)


scottyFun :: Env -> Template -> IO ()
scottyFun env template = scotty 3000 $ do
        get "/" $ html $ conv $ temp' template "John Galt" --(substitute template "Hilgard")
        --get "/" $ html "<h1>Todo Future</h1>"
        --get "/" $ html $ TL.pack . renderHtml $
        --    H.html $
        --        H.body $ do
        --            H.h1 "Blaze Template H1"
        --            H.form H.! A.method "post" H.! A.action "/" $ do
        --                H.input H.! A.type_ "text" H.! A.name "url"
        --                H.input H.! A.type_ "submit"
        post "/:email" $ do
            email <- param "email" :: ActionM String
            _ <- liftIO $ executeCreateUser (conn env) email "test-key"
            html (templateH1 email)
        get "/:email" $ do
            email <- param "email" :: ActionM TL.Text
            g <- liftIO $ executeGetUserByEmail (conn env) (TL.unpack email)
            case g of
                Nothing -> html (templateH1 "Not found")
                Just u -> html (templateH1 $ show u)

app :: Env -> IO ()
app env = do
    let searchSpace = [".", "./templates"]
        templateName = "mustache.html"
    compiled <- automaticCompile searchSpace templateName
    case compiled of
        Left err -> print err
        Right template -> scottyFun env template

--app' :: Either ParseError Template
app' = do
    let searchSpace = [".", "./templates"]
        templateName = "mustache.html"
    compiled <- automaticCompile searchSpace templateName
    return compiled
   -- case compiled of
   --     Left err -> print err
   --     Right template -> template


setUpEnv :: IO Env
setUpEnv = 
    let 
        toPair :: String -> (String, String)
        toPair ls = let w = words ls in (head w, w!!1)

        setupLocalEnv :: IO ()
        setupLocalEnv = do
            exists <- doesFileExist "env.local"
            Control.Monad.when exists $ do
                f <- readFile "env.local"
                let ls = lines f
                --mapM_ (\l -> let pr = toPair l in setEnv (fst pr) (snd pr) ) ls
                mapM_ (\l -> let pr = toPair l in uncurry setEnv pr) ls

        buildEnv :: [(String, String)] -> IO Env
        buildEnv e = 
            let 
                m = fromList e
                dbName = findWithDefault "" "DBNAME" m
            in do
                putStrLn ("dbname: " ++ dbName)
                db <- openDatabase dbName
                return $ Env db ""

    in do
        _ <- setupLocalEnv
        e <- getEnvironment
        buildEnv e
    
 
main :: IO ()
main = do
    --someFunc
    e <- setUpEnv
    app e
    
