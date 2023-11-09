{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty (scotty, get, html, param, redirect, post, ActionM)
import System.Environment (setEnv, getEnvironment)
import Data.Map (fromList, findWithDefault)
import Database.SQLite.Simple (Connection)
import System.Directory (doesFileExist)
import Lib
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad


data Env = Env { conn :: Connection }


templateH1 :: TL.Text -> TL.Text
templateH1 tx = (TL.pack "<h1>") <> tx <> (TL.pack "</h1>")

app :: Env -> IO ()
app env = 
    scotty 3000 $ do
        get "/" $ html "<h1>Todo Future</h1>"
        post "/:email" $ do
            email <- param "email" :: ActionM TL.Text
            _ <- liftIO $ executeCreateUser (conn env) (TL.unpack email) "test-key"
            html (templateH1 email)
        get "/:email" $ do
            email <- param "email" :: ActionM TL.Text
            g <- liftIO $ executeGetUserByEmail (conn env) (TL.unpack email)
            case g of
                Nothing -> html (templateH1 "Not found")
                Just u -> html (templateH1 $ TL.pack $ show u)


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
                return $ Env db

    in do
        _ <- setupLocalEnv
        e <- getEnvironment
        buildEnv e
    
 
main :: IO ()
main = do
    someFunc
    e <- setUpEnv
    app e
    
