module Lib where

import Database.SQLite.Simple (Query (Query), Connection, open, execute_, execute, query)
import Debug.Trace (trace)
import qualified Data.Text as T

data User = User { email :: String , key :: String } deriving (Show, Eq)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

initQuery :: Query
initQuery = Query (T.pack "CREATE TABLE IF NOT EXISTS users (email TEXT NOT NULL, key TEXT NOT NULL);")

getAllUsers :: Query
getAllUsers = Query (T.pack "SELECT email, key FROM users;")

createUserQuery :: Query
createUserQuery = Query (T.pack "INSERT INTO users (email, key) VALUES (?, ?);") 

getUserByEmailQuery :: Query
getUserByEmailQuery = Query (T.pack "SELECT email, key FROM users where email = ?")

executeCreateUser :: Connection -> String -> String -> IO ()
executeCreateUser db e k = do
    execute db createUserQuery (e, k)

executeGetUserByEmail :: Connection -> String -> IO (Maybe User)
executeGetUserByEmail db e = do
    r <- query db getUserByEmailQuery [e] :: IO [[String]]
    let s = safeHead r
    return $ fmap (\s' -> (User (s'!!0) (s'!!1))) s


openDatabase :: String -> IO Connection
openDatabase dbName = do
    db <- trace ("opening sqlite db: " ++ dbName) (open dbName)
    execute_ db initQuery
    return db
