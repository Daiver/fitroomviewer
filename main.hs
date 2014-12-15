{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

--import qualified Web.Scotty as S
import qualified Web.Scotty as S
import Network.Wai.Middleware.Static
import Control.Monad
import Data.Monoid

import Database.Persist.Sqlite (SqlPersist, withSqliteConn, runSqlConn, runMigration, runSqlite)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.IO.Class (liftIO)
--import Database.Persist.GenericSql
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Database.Persist.Sql

runDb = runSqlite "devDB.sqlite3"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Settings
    key   String
    value String
    deriving Show

User
    name     String
    password String
    deriving Show

UserScan
    --ownerId       UserId
    pathToModel   String
    pathToTexture String
    deriving Show

Garment
    name          String
    pathToPreview String
    pathToModel   String
    pathToTexture String
    deriving Show

|]

addInitDataIntoDB = runDb $ do
    _ <- insert $ Settings "isInitialized" "True"
    _ <- insert $ UserScan "Colour_Male_03_Pose_01_Decimated2.OBJ" ""
    return ()

initDbIfNotInitialized = do
    li <- runDb $ selectList [SettingsKey ==. "isInitialized"] []
    if null li then do
        print "Initializing"
        addInitDataIntoDB
        return ()
    else do
        print "BD already isInitialized"
        return ()

main = do
    runDb $ runMigration migrateAll
    initDbIfNotInitialized
    S.scotty 3000 $ do
        S.get "/" $ do
            S.file "index.html"
        S.get "/models/garment/:name" $
            S.file . ("data/models/garment/" ++ ) =<< S.param "name"
        S.get "/models/scans/:name" $ do
            S.file . ("data/models/scans/" ++ ) =<< S.param "name"

        S.get "/images/:name" $ do
            S.file . ("data/textures/" ++ ) =<< S.param "name"
            --cap <- S.param "name"
            --S.file ("data/Dress/" ++ cap)
        S.get (S.regex "^/three.js/(.*)$" ) $ do
            cap <- S.param "1"
            S.file ("three.js/" ++ cap)
