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
--
--

--
--import Web.Scotty.Hastache
--
--

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main = do
    S.scotty 3000 $ do
        S.get "/" $ do
            S.text "MAIN!"
        S.get (S.regex "^/models/(.*)$" ) $ do
            cap <- S.param "1"
            S.file ("data/Dress/" ++ cap)
        S.get (S.regex "^/images/(.*)$" ) $ do
            cap <- S.param "1"
            S.file ("data/Dress/" ++ cap)
        S.get (S.regex "^/(.*)$" ) $ do
            cap <- S.param "1"
            S.file ("" ++ cap)
