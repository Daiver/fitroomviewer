{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--import qualified Web.Scotty as S
import qualified Web.Scotty as S
import Network.Wai.Middleware.Static
import Control.Monad
import Data.Monoid
--
--import Web.Scotty.Hastache
--
--

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
