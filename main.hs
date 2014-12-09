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
        S.get (S.regex "^/(.*)$" ) $ do
            --path <- S.param "0"
            cap <- S.param "1"
            S.file ("" ++ cap)
            --S.text $ mconcat ["Path: ", path, "\nCapture: ", cap]
