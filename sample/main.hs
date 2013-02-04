-- To run, try the following:
--
-- a) using fay < 0.10
-- > runghc -D"MIN_VERSION_fay(a,b,c)=0" -i. -ifay-shared -i.. main.hs
-- b) using fay >= 0.10
-- > runghc -D"MIN_VERSION_fay(a,b,c)=1" -i. -ifay-shared -i.. main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Main where

import Yesod
import Yesod.Fay
import Yesod.Static
import SharedTypes
import Import
import Language.Fay.Convert (readFromFay)
import Language.Haskell.TH

data App = App
    { getStatic :: Static
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/fay-command FaySiteR FaySite getFaySite
/static StaticR Static getStatic
|]

instance Yesod App
instance YesodJquery App

-- Important! This declaration must come after mkYesod so that the FaySiteR
-- constructor is in scope.
instance YesodFay App where
    yesodFayCommand render command =
        case readFromFay command of
            Just (RollDie r) -> render r "Four" -- guaranteed to be random, see http://xkcd.com/221/
            Nothing -> invalidArgs ["Invalid command"]

    fayRoute = FaySiteR

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Fay Sample"
    [whamlet|<button #roll>Roll die|]
    $(fayFile' (ConE 'StaticR) "Home")

main :: IO ()
main = static "static" >>= warpDebug 3001 . App
