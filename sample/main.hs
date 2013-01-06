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
import SharedTypes
import Import

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/fay-command FaySiteR FaySite getFaySite
|]

instance Yesod App
instance YesodJquery App

-- Important! This declaration must come after mkYesod so that the FaySiteR
-- constructor is in scope.
instance YesodFay App where
    type YesodFayCommand App = Command

    yesodFayCommand render command =
        case command of
            RollDie r -> render r "Four" -- guaranteed to be random, see http://xkcd.com/221/

    fayRoute = FaySiteR

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Fay Sample"
    [whamlet|<button #roll>Roll die|]
    $(fayFile "Home")

main :: IO ()
main = warpDebug 3000 App
