-- To run, try the following:
--
-- > runghc -i. -ifay-shared -i.. main.hs
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

instance Yesod App
instance YesodJquery App
instance YesodFay App where
    type YesodFayCommand App = Command

mkYesod "App" [parseRoutes|
/ HomeR GET
/fay-command FayCommandR POST
|]

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Fay Sample"
    [whamlet|<button #roll>Roll die|]
    $(fayFile "Home")

postFayCommandR :: Handler RepJson
postFayCommandR = runCommandHandler $ \render command ->
    case command of
        RollDie r -> render r 4 -- guaranteed to be random, see http://xkcd.com/221/

main :: IO ()
main = warpDebug 3000 App
