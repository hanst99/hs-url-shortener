{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod

data Foundation = Foundation

mkYesod "Foundation" [parseRoutes|
/ HomeR GET
|]

instance Yesod Foundation

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<h1> Hello World!|]

main :: IO ()
main = warp 3000 Foundation
