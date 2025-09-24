{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Network.Wreq as W
import Control.Lens hiding ((.=))
import Data.Aeson.Lens (key, _String, _Array, values)
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import System.Random (randomRIO)
import Data.IORef
import Control.Monad.IO.Class (liftIO)

getHeroName body = body ^? key "name"
getBiography body = body ^? key "biography"
getWork body = body ^? key "work"
getConnections body = body ^? key "connections"
getImage body = body ^? key "image"

main :: IO ()
main = scotty 3000 $ do
    -- Serve o arquivo HTML estático
    get "/" $ do
        file "static/index.html"
    
    get "/static/script.js" $ do
        setHeader "Content-Type" "application/javascript"
        file "static/script.js"

    get "/static/styles.css" $ do
        setHeader "Content-Type" "text/css"
        file "static/styles.css"
    
    get "/hero" $ do
        randomId <- liftIO $ randomRIO (1, 731)
        let url = "https://superheroapi.com/api/347206d2dac16447fdf234cb9ac6ac76/" ++ show randomId
        r <- liftIO $ W.get url

        let body = r ^. W.responseBody
        
            heroObj = object
                [ "name"        .= getHeroName body
                , "biography"   .= getBiography body
                , "work"        .= getWork body
                , "connections" .= getConnections body
                , "image"       .= getImage body
                ]

        json heroObj