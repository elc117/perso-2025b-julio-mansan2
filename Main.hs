{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Network.Wreq as W
import Control.Lens hiding ((.=))
import Data.Aeson.Lens (key, _String, _Array, values)
import Data.Aeson (object, (.=), Value)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import System.Random (randomRIO)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)

getHeroName body = body ^? key "name"
getBiography body = body ^? key "biography"
getWork body = body ^? key "work"
getConnections body = body ^? key "connections"
getImage body = body ^? key "image"

randomHeroId :: IO Int
randomHeroId = randomRIO (1, 731)

heroUrl :: Int -> String
heroUrl heroId =
  "https://superheroapi.com/api/347206d2dac16447fdf234cb9ac6ac76/"
    ++ show heroId

fetchHero :: IO Value
fetchHero = do
    heroId <- randomHeroId
    r <- W.get (heroUrl heroId)
    let body = r ^. W.responseBody
    return $ object
        [ "name"        .= getHeroName body
        , "biography"   .= getBiography body
        , "work"        .= getWork body
        , "connections" .= getConnections body
        , "image"       .= getImage body
        ]

selectNameAndImage :: Value -> Value
selectNameAndImage hero =
    object
      [ "name"  .= (hero ^? key "name")
      , "image" .= (hero ^? key "image")
      ]

fetchFiveHeroesFiltered :: IO [Value]
fetchFiveHeroesFiltered = do
    heros <- replicateM 5 fetchHero
    return $ map selectNameAndImage heros

main :: IO ()
main = scotty 3000 $ do
    -- Página inicial
    get "/" $ file "static/index.html"
    
    -- Arquivos estáticos
    get "/static/script.js" $ do
        setHeader "Content-Type" "application/javascript"
        file "static/script.js"

    get "/static/styles.css" $ do
        setHeader "Content-Type" "text/css"
        file "static/styles.css"
    
    -- Rota para pegar herói aleatório
    get "/hero" $ do
        heroObj <- liftIO fetchHero
        json heroObj

    get "/quiz" $ do
        heroObj <- liftIO fetchFiveHeroesFiltered
        json heroObj

    -- Rota para o quiz
    get "/static/quiz.html" $ do
        file "static/quiz.html"