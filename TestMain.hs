{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Data.Aeson (object, (.=), Value)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Control.Lens hiding ((.=))
import Data.Aeson.Lens (key)

heroUrl :: Int -> String
heroUrl heroId = "https://superheroapi.com/api/347206d2dac16447fdf234cb9ac6ac76/" ++ show heroId

selectNameAndImage :: Value -> Value
selectNameAndImage hero =
    object
      [ "name"  .= (hero ^? key "name")
      , "image" .= (hero ^? key "image")
      ]

-- Testes para heroUrl
testHeroUrl :: [Test]
testHeroUrl =
  [ TestCase $ assertEqual "heroUrl 1" "https://superheroapi.com/api/347206d2dac16447fdf234cb9ac6ac76/1" (heroUrl 1),
    TestCase $ assertEqual "heroUrl 731" "https://superheroapi.com/api/347206d2dac16447fdf234cb9ac6ac76/731" (heroUrl 731)
  ]

-- Testes para selectNameAndImage
testSelectNameAndImage :: [Test]
testSelectNameAndImage =
  [ TestCase $
      assertEqual "selectNameAndImage pega name e image"
        (object [ "name" .= ("Batman" :: T.Text)
                , "image" .= ("batman.png" :: T.Text)
                ])
        (selectNameAndImage (object [ "name" .= ("Batman" :: T.Text)
                                    , "image" .= ("batman.png" :: T.Text)
                                    , "biography" .= ("some bio" :: T.Text)
                                    ])),

    TestCase $
      assertEqual "selectNameAndImage funciona mesmo sem campos extras"
        (object [ "name" .= (Nothing :: Maybe T.Text)
                , "image" .= (Nothing :: Maybe T.Text)
                ])
        (selectNameAndImage (object []))
  ]

main :: IO ()
main = do
  runTestTT (TestList (testHeroUrl ++ testSelectNameAndImage))
  return ()