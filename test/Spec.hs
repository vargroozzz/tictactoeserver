{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Server                         ( app )
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (return app)
    $                   describe "GET /game"
    $                   it "responds with 200"
    $                   get "/users"
    `shouldRespondWith` 200 it "responds with TurnRes"
    $                   do
                          let
                            table
                              = "{\"sideWon\":\"Nothing\",\"gridRes\":\"[\"O\",\"X\",\" \",\" \",\" \",\" \",\" \",\" \",\" \"]}"
                          get "/game" `shouldRespondWith` table
