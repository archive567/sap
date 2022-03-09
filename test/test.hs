{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Test.DocTest
import Prelude

main :: IO ()
main =
  doctest
    [ "src/Lib.hs",
      "app/app.hs"
    ]
