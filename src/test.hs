module Main where

import qualified Text.AccentuateUs as AU

main :: IO ()
main = do
    print =<< AU.langs (Just "en") 9
    print =<< AU.accentuate "haw" Nothing "piapa Hawaii"
