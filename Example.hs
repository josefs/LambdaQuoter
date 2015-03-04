{-# LANGUAGE QuasiQuotes #-}
module Example where

import LambdaQuoter

plus = (++)

main = putStrLn ([lambda| \ x y -> $plus x y |] ("hello ",("world",())))
