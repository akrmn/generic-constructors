{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main
  ( main
  ) where

import GHC.Generics
import Data.Tagged

import Constructors (constructors)
import qualified Constructors.Tagged as Tagged (constructors)

data Either4 a b c d
  = E4_1 a
  | E4_2 b
  | E4_3 c
  | E4_4 d
  deriving (Generic, Show)

listConstructors :: forall a x. ([a] -> (a -> [a] -> [a]) -> x) -> x
listConstructors = constructors @[a]

main :: IO ()
main = do
  constructors @(Int, Bool) \tup ->
    print [tup 3 False]

  constructors @[Int] \nil cons ->
    print [nil, cons 3 (cons 2 (cons 1 nil))]

  constructors @(Either Int Bool) \left right ->
    print [left 3, right False]

  constructors @(Maybe Int) \nothing just ->
    print [nothing, just 3]

  constructors @(Either4 Int Bool Char Double) \e1 e2 e3 e4 ->
    print [e1 3, e2 False, e3 'a', e4 3.14]

  Tagged.constructors @(Int, Bool)
    \(untag @"(,)" -> tup) ->
      print [tup 3 False]

  Tagged.constructors @[Int]
    \(untag @"[]" -> nil) ->
    \(untag @":" -> cons) ->
      print [nil, cons 3 (cons 2 (cons 1 nil))]

  Tagged.constructors @(Either Int Bool)
    \(untag @"Left" -> left) ->
    \(untag @"Right" -> right) ->
      print [left 3, right False]

  Tagged.constructors @(Maybe Int)
    \(untag @"Nothing" -> nothing) ->
    \(untag @"Just" -> just) ->
      print [nothing, just 3]

  Tagged.constructors @(Either4 Int Bool Char Double)
    \(untag @"E4_1" -> e1) ->
    \(untag @"E4_2" -> e2) ->
    \(untag @"E4_3" -> e3) ->
    \(untag @"E4_4" -> e4) ->
      print [e1 3, e2 False, e3 'a', e4 3.14]
