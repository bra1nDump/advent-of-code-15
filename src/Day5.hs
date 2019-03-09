{-# LANGUAGE UnicodeSyntax, LambdaCase, BlockArguments #-}

module Day5 (day5) where

import Data.List as L
import Control.Arrow
import Control.Applicative

allFalse = not . L.or

day5 :: String -> (String, String)
day5 input =
  (p p1 input, p p2 input)

p predicates =
  show . L.length . L.filter isNice . lines
  where isNice str = L.and (pure str <**> predicates)

sameCountPredicate p =
  L.group >>> L.map L.length
  >>> L.filter p

p1 =
  [ has3In "aeiou"
  , repeatedTwice
  , doesNotContain ["ab", "cd", "pq", "xy"]
  ]
  where
    has3In set = (>= 3) . L.length
                 . L.filter (`L.elem` set)
    repeatedTwice = sameCountPredicate (>= 2)
                    >>> not . L.null
    doesNotContain set str = allFalse . L.map (flip L.isInfixOf $ str) $ set

p2 =
  [ hasRepeatingPair
  , hasXaxPattern
  ]
  where
    hasRepeatingPair xs = fst .
                      L.foldr
                      (const
                        \case
                          (True, _) -> (True, [])
                          (False, x1:x2:rest) ->
                            (L.isInfixOf [x1,x2] rest, x2:rest)
                          _ -> (False, [])
                      )
                      (False, xs)
                      $ xs
    hasXaxPattern xs = fst .
                          L.foldr
                          (const
                            \case
                             (True, _) -> (True, [])
                             (False, x1:a:x2:rest) ->
                               (x1 == x2, a:x2:rest)
                             _ -> (False, [])
                          )
                          (False, xs)
                          $ xs
      
      



{--
It is kind of problematic to debug computations that are executed
as part of some operator, in this case it is (<**>)

## regarding p1 function
The way I solved it was set the breakpoint to the expression
`(pure str <**> predicates)`

> :b Day5 24 19
> day5 "ugknbfddgicrmopn"
> :show bindings
> :force _result

So this wasn't too bad. But what about debugging intermediate results
f >>> g or f . g ??.
This sure sounds auful

My solution would be:
1. set a breakpoint on the entire expression `f . g $ x`
2. force `x` to become bound
3. manually run `let gx = g x`
4. manually run `let fgx = f gx`

Its not that bad

Just went trough the regular stages of freaking out lal
--}
