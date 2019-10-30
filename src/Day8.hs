module Day8 where

import Common

p1 :: String -> String
p1 input = (code - memory) |> show
  where 
    code = sum . map length . lines $ input
    memory = sum . map count . lines $ input

    count "" = 0
    count str =
      1 +
      case str of
        '\\':'x':_:_:rest -> count rest -- \xAB
        '\\':_:rest -> count rest
        _:rest -> count rest

p2 :: String -> String
p2 = const "TODO"
