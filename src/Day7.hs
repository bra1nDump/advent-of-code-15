module Day7 where

import Data.Word
import Data.Bits
import qualified Data.Map.Strict as Map

import Debug.Trace

type WireId = String
type Fun = Word16 -> Word16
type Fun2 = Word16 -> Word16 -> Word16

data Value =
  Wire WireId
  | Literal Word16

data Instruction =
  Bind Value
  | UnOp Value Fun
  | BinOp Value Value Fun2

parseInstruction :: String -> (WireId, Instruction)
parseInstruction instruction = 
  case words instruction of 
    [value, _, wire] -> (wire, Bind (parseValue value))
    ["NOT", arg, _, wire] -> (wire, UnOp (parseValue arg) complement)
    [arg1, binOp, arg2, _, wire] -> 
      (wire, BinOp (parseValue arg1) (parseValue arg2) (parseBinOp binOp))
 
  where 
    parseValue str =
      if all (\c -> c `elem` ['0'..'9']) str 
      then Literal (read str)
      else Wire str
    parseBinOp :: String -> (Word16 -> Word16 -> Word16)
    parseBinOp str =
      case str of
        "AND" -> (.&.)
        "OR" -> (.|.)
        "LSHIFT" -> (\arg bits -> shiftL arg (fromIntegral bits))
        "RSHIFT" -> (\arg bits -> shiftR arg (fromIntegral bits))

type Instructions = Map.Map WireId Instruction
type MemoizedExp = Map.Map WireId Word16

parseInstructions :: String -> Instructions
parseInstructions manual =
  Map.fromList . map parseInstruction . lines $ manual

resolve :: Instructions -> MemoizedExp -> Value -> (Word16, MemoizedExp)
resolve instructions memoized value =
  case value of
    Literal value -> (value, memoized)
    Wire wire ->
      case memoized Map.!? wire of 
        Just exp -> (exp, memoized)
        Nothing ->
          let 
            (newExp, newMem) =
              case instructions Map.! wire of
                Bind value' -> resolve' memoized value'
                UnOp value' f -> 
                  let (exp, mem) = (resolve' memoized value') 
                  in (f exp, mem)
                BinOp value1 value2 f -> 
                  let 
                    (exp1, memoized1) = resolve' memoized value1
                    (exp2, memoized2) = resolve' memoized1 value2
                  in 
                    (f exp1 exp2, memoized2)
          in 
            (newExp, Map.insert wire newExp newMem)
  where resolve' = resolve instructions

p1 :: String -> String
p1 input =
  let 
    instructions = parseInstructions input 
    (expression, _) = resolve instructions Map.empty (Wire "a")
  in
  show expression

p2 :: String -> String
p2 input =
  let 
    instructions = parseInstructions input 
    (expression, _) = resolve instructions Map.empty (Wire "a")
    newInstructions = Map.insert "b" (Bind (Literal expression)) instructions
    (expressionNew, _) = resolve newInstructions Map.empty (Wire "a")
  in
  show expressionNew
