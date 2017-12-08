module Day08
  ( day08
  ) where

import Data.Functor (($>))
import qualified Data.Map as M
import Data.Map (Map)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

newtype Reg =
  Reg String
  deriving (Eq, Ord, Show)

data RegOp
  = Inc
  | Dec
  deriving (Show)

newtype Val =
  Val Int
  deriving (Show)

data Cmp
  = Lt
  | Gt
  | Lte
  | Gte
  | Equ
  | NEqu
  deriving (Show)

data Cond =
  Cond Reg
       Cmp
       Val
  deriving (Show)

data Instr =
  Instr Reg
        RegOp
        Val
        Cond
  deriving (Show)

runAll :: [Instr] -> [Map Reg Int]
runAll = scanl eval M.empty

eval :: Map Reg Int -> Instr -> Map Reg Int
eval regs (Instr reg regop val cond)
  | checkCond regs cond = M.insert reg (newVal (regVal regs reg) regop val) regs
  where
    newVal i Inc (Val j) = i + j
    newVal i Dec (Val j) = i - j
eval regs _ = regs

regVal :: Map Reg Int -> Reg -> Int
regVal regs reg = M.findWithDefault 0 reg regs

checkCond :: Map Reg Int -> Cond -> Bool
checkCond regs (Cond reg cmp (Val i)) = regVal regs reg `cmpOp` i
  where
    cmpOp =
      case cmp of
        Lt -> (<)
        Gt -> (>)
        Gte -> (>=)
        Lte -> (<=)
        Equ -> (==)
        NEqu -> (/=)

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser [Instr]
parser = many1 (instr <* newline)
  where
    instr = Instr <$> reg <*> (spc *> regOp) <*> (spc *> val) <*> (spc *> cond)
    reg = Reg <$> many1 lower
    regOp = (string "inc" $> Inc) <|> (string "dec" $> Dec)
    val = Val <$> number
    cond =
      Cond <$> (string "if" *> spc *> reg) <*> (spc *> cmp) <*> (spc *> val)
    cmp =
      try (string "<=" $> Lte) <|> try (string ">=" $> Gte) <|>
      (string "<" $> Lt) <|>
      (string ">" $> Gt) <|>
      (string "==" $> Equ) <|>
      (string "!=" $> NEqu)
    spc = char ' '
    number = do
      sign <- option '+' (char '-')
      digits <- many1 digit
      return $
        (if sign == '-'
           then -1
           else 1) *
        read digits

biggestValue :: Map Reg Int -> Int
biggestValue = maximum . (0 :) . M.elems

day08 :: IO ()
day08 =
  withInput "input/8.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ biggestValue $ last $ runAll parsed
    putStrLn "Part 2"
    print $ maximum (biggestValue <$> runAll parsed)
