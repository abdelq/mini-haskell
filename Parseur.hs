---------------------------------------------------------------------------
-- Parseur de Sexpression
-- Vous n'avez pas à modifier ce fichier
---------------------------------------------------------------------------

module Parseur (module Parseur) where

import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Control.Monad


type Symbol = String
data Sexp = SList [Sexp]
          | SSym Symbol
          | SNum Int
          deriving (Eq)

instance Show Sexp where
  show (SSym s) = s
  show (SNum n) = show n
  show (SList xs) =
    let showTail [] = ")"
        showTail [x] = show x ++ ")"
        showTail (y : ys) = show y ++ " " ++ showTail ys
    in "(" ++ showTail xs

pManySexp :: Parser [Sexp]
pManySexp = many1 pSexp <* eof

pOneSexp :: Parser Sexp
pOneSexp = pSexp <* eof

pSexp :: Parser Sexp
pSexp = whiteSpace *>
        (pAtom <|>
         (do _ <- char '('
             whiteSpace
             sexps <- many (pSexp <* whiteSpace)
             _ <- char ')'
             return $ SList sexps))
        <* whiteSpace

pComment :: Parser ()
pComment = do
  _ <- try $ string "--"
  _ <- manyTill anyChar (Control.Monad.void endOfLine <|> eof)
  return ()

whiteSpace :: Parser ()
whiteSpace = Control.Monad.void (many (Control.Monad.void space <|> pComment))

pAtom :: Parser Sexp
pAtom = do
  s <- many1 pSymchar
  return (case parse integer "" s of
             Right n -> SNum n
             _ -> SSym s)

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (`elem` "!@$%^&*_+-=:|/?<>")

integer :: Parser Int
integer = (do _ <- try $ char '-'
              n <- natural
              return (- n))
          <|> natural

natural :: Parser Int
natural = do
  digits <- many1 digit
  let i = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
  return i
