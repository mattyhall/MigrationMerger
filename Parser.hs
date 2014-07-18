{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Control.Applicative
import Types
import qualified Data.Text as T
import qualified Data.Map as M

parseName :: Parser T.Text
parseName = takeWhile1 (`notElem` "; ")

parseTypeChar :: Parser Type
parseTypeChar = do
  asciiCI "CHAR"
  skipSpace
  char '('
  skipSpace
  n <- many1 digit
  skipSpace
  char ')'
  return $ CChar (read n)

parseType :: Parser Type
parseType = (asciiCI "INT" *> pure CInt) <|> (asciiCI "DATE" *> pure CDate) <|>
            (asciiCI "REAL" *> pure CReal) <|> parseTypeChar

parseConstraint :: Parser Constraint
parseConstraint = (asciiCI "NOT" *> skipSpace *> asciiCI "NULL" *> pure NotNull) <|>
                  (asciiCI "PRIMARY" *> skipSpace *> "KEY" *> pure PrimaryKey)

parseConstraints :: Parser [Constraint]
parseConstraints = many1 (parseConstraint <* skipSpace)

parseColumn :: Parser (T.Text, Column)
parseColumn = do
  name <- parseName
  skipSpace
  t <- parseType
  skipSpace
  constraints <- parseConstraints <|> pure []
  return (name, Column t constraints)

parseCreate :: Parser Statement
parseCreate = do
  asciiCI "CREATE TABLE"
  skipSpace
  name <- parseName
  skipSpace
  char '('
  cols <- sepBy (skipSpace *> parseColumn <* skipSpace) (char ',') 
  skipSpace
  char ')'
  skipSpace
  try $ char ';'
  return $ Create name (CreateTable (M.fromList cols))

parseRenameTable :: Parser AlterStatement
parseRenameTable = do
  asciiCI "RENAME TO"
  skipSpace
  name <-  parseName
  return $ RenameTable name

parseRenameColumn :: Parser AlterStatement
parseRenameColumn = do
  asciiCI "RENAME"
  skipSpace
  oldName <- parseName
  skipSpace
  asciiCI "TO"
  skipSpace
  newName <- parseName
  return $ RenameColumn oldName newName

parseAddColumn :: Parser AlterStatement
parseAddColumn = do
  asciiCI "ADD"
  skipSpace
  (name, column) <- parseColumn
  return $ AddColumn name column

parseDropColumn :: Parser AlterStatement
parseDropColumn = do
  asciiCI "DROP"
  skipSpace
  name <- parseName
  return $ DropColumn name

parseChangeColumnType :: Parser AlterStatement
parseChangeColumnType = do
  name <- parseName
  skipSpace
  asciiCI "TYPE"
  skipSpace
  typ <- parseType
  return $ ChangeColumnType name typ

parseAlter :: Parser Statement
parseAlter =  do
  asciiCI "ALTER TABLE"
  skipSpace
  name <- parseName
  skipSpace
  alter <- parseRenameTable <|> parseRenameColumn <|> parseAddColumn <|> parseDropColumn <|> parseChangeColumnType
  skipSpace
  char ';' <|> pure ' '
  return $ Alter name alter

parseStatement :: T.Text -> Statement
parseStatement xs = q
  where (Right q) = parseOnly (parseCreate <|> parseAlter) xs
