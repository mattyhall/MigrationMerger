{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Types where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Data.List

class SqlShow a where
  sqlShow :: a -> T.Text

-- Column types. When have time get all from http://www.postgresql.org/docs/8.4/static/datatype.html
data Type = CInt
          | CChar Int
          | CDate
          | CReal
          deriving (Show, Eq)

instance SqlShow Type where
  sqlShow (CInt)    = "INT" 
  sqlShow (CChar n) = "CHAR(" <> T.pack (show n) <> ")" 
  sqlShow (CDate)   = "DATE"
  sqlShow (CReal)   = "REAL" 

-- Grab from http://www.postgresql.org/docs/9.1/static/sql-createtable.html
data Constraint = NotNull
                | PrimaryKey
                deriving (Show, Eq)

instance SqlShow Constraint where
  sqlShow (NotNull)    = "NOT NULL"
  sqlShow (PrimaryKey) = "PRIMARY KEY"

data Column = Column { _typ :: Type
                     , _constraints :: [Constraint]
                     } deriving (Show, Eq)

instance SqlShow Column where
  sqlShow (Column t cs) = sqlShow t <> text
    where csText = T.intercalate " " $ map sqlShow cs 
          text   = if T.null csText
                      then ""
                      else " " <> csText     

makeLenses ''Column

data Query = Create T.Text CreateQuery
           | Alter T.Text AlterQuery
           deriving (Show, Eq)

instance SqlShow Query where
  sqlShow (Create name q) = "CREATE TABLE " <> name <> " (" <> sqlShow q <> ")" <> ";"
  sqlShow (Alter name q)  = "ALTER TABLE " <> name <> " " <> sqlShow q <> ";"

data AlterQuery = RenameTable T.Text
                | RenameColumn T.Text T.Text
                | AddColumn T.Text Column
                | DropColumn T.Text
                | ChangeColumnType T.Text Type
                deriving (Show, Eq)

instance SqlShow AlterQuery where
  sqlShow (RenameTable name) = "RENAME TO " <> name
  sqlShow (RenameColumn old new) = "RENAME " <> old <> " TO " <> new
  sqlShow (AddColumn name col) = "ADD " <> name <> " " <> sqlShow col
  sqlShow (DropColumn name) = "DROP " <> name
  sqlShow (ChangeColumnType name typ) = name <> " TYPE " <> sqlShow typ 

data CreateQuery = CreateTable { _cols :: M.Map T.Text Column
                               } deriving (Show, Eq)

instance SqlShow CreateQuery where
  sqlShow (CreateTable cols) = T.intercalate ", " colsText
    where colsText = M.foldWithKey (\k val acc -> acc ++ [k <> " " <> sqlShow val]) [] cols

makeLenses ''CreateQuery
