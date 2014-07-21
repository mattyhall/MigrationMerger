{-# LANGUAGE OverloadedStrings #-}
module Sql where

import Types
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State
import qualified Data.Text as T
import Control.Monad
import Data.Monoid

type Merger a = StateT (M.Map T.Text CreateStatement) (Either T.Text) a

--singleStatementFromMigrations :: [Statement] -> [Statement]
--singleStatementFromMigrations queries = M.foldWithKey (\tblName cStatement acc -> Create tblName cStatement : acc) [] m
--  where m = execState (mapM_ mergeMigration queries) M.empty

singleStatementFromMigrations :: [Statement] -> Either T.Text [Statement]
singleStatementFromMigrations statements = fmap f m
    where m   = execStateT (mapM mergeMigration statements) M.empty
          f m = M.foldWithKey (\tblName cStatement acc -> Create tblName cStatement : acc) [] m

--mergeMigration :: Statement -> State (M.Map T.Text CreateStatement) ()
mergeMigration :: Statement -> Merger ()
mergeMigration (Create name ctbl) = do
    n <- use $ at name
    case n of 
        Just _ -> lift $ Left ("Table '" <> name <> "' already created")
        Nothing -> at name .= Just ctbl  

mergeMigration (Alter name (RenameTable newName)) = do
  tbl <- use $ at name                 
  case tbl of
    Just _  -> do at name .= Nothing
                  at newName .= tbl
    Nothing -> lift $ Left ("Table '" <> name <> "' does not exist so cannot rename. ")

mergeMigration (Alter name (RenameColumn oldCol newCol)) = do
  m <- get
  let col = m ^? at name . _Just . cols . at oldCol . _Just
  case col of
    Just _ -> do at name . _Just . cols . at oldCol .= Nothing
                 at name . _Just . cols . at newCol .= col
    Nothing -> lift $ Left ("Column '" <> oldCol <> "' does not exist in table '" <> name <> "' so cannot rename")

mergeMigration (Alter name (AddColumn colName col)) = do
    m <- get
    let c = m ^? at name . _Just . cols . at colName . _Just
    case c of
        Just _ -> lift $ Left ("Column '" <> colName <> "' already exists in table '" <> name <> "' so cannot add")
        Nothing -> at name . _Just . cols . at colName .= Just col

mergeMigration (Alter name (DropColumn colName)) = do
    m <- get
    let c = m ^? at name . _Just . cols . at colName . _Just
    case c of 
        Just _ -> at name . _Just . cols . at colName .= Nothing
        Nothing -> lift $ Left ("Column '" <> colName <> "' does not exist in table '" <> name <> "' so cannot drop")

mergeMigration (Alter name (ChangeColumnType colName t)) = do
    m <- get
    let c = m ^? at name . _Just . cols . at colName . _Just
    case c of 
        Just _ -> at name . _Just . cols . at colName . _Just . typ .= t
        Nothing -> lift $ Left ("Column '" <> colName <> "' does not exist in table '" <> name <> "' so cannot change type")

rollback :: Statement -> Statement
rollback (Alter name (RenameTable newName)) = Alter newName (RenameTable name)
rollback (Alter name (RenameColumn oldCol newCol)) = Alter name (RenameColumn newCol oldCol)
rollback (Alter name (AddColumn colName _)) = Alter name (DropColumn colName)
-- the following two are impossible to do without knowledge of the current schema
rollback (Alter _ (DropColumn _)) = undefined
rollback (Alter _ (ChangeColumnType _ _)) = undefined
