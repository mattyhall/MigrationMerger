module Sql where

import Types
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State
import qualified Data.Text as T

singleQueryFromMigrations :: [Query] -> [Query]
singleQueryFromMigrations queries = M.foldWithKey (\tblName cQuery acc -> Create tblName cQuery : acc) [] m
  where m = execState (mapM_ mergeMigration queries) M.empty

mergeMigration :: Query -> State (M.Map T.Text CreateQuery) ()
mergeMigration (Create name ctbl) = at name .= Just ctbl 

mergeMigration (Alter name (RenameTable newName)) = do
  tbl <- use $ at name                 
  at name .= Nothing
  at newName .= tbl

mergeMigration (Alter name (RenameColumn oldCol newCol)) = do
  m <- get
  let col = m ^? at name . _Just . cols . at oldCol . _Just
  at name . _Just . cols . at oldCol .= Nothing
  at name . _Just . cols . at newCol .= col

mergeMigration (Alter name (AddColumn colName col)) = at name . _Just . cols . at colName .= Just col

mergeMigration (Alter name (DropColumn colName)) = at name . _Just . cols . at colName .= Nothing

mergeMigration (Alter name (ChangeColumnType colName t)) = at name . _Just . cols . at colName . _Just . typ .= t

rollback :: Query -> Query
rollback (Alter name (RenameTable newName)) = Alter newName (RenameTable name)
rollback (Alter name (RenameColumn oldCol newCol)) = Alter name (RenameColumn newCol oldCol)
rollback (Alter name (AddColumn colName _)) = Alter name (DropColumn colName)
-- the following two are impossible to do without knowledge of the current schema
rollback (Alter _ (DropColumn _)) = undefined
rollback (Alter _ (ChangeColumnType _ _)) = undefined
