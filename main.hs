{-# LANGUAGE OverloadedStrings #-}
import Types
import Parser
import Sql
import qualified Data.Text.IO as TIO
import Data.Monoid

main :: IO ()
main = do
  let queryTexts = [ "CREATE TABLE tbl (col1 INT, col2 CHAR(20));"
                   , "ALTER TABLE tbl RENAME TO atbl"
                   , "ALTER TABLE atbl RENAME col1 TO first_col;"
                   , "ALTER TABLE atbl first_col TYPE CHAR(15)"
                   , "ALTER TABLE atbl ADD col3 INT;"
                   , "ALTER TABLE atbl ADD tmp INT;"
                   , "ALTER TABLE atbl DROP tmp;"]
  let queries = map parseQuery queryTexts
  putStrLn "=================================================================="
  putStrLn "Migrations:"
  mapM_ (TIO.putStrLn . sqlShow) queries
  putStrLn "=================================================================="
  putStrLn "Single query"
  mapM_ (TIO.putStrLn . sqlShow) $ singleQueryFromMigrations queries
  putStrLn "=================================================================="
  putStrLn "Rollback example"
  TIO.putStrLn ("Original: " <> sqlShow (queries !! 2))
  TIO.putStrLn ("Rollback " <> sqlShow (rollback (queries !! 2)))
