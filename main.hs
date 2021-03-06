{-# LANGUAGE OverloadedStrings #-}
import Types
import Parser
import Sql
import qualified Data.Text.IO as TIO
import Data.Monoid

main :: IO ()
main = do
  let stmntTexts = [ "CREATE TABLE tbl (col1 INT NOT NULL, col2 CHAR(20))"
--                   CREATE TABLE error
--                   , "CREATE TABLE tbl (col INT);"
                   , "ALTER TABLE tbl RENAME TO atbl"
                   , "ALTER TABLE atbl RENAME col1 TO first_col;"
                   , "ALTER TABLE atbl first_col CHAR(15)"
                   , "ALTER TABLE atbl ADD col3 INT;"
                   , "ALTER TABLE atbl ADD tmp INT;"
                   , "ALTER TABLE atbl DROP tmp;"]
  let stmnts = map parseStatement stmntTexts
  putStrLn "=================================================================="
  putStrLn "Migrations:"
  mapM_ (TIO.putStrLn . sqlShow) stmnts
  putStrLn "=================================================================="
  putStrLn "Single statement"
  print $ fmap (map sqlShow) $ singleStatementFromMigrations stmnts
  putStrLn "=================================================================="
  putStrLn "Rollback example"
  TIO.putStrLn ("Original: " <> sqlShow (stmnts !! 2))
  TIO.putStrLn ("Rollback " <> sqlShow (rollback (stmnts !! 2)))
