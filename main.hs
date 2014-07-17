{-# LANGUAGE OverloadedStrings #-}
import Types
import Parser
import Sql
import qualified Data.Text.IO as TIO

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
  putStrLn "Migrations:"
  mapM_ (TIO.putStrLn . sqlShow) queries
  putStrLn "=================================================\nSingle query:"
  mapM_ (TIO.putStrLn . sqlShow) $ singleQueryFromMigrations queries
