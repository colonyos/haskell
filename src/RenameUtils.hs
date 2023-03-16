module RenameUtils where

processFieldRename :: String -> String
processFieldRename "input" = "in"
processFieldRename "output" = "out"
processFieldRename name = name
