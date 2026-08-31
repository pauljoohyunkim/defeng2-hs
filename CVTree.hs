module CVTree where

data CVType = CVConsonantFormer | CVConsonantLatter | CVVowel deriving (Show, Eq)
data CVTree = Node CVType Int [CVTree] deriving Show

-- Type, Int => Tree
createTree :: CVType -> Int -> CVTree
createTree t 0                     = Node t 0 []
createTree CVConsonantFormer depth = Node CVConsonantFormer depth [createTree CVVowel (depth-1)]
createTree CVConsonantLatter depth = Node CVConsonantLatter depth [createTree CVConsonantFormer (depth-1)]
createTree CVVowel depth           = Node CVVowel depth [createTree CVConsonantFormer (depth-1), createTree CVConsonantLatter (depth-1)]

-- Tree c_former, v, c_latter, prefix => List of words.
visitTree :: CVTree -> [String] -> [String] -> [String] -> String -> [String]
visitTree (Node t depth subtrees) c_former v c_latter prefix
    | t == CVConsonantFormer = 
    | t == CVConsonantLatter = 
    | t == CVVowel = 

