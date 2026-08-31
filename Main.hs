module Main where
import System.IO
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure, die)
import CVTree

main :: IO ()
main = do
    args <- getArgs

    when (length args /= 3) $
        do 
            putStrLn "Need c_former.txt, v.txt, and c_latter.txt as arguments"
            exitFailure

    -- Read arguments
    let [c_former_filename, v_filename, c_latter_filename] = args

    -- Content parser
    let content_to_list content = filter (not . null) $ lines content

    -- C former
    content <- readFile c_former_filename
    let c_former_list = content_to_list content

    -- V
    content <- readFile v_filename
    let v_list = content_to_list content

    -- C latter
    content <- readFile c_latter_filename
    let c_latter_list = content_to_list content

    let min_len = 2
    let max_len = 4

    let depths = [min_len-1..max_len-1]
    
    let trees = map (createTree CVConsonantFormer) depths ++
                map (createTree CVVowel) depths

    print trees

    return ()
