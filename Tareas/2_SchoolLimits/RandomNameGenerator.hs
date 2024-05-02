module RandomNamePick where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text


type StudentName = String

data Student = Student{
    lastName::String,
    name::StudentName
    } deriving (Show, Eq,Ord)

data SchoolHouse = Griffindor
                    | Hufflepuff
                    | Ravenclaw 
                    | Slytherin 
                    deriving (Eq, Show, Ord, Read)


assignSchoolHouse :: [SchoolHouse] -> IO SchoolHouse
assignSchoolHouse xs = do
     house <- pickRandom xs
     return house


pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1) 
    return (xs !! idx) 


split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y)
             where (x,y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student{lastName=lastName, name=name}

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = map parseLine (lines contents)
    return students

deleteSchoolHouse :: SchoolHouse -> [SchoolHouse] -> [SchoolHouse] -> [SchoolHouse]
deleteSchoolHouse _ [] ys = ys
deleteSchoolHouse a (x:xs) ys = if a == x then  ys ++ xs else deleteSchoolHouse a xs (x:ys)

getList :: SchoolHouse -> [SchoolHouse] -> [SchoolHouse]
getList _ [x] = [Griffindor, Hufflepuff, Slytherin, Ravenclaw]
getList a xs = deleteSchoolHouse a xs [] 

initialSchoolList :: [SchoolHouse]
initialSchoolList = [Griffindor, Hufflepuff, Slytherin, Ravenclaw]

processStudentsList :: [Student] -> Bool -> [SchoolHouse] -> IO ()
processStudentsList [] _ _ = return ()
processStudentsList (x:xs) True _ = do 
                    schoolHouse <- assignSchoolHouse initialSchoolList
                    putStrLn $ show (x, schoolHouse)
                    processStudentsList xs False (getList schoolHouse initialSchoolList)
processStudentsList (x:xs) _ ys = do
                    schoolHouse <- assignSchoolHouse ys
                    putStrLn $ show (x, schoolHouse)
                    processStudentsList xs False (getList schoolHouse ys)

main :: IO()
main = do
    records <- readFileToList "/home/diego/VisualStudioCodeProjects/Lenguajes_De_Pogramacion/Tareas/1_GeneradorDeGrupos/list.txt"
    processStudentsList records True []