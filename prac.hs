{- 
 @Haskell
-}

import Data.List
import System.IO

{- Functions -}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

isOdd :: Int -> Bool
isOdd n = not (n `mod` 2 == 0)

whatGrade :: Int -> String
whatGrade age
  | (age >= 5) && (age <= 6) = "Kindergarten"
  | (age > 6) && (age <= 10) = "Elementary school"
  | (age > 10) && (age <= 14) = "Middle School"
  | (age > 14) && (age <= 17) = "High School"
  | otherwise = "Go to college"

getListItems :: [Int] -> String
getListItems [] = "Your List is empty"
getListItems (x: []) = "Your list starts with " ++ show x
getListItems (x: y: []) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x: xs) = "Your list contains " ++ show x ++ " and " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The First letter in " ++ all ++ " is " ++ [x]

timesFour :: Int -> Int
timesFour x = x * 4 

listTimesFour :: [Int] -> [Int]
listTimesFour [] = []
listTimesFour (x:xs) = timesFour x : listTimesFour xs

areStringsEqual :: [Char] -> [Char] -> Bool
areStringsEqual [] [] = True
areStringsEqual (x:xs) (y:ys) = x == y && areStringsEqual (xs) (ys)
areStringsEqual _ _ = False

doMultiply :: (Int -> Int) -> Int -> Int
doMultiply myFun x = myFun x

getAddFun :: Int -> (Int -> Int)
getAddFun x y = x + y
addsThree = getAddFun 3
threePlusList = map addsThree [1, 2, 3, 4, 5]

doubleList = map (\x -> x * 2) [1 .. 10]

doubleEvenNumber y =
  if ((y `mod` 2) /= 0)
    then y
  else y * 2

getClass :: Int -> String

getClass n = case n of
  5 -> "Go to Kindergarten"
  6 -> "Go to Elementary School"
  _ -> "Go Away"


data BaseballPlayer = Pitcher
  | Catcher
  | Infielder
  | Outfield
  deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True
barryInOf = print (barryBonds Outfield)

data Customer = Customer String String Double
  deriving Show
  
tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b

data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock ="Paper beats Rock"
shoot Rock Scissors ="Rock beats Scissors"
shoot Scissors Paper ="Scissors beats Paper"
shoot Scissors Rock ="Scissors looses to Rock"
shoot Paper Scissors ="Paper looses to Scissors"
shoot Rock Paper ="Rock looses to Paper"
shoot _ _ = "Error"

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y a b) = (abs $ a - x) * (abs $ y - b)

areaOfCircle = area $ Circle 50 60 20
areaOfRectangle = area $ Rectangle 10 10 100 100
sumAreaValue = putStrLn . show $ areaOfCircle + areaOfRectangle

data Employee = Employee {
  name :: String,
  position :: String,
  idNum :: Int
} deriving (Eq, Show)

samSmith = Employee {
  name = "Sam Smith",
  position = "Manager",
  idNum = 1000
}

pamMarx = Employee {
  name = "Pam Marx",
  position = "Sales",
  idNum = 1001
}

isSamPam = samSmith == pamMarx

samSmithData = show samSmith
pamMarxData = show pamMarx

class MyEq a where
  areEqual :: a -> a -> Bool

data ShirtSize = S | M | L

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False

newSize = areEqual M M

sayHello = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"

writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("Random Line of Text")
  hClose theFile

readFromFile = do
  theFile <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile
  putStr contents
  hClose theFile

{- 
  Fib Series?
  This below is enough. No more code needed!
-}

fib = 0 : 1 : [a + b | (a, b) <- zip fib (tail fib)]
