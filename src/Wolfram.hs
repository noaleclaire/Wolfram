module Wolfram where

import Text.Read
import Control.Monad

errorArgs :: Maybe Config -> String
errorArgs Nothing = "invalid options | ex: (the ruleset is mandatory)"
errorArgs _ = " "

type Cell = String

data ListCell =
    ListCell { left :: [Cell]
             , center :: Cell
             , right :: [Cell]
             , length_list :: Int
             , crop_number :: Int
             }

data Params =
    Params { rule :: Maybe Int
           , start :: Maybe Int
           , lignes :: Maybe Int
           , window :: Maybe Int
           , move :: Maybe Int
           }

data Rule = Rule30 | Rule90 | Rule110

data Config =
    Config { configRule :: Rule
           , configStart :: Int
           , configLignes :: Maybe Int
           , configWindow :: Int
           , configMove :: Int
           }

initConf :: Params
initConf = Params { rule = Nothing
                  , start = Nothing
                  , lignes = Nothing
                  , window = Nothing
                  , move = Nothing
                  }

toConfig :: Params -> Maybe Config
toConfig params = do
    rule' <- toRule $ rule params
    return $ Config { configRule = rule'
                   , configMove = maybe 0 id (move params)
                   , configStart = maybe 0 id (start params)
                   , configLignes = lignes params
                   , configWindow = maybe 80 id (window params)
                   }

toRule :: Maybe Int -> Maybe Rule
toRule (Just 30) = Just Rule30
toRule (Just 90) = Just Rule90
toRule (Just 110) = Just Rule110
toRule _ = Nothing

manageArgs :: [String] -> Maybe Params
manageArgs args = go initConf args
    where
        go :: Params -> [String] -> Maybe Params
        go config [] = Just config
        go config (_:[]) = Nothing
        go config ("--rule":valueRule:params) =
            go config{rule = readMaybe valueRule} params
        go config ("--start":valueStart:params) =
            go config{start = readMaybe valueStart} params
        go config ("--lines":valueLines:params) =
            go config{lignes = readMaybe valueLines} params
        go config ("--window":valueWindow:params) =
            go config{window = readMaybe valueWindow} params
        go config ("--move":valueMove:params) =
            go config{move = readMaybe valueMove} params
        go _ _ = Nothing

selectRule :: Rule -> ((Cell, Cell, Cell) -> Cell)
selectRule Rule30 = patternRule30
selectRule Rule90 = patternRule90
selectRule Rule110 = patternRule110

createPyramid :: Maybe Config -> ListCell -> IO()
createPyramid Nothing _ = return()
createPyramid (Just (Config rule 0 Nothing lenwindow move)) x =
    printListCell (lenwindow + move) 0 x >>
    createPyramid (Just (Config rule 0 Nothing
    lenwindow move)) (addToListCell(createNewListCell (selectRule rule)
    lenwindow x))
createPyramid (Just (Config rule start Nothing lenwindow move)) x =
    printListCell (lenwindow + move) start x >>
    createPyramid (Just (Config rule (start - 1) Nothing
    lenwindow move)) (addToListCell(createNewListCell (selectRule rule)
    (lenwindow + move) x))
createPyramid (Just (Config rule start (Just 1) lenwindow move)) x =
    printListCell (lenwindow + move) start x
createPyramid (Just (Config rule 0 (Just nblines) lenwindow move)) x =
    printListCell (lenwindow + move) 0 x >>
    createPyramid (Just (Config rule 0 (Just (nblines - 1))
    lenwindow move)) (addToListCell(createNewListCell (selectRule rule)
    (lenwindow + move) x))
createPyramid (Just (Config rule start (Just nblines) lenwindow move)) x =
    printListCell (lenwindow + move) start x >>
    createPyramid (Just (Config rule (start - 1) (Just nblines)
    lenwindow move)) (addToListCell(createNewListCell (selectRule rule)
    (lenwindow + move) x))

printListCell :: Int -> Int -> ListCell -> IO()
printListCell lenwindow 0 (ListCell left center right length_list
    crop_number)  |  length_list == lenwindow + 1 && even lenwindow == True =
    putStr (concat (reverse left))
    >> putStr center >>
    putStrLn (concat (cropList (reverse right) crop_number))
                  | length_list > lenwindow + 1  && even lenwindow == True =
    putStr (concat (reverse (cropList (reverse left) crop_number)))
    >> putStr center >>
    putStrLn (concat (cropList (reverse right) (crop_number + 1)))
                  | length_list > lenwindow && even lenwindow == False =
    putStr (concat (reverse (cropList (reverse left) crop_number)))
    >> putStr center >>
    putStrLn (concat (cropList (reverse right) crop_number))
printListCell lenwindow 0 (ListCell left center right length_list crop_number)
    = putStr (fillSideList (manageWindow lenwindow length_list "left")
    "") >> putStr (concat (reverse left))
    >> putStr center >> putStr (concat right) >>
    putStrLn (fillSideList (manageWindow lenwindow length_list "right") "")
printListCell _ _ _ = return()

cropList :: [Cell] -> Int -> [Cell]
cropList x 0 = reverse x
cropList (x:xs) n = cropList xs (n - 1)

manageWindow :: Int -> Int -> String -> Int
manageWindow length_window len_list "left" | even length_window == True =
    (div (length_window - len_list) 2) + 1
                                           | otherwise =
    (div (length_window - len_list) 2)
manageWindow length_window len_list "right" =
    (div (length_window - len_list) 2)

fillSideList :: Int -> String -> String
fillSideList n s    | n <= 0 = s
                    | otherwise = fillSideList (n - 1) (s ++ " ")

initListCell :: Cell -> ListCell
initListCell x = ListCell { left = [" ", " "]
                          , center = x
                          , right = [" ", " "]
                          , length_list = 5
                          , crop_number = 1
                          }

addToListCell :: ListCell -> ListCell
addToListCell (ListCell left center right length_list crop_number) =
    ListCell (reverse (" ":reverse left)) center (reverse (" ":reverse right))
    length_list crop_number

changeListCellRight :: ((Cell, Cell, Cell) -> Cell) -> Int -> Cell -> [Cell] ->
    [Cell] -> [Cell]
changeListCellRight f 1 x (y:ys) nl = reverse (y:nl)
changeListCellRight f n x (y:z:zs) nl =
    changeListCellRight f (n - 1) y (z:zs) (f(x, y, z):nl)

changeListCellLeft :: ((Cell, Cell, Cell) -> Cell) -> Int -> Cell -> [Cell] ->
    [Cell] -> [Cell]
changeListCellLeft f 1 x (y:ys) nl = reverse (y:nl)
changeListCellLeft f n x (y:z:zs) nl =
    changeListCellLeft f (n - 1) y (z:zs) (f(z, y, x):nl)

createNewListCell :: ((Cell, Cell, Cell) -> Cell) -> Int -> ListCell ->
    ListCell
createNewListCell f lenwindow (ListCell (left:ls) center (right:rs) length_list
    crop_number)    | length_list > lenwindow  && even lenwindow == False =
    ListCell (changeListCellLeft f (div (length_list - 1) 2)
    center (left:ls) []) (f(left, center, right))
    (changeListCellRight f (div (length_list - 1) 2) center (right:rs) [])
    (length_list + 2) (crop_number + 1)
                    | length_list > lenwindow + 1 && even lenwindow == True =
    ListCell (changeListCellLeft f (div (length_list - 1) 2)
    center (left:ls) []) (f(left, center, right))
    (changeListCellRight f (div (length_list - 1) 2) center (right:rs) [])
    (length_list + 2) (crop_number + 1)
                    | otherwise =
    ListCell (changeListCellLeft f (div (length_list - 1) 2)
    center (left:ls) []) (f(left, center, right))
    (changeListCellRight f (div (length_list - 1) 2) center (right:rs) [])
    (length_list + 2) crop_number

patternRule30 :: (Cell, Cell, Cell) -> Cell
patternRule30 ("*", "*", "*") = " "
patternRule30 ("*", "*", " ") = " "
patternRule30 ("*", " ", "*") = " "
patternRule30 ("*", " ", " ") = "*"
patternRule30 (" ", "*", "*") = "*"
patternRule30 (" ", "*", " ") = "*"
patternRule30 (" ", " ", "*") = "*"
patternRule30 (" ", " ", " ") = " "
patternRule30 (_, _, _) = " "

patternRule90 :: (Cell, Cell, Cell) -> Cell
patternRule90 ("*", "*", "*") = " "
patternRule90 ("*", "*", " ") = "*"
patternRule90 ("*", " ", "*") = " "
patternRule90 ("*", " ", " ") = "*"
patternRule90 (" ", "*", "*") = "*"
patternRule90 (" ", "*", " ") = " "
patternRule90 (" ", " ", "*") = "*"
patternRule90 (_, _, _) = " "

patternRule110 :: (Cell, Cell, Cell) -> Cell
patternRule110 ("*", "*", "*") = " "
patternRule110 ("*", "*", " ") = "*"
patternRule110 ("*", " ", "*") = "*"
patternRule110 ("*", " ", " ") = " "
patternRule110 (" ", "*", "*") = "*"
patternRule110 (" ", "*", " ") = "*"
patternRule110 (" ", " ", "*") = "*"
patternRule110 (_, _, _) = " "
