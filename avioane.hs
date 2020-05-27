import Data.Char
import System.Random

-- MACROS
type Coord = (Int, Int)
type Board = [[Bool]]
type Avion = [Coord]
matrixSize = 10
nrOfPlanes = 1
planeLength = 8

-- UTILITIES
initBoard :: Board
initBoard = take matrixSize (repeat (take matrixSize (repeat False)))
initBoardTrue :: Board
initBoardTrue = take matrixSize (repeat (take matrixSize (repeat True)))

matrixWithPlane :: Board -> Avion -> Board
matrixWithPlane board [] = board
matrixWithPlane board (x:xs) = matrixWithPlane (replaceItem (fst x) board (notAtIndex (snd x) ((!!) board (fst x)))) xs

matrixWithPlanes :: Board ->[Avion] -> Board
matrixWithPlanes board [] = board
matrixWithPlanes board (x:xs) = matrixWithPlanes (matrixWithPlane board x) xs

notAtIndex :: Int -> [Bool] -> [Bool]
notAtIndex n sir = take (n) sir ++ [not (last (take (n+1) sir))] ++ drop (n+1) sir

replaceItem :: Int -> [a] -> a -> [a]
replaceItem n sir val = take(n) sir ++ [val] ++ drop (n+1) sir

changeChar :: String -> [Int] -> Char -> String
changeChar string [] _ = string
changeChar string (x:xs) char = replaceItem x (changeChar string xs char) char

get :: Int -> Int -> Board -> Bool
get x y matrix = (!!)((!!) matrix x) y

matToString :: [[Bool]] -> Coord -> String
matToString board coord
    | fst coord < matrixSize && snd coord < matrixSize = if (get (fst coord) (snd coord) board) == True 
                                                            then 'x':matToString board (fst coord, snd coord + 1)
                                                            else '-':matToString board (fst coord, snd coord + 1)
    | fst coord < matrixSize = '\n':matToString board (fst coord + 1, 0)
    | otherwise = []


matToString2 :: Board -> [Avion] -> Coord -> String
matToString2 board avioane coord
        | fst coord < matrixSize && snd coord < matrixSize = if (get (fst coord) (snd coord) board) == True then
                                               if or [coord == coord' | avion <- avioane, coord' <- avion] then 'o' : ' ' : matToString2 board avioane (fst coord, snd coord + 1)
                                                   else 'x' : ' ' : matToString2 board avioane (fst coord, snd coord+1)
                                           else '~' : ' ' : matToString2 board avioane (fst coord, snd coord + 1)                               
        | fst coord < matrixSize = "\n" ++ (show (fst coord +1)) ++ " " ++ matToString2 board avioane (fst coord + 1, 0)
        | otherwise = []

printMatrix :: [[Bool]] -> IO ()
printMatrix board = putStrLn (matToString board (0,0))

printMatrix2 :: Board -> [Avion] ->IO ()
printMatrix2 board planes = putStrLn (init (init (init ("  0 1 2 3 4 5 6 7 8 9\n0 " ++ matToString2 board planes (0,0)))))

fst3 :: (a,a,a) -> a
fst3 (x,_,_) = x

snd3 :: (a,a,a) -> a
snd3 (_,x,_) = x

thrd3 :: (a,a,a) -> a
thrd3 (_,_,x) = x

-- GAME FLOW RELATED BUSINESS
removeDestroyedPlanes :: [Avion] -> [Avion]
removeDestroyedPlanes [] = []
removeDestroyedPlanes (x:xs) | null x    = removeDestroyedPlanes xs
                            | otherwise = x : removeDestroyedPlanes xs

playRound :: Board -> [Avion] -> Coord -> (Board, [Avion], Bool)
playRound board avioane coords = (attack board coords, removeDestroyedPlanes [fst (isPlaneDestroyed board avion coords) | avion <- avioane], or [snd (isPlaneDestroyed board avion coords) | avion <- avioane])

attack :: Board -> Coord -> Board
attack board (a, b) = replaceItem a board (notAtIndex b ((!!) board a))

beginGame :: Board -> [Avion] -> IO () 
beginGame board avioane = do 
                            putStrLn "Atacati!"
                            coord <- getLine
                            let (newBoard, newPlanes, isHit) = playRound board avioane (inputToCoord coord)
                            if isHit then putStrLn "Lovit!" else putStrLn "Ratat!"

                            if length newPlanes < length avioane then putStrLn "Avion doborat!"
                            else putStrLn  ""

                            printMatrix2 newBoard newPlanes
                            
                            if length newPlanes == 0 then
                                putStrLn "Gata!"
                            else 
                                beginGame newBoard newPlanes

isPlaneDestroyed :: Board -> Avion -> Coord -> (Avion, Bool)
isPlaneDestroyed board avion coord = if or [coord == coord' | coord' <- avion] == False then do
                                               (avion, False)
                                           else do
                                               if and [get (fst coord') (snd coord') board == True | coord' <- avion, coord' /= coord] == False then
                                                   (avion, True)
                                               else
                                                   ([], True)

-- HUMAN RELATED BUSINESS
setPlane :: Coord -> String -> IO (Avion)
setPlane coord directie | directie == "sus" =  return [coord,(fst coord+1,snd coord),(fst coord-1,snd coord),(fst coord,snd coord+1),(fst coord, snd coord-1),
                                                (fst coord + 2, snd coord -1), (fst coord + 2, snd coord), (fst coord + 2, snd coord + 1)]
                        | directie == "jos" = return [coord,(fst coord+1,snd coord),(fst coord-1,snd coord),(fst coord,snd coord+1),(fst coord, snd coord-1),
                                                (fst coord - 2, snd coord), (fst coord - 2, snd coord -1), (fst coord - 2, snd coord + 1)]
                        | directie == "dreapta" = return [coord,(fst coord+1,snd coord),(fst coord-1,snd coord),(fst coord,snd coord+1),(fst coord, snd coord-1),
                                                (fst coord, snd coord-2), (fst coord -1, snd coord-2), (fst coord + 1, snd coord-2)]
                        | directie == "stanga" = return [coord,(fst coord+1,snd coord),(fst coord-1,snd coord),(fst coord,snd coord+1),(fst coord, snd coord-1),
                                                (fst coord, snd coord+2), (fst coord-1, snd coord+2), (fst coord+1, snd coord+2)]
                        | otherwise = return []

invalidCoord :: Coord -> [Avion] -> Bool
invalidCoord coord avioane = fst coord < 0 || fst coord > 9 || snd coord < 0 || snd coord > 9 || or [coord == coord' | avion <- avioane , coord' <- avion]

invalidPlane :: Avion -> [Avion] -> Bool
invalidPlane [] _ = False
invalidPlane (x:xs) avioane = invalidCoord x avioane || invalidPlane xs avioane 


setPlanes :: Int -> [Avion] -> IO [Avion]
setPlanes nrAvion avioanePlasate | nrAvion <= nrOfPlanes = do 
                                                                putStrLn ("Introdu coordonata avionului " ++ (show nrAvion))
                                                                coord <- getLine
                                                                putStrLn ("Introdu orientarea avionului " ++ (show nrAvion))
                                                                orientare <- getLine
                                                                avion <- setPlane (inputToCoord coord) orientare
                                                                if invalidPlane avion avioanePlasate then 
                                                                    setPlanes nrAvion avioanePlasate
                                                                else do 
                                                                      planesList <- setPlanes (nrAvion+1) (avion:avioanePlasate)
                                                                      return (avion:planesList)
                                 | otherwise = return []

inputToCoord :: String -> Coord
inputToCoord ['(',x,',',y,')'] = ((digitToInt x), (digitToInt y))       

-- BOT RELATED BUSINESS

-- length (filter (== True)[2 == nr | array <- [[1,2,3],[2,3,4]], nr <- array ]) count appearances

getRoundPlanes :: Coord -> IO [Avion]
getRoundPlanes coord = do 
                        sus <- setPlane coord "sus"
                        jos <- setPlane coord "jos"
                        dreapta <- setPlane coord "dreapta"
                        stanga <- setPlane coord "stanga"
                        return [sus, jos, dreapta, stanga]

getOuterPlanes :: Coord -> IO [Avion]
getOuterPlanes coord = do 
                        plane1 <- setPlane (fst coord - 2, snd coord) "sus"
                        plane2 <- setPlane (fst coord - 2, snd coord + 1) "sus"
                        plane3 <- setPlane (fst coord - 2, snd coord - 1) "sus"
                        plane4 <- setPlane (fst coord, snd coord - 2) "stanga"
                        plane5 <- setPlane (fst coord - 1, snd coord - 2) "stanga"
                        plane6 <- setPlane (fst coord + 1, snd coord - 2) "stanga"
                        plane7 <- setPlane (fst coord + 2, snd coord) "jos"
                        plane8 <- setPlane (fst coord + 2, snd coord - 1) "jos"
                        plane9 <- setPlane (fst coord + 2, snd coord + 1) "jos"
                        plane10 <- setPlane (fst coord, snd coord + 2) "dreapta"
                        plane11 <- setPlane (fst coord - 1, snd coord + 2) "dreapta"
                        plane12 <- setPlane (fst coord + 1, snd coord + 2) "dreapta"
                        return [plane1, plane2, plane3, plane4, plane5, plane6, plane7, plane8, plane9, plane10, plane11, plane12]

getAllPossiblePlanes :: Coord -> IO [Avion]
getAllPossiblePlanes coord = do 
                                planesC <- getRoundPlanes coord
                                planesU <- getRoundPlanes ((fst coord) - 1, snd coord)
                                planesD <- getRoundPlanes ((fst coord) + 1, snd coord)
                                planesR <- getRoundPlanes (fst coord, (snd coord)  - 1)
                                planesL <- getRoundPlanes (fst coord, (snd coord)  + 1)
                                planesO <- getOuterPlanes coord
                                return (destroyOutOfBoundsPlanes (planesC ++ planesU ++ planesD ++ planesR ++ planesL ++ planesO))

destroyOutOfBoundsPlanes :: [Avion] -> [Avion]
destroyOutOfBoundsPlanes [] = []
destroyOutOfBoundsPlanes (x:xs) = if invalidPlane x [] then destroyOutOfBoundsPlanes xs
                                    else x:(destroyOutOfBoundsPlanes xs)

destroyUnfittingPlanes ::  [Avion] -> Coord -> Bool -> [Avion]
destroyUnfittingPlanes [] _ _ = []
destroyUnfittingPlanes (avion:avioane) coord isHit | isHit = if or [coord == coord' | coord' <- avion] then  avion:destroyUnfittingPlanes avioane coord isHit
                                                                else destroyUnfittingPlanes avioane coord isHit
                                                   | otherwise = if or [coord == coord' | coord' <- avion] then  destroyUnfittingPlanes avioane coord isHit
                                                                    else avion:destroyUnfittingPlanes avioane coord isHit

getRandomCoord :: [Coord] -> IO Coord
getRandomCoord coords = do 
                    x <- randomRIO (0,9)
                    y <- randomRIO (0,9)
                    if or [(x,y) == coord | coord <- coords ] then getRandomCoord coords
                        else return (x,y)

getRandomCoordOfPossiblePlanes :: [Avion] -> [Coord] -> IO Coord
getRandomCoordOfPossiblePlanes avioane hitCoords = do 
                                                    randomPlane <- randomRIO (0, length avioane -1)
                                                    randomIndex <- randomRIO (0, planeLength -1)
                                                    let coord = (!!) ((!!) avioane randomPlane) randomIndex
                                                    if or [coord == coord' | coord' <- hitCoords] then getRandomCoordOfPossiblePlanes avioane hitCoords
                                                        else return coord


getRandomOrientation :: IO String
getRandomOrientation = do 
                        x <- randomRIO (0,3)
                        if x == (0 :: Int) then return "sus" else 
                            if x == 1 then return "jos" else 
                                if x == 2 then return "dreapta" 
                                else return "stanga"
            


setBotPlane :: IO Avion
setBotPlane = do 
                coord <- getRandomCoord []
                orientation <- getRandomOrientation
                avion <- (setPlane coord orientation)
                return avion

setBotPlanes :: Int -> [Avion] -> IO [Avion]
setBotPlanes nrAvion avioanePlasate | nrAvion <= nrOfPlanes = do 
                                                                avion <- setBotPlane
                                                                if invalidPlane avion avioanePlasate then 
                                                                    setBotPlanes nrAvion avioanePlasate
                                                                else do 
                                                                      planesList <- setBotPlanes (nrAvion+1) (avion:avioanePlasate)
                                                                      return (avion:planesList)
                                 | otherwise = return []

beginGameBot :: Board -> [Avion] -> [Coord] -> [Avion]-> IO () 
beginGameBot board avioane checkedSpots [] = do 
                            putStrLn "Atacati!"
                            coord <- getRandomCoord checkedSpots
                            let (newBoard, newPlanes, isHit) = playRound board avioane coord
                            if isHit then do 
                                            putStrLn "Lovit!"
                                            printMatrix2 newBoard newPlanes
                                            possiblePlanes <- getAllPossiblePlanes coord
                                            beginGameBot newBoard newPlanes (coord:checkedSpots) possiblePlanes
                             else do 
                                    putStrLn "Ratat!"
                                    printMatrix2 newBoard newPlanes
                                    beginGameBot newBoard newPlanes (coord:checkedSpots) []

beginGameBot board avioane checkedSpots possiblePlanes = do 
                                                            putStrLn "Atacati!"
                                                            coord <- getRandomCoordOfPossiblePlanes possiblePlanes checkedSpots
                                                            let (newBoard, newPlanes, isHit) = playRound board avioane coord
                                                            if isHit then putStrLn "Lovit!" else putStrLn "Ratat!"

                                                            if (length newPlanes < length avioane) && length newPlanes > 0 then 
                                                                do 
                                                                    putStrLn "Avion doborat!"
                                                                    printMatrix2 newBoard newPlanes
                                                                    beginGameBot newBoard newPlanes (coord:checkedSpots) []
                                                             else if length newPlanes == 0 then do 
                                                                    printMatrix2 newBoard newPlanes
                                                                    putStrLn "Gata!"

                                                                else do 
                                                                        printMatrix2 newBoard newPlanes
                                                                        beginGameBot newBoard newPlanes (coord:checkedSpots) (destroyUnfittingPlanes possiblePlanes coord isHit)


-- MAIN

beginHvsAI :: Board -> Board -> [Avion] -> [Avion] -> [Coord] -> [Avion]-> IO ()
beginHvsAI hBoard aiBoard hPlanes aiPlanes checkedSpots [] = do 
                                                                -- Human part
                                                                putStrLn "Atacati!"
                                                                coord <- getLine
                                                                let (newBoard, newPlanes, isHit) = playRound aiBoard aiPlanes (inputToCoord coord)
                                                                if isHit then putStrLn "Ati Lovit!" else putStrLn "Ati Ratat!"

                                                                if length newPlanes < length aiPlanes then putStrLn "Ati doborat un avion!"
                                                                else putStrLn  ""

                                                                printMatrix2 newBoard newPlanes
                                                                
                                                                if length newPlanes == 0 then do 
                                                                    putStrLn "Ati castigat"
                                                                    return ()
                                                                else 
                                                                   do 
                                                                        putStrLn "Computerul ataca!"
                                                                        coord2 <- getRandomCoord checkedSpots
                                                                        let (newBoard2, newPlanes2, isHit2) = playRound hBoard hPlanes coord2
                                                                        if isHit2 then do 
                                                                                        putStrLn "Computerul a Lovit!"
                                                                                        printMatrix2 newBoard2 newPlanes2
                                                                                        possiblePlanes <- getAllPossiblePlanes coord2
                                                                                        beginHvsAI newBoard2 newBoard newPlanes2 newPlanes (coord2:checkedSpots) possiblePlanes
                                                                        else do 
                                                                                putStrLn "Computerul a Ratat!"
                                                                                printMatrix2 newBoard2 newPlanes2
                                                                                beginHvsAI newBoard2 newBoard newPlanes2 newPlanes (coord2:checkedSpots) []
                                                                       

beginHvsAI hBoard aiBoard hPlanes aiPlanes checkedSpots possiblePlanes = do 
                                                                            -- Human part
                                                                            putStrLn "Atacati!"
                                                                            coord <- getLine
                                                                            let (newBoard, newPlanes, isHit) = playRound aiBoard aiPlanes (inputToCoord coord)
                                                                            if isHit then putStrLn "Ati Lovit!" else putStrLn "Ati Ratat!"

                                                                            if length newPlanes < length aiPlanes then putStrLn "Ati doborat un avion!"
                                                                            else putStrLn  ""

                                                                            printMatrix2 newBoard newPlanes
                                                                            
                                                                            if length newPlanes == 0 then do
                                                                                putStrLn "Ati castigat"
                                                                                return ()
                                                                            else 
                                                                                do 
                                                                                    coord2 <- getRandomCoordOfPossiblePlanes possiblePlanes checkedSpots
                                                                                    let (newBoard2, newPlanes2, isHit2) = playRound hBoard hPlanes coord2
                                                                                    if isHit2 then putStrLn "Computerul a Lovit!" else putStrLn "Computerul a Ratat!"

                                                                                    if (length newPlanes2 < length hPlanes) && length newPlanes2 > 0 then 
                                                                                        do 
                                                                                            putStrLn "Computerul a doborat un avion!"
                                                                                            printMatrix2 newBoard2 newPlanes2
                                                                                            beginHvsAI newBoard2 newBoard newPlanes2 newPlanes (coord2:checkedSpots) []
                                                                                    else if length newPlanes2 == 0 then do 
                                                                                            printMatrix2 newBoard2 newPlanes2
                                                                                            putStrLn "Computerul a castigat!"
                                                                                            return ()

                                                                                        else do 
                                                                                                printMatrix2 newBoard2 newPlanes2
                                                                                                beginHvsAI newBoard2 newBoard newPlanes2 newPlanes (coord2:checkedSpots) (destroyUnfittingPlanes possiblePlanes coord2 isHit)

                                                                                    
                                                                            



main :: IO ()
main = do 
    --BOT TESTING
            -- avioane <- setBotPlanes 1 []
            -- printMatrix (matrixWithPlanes initBoard avioane)
            -- beginGameBot initBoard avioane [] []
            -- return ()

        --testing normal
          putStrLn "Bine ati venit la jocul avioane!"
          avioaneOm <- setPlanes 1 []
          putStrLn "Tabla domneavoastra! (Nu va faceti grijji, computerul nu le poate vedea)"
          printMatrix (matrixWithPlanes initBoard avioaneOm)
          putStrLn "Computerul a setat avioanele"
          avioaneAI <- setBotPlanes 1 []
          beginHvsAI initBoard initBoard avioaneOm avioaneAI [] []
          return()

        

                                  
