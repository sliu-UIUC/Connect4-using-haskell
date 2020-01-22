module Connect4 where 
     import Data.List
     import Data.Maybe
     import Debug.Trace
     import System.IO
     import System.Environment
     import Data.Char
     import System.Console.GetOpt
     import Control.Monad
     
     type Board = [Column]
     type Column = [Maybe Token]
     data Player = One | Two deriving (Show,Eq)
     data Outcome = Winner Player | Tie deriving (Show, Eq)
     type Token = Player
     type Score = Int
     type Move = Int
     type Game = (Player, Board)

     findColumn :: Move -> Board -> Column
     findColumn 1 board = head board
     findColumn num (x:xs) = findColumn (num-1) xs
              
     winner :: Game -> Maybe Outcome
     winner (player, board) =
         case winningPlayer board of
         Just p -> Just (Winner p)
         Nothing -> if possibleMoves board == []
                    then Just Tie
                    else Nothing

     winningPlayer :: Board -> Maybe Player
     winningPlayer board = case checkVertical board of
                     Just p -> Just p
                     Nothing -> case checkHorizontal board of
                                Just p -> Just p
                                Nothing -> checkDiagonal board

     checkVerticalHelper :: Column -> Maybe Player
     checkVerticalHelper column =
         if (replicate 4 (Just One)) `isInfixOf` column
         then Just One
         else if (replicate 4 (Just Two)) `isInfixOf` column
         then Just Two
         else Nothing

     checkVertical :: Board -> Maybe Player
     checkVertical [] = Nothing
     checkVertical (b:bs) = case checkVerticalHelper b of
                            Just p -> Just p
                            Nothing -> checkVertical bs
                       
     checkHorizontalHelper :: Column -> Column -> Column -> Column -> Maybe Player
     checkHorizontalHelper [] _ _ _ = Nothing
     checkHorizontalHelper _ [] _ _ = Nothing
     checkHorizontalHelper _ _ [] _ = Nothing
     checkHorizontalHelper _ _ _ [] = Nothing
     checkHorizontalHelper (c1:c1s) (c2:c2s) (c3:c3s) (c4:c4s) =
         let ones = replicate 4 (Just One)
             twos = replicate 4 (Just Two)
             row = [c1,c2,c3,c4]
         in if (row == ones)
         then Just One
         else if (row == twos)
         then Just Two
         else checkHorizontalHelper c1s c2s c3s c4s

     checkHorizontal :: Board -> Maybe Player
     checkHorizontal (b1:b2:b3:b4:bs) = case checkHorizontalHelper b1 b2 b3 b4 of
                                        Just p -> Just p
                                        Nothing -> checkHorizontal (b2:b3:b4:bs)
     checkHorizontal _ = Nothing

     checkDiagonal :: Board -> Maybe Player
     checkDiagonal ([]:bs) = Nothing
     checkDiagonal (_:[]:bs) = Nothing
     checkDiagonal (_:_:[]:bs) = Nothing
     checkDiagonal (_:_:_:[]:bs) = Nothing
     checkDiagonal (b1:b2:b3:b4:bs) = case checkHorizontalHelper b1 (drop 1 b2) (drop 2 b3) (drop 3 b4) of
                                      Just p -> Just p
                                      Nothing -> case checkHorizontalHelper (drop 3 b1) (drop 2 b2) (drop 1 b3) b4 of
                                                 Just p -> Just p
                                                 Nothing -> if (length bs == 0) then Nothing else checkDiagonal (b2:b3:b4:bs)
                                          

     makeMoveGame :: Game -> Move -> Maybe Game
     makeMoveGame (player, board) move =
         let newBoard = makeMove board move
         in case newBoard of
                 Nothing -> Nothing
                 Just newB ->
                  case player of
                       One -> Just (Two, newB)
                       Two -> Just (One, newB)
                  
     makeMove :: Board -> Move -> Maybe Board
     makeMove board move =
                        if((move `elem` [1..7])==False)
                        then Nothing
                        else ( do let front = take (move-1) board
                                  let back = drop move board
                                  let theColumn = findColumn move board
                                  newColumn <- dropPiece (turn board) theColumn
                                  return $ front ++ (newColumn:back)
                        )

     dropPiece :: Player -> Column -> Maybe Column
     dropPiece player column =
                     let nothings = [x | x <- column, x == Nothing]
                         tokens = [x | x <- column, x /= Nothing]
                     in  case Nothing `elem` column of
                         False -> Nothing
                         True -> Just (tokens ++ [Just (player)]++ (tail nothings))

     validMove board move =
                  if ((move `elem` [1..7]) && (Nothing `elem` theColumn))
                  then True
                  else False
              where theColumn = findColumn move board

     turn:: Board -> Player
     turn board = if (length countTokens) `mod` 2 == 0 then One else Two
           where countTokens = filter (\x -> x /= Nothing) [ y | x <- board, y <- x]
      
     findBestMove :: Game -> (Outcome, Maybe Move) 
     findBestMove game@(player, board) =
                 case winner game of
                      Just winner -> (winner, Nothing)    
                      Nothing ->  let possibleMvs = possibleMoves board
                                      moveAndOutc = [ (case makeMoveGame game x of
                                                            Nothing  -> (whoWins game, x)
                                                            Just gme -> (whoWins gme, x)) | x <- possibleMvs]
                                      winningMove = lookup (Winner player) moveAndOutc
                                      tieMove     = lookup Tie moveAndOutc
                                      winMvnOutc  = (Winner player, winningMove)
                                      tieMvnOutc  = (Tie, tieMove)
                                      anotherPlr  = if player == One then Two else One
                                  in case winningMove of 
                                          Just winMv -> winMvnOutc
                                          Nothing    -> case tieMove of
                                                        Just tieMv -> tieMvnOutc
                                                        Nothing    -> (Winner anotherPlr , Just(snd (head moveAndOutc)))

     whoWins :: Game -> Outcome
     whoWins game@(player, board) =
         case winner game of
             Just outcome -> outcome
             Nothing ->
                let possibleMvs = possibleMoves board
                    resultBds = catMaybes [makeMoveGame game x| x <- possibleMvs]
                    winners = [whoWins y | y <- resultBds]
                    possibleWin = filter (== (Winner player)) winners
                    possibleTie = filter (== (Tie)) winners
                in if Winner player `elem` possibleWin
                      then head possibleWin
                      else (if possibleTie /= []
                            then head possibleTie
                            else head winners )
                       

     isWinnerMove :: Board -> Move -> Player -> Bool
     isWinnerMove board move player =
             case makeMove board move of
                        Nothing -> False
                        Just bd ->  if (winningPlayer bd) == (Just player)
                                    then True else False

     possibleMoves :: Board -> [Move]
     possibleMoves  board = [x | x <- [1..7],(Nothing `elem` (findColumn x board))]

     printSpace :: Maybe Token -> Char
     printSpace Nothing = '-'
     printSpace (Just One) = '1'
     printSpace (Just Two) = '2'

     stripColumns :: Board -> [[Maybe Token]]
     stripColumns [] = []
     stripColumns ([]:cs) = []
     stripColumns board =
         let row = map head board
             newBoard = map tail board
         in row:(stripColumns newBoard)
         
    
     showBoard board =
         let rows = reverse $ stripColumns board
             rowStrings = [[printSpace x | x <- r] | r <- rows]
         in unlines rowStrings

     parseSpace :: Char -> Maybe Token
     parseSpace '1' = Just One
     parseSpace '2' = Just Two
     parseSpace _ = Nothing

     printHelperR [] = do putStrLn ""
     printHelperR (r:rs) = do
         putStrLn r
         printHelperR rs

     showGame :: Game -> String
     showGame (player, board) =
         let s = (printSpace (Just player)):"\n"
         in  s ++ (showBoard board)
    
     smallBoard = [[Just Two, Just One,Just Two, Nothing],
         [Just Two, Just Two, Nothing, Nothing],
         [Just One, Just One, Nothing, Just One],
         [Just One, Just Two, Just One, Just Two]]

     createBoard :: Board
     createBoard = [
         [Just One,Just Two,Just One,Just Two,Just One,Nothing],
         [Just One,Just One,Just Two,Just One,Just Two,Nothing],
         [Just Two,Just Two,Just Two,Just One,Just One,Nothing],
         [Just One,Just Two,Just One,Just One,Just Two,Nothing],
         [Just One,Just One,Just One,Just Two,Just One,Nothing],
         [Just Two,Just Two,Just Two,Just One,Just Two,Nothing],
         [Just Two,Just One, Just One, Just One,Just Two,Nothing]]


     testBoard =  [
         [Just One,Nothing,Nothing,Nothing,Nothing,Nothing],
         [Just One,Nothing,Nothing,Just One,Nothing,Nothing],
         [Just Two,Just Two,Just Two,Nothing,Nothing,Nothing],
         [Just One,Just Two,Nothing,Nothing,Nothing,Nothing],
         [Just One,Nothing,Just Two,Nothing,Nothing,Nothing],
         [Just One,Nothing,Nothing,Nothing,Nothing,Nothing],
         [Nothing,Nothing, Nothing, Nothing,Nothing,Nothing]]
    
    
     readBoard :: String -> IO (Maybe Game)
     readBoard file = do
         contents <- readBoardFile file
         return (parseGame contents)

     readBoardFile :: String -> IO [String]
     readBoardFile file = do
         handle <- openFile file ReadWriteMode
         contents <- hGetContents handle
         return (lines contents)
    
     parseGame :: [String] -> Maybe Game
     parseGame input = let player = case read $ head input of
                                    1 -> Just One
                                    2 -> Just Two
                                    _ -> Nothing
                           board = parseColumns $ tail input
                       in case player of
                          Just p -> Just (p, board)
                          Nothing -> Nothing

     parseColumns :: [String] -> Board
     parseColumns [] = []
     parseColumns (c:cs) = let tokens = map parseSpace $ map head (words c)
                           in tokens:(parseColumns cs)

     writeGame :: String -> Game -> IO ()
     writeGame filename game = do
         let contents = gameToString game
         writeFile filename $ unlines contents

     gameToString :: Game -> [String]
     gameToString (player, board) =
         let p = (printSpace $ Just player):" "
             rows = reverse $ stripColumns board
             rowStrings = [concat [(printSpace x):" " | x <- r] | r <- rows]
             colStrings = [concat [(printSpace x):" " | x <- c] | c <- board]
         in p:(colStrings)
     
