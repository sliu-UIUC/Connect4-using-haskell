module Solver where 
     import Data.List
     import Data.Maybe
     import Debug.Trace
     import System.IO
     import System.Environment
     import Data.Char
     import System.Console.GetOpt
     import Control.Monad
     import Connect4

     interactive :: Game -> Int->IO()
     interactive game n = do
           move <- prompt "please make a move: "
           case playOnBoard game move of
                Nothing -> interactive game n
                Just played ->
                     case winner played of
                            Just outcome -> putStrLn ((show(outcome)) 
                                            ++ "\n" ++ (showGame played))
                            Nothing ->
                              case makeMoveGame played (findGoodMove played n) of
                                        Nothing -> return ()
                                        Just aiPlayed -> do
                                            putStr (showGame aiPlayed)
                                            case winner aiPlayed of
                                                  Nothing ->
                                                             interactive aiPlayed n
                                                  Just outcome2 ->
                                                             putStrLn (show(outcome2))

     prompt :: String -> IO String
     prompt str = do
          putStr str
          hFlush stdout
          answer <- getLine
          return answer

     playOnBoard::Game->String ->Maybe Game
     playOnBoard game@(player,board) move =
           let properNum =['1'..'7']
           in if all (`elem` properNum) move
              then let moveNum = read move
                   in makeMoveGame game moveNum
              else Nothing


     countVertical :: Board -> Int
     countVertical [] = 0
     countVertical (v:vs) = (countVerticalHelper v) + (countVertical vs)

     countVerticalHelper :: Column -> Int
     countVerticalHelper (a:b:c:[]) = 0
     countVerticalHelper (a:b:c:d:xs) = let column = [a,b,c,d]
                                            players = catMaybes column
                                        in  if (One `elem` players && Two `elem` players) || (length players)<2
                                        then countVerticalHelper (b:c:d:xs)
                                        else
                                             let symbol = case One `elem` players of
                                                            True -> 1
                                                            False -> -1
                                                 num = case length players of
                                                            3 -> 5
                                                            2 -> 1
                                             in symbol*num + countVerticalHelper (b:c:d:xs)

     countHorizontal :: Board -> Int
     countHorizontal (b1:b2:b3:[]) = 0
     countHorizontal (b1:b2:b3:b4:bs) = (countHorizontalHelper b1 b2 b3 b4) + (countHorizontal (b2:b3:b4:bs))

     countHorizontalHelper :: Column -> Column -> Column -> Column -> Int
     countHorizontalHelper [] [] [] [] = 0
     countHorizontalHelper [] [] [] ds = 0
     countHorizontalHelper [] [] cs [] = 0
     countHorizontalHelper [] bs [] [] = 0
     countHorizontalHelper as [] [] [] = 0
     countHorizontalHelper (a:as) (b:bs) (c:cs) (d:ds) =
             let row = [a,b,c,d]
                 players = catMaybes row
             in  if (One `elem` players && Two `elem` players) || (length players)<2
                 then countHorizontalHelper as bs cs ds
                 else
                      let symbol = case One `elem` players of
                                       True -> 1
                                       False -> -1
                          num = case length players of
                                       3 -> 5
                                       2 -> 1
                      in symbol*num + (countHorizontalHelper as bs cs ds)

     countDiagonal :: Board -> Int
     countDiagonal ([]:bs) = 0
     countDiagonal (_:[]:bs) = 0
     countDiagonal (_:_:[]:bs) = 0
     countDiagonal (_:_:_:[]:bs) = 0
     countDiagonal (b1:b2:b3:b4:bs) =
                  countHorizontalHelper b1 ((drop 1 b2)++[Nothing]) ((drop 2 b3)++(replicate 2 Nothing)) ((drop 3 b4)++(replicate 3 Nothing))
               + (countHorizontalHelper ((drop 3 b1)++(replicate 3 Nothing)) ((drop 2 b2)++(replicate 2 Nothing)) ((drop 1 b3)++[Nothing]) b4)
               + (if (length bs > 1) then countDiagonal (b2:b3:b4:bs) else 0)


     evalBoard :: Game -> Int 
     evalBoard game@(player,board) = case  winner game of
                                              Just (Winner One) -> 1000
                                              Just (Winner Two) -> -1000
                                              Just Tie          -> 0
                                              Nothing           ->
                                                 let possibleM = possibleMoves board
                                                     possibleWinner = [isWinnerMove board x player| x<-possibleM]
                                                     symbol = case player of
                                                               One -> 1
                                                               Two -> -1
                                                 in if True `elem` possibleWinner
                                                    then 19*symbol
                                                    else (countHorizontal board) + (countVertical board) + (countDiagonal board)

     findGoodMove :: Game -> Int-> Move
     findGoodMove game@(player,board) n =
                       let possibleM = possibleMoves board
                           afterMoving = catMaybes [ (makeMove board x)|x<-possibleM]
                           scores = [bestOutcome (player,x) (n-1)|x<-afterMoving]
                       in case player of
                              One -> snd(maximum(zip scores possibleM))
                              Two -> snd(minimum(zip scores possibleM))

     bestOutcome:: Game -> Int -> Int
     bestOutcome game 0 = evalBoard game
     bestOutcome game@(player,board) n =
              let possibleM = possibleMoves board
                  afterMoving = catMaybes [ (makeMoveGame game x)|x<-possibleM]
                  scores = [bestOutcome x (n-1)|x<-afterMoving]
              in  case player of
                       One -> maximum scores
                       Two -> minimum scores

