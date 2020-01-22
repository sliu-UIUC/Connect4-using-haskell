module Main where
     import Data.List
     import Data.Maybe
     import Debug.Trace
     import System.IO
     import System.Environment
     import Data.Char
     import System.Console.GetOpt
     import System.Console.ANSI
     import Control.Monad
     import Connect4
     import Solver

     data Options = Options { help :: Bool
           , theWinner   :: Bool
           , depth       :: Int
           , makeMv      :: Int
           , verbose     :: Bool
           , interAction    :: Bool
           } deriving Show

     defaultOptions :: Options
     defaultOptions = Options {
             help        = False
           , theWinner   = False
           , depth       = 3
           , makeMv      = 0
           , verbose     = False
           , interAction = False
           }

     options :: [OptDescr (Options -> Options) ]
     options = [ Option ['h'] ["help"] (NoArg (\opts -> opts{help=True})) "Print out a help message and quit the program"
          , Option ['w'] ["winner"] (NoArg (\opts -> opts{theWinner=True})) "Print out who will win this game, using an exhaustive search (no cut-off depth)."
          , Option ['d'] ["depth"] (ReqArg (\n opts-> opts{depth=setDepth n}) "<num>") "Use <num> as a cutoff depth, instead of your default"
          , Option ['m'] ["move"] (ReqArg (\n opts-> opts{makeMv=read n}) "<num>") "Make <move> and print out the resulting board, in the input format, to stdout."
          , Option ['v'] ["verbose"] (NoArg (\opts->opts{verbose=True})) "Output both the move and a description of how good it is: win, lose, tie, or a rating."
          , Option ['i'] ["interactive"] (NoArg (\opts->opts{interAction=True})) "Start a new game and play against the computer."]


     main :: IO()
     main = do
         args <- getArgs
         (flags, others) <- compilerOpts args
         if (help flags || interAction flags) 
         then if help flags then helpIO else printInteractive startingGame flags
         else   case others of
                [fname] -> do theGame <- readBoard fname
                              let action 
                                    | verbose flags         = printBoundedWinner
                                    | theWinner flags       = printUnboundedWinner
                                    | (makeMv flags) > 0    = printMoveResult 
                                    | interAction flags     = printInteractive 
                              case theGame of
                                   Nothing -> putStrLn "Invalid file!"
                                   Just game -> action game flags
                []      -> do putStrLn "No file provided!" 
                              helpIO
                _       -> do putStrLn "Too many files provided!"
                              helpIO
         
     setDepth [] = 3
     setDepth x = read x

     printInteractive theGame flg = interactive theGame (depth flg)         
 
     printMoveResult theGame flg =
                    case makeMoveGame theGame (makeMv flg) of
                         Nothing        -> putStrLn "Can't make move. The board is full."
                         Just (plr, bd) -> putStrLn $ showBoard bd

     printUnboundedWinner theGame flag =
                    case (snd $ findBestMove theGame) of
                         Nothing -> putStrLn "No move. The game already ends."
                         Just mv -> putStrLn $ show mv


     startingGame :: Game
     startingGame = (One, replicate 7 $ replicate 6 Nothing)

     printBoundedWinner game@(plr, bd) flags = 
        let maybeResult = findBestMove game
            result = formatScore (verbose flags) game maybeResult
        in  result

     formatScore flgs (plr,bd) (outc, maybeMv) = 
                 case maybeMv of 
                 Nothing -> putStrLn ("No move. The game already ends. The outcome is : " ++ (show outc))
                 Just mv -> case makeMoveGame (plr,bd) mv of 
                            Nothing     -> putStrLn "The board is full."
                            Just newGm  -> ratingDesc newGm outc mv

     ratingDesc :: Game -> Outcome -> Move -> IO ()
     ratingDesc theGame outc mv =
                 if rating == 1000 
                 then putStrLn ("Move: "++(show mv) 
                      ++ " Player one wins!")
                 else if rating == -1000 
                      then putStrLn ("Move: "++(show mv)
                           ++ " Player two wins!")  
                      else putStrLn ("Move: "++(show mv)
                           ++". The rating is "++(show rating)
                           ++". With the move the outcome could be: "
                           ++(show outc))
               where rating = evalBoard theGame

     helpIO ::IO()
     helpIO = putStrLn $ usageInfo "Usage: connect4 [OPTIONS...]" options

     compilerOpts :: [String] -> IO (Options,[String])
     compilerOpts argv = case getOpt Permute options argv of
                         (flags,others, []) -> return (foldl (flip id) defaultOptions flags, others)
                         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "Usage: connect4 [OPTIONS...]" options))
