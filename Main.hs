{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

import qualified System.Random as R
import Control.Monad.RWS
import Control.Monad.Trans
import Control.Applicative
import Data.Time (getCurrentTime, UTCTime)


newtype PlayT m a = PlayT { unPlayT :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

type Play a = PlayT (RWST () [PlayLog] R.StdGen IO) a

type Replay a = PlayT (RWST () () [PlayLog] IO) a

instance MonadTrans PlayT where
    lift = PlayT

data PlayLog
    = Init
        { initTime :: UTCTime
        , initSeed :: R.StdGen
        }
    | Input
        { inputTime :: UTCTime
        , inputValue :: String
        }
    | Output
        { outputTime :: UTCTime
        }
    | RandomValue
        { randomTime :: UTCTime
        , randomSeed :: R.StdGen
        }
    | Final
        { finalTime :: UTCTime
        , finalSeed :: R.StdGen
        }
    deriving (Show, Read)

data RPS = Rock | Paper | Scissors
data Result = UserLose | Draw | UserWin

class (Functor m, Applicative m, Monad m) => Replayable m where
    -- Get random values from seed.
    random :: R.Random a => m a
    randomR :: R.Random a => (a, a) -> m a

    -- Execute an User input action.
    -- All IO actions are executed through this method.
    -- Input Value
    input :: IO String -> m String

    -- Execute an User output action.
    -- All IO actions are executed through this method.
    -- Timing
    output :: IO () -> m ()

-- Play
instance Replayable (PlayT (RWST () [PlayLog] R.StdGen IO)) where
    random = do
        gen <- lift get
        let (v, gen') = R.random gen
        lift $ put gen'
        t <- liftIO getCurrentTime
        lift $ tell [RandomValue t gen]
        return v
    randomR range = do
        gen <- lift get
        let (v, gen') = R.randomR range gen
        lift $ put gen'
        t <- liftIO getCurrentTime
        lift $ tell [RandomValue t gen]
        return v
    input action = do
        v <- liftIO action
        t <- liftIO getCurrentTime
        lift $ tell [Input t v]
        return v
    output action = do
        v <- liftIO action
        t <- liftIO getCurrentTime
        lift $ tell [Output t]
        return v

-- Replay
instance Replayable (PlayT (RWST () () [PlayLog] IO)) where
    random = do
        (RandomValue t gen : rest) <- lift get
        let (v, _) = R.random gen
        lift $ put rest
        return v
    randomR range = do
        (RandomValue t gen : rest) <- lift get
        let (v, _) = R.randomR range gen
        lift $ put rest
        return v
    input action = do
        (Input t v : rest) <- lift get
        liftIO $ putStrLn v
        lift $ put rest
        return v
    output action = do
        (Output t : rest) <- lift get
        v <- liftIO action
        lift $ put rest
        return v

play :: R.StdGen -> Play a -> IO [PlayLog]
play gen prog = do
    st <- getCurrentTime
    (_, gen', w) <- runRWST (unPlayT prog) () gen
    et <- getCurrentTime
    return $ Init st gen : (w ++ [Final et gen'])

replay :: [PlayLog] -> Replay a -> IO ()
replay log prog = do
    let (Init t gen : rest) = log
    void $ runRWST (unPlayT prog) () rest


echo :: Replayable m => String -> m ()
echo = output . putStrLn

getUsersHand :: Replayable m => m RPS
getUsersHand = do
    output $ putStr "Enter a Number (1=Rock, 2=Paper, 3=Scissors): "
    userInput <- input getLine
    case userInput of
        "1" -> return Rock
        "2" -> return Paper
        "3" -> return Scissors
        _ -> do
            echo "You entered a wrong number."
            getUsersHand

getNpcsHand :: Replayable m => m RPS
getNpcsHand = do
    hand :: Int <- randomR (1, 3)
    return $ case hand of
        1 -> Rock
        2 -> Paper
        3 -> Scissors
        _ -> error "Program is wrong"

userWin :: RPS -> RPS -> Result
userWin Rock     Scissors = UserWin
userWin Scissors Paper    = UserWin
userWin Paper    Rock     = UserWin
userWin Rock     Rock     = Draw
userWin Scissors Scissors = Draw
userWin Paper    Paper    = Draw
userWin _        _        = UserLose

outputResult :: Replayable m => Result -> m ()
outputResult UserWin  = echo "You win!"
outputResult Draw     = echo "Draw."
outputResult UserLose = echo "You lose.."

script :: Replayable m => m ()
script = do
    echo "Let's play Rock-paper-scissors."
    result <- userWin <$> getUsersHand <*> getNpcsHand
    outputResult result
    output $ putStr "Play again? [Y/n] :"
    again <- input getLine
    case again of
        "n" -> echo "Quit."
        _   -> script

main :: IO ()
main = do
    let gen = R.mkStdGen 12345
    log <- play gen script
    putStrLn "-------- Log Dump ----------------"
    print log
    putStrLn "-------- REPLAY ----------------"
    replay log script



