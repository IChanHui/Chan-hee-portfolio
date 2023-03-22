
module MonadTransformersTest where

import Control.Monad.Trans.Reader 
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS
import qualified Control.Monad.Trans.RWS.Lazy as G

type Output = Reader Int Int

test :: Output
test = do
    lamb <- ask
    x <- return $ lamb + 10
    x'' <- testReader2 x
    return x''


testReader :: Output
testReader = do 
    lamb <- ask 
    x <- return $ lamb + 10
    x' <- testMul x
    x'' <- testReader2 x'
    return x''
    
    
testMul :: Int -> Output
testMul x = do 
    lamb <- ask
    return $ x * lamb



testReader2 :: Int -> Output
testReader2 x = do
    lamb <- ask
    return $ 2*x + lamb


runR :: Int 
runR = runReader testReader lambda

lambda :: Int 
lambda = 10




-- testState :: State Int Int 
-- testState = do 
--     x <- state $ \s -> (10, s)
--     x <- state $ \s -> (x+5, s)
--     x <- state $ \s -> (s+x, s)
--     return x 



testState :: State Int Int
testState = do 
    modify $ \s -> s+1
    s' <- get
    x <- return 10
    return $ x + s'

runS :: (Int, Int)
runS = runState testState 11


testStateIO :: StateT Int IO ()
testStateIO = do 
    s <- get
    put $ s + 1
    s' <- get 
    liftIO $ putStrLn $ "print in stateT " ++ show s'

runSIO :: IO ((), Int) 
runSIO = runStateT testStateIO 0



testWriter :: Writer [Int] Int
testWriter = do 
    tell []
    x <- return 10
    tell [x]
    tell [x]
    tell [x]
    return x



runW :: (Int, [Int])
runW = runWriter testWriter 


testRWS :: G.RWS Int [Int] Int Int
testRWS = do
    -- s1 <- G.get
    r1 <- G.ask 
    -- let sum = s1 + r1 
    G.modify $ \s1 -> s1+r1
    -- G.put sum 
    sum <- G.get
    G.tell [sum] 
    return 10

runG :: (Int, Int, [Int])
runG = G.runRWS testRWS 5 5




-- rsFoo :: StateT Int (ReaderT Int (Writer [Int])) Int
rsFoo :: StateT Int (Reader Int) Int
rsFoo = do
    x <- lift ask
    modify $ \s -> s+5
    return $ x+100

-- runReader (runStateT rsFoo 1) 3


printFoo :: Show a => StateT a IO ()
printFoo = do
    s <- get 
    liftIO $ print s

