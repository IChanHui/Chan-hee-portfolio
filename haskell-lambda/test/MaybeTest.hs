
module MaybeTest where 



test :: IO ()
test = do 
    _ <- putStrLn "This is maybe test"
    _ <- putStrLn "here is IO binding do block"
    added <- ioBlock 
    putStrLn $ maybe "added is zero" tellMeWhat $ maybeBlock added

    
    where 
        tellMeWhat num = show num
        -- tellMeWhat num = "I expedted number to " ++ show num 
        

test2 :: IO ()
test2 = do 
    _ <- putStrLn "This is maybe test"
    _ <- putStrLn "here is IO binding do block"
    added <- ioBlock 
    putStrLn $ case maybeBlock added of 
        Nothing -> "added is zero"
        Just num -> "I expedted number to " ++ show num 
    
    


ioBlock :: IO Int 
ioBlock = do 
    val <- return 0
    val2 <- return 0
    return (val + val2)


maybeBlock :: Int -> Maybe Int 
maybeBlock i = (Just i)

maybeBlock' :: Int -> Maybe Int 
maybeBlock' i = case i of 
    0 -> Nothing 
    i' -> Just i'

