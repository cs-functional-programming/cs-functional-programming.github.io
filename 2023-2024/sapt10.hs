import System.Random

twoCoins :: StdGen -> (Bool, Bool, StdGen)
twoCoins g =
  let (b1, g1) = random g in
    let (b2, g2) = random g1 in
      (b1, b2, g2)

fourCoins :: StdGen -> (Bool, Bool, Bool, Bool, StdGen)
fourCoins g1 =
  let (b1, b2, g2) = twoCoins g1 in
    let (b3, b4, g3) = twoCoins g2 in
      (b1, b2, b3, b4, g3)

-- main :: IO ()
-- main = do
--       g1 <- getStdGen
--       (x1, g2) <- return $ randomR (1 :: Int, 10) g1
--       (x2, g3) <- return $ randomR (1 :: Int, 10) g2
--       putStrLn $ show x1
--       putStrLn $ show x2
--       g1' <- getStdGen
--       (x1', g2') <- return $ randomR (1 :: Int, 10) g1'
--       (x2', g3') <- return $ randomR (1 :: Int, 10) g2'
--       putStrLn $ show x1'
--       putStrLn $ show x2'
      
-- main :: IO ()
-- main = do
--       g1 <- getStdGen
--       (x1, g2) <- return $ randomR (1 :: Int, 10) g1
--       (x2, g3) <- return $ randomR (1 :: Int, 10) g2
--       putStrLn $ show x1
--       putStrLn $ show x2
--       g1' <- newStdGen
--       (x1', g2') <- return $ randomR (1 :: Int, 10) g1'
--       (x2', g3') <- return $ randomR (1 :: Int, 10) g2'
--       putStrLn $ show x1'
--       putStrLn $ show x2'

convert :: String -> Maybe Int
convert s = let l = reads s :: [(Int, String)] in
              case l of
                [] -> Nothing
                [(x, rest)] ->
                  if rest == "" && 1 <= x && x <= 10 then
                    Just x
                  else
                    Nothing
                _ -> Nothing

guess :: Int -> IO ()
guess x = do
  putStrLn "La ce numar intre 1 si 10 crezi ca m-am gandit?"
  line <- getLine
  case convert line of
    Nothing -> do
      putStrLn "Nu am inteles. Mai incearca."
      guess x
    Just y -> do
      if x == y then
        putStrLn "Exact, ai ghicit!"
      else if x < y then do
        putStrLn "Prea mare! Mai incearca."
        guess x
      else do
        putStrLn "Prea mic! Mai incearca."
        guess x
        
main :: IO ()
main = do
  g <- getStdGen
  (x, _) <- return $ randomR (1 :: Int, 10) g
  guess x

  