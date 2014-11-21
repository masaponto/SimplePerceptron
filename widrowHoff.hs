import Graphics.EasyPlot
import Data.List


-- inner product
innerP :: (Num a) => [a] -> [a] -> a
innerP a b = sum $ zipWith (*) a b


-- calcuate eerror
calcError :: [Double] -> (Double,[Double]) -> Double
calcError w xsi
    | fst xsi == 1 = ret - 1
    | otherwise = ret
    where ret = innerP w $ snd xsi


-- judge is allowable error
isEnough :: Double -> Double -> Bool
isEnough e x
    | abs x <= e = True
    | otherwise = False


-- update
wUpdate :: [Double] -> Double -> Double -> [(Double, [Double])] -> [Double]
wUpdate w r v xs = zipWith (-) w $ map ((r*).innerP e ) xp
    where e = map (calcError w) xs
          xp = transpose $ map snd xs

-- loop training
train :: [Double] -> Double -> Double -> Double -> [(Double, [Double])]-> [Double]
train w r v e xs
    | not $ all (isEnough e) $ map (calcError w ) xs = train (wUpdate w r v xs) r v e xs
    | otherwise = w


-- discriminal function
discriFuncP :: [Double] -> Double -> Double -> Double -> [(Double,[Double])] -> Double -> Double
discriFuncP wi r v e xs x = a * x + b
    where a = - (ws !! 1) / (ws !! 2)
          b = - head ws / (ws !! 2)
          ws = train wi r v e xs


-- list of data to vector
data2vecs :: [(Double, [Double])] -> [(Double, Double)]
data2vecs datas = zip as bs
    where as = map ((!! 1).snd) datas
          bs = map ((!! 2).snd) datas


-- class separation datalist
classSepa :: Double -> [(Double, [Double])] -> [(Double, [Double])]
classSepa no = filter (\t -> fst t == no)


-- plot discriminal function
plotDiscriFunc :: [Double] -> Double -> Double -> Double -> [(Double,[Double])] -> String -> IO Bool
plotDiscriFunc wi r v e xs ns = plot (PNG ns) [ Function2D [Title $ "discriminal" ++ rate ++ defaultW ++ threshould, Color Red ]
                                                           [Range (-3) 3] (discriFuncP wi r v e xs)
                                          , Data2D [Title "class1", Color Black] [] class1Points
                                          , Data2D [Title "class0", Color Blue] [] class0Points]
    where class1Points = data2vecs $ classSepa 1 xs
          class0Points = data2vecs $ classSepa 0 xs
          defaultW = ", w =" ++ show wi
          rate = ", r = " ++ show r
          threshould = ", v = " ++ show v



main :: IO()
main = do
  putStrLn "please input default weight vector, which type is [Double]"
  w <- getLine
  putStrLn "Please input learning rate, which type is [Double]"
  r <- getLine
  putStrLn "Please input threshould, which type is Double"
  v <- getLine
  putStrLn "Please input allowe error , which type is Double "
  e <- getLine
  putStrLn "Please input learning data, which type is [(Double,[Double])]"
  line <- getLine
  putStrLn "Please input save file name, which type is String"
  fName <- getLine
  putStrLn "start lerning"
  _ <- plotDiscriFunc  (read w :: [Double]) (read r :: Double) (read v :: Double) (read e :: Double) (read line :: [(Double,[Double])]) fName
  putStrLn "done!!"
  return ()
