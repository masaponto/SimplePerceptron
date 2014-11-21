import Graphics.EasyPlot

-- inner product
innerP :: (Num a) => [a] -> [a] -> a
innerP a b = sum $ zipWith (*) a b


-- step function
step :: Double -> Double
step n
    | n >= 0 = 1
    | otherwise = 0

-- discriminal function
discriFunc :: [Double] -> [Double] -> Double -> Double
discriFunc w x v = innerP w x - v


-- update weight
wUpdate :: Double -> Double -> [Double] -> (Double, [Double]) -> [Double]
wUpdate r v w xsi = zipWith (+) w $ map (r * (d - y) *) $ snd xsi
    where d = fst xsi
          y = step (discriFunc w (snd xsi) v)


-- is discriminal sucsess
isMatch :: [Double] -> Double -> (Double, [Double]) -> Bool
isMatch w v xsi
    | fst xsi == step (discriFunc w (snd xsi) v)  = True
    | otherwise = False


-- training
train :: [Double] -> Double -> Double ->  [(Double,[Double])] -> [Double]
train w r v xs
    | not $ all (isMatch w v) xs = train (foldl (wUpdate r v) w xs) r v xs
    | otherwise = w


-- change discriminal function for plot
discriFuncP :: [Double] -> Double -> Double -> [(Double,[Double])] -> Double -> Double
discriFuncP w r v xs x = a * x + b
    where a = - (ws !! 1) / (ws !! 2)
          b = - head ws / (ws !! 2)
          ws = train w r v xs


-- list of data to vector
data2vecs :: [(Double, [Double])] -> [(Double, Double)]
data2vecs datas = zip as bs
    where as = map ((!! 1).snd) datas
          bs = map ((!! 2).snd) datas


-- class separation datalist
classSepa :: Double -> [(Double, [Double])] -> [(Double, [Double])]
classSepa no = filter (\t -> fst t == no)


-- plot discriminal function
plotDiscriFunc :: [Double] -> Double -> Double -> [(Double,[Double])] -> String -> IO Bool
plotDiscriFunc wi r v xs ns = plot (PNG ns) [ Function2D [Title $ "discriminal" ++ rate ++ defaultW ++ threshould, Color Red ]
                                                           [Range (-3) 3] (discriFuncP wi r v xs)
                                          , Data2D [Title "class1", Color Black] [] class1Points
                                          , Data2D [Title "class0", Color Blue] [] class0Points]
    where class1Points = data2vecs $ classSepa 1 xs
          class0Points = data2vecs $ classSepa 0 xs
          defaultW = ", w =" ++ show wi
          rate = ", r = " ++ show r
          threshould = ", v = " ++ show v

main :: IO Bool
main = do
  putStrLn "please input default weight vector, which type is [Double]"
  w <- getLine
  putStrLn "Please input learning rate, which type is [Double]"
  r <- getLine
  putStrLn "Please input threshould, which type is Double"
  v <- getLine
  putStrLn "Please input learning data, which type is [(Double,[Double])]"
  line <- getLine
  putStrLn "Please input save file name, which type is String"
  fName <- getLine
  putStrLn "start lerning"
  _ <- plotDiscriFunc  (read w :: [Double]) (read r :: Double) (read v :: Double) (read line :: [(Double,[Double])]) fName
  putStrLn "done!!"
  return True
