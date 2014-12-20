import Graphics.EasyPlot
import System.Random

type Vector a = [a]

-- vector expression
-- innner product
(/*/) :: (Fractional a) => Vector a -> Vector a -> a
(/*/) xs ys = sum $ zipWith (*) xs ys

-- sum
(/+/) :: (Fractional a) => Vector a -> Vector a -> Vector a
(/+/) xs ys = zipWith (+) xs ys

-- time
(*/) :: (Fractional a) => a -> Vector a -> Vector a
(*/) a xs = map (a*) xs


-- step function
step :: (Fractional a ,Ord a) => a -> a -> a
step n v
    | n >= v = 1
    | otherwise = 0


-- discriminal function
g :: (Fractional a) => Vector a -> Vector a -> a -> a
g w x v = w /*/ x - v


-- update weight
updateW :: (Fractional a, Ord a) => a -> a -> Vector a -> (a, Vector a) -> Vector a
updateW r v w xs = w /+/ ( ( r * (d - y) ) */ x)
    where y = step ( g w x v ) v
          d = fst xs
          x = snd xs


-- check results is correct
isCorrect :: (Fractional a, Ord a) => Vector a -> a -> (a, Vector a) -> Bool
isCorrect w v xs
    | d == step (g w x v) v = True
    | otherwise = False
    where d = fst xs
          x = snd xs


-- training
train :: (Fractional a, Ord a) => Vector a -> a -> a -> [(a, Vector a)] -> Vector a
train w r v xss
     | not $ all (isCorrect w v) xss = train (foldl (updateW r v) w xss) r v xss
     | otherwise = w


-- discriminal function for plot
g' :: (Fractional a, Ord a) => Vector a -> a -> a -> [(a , Vector a)] -> a -> a
g'  w r v xss x = a * x + b
    where a = - (ws !! 1) / (ws !! 2)
          b = - head ws / (ws !! 2)
          ws = train w r v xss


-- change vecotr for plot
data2vecs :: [(a, Vector a)] -> [(a, a)]
data2vecs xss = zip as bs
     where as = map ((!! 1).snd) xss
           bs = map ((!! 2).snd) xss


-- class separation datalist for plot
classSepa :: (Fractional a, Ord a) => a -> [(a, Vector a)] -> [(a, Vector a)]
classSepa no = filter (\t -> fst t == no)


-- plot discriminal function
plotDiscriFunc :: (Fractional a, Ord a, Show a, Enum a) => Vector a -> a -> a -> [( a, Vector a)] -> IO Bool
plotDiscriFunc wi r v xss = plot (X11) [ Function2D
                                               [Title $"discriminal" ++ rate ++ threshould, Color Red] [Range (-3) 3] (g' wi r v xss)
                                           , Data2D [Title "class1", Color Black] [] class1Points
                                           , Data2D [Title "class0", Color Blue] [] class0Points ]
    where class1Points = data2vecs $ classSepa 1 xss
          class0Points = data2vecs $ classSepa 0 xss
          rate = ", r = " ++ show r
          threshould = ", v = " ++ show v

-- initialize weight vector randomly
initW :: Int -> IO (Vector Double)
initW d = newStdGen >>= return . (take d) . (randomRs (-1, 1))

-- add bius
addBius :: (Num a) => [(a, Vector a)] -> [(a, Vector a)]
addBius xss = zip (map fst xss) $ map (([1]++).snd) xss


main :: IO ()
main = do
  wi <- initW d
  _ <- plotDiscriFunc wi r v xss
  return ()
      where r = 0.5
            v = 0
            d = length $ snd $ head xss
            xss = addBius andData
            andData = [(1, [1, 1] ),
                     (0, [1, 0] ),
                     (0, [0, 1] ),
                     (0, [0, 0] )]
            orData = [(1, [1, 1] ),
                      (1, [1, 0] ),
                      (1, [0, 1] ),
                      (0, [0, 0] )]
