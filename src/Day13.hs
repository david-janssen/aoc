{-# OPTIONS_GHC -Wno-all #-}
module Day13

where


import Prelude hiding ()

import RIO.List (delete, sort, find, findIndex, (\\))
import RIO.List.Partial (maximum, tail, (!!), minimum)


--------------------------------------------------------------------------------
-- | Setup the basic data

-- | The data for this problem is some timestamp and some collection of busses
type Dat = (Int, [Maybe Int])

-- | How to parse the datafile
datP :: Parser Dat
datP = do
  t  <- lexeme intP
  let bus = Just <$> intP <|> Nothing <$ string "x"
  bs <- sepBy bus (string ",")
  pure $ (t, bs)

-- | Load the data from disc
load :: IO (Int, [Maybe Int])
load = parseDat "day13_times.txt" datP

loadExample :: IO (Int, [Maybe Int])
loadExample = parseDat "day13_example.txt" datP

loadExample2 :: IO (Int, [Maybe Int])
loadExample2 = parseDat "day13_example2.txt" datP

--------------------------------------------------------------------------------
-- | Solve the first problem

-- | Get the first occurence of a sequence of steps after some value
getNext :: ()
  => Int -- ^ Time
  -> Int -- ^ Period
  -> Int -- ^ Result
getNext t p = let (d, m) = t `divMod` p in
  if m == 0 then t else (d + 1) * p

-- | Some basic arithmetic
solve :: IO Int
solve = do
  (t, s) <- second catMaybes <$> load
  let (t', p) = minimum $ map (\p -> (getNext t p, p)) s
  pure $ p * (t' - t)

--------------------------------------------------------------------------------
-- | Solve the second problem
--
-- This gave me huge problems, so now I break it down and do it manually. This
-- whole solution relies on the fact that the busses are prime numbers

-- | Return the distance between a and the first (n * b) where (n * b) > a
(~~) :: Int -> Int -> Int
a ~~ b = let (d, m) = a `divMod` b in if m == 0 then 0 else ((d + 1) * b) - a

-- | Find where 2 prime numbers have a fixed delay
findD :: ()
  => Int -- ^ First period
  -> Int -- ^ Second period
  -> Int -- ^ Delay
  -> Int -- ^ Point where first occurs
findD a b d = let d' = d `mod` b in
   (fromJust . find ((d' ==) . (~~ b)) $ [a, a * 2 ..])


-- inp :: [(Int, Int)]
-- inp = [(643,19),(509,50),(41,9),(37,56),(29,79),(23,42),(19,0),(17,36),(13,37)]


-- go :: [(Int, Int)] -> Int
-- go [

solve2 :: IO Int
solve2 = do

  let xs = [(643,19),(509,50),(41,9),(37,56),(29,79),(23,42),(19,0),(17,36),(13,37)]

  -- First of all we take the largest number, 643. There are 4 numbers that
  -- should co-occur with 643. That should be: 37, 23, 19, 17
  --
  -- PROOF:
  -- 56 - 19 =  37 ..  37 % 37 = 0
  -- 42 - 19 =  23 ..  23 % 23 = 0
  -- 0  - 19 = -19 .. -19 % 19 = 0
  -- 36 - 19 =  17 ..  17 % 17 = 0
  --
  -- Therefore, the first moment all these numbers cooccur is 0, and the next
  -- period of their cooccurence is: t1 = 643 * 37 * 23 * 19 * 17 = 176743339

  let t1 = 176743339

  -- Then we find the first moment where the difference between t1 and 509 is
  -- (50 - 19 = 31)
  --
  -- t2 = 5832530187
  --
  -- As to be expected, t2 ~~ (643, 37, 23, 19, 17) all equals 0
  --                    t2 ~~ 509                       equals 31
  let t2 = findD t1 509 31
  let d2 = t1 * 509

  -- Now t2 is the first time our desired first set of constraints occurs, but
  -- the next time it will occur is at (t1 * 509) + t2, so we now look for a t3
  -- in this list.
  --
  -- So now we look where t2's coincide with (41, 9 - 19) == (41, 31)
  let t3 = fromJust . find ((31 ==) . (~~ 41)) $ [t2, t2 + d2 ..]
  let d3 = d2 * 41

  -- Rinse and repeat, this time for (29, 79 - 19) == (29, 2)
  let t4 = fromJust . find ((2 ==) . (~~ 29)) $ [t3, t3 + d3 ..]
  let d4 = d3 * 29

  -- And again, for (13, 37 - 19) == (13, 5)
  let t5 = fromJust . find ((5 ==) . (~~ 13)) $ [t4, t4 + d4 ..]

  -- pure t4
  pure $ t5 - 19

--   print $ 509
--   let t1 = findD 643 509 (50 - 19)
--   let p1 = 643 * 509               -- Whenever this signal repeats
--   print $ tshow t1 <> " % " <> tshow p1

--   print $ 41
--   let t2 = findD t1 41 (9 - 19)
--   let p2 = p1 * 41
--   print $ tshow t2 <> " % " <> tshow p2

--   print $ 37
--   let t3 = findD t2 37 (56 - 19)
--   let p3 = p2 * 37
--   print $ tshow t3 <> " % " <> tshow p3

--   print $ 29
--   let t4 = findD t3 29 (79 - 19)
--   let p4 = p3 * 29
--   print $ tshow t4 <> " % " <> tshow p4

--   print $ 23
--   let t5 = findD t4 23 (42 - 19)
--   let p5 = p4 * 23
--   print $ tshow t5 <> " % " <> tshow p5

--   print $ 19
--   let t6 = findD t5 19 (0 - 19)
--   let p6 = p5 * 19
--   print $ tshow t6 <> " % " <> tshow p6

--   print $ 17
--   let t7 = findD t6 17 (36 - 19)
--   let p7 = p6 * 17
--   print $ tshow t7 <> " % " <> tshow p7

--   -- print $ 13
--   -- let t8 = findD t7 13 (37 - 19 - 13)
--   -- let p8 = p7 * 13
--   -- print $ tshow t8 <> " % " <> tshow p8

--   pure $ 0 --t8 - 19
-- -- -- | Factorize an int
-- -- factors :: Int -> [Int]
-- -- factors n = go n 2 []
-- --   where
-- --     go 1 _ acc = acc
-- --     go n f acc = let (d, m) = n `divMod` f in
-- --           if m == 0 then go d f (f:acc) else go n (f + 1) acc

-- -- -- | Find the period of when 2 sequences synchronize
-- -- --
-- -- -- So if you map (`mod` p) over [t1,t2..] the result is a sequence of
-- -- -- differences that is periodic. The period is equal to the numerator times the
-- -- -- denominator divided by any shared factors.
-- -- sync :: Int -> Int -> Int
-- -- sync t p = product $ t:(factors p \\ factors t)

-- -- -- | Get the time between some time and the next occurence of some periodic signal
-- -- getWait :: Int -> Int -> Int
-- -- getWait t p = let (d, m) = t `divMod` p in
-- --   if m == 0 then 0 else p - m

-- -- -- | Infinite list of all waits between 2 values
-- -- waits :: ()
-- --   => Int   -- ^ Period of the thing that should happen first
-- --   -> Int   -- ^ Period of the thing that should happen after
-- --   -> [Int]
-- -- waits a b = map ((`getWait` b) . (a*)) [0..]

-- -- -- | Find where a wait between 2 sequences is some fixed number
-- -- findDelay :: ()
-- --   => Int -- ^ Period of the thing that should happen first
-- --   -> Int -- ^ Period of the thing that should happen after
-- --   -> Int -- ^ Time required between a and b
-- --   -> Int -- ^ When 'a' happens
-- -- findDelay a b d = (a *) . fromJust . findIndex (== d) $ waits a b

-- -- -- | Run a series of constraints-tests on some integer
-- -- test :: ()
-- --   => Int          -- ^ The number to test
-- --   -> [(Int, Int)] -- ^ Series of (period, offset) pairs
-- --   -> Bool         -- ^ Whether all periods fall on offsets
-- -- test n = all $ \(p, o) -> (n + o) `mod` p == 0

-- -- -- | Recursively search for a number that satisfies some distancing constrains
-- -- calc :: [(Int, Int)] -> Int
-- -- calc (y:ys) = go y ys
-- --   where
-- --     go (p, _) [] = p
-- --     go (p, o) ((p', o'):rst) = let p'' = sync p p'
-- --                                    o'' = findDelay p p' (o - o')
-- --                                in go (p'', o'') rst
-- --       -- d' = sync n p
-- --       -- -- in find
-- --       -- in undefined

-- -- search :: [(Int, Int)] -> Int
-- -- search []            = undefined
-- -- search ((p1, d1):xs) = go p1 d1 xs
-- --   where
-- --     go _ d []            = d
-- --     go p d ((p',d'):rst) = let
-- --       nxtP = sync p p'
-- --       -- xs   = [d, d+p..]
-- --       -- nxtD = fromJust $ find ((d' ==) . (`getWait` p')) xs
-- --       nxtD = findNext p p' d d'
-- --       in traceShow (take 5 xs) $ go nxtP nxtD rst

-- -- findNext :: Int -> Int -> Int -> Int -> Int
-- -- findNext p p' d d' = go 0 [d, d+p..]
-- --   where go n (x:rst) = if
-- --           | n == 1000000         -> traceShow x   $ go 0 (x:rst)
-- --           | x `getWait` p' == d' -> trace "found" $ x
-- --           | otherwise            -> go (n + 1) rst

-- -- -- solve2 :: IO Int
-- -- solve2 = do
-- --   -- Load the data
-- --   (_, raw) <- load
-- --   -- (_, raw) <- loadExample2
-- --   -- Tidy and sort the data, extracting the largest period
-- --   let cs = reverse . sort . catMaybes
-- --          . map (\(i, a) -> (,i) <$> a)
-- --          $ zip [0..] raw
-- --   print cs

-- --   let n  = productOf (folded . _1) cs
-- --   let ys = map ((n `div`) . fst) cs
-- --   print n
-- --   print ys
-- --   -- let (_, d1) = head cs
-- --   -- let ns = flip map cs $ \(p, d) -> (p, if d < d1 then p + d - d1 else d - d1)
-- --   -- print cs
-- --   -- print ns
-- --   -- print d1
-- --   -- print $ search ns - d1
-- --   -- pure $ calc ns
-- --   -- -- Renormalize the data to the largest factor
-- --   -- -- let go (p, d) =
-- --   -- let ns = flip map cs $ \(p, d) -> (p, if d < d1 then p + d - d1 else d - d1)
-- --   -- print (p1, d1)
-- --   -- print cs
-- --   -- print ns
-- --   -- Tidy and sort the data, extracting the 2 largest periods and when they occur
-- --   -- let ((p1,d1):(p2,d2):cs) = reverse . sort . catMaybes
-- --   --                          . map (\(i, a) -> (,i) <$> a)
-- --   --                          $ zip [0..] raw

-- --   -- Find the first sync period and occurrence of the required delay between p1 and p2
-- --   -- let t1 = (findDelay p1 p2 (d2 - d1)) - d1
-- --   -- let d  = sync p1 p2
-- --   -- undefined
-- --   -- -- Find the first sync period and occurence of the required delay between t

-- --   -- print $ (t1, d)
-- --   -- print $ cs
-- --   -- -- Start looking
-- --   -- let go i n = do
-- --   --       when (n `mod` 10000 == 0) $ print i
-- --   --       if test i cs then pure i else go (i + d) (n + 1)

-- --   -- go t1 0
-- --   -- case find (\i -> test i cs) [t1, t1 + d..] of
-- --   --   Just x -> print x
-- --   --   Nothing -> undefined
-- --   -- print (t1, d)

-- --   -- print (p1, d1)
-- --   -- print (p2, d2)

-- --   -- print cs
-- --   -- -- Extract the 2 largest constraints
-- --   -- let (p1, d1) = cs !! 0
-- --   -- let (p2, d2) = cs !! 1
-- --   -- let cs' = drop 2 cs

-- --   -- -- Find the periodicity of the delay between the 2 slowest busses
-- --   -- let ds = delays p1 p2
-- --   -- let ix = (+1) . fromJust . findIndex (== ds !! 0) $ tail ds
-- --   -- let sp = ix * p1

-- --   -- print sp

-- --   -- -- RETURN HERE

-- --   -- -- Set up the call to search for n
-- --   -- let go n i = do
-- --   --       when (i `mod` 1000 == 0) $ print n
-- --   --       if test n cs'
-- --   --         then pure n
-- --   --         else go (n + sp) (i + 1)

-- --   -- -- S

-- --   -- pure $ find (d1 - d2 ==) ds

-- --   -- Setup all the times to check
-- --   -- let ts = [d1, (sp + d1) ..]

-- --   -- pure $ find (flip test cs') ts
-- --   --
-- --   -- print $ (p1, d1)
-- --   -- print $ (p2, d2)
-- --   -- print cs'
-- --   -- print ix

-- -- -- | Find the time between 2 busses
-- -- --
-- -- -- delays :: Int -> Int -> [Int]
-- -- -- delays p1 p2 = map (\c -> getNext p2 c - c)  $ [0, p1..]

-- --   -- let cs = [(17, 0), (13, 2), (19, 3)]
-- --   -- let cs = [(1789, 0), (37, 1), (47, 2), (1889, 3)]

-- --   -- Setup the constraints and the steps to check
-- --   -- let (p, d) = maximum cs
-- --   -- let cs' = reverse . sort $ delete (p, d) cs
-- --   -- pure cs'
-- --   -- Setup some printing and a function to check values
-- --   -- let go n = do
-- --   --       when (n `mod` 500000000 <= p) $ print n
-- --   --       if test n cs' then pure n else go $ n + p

-- --   -- go (p - d)
-- --   -- pure cs



-- -- -- 35500000403
-- -- -- 100000000000000

-- -- foo :: Int -> Int -> IO ()
-- -- foo t p = do
-- --   let n = 30
-- --   let ts = take n [0,t..]
-- --   let ds = map (`getWait` p) ts
-- --   print $ ts
-- --   print $ ds

-- -- -- 26091319915828
-- -- -- 100000000000000

-- -- (~~) :: Int -> Int -> (Int, Int)
-- -- a ~~ b = (a `div` b, a `getWait` b)

-- -- -- search ::

-- --       -- go (sync p p') ()
-- --       -- let nxtP = sync p p'
-- --       --                          nxtP =

-- -- bar :: IO ()
-- -- bar = do
-- --   let l = [(59,0),(31,2),(19,3),(13,10),(7,3)]

-- --   print $ search l

-- --   -- let (p1, d1) = (59, 0)
-- --   -- let (p2, d2) = (31, 2)
-- --   -- let (p3, d3) = (19, 3)
-- --   -- let (p4, d4) = (13, 10)
-- --   -- let (p5, d5) = (7, 3)

-- --   -- let p12 = sync p1 p2
-- --   -- let d12 = findDelay p1 p2 d2
-- --   -- print $ "Period of coincidence: " <> tshow p12
-- --   -- print $ "First occasion:        " <> tshow d12

-- --   -- let xs = [d12, d12 + p12..]
-- --   -- let p123 = sync p12 p3
-- --   -- let d123 = fromJust $ find ((d3 ==) . (`getWait` p3)) xs
-- --   -- print $ "Period of coincidence: " <> tshow p123
-- --   -- print $ "First occasion:        " <> tshow d123


-- --   -- let xs = [d123, d123 + p123..]
-- --   -- let p1234 = sync p123 p4
-- --   -- let d1234 = fromJust $ find ((d4 ==) . (`getWait` p4)) xs
-- --   -- print $ "Period of coincidence: " <> tshow p1234
-- --   -- print $ "First occasion:        " <> tshow d1234

-- --   -- let xs = [d1234, d1234 + p1234..]
-- --   -- let p12345 = sync p1234 p5
-- --   -- let d12345 = fromJust $ find ((d5 ==) . (`getWait` p5)) xs
-- --   -- print $ "Period of coincidence: " <> tshow p12345
-- --   -- print $ "First occasion:        " <> tshow d12345
-- --   -- print $ take 10 l

-- --   -- let p123 = sync p12 p3
-- --   -- let d123 = findDelay p12 p3

-- --   -- search [649, 649 + 1828..]
-- --   -- 649
