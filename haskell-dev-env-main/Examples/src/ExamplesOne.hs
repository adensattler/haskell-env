module ExamplesOne where


    -- cons operator (:) : builds lists for you
    --      1:2:3:[]
    --

    len :: Num p => [a] -> p
    len [] = 0
    len (q : qs) = 1 + len qs

    doubleList :: [Integer] -> [Integer]
    doubleList [] = []
    doubleList (q : qs) = (2 * q) : doubleList qs

    squareList :: [Integer] -> [Integer]
    squareList [] = []
    squareList (q : qs) = (q * q) : squareList qs

    modifyList :: (Integer -> Integer) -> [Integer] -> [Integer]
    modifyList f [] = []
    modifyList f (q : qs) = (f q) : modifyList f qs

    greaterThan2 :: [Integer] -> [Integer]
    greaterThan2 [] = []
    greaterThan2 (q : qs) = if q > 2
                                then q : greaterThan2 qs
                                else greaterThan2 qs