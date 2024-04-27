module WeeklyThree where

    data Vec = Vec [Double]

    instance Show Vec where
        show (Vec xs) = "Vec " ++ show xs

    instance Num Vec where
        (+) (Vec xs) (Vec ys) = Vec (zipWith (+) xs ys)
        (-) (Vec xs) (Vec ys) = Vec (zipWith (-) xs ys)
        (*) (Vec xs) (Vec ys) = Vec (zipWith (*) xs ys) 
        negate (Vec xs) = Vec (map negate xs) 
        abs (Vec xs) = Vec (map abs xs) 
        signum (Vec xs) = Vec (map signum xs) 
        fromInteger n = Vec (repeat (fromInteger n))

    instance Eq Vec where
        (==) (Vec xs) (Vec ys) = Vec and (zipWith (==) xs ys)

    instance Ord Vec where
        -- do i need to compare based on magnitude defined below??
        -- (>=) (Vec xs) (Vec ys) = 
        -- compare (Vec xs) (Vec ys) = compare xs ys
        -- min 
        -- max 

    class VecT a where
        magnitude :: VecT a => a -> Double
            
    instance VecT Vec where
        magnitude (Vec xs) = sqrt (sum (map (^2) xs))

    instance Semigroup Vec where
        -- put xs before <> ???
        (<>) (Vec xs) (Vec ys) = Vec (zipWith (+) xs ys)

    instance Monoid Integer where
        -- is this okay??
        mempty = Vec [0.0]


