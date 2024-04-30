module WeeklyThree where
    -- Define a new type called Vec that contains a list of Double values
    data Vec = Vec [Double]

    -- Define how Vec values should be displayed when using the show function
    instance Show Vec where
        show (Vec xs) = "Vec " ++ show xs

    -- Implement Num typeclass for Vec
    instance Num Vec where
        -- Vec addition
        (+) (Vec xs) (Vec ys) = Vec (zipWith (+) xs ys)
        -- Vec subtraction
        (-) (Vec xs) (Vec ys) = Vec (zipWith (-) xs ys)
        -- Vec multiplication (element by element)
        (*) (Vec xs) (Vec ys) = Vec (zipWith (*) xs ys) 
        -- Negate each element of Vec
        negate (Vec xs) = Vec (map negate xs) 
        -- Take abs of each element of Vec
        abs (Vec xs) = Vec (map abs xs) 
        -- Get the signum of each element of Vec
        signum (Vec xs) = Vec (map signum xs) 
        -- Convert and Integer to a Vec by repeating the value
        fromInteger n = Vec (repeat (fromInteger n))

    -- Implement Eq typeclass for Vec
    instance Eq Vec where
        -- Check if two vectors are equal by comparing each corresponding element
        (==) (Vec xs) (Vec ys) = and (zipWith (==) xs ys)

    -- Implement Ord typeclass for Vec
    instance Ord Vec where
        -- Compare two vectors lexicographically. This accounts for all possible orderings including min and max functions
        -- See tests for verification
        compare (Vec xs) (Vec ys) = compare xs ys

    -- Define a typeclass VecT with a single function magnitude
    class VecT a where
        magnitude :: VecT a => a -> Double
    
    -- Implement VecT typeclass for Vec
    instance VecT Vec where
        -- Calculate the magnitude of a vector using Euclidean norm
        magnitude (Vec xs) = sqrt (sum (map (^2) xs))

    -- Implement Semigroup typeclass for Vec
    instance Semigroup Vec where
        -- Combine two vectors using element-wise addition
        (<>) (Vec xs) (Vec ys) = Vec (zipWith (+) xs ys)

    -- Implement Monoid typeclass for Vec
    instance Monoid Vec where
        -- Define the empty element for vectors as a vector of zeros
        mempty = Vec (repeat 0.0)


