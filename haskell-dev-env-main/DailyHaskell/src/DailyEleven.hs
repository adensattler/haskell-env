module DailyEleven where

    -- Checks if the first functor law holds for a given functor value.
    -- Parameters:
    --   - functor: the functor value to be tested
    -- Result:
    --   - True if fmap id functor == functor, otherwise False
    firstFunctorLaw :: (Eq (f a), Functor f) => f a -> Bool
    firstFunctorLaw functor = fmap id functor == functor

    -- Checks if the second functor law holds for given functions and a functor value.
    -- Parameters:
    --   - f: the first function to be applied
    --   - g: the second function to be applied
    --   - functor: the functor value to be tested
    -- Result:
    --   - True if fmap (f . g) functor == fmap f (fmap g functor), otherwise False
    secondFunctorLaw :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
    secondFunctorLaw f g functor = fmap (f . g) functor == fmap f (fmap g functor)

    -- Increments an integer wrapped in a Maybe context.
    -- Parameters:
    --   - maybeInt: the Maybe Integer value to be incremented
    -- Result:
    --   - A Maybe Integer with the value incremented by 1, or Nothing if maybeInt is Nothing
    incInteger :: Maybe Integer -> Maybe Integer
    incInteger = fmap (+1)

    -- Doubles an integer wrapped in a Maybe context.
    -- Parameters:
    --   - maybeInt: the Maybe Integer value to be doubled
    -- Result:
    --   - A Maybe Integer with the value doubled, or Nothing if maybeInt is Nothing
    dubInteger :: Maybe Integer -> Maybe Integer
    dubInteger = fmap (*2)
