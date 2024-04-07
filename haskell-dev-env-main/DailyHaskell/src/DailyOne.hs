module DailyOne where

    -- NOTE: I chose to use Floats for more flexibility and therefore used the ** operator for exponentiation!
    
    -- Calculates the quadratic expression: a + b*x + c*x^2
    -- Params:
        -- a - The coefficient of x^0
        -- b - The coefficient of x^1
        -- c - The coefficient of x^2
        -- x - The value of x
    -- Result: The value of the quadratic expression as a Float
    quadratic :: Float -> Float -> Float -> Float -> Float
    quadratic a b c x = a + b*x + c*(x**2)

    -- Scales a 2-dimensional vector by a given factor
    -- Parameters:
        -- a - The scaling factor
        -- (x, y) - The 2-dimensional vector
    -- Result: A 2-tuple which represents the vector scaled by the value
    scaleVector :: Float -> (Float, Float) -> (Float, Float)
    scaleVector a (x, y) = (a*x, a*y)

    -- Calculates the Cartesian distance between two 3-dimensional points
    -- Parameters:
    -- (x1, y1, z1) - Coordinates of the first point in a three tuple
    -- (x2, y2, z2) - Coordinates of the second point in a three tuple
    -- Result: The Cartesian distance between the two points as a Float
    tripleDistance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
    tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt ((x2 - x1)**2 + (y2 - y1)**2 + (z2 - z1)**2)
