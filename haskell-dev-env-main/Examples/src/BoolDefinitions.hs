module BoolDefinitions where 

    data ParseTree = AndNode ParseTree ParseTree |
                     OrNode ParseTree ParseTree |
                     NotNode ParseTree | 
                     LetNode ParseTree ParseTree ParseTree |
                     CallNode ParseTree ParseTree |
                     LambdaNode ParseTree ParseTree |
                     ValueNode ValueType |
                     IdNode String | 
                     ParenthesizedNode ParseTree |
                     EmptyNode
                        deriving(Show)

    type Environment = [(String,ValueType)]

    data ClosureType = Closure String ParseTree Environment
                    deriving(Show) 
                    
    data ValueType = BoolType Bool |
                     ClosureType String ParseTree Environment 
        deriving(Show)