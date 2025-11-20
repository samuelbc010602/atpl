module PrettyOp where
import QubitOperators
import Data.List (intercalate)

showOp :: Op -> String
showOp op = case op of
    C a             -> "C ("++ showOp a ++ ")"
    a `Tensor`    b -> "(" ++ showOp a ++ " ⊕ " ++ showOp b ++ ")"
    a `DirectSum` b -> "(" ++ showOp a ++ " ⊕ "  ++ showOp b ++ ")"
    a `Compose`   b -> showOp a ++ " <> " ++ showOp b
    Inverse a       -> "(adj " ++ showOp a ++ ")"
    _               -> show op

showStep :: Step -> String
showStep (Measure ks) = "Measure " ++ show ks
showStep (Unitary op) = "Unitary $ " ++ showOp op

showProgram :: Program -> String
showProgram steps = intercalate "\n" [ "step" ++ show i ++ " = " ++ (showStep step)
                     | (i :: Int,step) <- zip [1..] steps
                    ]

printOp :: Op -> IO ()
printOp = putStrLn . showOp