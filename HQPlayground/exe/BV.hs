module Main where 

import HQP.QOp
import HQP.QOp.MatrixSemantics
import HQP.PrettyPrint
import System.Random(mkStdGen, randoms)


prepare :: Integer -> StateT
prepare n = prepareState (n-1) (ket [0])

prepareRFS :: Integer -> StateT
prepareRFS n = prepareState (n-1) (ket [0] ⊗ ket[0])

prepareState :: Integer -> StateT -> StateT
prepareState 0 s = s ⊗ ket [1]
prepareState n s = prepareState (n-1) (s ⊗ ket [0])

h_n :: Integer -> QOp
h_n 1 = H
h_n n = H ⊗ (h_n (n-1))

circuit2 :: Program
circuit2 = let
    h_3 = h_n 3
    
    h1 = Unitary $ h_3 ⊗ H
    oracle = Unitary $ C (I ⊗ I ⊗ X) >: I ⊗ C (I ⊗ X) >: I ⊗ I ⊗ C X
    h2 = Unitary $ h_3 ⊗ I
    in
    [h1, oracle, h2, Measure [0,1,2,3]]

circuit :: Integer -> QOp -> Program
circuit n o = let
    h = h_n n
    h1 = Unitary $ h ⊗ H
    oracle = Unitary $ o
    h2 = Unitary $ h ⊗ I
    in
    [h1, oracle, h2, Measure[0..((fromIntegral n))]]

oracle :: Integer -> [Integer] -> QOp
oracle n s = oracle_helper n s (n-1) 0

oracle_helper :: Integer -> [Integer] -> Integer -> Integer -> QOp
oracle_helper n s 0 i =
    if (s !! fromIntegral i) == 1 then (i_n (n-1)) ⊗ C X
    else i_n (n+1)
oracle_helper n s c 0 =
    if (s !! fromIntegral 0) == 1 then C (i_n (n-1) ⊗ X) >: oracle_helper n s (c-1) 1
    else oracle_helper n s (c-1) 1
oracle_helper n s c i =
    if (s !! fromIntegral i) == 1 then
        i_n i ⊗ C (i_n (n-i-1) ⊗ X) >: oracle_helper n s (c-1) (i+1)
    else
        oracle_helper n s (c-1) (i+1)

bv_oracle :: Integer -> [Integer] -> QOp
bv_oracle n s = bv_oracle_helper n s (n-1) 0

--generalize amount of extra identies added to account of y and y'

bv_oracle_helper :: Integer -> [Integer] -> Integer -> Integer -> QOp
bv_oracle_helper n s 0 i =
    if (s !! fromIntegral i) == 1 then if ((n-1)>0) then (i_n (n-1)) ⊗ C X ⊗ I else C X ⊗ I
    else
        i_n (n+2)
--bv_oracle_helper n s c 0 =
--   if (s !! fromIntegral 0) == 1 then C (i_n (n-1) ⊗ X) >: bv_oracle_helper n s (c-1) 1
--    else bv_oracle_helper n s (c-1) 1
bv_oracle_helper n s c i =
    if (s !! fromIntegral i) == 1 then
        i_n i ⊗ C (i_n (n-i-1) ⊗ X) >: bv_oracle_helper n s (c-1) (i+1)
    else
        bv_oracle_helper n s (c-1) (i+1)


rcf_oracle :: Integer -> Integer -> [[Integer]] -> QOp
rcf_oracle d n s = rcf_oracle_helper d n 0 s

rcf_oracle_helper :: Integer -> Integer -> Integer -> [[Integer]] -> QOp
rcf_oracle_helper l n i s = oracle n (s !! fromIntegral i) >: rcf_oracle_helper (l-1) n (i+1) s
rcf_oracle_helper 1 n i s = oracle n (s !! fromIntegral i)

path_circuit  :: Integer -> Integer -> QOp
path_circuit n l = xI_n 3 (n+2) >: cI_n [1] 1 0 n >: xI_n 3 (n+2) >: cI_n [0] 1 0 n
--path_circuit n l = cI_n [0] 1 0 n

unitary_g :: [Integer] -> Integer -> QOp
unitary_g s n = g s 0 n

g :: [Integer] -> Integer -> Integer -> QOp
g s i 1 = if(elem i s) then C (I ⊗ X) else (I ⊗ X)
g s i n = if(elem i s) then C (g s (i+1) (n-1)) else I ⊗ g s (i+1) (n-1)

xI_n :: Integer -> Integer -> QOp
xI_n i 1 = if (i == 1) then X else I ⊗ I
xI_n i n = if (i == n) then X ⊗ xI_n i (n-1) else I ⊗ xI_n i (n-1)

cI_n :: [Integer] -> Integer -> Integer -> Integer -> QOp
cI_n s l i 1 = if(elem i s) then C (bv_oracle 1 [0]) else I ⊗ bv_oracle 1 [0]
cI_n s l i n = if(elem i s) then C (cI_n s l (i+1) (n-1)) else I ⊗ cI_n s l (i+1) (n-1)

-- FUcntion to pass all possible paths as integer list to be given to the path circuit
-- path citcuit will recurse over each element to create the paths.
-- create_path :: Integer -> Integer -> [Integer]

{-rcf_circuit :: Integer -> QOp -> Program
rcf_circuit 0 = 
rcf_circuit d = 
    -}

i_n :: Integer -> QOp
i_n 1 = I
i_n n = I ⊗ i_n (n-1)

opI_n :: Integer -> Integer -> QOp -> QOp
opI_n i 1 op = if (i == 1) then op else I
opI_n i n op = if (i == n) then op ⊗ opI_n i (n-1) op else I ⊗ opI_n i (n-1) op

main :: IO()
main =  do
    let rng0 = randoms (mkStdGen 42) :: [Double] 
    let n = 2
    let l = 2
    let state = prepare n
    let s = [[1,1]]  -- marked states
    let staterfs = prepareRFS n
    let u_f = path_circuit 1 1
    let u_g = unitary_g [1] 2   
    let h = h_n 4
    let h1 = opI_n 3 4 H
    let hn = opI_n 2 4 H
    let hy = opI_n 1 4 H
    let circuit_fk = [Unitary $ h, Unitary $ u_f, Unitary $ h1, Unitary $ u_g, Unitary $ h1 , Unitary $ u_f , Unitary $ h1 , Unitary $ hn, Unitary $ hy, Measure[0,1,2,3]]
    --let circuit_fk = [Unitary $ unitary_g [1] 2]
    --let circuit1 = circuit n (oracle n s)
    let (end_state, _) = evalProg circuit_fk staterfs rng0
    putStr $ "Initial state:\n" ++ (showState state) ++ "\n\n"
    putStr $ "\nCircuit:\n" ++ showProgram circuit_fk ++ "\n\n"
    putStr $ "Final state:\n" ++ (showState end_state) ++ "\n\n"
    --putStr $ "H_n" showState (hadamard_n n <> state) ++ "\n\n"


{-
module Main where 

import HQP
import HQP.QOp.MatrixSemantics
import System.Random(mkStdGen, randoms)
import Data.List (foldl')
import Debug.Trace
-- import Data.List(intercalate)

apply_gate :: QOp -> Int -> Int -> Bool -> QOp
apply_gate gate level dim True = let
    sep_op = [if x == dim then gate else I | x <- [0..dim]]
    accum = scanl1 (\acc x -> acc ⊗ x)
    in last (accum sep_op)
    
apply_gate gate level dim False = let
    sep_op = [if x == level then gate else I | x <- [0..dim]]
    accum = scanl1 (\acc x -> acc ⊗ x)
    mapped = accum sep_op
    flag = if level+1 == dim then True else False
    recur = (last mapped) >: (apply_gate gate (level+1) dim flag) 
    in recur

set_C_X_y :: Int -> Int -> QOp
set_C_X_y dim loc = let
    amount = (fromIntegral (dim - 2) / 2 :: Double)
    remaining = (last $ scanl1 (\acc x -> acc ⊗ x) ([I | x <- [0..amount]]++[I,X])) 
    in if loc /= 0 then (last $ scanl1 (\acc x -> acc ⊗ x) [I | x <- [0..loc-1]]) ⊗ C (remaining) else C (remaining)

set_C_X_yp :: Int -> Int -> QOp
set_C_X_yp dim loc = let
    amount = (fromIntegral (dim - 2) / 2 :: Double)
    remaining = (last $ scanl1 (\acc x -> acc ⊗ x) ([I | x <- [0..amount]]++[X,I])) 
    in if loc /= 0 then (last $ scanl1 (\acc x -> acc ⊗ x) [I | x <- [0..loc-1]]) ⊗ C (remaining) else C (remaining)

oracle_fk :: [Int] -> QOp
oracle_fk s = let 
    dim = length s
    check_against = [y | (x,y) <- zip s [0..(dim-1)], x == 1]
    totake = ((fromIntegral (dim - 2) / 2 :: Double) - 1)
    check = map (set_C_X_yp dim) (take (floor totake) check_against)
    in last $ scanl1 (\acc x -> acc >: x) check

oracle_gk :: [Int] -> QOp
oracle_gk s = let 
    dim = length s
    check_against = [y | (x,y) <- zip s [0..(dim-1)], x == 1]
    totake = ((fromIntegral (dim - 2) / 2 :: Double) - 1)
    check = map (set_C_X_y dim) (take (floor totake) check_against)
    in last $ scanl1 (\acc x -> acc >: x) check
    
takeXPlusLast2 :: Num a => Int -> [a] -> [a]
takeXPlusLast2 x xs = (take x xs) ++ [0] ++ (drop (length xs - 2) xs)

-- n = 3
-- psi_x1,psi_x2,psi_x3,psi_x4,psi_x5,psi_x6,psi_y',psi_y


bernsteinProg :: Int -> Int -> [Int] -> Program
bernsteinProg level dim input_bits =                         
    let
        circ = Unitary $ --2d
            I ⊗ I ⊗ H ⊗ H ⊗ H ⊗ I  --apply hadamard to psi_xk+1 and psi_y'
            >: (oracle_fk input_bits) --apply U_fk+1 to psi_x1 to psi_xk+1 and psi_y'
            >: I ⊗ I ⊗ H ⊗ H ⊗ I ⊗ I  --apply hadamard to psi_xk+1
            >: (oracle_gk input_bits) --apply oracle and save result in psi_y
            >: I ⊗ I ⊗ H ⊗ H ⊗ I ⊗ I  --resetting 
            >: (oracle_fk input_bits) --resetting 
            >: I ⊗ I ⊗ H ⊗ H ⊗ H ⊗ I  --resetting 

            --1d
            >: I ⊗ I ⊗ H ⊗ I ⊗ H ⊗ I  --apply hadamard to psi_xk+1 and psi_y'
            >: (oracle_fk $ takeXPlusLast2 1 input_bits) --apply U_fk+1 to psi_x1 to psi_xk+1 and psi_y'
            >: I ⊗ I ⊗ H ⊗ I ⊗ I ⊗ I  --apply hadamard to psi_xk+1
            >: (oracle_gk $ takeXPlusLast2 1 input_bits) --apply oracle and save result in psi_y
            >: I ⊗ I ⊗ H ⊗ I ⊗ I ⊗ I  --resetting 
            >: (oracle_fk $ takeXPlusLast2 1 input_bits) --resetting 
            >: I ⊗ I ⊗ H ⊗ I ⊗ H ⊗ I  --resetting s
    in [circ, Measure $ reverse [x | x <- [0..((length input_bits)-1)]]]

create_kets :: Int -> CMat -- first dim-1 input qubits psi_xj 1<=j<=k, (dim-1)'th qubit = psi_xk+1, dim'th qubit psi_y = 1 
create_kets dim = ket ([if x < dim then 0 else 1 | x <- [0..dim]])

bernstein :: RNG -> Int -> [Int] -> (StateT, RNG)
bernstein rng dim input_bits = let
        psi     = ket (input_bits)
    in
        evalProg (bernsteinProg 0 dim input_bits) psi rng

main :: IO()
main = do
    let rng0 = randoms (mkStdGen 42) :: [Double]  

    let secret = [1,0]

    let input_bits = secret ++ [1,1]
    
    let (end_state,_) = bernstein rng0 (length input_bits) input_bits
    putStr $ "Final " ++ show (length input_bits) ++ "-qubit state:\n" ++ (showState end_state) ++ "\n\n"
-}
