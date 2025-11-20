module QubitOperators where
import Data.Complex


{- A pure quantum program on n qubits is a unitary linear operator U: C^{2^n} -> C^{2^n}. 

 We can built any unitary from 1-Q unitaries and a single 1-Q unitary (e.g. CNOT) through tensor products and composition. Tensor product of an m-qubit and an n-qubit unitary yields an m+n-qubit unitary, and omposing two n-qubit unitaries yields another n-qubit unitary. Thus, no matter how many operations a pure quantum program involves, it is still simply a unitary linear operator.

 A general quantum program can include measurement steps. A measurement collapses parts of the   wavefunction to a classical result. Note that even if one measures only a single qubit, it changes the full n-qubit state: it also affects which values *any other entangled qubit can  take*. This is a non-unitary operation: measuring  qubit k to the classical value v acts as a *projection* of the n-qubit state onto the subspace in which qubit k has value v, followed by re-normalization. 
-}

type RealT = Double  -- Can be replaced by e.g. exact fractions or constructive reals
type ComplexT = Complex RealT

{-| The Operator type class defines the interface to unitary operators. We will implement it using different representations (matrices, tensor networks, stabilizers, etc.). |-}
class Semigroup o => Operator o where
  compose    :: o -> o -> o   -- Sequential composition 
  tensorProd :: o -> o -> o   -- Tensor product ⊗
  directSum  :: o -> o -> o   -- Direct sum     ⊕
  adj        :: o -> o        -- Adjoint: Inverse for unitary operators, bra <-> ket for states

  compose = (<>) -- Default semigroup compose operator. Operators form a SG under composition.

  -- Syntactic sugar in Unicode and ASCII
  (⊗), (⊕), (<.>), (<+>) :: o -> o -> o
  (⊗)   = tensorProd
  (⊕)   = directSum
  (<.>) = tensorProd 
  (<+>) = directSum  

{-| The "Op" type is a symbolic unitary operator, which just builds an abstract syntax tree (AST).
    It provides building blocks for building any n-qubit unitary operator.  |-}
data Op 
  = I | X | SX | Y | Z | H | R Op RealT  -- 1-qubit gates
  | C Op | SWAP                          -- 2-qubit gates
  | Tensor Op Op | DirectSum Op Op       -- Building multi-qubit operators
  | Compose Op Op                        -- Building multi-step  operators
  | Inverse Op                           -- Every unitary has an inverse (the adjoint)
  deriving (Show,Eq)

instance Semigroup Op where
  (<>) = Compose

instance Operator Op where
  tensorProd = Tensor
  directSum  = DirectSum
  adj        = Inverse

-- Quantum programs including measurement (linear, non-reversible)
data Step
  = Unitary Op    -- A unitary quantum program
  | Measure [Int] -- Measurement of qubits ks
  deriving (Show, Eq)

type Program = [Step]

infixl 8 ⊗, <.>
infixl 7 ⊕, <+>






