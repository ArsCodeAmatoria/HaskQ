-- | HaskQ Framework Core
-- 
-- This module provides the main interface for the HaskQ quantum computing framework,
-- combining elegant Haskell DSL with high-performance Rust simulation.
module HaskQ.Framework.Core
  ( -- * Core Types
    Quantum
  , QuantumT
  , Qubit(..)
  , QReg(..)
  , Circuit
  
  -- * Quantum Monad
  , runQuantum
  , runQuantumT
  , withSimulator
  , getState
  , measure
  , measureAll
  
  -- * Qubit Management
  , qubits
  , qubit
  , allocQubit
  , allocQubits
  
  -- * Gate Operations
  , gate
  , (#)
  , (|>)
  
  -- * Basic Gates
  , i, x, y, z
  , h, s, t
  , rx, ry, rz, phase
  
  -- * Multi-Qubit Gates
  , cnot, cz, swap
  , toffoli, fredkin
  , controlled
  
  -- * Circuit Composition
  , circuit
  , sequential
  , parallel
  , conditional
  
  -- * State Access
  , amplitudes
  , probabilities
  , expectation
  
  -- * Advanced Operations
  , qft
  , inverseQft
  , groverOperator
  , teleport
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Exception (bracket)
import Data.Complex
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import qualified HaskQ.Framework.Internal.FFI as FFI

-- | Quantum computation monad transformer
type QuantumT = ReaderT QuantumEnv (StateT QuantumState (ExceptT QuantumError IO))

-- | Quantum computation monad
type Quantum = QuantumT ()

-- | Quantum environment
data QuantumEnv = QuantumEnv
  { envSimulator :: FFI.SimulatorId
  , envNumQubits :: Int
  } deriving (Show)

-- | Quantum computation state
data QuantumState = QuantumState
  { stateRegister :: QReg
  , stateCircuit  :: [GateOp]
  } deriving (Show)

-- | Quantum errors
data QuantumError
  = SimulatorError String
  | QubitError String
  | GateError String
  | MeasurementError String
  deriving (Show, Eq)

-- | Quantum register
newtype QReg = QReg { unQReg :: Vector Qubit }
  deriving (Show, Eq)

-- | Quantum bit with type-level tracking
newtype Qubit = Qubit { qubitIndex :: Int }
  deriving (Show, Eq, Ord)

-- | Quantum circuit representation
type Circuit = [GateOp]

-- | Gate operations
data GateOp
  = SingleGate SingleGateType Qubit
  | RotationGate RotationType Double Qubit
  | TwoQubitGate TwoQubitType Qubit Qubit
  | ThreeQubitGate ThreeQubitType Qubit Qubit Qubit
  | ControlledGate Qubit GateOp
  | MeasureGate Qubit
  deriving (Show, Eq)

-- | Single-qubit gate types
data SingleGateType = I | X | Y | Z | H | S | T
  deriving (Show, Eq)

-- | Rotation gate types
data RotationType = RX | RY | RZ | Phase
  deriving (Show, Eq)

-- | Two-qubit gate types
data TwoQubitType = CNOT | CZ | SWAP
  deriving (Show, Eq)

-- | Three-qubit gate types
data ThreeQubitType = Toffoli | Fredkin
  deriving (Show, Eq)

-- | Run a quantum computation
runQuantum :: Int -> Quantum a -> IO (Either QuantumError a)
runQuantum numQubits comp = runQuantumT numQubits comp ()

-- | Run a quantum computation with environment
runQuantumT :: Int -> QuantumT env a -> env -> IO (Either QuantumError a)
runQuantumT numQubits comp env = do
  simResult <- FFI.createSimulator numQubits
  case simResult of
    Nothing -> return $ Left $ SimulatorError "Failed to create simulator"
    Just simId -> bracket
      (return simId)
      FFI.destroySimulator
      (\sid -> do
        let qenv = QuantumEnv sid numQubits
            qstate = QuantumState (QReg $ V.fromList $ map Qubit [0..numQubits-1]) []
        result <- runExceptT $ evalStateT (runReaderT comp env) qstate
        return result)

-- | Execute computation with managed simulator
withSimulator :: Int -> (QuantumEnv -> Quantum a) -> IO (Either QuantumError a)
withSimulator numQubits f = runQuantum numQubits (ask >>= f)

-- | Get current quantum state
getState :: Quantum QuantumState
getState = lift get

-- | Measure a single qubit
measure :: Qubit -> Quantum Bool
measure q = do
  env <- ask
  result <- liftIO $ FFI.measureAll (envSimulator env)
  case result of
    Nothing -> throwE $ MeasurementError "Failed to measure qubit"
    Just bits -> 
      if qubitIndex q < length bits
        then return $ bits !! qubitIndex q
        else throwE $ QubitError "Qubit index out of bounds"

-- | Measure all qubits
measureAll :: Quantum [Bool]
measureAll = do
  env <- ask
  result <- liftIO $ FFI.measureAll (envSimulator env)
  case result of
    Nothing -> throwE $ MeasurementError "Failed to measure all qubits"
    Just bits -> return bits

-- | Qubit register creation
qubits :: Int -> Quantum QReg
qubits n = do
  env <- ask
  if n <= envNumQubits env
    then return $ QReg $ V.fromList $ map Qubit [0..n-1]
    else throwE $ QubitError "Not enough qubits available"

-- | Single qubit access
qubit :: Int -> Quantum Qubit
qubit i = do
  env <- ask
  if i < envNumQubits env
    then return $ Qubit i
    else throwE $ QubitError "Qubit index out of bounds"

-- | Allocate a new qubit
allocQubit :: Quantum Qubit
allocQubit = qubit 0  -- Simplified for now

-- | Allocate multiple qubits
allocQubits :: Int -> Quantum QReg
allocQubits = qubits

-- | Apply a gate operation
gate :: GateOp -> Quantum ()
gate gateOp = do
  env <- ask
  success <- liftIO $ applyGateFFI (envSimulator env) gateOp
  if success
    then lift $ modify $ \s -> s { stateCircuit = stateCircuit s ++ [gateOp] }
    else throwE $ GateError $ "Failed to apply gate: " ++ show gateOp

-- | Gate application operator
(#) :: GateOp -> Qubit -> Quantum ()
(#) gateOp q = case gateOp of
  SingleGate gt _ -> gate $ SingleGate gt q
  RotationGate rt angle _ -> gate $ RotationGate rt angle q
  _ -> throwE $ GateError "Invalid gate for single qubit application"

-- | Sequential composition operator
(|>) :: Quantum a -> Quantum b -> Quantum b
(|>) = (>>)

-- | Basic single-qubit gates
i, x, y, z, h, s, t :: Qubit -> Quantum ()
i q = gate $ SingleGate I q
x q = gate $ SingleGate X q
y q = gate $ SingleGate Y q
z q = gate $ SingleGate Z q
h q = gate $ SingleGate H q
s q = gate $ SingleGate S q
t q = gate $ SingleGate T q

-- | Rotation gates
rx, ry, rz, phase :: Double -> Qubit -> Quantum ()
rx angle q = gate $ RotationGate RX angle q
ry angle q = gate $ RotationGate RY angle q
rz angle q = gate $ RotationGate RZ angle q
phase angle q = gate $ RotationGate Phase angle q

-- | Two-qubit gates
cnot, cz, swap :: Qubit -> Qubit -> Quantum ()
cnot control target = gate $ TwoQubitGate CNOT control target
cz control target = gate $ TwoQubitGate CZ control target
swap q1 q2 = gate $ TwoQubitGate SWAP q1 q2

-- | Three-qubit gates
toffoli, fredkin :: Qubit -> Qubit -> Qubit -> Quantum ()
toffoli c1 c2 target = gate $ ThreeQubitGate Toffoli c1 c2 target
fredkin control t1 t2 = gate $ ThreeQubitGate Fredkin control t1 t2

-- | Controlled gate construction
controlled :: Qubit -> GateOp -> Quantum ()
controlled control gateOp = gate $ ControlledGate control gateOp

-- | Circuit construction
circuit :: [Quantum ()] -> Quantum ()
circuit = sequence_

-- | Sequential gate application
sequential :: [Quantum ()] -> Quantum ()
sequential = sequence_

-- | Parallel gate application (simplified)
parallel :: [Quantum ()] -> Quantum ()
parallel = sequence_  -- In a full implementation, this would apply gates in parallel

-- | Conditional gate application
conditional :: Quantum Bool -> Quantum () -> Quantum () -> Quantum ()
conditional condition thenAction elseAction = do
  result <- condition
  if result then thenAction else elseAction

-- | Get quantum state amplitudes
amplitudes :: Quantum (Vector (Complex Double))
amplitudes = do
  env <- ask
  result <- liftIO $ FFI.getAmplitudes (envSimulator env)
  case result of
    Nothing -> throwE $ SimulatorError "Failed to get amplitudes"
    Just amps -> return $ V.convert amps

-- | Get measurement probabilities
probabilities :: Quantum (Vector Double)
probabilities = do
  env <- ask
  result <- liftIO $ FFI.getProbabilities (envSimulator env)
  case result of
    Nothing -> throwE $ SimulatorError "Failed to get probabilities"
    Just probs -> return $ V.convert probs

-- | Calculate expectation value
expectation :: (Vector (Complex Double) -> Complex Double) -> Quantum (Complex Double)
expectation observable = do
  amps <- amplitudes
  return $ observable amps

-- | Quantum Fourier Transform
qft :: QReg -> Quantum ()
qft (QReg qs) = qftImpl (V.toList qs)
  where
    qftImpl [] = return ()
    qftImpl (q:rest) = do
      h q
      mapM_ (\(k, qk) -> controlled qk (RotationGate Phase (pi / 2^k) q)) (zip [2..] rest)
      qftImpl rest

-- | Inverse Quantum Fourier Transform
inverseQft :: QReg -> Quantum ()
inverseQft qreg = qft qreg  -- Simplified - should reverse operations

-- | Grover operator
groverOperator :: QReg -> [Int] -> Quantum ()
groverOperator (QReg qs) marked = do
  -- Oracle
  mapM_ (\i -> if i < V.length qs then z (qs V.! i) else return ()) marked
  -- Diffuser
  mapM_ h (V.toList qs)
  mapM_ x (V.toList qs)
  -- Multi-controlled Z (simplified)
  mapM_ z (V.toList qs)
  mapM_ x (V.toList qs)
  mapM_ h (V.toList qs)

-- | Quantum teleportation protocol
teleport :: Qubit -> Qubit -> Qubit -> Quantum (Bool, Bool)
teleport alice bob charlie = do
  h alice
  cnot alice bob
  cnot alice charlie
  h alice
  m1 <- measure alice
  m2 <- measure charlie
  if m2 then x bob else return ()
  if m1 then z bob else return ()
  return (m1, m2)

-- | Helper function to apply gates via FFI
applyGateFFI :: FFI.SimulatorId -> GateOp -> IO Bool
applyGateFFI simId gateOp = case gateOp of
  SingleGate I q -> FFI.applyI simId (qubitIndex q)
  SingleGate X q -> FFI.applyX simId (qubitIndex q)
  SingleGate Y q -> FFI.applyY simId (qubitIndex q)
  SingleGate Z q -> FFI.applyZ simId (qubitIndex q)
  SingleGate H q -> FFI.applyH simId (qubitIndex q)
  SingleGate S q -> FFI.applyS simId (qubitIndex q)
  SingleGate T q -> FFI.applyT simId (qubitIndex q)
  RotationGate RX angle q -> FFI.applyRX simId angle (qubitIndex q)
  RotationGate RY angle q -> FFI.applyRY simId angle (qubitIndex q)
  RotationGate RZ angle q -> FFI.applyRZ simId angle (qubitIndex q)
  RotationGate Phase angle q -> FFI.applyPhase simId angle (qubitIndex q)
  TwoQubitGate CNOT c t -> FFI.applyCNOT simId (qubitIndex c) (qubitIndex t)
  TwoQubitGate CZ c t -> FFI.applyCZ simId (qubitIndex c) (qubitIndex t)
  TwoQubitGate SWAP q1 q2 -> FFI.applySwap simId (qubitIndex q1) (qubitIndex q2)
  ThreeQubitGate Toffoli c1 c2 t -> FFI.applyToffoli simId (qubitIndex c1) (qubitIndex c2) (qubitIndex t)
  _ -> return False  -- Other gates not implemented yet

-- | Utility functions
instance MonadIO m => MonadIO (ReaderT r (StateT s (ExceptT e m))) where
  liftIO = lift . lift . lift

throwE :: Monad m => e -> ExceptT e m a
throwE = Control.Monad.Trans.Except.throwE 