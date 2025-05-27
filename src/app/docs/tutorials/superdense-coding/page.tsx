'use client';

import React from 'react';
import Link from 'next/link';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

export default function SuperdenseCodingPage() {
  return (
    <div className="max-w-none prose dark:prose-invert">
      <h1 className="text-3xl font-bold mb-6">Superdense Coding Tutorial</h1>
      
      <p className="text-lg mb-6">
        Superdense coding is a quantum communication protocol that allows us to transmit two classical bits
        of information by sending just one qubit, leveraging quantum entanglement as a resource.
        It's the dual of quantum teleportation and showcases the power of quantum information.
      </p>
      
      <InfoBox type="note">
        <p>This tutorial assumes familiarity with quantum entanglement and Bell states. If you're new to these concepts, 
        we recommend starting with our <Link href="/docs/tutorials/bell-states" className="text-indigo-600 dark:text-indigo-400 hover:underline">Bell States Tutorial</Link>.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">The Superdense Coding Protocol</h2>
      
      <p className="mb-4">
        Superdense coding allows us to transmit two classical bits from a sender (Alice) to a receiver (Bob) 
        by manipulating and sending a single qubit. Here's how it works:
      </p>
      
      <ol className="list-decimal ml-8 space-y-2 mb-6">
        <li>Alice and Bob share an entangled pair of qubits (a Bell state)</li>
        <li>Alice wants to send two classical bits (00, 01, 10, or 11) to Bob</li>
        <li>Based on these two bits, Alice performs one of four possible quantum operations on her qubit</li>
        <li>Alice sends her qubit to Bob</li>
        <li>Bob now has both qubits of the previously shared entangled pair</li>
        <li>Bob performs a joint measurement on both qubits</li>
        <li>The measurement result reveals the two classical bits that Alice encoded</li>
      </ol>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Circuit for Superdense Coding</h2>
      
      <p className="mb-4">
        The superdense coding protocol can be represented as the following quantum circuit:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg text-center my-6">
        <pre className="whitespace-pre">
{`Alice:                       Bob:
|0⟩ ──H──X───[Apply Ui]─────────•─────H─────M──
                                 │           │
|0⟩ ─────────────────────────────X───────────M──

Operations Ui based on classical bits xy:
00: I (Identity)
01: X (Bit-flip)
10: Z (Phase-flip)
11: ZX (Both phase and bit-flip)`}
        </pre>
      </div>
      
      <p className="mb-4">
        In this circuit, Alice and Bob initially share a Bell state (created by applying a Hadamard gate followed by a CNOT).
        Alice applies one of four operations (I, X, Z, or ZX) to her qubit based on the two classical bits she wants to send.
        Bob receives Alice's qubit, applies a CNOT followed by a Hadamard, and measures both qubits to recover the two classical bits.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Implementing Superdense Coding in HaskQ</h2>
      
      <p className="mb-4">
        Let's implement the superdense coding protocol step by step using HaskQ:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`import HaskQ.Prelude
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import HaskQ.Simulator.Circuit

-- Function to create a Bell state (|00⟩ + |11⟩)/√2
createBellState :: Circ (Qubit, Qubit)
createBellState = do
  -- Create two qubits in the |0⟩ state
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Apply Hadamard to the first qubit
  q1' <- hadamard q1
  
  -- Apply CNOT with q1 as control and q2 as target
  (q1'', q2') <- controlled not q1' q2
  
  -- Return the entangled qubits
  pure (q1'', q2')

-- Encode two classical bits into Alice's qubit
encodeMessage :: Qubit -> (Bool, Bool) -> Circ Qubit
encodeMessage qubit (bit1, bit2) = do
  -- Apply operations based on the two classical bits:
  -- 00: I (Do nothing)
  -- 01: X (Bit-flip)
  -- 10: Z (Phase-flip)
  -- 11: ZX (Both phase and bit-flip)
  qubit' <- case (bit1, bit2) of
    (False, False) -> pure qubit            -- 00: Identity (do nothing)
    (False, True)  -> pauliX qubit          -- 01: X gate (bit-flip)
    (True, False)  -> pauliZ qubit          -- 10: Z gate (phase-flip)
    (True, True)   -> do                    -- 11: ZX (both phase and bit-flip)
                       qubit' <- pauliZ qubit
                       pauliX qubit'
  
  pure qubit'

-- Decode the message by applying joint measurement
decodeMessage :: (Qubit, Qubit) -> Circ (Bool, Bool)
decodeMessage (qubit1, qubit2) = do
  -- Apply CNOT
  (qubit1', qubit2') <- controlled not qubit1 qubit2
  
  -- Apply Hadamard to the first qubit
  qubit1'' <- hadamard qubit1'
  
  -- Measure both qubits
  (m1, _) <- measure qubit1''
  (m2, _) <- measure qubit2'
  
  -- Convert measurements to booleans
  let bit1 = case m1 of Zero -> False; One -> True
      bit2 = case m2 of Zero -> False; One -> True
  
  pure (bit1, bit2)

-- Complete superdense coding protocol
superdenseCoding :: (Bool, Bool) -> Circ (Bool, Bool)
superdenseCoding messageBits = do
  -- Step 1: Create a Bell state (shared between Alice and Bob)
  (aliceQubit, bobQubit) <- createBellState
  
  -- Step 2: Alice encodes her message into her qubit
  aliceQubit' <- encodeMessage aliceQubit messageBits
  
  -- Step 3: Alice sends her qubit to Bob (in simulation, Bob now has both qubits)
  
  -- Step 4: Bob decodes the message
  decodeMessage (aliceQubit', bobQubit)

-- Main function to demonstrate superdense coding
main :: IO ()
main = do
  -- Try all possible 2-bit messages
  let messages = [(False, False), (False, True), (True, False), (True, True)]
  
  -- Run the protocol for each message
  results <- mapM runProtocol messages
  
  -- Print the results
  putStrLn "Superdense Coding Results:"
  mapM_ (\\(msg, decoded) -> do
          putStrLn $ "Original message: " ++ bitsToString msg
          putStrLn $ "Decoded message:  " ++ bitsToString decoded
          putStrLn $ "Success: " ++ show (msg == decoded)
          putStrLn "") 
        (zip messages results)
  
  -- Check if all messages were successfully transmitted
  let success = all (\\(msg, decoded) -> msg == decoded) (zip messages results)
  putStrLn $ "Overall success: " ++ show success

-- Helper function to run the protocol and extract the result
runProtocol :: (Bool, Bool) -> IO (Bool, Bool)
runProtocol messageBits = do
  let result = simulateCircuit 2 $ do
        decodedBits <- superdenseCoding messageBits
        pure decodedBits
  
  -- The simulation returns a tuple of booleans in the custom data field
  -- Let's extract it (this is simulation-specific)
  pure $ customData result

-- Helper function to convert bits to a string representation
bitsToString :: (Bool, Bool) -> String
bitsToString (b1, b2) = map (\\b -> if b then '1' else '0') [b1, b2]

-- For this simulation, we're using the customData field to store the boolean result
-- In a real HaskQ implementation, this would be handled differently
customData :: SimulationResult -> (Bool, Bool)
customData _ = (False, False)  -- This is a placeholder, in a real implementation
                               -- we would extract the actual result from the simulation`}
        className="my-6"
      />
      
      <h3 className="text-xl font-bold mt-6 mb-3">Extended Superdense Coding</h3>
      
      <p className="mb-4">
        We can extend the superdense coding protocol to handle multiple bits by using multiple Bell pairs:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Encode multiple bits using multiple Bell pairs
encodeMultipleBits :: [Qubit] -> [Bool] -> Circ [Qubit]
encodeMultipleBits qubits bits = do
  -- Group the bits in pairs
  let bitPairs = groupPairs bits
  
  -- Apply operations to each qubit
  zipWithM encodeQubit qubits bitPairs
  where
    -- Group a list of bits into pairs
    groupPairs :: [Bool] -> [(Bool, Bool)]
    groupPairs [] = []
    groupPairs [b] = [(b, False)]  -- Pad with 0 if odd number
    groupPairs (b1:b2:bs) = (b1, b2) : groupPairs bs
    
    -- Apply the appropriate operation to a single qubit
    encodeQubit :: Qubit -> (Bool, Bool) -> Circ Qubit
    encodeQubit q (bit1, bit2) = encodeMessage q (bit1, bit2)

-- Decode multiple bits from multiple qubits
decodeMultipleBits :: [(Qubit, Qubit)] -> Circ [Bool]
decodeMultipleBits qubitPairs = do
  -- Decode each pair of qubits
  bitPairs <- mapM decodeMessage qubitPairs
  
  -- Flatten the pairs into a single list
  pure $ concat [[b1, b2] | (b1, b2) <- bitPairs]

-- Extended superdense coding for multiple bits
extendedSuperdenseCoding :: [Bool] -> Circ [Bool]
extendedSuperdenseCoding bits = do
  -- Create Bell pairs (one for each pair of bits)
  let numPairs = (length bits + 1) \`div\` 2  -- Ceiling division
  bellPairs <- replicateM numPairs createBellState
  
  -- Extract Alice's and Bob's qubits
  let aliceQubits = map fst bellPairs
      bobQubits = map snd bellPairs
  
  -- Alice encodes her message
  aliceQubits' <- encodeMultipleBits aliceQubits bits
  
  -- Alice sends her qubits to Bob
  
  -- Bob decodes the message
  decodeMultipleBits (zip aliceQubits' bobQubits)`}
        className="my-6"
      />

      <InfoBox type="warning">
        <p>The <code>customData</code> function is simplified for this tutorial. In a real HaskQ implementation, 
        you would need to extract the result from the simulation in a way that's compatible with your version 
        of the HaskQ simulator.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Detailed Explanation of the Protocol</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Step 1: Creating the Shared Bell State</h3>
      
      <p className="mb-4">
        The protocol begins with Alice and Bob sharing a Bell state, typically |Φ⁺⟩ = (|00⟩ + |11⟩)/√2.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Step 2: Alice's Encoding Process</h3>
      
      <p className="mb-4">
        Alice applies one of four operations to her qubit based on the two classical bits she wants to transmit:
      </p>
      
      <ul className="list-disc ml-8 space-y-2 mb-6">
        <li><strong>00:</strong> Identity (I) - No change to the Bell state</li>
        <li><strong>01:</strong> Bit-flip (X) - Transforms the state to (|10⟩ + |01⟩)/√2</li>
        <li><strong>10:</strong> Phase-flip (Z) - Transforms the state to (|00⟩ - |11⟩)/√2</li>
        <li><strong>11:</strong> Both phase and bit-flip (ZX) - Transforms the state to (|10⟩ - |01⟩)/√2</li>
      </ul>
      
      <p className="mb-4">
        These transformations produce the four Bell states, which are orthogonal to each other and can be distinguished by measurement.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Step 3: Transmission of the Qubit</h3>
      
      <p className="mb-4">
        Alice sends her qubit to Bob over a quantum channel. Note that only one qubit is transmitted, even though 
        two bits of classical information are being communicated.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Step 4: Bob's Decoding Process</h3>
      
      <p className="mb-4">
        After receiving Alice's qubit, Bob has both qubits of the originally shared Bell pair. He applies:
      </p>
      
      <ol className="list-decimal ml-8 space-y-2 mb-6">
        <li>A CNOT gate, using the first qubit as control and the second qubit as target</li>
        <li>A Hadamard gate on the first qubit</li>
        <li>Measurement of both qubits</li>
      </ol>
      
      <p className="mb-4">
        The measurement results correspond directly to the two classical bits Alice encoded. This works because 
        the operations performed by Bob effectively reverse the Bell state creation, causing the state to collapse 
        to the basis state corresponding to Alice's message.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Mathematical Analysis</h3>
      
      <p className="mb-4">
        Let's analyze how the transformations work:
      </p>
      
      <ol className="list-decimal ml-8 space-y-2 mb-6">
        <li>
          Starting with the Bell state |Φ⁺⟩ = (|00⟩ + |11⟩)/√2
        </li>
        <li>
          For input <strong>00</strong>: Apply I (do nothing), state remains (|00⟩ + |11⟩)/√2
        </li>
        <li>
          For input <strong>01</strong>: Apply X to Alice's qubit, state becomes (|10⟩ + |01⟩)/√2
        </li>
        <li>
          For input <strong>10</strong>: Apply Z to Alice's qubit, state becomes (|00⟩ - |11⟩)/√2
        </li>
        <li>
          For input <strong>11</strong>: Apply ZX to Alice's qubit, state becomes (|10⟩ - |01⟩)/√2
        </li>
      </ol>
      
      <p className="mb-4">
        These four resulting states are the Bell basis states, which can be perfectly distinguished by Bob's 
        measurement after applying the CNOT and Hadamard operations.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Practical Considerations</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Limitations and Challenges</h3>
      
      <ul className="list-disc ml-8 space-y-2 mb-6">
        <li><strong>Quantum Noise:</strong> In real quantum systems, noise can disrupt the entangled state and introduce errors</li>
        <li><strong>Quantum Memory:</strong> Maintaining entangled qubits for long periods is challenging</li>
        <li><strong>Quantum Channel:</strong> A quantum channel is required to send the qubit from Alice to Bob</li>
        <li><strong>Pre-shared Entanglement:</strong> Alice and Bob must pre-share entangled qubits before communication</li>
      </ul>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Applications</h3>
      
      <p className="mb-4">
        Superdense coding has several potential applications in quantum information science:
      </p>
      
      <ul className="list-disc ml-8 space-y-2 mb-6">
        <li><strong>Quantum Networking:</strong> Efficient communication in quantum networks</li>
        <li><strong>Quantum Key Distribution:</strong> Component in secure communication protocols</li>
        <li><strong>Quantum Teleportation:</strong> Partner protocol to quantum teleportation</li>
        <li><strong>Quantum Computing:</strong> Theoretical component in certain quantum algorithms</li>
      </ul>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Superdense Coding vs. Quantum Teleportation</h2>
      
      <p className="mb-4">
        Superdense coding and quantum teleportation are complementary protocols:
      </p>
      
      <ul className="list-disc ml-8 space-y-2 mb-6">
        <li><strong>Superdense coding:</strong> Uses 1 shared Bell pair + 1 quantum bit transmission to send 2 classical bits</li>
        <li><strong>Quantum teleportation:</strong> Uses 1 shared Bell pair + 2 classical bit transmissions to send 1 quantum bit</li>
      </ul>
      
      <p className="mb-4">
        Both protocols showcase the power of quantum entanglement as a resource for information processing.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Summary</h2>
      
      <p className="mb-4">
        In this tutorial, we've explored:
      </p>
      
      <ul className="list-disc ml-8 space-y-2 mb-6">
        <li>The superdense coding protocol and how it works</li>
        <li>Implementation in HaskQ for basic and extended versions</li>
        <li>Detailed analysis of the quantum operations involved</li>
        <li>Practical considerations and applications</li>
      </ul>
      
      <p className="mb-4">
        Superdense coding is a fundamental quantum communication protocol that demonstrates the advantage of 
        quantum information over classical information. It highlights how entanglement can be leveraged as a 
        resource to achieve communication tasks that would be impossible with classical resources alone.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Further Reading</h2>
      
      <ul className="list-disc ml-8 space-y-2 mb-6">
        <li>
          <Link href="/docs/tutorials/bell-states" className="text-indigo-600 dark:text-indigo-400 hover:underline">
            Bell States Tutorial
          </Link> - Understanding the entangled states used in superdense coding
        </li>
        <li>
          <Link href="/docs/tutorials/quantum-teleportation" className="text-indigo-600 dark:text-indigo-400 hover:underline">
            Quantum Teleportation Tutorial
          </Link> - The complementary protocol to superdense coding
        </li>
        <li>
          <Link href="/docs/core-concepts/quantum-gates" className="text-indigo-600 dark:text-indigo-400 hover:underline">
            Quantum Gates
          </Link> - More about the quantum operations used in the protocol
        </li>
      </ul>
    </div>
  );
} 