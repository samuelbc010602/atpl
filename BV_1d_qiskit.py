from qiskit import QuantumCircuit
from qiskit_aer import AerSimulator
from qiskit import transpile

def oracle(qc, secret):
    for i, bit in enumerate(s):
        if bit == 1:
            qc.cx(i, n)

if __name__ == "__main__":
    s = [1,0,1,0,0,1]
    n = len(s)
    qc = QuantumCircuit(n + 1, n)

    # Prepare ancilla |-> 
    qc.x(n) #er ikke helt sikker hvorfor denne skal være der
    qc.h(n)

    # Hadamards on input
    for i in range(n):
        qc.h(i)
        
    oracle(qc,s)

    # Final Hadamards
    for i in range(n):
        qc.h(i)

    # Measure
    for i in range(n):
        qc.measure(i, i)

    sim = AerSimulator(method="stabilizer")
    compiled = transpile(qc, sim)
    result = sim.run(compiled).result()

    print(result)
