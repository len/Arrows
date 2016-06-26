"the [7,4,3] Hamming code"
C _ LinearCode subspace: ((Field GF: 3) ** 4 span: {(1,1,1,0) % 3. (0,1,2,1) % 3}).
 [4,2,3]3

C asVectorSpace "view the code as a linear subspace of (Z/<3>)^4"
 <(1,1,1,0); (0,1,2,1)>

C dual "the code is self-dual"
 [4,2,3]3

C weightEnumerator
 x4 + 8xy3

F _ Integers new modPrime: 2.
 Z/<2>

"construct the generating matrix for the Hamming code [7,4,3]"
A _ (F ** (4@7)) new.
A addRow: (c _ (1,1,0,1,0,0,0) % 2).
3 timesRepeat: [A addRow: (c _ c >> 1)].
A
 [1101000]
[0110100]
[0011010]
[0001101]

C _ LinearCode matrix: A
 [7,4,3]2

C weightEnumerator
 x7 + 7x4y3 + 7x3y4 + y7

C dual
 [7,3,4]2

C asVectorSpace
 <1101000; 0110100; 0011010; 0001101>

C dual asVectorSpace
 <1001011; 0101110; 0010111>
