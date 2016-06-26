A4 _ AlternatingGroup on: (1 to: 4).
 Alt((1 to: 4))
a _ Permutation fromArray: (2,1,4,3).
 (1 2)(3 4)
b _ Permutation fromArray: (2,3,1,4).
 (1 2 3)
A4 cayleyGraph: {a.b.a*b} :: adjacencyMatrix: A4 elements asArray :: minimalPolynomial
 x5 - 3x4 + 2x3 - 6x2 - 3x + 9

A4 cayleyGraphMorph3D: {a.b. a*b}
