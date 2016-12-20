A4 _ AlternatingGroup on: (1 to: 4).
 Alt([1..4]) 

a _ Permutation image: #(2 1 4 3).
 (3 4)(1 2) 

b _ Permutation image: #(2 3 1 4).
 (1 2 3) 

A4 cayleyGraphMorph: {a.b. a*b}

A4 cayleyGraph: {a.b.a*b} :: adjacencyMatrix: A4 elements asArray :: minimalPolynomial
 x5 - 3x4 + 2x3 - 6x2 - 3x + 9

A4 cayleyGraphMorph: {a.b. a*b}
