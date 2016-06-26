"Two cayley graphs for Alt(5):"

A5 _ AlternatingGroup new: 5.
 Alt((1 to: 5))
a _ Permutation cycles: #((1 2 3 4 5)).
 (1 2 3 4 5)
b _ Permutation cycles: #((1 2) (3 4)).
 (1 2)(3 4)
A5 cayleyGraphMorph: {a.b} :: hideLabels "truncated icosahedron"

a _ Permutation cycles: #((1 2 3)).
 (1 2 3)
b _ Permutation cycles: #((1 5) (2 4)).
 (1 5)(2 4)
A5 cayleyGraphMorph: {a.b} :: hideLabels "truncated dodecahedron"

