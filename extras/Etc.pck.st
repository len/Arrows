'From Cuis 6.0 [latest update: #5866] on 19 June 2023 at 3:18:56 pm'!
'Description '!
!provides: 'Etc' 1 30!
!requires: 'Algebra' 1 7 nil!
SystemOrganization addCategory: 'Etc'!
SystemOrganization addCategory: 'Etc-Matrices'!
SystemOrganization addCategory: 'Etc-Algebras-Solvable'!
SystemOrganization addCategory: 'Etc-Algebras-Path'!
SystemOrganization addCategory: 'Etc-Lie'!


!classDefinition: #IdentityArray category: 'Etc'!
Array variableSubclass: #IdentityArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'IdentityArray class' category: 'Etc'!
IdentityArray class
	instanceVariableNames: ''!

!classDefinition: #ZechArray category: 'Etc'!
ArrayedCollection subclass: #ZechArray
	instanceVariableNames: 'logarithms table'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ZechArray class' category: 'Etc'!
ZechArray class
	instanceVariableNames: ''!

!classDefinition: #StandardBasis category: 'Etc'!
SequenceableCollection subclass: #StandardBasis
	instanceVariableNames: 'elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'StandardBasis class' category: 'Etc'!
StandardBasis class
	instanceVariableNames: ''!

!classDefinition: #GroebnerBasis category: 'Etc'!
StandardBasis subclass: #GroebnerBasis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GroebnerBasis class' category: 'Etc'!
GroebnerBasis class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeFieldTest category: 'Etc'!
TestCase subclass: #SmallPrimeFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeFieldTest class' category: 'Etc'!
SmallPrimeFieldTest class
	instanceVariableNames: ''!

!classDefinition: #ProductGroup category: 'Etc'!
Group subclass: #ProductGroup
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ProductGroup class' category: 'Etc'!
ProductGroup class
	instanceVariableNames: ''!

!classDefinition: #SemidirectProductGroup category: 'Etc'!
Group subclass: #SemidirectProductGroup
	instanceVariableNames: 'left right action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SemidirectProductGroup class' category: 'Etc'!
SemidirectProductGroup class
	instanceVariableNames: ''!

!classDefinition: #CoxeterGroup category: 'Etc-Lie'!
Group subclass: #CoxeterGroup
	instanceVariableNames: 'diagram'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterGroup class' category: 'Etc-Lie'!
CoxeterGroup class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeField category: 'Etc'!
PrimeField subclass: #SmallPrimeField
	instanceVariableNames: 'expTable logTable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeField class' category: 'Etc'!
SmallPrimeField class
	instanceVariableNames: ''!

!classDefinition: #GRAlgebra category: 'Etc-Algebras-Solvable'!
QuotientRing subclass: #GRAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'GRAlgebra class' category: 'Etc-Algebras-Solvable'!
GRAlgebra class
	instanceVariableNames: ''!

!classDefinition: #PowerAlgebra category: 'Etc'!
FiniteAlgebra subclass: #PowerAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'PowerAlgebra class' category: 'Etc'!
PowerAlgebra class
	instanceVariableNames: ''!

!classDefinition: #GAlgebra category: 'Etc-Algebras-Solvable'!
Ring subclass: #GAlgebra
	instanceVariableNames: 'representatives multiplicativeConstants additiveConstants'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'GAlgebra class' category: 'Etc-Algebras-Solvable'!
GAlgebra class
	instanceVariableNames: ''!

!classDefinition: #WeylAlgebra category: 'Etc-Algebras-Solvable'!
GAlgebra subclass: #WeylAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'WeylAlgebra class' category: 'Etc-Algebras-Solvable'!
WeylAlgebra class
	instanceVariableNames: ''!

!classDefinition: #PathAlgebra category: 'Etc-Algebras-Path'!
Ring subclass: #PathAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'PathAlgebra class' category: 'Etc-Algebras-Path'!
PathAlgebra class
	instanceVariableNames: ''!

!classDefinition: #CoxeterSystem category: 'Etc-Lie'!
Structure subclass: #CoxeterSystem
	instanceVariableNames: 'group generators matrix diagram'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterSystem class' category: 'Etc-Lie'!
CoxeterSystem class
	instanceVariableNames: ''!

!classDefinition: #Quiver category: 'Etc-Algebras-Path'!
Domain subclass: #Quiver
	instanceVariableNames: 'edges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'Quiver class' category: 'Etc-Algebras-Path'!
Quiver class
	instanceVariableNames: ''!

!classDefinition: #ColumnMajorMatrix category: 'Etc-Matrices'!
Matrix subclass: #ColumnMajorMatrix
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'ColumnMajorMatrix class' category: 'Etc-Matrices'!
ColumnMajorMatrix class
	instanceVariableNames: ''!

!classDefinition: #PermutationMatrix category: 'Etc-Matrices'!
Matrix subclass: #PermutationMatrix
	instanceVariableNames: 'permutation'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'PermutationMatrix class' category: 'Etc-Matrices'!
PermutationMatrix class
	instanceVariableNames: ''!

!classDefinition: #TriangularMatrix category: 'Etc-Matrices'!
Matrix subclass: #TriangularMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'TriangularMatrix class' category: 'Etc-Matrices'!
TriangularMatrix class
	instanceVariableNames: ''!

!classDefinition: #DiagonalMatrix category: 'Etc-Matrices'!
TriangularMatrix subclass: #DiagonalMatrix
	instanceVariableNames: 'diagonal'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'DiagonalMatrix class' category: 'Etc-Matrices'!
DiagonalMatrix class
	instanceVariableNames: ''!

!classDefinition: #LowerTriangularMatrix category: 'Etc-Matrices'!
TriangularMatrix subclass: #LowerTriangularMatrix
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'LowerTriangularMatrix class' category: 'Etc-Matrices'!
LowerTriangularMatrix class
	instanceVariableNames: ''!

!classDefinition: #ScalarMatrix category: 'Etc-Matrices'!
TriangularMatrix subclass: #ScalarMatrix
	instanceVariableNames: 'scalar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'ScalarMatrix class' category: 'Etc-Matrices'!
ScalarMatrix class
	instanceVariableNames: ''!

!classDefinition: #UpperTriangularMatrix category: 'Etc-Matrices'!
TriangularMatrix subclass: #UpperTriangularMatrix
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'UpperTriangularMatrix class' category: 'Etc-Matrices'!
UpperTriangularMatrix class
	instanceVariableNames: ''!

!classDefinition: #QuiverMorphism category: 'Etc-Algebras-Path'!
Morphism subclass: #QuiverMorphism
	instanceVariableNames: 'domain codomain images'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'QuiverMorphism class' category: 'Etc-Algebras-Path'!
QuiverMorphism class
	instanceVariableNames: ''!

!classDefinition: #GroupTuple category: 'Etc'!
GroupElement subclass: #GroupTuple
	instanceVariableNames: 'parent components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GroupTuple class' category: 'Etc'!
GroupTuple class
	instanceVariableNames: ''!

!classDefinition: #SemidirectProductElement category: 'Etc'!
GroupElement subclass: #SemidirectProductElement
	instanceVariableNames: 'parent left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SemidirectProductElement class' category: 'Etc'!
SemidirectProductElement class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeResidue category: 'Etc'!
PrimeResidue subclass: #SmallPrimeResidue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeResidue class' category: 'Etc'!
SmallPrimeResidue class
	instanceVariableNames: ''!

!classDefinition: #RelaxedAdicInteger category: 'Etc'!
RingElement subclass: #RelaxedAdicInteger
	instanceVariableNames: 'parent digits'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedAdicInteger class' category: 'Etc'!
RelaxedAdicInteger class
	instanceVariableNames: ''!

!classDefinition: #RelaxedAdicNumber category: 'Etc'!
RingElement subclass: #RelaxedAdicNumber
	instanceVariableNames: 'parent valuation unit precision'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedAdicNumber class' category: 'Etc'!
RelaxedAdicNumber class
	instanceVariableNames: ''!

!classDefinition: #RelaxedPowerSeries category: 'Etc'!
RingElement subclass: #RelaxedPowerSeries
	instanceVariableNames: 'parent coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedPowerSeries class' category: 'Etc'!
RelaxedPowerSeries class
	instanceVariableNames: ''!

!classDefinition: #GAlgebraElement category: 'Etc-Algebras-Solvable'!
RingElement subclass: #GAlgebraElement
	instanceVariableNames: 'parent representative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'GAlgebraElement class' category: 'Etc-Algebras-Solvable'!
GAlgebraElement class
	instanceVariableNames: ''!

!classDefinition: #PathAlgebraElement category: 'Etc-Algebras-Path'!
RingElement subclass: #PathAlgebraElement
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'PathAlgebraElement class' category: 'Etc-Algebras-Path'!
PathAlgebraElement class
	instanceVariableNames: ''!

!classDefinition: #GaussianElimination2 category: 'Etc'!
Echelonization subclass: #GaussianElimination2
	instanceVariableNames: 'pivots currentPivot determinantFactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GaussianElimination2 class' category: 'Etc'!
GaussianElimination2 class
	instanceVariableNames: ''!

!classDefinition: #BerlekampMassey category: 'Etc'!
Object subclass: #BerlekampMassey
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BerlekampMassey class' category: 'Etc'!
BerlekampMassey class
	instanceVariableNames: ''!

!classDefinition: #ExtendedSmithReduction category: 'Etc'!
Object subclass: #ExtendedSmithReduction
	instanceVariableNames: 'matrix elementaryDivisors L S R'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ExtendedSmithReduction class' category: 'Etc'!
ExtendedSmithReduction class
	instanceVariableNames: ''!

!classDefinition: #ModularHermiteReduction category: 'Etc'!
Object subclass: #ModularHermiteReduction
	instanceVariableNames: 'matrix HNF'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ModularHermiteReduction class' category: 'Etc'!
ModularHermiteReduction class
	instanceVariableNames: ''!

!classDefinition: #MultivariateReducer category: 'Etc'!
Object subclass: #MultivariateReducer
	instanceVariableNames: 'divisors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'MultivariateReducer class' category: 'Etc'!
MultivariateReducer class
	instanceVariableNames: ''!

!classDefinition: #GlobalMultivariateReducer category: 'Etc'!
MultivariateReducer subclass: #GlobalMultivariateReducer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GlobalMultivariateReducer class' category: 'Etc'!
GlobalMultivariateReducer class
	instanceVariableNames: ''!

!classDefinition: #NTT category: 'Etc'!
Object subclass: #NTT
	instanceVariableNames: 'modulus primitiveRoot'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'NTT class' category: 'Etc'!
NTT class
	instanceVariableNames: ''!

!classDefinition: #OEIS category: 'Etc'!
Object subclass: #OEIS
	instanceVariableNames: 'entries'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'OEIS class' category: 'Etc'!
OEIS class
	instanceVariableNames: ''!

!classDefinition: #RealBox category: 'Etc'!
Object subclass: #RealBox
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RealBox class' category: 'Etc'!
RealBox class
	instanceVariableNames: ''!

!classDefinition: #SmallBitArray category: 'Etc'!
Object subclass: #SmallBitArray
	instanceVariableNames: 'bits size'
	classVariableNames: 'One Zero'
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallBitArray class' category: 'Etc'!
SmallBitArray class
	instanceVariableNames: ''!

!classDefinition: #SmithReduction category: 'Etc'!
Object subclass: #SmithReduction
	instanceVariableNames: 'matrix invariants'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmithReduction class' category: 'Etc'!
SmithReduction class
	instanceVariableNames: ''!

!classDefinition: #SparseTuple category: 'Etc'!
Object subclass: #SparseTuple
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SparseTuple class' category: 'Etc'!
SparseTuple class
	instanceVariableNames: ''!

!classDefinition: #SturmChain category: 'Etc'!
Object subclass: #SturmChain
	instanceVariableNames: 'chain'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SturmChain class' category: 'Etc'!
SturmChain class
	instanceVariableNames: ''!

!classDefinition: #SubgroupSeries category: 'Etc'!
Object subclass: #SubgroupSeries
	instanceVariableNames: 'subgroups'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SubgroupSeries class' category: 'Etc'!
SubgroupSeries class
	instanceVariableNames: ''!

!classDefinition: #ZechArrayTest category: 'Etc'!
Object subclass: #ZechArrayTest
	instanceVariableNames: 'table'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ZechArrayTest class' category: 'Etc'!
ZechArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ZechTable category: 'Etc'!
Object subclass: #ZechTable
	instanceVariableNames: 'scalars base q q̄ minusOne successors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ZechTable class' category: 'Etc'!
ZechTable class
	instanceVariableNames: ''!

!classDefinition: #ZechTableTest category: 'Etc'!
Object subclass: #ZechTableTest
	instanceVariableNames: 'table'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ZechTableTest class' category: 'Etc'!
ZechTableTest class
	instanceVariableNames: ''!

!classDefinition: #Coechelonization category: 'Etc-Matrices'!
Object subclass: #Coechelonization
	instanceVariableNames: 'matrix echelon transformation'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'Coechelonization class' category: 'Etc-Matrices'!
Coechelonization class
	instanceVariableNames: ''!

!classDefinition: #GaussJordanCoechelonization category: 'Etc-Matrices'!
Coechelonization subclass: #GaussJordanCoechelonization
	instanceVariableNames: 'determinantFactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'GaussJordanCoechelonization class' category: 'Etc-Matrices'!
GaussJordanCoechelonization class
	instanceVariableNames: ''!

!classDefinition: #HermiteCoechelonization category: 'Etc-Matrices'!
Coechelonization subclass: #HermiteCoechelonization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'HermiteCoechelonization class' category: 'Etc-Matrices'!
HermiteCoechelonization class
	instanceVariableNames: ''!

!classDefinition: #HowellCoechelonization category: 'Etc-Matrices'!
Coechelonization subclass: #HowellCoechelonization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'HowellCoechelonization class' category: 'Etc-Matrices'!
HowellCoechelonization class
	instanceVariableNames: ''!

!classDefinition: #QuiverPath category: 'Etc-Algebras-Path'!
Object subclass: #QuiverPath
	instanceVariableNames: 'parent source walk'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'QuiverPath class' category: 'Etc-Algebras-Path'!
QuiverPath class
	instanceVariableNames: ''!

!classDefinition: #CoxeterDiagram category: 'Etc-Lie'!
Object subclass: #CoxeterDiagram
	instanceVariableNames: 'matrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterDiagram class' category: 'Etc-Lie'!
CoxeterDiagram class
	instanceVariableNames: ''!

!classDefinition: #RootSystem category: 'Etc-Lie'!
Object subclass: #RootSystem
	instanceVariableNames: 'roots ambient'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'RootSystem class' category: 'Etc-Lie'!
RootSystem class
	instanceVariableNames: ''!

!classDefinition: #RootSystemTest category: 'Etc-Lie'!
Object subclass: #RootSystemTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'RootSystemTest class' category: 'Etc-Lie'!
RootSystemTest class
	instanceVariableNames: ''!

!classDefinition: #EuclideanGaussianElimination category: 'Etc'!
ProtoObject subclass: #EuclideanGaussianElimination
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'EuclideanGaussianElimination class' category: 'Etc'!
EuclideanGaussianElimination class
	instanceVariableNames: ''!

!classDefinition: #GaussBareiss category: 'Etc'!
ProtoObject subclass: #GaussBareiss
	instanceVariableNames: 'currentPivot echelon'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GaussBareiss class' category: 'Etc'!
GaussBareiss class
	instanceVariableNames: ''!

!classDefinition: #PolynomialCode category: 'Etc'!
ProtoObject subclass: #PolynomialCode
	instanceVariableNames: 'generator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'PolynomialCode class' category: 'Etc'!
PolynomialCode class
	instanceVariableNames: ''!

!classDefinition: #CyclicCode category: 'Etc'!
PolynomialCode subclass: #CyclicCode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'CyclicCode class' category: 'Etc'!
CyclicCode class
	instanceVariableNames: ''!


!ZechArray commentStamp: '<historical>' prior: 0!
Arrays of elements of small Galois fields GF(p,r). Each element is stored as its Zech logarithm using 8 or 16 bits per element. See ZechTable.!

!StandardBasis commentStamp: '<historical>' prior: 0!
Standard bases of polynomial ideals. A standard basis is nothing but a special set of generators with the property that the leading monomials generate the leading ideal.

When the choosen monomial ordering is global (a well-ordering), these are Groebner bases and are computed with Buchberger's algorithm. For arbitrary monomial orderings, they are computed using Mora's normal form.

See also Buchberger and ExtendedBuchberger.!

!GroebnerBasis commentStamp: '<historical>' prior: 0!
Groebner bases are a special case of standard bases.

For a fixed global monomial ordering, a subset G = {g1,...,gm} of an ideal I = <f1,...,fm> of a polynomial ring is called a Groebner basis for I if every polynomial f in I is reducible by G, i.e. there exists g in G such that its leading monomial LM(g) divides LM(f).

When all generators {f1,...,fm} are linear, a Groebner basis corresponds to "row echelon form" or "triangular system"; when they are univariate, then a Groebner basis corresponds to gcd(f1,...,fm). In general, a Groebner basis for an ideal I consists of all the "smallest polynomials" in I under the given monomial ordering.

See also Buchberger and ExtendedBuchberger.!

!ProductGroup commentStamp: '<historical>' prior: 0!
Direct products of groups. Elements are tuples (see GroupTuple), and the group composition law is defined component-wise.!

!SemidirectProductGroup commentStamp: '<historical>' prior: 0!
Semidirect products of groups.!

!SmallPrimeField commentStamp: '<historical>' prior: 0!
Prime fields Z/pZ for small primes p, such that it is feasable to build exponentiation and logarithmic tables and accelerate arithmetic through table lookups.

The initialization of the tables is very fast. The main concern is memory usage. For a field Z/pZ, each of the two tables have p-1 elements. They are WordArrays (32 bits per entry), so a field Z/pZ uses about 8*(p-1) bytes of memory. For example, Z/65537 uses 524K of memory, and Z/1048583 (a 20-bits modulus) uses about 8M.
!

!ColumnMajorMatrix commentStamp: '<historical>' prior: 0!
Matrices stored in column-major format. The coefficients are stored as the concatenation of the columns, starting with the first coefficient of the first column and ending with the last coefficient of the last column.!

!GroupTuple commentStamp: '<historical>' prior: 0!
Elements of group direct products, i.e. tuples of group elements. See ProductGroup.!

!SemidirectProductElement commentStamp: '<historical>' prior: 0!
Elements of a semidirect product of groups. See SemidirectProductGroup.!

!RelaxedAdicInteger commentStamp: '<historical>' prior: 0!
Elements of the p-adic ring Zp. See AdicRing.!

!RelaxedAdicNumber commentStamp: '<historical>' prior: 0!
Elements of the p-adic field Qp. See AdicField.

A p-adic number z is represented uniquely by an integer k (the valuation) and a unit u in Zp, such that z = (p^k)*u.!

!RelaxedPowerSeries commentStamp: '<historical>' prior: 0!
Univariate formal power series with coefficients over a commutative ring. See PowerSeriesRing.!

!GaussianElimination2 commentStamp: '<historical>' prior: 0!
This is the Gaussian Elimination algorithm.

My instances perform Gaussian reduction on a matrix to compute its echelon form, linear dependency of the columns, rank, determinant, inverse, etc.

Input: a matrix with coefficients in a field;
Output: the reduced echelon form of the matrix, determinant and left inverse.

The algorithm performs O(2n^3/3) field operations for n by n matrices, but due to exponential growth of intermediate coefficients it can be impractical in many situations (for example, with rational coefficients).!

!BerlekampMassey commentStamp: '<historical>' prior: 0!
This is the Berlekamp-Massey algorithm. The initial purpose of this algorithm was to evaluate Binary BCH codes, but it has found many uses, including inverting matrices of constant diagonal, solving linear recurrences (synthesizing LFSRs with a specified output sequence), etc.

Input: a_1..a_{2n} the first 2n coefficients of a linear recursive sequence over a field K of order at most n (the minimal polynomial has degree bound n);
Output: the minimal polynomial of the sequence.

The algorithm performs O(n^2/2) multiplications; more precisely, at most 2 floor(n^2/4) - n + 1 multiplications.!

!NTT commentStamp: '<historical>' prior: 0!
Number Theoretic Transforms, the analog of DFTs for 𝔽ₚ. Special cases are the Fermat Number Transform (p = 2^(2^k) + 1) and the Mersenne Number Transform (p = 2^k - 1).

The (pseudo) number theoretic transform may be meaningful in the ring ℤ/mℤ even when the modulo m is not prime, provided a principal root of order n exists.!

!RealBox commentStamp: '<historical>' prior: 0!
Boxes in R^n built by the cartesian product of one-dimensional intervals (instances of RealInterval).

Example:
	RealBox new
		add: (RealInterval closed: -1 closed: 1);
		add: (RealInterval open: 3 open: 4);
		add: (RealInterval closed: 0 open: Infinity positive);
		yourself.
!

!SmallBitArray commentStamp: '<historical>' prior: 0!
Arrays of integer residue classes modulo 2, i.e. binary arrays, bit strings, or arrays of bits. The coefficient field is ℤ/2ℤ.

They are stored as an Integer, and they are fast and memory-efficient when they fit in a SmallInteger.!

!SmithReduction commentStamp: '<historical>' prior: 0!
This is the Smith reduction algorithm. Given a matrix with coefficients in a PID, it reduces the matrix to its Smith normal form.

The Smith normal form is very useful for working with finitely generated modules over a PID, and in particular for deducing the structure of a quotient of a free module.

See [Coh96], Algorithm 2.4.14.!

!SparseTuple commentStamp: 'len 7/21/2020 17:34:32' prior: 0!
Sparse tuples are tuples that use a Dictionary to store the components, and zero entries are not stored explicitly. This makes them more efficient when most of the entries are zero.!

!Coechelonization commentStamp: '<historical>' prior: 0!
Abstract superclass for column-style echelonization algorithms. Given a matrix A, they compute a matrix E in reduced column echelon form and a unimodular matrix T such that A*T = E.!

!GaussJordanCoechelonization commentStamp: '<historical>' prior: 0!
This is the Gauss-Jordan elimination algorithm. It performs Gaussian elimination on a matrix to compute its reduced column echelon form. It can work with matrices over fields or more general division rings such as the quaternions.

The algorithm performs O(2n^3/3) field operations for n by n matrices, but due to exponential growth of intermediate coefficients it can be impractical in many situations (for example over the rationals).!

!HermiteCoechelonization commentStamp: '<historical>' prior: 0!
This is the Hermite column reduction algorithm. Given a matrix with coefficients in a Euclidean domain, it reduces the matrix to its column-style Hermite normal form.

The algorithm is a generalization of [Coh96, Algorithm 2.4.4] to arbitrary Euclidean domains.!

!HowellCoechelonization commentStamp: '<historical>' prior: 0!
This is the Howell column reduction algorithm. Given a matrix with coefficients in a PIR with an extended GCD algorithm to compute Bézout coefficients (for example any Euclidean ring, possibly with zero divisors), it reduces the matrix to its column-style Howell normal form in O(m^2 * max(m,n)) operations. See [How86], [Sto00], [SM98], [FH16].!

!GaussBareiss commentStamp: '<historical>' prior: 0!
This is the Gauss-Bereiss algorithm, a variant of Gassian elimination. It performs division-free Gaussian elimination on a matrix with coefficients in an integral domain, and computes the echelon form.

Input: a matrix with coefficients in a an integral domain;
Output: the reduced echelon form and the determinant.

The algorithm performs O(n^3) ring operations for nxn matrices. While the standard Gaussian elimination suffers from exponential intermediate coefficients growth, the Gauss-Bareiss algorithm has bit complexity O(n^5).
!

!PolynomialCode commentStamp: '<historical>' prior: 0!
In coding theory, a polynomial code is a type of linear code whose set of valid codewords consists of those polynomials (usually of some fixed length) that are divisible by some fixed polynomial (of shorter length, called the generator polynomial).

A polynomial code is cyclic if and only if the generator polynomial divides x^n - 1. See CyclicCode.
!

!IdentityArray methodsFor: 'as yet unclassified' stamp: 'len 12/23/2021 15:35:29'!
hasEqualElements: otherCollection
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	otherCollection isSequenceable ifFalse: [^ false].
	(size _ self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) == (otherCollection at: index) ifFalse: [^ false]].
	^ true! !

!IdentityArray methodsFor: 'as yet unclassified' stamp: 'len 12/23/2021 15:37:27'!
hashQuick
	| hash size step |
	size _ self size.
	hash _ (self species hash + size hash) hashMultiply.
	step _ size < 64 ifTrue: [1] ifFalse: [size//64].
	1 to: size by: step do: [ :i | | elem |
		elem _ self at: i.
		elem == self ifFalse: [
			hash _ (hash + elem identityHash) hashMultiply]].
	^hash! !

!ZechArray methodsFor: 'accessing' stamp: 'len 5/11/2023 15:08:42'!
at: anInteger
	^ table exp: (logarithms at: anInteger)! !

!ZechArray methodsFor: 'accessing' stamp: 'len 5/11/2023 15:08:42'!
at: anInteger put: anElement
	logarithms at: anInteger put: (table log: anElement).
	^ anElement! !

!ZechArray methodsFor: 'accessing' stamp: 'len 5/11/2023 15:24:19'!
atAllPut: anElement
	logarithms atAllPut: (table log: anElement)! !

!ZechArray methodsFor: 'accessing' stamp: 'len 5/11/2023 15:08:53'!
logarithms
	^ logarithms! !

!ZechArray methodsFor: 'accessing' stamp: 'len 5/10/2023 13:06:30'!
scalars
	^ table scalars! !

!ZechArray methodsFor: 'accessing' stamp: 'len 5/11/2023 15:08:42'!
size
	^ logarithms size! !

!ZechArray methodsFor: 'accessing' stamp: 'len 5/11/2023 15:08:42'!
swap: oneIndex with: anotherIndex 
	logarithms swap: oneIndex with: anotherIndex! !

!ZechArray methodsFor: 'accessing' stamp: 'len 5/11/2023 15:13:53'!
table
	^ table! !

!ZechArray methodsFor: 'comparing' stamp: 'len 5/11/2023 17:01:46'!
= anObject
	^ self class = anObject class and: [logarithms = anObject logarithms]! !

!ZechArray methodsFor: 'comparing' stamp: 'len 5/11/2023 15:08:42'!
hash
	| answer |
	answer _ 0.
	logarithms do: [:each| each = 0 ifFalse: [answer _ answer hashMultiply + each hash]].
	^ answer! !

!ZechArray methodsFor: 'copying' stamp: 'len 5/11/2023 15:11:17'!
postCopy
	super postCopy.
	logarithms _ logarithms copy! !

!ZechArray methodsFor: 'operations' stamp: 'len 5/11/2023 15:32:39'!
product
	| answer |
	answer _ 1.
	logarithms do: [:each|
		each = 0 ifTrue: [^ self scalars zero].
		answer _ answer + each].
	^ table exp: answer - 1 \\ table q̄ + 1! !

!ZechArray methodsFor: 'operations' stamp: 'len 5/11/2023 15:25:30'!
reversed
	^ self class logarithms: logarithms reversed table: table! !

!ZechArray methodsFor: 'operations' stamp: 'len 5/11/2023 15:47:44'!
sum
	^ table exp: (logarithms inject: 0 into: [:sum :each| table op: sum plus: each])! !

!ZechArray methodsFor: 'operations' stamp: 'len 5/11/2023 15:29:57'!
weight
	^ logarithms weight! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/21/2023 00:18:59'!
· anArray
	^ table exp: (logarithms dot: anArray logarithms zechTable: table)! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/22/2023 16:54:52'!
accumulateFrom: start to: stop withMatrix: anArray timesTuple: anotherArray
	logarithms accumulateFrom: start to: stop withMatrix: anArray logarithms timesTuple: anotherArray logarithms zechTable: table! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 21:27:10'!
findFirstNonzeroFrom: start to: stop
	^ logarithms findFirstNonzeroFrom: start to: stop! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 21:27:23'!
findLastNonzeroFrom: start to: stop
	^ logarithms findLastNonzeroFrom: start to: stop! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 23:39:43'!
polynomialValue: anElement
	^ table exp: (logarithms polynomialValue: (table log: anElement) zechTable: table)! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/23/2023 10:45:31'!
reduceBy: g quotient: q
	logarithms reduceBy: g logarithms quotient: (q ifNotNil: [q logarithms]) zechTable: table! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 15:10:53'!
replaceFrom: start to: stop with: replacement startingAt: repStart
	(replacement class = self class and: [table = replacement table])
		ifFalse: [^ super replaceFrom: start to: stop with: replacement startingAt: repStart].
	logarithms replaceFrom: start to: stop with: replacement logarithms startingAt: repStart! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/12/2023 14:04:38'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2
	logarithms replaceFrom: start to: stop with: anArray1 logarithms startingAt: start1 minus: anArray2 logarithms startingAt: start2 zechTable: table! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 21:31:57'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2
	logarithms replaceFrom: start to: stop with: anArray1 logarithms startingAt: start1 plus: anArray2 logarithms startingAt: start2 zechTable: table! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 21:31:22'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: anElement
	logarithms replaceFrom: start to: stop with: anArray1 logarithms startingAt: start1 plus: anArray2 logarithms startingAt: start2 timesScalar: (table log: anElement) zechTable: table! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/12/2023 21:27:53'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2
	logarithms replaceFrom: start to: stop with: anArray1 logarithms startingAt: start1 times: anArray2 logarithms startingAt: start2 zechTable: table! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 21:32:42'!
replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: anElement
	logarithms replaceFrom: start to: stop with: anArray logarithms startingAt: srcStart timesScalar: (table log: anElement) zechTable: table
! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 21:33:09'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart
	logarithms replaceFrom: start to: stop withNegated: anArray logarithms startingAt: srcStart zechTable: table! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/12/2023 21:41:52'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2
	logarithms replaceFrom: start with: anArray1 logarithms from: start1 to: stop1 convolutionLong: anArray2 logarithms from: start2 to: stop2 zechTable: table! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/18/2023 17:33:57'!
replaceWithMatrixTranspose: anArray width: width
	logarithms replaceWithMatrixTranspose: anArray logarithms width: width! !

!ZechArray methodsFor: 'operations-low level' stamp: 'len 5/11/2023 15:42:35'!
swapFrom: start to: stop with: anArray startingAt: start2
	logarithms swapFrom: start to: stop with: anArray logarithms startingAt: start2! !

!ZechArray methodsFor: 'testing' stamp: 'len 5/11/2023 15:08:42'!
isZero
	^ logarithms isZero! !

!ZechArray methodsFor: 'testing' stamp: 'len 5/11/2023 15:08:42'!
isZeroAt: anInteger
	^ logarithms isZeroAt: anInteger! !

!ZechArray methodsFor: 'private' stamp: 'len 5/11/2023 15:09:20'!
logarithms: anArray table: aZechTable
	logarithms _ anArray.
	table _ aZechTable! !

!ZechArray methodsFor: 'private' stamp: 'len 5/11/2023 17:01:29'!
species
	^ table! !

!ZechArray class methodsFor: 'instance creation' stamp: 'len 5/11/2023 15:13:24'!
logarithms: anArray table: aZechTable
	^ self basicNew logarithms: anArray table: aZechTable! !

!StandardBasis methodsFor: 'printing' stamp: 'len 5/7/2018 23:22:28'!
printOn: aStream
	aStream nextPut: ${.
	elements do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $}! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 5/7/2018 23:22:50'!
add: aPolynomial
	^ elements add: aPolynomial! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 5/7/2018 23:21:31'!
addAll: aCollection
	^ elements addAll: aCollection! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 5/7/2018 23:22:47'!
at: anInteger
	^ elements at: anInteger! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 11/21/2015 00:10'!
first
	^ self at: 1! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 1/23/2017 11:47:32'!
indeterminates
	^ self anyOne parent indeterminates! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 1/23/2017 11:47:29'!
ordering
	"Answer the monomial ordering of the receiver."
	^ self anyOne parent ordering! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 5/7/2018 23:21:22'!
size
	^ elements size! !

!StandardBasis methodsFor: 'accessing-private' stamp: 'len 5/7/2018 23:23:36'!
elements: aCollection
	elements _ aCollection! !

!StandardBasis methodsFor: 'copying' stamp: 'len 5/10/2018 08:03:11'!
copy
	"Answer a copy of the reciever."
	^ self class new elements: elements copy! !

!StandardBasis methodsFor: 'copying' stamp: 'len 11/20/2016 15:32:11'!
copyEmpty
	^ self class new! !

!StandardBasis methodsFor: 'copying' stamp: 'len 5/7/2018 23:23:29'!
copyWithout: aPolynomial
	^ self class new elements: (elements copyWithout: aPolynomial)! !

!StandardBasis methodsFor: 'enumerating' stamp: 'len 7/8/2016 20:57'!
apply: aMorphism
	| answer |
	answer _ self copyEmpty.
	self do: [:each| answer add: (aMorphism value: each)].
	^ answer! !

!StandardBasis methodsFor: 'enumerating' stamp: 'len 5/7/2018 23:21:19'!
do: aBlock
	elements do: aBlock! !

!StandardBasis methodsFor: 'initialization' stamp: 'len 5/10/2018 08:03:16'!
initialize
"	elements _ SortedCollection sortBlock: [:a :b| a degree <= b degree]."
"	elements _ SortedCollection sortBlock: [:a :b| a leadingMonomial <= b leadingMonomial]."
	elements _ OrderedCollection new! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/28/2020 17:47:47'!
elementAt: anArray
	^ (1 to: anArray size) sum: [:i| (self at: i) * (anArray at: i)]! !

!StandardBasis methodsFor: 'operations' stamp: 'len 1/26/2017 22:51:21'!
leadingTerms
	"Answer the standard basis made of all leading monomials of polynomials in the receiver."
	| answer |
	answer _ self class new.
	self do: [:each| | lt | lt _ each leadingTerm. (answer reduces: lt) ifFalse: [answer add: lt]].
	^ answer! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/6/2023 18:14:22'!
macaulayMatrix
	| monomials R |
	monomials _ Set new.
	self do: [:each| monomials addAll: each monomials].
	monomials _ monomials asSortedCollection: [:a :b| a >= b].
	R _ self anyOne parent scalars.
	^ R ^ monomials size to: R ^ self size fill: [:i :j| (self at: i) at: (monomials at: j)]! !

!StandardBasis methodsFor: 'operations' stamp: 'len 12/17/2021 14:44:42'!
minimal
	"Answer the minimal Groebner basis of the receiver."
	| heads goods answer |
	self isMinimal ifTrue: [^ self].
	heads _ elements collect: [:each| each leadingMonomial].
	goods _ (1 to: heads size) asSet.
	heads withIndexDo: [:head :i|
		(goods anySatisfy: [:one| i ~= one and: [(heads at: one) | head]])
			ifTrue: [goods remove: i]].
	answer _ self class new.
	(goods asSortedCollection: [:a :b| "make it triangular for lex" (elements at: a) leadingMonomial >= (elements at: b) leadingMonomial])
		do: [:i| answer add: (elements at: i) normalized].
	^ answer! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/10/2019 08:31:13'!
reduce1: aPolynomial 
	"Answer Mora's normal form of aPolynomial with respect to the receiver)."
	| h L M |
	h _ aPolynomial.
	L _ OrderedCollection withAll: self.
	([h isZero or: [(M _ L select: [:g| g leadingMonomial | h leadingMonomial]) isEmpty]])
		whileFalse:
			[| g |
			g _ M detectMin: [:each| each ecart].
			g ecart > h ecart ifTrue: [L add: h].
			h _ h - (g * (h leadingMonomial / g leadingMonomial) * (h leadingCoefficient / g leadingCoefficient))].
	^ h! !

!StandardBasis methodsFor: 'operations' stamp: 'len 7/29/2021 17:12:23'!
reduce: aPolynomial 
	"Answer Mora's normal form of aPolynomial with respect to the receiver."
	| h L lh g ecartg |
	h _ aPolynomial.
	L _ OrderedCollection withAll: self.
	[h isZero ifTrue: [^ h].
	lh _ h leadingMonomial.
	g _ nil.
	ecartg _ -1.
	L do: [:each|
		| ecart |
		(each leadingMonomial | lh and: [ecart _ each ecart. g isNil or: [ecart < ecartg]])
			ifTrue: [g _ each. ecartg _ ecart]].
	g isNil ifTrue: [^ h].
	ecartg > h ecart ifTrue: [L add: h].
	h _ h + (g * (lh / g leadingMonomial) * (h leadingCoefficient negated / g leadingCoefficient))] repeat! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/11/2018 06:26:35'!
reduceFull: aPolynomial
	"In the general case of standard bases, this is the same as >>reduce:.
	It is reimplemented in GroebnerBasis."
	^ self reduce: aPolynomial! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/12/2018 17:54:24'!
reduceFull: aPolynomial startingAt: startIndex
	"In the general case of standard bases, this is the same as >>reduce:.
	It is reimplemented in GroebnerBasis."
	^ self reduce: aPolynomial! !

!StandardBasis methodsFor: 'operations' stamp: 'len 12/30/2021 14:30:32'!
reduced
	"Answer a reduced basis for the ideal generated by the receiver.
	Based on [BW93], algorithm REDGROEBNER (p. 216) and REDUCTION (p. 203).
	[BW93] Thomas Becker; Volker Weispfenning; 'Groebner Bases: A Computational Approach to Commutative Algebra' (1993)"
	| G H |
	H _ OrderedCollection new.
	1 to: self size do: [:i| | fi lm |
		fi _ self at: i.
		lm _ fi leadingMonomial.
		(((i+1 to: self size) noneSatisfy: [:j| (self at: j) leadingMonomial | lm])
			and: [H noneSatisfy: [:h| h leadingMonomial | lm]])
				ifTrue: [H add: fi]].
	H _ H asArray.
	G _ self class new.
	1 to: H size do: [:i| | hi r |
		hi _ H at: i.
		(r _ hi \\ (H copyWithoutIndex: i)) isZero ifFalse: [G add: r normalized]].
	G sort: [:a :b| a leadingMonomial >= b leadingMonomial]. "make it triangular for lex"
	^ G! !

!StandardBasis methodsFor: 'sorting' stamp: 'len 5/10/2018 17:52:05'!
sort: aBlock
	elements sort: aBlock! !

!StandardBasis methodsFor: 'testing' stamp: 'len 7/28/2021 15:19:12'!
isMinimal
	"A Groebner basis G of an ideal I is called 'minimal' if the lead terms of the elements in G minimally generate in(I); or equivalently, if for all p in G:
	- LC(p) = 1, and
	- LT(p) is not in <G-{p}> (i.e., leading-term-minimal).
	Note that any leading-term-minimal Groebner basis is also cardinality-minimal."
	self do: [:g| g leadingCoefficient isOne ifFalse: [^ false]].
	self do: [:g|
		self do: [:f|
			(f ~~ g and: [f leadingMonomial | g leadingMonomial]) ifTrue: [^ false]]].
	^ true! !

!StandardBasis methodsFor: 'testing' stamp: 'len 5/10/2018 03:10:42'!
isMinimal2
	"A Groebner basis G is minimal when for all p in G:
	1. LC(p) = 1;
	2. LT(p) is not in <G-{p}>."
	| one |
	one _ self first scalars one.
	"Check all polynomials are monic:"
	self do: [:each| each leadingCoefficient ~= one ifTrue: [^ false]].
	self do: [:each| "Check if the basis without it still generates its leading term:"
		((self copyWithout: each) reduces: each leadingTerm)
				ifTrue: [^ false]].
	^ true! !

!StandardBasis methodsFor: 'testing' stamp: 'len 7/28/2021 15:18:41'!
isReduced
	"A Groebner basis G is called 'reduced' (or 'auto-reduced') if for all p in G:
	- LC(p) = 1, and
	- no monomial of p is in <G-{p}>.
	In particular, a reduced Groebner basis is leading-term-minimal (no leading term of any generator divides any other generator's leading term), and a leading-term-minimal basis is also cardinality-minimal."
	self do: [:g| g leadingCoefficient isOne ifFalse: [^ false]].
	self do: [:g|
		g monomialsDo: [:gi|
			self do: [:f|
				(f ~~ g and: [f leadingMonomial | gi]) ifTrue: [^ false]]]].
	^ true! !

!StandardBasis methodsFor: 'testing' stamp: 'len 5/9/2018 23:21:42'!
isReduced2
	"A Groebner basis G is reduced when for all p in G:
	1. LC(p) = 1;
	2. No monomial of p is contained in <G-{p}>."
	| one subBasis |
	one _ self first scalars one.
	"Check all polynomials are monic:"
	self do: [:each| each leadingCoefficient ~= one ifTrue: [^ false]].
	self do: [:each| "Check if the basis without it still generates its monomials:"
		subBasis _ self copyWithout: each.
		(each terms allSatisfy: [:other| subBasis reduces: other]) ifTrue: [^ false]].
	^ true! !

!StandardBasis methodsFor: 'testing' stamp: 'len 5/7/2018 23:21:47'!
isTrivial
	"Answer true if the basis is {1}."
	^ elements size = 1 and: [elements first isConstant and: [elements first isMonic]]
"	^ elements anySatisfy: [:f| f isConstant]"! !

!StandardBasis methodsFor: 'testing' stamp: 'len 10/29/2016 16:36'!
reduces: aPolynomial
	^ (self reduce: aPolynomial) isZero! !

!StandardBasis methodsFor: 'private' stamp: 'len 3/6/2016 20:37'!
species
	^ Array! !

!StandardBasis methodsFor: 'private' stamp: 'len 7/8/2022 11:12:23'!
validate
	1 to: self size do: [:i| | fi |
		fi _ self at: i.
		i+1 to: self size do: [:j| | fj |
			fj _ self at: j.
			self assert: (self reduces: (fi s: fj))]]! !

!GroebnerBasis methodsFor: 'operations' stamp: 'len 7/29/2021 16:58:08'!
reduce: aPolynomial
	"Answer a top-reduced normal form of aPolynomial with respect to the receiver (without doing tail reduction)."
	| h |
	h _ aPolynomial.
	[h isZero]
		whileFalse:
			[| lh lg g |
			lh _ h leadingMonomial.
			g _ elements detect: [:one| (lg _ one leadingMonomial) | lh] ifNone: [^ h].
			h _ h + (g * (lh / lg) * (h leadingCoefficient negated / g leadingCoefficient))].
	^ h! !

!GroebnerBasis methodsFor: 'operations' stamp: 'len 7/29/2021 16:58:21'!
reduce: aPolynomial startingAt: startIndex
	"Answer a top-reduced normal form of aPolynomial with respect to the receiver (without doing tail reduction)."
	| h |
	h _ aPolynomial.
	[h isZero]
		whileFalse:
			[| lh lg g |
			lh _ h leadingMonomial.
			(startIndex to: elements size) detect: [:i| (lg _ (g _ elements at: i) leadingMonomial) | lh] ifNone: [^ h].
			h _ h + (g * (lh / lg) * (h leadingCoefficient negated / g leadingCoefficient))].
	^ h! !

!GroebnerBasis methodsFor: 'operations' stamp: 'len 12/17/2021 14:43:33'!
reduceFull: aPolynomial
	"Answer the tail-reduced normal form of aPolynomial with respect to the receiver (doing full tail reduction)."
	| g h |
	g _ aPolynomial.
	h _ g zero.
	[g isZero]
		whileFalse:
			[g _ self reduce: g.
			g isZero ifFalse: [h _ h + g leadingTerm. g _ g tail]].
	^ h normalized! !

!GroebnerBasis methodsFor: 'operations' stamp: 'len 12/17/2021 14:43:39'!
reduceFull: aPolynomial startingAt: startIndex
	"Answer the tail-reduced normal form of aPolynomial with respect to the receiver (doing full tail reduction)."
	| g h |
	g _ aPolynomial.
	h _ g zero.
	[g isZero]
		whileFalse:
			[g _ self reduce: g startingAt: startIndex.
			g isZero ifFalse: [h _ h + g leadingTerm. g _ g tail]].
	^ h normalized! !

!SmallPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/13/2022 13:17:56'!
testSmallPrimeField
	| p F E |
	p _ 20 atRandom nextPrime.
	F _ ℤ/p.
	E _ SmallPrimeField new: p.
	F do: [:a|
		F do: [:b| | c d |
			c _ E !! a representative.
			d _ E !! b representative.
			self assert: (a+b) representative = (c+d) representative.
			self assert: (a-b) representative = (c-d) representative.
			self assert: (a*b) representative = (c*d) representative.
			b isZero ifFalse:
				[self assert: (a/b) representative = (c/d) representative.
				self assert: b inverse representative = d inverse representative]]]! !

!SmallPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/13/2022 13:18:02'!
testSmallPrimeFieldTables
	| p F g |
	p _ 20 atRandom nextPrime.
	F _ SmallPrimeField new: p.
	g _ F primitiveRoot.
	1 to: p-1 do: [:n| self assert: (F expTable at: n) = (g^n) representative].
	0 to: p-2 do: [:n| self assert: (F logTable at: (g^n) representative) = n]! !

!ProductGroup methodsFor: 'accessing' stamp: 'len 4/19/2019 07:14:28'!
ambient
	^ self propertyAt: #ambient ifAbsentPut: [(components allSatisfy: [:each| each isAmbient]) ifTrue: [self] ifFalse: [self class components: (components collect: [:each| each ambient])]]! !

!ProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 20:00'!
components
	^ components! !

!ProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 20:10'!
size
	^ components product: [:each| each size]! !

!ProductGroup methodsFor: 'comparing' stamp: 'len 6/15/2022 10:47:47'!
= anObject
	^ (anObject isKindOf: ProductGroup) and: [anObject components = components]! !

!ProductGroup methodsFor: 'comparing' stamp: 'len 2/12/2016 20:05'!
hash
	^ components hash! !

!ProductGroup methodsFor: 'elements' stamp: 'len 5/7/2023 17:24:34'!
compose: anArray
	^ GroupTuple components: anArray asArray parent: self! !

!ProductGroup methodsFor: 'elements' stamp: 'len 6/19/2022 12:29:14'!
identity
	"Answer the identity element of the receiver."
	^ self compose: (components collect: [:each| each identity])! !

!ProductGroup methodsFor: 'enumerating' stamp: 'len 10/17/2020 19:59:54'!
do: aBlock
	self asCartesianProduct do: [:each| aBlock value: (self compose: each)]! !

!ProductGroup methodsFor: 'morphisms' stamp: 'len 10/6/2022 11:36:47'!
coprojection: i
	^ ((self at: i) to: self map: [:x| self compose: (components withIndexCollect: [:each :k| k = i ifTrue: [x] ifFalse: [each identity]])]) name: 'ι', i printString sub! !

!ProductGroup methodsFor: 'morphisms' stamp: 'len 9/16/2022 07:54:00'!
projection: i
	^ (self to: (self at: i) map: [:x| x at: i]) name: 'π', i printString sub! !

!ProductGroup methodsFor: 'printing' stamp: 'len 5/23/2020 07:19:05'!
printOn: aStream
	components do: [:each| aStream print: each] separatedBy: [aStream nextPut: $×]! !

!ProductGroup methodsFor: 'random' stamp: 'len 10/17/2020 19:59:29'!
atRandom: aRandom
	^ self compose: (self asCartesianProduct atRandom: aRandom)! !

!ProductGroup methodsFor: 'random' stamp: 'len 10/17/2020 19:59:42'!
atRandom: aRandom bits: bits
	^ self compose: (self asCartesianProduct atRandom: aRandom bits: bits)! !

!ProductGroup methodsFor: 'testing' stamp: 'len 4/25/2020 06:54:01'!
includes: anObject
	^ anObject parent = self! !

!ProductGroup methodsFor: 'testing' stamp: 'len 4/14/2020 21:09:53'!
isCoproduct
	^ true! !

!ProductGroup methodsFor: 'testing' stamp: 'len 4/14/2020 21:09:49'!
isProduct
	^ true! !

!ProductGroup methodsFor: 'private' stamp: 'len 11/5/2016 06:40'!
components: anArray
	components _ anArray! !

!ProductGroup methodsFor: 'private' stamp: 'len 12/30/2017 15:11:22'!
computeGenerators
	| answer |
	answer _ OrderedCollection new.
	1 to: self arity do: [:i|
		| p |
		p _ self projection: i.
		(self at: i) generators do: [:each| answer add: (p value: each)]].
	^ answer! !

!ProductGroup class methodsFor: 'instance creation' stamp: 'len 2/12/2016 20:02'!
components: anArray
	^ self new components: anArray! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 06:15'!
action
	^ action! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/16/2016 07:30'!
arity
	^ 2! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 06:16'!
left
	^ left! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 06:16'!
right
	^ right! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 06:55'!
size
	^ left size * right size! !

!SemidirectProductGroup methodsFor: 'converting' stamp: 'len 4/19/2016 20:55'!
asCartesianProduct
	^ (left, right)! !

!SemidirectProductGroup methodsFor: 'elements' stamp: 'len 6/19/2022 12:29:46'!
identity
	"Answer the identity element of the receiver."
	^ self project: {left identity. right identity}! !

!SemidirectProductGroup methodsFor: 'enumerating' stamp: 'len 2/12/2016 06:57'!
do: aBlock
	left do: [:g| right do: [:h| aBlock value: (self project: {g.h})]]! !

!SemidirectProductGroup methodsFor: 'morphisms' stamp: 'len 10/6/2016 15:35'!
project: anArray
	"Project a cartesian product of two elements to their semidirect product."
	^ SemidirectProductElement new
		parent: self;
		left: (anArray at: 1);
		right: (anArray at: 2)! !

!SemidirectProductGroup methodsFor: 'morphisms' stamp: 'len 9/16/2022 07:54:21'!
projection
	"Answer the projection from the cartesian product of two groups to their semidirect product."
	^ ((left, right) to: self map: [:each| self project: each]) name: 'π'! !

!SemidirectProductGroup methodsFor: 'printing' stamp: 'len 4/21/2016 00:16'!
printOn: aStream
	aStream print: left; nextPutAll: ' X| '; print: right! !

!SemidirectProductGroup methodsFor: 'random' stamp: 'len 4/19/2016 20:52'!
atRandom: aRandom
	^ self project: (self asCartesianProduct atRandom: aRandom)! !

!SemidirectProductGroup methodsFor: 'random' stamp: 'len 4/19/2016 20:52'!
atRandom: aRandom bits: bitSize
	^ self project: (self asCartesianProduct atRandom: aRandom bits: bitSize)! !

!SemidirectProductGroup methodsFor: 'private' stamp: 'len 2/12/2016 06:17'!
action: anAction
	action _ anAction! !

!SemidirectProductGroup methodsFor: 'private' stamp: 'len 2/12/2016 06:17'!
left: aGroup
	left _ aGroup! !

!SemidirectProductGroup methodsFor: 'private' stamp: 'len 2/12/2016 06:17'!
right: aGroup
	right _ aGroup! !

!SemidirectProductGroup class methodsFor: 'instance creation' stamp: 'len 2/12/2016 06:16'!
left: aGroup right: anotherGroup action: anAction
	^ self new left: aGroup; right: anotherGroup; action: anAction! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:25'!
diagram
	^ diagram! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:24'!
diagram: aGraph
	diagram _ aGraph! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:39'!
gramMatrix
	^ self matrix collect: [:x| -2 * (Float pi / x) cos]! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:39'!
isHyperbolic
	^ (self gramMatrix spectrum count: [:each| each negative]) = 1! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 5/10/2019 09:57:34'!
presentation
	self notYetImplemented
"	| generators relators |
	generators _ diagram values collect: [:each| Word x: each].
	relators _ OrderedCollection new.
	diagram edgesAndLabelsDo: [:each :label|
		label isInfinity
			ifFalse: [relators add: (Word x: each key) * (Word x: each value) ^ (label ifNil: [3])]].
	^ GroupPresentation generators: generators relators: relators"! !

!CoxeterGroup class methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:25'!
diagram: aGraph
	^ self new diagram: aGraph! !

!CoxeterGroup class methodsFor: 'as yet unclassified' stamp: 'len 4/5/2018 15:26:58'!
matrix: aMatrix
	| n graph |
	n _ aMatrix width.
	graph _ Graph unorderedLabeled.
	graph addAll: (1 to: n).
	1 to: n do: [:i|
		1 to: n do: [:j|
			| m |
			m _ aMatrix at: i@j.
			m = 3 ifTrue: [graph addEdgeFrom: i to: j label: nil].
			m > 3 ifTrue: [graph addEdgeFrom: i to: j label: m]]].
	^ self diagram: graph! !

!SmallPrimeField methodsFor: 'accessing' stamp: 'len 6/9/2018 20:23:46'!
expTable
	^ expTable! !

!SmallPrimeField methodsFor: 'accessing' stamp: 'len 6/9/2018 20:23:52'!
logTable
	^ logTable! !

!SmallPrimeField methodsFor: 'accessing-private' stamp: 'len 11/30/2016 09:43:55'!
modulus: anInteger
	super modulus: anInteger.
	self computeTables! !

!SmallPrimeField methodsFor: 'private' stamp: 'len 9/2/2018 21:27:06'!
computeTables
	| g x |
	expTable _ WordArray new: modulus - 1. "n -> g^n"
	logTable _ WordArray new: modulus - 1. "g^n -> n"
	g _ self primitiveRoot representative.
	x _ 1.
	1 to: modulus - 1 do: [:i|
		"g^n -> n:"
		logTable at: x put: i - 1.
		x _ x * g \\ modulus.
		"n -> g^n:"
		expTable at: i put: x]! !

!SmallPrimeField methodsFor: 'private' stamp: 'len 6/8/2019 12:35:48'!
elementClass
	^ SmallPrimeResidue! !

!SmallPrimeField methodsFor: 'private' stamp: 'len 11/30/2016 19:12:33'!
inverseOfRepresentative: anInteger
	anInteger == 0 ifTrue: [^ nil].
	anInteger == 1 ifTrue: [^ 1].
	^ expTable at: modulus - 1 - (logTable at: anInteger)! !

!SmallPrimeField class methodsFor: 'instance creation' stamp: 'len 5/1/2022 12:35:48'!
new: aPrime
	^ self new modulus: aPrime! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 04:40:15'!
arity
	^ self rank! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 05:09:10'!
components
	^ (1 to: self rank) collect: [:each| self scalars]! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 04:40:52'!
dimension
	"Answer the Krull dimension of the receiver. This is the length of the longest ascending chain of prime ideals."
	^ self scalars dimension! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 04:48:22'!
one
	| one |
	one _ self scalars one.
	^ self compose: self scalars !!!! ((1 to: self rank) collect: [:each| one])! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 04:42:30'!
size
	^ self scalars size ^ self rank! !

!PowerAlgebra methodsFor: 'comparing' stamp: 'len 10/13/2022 04:45:09'!
= anObject
	^ self class = anObject class and: [self asModule = anObject asModule]! !

!PowerAlgebra methodsFor: 'comparing' stamp: 'len 10/13/2022 04:44:48'!
hash
	^ self asModule hash! !

!PowerAlgebra methodsFor: 'computing-matrices' stamp: 'len 11/23/2022 21:38:37'!
matrix: B quotientBy: A
	"Answer X such that AX + N = B, satisfying that if the i-th column of B is a linear combination of the columns of A then the i-th column of N is zero.
	The algorithm solves the system for each projection and then composes back to the product ring."
	| results |
	results _ self ⇉ self components collect: [:p| (B apply: p) quotientBy: (A apply: p)].
	^ self matrix: A width @ B width fill: [:i :j| self compose: (results collect: [:each| each at: i@j])]! !

!PowerAlgebra methodsFor: 'computing-matrices' stamp: 'len 11/23/2022 21:38:47'!
syzygiesOf: aMatrix
	"Answer a matrix whose rows span the kernel of the given matrix.
	For a matrix A, the output is X with AX = 0.
	The algorithm computes syzygies for each projection and then composes back to the product ring."
	| results |
	results _ self ⇉ self components collect: [:p| (aMatrix apply: p) syzygies].
	^ self matrix: aMatrix width @ (results max: [:each| each width]) fill: [:i :j| self compose: (results withIndexCollect: [:each :k| each at: i@j ifAbsent: [(self components at: k) zero]])]! !

!PowerAlgebra methodsFor: 'converting' stamp: 'len 10/14/2022 08:51:41'!
asAbelianGroup
	"Answer the receiver as a ℤ-module."
	^ self scalars asAbelianGroup ^⊕ self rank! !

!PowerAlgebra methodsFor: 'elements' stamp: 'len 6/19/2023 12:12:14'!
compose: anArray
	^ Product components: self scalars !!!! anArray parent: self! !

!PowerAlgebra methodsFor: 'elements' stamp: 'len 6/19/2023 12:12:14'!
fromModuleElement: aTuple
	^ Product components: aTuple parent: self! !

!PowerAlgebra methodsFor: 'morphisms' stamp: 'len 10/13/2022 04:50:49'!
projection: i
	"Answer the canonical projection to the i-th component."
	^ (self to: self scalars map: [:x| x at: i]) name: 'π', i printString sub! !

!PowerAlgebra methodsFor: 'operations' stamp: 'len 10/13/2022 07:39:54'!
nilradical
	"Answer the ideal consisting of all nilpotent elements, i.e. all x such that there exists some n with x^n = 0. It is the intersection of all prime ideals."
	| generators |
	generators _ OrderedCollection new.
	self scalars nilradical generators do: [:g|
		1 to: self rank do: [:i|
			generators add: self !! ((self asModule x: i) * g)]].
	^ self * generators! !

!PowerAlgebra methodsFor: 'printing' stamp: 'len 10/13/2022 04:40:08'!
printOn: aStream
	aStream print: self scalars; nextPutAll: ('×', self rank printString) super! !

!PowerAlgebra methodsFor: 'random' stamp: 'len 10/13/2022 04:51:54'!
unitAtRandom: aRandom
	^ self compose: ((1 to: self rank) collect: [:each| self scalars unitAtRandom: aRandom])! !

!PowerAlgebra methodsFor: 'random' stamp: 'len 10/13/2022 04:52:25'!
unitAtRandom: aRandom bits: bitSize
	| n |
	n _ bitSize / self rank.
	^ self compose: ((1 to: self rank) collect: [:each| self scalars unitAtRandom: aRandom bits: n])! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:02'!
hasCanonicalAssociates
	^ self scalars hasCanonicalAssociates! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:12'!
hasGCD
	^ self scalars hasGCD! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 12/21/2022 13:28:04'!
isBezout
	"Answer true if the receiver is a Bézout ring, i.e. every finitely generated ideal is principal."
	"Bézout rings (like PIRs) are closed under products:"
	^ self scalars isBezout or: [super isBezout]! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:33'!
isCommutative
	^ self scalars isCommutative! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:46'!
isIntegralDomain
	"The product of two or more nontrivial rings always has nontrivial zero divisors."
	^ self scalars isTrivial or: [self rank < 2]! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:55'!
isInvolutive
	^ self scalars isInvolutive! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:42:03'!
isPIR
	"Answer true if the receiver is a principal ideal ring, i.e. all ideals are principal (but it is not necessarily a PID, i.e. it is not necessarily an integral domain)."
	"A product ring is a PIR if and only if all its components are PIRs."
	^ self scalars isPIR! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:48:50'!
isProduct
	^ true! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:49:17'!
isReduced
	"Answer true if the receiver has no nonzero nilpotent elements, or equivalently if it has no nonzero elements with square zero, i.e. x^2 = 0 implies x = 0."
	self scalars isReduced ifTrue: [^ true]. "is it iff?"
	^ super isReduced! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:42:11'!
isTrivial
	^ self scalars isTrivial! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:42:19'!
isUFR
	"A unique factorization ring (UFR) is a commutative ring in which every nonzero nonunit element can be written as a product of irreducible elements, uniquely up to order and associates."
	"The product of a finite number of UFRs is a UFR. In particular, the product of a finite number of UFDs is a UFR [Fle69]."
	^ self scalars isUFR! !

!PowerAlgebra class methodsFor: 'instance creation' stamp: 'len 10/13/2022 04:58:29'!
on: aFreeModule
	^ self new module: aFreeModule! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/2/2022 12:15:04'!
additiveConstants
	^ additiveConstants! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/4/2022 12:43:48'!
c: aPoint
	^ (multiplicativeConstants at: aPoint y - 1) at: aPoint x! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 9/16/2022 08:24:35'!
cover
	| answer |
	self propertyAt: #cover ifPresent: [:aFreeAlgebra| ^ aFreeAlgebra].
	answer _ FreeAlgebra new: representatives names over: representatives scalars.
	answer addCoercionTo: representatives map: [:f| f value: representatives generators].
	answer addCoercionFrom: representatives map: [:f| f value: answer generators].
	^ self propertyAt: #cover put: answer! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/4/2022 12:43:43'!
d: aPoint
	^ (additiveConstants at: aPoint y - 1) at: aPoint x! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/2/2022 12:14:54'!
multiplicativeConstants
	^ multiplicativeConstants! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/4/2022 12:22:29'!
one
	^ self representative: representatives one! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/4/2022 12:18:30'!
rank
	^ representatives rank! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/4/2022 12:42:31'!
relators
	| answer F n |
	answer _ OrderedCollection new.
	F _ self cover.
	n _ F rank.
	2 to: n do: [:j| 1 to: j-1 do: [:i|
		| f |
		f _ (F x: j) * (F x: i) - ((F x: i) * (F x: j) * (self c: i @ j) - (self d: i @ j)).
		f isZero ifFalse: [answer add: f]]].
	^ answer! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/4/2022 12:18:34'!
scalars
	^ representatives scalars! !

!GAlgebra methodsFor: 'accessing' stamp: 'len 7/4/2022 12:23:39'!
zero
	^ self representative: representatives zero! !

!GAlgebra methodsFor: 'comparing' stamp: 'len 7/2/2022 12:15:59'!
= anObject
	^ self class = anObject class and: [multiplicativeConstants = anObject multiplicativeConstants and: [additiveConstants = anObject additiveConstants]]! !

!GAlgebra methodsFor: 'comparing' stamp: 'len 7/2/2022 12:16:50'!
hash
	^ multiplicativeConstants hash + additiveConstants hash! !

!GAlgebra methodsFor: 'elements' stamp: 'len 7/4/2022 12:22:40'!
representative: aPolynomial
	"Answer the element represented by the given commutative polynomial.
	The argument should be a polynomial in the ring of polynomials used to represent elements of this algebra."
	^ GAlgebraElement representative: aPolynomial parent: self! !

!GAlgebra methodsFor: 'elements' stamp: 'len 7/4/2022 12:22:53'!
x: anInteger
	^ self representative: (representatives x: anInteger)! !

!GAlgebra methodsFor: 'elements' stamp: 'len 7/4/2022 12:50:59'!
x: i to: n x: j to: m
	| c d |
	i <= j ifTrue: [^ self representative: (representatives x: i to: n x: j to: m)].
	c _ self c: i @ j.
	d _ self d: i @ j.
	d isZero ifTrue: [	^ self representative: (representatives x: j x: i) * (c ^ (n*m))].
	^ self notYetImplemented! !

!GAlgebra methodsFor: 'elements' stamp: 'len 7/4/2022 12:49:53'!
x: i x: j
	i <= j ifTrue: [^ self representative: (representatives x: i x: j)].
	^ self representative: (representatives x: j x: i) * (self c: j @ i) + (self d: j @ i)! !

!GAlgebra methodsFor: 'operations' stamp: 'len 7/1/2022 06:09:47'!
opposite
	^ self notYetImplemented! !

!GAlgebra methodsFor: 'printing' stamp: 'len 7/1/2022 11:37:32'!
printOn: aStream
	aStream print: self cover; nextPutAll: '/<'.
	self relators do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $>! !

!GAlgebra methodsFor: 'testing' stamp: 'len 7/1/2022 20:26:00'!
includes: anElement
	^ anElement parent = self! !

!GAlgebra methodsFor: 'testing' stamp: 'len 7/4/2022 12:54:06'!
isCommutative
	^ self isQuasiCommutative and: [self isLie]! !

!GAlgebra methodsFor: 'testing' stamp: 'len 7/8/2022 10:26:18'!
isFinitelyGenerated
	^ true! !

!GAlgebra methodsFor: 'testing' stamp: 'len 7/2/2022 11:51:21'!
isIntegralDomain
	"A ring is an integral domain if the product of any two nonzero elements is nonzero."
	^ self scalars isIntegralDomain! !

!GAlgebra methodsFor: 'testing' stamp: 'len 7/2/2022 11:48:00'!
isLie
	"Answer true if the receiver is an algebra of Lie type."
	^ multiplicativeConstants allSatisfy: [:each| each allSatisfy: [:f| f isOne]]! !

!GAlgebra methodsFor: 'testing' stamp: 'len 7/2/2022 11:50:48'!
isNoetherian
	"A ring is called Noetherian if every ideal is finitely generated.
	G-algebras are Noetherian. Even more, they always have finite Groebner bases."
	^ true! !

!GAlgebra methodsFor: 'testing' stamp: 'len 7/4/2022 12:53:42'!
isQuasiCommutative
	"Answer true if the receiver is a quasi-commutative algebra."
	^ additiveConstants allSatisfy: [:each| each allSatisfy: [:f| f isZero]]! !

!GAlgebra methodsFor: 'private' stamp: 'len 7/4/2022 12:37:48'!
multiplicativeConstants: anArray additiveConstants: anotherArray
	multiplicativeConstants _ anArray.
	additiveConstants _ anotherArray.
	representatives _ additiveConstants last first parent.
	self validate! !

!GAlgebra methodsFor: 'private' stamp: 'len 7/4/2022 12:22:13'!
multiplyMonomial: a timesMonomial: b
	| i j a2 b2 n |
	i _ a lastIndeterminate.
	j _ b firstIndeterminate.
	i <= j ifTrue: [^ self representative: (representatives monomial: a * b)].
	n _ (a at: i) min: (b at: j).
	a2 _ a parent exponents: (a exponents copy at: i put: (a at: i) - n; yourself).
	b2 _ b parent exponents: (b exponents copy at: j put: (b at: j) - n; yourself).
	^ (self representative: ((multiplicativeConstants at: i) at: j) * (representatives x: i)a2 + ((additiveConstants at: i) at: j)) * b2! !

!GAlgebra methodsFor: 'private' stamp: 'len 7/1/2022 12:57:30'!
species
	^ GAlgebra! !

!GAlgebra methodsFor: 'private' stamp: 'len 7/4/2022 12:45:38'!
validate
	| n d |
	n _ representatives rank.
	"Ordering condition:"
	2 to: n do: [:j|
		1 to: j - 1 do: [:i|
			d _ self d: i @ j.
			self assert: (d isZero or: [(self d: i @ j) leadingMonomial < (representatives monomials x: i x: j)])]].
	"Non-degeneracy condition:"
"	1 to: n - 2 do: [:i|"! !

!GAlgebra class methodsFor: 'examples' stamp: 'len 7/2/2022 11:43:39'!
quantumPlane: q
	| P |
	P _ q parent fractions polynomialsIn: #(x y).
	^ self multiplicativeConstants: {{q}} additiveConstants: {{P zero}}! !

!GAlgebra class methodsFor: 'instance creation' stamp: 'len 7/2/2022 11:40:47'!
multiplicativeConstants: anArray additiveConstants: anotherArray
	^ self new multiplicativeConstants: anArray additiveConstants: anotherArray! !

!WeylAlgebra class methodsFor: 'instance creation' stamp: 'len 7/2/2022 12:06:52'!
multiplicativeConstants: anArray additiveConstants: anotherArray
	^ self shouldNotImplement! !

!WeylAlgebra class methodsFor: 'instance creation' stamp: 'len 8/7/2022 10:44:10'!
new: anInteger over: aRing
	| P C D |
	P _ aRing polynomialsIn: ((1 to: anInteger) collect: [:i| 'x', i printString sub]), ((1 to: anInteger) collect: [:i| '∂', i printString sub]).
	C _ (2 to: anInteger*2) collect: [:j| (1 to: j-1) collect: [:i| aRing one]].
	D _ (2 to: anInteger*2) collect: [:j| (1 to: j-1) collect: [:i| (i = (j+anInteger) or: [i+anInteger = j]) ifTrue: [P one] ifFalse: [P zero]]].
	^ super multiplicativeConstants: C additiveConstants: D! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/5/2018 15:27:04'!
computeDiagram
	| n graph |
	n _ matrix width.
	graph _ Graph unorderedLabeled.
	graph addAll: (1 to: n).
	1 to: n do: [:i|
		1 to: n do: [:j|
			| m |
			m _ matrix at: i@j.
			m = 3 ifTrue: [graph addEdgeFrom: i to: j label: nil].
			m > 3 ifTrue: [graph addEdgeFrom: i to: j label: m]]].
	^ graph! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 07:54'!
diagram
	^ diagram ifNil: [diagram _ self computeDiagram]! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 07:57'!
diagram: aGraph
	diagram _ aGraph! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 07:55'!
matrix
	^ matrix ifNil: [matrix _ self computeMatrix]! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 07:57'!
matrix: aMatrix
	matrix _ aMatrix! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 5/10/2019 09:57:21'!
presentation
	self notYetImplemented
"	| generators relators |
	generators _ self diagram values collect: [:each| Word x: each].
	relators _ OrderedCollection new.
	self diagram edgesAndLabelsDo: [:each :label|
		label isInfinity
			ifFalse: [relators add: (Word x: each key) * (Word x: each value) ^ (label ifNil: [3])]].
	^ GroupPresentation generators: generators relators: relators"! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 03:17'!
reflections
	"Answer the conjugates of the generators."
	^ self notYetImplemented! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 08:02'!
schlafliMatrix
	^ self matrix collect: [:x| -2 * (Float pi / x) cos]! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 03:28'!
simpleReflections
	^ generators! !

!Quiver methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 10:11:11'!
edges
	^ edges! !

!Quiver methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 10:19:23'!
edges: anArray
	edges _ anArray! !

!Quiver methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 10:16:30'!
source: aVertex walk: anArray
	"Answer a path starting at aVertex and following the edges in anArray."
	^ QuiverPath source: aVertex walk: anArray parent: self! !

!Quiver class methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 10:19:12'!
edges: anArray
	^ self new edges: anArray! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 12/15/2022 12:42:26'!
atRow: i column: j
	^ coefficients at: j - 1 * self height + i! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 12/15/2022 12:42:46'!
atRow: i column: j ifAbsent: exceptionBlock
	((i between: 1 and: self height) and: [j between: 1 and: self width])
		ifFalse: [^ exceptionBlock value].
	^ coefficients at: j - 1 * self height + i "ifAbsent: exceptionBlock"! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 12/15/2022 12:43:01'!
atRow: i column: j put: anObject
	((i between: 1 and: self height) and: [j between: 1 and: self width])
		ifFalse: [^ self error: 'indices out of bounds'].
	^ coefficients at: j - 1 * self height + i put: anObject! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 5/5/2023 16:49:18'!
columnAt: j
	^ codomain coefficients: (coefficients copyFrom: j - 1 * self height + 1 to: j * self height)! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 5/5/2023 16:49:46'!
columnAt: j put: aTuple
	coefficients replaceFrom: j - 1 * self height + 1 to: j * self height with: aTuple coefficients startingAt: 1! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 12/15/2022 11:51:15'!
lift
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R."
	^ self class from: domain lift to: codomain lift coefficients: coefficients lift! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 12/15/2022 12:52:04'!
projection: anInteger
	"If the coefficients ring is a product ring, answer matrix induced by the projection to the anInteger-th component."
	^ self class from: (domain over: (domain scalars at: anInteger)) to: (codomain over: (domain scalars at: anInteger)) coefficients: (coefficients projection: anInteger)! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 12/15/2022 12:52:12'!
representative
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R."
	^ self class from: domain lift to: codomain lift coefficients: coefficients representative! !

!ColumnMajorMatrix methodsFor: 'accessing' stamp: 'len 12/15/2022 12:53:59'!
zero
	^ self class from: domain to: codomain coefficients: coefficients zero! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/19/2022 10:23:37'!
addColumn: j₁ times: anElement to: j₂
	self addColumn: j₁ times: anElement to: j₂ startingAt: 1! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/19/2022 10:30:38'!
addColumn: j₁ times: anElement to: j₂ startingAt: startRow
	| height start₁ start₂ |
	height _ self height.
	start₁ _ j₁ - 1 * height + startRow.
	start₂ _ j₂ - 1 * height + startRow.
	coefficients
		replaceFrom: start₂ to: j₂ * height
		with: coefficients startingAt: start₂
		plus: coefficients startingAt: start₁
		timesScalar: anElement! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/20/2022 11:09:25'!
cancelColumn: j₁ and: j₂ with: mgcd
	| s t u v A₁ A₂ |
	s _ mgcd at: 2.
	t _ mgcd at: 3.
	u _ mgcd at: 4.
	v _ mgcd at: 5.
	A₁ _ (self columnAt: j₁) copy.
	A₂ _ (self columnAt: j₂) copy.
	self columnAt: j₁ put: A₁*s + (A₂*t).
	self columnAt: j₂ put: A₁*u + (A₂*v)! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/20/2022 06:04:15'!
copyColumn: j₁ times: anElement to: j₂
	self copyColumn: j₁ times: anElement to: j₂ startingAt: 1! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/20/2022 06:03:43'!
copyColumn: j₁ times: anElement to: j₂ startingAt: startRow
	| height start₁ start₂ |
	height _ self height.
	start₁ _ j₁ - 1 * height + startRow.
	start₂ _ j₂ - 1 * height + startRow.
	coefficients
		replaceFrom: start₂ to: j₂ * height
		with: coefficients startingAt: start₁
		timesScalar: anElement! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/20/2022 05:58:34'!
copyColumn: j₁ to: j₂
	self copyColumn: j₁ to: j₂ startingAt: 1! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/20/2022 05:58:14'!
copyColumn: j₁ to: j₂ startingAt: startRow
	| height start₁ start₂ |
	height _ self height.
	start₁ _ j₁ - 1 * height + startRow.
	start₂ _ j₂ - 1 * height + startRow.
	coefficients
		replaceFrom: start₂ to: j₂ * height
		with: coefficients startingAt: start₁! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/15/2022 11:53:16'!
multiplyColumn: j by: anElement
	self multiplyColumn: j by: anElement startingAt: 1! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/19/2022 10:20:08'!
multiplyColumn: j by: anElement startingAt: startRow
	| height start |
	height _ self height.
	start _ j - 1 * height + startRow.
	coefficients
		replaceFrom: start to: j * height
		with: coefficients startingAt: start
		timesScalar: anElement! !

!ColumnMajorMatrix methodsFor: 'column operations' stamp: 'len 12/19/2022 10:19:09'!
swapColumn: j₁ with: j₂
	| height |
	height _ self height.
	coefficients
		swapFrom: j₁ - 1 * height + 1 to: j₁ * height
		with: coefficients startingAt: j₂ - 1 * height + 1! !

!ColumnMajorMatrix methodsFor: 'comparing' stamp: 'len 12/15/2022 11:57:59'!
= anObject
	^ (self class = anObject class and: [coefficients = anObject coefficients]) or: [super = anObject]! !

!ColumnMajorMatrix methodsFor: 'converting' stamp: 'len 12/18/2022 10:19:18'!
asColumnMajor
	^ self! !

!ColumnMajorMatrix methodsFor: 'converting' stamp: 'len 5/12/2023 20:45:39'!
asRowMajor
	^ DenseMatrix from: self domain to: self codomain coefficients: self transpose coefficients! !

!ColumnMajorMatrix methodsFor: 'converting' stamp: 'len 5/5/2023 17:16:22'!
over: aRing
	self scalars = aRing ifTrue: [^ self].
	^ self class from: (domain over: aRing) to: (codomain over: aRing) coefficients: (aRing new: coefficients size fill: [:i| coefficients at: i])! !

!ColumnMajorMatrix methodsFor: 'copying' stamp: 'len 12/15/2022 12:49:15'!
copyFrom: start to: stop
	"Answer the submatrix of the receiver from start to stop."
	| h w subcoefficients |
	(stop x > self height or: [stop y > self width]) ifTrue: [^ self error: 'index out of bounds'].
	h _ stop x - start x + 1 max: 0.
	w _ stop y - start y + 1 max: 0.
	subcoefficients _ self scalars new: h*w.
	h > 0 ifTrue: [start y to: stop y do: [:j| subcoefficients replaceFrom: j - start y * h + 1 to: j - start y * h + h with: coefficients startingAt: j - 1 * self height + start x]].
	^ self class
		from: (domain copyFrom: start y to: stop y)
		to: (codomain copyFrom: start x to: stop x)
		coefficients: subcoefficients! !

!ColumnMajorMatrix methodsFor: 'copying' stamp: 'len 12/20/2022 09:45:45'!
postCopy
	super postCopy.
	properties _ nil.
	coefficients _ coefficients copy! !

!ColumnMajorMatrix methodsFor: 'enumerating' stamp: 'len 12/15/2022 12:41:46'!
allSatisfy: aBlock
	^ coefficients allSatisfy: aBlock! !

!ColumnMajorMatrix methodsFor: 'enumerating' stamp: 'len 12/15/2022 12:43:21'!
collect: aBlock
	^ self class from: domain to: codomain coefficients: (coefficients collect: aBlock)! !

!ColumnMajorMatrix methodsFor: 'enumerating' stamp: 'len 12/15/2022 12:50:20'!
diagonalDo: aBlock
	| i step |
	i _ 1.
	step _ self height + 1.
	[i <= coefficients size]
		whileTrue:
			[aBlock value: (coefficients at: i).
			i _ i + step]! !

!ColumnMajorMatrix methodsFor: 'enumerating' stamp: 'len 12/15/2022 12:50:40'!
do: aBlock
	coefficients do: aBlock! !

!ColumnMajorMatrix methodsFor: 'enumerating' stamp: 'len 12/15/2022 13:01:21'!
withIndicesDo: aBlock
	| h i j |
	h _ self height.
	i _ 1.
	j _ 1.
	coefficients do: [:each|
		aBlock value: each value: i value: j.
		(i _ i + 1) > h ifTrue: [i _ 1. j _ j + 1]].! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/15/2022 11:57:38'!
* anObject
	(self scalars includes: anObject) ifTrue: [^ self class from: domain to: codomain coefficients: coefficients * anObject].
	anObject isInteger ifTrue: [^ self * (self scalars !! anObject)].
	anObject isMatrix ifTrue: [^ self · anObject].
	anObject isTuple ifTrue: [^ self value: anObject].
	(anObject isKindOf: ModuleMap) ifTrue: [^ self · anObject].
	^ DomainError signal! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/15/2022 11:57:45'!
+ aMatrix
	self class = aMatrix class ifFalse: [^ super + aMatrix].
	self checkDomainsMatch: aMatrix.
	self checkCodomainsMatch: aMatrix.
	^ self class from: domain to: codomain coefficients: coefficients + aMatrix coefficients! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/15/2022 11:57:53'!
- aMatrix
	self class = aMatrix class ifFalse: [^ super - aMatrix].
	self checkDomainsMatch: aMatrix.
	self checkCodomainsMatch: aMatrix.
	^ self class from: domain to: codomain coefficients: coefficients - aMatrix coefficients! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/15/2022 12:35:58'!
⊕ aMatrix
	"Answer the block sum of the receiver and the argument.
	If the receiver is A and the argument is B, output a matrix of the form:
	  [A 0]
	  [0 B]"
	| newCoefficients h |
	self class = aMatrix class ifFalse: [^ super ⊕ aMatrix].
	h _ self height + aMatrix height.
	newCoefficients _ self scalars zeros: h * (self width + aMatrix width).
	1 to: self width do: [:j|
		newCoefficients replaceFrom: j - 1 * h + 1 to: j - 1 * h + self height with: coefficients startingAt: j - 1 * self height + 1].
	1 to: aMatrix width do: [:j|
		newCoefficients replaceFrom: j - 1 + self width * h + self height + 1 to: j + self width * h with: aMatrix coefficients startingAt: j - 1 * aMatrix height + 1].
	^ self class from: domain ⊕ aMatrix domain to: codomain ⊕ aMatrix codomain coefficients: newCoefficients
! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/27/2022 22:19:05'!
∘ aMatrix
	self class = aMatrix class ifFalse: [^ super ∘ aMatrix].
	self checkCompatibleComposition: aMatrix.
	(coefficients isEmpty or: [aMatrix coefficients isEmpty])
		ifTrue: [^ self class from: aMatrix domain to: codomain coefficients: (self scalars zeros: aMatrix width * self height)].
	^ self class
		from: aMatrix domain
		to: codomain
		coefficients:
			((coefficients species new: aMatrix width * self height)
				fillWithColumnMajorMatrix: coefficients height: self height
				timesMatrix: aMatrix coefficients height: aMatrix height)! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/25/2022 13:38:53'!
⊓ aMatrix
	"Answer the product morphism of the receiver and the argument.
	This is the row-augmented matrix with the receiver stacked on top of the argument.
	If the receiver is A and the argument is B, output a matrix of the form:
	  [A]
	  [B]"
	| newCoefficients h |
	self class = aMatrix class ifFalse: [^ super ⊓ aMatrix].
	self checkDomainsMatch: aMatrix.
	newCoefficients _ self scalars new: coefficients size + aMatrix coefficients size.
	h _ self height + aMatrix height.
	1 to: self width do: [:j|
		newCoefficients
			replaceFrom: j - 1 * h + 1 to: j - 1 * h + self height with: coefficients startingAt: j - 1 * self height + 1;
			replaceFrom: j - 1 * h + self height + 1 to: j * h with: aMatrix coefficients startingAt: j - 1 * aMatrix height + 1].
	^ self class from: domain to: codomain ⊕ aMatrix codomain coefficients: newCoefficients! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/18/2022 10:20:43'!
⊔ aMatrix
	"Answer the coproduct morphism of the receiver and the argument.
	This is the column-augmented matrix with the receiver concatenated with the argument.
	If the receiver is A and the argument is B, output a matrix of the form:
	  [A B]"
	self class = aMatrix class ifFalse: [^ super ⊔ aMatrix].
	self checkCodomainsMatch: aMatrix.
	^ self class from: domain ⊕ aMatrix domain to: codomain coefficients: coefficients, aMatrix coefficients! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/15/2022 12:41:57'!
apply: aFunction
	"Answer a new tuple with coefficients obtainted by applying aFunction to the coefficients of the receiver.
	If aFunction is a block we assume the parent doesn't change, i.e. the block must evaluate to values within the scalars ring of the receiver."

	aFunction isBlock ifTrue: [^ self collect: aFunction].
	^ self class from: (domain over: aFunction codomain) to: (codomain over: aFunction codomain) coefficients: (coefficients apply: aFunction)! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/15/2022 12:51:23'!
negated
	^ self class from: domain to: codomain coefficients: coefficients negated! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 12/18/2022 10:17:42'!
trace
	self checkIsSquare.
	self isEmpty ifTrue: [^ self scalars zero].
	^ coefficients sumFrom: 1 to: coefficients size by: self height + 1! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 5/9/2023 17:59:51'!
transpose
	^ self class from: codomain to: domain coefficients: ((coefficients species new: coefficients size) fillWithMatrixTranspose: coefficients width: self height)! !

!ColumnMajorMatrix methodsFor: 'operations' stamp: 'len 5/5/2023 17:25:58'!
value: aTuple
"	(self domain includes: aTuple) ifFalse: [^ DomainError signal]."
	(coefficients isEmpty or: [aTuple isEmpty]) ifTrue: [^ codomain zero].
	^ codomain coefficients:
		((coefficients species new: self height)
			replaceFrom: 1 to: self height
			withMatrix: coefficients height: self height width: self width transpose: false
			timesTuple: aTuple coefficients startingAt: 1)! !

!ColumnMajorMatrix methodsFor: 'testing' stamp: 'len 12/15/2022 12:50:45'!
isZero
	^ coefficients isZero! !

!ColumnMajorMatrix methodsFor: 'testing' stamp: 'len 12/15/2022 12:51:00'!
isZeroAt: aPoint
	^ coefficients isZeroAt: aPoint y - 1 * self height + aPoint x! !

!ColumnMajorMatrix methodsFor: 'testing' stamp: 'len 12/22/2022 19:04:05'!
isZeroAtColumn: anInteger
	| height start |
	height _ self height.
	start _ anInteger - 1 * height + 1.
	^ (coefficients findFirstNonzeroFrom: start to: start + height - 1) isNil! !

!ColumnMajorMatrix methodsFor: 'private' stamp: 'len 12/15/2022 12:43:08'!
coefficients
	^ coefficients! !

!ColumnMajorMatrix methodsFor: 'private' stamp: 'len 5/8/2023 16:32:11'!
coefficients: anArray
	coefficients _ anArray! !

!ColumnMajorMatrix methodsFor: 'private' stamp: 'len 12/15/2022 12:54:50'!
validate
	self assert: coefficients size = (domain rank * codomain rank).
	^ super validate! !

!ColumnMajorMatrix class methodsFor: 'as yet unclassified' stamp: 'len 12/16/2022 09:46:11'!
from: aDomain to: aCodomain coefficients: anArray
	^ self new domain: aDomain; codomain: aCodomain; coefficients: anArray! !

!PermutationMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/24/2022 16:57:44'!
= anObject
	^ (self class = anObject class and: [permutation = anObject permutation]) or: [super = anObject]! !

!PermutationMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:49:24'!
atRow: i column: j ifAbsent: exceptionBlock
	^ (permutation at: i) = j ifTrue: [self scalars one] ifFalse: [self scalars zero]! !

!PermutationMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/24/2022 16:57:53'!
permutation
	^ permutation! !

!PermutationMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:47:52'!
permutation: anArray
	permutation _ anArray! !

!PermutationMatrix class methodsFor: 'instance creation' stamp: 'len 12/22/2022 19:48:24'!
from: aDomain to: aCodomain permutation: anArray
	^ self new domain: aDomain; codomain: aCodomain; permutation: anArray! !

!TriangularMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:33:01'!
determinant
	^ self diagonal product! !

!TriangularMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:33:46'!
isTriangular
	^ true! !

!TriangularMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:32:39'!
isUnimodular
	^ self isSquare and: [self diagonal allSatisfy: [:each| each isUnit]]! !

!DiagonalMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/20/2022 17:31:55'!
atRow: i column: j ifAbsent: exceptionBlock
	i = j ifFalse: [^ self scalars zero].
	^ diagonal at: i! !

!DiagonalMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/20/2022 17:26:18'!
determinant
	^ diagonal product! !

!DiagonalMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:28:13'!
diagonal
	^ diagonal! !

!DiagonalMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:28:20'!
diagonal: aTuple
	diagonal _ aTuple! !

!DiagonalMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:34:22'!
isDiagonal
	^ true! !

!DiagonalMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/20/2022 17:43:37'!
isLowerTriangular
	^ true! !

!DiagonalMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/20/2022 17:43:41'!
isUpperTriangular
	^ true! !

!DiagonalMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:43:45'!
transpose
	^ self! !

!DiagonalMatrix class methodsFor: 'instance creation' stamp: 'len 12/22/2022 19:27:35'!
diagonal: aTuple
	| V |
	V _ aTuple parent.
	^ self from: V to: V diagonal: aTuple! !

!DiagonalMatrix class methodsFor: 'instance creation' stamp: 'len 12/22/2022 19:26:20'!
from: aDomain to: aCodomain diagonal: aTuple
	^ self new domain: aDomain; codomain: aCodomain; diagonal: aTuple! !

!LowerTriangularMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/20/2022 17:44:22'!
isLowerTriangular
	^ true! !

!LowerTriangularMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:44:55'!
transpose
	^ UpperTriangularMatrix from: codomain to: domain coefficients: coefficients! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:23:47'!
atRow: i column: j ifAbsent: exceptionBlock
	^ i = j ifTrue: [scalar] ifFalse: [scalar zero]! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:23:47'!
determinant
	self checkIsSquare.
	^ scalar ^ self domain rank! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:35:08'!
isDiagonal
	^ true! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:35:31'!
isLowerTriangular
	^ true! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:35:02'!
isScalar
	^ true! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:31:46'!
isUnimodular
	^ scalar isUnit! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:35:37'!
isUpperTriangular
	^ true! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:23:55'!
scalar
	^ scalar! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:25:03'!
scalar: anElement
	scalar _ anElement! !

!ScalarMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:45:45'!
transpose
	^ self! !

!ScalarMatrix class methodsFor: 'instance creation' stamp: 'len 12/22/2022 19:22:59'!
from: aDomain to: aCodomain scalar: anElement
	^ self new domain: aDomain; codomain: aCodomain; scalar: anElement! !

!UpperTriangularMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/20/2022 17:44:11'!
isUpperTriangular
	^ true! !

!UpperTriangularMatrix methodsFor: 'as yet unclassified' stamp: 'len 12/22/2022 19:45:08'!
transpose
	^ LowerTriangularMatrix from: codomain to: domain coefficients: coefficients! !

!GroupTuple methodsFor: 'accessing' stamp: 'len 4/25/2020 06:38:06'!
at: anInteger
	^ components at: anInteger! !

!GroupTuple methodsFor: 'accessing' stamp: 'len 4/25/2020 06:38:11'!
parent
	^ parent! !

!GroupTuple methodsFor: 'accessing' stamp: 'len 4/25/2020 06:38:16'!
size
	^ components size! !

!GroupTuple methodsFor: 'comparing' stamp: 'len 4/25/2020 06:38:33'!
= anObject
	^ self class = anObject class and: [components = anObject components]! !

!GroupTuple methodsFor: 'comparing' stamp: 'len 4/25/2020 06:38:37'!
hash
	^ components hash! !

!GroupTuple methodsFor: 'operations' stamp: 'len 11/24/2022 17:50:13'!
· anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #·].
	^ self class components: (components withIndexCollect: [:each :i| (parent at: i) isAdditiveGroup ifTrue: [each + (anElement at: i)] ifFalse: [each · (anElement at: i)]]) parent: parent! !

!GroupTuple methodsFor: 'operations' stamp: 'len 11/24/2022 17:49:59'!
inverse
	^ self class components: (components with: parent components collect: [:g :G| G isAdditiveGroup ifTrue: [g negated] ifFalse: [g inverse]]) parent: parent! !

!GroupTuple methodsFor: 'printing' stamp: 'len 4/25/2020 06:38:47'!
printOn: aStream
	aStream print: components! !

!GroupTuple methodsFor: 'private' stamp: 'len 4/25/2020 06:39:26'!
components
	^ components! !

!GroupTuple methodsFor: 'private' stamp: 'len 11/24/2022 17:49:10'!
components: anArray parent: aDirectProductGroup
	components _ anArray.
	parent _ aDirectProductGroup! !

!GroupTuple class methodsFor: 'instance creation' stamp: 'len 5/7/2023 17:24:17'!
components: anArray parent: aGroup
	^ self new components: anArray parent: aGroup! !

!SemidirectProductElement methodsFor: 'accessing' stamp: 'len 5/5/98 00:18'!
left
	"Answer the left component of the receiver."
	^ left! !

!SemidirectProductElement methodsFor: 'accessing' stamp: 'len 10/6/2016 15:37'!
parent
	^ parent! !

!SemidirectProductElement methodsFor: 'accessing' stamp: 'len 5/5/98 00:18'!
right
	"Answer the right component of the receiver."
	^ right! !

!SemidirectProductElement methodsFor: 'constants' stamp: 'len 1/23/2017 11:46:24'!
identity
	^ self parent identity! !

!SemidirectProductElement methodsFor: 'operations' stamp: 'len 5/5/2019 18:19:05'!
· aSemidirectProduct
	^ self parent
		project:
			{left · aSemidirectProduct left.
			parent action value: {aSemidirectProduct left. right} · aSemidirectProduct right}! !

!SemidirectProductElement methodsFor: 'operations' stamp: 'len 5/5/2019 18:17:09'!
inverse
	| leftInverse rightInverse |
	leftInverse _ left inverse.
	rightInverse _ right inverse.
	^ parent project: {leftInverse. (parent action value: {leftInverse. rightInverse}) inverse}! !

!SemidirectProductElement methodsFor: 'printing' stamp: 'len 2/12/2016 06:10'!
printOn: aStream
	aStream nextPut: $(; print: left; nextPutAll: ', '; print: right; nextPut: $)! !

!SemidirectProductElement methodsFor: 'private' stamp: 'len 5/5/98 00:17'!
left: anObject
	left _ anObject! !

!SemidirectProductElement methodsFor: 'private' stamp: 'len 10/6/2016 15:37'!
parent: aSemidirectProductGroup
	parent _ aSemidirectProductGroup! !

!SemidirectProductElement methodsFor: 'private' stamp: 'len 5/5/98 00:17'!
right: anObject
	right _ anObject! !

!SmallPrimeResidue methodsFor: 'operations' stamp: 'len 9/2/2018 21:50:50'!
* aModularInteger
	| b i p |
	aModularInteger class == self class ifFalse: [^ super * aModularInteger].
	b _ aModularInteger representative.
	(representative = 0 or: [b = 1]) ifTrue: [^ self].
	(representative = 1 or: [b = 0]) ifTrue: [^ aModularInteger].
	i _ (parent logTable at: representative) + (parent logTable at: b).
	i >= (p _ parent modulus) ifTrue: [i _ i - p + 1].
	^ parent representative: (parent expTable at: i)! !

!SmallPrimeResidue methodsFor: 'operations' stamp: 'len 5/28/2022 12:20:51'!
colift: anElement
	"Answer x such that xa = b, where a is the receiver and b is the argument. Answer nil if there's no solution.
	This is the right division of b by a, written b / a."
	| i |
	anElement class == self class ifFalse: [^ super colift: anElement].
	(anElement isZero or: [representative = 1]) ifTrue: [^ anElement].
	representative = 0 ifTrue: [^ nil].
	i _ (parent logTable at: anElement representative) - (parent logTable at: representative).
	i <= 0 ifTrue: [i _ i + parent modulus - 1].
	^ parent representative: (parent expTable at: i)! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/3/2019 17:26:18'!
<< anInteger
	^ self >> anInteger negated! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/3/2019 17:28:33'!
>> anInteger
	"Multiply the receiver by p^anInteger shifting the digits by anInteger places."
	^ parent digits: digits >> anInteger! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/5/2019 13:18:16'!
abs
	"Answer the p-adic absolute value (or norm) of the receiver."
	^ self isZero ifTrue: [0] ifFalse: [self p ^ self valuation negated]! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/2/2019 10:18:59'!
absolutePrecision
	^ parent precision! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/2/2019 16:35:37'!
at: anInteger
	anInteger < 0 ifTrue: [^ 0].
	^ digits at: anInteger + 1! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/1/2019 17:30:48'!
digits
	^ digits! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/1/2019 17:24:34'!
p
	^ parent p! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 12/17/2016 20:59:36'!
parent
	^ parent! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/2/2019 13:54:50'!
relativePrecision
	^ self absolutePrecision! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 12/29/2021 11:55:57'!
size
	^ Cardinal new: 1! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/2/2019 11:30:57'!
valuation
	self isZero ifTrue: [^ Infinity positive].
	^ (digits findFirst: [:x| x isZero not]) - 1
"	i _ 0.
	precision _ parent precision.
	[(digits at: (i _ i + 1)) isZero not ifTrue: [^ i].
	precision > i ifFalse: [^ i]] repeat"! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/2/2019 11:02:42'!
* anAdic
	| a b n carry  p |
	anAdic class = self class ifFalse: [^ self * (parent !! anAdic)].
	a _ digits.
	b _ anAdic digits.
	p _ self p.
	n _ 1.
	carry _ 0.
	^ parent streaming:
		[| t |
		t _ ((1 to: n) sum: [:i| (a at: i) * (b at: n-i+1)]) + carry.
		n _ n + 1.
		carry _ t // p.
		t \\ p]! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/1/2019 17:30:18'!
+ anAdic
	| a b n carry  p |
	a _ digits.
	b _ anAdic digits.
	p _ self p.
	n _ 1.
	carry _ 0.
	^ parent streaming:
		[| t |
		t _ (a at: n) + (b at: n) + carry.
		n _ n + 1.
		t < p ifTrue: [carry _ 0. t] ifFalse: [carry _ 1. t - p]]! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/1/2019 17:30:22'!
- anAdic
	| a b n carry  p |
	a _ digits.
	b _ anAdic digits.
	p _ self p.
	n _ 1.
	carry _ 0.
	^ parent streaming:
		[| t |
		t _ (a at: n) - (b at: n) - carry.
		n _ n + 1.
		t >= 0 ifTrue: [carry _ 0. t] ifFalse: [carry _ 1. t + p]]! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/2/2019 11:39:02'!
negated
	| n p |
	p _ self p.
	n _ 0.
	^ parent streaming: [n = 0 ifTrue: [p - (digits at: (n _ n + 1))] ifFalse: [p - 1 - (digits at: (n _ n + 1))]]! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/6/2019 03:43:40'!
reciprocal
	| p a b1 carry |
	p _ self p.
	a _ digits.
	b1 _ (a at: 1) reciprocalModulo: p.
	carry _ 0.
	^ parent digits:
		(Sequence
			initialValues: {b1}
			recurrence:
				[:b :n| | q |
				q _ b1 negated * (((2 to: n) sum: [:i| (a at: i) * (b at: n-i+1)]) + carry).
				carry _ q // p.
				q \\ p])! !

!RelaxedAdicInteger methodsFor: 'comparing' stamp: 'len 8/7/2019 06:47:58'!
= anAdic
	((anAdic isKindOf: RelaxedAdicInteger) and: [parent = anAdic parent]) ifFalse: [^ false].
	parent isBounded ifFalse: [^ false].
	"a bounded p-adics that are equal up to their precision are considered equal
	(but maybe it should answer always false, like Magma)"
	1 to: parent precision do: [:i|
		(digits at: i) = (anAdic digits at: i) ifFalse: [^ false]].
	^ true! !

!RelaxedAdicInteger methodsFor: 'comparing' stamp: 'len 6/3/2019 15:24:04'!
hash
	| answer |
	answer _ 0.
	1 to: 4 do: [:i| "just 4 terms? how to decide how many terms to use?"
		answer _ answer hashMultiply + (digits at: i) hash].
	^ answer! !

!RelaxedAdicInteger methodsFor: 'converting' stamp: 'len 8/7/2019 10:08:38'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the truncation to anInteger terms."
	^ (self parent jet: anInteger) !! self! !

!RelaxedAdicInteger methodsFor: 'printing' stamp: 'len 6/2/2019 10:09:25'!
printOn: aStream
	self printOn: aStream withPowersShiftedBy: 0! !

!RelaxedAdicInteger methodsFor: 'printing' stamp: 'len 5/23/2020 07:42:09'!
printOn: aStream withPowersShiftedBy: k
	| printPrecision first |
	self isZero
		ifTrue: [aStream nextPut: $0]
		ifFalse:
			[printPrecision _ 10.
			first _ true.
			self valuation - k to: self valuation + printPrecision - 1 - k do: [:i|
				| x |
				(x _ self digits at: i+1+k) isZero
					ifFalse:
						[first ifFalse: [aStream nextPutAll: ' + '].
						(x ~= 1 or: [i = 0]) ifTrue: [aStream print: x. i ~= 0 ifTrue: [aStream nextPut: $·]].
						i ~= 0 ifTrue: [aStream print: self p. i ~= 1 ifTrue: [aStream nextPutAll: i printString super]].
						first _ false]]].
	aStream nextPutAll: ' + ...'
	"parent isBounded
		ifTrue: [aStream nextPutAll: ' + ', 'O' italic; nextPut: $(; print: self p; nextPutAll: parent precision printString super; nextPut: $)]
		ifFalse: [aStream nextPutAll: ' + ...']"! !

!RelaxedAdicInteger methodsFor: 'testing' stamp: 'len 6/2/2019 10:00:28'!
isZero
	parent isBounded ifFalse: [^ false].
	"a bounded p-adic that is 0 up to its precision is considered 0
	(but maybe it should answers always false, like Magma)"
	1 to: parent precision do: [:i|
		(digits at: i) isZero ifFalse: [^ false]].
	^ true! !

!RelaxedAdicInteger methodsFor: 'private' stamp: 'len 6/1/2019 16:50:33'!
digits: aSequence
	digits _ aSequence! !

!RelaxedAdicInteger methodsFor: 'private' stamp: 'len 6/1/2019 16:50:14'!
parent: aPAdicIntegersRing
	parent _ aPAdicIntegersRing! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/5/2019 13:19:40'!
abs
	"Answer the p-adic absolute value (or norm) of the receiver."
	^ self isZero ifTrue: [0] ifFalse: [self p ^ valuation negated]! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:20:17'!
absolutePrecision
	^ unit absolutePrecision! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:37:29'!
denominator
	^ valuation < 0 ifTrue: [unit parent !! (self p ^ valuation negated)] ifFalse: [unit one]! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:21:46'!
numerator
	^ valuation <= 0 ifTrue: [unit] ifFalse: [unit >> valuation]! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:21:54'!
p
	^ unit p! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/5/2019 11:26:00'!
parent
	^ unit parent fractions! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/2/2019 10:46:39'!
relativePrecision
	^ self absolutePrecision - valuation! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:22:52'!
unit
	^ unit! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/2/2019 10:48:05'!
valuation
	^ valuation! !

!RelaxedAdicNumber methodsFor: 'printing' stamp: 'len 6/3/2019 18:28:16'!
printOn: aStream
	unit printOn: aStream withPowersShiftedBy: valuation negated! !

!RelaxedAdicNumber methodsFor: 'private' stamp: 'len 6/3/2019 18:26:19'!
valuation: anInteger unit: anAdicInteger
	valuation _ anInteger.
	unit _ anAdicInteger! !

!RelaxedAdicNumber class methodsFor: 'instance creation' stamp: 'len 6/3/2019 18:25:44'!
valuation: anInteger unit: anAdicInteger
	^ self new valuation: anInteger unit: anAdicInteger! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 12/27/2015 15:45'!
coefficients
	^ coefficients! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 12/27/2015 15:45'!
indeterminate
	^ 1! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 10/28/2016 10:37'!
parent
	^ parent! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 12/28/2015 04:12'!
scalars
	^ coefficients codomain! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 9/16/2022 08:08:19'!
terms
	| P |
	P _ self scalars polynomials.
	^ Sequence to: P map: [:i| P coefficient: (coefficients at: i) degree: i-1]! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 6/14/2019 01:33:51'!
valuation
	^ (self coefficients findFirst: [:each| each isZero not]) - 1! !

!RelaxedPowerSeries methodsFor: 'accessing-private' stamp: 'len 12/27/2015 15:46'!
coefficients: aSequence
	coefficients _ aSequence! !

!RelaxedPowerSeries methodsFor: 'accessing-private' stamp: 'len 10/28/2016 10:38'!
parent: aPowerSeriesRing
	parent _ aPowerSeriesRing! !

!RelaxedPowerSeries methodsFor: 'arithmetic' stamp: 'len 8/9/2019 09:43:30'!
* anObject
	((anObject isKindOf: RelaxedPowerSeries) and: [anObject parent = parent])
		ifTrue: [^ parent coefficients: (coefficients convolution: anObject coefficients)].
	(self scalars includes: anObject)
		ifTrue: [^ parent coefficients: coefficients * anObject].
	(anObject isKindOf: Polynomial)
		ifTrue: [^ self * (parent !! anObject)].
	^ DomainError signal! !

!RelaxedPowerSeries methodsFor: 'arithmetic' stamp: 'len 8/9/2019 09:43:30'!
+ anObject
	(anObject isKindOf: RelaxedPowerSeries)
		ifTrue: [^ parent coefficients: coefficients + anObject coefficients].
	(self scalars includes: anObject)
		ifTrue: [^ parent coefficients: coefficients + (CFiniteSequence to: self scalars initialValues: {anObject} periodic: {anObject zero})].
	(anObject isKindOf: Polynomial)
		ifTrue: [^ self + (parent !! anObject)].
	^ DomainError signal! !

!RelaxedPowerSeries methodsFor: 'arithmetic' stamp: 'len 6/2/2019 17:14:04'!
negated
	^ parent coefficients: coefficients negated! !

!RelaxedPowerSeries methodsFor: 'arithmetic' stamp: 'len 6/5/2019 17:04:52'!
reciprocal
	| b1 a |
	a _ coefficients.
	b1 _ (a at: 1) reciprocal.
	^ parent coefficients:
		(Sequence
			to: self scalars
			initialValues: {b1}
			recurrence:
				[:b :n|
				b1 negated * ((2 to: n) sum: [:i| (a at: i) * (b at: n-i+1)])])! !

!RelaxedPowerSeries methodsFor: 'comparing' stamp: 'len 8/9/2019 09:43:30'!
= anObject
	^ (anObject isKindOf: RelaxedPowerSeries) and: [coefficients = anObject coefficients]! !

!RelaxedPowerSeries methodsFor: 'comparing' stamp: 'len 12/28/2015 19:45'!
hash
	^ coefficients hash! !

!RelaxedPowerSeries methodsFor: 'converting' stamp: 'len 12/27/2015 19:09'!
asRationalFunction
	^ coefficients generatingFunction! !

!RelaxedPowerSeries methodsFor: 'converting' stamp: 'len 8/7/2019 10:08:50'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the truncation to anInteger terms."
	^ (self parent jet: anInteger) !! self! !

!RelaxedPowerSeries methodsFor: 'operations' stamp: 'len 6/2/2019 17:11:59'!
derivative
	^ parent coefficients: coefficients << 1 * CFiniteSequence identity! !

!RelaxedPowerSeries methodsFor: 'operations' stamp: 'len 2/29/2016 04:58'!
value: anObject upTo: n
	| answer |
	answer _ anObject zero.
	self terms do: [:each| each degree > n ifTrue: [^ answer]. answer _ answer + (each value: anObject)].
	^ answer! !

!RelaxedPowerSeries methodsFor: 'printing' stamp: 'len 6/6/2019 13:22:39'!
printOn: aStream
	| P f n |
	n _ coefficients printLimit.
	P _ parent base.
	f _ (P coefficients: (coefficients first: n)) orderedBy: #llex.
	aStream print: f; nextPutAll: ' + ', 'O' italic, '('; print: P x ^ (n+1); nextPut: $)! !

!RelaxedPowerSeries methodsFor: 'testing' stamp: 'len 4/11/2016 23:09'!
isZero
	^ coefficients isZero! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 9/16/2022 08:08:03'!
cos
	"The power series expansion of cos(x)."
	^ ℚ powerSeries coefficients:
		(Sequence to: ℚ map: [:n| n even ifTrue: [0] ifFalse: [|k| k _ n-1/2. (-1)^k / (2*k) factorial]])! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 9/16/2022 08:08:09'!
exp
	"The power series expansion of exp(x)."
	^ ℚ powerSeries
		coefficients: (Sequence to: ℚ map: [:n| 1 / n factorial])! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 9/16/2022 08:08:14'!
sin
	"The power series expansion of sin(x)."
	^ ℚ powerSeries
		coefficients: (Sequence to: ℚ map: [:n| n odd ifTrue: [0] ifFalse: [|k| k _ n/2-1. (-1)^k / (2*k+1) factorial]])! !

!RelaxedPowerSeries class methodsFor: 'instance creation' stamp: 'len 12/27/2015 16:21'!
coefficients: aSequence
	^ self new coefficients: aSequence! !

!RelaxedPowerSeries class methodsFor: 'instance creation' stamp: 'len 11/25/2016 10:32:44'!
fromPolynomial: aPolynomial
	^ self coefficients: (CFiniteSequence to: aPolynomial scalars initialValues: aPolynomial coefficients periodic: {aPolynomial scalars zero})! !

!GAlgebraElement methodsFor: 'accessing' stamp: 'len 7/4/2022 12:20:47'!
at: aMonomial
	^ representative at: aMonomial! !

!GAlgebraElement methodsFor: 'accessing' stamp: 'len 7/4/2022 12:20:54'!
independentCoefficient
	^ representative independentCoefficient! !

!GAlgebraElement methodsFor: 'accessing' stamp: 'len 7/4/2022 12:21:04'!
leadingCoefficient
	^ representative leadingCoefficient! !

!GAlgebraElement methodsFor: 'accessing' stamp: 'len 7/4/2022 12:21:07'!
leadingMonomial
	^ representative leadingMonomial! !

!GAlgebraElement methodsFor: 'accessing' stamp: 'len 7/4/2022 12:24:23'!
representative
	^ representative! !

!GAlgebraElement methodsFor: 'accessing' stamp: 'len 7/1/2022 05:40:43'!
scalars
	^ parent scalars! !

!GAlgebraElement methodsFor: 'comparing' stamp: 'len 7/4/2022 12:20:44'!
= anObject
	^ self class = anObject class and: [representative = anObject representative]! !

!GAlgebraElement methodsFor: 'comparing' stamp: 'len 7/4/2022 12:20:51'!
hash
	^ representative hash! !

!GAlgebraElement methodsFor: 'operations' stamp: 'len 7/4/2022 12:20:00'!
* anElement
	(anElement isInteger or: [self scalars includes: anElement])
		ifTrue: [^ self class representative: representative * anElement parent: parent].
	(parent includes: anElement)
		ifTrue: [^ self multipliedBy: anElement].
	(anElement isKindOf: Monomial)
		ifTrue: [^ self multipliedByMonomial: anElement].
	^ self adapt: anElement andSend: #*! !

!GAlgebraElement methodsFor: 'operations' stamp: 'len 7/4/2022 12:20:19'!
+ anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	^ self class representative: representative + anElement representative parent: parent! !

!GAlgebraElement methodsFor: 'operations' stamp: 'len 7/4/2022 12:20:36'!
- anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #-].
	^ self class representative: representative - anElement representative parent: parent! !

!GAlgebraElement methodsFor: 'operations' stamp: 'len 7/4/2022 12:21:22'!
negated
	^ self class representative: representative negated parent: parent! !

!GAlgebraElement methodsFor: 'printing' stamp: 'len 7/4/2022 12:21:25'!
printOn: aStream
	representative printOn: aStream! !

!GAlgebraElement methodsFor: 'testing' stamp: 'len 7/4/2022 12:20:57'!
isConstant
	^ representative isConstant! !

!GAlgebraElement methodsFor: 'testing' stamp: 'len 7/4/2022 12:21:00'!
isOne
	^ representative isOne! !

!GAlgebraElement methodsFor: 'testing' stamp: 'len 7/4/2022 12:21:02'!
isZero
	^ representative isZero! !

!GAlgebraElement methodsFor: 'private' stamp: 'len 7/4/2022 12:21:15'!
multipliedBy: anElement
	| answer |
	answer _ self zero.
	representative associationsDo: [:each|
		anElement representative associationsDo: [:other|
			answer _ (parent multiplyMonomial: each key timesMonomial: other key) * (each value * other value) + answer]].
	^ answer! !

!GAlgebraElement methodsFor: 'private' stamp: 'len 7/1/2022 06:25:07'!
multipliedByMonomial: aMonomial
	^ self multipliedBy: (parent coefficient: parent scalars one monomial: aMonomial)! !

!GAlgebraElement methodsFor: 'private' stamp: 'len 7/1/2022 05:38:46'!
parent
	^ parent! !

!GAlgebraElement methodsFor: 'private' stamp: 'len 7/4/2022 12:21:43'!
representative: aPolynomial parent: aRing
	representative _ aPolynomial.
	parent _ aRing! !

!GAlgebraElement class methodsFor: 'instance creation' stamp: 'len 7/4/2022 12:23:52'!
representative: aPolynomial parent: aGAlgebra
	^ self new representative: aPolynomial parent: aGAlgebra! !

!PathAlgebraElement methodsFor: 'as yet unclassified' stamp: 'len 7/9/2022 11:15:24'!
+ anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	^ self class coefficients: coefficients + anElement coefficients parent: parent! !

!PathAlgebraElement methodsFor: 'as yet unclassified' stamp: 'len 7/9/2022 11:17:17'!
- anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #-].
	^ self class coefficients: coefficients - anElement coefficients parent: parent! !

!PathAlgebraElement methodsFor: 'as yet unclassified' stamp: 'len 7/9/2022 11:17:23'!
= anObject
	^ self == anObject or: [self class = anObject class and: [coefficients = anObject coefficients]]! !

!GaussianElimination2 methodsFor: 'accessing' stamp: 'len 12/13/2022 13:12:29'!
determinant
	"Answer the determinant of the matrix."
	| determinant |
	determinant _ echelon scalars one.
	1 to: echelon height do: [:i|
		determinant _ determinant * (echelon at: i@i)].
	^ determinant / determinantFactor! !

!GaussianElimination2 methodsFor: 'operations' stamp: 'len 12/19/2022 10:14:52'!
addRow: i0 times: aNumber to: i1
	self addRow: i0 times: aNumber to: i1 startingAt: currentPivot y! !

!GaussianElimination2 methodsFor: 'operations' stamp: 'len 1/19/2018 21:54:00'!
multiplyRow: i by: aNumber
	self multiplyRow: i by: aNumber startingAt: currentPivot y! !

!GaussianElimination2 methodsFor: 'operations' stamp: 'len 11/29/2022 19:22:41'!
multiplyRow: i by: aNumber startingAt: startIndex
	super multiplyRow: i by: aNumber startingAt: startIndex.
	determinantFactor _ determinantFactor * aNumber! !

!GaussianElimination2 methodsFor: 'operations' stamp: 'len 12/12/2022 13:43:37'!
swapRow: i
	"Swap the anInteger-th row with the row at the current pivot."
	i = currentPivot x ifTrue: [^ self].
	self swapRow: i with: currentPivot x.
	pivots removeLast.
	currentPivot _ pivots add: i @ currentPivot y! !

!GaussianElimination2 methodsFor: 'operations' stamp: 'len 11/29/2022 19:23:22'!
swapRow: i with: j
	super swapRow: i with: j.
	determinantFactor _ determinantFactor negated! !

!GaussianElimination2 methodsFor: 'private' stamp: 'len 12/13/2022 13:12:29'!
nextPivot
	"Advance the position to the next available pivot."
	| col |
	col _ currentPivot y + 1.
	[col <= echelon width]
		whileTrue:
			[1 to: echelon height do: [:each|
				((pivots anySatisfy: [:one| one x = each]) or: [echelon isZeroAt: each @ col])
						ifFalse: [^ currentPivot _ pivots add: each @ col]].
			col _ col + 1].
	^ nil! !

!GaussianElimination2 methodsFor: 'private' stamp: 'len 12/19/2022 10:12:43'!
reduceStep
	"Perform one step in the reduction of the matrix using the current pivot."
	| row col |
	row _ currentPivot x.
	col _ currentPivot y.
	self multiplyRow: row by: (echelon at: row @ col) inverse.
	1 to: echelon height do: [:i|
		(i = row or: [echelon isZeroAt: i @ col])
			ifFalse: [self addRow: row times: (echelon at: i @ col) negated to: i]]! !

!GaussianElimination2 methodsFor: 'private' stamp: 'len 12/13/2022 13:23:24'!
run
	| row |
	pivots _ OrderedCollection new: matrix height.
	currentPivot _ 0 @ 0.
	determinantFactor _ matrix scalars one.
	row _ 1.
	[self nextPivot isNil]
		whileFalse:
			[self swapRow: row.
			row _ row + 1.
			self reduceStep]! !

!BerlekampMassey methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 09:33:44'!
run
	| F Fx x C L N m B d b s |
	x _ Fx x.
	C _ Fx one.
	L _ 0. "current number of errors"
	N _ 100. "total number of syndromes"
	B _ C.
	b _ F one. "copy of the last discrepancy d"
	m _ 1. "number of iterations since last update"
	0 to: N do: [:n|
		"calculate discrepancy:"
		d _ (s at: n) + ((1 to: L) sum: [:i| (C at: i) * (s at: n-i)]).
		d = 0
			ifTrue: [m _ m + 1]
			ifFalse: [2*L <= n
				ifTrue:
					[|t|
					t _ C.
					C _ C - ((x^m)*(d/b)*B).
					B _ t.
					L _ n + 1 - L.
					b _ d.
					m _ 1]
				ifFalse:
					[C _ C - ((x^m)*(d/b)*B).
					m _ m + 1]]].
	^ L! !

!ExtendedSmithReduction methodsFor: 'as yet unclassified' stamp: 'len 5/5/2019 12:43:36'!
matrix
	^ matrix! !

!ExtendedSmithReduction methodsFor: 'as yet unclassified' stamp: 'len 5/5/2019 12:44:19'!
matrix: aMatrix
	matrix _ aMatrix! !

!ExtendedSmithReduction methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 09:43:10'!
reduceStep: i mod: D
	| j c euclid B Ai Aj |
	"2. Initialize j for row reduction:"
	[j _ i-1. c _ 0.
	"3. Check zero:"
	[[j >= 1 and: [(S at: i@j) isZero]] whileTrue: [j _ j - 1].
	j >= 1]
		whileTrue: "4. Euclidean step:"
			[euclid _ ExtendedEuclid on: {S at: i@i. S at: i@j}.
"			euclid verifyMinimal ifFalse: [self halt]."
			B _ (Ai _ S columnAt: i) * euclid bezout first + ((Aj _ S columnAt: j) * euclid bezout second).
			S columnAt: j put: Aj * ((S at: i@i) // euclid gcd) - (Ai * ((S at: i@j) // euclid gcd)) \\ D.
			S columnAt: i put: B \\ D.
			j _ j - 1].
	"5. Initialize j for column reduction:"
	j _ i-1.
	"6. Check zero (find pivot):"
	[[j >= 1 and: [(S at: j@i) isZero]] whileTrue: [j _ j - 1].
	j >= 1]
		whileTrue: "7. Euclidean step:"
			[euclid _ ExtendedEuclid on: {S at: i@i. S at: j@i}.
"			euclid verifyMinimal ifFalse: [self halt]."
			B _ (Ai _ S rowAt: i) * euclid bezout first + ((Aj _ S rowAt: j) * euclid bezout second).
			S rowAt: j put: Aj * ((S at: i@i) // euclid gcd) - (Ai * ((S at: j@i) // euclid gcd)) \\ D.
			S rowAt: i put: B \\ D.
			j _ j - 1.
			c _ c + 1].
	c > 0] whileTrue "8. Repeat stage i?"! !

!ExtendedSmithReduction methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 09:42:05'!
run
	| n i D b k |
	n _ matrix height.
	L _ matrix codomain id.
	R _ matrix domain id.
	S _ matrix copy.
	elementaryDivisors _ OrderedCollection new: n.
	"1. Initialize i:"
	i _ n.
	D _ S determinant normalized.
	n = 1 ifTrue: [^ self].
	"2-9. Reduction:"
	[[self reduceStep: i mod: D.
	"9. Check the rest of the matrix:"
	b _ S at: i@i.
	k _ (1 to: i-1) detect: [:k0| ((1 to: i-1) allSatisfy: [:l| b | (S at: k0@l)]) not] ifNone: [].
	k isNil]
		whileFalse:
			[S rowAt: i put: (S rowAt: i) + (S rowAt: k).
			L rowAt: i put: (L rowAt: i) + (L rowAt: k)].
	"10. Next stage:"
	elementaryDivisors add: ((S at: i@i) gcd: D).
	D _ D // elementaryDivisors last.
	i = 2]
		whileFalse: [i _ i - 1].
	elementaryDivisors add: ((S at: 1@1) gcd: D).
	^ {L. S. R}! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 4/5/2018 15:36:20'!
addColumn: a to: B
	^ (B columnAugmented: a) HNF columnAt: B width + 1! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 4/5/2018 15:36:28'!
addRow: b to: H
	| H0 |
	H0 _ H isSquare
		ifTrue: [H]
		ifFalse:
			[H rowAugmented: (H scalars ^ (H width - 1) :: zero, (H rowAugmented: b) determinant)].
	"..."! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 7/16/2016 01:28'!
matrix: aMatrix
	matrix _ aMatrix! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 4/12/2021 12:37:29'!
probablePivotsOf: aMatrix
	^ aMatrix % (10007 to: 46000) atRandom nextPrime :: echelon pivots
! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 7/16/2016 04:35'!
run
"
	HNF _ matrix zero.
	HNF at: 1@1 put: (matrix at: 1@1).
	2 to: matrix numberOfColumns do: [:j|
		a _ matrix columnAt: j.
		x _ self addColumn: a.
		HNF _ addRow: a to: HNF | x
"! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:24:18'!
divide: anElement
	"Perform the division algorithm computing quotients, remainder and a unit."
	^ self notYetImplemented! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:25:02'!
divisors: anArray
	divisors _ anArray! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:18:30'!
reduce: anElement 
	"Answer Mora's normal form of anElement."
	| h L lh g ecartg |
	h _ anElement.
	L _ OrderedCollection withAll: divisors.
	[h isZero ifTrue: [^ h].
	lh _ h leadingMonomial.
	g _ nil.
	ecartg _ -1.
	L do: [:each|
		| ecart |
		(each leadingMonomial | lh and: [ecart _ each ecart. g isNil or: [ecart < ecartg]])
			ifTrue: [g _ each. ecartg _ ecart]].
	g isNil ifTrue: [^ h].
	ecartg > h ecart ifTrue: [L add: h].
	h _ h + (g * (lh / g leadingMonomial) * (h leadingCoefficient negated / g leadingCoefficient))] repeat! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:18:57'!
reduceFull: anElement
	^ self reduce: anElement! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:19:20'!
reduces: anElement
	^ (self reduce: anElement) isZero! !

!MultivariateReducer class methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:25:16'!
divisors: anArray
	^ self new divisors: anArray! !

!GlobalMultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:23:36'!
divide: anElement
	"Perform the division algorithm computing quotients and remainder."
	| R quotients remainder p s fi q head i |
	R _ anElement isTuple ifTrue: [anElement scalars] ifFalse: [anElement parent].
	quotients _ Array new: divisors size withAll: R zero.
	divisors isEmpty ifTrue: [remainder _ anElement. ^ self].
	remainder _ anElement zero.
	s _ divisors size.
	p _ anElement.
	[p isZero] whileFalse:
		[head _ p leadingMonomial.
		i _ 1.
		[(fi _ divisors at: i) leadingMonomial | head or: [(i _ i + 1) > s]] whileFalse.
		i <= s "i.e. found fi with leading monomial dividing head"
			ifTrue:
				[q _ R coefficient: p leadingCoefficient / fi leadingCoefficient monomial: head / fi leadingMonomial.
				quotients at: i put: (quotients at: i) + q.
				p _ p + (fi * q negated)]
			ifFalse:
				[remainder _ remainder + p leadingTerm.
				p _ p tail]].
	^ {quotients. remainder}! !

!GlobalMultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:23:11'!
reduce: anElement
	"Answer a top-reduced normal form of anElement (without doing tail reduction)."
	| h |
	divisors isEmpty ifTrue: [^ anElement].
	h _ anElement.
	[h isZero]
		whileFalse:
			[| lh lg g |
			lh _ h leadingMonomial.
			g _ divisors detect: [:one| (lg _ one leadingMonomial) | lh] ifNone: [^ h].
			h _ h + (g * (lh / lg) * (h leadingCoefficient negated / g leadingCoefficient))].
	^ h! !

!GlobalMultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:22:49'!
reduceFull: anElement
	"Answer the tail-reduced normal form of anElement (doing full tail reduction)."
	| g h |
	g _ anElement.
	h _ g zero.
	[g isZero]
		whileFalse:
			[g _ self reduce: g.
			g isZero ifFalse: [h _ h + g leadingTerm. g _ g tail]].
	^ h normalized! !

!NTT methodsFor: 'accessing' stamp: 'len 4/25/2016 00:28'!
modulus
	^ modulus! !

!NTT methodsFor: 'initialization' stamp: 'len 5/20/2023 12:10:36'!
modulus: anInteger
	self modulus: anInteger primitiveRoot: anInteger primitiveRoot! !

!NTT methodsFor: 'initialization' stamp: 'len 5/20/2023 12:10:24'!
modulus: anInteger primitiveRoot: anotherInteger
	modulus := anInteger.
	primitiveRoot := anotherInteger! !

!NTT methodsFor: 'running' stamp: 'len 5/20/2023 12:13:15'!
inverseValue: input
	"Naive algorithm, O(n²)."
	^ (1 to: input size) collect: [:i| ((1 to: input size) sum: [:j| (primitiveRoot negated raisedTo: i*j modulo: modulus) * (input at: j) \\ modulus]) \\ modulus]! !

!NTT methodsFor: 'running' stamp: 'len 5/20/2023 12:13:26'!
value: input
	"Naive algorithm, O(n²)."
	^ (1 to: input size) collect: [:i| ((1 to: input size) sum: [:j| (primitiveRoot raisedTo: i*j modulo: modulus) * (input at: j) \\ modulus]) \\ modulus]! !

!NTT class methodsFor: 'instance creation' stamp: 'len 5/20/2023 12:12:21'!
modulus: anInteger
	^ self new modulus: anInteger! !

!OEIS methodsFor: 'as yet unclassified' stamp: 'len 4/27/2016 07:02'!
initialize
	entries _ OrderedCollection new.
	'oeis.txt' asFileEntry readStream: [:aStream|
		[aStream atEnd]
			whileFalse:
				[| line |
				line _ aStream nextLine.
				line first = $#
					ifFalse:
						[| name tokens |
						tokens _ line findTokens: ','.
						name _ tokens first allButLast.
						entries add: name -> (tokens allButFirst collect: [:each| each asInteger]) asArray]]].
	entries _ entries asArray sort: [:a :b| a value size >= b value size]! !

!OEIS methodsFor: 'as yet unclassified' stamp: 'len 4/27/2016 07:02'!
longestEntrySize
	^ entries first value size! !

!OEIS methodsFor: 'as yet unclassified' stamp: 'len 4/20/2016 21:41'!
lookup2: anArray
	'oeis.txt' asFileEntry readStream: [:aStream|
		[aStream atEnd]
			whileFalse:
				[| line |
				line _ aStream nextLine.
				line first = $#
					ifFalse:
						[| name tokens |
						tokens _ line findTokens: ','.
						name _ tokens first allButLast.
						((1 to: (tokens size - 1 min: anArray size))
							allSatisfy: [:i| (tokens at: i+1) asInteger = (anArray at: i)])
								ifTrue: [^ name]]]].
		^ nil! !

!OEIS methodsFor: 'as yet unclassified' stamp: 'len 4/27/2016 07:04'!
lookup: anArray
	| array |
	array _ (anArray is: #Sequence) ifTrue: [anArray copyFrom: 1 to: self longestEntrySize] ifFalse: [anArray].
	entries do: [:each|
		each value size = 0 ifTrue: [^ nil].
		((1 to: (each value size min: array size))
			allSatisfy: [:i| (each value at: i) = (array at: i)])
				ifTrue: [^ each key]].			
	^ nil! !

!OEIS class methodsFor: 'as yet unclassified' stamp: 'len 4/25/2016 07:16'!
default
	^ Default ifNil: [Default _ self new]! !

!RealBox methodsFor: 'printing' stamp: 'len 5/23/2020 07:19:23'!
printOn: aStream
	components do: [:each| aStream print: each] separatedBy: [aStream nextPut: $×]! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:33'!
add: aRealInterval
	^ components add: aRealInterval! !

!RealBox methodsFor: 'accessing' stamp: 'len 12/13/97 15:21'!
addAll: aCollection
	"Add the elements in the argument to the receiver. Answer the argument."

	aCollection do: [ :each | self add: each].
	^ aCollection! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:33'!
at: anInteger
	"Answer the anInteger-th component of the receiver."
	^ components at: anInteger! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:35'!
dimension
	^ components inject: 0 into: [:sum :each| sum + each]! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:35'!
first
	^ self at: 1! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:35'!
last
	^ self at: self size! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:36'!
measure
	^ components inject: 1 into: [:measure :each| measure * each measure]! !

!RealBox methodsFor: 'accessing' stamp: 'len 11/23/2022 17:14:31'!
midpoint
	^ ℝ !!!! (components collect: [:each| each midpoint])! !

!RealBox methodsFor: 'accessing' stamp: 'len 11/23/2022 21:31:50'!
sample
	"Answer a sample point in the receiver."
	^ ℝ^self size fill: [:each| (self at: each) sample]! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:37'!
size
	^ components size! !

!RealBox methodsFor: 'accessing-private' stamp: 'len 1/11/2016 02:37'!
components
	^ components! !

!RealBox methodsFor: 'accessing-private' stamp: 'len 1/11/2016 02:37'!
components: aCollection
	components _ aCollection! !

!RealBox methodsFor: 'comparing' stamp: 'len 12/19/97 22:06'!
= anObject
	"Answer true if the receiver equals the argument."

	self size = anObject size ifFalse: [^ false].
	1 to: self size do: [ :each | (self at: each) = (anObject at: each) ifFalse: [^ false]].
	^ true! !

!RealBox methodsFor: 'comparing' stamp: 'len 6/28/2016 09:28'!
hash
	"Answer the hash value of the receiver."

	^ self size = 0
		ifTrue: [0]
		ifFalse: [self first hash hashMultiply + self last hash hashMultiply + self size hash]! !

!RealBox methodsFor: 'enumerating' stamp: 'len 1/11/2016 02:38'!
do: aBlock
	"Enumerate the components of the receiver."
	components do: aBlock! !

!RealBox methodsFor: 'initialization' stamp: 'len 1/11/2016 02:38'!
initialize
	components _ OrderedCollection new: 3! !

!RealBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:40'!
includes: aTuple
	1 to: self size do: [:each|
		((self at: each) includes: (aTuple at: each)) ifFalse: [^ false]].
	^ true! !

!RealBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:40'!
isBounded
	self do: [:each| each isBounded ifFalse: [^ false]].
	^ true! !

!RealBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:40'!
isClosed
	self do: [:each| each isClosed ifFalse: [^ false]].
	^ true! !

!RealBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:41'!
isOpen
	self do: [ :each | each isOpen ifFalse: [^ false]].
	^ true! !

!SmallBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:44:06'!
atAllPut: aBit
	| bit |
	bit _ aBit.
	bit isInteger ifTrue: [bit _ bit even ifTrue: [Zero] ifFalse: [One]].
	bit = One
		ifTrue: [bits _ 1 << size - 1]
		ifFalse: [bit = Zero ifTrue: [bits _ 0] ifFalse: [self error: 'not a bit']]! !

!SmallBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:44:06'!
bitAt: anInteger
	^ bits bitAt: anInteger! !

!SmallBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:44:06'!
bitAt: anInteger put: oneOrZero
	(anInteger > self size and: [oneOrZero ~= 0]) ifTrue: [self error: 'index out of bounds'].
	bits _ bits bitAt: anInteger put: oneOrZero.
	^ oneOrZero! !

!SmallBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:44:06'!
lastNonzeroIndex
	"Answer the index of the last nonzero entry, or nil."
	| n |
	^ (n _ bits highBit) = 0 ifFalse: [n]! !

!SmallBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:44:06'!
size
	^ size! !

!SmallBitArray methodsFor: 'comparing' stamp: 'len 5/4/2023 23:44:06'!
= anObject
	^ self class = anObject class and: [size = anObject size and: [bits = anObject asInteger]]! !

!SmallBitArray methodsFor: 'comparing' stamp: 'len 5/4/2023 23:44:06'!
hash
	^ bits hash! !

!SmallBitArray methodsFor: 'converting' stamp: 'len 5/4/2023 23:44:06'!
asInteger
	^ bits! !

!SmallBitArray methodsFor: 'copying' stamp: 'len 5/4/2023 23:44:06'!
, anObject
	self flag: #deprecated.
	anObject class = self class ifFalse: [^ super , anObject].
	^ self class new: self size+anObject size bits: (bits bitXor: (anObject asInteger bitShift: self size))! !

!SmallBitArray methodsFor: 'copying' stamp: 'len 5/4/2023 23:44:06'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."
	| n |
	n _ stop - start + 1 max: 0.
	^ self class new: n bits: ((bits bitShift: 1 - start) bitAnd: (1 bitShift: n) - 1)! !

!SmallBitArray methodsFor: 'copying' stamp: 'len 5/4/2023 23:44:06'!
copyWith: aBit
	| answer |
	answer _ self class new: self size + 1 bits: bits.
	aBit = One ifTrue: [answer at: self size+1 put: One. ^ answer].
	aBit = Zero ifFalse: [DomainError signal].
	^ answer! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
* anObject
	anObject isInteger ifTrue: [^ anObject even ifTrue: [self zero] ifFalse: [self]].
	self class = anObject class ifFalse: [^ super * anObject].
	^ self class new: (self size min: anObject size) bits: (bits bitAnd: anObject asInteger)! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
+ anObject
	anObject class = self class ifFalse: [^ (((anObject isInteger and: [anObject even]) or: [anObject isZero]) or: [size = 0]) ifTrue: [self] ifFalse: [self class new: size bits: (bits bitXor: (1 << size - 1))]].
	^ self class
		new: (self size max: anObject size)
		bits: (bits bitXor: anObject asInteger)! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
>> anInteger
	^ self class new: size bits: ((bits bitShift: anInteger) bitAnd: 1<<size - 1)! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
· aTuple
	^ (bits bitAnd: aTuple asInteger) bitParity = 0 ifTrue: [Zero] ifFalse: [One]! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
autoconvolution
	self flag: #fixme. "TODO: optimize"
	^ self class new: (size * 2 - 1 max: 0) bits: (bits bitConvolution: bits)! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
convolution: aBitTuple
	^ self class new: (size + aBitTuple size - 1 max: 0) bits: (bits bitConvolution: aBitTuple asInteger)! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
parity
	^ bits bitParity! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
reduceBy: aBitTuple
	| r b n m |
	b _ aBitTuple asInteger.
	(m _ b highBit) = 0 ifTrue: [^ self error: 'division by zero'].
	m _ b highBit.
	r _ bits.
	[(n _ r highBit) >= m]
		whileTrue:
			[r _ r bitXor: b << (n-m)]. "also wasting some memory here"
	bits _ r! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
reduceBy: aBitTuple leadingInverse: inv addingQuotientTo: anotherBitTuple
	| q r b n m |
	anotherBitTuple isNil ifTrue: [^ self reduceBy: aBitTuple].
	b _ aBitTuple asInteger.
	(m _ b highBit) = 0 ifTrue: [^ self error: 'division by zero'].
	m _ b highBit.
	q _ 0.
	r _ bits.
	[(n _ r highBit) >= m]
		whileTrue:
			[| i |
			i _ n-m.
			q _ q bitXor: 1 << i. "wasting some memory while creating 2^i"
			r _ r bitXor: b << i]. "also wasting some memory here"
	bits _ r.
	q highBit <= anotherBitTuple size ifFalse: [^ self error: 'index out of bounds']. "unnecesary?"
	anotherBitTuple bits: (anotherBitTuple asInteger bitXor: q) size: anotherBitTuple size! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
reversed
	^ self class new: self size bits: (bits bitReverse: self size)! !

!SmallBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:44:06'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ self asInteger bitCount! !

!SmallBitArray methodsFor: 'operations-low level' stamp: 'len 5/4/2023 23:44:06'!
replaceFrom: start to: stop with: replacement startingAt: repStart	
	| mask n slice |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
	mask _ 1<<n - 1.
	slice _ replacement asInteger >> (repStart-1) bitAnd: mask.
	bits _ (bits bitAnd: (mask << (start-1)) bitInvert) bitXor: slice << (start-1)! !

!SmallBitArray methodsFor: 'operations-low level' stamp: 'len 5/4/2023 23:44:06'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2	
	| mask n slice |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
	mask _ 1<<n - 1.
	slice _ anArray1 asInteger >> (start1-1) bitAnd: mask.
	slice _ (anArray2 asInteger >> (start2-1) bitAnd: mask) bitXor: slice.
	bits _ (bits bitAnd: (mask << (start-1)) bitInvert) bitXor: slice << (start-1)! !

!SmallBitArray methodsFor: 'operations-low level' stamp: 'len 5/4/2023 23:44:06'!
replaceFrom: start to: stop with: anArray startingAt: srcStart plusScalar: anElement
	| mask n slice |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
	mask _ 1<<n - 1.
	slice _ anArray asInteger >> (srcStart-1) bitAnd: mask.
	anElement isZero ifFalse: [slice _ slice bitInvert bitAnd: mask].
	bits _ (bits bitAnd: (mask << (start-1)) bitInvert) bitXor: slice << (start-1)! !

!SmallBitArray methodsFor: 'operations-low level' stamp: 'len 5/4/2023 23:44:06'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2
	| mask n slice |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
	mask _ 1<<n - 1.
	slice _ anArray1 asInteger >> (start1-1) bitAnd: mask.
	slice _ anArray2 asInteger >> (start2-1) bitAnd: slice.
	bits _ (bits bitAnd: (mask << (start-1)) bitInvert) bitXor: slice << (start-1)! !

!SmallBitArray methodsFor: 'testing' stamp: 'len 5/4/2023 23:44:06'!
isZero
	^ bits = 0! !

!SmallBitArray methodsFor: 'private' stamp: 'len 5/4/2023 23:44:06'!
bits: anInteger size: anotherInteger
	bits _ anInteger.
	size _ anotherInteger! !

!SmallBitArray methodsFor: 'private' stamp: 'len 5/4/2023 23:44:06'!
validate
	self assert: (bits >= 0 and: [size >= 0 and: [bits highBit <= size]])! !

!SmallBitArray class methodsFor: 'instance creation' stamp: 'len 5/4/2023 23:44:06'!
bits: anInteger
	^ self new: anInteger highBit bits: anInteger! !

!SmallBitArray class methodsFor: 'instance creation' stamp: 'len 5/4/2023 23:44:06'!
new: anInteger
	^ self new: anInteger bits: 0! !

!SmallBitArray class methodsFor: 'instance creation' stamp: 'len 5/4/2023 23:44:06'!
new: anInteger bits: anotherInteger
	self assert: anotherInteger highBit <= anInteger.
	^ self basicNew bits: anotherInteger size: anInteger! !

!SmallBitArray class methodsFor: 'instance creation' stamp: 'len 5/4/2023 23:44:06'!
newFrom: anInteger
	anInteger isInteger ifTrue: [^ self bits: anInteger].
	^ super newFrom: anInteger! !

!SmithReduction methodsFor: 'accessing' stamp: 'len 5/28/2019 08:51:31'!
invariants
	invariants isNil ifTrue: [self run].
	^ invariants! !

!SmithReduction methodsFor: 'accessing' stamp: 'len 5/25/2019 10:28:41'!
matrix
	^ matrix! !

!SmithReduction methodsFor: 'accessing' stamp: 'len 5/25/2019 10:28:41'!
matrix: aMatrix
	matrix _ aMatrix! !

!SmithReduction methodsFor: 'accessing' stamp: 'len 6/1/2020 12:00:45'!
reduced
	^ (matrix scalars^self invariants size) endomorphisms diagonal: self invariants! !

!SmithReduction methodsFor: 'private' stamp: 'len 1/9/2022 09:42:37'!
reduceStep: i mod: R
	| j c euclid B Ai Aj |
	"2. Initialize j for row reduction:"
	[j _ i-1. c _ 0.
	"3. Check zero:"
	[[j >= 1 and: [(matrix at: i@j) isZero]] whileTrue: [j _ j - 1].
	j >= 1]
		whileTrue: "4. Euclidean step:"
			[euclid _ ExtendedEuclid on: {matrix at: i@i. matrix at: i@j}.
"			euclid verifyMinimal ifFalse: [self halt]."
			B _ (Ai _ matrix columnAt: i) * euclid bezout first + ((Aj _ matrix columnAt: j) * euclid bezout second).
			matrix columnAt: j put: Aj * ((matrix at: i@i) // euclid gcd) - (Ai * ((matrix at: i@j) // euclid gcd)) \\ R.
			matrix columnAt: i put: B \\ R.
			j _ j - 1].
	"5. Initialize j for column reduction:"
	j _ i-1.
	"6. Check zero (find pivot):"
	[[j >= 1 and: [(matrix at: j@i) isZero]] whileTrue: [j _ j - 1].
	j >= 1]
		whileTrue: "7. Euclidean step:"
			[euclid _ ExtendedEuclid on: {matrix at: i@i. matrix at: j@i}.
"			euclid verifyMinimal ifFalse: [self halt]."
			B _ (Ai _ matrix rowAt: i) * euclid bezout first + ((Aj _ matrix rowAt: j) * euclid bezout second).
			matrix rowAt: j put: Aj * ((matrix at: i@i) // euclid gcd) - (Ai * ((matrix at: j@i) // euclid gcd)) \\ R.
			matrix rowAt: i put: B \\ R.
			j _ j - 1.
			c _ c + 1].
	c > 0] whileTrue "8. Repeat stage i?"! !

!SmithReduction methodsFor: 'private' stamp: 'len 1/9/2022 09:42:47'!
run
	| n i R b k |
	n _ matrix height.
	invariants _ OrderedCollection new: n.
	"1. Initialize i:"
	i _ n.
	R _ matrix determinant.
	R _ R normalized. "replaces R _ R abs"
	n = 1 ifTrue: [invariants add: R. ^ self].
	"2-9. Reduction:"
	[[self reduceStep: i mod: R.
	"9. Check the rest of the matrix:"
	b _ matrix at: i@i.
	k _ (1 to: i-1) detect: [:k0| ((1 to: i-1) allSatisfy: [:l| b | (matrix at: k0@l)]) not] ifNone: [].
	k isNil]
		whileFalse: [matrix rowAt: i put: (matrix rowAt: i) + (matrix rowAt: k)].
	"10. Next stage:"
	invariants add: ((matrix at: i@i) gcd: R).
	R _ R // invariants last.
	i = 2]
		whileFalse: [i _ i - 1].
	invariants add: ((matrix at: 1@1) gcd: R).
"	diagonal _ matrix scalars tuple: diagonal"! !

!SparseTuple methodsFor: 'copying' stamp: 'len 7/18/2020 06:12:12'!
postCopy
	components _ components copy! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 05:41:20'!
at: key
	^ components at: key ifAbsent: [self scalars zero]! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 05:42:55'!
at: key ifAbsent: exceptionBlock
	^ components at: key ifAbsent: exceptionBlock! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 06:10:41'!
at: anObject ifPresent: aBlock
	^ components at: anObject ifPresent: aBlock! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 05:43:24'!
at: key put: value
	value isZero ifTrue: [components removeKey: key ifAbsent: []. ^ value].
	^ components at: key put: value! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 05:43:40'!
bitSize
	| answer |
	answer _ 0.
	components keysAndValuesDo: [:key :value| answer _ answer + key bitSize + value bitSize].
	^ answer! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 07:00:41'!
size
	^ components size! !

!SparseTuple methodsFor: 'comparing' stamp: 'len 7/18/2020 06:08:29'!
= anObject
	^ self class = anObject class and: [components = anObject components]! !

!SparseTuple methodsFor: 'comparing' stamp: 'len 7/18/2020 06:08:44'!
hash
	^ components hash! !

!SparseTuple methodsFor: 'enumerating' stamp: 'len 7/18/2020 07:02:56'!
collect: aBlock 
	| newComponents |
	newComponents _ Dictionary new: self size.
	components associationsDo: [:each| | newValue |
		(newValue _ aBlock value: each value) isZero ifFalse: [newComponents at: each key put: newValue]].
	^ self class components: newComponents parent: parent! !

!SparseTuple methodsFor: 'enumerating' stamp: 'len 7/18/2020 06:57:14'!
withIndexDo: elementAndIndexBlock 
	components keysAndValuesDo: [:key :value| elementAndIndexBlock value: value value: key]! !

!SparseTuple methodsFor: 'initialization' stamp: 'len 7/18/2020 07:04:57'!
components: aDictionary
	components _ aDictionary! !

!SparseTuple methodsFor: 'initialization' stamp: 'len 7/18/2020 05:37:44'!
initialize
	super initialize.
	components _ Dictionary new! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/18/2020 06:58:12'!
+ aTuple
	| other answer |
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #+].
	self size <= aTuple size
		ifTrue: [answer _ aTuple copy. other _ self]
		ifFalse: [answer _ self copy. other _ aTuple].
	other withIndexDo: [:each :index| answer at: index add: each].
	^ answer! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/18/2020 06:58:49'!
- aTuple
	| answer |
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #-].
	self size <= aTuple size
		ifTrue:
			[answer _ aTuple negated.
			self withIndexDo: [:each :index| answer at: index add: each]]
		ifFalse:
			[answer _ self copy.
			aTuple withIndexDo: [:each :index| answer at: index add: each negated]].
	^ answer! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/18/2020 06:59:44'!
· aTuple
	| answer |
	answer _ self scalars zero.
	self withIndexDo: [:each :index|
		aTuple at: index ifPresent: [:other| answer _ each*other + answer]].
	^ answer! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/18/2020 07:00:07'!
negated
	^ self copy components associationsDo: [:each| each value: each value negated]! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/27/2020 15:48:16'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ components size! !

!SparseTuple methodsFor: 'private' stamp: 'len 7/18/2020 05:41:14'!
at: key add: value
	^ components at: key addAndRemoveIfZero: value! !

!SparseTuple methodsFor: 'private' stamp: 'len 7/18/2020 06:04:59'!
components
	^ components! !

!SparseTuple class methodsFor: 'instance creation' stamp: 'len 7/18/2020 07:03:57'!
components: anArrayOrDictionary parent: aFreeModule
	| answer |
	anArrayOrDictionary class = Dictionary
		ifTrue: [^ self basicNew components: anArrayOrDictionary; parent: aFreeModule].
	answer _ self new parent: aFreeModule.
	anArrayOrDictionary withIndexDo: [:each :i| each isZero ifFalse: [answer at: i put: each]].
	^ answer! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:50'!
countSignChanges: signs
	| x count |
	x _ signs first.
	count _ 0.
	signs do: [ :each | each sign ~= x ifTrue: [ count _ count + 1 ]. x _ each sign ].
	^ count! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:44'!
numberOfRealRoots
	^ self numberOfSignChangesAtMinusInfinity - self numberOfSignChangesAtInfinity! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:51'!
numberOfSignChangesAt: aNumber
	^ self countSignChanges: (chain collect: [ :each | (each value: aNumber) sign])! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:51'!
numberOfSignChangesAtInfinity
	^ self countSignChanges: (chain collect: [ :each | each leadingCoefficient sign ])! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:51'!
numberOfSignChangesAtMinusInfinity
	^ self countSignChanges: (chain collect: [ :each | each leadingCoefficient sign * (each degree odd ifTrue: [-1] ifFalse: [1])])! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 20:45'!
polynomial
	^ chain first! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 1/15/2016 06:27'!
polynomial: aPolynomial
	chain _ OrderedCollection with: aPolynomial with: aPolynomial derivative.
	[ chain last degree > 0 ] whileTrue: [ chain add: ((chain at: chain size - 1) \\ chain last) negated ]! !

!SturmChain class methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:46'!
on: aPolynomial
	^ self basicNew setPolynomial: aPolynomial! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 5/1/98 17:35'!
add: aSubgroup
	^ self subgroups add: aSubgroup! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 5/1/98 17:34'!
at: anInteger
	^ self subgroups at: anInteger! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 2/8/2016 03:55'!
objects
	^ subgroups! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 5/4/2016 06:25'!
size
	^ subgroups size! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 5/1/98 17:33'!
subgroups
	^ subgroups! !

!SubgroupSeries methodsFor: 'enumerating' stamp: 'len 5/1/98 17:35'!
do: aBlock
	self subgroups do: aBlock! !

!SubgroupSeries methodsFor: 'initialization' stamp: 'len 5/1/98 17:32'!
initialize
	self subgroups: OrderedCollection new! !

!SubgroupSeries methodsFor: 'testing' stamp: 'len 5/1/98 17:52'!
isAbelian
	"Answer true if the receiver is an abelian tower of subgroups."
	self isNormal ifFalse: [^ false].
	2 to: self size do: [ :each | ((self at: each - 1) / (self at: each)) isAbelian ifFalse: [^ false]].
	^ true! !

!SubgroupSeries methodsFor: 'testing' stamp: 'len 5/3/98 01:07'!
isCyclic
	"Answer true if the receiver is a cyclic tower of subgroups."
	self isNormal ifFalse: [^ false].
	2 to: self size do: [ :each | ((self at: each - 1) / (self at: each)) isCyclic ifFalse: [^ false]].
	^ true! !

!SubgroupSeries methodsFor: 'testing' stamp: 'len 5/1/98 17:47'!
isNormal
	"Answer true if the receiver is a normal tower of subgroups."
	2 to: self size do: [ :each | ((self at: each) isNormalIn: (self at: each - 1)) ifFalse: [^ false]].
	^ true! !

!SubgroupSeries methodsFor: 'private' stamp: 'len 5/1/98 17:33'!
subgroups: aCollection
	subgroups _ aCollection! !

!ZechArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/11/2023 17:00:35'!
newArray: n fill: aBlock
	| answer |
	answer _ table new: n.
	1 to: answer size do: [:i| answer at: i put: scalars !! (aBlock value: i)].
	^ answer! !

!ZechArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:04:11'!
setUp
	super setUp.
	scalars := ℤ/(10 atRandom: random) nextPrime.
	table := ZechTable over: scalars! !

!ZechTable methodsFor: 'accessing' stamp: 'len 5/10/2023 11:53:03'!
base
	^ base! !

!ZechTable methodsFor: 'accessing' stamp: 'len 5/10/2023 23:44:54'!
exp: anInteger
	anInteger = 0 ifTrue: [^ scalars zero].
	anInteger = q̄ ifTrue: [^ scalars one].
	^ base ^ anInteger! !

!ZechTable methodsFor: 'accessing' stamp: 'len 5/11/2023 21:36:29'!
log: anElement
	| a g i |
	a _ scalars !! anElement.
	a isZero ifTrue: [^ 0].
	a isOne ifTrue: [^ q̄].
	i _ 1.
	g _ base.
	[a = g] whileFalse: [i _ i + 1. g _ g * base. i >= q ifTrue: [^ self halt]].
	^ i! !

!ZechTable methodsFor: 'accessing' stamp: 'len 5/11/2023 15:32:59'!
q̄
	^ q̄! !

!ZechTable methodsFor: 'accessing' stamp: 'len 5/10/2023 21:08:26'!
scalars
	^ scalars! !

!ZechTable methodsFor: 'operations' stamp: 'len 5/11/2023 00:12:55'!
invert: i
	i = 0 ifTrue: [^ DivisibilityError new signalReceiver: scalars one selector: #/ argument: scalars zero].
	i = q̄ ifTrue: [^ i].
	^ q̄ - i! !

!ZechTable methodsFor: 'operations' stamp: 'len 5/11/2023 15:01:13'!
negate: i
	| answer |
	q even ifTrue: [^ i].
	i = 0 ifTrue: [^ 0].
	^ (answer _ i - minusOne) < 1 ifTrue: [answer + q̄] ifFalse: [answer]! !

!ZechTable methodsFor: 'operations' stamp: 'len 5/12/2023 14:22:38'!
op: i minus: j
	| minusJ k answer |
	j = 0 ifTrue: [^ i].
	minusJ _ q even ifTrue: [j] ifFalse: [(minusJ _ j - minusOne) < 1 ifTrue: [minusJ + q̄] ifFalse: [minusJ]].
	i = 0 ifTrue: [^ minusJ].
	(k _ minusJ - i) > 0 ifFalse: [k _ k + q̄].
	k = minusOne ifTrue: [^ 0].
	^ (answer _ i + (successors at: k + 1)) > q̄ ifTrue: [answer - q̄] ifFalse: [answer]! !

!ZechTable methodsFor: 'operations' stamp: 'len 5/11/2023 15:01:03'!
op: i plus: j
	| k answer |
	i = 0 ifTrue: [^ j].
	j = 0 ifTrue: [^ i].
	(k _ j - i) > 0 ifFalse: [k _ k + q̄].
	k = minusOne ifTrue: [^ 0].
	^ (answer _ i + (successors at: k + 1)) > q̄ ifTrue: [answer - q̄] ifFalse: [answer]! !

!ZechTable methodsFor: 'operations' stamp: 'len 5/11/2023 00:10:35'!
op: i times: j
	| answer |
	(i = 0 or: [j = 0]) ifTrue: [^ 0].
	^ (answer _ i + j) > q̄ ifTrue: [answer - q̄] ifFalse: [answer]! !

!ZechTable methodsFor: 'operations' stamp: 'len 5/10/2023 21:32:54'!
successor: i
	^ successors at: i + 1! !

!ZechTable methodsFor: 'private' stamp: 'len 5/12/2023 21:24:44'!
scalars: aFiniteField
	| g |
	scalars _ aFiniteField.
	q _ aFiniteField size.
	q̄ _ q - 1.
	q̄ <= 16rFFFF ifFalse: [^ self error: 'field too big'].
	minusOne _ q = 2 ifTrue: [1] ifFalse: [q̄ // 2].
	base _ aFiniteField primitiveRoot.
	successors _ self logarithmsArrayClass new: q. "this can be ByteArray or DoubleByteArray"
	successors at: 1 put: q̄.
	g _ base.
	2 to: successors size do: [:i|
		successors at: i put: (self log: g + 1).
		g _ g * base]! !

!ZechTable methodsFor: 'as yet unclassified' stamp: 'len 5/12/2023 21:22:56'!
logarithmsArrayClass
	^ q̄ <= 16rFF ifTrue: [ByteArray] ifFalse: [DoubleByteArray]! !

!ZechTable methodsFor: 'as yet unclassified' stamp: 'len 5/12/2023 21:23:18'!
new: anInteger
	^ ZechArray logarithms: (self logarithmsArrayClass new: anInteger) table: self! !

!ZechTable class methodsFor: 'instance creation' stamp: 'len 5/10/2023 21:35:44'!
over: aFiniteField
	^ self new scalars: aFiniteField! !

!ZechTableTest methodsFor: 'as yet unclassified' stamp: 'len 5/10/2023 21:44:08'!
setUp
	table _ ZechTable over: (GaloisField new: 5 atRandom nextPrime to: 1 "3 atRandom")! !

!ZechTableTest methodsFor: 'as yet unclassified' stamp: 'len 5/10/2023 21:47:00'!
testAddition
	table scalars do: [:x|
		| i |
		i _ table log: x.
		table scalars do: [:y|
			| j |
			j _ table log: y.
			self assert: (table exp: (table op: i plus: j)) = (x + y)]]! !

!ZechTableTest methodsFor: 'as yet unclassified' stamp: 'len 5/10/2023 21:56:31'!
testAdditiveInverse
	table scalars do: [:x| self assert: (table exp: (table negate: (table log: x))) = x negated]! !

!ZechTableTest methodsFor: 'as yet unclassified' stamp: 'len 5/10/2023 21:38:50'!
testLogExp
	table scalars do: [:x| self assert: (table exp: (table log: x)) = x]! !

!ZechTableTest methodsFor: 'as yet unclassified' stamp: 'len 5/10/2023 21:47:25'!
testMultiplication
	table scalars do: [:x|
		| i |
		i _ table log: x.
		table scalars do: [:y|
			| j |
			j _ table log: y.
			self assert: (table exp: (table op: i times: j)) = (x * y)]]! !

!ZechTableTest methodsFor: 'as yet unclassified' stamp: 'len 5/10/2023 21:56:44'!
testMultiplicativeInverse
	table scalars units do: [:x| self assert: (table exp: (table invert: (table log: x))) = x inverse].
	self should: [table invert: (table log: table scalars zero)] raise: DivisibilityError! !

!ZechTableTest methodsFor: 'as yet unclassified' stamp: 'len 5/11/2023 23:37:45'!
testSubstraction
	table scalars do: [:x|
		| i |
		i _ table log: x.
		table scalars do: [:y|
			| j |
			j _ table log: y.
			self assert: (table exp: (table op: i minus: j)) = (x - y)]]! !

!ZechTableTest methodsFor: 'as yet unclassified' stamp: 'len 5/10/2023 21:48:51'!
testSuccessor
	table scalars do: [:x| self assert: (table exp: (table successor: (table log: x))) = (x + 1)]! !

!Coechelonization methodsFor: 'accessing' stamp: 'len 12/18/2022 10:56:00'!
echelon
	echelon isNil ifTrue: [echelon _ matrix asColumnMajor copy. transformation _ nil. self run].
	^ echelon! !

!Coechelonization methodsFor: 'accessing' stamp: 'len 12/18/2022 11:01:29'!
echelonAndTransformation
	transformation isNil ifTrue: [echelon _ matrix asColumnMajor copy. transformation _ matrix domain id asColumnMajor copy. self run].
	^ {echelon. transformation}! !

!Coechelonization methodsFor: 'column operations' stamp: 'len 12/19/2022 10:07:06'!
addColumn: j₀ times: anElement to: j₁
	self addColumn: j₀ times: anElement to: j₁ startingAt: 1! !

!Coechelonization methodsFor: 'column operations' stamp: 'len 12/19/2022 10:08:29'!
addColumn: j₀ times: anElement to: j₁ startingAt: start
	echelon addColumn: j₀ times: anElement to: j₁ startingAt: start.
	transformation ifNotNil: [transformation addColumn: j₀ times: anElement to: j₁]! !

!Coechelonization methodsFor: 'column operations' stamp: 'len 12/17/2022 10:08:58'!
multiplyColumn: i by: anElement
	self multiplyColumn: i by: anElement startingAt: 1! !

!Coechelonization methodsFor: 'column operations' stamp: 'len 12/17/2022 10:09:31'!
multiplyColumn: i by: anElement startingAt: startIndex
	echelon multiplyColumn: i by: anElement startingAt: startIndex.
	transformation ifNotNil: [transformation multiplyColumn: i by: anElement]! !

!Coechelonization methodsFor: 'column operations' stamp: 'len 12/17/2022 10:08:12'!
swapColumn: i with: j
	echelon swapColumn: i with: j.
	transformation ifNotNil: [transformation swapColumn: i with: j]! !

!Coechelonization methodsFor: 'private' stamp: 'len 12/17/2022 10:07:38'!
matrix: aMatrix
	matrix _ aMatrix! !

!Coechelonization class methodsFor: 'instance creation' stamp: 'len 12/29/2022 10:17:07'!
on: aMatrix
	| R |
	self = Coechelonization ifFalse: [^ self new matrix: aMatrix].
	R _ aMatrix scalars.
	^ (R isDivisionRing
		ifTrue: [GaussJordanCoechelonization]
		ifFalse: [R isEuclideanDomain
			ifTrue: [HermiteCoechelonization]
			ifFalse: [R isEuclidean
					ifTrue: [HowellCoechelonization]
					ifFalse: [^ self notYetImplemented]]]) new matrix: aMatrix! !

!GaussJordanCoechelonization methodsFor: 'accessing' stamp: 'len 12/29/2022 10:14:47'!
determinant
	| determinant |
	determinant _ matrix scalars one.
	self echelon diagonalDo: [:each| determinant _ determinant * each].
	^ determinant / determinantFactor! !

!GaussJordanCoechelonization methodsFor: 'column operations' stamp: 'len 12/18/2022 11:52:26'!
multiplyColumn: i by: aNumber startingAt: startIndex
	super multiplyColumn: i by: aNumber startingAt: startIndex.
	determinantFactor _ determinantFactor * aNumber! !

!GaussJordanCoechelonization methodsFor: 'column operations' stamp: 'len 12/18/2022 11:53:23'!
swapColumn: i with: j
	i = j ifTrue: [^ self].
	super swapColumn: i with: j.
	determinantFactor _ determinantFactor negated! !

!GaussJordanCoechelonization methodsFor: 'computing' stamp: 'len 12/19/2022 10:08:52'!
reduceStep: pivot
	"Perform the reduction step on row i and column j."
	| i j j₀ |
	i _ pivot x.
	j _ pivot y.
	"2. Row finished?"
	[j₀ _ (echelon width to: j+1 by: -1)
		detect: [:k| (echelon isZeroAt: i@k) not]
		ifNone: [^ self multiplyColumn: j by: (echelon at: pivot) normalization "go to step 5"].
	j₀ > j ifTrue: [self swapColumn: j₀ with: j].
	self multiplyColumn: j by: (echelon at: pivot) inverse.
	"4. Reduce:"
	echelon width to: j+1 by: -1 do: [:k|
		self addColumn: j times: (echelon at: i@k) negated to: k]] repeat! !

!GaussJordanCoechelonization methodsFor: 'computing' stamp: 'len 12/19/2022 10:09:03'!
run
	| n m i j b |
	determinantFactor _ matrix scalars one.
	m _ echelon height.
	n _ echelon width.
	(m = 0 or: [n = 0]) ifTrue: [^ self]. "nothing to do"
	i _ 1.
	j _ 1.
	[self reduceStep: i@j.
	b _ echelon at: i@j.
	b isZero
		ifTrue:
			[j _ j - 1]
		ifFalse:
			[1 to: j-1 do: [:k| | q |
				q _ (echelon at: i@k) / b.
				self addColumn: j times: q negated to: k]].
	j = n or: [i = m]] whileFalse: [i _ i + 1. j _ j + 1]! !

!HermiteCoechelonization methodsFor: 'computing' stamp: 'len 12/19/2022 10:09:08'!
reduceStep: pivot
	"Perform the reduction step on row i and column j."
	| i j j₀ b |
	i _ pivot x.
	j _ pivot y.
	"2. Row finished?"
	[j₀ _ (echelon width to: j+1 by: -1)
		detect: [:k| (echelon isZeroAt: i@k) not]
		ifNone: [self multiplyColumn: j by: (echelon at: pivot) normalization. ^ self "go to step 5"].
	"3. Choose nonzero entry with smallest Euclidean valuation:"
	j₀ _ ((j to: j₀) select: [:k| (echelon isZeroAt: i@k) not]) detectMin: [:k| (echelon at: i@k) gauge].
	j₀ > j ifTrue: [self swapColumn: j₀ with: j].
	self multiplyColumn: j by: (echelon at: pivot) normalization.
	b _ echelon at: pivot.
	"4. Reduce:"
	echelon width to: j+1 by: -1 do: [:k| | q |
		q _ (echelon at: i@k) // b.
		self addColumn: j times: q negated to: k]] repeat! !

!HermiteCoechelonization methodsFor: 'computing' stamp: 'len 12/19/2022 10:09:22'!
run
	| n m i j b |
	m _ echelon height.
	n _ echelon width.
	(m = 0 or: [n = 0]) ifTrue: [^ self]. "nothing to do"
	"1. Initialize:"
	i _ 1.
	j _ 1.
	[self reduceStep: i@j.
	"5. Final reductions:"
	b _ echelon at: i@j.
	b isZero
		ifTrue:
			[j _ j - 1]
		ifFalse:
			[1 to: j-1 do: [:k| | q |
				q _ (echelon at: i@k) // b.
				self addColumn: j times: q negated to: k]].
	"6. Finished?"
	j = n or: [i = m]] whileFalse: [i _ i + 1. j _ j + 1]! !

!HowellCoechelonization methodsFor: 'column operations' stamp: 'len 12/20/2022 06:26:58'!
addZeroColumns: n
	echelon _ echelon ⊔ (echelon scalars ^ n ⇒ echelon codomain) zero asColumnMajor.
	transformation ifNotNil: [transformation _ transformation ⊔ (echelon scalars ^ n ⇒ transformation codomain) zero asColumnMajor]! !

!HowellCoechelonization methodsFor: 'column operations' stamp: 'len 12/29/2022 16:32:53'!
cancel: aPoint with: j
	| mgcd |
	mgcd _ (echelon at: aPoint) xxgcd: (echelon at: aPoint x @ j).
	echelon cancelColumn: aPoint y and: j with: mgcd.
	transformation ifNotNil: [transformation cancelColumn: aPoint y and: j with: mgcd]! !

!HowellCoechelonization methodsFor: 'column operations' stamp: 'len 12/20/2022 06:09:38'!
copyColumn: j₀ times: anElement to: j₁
	self copyColumn: j₀ times: anElement to: j₁ startingAt: 1! !

!HowellCoechelonization methodsFor: 'column operations' stamp: 'len 12/20/2022 06:08:46'!
copyColumn: j₀ times: anElement to: j₁ startingAt: start
	echelon copyColumn: j₀ times: anElement to: j₁ startingAt: start.
	transformation ifNotNil: [transformation copyColumn: j₀ times: anElement to: j₁]! !

!HowellCoechelonization methodsFor: 'column operations' stamp: 'len 12/20/2022 06:09:26'!
copyColumn: j₀ to: j₁
	self copyColumn: j₀ to: j₁ startingAt: 1! !

!HowellCoechelonization methodsFor: 'column operations' stamp: 'len 12/20/2022 06:09:11'!
copyColumn: j₀ to: j₁ startingAt: start
	echelon copyColumn: j₀ to: j₁ startingAt: start.
	transformation ifNotNil: [transformation copyColumn: j₀ to: j₁]! !

!HowellCoechelonization methodsFor: 'computing' stamp: 'len 12/29/2022 16:43:18'!
run
	| width height |
	width _ echelon width.
	height _ echelon height.
	(height = 0 or: [width = 0]) ifTrue: [^ self]. "nothing to do"
	"If more rows than columns, augment the matrix with zero columns to make it square:"
	height > width ifTrue:
		[self addZeroColumns: height - width.
		width _ height].
	"Put it in lower triangular form:"
	1 to: height do: [:i| i+1 to: width do: [:j| self cancel: i@i with: j]].
	self assert: echelon isLowerTriangular.
	"Put it in Howell form:"
	self addZeroColumns: 1. "augment with a zero column"
	1 to: height do: [:j|
		(echelon isZeroAt: j@j)
			ifTrue:
				[self copyColumn: j to: height+1]
			ifFalse:
				[self multiplyColumn: j by: (echelon at: j@j) normalization.
				1 to: j-1 do: [:i| self addColumn: j times: ((echelon at: j@i) // (echelon at: j@j)) negated to: i].
				self copyColumn: j times: (echelon at: j@j) annihilator to: height+1].
		j+1 to: height do: [:i| self cancel: i@i with: height+1]].
	"Sort columns:"
	self sort.
	"Output first m columns:"
	echelon _ echelon copyFromColumn: 1 to: height.
	transformation ifNotNil: [transformation _ transformation copyFromColumn: 1 to: height]! !

!HowellCoechelonization methodsFor: 'computing' stamp: 'len 12/21/2022 13:30:27'!
sort
	"Sort the columns to put the matrix in echelon form."
	| height width i j |
	height _ echelon height.
	width _ echelon width.
	i _ 1.
	j _ 1.
	[(echelon isZeroAt: i@j)
		ifTrue:
			[(j+1 to: width)
				detect: [:k| (echelon isZeroAt: i@k) not]
				ifFound: [:k| self swapColumn: k with: j]
				ifNone: [j _ j - 1]].
	j _ j + 1.
	i _ i + 1.
	i <= height] whileTrue! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/12/2022 06:50:19'!
= anObject
	^ self class = anObject class and: [source = anObject source and: [walk = anObject walk]]! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/12/2022 06:49:36'!
hash
	^ walk isEmpty ifTrue: [source hash] ifFalse: [source hash + walk hash]! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 10:02:12'!
length
	^ walk size! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 09:56:26'!
parent
	^ parent! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 8/7/2022 10:45:45'!
printMonomialOn: aStream
	walk isEmpty ifTrue: [aStream nextPutAll: 'v' asText, source printString sub].
	walk do: [:each| aStream nextPutAll: 'e', each printString sub]! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 10:21:59'!
printOn: aStream
	aStream print: source.
	walk do: [:each| aStream nextPutAll: ' -> '; print: (parent edges at: each) value]! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/9/2022 17:01:06'!
source
	^ source! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 10:17:23'!
source: aVertex walk: anArray parent: aQuiver
	source _ aVertex.
	walk _ anArray.
	parent _ aQuiver! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/9/2022 17:02:09'!
target
	^ walk isEmpty ifTrue: [source] ifFalse: [walk last target]! !

!QuiverPath methodsFor: 'as yet unclassified' stamp: 'len 7/12/2022 06:50:27'!
walk
	^ walk! !

!QuiverPath class methodsFor: 'as yet unclassified' stamp: 'len 7/11/2022 10:16:58'!
source: aVertex walk: anArray parent: aQuiver
	^ self new source: aVertex walk: anArray parent: aQuiver! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:11'!
gramMatrix
	^ matrix collect: [:x| -2 * (Float pi / x) cos]! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:14'!
isConnected
	^ self notYetImplemented! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:14'!
isElliptic
	^ self notYetImplemented! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:41'!
isHyperbolic
	^ (self gramMatrix spectrum count: [:each| each negative]) = 1! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:22'!
isLannerDiagram
	^ self isConnected and: [self isElliptic not and: [self gramMatrix determinant < 0]]! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:23'!
isParabolic
	| spectrum |
	spectrum _ self gramMatrix spectrum.
	^ (spectrum occurrencesOf: 0) = 1 and: [spectrum allSatisfy: [:each| each >= 0]]! !

!RootSystem methodsFor: 'printing' stamp: 'len 2/11/2016 01:34'!
printOn: aStream
	aStream nextPut: ${.
	roots do: [:each| aStream print: each] separatedBy: [aStream nextPut: $,].
	aStream nextPut: $}! !

!RootSystem methodsFor: 'accessing' stamp: 'len 5/23/2016 05:00'!
ambient
	"Answer the ambient space of the receiver."
	^ ambient! !

!RootSystem methodsFor: 'accessing' stamp: 'len 5/23/2016 04:59'!
rank
	^ ambient dimension! !

!RootSystem methodsFor: 'accessing' stamp: 'len 1/6/2016 04:46'!
roots
	^ roots! !

!RootSystem methodsFor: 'accessing-private' stamp: 'len 5/23/2016 05:00'!
ambient: aVectorSpace
	ambient _ aVectorSpace! !

!RootSystem methodsFor: 'accessing-private' stamp: 'len 2/11/2016 19:21'!
roots: aCollection
	roots _ aCollection! !

!RootSystem methodsFor: 'operations' stamp: 'len 5/5/2019 18:33:56'!
coxeterElement
	| O answer |
	O _ ambient automorphisms orthogonal.
	answer _ ambient id.
	self simpleRoots do: [:each| answer _ answer · (O reflectionAt: each)].
	^ answer! !

!RootSystem methodsFor: 'operations' stamp: 'len 11/23/2022 21:40:16'!
coxeterMatrix
	"Answer the Coxeter matrix of the receiver.
	Given a basis a_i of simple roots, the Coxeter matrix m_ij is defined by <a_i,a_j> = -2 cos(pi / m_ij)."
	| S |
	S _ self normalized simpleRoots.
	^ ambient scalars
		matrix: S size
		fill: [:i :j| Float pi / ((ambient innerProduct value: {(S at: i). (S at: j)}) / -2) arcCos]! !

!RootSystem methodsFor: 'operations' stamp: 'len 6/18/2016 17:14'!
coxeterNumber
	^ self weylGroup orderOf: self coxeterElement! !

!RootSystem methodsFor: 'operations' stamp: 'len 5/23/2016 04:59'!
dual
	^ self class on: ambient roots: self coroots! !

!RootSystem methodsFor: 'operations' stamp: 'len 2/11/2016 20:59'!
exponents
	"Answer the 'sequence of exponents' of the root system."
	| h |
	self flag: #fix. "complex eigenvalues, but how about finite fields?"
	h _ self coxeterNumber.
	^ self coxeterElement eigenvalues collect: [:each| each log / (Float pi * 2 i / h)]! !

!RootSystem methodsFor: 'operations' stamp: 'len 8/6/2022 07:13:23'!
lattice
	"Answer the root lattice, the ℤ-submodule spanned by roots of the receiver."
	^ Lattice basis: self simpleRoots! !

!RootSystem methodsFor: 'operations' stamp: 'len 5/23/2016 05:01'!
normalized
	^ self class on: ambient roots: (roots collect: [:x| x * (2 / (ambient innerProduct value: {x.x}))])! !

!RootSystem methodsFor: 'operations' stamp: 'len 2/10/2016 23:01'!
positiveWeylChamber
	"Answer the positive Weyl chamber that is choosen in the receiver to define the positive roots."
	^ self notYetImplemented! !

!RootSystem methodsFor: 'operations' stamp: 'len 5/20/2018 15:59:29'!
weightLattice
	"Answer the lattice spanned by the weights of the receiver. This lattice is invariant under the Weyl group and also under the map v -> -v. Also, the root lattice is contained in the weight lattice."
	^ Lattice basis: self weights! !

!RootSystem methodsFor: 'operations' stamp: 'len 1/7/2016 05:42'!
weights
	"Answer the fundamental weights of the receiver.
	A vector v is integral iff it can be expressed as integer linear combination of the fundamental weights."
	^ self notYetImplemented! !

!RootSystem methodsFor: 'operations' stamp: 'len 9/16/2022 08:17:49'!
weylAction
	| W |
	W _ self weylGroup.
	W name: 'W'.
	^ GroupAction from: (W, self roots) to: self roots mapWithArguments: [:f :x| f value: x]! !

!RootSystem methodsFor: 'operations' stamp: 'len 9/27/2018 21:19:17'!
weylGroup
	"Answer the Weyl group of the root system, i.e. the (finite) group of isometries of the ambient vector space generated by reflections through hyperplanes perpendicular to the roots. The Weyl group W(R) is the subgroup of GL(V) (and more specifically O(V)) generated by the reflections {s_r} that act on each root r by -1: s_r(r) = -r."
	| O W |
	O _ ambient automorphisms orthogonal.
	W _ O span: (self roots collect: [:each| O reflectionAt: each]).
"	W name: 'W'."
	^ W! !

!RootSystem methodsFor: 'operations' stamp: 'len 1/6/2016 00:00'!
weylVector
	^ self positiveRoots sum / 2! !

!RootSystem methodsFor: 'roots' stamp: 'len 5/23/2016 05:00'!
coroots
	^ roots collect: [:v| v * 2 / (ambient innerProduct value: {v.v})]! !

!RootSystem methodsFor: 'roots' stamp: 'len 1/6/2016 05:01'!
degreeOf: aPositiveRoot
	"Answer the degree of the given positive root."
	^ (self simpleRoots coordinatesOf: aPositiveRoot) asArray sum! !

!RootSystem methodsFor: 'roots' stamp: 'len 1/6/2016 03:52'!
negativeRoots
	^ self positiveRoots negated! !

!RootSystem methodsFor: 'roots' stamp: 'len 6/25/2016 23:08'!
positiveRoots
	| answer |
	answer _ Set new.
	roots do: [:a| (answer includes: a negated)
		ifFalse:
			[answer add: a.
			answer copy do: [:b| (roots includes: a+b) ifTrue: [answer add: b]]]].
	^ answer! !

!RootSystem methodsFor: 'roots' stamp: 'len 1/6/2016 04:51'!
positiveRootsSorted
	^ self positiveRoots asSortedCollection: [:a :b| (self simpleRoots coordinatesOf: b - a) asArray allSatisfy: [:each| each positive]]! !

!RootSystem methodsFor: 'roots' stamp: 'len 3/6/2020 18:19:43'!
simpleRoots
	| M |
	M _ Matrix columns: (self positiveRoots asArray collect: [:each| ambient coordinatesOf: each]).
	^ M image! !

!RootSystem methodsFor: 'testing' stamp: 'len 5/23/2016 04:59'!
isCrystallographic
	^ self roots allSatisfy: [:x|
		self coroots allSatisfy: [:y|
			(ambient innerProduct value: {x. y}) isInteger]]! !

!RootSystem methodsFor: 'testing' stamp: 'len 2/11/2016 21:16'!
isIntegral
	^ self isCrystallographic! !

!RootSystem methodsFor: 'testing' stamp: 'len 2/11/2016 20:46'!
isIrreducible
	^ self coxeterDiagram isConnected! !

!RootSystem methodsFor: 'testing' stamp: 'len 5/23/2016 05:01'!
isNormalized
	^ roots allSatisfy: [:each| (ambient innerProduct value: {each.each}) = 2]! !

!RootSystem class methodsFor: 'examples' stamp: 'len 7/14/2016 00:09'!
A: V
	| roots |
	roots _ Set new.
	1 to: V dimension do: [:i|
		i to: V dimension do: [:j|
			roots add: (V e: i) - (V e: j)]].
	^ self on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 7/14/2016 00:09'!
B: V
	| roots |
	roots _ Set new.
	1 to: V dimension do: [:i|
		roots add: (V e: i).
		i to: V dimension do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: i) + (V e: j)]].
	^ self on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 7/14/2016 00:09'!
C: V
	| roots |
	roots _ Set new.
	1 to: V dimension do: [:i|
		roots add: (V e: i)*2.
		i to: V dimension do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: i) + (V e: j)]].
	^ self on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 7/14/2016 00:09'!
D: V
	| roots |
	roots _ Set new.
	1 to: V dimension do: [:i|
		i to: V dimension do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: i) + (V e: j)]].
	^ self on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example1
	"This is a normalized root system in R^n. The Weyl group associated to it is the symmetric group S_n."
	| V roots |
	V _ ℚ ^ 3.
	roots _ Set new.
	1 to: V dimension do: [:i|
		1 to: i-1 do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: j) - (V e: i)]].
	^ RootSystem on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 12/9/2022 21:35:13'!
example2
	"This is a normalized root system in R^n. The Weyl group associated to it is the hyperoctahedral group."
	
	self notYetImplemented
"	| V roots |
	V _ ℝ ^ 3.
	roots _ Set new.
	1 to: V dimension do: [:i|
		1 to: i-1 do: [:j|
			roots
				add: (V e: i) * (RealAlgebraicNumber sqrt: 2);
				add: (V e: i) * (RealAlgebraicNumber sqrt: 2) negated;
				add: (V e: i) - (V e: j);
				add: (V e: j) - (V e: i);
				add: (V e: i) + (V e: j);
				add: ((V e: i) + (V e: j)) negated]].
	^ RootSystem on: V roots: roots"! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example3
	"This is a normalized root system in R^n. The Weyl group associated to it is an index 2 subgroup of the hyperoctahedral group."
	| V roots |
	V _ ℚ ^ 3.
	roots _ Set new.
	1 to: 3 do: [:i|
		1 to: i-1 do: [:j|
			roots
				add: (V e: i) - (V e: j);
				add: (V e: j) - (V e: i);
				add: (V e: i) + (V e: j);
				add: ((V e: i) + (V e: j)) negated]].
	^ RootSystem on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
hexagonal
	^ self A: ℚ ^ 2! !

!RootSystem class methodsFor: 'instance creation' stamp: 'len 5/23/2016 05:00'!
on: aVectorSpace roots: aCollection
	^ self new ambient: aVectorSpace; roots: aCollection! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
rootSystem1
	| V roots |
	V _ ℚ ^ 3.
	roots _ Set new.
	1 to: 3 do: [:i| 1 to: i-1 do: [:j| |r| roots add: (r _ (V x: i) - (V x: j)); add: r negated]].
	^ RootSystem on: V roots: roots! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
rootSystem2
	| V roots |
	V _ ℚ ^ 3.
	roots _ Set new.
	1 to: 3 do: [:i|
		1 to: i-1 do: [:j|
			roots
				add: (V x: i) - (V x: j);
				add: (V x: j) - (V x: i);
				add: (V x: i) + (V x: j);
				add: ((V x: i) + (V x: j)) negated]].
	^ RootSystem on: V roots: roots! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 2/23/2016 07:26'!
testBaseSignCondition
	| R |
	R _ self rootSystem1.
	R roots do: [:each| | coordinates |
		coordinates _ R simpleRoots coordinatesOf: each :: asArray.
		self assert: (coordinates allSatisfy: [:xi| xi >= 0]) | (coordinates allSatisfy: [:xi| xi <= 0])]! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 6/25/2016 23:09'!
testPositiveRoots
	| V R |
	R _ self rootSystem1.
	V _ R ambient.
	R positiveRoots do: [:each|
		self assert: (V innerProduct value: {each. (1,1,1)}) > 0] "the point must be in the positive Weyl chamber"! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 01:39'!
testWeylGroup
	| R W |
	R _ self rootSystem1.
	W _ R weylGroup.
	"The Weyl group of this root system is isomorphic to Sym(3), so we test some properties of Sym(3):"
	self assert: W order = 6.
	self assert: W center isTrivial.
	self assert: W isSolvable.
	self assert: W isCyclic not! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 2/10/2016 22:43'!
testWeylGroup2
	| R W |
	R _ self rootSystem2.
	W _ R weylGroup. "this is an index 2 subgroup of the hyperoctahedral group"
	self assert: W order = 24! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 2/10/2016 22:56'!
testWeylGroupAndSimpleRoots
	"For every root, there exists an transformation in the Weyl group that makes it a simple root."
	| R W |
	R _ self rootSystem1.
	W _ R weylGroup.
	R roots do: [:r| self assert: (W anySatisfy: [:w| R simpleRoots includes: (w value: r)])]! !

!EuclideanGaussianElimination methodsFor: 'private' stamp: 'len 12/13/2022 13:12:29'!
reduceStep
	"Perform one step in the reduction of the matrix using the current pivot."
	| row j |
	row _ currentPivot x.
	j _ currentPivot y.
	self assert: row = j.
	1 to: echelon height do: [:i|
		(i = j or: [echelon isZeroAt: i @ j])
			ifFalse:
				[| ajj aij euclid g bi bj ri rj |
				ajj _ echelon at: j @ j.
				aij _ echelon at: i @ j.
				euclid _ ExtendedEuclid on: {aij. ajj}.
				g _ euclid gcd.
				bi _ euclid bezout first.
				bj _ euclid bezout last.
"				euclid validate ifFalse: [self halt]."
				ri _ echelon rowAt: i.
				rj _ echelon rowAt: j.
				echelon rowAt: j put: ri * bi + (rj * bj).
				echelon rowAt: i put: rj * (aij // g) - (ri * (ajj // g))]]! !

!GaussBareiss methodsFor: 'private' stamp: 'len 12/19/2022 10:13:12'!
reduceStep
	"Perform one step reduction of the matrix using the current pivot."
	| row col pivot value |
	row _ currentPivot x.
	col _ currentPivot y.
	pivot _ echelon at: row @ col.
	1 to: echelon height do: [:i|
		(i = row or: [(value _ echelon at: i @ col) isZero])
			ifFalse:
				[self multiplyRow: i by: pivot.
				self addRow: row times: value negated to: i]]! !

!PolynomialCode methodsFor: 'accessing' stamp: 'len 4/4/2016 20:19'!
generator
	^ generator! !

!PolynomialCode methodsFor: 'accessing' stamp: 'len 5/12/2016 07:20'!
rank
	^ self length - generator degree! !

!PolynomialCode methodsFor: 'accessing' stamp: 'len 1/23/2017 11:44:09'!
size
	^ generator parent scalars size ^ (self length - generator degree)! !

!PolynomialCode methodsFor: 'converting' stamp: 'len 7/31/2020 10:28:26'!
asIdeal
	^ generator parent * generator! !

!PolynomialCode methodsFor: 'encoding/decoding' stamp: 'len 5/12/2016 09:33'!
decode: aPolynomial
	self flag: #fix. "add error-detection and correction"
	^ aPolynomial // generator! !

!PolynomialCode methodsFor: 'encoding/decoding' stamp: 'len 7/15/2021 12:38:39'!
encode: aPolynomial
	| f |
	f _ aPolynomial << generator degree.
	^ f - (f \\ generator)! !

!PolynomialCode methodsFor: 'enumerating' stamp: 'len 7/31/2020 10:31:23'!
do: aBlock
	self asIdeal do: aBlock! !

!PolynomialCode methodsFor: 'initialization' stamp: 'len 4/4/2016 20:19'!
generator: aPolynomial
	generator _ aPolynomial! !

!PolynomialCode methodsFor: 'testing' stamp: 'len 5/12/2016 07:17'!
includes: aCodeword
	^ aCodeword | generator! !

!PolynomialCode methodsFor: 'testing' stamp: 'len 1/23/2017 11:44:04'!
isCyclic
	| n x |
	n _ self length.
	x _ generator parent x.
	^ generator | (x ^ n - x parent one)! !

!PolynomialCode class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
G23
	"Answer the perfect binary Golay code [23,12,7]."
	| g |
	g _ (ℤ/2) polynomials !! [:x| x^11 + (x^10) + (x^6) + (x^5) + (x^4) + (x^2) + 1].
	^ self new: 23 generator: g! !

!PolynomialCode class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
H7
	"Answer the Hamming code [7,4,3]."
	^ self new: 7 generator: (ℤ/2) polynomials !! [:x| x^3 + x + 1]! !

!PolynomialCode class methodsFor: 'instance creation' stamp: 'len 5/12/2016 07:42'!
generator: aPolynomial
	^ self new: aPolynomial degree generator: aPolynomial! !

!PolynomialCode class methodsFor: 'instance creation' stamp: 'len 8/2/2020 12:23:40'!
new: n generator: aPolynomial
	| V W subspace |
	V _ aPolynomial parent filtrationAt: n - 1.
	W _ aPolynomial parent filtrationAt: n - aPolynomial degree - 1.
	subspace _ V span: (W asArray collect: [:f| aPolynomial * f]).
	^ (self on: subspace) generator: aPolynomial! !

!CyclicCode methodsFor: 'accessing' stamp: 'len 1/23/2017 11:31:24'!
checkPolynomial
	| x one |
	x _ generator parent x.
	one _ generator parent one.
	^ x ^ self length - one / generator! !

!CyclicCode methodsFor: 'accessing' stamp: 'len 7/31/2020 10:30:03'!
dimension
	^ self length - generator degree! !

!CyclicCode methodsFor: 'operations' stamp: 'len 6/29/2016 22:18'!
dual
	^ self class new: self length generator: self generator reversed! !

!CyclicCode methodsFor: 'testing' stamp: 'len 5/15/2019 01:42:35'!
includes: aCodeword
	| x |
	x _ generator parent x.
	^ (self checkPolynomial * aCodeword \\ (x ^ self length - x one)) isZero! !

!CyclicCode methodsFor: 'testing' stamp: 'len 6/29/2016 22:18'!
isIrreducible
	^ generator isIrreducible! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/22/2023 16:55:55'!
accumulateFrom: start to: stop withMatrix: anArray1 timesTuple: anArray2 zechTable: table
	| width j |
	(width := anArray2 size) = 0 ifTrue: [^ self].
	j := 1.
	start to: stop do: [:i|
		| value |
		value := self at: i.
		j to: j+width-1 do: [:k| value := table op: value plus: (table op: (anArray1 at: k) times: (anArray2 at: k-j+1))].
		self at: i put: value.
		j := j + width]! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/21/2023 00:20:56'!
dot: anArray zechTable: table
	| answer n i |
	answer := 0.
	n := self size min: anArray size.
	i := 1.
	[i <= n] whileTrue:
		[answer := table op: answer plus: (table op: (self at: i) times: (anArray at: i)).
		i := i + 1].
	^ answer! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/11/2023 21:38:14'!
polynomialValue: anInteger zechTable: table
	| answer i |
	self isEmpty ifTrue: [^ 0].
	anInteger = 0 ifTrue: [^ self at: 1].
	answer _ 0.
	i _ self size.
	[i > 0] whileTrue:
		[answer _ table op: (table op: anInteger times: answer) plus: (self at: i).
		i _ i - 1].
	^ answer! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/23/2023 10:51:05'!
reduceBy: g quotient: q zechTable: table
	| n m minusU |
	m := g size "lastNonzeroIndex".
	minusU := table negate: (table invert: (g at: m)).
	[(n := self lastNonzeroIndex ifNil: [0]) >= m]
		whileTrue:
			[| i c |
			i := n-m+1.
			c := table op: (self at: n) times: minusU.
			q ifNotNil: [q at: i put: (table op: (q at: i) minus: c)].
			self replaceFrom: i to: i+m-1 with: self startingAt: i plus: g startingAt: 1 timesScalar: c zechTable: table]! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/11/2023 23:26:15'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2 zechTable: table
	| j1 j2 |
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i| | x |
		self at: i put: ((x _ anArray2 at: j2) = 0 ifTrue: [anArray1 at: j1] ifFalse: [table op: (anArray1 at: j1) minus: x]).
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/11/2023 18:13:10'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: anInteger zechTable: table
	| j1 j2 |
	anInteger = 0 ifTrue: [^ self replaceFrom: start to: stop with: anArray1 startingAt: start1].
	anInteger = table q̄ ifTrue: [^ self replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 zechTable: table].
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i|
		self at: i put: (table op: (anArray1 at: j1) plus: (table op: (anArray2 at: j2) times: anInteger)).
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/11/2023 18:07:44'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 zechTable: table
	| j1 j2 |
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i|
		self at: i put: (table op: (anArray1 at: j1) plus: (anArray2 at: j2)).
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/12/2023 21:27:34'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2 zechTable: table
	| j1 j2 |
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i|
		self at: i put: (table op: (anArray1 at: j1) times: (anArray2 at: j2)).
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/11/2023 18:10:37'!
replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: anInteger zechTable: table
	| j |
	anInteger = 0 ifTrue: [^ self from: start to: stop put: 0].
	anInteger = table q̄ ifTrue: [^ self replaceFrom: start to: stop with: anArray startingAt: srcStart].
	j _ srcStart.
	start to: stop do: [:i|
		self at: i put: (table op: (anArray at: j) times: anInteger).
		j _ j + 1]! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/11/2023 17:49:40'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart zechTable: table
	| j |
	j _ srcStart.
	start to: stop do: [:i|
		self at: i put: (table negate: (anArray at: j)).
		j _ j + 1]! !

!ArrayedCollection methodsFor: '*etc-low level-zech logs' stamp: 'len 5/12/2023 21:41:18'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2 zechTable: table
	self from: start to: start + stop1 - start1 + stop2 - start2 put: 0.
	start2 to: stop2 do: [:i| | x |
		(x _ anArray2 at: i) = 0 ifFalse:
			[| index |
			index _ i-start2+start.
			self replaceFrom: index to: index+stop1-start1
				with: self startingAt: index
				plus: anArray1 startingAt: start1 timesScalar: x
				zechTable: table]]! !
