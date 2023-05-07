'From Cuis 6.0 [latest update: #5597] on 7 May 2023 at 7:06:51 pm'!
'Description '!
!provides: 'Etc' 1 24!
!requires: 'Algebra' 1 7 nil!
SystemOrganization addCategory: #Etc!
SystemOrganization addCategory: #'Etc-Matrices'!
SystemOrganization addCategory: #'Etc-Algebras-Solvable'!
SystemOrganization addCategory: #'Etc-Algebras-Path'!
SystemOrganization addCategory: #'Etc-Lie'!


!classDefinition: #IdentityArray category: #Etc!
Array variableSubclass: #IdentityArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'IdentityArray class' category: #Etc!
IdentityArray class
	instanceVariableNames: ''!

!classDefinition: #BitArray2 category: #Etc!
ArrayedCollection subclass: #BitArray2
	instanceVariableNames: 'bytes length'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BitArray2 class' category: #Etc!
BitArray2 class
	instanceVariableNames: ''!

!classDefinition: #TupleOld category: #Etc!
ArrayedCollection subclass: #TupleOld
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'TupleOld class' category: #Etc!
TupleOld class
	instanceVariableNames: ''!

!classDefinition: #BitTuple category: #Etc!
TupleOld subclass: #BitTuple
	instanceVariableNames: ''
	classVariableNames: 'One Zero'
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BitTuple class' category: #Etc!
BitTuple class
	instanceVariableNames: ''!

!classDefinition: #LargeBitTuple category: #Etc!
BitTuple subclass: #LargeBitTuple
	instanceVariableNames: 'words size'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'LargeBitTuple class' category: #Etc!
LargeBitTuple class
	instanceVariableNames: ''!

!classDefinition: #SmallBitTuple category: #Etc!
BitTuple subclass: #SmallBitTuple
	instanceVariableNames: 'bits size'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallBitTuple class' category: #Etc!
SmallBitTuple class
	instanceVariableNames: ''!

!classDefinition: #DenseTuple category: #Etc!
TupleOld subclass: #DenseTuple
	instanceVariableNames: 'coefficients scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'DenseTuple class' category: #Etc!
DenseTuple class
	instanceVariableNames: ''!

!classDefinition: #PolynomialTuple category: #Etc!
DenseTuple subclass: #PolynomialTuple
	instanceVariableNames: 'leadingMonomial'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'PolynomialTuple class' category: #Etc!
PolynomialTuple class
	instanceVariableNames: ''!

!classDefinition: #GaloisTuple category: #Etc!
TupleOld subclass: #GaloisTuple
	instanceVariableNames: 'coefficients scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GaloisTuple class' category: #Etc!
GaloisTuple class
	instanceVariableNames: ''!

!classDefinition: #ModularTuple category: #Etc!
TupleOld subclass: #ModularTuple
	instanceVariableNames: 'representatives scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ModularTuple class' category: #Etc!
ModularTuple class
	instanceVariableNames: ''!

!classDefinition: #ProductTuple category: #Etc!
TupleOld subclass: #ProductTuple
	instanceVariableNames: 'components scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ProductTuple class' category: #Etc!
ProductTuple class
	instanceVariableNames: ''!

!classDefinition: #StandardBasis category: #Etc!
SequenceableCollection subclass: #StandardBasis
	instanceVariableNames: 'elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'StandardBasis class' category: #Etc!
StandardBasis class
	instanceVariableNames: ''!

!classDefinition: #GroebnerBasis category: #Etc!
StandardBasis subclass: #GroebnerBasis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GroebnerBasis class' category: #Etc!
GroebnerBasis class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeFieldTest category: #Etc!
TestCase subclass: #SmallPrimeFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeFieldTest class' category: #Etc!
SmallPrimeFieldTest class
	instanceVariableNames: ''!

!classDefinition: #DivisorGroup category: #Etc!
Group subclass: #DivisorGroup
	instanceVariableNames: 'scalars space'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'DivisorGroup class' category: #Etc!
DivisorGroup class
	instanceVariableNames: ''!

!classDefinition: #SemidirectProductGroup category: #Etc!
Group subclass: #SemidirectProductGroup
	instanceVariableNames: 'left right action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SemidirectProductGroup class' category: #Etc!
SemidirectProductGroup class
	instanceVariableNames: ''!

!classDefinition: #CoxeterGroup category: #'Etc-Lie'!
Group subclass: #CoxeterGroup
	instanceVariableNames: 'diagram'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterGroup class' category: #'Etc-Lie'!
CoxeterGroup class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeField category: #Etc!
PrimeField subclass: #SmallPrimeField
	instanceVariableNames: 'expTable logTable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeField class' category: #Etc!
SmallPrimeField class
	instanceVariableNames: ''!

!classDefinition: #GRAlgebra category: #'Etc-Algebras-Solvable'!
QuotientRing subclass: #GRAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'GRAlgebra class' category: #'Etc-Algebras-Solvable'!
GRAlgebra class
	instanceVariableNames: ''!

!classDefinition: #ExteriorAlgebra category: #'Etc-Algebras-Solvable'!
GRAlgebra subclass: #ExteriorAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'ExteriorAlgebra class' category: #'Etc-Algebras-Solvable'!
ExteriorAlgebra class
	instanceVariableNames: ''!

!classDefinition: #PowerAlgebra category: #Etc!
FiniteAlgebra subclass: #PowerAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'PowerAlgebra class' category: #Etc!
PowerAlgebra class
	instanceVariableNames: ''!

!classDefinition: #CompletionRing category: #Etc!
Ring subclass: #CompletionRing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'CompletionRing class' category: #Etc!
CompletionRing class
	instanceVariableNames: ''!

!classDefinition: #GAlgebra category: #'Etc-Algebras-Solvable'!
Ring subclass: #GAlgebra
	instanceVariableNames: 'representatives multiplicativeConstants additiveConstants'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'GAlgebra class' category: #'Etc-Algebras-Solvable'!
GAlgebra class
	instanceVariableNames: ''!

!classDefinition: #WeylAlgebra category: #'Etc-Algebras-Solvable'!
GAlgebra subclass: #WeylAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'WeylAlgebra class' category: #'Etc-Algebras-Solvable'!
WeylAlgebra class
	instanceVariableNames: ''!

!classDefinition: #PathAlgebra category: #'Etc-Algebras-Path'!
Ring subclass: #PathAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'PathAlgebra class' category: #'Etc-Algebras-Path'!
PathAlgebra class
	instanceVariableNames: ''!

!classDefinition: #CoxeterSystem category: #'Etc-Lie'!
Structure subclass: #CoxeterSystem
	instanceVariableNames: 'group generators matrix diagram'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterSystem class' category: #'Etc-Lie'!
CoxeterSystem class
	instanceVariableNames: ''!

!classDefinition: #Quiver category: #'Etc-Algebras-Path'!
Domain subclass: #Quiver
	instanceVariableNames: 'edges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'Quiver class' category: #'Etc-Algebras-Path'!
Quiver class
	instanceVariableNames: ''!

!classDefinition: #TriangularMatrix category: #'Etc-Matrices'!
Matrix subclass: #TriangularMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'TriangularMatrix class' category: #'Etc-Matrices'!
TriangularMatrix class
	instanceVariableNames: ''!

!classDefinition: #DiagonalMatrix category: #'Etc-Matrices'!
TriangularMatrix subclass: #DiagonalMatrix
	instanceVariableNames: 'diagonal'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'DiagonalMatrix class' category: #'Etc-Matrices'!
DiagonalMatrix class
	instanceVariableNames: ''!

!classDefinition: #LowerTriangularMatrix category: #'Etc-Matrices'!
TriangularMatrix subclass: #LowerTriangularMatrix
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'LowerTriangularMatrix class' category: #'Etc-Matrices'!
LowerTriangularMatrix class
	instanceVariableNames: ''!

!classDefinition: #ScalarMatrix category: #'Etc-Matrices'!
TriangularMatrix subclass: #ScalarMatrix
	instanceVariableNames: 'scalar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'ScalarMatrix class' category: #'Etc-Matrices'!
ScalarMatrix class
	instanceVariableNames: ''!

!classDefinition: #UpperTriangularMatrix category: #'Etc-Matrices'!
TriangularMatrix subclass: #UpperTriangularMatrix
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'UpperTriangularMatrix class' category: #'Etc-Matrices'!
UpperTriangularMatrix class
	instanceVariableNames: ''!

!classDefinition: #QuiverMorphism category: #'Etc-Algebras-Path'!
Morphism subclass: #QuiverMorphism
	instanceVariableNames: 'domain codomain images'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'QuiverMorphism class' category: #'Etc-Algebras-Path'!
QuiverMorphism class
	instanceVariableNames: ''!

!classDefinition: #Divisor category: #Etc!
GroupElement subclass: #Divisor
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'Divisor class' category: #Etc!
Divisor class
	instanceVariableNames: ''!

!classDefinition: #SemidirectProductElement category: #Etc!
GroupElement subclass: #SemidirectProductElement
	instanceVariableNames: 'parent left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SemidirectProductElement class' category: #Etc!
SemidirectProductElement class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeResidue category: #Etc!
PrimeResidue subclass: #SmallPrimeResidue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeResidue class' category: #Etc!
SmallPrimeResidue class
	instanceVariableNames: ''!

!classDefinition: #RelaxedAdicInteger category: #Etc!
RingElement subclass: #RelaxedAdicInteger
	instanceVariableNames: 'parent digits'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedAdicInteger class' category: #Etc!
RelaxedAdicInteger class
	instanceVariableNames: ''!

!classDefinition: #RelaxedAdicNumber category: #Etc!
RingElement subclass: #RelaxedAdicNumber
	instanceVariableNames: 'parent valuation unit precision'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedAdicNumber class' category: #Etc!
RelaxedAdicNumber class
	instanceVariableNames: ''!

!classDefinition: #RelaxedPowerSeries category: #Etc!
RingElement subclass: #RelaxedPowerSeries
	instanceVariableNames: 'parent coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedPowerSeries class' category: #Etc!
RelaxedPowerSeries class
	instanceVariableNames: ''!

!classDefinition: #GAlgebraElement category: #'Etc-Algebras-Solvable'!
RingElement subclass: #GAlgebraElement
	instanceVariableNames: 'parent representative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Solvable'!
!classDefinition: 'GAlgebraElement class' category: #'Etc-Algebras-Solvable'!
GAlgebraElement class
	instanceVariableNames: ''!

!classDefinition: #PathAlgebraElement category: #'Etc-Algebras-Path'!
RingElement subclass: #PathAlgebraElement
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'PathAlgebraElement class' category: #'Etc-Algebras-Path'!
PathAlgebraElement class
	instanceVariableNames: ''!

!classDefinition: #Sum category: #Etc!
ModuleElement subclass: #Sum
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'Sum class' category: #Etc!
Sum class
	instanceVariableNames: ''!

!classDefinition: #TupleOlder category: #Etc!
ModuleElement subclass: #TupleOlder
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'TupleOlder class' category: #Etc!
TupleOlder class
	instanceVariableNames: ''!

!classDefinition: #GaussianElimination2 category: #Etc!
Echelonization subclass: #GaussianElimination2
	instanceVariableNames: 'pivots currentPivot determinantFactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GaussianElimination2 class' category: #Etc!
GaussianElimination2 class
	instanceVariableNames: ''!

!classDefinition: #BerlekampMassey category: #Etc!
Object subclass: #BerlekampMassey
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BerlekampMassey class' category: #Etc!
BerlekampMassey class
	instanceVariableNames: ''!

!classDefinition: #BitTuple2 category: #Etc!
Object subclass: #BitTuple2
	instanceVariableNames: 'bits'
	classVariableNames: 'One Zero'
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BitTuple2 class' category: #Etc!
BitTuple2 class
	instanceVariableNames: ''!

!classDefinition: #ExtendedSmithReduction category: #Etc!
Object subclass: #ExtendedSmithReduction
	instanceVariableNames: 'matrix elementaryDivisors L S R'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ExtendedSmithReduction class' category: #Etc!
ExtendedSmithReduction class
	instanceVariableNames: ''!

!classDefinition: #LargeBitArray category: #Etc!
Object subclass: #LargeBitArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'LargeBitArray class' category: #Etc!
LargeBitArray class
	instanceVariableNames: ''!

!classDefinition: #ModularHermiteReduction category: #Etc!
Object subclass: #ModularHermiteReduction
	instanceVariableNames: 'matrix HNF'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ModularHermiteReduction class' category: #Etc!
ModularHermiteReduction class
	instanceVariableNames: ''!

!classDefinition: #MultivariateReducer category: #Etc!
Object subclass: #MultivariateReducer
	instanceVariableNames: 'divisors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'MultivariateReducer class' category: #Etc!
MultivariateReducer class
	instanceVariableNames: ''!

!classDefinition: #GlobalMultivariateReducer category: #Etc!
MultivariateReducer subclass: #GlobalMultivariateReducer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GlobalMultivariateReducer class' category: #Etc!
GlobalMultivariateReducer class
	instanceVariableNames: ''!

!classDefinition: #OEIS category: #Etc!
Object subclass: #OEIS
	instanceVariableNames: 'entries'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'OEIS class' category: #Etc!
OEIS class
	instanceVariableNames: ''!

!classDefinition: #RealBox category: #Etc!
Object subclass: #RealBox
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RealBox class' category: #Etc!
RealBox class
	instanceVariableNames: ''!

!classDefinition: #SmallBitArray category: #Etc!
Object subclass: #SmallBitArray
	instanceVariableNames: 'bits size'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallBitArray class' category: #Etc!
SmallBitArray class
	instanceVariableNames: ''!

!classDefinition: #SmithReduction category: #Etc!
Object subclass: #SmithReduction
	instanceVariableNames: 'matrix invariants'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmithReduction class' category: #Etc!
SmithReduction class
	instanceVariableNames: ''!

!classDefinition: #SparseTuple category: #Etc!
Object subclass: #SparseTuple
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SparseTuple class' category: #Etc!
SparseTuple class
	instanceVariableNames: ''!

!classDefinition: #SturmChain category: #Etc!
Object subclass: #SturmChain
	instanceVariableNames: 'chain'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SturmChain class' category: #Etc!
SturmChain class
	instanceVariableNames: ''!

!classDefinition: #SubgroupSeries category: #Etc!
Object subclass: #SubgroupSeries
	instanceVariableNames: 'subgroups'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SubgroupSeries class' category: #Etc!
SubgroupSeries class
	instanceVariableNames: ''!

!classDefinition: #Coechelonization category: #'Etc-Matrices'!
Object subclass: #Coechelonization
	instanceVariableNames: 'matrix echelon transformation'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'Coechelonization class' category: #'Etc-Matrices'!
Coechelonization class
	instanceVariableNames: ''!

!classDefinition: #GaussJordanCoechelonization category: #'Etc-Matrices'!
Coechelonization subclass: #GaussJordanCoechelonization
	instanceVariableNames: 'determinantFactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'GaussJordanCoechelonization class' category: #'Etc-Matrices'!
GaussJordanCoechelonization class
	instanceVariableNames: ''!

!classDefinition: #HermiteCoechelonization category: #'Etc-Matrices'!
Coechelonization subclass: #HermiteCoechelonization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'HermiteCoechelonization class' category: #'Etc-Matrices'!
HermiteCoechelonization class
	instanceVariableNames: ''!

!classDefinition: #HowellCoechelonization category: #'Etc-Matrices'!
Coechelonization subclass: #HowellCoechelonization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Matrices'!
!classDefinition: 'HowellCoechelonization class' category: #'Etc-Matrices'!
HowellCoechelonization class
	instanceVariableNames: ''!

!classDefinition: #QuiverPath category: #'Etc-Algebras-Path'!
Object subclass: #QuiverPath
	instanceVariableNames: 'parent source walk'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Algebras-Path'!
!classDefinition: 'QuiverPath class' category: #'Etc-Algebras-Path'!
QuiverPath class
	instanceVariableNames: ''!

!classDefinition: #CoxeterDiagram category: #'Etc-Lie'!
Object subclass: #CoxeterDiagram
	instanceVariableNames: 'matrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterDiagram class' category: #'Etc-Lie'!
CoxeterDiagram class
	instanceVariableNames: ''!

!classDefinition: #RootSystem category: #'Etc-Lie'!
Object subclass: #RootSystem
	instanceVariableNames: 'roots ambient'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'RootSystem class' category: #'Etc-Lie'!
RootSystem class
	instanceVariableNames: ''!

!classDefinition: #RootSystemTest category: #'Etc-Lie'!
Object subclass: #RootSystemTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'RootSystemTest class' category: #'Etc-Lie'!
RootSystemTest class
	instanceVariableNames: ''!

!classDefinition: #EuclideanGaussianElimination category: #Etc!
ProtoObject subclass: #EuclideanGaussianElimination
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'EuclideanGaussianElimination class' category: #Etc!
EuclideanGaussianElimination class
	instanceVariableNames: ''!

!classDefinition: #GaussBareiss category: #Etc!
ProtoObject subclass: #GaussBareiss
	instanceVariableNames: 'currentPivot echelon'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GaussBareiss class' category: #Etc!
GaussBareiss class
	instanceVariableNames: ''!

!classDefinition: #PolynomialCode category: #Etc!
ProtoObject subclass: #PolynomialCode
	instanceVariableNames: 'generator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'PolynomialCode class' category: #Etc!
PolynomialCode class
	instanceVariableNames: ''!

!classDefinition: #CyclicCode category: #Etc!
PolynomialCode subclass: #CyclicCode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'CyclicCode class' category: #Etc!
CyclicCode class
	instanceVariableNames: ''!


!TupleOld commentStamp: '<historical>' prior: 0!
Tuples with coefficients in a ring R, i.e. finite ordered lists of n elements of R indexed by integers 1 to n. They are special types of arrays whose elements belong to a single ring R. They are elements of free modules R^n (see FreeModule).

Different ways to create the 4-tuple (1,2,4,8) with integer coefficients:
	(1,2,4,8).
	ℤ !!!! #(1 2 4 8).
	ℤ^4 !! #(1 2 4 8).
	ℤ^4 fill: [:i| 2^(i-1)]!

!BitTuple commentStamp: '<historical>' prior: 0!
Arrays of integer residue classes modulo 2, i.e. binary arrays, bit strings, or arrays of bits. The coefficient field is ℤ/2ℤ.
!

!LargeBitTuple commentStamp: '<historical>' prior: 0!
Arrays of integer residue classes modulo 2, i.e. binary arrays, bit strings, or arrays of bits. The coefficient field is ℤ/2ℤ.

They are stored as a WordArray (32-bit words).!

!SmallBitTuple commentStamp: '<historical>' prior: 0!
Arrays of integer residue classes modulo 2, i.e. binary arrays, bit strings, or arrays of bits. The coefficient field is ℤ/2ℤ.

They are stored as an Integer, and they are fast and memory-efficient when they fit in a SmallInteger.!

!PolynomialTuple commentStamp: '<historical>' prior: 0!
Tuples with coefficients in a polynomial ring R. They are elements of free modules R^n (see FreeModule).
!

!GaloisTuple commentStamp: '<historical>' prior: 0!
Arrays of elements of a Galois ring or field.!

!ModularTuple commentStamp: '<historical>' prior: 0!
Arrays of elements of an integer residue class ring or field ℤ/mℤ. See ModularInteger, ModularIntegerRing, PrimeField.
!

!ProductTuple commentStamp: 'len 11/20/2022 12:19:48' prior: 0!
Arrays of elements of a product ring. See ProductRing.!

!StandardBasis commentStamp: '<historical>' prior: 0!
Standard bases of polynomial ideals. A standard basis is nothing but a special set of generators with the property that the leading monomials generate the leading ideal.

When the choosen monomial ordering is global (a well-ordering), these are Groebner bases and are computed with Buchberger's algorithm. For arbitrary monomial orderings, they are computed using Mora's normal form.

See also Buchberger and ExtendedBuchberger.!

!GroebnerBasis commentStamp: '<historical>' prior: 0!
Groebner bases are a special case of standard bases.

For a fixed global monomial ordering, a subset G = {g1,...,gm} of an ideal I = <f1,...,fm> of a polynomial ring is called a Groebner basis for I if every polynomial f in I is reducible by G, i.e. there exists g in G such that its leading monomial LM(g) divides LM(f).

When all generators {f1,...,fm} are linear, a Groebner basis corresponds to "row echelon form" or "triangular system"; when they are univariate, then a Groebner basis corresponds to gcd(f1,...,fm). In general, a Groebner basis for an ideal I consists of all the "smallest polynomials" in I under the given monomial ordering.

See also Buchberger and ExtendedBuchberger.!

!SemidirectProductGroup commentStamp: '<historical>' prior: 0!
Semidirect products of groups.!

!SmallPrimeField commentStamp: '<historical>' prior: 0!
Prime fields Z/pZ for small primes p, such that it is feasable to build exponentiation and logarithmic tables and accelerate arithmetic through table lookups.

The initialization of the tables is very fast. The main concern is memory usage. For a field Z/pZ, each of the two tables have p-1 elements. They are WordArrays (32 bits per entry), so a field Z/pZ uses about 8*(p-1) bytes of memory. For example, Z/65537 uses 524K of memory, and Z/1048583 (a 20-bits modulus) uses about 8M.
!

!CompletionRing commentStamp: '<historical>' prior: 0!
Completion of a given commutative ring R at a given maximal ideal I. This is the inverse limit of the quotient rings R / I^n.!

!Divisor commentStamp: '<historical>' prior: 0!
Formal sums with integer or rational coefficients of prime divisors (integral closed subschemes of codimension 1) of an integral locally Noetherian scheme.!

!SemidirectProductElement commentStamp: '<historical>' prior: 0!
Elements of a semidirect product of groups. See SemidirectProductGroup.!

!RelaxedAdicInteger commentStamp: '<historical>' prior: 0!
Elements of the p-adic ring Zp. See AdicRing.!

!RelaxedAdicNumber commentStamp: '<historical>' prior: 0!
Elements of the p-adic field Qp. See AdicField.

A p-adic number z is represented uniquely by an integer k (the valuation) and a unit u in Zp, such that z = (p^k)*u.!

!RelaxedPowerSeries commentStamp: '<historical>' prior: 0!
Univariate formal power series with coefficients over a commutative ring. See PowerSeriesRing.!

!Sum commentStamp: '<historical>' prior: 0!
Formal sums (or formal linear combinations) of arbitrary symbols with coefficients over a given ring R. They can be thought of as Tuples indexed not by positive integers but by arbitrary symbols. They form free R-modules (see FreeModule).!

!TupleOlder commentStamp: '<historical>' prior: 0!
R-tuples with coefficients in a ring R, i.e. finite ordered lists of n elements of R indexed by integers 1 to n. They are elements of free modules R^n (see FreeModule).

Different ways to create the 4-tuple (1,2,4,8) with integer coefficients:
	ℤ^4 !! #(1 2 4 8).
	ℤ^4 filling: [:i| 2^(i-1)].
	(1,2,4,8)
!

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

!BitTuple2 commentStamp: '<historical>' prior: 0!
Tuples of integer residue classes modulo 2, i.e. binary tuples, bit strings, or tuples of bits. The coefficient field is ℤ/2ℤ.

Example:
	"A handy way to create a binary tuple:"
	(1,0,1,1,0,1,1,1) % 2
!

!LargeBitArray commentStamp: '<historical>' prior: 0!
Arrays of integer residue classes modulo 2, i.e. binary arrays, bit strings, or arrays of bits. The coefficient field is ℤ/2ℤ.

They are stored as a WordArray (32-bit words).!

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
This is the Howell column reduction algorithm. Given a matrix with coefficients in a PIR with an extended GCD algorithm to compute Bézout coefficients (for example ℤ/mℤ or other Euclidean rings, possibly with zero divisors), it reduces the matrix to its column-style Howell normal form in O(m^2 * max(m,n)) operations. See [How86], [Sto00], [SM98], [FH16].!

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

!BitArray2 methodsFor: 'printing' stamp: 'len 12/21/2015 20:42'!
printOn: aStream
	1 to: self size do: [:i| aStream print: (self at: i)]! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 22:30'!
at: anInteger
	^ (bytes at: anInteger - 1 // 8 + 1 ifAbsent: [^ 0]) bitAt: anInteger - 1 \\ 8 + 1! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 22:07'!
at: anInteger put: aBit
	| i i0 b |
	i _ anInteger - 1 // 8 + 1.
	i0 _ anInteger - 1 \\ 8 + 1.
	b _ bytes at: i.
	(b bitAt: i0) == aBit
		ifFalse: [b _ b bitXor: (1 bitShift: i0-1)].
	bytes at: i put: b.
	^ aBit! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 22:16'!
byteAt: anInteger
	^ bytes at: anInteger ifAbsent: [0]! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 22:16'!
byteAt: anInteger put: aByte
	^ bytes at: anInteger put: aByte! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 20:41'!
size
	^ length! !

!BitArray2 methodsFor: 'arithmetic' stamp: 'len 12/21/2015 22:15'!
* aBitArray
	| answer |
	answer _ self class new: (self size max: aBitArray size).
	1 to: answer byteSize do: [:i|
		answer byteAt: i put: ((self byteAt: i) bitAnd: (aBitArray byteAt: i))].
	^ answer! !

!BitArray2 methodsFor: 'arithmetic' stamp: 'len 12/21/2015 22:14'!
+ aBitArray
	| answer |
	answer _ self class new: (self size max: aBitArray size).
	1 to: answer byteSize do: [:i|
		answer byteAt: i put: ((self byteAt: i) bitXor: (aBitArray byteAt: i))].
	^ answer! !

!BitArray2 methodsFor: 'converting' stamp: 'len 12/21/2015 22:17'!
asByteArray
	^ bytes! !

!BitArray2 methodsFor: 'converting' stamp: 'len 12/21/2015 22:21'!
asInteger
	"Answer the receiver as a possitive integer."
	| answer i |
	answer _ 0.
	i _ 0.
	bytes do: [:each| answer _ answer + (each bitShift: i). i _ i + 8].
	^ answer! !

!BitArray2 methodsFor: 'initialization' stamp: 'len 12/21/2015 20:31'!
bytes: aByteArray length: anInteger
	bytes _ aByteArray.
	length _ anInteger! !

!BitArray2 class methodsFor: 'instance creation' stamp: 'len 12/21/2015 20:41'!
new: anInteger
	^ self new: anInteger bytes: (ByteArray new: (anInteger / 8) ceiling)! !

!BitArray2 class methodsFor: 'instance creation' stamp: 'len 12/21/2015 20:40'!
new: anInteger bytes: aByteArray
	^ self basicNew bytes: aByteArray length: anInteger! !

!BitArray2 class methodsFor: 'instance creation' stamp: 'len 12/21/2015 22:26'!
random: anInteger
	| answer |
	answer _ self new: anInteger.
	1 to: answer size do: [:i| answer at: i put: 2 atRandom - 1].
	^ answer! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/23/2022 10:24:30'!
at: anInteger add: anElement
	^ self at: anInteger put: (self at: anInteger) + anElement! !

!TupleOld methodsFor: 'accessing' stamp: 'len 12/2/2022 14:21:55'!
denominator
	"If the coefficients ring is a localization, answer the LCM of the denominators of the coefficients."
	| answer |
	self scalars isLocalization ifFalse: [^ self scalars one].
	answer _ self scalars numerators one.
	self do: [:each| answer _ each denominator lcm: answer].
	^ answer! !

!TupleOld methodsFor: 'accessing' stamp: 'len 12/2/2022 14:23:21'!
lift
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R."
	^ self scalars cover new: self size fill: [:i| (self at: i) lift]! !

!TupleOld methodsFor: 'accessing' stamp: 'len 12/2/2022 14:21:17'!
numerator
	"If the coefficients ring is a localization, answer the product of the receiver by the LCM of the denominators of the coefficients. The answer is a tuple over the ring of numerators of the localization ring."
	| d |
	self scalars isLocalization ifFalse: [^ self].
	d _ self scalars !! self denominator.
	^ self scalars numerators new: self size fill: [:i| ((self at: i) * d) numerator]! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/23/2022 10:38:25'!
support
	^ (1 to: self size) reject: [:each| self isZeroAt: each]! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/23/2022 10:41:18'!
x
	^ self at: 1! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/23/2022 10:41:21'!
y
	^ self at: 2! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/23/2022 10:41:23'!
z
	^ self at: 3! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/23/2022 21:13:53'!
zero
	self isEmpty ifTrue: [^ self].
	^ self scalars zeros: self size! !

!TupleOld methodsFor: 'comparing' stamp: 'len 11/24/2022 19:37:25'!
= anObject
	self == anObject ifTrue: [^ true].
	^ anObject isTuple and: [self scalars = anObject scalars and: [self hasEqualElements: anObject]]! !

!TupleOld methodsFor: 'converting' stamp: 'len 11/23/2022 10:23:25'!
asMatrix
	"Tuples are equivalent to columns."
	^ Matrix columns: {self}! !

!TupleOld methodsFor: 'converting' stamp: 'len 11/23/2022 10:23:30'!
asRowMatrix
	^ Matrix rows: {self}! !

!TupleOld methodsFor: 'converting' stamp: 'len 11/23/2022 10:23:35'!
asTuple
	^ self! !

!TupleOld methodsFor: 'converting' stamp: 'len 11/23/2022 21:17:14'!
over: aRing
	self scalars = aRing ifFalse: [^ aRing !!!! self]! !

!TupleOld methodsFor: 'copying' stamp: 'len 11/24/2022 11:59:43'!
, anObject
	anObject isCollection ifTrue: [^ super , anObject].
	^ self copyWith: self scalars !! anObject! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 11/23/2022 10:38:34'!
supportDo: aBlock
	1 to: self size do: [:index| (self isZeroAt: index) ifFalse: [aBlock value: index]]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 11/23/2022 10:41:03'!
withSupportDo: aBlock
	1 to: self size do: [:index| (self isZeroAt: index) ifFalse: [aBlock value: (self at: index) value: index]]! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:15:00'!
% anInteger
	self scalars = ℤ ifFalse: [self error: 'not over the integers'].
	^ self apply: (ℤ / anInteger) projection! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 13:05:08'!
* anObject
	| n |
	(self scalars includes: anObject)
		ifTrue: [^ (self species new: (n _ self size)) replaceFrom: 1 to: n with: self startingAt: 1 timesScalar: anObject].
	anObject isMatrix
		ifTrue: [^ (self asRowMatrix * anObject) asTuple].
	anObject isTuple
		ifTrue: [^ (self species new: (n _ self size min: anObject size)) replaceFrom: 1 to: n with: self startingAt: 1 times: anObject startingAt: 1].
	^ self * (self scalars !! anObject)! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 13:01:28'!
+ anObject
	| n m answer |
	n _ self size.
	anObject isTuple ifFalse: [^ (self species new: n) replaceFrom: 1 to: n with: self startingAt: 1 plusScalar: self scalars !! anObject].
	m _ anObject size.
	answer _ self species new: (n max: m).
	answer replaceFrom: 1 to: (n min: m) with: self startingAt: 1 plus: anObject startingAt: 1.
	m > n
		ifTrue: [answer replaceFrom: n+1 to: m with: anObject startingAt: n+1]
		ifFalse: [m < n ifTrue: [answer replaceFrom: m+1 to: n with: self startingAt: m+1]].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 13:02:06'!
- anObject
	| n m answer |
	n _ self size.
	anObject isTuple ifFalse: [^ (self species new: n) replaceFrom: 1 to: n with: self startingAt: 1 plusScalar: self scalars !! anObject negated].
	m _ anObject size.
	answer _ self species new: (n max: m).
	answer replaceFrom: 1 to: (n min: m) with: self startingAt: 1 minus: anObject startingAt: 1.
	m > n
		ifTrue: [answer replaceFrom: n+1 to: m withNegated: anObject startingAt: n+1]
		ifFalse: [m < n ifTrue: [answer replaceFrom: m+1 to: n with: self startingAt: m+1]].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/9/2022 09:04:32'!
/ anObject
	"If the argument is a matrix, answer X such that XA = B, where B is self and A is the argument.  X is called a lift of B along A.
	If the argument is a scalar, just divide each element of the tuple by it."
	(self scalars includes: anObject) ifTrue: [^ self collect: [:each| each / anObject]].
	anObject isMatrix ifTrue: [^ (anObject colift: self asRowMatrix) asTuple].
	anObject isTuple ifTrue: [^ 	super / anObject].
	^ self / (self scalars !! anObject)! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:13:31'!
// anElement
	(self scalars includes: anElement) ifTrue: [^ self collect: [:each| each // anElement]].
	^ super // anElement! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 11:51:45'!
<< anInteger
	"Answer the left shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	^ self >> anInteger negated! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 11:50:51'!
<<< anInteger
	"Answer the circular left shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	^ self >>> anInteger negated! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 11:51:54'!
>> anInteger
	"Answer the right shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	| answer zero n |
	anInteger = 0 ifTrue: [^ self].
	answer _ self species new: self size.
	zero _ self first zero.
	n _ self size min: anInteger max: self size negated.
	n > 0
		ifTrue:
			[answer from: 1 to: n put: zero.
			answer replaceFrom: 1+n to: answer size with: self startingAt: 1]
		ifFalse:
			[answer from: answer size + n + 1 to: answer size put: zero.
			answer replaceFrom: 1 to: answer size + n with: self startingAt: 1 - n].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 11:50:45'!
>>> anInteger
	"Answer the circular right shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	| n m |
	(anInteger = 0 or: [(m _ self size) = 0]) ifTrue: [^ self].
	n _ anInteger \\ m.
	^ (self species new: m)
		replaceFrom: 1 to: n with: self startingAt: m - n + 1;
		replaceFrom: 1+n to: m with: self startingAt: 1! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:16:08'!
\\ anElement
	(self scalars includes: anElement) ifTrue: [^ self collect: [:each| each \\ anElement]].
	^ super \\ anElement! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/7/2022 19:15:54'!
· aTuple
	| answer |
	answer _ self scalars zero.
	1 to: (self size min: aTuple size) do: [:i| answer _ (self at: i) * (aTuple at: i) + answer].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:18:16'!
× aTuple
	"Answer the cross product of the receiver with the argument."
	(self size = 3 and: [aTuple size = 3]) ifFalse: [DomainError signal].
	^ self scalars !!!!
			{(self at: 2)*(aTuple at: 3) - ((self at: 3)*(aTuple at: 2)).
			(self at: 3)*(aTuple at: 1) - ((self at: 1)*(aTuple at: 3)).
			(self at: 1)*(aTuple at: 2) - ((self at: 2)*(aTuple at: 1))}! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/15/2022 11:19:22'!
⊗ anElement
	"Answer the outer product of the receiver with the argument."
	anElement isTuple ifTrue: [^ Matrix from: anElement parent to: self parent fill: [:i :j| (self at: i)*(anElement at: j)]].
	self flag: #fixme. "this looks wrong over noncommutative rings"
	anElement isMatrix ifTrue: [^ self asMatrix ⊗ anElement transpose].
	^ super ⊗ anElement! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:20:13'!
annihilator
	^ self annihilatorIdeal generator! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:20:08'!
annihilatorIdeal
	| answer |
	answer _ self scalars asIdeal.
	self isZero ifTrue: [^ answer].
	self do: [:each| answer _ answer ∩ each annihilatorIdeal. answer isTrivial ifTrue: [^ answer]].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:23:42'!
associates
	^ self scalars units collect: [:each| self * each]! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:27:07'!
conjugate
	^ self collect: [:each| each conjugate]! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:27:23'!
content
	"Answer the GCD of the coefficients of the receiver.
	In a field, it is convenient to define the content as 1.
	For example, (2,4) has content 2 over Z and 1 over Q."
	| answer |
	self isZero ifTrue: [^ self scalars zero].
	self scalars isField ifTrue: [^ self scalars one].
	answer _ self scalars zero.
	self do: [:each| answer _ answer gcd: each].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:30:36'!
height
	| answer |
	answer _ 1.
	self do: [:each| answer _ answer max: each height].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:30:49'!
homogenizedAt: anInteger
	^ (self copyFrom: 1 to: anInteger - 1), self scalars one, (self copyFrom: anInteger to: self size)! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:32:25'!
length
	^ self norm sqrt! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 12:38:25'!
negated
	^ (self species new: self size) replaceFrom: 1 to: self size withNegated: self startingAt: 1! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:33:32'!
norm
	^ self · self! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:33:48'!
normalization
	"Answer a unit u such that 'self * u' is a unique choice of associate. See >>normalized.
	NOTE: when the coefficients ring is not an integral domain this choice of unit might not be unique."
	^ (self at: (self lastNonzeroIndex ifNil: [^ self scalars one])) normalization! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 22:05:21'!
normalized
	"Answer a unique choice of associate. See >>normalization."
	^ self * self normalization! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:36:26'!
primitivePart
	"Answer the primitive part of the receiver."
	^ self isZero ifTrue: [self] ifFalse: [self / self content]! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:36:47'!
product
	self isEmpty ifTrue: [^ self scalars one].
	^ super product! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:37:58'!
stabilizers
	| answer last |
	self size <= 1 ifTrue: [^ #()].
	answer _ OrderedCollection new: self size - 1.
	last _ self at: 1.
	2 to: self size do: [:i| | ai ci |
		ai _ self at: i.
		ci _ last stabilizerWith: ai.
		answer add: ci.
		last _ ci*ai + last].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/23/2022 10:38:09'!
sum
	self isEmpty ifTrue: [^ self scalars zero].
	^ super sum! !

!TupleOld methodsFor: 'operations' stamp: 'len 12/6/2022 11:54:09'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ (1 to: self size) count: [:each| (self isZeroAt: each) not]! !

!TupleOld methodsFor: 'operations-low level' stamp: 'len 12/1/2022 10:01:32'!
zeroFrom: start to: stop
	self from: start to: stop put: self scalars zero! !

!TupleOld methodsFor: 'printing' stamp: 'len 12/6/2022 12:27:00'!
printOn: aStream
	aStream nextPut: $(.
	self do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)! !

!TupleOld methodsFor: 'testing' stamp: 'len 12/18/2022 11:20:22'!
~ aTuple
	"Answer true if the receiver and the argument are equivalent as homogeneous tuples."
	| depth |
	(depth _ self lastNonzeroIndex) ~= aTuple lastNonzeroIndex ifTrue: [^ false].
	depth isNil ifTrue: [^ true].
	self scalars isIntegralDomain ifTrue:
		[| x y |
		x _ self at: depth.
		y _ aTuple at: depth.
		1 to: depth do: [:i| (self at: i) * y = ((aTuple at: i) * x) ifFalse: [^ false]].
		^ true].
	"Over a ring with standard bases we compare the standard bases:"
	(self scalars hasCanonicalBases and: [self scalars size > 255])
		ifTrue: [^ self asMatrix basis = aTuple asMatrix basis].
	"In a general ring we have to check all units:"
	^ self scalars units anySatisfy: [:any| self * any = aTuple]! !

!TupleOld methodsFor: 'testing' stamp: 'len 11/23/2022 10:18:56'!
⊥ aTuple
	"Answer true if the receiver and the argument are orthogonal."
	^ (self · aTuple) isZero! !

!TupleOld methodsFor: 'testing' stamp: 'len 11/23/2022 10:31:18'!
isMonomial
	^ self allSatisfy: [:each| each isMonomial]! !

!TupleOld methodsFor: 'testing' stamp: 'len 11/23/2022 10:27:50'!
isTuple
	^ true! !

!TupleOld methodsFor: 'testing' stamp: 'len 11/23/2022 10:31:51'!
isUnimodular
	"Answer true if the coefficients of the receiver generate the unit ideal.
	Some authors use the name 'primitive' instead of 'unimodular'."
	^ (self scalars * self asArray) isOne! !

!TupleOld methodsFor: 'private' stamp: 'len 11/23/2022 10:10:08'!
parent
	^ self scalars ^ self size! !

!TupleOld methodsFor: 'private' stamp: 'len 11/23/2022 10:10:28'!
species
	^ self scalars! !

!BitTuple methodsFor: 'accessing' stamp: 'len 5/4/2022 11:28:25'!
at: anInteger
	^ (self bitAt: anInteger) = 1 ifTrue: [One] ifFalse: [Zero]! !

!BitTuple methodsFor: 'accessing' stamp: 'len 5/4/2022 11:30:08'!
at: anInteger put: aBit
	self bitAt: anInteger put: aBit representative.
	^ aBit! !

!BitTuple methodsFor: 'accessing' stamp: 'len 12/2/2022 14:24:40'!
lift
	"Answer a representative tuple of integers."
	^ ℤ new: self size fill: [:i| self bitAt: i]! !

!BitTuple methodsFor: 'accessing' stamp: 'len 4/30/2022 06:53:15'!
scalars
	^ One parent! !

!BitTuple methodsFor: 'enumerating' stamp: 'len 5/4/2022 12:33:42'!
anySatisfy: aBlock
	"Optimized. There are only two possible elements that could satisfy aBlock (1 or 0)."
	self isEmpty ifTrue: [^ false].
	(self isZero not and: [aBlock value: One]) ifTrue: [^ true].
	^ (aBlock value: Zero) and: [super anySatisfy: [:any| any = Zero]]! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/20/2022 09:55:07'!
- anObject
	^ self + anObject! !

!BitTuple methodsFor: 'operations' stamp: 'len 4/30/2022 12:29:13'!
length
	^ self norm! !

!BitTuple methodsFor: 'operations' stamp: 'len 4/30/2022 12:23:13'!
negated
	^ self! !

!BitTuple methodsFor: 'operations' stamp: 'len 4/30/2022 12:20:24'!
norm
	^ self sum! !

!BitTuple methodsFor: 'operations' stamp: 'len 12/27/2022 11:13:06'!
polynomialValue: anObject
	self size = 0 ifTrue: [^ Zero].
	Zero = anObject ifTrue: [^ self at: 1].
	One = anObject ifTrue: [^ self sum].
	^ super polynomialValue: anObject! !

!BitTuple methodsFor: 'operations' stamp: 'len 4/30/2022 12:27:25'!
sum
	"Answer the sum of the bits, i.e. the parity."
	^ self parity = 0 ifTrue: [Zero] ifFalse: [One]! !

!BitTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 10:08:33'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2
	self replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2! !

!BitTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 10:10:19'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: anElement
	anElement isZero ifTrue: [^ self replaceFrom: start to: stop with: anArray1 startingAt: start1].
	self replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2! !

!BitTuple methodsFor: 'operations-low level' stamp: 'len 12/6/2022 18:59:45'!
replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: anElement
	anElement isZero ifTrue: [^ self zeroFrom: start to: stop].
	self replaceFrom: start to: stop with: anArray startingAt: srcStart! !

!BitTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 08:18:31'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart
	self replaceFrom: start to: stop with: anArray startingAt: srcStart! !

!BitTuple methodsFor: 'operations-low level' stamp: 'len 12/9/2022 08:03:15'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2
	self zeroFrom: start to: start + stop1 - start1 + stop2 - start2.
	start2 to: stop2 do: [:i| 
		(anArray2 isZeroAt: i) ifFalse:
			[| index |
			index _ i-start2+start.
			self replaceFrom: index to: index+stop1-start1
				with: self startingAt: index
				plus: anArray1 startingAt: start1]]! !

!BitTuple methodsFor: 'operations-low level' stamp: 'len 12/9/2022 08:07:51'!
replaceFrom: start withAutoconvolutionLong: anArray1 from: start1 to: stop1
	self flag: #fixme. "TOOD"
	self replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionLong: anArray1 from: start1 to: stop1! !

!BitTuple methodsFor: 'printing' stamp: 'len 4/30/2022 06:52:27'!
printOn: aStream
	1 to: self size do: [:i| aStream print: (self at: i)]! !

!BitTuple methodsFor: 'testing' stamp: 'len 5/4/2022 12:34:30'!
isZeroAt: anInteger
	^ (self bitAt: anInteger) = 0! !

!BitTuple class methodsFor: 'class initialization' stamp: 'len 12/18/2022 12:30:01'!
initialize
	| Z2 |
	super initialize.
	ℤ isNil ifTrue: [^ self].
	Z2 _ ℤ / 2.
	One _ Z2 one.
	Zero _ Z2 zero! !

!BitTuple class methodsFor: 'instance creation' stamp: 'len 5/4/2023 23:41:56'!
new: anInteger
	^ (anInteger <= 60 ifTrue: [SmallBitTuple] ifFalse: [LargeBitTuple]) new: anInteger! !

!LargeBitTuple methodsFor: 'accessing' stamp: 'len 5/20/2022 10:02:52'!
bitAt: anInteger
	anInteger <= size ifFalse: [^ self errorOutOfBounds].
	^ words bitAt: anInteger! !

!LargeBitTuple methodsFor: 'accessing' stamp: 'len 5/20/2022 10:02:56'!
bitAt: anInteger put: aBit
	anInteger <= size ifFalse: [^ self errorOutOfBounds].
	^ words bitAt: anInteger put: aBit! !

!LargeBitTuple methodsFor: 'accessing' stamp: 'len 5/20/2022 09:06:14'!
lastNonzeroIndex
	"Answer the index of the last nonzero entry, or nil."
	| wordIndex |
	(wordIndex _ words lastNonzeroIndex) ifNil: [^ nil].
	^ wordIndex - 1 * 32 + (words at: wordIndex) highBit! !

!LargeBitTuple methodsFor: 'accessing' stamp: 'len 4/30/2022 06:18:18'!
size
	^ size! !

!LargeBitTuple methodsFor: 'comparing' stamp: 'len 4/30/2022 06:54:21'!
= anObject
	^ self class = anObject class and: [size = anObject size and: [words = anObject words]]! !

!LargeBitTuple methodsFor: 'comparing' stamp: 'len 4/30/2022 06:53:46'!
hash
	^ words hash! !

!LargeBitTuple methodsFor: 'converting' stamp: 'len 4/30/2022 05:59:02'!
asInteger
	| answer |
	answer _ 0.
	words reverseDo: [:each| answer _ answer << 32 bitXor: each].
	^ answer! !

!LargeBitTuple methodsFor: 'copying' stamp: 'len 11/24/2022 11:43:14'!
, anObject
	self flag: #deprecated.
	anObject class = self class ifFalse: [^ super , anObject].
	^ (self class new: self size + anObject size)
		replaceFrom: 1 to: self size with: self;
		replaceFrom: self size + 1 to: self size + anObject size with: anObject! !

!LargeBitTuple methodsFor: 'copying' stamp: 'len 4/30/2022 18:11:24'!
copyWith: aBit
	| answer |
	answer _ self class new: self size + 1 words: (size \\ 32 = 0 ifTrue: [words copyWith: 0] ifFalse: [words copy]).
	aBit = One ifTrue: [^ answer at: self size+1 put: One; yourself].
	aBit = Zero ifTrue: [^ answer].
	^ self error: 'not one or zero'! !

!LargeBitTuple methodsFor: 'copying' stamp: 'len 12/20/2022 09:33:05'!
postCopy
	super postCopy.
	words _ words copy! !

!LargeBitTuple methodsFor: 'operations' stamp: 'len 11/25/2022 10:15:37'!
* anObject
	| newWords |
	anObject isInteger ifTrue: [^ anObject even ifTrue: [self zero] ifFalse: [self]].
	self class = anObject class ifFalse: [^ super * anObject].
	newWords _ WordArray new: (words size min: anObject words size).
	1 to: newWords size do: [:i| newWords at: i put: ((words at: i) bitAnd: (anObject words at: i))].
	^ self class new: (self size min: anObject size) words: newWords! !

!LargeBitTuple methodsFor: 'operations' stamp: 'len 12/10/2022 12:38:45'!
· aTuple
	| sum |
	sum _ 0.
	1 to: (words size min: aTuple words size) do: [:i|
		sum _ ((words at: i) bitAnd: (aTuple words at: i)) bitParity bitXor: sum].
	^ sum bitParity = 0 ifTrue: [Zero] ifFalse: [One]! !

!LargeBitTuple methodsFor: 'operations' stamp: 'len 4/30/2022 12:27:10'!
parity
	^ words inject: 0 into: [:result :each| result bitXor: each bitParity]! !

!LargeBitTuple methodsFor: 'operations' stamp: 'len 4/30/2022 12:28:38'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ words inject: 0 into: [:sum :each| sum + each bitCount]! !

!LargeBitTuple methodsFor: 'operations-low level' stamp: 'len 5/20/2022 09:12:27'!
replaceFrom: start to: stop with: replacement startingAt: repStart	
	| n shift i j w r |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
"	start to: stop do: [:i| self bitAt: i put: (replacement bitAt: i-start+repStart)]"
	shift _ start - 1 \\ 32.
	i _ start - 1 // 32 + 1.
	j _ repStart.
	shift > 0 ifTrue:
		[w _ words at: i.
		r _ replacement wordAtBitIndex: j.
		n < 32
			ifTrue: [r _ r bitAnd: (1 << n - 1). w _ w bitAnd: (1 << n - 1 << shift) bitInvert]
			ifFalse: [w _ w bitAnd: 1 << shift - 1].
		words at: i put: (w bitXor: (r bitAnd: 1 << (32 - shift) - 1) << shift).
		n _ n - (32 - shift).
		j _ j + 32 - shift.
		i _ i + 1].
	[n > 32] whileTrue:
		[w _ words at: i.
		r _ replacement wordAtBitIndex: j.
		words at: i put: r.
		n _ n - 32.
		j _ j + 32.
		i _ i + 1].
	n > 0 ifTrue:
		[w _ words at: i.
		r _ replacement wordAtBitIndex: j.
		words at: i put: ((w bitAnd: (1 << n - 1) bitInvert) bitXor: (r bitAnd: 1 << n - 1))]! !

!LargeBitTuple methodsFor: 'operations-low level' stamp: 'len 5/4/2022 12:11:56'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2	
	^ super replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2	

"	| mask n slice |
	(n _ stop - start + 1) < 0 ifTrue: [^ self].
	startWords _ start - 1 // 32 + 1.
	startBitPosition _ start - (startWords * 32).
	stopWords _ stop - 1 // 32 + 1.
	start1Words _ start1 - 1 // 32 + 1.
	start1BitPosition _ start - (start1Words * 32).
	start2Words _ start2 - 1 // 32 + 1.
	start2BitPosition _ start - (start2Words * 32).
	carry1 _ 0.
	carry2 _ 0.
	j _ start1 - 1 // 32 + 1.
	k _ start2 - 1 // 32 + 1.
	startWords to: stopWords do: [:i|
		words at: i put: ((anArray1 wordAt: j) >> shift bitXor: carrier).
		carrier _ (words at: j) << (32 - shift) bitAnd: 16rFFFFFFFF.
		j _ j + 1].
	n > 0 ifTrue: [newWords at: newWords size put: ((newWords at: newWords size) bitAnd: 16rFFFFFFFF >> (32 - (n \\ 32)))].
	"! !

!LargeBitTuple methodsFor: 'operations-low level' stamp: 'len 5/4/2022 20:09:47'!
wordAtBitIndex: bitIndex
	| wordIndex shift answer |
	wordIndex _ bitIndex - 1 // 32 + 1.
	wordIndex > words size ifTrue: [^ 0].
	answer _ words at: wordIndex.
	(shift _ bitIndex - 1 \\ 32) = 0 ifTrue: [^ answer].
	answer _ answer >> shift.
	wordIndex = words size ifTrue: [^ answer].
	^ answer bitXor: ((words at: wordIndex+1) << (32 - shift) bitAnd: 0xFFFFFFFF)! !

!LargeBitTuple methodsFor: 'testing' stamp: 'len 5/20/2022 11:58:11'!
isZero
	^ words isZero! !

!LargeBitTuple methodsFor: 'private' stamp: 'len 4/30/2022 06:54:28'!
words
	^ words! !

!LargeBitTuple methodsFor: 'private' stamp: 'len 4/30/2022 18:00:59'!
words: aWordArray size: anInteger
	| n |
	n _ aWordArray size.
	anInteger - 1 // 32 + 1 = n ifFalse: [self error: 'too many words'].
"	words size = 0 or: [words last highBit <= (size \\ 32 + 1)]"
	words _ aWordArray.
	size _ anInteger! !

!LargeBitTuple class methodsFor: 'instance creation' stamp: 'len 4/30/2022 06:16:47'!
new: anInteger
	^ self new: anInteger words: (WordArray new: anInteger - 1 // 32 + 1)! !

!LargeBitTuple class methodsFor: 'instance creation' stamp: 'len 4/30/2022 06:14:33'!
new: anInteger words: aWordArray
	^ self basicNew words: aWordArray size: anInteger! !

!LargeBitTuple class methodsFor: 'instance creation' stamp: 'len 4/30/2022 06:17:56'!
newFrom: anObject
	| n r words |
	anObject isInteger ifFalse: [^ super newFrom: anObject].
	n _ anObject.
	r _ n highBit.
	words _ WordArray new: r - 1 // 32 + 1.
	1 to: words size do: [:i|
		words at: i put: (n bitAnd: 16rFFFFFFFF).
		n _ n >> 32].
	^ self new: r words: words! !

!SmallBitTuple methodsFor: 'accessing' stamp: 'len 5/1/2022 21:22:41'!
atAllPut: aBit
	| bit |
	bit _ aBit.
	bit isInteger ifTrue: [bit _ bit even ifTrue: [Zero] ifFalse: [One]].
	bit = One
		ifTrue: [bits _ 1 << size - 1]
		ifFalse: [bit = Zero ifTrue: [bits _ 0] ifFalse: [self error: 'not a bit']]! !

!SmallBitTuple methodsFor: 'accessing' stamp: 'len 7/19/2020 08:06:56'!
bitAt: anInteger
	^ bits bitAt: anInteger! !

!SmallBitTuple methodsFor: 'accessing' stamp: 'len 5/4/2022 11:24:19'!
bitAt: anInteger put: oneOrZero
	(anInteger > self size and: [oneOrZero ~= 0]) ifTrue: [self error: 'index out of bounds'].
	bits _ bits bitAt: anInteger put: oneOrZero.
	^ oneOrZero! !

!SmallBitTuple methodsFor: 'accessing' stamp: 'len 5/19/2022 05:53:28'!
lastNonzeroIndex
	"Answer the index of the last nonzero entry, or nil."
	| n |
	^ (n _ bits highBit) = 0 ifFalse: [n]! !

!SmallBitTuple methodsFor: 'accessing' stamp: 'len 4/17/2022 11:31:59'!
size
	^ size! !

!SmallBitTuple methodsFor: 'comparing' stamp: 'len 5/20/2022 06:33:17'!
= anObject
	^ self class = anObject class and: [size = anObject size and: [bits = anObject asInteger]]! !

!SmallBitTuple methodsFor: 'comparing' stamp: 'len 5/8/2019 03:06:15'!
hash
	^ bits hash! !

!SmallBitTuple methodsFor: 'converting' stamp: 'len 5/8/2019 03:10:19'!
asInteger
	^ bits! !

!SmallBitTuple methodsFor: 'copying' stamp: 'len 11/24/2022 11:43:44'!
, anObject
	self flag: #deprecated.
	anObject class = self class ifFalse: [^ super , anObject].
	^ self class new: self size+anObject size bits: (bits bitXor: (anObject asInteger bitShift: self size))! !

!SmallBitTuple methodsFor: 'copying' stamp: 'len 4/30/2022 13:02:18'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."
	| n |
	n _ stop - start + 1 max: 0.
	^ self class new: n bits: ((bits bitShift: 1 - start) bitAnd: (1 bitShift: n) - 1)! !

!SmallBitTuple methodsFor: 'copying' stamp: 'len 4/30/2022 13:02:32'!
copyWith: aBit
	| answer |
	answer _ self class new: self size + 1 bits: bits.
	aBit = One ifTrue: [answer at: self size+1 put: One. ^ answer].
	aBit = Zero ifFalse: [DomainError signal].
	^ answer! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 11/25/2022 10:15:48'!
* anObject
	anObject isInteger ifTrue: [^ anObject even ifTrue: [self zero] ifFalse: [self]].
	self class = anObject class ifFalse: [^ super * anObject].
	^ self class new: (self size min: anObject size) bits: (bits bitAnd: anObject asInteger)! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 5/2/2022 08:03:12'!
+ anObject
	anObject class = self class ifFalse: [^ (((anObject isInteger and: [anObject even]) or: [anObject isZero]) or: [size = 0]) ifTrue: [self] ifFalse: [self class new: size bits: (bits bitXor: (1 << size - 1))]].
	^ self class
		new: (self size max: anObject size)
		bits: (bits bitXor: anObject asInteger)! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 5/2/2022 11:30:39'!
>> anInteger
	^ self class new: size bits: ((bits bitShift: anInteger) bitAnd: 1<<size - 1)! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 12/10/2022 12:39:11'!
· aTuple
	^ (bits bitAnd: aTuple asInteger) bitParity = 0 ifTrue: [Zero] ifFalse: [One]! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 12/9/2022 08:09:23'!
autoconvolution
	self flag: #fixme. "TODO: optimize"
	^ self class new: (size * 2 - 1 max: 0) bits: (bits bitConvolution: bits)! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 12/6/2022 19:02:58'!
convolution: aBitTuple
	^ self class new: (size + aBitTuple size - 1 max: 0) bits: (bits bitConvolution: aBitTuple asInteger)! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 4/30/2022 12:27:40'!
parity
	^ bits bitParity! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 12/6/2022 19:03:15'!
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

!SmallBitTuple methodsFor: 'operations' stamp: 'len 12/6/2022 19:04:29'!
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

!SmallBitTuple methodsFor: 'operations' stamp: 'len 4/30/2022 13:03:17'!
reversed
	^ self class new: self size bits: (bits bitReverse: self size)! !

!SmallBitTuple methodsFor: 'operations' stamp: 'len 11/12/2016 08:41'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ self asInteger bitCount! !

!SmallBitTuple methodsFor: 'operations-low level' stamp: 'len 5/4/2022 12:32:17'!
replaceFrom: start to: stop with: replacement startingAt: repStart	
	| mask n slice |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
	mask _ 1<<n - 1.
	slice _ replacement asInteger >> (repStart-1) bitAnd: mask.
	bits _ (bits bitAnd: (mask << (start-1)) bitInvert) bitXor: slice << (start-1)! !

!SmallBitTuple methodsFor: 'operations-low level' stamp: 'len 5/4/2022 12:41:23'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2	
	| mask n slice |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
	mask _ 1<<n - 1.
	slice _ anArray1 asInteger >> (start1-1) bitAnd: mask.
	slice _ (anArray2 asInteger >> (start2-1) bitAnd: mask) bitXor: slice.
	bits _ (bits bitAnd: (mask << (start-1)) bitInvert) bitXor: slice << (start-1)! !

!SmallBitTuple methodsFor: 'operations-low level' stamp: 'len 5/4/2022 12:41:29'!
replaceFrom: start to: stop with: anArray startingAt: srcStart plusScalar: anElement
	| mask n slice |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
	mask _ 1<<n - 1.
	slice _ anArray asInteger >> (srcStart-1) bitAnd: mask.
	anElement isZero ifFalse: [slice _ slice bitInvert bitAnd: mask].
	bits _ (bits bitAnd: (mask << (start-1)) bitInvert) bitXor: slice << (start-1)! !

!SmallBitTuple methodsFor: 'operations-low level' stamp: 'len 5/4/2022 12:41:33'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2
	| mask n slice |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
	mask _ 1<<n - 1.
	slice _ anArray1 asInteger >> (start1-1) bitAnd: mask.
	slice _ anArray2 asInteger >> (start2-1) bitAnd: slice.
	bits _ (bits bitAnd: (mask << (start-1)) bitInvert) bitXor: slice << (start-1)! !

!SmallBitTuple methodsFor: 'testing' stamp: 'len 5/20/2022 11:22:46'!
isZero
	^ bits = 0! !

!SmallBitTuple methodsFor: 'private' stamp: 'len 4/17/2022 11:30:54'!
bits: anInteger size: anotherInteger
	bits _ anInteger.
	size _ anotherInteger! !

!SmallBitTuple methodsFor: 'private' stamp: 'len 7/8/2022 11:12:05'!
validate
	self assert: (bits >= 0 and: [size >= 0 and: [bits highBit <= size]])! !

!SmallBitTuple class methodsFor: 'instance creation' stamp: 'len 4/30/2022 13:01:53'!
bits: anInteger
	^ self new: anInteger highBit bits: anInteger! !

!SmallBitTuple class methodsFor: 'instance creation' stamp: 'len 4/30/2022 13:02:45'!
new: anInteger
	^ self new: anInteger bits: 0! !

!SmallBitTuple class methodsFor: 'instance creation' stamp: 'len 4/30/2022 12:59:53'!
new: anInteger bits: anotherInteger
	self assert: anotherInteger highBit <= anInteger.
	^ self basicNew bits: anotherInteger size: anInteger! !

!SmallBitTuple class methodsFor: 'instance creation' stamp: 'len 4/17/2022 11:40:37'!
newFrom: anInteger
	anInteger isInteger ifTrue: [^ self bits: anInteger].
	^ super newFrom: anInteger! !

!DenseTuple methodsFor: 'accessing' stamp: 'len 11/23/2022 10:24:04'!
at: anInteger
	anInteger isInteger ifFalse: [^ (coefficients at: anInteger position) at: anInteger monomial].
	^ coefficients at: anInteger! !

!DenseTuple methodsFor: 'accessing' stamp: 'len 11/23/2022 10:24:41'!
at: anInteger ifAbsent: aBlock
	anInteger isInteger ifFalse: [^ (coefficients at: anInteger position ifAbsent: [^aBlock value]) at: anInteger monomial ifAbsent: aBlock].
	^ coefficients at: anInteger ifAbsent: aBlock! !

!DenseTuple methodsFor: 'accessing' stamp: 'len 12/6/2022 19:07:17'!
at: anInteger put: anObject
	^ coefficients at: anInteger put: scalars !! anObject! !

!DenseTuple methodsFor: 'accessing' stamp: 'len 11/23/2022 10:22:50'!
scalars
	^ scalars! !

!DenseTuple methodsFor: 'accessing' stamp: 'len 11/23/2022 21:47:32'!
size
	^ coefficients size! !

!DenseTuple methodsFor: 'comparing' stamp: 'len 11/24/2022 19:43:09'!
= anObject
	^ self class = anObject class and: [coefficients = anObject coefficients and: [scalars = anObject scalars]]! !

!DenseTuple methodsFor: 'comparing' stamp: 'len 11/24/2022 19:41:58'!
hash
	^ coefficients hash! !

!DenseTuple methodsFor: 'converting' stamp: 'len 11/24/2022 12:25:28'!
asArray
	^ coefficients! !

!DenseTuple methodsFor: 'copying' stamp: 'len 12/20/2022 09:33:08'!
postCopy
	super postCopy.
	coefficients _ coefficients copy! !

!DenseTuple methodsFor: 'private' stamp: 'len 11/23/2022 10:26:01'!
coefficients
	^ coefficients! !

!DenseTuple methodsFor: 'private' stamp: 'len 11/23/2022 10:26:21'!
coefficients: anArray scalars: aRing
	coefficients _ anArray.
	scalars _ aRing! !

!DenseTuple methodsFor: 'private' stamp: 'len 11/23/2022 10:39:20'!
validate
	super validate.
	self assert: (coefficients allSatisfy: [:each| scalars includes: each])! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/27/2022 22:17:29'!
fillWithColumnMajorMatrix: anArray width: width1 timesMatrix: anotherArray width: width2
	coefficients fillWithColumnMajorMatrix: anArray coefficients width: width1 timesMatrix: anotherArray coefficients width: width2! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:12:04'!
fillWithMatrixTranspose: anArray width: width
	coefficients fillWithMatrixTranspose: anArray width: width! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/27/2022 22:16:50'!
fillWithRowMajorMatrix: anArray width: width1 timesMatrix: anotherArray width: width2
	coefficients fillWithRowMajorMatrix: anArray coefficients width: width1 timesMatrix: anotherArray coefficients width: width2! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:13:20'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2
	coefficients replaceFrom: start to: stop with: anArray1 coefficients startingAt: start1 minus: anArray2 coefficients startingAt: start2
! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:13:41'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2
	coefficients replaceFrom: start to: stop with: anArray1 coefficients startingAt: start1 plus: anArray2 coefficients startingAt: start2
! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:14:14'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: anElement
	coefficients replaceFrom: start to: stop with: anArray1 coefficients startingAt: start1 plus: anArray2 coefficients startingAt: start2 timesScalar: anElement! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:14:50'!
replaceFrom: start to: stop with: anArray startingAt: srcStart plusScalar: anElement
	coefficients replaceFrom: start to: stop with: anArray coefficients startingAt: srcStart plusScalar: anElement! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:15:09'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2
	coefficients replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:15:32'!
replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: anElement
	coefficients replaceFrom: start to: stop with: anArray coefficients startingAt: srcStart timesScalar: anElement! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/24/2022 11:47:38'!
replaceFrom: start to: stop withMatrix: anArray height: height transpose: transpose width: width timesTuple: anotherArray startingAt: start2
	coefficients replaceFrom: start to: stop withMatrix: anArray coefficients height: height width: width transpose: transpose timesTuple: anotherArray coefficients startingAt: start2
! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:16:25'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart
	coefficients replaceFrom: start to: stop withNegated: anArray coefficients startingAt: srcStart

! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:16:50'!
replaceFrom: start to: stop withSparseMatrix: anArray columnIndices: columnIndices rowPointers: rowPointers timesTuple: anotherArray
	coefficients replaceFrom: start to: stop withSparseMatrix: anArray coefficients columnIndices: columnIndices rowPointers: rowPointers timesTuple: anotherArray coefficients! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/9/2022 07:52:58'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolution: anArray2 from: start2 to: stop2
	coefficients replaceFrom: start with: anArray1 coefficients from: start1 to: stop1 convolution: anArray2 coefficients from: start2 to: stop2! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/9/2022 07:53:47'!
replaceFrom: start withAutoconvolution: anArray1 from: start1 to: stop1
	coefficients replaceFrom: start withAutoconvolution: anArray1 coefficients from: start1 to: stop1! !

!DenseTuple methodsFor: 'as yet unclassified' stamp: 'len 12/6/2022 19:17:54'!
zeroFrom: start to: stop
	coefficients from: start to: stop put: scalars zero! !

!DenseTuple class methodsFor: 'instance creation' stamp: 'len 11/23/2022 10:46:13'!
coefficients: anArray scalars: aRing
	^ self basicNew coefficients: anArray scalars: aRing! !

!PolynomialTuple methodsFor: 'accessing' stamp: 'len 7/6/2022 11:33:09'!
degree
	self isEmpty ifTrue: [^ Infinity negative].
	^ coefficients max: [:each| each degree]! !

!PolynomialTuple methodsFor: 'accessing' stamp: 'len 1/14/2022 13:54:47'!
ecart
	"Answer the ecart of the receiver. This is a measure of how far a tuple is from being homogeneous."
	self isZero ifTrue: [^ Infinity negative].
	^ (self max: [:each| each degree]) - self leadingMonomial degree! !

!PolynomialTuple methodsFor: 'accessing' stamp: 'len 1/13/2022 10:10:04'!
leadingCoefficient
	^ self at: self leadingMonomial! !

!PolynomialTuple methodsFor: 'accessing' stamp: 'len 4/11/2021 11:01:33'!
leadingMonomial
	^ leadingMonomial ifNil: [leadingMonomial _ self ordering leadingMonomialOf: self]! !

!PolynomialTuple methodsFor: 'accessing' stamp: 'len 1/13/2022 10:10:22'!
leadingTerm
	^ self termAt: self leadingMonomial! !

!PolynomialTuple methodsFor: 'accessing' stamp: 'len 11/23/2022 17:39:58'!
ordering
	^ self parent ordering! !

!PolynomialTuple methodsFor: 'accessing' stamp: 'len 1/13/2022 10:10:35'!
tail
	^ self - self leadingTerm! !

!PolynomialTuple methodsFor: 'accessing' stamp: 'len 5/5/2023 00:59:22'!
termAt: aModuleMonomial
	^ (self parent x: aModuleMonomial position) * ((self at: aModuleMonomial position) termAt: aModuleMonomial monomial)! !

!PolynomialTuple methodsFor: 'enumerating' stamp: 'len 5/5/2023 00:38:36'!
associationsDo: aBlock
	| ordering |
	ordering _ self ordering.
	self withIndexDo: [:fi :i|
		fi associationsDo: [:each| aBlock value: (Association key: (ModuleMonomial new ordering: ordering; position: i; monomial: each key) value: each value)]]! !

!PolynomialTuple methodsFor: 'enumerating' stamp: 'len 5/5/2023 00:38:36'!
monomialsDo: aBlock
	| ordering |
	ordering _ self ordering.
	self withIndexDo: [:fi :i|
		fi monomialsDo: [:m| aBlock value: (ModuleMonomial new ordering: ordering; position: i; monomial: m)]]! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 5/5/2023 00:38:36'!
* anObject
	anObject class = ModuleMonomial ifTrue: [^ (self parent x: anObject position) * ((self at: anObject position) * anObject monomial)].
	anObject class = Monomial ifTrue: [^ self collect: [:each| each * anObject]].
	^ super * anObject! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 5/5/2023 00:38:36'!
// anObject
	anObject isCollection ifTrue: [^ (self divisionBy: anObject) quotients].
	anObject class = ModuleMonomial ifTrue: [^ self withIndexCollect: [:each :i| i = anObject position ifTrue: [each // anObject monomial] ifFalse: [each]]].
	^ self collect: [:each| each // anObject]! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 1/13/2022 10:05:00'!
\\ anObject
	anObject isCollection ifTrue: [^ self reductionFullBy: anObject].
	^ self collect: [:each| each \\ anObject]! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 1/13/2022 09:23:17'!
divisionBy: anArray
	self scalars ordering isGlobal ifFalse: [^ self notYetImplemented].
	^ MultivariateDivision divide: self by: (anArray isCollection ifTrue: [anArray] ifFalse: [{anArray}])! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 5/22/2022 12:35:17'!
g: aTuple
	"Answer the GCD-tuple of the receiver with the argument."
	| m xgcd |
	m _ self leadingMonomial lcm: aTuple leadingMonomial.
	xgcd _ self leadingCoefficient xgcd: aTuple leadingCoefficient.
	^ self * (m / self leadingMonomial) * (xgcd at: 2) + (aTuple * (m / aTuple leadingMonomial) * (xgcd at: 3))! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 5/24/2022 12:56:29'!
normalization
	"Answer a unit u such that 'self * u' is a unique choice of associate. See >>normalized.
	NOTE: when the coefficients ring is not an integral domain this choice of unit might not be unique."
	^ self leadingCoefficient normalization! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 1/13/2022 09:22:38'!
reductionBy: anArray
	"If the ordering is global answer a top-reduction of the receiver with respect to the argument (without doing tail reduction). Otherwise answer the Mora normal form."
	^ self scalars ordering isGlobal
		ifTrue: [self reductionGlobalBy: anArray]
		ifFalse: [self reductionMoraBy: anArray]! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 1/13/2022 10:05:13'!
reductionFullBy: anArray
	"Answer the tail-reduced normal form of the receiver with respect to the argument (doing full tail reduction).
	For an ordering that is not global, answer the Mora normal form."
	| g h |
	self scalars ordering isGlobal ifFalse: [^ self reductionMoraBy: anArray].
	g _ self.
	h _ g zero.
	[g isZero]
		whileFalse:
			[g _ g reductionGlobalBy: anArray.
			g isZero ifFalse: [h _ h + g leadingTerm. g _ g tail]].
	^ h! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 5/22/2022 11:18:46'!
reductionGlobalBy: anArray
	"Answer a top-reduction of the receiver with respect to the argument (without doing tail reduction)."
	| h |
	h _ self.
	[h isZero]
		whileFalse:
			[| m c g |
			m _ h leadingMonomial.
			c _ h leadingCoefficient.
			g _ anArray detect: [:one| one leadingMonomial | m and: [one leadingCoefficient | c]] ifNone: [^ h].
			h _ h + (g * (m / g leadingMonomial) * (c negated / g leadingCoefficient))].
	^ h! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 6/7/2022 12:30:54'!
reductionMoraBy: anArray
	"Answer Mora's normal form of the receiver with respect to the argument."
	| h L m c g ecartg |
	h _ self.
	L _ OrderedCollection withAll: anArray.
	[h isZero ifTrue: [^ h].
	m _ h leadingMonomial.
	c _ h leadingCoefficient.
	g _ ecartg _ nil.
	L do: [:each|
		| ecart |
		(each leadingMonomial | m and: [each leadingCoefficient | c and: [ecart _ each ecart. g isNil or: [ecart < ecartg]]])
			ifTrue: [g _ each. ecartg _ ecart]].
	g isNil ifTrue: [^ h].
	ecartg > h ecart ifTrue: [L add: h].
	h _ h + (g * (m / g leadingMonomial) * (c negated / g leadingCoefficient))] repeat! !

!PolynomialTuple methodsFor: 'operations' stamp: 'len 5/24/2022 14:05:44'!
s: aTuple
	"Answer the S-tuple of the receiver with the argument."
	| m c |
	m _ (self leadingMonomial lcm: aTuple leadingMonomial) ifNil: [^ nil].
	c _ self leadingCoefficient lcm: aTuple leadingCoefficient.
	^ self * (m / self leadingMonomial) * (c / self leadingCoefficient) + (aTuple * (m / aTuple leadingMonomial) * (c negated / aTuple leadingCoefficient))! !

!PolynomialTuple methodsFor: 'testing' stamp: 'len 8/16/2022 09:48:19'!
isHomogeneous
	| n |
	n _ self degree.
	^ coefficients allSatisfy: [:each| each isZero or: [each degree = n]]! !

!GaloisTuple methodsFor: 'accessing' stamp: 'len 4/26/2022 07:19:17'!
at: anInteger
	| n startIndex |
	n _ scalars degree.
	startIndex _ anInteger - 1 * n + 1.
	^ scalars coefficients: (coefficients copyFrom: startIndex to: startIndex + n - 1)! !

!GaloisTuple methodsFor: 'accessing' stamp: 'len 5/1/2022 19:59:08'!
at: anInteger put: anElement
	| n startIndex elementCoefficients |
	n _ scalars degree.
	startIndex _ anInteger - 1 * n + 1.
	elementCoefficients _ anElement "representative" coefficients.
	coefficients replaceFrom: startIndex to: startIndex + elementCoefficients size - 1 with: elementCoefficients.
	self assert: elementCoefficients size = n.
"	elementCoefficients size < n
		ifTrue: [coefficients from: startIndex + elementCoefficients size to: startIndex + n - 1 put: scalars scalars zero]."
	^ anElement! !

!GaloisTuple methodsFor: 'accessing' stamp: 'len 4/26/2022 12:36:17'!
scalars
	^ scalars! !

!GaloisTuple methodsFor: 'accessing' stamp: 'len 4/26/2022 07:18:40'!
size
	^ coefficients size // scalars degree! !

!GaloisTuple methodsFor: 'accessing' stamp: 'len 5/3/2022 13:27:01'!
swap: oneIndex with: anotherIndex 
	"Move the element at oneIndex to anotherIndex, and vice-versa."
	| n start1 start2 |
	n _ scalars degree.
	start1 _ oneIndex - 1 * n + 1.
	start2 _ anotherIndex - 1 * n + 1.
	coefficients swapFrom: start1 to: start1 + n - 1 with: coefficients startingAt: start2! !

!GaloisTuple methodsFor: 'comparing' stamp: 'len 5/2/2022 07:45:42'!
= anObject
	^ self class = anObject class and: [coefficients = anObject coefficients and: [scalars = anObject scalars]]! !

!GaloisTuple methodsFor: 'comparing' stamp: 'len 4/26/2022 16:27:59'!
hash
	^ coefficients hash! !

!GaloisTuple methodsFor: 'copying' stamp: 'len 12/20/2022 09:33:11'!
postCopy
	super postCopy.
	coefficients _ coefficients copy! !

!GaloisTuple methodsFor: 'copying' stamp: 'len 5/2/2022 07:44:49'!
replaceFrom: start to: stop with: replacement startingAt: repStart
	| n |
	(replacement class = self class and: [scalars = replacement scalars])
		ifFalse: [^ super replaceFrom: start to: stop with: replacement startingAt: repStart].
	n _ scalars degree.
	coefficients replaceFrom: start-1*n+1 to: stop*n with: replacement coefficients startingAt: repStart-1*n+1! !

!GaloisTuple methodsFor: 'operations' stamp: 'len 5/20/2022 10:50:01'!
>> anInteger
	"Answer the right shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	^ self class coefficients: coefficients >> (anInteger * scalars degree) scalars: scalars! !

!GaloisTuple methodsFor: 'operations' stamp: 'len 5/20/2022 10:50:28'!
>>> anInteger
	"Answer the circular right shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	^ self class coefficients: coefficients >>> (anInteger * scalars degree) scalars: scalars! !

!GaloisTuple methodsFor: 'operations' stamp: 'len 12/24/2022 11:25:03'!
· aTuple
	^ self from: 1 to: (self size min: aTuple size) by: 1 dot: aTuple startingAt: 1 by: 1! !

!GaloisTuple methodsFor: 'operations' stamp: 'len 11/24/2022 11:49:44'!
reversed
	| m n newCoefficients |
	true ifTrue: [^ super reversed].
	n _ scalars degree.
	m _ coefficients size.
	newCoefficients _ coefficients species new: m.
	1 to: m-n+1 by: n do: [:i|
		newCoefficients replaceFrom: i to: i+n-1 with: coefficients startingAt: m-n-i+2].
	^ self class coefficients: newCoefficients scalars: scalars! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 5/20/2022 11:47:39'!
findFirstNonzeroFrom: start to: stop
	"Answer the index of the first nonzero entry between start and stop, or nil."
	| n |
	n _ scalars degree.
	^ ((coefficients findFirstNonzeroFrom: start-1*n+1 to: stop*n) ifNil: [^ nil]) - 1 // n + 1! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 5/20/2022 11:48:16'!
findLastNonzeroFrom: start to: stop
	"Answer the index of the last nonzero entry between start and stop, or nil."
	| n |
	n _ scalars degree.
	^ ((coefficients findLastNonzeroFrom: start-1*n+1 to: stop*n) ifNil: [^ nil]) - 1 // n + 1! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 12/24/2022 11:27:39'!
from: start to: stop by: increment dot: anArray startingAt: start2 by: increment2
	| n modulusCoefficients inv tmp1 tmp2 j |
	n _ scalars degree.
	tmp1 _ scalars scalars new: n*2 - 1.
	tmp2 _ scalars scalars zeros: n*2 - 1.
	j _ start2-1*n+1.
	start-1*n+1 to: stop-1*n+1 by: increment*n do: [:i|
		tmp1 replaceFrom: 1 with: coefficients from: i to: i+n-1 convolution: anArray coefficients from: j to: j+n-1.
		tmp2 replaceFrom: 1 to: tmp2 size with: tmp2 startingAt: 1 plus: tmp1 startingAt: 1.
		j _ j + (increment2*n)].
	modulusCoefficients _ scalars modulus coefficients.
	inv _ modulusCoefficients last "inverse".
	self assert: inv isOne. "the modulus should be monic"
	^ scalars coefficients: (tmp2 reduceBy: modulusCoefficients leadingInverse: inv addingQuotientTo: nil)! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 07:43:11'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2
	| n |
	n _ scalars degree.
	coefficients
		replaceFrom: start-1*n+1 to: stop*n
		with: anArray1 coefficients startingAt: start1-1*n+1
		minus: anArray2 coefficients startingAt: start2-1*n+1! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 07:43:07'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2
	| n |
	n _ scalars degree.
	coefficients
		replaceFrom: start-1*n+1 to: stop*n
		with: anArray1 coefficients startingAt: start1-1*n+1
		plus: anArray2 coefficients startingAt: start2-1*n+1! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 12/9/2022 07:01:11'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: anElement
	| n scalarCoefficients modulusCoefficients inv tmp j1 j2 |
	n _ scalars degree.
	modulusCoefficients _ scalars modulus coefficients.
	inv _ modulusCoefficients last "inverse".
	self assert: inv isOne. "the modulus should be monic"
	scalarCoefficients _ anElement coefficients.
	tmp _ scalars scalars new: n*2-1.
	j1 _ start1-1*n+1.
	j2 _ start2-1*n+1.
	start-1*n+1 to: stop-1*n+1 by: n do: [:i|
		self flag: #fixme.
		"convolution + reduction could be combined in a single method for efficiency:"
		tmp replaceFrom: 1 with: anArray2 coefficients from: j2 to: j2+n-1 convolution: scalarCoefficients from: 1 to: n.
		tmp reduceBy: modulusCoefficients leadingInverse: inv addingQuotientTo: nil.
		coefficients replaceFrom: i to: i+n-1 with: anArray1 coefficients startingAt: j1 plus: tmp startingAt: 1.
		j1 _ j1 + n.
		j2 _ j2 + n]! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 07:43:03'!
replaceFrom: start to: stop with: anArray startingAt: srcStart plusScalar: anElement
	| n scalarCoefficients j |
	n _ scalars degree.
	scalarCoefficients _ anElement coefficients.
	j _ srcStart-1*n+1.
	start-1*n+1 to: stop-1*n+1 by: n do: [:i|
		coefficients
			replaceFrom: i to: i+n-1
			with: anArray coefficients startingAt: j
			plus: scalarCoefficients startingAt: 1.
		j _ j + n]! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 12/9/2022 13:47:56'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2
	| n modulusCoefficients inv tmp j1 j2 |
	n _ scalars degree.
	modulusCoefficients _ scalars modulus coefficients.
	inv _ modulusCoefficients last "inverse".
	self assert: inv isOne. "the modulus should be monic"
	tmp _ scalars scalars new: n*2-1.
	j1 _ start1-1*n+1.
	j2 _ start2-1*n+1.
	start-1*n+1 to: stop-1*n+1 by: n do: [:i|
		tmp replaceFrom: 1 with: anArray1 coefficients from: j1 to: j1+n-1 convolution: anArray2 coefficients from: j2 to: j2+n-1.
		tmp reduceBy: modulusCoefficients leadingInverse: inv addingQuotientTo: nil.
		coefficients replaceFrom: i to: i+n-1 with: tmp startingAt: 1.
		j1 _ j1 + n.
		j2 _ j2 + n]! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 12/7/2022 12:06:17'!
replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: anElement
	| n scalarCoefficients modulusCoefficients inv tmp j |
	anElement isInteger ifTrue: [^ super replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: anElement].
	n _ scalars degree.
	modulusCoefficients _ scalars modulus coefficients.
	inv _ modulusCoefficients last "inverse".
	self assert: inv isOne. "the modulus should be monic"
	scalarCoefficients _ anElement coefficients.
	tmp _ scalars scalars new: n*2-1.
	j _ srcStart-1*n+1.
	start-1*n+1 to: stop-1*n+1 by: n do: [:i|
		tmp replaceFrom: 1 with: anArray coefficients from: j to: j+n-1 convolution: scalarCoefficients from: 1 to: n.
		tmp reduceBy: modulusCoefficients leadingInverse: inv addingQuotientTo: nil.
		coefficients replaceFrom: i to: i+n-1 with: tmp startingAt: 1.
		j _ j + n]! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 07:43:44'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart
	| n |
	n _ scalars degree.
	coefficients replaceFrom: start-1*n+1 to: stop*n withNegated: anArray coefficients startingAt: srcStart-1*n+1! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 07:42:51'!
swapFrom: start to: stop with: anArray startingAt: start2
	| n |
	n _ scalars degree.
	coefficients swapFrom: start-1*n+1 to: stop*n with: anArray coefficients startingAt: start2-1*n+1! !

!GaloisTuple methodsFor: 'operations-low level' stamp: 'len 12/7/2022 11:28:55'!
zeroFrom: start to: stop
	| n |
	n _ scalars degree.
	coefficients zeroFrom: start-1*n+1 to: stop*n! !

!GaloisTuple methodsFor: 'testing' stamp: 'len 5/20/2022 11:53:38'!
isZero
	^ coefficients isZero! !

!GaloisTuple methodsFor: 'testing' stamp: 'len 5/20/2022 11:55:02'!
isZeroAt: anInteger
	| n |
	n _ scalars degree.
	^ (coefficients findFirstNonzeroFrom: anInteger-1*n+1 to: anInteger*n) isNil! !

!GaloisTuple methodsFor: 'private' stamp: 'len 4/26/2022 11:05:35'!
coefficients
	^ coefficients! !

!GaloisTuple methodsFor: 'private' stamp: 'len 5/1/2022 20:03:04'!
coefficients: anArray scalars: aGaloisRing
	coefficients _ anArray.
	scalars _ aGaloisRing.
	self assert: scalars degree | coefficients size! !

!GaloisTuple methodsFor: 'private' stamp: 'len 4/26/2022 12:30:30'!
species
	^ scalars! !

!GaloisTuple class methodsFor: 'instance creation' stamp: 'len 4/26/2022 07:00:08'!
coefficients: anArray scalars: aGaloisRing
	^ self basicNew coefficients: anArray scalars: aGaloisRing! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 4/17/2022 11:48:59'!
at: anInteger
	^ scalars representative: (representatives at: anInteger ifAbsent: [^ scalars zero])! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 4/17/2022 11:49:08'!
at: anInteger put: anElement
	anElement isInteger ifTrue: [^ representatives at: anInteger put: anElement \\ scalars modulus].
	representatives at: anInteger put: anElement representative.
	^ anElement! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 4/29/2022 09:59:23'!
atAllPut: aModularInteger
	representatives atAllPut: aModularInteger representative! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 11/24/2022 12:26:28'!
representative
	"Answer a representative integer tuple."
	^ ℤ !!!! representatives! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 4/17/2022 11:55:55'!
scalars
	^ scalars! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 7/26/2020 13:29:14'!
size
	^ representatives size! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 4/24/2022 20:36:32'!
swap: oneIndex with: anotherIndex 
	"Move the element at oneIndex to anotherIndex, and vice-versa."
	representatives swap: oneIndex with: anotherIndex! !

!ModularTuple methodsFor: 'comparing' stamp: 'len 11/24/2022 19:44:01'!
= anObject
	^ self class = anObject class and: [representatives = anObject representatives and: [scalars = anObject scalars]]! !

!ModularTuple methodsFor: 'comparing' stamp: 'len 7/27/2020 08:39:57'!
hash
	| answer |
	answer _ 0.
	representatives do: [:each| each = 0 ifFalse: [answer _ answer hashMultiply + each hash]].
	^ answer! !

!ModularTuple methodsFor: 'copying' stamp: 'len 12/20/2022 09:33:14'!
postCopy
	super postCopy.
	representatives _ representatives copy! !

!ModularTuple methodsFor: 'copying' stamp: 'len 5/1/2022 19:42:47'!
replaceFrom: start to: stop with: replacement startingAt: repStart
	(replacement class = self class and: [scalars = replacement scalars])
		ifFalse: [^ super replaceFrom: start to: stop with: replacement startingAt: repStart].
	representatives replaceFrom: start to: stop with: replacement representatives startingAt: repStart! !

!ModularTuple methodsFor: 'operations' stamp: 'len 12/27/2022 11:04:27'!
· aTuple
	^ scalars representative: (representatives from: 1 to: (self size min: aTuple size) by: 1 dot: aTuple representatives startingAt: 1 by: 1 modulo: scalars modulus)! !

!ModularTuple methodsFor: 'operations' stamp: 'len 12/27/2022 11:10:55'!
polynomialValue: anElement
	^ scalars representative: (representatives polynomialValue: anElement representative modulo: scalars modulus)! !

!ModularTuple methodsFor: 'operations' stamp: 'len 4/20/2022 16:59:47'!
product
	| modulus answer |
	modulus _ scalars modulus.
	answer _ 1.
	representatives do: [:each| answer _ each * answer \\ modulus].
	^ scalars representative: answer! !

!ModularTuple methodsFor: 'operations' stamp: 'len 4/17/2022 12:03:29'!
reversed
	^ self class representatives: representatives reversed scalars: scalars! !

!ModularTuple methodsFor: 'operations' stamp: 'len 11/13/2022 10:39:52'!
sum
	^ scalars project: representatives sum! !

!ModularTuple methodsFor: 'operations' stamp: 'len 12/3/2016 09:13:11'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ representatives count: [:each| each ~~ 0]! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 5/16/2022 20:17:35'!
fillWithMatrixTranspose: anArray width: width
	representatives fillWithMatrixTranspose: anArray representatives width: width! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 5/20/2022 11:43:29'!
findFirstNonzeroFrom: start to: stop
	"Answer the index of the first nonzero entry between start and stop, or nil."
	^ representatives findFirstNonzeroFrom: start to: stop! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 5/20/2022 11:43:49'!
findLastNonzeroFrom: start to: stop
	"Answer the index of the last nonzero entry between start and stop, or nil."
	^ representatives findLastNonzeroFrom: start to: stop! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 12/24/2022 11:28:13'!
from: start to: stop by: increment dot: anArray startingAt: start2 by: increment2
	^ scalars representative: (representatives from: start to: stop by: increment dot: anArray representatives startingAt: start2 by: increment2 modulo: scalars modulus)! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 12/2/2022 14:24:49'!
lift
	"Answer a balanced representative tuple of integers."
	| m h |
	m _ scalars modulus.
	h _ m >> 1.
	^ ℤ new: self size fill: [:i| | r | r _ representatives at: i. r <= h ifTrue: [r] ifFalse: [r - m]]! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 4/29/2022 08:13:39'!
partiallyReduceBy: g addingQuotientTo: q
	representatives
		partiallyReduceBy: g representatives
		addingQuotientTo: (q ifNotNil: [q representatives])
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 4/29/2022 12:41:47'!
reduceBy: g leadingInverse: inv addingQuotientTo: q
	representatives
		reduceBy: g representatives
		leadingInverse: inv representative
		addingQuotientTo: (q ifNotNil: [q representatives])
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 4/25/2022 13:16:02'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2
	representatives
		replaceFrom: start to: stop
		with: anArray1 representatives startingAt: start1
		minus: anArray2 representatives startingAt: start2
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 4/25/2022 13:16:24'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2
	representatives
		replaceFrom: start to: stop
		with: anArray1 representatives startingAt: start1
		plus: anArray2 representatives startingAt: start2
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 4/25/2022 13:16:49'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: aNumber
	representatives
		replaceFrom: start to: stop
		with: anArray1 representatives startingAt: start1
		plus: anArray2 representatives startingAt: start2
		timesScalar: (aNumber isInteger ifTrue: [aNumber] ifFalse: [aNumber representative])
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 5/2/2022 10:19:30'!
replaceFrom: start to: stop with: anArray startingAt: srcStart plusScalar: anElement
	representatives
		replaceFrom: start to: stop
		with: anArray representatives startingAt: srcStart
		plusScalar: (anElement isInteger ifTrue: [anElement] ifFalse: [anElement representative])
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 4/25/2022 13:17:32'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2
	representatives
		replaceFrom: start to: stop
		with: anArray1 representatives startingAt: start1
		times: anArray2 representatives startingAt: start2
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 4/25/2022 13:17:54'!
replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: aNumber
	representatives
		replaceFrom: start to: stop
		with: anArray representatives startingAt: srcStart
		timesScalar: (aNumber isInteger ifTrue: [aNumber] ifFalse: [aNumber representative])
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 12/24/2022 11:50:12'!
replaceFrom: start to: stop withMatrix: anArray height: height width: width transpose: transpose timesTuple: anotherArray startingAt: start2
	representatives
		replaceFrom: start to: stop
		withMatrix: anArray representatives height: height width: width transpose: transpose
		timesTuple: anotherArray representatives startingAt: start2
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 4/25/2022 13:18:09'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart
	representatives
		replaceFrom: start to: stop
		withNegated: anArray representatives startingAt: srcStart
		modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 12/9/2022 07:34:45'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2
	representatives replaceFrom: start with: anArray1 representatives from: start1 to: stop1 convolutionLong: anArray2 representatives from: start2 to: stop2 modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 12/9/2022 07:54:50'!
replaceFrom: start withAutoconvolutionLong: anArray1 from: start1 to: stop1
	representatives replaceFrom: start withAutoconvolutionLong: anArray1 representatives from: start1 to: stop1 modulo: scalars modulus! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 12/10/2022 12:25:01'!
sumFrom: start to: stop by: increment
	^ scalars representative: (representatives sumFrom: start to: stop by: increment modulo: scalars modulus)! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 5/1/2022 07:34:01'!
swapFrom: start to: stop with: anArray startingAt: start2
	representatives swapFrom: start to: stop with: anArray representatives startingAt: start2! !

!ModularTuple methodsFor: 'operations-low level' stamp: 'len 12/2/2022 11:47:10'!
zeroFrom: start to: stop
	representatives from: start to: stop put: 0! !

!ModularTuple methodsFor: 'printing' stamp: 'len 12/6/2022 12:26:34'!
printOn: aStream
	self modulus <= 16 ifFalse: [^ super printOn: aStream].
	representatives do: [:each| each printOn: aStream base: 16]! !

!ModularTuple methodsFor: 'testing' stamp: 'len 5/20/2022 11:56:59'!
isZero
	^ representatives isZero! !

!ModularTuple methodsFor: 'testing' stamp: 'len 5/20/2022 11:57:23'!
isZeroAt: anInteger
	^ representatives isZeroAt: anInteger! !

!ModularTuple methodsFor: 'private' stamp: 'len 5/28/2020 13:23:21'!
modulus
	^ self scalars modulus! !

!ModularTuple methodsFor: 'private' stamp: 'len 3/31/2016 07:43'!
representatives
	^ representatives! !

!ModularTuple methodsFor: 'private' stamp: 'len 4/17/2022 11:55:33'!
representatives: anArray scalars: aModularIntegerRing
	representatives _ anArray.
	scalars _ aModularIntegerRing
"	representatives do: [:each| (each between: 0 and: modulus) ifFalse: [self halt]]"! !

!ModularTuple methodsFor: 'private' stamp: 'len 4/26/2022 12:31:51'!
species
	^ scalars! !

!ModularTuple class methodsFor: 'instance creation' stamp: 'len 4/17/2022 11:54:22'!
representatives: anArray scalars: aModularIntegerRing
	^ self basicNew representatives: anArray scalars: aModularIntegerRing! !

!ProductTuple methodsFor: 'accessing' stamp: 'len 11/20/2022 12:24:44'!
at: anInteger
	^ scalars compose: (components collect: [:each| each at: anInteger])! !

!ProductTuple methodsFor: 'accessing' stamp: 'len 11/21/2022 16:26:17'!
at: anInteger put: anElement
	components withIndexDo: [:each :i| each at: anInteger put: (anElement at: i)].
	^ anElement! !

!ProductTuple methodsFor: 'accessing' stamp: 'len 11/22/2022 16:06:58'!
atAllPut: anElement
	components withIndexDo: [:each :i| each atAllPut: (anElement at: i)]! !

!ProductTuple methodsFor: 'accessing' stamp: 'len 11/20/2022 12:21:56'!
projection: anInteger
	^ components at: anInteger! !

!ProductTuple methodsFor: 'accessing' stamp: 'len 11/21/2022 16:18:42'!
scalars
	^ scalars! !

!ProductTuple methodsFor: 'accessing' stamp: 'len 11/20/2022 12:24:16'!
size
	^ components first size! !

!ProductTuple methodsFor: 'accessing' stamp: 'len 11/22/2022 16:08:53'!
swap: oneIndex with: anotherIndex 
	"Move the element at oneIndex to anotherIndex, and vice-versa."
	components do: [:each| each swap: oneIndex with: anotherIndex]! !

!ProductTuple methodsFor: 'comparing' stamp: 'len 11/24/2022 19:44:43'!
= anObject
	^ self class = anObject class and: [components = anObject components and: [scalars = anObject scalars]]! !

!ProductTuple methodsFor: 'comparing' stamp: 'len 11/21/2022 16:21:57'!
hash
	^ components hash! !

!ProductTuple methodsFor: 'copying' stamp: 'len 12/20/2022 09:33:17'!
postCopy
	super postCopy.
	components _ components collect: [:each| each copy]! !

!ProductTuple methodsFor: 'operations' stamp: 'len 12/7/2022 19:21:47'!
· aTuple
	^ scalars compose: (components withIndexCollect: [:each :i| each · (aTuple projection: i)])! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 15:16:01'!
fillWithMatrixTranspose: anArray width: width
	components withIndexDo: [:each :i | each fillWithMatrixTranspose: (anArray projection: i) width: width]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 15:18:51'!
findFirstNonzeroFrom: start to: stop
	| answer |
	answer _ nil.
	components do: [:each| (each findFirstNonzeroFrom: start to: stop) ifNotNil: [:index| answer _ answer isNil ifTrue: [index] ifFalse: [answer min: index]]].
	^ answer! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 15:19:51'!
findLastNonzeroFrom: start to: stop
	| answer |
	answer _ nil.
	components do: [:each| (each findLastNonzeroFrom: start to: stop) ifNotNil: [:index| answer _ answer isNil ifTrue: [index] ifFalse: [answer max: index]]].
	^ answer! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 12/24/2022 11:24:47'!
from: start to: stop by: increment dot: anArray startingAt: start2 by: increment2
	^ scalars compose: (components withIndexCollect: [:each :i| each from: start to: stop by: increment dot: (anArray projection: i) startingAt: start2 by: increment2])! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:36:07'!
partiallyReduceBy: g addingQuotientTo: q
	components withIndexDo: [:each :i|
		each partiallyReduceBy: (g projection: i) addingQuotientTo: (q ifNotNil: [q projection: i])]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:37:15'!
reduceBy: g leadingInverse: inv addingQuotientTo: q
	components withIndexDo: [:each :i|
		each
			reduceBy: (g projection: i)
			leadingInverse: (inv at: i)
			addingQuotientTo: (q ifNotNil: [q projection: i])]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:38:03'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2
	components withIndexDo: [:each :i|
		each
			replaceFrom: start to: stop
			with: (anArray1 projection: i) startingAt: start1
			minus: (anArray2 projection: i) startingAt: start2]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:38:24'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2
	components withIndexDo: [:each :i|
		each
			replaceFrom: start to: stop
			with: (anArray1 projection: i) startingAt: start1
			plus: (anArray2 projection: i) startingAt: start2]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:39:20'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: anElement
	components withIndexDo: [:each :i|
		each
			replaceFrom: start to: stop
			with: (anArray1 projection: i) startingAt: start1
			plus: (anArray2 projection: i) startingAt: start2
			timesScalar: (anElement at: i)]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:40:35'!
replaceFrom: start to: stop with: anArray startingAt: srcStart plusScalar: anElement
	components withIndexDo: [:each :i|
		each
			replaceFrom: start to: stop
			with: (anArray projection: i) startingAt: srcStart
			plusScalar: (anElement at: i)]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:41:13'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2
	components withIndexDo: [:each :i|
		each
			replaceFrom: start to: stop
			with: (anArray1 projection: i) startingAt: start1
			times: (anArray2 projection: i) startingAt: start2]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:41:40'!
replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: anElement
	components withIndexDo: [:each :i|
		each
			replaceFrom: start to: stop
			with: (anArray projection: i) startingAt: srcStart
			timesScalar: (anElement at: i)]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 12/24/2022 11:51:01'!
replaceFrom: start to: stop withMatrix: anArray height: height width: width transpose: transpose timesTuple: anotherArray startingAt: start2
	components withIndexDo: [:each :i|
		each
			replaceFrom: start to: stop
			withMatrix: (anArray projection: i) height: height width: width transpose: transpose
			timesTuple: (anotherArray projection: i) startingAt: start2]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:43:40'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart
	components withIndexDo: [:each :i|
		each
			replaceFrom: start to: stop
			withNegated: (anArray projection: i) startingAt: srcStart]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 12/7/2022 11:00:57'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolution: anArray2 from: start2 to: stop2
	components withIndexDo: [:each :i|
		each replaceFrom: start with: (anArray1 projection: i) from: start1 to: stop1 convolution: (anArray2 projection: i) from: start2 to: stop2]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 12/7/2022 12:02:03'!
replaceFrom: start withAutoconvolution: anArray1 from: start1 to: stop1
	components withIndexDo: [:each :i|
		each replaceFrom: start withAutoconvolution: (anArray1 projection: i) from: start1 to: stop1]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 12/10/2022 12:31:41'!
sumFrom: start to: stop by: increment
	^ scalars compose: (components withIndexCollect: [:each :i| each sumFrom: start to: stop by: increment])! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 11/22/2022 16:44:38'!
swapFrom: start to: stop with: anArray startingAt: start2
	components withIndexDo: [:each :i|
		each swapFrom: start to: stop with: (anArray projection: i) startingAt: start2]! !

!ProductTuple methodsFor: 'operations-low level' stamp: 'len 12/7/2022 11:27:54'!
zeroFrom: start to: stop
	components do: [:each| each zeroFrom: start to: stop]! !

!ProductTuple methodsFor: 'testing' stamp: 'len 11/22/2022 16:31:05'!
isZero
	^ components allSatisfy: [:each| each isZero]! !

!ProductTuple methodsFor: 'testing' stamp: 'len 11/22/2022 16:31:40'!
isZeroAt: anInteger
	^ components allSatisfy: [:each| (each isZeroAt: anInteger)]! !

!ProductTuple methodsFor: 'private' stamp: 'len 11/21/2022 16:22:29'!
components
	^ components! !

!ProductTuple methodsFor: 'private' stamp: 'len 11/20/2022 12:20:45'!
components: anArray scalars: aProductRing
	components _ anArray.
	scalars _ aProductRing! !

!ProductTuple methodsFor: 'private' stamp: 'len 11/21/2022 16:18:36'!
species
	^ scalars! !

!ProductTuple class methodsFor: 'as yet unclassified' stamp: 'len 11/20/2022 12:21:40'!
components: anArray scalars: aProductRing
	^ self basicNew components: anArray scalars: aProductRing! !

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

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/15/2022 11:43:09'!
isAdditiveGroup
	^ true! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:58:39'!
printOn: aStream
	aStream nextPutAll: 'Div'.
	scalars = ℤ ifFalse: [aStream nextPutAll: scalars printString sub].
	aStream nextPut: $(; print: space; nextPut: $)! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:57:29'!
scalars
	^ scalars! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:57:21'!
space
	^ space! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 12:00:07'!
space: aScheme scalars: aRing
	scalars _ aRing.
	space _ aScheme! !

!DivisorGroup class methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:59:50'!
on: aScheme
	^ self on: aScheme over: ℤ! !

!DivisorGroup class methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:59:31'!
on: aScheme over: aRing
	^ self new space: aScheme scalars: aRing! !

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

!PowerAlgebra methodsFor: 'elements' stamp: 'len 5/7/2023 17:23:07'!
compose: anArray
	^ RingTuple components: self scalars !!!! anArray parent: self! !

!PowerAlgebra methodsFor: 'elements' stamp: 'len 5/7/2023 17:23:07'!
fromModuleElement: aTuple
	^ RingTuple components: aTuple parent: self! !

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

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 11:56:17'!
base
	^ self completionIdeal scalars! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 11:11:18'!
completionIdeal
	^ self propertyAt: #completionIdeal! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 18:52:55'!
dimension
	"Answer the Krull dimension of the receiver."
	(self base isNoetherian and: [self base isLocal])
		ifTrue: [^ self base dimension].
	^ super dimension! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 10:56:36'!
maximalIdeal
	"Answer the unique maximal ideal in the receiver."
	^ self * (self completionIdeal generators collect: [:each| self embed: each])! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 16:52:57'!
one
	^ self embed: self base one! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 16:53:02'!
zero
	^ self embed: self base zero! !

!CompletionRing methodsFor: 'as yet unclassified' stamp: 'len 6/22/2019 03:13:21'!
> aRing
	self base > aRing ifTrue: [^ true].
	^ super > aRing! !

!CompletionRing methodsFor: 'as yet unclassified' stamp: 'len 6/22/2019 03:14:37'!
>= aRing
	super >= aRing ifTrue: [^ true].
	self base >= aRing ifTrue: [^ true].
	^ false! !

!CompletionRing methodsFor: 'as yet unclassified' stamp: 'len 6/6/2019 18:58:05'!
hilbertSeriesAt: t
	"Answer the Hilbert series of the receiver."
	(self base isNoetherian and: [self base isLocal]) ifTrue: [^ self base hilbertSeriesAt: t].
	^ super hilbertSeriesAt: t! !

!CompletionRing methodsFor: 'comparing' stamp: 'len 6/6/2019 17:06:14'!
= anObject
	self == anObject ifTrue: [^ true].
	^ (anObject isKindOf: CompletionRing) and: [anObject completionIdeal = self completionIdeal]! !

!CompletionRing methodsFor: 'comparing' stamp: 'len 6/6/2019 17:06:28'!
hash
	^ self completionIdeal hash + self base hash! !

!CompletionRing methodsFor: 'morphisms' stamp: 'len 10/6/2022 11:38:03'!
from: aDomain
	aDomain = self base ifTrue: [^ self addMap: ((self base into: self map: [:x| self embed: x]) name: 'ι')].
	^ super from: aDomain! !

!CompletionRing methodsFor: 'operations' stamp: 'len 6/6/2019 11:59:39'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the quotient ring isomorphic to the anInteger-th truncation of the receiver."
	^ self base / (self completionIdeal ^ anInteger)! !

!CompletionRing methodsFor: 'testing' stamp: 'len 6/22/2019 03:11:07'!
isCompletion
	^ true! !

!CompletionRing methodsFor: 'testing' stamp: 'len 6/6/2019 10:55:42'!
isLocal
	^ true! !

!CompletionRing methodsFor: 'private' stamp: 'len 6/6/2019 11:11:28'!
completionIdeal: anIdeal
	self propertyAt: #completionIdeal put: anIdeal! !

!CompletionRing class methodsFor: 'instance creation' stamp: 'len 6/6/2019 12:04:12'!
at: anIdeal
	^ self new completionIdeal: anIdeal! !

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

!Divisor methodsFor: 'accessing' stamp: 'len 7/18/2022 06:23:40'!
degree
	coefficients isEmpty ifTrue: [^ self scalars zero].
	^ coefficients sum! !

!Divisor methodsFor: 'accessing' stamp: 'len 7/18/2022 06:22:38'!
scalars
	^ parent scalars! !

!Divisor methodsFor: 'accessing' stamp: 'len 8/12/2022 13:18:46'!
support
	| answer |
	answer _ nil. "should start from the empty variety"
	coefficients keysDo: [:each| answer _ answer ifNil: [each] ifNotNil: [answer ∪ each]].
	^ answer! !

!Divisor methodsFor: 'comparing' stamp: 'len 1/11/2017 16:34:02'!
<= aDivisor
	^ (aDivisor - self) isEffective! !

!Divisor methodsFor: 'comparing' stamp: 'len 1/2/2017 15:43:41'!
= anObject
	^ (anObject isKindOf: Divisor) and: [self ~ anObject]! !

!Divisor methodsFor: 'comparing' stamp: 'len 10/27/2016 10:26'!
>= aDivisor
	^ (self - aDivisor) isEffective! !

!Divisor methodsFor: 'comparing' stamp: 'len 1/2/2017 15:43:29'!
~ aDivisor
	"Answer true if the receiver and the argument are linearly equivalent."
	^ (self - aDivisor) isPrincipal! !

!Divisor methodsFor: 'comparing' stamp: 'len 7/25/2022 11:56:06'!
hash
	^ 0! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:22:17'!
* anElement
	(anElement isInteger or: [self scalars includes: anElement])
		ifTrue: [^ self class coefficients: coefficients * anElement parent: parent].
	^ self error: 'not a scalar'! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:21:07'!
+ aDivisor
	(parent includes: aDivisor) ifFalse: [^ self adapt: aDivisor andSend: #+].
	^ self class coefficients: coefficients + aDivisor coefficients parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:21:26'!
- aDivisor
	(parent includes: aDivisor) ifFalse: [^ self adapt: aDivisor andSend: #-].
	^ self class coefficients: coefficients - aDivisor coefficients parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 8/21/2022 08:15:14'!
gcd: aDivisor
	| newCoefficients |
	newCoefficients _ SortedDictionary new.
	coefficients keysAndValuesDo: [:key :value|
		aDivisor coefficients at: key ifPresent: [:value2| newCoefficients at: key put: (value min: value2)]].
	^ self class coefficients: newCoefficients parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:19:06'!
lcm: aDivisor
	| newCoefficients |
	newCoefficients _ coefficients copy.
	aDivisor coefficients keysAndValuesDo: [:key :value|
		newCoefficients at: key put: ((coefficients at: key) max: value)].
	^ self class coefficients: newCoefficients parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:21:39'!
negated
	^ self class coefficients: coefficients negated parent: parent! !

!Divisor methodsFor: 'testing' stamp: 'len 2/25/2017 09:09:25'!
isEffective
	^ self allSatisfy: [:each| each positive]! !

!Divisor methodsFor: 'private' stamp: 'len 7/18/2022 06:20:16'!
coefficients: aDictionary parent: aDivisorGroup
	coefficients _ aDictionary.
	parent _ aDivisorGroup! !

!Divisor class methodsFor: 'instance creation' stamp: 'len 7/18/2022 06:19:38'!
coefficients: aDictionary parent: aDivisorGroup
	^ self new coefficients: aDictionary parent: aDivisorGroup! !

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

!Sum methodsFor: 'accessing' stamp: 'len 1/10/2018 21:04:34'!
at: anObject
	^ coefficients at: anObject ifAbsent: [self scalars zero]! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 10:02:25'!
at: key add: value
	^ coefficients at: key addAndRemoveIfZero: value! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 17:29:29'!
at: anObject ifAbsent: aBlock
	^ coefficients at: anObject ifAbsent: aBlock! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 17:29:44'!
at: anObject ifPresent: aBlock
	^ coefficients at: anObject ifPresent: aBlock! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 17:27:44'!
at: key put: value
	value isZero ifTrue: [coefficients removeKey: key ifAbsent: []. ^ value].
	^ coefficients at: key put: value! !

!Sum methodsFor: 'accessing' stamp: 'len 7/22/2020 09:33:46'!
bitSize
	| answer |
	answer _ 0.
	coefficients withSupportDo: [:value :key| answer _ answer + key bitSize + value bitSize].
	^ answer! !

!Sum methodsFor: 'accessing' stamp: 'len 11/20/2022 11:33:44'!
denominator
	| answer |
	self scalars isLocalization ifFalse: [^ self scalars one].
	answer _ self scalars numerators one.
	self do: [:each| answer _ each denominator lcm: answer].
	^ answer! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 20:33:21'!
keys
	^ coefficients keys! !

!Sum methodsFor: 'accessing' stamp: 'len 11/20/2022 11:33:48'!
numerator
	self scalars isLocalization ifFalse: [^ self].
	^ self * self denominator over: self scalars numerators! !

!Sum methodsFor: 'accessing' stamp: 'len 7/21/2020 17:39:14'!
parent
	^ parent! !

!Sum methodsFor: 'accessing' stamp: 'len 4/23/2018 20:40:20'!
representative
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ self apply: self scalars projection section! !

!Sum methodsFor: 'accessing' stamp: 'len 1/10/2018 19:57:14'!
scalars
	^ parent scalars! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 09:55:05'!
size
	^ coefficients size! !

!Sum methodsFor: 'accessing' stamp: 'len 1/10/2018 21:09:50'!
zero
	^ parent zero! !

!Sum methodsFor: 'comparing' stamp: 'len 7/23/2020 08:15:05'!
= aSum
	^ coefficients = aSum coefficients! !

!Sum methodsFor: 'comparing' stamp: 'len 11/18/2016 20:11:39'!
hash
	^ coefficients hash! !

!Sum methodsFor: 'converting' stamp: 'len 1/10/2018 21:09:44'!
over: aRing
	| newCoefficients |
	self scalars = aRing ifTrue: [^ self].
	newCoefficients _ Dictionary new: self size.
	self associationsDo: [:each| | newValue |
		(newValue _ aRing !! each value) isZero
			ifFalse: [newCoefficients add: (each copy value: newValue)]].
	^ (self class coefficients: newCoefficients) parent: (parent over: aRing)! !

!Sum methodsFor: 'copying' stamp: 'len 11/18/2016 17:34:33'!
postCopy
	coefficients _ coefficients copy! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 09:54:31'!
associationsDo: aBlock
	coefficients associationsDo: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 1/10/2018 21:43:05'!
coefficientsDo: aBlock
	coefficients do: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 1/10/2018 21:22:05'!
collect: aBlock 
	| newCoefficients |
	newCoefficients _ Dictionary new: self size.
	self associationsDo: [:each| | newValue |
		(newValue _ aBlock value: each value) isZero ifFalse: [newCoefficients at: each key put: newValue]].
	^ (self class coefficients: newCoefficients) parent: parent! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 09:54:10'!
collectNoZero: aBlock
	"The result of aBlock must never be zero."
	^ self copy associationsDo: [:each| each value: (aBlock value: each value)]! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 20:24:16'!
do: aBlock
	coefficients do: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 17:45:00'!
keysAndValuesDo: aBlock
	coefficients keysAndValuesDo: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 17:45:38'!
keysDo: aBlock
	coefficients keysDo: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 7/22/2020 09:32:43'!
supportDo: aBlock
	coefficients keysDo: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 7/24/2020 12:14:38'!
withIndexDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	self withSupportDo: elementAndIndexBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 7/24/2020 12:14:26'!
withSupportDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	coefficients keysAndValuesDo: [:key :value| elementAndIndexBlock value: value value: key]! !

!Sum methodsFor: 'operations' stamp: 'len 11/18/2016 09:51:39'!
* aNumber
	^ self collect: [:each| each*aNumber]! !

!Sum methodsFor: 'operations' stamp: 'len 5/6/2019 02:56:21'!
+ aSum
	| other answer |
	self size <= aSum size
		ifTrue: [answer _ aSum copy. other _ self]
		ifFalse: [answer _ self copy. other _ aSum].
	other associationsDo: [:each| answer at: each key add: each value].
	^ answer! !

!Sum methodsFor: 'operations' stamp: 'len 5/6/2019 02:56:07'!
- aSum
	| answer |
	self size <= aSum size
		ifTrue:
			[answer _ aSum negated.
			self associationsDo: [:each| answer at: each key add: each value]]
		ifFalse:
			[answer _ self copy.
			aSum associationsDo: [:each| answer at: each key add: each value negated]].
	^ answer! !

!Sum methodsFor: 'operations' stamp: 'len 2/6/2017 03:13:39'!
/ aNumber
	^ self collect: [:each| each / aNumber]! !

!Sum methodsFor: 'operations' stamp: 'len 5/14/2020 12:31:41'!
// anElement
	^ self collect: [:each| each // anElement]! !

!Sum methodsFor: 'operations' stamp: 'len 5/14/2020 12:31:08'!
\\ anElement
	^ self collect: [:each| each \\ anElement]! !

!Sum methodsFor: 'operations' stamp: 'len 5/6/2019 02:55:32'!
· aSum
	| answer |
	self size <= aSum size ifFalse: [^ aSum · self].
	answer _ self scalars zero.
	coefficients associationsDo: [:each|
		aSum at: each key ifPresent: [:value2| answer _ answer + (each value*value2)]].
	^ answer! !

!Sum methodsFor: 'operations' stamp: 'len 7/18/2020 05:17:14'!
convolution: aSum
	| newCoefficients |
	newCoefficients _ Dictionary new: self size + aSum size.
	self associationsDo: [:each|
		aSum associationsDo: [:other|
			newCoefficients at: each key * other key addAndRemoveIfZero: each value * other value]].
	^ (self class coefficients: newCoefficients) parent: parent! !

!Sum methodsFor: 'operations' stamp: 'len 5/6/2019 02:55:06'!
hadamard: aSum
	"Answer the pointwise product with the argument, also called Hadamard product."
	| answer |
	self size <= aSum size ifFalse: [^ aSum hadamard: self]. "assuming it's commutative"
	answer _ self copyEmpty.
	coefficients associationsDo: [:each| | key |
		key _ each key.
		answer at: key put: each value * (aSum at: key)].
	^ answer! !

!Sum methodsFor: 'operations' stamp: 'len 11/18/2016 09:57:04'!
negated
	^ self collectNoZero: [:each| each negated]! !

!Sum methodsFor: 'printing' stamp: 'len 2/25/2017 09:43:39'!
printCoefficient: coefficient on: aStream
	| c |
	((c _ coefficient) printString first = $- "or: [coefficient isNumber and: [coefficient negative]]")
		ifTrue: [aStream nextPut: $-. c _ c negated].
	((c isKindOf: Polynomial) and: [c isMonomial])
		ifTrue: [aStream print: c. ^ self].
	(c printString allSatisfy: [:each| each isAlphaNumeric or: ['{}()/' includes: each]])
		ifTrue: [aStream print: c. ^ self].
	aStream nextPut: $(; print: c; nextPut: $)! !

!Sum methodsFor: 'printing' stamp: 'len 2/28/2017 08:43:47'!
printLiteral: anObject on: aStream
	((anObject isKindOf: Word) and: [anObject isEmpty])
		ifTrue: [^ self].
	anObject isInteger
		ifTrue:
			[aStream isText
				ifTrue: [aStream nextPut: $x; nextPutAll: anObject printString sub]
				ifFalse: [aStream nextPutAll: '{x', anObject printString, '}'].
			^ self].
	(aStream isText and: [anObject isString])
			ifTrue: [aStream nextPutAll: anObject asText]
			ifFalse: [aStream print: anObject]! !

!Sum methodsFor: 'printing' stamp: 'len 2/25/2017 08:38:18'!
printOn2: aStream
	aStream nextPut: $(.
	(Iterator on: self performing: #associationsDo:)
		do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)! !

!Sum methodsFor: 'printing' stamp: 'len 1/10/2018 21:04:41'!
printOn: aStream
	| elements c |
	self isZero ifTrue: [aStream nextPutAll: '0'. ^ self].
	elements _ self keys asSortedCollection: [:a :b| a printString <= b printString].
	c _ self at: elements first.
	c printString first = $-
		ifTrue: [c _ c negated. aStream nextPut: $-].
	c ~= self scalars one ifTrue: [self printCoefficient: c on: aStream].
	self printLiteral: elements first on: aStream.
	elements allButFirst do: [:each|
		c _ self at: each.
		c printString first = $-
			ifTrue: [aStream nextPutAll: ' - '. c _ c negated]
			ifFalse: [aStream nextPutAll: ' + '].
		c ~= c one ifTrue: [self printCoefficient: c on: aStream].
		self printLiteral: each on: aStream]! !

!Sum methodsFor: 'testing' stamp: 'len 11/18/2016 09:56:53'!
isZero
	^ coefficients isEmpty! !

!Sum methodsFor: 'private' stamp: 'len 11/18/2016 20:11:53'!
coefficients
	^ coefficients! !

!Sum methodsFor: 'private' stamp: 'len 9/16/2018 16:12:06'!
coefficients: aDictionary
	coefficients _ aDictionary.
	coefficients associationsDo: [:each| each value isZero ifTrue: [self error: 'the coefficinets dictionary contains zeroes']]! !

!Sum methodsFor: 'private' stamp: 'len 7/21/2020 17:39:28'!
parent: aFreeModule
	parent _ aFreeModule! !

!Sum class methodsFor: 'instance creation' stamp: 'len 1/10/2018 19:58:43'!
coefficients: aDictionary
	^ self basicNew coefficients: aDictionary! !

!Sum class methodsFor: 'instance creation' stamp: 'len 7/23/2020 08:13:42'!
coefficients: aDictionary parent: aFreeModule
	^ self basicNew coefficients: aDictionary; parent: aFreeModule! !

!Sum class methodsFor: 'instance creation' stamp: 'len 9/16/2018 16:12:13'!
new
	^ self coefficients: Dictionary new! !

!Sum class methodsFor: 'instance creation' stamp: 'len 1/10/2018 19:58:56'!
new: anInteger
	^ self coefficients: (Dictionary new: anInteger)! !

!Sum class methodsFor: 'instance creation' stamp: 'len 7/23/2020 08:14:37'!
new: anInteger parent: aFreeModule
	^ self coefficients: (Dictionary new: anInteger) parent: aFreeModule! !

!Sum class methodsFor: 'instance creation' stamp: 'len 7/23/2020 08:14:21'!
parent: aFreeModule
	^ self coefficients: Dictionary new parent: aFreeModule! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 5/10/2020 19:24:35'!
associates
	^ self scalars units collect: [:each| self * each]! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 4/20/2022 18:27:33'!
at: anInteger
	anInteger isInteger ifFalse: [^ (coefficients at: anInteger position) at: anInteger monomial].
	^ coefficients at: anInteger! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 4/20/2022 18:27:40'!
at: anInteger ifAbsent: aBlock
	anInteger isInteger ifFalse: [^ (coefficients at: anInteger position ifAbsent: [^aBlock value]) at: anInteger monomial ifAbsent: aBlock].
	^ coefficients at: anInteger ifAbsent: aBlock! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 3/29/2016 18:54'!
bitSize
	| answer |
	answer _ 0.
	self do: [:each| answer _ answer + each bitSize].
	^ answer! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 11/20/2022 11:33:33'!
denominator
	| answer |
	self scalars isLocalization ifFalse: [^ self scalars one].
	answer _ self scalars numerators one.
	self do: [:each| answer _ each denominator lcm: answer].
	^ answer! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 11/5/2016 09:26'!
first
	^ self at: 1! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 4/26/2022 05:58:10'!
firstNonzeroIndex
	"Answer the index of the first nonzero entry, or nil."
	^ coefficients firstNonzeroIndex! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 11/6/2016 04:32'!
last
	^ self at: self size! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 5/19/2022 05:53:36'!
lastNonzeroIndex
	"Answer the index of the last nonzero entry, or nil."
	^ coefficients lastNonzeroIndex! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 11/23/2022 21:31:06'!
lift
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ parent lift fill: [:i| (self at: i) lift]! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 11/20/2022 11:33:39'!
numerator
	| d |
	self scalars isLocalization ifFalse: [^ self].
	d _ self scalars !! self denominator.
	^ self scalars numerators ^ self size filling: [:i| ((self at: i) * d) numerator]! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 5/28/2020 13:09:52'!
parent
	^ parent! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 5/28/2020 13:09:44'!
scalars
	"Answer the ring of scalars (or ring of coefficients) of the receiver."
	^ parent scalars! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 5/16/2020 12:27:17'!
second
	^ self at: 2! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 4/20/2022 18:30:18'!
size
	^ coefficients size

"or:
	^ parent size"! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 3/29/2016 19:06'!
x
	^ self at: 1! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 3/29/2016 19:06'!
y
	^ self at: 2! !

!TupleOlder methodsFor: 'accessing' stamp: 'len 3/29/2016 19:06'!
z
	^ self at: 3! !

!TupleOlder methodsFor: 'comparing' stamp: 'len 5/19/2022 05:50:52'!
= anObject
	| n otherCoefficients |
	self class = anObject class ifFalse: [^ false].
"	scalars = anObject scalars ifFalse: [^ false]."
	otherCoefficients _ anObject coefficients.
	(n _ coefficients size) = otherCoefficients size ifFalse:
		[(n _ self lastNonzeroIndex) = anObject lastNonzeroIndex ifFalse: [^ false]].
	n ifNotNil: [1 to: n do: [:i| (coefficients at: i) = (otherCoefficients at: i) ifFalse: [^ false]]].
	^ true! !

!TupleOlder methodsFor: 'comparing' stamp: 'len 5/24/2022 06:04:10'!
~ aTuple
	"Answer true if the receiver and the argument are equivalent as homogeneous tuples."
	self scalars isIntegralDomain ifTrue:
		[| depth x y |
		(depth _ self lastNonzeroIndex) ~= aTuple lastNonzeroIndex ifTrue: [^ false].
		depth isNil ifTrue: [^ true].
		x _ self at: depth.
		y _ aTuple at: depth.
		1 to: depth do: [:i| (self at: i) * y = ((aTuple at: i) * x) ifFalse: [^ false]].
		^ true].
	"Over a PIR we compare use their Howell normal form:"
"	self scalars isPIR ifTrue: [^ self asRowMatrix echelon = aTuple asRowMatrix echelon]."
	"In a general ring we have to check all units:"
	^ self scalars units anySatisfy: [:any| self * any = aTuple]! !

!TupleOlder methodsFor: 'comparing' stamp: 'len 5/22/2022 20:09:29'!
⊥ aTuple
	"Answer true if the receiver and the argument are orthogonal."
	^ (self · aTuple) isZero! !

!TupleOlder methodsFor: 'comparing' stamp: 'len 4/20/2022 18:29:33'!
hash
	| answer |
	answer _ 0.
	coefficients do: [:each| each isZero ifFalse: [answer _ answer hashMultiply + each hash]].
	^ answer! !

!TupleOlder methodsFor: 'converting' stamp: 'len 11/8/2016 07:19'!
adaptToCollection: aCollection andSend: selector
	self size = aCollection size ifFalse: [^ self error: 'sizes don''t match'].
	^ aCollection withIndexCollect: [:each :index| each perform: selector with: (self at: index)]! !

!TupleOlder methodsFor: 'converting' stamp: 'len 4/20/2022 18:27:20'!
asArray
	^ coefficients asArray! !

!TupleOlder methodsFor: 'converting' stamp: 'len 7/7/2020 09:46:27'!
asMatrix
	"Tuples are equivalent to columns."
	^ Matrix columns: {self}! !

!TupleOlder methodsFor: 'converting' stamp: 'len 3/13/2018 18:48:37'!
asRowMatrix
	^ Matrix rows: {self}! !

!TupleOlder methodsFor: 'converting' stamp: 'len 11/7/2016 13:48'!
asTuple
	^ self! !

!TupleOlder methodsFor: 'converting' stamp: 'len 5/29/2020 08:49:26'!
over: aRing
	self scalars = aRing ifFalse: [^ (self parent over: aRing) elementAt: self]! !

!TupleOlder methodsFor: 'converting' stamp: 'len 12/13/2016 16:49:22'!
readStream
	^ ReadStream on: self! !

!TupleOlder methodsFor: 'copying' stamp: 'len 4/20/2022 18:25:42'!
, anObject
	anObject isTuple
		ifTrue: [^ self scalars^(self size + anObject size) elementAt: coefficients, anObject coefficients].
	anObject isArray
		ifTrue: [^ self scalars^(self size + anObject size) elementAt: coefficients asArray, anObject asArray].
	^ self copyWith: anObject! !

!TupleOlder methodsFor: 'copying' stamp: 'len 5/30/2020 14:29:08'!
coproject: aList
	| answer |
	answer _ self zero copy.
	aList do: [:i| answer at: i put: (self at: i)].
	^ answer! !

!TupleOlder methodsFor: 'copying' stamp: 'len 4/20/2022 18:28:33'!
copyFrom: start to: stop
	^ self class coefficients: (coefficients copyFrom: start to: stop) parent: self scalars^(stop-start+1)! !

!TupleOlder methodsFor: 'copying' stamp: 'len 4/20/2022 18:28:43'!
copyWith: anElement
	^ self class coefficients: (coefficients copyWith: self scalars !! anElement) parent: self scalars^(self size + 1)! !

!TupleOlder methodsFor: 'copying' stamp: 'len 11/22/2022 15:51:10'!
postCopy
	coefficients _ coefficients copy! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:27:15'!
allSatisfy: aBlock
	^ coefficients allSatisfy: aBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:27:18'!
anySatisfy: aBlock
	^ coefficients anySatisfy: aBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:28:01'!
collect: aBlock
	^ self class coefficients: (coefficients collect: aBlock) parent: parent! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 5/1/2022 07:51:22'!
count: aBlock
	^ coefficients count: aBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:28:50'!
detect: aBlock ifNone: exceptionBlock 
	^ coefficients detect: aBlock ifNone: exceptionBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:28:57'!
do: aBlock
	coefficients do: aBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 11/5/2016 09:26'!
do: elementBlock separatedBy: separatorBlock
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst _ true.
	self do: [ :element |
		beforeFirst
			ifTrue: [beforeFirst _ false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:29:04'!
findFirst: aBlock
	^ coefficients findFirst: aBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:29:13'!
findLast: aBlock
	^ coefficients findLast: aBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 11/5/2016 06:48'!
inject: thisValue into: binaryBlock 
	| nextValue |
	nextValue _ thisValue.
	self do: [:each | nextValue _ binaryBlock value: nextValue value: each].
	^ nextValue! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 1/13/2022 09:27:25'!
max: aBlock
	"Answer the maximum of evaluating aBlock on all the coefficients of the receiver."
	| answer |
	self isEmpty ifTrue: [^ self error: 'empty tuple'].
	self do: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer max: (aBlock value: each)]].
	^ answer! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:30:05'!
product
	self isEmpty ifTrue: [^ self scalars one].
	^ coefficients product! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 12/13/2016 09:55:53'!
product: aBlock
	^ self inject: self scalars one into: [:nextValue :each| nextValue * (aBlock value: each)]! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:30:08'!
reverseDo: aBlock
	coefficients reverseDo: aBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:30:23'!
sum
	self isEmpty ifTrue: [^ self scalars zero].
	^ coefficients sum! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 12/13/2016 09:55:29'!
sum: aBlock
	^ self inject: self scalars zero into: [:nextValue :each| nextValue + (aBlock value: each)]! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 7/22/2020 09:31:48'!
supportDo: aBlock
	1 to: self size do: [:index| (self isZeroAt: index) ifFalse: [aBlock value: index]]! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 1/4/2022 12:32:19'!
with: aTuple collect: aBlock
	^ parent filling: [:i| aBlock value: (self at: i) value: (aTuple at: i)]! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 3/13/2020 16:15:51'!
with: otherCollection do: twoArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	1 to: self size do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 5/29/2020 16:31:58'!
withIndexCollect: aBlock
	^ parent filling: [:i| aBlock value: (self at: i) value: i]! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:30:29'!
withIndexDo: aBlock
	coefficients withIndexDo: aBlock! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 7/26/2020 13:16:06'!
withNamesDo: aBlock
	| names |
	names _ parent names.
	1 to: self size do: [:index| (self isZeroAt: index) ifFalse: [aBlock value: (self at: index) value: (names at: index)]]! !

!TupleOlder methodsFor: 'enumerating' stamp: 'len 7/19/2020 07:58:07'!
withSupportDo: aBlock
	1 to: self size do: [:index| (self isZeroAt: index) ifFalse: [aBlock value: (self at: index) value: index]]! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/1/2022 07:37:16'!
% anInteger
	self scalars = ℤ ifFalse: [self error: 'not over the integers'].
	^ self apply: (ℤ / anInteger) projection! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/1/2022 07:38:26'!
* anObject
	(self scalars includes: anObject) ifTrue: [^ self class coefficients: coefficients * anObject parent: parent].
	anObject isMatrix ifTrue: [^ (self asRowMatrix * anObject) asTuple].
	anObject isTuple ifTrue: [^ 	self class coefficients: coefficients * anObject coefficients parent: (self size <= anObject size ifTrue: [parent] ifFalse: [anObject parent])].
	^ self * (self scalars !! anObject)! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/20/2022 18:25:30'!
+ aTuple
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #+].
	^ self class coefficients: coefficients + aTuple coefficients parent: (self size >= aTuple size ifTrue: [parent] ifFalse: [aTuple parent])! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/20/2022 18:25:55'!
- aTuple
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #-].
	^ self class coefficients: coefficients - aTuple coefficients parent: (self size >= aTuple size ifTrue: [parent] ifFalse: [aTuple parent])! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/17/2022 04:23:43'!
/ anElement
	"If the argument is a matrix, answer X such that XA = B, where B is self and A is the argument.  X is called a lift of B along A.
	If the argument is a scalar, just divide each element of the tuple by it."
	anElement isMatrix ifTrue: [^ (anElement colift: self asMatrix transpose) asTuple].
	"We force coercion to the coefficients ring in case the division was not exact and it returned an element outside of the ring. This can happen with Integers, for example 1 / 2 doesn't produce a division error but instead returns a Fraction."
	^ self class coefficients: parent scalars !!!! (coefficients / anElement) parent: parent! !

!TupleOlder methodsFor: 'operations' stamp: 'len 1/13/2022 09:14:56'!
// anElement
	^ self collect: [:each| each // anElement]! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/6/2022 05:55:57'!
<< anInteger
	"Answer the left shift of the receiver by anInteger places.
	The answer is a new tuple of the same size as the receiver."
	^ self >> anInteger negated! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/6/2022 06:03:31'!
<<< anInteger
	"Answer the circular left shift of the receiver by anInteger places.
	The answer is a new tuple of the same size as the receiver."
	^ self >>> anInteger negated! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/6/2022 05:55:42'!
>> anInteger
	"Answer the right shift of the receiver by anInteger places.
	The answer is a new tuple of the same size as the receiver."
	anInteger = 0 ifTrue: [^ self].
	^ self class coefficients: coefficients >> anInteger parent: parent! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/6/2022 06:04:28'!
>>> anInteger
	"Answer the circular right shift of the receiver by anInteger places.
	The answer is a new tuple of the same size as the receiver."
	anInteger = 0 ifTrue: [^ self].
	^ self class coefficients: coefficients >>> anInteger parent: parent! !

!TupleOlder methodsFor: 'operations' stamp: 'len 1/13/2022 09:14:51'!
\\ anElement
	^ self collect: [:each| each \\ anElement]! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/20/2022 18:27:03'!
· aTuple
	(self isEmpty or: [aTuple isEmpty]) ifTrue: [^ self scalars zero].
	^ coefficients · aTuple coefficients! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/20/2022 08:07:21'!
× aTuple
	"Answer the cross product of the receiver with the argument."
	(self size = 3 and: [aTuple parent = self parent]) ifFalse: [DomainError signal].
	^ self parent elementAt:
			{(self at: 2)*(aTuple at: 3) - ((self at: 3)*(aTuple at: 2)).
			(self at: 3)*(aTuple at: 1) - ((self at: 1)*(aTuple at: 3)).
			(self at: 1)*(aTuple at: 2) - ((self at: 2)*(aTuple at: 1))}! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/28/2021 11:40:42'!
⊗ anElement
	"Answer the outer product of the receiver with the argument."
	anElement isTuple ifTrue: [^ Matrix from: anElement parent to: self parent filling: [:i :j| (self at: i)*(anElement at: j)]].
	anElement isMatrix ifTrue: [^ self asMatrix ⊗ anElement transpose].
	^ super ⊗ anElement! !

!TupleOlder methodsFor: 'operations' stamp: 'len 7/15/2021 10:41:40'!
annihilator
	^ self annihilatorIdeal generator! !

!TupleOlder methodsFor: 'operations' stamp: 'len 8/12/2022 13:13:17'!
annihilatorIdeal
	| answer |
	answer _ self scalars asIdeal.
	self isZero ifTrue: [^ answer].
	self do: [:each| answer _ answer ∩ each annihilatorIdeal. answer isTrivial ifTrue: [^ answer]].
	^ answer! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/28/2022 07:36:07'!
apply: aFunction
	"Answer a new tuple with coefficients obtainted by applying aFunction to the coefficients of the receiver.
	If aFunction is a block we assume the parent doesn't change, i.e. the block must evaluate to values within the scalars ring of the receiver."

	^ self class coefficients: (coefficients apply: aFunction) parent: (aFunction isBlock ifTrue: [parent] ifFalse: [parent over: aFunction codomain])! !

!TupleOlder methodsFor: 'operations' stamp: 'len 8/9/2020 07:47:06'!
conjugate
	^ self collect: [:each| each conjugate]! !

!TupleOlder methodsFor: 'operations' stamp: 'len 7/17/2019 11:02:11'!
content
	"Answer the GCD of the coefficients of the receiver.
	In a field, it is convenient to define the content as 1.
	For example, (2,4) has content 2 over Z and 1 over Q."
	| answer |
	self isZero ifTrue: [^ self scalars zero].
	self scalars isField ifTrue: [^ self scalars one].
	answer _ self scalars zero.
	self do: [:each| answer _ answer gcd: each].
	^ answer! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/20/2022 18:28:14'!
convolution: aTuple
	| result |
	self isEmpty ifTrue: [^ self].
	aTuple isEmpty ifTrue: [^ aTuple].
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #convolution:].
	result _ coefficients convolution: aTuple coefficients.
	^ self class coefficients: result parent: self scalars ^ result size! !

!TupleOlder methodsFor: 'operations' stamp: 'len 11/27/2016 13:25:12'!
height
	| answer |
	answer _ 1.
	self do: [:each| answer _ answer max: each height].
	^ answer! !

!TupleOlder methodsFor: 'operations' stamp: 'len 1/14/2022 10:59:49'!
homogenizedAt: anInteger
	^ (self copyFrom: 1 to: anInteger - 1), self scalars one, (self copyFrom: anInteger to: self size)! !

!TupleOlder methodsFor: 'operations' stamp: 'len 7/27/2020 15:03:24'!
length
	^ self norm sqrt! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/1/2022 07:44:09'!
max
	^ coefficients max! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/1/2022 07:44:17'!
min
	^ coefficients min! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/20/2022 18:29:59'!
negated
	^ self class coefficients: coefficients negated parent: parent! !

!TupleOlder methodsFor: 'operations' stamp: 'len 8/14/2022 12:24:20'!
norm
	^ self · self! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/23/2022 20:20:56'!
normalization
	"Answer a unit u such that 'self * u' is a unique choice of associate. See >>normalized.
	NOTE: when the coefficients ring is not an integral domain this choice of unit might not be unique."
	^ (self at: (self lastNonzeroIndex ifNil: [^ self scalars one])) normalization! !

!TupleOlder methodsFor: 'operations' stamp: 'len 1/9/2022 08:19:26'!
primitivePart
	"Answer the primitive part of the receiver."
	^ self isZero ifTrue: [self] ifFalse: [self / self content]! !

!TupleOlder methodsFor: 'operations' stamp: 'len 4/20/2022 18:30:15'!
reversed
	^ self class coefficients: coefficients reversed parent: parent! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/16/2020 13:32:19'!
stabilizers
	| answer last |
	self size <= 1 ifTrue: [^ #()].
	answer _ OrderedCollection new: self size - 1.
	last _ self at: 1.
	2 to: self size do: [:i| | ai ci |
		ai _ self at: i.
		ci _ last stabilizerWith: ai.
		answer add: ci.
		last _ ci*ai + last].
	^ answer! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/1/2022 07:44:56'!
support
	^ (1 to: self size) reject: [:each| self isZeroAt: each]! !

!TupleOlder methodsFor: 'operations' stamp: 'len 5/1/2022 07:46:21'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ coefficients weight! !

!TupleOlder methodsFor: 'printing' stamp: 'len 4/20/2022 18:30:02'!
printOn: aStream
	coefficients printOn: aStream! !

!TupleOlder methodsFor: 'printing' stamp: 'len 7/8/2022 11:17:38'!
validate
	super validate.
	self assert: self parent rank = self size.
	self assert: (self allSatisfy: [:each| self scalars includes: each])! !

!TupleOlder methodsFor: 'testing' stamp: 'len 8/11/2019 12:20:17'!
beginsWith: anArray
	"Answer true if the receiver starts with the argument collection."
	| n |
	self size < (n _ anArray size) ifTrue: [^ false].
	1 to: n do: [:index| (anArray at: index) = (self at: index) ifFalse: [^ false]].
	^ true! !

!TupleOlder methodsFor: 'testing' stamp: 'len 4/20/2022 18:29:38'!
includes: anObject
	^ coefficients includes: anObject! !

!TupleOlder methodsFor: 'testing' stamp: 'len 11/5/2016 07:12'!
isEmpty
	^ self size = 0! !

!TupleOlder methodsFor: 'testing' stamp: 'len 7/26/2021 15:24:17'!
isMonomial
	^ self allSatisfy: [:each| each isMonomial]! !

!TupleOlder methodsFor: 'testing' stamp: 'len 5/22/2018 16:53:54'!
isSequenceable
	^ true! !

!TupleOlder methodsFor: 'testing' stamp: 'len 3/28/2018 18:01:30'!
isTuple
	^ true! !

!TupleOlder methodsFor: 'testing' stamp: 'len 5/16/2020 10:34:21'!
isUnimodular
	"Answer true if the coefficients of the receiver generate the unit ideal.
	Some authors use the name 'primitive' instead of 'unimodular'."
	^ (self scalars * self asArray) isOne! !

!TupleOlder methodsFor: 'testing' stamp: 'len 4/20/2022 18:29:44'!
isZero
	^ coefficients isZero! !

!TupleOlder methodsFor: 'testing' stamp: 'len 4/20/2022 18:29:46'!
isZeroAt: anInteger
	^ coefficients isZeroAt: anInteger! !

!TupleOlder methodsFor: 'private' stamp: 'len 6/6/2019 09:41:21'!
at: anInteger add: anElement
	^ self at: anInteger put: (self at: anInteger) + anElement! !

!TupleOlder methodsFor: 'private' stamp: 'len 4/20/2022 18:27:52'!
at: anInteger put: anObject
	^ coefficients at: anInteger put: anObject! !

!TupleOlder methodsFor: 'private' stamp: 'len 4/20/2022 18:31:34'!
coefficients
	^ coefficients! !

!TupleOlder methodsFor: 'private' stamp: 'len 4/20/2022 18:30:48'!
coefficients: anArray parent: aFreeModule
	coefficients _ anArray.
	parent _ aFreeModule! !

!TupleOlder methodsFor: 'private' stamp: 'len 4/20/2022 18:30:26'!
swap: i with: j
	coefficients swap: i with: j! !

!TupleOlder class methodsFor: 'instance creation' stamp: 'len 4/20/2022 18:32:21'!
coefficients: anArray parent: aFreeModule
	^ self basicNew coefficients: anArray parent: aFreeModule! !

!TupleOlder class methodsFor: 'instance creation' stamp: 'len 4/18/2022 15:41:57'!
new
	^ self shouldNotImplement! !

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

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/15/2019 23:57:46'!
* aBit
	aBit isMatrix ifTrue: [^ super * aBit].
	^ ((aBit isInteger and: [aBit even]) or: [aBit isZero]) ifTrue: [self zero] ifFalse: [self]! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/28/2020 14:27:21'!
+ aBitTuple
	^ self class
		bits: (bits bitXor: aBitTuple asInteger)
		parent: (self size >= aBitTuple size ifTrue: [parent] ifFalse: [aBitTuple parent])! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 3/28/2016 06:51'!
- aBitTuple
	^ self + aBitTuple! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 7/15/2021 12:42:04'!
>> anInteger
	^ self class bits: (bits bitShift: anInteger) parent: parent! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/28/2020 14:30:57'!
>>> anInteger
	anInteger = 0 ifTrue: [^ self].
	^ self class bits: (bits bitShift: anInteger) parent: self scalars^(self size + anInteger max: 0)! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/8/2019 03:09:41'!
· aBitTuple
	^ (bits bitAnd: aBitTuple asInteger) bitParity = 0 ifTrue: [Zero] ifFalse: [One]

"	^ self hadamard: aBitTuple :: sum
"

"	| x |
	x _ self asInteger bitAnd: aBitTuple asInteger.
	[x > 1]
		whileTrue: [x _ x >> (x highBit // 2) bitAnd: x].
	^ x"! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/29/2020 07:47:48'!
convolution: aBitTuple
	| a b c |
	a _ bits.
	b _ aBitTuple asInteger.
	c _ 0.
	1 to: a highBit do: [:i|
		(a bitAt: i) = 1 ifTrue: [c _ c bitXor: b].
		b _ b bitShift: 1].
	^ self class bits: c parent: self scalars ^ (self size + aBitTuple size - 1 max: 0)! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/28/2020 14:34:18'!
hadamard: aBitTuple
	"Answer the entrywise product with the argument, also called Hadamard product."
	^ self class bits: (bits bitAnd: aBitTuple asInteger) parent: (self size <= aBitTuple size ifTrue: [parent] ifFalse: [aBitTuple parent])! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 7/10/2020 17:23:55'!
length
	^ self norm! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/8/2019 03:09:27'!
lift
	"Answer a representative integer tuple."
	^ self representative! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 3/28/2016 07:30'!
negated
	^ self! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 7/10/2020 17:25:49'!
norm
	self flag: #fix. "this assumes the inner product is the dot product"
	^ self sum! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/8/2019 03:08:16'!
parity
	"Answer 1 if the receiver has an odd number of bits on, 0 if it has an even number of bits on."
	| n parity |
	bits = 0 ifTrue: [^ 0].
"	n _ bits >> (bits lowBit - 1). skip initial zeros, we could also skip chunks of zeros in the middle"
	n _ bits.
	parity _ 0.
	[n = 0] whileFalse: [parity _ parity bitXor: (n bitAnd: 1). n _ n >> 1].
	^ parity! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/29/2020 19:40:07'!
polynomialDivisionBy: aTuple
	| q r b n m |
	b _ aTuple asInteger.
	(m _ b highBit) = 0 ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialDivisionBy: argument: aTuple) signal].
	q _ 0.
	r _ self asInteger.
	[(n _ r highBit) >= m] "n = degree(r)+1"
		whileTrue:
			[| i |
			i _ n-m.
			q _ q bitXor: 1 << i. "wasting some memory while creating 2^i"
			r _ r bitXor: b << i]. "also wasting some memory here"
	^ {self class newFrom: q. self class newFrom: r}! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/29/2020 19:40:15'!
polynomialRemainderOfDivisionBy: aTuple
	| r b n m |
	b _ aTuple asInteger.
	(m _ b highBit) = 0 ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialRemainderOfDivisionBy: argument: aTuple) signal].
	r _ self asInteger.
	[(n _ r highBit) >= m] "n = degree(r)+1"
		whileTrue:
			[r _ r bitXor: b << (n-m)]. "wasting some memory here"
	^ self class newFrom: r! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 11/23/2016 05:42:05'!
polynomialValue: anObject
	Zero = anObject ifTrue: [^ self at: 1].
	One = anObject ifFalse: [^ super polynomialValue: anObject].
	^ self sum! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/28/2020 14:25:18'!
reversed
	^ self class bits: (bits bitReverse: self size) parent: parent! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/8/2019 02:58:43'!
sum
	"Answer the sum of the bits, i.e. the parity."
	^ bits bitParity = 0 ifTrue: [Zero] ifFalse: [One]! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 11/12/2016 08:41'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ self asInteger bitCount! !

!BitTuple2 methodsFor: 'printing' stamp: 'len 3/28/2016 04:52'!
printOn: aStream
	1 to: self size do: [:i| aStream print: (self at: i)]! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 5/8/2019 02:58:37'!
at: anInteger
	^ (bits bitAt: anInteger) = 1 ifTrue: [One] ifFalse: [Zero]! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 1/1/2022 21:56:38'!
at: anInteger put: aBit
	| bit |
	anInteger > self size ifTrue: [self error: 'index out of bounds'].
	bit _ aBit.
	bit isInteger ifTrue: [bit _ bit even ifTrue: [Zero] ifFalse: [One]].
	(self at: anInteger) = bit
		ifFalse: [bits _ bits bitXor: (1 bitShift: anInteger - 1)].
	^ bit! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 7/19/2020 08:06:56'!
bitAt: anInteger
	^ bits bitAt: anInteger! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 7/19/2020 08:08:54'!
bitAt: anInteger put: oneOrZero
	anInteger > self size ifTrue: [self error: 'index out of bounds'].
	(self bitAt: anInteger) = oneOrZero
		ifFalse: [bits _ bits bitXor: 1 << (anInteger - 1)].
	^ oneOrZero! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 1/2/2022 09:56:51'!
depth
	"Answer the index of the last nonzero entry, or 0.
	(Instead, Magma defines this as the index of the *first* nonzero entry.)
	See >>pivot for the index of the first nonzero entry."
	^ bits highBit! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 5/29/2020 15:21:10'!
representative
	"Answer a representative integer tuple."
	^ parent lift filling: [:i| bits bitAt: i]! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 5/4/2018 19:09:27'!
scalars
	^ One parent! !

!BitTuple2 methodsFor: 'comparing' stamp: 'len 7/27/2020 08:43:41'!
= anObject
	^ self class = anObject class and: [bits = anObject asInteger]! !

!BitTuple2 methodsFor: 'comparing' stamp: 'len 5/8/2019 03:06:15'!
hash
	^ bits hash! !

!BitTuple2 methodsFor: 'constants' stamp: 'len 5/28/2020 14:24:55'!
zero
	^ self class bits: 0 parent: parent! !

!BitTuple2 methodsFor: 'converting' stamp: 'len 5/8/2019 03:10:19'!
asInteger
	^ bits! !

!BitTuple2 methodsFor: 'copying' stamp: 'len 5/28/2020 14:29:53'!
, anObject
	anObject isTuple
		ifTrue: [^ self class bits: (bits bitXor: (anObject asInteger bitShift: self size)) parent: self scalars^(self size+anObject size)].
	^ self copyWith: anObject! !

!BitTuple2 methodsFor: 'copying' stamp: 'len 1/1/2022 21:09:00'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."
	| n |
	n _ stop - start + 1 max: 0.
	^ self class bits: ((bits bitShift: 1 - start) bitAnd: (1 bitShift: n) - 1) parent: self scalars^n! !

!BitTuple2 methodsFor: 'copying' stamp: 'len 5/28/2020 14:24:12'!
copyWith: aBit
	| answer |
	answer _ self class bits: bits parent: self scalars^(self size + 1).
	aBit = One ifTrue: [answer at: self size+1 put: One. ^ answer].
	aBit = Zero ifFalse: [DomainError signal].
	^ answer! !

!BitTuple2 methodsFor: 'enumerating' stamp: 'len 11/5/2016 09:14'!
anySatisfy: aBlock
	"Optimized. There are only two possible elements that could satisfy aBlock (1 or 0)."
	self isEmpty ifTrue: [^ false].
	(self isZero not and: [aBlock value: One]) ifTrue: [^ true].
	^ (aBlock value: Zero) and: [super anySatisfy: [:any| any = Zero]]! !

!BitTuple2 methodsFor: 'testing' stamp: 'len 1/3/2022 18:29:45'!
isZero
	^ bits == 0! !

!BitTuple2 methodsFor: 'testing' stamp: 'len 12/30/2021 11:34:15'!
isZeroAt: anInteger
	^ (bits bitAt: anInteger) = 0! !

!BitTuple2 methodsFor: 'private' stamp: 'len 5/8/2019 03:03:28'!
bits: anInteger
	bits _ anInteger! !

!BitTuple2 class methodsFor: 'initialization' stamp: 'len 5/23/2020 08:58:58'!
initialize
	| Z2 |
	super initialize.
	Z2 _ ℤ / 2.
	One _ Z2 one.
	Zero _ Z2 zero! !

!BitTuple2 class methodsFor: 'instance creation' stamp: 'len 1/1/2022 20:27:22'!
bits: anInteger parent: aFreeModule
	self assert: anInteger highBit <= aFreeModule rank.
	^ self basicNew bits: anInteger; parent: aFreeModule! !

!BitTuple2 class methodsFor: 'instance creation' stamp: 'len 5/28/2020 14:51:42'!
components: anArray parent: aFreeModule
	| answer |
	answer _ self bits: 0 parent: aFreeModule.
	anArray withIndexDo: [:each :i| answer at: i put: each].
	^ answer! !

!BitTuple2 class methodsFor: 'instance creation' stamp: 'len 11/5/2016 07:09'!
new
	^ self shouldNotImplement! !

!BitTuple2 class methodsFor: 'instance creation' stamp: 'len 5/28/2020 14:38:16'!
newFrom: anInteger
	anInteger isInteger ifTrue: [^ self bits: anInteger parent: One parent^anInteger highBit].
	^ super newFrom: anInteger! !

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

!LargeBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:43:46'!
bitAt: anInteger
	anInteger <= size ifFalse: [^ self errorOutOfBounds].
	^ words bitAt: anInteger! !

!LargeBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:43:46'!
bitAt: anInteger put: aBit
	anInteger <= size ifFalse: [^ self errorOutOfBounds].
	^ words bitAt: anInteger put: aBit! !

!LargeBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:43:46'!
lastNonzeroIndex
	"Answer the index of the last nonzero entry, or nil."
	| wordIndex |
	(wordIndex _ words lastNonzeroIndex) ifNil: [^ nil].
	^ wordIndex - 1 * 32 + (words at: wordIndex) highBit! !

!LargeBitArray methodsFor: 'accessing' stamp: 'len 5/4/2023 23:43:46'!
size
	^ size! !

!LargeBitArray methodsFor: 'comparing' stamp: 'len 5/4/2023 23:43:46'!
= anObject
	^ self class = anObject class and: [size = anObject size and: [words = anObject words]]! !

!LargeBitArray methodsFor: 'comparing' stamp: 'len 5/4/2023 23:43:46'!
hash
	^ words hash! !

!LargeBitArray methodsFor: 'converting' stamp: 'len 5/4/2023 23:43:46'!
asInteger
	| answer |
	answer _ 0.
	words reverseDo: [:each| answer _ answer << 32 bitXor: each].
	^ answer! !

!LargeBitArray methodsFor: 'copying' stamp: 'len 5/4/2023 23:43:46'!
, anObject
	self flag: #deprecated.
	anObject class = self class ifFalse: [^ super , anObject].
	^ (self class new: self size + anObject size)
		replaceFrom: 1 to: self size with: self;
		replaceFrom: self size + 1 to: self size + anObject size with: anObject! !

!LargeBitArray methodsFor: 'copying' stamp: 'len 5/4/2023 23:43:46'!
copyWith: aBit
	| answer |
	answer _ self class new: self size + 1 words: (size \\ 32 = 0 ifTrue: [words copyWith: 0] ifFalse: [words copy]).
	aBit = One ifTrue: [^ answer at: self size+1 put: One; yourself].
	aBit = Zero ifTrue: [^ answer].
	^ self error: 'not one or zero'! !

!LargeBitArray methodsFor: 'copying' stamp: 'len 5/4/2023 23:43:46'!
postCopy
	super postCopy.
	words _ words copy! !

!LargeBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:43:46'!
* anObject
	| newWords |
	anObject isInteger ifTrue: [^ anObject even ifTrue: [self zero] ifFalse: [self]].
	self class = anObject class ifFalse: [^ super * anObject].
	newWords _ WordArray new: (words size min: anObject words size).
	1 to: newWords size do: [:i| newWords at: i put: ((words at: i) bitAnd: (anObject words at: i))].
	^ self class new: (self size min: anObject size) words: newWords! !

!LargeBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:43:46'!
· aTuple
	| sum |
	sum _ 0.
	1 to: (words size min: aTuple words size) do: [:i|
		sum _ ((words at: i) bitAnd: (aTuple words at: i)) bitParity bitXor: sum].
	^ sum bitParity = 0 ifTrue: [Zero] ifFalse: [One]! !

!LargeBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:43:46'!
parity
	^ words inject: 0 into: [:result :each| result bitXor: each bitParity]! !

!LargeBitArray methodsFor: 'operations' stamp: 'len 5/4/2023 23:43:46'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ words inject: 0 into: [:sum :each| sum + each bitCount]! !

!LargeBitArray methodsFor: 'operations-low level' stamp: 'len 5/4/2023 23:43:46'!
replaceFrom: start to: stop with: replacement startingAt: repStart	
	| n shift i j w r |
	(n _ stop - start + 1) <= 0 ifTrue: [^ self].
"	start to: stop do: [:i| self bitAt: i put: (replacement bitAt: i-start+repStart)]"
	shift _ start - 1 \\ 32.
	i _ start - 1 // 32 + 1.
	j _ repStart.
	shift > 0 ifTrue:
		[w _ words at: i.
		r _ replacement wordAtBitIndex: j.
		n < 32
			ifTrue: [r _ r bitAnd: (1 << n - 1). w _ w bitAnd: (1 << n - 1 << shift) bitInvert]
			ifFalse: [w _ w bitAnd: 1 << shift - 1].
		words at: i put: (w bitXor: (r bitAnd: 1 << (32 - shift) - 1) << shift).
		n _ n - (32 - shift).
		j _ j + 32 - shift.
		i _ i + 1].
	[n > 32] whileTrue:
		[w _ words at: i.
		r _ replacement wordAtBitIndex: j.
		words at: i put: r.
		n _ n - 32.
		j _ j + 32.
		i _ i + 1].
	n > 0 ifTrue:
		[w _ words at: i.
		r _ replacement wordAtBitIndex: j.
		words at: i put: ((w bitAnd: (1 << n - 1) bitInvert) bitXor: (r bitAnd: 1 << n - 1))]! !

!LargeBitArray methodsFor: 'operations-low level' stamp: 'len 5/4/2023 23:43:46'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2	
	^ super replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2	

"	| mask n slice |
	(n _ stop - start + 1) < 0 ifTrue: [^ self].
	startWords _ start - 1 // 32 + 1.
	startBitPosition _ start - (startWords * 32).
	stopWords _ stop - 1 // 32 + 1.
	start1Words _ start1 - 1 // 32 + 1.
	start1BitPosition _ start - (start1Words * 32).
	start2Words _ start2 - 1 // 32 + 1.
	start2BitPosition _ start - (start2Words * 32).
	carry1 _ 0.
	carry2 _ 0.
	j _ start1 - 1 // 32 + 1.
	k _ start2 - 1 // 32 + 1.
	startWords to: stopWords do: [:i|
		words at: i put: ((anArray1 wordAt: j) >> shift bitXor: carrier).
		carrier _ (words at: j) << (32 - shift) bitAnd: 16rFFFFFFFF.
		j _ j + 1].
	n > 0 ifTrue: [newWords at: newWords size put: ((newWords at: newWords size) bitAnd: 16rFFFFFFFF >> (32 - (n \\ 32)))].
	"! !

!LargeBitArray methodsFor: 'operations-low level' stamp: 'len 5/4/2023 23:43:46'!
wordAtBitIndex: bitIndex
	| wordIndex shift answer |
	wordIndex _ bitIndex - 1 // 32 + 1.
	wordIndex > words size ifTrue: [^ 0].
	answer _ words at: wordIndex.
	(shift _ bitIndex - 1 \\ 32) = 0 ifTrue: [^ answer].
	answer _ answer >> shift.
	wordIndex = words size ifTrue: [^ answer].
	^ answer bitXor: ((words at: wordIndex+1) << (32 - shift) bitAnd: 0xFFFFFFFF)! !

!LargeBitArray methodsFor: 'testing' stamp: 'len 5/4/2023 23:43:46'!
isZero
	^ words isZero! !

!LargeBitArray methodsFor: 'private' stamp: 'len 5/4/2023 23:43:46'!
words
	^ words! !

!LargeBitArray methodsFor: 'private' stamp: 'len 5/4/2023 23:43:46'!
words: aWordArray size: anInteger
	| n |
	n _ aWordArray size.
	anInteger - 1 // 32 + 1 = n ifFalse: [self error: 'too many words'].
"	words size = 0 or: [words last highBit <= (size \\ 32 + 1)]"
	words _ aWordArray.
	size _ anInteger! !

!LargeBitArray class methodsFor: 'instance creation' stamp: 'len 5/4/2023 23:43:46'!
new: anInteger
	^ self new: anInteger words: (WordArray new: anInteger - 1 // 32 + 1)! !

!LargeBitArray class methodsFor: 'instance creation' stamp: 'len 5/4/2023 23:43:46'!
new: anInteger words: aWordArray
	^ self basicNew words: aWordArray size: anInteger! !

!LargeBitArray class methodsFor: 'instance creation' stamp: 'len 5/4/2023 23:43:46'!
newFrom: anObject
	| n r words |
	anObject isInteger ifFalse: [^ super newFrom: anObject].
	n _ anObject.
	r _ n highBit.
	words _ WordArray new: r - 1 // 32 + 1.
	1 to: words size do: [:i|
		words at: i put: (n bitAnd: 16rFFFFFFFF).
		n _ n >> 32].
	^ self new: r words: words! !

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
BitTuple initialize!
BitTuple2 initialize!
