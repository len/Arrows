'From Cuis 6.0 [latest update: #5898] on 20 November 2023 at 4:24:44 pm'!
'Description '!
!provides: 'Tests-Algebra' 1 102!
SystemOrganization addCategory: #'Tests-Algebra'!
SystemOrganization addCategory: #'Tests-Algebra-Rings'!
SystemOrganization addCategory: #'Tests-Algebra-Arrays'!
SystemOrganization addCategory: #'Tests-Algebra-Tuples'!
SystemOrganization addCategory: #'Tests-Algebra-Matrices'!
SystemOrganization addCategory: #'Tests-Algebra-Modules'!
SystemOrganization addCategory: #'Tests-Algebra-Groebner Bases'!
SystemOrganization addCategory: #'Tests-Algebra-Benchmarks'!


!classDefinition: #AbelianGroupTest category: #'Tests-Algebra'!
TestCase subclass: #AbelianGroupTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'AbelianGroupTest class' category: #'Tests-Algebra'!
AbelianGroupTest class
	instanceVariableNames: ''!

!classDefinition: #AlgebraicGeometryTest category: #'Tests-Algebra'!
TestCase subclass: #AlgebraicGeometryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'AlgebraicGeometryTest class' category: #'Tests-Algebra'!
AlgebraicGeometryTest class
	instanceVariableNames: ''!

!classDefinition: #AlgebraicGraphTest category: #'Tests-Algebra'!
TestCase subclass: #AlgebraicGraphTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'AlgebraicGraphTest class' category: #'Tests-Algebra'!
AlgebraicGraphTest class
	instanceVariableNames: ''!

!classDefinition: #ChainComplexTest category: #'Tests-Algebra'!
TestCase subclass: #ChainComplexTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'ChainComplexTest class' category: #'Tests-Algebra'!
ChainComplexTest class
	instanceVariableNames: ''!

!classDefinition: #CommutativeAlgebraTest category: #'Tests-Algebra'!
TestCase subclass: #CommutativeAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'CommutativeAlgebraTest class' category: #'Tests-Algebra'!
CommutativeAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #MathematicsTest category: #'Tests-Algebra'!
TestCase subclass: #MathematicsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'MathematicsTest class' category: #'Tests-Algebra'!
MathematicsTest class
	instanceVariableNames: ''!

!classDefinition: #MonomialTest category: #'Tests-Algebra'!
TestCase subclass: #MonomialTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'MonomialTest class' category: #'Tests-Algebra'!
MonomialTest class
	instanceVariableNames: ''!

!classDefinition: #NumberFieldTest category: #'Tests-Algebra'!
TestCase subclass: #NumberFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'NumberFieldTest class' category: #'Tests-Algebra'!
NumberFieldTest class
	instanceVariableNames: ''!

!classDefinition: #PolynomialTest category: #'Tests-Algebra'!
TestCase subclass: #PolynomialTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'PolynomialTest class' category: #'Tests-Algebra'!
PolynomialTest class
	instanceVariableNames: ''!

!classDefinition: #RandomizedTestCase category: #'Tests-Algebra'!
TestCase subclass: #RandomizedTestCase
	instanceVariableNames: 'seed random'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'RandomizedTestCase class' category: #'Tests-Algebra'!
RandomizedTestCase class
	instanceVariableNames: ''!

!classDefinition: #AbstractAbelianCategoryTest category: #'Tests-Algebra'!
RandomizedTestCase subclass: #AbstractAbelianCategoryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'AbstractAbelianCategoryTest class' category: #'Tests-Algebra'!
AbstractAbelianCategoryTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerModuleAbelianCategoryTest category: #'Tests-Algebra'!
AbstractAbelianCategoryTest subclass: #IntegerModuleAbelianCategoryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'IntegerModuleAbelianCategoryTest class' category: #'Tests-Algebra'!
IntegerModuleAbelianCategoryTest class
	instanceVariableNames: ''!

!classDefinition: #BilinearFormTest category: #'Tests-Algebra'!
RandomizedTestCase subclass: #BilinearFormTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'BilinearFormTest class' category: #'Tests-Algebra'!
BilinearFormTest class
	instanceVariableNames: ''!

!classDefinition: #GroupTest category: #'Tests-Algebra'!
RandomizedTestCase subclass: #GroupTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'GroupTest class' category: #'Tests-Algebra'!
GroupTest class
	instanceVariableNames: ''!

!classDefinition: #LinearGroupsTest category: #'Tests-Algebra'!
RandomizedTestCase subclass: #LinearGroupsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'LinearGroupsTest class' category: #'Tests-Algebra'!
LinearGroupsTest class
	instanceVariableNames: ''!

!classDefinition: #QuadraticFormTest category: #'Tests-Algebra'!
RandomizedTestCase subclass: #QuadraticFormTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'QuadraticFormTest class' category: #'Tests-Algebra'!
QuadraticFormTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractRingTest category: #'Tests-Algebra-Rings'!
RandomizedTestCase subclass: #AbstractRingTest
	instanceVariableNames: 'R'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractRingTest class' category: #'Tests-Algebra-Rings'!
AbstractRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractAlgebraTest category: #'Tests-Algebra-Rings'!
AbstractRingTest subclass: #AbstractAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractAlgebraTest class' category: #'Tests-Algebra-Rings'!
AbstractAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractFiniteAlgebraTest category: #'Tests-Algebra-Rings'!
AbstractAlgebraTest subclass: #AbstractFiniteAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractFiniteAlgebraTest class' category: #'Tests-Algebra-Rings'!
AbstractFiniteAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractMaximalOrderTest category: #'Tests-Algebra-Rings'!
AbstractFiniteAlgebraTest subclass: #AbstractMaximalOrderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractMaximalOrderTest class' category: #'Tests-Algebra-Rings'!
AbstractMaximalOrderTest class
	instanceVariableNames: ''!

!classDefinition: #CyclotomicRingTest category: #'Tests-Algebra-Rings'!
AbstractMaximalOrderTest subclass: #CyclotomicRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'CyclotomicRingTest class' category: #'Tests-Algebra-Rings'!
CyclotomicRingTest class
	instanceVariableNames: ''!

!classDefinition: #QuadraticRingTest category: #'Tests-Algebra-Rings'!
AbstractMaximalOrderTest subclass: #QuadraticRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'QuadraticRingTest class' category: #'Tests-Algebra-Rings'!
QuadraticRingTest class
	instanceVariableNames: ''!

!classDefinition: #CliffordAlgebraTest category: #'Tests-Algebra-Rings'!
AbstractFiniteAlgebraTest subclass: #CliffordAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'CliffordAlgebraTest class' category: #'Tests-Algebra-Rings'!
CliffordAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixAlgebraTest category: #'Tests-Algebra-Rings'!
AbstractFiniteAlgebraTest subclass: #MatrixAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'MatrixAlgebraTest class' category: #'Tests-Algebra-Rings'!
MatrixAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #QuaternionAlgebraTest category: #'Tests-Algebra-Rings'!
AbstractFiniteAlgebraTest subclass: #QuaternionAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'QuaternionAlgebraTest class' category: #'Tests-Algebra-Rings'!
QuaternionAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #RationalAffineAlgebraTest category: #'Tests-Algebra-Rings'!
AbstractAlgebraTest subclass: #RationalAffineAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'RationalAffineAlgebraTest class' category: #'Tests-Algebra-Rings'!
RationalAffineAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractCommutativeRingTest category: #'Tests-Algebra-Rings'!
AbstractRingTest subclass: #AbstractCommutativeRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractCommutativeRingTest class' category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractGCDRingTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #AbstractGCDRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractGCDRingTest class' category: #'Tests-Algebra-Rings'!
AbstractGCDRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractEuclideanRingTest category: #'Tests-Algebra-Rings'!
AbstractGCDRingTest subclass: #AbstractEuclideanRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractEuclideanRingTest class' category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractEuclideanDomainTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #AbstractEuclideanDomainTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractEuclideanDomainTest class' category: #'Tests-Algebra-Rings'!
AbstractEuclideanDomainTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractDVRTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanDomainTest subclass: #AbstractDVRTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractDVRTest class' category: #'Tests-Algebra-Rings'!
AbstractDVRTest class
	instanceVariableNames: ''!

!classDefinition: #LocalIntegerRingTest category: #'Tests-Algebra-Rings'!
AbstractDVRTest subclass: #LocalIntegerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'LocalIntegerRingTest class' category: #'Tests-Algebra-Rings'!
LocalIntegerRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractFieldPolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanDomainTest subclass: #AbstractFieldPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractFieldPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
AbstractFieldPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldPolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractFieldPolynomialRingTest subclass: #FiniteFieldPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'FiniteFieldPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
FiniteFieldPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryExtensionFieldPolynomialRingTest category: #'Tests-Algebra-Rings'!
FiniteFieldPolynomialRingTest subclass: #BinaryExtensionFieldPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'BinaryExtensionFieldPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
BinaryExtensionFieldPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #PrimeFieldPolynomialRingTest category: #'Tests-Algebra-Rings'!
FiniteFieldPolynomialRingTest subclass: #PrimeFieldPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'PrimeFieldPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
PrimeFieldPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryFieldPolynomialRingTest category: #'Tests-Algebra-Rings'!
PrimeFieldPolynomialRingTest subclass: #BinaryFieldPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'BinaryFieldPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
BinaryFieldPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryFieldPolynomialRingTest category: #'Tests-Algebra-Rings'!
PrimeFieldPolynomialRingTest subclass: #TernaryFieldPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'TernaryFieldPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
TernaryFieldPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryExtensionFieldPolynomialRingTest category: #'Tests-Algebra-Rings'!
FiniteFieldPolynomialRingTest subclass: #TernaryExtensionFieldPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'TernaryExtensionFieldPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
TernaryExtensionFieldPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #RationalPolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractFieldPolynomialRingTest subclass: #RationalPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'RationalPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
RationalPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractFieldTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanDomainTest subclass: #AbstractFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractFieldTest class' category: #'Tests-Algebra-Rings'!
AbstractFieldTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractFiniteFieldTest category: #'Tests-Algebra-Rings'!
AbstractFieldTest subclass: #AbstractFiniteFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractFiniteFieldTest class' category: #'Tests-Algebra-Rings'!
AbstractFiniteFieldTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisFieldTest category: #'Tests-Algebra-Rings'!
AbstractFiniteFieldTest subclass: #GaloisFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'GaloisFieldTest class' category: #'Tests-Algebra-Rings'!
GaloisFieldTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryExtensionFieldTest category: #'Tests-Algebra-Rings'!
GaloisFieldTest subclass: #BinaryExtensionFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'BinaryExtensionFieldTest class' category: #'Tests-Algebra-Rings'!
BinaryExtensionFieldTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryExtensionFieldTest category: #'Tests-Algebra-Rings'!
GaloisFieldTest subclass: #TernaryExtensionFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'TernaryExtensionFieldTest class' category: #'Tests-Algebra-Rings'!
TernaryExtensionFieldTest class
	instanceVariableNames: ''!

!classDefinition: #PrimeFieldTest category: #'Tests-Algebra-Rings'!
AbstractFiniteFieldTest subclass: #PrimeFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'PrimeFieldTest class' category: #'Tests-Algebra-Rings'!
PrimeFieldTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryFieldTest category: #'Tests-Algebra-Rings'!
PrimeFieldTest subclass: #BinaryFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'BinaryFieldTest class' category: #'Tests-Algebra-Rings'!
BinaryFieldTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryFieldTest category: #'Tests-Algebra-Rings'!
PrimeFieldTest subclass: #TernaryFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'TernaryFieldTest class' category: #'Tests-Algebra-Rings'!
TernaryFieldTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractNumberFieldTest category: #'Tests-Algebra-Rings'!
AbstractFieldTest subclass: #AbstractNumberFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractNumberFieldTest class' category: #'Tests-Algebra-Rings'!
AbstractNumberFieldTest class
	instanceVariableNames: ''!

!classDefinition: #CyclotomicFieldTest category: #'Tests-Algebra-Rings'!
AbstractNumberFieldTest subclass: #CyclotomicFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'CyclotomicFieldTest class' category: #'Tests-Algebra-Rings'!
CyclotomicFieldTest class
	instanceVariableNames: ''!

!classDefinition: #GaussianFieldTest category: #'Tests-Algebra-Rings'!
AbstractNumberFieldTest subclass: #GaussianFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'GaussianFieldTest class' category: #'Tests-Algebra-Rings'!
GaussianFieldTest class
	instanceVariableNames: ''!

!classDefinition: #QuadraticFieldTest category: #'Tests-Algebra-Rings'!
AbstractNumberFieldTest subclass: #QuadraticFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'QuadraticFieldTest class' category: #'Tests-Algebra-Rings'!
QuadraticFieldTest class
	instanceVariableNames: ''!

!classDefinition: #ComplexAlgebraicFieldTest category: #'Tests-Algebra-Rings'!
AbstractFieldTest subclass: #ComplexAlgebraicFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'ComplexAlgebraicFieldTest class' category: #'Tests-Algebra-Rings'!
ComplexAlgebraicFieldTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldRationalFunctionFieldTest category: #'Tests-Algebra-Rings'!
AbstractFieldTest subclass: #FiniteFieldRationalFunctionFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'FiniteFieldRationalFunctionFieldTest class' category: #'Tests-Algebra-Rings'!
FiniteFieldRationalFunctionFieldTest class
	instanceVariableNames: ''!

!classDefinition: #RationalFieldTest category: #'Tests-Algebra-Rings'!
AbstractFieldTest subclass: #RationalFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'RationalFieldTest class' category: #'Tests-Algebra-Rings'!
RationalFieldTest class
	instanceVariableNames: ''!

!classDefinition: #RationalRationalFunctionFieldTest category: #'Tests-Algebra-Rings'!
AbstractFieldTest subclass: #RationalRationalFunctionFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'RationalRationalFunctionFieldTest class' category: #'Tests-Algebra-Rings'!
RationalRationalFunctionFieldTest class
	instanceVariableNames: ''!

!classDefinition: #RealAlgebraicFieldTest category: #'Tests-Algebra-Rings'!
AbstractFieldTest subclass: #RealAlgebraicFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'RealAlgebraicFieldTest class' category: #'Tests-Algebra-Rings'!
RealAlgebraicFieldTest class
	instanceVariableNames: ''!

!classDefinition: #EisensteinRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanDomainTest subclass: #EisensteinRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'EisensteinRingTest class' category: #'Tests-Algebra-Rings'!
EisensteinRingTest class
	instanceVariableNames: ''!

!classDefinition: #GaussianRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanDomainTest subclass: #GaussianRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'GaussianRingTest class' category: #'Tests-Algebra-Rings'!
GaussianRingTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanDomainTest subclass: #IntegerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'IntegerRingTest class' category: #'Tests-Algebra-Rings'!
IntegerRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractGaloisRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #AbstractGaloisRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractGaloisRingTest class' category: #'Tests-Algebra-Rings'!
AbstractGaloisRingTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingTest category: #'Tests-Algebra-Rings'!
AbstractGaloisRingTest subclass: #GaloisRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'GaloisRingTest class' category: #'Tests-Algebra-Rings'!
GaloisRingTest class
	instanceVariableNames: ''!

!classDefinition: #PrimePowerRingTest category: #'Tests-Algebra-Rings'!
AbstractGaloisRingTest subclass: #PrimePowerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'PrimePowerRingTest class' category: #'Tests-Algebra-Rings'!
PrimePowerRingTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryPowerRingTest category: #'Tests-Algebra-Rings'!
PrimePowerRingTest subclass: #BinaryPowerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'BinaryPowerRingTest class' category: #'Tests-Algebra-Rings'!
BinaryPowerRingTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldDualNumbersTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #FiniteFieldDualNumbersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'FiniteFieldDualNumbersTest class' category: #'Tests-Algebra-Rings'!
FiniteFieldDualNumbersTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #ModularIntegerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'ModularIntegerRingTest class' category: #'Tests-Algebra-Rings'!
ModularIntegerRingTest class
	instanceVariableNames: ''!

!classDefinition: #RationalDualNumbersTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #RationalDualNumbersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'RationalDualNumbersTest class' category: #'Tests-Algebra-Rings'!
RationalDualNumbersTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldMultivariatePolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractGCDRingTest subclass: #FiniteFieldMultivariatePolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'FiniteFieldMultivariatePolynomialRingTest class' category: #'Tests-Algebra-Rings'!
FiniteFieldMultivariatePolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerMultivariatePolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractGCDRingTest subclass: #IntegerMultivariatePolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'IntegerMultivariatePolynomialRingTest class' category: #'Tests-Algebra-Rings'!
IntegerMultivariatePolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerPolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractGCDRingTest subclass: #IntegerPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'IntegerPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
IntegerPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #ProductRingTest category: #'Tests-Algebra-Rings'!
AbstractGCDRingTest subclass: #ProductRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'ProductRingTest class' category: #'Tests-Algebra-Rings'!
ProductRingTest class
	instanceVariableNames: ''!

!classDefinition: #RationalMultivariatePolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractGCDRingTest subclass: #RationalMultivariatePolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'RationalMultivariatePolynomialRingTest class' category: #'Tests-Algebra-Rings'!
RationalMultivariatePolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteRingExample1Test category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #FiniteRingExample1Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'FiniteRingExample1Test class' category: #'Tests-Algebra-Rings'!
FiniteRingExample1Test class
	instanceVariableNames: ''!

!classDefinition: #FiniteRingExample2Test category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #FiniteRingExample2Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'FiniteRingExample2Test class' category: #'Tests-Algebra-Rings'!
FiniteRingExample2Test class
	instanceVariableNames: ''!

!classDefinition: #FiniteRingExample3Test category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #FiniteRingExample3Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'FiniteRingExample3Test class' category: #'Tests-Algebra-Rings'!
FiniteRingExample3Test class
	instanceVariableNames: ''!

!classDefinition: #FiniteRingExample4Test category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #FiniteRingExample4Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'FiniteRingExample4Test class' category: #'Tests-Algebra-Rings'!
FiniteRingExample4Test class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingMultivariatePolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #GaloisRingMultivariatePolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'GaloisRingMultivariatePolynomialRingTest class' category: #'Tests-Algebra-Rings'!
GaloisRingMultivariatePolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingPolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #GaloisRingPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'GaloisRingPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
GaloisRingPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #PrimePowerRingPolynomialRingTest category: #'Tests-Algebra-Rings'!
GaloisRingPolynomialRingTest subclass: #PrimePowerRingPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'PrimePowerRingPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
PrimePowerRingPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryPowerRingPolynomialRingTest category: #'Tests-Algebra-Rings'!
PrimePowerRingPolynomialRingTest subclass: #BinaryPowerRingPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'BinaryPowerRingPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
BinaryPowerRingPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerMultivariatePolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #ModularIntegerMultivariatePolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'ModularIntegerMultivariatePolynomialRingTest class' category: #'Tests-Algebra-Rings'!
ModularIntegerMultivariatePolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerPolynomialRingTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #ModularIntegerPolynomialRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'ModularIntegerPolynomialRingTest class' category: #'Tests-Algebra-Rings'!
ModularIntegerPolynomialRingTest class
	instanceVariableNames: ''!

!classDefinition: #QuasiGaloisRingTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #QuasiGaloisRingTest
	instanceVariableNames: 'p r n'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'QuasiGaloisRingTest class' category: #'Tests-Algebra-Rings'!
QuasiGaloisRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractArrayTest category: #'Tests-Algebra-Arrays'!
RandomizedTestCase subclass: #AbstractArrayTest
	instanceVariableNames: 'scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'AbstractArrayTest class' category: #'Tests-Algebra-Arrays'!
AbstractArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #ArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'ArrayTest class' category: #'Tests-Algebra-Arrays'!
ArrayTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #BinaryArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'BinaryArrayTest class' category: #'Tests-Algebra-Arrays'!
BinaryArrayTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryExtensionSlicedArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #BinaryExtensionSlicedArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'BinaryExtensionSlicedArrayTest class' category: #'Tests-Algebra-Arrays'!
BinaryExtensionSlicedArrayTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryPowerArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #BinaryPowerArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'BinaryPowerArrayTest class' category: #'Tests-Algebra-Arrays'!
BinaryPowerArrayTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisFieldArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #GaloisFieldArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'GaloisFieldArrayTest class' category: #'Tests-Algebra-Arrays'!
GaloisFieldArrayTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #GaloisRingArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'GaloisRingArrayTest class' category: #'Tests-Algebra-Arrays'!
GaloisRingArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ModularArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #ModularArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'ModularArrayTest class' category: #'Tests-Algebra-Arrays'!
ModularArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ModularByteArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #ModularByteArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'ModularByteArrayTest class' category: #'Tests-Algebra-Arrays'!
ModularByteArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ModularDoubleByteArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #ModularDoubleByteArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'ModularDoubleByteArrayTest class' category: #'Tests-Algebra-Arrays'!
ModularDoubleByteArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ModularWordArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #ModularWordArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'ModularWordArrayTest class' category: #'Tests-Algebra-Arrays'!
ModularWordArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ProductArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #ProductArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'ProductArrayTest class' category: #'Tests-Algebra-Arrays'!
ProductArrayTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryArrayTest category: #'Tests-Algebra-Arrays'!
AbstractArrayTest subclass: #TernaryArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'TernaryArrayTest class' category: #'Tests-Algebra-Arrays'!
TernaryArrayTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractTupleTest category: #'Tests-Algebra-Tuples'!
RandomizedTestCase subclass: #AbstractTupleTest
	instanceVariableNames: 'scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'AbstractTupleTest class' category: #'Tests-Algebra-Tuples'!
AbstractTupleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractCommutativeRingTupleTest category: #'Tests-Algebra-Tuples'!
AbstractTupleTest subclass: #AbstractCommutativeRingTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'AbstractCommutativeRingTupleTest class' category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractModularIntegerTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #AbstractModularIntegerTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'AbstractModularIntegerTupleTest class' category: #'Tests-Algebra-Tuples'!
AbstractModularIntegerTupleTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryPowerTupleTest category: #'Tests-Algebra-Tuples'!
AbstractModularIntegerTupleTest subclass: #BinaryPowerTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'BinaryPowerTupleTest class' category: #'Tests-Algebra-Tuples'!
BinaryPowerTupleTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryTupleTest category: #'Tests-Algebra-Tuples'!
AbstractModularIntegerTupleTest subclass: #BinaryTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'BinaryTupleTest class' category: #'Tests-Algebra-Tuples'!
BinaryTupleTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerByteTupleTest category: #'Tests-Algebra-Tuples'!
AbstractModularIntegerTupleTest subclass: #ModularIntegerByteTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'ModularIntegerByteTupleTest class' category: #'Tests-Algebra-Tuples'!
ModularIntegerByteTupleTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerDoubleByteTupleTest category: #'Tests-Algebra-Tuples'!
AbstractModularIntegerTupleTest subclass: #ModularIntegerDoubleByteTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'ModularIntegerDoubleByteTupleTest class' category: #'Tests-Algebra-Tuples'!
ModularIntegerDoubleByteTupleTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerTupleTest category: #'Tests-Algebra-Tuples'!
AbstractModularIntegerTupleTest subclass: #ModularIntegerTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'ModularIntegerTupleTest class' category: #'Tests-Algebra-Tuples'!
ModularIntegerTupleTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerWordTupleTest category: #'Tests-Algebra-Tuples'!
AbstractModularIntegerTupleTest subclass: #ModularIntegerWordTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'ModularIntegerWordTupleTest class' category: #'Tests-Algebra-Tuples'!
ModularIntegerWordTupleTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryTupleTest category: #'Tests-Algebra-Tuples'!
AbstractModularIntegerTupleTest subclass: #TernaryTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TernaryTupleTest class' category: #'Tests-Algebra-Tuples'!
TernaryTupleTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryExtensionFieldTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #BinaryExtensionFieldTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'BinaryExtensionFieldTupleTest class' category: #'Tests-Algebra-Tuples'!
BinaryExtensionFieldTupleTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #FiniteFieldTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'FiniteFieldTupleTest class' category: #'Tests-Algebra-Tuples'!
FiniteFieldTupleTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #GaloisRingTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'GaloisRingTupleTest class' category: #'Tests-Algebra-Tuples'!
GaloisRingTupleTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #IntegerTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'IntegerTupleTest class' category: #'Tests-Algebra-Tuples'!
IntegerTupleTest class
	instanceVariableNames: ''!

!classDefinition: #MultivariateTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #MultivariateTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'MultivariateTupleTest class' category: #'Tests-Algebra-Tuples'!
MultivariateTupleTest class
	instanceVariableNames: ''!

!classDefinition: #ProductRingTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #ProductRingTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'ProductRingTupleTest class' category: #'Tests-Algebra-Tuples'!
ProductRingTupleTest class
	instanceVariableNames: ''!

!classDefinition: #RationalPolynomialRingTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #RationalPolynomialRingTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'RationalPolynomialRingTupleTest class' category: #'Tests-Algebra-Tuples'!
RationalPolynomialRingTupleTest class
	instanceVariableNames: ''!

!classDefinition: #RationalTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #RationalTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'RationalTupleTest class' category: #'Tests-Algebra-Tuples'!
RationalTupleTest class
	instanceVariableNames: ''!

!classDefinition: #SmallFiniteFieldTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #SmallFiniteFieldTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'SmallFiniteFieldTupleTest class' category: #'Tests-Algebra-Tuples'!
SmallFiniteFieldTupleTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryExtensionFieldTupleTest category: #'Tests-Algebra-Tuples'!
AbstractCommutativeRingTupleTest subclass: #TernaryExtensionFieldTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TernaryExtensionFieldTupleTest class' category: #'Tests-Algebra-Tuples'!
TernaryExtensionFieldTupleTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixAlgebraTupleTest category: #'Tests-Algebra-Tuples'!
AbstractTupleTest subclass: #MatrixAlgebraTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'MatrixAlgebraTupleTest class' category: #'Tests-Algebra-Tuples'!
MatrixAlgebraTupleTest class
	instanceVariableNames: ''!

!classDefinition: #QuaternionAlgebraTupleTest category: #'Tests-Algebra-Tuples'!
AbstractTupleTest subclass: #QuaternionAlgebraTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'QuaternionAlgebraTupleTest class' category: #'Tests-Algebra-Tuples'!
QuaternionAlgebraTupleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractMatrixTest category: #'Tests-Algebra-Matrices'!
RandomizedTestCase subclass: #AbstractMatrixTest
	instanceVariableNames: 'scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractMatrixTest class' category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractCommutativeRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #AbstractCommutativeRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractCommutativeRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
AbstractCommutativeRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractIntegralDomainMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractCommutativeRingMatrixTest subclass: #AbstractIntegralDomainMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractIntegralDomainMatrixTest class' category: #'Tests-Algebra-Matrices'!
AbstractIntegralDomainMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #CyclotomicIntegerMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractIntegralDomainMatrixTest subclass: #CyclotomicIntegerMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'CyclotomicIntegerMatrixTest class' category: #'Tests-Algebra-Matrices'!
CyclotomicIntegerMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #QuadraticRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractIntegralDomainMatrixTest subclass: #QuadraticRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'QuadraticRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
QuadraticRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractStandardRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractCommutativeRingMatrixTest subclass: #AbstractStandardRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractStandardRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
AbstractStandardRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractEuclideanRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractStandardRingMatrixTest subclass: #AbstractEuclideanRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractEuclideanRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
AbstractEuclideanRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractEuclideanDomainMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanRingMatrixTest subclass: #AbstractEuclideanDomainMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractEuclideanDomainMatrixTest class' category: #'Tests-Algebra-Matrices'!
AbstractEuclideanDomainMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractFieldMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanDomainMatrixTest subclass: #AbstractFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryExtensionFieldMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #BinaryExtensionFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'BinaryExtensionFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
BinaryExtensionFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #BinaryMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'BinaryMatrixTest class' category: #'Tests-Algebra-Matrices'!
BinaryMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #CyclotomicFieldMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #CyclotomicFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'CyclotomicFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
CyclotomicFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #FiniteFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'FiniteFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
FiniteFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #GaussianFieldMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #GaussianFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'GaussianFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
GaussianFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #QuadraticFieldMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #QuadraticFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'QuadraticFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
QuadraticFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #RationalMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #RationalMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'RationalMatrixTest class' category: #'Tests-Algebra-Matrices'!
RationalMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #SmallFiniteFieldMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #SmallFiniteFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'SmallFiniteFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
SmallFiniteFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryExtensionFieldMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #TernaryExtensionFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'TernaryExtensionFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
TernaryExtensionFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractFieldMatrixTest subclass: #TernaryMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'TernaryMatrixTest class' category: #'Tests-Algebra-Matrices'!
TernaryMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #EisensteinIntegerMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanDomainMatrixTest subclass: #EisensteinIntegerMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'EisensteinIntegerMatrixTest class' category: #'Tests-Algebra-Matrices'!
EisensteinIntegerMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #GaussianIntegerMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanDomainMatrixTest subclass: #GaussianIntegerMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'GaussianIntegerMatrixTest class' category: #'Tests-Algebra-Matrices'!
GaussianIntegerMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanDomainMatrixTest subclass: #IntegerMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'IntegerMatrixTest class' category: #'Tests-Algebra-Matrices'!
IntegerMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #PrimeFieldPolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanDomainMatrixTest subclass: #PrimeFieldPolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'PrimeFieldPolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
PrimeFieldPolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #RationalPolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanDomainMatrixTest subclass: #RationalPolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'RationalPolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
RationalPolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryPowerRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanRingMatrixTest subclass: #BinaryPowerRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'BinaryPowerRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
BinaryPowerRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanRingMatrixTest subclass: #GaloisRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'GaloisRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
GaloisRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #LocalIntegerRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanRingMatrixTest subclass: #LocalIntegerRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'LocalIntegerRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
LocalIntegerRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanRingMatrixTest subclass: #ModularIntegerMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'ModularIntegerMatrixTest class' category: #'Tests-Algebra-Matrices'!
ModularIntegerMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #PrimePowerRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractEuclideanRingMatrixTest subclass: #PrimePowerRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'PrimePowerRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
PrimePowerRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingMultivariatePolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractStandardRingMatrixTest subclass: #GaloisRingMultivariatePolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'GaloisRingMultivariatePolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
GaloisRingMultivariatePolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerMultivariatePolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractStandardRingMatrixTest subclass: #ModularIntegerMultivariatePolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'ModularIntegerMultivariatePolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
ModularIntegerMultivariatePolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerPolynomialRingPolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractStandardRingMatrixTest subclass: #ModularIntegerPolynomialRingPolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'ModularIntegerPolynomialRingPolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
ModularIntegerPolynomialRingPolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #PrimeFieldMultivariatePolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractStandardRingMatrixTest subclass: #PrimeFieldMultivariatePolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'PrimeFieldMultivariatePolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
PrimeFieldMultivariatePolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #RationalMultivariatePolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractStandardRingMatrixTest subclass: #RationalMultivariatePolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'RationalMultivariatePolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
RationalMultivariatePolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #RationalPolynomialRingPolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractStandardRingMatrixTest subclass: #RationalPolynomialRingPolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'RationalPolynomialRingPolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
RationalPolynomialRingPolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #PrimeFieldAffineAlgebraMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractCommutativeRingMatrixTest subclass: #PrimeFieldAffineAlgebraMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'PrimeFieldAffineAlgebraMatrixTest class' category: #'Tests-Algebra-Matrices'!
PrimeFieldAffineAlgebraMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #ProductRingMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractCommutativeRingMatrixTest subclass: #ProductRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'ProductRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
ProductRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #RationalAffineAlgebraMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractCommutativeRingMatrixTest subclass: #RationalAffineAlgebraMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'RationalAffineAlgebraMatrixTest class' category: #'Tests-Algebra-Matrices'!
RationalAffineAlgebraMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #CliffordAlgebraMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #CliffordAlgebraMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'CliffordAlgebraMatrixTest class' category: #'Tests-Algebra-Matrices'!
CliffordAlgebraMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #GroupAlgebraMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #GroupAlgebraMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'GroupAlgebraMatrixTest class' category: #'Tests-Algebra-Matrices'!
GroupAlgebraMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixAlgebraMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #MatrixAlgebraMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixAlgebraMatrixTest class' category: #'Tests-Algebra-Matrices'!
MatrixAlgebraMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #QuaternionAlgebraMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #QuaternionAlgebraMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'QuaternionAlgebraMatrixTest class' category: #'Tests-Algebra-Matrices'!
QuaternionAlgebraMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractModuleTest category: #'Tests-Algebra-Modules'!
RandomizedTestCase subclass: #AbstractModuleTest
	instanceVariableNames: 'scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'AbstractModuleTest class' category: #'Tests-Algebra-Modules'!
AbstractModuleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractCommutativeRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #AbstractCommutativeRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'AbstractCommutativeRingModuleTest class' category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #AbstractVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'AbstractVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #BinaryVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #BinaryVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'BinaryVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
BinaryVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #CyclotomicFieldVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #CyclotomicFieldVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'CyclotomicFieldVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
CyclotomicFieldVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #FiniteFieldVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'FiniteFieldVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
FiniteFieldVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #GaussianFieldVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #GaussianFieldVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'GaussianFieldVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
GaussianFieldVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #QuadraticFieldVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #QuadraticFieldVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'QuadraticFieldVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
QuadraticFieldVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #RationalVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #RationalVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'RationalVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
RationalVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #TernaryVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #TernaryVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'TernaryVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
TernaryVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #CyclotomicIntegerModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #CyclotomicIntegerModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'CyclotomicIntegerModuleTest class' category: #'Tests-Algebra-Modules'!
CyclotomicIntegerModuleTest class
	instanceVariableNames: ''!

!classDefinition: #EisensteinIntegerModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #EisensteinIntegerModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'EisensteinIntegerModuleTest class' category: #'Tests-Algebra-Modules'!
EisensteinIntegerModuleTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldAffineAlgebraModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #FiniteFieldAffineAlgebraModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'FiniteFieldAffineAlgebraModuleTest class' category: #'Tests-Algebra-Modules'!
FiniteFieldAffineAlgebraModuleTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldDualNumbersModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #FiniteFieldDualNumbersModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'FiniteFieldDualNumbersModuleTest class' category: #'Tests-Algebra-Modules'!
FiniteFieldDualNumbersModuleTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #GaloisRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'GaloisRingModuleTest class' category: #'Tests-Algebra-Modules'!
GaloisRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingMultivariatePolynomialRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #GaloisRingMultivariatePolynomialRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'GaloisRingMultivariatePolynomialRingModuleTest class' category: #'Tests-Algebra-Modules'!
GaloisRingMultivariatePolynomialRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #GaussianIntegerModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #GaussianIntegerModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'GaussianIntegerModuleTest class' category: #'Tests-Algebra-Modules'!
GaussianIntegerModuleTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #IntegerModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'IntegerModuleTest class' category: #'Tests-Algebra-Modules'!
IntegerModuleTest class
	instanceVariableNames: ''!

!classDefinition: #LocalIntegerRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #LocalIntegerRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'LocalIntegerRingModuleTest class' category: #'Tests-Algebra-Modules'!
LocalIntegerRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #ModularIntegerModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModularIntegerModuleTest class' category: #'Tests-Algebra-Modules'!
ModularIntegerModuleTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerMultivariatePolynomialRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #ModularIntegerMultivariatePolynomialRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModularIntegerMultivariatePolynomialRingModuleTest class' category: #'Tests-Algebra-Modules'!
ModularIntegerMultivariatePolynomialRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #ProductRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #ProductRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ProductRingModuleTest class' category: #'Tests-Algebra-Modules'!
ProductRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #QuadraticRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #QuadraticRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'QuadraticRingModuleTest class' category: #'Tests-Algebra-Modules'!
QuadraticRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #RationalAffineAlgebraModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #RationalAffineAlgebraModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'RationalAffineAlgebraModuleTest class' category: #'Tests-Algebra-Modules'!
RationalAffineAlgebraModuleTest class
	instanceVariableNames: ''!

!classDefinition: #RationalDualNumbersModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #RationalDualNumbersModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'RationalDualNumbersModuleTest class' category: #'Tests-Algebra-Modules'!
RationalDualNumbersModuleTest class
	instanceVariableNames: ''!

!classDefinition: #RationalMultivariatePolynomialRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #RationalMultivariatePolynomialRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'RationalMultivariatePolynomialRingModuleTest class' category: #'Tests-Algebra-Modules'!
RationalMultivariatePolynomialRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #RationalPolynomialRingModuleTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeRingModuleTest subclass: #RationalPolynomialRingModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'RationalPolynomialRingModuleTest class' category: #'Tests-Algebra-Modules'!
RationalPolynomialRingModuleTest class
	instanceVariableNames: ''!

!classDefinition: #CliffordAlgebraModuleTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #CliffordAlgebraModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'CliffordAlgebraModuleTest class' category: #'Tests-Algebra-Modules'!
CliffordAlgebraModuleTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixAlgebraModuleTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #MatrixAlgebraModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'MatrixAlgebraModuleTest class' category: #'Tests-Algebra-Modules'!
MatrixAlgebraModuleTest class
	instanceVariableNames: ''!

!classDefinition: #QuaternionAlgebraModuleTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #QuaternionAlgebraModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'QuaternionAlgebraModuleTest class' category: #'Tests-Algebra-Modules'!
QuaternionAlgebraModuleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractGroebnerBasisTest category: #'Tests-Algebra-Groebner Bases'!
RandomizedTestCase subclass: #AbstractGroebnerBasisTest
	instanceVariableNames: 'scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Groebner Bases'!
!classDefinition: 'AbstractGroebnerBasisTest class' category: #'Tests-Algebra-Groebner Bases'!
AbstractGroebnerBasisTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldGroebnerBasisTest category: #'Tests-Algebra-Groebner Bases'!
AbstractGroebnerBasisTest subclass: #FiniteFieldGroebnerBasisTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Groebner Bases'!
!classDefinition: 'FiniteFieldGroebnerBasisTest class' category: #'Tests-Algebra-Groebner Bases'!
FiniteFieldGroebnerBasisTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingGroebnerBasisTest category: #'Tests-Algebra-Groebner Bases'!
AbstractGroebnerBasisTest subclass: #GaloisRingGroebnerBasisTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Groebner Bases'!
!classDefinition: 'GaloisRingGroebnerBasisTest class' category: #'Tests-Algebra-Groebner Bases'!
GaloisRingGroebnerBasisTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerGroebnerBasisTest category: #'Tests-Algebra-Groebner Bases'!
AbstractGroebnerBasisTest subclass: #IntegerGroebnerBasisTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Groebner Bases'!
!classDefinition: 'IntegerGroebnerBasisTest class' category: #'Tests-Algebra-Groebner Bases'!
IntegerGroebnerBasisTest class
	instanceVariableNames: ''!

!classDefinition: #ModularIntegerGroebnerBasisTest category: #'Tests-Algebra-Groebner Bases'!
AbstractGroebnerBasisTest subclass: #ModularIntegerGroebnerBasisTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Groebner Bases'!
!classDefinition: 'ModularIntegerGroebnerBasisTest class' category: #'Tests-Algebra-Groebner Bases'!
ModularIntegerGroebnerBasisTest class
	instanceVariableNames: ''!

!classDefinition: #RationalGroebnerBasisTest category: #'Tests-Algebra-Groebner Bases'!
AbstractGroebnerBasisTest subclass: #RationalGroebnerBasisTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Groebner Bases'!
!classDefinition: 'RationalGroebnerBasisTest class' category: #'Tests-Algebra-Groebner Bases'!
RationalGroebnerBasisTest class
	instanceVariableNames: ''!

!classDefinition: #RingTest category: #'Tests-Algebra'!
TestCase subclass: #RingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'RingTest class' category: #'Tests-Algebra'!
RingTest class
	instanceVariableNames: ''!

!classDefinition: #SimplicialComplexTest category: #'Tests-Algebra'!
TestCase subclass: #SimplicialComplexTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'SimplicialComplexTest class' category: #'Tests-Algebra'!
SimplicialComplexTest class
	instanceVariableNames: ''!

!classDefinition: #WordArrayTest category: #'Tests-Algebra-Arrays'!
TestCase subclass: #WordArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Arrays'!
!classDefinition: 'WordArrayTest class' category: #'Tests-Algebra-Arrays'!
WordArrayTest class
	instanceVariableNames: ''!

!classDefinition: #DisabledTest category: #'Tests-Algebra'!
Object subclass: #DisabledTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'DisabledTest class' category: #'Tests-Algebra'!
DisabledTest class
	instanceVariableNames: ''!

!classDefinition: #FiniteFieldRationalFunctionFieldMatrixTest category: #'Tests-Algebra-Matrices'!
Object subclass: #FiniteFieldRationalFunctionFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'FiniteFieldRationalFunctionFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
FiniteFieldRationalFunctionFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #RationalLocalPolynomialRingMatrixTest category: #'Tests-Algebra-Matrices'!
Object subclass: #RationalLocalPolynomialRingMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'RationalLocalPolynomialRingMatrixTest class' category: #'Tests-Algebra-Matrices'!
RationalLocalPolynomialRingMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #RationalRationalFunctionFieldMatrixTest category: #'Tests-Algebra-Matrices'!
Object subclass: #RationalRationalFunctionFieldMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'RationalRationalFunctionFieldMatrixTest class' category: #'Tests-Algebra-Matrices'!
RationalRationalFunctionFieldMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #Benchmark category: #'Tests-Algebra-Benchmarks'!
Object subclass: #Benchmark
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'Benchmark class' category: #'Tests-Algebra-Benchmarks'!
Benchmark class
	instanceVariableNames: ''!

!classDefinition: #AlgebraicNumbersBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #AlgebraicNumbersBenchmark
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'AlgebraicNumbersBenchmark class' category: #'Tests-Algebra-Benchmarks'!
AlgebraicNumbersBenchmark class
	instanceVariableNames: ''!

!classDefinition: #CharacteristicPolynomialOverIntegersBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #CharacteristicPolynomialOverIntegersBenchmark
	instanceVariableNames: 'M'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'CharacteristicPolynomialOverIntegersBenchmark class' category: #'Tests-Algebra-Benchmarks'!
CharacteristicPolynomialOverIntegersBenchmark class
	instanceVariableNames: ''!

!classDefinition: #DeterminantOverCommutativeRingBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #DeterminantOverCommutativeRingBenchmark
	instanceVariableNames: 'M'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'DeterminantOverCommutativeRingBenchmark class' category: #'Tests-Algebra-Benchmarks'!
DeterminantOverCommutativeRingBenchmark class
	instanceVariableNames: ''!

!classDefinition: #DeterminantOverPolynomialRingBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #DeterminantOverPolynomialRingBenchmark
	instanceVariableNames: 'M'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'DeterminantOverPolynomialRingBenchmark class' category: #'Tests-Algebra-Benchmarks'!
DeterminantOverPolynomialRingBenchmark class
	instanceVariableNames: ''!

!classDefinition: #FatemanBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #FatemanBenchmark
	instanceVariableNames: 'f p'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'FatemanBenchmark class' category: #'Tests-Algebra-Benchmarks'!
FatemanBenchmark class
	instanceVariableNames: ''!

!classDefinition: #GroebnerBasisBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #GroebnerBasisBenchmark
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'GroebnerBasisBenchmark class' category: #'Tests-Algebra-Benchmarks'!
GroebnerBasisBenchmark class
	instanceVariableNames: ''!

!classDefinition: #GroupBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #GroupBenchmark
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'GroupBenchmark class' category: #'Tests-Algebra-Benchmarks'!
GroupBenchmark class
	instanceVariableNames: ''!

!classDefinition: #IntegersBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #IntegersBenchmark
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'IntegersBenchmark class' category: #'Tests-Algebra-Benchmarks'!
IntegersBenchmark class
	instanceVariableNames: ''!

!classDefinition: #ModularMatrixBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #ModularMatrixBenchmark
	instanceVariableNames: 'field matrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'ModularMatrixBenchmark class' category: #'Tests-Algebra-Benchmarks'!
ModularMatrixBenchmark class
	instanceVariableNames: ''!

!classDefinition: #PolynomialBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #PolynomialBenchmark
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'PolynomialBenchmark class' category: #'Tests-Algebra-Benchmarks'!
PolynomialBenchmark class
	instanceVariableNames: ''!

!classDefinition: #SmallMatrixBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #SmallMatrixBenchmark
	instanceVariableNames: 'integerMatrix binaryMatrix modularMatrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'SmallMatrixBenchmark class' category: #'Tests-Algebra-Benchmarks'!
SmallMatrixBenchmark class
	instanceVariableNames: ''!

!classDefinition: #SparsePolynomialMultiplicationBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #SparsePolynomialMultiplicationBenchmark
	instanceVariableNames: 'p pPlusOne pBigCoefficients pBigExponents pBigCoefficientsPlusOne pBigExponentsPlusOne'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'SparsePolynomialMultiplicationBenchmark class' category: #'Tests-Algebra-Benchmarks'!
SparsePolynomialMultiplicationBenchmark class
	instanceVariableNames: ''!

!classDefinition: #CASBenchmark category: #'Tests-Algebra-Benchmarks'!
Object subclass: #CASBenchmark
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'CASBenchmark class' category: #'Tests-Algebra-Benchmarks'!
CASBenchmark class
	instanceVariableNames: ''!


!AlgebraicNumbersBenchmark commentStamp: '<historical>' prior: 0!
AlgebraicNumbersBenchmark new run

testComplexAlgebraicNumberAddition	1.1 per second. (25 May 2018)
testComplexAlgebraicNumberDivision	63.9 per second.
testComplexAlgebraicNumberProduct	68.3 per second.
testComplexAlgebraicNumberSquare	2,080 per second.
testRealAlgebraicNumberAddition	0.936 per second.
testRealAlgebraicNumberDivision	61.7 per second.
testRealAlgebraicNumberProduct	67.1 per second.
testRealAlgebraicNumberSquare	2,010 per second.

testComplexAlgebraicNumberAddition	0.907 per second. (18 May 2018)
testComplexAlgebraicNumberDivision	61 per second.
testComplexAlgebraicNumberProduct	64.7 per second.
testComplexAlgebraicNumberSquare	1,760 per second.
testRealAlgebraicNumberAddition	0.997 per second.
testRealAlgebraicNumberDivision	58.2 per second.
testRealAlgebraicNumberProduct	62.6 per second.
testRealAlgebraicNumberSquare	1,730 per second.

testComplexAlgebraicNumberAddition	0.84 per second.
testComplexAlgebraicNumberDivision	58.4 per second.
testComplexAlgebraicNumberProduct	61.6 per second.
testComplexAlgebraicNumberSquare	1,770 per second.
testRealAlgebraicNumberAddition	0.952 per second.
testRealAlgebraicNumberDivision	48.3 per second.
testRealAlgebraicNumberProduct	56.2 per second.
testRealAlgebraicNumberSquare	1,460 per second.

testComplexAlgebraicNumberAddition	1.08 per second. (29 Apr 2018)
testComplexAlgebraicNumberDivision	57.6 per second.
testComplexAlgebraicNumberProduct	61.2 per second.
testComplexAlgebraicNumberSquare	1,660 per second.
testRealAlgebraicNumberAddition	0.78 per second.
testRealAlgebraicNumberDivision	55.8 per second.
testRealAlgebraicNumberProduct	59.1 per second.
testRealAlgebraicNumberSquare	1,610 per second.' 
!

!CharacteristicPolynomialOverIntegersBenchmark commentStamp: 'len 12/9/2022 13:23:51' prior: 0!
CharacteristicPolynomialOverIntegersBenchmark new run

testCharacteristicPolynomial	2.23  seconds per run
testCharacteristicPolynomial	2.85  seconds per run!

!DeterminantOverCommutativeRingBenchmark commentStamp: '<historical>' prior: 0!
https://www.nemocas.org/benchmarks/

SageMath 6.8: 43.5s
Pari/GP 2.7.4: 456s
Magma v2.21-4: -
Nemo-0.4: 7.5s

DeterminantOverCommutativeRingBenchmark new run

testDeterminant	395.23  seconds per run
testDeterminant	376.23  seconds per run!

!DeterminantOverPolynomialRingBenchmark commentStamp: 'len 12/9/2022 13:17:21' prior: 0!
DeterminantOverPolynomialRingBenchmark new run

'testDeterminant	20.14  seconds per run'!

!GroebnerBasisBenchmark commentStamp: 'len 5/12/2018 18:35:32' prior: 0!
GroebnerBasisBenchmark new run

fixed katsura generators:
testCyclic3	10,600 per second.
testCyclic4	1,870 per second.
testCyclic5	6.69 per second.
testEliminationExample	3,280 per second.
testKatsura2	16,400 per second.
testKatsura3	3,990 per second.
testKatsura4	559 per second.
testModularCyclic3	9,680 per second.
testModularKatsura2	14,800 per second.
testModularKatsura3	4,110 per second.
testModularKatsura4	674 per second.

changed Buchberger>>run to do full reductions (calling reduceFull:).
This seems to be slower when the number of variables is low, but starts to pay off for n > 3
testCyclic3	10,200 per second.
testCyclic4	1,810 per second.
testCyclic5	6.56 per second.
testEliminationExample	3,260 per second.
testKatsura2	16,800 per second.
testKatsura3	4,230 per second.
testKatsura4	554 per second.
testModularCyclic3	9,810 per second.
testModularKatsura2	16,000 per second.
testModularKatsura3	4,470 per second.
testModularKatsura4	695 per second.

testCyclic3	14,400 per second. (11 May 2018, Buchberger>>run replaced Set by OrderedCollection)
testCyclic4	1,810 per second.
testCyclic5	3.37 per second.
testEliminationExample	3,950 per second.
testKatsura2	23,600 per second.
testKatsura3	6,120 per second.
testKatsura4	487 per second.
testModularCyclic3	13,300 per second.
testModularKatsura2	22,000 per second.
testModularKatsura3	6,270 per second.
testModularKatsura4	593 per second.

testCyclic3	14,600 per second. (10 May 2018, optimized Buchberger to reduce the polynomials from S before adding them to basis, fixed definition of the ideal of cyclic n-roots)
testCyclic4	1,860 per second.
testCyclic5	2.64 per second.
testEliminationExample	3,920 per second.
testKatsura2	23,900 per second.
testKatsura3	6,400 per second.
testKatsura4	496 per second.
testModularCyclic3	13,900 per second.
testModularKatsura2	22,500 per second.
testModularKatsura3	6,400 per second.
testModularKatsura4	613 per second.' 

testCyclic3	8,190 per second. (9 May 2018, optimized Buchberger>>run, added i0)
testCyclic4	2,920 per second.
testCyclic5	1,060 per second.
testCyclic6	381 per second.
testEliminationExample	2,820 per second.
testKatsura2	16,300 per second.
testKatsura3	3,930 per second.
testKatsura4	42.2 per second.
testModularCyclic3	7,820 per second.
testModularKatsura2	15,200 per second.
testModularKatsura3	3,630 per second.
testModularKatsura4	54.4 per second.

testCyclic3	4,730 per second. (25 Dec 2017, after refactoring MultivariatePolynomial and removing MonomialIndexedTuple)
testCyclic4	1,040 per second.
testCyclic5	258 per second.
testCyclic6	63.5 per second.
testEliminationExample	1,700 per second.
testKatsura2	13,900 per second.
testKatsura3	2,060 per second.
testKatsura4	16.7 per second.
testModularCyclic3	4,480 per second.
testModularKatsura2	12,800 per second.
testModularKatsura3	1,900 per second.
testModularKatsura4	22.3 per second.' 

testCyclic3	4,400 per second. (optimized GroebnerBasis>>reduce:)
testCyclic4	1,000 per second.
testCyclic5	238 per second.
testCyclic6	58 per second.
testEliminationExample	1,840 per second.
testKatsura2	13,600 per second.
testKatsura3	1,950 per second.
testKatsura4	15.6 per second.
testModularCyclic3	4,270 per second.
testModularKatsura2	12,600 per second.
testModularKatsura3	1,800 per second.
testModularKatsura4	20.6 per second.

testCyclic3	2,260 per second. (modular inversion using extended euclid)
testCyclic4	520 per second.
testCyclic5	124 per second.
testCyclic6	28.9 per second.
testEliminationExample	897 per second.
testKatsura2	6,690 per second.
testKatsura3	869 per second.
testKatsura4	10.6 per second.
testModularCyclic3	2,430 per second.
testModularKatsura2	7,470 per second.
testModularKatsura3	1,050 per second.
testModularKatsura4	15.5 per second.

testCyclic3	2,230 per second. (polynomials as sorted dictionaries monomial -> coefficient)
testCyclic4	512 per second.
testCyclic5	121 per second.
testCyclic6	28.7 per second.
testEliminationExample	867 per second.
testKatsura2	6,700 per second.
testKatsura3	860 per second.
testKatsura4	11 per second.
testModularCyclic3	2,320 per second.
testModularKatsura2	6,960 per second.
testModularKatsura3	858 per second.
testModularKatsura4	12.1 per second.

testCyclic3	2,270 per second. (polynomials as dictionaries monomial -> coefficient)
testCyclic4	511 per second.
testCyclic5	118 per second.
testCyclic6	26.1 per second.
testEliminationExample	816 per second.
testKatsura2	6,750 per second.
testKatsura3	866 per second.
testKatsura4	11.8 per second.
testModularCyclic3	2,410 per second.
testModularKatsura2	6,780 per second.
testModularKatsura3	844 per second.
testModularKatsura4	13.3 per second.

testCyclic3	2,240 per second. (with Groebner bases kept sorted, GroebnerBasis>>initialize)
testCyclic4	520 per second.
testCyclic5	120 per second.
testCyclic6	26 per second.
testEliminationExample	720 per second.
testKatsura2	6,730 per second.
testKatsura3	755 per second.
testKatsura4	34.9 per second.
testModularCyclic3	2,310 per second.
testModularKatsura2	6,720 per second.
testModularKatsura3	727 per second.
testModularKatsura4	33 per second.

testCyclic3	2,140 per second. (reverted Monomial>>hash)
testCyclic4	676 per second.
testCyclic5	112 per second.
testCyclic6	36.2 per second.
testEliminationExample	773 per second.
testKatsura2	6,430 per second.
testKatsura3	821 per second.
testKatsura4	7.72 per second.
testModularCyclic3	2,220 per second.
testModularKatsura2	6,490 per second.
testModularKatsura3	797 per second.
testModularKatsura4	12.6 per second.

testCyclic3	2,240 per second. (defined Monomial>>hash as 'exponents hash')
testCyclic4	682 per second.
testCyclic5	116 per second.
testCyclic6	40.4 per second.
testEliminationExample	827 per second.
testKatsura2	6,650 per second.
testKatsura3	857 per second.
testKatsura4	7.82 per second.
testModularCyclic3	2,370 per second.
testModularKatsura2	6,840 per second.
testModularKatsura3	861 per second.
testModularKatsura4	13.6 per second.

testCyclic3	1,780 per second. (spur 32 bits)
testCyclic4	354 per second.
testCyclic5	26.8 per second.
testCyclic6	12.2 per second.
testEliminationExample	614 per second.
testKatsura2	5,540 per second.
testKatsura3	130 per second.
testKatsura4	10.7 per second.
testModularCyclic3	177 per second.
testModularKatsura2	194 per second.

[self new modularCyclic: 7 :: groebnerBasis] bench
 '3.63 per second.' (spur 32 bits)
 '165 seconds.' 

testCyclic3	1,300 per second.
testCyclic4	264 per second.
testCyclic5	60.1 per second.
testCyclic6	9.7 per second.
testElimination	143 per second.
testKatsura2	4,230 per second.
testKatsura3	407 per second.
testKatsura4	4.06 per second.
testModularCyclic3	175 per second.
testModularKatsura2	194 per second. 

testCyclic3	1,130 per second.
testCyclic4	195 per second.
testCyclic5	48.7 per second.
testCyclic6	9.31 per second.
testElimination	116 per second.
testKatsura2	3,440 per second.
testKatsura3	354 per second.
testKatsura4	2.8 per second.
testModularCyclic3	149 per second.
testModularKatsura2	184 per second.'

'testCyclic3	1,080 per second.
testCyclic4	36.8 per second.
testCyclic5	1.19 per second.
testElimination	77.1 per second.
testKatsura2	3,810 per second.
testKatsura3	26.1 per second.
testModularCyclic3	173 per second.
testModularKatsura2	201 per second.' 

testCyclic3	1,060 per second.
testCyclic4	56.3 per second.
testCyclic5	1.08 per second.
testElimination	241 per second.
testKatsura2	4,790 per second.
testKatsura3	11.5 per second.
testModularCyclic3	173 per second.
testModularKatsura2	200 per second.

testCyclic3	615 per second.
testCyclic4	29.2 per second.
testCyclic5	1.38 per second.
testElimination	43.5 per second.
testKatsura2	2,330 per second.
testKatsura3	7.21 per second.

testCyclic3	632 per second.
testCyclic4	4.34 per second.
testElimination	3.45 per second.
testKatsura2	2,190 per second.
testKatsura3	0.288 per second.

testCyclic3	524 per second.
testCyclic4	4.85 per second.
testElimination	2.82 per second.
testKatsura2	1,950 per second.
testKatsura3	0.251 per second.

testCyclic3	540 per second.
testCyclic4	3.73 per second.
testElimination	2 per second.
testKatsura2	2,130 per second.
testKatsura3	0.257 per second.

testCyclic3	494 per second.
testCyclic4	2.41 per second.
testElimination	1.34 per second.
testKatsura2	1,950 per second.
testKatsura3	5.38 seconds.

testCyclic3	393 per second.
testCyclic4	1.62 per second.
testElimination	1.29 per second.
testKatsura2	1,830 per second.
testKatsura3	6.58 seconds.

old:
testCyclic3	312 per second.
testElimination	0.911 per second.
testKatsura2	1,260 per second.
!

!GroupBenchmark commentStamp: '<historical>' prior: 0!
GroupBenchmark new run

testJ1Order	3.04  runs per second (cog 64)
testM12Order	58.29  runs per second

testJ1Order	3.95 per second. (spur 32 bits)
testM12Order	86.7 per second.

testJ1Order	2.39 per second.
testM12Order	57.4 per second.

testJ1Order	2.51 per second.
testM12Order	56.9 per second.
!

!IntegersBenchmark commentStamp: 'len 11/22/2016 11:57:33' prior: 0!
self new run

test1000Factorial	5,620 per second. (spur 32 bits)

test1000Factorial	1,250 per second.
!

!ModularMatrixBenchmark commentStamp: '<historical>' prior: 0!
Some tests on a 300x300 matrix over Z/32003.

self new run

testAddition	504.30  runs per second
testGaussianElimination	2.16  runs per second
testImage	1.19  seconds per run
testKernel	1.26  seconds per run
testSquared	1.39  runs per second
testTransposition	607.39  runs per second

testAddition	392.93  runs per second
testGaussianElimination	1.90  runs per second
testImage	1.50  runs per second
testKernel	1.41  seconds per run
testSquared	1.36  runs per second
testTransposition	651.41  runs per second' 

testAddition	384 per second.
testGaussianElimination	1.88 per second.
testImage	1.84 per second.
testKernel	0.736 per second.
testSquared	1.39 per second.
testTransposition	89.8 per second.

testAddition	387 per second.
testGaussianElimination	1.82 per second.
testSquared	1.43 per second.
testTransposition	89.7 per second.

testAddition	203 per second.
testGaussianElimination	1.81 per second.
testSquared	1.32 per second.
testTransposition	66.9 per second.!

!PolynomialBenchmark commentStamp: 'len 5/26/2018 19:40:36' prior: 0!
PolynomialBenchmark new run

testAdditionOverZ	25.4 per second.
testAdditionOverZ2	283 per second.
testDivisionOverQ	5.23 per second.
testDivisionOverZ	5.25 per second.
testFactorizationOverZ17	10.2 per second
testFiniteField	20.1 per second.
testFiniteFieldPolynomialFactorization	336 per second.
testGCDOverQ	8.2 per second.
testGCDOverZ	4.81 per second.
testGCDOverZ2	10.3 per second.
testMultiplication2OverZ	154 per second.
testMultiplication2OverZ2	681 per second.
testMultiplicationOverZ	2.7 per second.
testMultiplicationOverZ2	4,060 per second.
testMultivariatePolynomialAdditionOverZ	106 per second.
testPseudoDivision	5,060 per second.
testPseudoRemainder	5,050 per second.
testRealRoots	24.4 per second.

testFiniteField	18.2 per second. (25 Dec 2017, after refactoring MultivariatePolynomial and removing MonomialIndexedTuple)
testFiniteFieldPolynomialFactorization	255 per second.
testMultivariatePolynomialAdditionOverZ	102 per second.
testPolynomialAdditionOverZ	33.4 per second.
testPolynomialAdditionOverZ2	269 per second.
testPolynomialDivisionOverQ	3.32 per second.
testPolynomialDivisionOverZ	2.96 per second.
testPolynomialGCDOverQ	3.56 per second.
testPolynomialGCDOverZ	4.74 per second.
testPolynomialGCDOverZ2	24.6 per second.
testPolynomialMultiplication2OverZ	168 per second.
testPolynomialMultiplication2OverZ2	686 per second.
testPolynomialMultiplicationOverZ	2.72 per second.
testPolynomialMultiplicationOverZ2	4,090 per second.
testPolynomialPseudoDivision	4,840 per second.
testPolynomialPseudoRemainder	4,810 per second.
testPolynomialRealRoots	40.7 per second.
testRealAlgebraicNumberAddition	2.9 per second.
testRealAlgebraicNumberDivision	241 per second.
testRealAlgebraicNumberProduct	308 per second.
testRealAlgebraicNumberSquare	5,630 per second.

testFiniteField	17.1 per second.
testFiniteFieldPolynomialFactorization	97.8 per second.
testMultivariatePolynomialAdditionOverZ	93.9 per second.
testPolynomialAdditionOverZ	32.6 per second.
testPolynomialAdditionOverZ2	136 per second.
testPolynomialDivisionOverQ	2.63 per second.
testPolynomialDivisionOverZ	2.74 per second.
testPolynomialGCDOverQ	2.88 per second.
testPolynomialGCDOverZ	4.3 per second.
testPolynomialGCDOverZ2	11.3 per second.
testPolynomialMultiplication2OverZ	163 per second.
testPolynomialMultiplication2OverZ2	628 per second.
testPolynomialMultiplicationOverZ	2.7 per second.
testPolynomialMultiplicationOverZ2	3,760 per second.
testPolynomialPseudoDivision	4,110 per second.
testPolynomialPseudoRemainder	4,080 per second.
testPolynomialRealRoots	39.7 per second.
testRealAlgebraicNumberAddition	5.44 seconds.
testRealAlgebraicNumberDivision	154 per second.
testRealAlgebraicNumberProduct	177 per second.
testRealAlgebraicNumberSquare	4,200 per second.

testFiniteField	17.5 per second. (spur 32 bits)
testFiniteFieldPolynomialFactorization	81.3 per second.
testMultivariatePolynomialAdditionOverZ	86.8 per second.
testPolynomialAdditionOverZ	31.2 per second.
testPolynomialAdditionOverZ2	135 per second.
testPolynomialDivisionOverQ	2.57 per second.
testPolynomialDivisionOverZ	2.64 per second.
testPolynomialGCDOverQ	2.44 per second.
testPolynomialGCDOverZ	3.51 per second.
testPolynomialGCDOverZ2	9.86 per second.
testPolynomialMultiplication2OverZ	105 per second.
testPolynomialMultiplication2OverZ2	627 per second.
testPolynomialMultiplicationOverZ	2.72 per second.
testPolynomialMultiplicationOverZ2	3,400 per second.
testPolynomialPseudoDivision	3,660 per second.
testPolynomialPseudoRemainder	3,640 per second.
testPolynomialRealRoots	38.7 per second.
testRealAlgebraicNumberAddition	5.27 seconds.
testRealAlgebraicNumberDivision	148 per second.
testRealAlgebraicNumberProduct	170 per second.
testRealAlgebraicNumberSquare	3,970 per second.

testFiniteField	14.6 per second.
testFiniteFieldPolynomialFactorization	58.4 per second.
testMultivariatePolynomialAdditionOverZ	51.7 per second.
testPolynomialAdditionOverZ	15.8 per second.
testPolynomialAdditionOverZ2	94 per second.
testPolynomialDivisionOverQ	1.66 per second.
testPolynomialDivisionOverZ	1.67 per second.
testPolynomialGCDOverQ	1.54 per second.
testPolynomialGCDOverZ	2.22 per second.
testPolynomialGCDOverZ2	7.01 per second.
testPolynomialMultiplication2OverZ	78 per second.
testPolynomialMultiplication2OverZ2	384 per second.
testPolynomialMultiplicationOverZ	0.598 per second.
testPolynomialMultiplicationOverZ2	2,220 per second.
testPolynomialPseudoDivision	2,300 per second.
testPolynomialPseudoRemainder	2,300 per second.
testPolynomialRealRoots	27.3 per second.
testRealAlgebraicNumberAddition	5.24 seconds.
testRealAlgebraicNumberDivision	110 per second.
testRealAlgebraicNumberProduct	132 per second.
testRealAlgebraicNumberSquare	2,720 per second.

testFiniteField	11.1 per second.
testFiniteFieldPolynomialFactorization	58 per second.
testMultivariatePolynomialAdditionOverZ	FAILED
testPolynomialAdditionOverZ	11.9 per second.
testPolynomialAdditionOverZ2	76.2 per second.
testPolynomialDivisionOverQ	1.23 per second.
testPolynomialDivisionOverZ	1.12 per second.
testPolynomialGCDOverQ	1.16 per second.
testPolynomialGCDOverZ	1.45 per second.
testPolynomialGCDOverZ2	4.64 per second.
testPolynomialMultiplication2OverZ	33.9 per second.
testPolynomialMultiplication2OverZ2	58.4 per second.
testPolynomialMultiplicationOverZ	0.588 per second.
testPolynomialMultiplicationOverZ2	2,260 per second.
testPolynomialPseudoDivision	1,210 per second.
testPolynomialPseudoRemainder	1,220 per second.
testPolynomialRealRoots	22.8 per second.
testRealAlgebraicNumberAddition	5.28 seconds.
testRealAlgebraicNumberDivision	74.1 per second.
testRealAlgebraicNumberProduct	88.6 per second.
testRealAlgebraicNumberSquare	2,020 per second.

testFiniteField	10.8 per second.
testFiniteFieldPolynomialFactorization	55.1 per second.
testMultivariatePolynomialAdditionOverZ	FAILED
testPolynomialAdditionOverZ	11.9 per second.
testPolynomialAdditionOverZ2	69 per second.
testPolynomialDivisionOverQ	1.2 per second.
testPolynomialDivisionOverZ	1.04 per second.
testPolynomialGCDOverQ	1.12 per second.
testPolynomialGCDOverZ	1.38 per second.
testPolynomialGCDOverZ2	4.25 per second.
testPolynomialMultiplication2OverZ	33 per second.
testPolynomialMultiplication2OverZ2	61.4 per second.
testPolynomialMultiplicationOverZ	0.578 per second.
testPolynomialMultiplicationOverZ2	2,220 per second.
testPolynomialPseudoDivision	1,220 per second.
testPolynomialPseudoRemainder	1,220 per second.
testPolynomialRealRoots	23.1 per second.
testRealAlgebraicNumberAddition	6.11 seconds.
testRealAlgebraicNumberDivision	78.2 per second.
testRealAlgebraicNumberProduct	90.3 per second.
testRealAlgebraicNumberSquare	2,020 per second.

testFiniteField	11.3 per second.
testFiniteFieldPolynomialFactorization	54.5 per second.
testPolynomialAdditionOverZ	10.8 per second.
testPolynomialAdditionOverZ2	75 per second.
testPolynomialDivisionOverQ	1.17 per second.
testPolynomialDivisionOverZ	1.06 per second.
testPolynomialGCDOverQ	1.11 per second.
testPolynomialGCDOverZ	1.38 per second.
testPolynomialGCDOverZ2	4.53 per second.
testPolynomialMultiplication2OverZ	32 per second.
testPolynomialMultiplication2OverZ2	58 per second.
testPolynomialMultiplicationOverZ	0.563 per second.
testPolynomialMultiplicationOverZ2	2,120 per second.
testPolynomialPseudoDivision	1,220 per second.
testPolynomialPseudoRemainder	1,190 per second.
testPolynomialRealRoots	23 per second.
testRealAlgebraicNumberAddition	5.96 seconds.
testRealAlgebraicNumberDivision	70.8 per second.
testRealAlgebraicNumberProduct	81.7 per second.
testRealAlgebraicNumberSquare	1,820 per second!

!SmallMatrixBenchmark commentStamp: 'len 1/22/2018 20:06:14' prior: 0!
This benchmark tests some basic operations on 64x64 matrices over Z, Z/2Z, and Z/32003Z.

self new run

testAdditionZ	7,830 per second. (23/1/2018)
testAdditionZ2	216,000 per second.
testAdditionZp	8,140 per second.
testCopyZ	158,000 per second.
testCopyZ2	448,000 per second.
testCopyZp	176,000 per second.
testImageZ	FAILED
testImageZ2	35.5 per second.
testImageZp	141 per second.
testKernelZ	FAILED
testKernelZ2	14.4 per second.
testKernelZp	61.5 per second.
testMultiplicationZ	164 per second.
testMultiplicationZ2	171 per second.
testMultiplicationZp	130 per second.
testTranspositionZ	4,520 per second.
testTranspositionZ2	1,400 per second.
testTranspositionZp	1,930 per second.

testAdditionZ	7,770 per second.
testAdditionZ2	88,100 per second.
testAdditionZp	8,140 per second.
testCopyZ	145,000 per second.
testCopyZ2	441,000 per second.
testCopyZp	164,000 per second.
testImageZ	FAILED
testImageZ2	31.2 per second.
testImageZp	133 per second.
testKernelZ	FAILED
testKernelZ2	13.7 per second.
testKernelZp	59.7 per second.
testMultiplicationZ	169 per second.
testMultiplicationZ2	158 per second.
testMultiplicationZp	128 per second.
testTranspositionZ	4,360 per second.
testTranspositionZ2	1,090 per second.
testTranspositionZp	1,960 per second.

testAdditionZ	7,960 per second.
testAdditionZ2	85,700 per second.
testAdditionZp	7,710 per second.
testCopyZ	162,000 per second.
testCopyZ2	477,000 per second.
testCopyZp	181,000 per second.
testMultiplicationZ	174 per second.
testMultiplicationZ2	161 per second.
testMultiplicationZp	130 per second.
testTranspositionZ	4,600 per second.
testTranspositionZ2	1,090 per second.
testTranspositionZp	2,110 per second.

testAdditionZ	7,840 per second. (spur 32 bits)
testAdditionZ2	86,600 per second.
testCopyZ	144,000 per second.
testCopyZ2	471,000 per second.
testMultiplicationZ	171 per second.
testMultiplicationZ2	159 per second.
testTranspositionZ	4,390 per second.
testTranspositionZ2	1,010 per second.

testAdditionZ	5,220 per second.
testAdditionZ2	42,600 per second.
testCopyZ	84,700 per second.
testCopyZ2	210,000 per second.
testMultiplicationZ	122 per second.
testMultiplicationZ2	106 per second.
testTranspositionZ	2,020 per second.
testTranspositionZ2	603 per second.

testAdditionZ	4,500 per second.
testAdditionZ2	33,700 per second.
testCopyZ	58,200 per second.
testCopyZ2	164,000 per second.
testMultiplicationZ	20.1 per second.
testMultiplicationZ2	7.81 per second.
testTranspositionZ	1,580 per second.
testTranspositionZ2	565 per second.
!

!SparsePolynomialMultiplicationBenchmark commentStamp: 'len 11/25/2022 11:20:32' prior: 0!
Timings giving in [1] on computers with 0.8ghz - 1ghz intel cpus: general purpose system 20s-100s, tuned systems 2.3s-10s. For big coefficients: singular 28s, pari 77s.

[1] R.J. Fateman, Comparing the speed of sparse polynomial multiplication.


self new run

testBigCoefficientsProduct	5.25  seconds per run (25 Nov 2022)
testBigExponentsProduct	2.00  seconds per run
testProduct	2.00  seconds per run

testBigCoefficientsProduct	8.5 seconds. (25 Dec 2017, after refactoring MultivariatePolynomial and removing MonomialIndexedTuple)
testBigExponentsProduct	6.38 seconds.
testProduct	0.295 per second.

testBigCoefficientsProduct	8.39 seconds. (polynomials as sorted dictionaries monomial -> coefficient)
testBigExponentsProduct	9.86 seconds.
testProduct	0.328 per second.

testBigCoefficientsProduct	10.5 seconds. (polynomials as dictionaries monomial -> coefficient)
testBigExponentsProduct	10.2 seconds.
testProduct	0.289 per second.
!

!CASBenchmark commentStamp: '<historical>' prior: 0!
These benchmarks show some of the fundamental bottlenecks for a computational algebra system in pure Smalltalk. Many important algorithms end up doing these operations.

To run the benchmarks, just doIt:
self new run

Cuis 4.5 with Cog 4.5 #9 (r3370, 2015-06-04), 32 bits, Intel i5-3210M @ 2.5Ghz, cache 3072 KB, 4990 bobomips, running inside virtualbox
testEqualsBytes	40,600 per second.
testEqualsWords	36,100 per second.
testHashBytes	245,000 per second.
testHashWords	23,500 per second.
testModularConvolutionBytes	20.6 per second.
testModularConvolutionWords	16.9 per second.
testModularDotProductBytes	36,900 per second.
testModularDotProductWords	30,400 per second.
testModularSumBytes	26,200 per second.
testModularSumWords	21,600 per second.
testPermutationCompositionArray	33,900 per second.
testPermutationCompositionWords	31,400 per second.

Cuis 5.0 with Cog 5.0-201611161032, 32 bits, Intel i5-3210M @ 2.5Ghz, cache 3072 KB, 4990 bobomips, running inside virtualbox
testEqualsBytes	59,900 per second.
testEqualsWords	49,600 per second.
testHashBytes	249,000 per second.
testHashWords	23,000 per second.
testModularConvolutionBytes	27.2 per second.
testModularConvolutionWords	23.9 per second.
testModularDotProductBytes	41,000 per second.
testModularDotProductWords	37,000 per second.
testModularSumBytes	37,900 per second.
testModularSumWords	34,100 per second.
testPermutationCompositionArray	69,100 per second.
testPermutationCompositionWords	51,900 per second.
!

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:29:17'!
testAbelianGroup: G
	| zero a b c |
	zero _ G identity.
	a _ G atRandomBits: 100.
	b _ G atRandomBits: 100.
	c _ G atRandomBits: 100.
	self assert: zero + a = a.
	self assert: a + zero = a.
	self assert: a + b = (b + a). "commutativity"
	self assert: (a + b + c) = (a + (b + c)). "associativity"
	self assert: a + a negated = zero! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/14/2022 08:38:09'!
testAbelianGroupHomomorphism: φ
	| G H a b |
	G _ φ domain.
	H _ φ codomain.
	a _ G atRandomBits: 100.
	b _ G atRandomBits: 100.
	self assert: (φ value: G identity) = H identity.
	self assert: (φ value: a + b) = ((φ value: a) + (φ value: b))! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:14:44'!
testAsPermutationGroup
	| A |
	A _ (ℤ/2 × (ℤ/5) × (ℤ/4) × (ℤ/2)) asAbelianGroup.
	self assert: A asPermutationGroup asAbelianGroup invariants = A invariants! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 16:51:45'!
testDirectSum
	| A B C |
	A _ ℤ asAbelianGroup.
	B _ (ℤ / (2 to: 10) atRandom) asAbelianGroup.
	C _ A ⊕ B.
	self testAbelianGroup: C.
	C ⇉ (A, B) do: [:each| self testAbelianGroupHomomorphism: each].
	C ⇇ (A, B) do: [:each| self testAbelianGroupHomomorphism: each]! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:15:41'!
testExponent
	| G |
	G _ (ℤ/2 × (ℤ/3) × (ℤ/7)) asAbelianGroup.
	self assert: G exponent = 42.
	G _ (ℤ/2 × (ℤ/4) × (ℤ/6)) asAbelianGroup.
	self assert: G exponent = 12! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:16:12'!
testFreeAbelianGroup
	| G |
	G _ ℤ^2.
	self assert: G rank = 2.
	self assert: G invariants = #(0 0).
	self assert: G torsion invariants = #().
	self assert: G torsion isTrivial.
	self assert: G isTorsion not.
	self assert: G isFree.
	self assert: G torsion isTorsion.
	self assert: G torsion isFree "because it's trivial"! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 08:53:07'!
testIntegerTuples
	| M |
	M _ ℤ ^ (1 to: 10) atRandom.
	self testAbelianGroup: M asAbelianGroup.
	self testAbelianGroupHomomorphism: (M projection: (1 to: M rank) atRandom) asAbelianGroupMap! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2020 08:16:40'!
testIntegers
	| G x1 x2 g1 g2 |
	G _ ℤ asAbelianGroup.
	self testAbelianGroup: G.
	x1 _ ℤ atRandomBits: 100.
	x2 _ ℤ atRandomBits: 100.
	g1 _ G !! x1.
	g2 _ G !! x2.
	self assert: ℤ !! g1 = x1.
	self assert: ℤ !! G identity = 0.
	self assert: ℤ !! (g1 + g2) = (x1 + x2).
	self assert: ℤ !! g1 negated = x1 negated.! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/22/2020 06:50:23'!
testInvariants
	self assert: (ℤ/1 × (ℤ/3) × (ℤ/5) × (ℤ/12)) asAbelianGroup invariants = #(3 60).
	self assert: (ℤ/2 × (ℤ/3)) asAbelianGroup invariants = #(6).
	self assert: (ℤ/6) asAbelianGroup invariants = #(6).
	self assert: (ℤ/2 × (ℤ/6)) asAbelianGroup invariants = #(2 6).
	self assert: (ℤ/3 × (ℤ/4) × (ℤ/5)) asAbelianGroup invariants = #(60).
	self assert: (ℤ × (ℤ/6)) asAbelianGroup invariants = #(0 6)! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2020 09:02:52'!
testInvariantsAndPrimaryInvariants
	| invariants primaryInvariants A |
	invariants _ {2^2*3*(5^2). 2^5*(3^3)*(5^2)}.
	primaryInvariants _ {2^2. 2^5. 3. 3^3. 5^2. 5^2} asBag.
	A _ ℤ^0.
	A _ invariants shuffled inject: ℤ^0 into: [:sum :each| sum ⊕ (ℤ/each) asAbelianGroup].
	self assert: A invariants = invariants.
	self assert: A primaryInvariants = primaryInvariants.
	A _ primaryInvariants asArray shuffled inject: ℤ^0 into: [:sum :each| sum ⊕ (ℤ/each) asAbelianGroup].
	self assert: A invariants = invariants.
	self assert: A primaryInvariants = primaryInvariants! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/22/2020 06:47:24'!
testIsCyclic
	| G |
	G _ (ℤ/2 × (ℤ/3)) asAbelianGroup.
	self assert: G isCyclic.
	G _ (ℤ/6) asAbelianGroup.
	self assert: G isCyclic.
	G _ (ℤ/2 × (ℤ/2)) asAbelianGroup.
	self assert: G isCyclic not.
	G _ (ℤ/2 × (ℤ/4)) asAbelianGroup.
	self assert: G isCyclic not.
	G _ ℤ^0.
	self assert: G isCyclic.
	G _ (ℤ/1) asAbelianGroup.
	self assert: G isCyclic.
	G _ (ℤ/3 × (ℤ/4) × (ℤ/5)) asAbelianGroup.
	self assert: G isCyclic! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/22/2020 06:45:43'!
testIsTrivial
	| G |
	G _ ℤ^0.
	self assert: G isTrivial.
	G _ (ℤ/1) asAbelianGroup.
	self assert: G isTrivial.
	G _ (ℤ/1 × (ℤ/1)) asAbelianGroup.
	self assert: G isTrivial.
	G _ (ℤ/1 × (ℤ/1) × (ℤ/2)) asAbelianGroup.
	self assert: G isTrivial not.
	G _ (ℤ/1 × (ℤ/1) × ℤ) asAbelianGroup.
	self assert: G isTrivial not.
	G _ (ℤ/2) asAbelianGroup.
	self assert: G isTrivial not.
	G _ ℤ asAbelianGroup.
	self assert: G isTrivial not! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:24:31'!
testMixedAbelianGroup
	| G |
	G _ (ℤ × ℤ × (ℤ/2) × (ℤ/3)) asAbelianGroup.
	self assert: G rank = 2.
	self assert: G primaryInvariants = #(0 0 2 3) asBag.
	self assert: G torsion primaryInvariants = #(2 3) asBag.
	self assert: G isTorsion not.
	self assert: G isFree not.
	self assert: G isMixed.
	self assert: G torsion isTorsion.
	self assert: G torsion isFree not.
	self assert: G torsionFree isFree.
	self assert: G torsionFree rank = 2! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 08:55:36'!
testModularIntegers
	| R |
	R _ ℤ / (2 to: 10) atRandom.
	self testAbelianGroup: R asAbelianGroup.
	self testAbelianGroupHomomorphism: R projection asAbelianGroupMap! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:39:42'!
testOrder
	self assert: (ℤ/2 × (ℤ/3)) asAbelianGroup size = 6.
	self assert: (ℤ × (ℤ/2) × (ℤ/3)) asAbelianGroup size isInfinite! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 7/9/2023 18:02:08'!
testOrderOfElements
	| G g |
	G := (ℤ × (ℤ/2) × (ℤ/3) × (ℤ/4) × (ℤ/5)) asAbelianGroup.
	g := G !! (0,1,2,0,3).
	self assert: g order = 30.
	g := G !! (1,0,0,0,1).
	self assert: g order = 0! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 7/11/2023 17:53:48'!
testTensorProductExample
	| G₁ G₂ |
	G₁ := (ℤ/100) asAbelianGroup.
	G₂ := (ℤ/125) asAbelianGroup.
	self assert: G₁ ⊗ G₂ ~ (ℤ/25) asAbelianGroup! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:26:02'!
testTorsionAbelianGroup
	| G |
	G _ (ℤ/2 × (ℤ/3)) asAbelianGroup.
	self assert: G rank = 0.
	self assert: G primaryInvariants = #(2 3) asBag.
	self assert: G = G torsion.
	self assert: G isTorsion.
	self assert: G isFree not.
	self assert: G isMixed not.
	self assert: G torsionFree isTrivial! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 6/26/2023 14:38:17'!
testAffinePlaneCurveMultiplicity
	| A C |
	A := AffineSpace new: #(x y) over: ℚ.
	C := A sub: A coordinateRing !! [:x :y| y^2 - (x^3)].
	self assert: (C multiplicityAt: (1,1)) = 1.
	self assert: (C multiplicityAt: (0,0)) = 2! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 6/23/2023 20:05:01'!
testAffineSchemeCartesianProduct1
	| X₁ X₂ P |
	X₁ := AffineVariety circle × ℚ spec.
	X₂ := AffineVariety romanSurface × ℚ spec.
	P := X₁ × X₂.
	self assert: (P ambient ⇉ (X₁ ambient, X₂ ambient) collect: [:each| P apply: each]) = {X₁. X₂}.
	self assert: X₁ dimension + X₂ dimension = P dimension.
	self assert: X₁ ambient dimension + X₂ ambient dimension = P ambient dimension! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 6/23/2023 20:05:22'!
testAffineSchemeCartesianProduct2
	| X₁ X₂ P |
	X₁ := AffineVariety circle.
	X₂ := AffineVariety romanSurface.
	P := X₁ × X₂.
	self assert: (𝔽₃ spec ⇒ X₁) size * (𝔽₃ spec ⇒ X₂) size = (𝔽₃ spec ⇒ P) size.
	self assert: (P satisfies: (5,2,6,2,6) % 7)! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 6/26/2023 14:38:25'!
testAffineSchemeCreation
	| A x y z X Y |
	A := AffineSpace new: 3 over: ℚ.
	x := A coordinateRing x.
	y := A coordinateRing y.
	z := A coordinateRing z.
	X := A sub: x - y.
	Y := X sub: {x^2 - (z^3). y^3 - (z^4)}.
	self assert: X ambient = A.
	"Y is defined as subscheme of X, so it contains the equations of X plus the two new equations, but its ambient space is still A (not X):"
	self assert: Y ambient = A! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 6/23/2023 20:07:34'!
testAffineSpace
	| K A |
	K := ℤ/23.
	A := AffineSpace new: 3 over: K.
	self assert: A dimension = 3.
	self assert: A scalars = K.
	self assert: A coordinateRing = (K polynomialsIn: 3).
	self assert: A ambient = A.
	self assert: A coordinateRing spec = A! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 6/28/2023 00:17:09'!
testProjectiveClosure
	"From Magma Handbook example Scheme_projective-closure (H118E23)."
	| A1 u v X PX X2 |
	A1 := (ℤ/5 polynomialsIn: #(u v)) spec.
	u := A1 coordinateRing x: 1.
	v := A1 coordinateRing x: 2.
	X := A1 sub: u^2 - (v^5).
	PX := X projectiveClosure.
	self assert: X projectiveClosure == PX.
	self assert: PX ambient == A1 projectiveClosure.
	self assert: (PX patch: 3) == X.
	X2 := PX patch: 2.
	self assert: X2 projectiveClosure == X projectiveClosure! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 6/28/2023 00:06:31'!
testProjectiveEmbedding
	| A u v X PX affinePointsEmbedded projectivePoints |
	A := AffineSpace new: #(u v) over: ℤ/5.
	u := A coordinateRing x: 1.
	v := A coordinateRing x: 2.
	X := A sub: u^2 - (v^5).
	PX := X projectiveClosure.
	affinePointsEmbedded := X points asSet apply: X → PX.
	projectivePoints := PX points asSet.
	self assert: (projectivePoints includesAllOf: affinePointsEmbedded).
	"the projective scheme has one more point at infinity:"
	self assert: affinePointsEmbedded size + 1 = projectivePoints size! !

!AlgebraicGeometryTest methodsFor: 'conics' stamp: 'len 11/24/2022 19:12:48'!
testConicCoefficients
	self assert: (Conic coefficients: (1,2,3,4,5,6)) coefficients = (1,2,3,4,5,6).
	self assert: (Conic polynomial: (ℚ polynomialsIn: 3) !! [:x :y :z| x^2 + (x*y*5) + (y^2) + (z^2)]) coefficients = (ℚ !!!! (1,5,0,1,0,1))! !

!AlgebraicGeometryTest methodsFor: 'conics' stamp: 'len 6/23/2023 20:08:39'!
testConicDeterminant
	self assert: (Conic coefficients: ℚ !!!! #(1 2 3 4 5 6)) determinant = (41/4)! !

!AlgebraicGeometryTest methodsFor: 'conics' stamp: 'len 6/23/2023 20:09:15'!
testConicIsSmooth
	self assert: (Conic coefficients: ℚ !!!! #(1 -1 0)) isSmooth not.
	self assert: (Conic coefficients: 𝔽₂ !!!! #(1 1 1 1 1 0)) isSmooth! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:16'!
testProjectiveLineOverFiniteRing
	| P |
	P _ ProjectiveLine over: ℤ/6.
	"P(Z/6Z) has 12 points: [0:1], [1:1], [2:1], [3:1], [4:1], [5:1], [1:2], [3:2], [5:2], [1:3], [2:3] and [1:0] (Henri Cohen, 'A Course in Computational Algebraic Number Theory', 478p)"
	self assert: P points size = 12.
	self assert: (Iterator on: P points performing: #do:) size = 12! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:27'!
testProjectiveLineOverFiniteRing2AsDirectProduct
	| P |
	P _ ProjectiveLine over: ℤ/2 × (ℤ/2). "isomorphic to ℤ/2[x]/<x(x+1)>"
	self assert: P points size = 9.
	self assert: (Iterator on: P points performing: #do:) size = 9! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:37'!
testProjectiveLineOverFiniteRing2AsQuotient
	| R P |
	R _ (ℤ/2) polynomials.
	R _ R / (R x * (R x + 1)). "isomorphic to ℤ/2 × (ℤ/2)"
	P _ ProjectiveLine over: R.
	self assert: P points size = 9.
	self assert: (Iterator on: P points performing: #do:) size = 9! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:42'!
testProjectiveLineOverFiniteRing4
	| P |
	P _ ProjectiveLine over: ℤ/4 × (ℤ/4).
	self assert: P points size = 36.
	self assert: (Iterator on: P points performing: #do:) size = 36! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:49'!
testProjectiveLineOverFiniteRing5
	| P |
	P _ ProjectiveLine over: ℤ/2 × (ℤ/2) × (ℤ/2).
	self assert: P points size = 27.
	self assert: (Iterator on: P points performing: #do:) size = 27! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:53'!
testProjectiveLineOverFiniteRing6
	| R P |
	R _ (ℤ/2) polynomials.
	R _ R / (R x^3 - R x).
	P _ ProjectiveLine over: R.
	self assert: P points size = 18.
	self assert: (Iterator on: P points performing: #do:) size = 18! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 7/13/2023 14:49:28'!
testArithmeticGenusExample1
	| R I C |
	R :=  ℚ polynomialsIn: 4.
	I := R !! [:x :y :z :w| {w*z - (x^2). w^2 + (y^2) + (z^2)}].
	C := (R / I) proj asCurve.
	self assert: C arithmeticGenus = 1! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 7/13/2023 14:49:54'!
testArithmeticGenusExample2
	| R I C |
	R :=  ℤ/7 polynomialsIn: 5.
	I := R !! [:x :y :z :w :t| {t^3 - (x*y*w). x^3 + (y^3) + (z^3). z - w}].
	C := (R / I) proj asCurve.
	self assert: C arithmeticGenus = 10! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 7/13/2023 14:56:37'!
testArithmeticGenusExample3
	| R f C |
	R :=  ℤ/5 polynomialsIn: 3.
	f := R !! [:x :y :z| y^2*(z^7) - (x^9) - (x*(z^8))].
	C := (R / f) proj asCurve.
	self assert: C arithmeticGenus = 28.
	"self assert: C genus = 4"! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 14:38:33'!
testDifference
	| A x y C X2 D O |
	A := AffineSpace new: 2 over: ℚ.
	x := A coordinateRing x.
	y := A coordinateRing y.
	C := A sub: x * y. "union of x-axis and y-axis"
	X2 := A sub: x^2. "y-axis with double multiplicity"
	D := X2 - C. "y-axis with multiplicity 1"
	self assert: D asIdeal = (A coordinateRing * x).
	O := A sub: {x. y}.
	D := C - O.
	self assert: D = C.
	self assert: (C - A) isEmpty "removing the ambient space removes all"! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:54:42'!
testEllipticCurvesAreSmooth
	| R x y C |
	R := ℚ polynomialsIn: #(x y z).
	x := R x.
	y := R y.
	C := (R / (x^3 - (x*3) + 3 - (y^2) homogenizedIn: 3)) proj.
	self assert: C isSmooth.
	C := (R / (x^3 + x - (y^2) homogenizedIn: 3)) proj.
	self assert: C isSmooth.
	C := (R / (x^3 - x - (y^2) homogenizedIn: 3)) proj.
	self assert: C isSmooth! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:54:13'!
testEmpty
	| A X |
	A := ℚ polynomialsIn: #(x y z).
	X := (A / {A continuantOfOrder: 2. A continuantOfOrder: 3}) spec.
	self assert: X isEmpty! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/23/2023 20:10:07'!
testEqualVarietiesDifferentIdeals1
	| A x I I2 V V2 |
	A := ℚ polynomials.
	x := A x.
	I := A * {x}.
	I2 := A * {x^2}.
	V := I variety.
	V2 := I2 variety.
	self assert: V = V2.
	self assert: I ~= I2! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testEqualVarietiesDifferentIdeals2
	| A x y I I2 V V2 |
	A _ ℚ polynomialsIn: #(x y).
	x _ A x.
	y _ A y.
	I _ A * {x. y}.
	I2 _ A * {x^2. y}.
	V _ I variety.
	V2 _ I2 variety.
	self assert: V = V2.
	self assert: I ~= I2! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testFinite
	| A x y X |
	A _ ℚ polynomialsIn: #(x y).
	x _ A x.
	y _ A y.
	X _ A / {x*x - 1. y*y - (x*x) - 3} :: spec.
	self assert: X isFinite! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 8/12/2022 13:05:39'!
testIntersection
	| A x y V V2 W |
	A _ ℚ polynomialsIn: #(x y).
	x _ A x.
	y _ A y.
	V _ A / {y} :: spec.
	V2 _ A / {x*x - 1} :: spec.
	W _ V ∩ V2.
	self assert: (W satisfies: (1, 0)).
	self assert: (W satisfies: (0, 1)) not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/28/2020 18:15:56'!
testLinearSubspaceAsAlgebraicVariety
	| S V |
	S _ ℚ ^ 3 grassmannian: 2 :: atRandomBits: 10.
	V _ S asAlgebraicVariety.
	self assert: V dimension + 1 = S rank. "because it's projective"
	self assert: (V satisfies: (S x: 1)).
	self assert: (V satisfies: (S x: 2))! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:42:16'!
testMorphismsOfProjectiveVarieties
	"'A Singular Introduction To Commutative Algebra', Example A.6.8 (morphisms of projective varieties)."
	| P A x y z X f₁ f₂ f₃ I J Y φ Z |
	P := ProjectiveSpace new: #(x y z) over: ℚ. "projective plane"
	A := P coordinateRing. "C[x,y,z]"
	x := A x. y := A y. z := A z.
	X := P sub: {z^3 - (x*(y^2)) + (y^3)}.
	f₁ := x*z. f₂ := x*y. f₃ := x^2 + (y*z).
	"In order for (f₁,f₂,f₃) to define a projective morphism (not just a rational map) it must never be zero at any point of X, this means the intersection of X with V(J) must be empty:"
	I := X asIdeal.
	J := A * {f₁. f₂. f₃}.
	self assert: (A / (I + J)) dimension = 0.
	Y := P sub: J.
	self assert: (X ∩ Y) isEmpty.
	φ := X to: P representatives: {f₁. f₂. f₃}.
	"The image is a curve of degree 6:"
	Z := φ image.
	self assert: Z isCurve.
	self assert: Z degree = 6! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:43:01'!
testMultiplicity
	"Example from Sage documentation."
	| A X q₁ q₂ |
	A := AffineSpace new: 4 over: ℚ.
	X := A sub: (A coordinateRing !! [:x :y :z :w| {z*y - (x^7). w - (2*z)}]).
	q₁ := ℚ spec ⇒ X !! {1. (1/3). 3. 6}.
	q₂ := ℚ spec ⇒ X !! #(0 0 0 0).
	self assert: q₁ multiplicity = 1.
	self assert: q₂ multiplicity = 2! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:44:11'!
testMultiplicity2
	"Example from Sage documentation."
	| P X q₁ q₂ q₃ |
	P := ProjectiveSpace new: 4 over: ℚ.
	X := P sub: (P coordinateRing !! [:x :y :z :w :t| {y^2 - (x*t). w^7 - (t*w*(x^5)) - (z^7)}]).
	q₁ := X !! (0,0,1,1,1).
	self assert: q₁ multiplicity = 1.
	q₂ := X !! (1,0,0,0,0).
	self assert: q₂ multiplicity = 3.
	q₃ := X !! (0,0,0,0,1).
	self assert: q₃ multiplicity = 7! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 14:38:55'!
testMultiplicity3
	"Example from Sage documentation."
	| P X q |
	P := ProjectiveSpace new: 3 over: ℚ.
	X := P sub: (P coordinateRing !! [:x :y :z :w| z^5*(x^2)*w - (y^8)]).
	q := X !! (2,0,0,1).
	self assert: q multiplicity = 5! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testMultiplicityAndTangentCone
	"'A Singular Introduction to Commutative Algebra', Example A.8.5."
	| R C T p |
	R _ ℚ polynomialsIn: #(x y).
	C _ (R / (R !! [:x :y| x^3 - y squared])) spec.
	p _ C !! (0,0).
	self assert: p multiplicity = 2.
	T _ (R / (R !! [:x :y| y^2])) spec.
	self assert: p tangentCone = T.
"	L _ (R / (R !! [:x :y| 100 atRandom * x + (100 atRandom * y)])) spec
	...
"! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:47:37'!
testNotEmpty
	| A X |
	A := ℚ polynomialsIn: #(x y).
	X := (A  / (A !! [:x :y| {x*x - 1. y*y - (x*x) - 3}])) spec.
	self assert: X isEmpty not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:48:30'!
testNotFinite
	| A X |
	A := ℚ polynomialsIn: #(x y).
	X := (A / (A !! [:x :y| {y*y - (x*x) - 3}])) spec.
	self assert: X isFinite not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:49:33'!
testReduced
	| 𝔸³ R x y z X |
	𝔸³ := AffineSpace new: 3 over: ℚ.
	R := 𝔸³ coordinateRing.
	x := R x.
	y := R y.
	z := R z.
	X := 𝔸³ sub: {x*(y^3). x^2*z}.
	self assert: X isReduced not.
	self assert: X reduced = (𝔸³ sub: {x*y. x*z})! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 10/14/2022 08:40:09'!
testRingHomomorphismSpec
	| A B x y φ |
	A _ ℚ polynomialsIn: #(x y z).
	B _ ℚ polynomialsIn: #(x y).
	x _ B x.
	y _ B y.
	φ _ A to: B map: [:f| f value: {x-y. x. y}].
	self assert: φ spec domain = B spec.
	self assert: φ spec codomain = A spec.
	self assert: φ spec asRingMap = φ! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:51:02'!
testSingularCubics
	| R x y C |
	R := ℚ polynomialsIn: #(x y z).
	x := R x.
	y := R y.
	C := (R / (x^3 - (y^2) homogenizedIn: 3)) proj.
	self assert: (C !! (1,1,1)) isSmooth.
	self assert: (C !! (0,0,1)) isSmooth not.
	self assert: C isSingular.
	C := (R / (x^3 + (x^2) - (y^2) homogenizedIn: 3)) proj.
	self assert: C isSingular! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:51:15'!
testSmoothness
	| R C |
	R := ℚ polynomialsIn: #(x y).
	"Cuspidal curve:"
	C := (R / (R !! [:x :y| y^2 - (x^3)])) spec.
	self assert: C isSmooth not.
	self assert: C isSingular.
	self assert: (C !! (1,1)) isSmooth.
	self assert: (C !! (0,0)) isSmooth not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 14:40:03'!
testSmoothnessOverCC
	| P R C |
	P := ProjectiveSpace new: 2 over: ℚ. "over ℂ or over ℚ ?"
	R := P coordinateRing.
	C := P sub: R !! [:x :y :z| x].
	self assert: C isSingular not.
	C := P sub: R !! [:x :y :z| y^2*z - (x^3)].
	self assert: C isSingular.
	C := P sub: R !! [:x :y :z| y^2*z - (x^3) + (z^3)].
	self assert: C isSingular not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 14:40:14'!
testSmoothnessOverFiniteField
	| P R C |
	P := ProjectiveSpace new: 2 over: ℤ/19.
	R := P coordinateRing.
	C := P sub: R !! [:x :y :z| x^3 + (y^3) + (z^3)].
	self assert: C isSingular not.
	C := P sub: R !! [:x :y :z| x^4 - (x*(z^3))].
	self assert: C isSingular.
	C := P sub: R !! [:x :y :z| x^5 + (y^5*19) + (z^5)].
	self assert: C isSingular.
	C := P sub: R !! [:x :y :z| x^5 + (y^5*9) + (z^5)].
	self assert: C isSingular not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 14:40:19'!
testSmoothnessOverQQ
	| P C |
	P := ProjectiveSpace new: 2 over: ℚ.
	C := P sub: P coordinateRing !! [:x :y :z| x^3 - (y^2*z)].
	self assert: C isSingular! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 16:52:36'!
testSmoothnessOverQQ2
	| R I J |
	R := ℚ polynomialsIn: #(x y).
	I := R * (R !! [:x :y| {x - y squared}]).
	J := R * (R !! [:x :y| {x squared - y squared}]).
	self assert: (R/I) spec isSmooth.
	self assert: (R/J) spec isSmooth not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 6/26/2023 17:04:17'!
testSpecZ
	self assert: ℤ spec dimension = 1.
	self assert: ℤ spec isNoetherian.
	self assert: (ℤ spec includes: ℤ * 5).
	self assert: (ℤ spec includes: ℤ * 6) not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 8/12/2022 13:06:11'!
testSpecZSubschemes
	self assert: ℤ spec >= (ℤ / 2) spec.
	self assert: (ℤ / 10) spec >= (ℤ / 5) spec.
	self deny: (ℤ / 10) spec >= (ℤ / 3) spec.
	self assert: (ℤ / 10) spec ∩ (ℤ / 15) spec = (ℤ / 5) spec.
	self assert: (ℤ / 10) spec ∪ (ℤ / 15) spec = (ℤ / 30) spec! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 7/12/2023 14:41:17'!
testWeilRestriction1
	| A |
	A := ((QuadraticField new: 5) polynomialsIn: 2) spec.
	self assert: A restriction scalars = ℚ.
	self assert: A restriction rank = 4! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 7/12/2023 14:40:10'!
testWeilRestriction2
	| R V P |
	R := (GaloisField new: 2 to: 2) polynomialsIn: 2.
	V := (R / {R x * R y + 1. R scalars x * R x + 1}) spec.
	P := V points asSet atRandom.
	self assert: (V satisfies: P coordinates).
	self assert: (V restriction satisfies: P restriction coordinates)! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 7/12/2023 14:42:12'!
testWeilRestriction3
	| A f P |
	A := ((QuadraticField new: 5) polynomialsIn: 2) spec.
	f := A id.
	P := A points !! #(2 1).
	self assert: (f value: P) restriction = (f restriction value: P restriction)! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 08:56:59'!
testProjectivePlaneOverFiniteField
	| P |
	P _ ProjectiveSpace new: 2 over: ℤ/5.
	self assert: P points size = 31.
	self assert: (Iterator on: P points performing: #do:) size = 31! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 08:57:01'!
testProjectivePlaneOverFiniteRing
	| P |
	P _ ProjectiveSpace new: 2 over: ℤ/4.
	self assert: P points size = 28
	"this plane has 28 points and 28 lines (falling into 7 neighbour classes of size 4), each line has 6 points and each point is on 6 lines"! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 6/26/2023 14:39:05'!
testProjectiveSchemeAffinePatches
	| P E p a |
	P := ProjectivePlane over: ℚ.
	"cremona37a elliptic curve:"
	E := P sub: (P coordinateRing !! [:x :y :z| y^2*z + (z^2*x) + (z^2*y) - (x^3)]).
	p := E !! ((2,-3,1) over: ℚ).
	a := p dehomogenizedIn: 2.
	"the affine point is in the second standard affine patch:"
	self assert: (E patch: 2) == a codomain.
	"and when we homogenize again, the same projective embedding is used and come back to the same projective point on exactly the same projective curve:"
	self assert: a homogenized = p.
	self assert: a homogenized codomain == E! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 6/26/2023 14:39:10'!
testProjectiveSchemeDegree
	"Example from Sage documentation."
	| P X |
	P := ProjectiveSpace new: 5 over: ℚ.
	X := P sub: (P coordinateRing !! [:x :y :z :w :t :u| x^7 + (x*y*z*(t^4)) - (u^7)]).
	self assert: X degree = 7! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 6/26/2023 14:39:14'!
testProjectiveSchemeDegree2
	"Example from Sage documentation."
	| P X |
	P := ProjectiveSpace new: 3 over: ℤ/13.
	X := P sub: (P coordinateRing !! [:x :y :z :w| {y^3 - (w^3). x + (7*z)}]).
	self assert: X degree = 3! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 6/26/2023 14:39:18'!
testProjectiveSchemeDegree3
	"Example from Sage documentation."
	| P X |
	P := ProjectiveSpace new: 4 over: ℚ.
	X := P sub: (P coordinateRing !! [:x :y :z :w :u| {x^7 - (y*(z^3)*(w^2)*u). w*(z^2) - (y*(u^2)). z^3 + (y^3)}]).
	self assert: X degree = 63! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 10/18/2016 20:58'!
testProjectiveSpace
	| K P |
	K _ PrimeField new: 23.
	P _ ProjectiveSpace new: 3 over: K.
	self assert: P dimension = 3.
	self assert: P scalars = K.
	self assert: P coordinateRing = (K polynomialsIn: 4).
	self assert: P ambient = P.
	self assert: P coordinateRing proj = P! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 08:57:08'!
testProjectiveSpaceOverFiniteField
	| P S |
	P _ ProjectiveSpace new: 1 over: ℤ/3.
	S _ P points.
	self assert: S size = 4.
	self assert: S asSet size = 4.
	S _ P pointsOver: (ℤ/3 extensionDegree: 2).
	self assert: S size = 10.
	self assert: S asSet size = 10! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 8/5/2021 08:37:07'!
testProjectiveSpaceOverModularIntegers
	| N n P numberOfPoints |
	N _ (2 to: 6) atRandom.
	n _ (1 to: 3) atRandom.
	P _ ProjectiveSpace new: n over: ℤ/N.
	"formula from Cohen's 'A course in computational algebraic number theory' page 486:"
	numberOfPoints _ N^n * (N factorization asSet product: [:p| (0 to: n) sum: [:i| 1/(p^i)]]).
	self assert: P points asSet size = numberOfPoints.
	self assert: P points size = numberOfPoints! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 6/26/2023 17:10:58'!
testRationalMap
	"Example from Magma handbook Scheme_map-creation (H118E32)."
	| 𝔸¹ 𝔸² x y f X |
	𝔸¹ := AffineSpace new: #(t) over: ℚ.
	𝔸² := AffineSpace new: #(x y) over: ℚ.

	f := 𝔸¹ to: 𝔸² mapWithArguments: [:t| {t^3 + t. t^2 - 3}].

	self assert: f domain = 𝔸¹.
	self assert: f codomain = 𝔸².

	x := 𝔸¹ coordinateRing x.
	y := 𝔸² coordinateRing y.
	X := 𝔸² sub: {x^2*-1 + (y^3) + (y^2*11) + (y*40) + 48}.
	self assert: f image = X! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 6/26/2023 17:14:36'!
testRationalMap2
	"Example from Cox's IVA pag. 234. Parametrization of the tangent surface of the twisted cubic."
	| 𝔸² 𝔸³ x y z f X |
	𝔸² := AffineSpace new: #(t u) over: ℚ.
	𝔸³ := AffineSpace new: 3 over: ℚ.

	f := 𝔸² to: 𝔸³ mapWithArguments: [:t :u| {t + u. t^2 + (t*u*2). t^3 + (t^2*u*3)}].

	x := 𝔸³ coordinateRing x.
	y := 𝔸³ coordinateRing y.
	z := 𝔸³ coordinateRing z.
	X := 𝔸³ sub: {x^3*z - (x*y^2*(3/4)) - (x*y*z*(3/2)) + (y^3) + (z^2*(1/4))}.
	self assert: f image = X! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 7/12/2023 17:04:14'!
testRationalMapAffineExample
	"From lecture notes MIT 18.782 Introduction to Arithmetic Geometry, Lecture #15, Fall 2013, 10/29/2013, Example 15.19."
	| 𝔸² x y X 𝔸¹ φ φ₂ ψ |
	𝔸² := AffineSpace new: 2 over: ℚ.
	x := 𝔸² coordinateRing x.
	y := 𝔸² coordinateRing y.
	X := 𝔸² sub: x^2 + (y^2) - 1.
	𝔸¹ := AffineSpace new: 1 over: ℚ.
	φ := X to: 𝔸¹ mapWithArguments: [:x₀ :y₀| {y₀ / (x₀ + 1)}].
	φ₂ := X to: 𝔸¹ mapWithArguments: [:x₀ :y₀| {1 - x₀ / y₀}].
	self assert: φ = φ₂.
	ψ := 𝔸¹ to: X mapWithArguments: [:t| {1 - (t^2) / (1 + (t^2)). t*2 / (1 + (t^2))}].
	self assert: (φ · ψ) isIdentity.
	self assert: φ isRegular not.
	self assert: φ isDominant.
	self assert: φ isSurjective.
	self assert: ψ isRegular not.
	self assert: ψ isDominant.
	self assert: ψ isSurjective not! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 6/26/2023 17:27:51'!
testRationalMapImage
	| A X X₂ f |
	A := AffineSpace new: #(x y) over: ℚ.
	"circle centered at (0,0):"
	X := A sub: A coordinateRing !! [:x :y| x^2 + (y^2) - 1].
	"circle centered at: (1,2):"
	X₂ := A sub: A coordinateRing !! [:x :y| (x-1)^2 + ((y-2)^2) - 1].
	"translation by (1,2):"
	f := X to: A mapWithArguments: [:x :y| {x+1. y+2}].
	self assert: f image = X₂.
	"global translation by (1,2):"
	f := A to: A mapWithArguments: [:x :y| {x+1. y+2}].
	self assert: (X apply: f) = X₂! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 10/5/2022 17:24:12'!
testRationalMapLinearMapConversion
	| A2 A3 V2 V3 f f2 p f3 |
	A2 _ AffineSpace new: 2 over: ℚ.
	A3 _ AffineSpace new: 3 over: ℚ.
	V2 _ ℚ^2.
	V3 _ ℚ^3.

	f _ V2 ⇒ V3 atRandomBits: 100.
	f2 _ f asRationalMap.
	self assert: (A2 ⇒ A3 includes: f2).

	p _ V2 atRandomBits: 100.
	self assert: (f value: p) = (f2 value: p).

	f3 _ V2 ⇒ V3 !! f2.
	self assert: f = f3! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 6/27/2023 16:04:37'!
testRationalMapProjectiveExample
	"Example from Magma documentation, Example Scheme_ex5 H119E5."
	| ℙ¹ ℙ² f p X |
	ℙ¹ := ProjectiveSpace new: #(s t) over: ℚ.
	ℙ² := ProjectiveSpace new: #(x y z) over: ℚ.
	f := ℙ¹ to: ℙ² mapWithArguments: [:s :t| {s/t. s^2/(s^2 - (t^2)). t/s}].
	p := ℙ¹ points !! #(3 2).
	self assert: (f value: p) = (ℙ² points !! {9/4. 27/10. 1}).
	X := ℙ² sub: (ℙ² coordinateRing !! [:x :y :z| {(x*y)^2 - (x^3*z) - (x*(y^2)*z*2) + ((y*z)^2)}]).
	self assert: f image = X.
	self assert: f isRegular
! !

!AlgebraicGeometryTest methodsFor: 'rational points' stamp: 'len 6/26/2023 14:39:41'!
testRationalPointsOverFiniteField
	| P C |
	P := ProjectiveSpace new: 2 over: ℤ/7.
	C := P sub: P coordinateRing !! [:x :y :z| x^3 + (y^3) - (z^3)].
	self assert: C points size = 9.
	self assert: (Iterator on: C points performing: #do:) size = 9! !

!AlgebraicGeometryTest methodsFor: 'tangent cone' stamp: 'len 12/15/2021 12:00:25'!
testTangentCone
	"'A Singular Introduction to Commutative Algebra', Example 5.5.13."
	| R I J X |
	R _ ℚ polynomialsIn: #(x y z).
	I _ R * (R !! [:x :y :z| {y*z + (z^2) + (x^3). y^2 + (x*z) + (y^4)}]).
	X _ (R/I) spec.
	J _ R * (R !! [:x :y :z| {y^2 + (x*z). y*z + (z^2). x*(z^2) - (y*(z^2))}]).
	self assert: (X !! (0,0,0)) tangentCone asIdeal = J! !

!AlgebraicGeometryTest methodsFor: 'tangent cone' stamp: 'len 7/26/2022 13:43:08'!
testTangentCone2
	"M. Kreuzer, 'Computational Commutative Algebra', Example 6.5.27."
	| R J T I |
	R _ ℚ polynomialsIn: 4.
	J _ R * (R !! [:x1 :x2 :x3 :x4| {x1*x2 - (x3^2). x2^2 - (x4^5)}]).
	T _ (R/J) spec tangentConeAt: (0,0,0,0).
	I _ R * (R !! [:x1 :x2 :x3 :x4| {x1*x2 - (x3^2). x2^2. x2*(x3^2). x3^4}]).
	self assert: T asIdeal = I! !

!AlgebraicGeometryTest methodsFor: 'tangent cone' stamp: 'len 6/26/2023 14:40:25'!
testTangentConeAndTangentSpace
	"Example from https://en.wikipedia.org/wiki/Tangent_cone."
	| A x y S o |
	A := AffineSpace new: #(x y) over: ℚ.
	x := A coordinateRing x.
	y := A coordinateRing y.
	"this curve is singular at (0,0):"
	S := A sub: x^2 + (x^3) - (y^2).
	o := S !! (0,0).
	"the tangent cone is the union of the two lines x = y and x = -y:"
	self assert: o tangentCone = (A sub: (x + y) * (x - y)).
	"the tangent space is the whole plane:"
	self assert: o tangentSpace = A! !

!AlgebraicGeometryTest methodsFor: 'stalks' stamp: 'len 7/26/2022 13:32:06'!
testLocalIntegerRingSpecStalks
	| R X p |
	R _ ℤ @ (ℤ * 5) .
	X _ R spec.
	p _ R * 0.
	self assert: X @ p = ℚ.
	p _ R * 5.
	self assert: X @ p = R! !

!AlgebraicGeometryTest methodsFor: 'examples' stamp: 'len 6/3/2023 21:09:53'!
testStillmanExample1
	"Example from [Sti06]"
	| K P s t S P1 P3 φ C M |
	K := ℤ/32003.
	P := K polynomialsIn: #(s t).
	s := P x: 1.
	t := P x: 2.
	S := K polynomialsIn: #(a b c d).
	P1 := P proj.
	P3 := S proj.
	φ := P1 to: P3 representatives: {s^4. s^3*t. s*(t^3). t^4}.
	C := φ image.
	self assert: φ asRingMap kernel = C asIdeal.
	self assert: C codimension = 2.
	self assert: C degree = 4.
	self flag: #fixme. "TODO:"
	"self assert: C genus = 0."
	M := S asIdeal / C asIdeal.
	self assert: M isGraded.
	"M resolution..."! !

!AlgebraicGeometryTest methodsFor: 'examples' stamp: 'len 1/14/2022 11:32:38'!
testSturmfelsExample1
	"Bernd Sturmfels, 'Ideals, Varieties and Macaulay 2', 1. A Curve in Affine 3-Space."
	| R x y I C C1 C2 |
	R _ ℚ polynomialsIn: #(x y z). "default ordering is trevlex"
	x _ R x.
	y _ R y.
"	z _ R z."
	I _ R * {x^4 - (y^5). x^3 - (y^7)}.
	self assert: I basis "reduced" printString = '(x^8 - x^3y^3, x^4y^2 - x^3, y^5 - x^4)'.
	C _ I variety.
	self assert: C dimension = 1.
	self assert: C codimension = 2.
	"The degree of a curve in complex affine 3-space is the number of intersection points with a general plane. It coincides with the degree of the projective closure of our curve:"
	self assert: C projectiveClosure degree = 28.
	"We first extract the components which are transverse to the plane x = 0:"
	C1 _ C \ (R / R x) spec.
	self assert: C1 = (R / {x*(y^2) - 1. y^5 - (x^4). x^5 - (y^3)}) spec.
	"And next we extract the component which lies in the clane x = 0:"
	C2 _ C - C1. "NOTE: the article uses ideal saturation, and this is colon"
	self assert: C2 = (R / {x^3. y^5}) spec.
	"self deny: C isReduced." "this fails!!!!!!"
	self assert: C1 projectiveClosure degree = 13.
	self assert: C1 isReduced "equivalent to C1 ideal = C1 ideal radical"! !

!AlgebraicGeometryTest methodsFor: 'examples' stamp: 'len 8/12/2022 13:06:48'!
testSturmfelsExample2
	"Bernd Sturmfels, 'Ideals, Varieties and Macaulay 2', 2. Intersecting Our Curve With a Surface."
	| R x y z C S theirunion ourpoints |
	R _ ℚ polynomialsIn: #(x y z). "default ordering is trevlex"
	x _ R x.
	y _ R y.
	z _ R z.
	C _ (R / {x^4 - (y^5). x^3 - (y^7)}) spec.
	C _ C \ (R / R x) spec. "this is C1 from example1"
	S _ (R / (x^5 + (y^5) + (z^5) - 1)) spec.
	theirunion _ C ∪ S.
	"In this case, the union's ideal coincides with the product of the ideals:"
	self assert: theirunion asIdeal = (C asIdeal * S asIdeal).
	"The intersection of the curve and the surface (whose defining ideal is the sum of the respective ideals):"
	ourpoints _ C ∩ S.
	"We get a finite set of points:"
	self assert: ourpoints dimension = 0.
	"The number of points is 65:"
	self assert: ourpoints projectiveClosure degree = 65.
	"Each of the points is multiplicity-free:"
	self assert: ourpoints isReduced. "equivalent to the original: ourpoints reduced projectiveClosure degree = 65."! !

!AlgebraicGeometryTest methodsFor: 'examples' stamp: 'len 6/26/2023 14:40:29'!
testTwistedCubic
	| P R M C |
	P := ProjectiveSpace new: 3 over: ℚ.
	R := P coordinateRing.
	"R indeterminateNames: #(u v w t)"
	M := R^3 ⇒ (R^2) !! (#(1 2 3 2 3 4) collect: [:i| R x: i]).
	C := P sub: (M minors: 2).
	self assert: C dimension = 1.
	self assert: C isSmooth! !

!AlgebraicGeometryTest methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
testTwistedCubicDegree
	"Twisted cubic in P^3 with homogeneous coordinates x,y,w,z."
	| A X |
	A _ ℚ polynomialsIn: #(x y z w).
	X _ (A / [:x :y :z :w| {x^2 - (y*w). x*z - (y^2)}]) proj.
	"At this point X is still reducible, it is the union of a line x=y=0 and the twisted cubic:"
	self assert: X degree = 4.
	"To cut out X, we impose an additional equation x^3 - (z * (w^2)) = 0:"
	X _ (A / [:x :y :z :w| {x^2 - (y*w). x*z - (y^2). x^3 - (z*(w^2))}]) proj.
	"But we get the twisted cubic (in some sense) 'counted with multiplicity 2', so the degree is 6
	(BUT NOT REALLY, MAGMA GIVES 3 LIKE US, I DONT REMEMBER WHERE I GOT THIS EXAMPLE FROM BUT THE CORRECT ANSWER IS 3)"
	self assert: X degree = 3! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:08:30'!
testAutomorphismsK3
	"Aut(K3) = Sym(3)"
	| G A |
	G _ Graph K: 3.
	A _ G automorphisms.
	self assert: A = A ambient
! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:08:37'!
testAutomorphismsP3
	"Aut(P3) = {id, (1 3)}"
	| G A |
	G _ Graph P: 3.
	A _ G automorphisms.
	self assert: A = (PermutationGroup on: G values generators: {#((1 3))})! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:08:46'!
testChromaticNumber
	| G |
	G _ Graph unordered add: 1; add: 2; add: 3; yourself. 
	self assert: G chromaticNumber = 1. "edgeless graph"
	self assert: (Graph K: 3) chromaticNumber = 3.
	self assert: (Graph C: 5) chromaticNumber = 3.
	self assert: (Graph C: 6) chromaticNumber = 2.
	self assert: (Graph W: 6) chromaticNumber = 4.
	self assert: (Graph W: 5) chromaticNumber = 3.
	self assert: (Graph S: 6) chromaticNumber = 2! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:08:55'!
testChromaticPolynomialComplete
	| x |
	x _ ℤ polynomials x.
	self assert: (Graph K: 3) chromaticPolynomial = (x*(x-1)*(x-2))! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:09:03'!
testChromaticPolynomialCylic
	| x n |
	x _ ℤ polynomials x.
	n _ 4 atRandom.
	self assert: (Graph C: n) chromaticPolynomial = (x-1^n + ((-1)^n * (x-1)))! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:09:12'!
testChromaticPolynomialPath
	| x n |
	x _ ℤ polynomials x.
	n _ 4 atRandom + 1.
	self assert: (Graph P: n) chromaticPolynomial = (x-1^(n-1)*x)! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:09:52'!
testIsomorphism
	| G H |
	G _ Graph unordered.
	5 timesRepeat: [G addEdge: 5 atRandom -> 5 atRandom].
	H _ G shuffled.
	self assert: G ~ H.
	G _ Graph C: 5. "pentagon"
	H _ G copy.
	G addEdges: {1 -> 6. 6 -> 2}. "make a little triangle"
	H addEdges: {1 -> 6. 6 -> 3}. "same, but skip vertex 2 in the middle"
	H _ H shuffled.
	self deny: G ~ H! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:10:09'!
testTuttePolynomial
	| G P x y T |
	G _ Graph ordered addEdges: {1->2. 2->3. 3->4. 4->1. 1->3}; yourself.
	P _ ℤ polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	T _ x*(x^2 + (x+y)) + (x^2 + (x+y)) + (x*y + (y^2)).
	self assert: G tuttePolynomial = T! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:10:14'!
testTuttePolynomial2
	| G P x y T |
	G _ Graph ordered addEdges: {1->2. 2->3. 3->4. 4->1. 1->3}; yourself.
	P _ ℤ polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	T _ x*(x^2 + (x+y)) + (x^2 + (x+y)) + (x*y + (y^2)).
	self assert: G tuttePolynomial = T! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:10:19'!
testTuttePolynomialK3
	| G P x y T |
	G _ Graph K: 3.
	P _ ℤ polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	T _ x^2 + x + y.
	self assert: G tuttePolynomial = T! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:10:25'!
testTuttePolynomialK4
	| G P x y T |
	G _ Graph K: 4.
	P _ ℤ polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	T _ x^3 + (x^2*3) + (x*2) + (x*y*4) + (y*2) + (y^2*3) + (y^3).
	self assert: G tuttePolynomial = T! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 08:59:39'!
newChainComplex
	^ self newChainComplexOver: {ℤ. ℤ/64 atRandom nextPrime. ℚ} atRandom! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/23/2020 00:35:38'!
newChainComplexOver: aRing
	| X |
	X _ SimplicialComplex perform: #(kleinBottle realProjectivePlane torus) atRandom.
	^ X chainComplexOver: aRing! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2022 11:06:40'!
testChainComplexExample
	self newChainComplex validate! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/23/2020 00:38:24'!
testIdentity
	| C id |
	C _ self newChainComplex.
	id _ C id.
	self assert: id isIdentity! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/23/2020 00:38:31'!
testIdentityImage
	| C id |
	C _ self newChainComplex.
	id _ C id.
	self assert: id image = C! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/23/2020 00:38:36'!
testIdentityKernel
	| C id |
	C _ self newChainComplex.
	id _ C id.
	self assert: id kernel isTrivial! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2022 11:06:52'!
testShift
	| C k i |
	C _ self newChainComplex.
	self assert: C >> 0 = C.
	k _ (-5 to: 5) atRandom.
	(C >> k) validate.
	self assert: C >> k << k = C.
	self assert: C >> k = (C << k negated).
	self assert: (C >> k) minDegree = (C minDegree + k).
	self assert: (C >> k) maxDegree = (C maxDegree + k).
	i _ (-5 to: 5) atRandom.
	self assert: (C >> k at: i+k) = ((C at: i) * ((-1)^k))! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 7/26/2022 13:43:15'!
testAffineRingElementOperations
	"Magma handbook example H105E1."
	| R A x y |
	R _ ℚ polynomialsIn: #(x y).
	A _ R / (R !! [:x1 :y1| {x1^2 - (y1^2) + 2. y1^3 - 5}]).
	x _ A x. y _ A y.
	self assert: x^2 = (y^2 - 2).
	"self assert: x reciprocal = ..."! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 8/12/2022 13:07:01'!
testAffineRingIdealIntersectionBug
	| P x y A I J |
	P _ ℚ polynomialsIn: #(x y).
	x _ P x. y _ P y.
	A _ P / {y^2 - x + 1. y - 1}.
	I _ A * {1. 2}.
	J _ A * {-1. 2}.
	self assert: I ∩ J = A asIdeal! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 5/23/2020 09:33:01'!
testAffineRingIdealIntersectionRandom
	| R f1 f2 J A |
	R _ ℚ polynomialsIn: #(x y).
	[(f1 _ R atRandomBits: 10) isZero] whileTrue.
	[(f2 _ R atRandomBits: 10) isZero] whileTrue.
	J _ R * {f1. f2}.
	A _ R/J.
	self testIdealIntersectionRandom: A! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 5/23/2020 09:33:01'!
testAffineRingIdealMembershipRandom
	| R f1 f2 J A g I h |
	R _ ℚ polynomialsIn: #(x y z).
	[(f1 _ R atRandomBits: 10) isZero] whileTrue.
	[(f2 _ R atRandomBits: 10) isZero] whileTrue.
	J _ R * {f1. f2}.
	A _ R/J.
	g _ A atRandomBits: 10.
	I _ A * g.
	h _ I atRandomBits: 10.
	self assert: (I includes: h)! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 10/24/2023 16:07:02'!
testAffineRingIdealRadical
	"Magma handbook, example H105E2."
	| R A I J |
	R := ℚ polynomialsIn: #(x y z) "ordering: #lex".
	A := R / (R !! [:x :y :z| {x^2 - y + 1. y^3 + z - 1}]).
	I := A * (A !! [:x :y :z| x^3*y*(z^2)]).
	self deny: I isRadical.
	J := A * (A !! [:x :y :z| {x*(y^2) + (x*y) - (x*z) + x. y*z. z^2 - z}]).
	self assert: I radical = J.
	self flag: #fixme. "TODO..."
	"... and then some tests of primary decomposition"! !

!CommutativeAlgebraTest methodsFor: 'algorithms' stamp: 'len 1/13/2022 12:02:33'!
testExtendedBuchberger
	| R I A basisAndTransformation G T |
	R _ ℚ polynomialsIn: #(a b c d).
	I _ R cyclicIdeal.
	I _ I orderedBy: #grevlex.
	A _ I generatorMatrix.
	basisAndTransformation _ A basisAndTransformation.
	G _ basisAndTransformation first.
	T _ basisAndTransformation second.
	self assert: (A * T) = G! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 6/3/2023 21:04:10'!
testBuchbergerNormalForm
	"A Singular Introduction to Commutative Algebra, Example 1.6.12."
	| R x y z f G h |
	R := ℚ polynomialsIn: #(x y z) ordering: #glex.
	x := R x. y := R y. z := R z.
	f := x^3 + (y^2) + (z^2*2) + x + y + 1.
	G := {x. y}.
	h := f reductionBy: G.
	self assert: h = (z^2*2 + x + y + 1).
	h := f reductionFullBy: G.
	self flag: #fixme. "is this ok? in Singular they make it monic, we don't"
	self assert: h = "(z^2 + (1/2))" (z^2 * 2 + 1)! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 1/13/2022 10:41:54'!
testMoraNormalForm
	"A Singular Introduction to Commutative Algebra, Example 1.7.7."
	| R f G h x y z |
	R _ ℚ polynomialsIn: #(x y z) ordering: #lgrevlex.
	x _ R x. y _ R y. z _ R z.
	f _ x^2 + (y^2) + (z^3) + (x^4) + (y^5).
	G _ {x. y}.
	h _ f reductionBy: G.
	self assert: h = (z^3 + (x^4) + (y^5))! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateDivisionExample
	| P x y z F f |
	P _ ℚ polynomialsIn: #(x y z) ordering: #glex.
	x _ P x.
	y _ P y.
	z _ P z.
	F _ {x^2 - (y*z). y^2 - (z*x). z^2 - (x*y)}.
	f _ x^3 + (y^3 * 3).
	self assert: (f divisionBy: F) remainder = (y^3 * 4)! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateDivisionExample2
	| R f f1 f2 |
	R _ ℚ polynomialsIn: #(x y z) ordering: #lex.
	f _ R !! [:x :y :z| x^5*(y^3)].
	f1 _ R !! [:x :y :z| x^3*(y^2) - ((y^2)*z)].
	f2 _ R !! [:x :y :z| x*(y^2) - (y*z)].
	self assert: (f divisionBy: {f1. f2}) printString = 'x^5y^3 = (x^2y)(x^3y^2 - y^2z) + (xyz + z^2)(xy^2 - yz) + yz^3'! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateDivisionExample3
	| R f f1 f2 |
	R _ ℚ polynomialsIn: #(x y z) ordering: #lex.
	f _ R !! [:x :y :z| x^3*(y^2) + (x*y) + x + 1].
	f1 _ R !! [:x :y :z| x^3 + 1].
	f2 _ R !! [:x :y :z| y^2 + 1].
	self assert: (f divisionBy: {f1. f2}) printString = 'x^3y^2 + xy + x + 1 = (y^2)(x^3 + 1) + (-1)(y^2 + 1) + xy + x + 2'! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateDivisionExample4
	| R f f1 f2 |
	R _ ℚ polynomialsIn: #(x y) ordering: #lex.
	f _ R !! [:x :y| x^2*(y^3) - (x*(y^2)*2)].
	f1 _ R !! [:x :y| x^2*y - (x*2)].
	f2 _ R !! [:x :y| y^3 + 4].
	self assert: (f divisionBy: {f1. f2}) printString = 'x^2y^3 - 2xy^2 = (y^2)(x^2y - 2x) + (0)(y^3 + 4) + 0'.
	self assert: (f divisionBy: {f2. f1}) printString = 'x^2y^3 - 2xy^2 = (x^2)(y^3 + 4) + (0)(x^2y - 2x) + -4x^2 - 2xy^2'! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 7/8/2022 11:07:01'!
testGroebnerBasis: G ideal: I
	"Test G is a Groebner basis of the ideal I."
	| g division |
"	self assert: G verify."
	I generators do: [:each| self assert: (each reductionBy: G) isZero].
	G do: [:each| self assert: (I includes: each)]. "but this will use Groebner bases.."
	[g _ I atRandomBits: 200. g isZero] whileTrue.
	self assert: (g reductionBy: G) isZero.
	"The leading term of any polynomial in I is divisible by the leading term of some polynomial in G:"
	self assert: (G anySatisfy: [:any| any leadingMonomial | g leadingMonomial]).
	"The multivariate division of any polynomial in I by G gives the remainder 0:"
	division _ g divisionBy: G.
	self assert: division remainder isZero.
	division validate.
"	self assert: G reduced isReduced.
	self assert: G minimal isMinimal"! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:38'!
testGroebnerCyclic3
	| I G |
	I _ (ℚ polynomialsIn: 3) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:43'!
testGroebnerCyclic3Lexicographic
	| I G |
	I _ (ℚ polynomialsIn: 3 ordering: #lex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:53'!
testGroebnerCyclic3OverZ2
	| I G |
	I _ (ℤ/2 polynomialsIn: 3) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 12:03:25'!
testGroebnerCyclic3Reduced
	| I G |
	I _ (ℚ polynomialsIn: #(x y z)) cyclicIdeal.
	I _ I orderedBy: #lex.
	G _ I basis "reduced".
	self assert: G printString = '(x + y + z, y^2 + yz + z^2, z^3 - 1)'! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:56'!
testGroebnerCyclic3TotalLexicographic
	| I G |
	I _ (ℚ polynomialsIn: 3 ordering: #glex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:59'!
testGroebnerCyclic3TotalReverseLexicographic
	| I G |
	I _ (ℚ polynomialsIn: 3 ordering: #grevlex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:02'!
testGroebnerCyclic4
	| I G |
	I _ (ℚ polynomialsIn: 4) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:04'!
testGroebnerCyclic4OverZ127
	| I G |
	I _ (ℤ/127 polynomialsIn: 4) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:06'!
testGroebnerCyclic4OverZ2
	| I G |
	I _ (ℤ/2 polynomialsIn: 4) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:09'!
testGroebnerCyclic5
	| I G |
	I _ (ℚ polynomialsIn: 5) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:11'!
testGroebnerCyclic5OverZ127
	| I G |
	I _ (ℤ/127 polynomialsIn: 5) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:14'!
testGroebnerCyclic5OverZ2
	| I G |
	I _ (ℤ/2 polynomialsIn: 5) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 12/18/2022 20:18:45'!
testGroebnerCyclic5Reduced
	| I G |
	I _ (ℚ polynomialsIn: 5) cyclicIdeal.
	G _ I basis "reduced".
	self assert: I basisMatrix isReducedStandardBasis.
	self testGroebnerBasis: G ideal: I.
	self assert: (I scalars * G) basis "reduced" = G! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:43'!
testGroebnerExample
	| P x y z I G |
	P _ ℚ polynomialsIn: #(x y z) ordering: #lex.
	x _ P x.
	y _ P y.
	z _ P z.
	I _ P * {x^2 - (y*z). y^2 - (z*x). z^2 - (x*y)}.
	G _ I basis "minimal".
	self assert: G asSet = {x^2 - (y*z). x*z - (y^2). x*y - (z^2). y^3 - (z^3)} asSet! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 12:00:51'!
testGroebnerExample2
	| R x y f1 f2 I G |
	R _ ℚ polynomialsIn: #(x y) ordering: #lex.
	x _ R x.
	y _ R y.
	f1 _ (x-1)^2 + (y-2^2) - (3^2).
	f2 _ (x+1)^2 + (y-1^2) - (2^2).
	I _ R * {f1. f2}.
	G _ I basis "reduced".
	self assert: G printString = '(x + (1/2)y + (1/2), y^2 - 2y - (11/5))'! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 12:01:04'!
testGroebnerExample3
	"From some question in math.stackexchange.com."
	| R I G |
	R _ ℚ polynomialsIn: #(x y) ordering: #glex.
	I _ R * (R !! [:x :y| {x^2*y + x + 1. x*(y^2) + y + 1. x - y}]).
	G _ I basis "reduced".
	self assert: G printString = '(y^3 + y + 1, x - y)' "this is as computed by Singular"! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 12:01:43'!
testGroebnerExample4Minimal
	"Example from Cox, Little, O'Shea, 'Ideals, Varieties and Algorithms', p. 90, continued in p. 93."
	| R I G |
	R _ ℚ polynomialsIn: #(x y) ordering: #grevlex.
	I _ R * (R !! [:x :y| {x^3 - (x*y*2). x^2*y - (y^2*2) + x}]).
	G _ I basis "minimal".
	self assert: G printString = '(x^2, xy, y^2 - (1/2)x)'! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:35:21'!
testGroebnerKatsura2
	| I G |
	I _ (ℚ polynomialsIn: 2) katsuraIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:35:25'!
testGroebnerKatsura3
	| I G |
	I _ (ℚ polynomialsIn: 3) katsuraIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:35:53'!
testGroebnerTrivial
	"The 3 polynomials have no common zeroes, the Groebner basis should be {1}."
	| Qx x y I |
	Qx _ ℚ polynomialsIn: #(x y) ordering: #lex.
	x _ Qx x.
	y _ Qx y.
	I _ Qx * {x + y. x^2 - 1. y^2 - (x*2)}.
	self assert: I isOne! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 10/9/2023 09:44:28'!
testAffineRingDimensionAsVectorSpace
	"Example from answer in math.stackexchange 'What does Hilbert series of monomial ideals describe?'."
	| S a b I |
	S := ℤ/17 polynomialsIn: #(a b).
	a := S x: 1.
	b := S x: 2.
	I := S * {a^6. a^3*b. a^2*(b^4). b^5}.
	self assert: (S/I) vectorSpaceDimension = 17.
	self assert: (S/I) basis size = 17! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 7/13/2023 16:09:59'!
testHilbertPolynomialExample1
	| R I H |
	R := ℚ polynomialsIn: #(x y z).
	I := R * (R !! [:x :y :z| {x^3*(y^2) + (x^2*(y^2)*z*3) + (y^3*(z^2)) + (z^5)}]).
	H := (R/I) hilbertPolynomial.
	self assert: H = (H parent x * 5 - 5)! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 7/13/2023 16:10:08'!
testHilbertPolynomialExample2
	| R I H |
	R := ℚ polynomialsIn: #(x y z).
	I := R * (R !! [:x :y :z| {x^2*(y^3). x*z}]).
	H := (R/I) hilbertPolynomial.
	self assert: H = (H parent x + 5)! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 7/13/2023 16:09:33'!
testHilbertSeriesExample1
	"Example from Sage (but Sage defines this as the Hilbert series of the ideal I instead of the quotient R/I):"
	| R I H t |
	R := ℚ polynomialsIn: #(x y z).
	I := R * (R !! [:x :y :z| {x^3*(y^2) + (x^2*(y^2)*z*3) + (y^3*(z^2)) + (z^5)}]).
	H := (R/I) hilbertSeries.
	t := H parent x.
	self assert: H = (t^4 + (t^3) + (t^2) + t + 1 / (t^2 - (t*2) + 1))! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 7/13/2023 16:09:25'!
testHilbertSeriesExample2
	"Example from Sage (but Sage defines this as the Hilbert series of the ideal I instead of the quotient R/I):"
	| R I H t |
	R := ℚ polynomialsIn: #(a b).
	I := R * (R !! [:a :b| {a^2*b. a*(b^2)}]).
	H := (R/I) hilbertSeries.
	t := H parent x.
	self assert: H = (t^3 - (t^2) - t - 1 / (t - 1))! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 5/23/2020 09:33:01'!
testHilbertSeriesOfPolynomialIdeal
	"Derksen, Kemper 'Computational Invariant Theory', Example 1.4.9."
	| t R x y z w I H |
	t _ ℚ polynomials fractions x.
	R _ ℚ polynomialsIn: #(x y z w) "ordering: #lex".
	x _ R x.
	y _ R y.
	z _ R z.
	w _ R x: 4.
	I _ R * {x*z - (y^2). x*w - (y*z). y*w - (z^2)}.
	H _ I hilbertSeriesAt: t.
	self assert: H = (t^2 * 3 - (t^3 * 2) / ((1 - t) ^ 4))! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 5/23/2020 09:33:01'!
testHilbertSeriesOfPolynomialQuotient
	"Derksen, Kemper 'Computational Invariant Theory', Example 1.4.9."
	| t R x y z w I H |
	t _ ℚ polynomials fractions x.
	R _ ℚ polynomialsIn: #(x y z w) "ordering: #lex".
	x _ R x.
	y _ R y.
	z _ R z.
	w _ R x: 4.
	I _ R * {x*z - (y^2). x*w - (y*z). y*w - (z^2)}.
	H _ (R/I) hilbertSeriesAt: t.
	self assert: H = (1 + (2*t) / (1 - t) squared)! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 3/6/2021 07:21:16'!
testHilbertSeriesOfPolynomialQuotient2
	"Derksen, Kemper 'Computational Invariant Theory', Example 1.4.9."
	| t R x y z w I H dim deg |
	t _ ℚ polynomials fractions x.
	R _ ℚ polynomialsIn: #(x y z w).
	x _ R x.
	y _ R y.
	z _ R z.
	w _ R x: 4.
	I _ R * {x*z - (y^2). x*w - (y*z). y*w - (z^2)}.
	H _ (R/I) hilbertSeriesAt: t.
	dim _ (H orderAt: 1) negated.
	self assert: (R/I) dimension = dim.
	deg _ (H * ((1 - H numerator parent x) ^ dim) value: 1) asInteger. "should be limit from below"
	self assert: I degree = deg.
	self assert: (R/I) proj degree = deg! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 5/23/2020 09:33:01'!
testHilbertSeriesOfPolynomialRing
	| t R H |
	t _ ℚ polynomials fractions x.
	R _ ℚ polynomialsIn: 4.
	H _ R hilbertSeriesAt: t.
	self assert: H = ((1 - t) ^ -4)! !

!CommutativeAlgebraTest methodsFor: 'ideal basics' stamp: 'len 5/23/2020 09:33:01'!
testIdealElementOperations
	"Magma handbook example H103E2."
	| R x y z I |
	R _ ℚ polynomialsIn: #(x y z) "ordering: #lex".
	x _ R x. y _ R y. z _ R z.
	I _ R * {(x + y)^3. (y - z)^2. y^2*z + z}.
	self assert: (I reduce: (y^2*z + z)) isZero.
	"I basis reduceFull: x^3 = ..."
	"I basis reduce: z^4 + (y^2))"
	self deny: (I includes: x+y).
	self assert: (I radicalContains: x+y).
	self assert: (I radicalContains: (x+y)^2).
	self deny: (I radicalContains: z).
	self assert: (x^4 + y - z s: x^2 + y - z) = (x^2*y negated + (x^2*z) + y - z)! !

!CommutativeAlgebraTest methodsFor: 'ideal basics' stamp: 'len 10/21/2023 13:49:29'!
testMonomialIdeal
	"Test ideals generated by monomials."
	| R f g h |
	R := ℚ polynomialsIn: 3.
	f := R !! [:x :y :z| x^2 + y].
	g := R !! [:x :y :z| z^3].
	h := R !! [:x :y :z| x^2*y].
	self assert: (R * {f. g. h}) isMonomial not.
	self assert: (R * {g. h}) isMonomial! !

!CommutativeAlgebraTest methodsFor: 'ideal basics' stamp: 'len 5/23/2020 09:34:55'!
testZeroIdeal
	| zero |
	zero _ ℤ * #(). "the zero ideal {0}"
	self assert: zero isZero.
	self assert: (zero includes: 0).
	self deny: (zero includes: 42)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealColon
	| R x y z I J |
	R _ ℚ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	z _ R z.
	I _ R * {x*y - 1. x^3*(z^2) - (y^2). x*(z^3) - x - 1}.
"	J _ R * {x*y - 1. x^2*z - y. x*(z^3) - x - 1}."
	self assert: I : R asIdeal = I.
	self assert: R asIdeal : I = R asIdeal.
	J _ R * {I atRandomBits: 10. I atRandomBits: 10}. "J <= I"
	self assert: I : J = R asIdeal! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 8/12/2022 13:07:11'!
testIdealColon2
	"Example from Cox IVA 205p."
	| R x y z I J Q |
	R _ ℚ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	z _ R z.
	I _ R * {x*z - (y^2). x^3 - (y*z)}.
	J _ R * {x. y}.
	Q _ I : J.
	self assert: Q = (I : (R * x) ∩ (I : (R * y))).
	self assert: Q = (R * {x*z - (y^2). x^3 - (y*z). x^2*y - (z^2)})! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealElimination
	"Example from Cox IVA 123 p. NOTE: this is a little slow."
	| Qx x y z I |
	Qx _ ℚ polynomialsIn: #(x y z).
	x _ Qx x. y _ Qx y. z _ Qx z.
	I _ Qx * {x^2 + y + z - 1. x + (y^2) + z - 1. x + y + (z^2) - 1}.
	self assert: (I eliminateAll: #(1 2)) generator = (z^6 - (z^4*4) + (z^3*4) - (z^2))! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 4/8/2021 12:35:55'!
testIdealEqualsUnitIdeal
	| R x y I |
	R _ ℤ/7 polynomialsIn: #(x y) ordering: #glex.
	x _ R x.
	y _ R y.
	I _ R * {x + 1. x - 1. y - 2}.
	self assert: I = R asIdeal! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 3/4/2021 18:21:29'!
testIdealHomogenization
	"The homogenization of an ideal can be bigger than the ideal generated by the homogenized generators."
	| P x y z f1 f2 I f3 J |
	P _ ℚ polynomialsIn: #(x y z).
	x _ P x.
	y _ P y.
	z _ P z.
	"Ideal for the twisted cubic:"
	f1 _ y - (x^2).
	f2 _ z - (x^3).
	I _ P * {f1. f2}.
	"Now a polynomial in I:"
	f3 _ f2 - (f1*x).
	self assert: (I homogenized includes: f3 homogenized).
	"But it's not in the ideal generated by the homogenized generators:"
	J _ (ℚ polynomialsIn: 4) * {f1 homogenized. f2 homogenized}.
	self deny: (J includes: f3 homogenized)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 8/12/2022 13:07:19'!
testIdealIntersection
	"Example from Cox IVA 194 p."
	| Qxy x y I J K |
	Qxy _ ℚ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	I _ Qxy * {x^2 * y}.
	J _ Qxy * {x * (y^2)}.
	K _ Qxy * {x^2*(y^2)}.
	self assert: I ∩ J = K! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealIntersectionRandom
	self testIdealIntersectionRandom: (ℚ polynomialsIn: #(x y))! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 8/12/2022 13:07:26'!
testIdealIntersectionRandom: aRing
	| R f1 f2 g I J K |
	R _ aRing.
	f1 _ R atRandomBits: 10.
	f2 _ R atRandomBits: 10.
	g _ R atRandomBits: 10.
	I _ R * {f1. g}.
	J _ R * {f2. g}.
	K _ R * {g}.
	self assert: I ∩ J >= K! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 7/5/2022 12:21:34'!
testIdealIsRadical
	"Becker, Weispfenning, 'Groebner Bases', Exercise 8.21."
	| R I |
	R _ ℚ polynomialsIn: #(x y).
	I _ R !! [:x :y| R * {x^2 + y. y^2 + x}].
	self assert: (R / I) dimension = 0.
	self assert: I isRadical! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealMembership
	| Qxy x y f g I J h k |
	Qxy _ ℚ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	f _ x^2 - y.
	g _ x^3 - x.
	I _ Qxy * {f. g}.
	J _ Qxy * {x^2. y^3}.
	h _ y^2 - y.
	k _ x*y - x.
	self assert: (I includes: h).
	self assert: (I includes: k).
	self assert: (J includes: h) not.
	self assert: (J includes: k) not! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealMembership2
	| Qxy x y I |
	Qxy _ ℚ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	I _ Qxy * {x*x. y*2 + x}.
	self assert: (I includes: (x + y) squared)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 7/5/2022 12:22:00'!
testIdealMembership3
	| Qx x y z w f g I |
	Qx _ ℚ polynomialsIn: #(x y z w).
	x _ Qx x.
	y _ Qx y.
	z _ Qx z.
	w _ Qx x: 4.
	f _ x*z-(y^2). "twisted cubic"
	g _ z*(y*w-(z^2)) - (w*(x*w-(y*z))).
	I _ Qx * {f. g}.
	"The twisted cubic C in P^3 is the set-theoretic complete intersection
	of f=0 and g=0, but not a scheme-theoretic or ideal-theoretic complete
	intersection (the ideal is not radical since includes f^2):"
	self assert: (I includes: f^2)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:00:26'!
testIdealMembership4
	| R x y z I |
	R _ ℤ/127 polynomialsIn: #(x y z) ordering: #glex.
	x _ R x.
	y _ R y.
	z _ R z.
	I _ R * {x*y + z. y^3 + 1. z^2 - (x*5) - 1}.
	self assert: (I includes: (x*y + z) + (y^3 + 1)).
	self assert: (I includes: x*z*(z^2 - (x*5) - 1))! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:34:55'!
testIdealMembership5
	| R x y I |
	R _ ℤ/7 polynomialsIn: #(x y) ordering: #glex.
	x _ R x.
	y _ R y.
	I _ R * {x*y + 1. y^2 - 1}.
	self assert: (I includes: x+y)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealMembershipRandom
	self testIdealMembershipRandom: (ℚ polynomialsIn: #(x y z))! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 2/23/2017 09:08:24'!
testIdealMembershipRandom: aRing
	| R f1 f2 f3 I g |
	R _ aRing.
	[(f1 _ R atRandomBits: 10) isZero] whileTrue.
	[(f2 _ R atRandomBits: 10) isZero] whileTrue.
	[(f3 _ R atRandomBits: 10) isZero] whileTrue.
	I _ R * {f1. f2. f3}.
	g _ I atRandomBits: 100.
	self assert: (I includes: g)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 11/5/2023 15:08:05'!
testIdealRadicalExample1
	| R x y I J |
	R := ℚ polynomialsIn: #(x y).
	x := R x: 1.
	y := R x: 2.
	I := R * {x^3. y^2}.
	J := R * {x. y}.
	self assert: I radical = J! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 11/5/2023 15:08:35'!
testIdealRadicalExample2
	| R I |
	R := ℤ/2 polynomialsIn: #(x y z w).
	I := R * (R !! [:x :y :z :w| {y+z. x*z squared * w. x squared*z squared}]).
	self assert: I radical = (R * (R !! [:x :y :z :w| {y+z. x*z}]))
! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 11/5/2023 15:10:04'!
testIdealRadicalExample3
	| R I |
	R := ℚ polynomialsIn: #(x y z w).
	I := R * (R !! [:x :y :z :w| {y+z. x*z squared * w. x squared*z squared}]).
	self assert: I radical = (R * (R !! [:x :y :z :w| {y+z. x*z}]))
! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 11/5/2023 15:12:22'!
testIdealRadicalExample4
	| R I |
	R := ℚ polynomialsIn: #(y z).
	I := R * (R !! [:y :z| {y+z. z squared}]).
	self assert: I radical = (R * (R !! [:y :z| {y. z}]))
! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 10/24/2023 16:07:13'!
testIdealRadicalZeroDimPerfectField
	"Seidenberg algorithm works for 0-dimensional ideals over a perfect field." 
	| R y z I J |
	R := ℚ polynomialsIn: #(y z).
	y := R x: 1.
	z := R x: 2.
	I := R * {y + z. z^2}.
	J := R * {y. z}.
	self assert: I radical = J! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealSaturation
	"Example from SageMath reference."
	| R x y z I J S |
	R _ ℚ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	z _ R z.
	I _ R * {x^5*(z^3). x*y*z. y*(z^4)}.
	J _ R * z.
	S _ R * {y. x^5}.
	self assert: (I saturation: J) = S! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 6/20/2023 21:02:43'!
testIdealSaturation2
	"Example from Macaulay2 documentation."
	| R a b c d I S |
	R := ℤ/32003 polynomialsIn: #(a b c d).
	a := R x: 1.
	b := R x: 2.
	c := R x: 3.
	d := R x: 4.
	I := R * {a^3 - (b*(d^2)). a^4 - (c*(d^3))}.
	S := R * {a*b - (c*d). a^2*c - (b^2*d). b^3 - (a*(c^2)). a^3 - (b*(d^2))}.
	self assert: I saturation = S! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 8/12/2022 13:07:34'!
testLocalIdeals
	"Magma handbook, example H104E4."
	| R I J A M |
	R _ ℚ polynomialsIn: #(x y z) ordering: #llex.
	I _ R * (R !! [:x :y :z| {x*y - z. x^3*(z^2) - (y^2). x*(z^3) - x - y}]).
	J _ R * (R !! [:x :y :z| {x*y - z. x^2*z - y. x*(z^3) - x - y}]).
	A _ R * (R !! [:x :y :z|
		{x^2 - (y^2) + (2*(x^3)*z).
		x*y + (y^2) - (x^3*z).
		y^3.
		x*z + (y*z).
		y*z.
		z^2}]).
	self assert: I * J = A.
	M _ R * (R !! [:x :y :z| {x + y. y^2. z}]).
	self assert: I ∩ J = M.
	self deny: A = M.
	self assert: A <= M! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 10/14/2022 08:40:24'!
testPolynomialRingHomomorphismKernel
	| A B x y φ |
	A _ ℚ polynomialsIn: #(x y z).
	B _ ℚ polynomialsIn: #(x y).
	x _ B x.
	y _ B y.
	φ _ A to: B images: {x-y. x. y}.
	self assert: φ kernel = (A * (A x - A y + A z)).
	self assert: (φ value: (φ kernel atRandomBits: 15)) isZero! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testRadicalMembership
	| R x y z g1 g2 f I |
	R _ ℚ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	z _ R z.
	g1 _ x^4*(y^2) + (z^2) - (x*(y^3)*z*4) - (y^5*z*2).
	g2 _ (x + (y^2)) ^ 2.
	f _ y*z - (x^3).
	I _ R * {g1. g2}.
	self assert: (I radicalContains: f)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testSquarefreeMonomialIdealPrimaryDecomposition
	"Miller, Sturmfels, 'Combinatorial Commutative Algebra', Example 1.8."
	| R a b c d e I |
	R _ ℚ polynomialsIn: #(a b c d e).
	a _ R x: 1. b _ R x: 2. c _ R x: 3. d _ R x: 4. e _ R x: 5.
	I _ R * {a*d. a*e. b*c*d. b*e. c*e. d*e}.
	self assert: I primaryDecomposition = {R * {d. e}. R * {a. b. e}. R * {a. c. e}. R * {a. b. c. d}} asSet! !

!CommutativeAlgebraTest methodsFor: 'krull dimension' stamp: 'len 5/23/2020 09:33:01'!
testKrullDimension
	| R I Q |
	R _ ℚ polynomialsIn: #(x y).
	self assert: R dimension = 2.
	I _ R * {R x. R y}.
	Q _ R / I.
	self assert: Q dimension = 0.
	I _ R * R zero. "zero ideal"
	Q _ R / I.
	self assert: Q dimension = 2! !

!CommutativeAlgebraTest methodsFor: 'krull dimension' stamp: 'len 5/23/2020 09:33:01'!
testKrullDimension2
	"Example from SageMath."
	| R x y I |
	R _ ℚ polynomialsIn: #(x y).
	x _ R x.
	y _ R y.
	I _ R * {x*y. x*y + 1}.
	self assert: (R / I) dimension < 0.
	I _ R * {x*(x*y + 1). y*(x*y + 1)}.
	self assert: (R / I) dimension = 1.
	I _ R * {x^3*y. x*(y^2)}.
	self assert: (R / I) dimension = 1! !

!CommutativeAlgebraTest methodsFor: 'krull dimension' stamp: 'len 5/23/2020 09:34:55'!
testKrullDimensionFiniteField
	"Example from SageMath."
	| R x y I Q |
	R _ ℤ/32003 polynomialsIn: #(x y z).
	self assert: R dimension = 3.
	x _ R x.
	y _ R y.
	I _ R * {x^2 - y. x^3}.
	Q _ R / I.
	self assert: Q dimension = 1! !

!CommutativeAlgebraTest methodsFor: 'krull dimension' stamp: 'len 5/23/2020 09:34:55'!
testKrullDimensionFiniteField2
	"Example from SageMath."
	| R x y I |
	R _ ℤ/2147483659 polynomialsIn: #(x y).
	x _ R x.
	y _ R y.
	I _ R * {x*y. x*y + 1}.
	self assert: (R / I) dimension < 0.
	I _ R * {x*(x*y + 1). y*(x*y + 1)}.
	self assert: (R / I) dimension = 1.
	I _ R * {x^3*y. x*(y^2)}.
	self assert: (R / I) dimension = 1! !

!CommutativeAlgebraTest methodsFor: 'krull dimension' stamp: 'len 5/23/2020 09:35:10'!
testKrullDimensionOverZZ
	| R I Q |
	R _ ℤ polynomialsIn: #(x y).
	self assert: R dimension = 3.
"	I _ R * {R x. R y}.
	Q _ R / I.
	self assert: Q dimension = 0.
"	I _ R * R zero. "zero ideal"
	Q _ R / I.
	self assert: Q dimension = 3! !

!CommutativeAlgebraTest methodsFor: 'testing examples' stamp: 'len 5/18/2023 18:51:58'!
testCyclic7Correctness
	"Test correctness of the ideal of cyclic 7-roots checking against one generated by Singular."
	| txt |
	txt := ((ℚ polynomialsIn: 7) cyclicIdealGenerators sorted: [:a :b| a leadingMonomial >= b leadingMonomial]) asArray printString.
	#('(' ')' '{' '}' ' ') do: [:each| txt := txt copyReplaceAll: each with: ''].
	self assert: txt = 'x1x2x3x4x5x6x7-1,x1x2x3x4x5x6+x1x2x3x4x5x7+x1x2x3x4x6x7+x1x2x3x5x6x7+x1x2x4x5x6x7+x1x3x4x5x6x7+x2x3x4x5x6x7,x1x2x3x4x5+x2x3x4x5x6+x1x2x3x4x7+x1x2x3x6x7+x1x2x5x6x7+x1x4x5x6x7+x3x4x5x6x7,x1x2x3x4+x2x3x4x5+x3x4x5x6+x1x2x3x7+x1x2x6x7+x1x5x6x7+x4x5x6x7,x1x2x3+x2x3x4+x3x4x5+x4x5x6+x1x2x7+x1x6x7+x5x6x7,x1x2+x2x3+x3x4+x4x5+x5x6+x1x7+x6x7,x1+x2+x3+x4+x5+x6+x7'! !

!CommutativeAlgebraTest methodsFor: 'testing examples' stamp: 'len 5/18/2023 18:52:09'!
testKatsura7Correctness
	"Test correctness of Katsura-7 ideal checking against one generated by Singular."
	| txt |
	txt := ((ℚ polynomialsIn: 7) katsuraIdealGenerators sorted: [:a :b| a leadingMonomial >= b leadingMonomial]) asArray printString.
	#('(' ')' '{' '}' ' ') do: [:each| txt := txt copyReplaceAll: each with: ''].
	self assert: txt = 'x1^2+2x2^2+2x3^2+2x4^2+2x5^2+2x6^2+2x7^2-x1,2x1x2+2x2x3+2x3x4+2x4x5+2x5x6+2x6x7-x2,x2^2+2x1x3+2x2x4+2x3x5+2x4x6+2x5x7-x3,2x2x3+2x1x4+2x2x5+2x3x6+2x4x7-x4,x3^2+2x2x4+2x1x5+2x2x6+2x3x7-x5,2x3x4+2x2x5+2x1x6+2x2x7-x6,x1+2x2+2x3+2x4+2x5+2x6+2x7-1'! !

!CommutativeAlgebraTest methodsFor: 'gradings' stamp: 'len 5/6/2023 18:56:46'!
testPolynomialsWithWeightedDegrees
	| P x y z I |
	P _ ℚ polynomialsIn: #(x y z) weights: #(1 2 4).
	self assert: P ordering weights = #(1 2 4).
	x _ P x: 1.
	y _ P x: 2.
	z _ P x: 3.
	self assert: x degree = 1.
	self assert: y degree = 2.
	self assert: z degree = 4.
	self assert: (x^2*y*(z^3)) degree = 16.
	self assert: x isHomogeneous.
	self deny: (x+y) isHomogeneous.
	self assert: (x^2 + y) isHomogeneous.
	I _ P * {x^2*y + z. (x^4 + z)^2. y^2 + z}.
	self assert: I isHomogeneous! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testAlgebraicExtension
	| x E a b |
	x _ ℚ polynomials x.
	E _ ℚ extension: x^4 - 2.
	a _ E x.
	self assert: (a^4) representative asScalar = 2.
	b _ a^2 + (E embed: 3).
	self assert: b minimalPolynomial = (x^2 - (x*6) + 7)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 4/14/2021 05:58:20'!
testCRT
	| crt |
	crt _ CRT moduli: #(2 5 7).
	self assert: (crt solve: #(0 2 0)) = 42! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/16/2016 13:35:30'!
testCartesianProduct
	| P x |
	P _ CartesianProduct components: {PrimeField new: 3. GaloisField new: 4. SymmetricGroup new: 5}.
	self assert: P elements size = (3*4*5 factorial).
	x _ {(P at: 1) atRandom. (P at: 2) atRandom. (P at: 3) atRandom}.
	self assert: (P elements includes: x)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testCyclotomicPolynomials
	| P |
	P _ ℚ polynomials.
	1 to: 20 do: [:n| self assert: (P cyclotomic: n) degree = n phi]! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testCyclotomicPolynomials2
	| P x n |
	P _ ℚ polynomials.
	x _ P x.
	n _ 100 atRandom.
	self assert: (n divisors product: [:d| P cyclotomic: d]) = (x^n - 1)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/9/2020 14:00:45'!
testExtendedEuclid
	| a b euclid g u v |
	a _ -1000 to: 1000 :: atRandom.
	b _ -1000 to: 1000 :: atRandom.
	euclid _ a xgcd: b.
	g _ euclid at: 1.
	u _ euclid at: 2.
	v _ euclid at: 3.
	self assert: g | a.
	self assert: g | b.
	self assert: ({u. v} * {a. b}) sum = g.
	self assert: g = (a gcd: b)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testFreeAlgebra
	| A f g h |
	A _ ℚ freeAlgebraIn: #(x y z).
	f _ A atRandomBits: 100.
	g _ A atRandomBits: 100.
	h _ A atRandomBits: 100.
	self assert: (f*g)*h = (f*(g*h)). "associativity"
	self assert: (f+g)*h = (f*h + (g*h)). "distributivity"
	self assert: f * A one = f.
	self assert: A one * f = f! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:38:39'!
testFunctionComposition
	| f g a |
	f _ ℤ carrier to: ℤ carrier map: [:x| x * 2].
	g _ ℤ carrier to: ℤ carrier map: [:x| x + 1].
	a _ 1000 atRandom.
	self assert: (f · g value: a) = (f value: (g value: a)).
	self assert: (g · f value: a) = (g value: (f value: a))! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:38:47'!
testFunctionComposition2
	| f g a |
	f _ ℤ carrier to: ℚ carrier map: [:x| x asRational / 2].
	g _ ℚ carrier to: ℤ carrier map: [:x| x numerator].
	a _ 1000 atRandom.
	self assert: (f · g value: a asRational) = (f value: (g value: a asRational)).
	self assert: (g · f value: a) = (g value: (f value: a))! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:38:55'!
testFunctionComposition3
	| f g |
	f _ ℤ carrier to: ℚ carrier map: [:x| x / 2].
	g _ ℤ carrier to: ℤ carrier map: [:x| x + 1].
	self should: [g · f] raise: DomainError! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testIteratorCovering
	| F |
	F _ ℤ / 3.
	self assert: (F^3 upTo: 3^3) asSet size = (3^3).
	self assert: (ℤ^3 upTo: 100) asSet size = 100! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/16/2016 13:35:55'!
testIteratorCovering2
	| F |
	F _ GaloisField new: 9.
	self assert: ((F^3) upTo: 9^3) asSet size = (9^3).
	self assert: ((F^3) upTo: 9^3+100) asSet size = (9^3)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/16/2016 13:36:04'!
testIteratorCovering3
	| F |
	F _ GaloisField new: 3.
	self assert: ((F^4) upTo: 3^4) asSet size = (3^4).
	self assert: ((F^4) upTo: 3^4+100) asSet size = (3^4)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:24:41'!
testMapsInducedByHom
	| X Y Z f g h yz zx |
	X _ #(1 2 3) asSet asDomain.
	Y _ {$x. $y. $z} asSet asDomain.
	Z _ #(a b c) asSet asDomain.
	f _ X to: Y map: [:each| Y atRandom: (Random seed: each hash)].
	g _ f ⇒ Z. "Hom(-,Z)"
	self assert: g domain = (Y ⇒ Z). "it is contravariant"
	self assert: g codomain = (X ⇒ Z).
	yz _ Y to: Z map: [:each| Z atRandom: (Random seed: each hash)].
	self assert: (g value: yz) = (yz · f).
	h _ Z ⇒ f. "Hom(Z,-)"
	self assert: h domain = (Z ⇒ X). "it is covariant"
	self assert: h codomain = (Z ⇒ Y).
	zx _ Z to: X map: [:each| X atRandom: (Random seed: each hash)].
	self assert: (h value: zx) = (f · zx)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 2/1/2018 22:41:50'!
testPermutationProduct
	| G a b |
	G _ SymmetricGroup new: 5.
	a _ G !! #((1 4 5) (2 3)).
	b _ G !! #((2 4) (5 1)).
	self assert: b*a = (G !! #((1 2 3 4))).
	self assert: a*a = (G !! #((1 5 4)))! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 2/1/2018 22:42:32'!
testPermutationProduct2
	| G a b |
	G _ SymmetricGroup new: 5.
	a _ G !! #((1 4 5) (2 3)).
	b _ G !! #((2 4) (5 1)).
	self assert: b*a = (G !! #((1 2 3 4))).
	self assert: a*a = (G !! #((1 5 4)))! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 2/1/2018 23:38:29'!
testPermutationProduct3
	| G a b |
	G _ SymmetricGroup new: 4.
	a _ G !! #((2 3 4)).
	b _ G !! #((1 2) (3 4)).
	self assert: b*a = (G !! #((1 2 4))).
	self assert: a*b = (G !! #((1 3 2)))! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:48:21'!
testPolynomialVectorSpace
	| Qx V S x |
	Qx := ℚ polynomials.
	x := Qx x.
	V := Qx filtrationAt: 5.
	S := V sub: {Qx one. x^2. x*2}.
	self assert: (S includes: V !! (x^2 + 2)).
	self deny: (S includes: V !! (x^3 + 2)).
	self assert: Qx !! (S orthogonalProjection value: V !! (x^3 - (x^2) + (x*2) + 3)) = ((x^2) negated + (x*2) + 3)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 6/16/2023 13:35:33'!
testSplittingField1
	| f K |
	f := (ℤ/7) polynomials !! [:x| x^2 + 1].
	K := f splittingField.
	self assert: K isFiniteField.
	self assert: K size = 49! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 6/16/2023 13:35:37'!
testSplittingField2
	| f K |
	f := (ℤ/7) polynomials !! [:x| x^2 - 1].
	K := f splittingField.
	self assert: K isFiniteField.
	self assert: K size = 7! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 6/17/2023 22:41:12'!
testSplittingField3
	| p r f K |
	p := (3 to: 7) atRandom nextPrime.
	self flag: #fixme.
	r := 2 atRandom. "3 is slow"
	f := (ℤ/p) polynomials !! [:x| x^(p^r) - x].
	K := f splittingField.
	self assert: K isFiniteField.
	self assert: K characteristic = p.
	self assert: K degree = r! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 3/3/2016 23:22'!
testWord
	| x y |
	x _ Word x: #x.
	y _ Word x: #y.
	self assert: (x*y) * y reciprocal = x.
	self assert: (x*y * (y reciprocal * x reciprocal)) isEmpty.
	self assert: x*y*y * (y reciprocal * y reciprocal * x) = (x^2).
	self assert: x * (y * y * x) = (x * y * (y * x)).
	self assert: (x*y * (x*y) reciprocal) isEmpty! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 1/10/2018 11:31:05'!
testWordOrdering
	| F p q r |
	F _ FreeMonoid new: 3.
	p _ F atRandomBits: 20 atRandom.
	r _ F atRandomBits: 20 atRandom.
	[p < (q _ F atRandomBits: 20 atRandom)] whileFalse.
	self assert: p*r <= (q*r).
	self assert: r*p <= (r*q).
	p _ q * r.
	self assert: (p > q or: [p = q and: [r isEmpty]]).
	self assert: (p > r or: [p = r and: [q isEmpty]])! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 4/28/2021 10:52:02'!
testAllMonomialsOfDegree
	| d n |
	n _ 5 atRandom.
	d _ 5 atRandom.
	self assert: (FreeAbelianMonoid new: n :: allMonomialsOfDegree: d) size = (n+d-1 choose: d)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 2/26/2017 09:25:17'!
testAntigradedLexOrdering
	"Example from 'Using Algebraic Geometry', Cox, Little, O'Shea, Ch. 4, 3.2."
	| M x y |
	M _ FreeAbelianMonoid new: 2 ordering: #lglex. "antigraded lex order"
	x _ M x: 1.
	y _ M x: 2.
	self assert: M identity > x.
	self assert: x > y.
	self assert: x^2 > (x*y).
	self assert: x*y > (y^2).
	self assert: (y^2) > (x^3)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 2/26/2017 09:25:24'!
testAntigradedRevlexOrdering
	"Example from 'Using Algebraic Geometry', Cox, Little, O'Shea, Ch. 4, 3.3."
	| M x y z ordering |
	"The antigraded revlex order:"
	ordering _ LocalGradedReverseLexicographicOrdering indeterminates: #(3 2 1).
	M _ FreeAbelianMonoid new: 3 ordering: ordering.
	x _ M x: 1.
	y _ M x: 2.
	z _ M x: 3.
	self assert: M identity > x.
	self assert: x > y.
	self assert: y > z.
	self assert: x^2 > (x*y).
	self assert: x*y > (y^2).
	self assert: (x*z) > (y*z).
	self assert: (y*z) > (z^2)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 7/4/2022 12:29:28'!
testHash
	| m1 m2 |
	m1 _ Monomial exponents: #(1 2 0) parent: nil.
	m2 _ Monomial exponents: #(1 2 0 0 0 0 0) parent: nil.
	self assert: m1 hash = m2 hash! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 7/26/2022 13:44:21'!
testLocalMonomialOrderings
	"Examples from Magma handbook."
	| f |
	f _ (ℤ polynomialsIn: #(x y z) ordering: #llex) !! [:x :y :z| 1 + x + y + z + (x^7) + (x^8*(y^7)) + (y^5) + (z^10)].
	self assert: f printString = '1 + x + x^7 + y + y^5 + x^8y^7 + z + z^{10}'.
	self assert: (f orderedBy: #lgrevlex) printString = '1 + z + y + x + y^5 + x^7 + z^{10} + x^8y^7'! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 2/26/2017 09:25:19'!
testMonomialDivision
	| M x y |
	M _ FreeAbelianMonoid new: 2.
	x _ M x: 1.
	self assert: x*x = (M x: 1 to: 2).
	y _ M x: 2.
	self assert: x*x*y / (x*y) = x.
	self assert: x*y*x / x = (x*y).
	self assert: x*y*x*y / (x*y) = (x*y)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 2/26/2017 09:25:22'!
testMonomialGCD
	| M x y a b |
	M _ FreeAbelianMonoid new: 2.
	x _ M x: 1.
	y _ M x: 2.
	a _ x*x*y*y*y.
	b _ x*x*x*y*y.
	self assert: (a gcd: b) = (x*x*y*y)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 2/26/2017 09:25:12'!
testMonomialLCM
	| M x y a b |
	M _ FreeAbelianMonoid new: 2.
	x _ M x: 1.
	y _ M x: 2.
	a _ x*x.
	b _ x*y*y.
	self assert: (a lcm: b) = (x*x*y*y)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 7/29/2021 17:22:36'!
testMonomialOrderings
	| P x y z f |
	P _ ℤ polynomialsIn: 3.

	x _ P x.
	y _ P y.
	z _ P z.

	f _ 10*x - (7*(y^4)) + (11*(y^3)*z).

	self assert: (f orderedBy: #lex) leadingCoefficient = 10.
	self assert: (f orderedBy: #glex) leadingCoefficient = -7.
	self assert: (f orderedBy: #grevlex) leadingCoefficient = -7 "this matches the definition in Singular"! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMonomialOrderings2
	"Examples from Macaulay2 documentation."
	| R a b c d f |
	R _ ℤ polynomialsIn: #(a b c d).
	a _ R x: 1.
	b _ R x: 2.
	c _ R x: 3.
	d _ R x: 4.

	f _ a + (b^100) + (c*d).

	self assert: (f orderedBy: #lex) printString = 'a + b^{100} + cd'.
	self assert: (f orderedBy: #grevlex) printString = 'b^{100} + cd + a'.
	self assert: (f orderedBy: #glex) printString = 'b^{100} + cd + a'! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMonomialOrderings3
	"Examples from Derksen, Kemper 'Computational Invariant Theory' 1.1.1."
	| P x1 x2 x3 x4 f |
	P _ ℤ polynomialsIn: 4.
	x1 _ P x: 1.
	x2 _ P x: 2.
	x3 _ P x: 3.
	x4 _ P x: 4.
	f _ x1 + (x2*x4) + (x3^2).
	self assert: (f orderedBy: #lex) leadingTerm = x1.
	self assert: (f orderedBy: #glex) leadingTerm = (x2*x4).
	self assert: (f orderedBy: #grevlex) leadingTerm = (x3^2)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 11:17:06'!
testMonomialOrderings4
	"Examples from en.wikipedia.org/wiki/Monomial_order to illustrate the difference between graded lexicographic and graded reverse lexicographic."
	| M S |
	M _ FreeAbelianMonoid new: 3 ordering: #glex.
	S _ Set new.
	1 to: 3 do: [:i| i to: 3 do: [:j| S add: (M x: i) * (M x: j)]].
	self assert: (S sorted: [:a :b| a > b]) printString = '(x1^2, x1x2, x1x3, x2^2, x2x3, x3^2)'.
	M _ M orderedBy: #grevlex.
	S _ S collect: [:each| M !! each].
	self assert: (S sorted: [:a :b| a > b]) printString = '(x1^2, x1x2, x2^2, x1x3, x2x3, x3^2)'! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2020 06:30:07'!
testMonomialOrderingsExample
	| a b lex glex grevlex |
	lex _ FreeAbelianMonoid new: 3 ordering: #lex.
	a _ lex exponents: #(3 2 8).
	b _ lex exponents: #(2 9 2).
	glex _ FreeAbelianMonoid new: 3 ordering: #glex.
	grevlex _ FreeAbelianMonoid new: 3 ordering: #grevlex.
	self assert: a > b.
	self assert: glex !! a > (glex !! b).
	self assert: grevlex !! a < (grevlex !! b)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2020 06:30:42'!
testMonomialOrderingsExample2
	| a b lex glex |
	lex _ FreeAbelianMonoid new: 3 ordering: #lex.
	a _ lex exponents: #(0 2 9).
	b _ lex exponents: #(0 5 3).
	glex _ FreeAbelianMonoid new: 3 ordering: #glex.
"	grevlex _ FreeAbelianMonoid new: 3 ordering: #grevlex."
	self assert: b > a.
	self assert: glex !! a > (glex !! b).
"	self assert: grevlex !! a < (grevlex !! b)"! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2020 06:31:12'!
testMonomialOrderingsExample3
	| a b lex glex grevlex |
	lex _ FreeAbelianMonoid new: 3 ordering: #lex.
	a _ lex exponents: #(2 1 2).
	b _ lex exponents: #(1 3 1).
	glex _ FreeAbelianMonoid new: 3 ordering: #glex.
	grevlex _ FreeAbelianMonoid new: 3 ordering: #grevlex.
	self assert: a > b.
	self assert: glex !! a > (glex !! b).
	self assert: grevlex !! a < (grevlex !! b)! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 7/29/2021 17:23:59'!
testMonomialOrderingsOfIndeterminates
	| M x y z |
	M _ FreeAbelianMonoid new: 3.
	x _ M x: 1.
	y _ M x: 2.
	z _ M x: 3.
	{#lex.
	#llex.
	#glex.
	#grevlex} do: [:ordering| 
		self assert: (x orderedBy: ordering) > (y orderedBy: ordering).
		self assert: (y orderedBy: ordering) > (z orderedBy: ordering)]! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 2/26/2017 09:25:09'!
testMonomialOrderingsOfIndeterminatesReordered
	| M x y z |
	M _ FreeAbelianMonoid new: 3 ordering: (MonomialOrdering lex: #(2 1 3)).
	x _ M x: 1.
	y _ M x: 2.
	z _ M x: 3.
	self assert: y > x.
	self assert: x > z! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testMonomialOrderingsSingularExample
	"'A Singular Introduction to Commutative Algebra', Example 1.2.13 (monomial orderings)."
	| f |
	f _ (ℚ polynomialsIn: #(x y z)) !! [:x :y :z| x^3*y*z + (x^3) + (x*(y^2)) + (y^5) + (z^4)].
	"Global orderings:"
	self assert: (f orderedBy: #lex) printString =  'x^3yz + x^3 + xy^2 + y^5 + z^4'.
	self assert: (f orderedBy: #grevlex) printString = 'y^5 + x^3yz + z^4 + x^3 + xy^2'.
	self assert: (f orderedBy: #glex) printString = 'x^3yz + y^5 + z^4 + x^3 + xy^2'.
	"TODO: weighted total lexicographic ordering"
	"Local orderings don't match because we implemented them with Magma conventions."
! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 2/26/2017 09:25:14'!
testMonomialProduct
	| M x y |
	M _ FreeAbelianMonoid new: 2.
	x _ M x: 1.
	self assert: x*x = (M x: 1 to: 2).
	y _ M x: 2.
	self assert: x*y = (y*x).
	self assert: x*y*y = (x*(y*y)).
	self assert: ((x*y*y) at: 2) = 2.
	self assert: (x*y*x*x*y) degree = 5! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testMinimalPolynomial
	"Example from 'Algebraic Number Theory: A Computational Approach' by William Stein (28 pp)."
	| x K a |
	x _ ℚ polynomials x.
	K _ ℚ extension: x^2 - 2.
	a _ K x.
	self assert: a minimalPolynomial = (x^2 - 2).
	self assert: (a/2 + 3) minimalPolynomial = (x^2 - (x*6) + (17/2))! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:22:33'!
testMinimalPolynomial2
	"Example from 'Algebraic Number Theory: A Computational Approach' by William Stein (29 pp)."
	| x K a alpha |
	x := ℚ polynomials x.
	K := QuadraticField new: 5.
	a := K x.
	self assert: a squared = (K !! 5).
	alpha := (1 + a) / 2.
	self assert: alpha minimalPolynomial = (x^2 - x - 1)! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testNormTraceAndMinimalPolynomial
	"Example H38E17 from Magma documentation."
	| x F a |
	x _ ℚ polynomials x.
	F _ ℚ extension: x^4 - (x^2*420) + 40000.
	a _ F x / 2.
	self assert: a trace = 0.
	self assert: a norm = 2500.
	self assert: a minimalPolynomial = (x^4 - (x^2*105) + 2500)! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:22:39'!
testQuadraticFieldSqrt2
	| E x |
	E := QuadraticField new: 2.
	x := ℚ polynomials x.
	self assert: E x minimalPolynomial = E modulus.
	self assert: E x squared minimalPolynomial = (x - 2).
	self assert: E discriminant = 8! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:22:45'!
testQuadraticFieldSqrtMinusFive
	| E |
	E := QuadraticField new: -5.
	self assert: E x minimalPolynomial = E modulus.
	self assert: E x squared = (E embed: -5).
	self assert: E x ^ 4 = (E embed: 25).
	self assert: E discriminant = -20! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/16/2021 10:26:19'!
testSignature
	| x F |
	x _ ℚ polynomials x.
	F _ NumberField modulus: x^2 + 1.
	self assert: F signature = #(0 1).
	F _ NumberField modulus: x^3 - 2.
	self assert: F signature = #(1 1).
	F _ CyclotomicField new: 5.
	self assert: F signature = #(0 2).
	F _ NumberField modulus: x^6 + x + 1.
	self assert: F signature = #(0 3).
	F _ NumberField modulus: x^3 + (x^2) - (x*5) - 1.
	self assert: F signature = #(3 0)! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/19/2023 11:28:31'!
testTotallyImaginary
	| x E |
	x := ℚ polynomials x.
	E := NumberField modulus: x^2 + 2.
	self assert: E isTotallyImaginary.
	E := NumberField modulus: x^2 - 2.
	self assert: E isTotallyImaginary not.
	E := NumberField modulus: x^4 - 2.
	self assert: E isTotallyImaginary not! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/19/2023 11:29:02'!
testTotallyReal
	| x E |
	x := ℚ polynomials x.
	E := NumberField modulus: x^2 + 2.
	self assert: E isTotallyReal not.
	E := NumberField modulus: x^2 - 2.
	self assert: E isTotallyReal.
	E := NumberField modulus: x^4 - 2.
	self assert: E isTotallyReal not! !

!PolynomialTest methodsFor: 'arithmetic' stamp: 'len 4/25/2018 21:52:52'!
testArithmeticOver: R
	| x f g |
	x _ R polynomials x.
	f _ x^2 + (x*2) - 1.
	g _ x^2 - (x*2) + 1.
	self assert: f+g = (x^2 * 2).
	self assert: f-g = (x*4 - 2).
	self assert: f*g = (x^4 - (x^2 * 4) + (x*4) - 1).
	self assert: f*g // g = f.
	self assert: (f*g \\ g) isZero.
	self assert: (f*g + (x - 1) \\ g) = (x - 1)
! !

!PolynomialTest methodsFor: 'arithmetic' stamp: 'len 5/23/2020 09:33:21'!
testArithmeticQQ
	self testArithmeticOver: ℚ! !

!PolynomialTest methodsFor: 'arithmetic' stamp: 'len 5/23/2020 09:34:55'!
testArithmeticZ2
	self testArithmeticOver: ℤ/2! !

!PolynomialTest methodsFor: 'arithmetic' stamp: 'len 5/23/2020 09:34:55'!
testArithmeticZ3
	self testArithmeticOver: ℤ/3! !

!PolynomialTest methodsFor: 'arithmetic' stamp: 'len 4/20/2022 13:12:40'!
testArithmeticZm
	self testArithmeticOver: ℤ/(2 + 10000000000000 atRandom)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/12/2020 12:33:23'!
testDivision: F
	| Fx f g |
	Fx _ F polynomials.
	f _ Fx atRandomBits: 50 maxDegree: 5 atRandom.
	[g _ Fx atRandomBits: 50 maxDegree: 5 atRandom. g isZero] whileTrue.
	self assert: (f // g) * g + (f \\ g) = f! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/28/2023 19:34:16'!
testDivisionPrimeField65535
	self flag: #fixme. "this hangs with 2^16 - 1"
	self testDivision: (PrimeField new: 2^17 - 1)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/10/2020 02:22:44'!
testDivisionPrimeFieldZ2
	self testDivision: (PrimeField new: 2)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testDivisionQ
	self testDivision: ℚ! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testDivisionQ1
	| P x f g |
	P _ ℚ polynomials.
	x _ P x.
	f _ x.
	g _ 1 - (x*3).
	self assert: f // g = (P !! (-1/3)).
	self assert: f \\ g = (P !! (1/3))! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testDivisionQ2
	| P x f g |
	P _ ℚ polynomials.
	x _ P x.
	f _ x.
	g _ 1 + (x*3).
	self assert: f // g = (P !! (1/3)).
	self assert: f \\ g = (P !! (-1/3))! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testDivisionRationalFunctions
	self testDivision: ℚ polynomials fractions! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionRationalFunctionsZ2
	self testDivision: (ℤ/2) polynomials fractions! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/13/2020 08:53:01'!
testDivisionRationalFunctionsZ65535
	self testDivision: (PrimeField new: 2^17 - 1) polynomials fractions! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ1
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x.
	g _ 1 + (x*3).
	self assert: f // g = (P !! 0).
	self assert: f \\ g = x! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ2
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x.
	g _ 1 - (x*3).
	self assert: f // g = (P !! -1).
	self assert: f \\ g = (1 - (x*2))! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ3
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x.
	g _ 1 - x.
	self assert: f // g = (P !! -1).
	self assert: f \\ g = (P !! 1)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ4
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x.
	g _ 1 + x.
	self assert: f // g = (P !! 1).
	self assert: f \\ g = (P !! -1)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ5
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x*4.
	g _ x*2.
	self assert: f // g = (P !! 2).
	self assert: f \\ g = (P !! 0)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ6
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x*4.
	g _ x*3.
	self assert: f // g = (P !! 1).
	self assert: f \\ g = x! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ7
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x*4+1.
	g _ x^2*3.
	self assert: f // g = (P !! 0).
	self assert: f \\ g = f! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ8
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x*(-10).
	g _ x*(-9).
	self assert: f // g = (P !! 1).
	self assert: f \\ g = x negated! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ9
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x*(-10).
	g _ x*(-3).
	self assert: f // g = (P !! 3).
	self assert: f \\ g = (x*(-1))! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:17:56'!
testExactDivisionBug
	| Zx x f |
	Zx _ ℤ polynomials.
	x _ Zx x.
	f _ x*2.
	self assert: f / f = Zx one! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:18:03'!
testExactDivisionMultivariate
	| Qxy x y f g |
	Qxy _ ℚ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	f _ x-y.
	g _ x+y.
	self assert: f * g / g = f.
	self assert: f * g / f = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testExactDivisionMultivariate2
	| Qxy x y f g |
	Qxy _ ℚ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	f _ x-y.
	g _ x+y.
	self assert: (f * g divisionBy: g) quotient = f.
	self assert: (f * g divisionBy: f) quotient = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:18:09'!
testExactDivisionQ
	| x f g |
	x _ ℚ polynomials x.
	f _ x^2 + x + 1.
	g _ 1 - x.
	self assert: f * g / g = f.
	self assert: f * g / f = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:18:14'!
testExactDivisionQRandom
	| Qx f g |
	Qx _ ℚ polynomials.
	f _ Qx atRandomBits: 100 maxDegree: (1 to: 10) atRandom.
	g _ Qx atRandomBits: 100 maxDegree: (1 to: 10) atRandom.
	self assert: f * g / g = f.
	self assert: f * g / f = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:18:21'!
testExactDivisionZ
	| x f g |
	x _ ℤ polynomials x.
	f _ x^2 + x + 1.
	g _ 1 - x.
	self assert: f * g / g = f.
	self assert: f * g / f = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 7/8/2022 11:07:09'!
testMultivariateDivision
	"Example from Cox IVA 62 p."
	| Zxy x y f f1 f2 division |
	Zxy _ ℤ polynomialsIn: #(x y) ordering: (MonomialOrdering lex: #(2 1)).
	x _ Zxy x.
	y _ Zxy y.
	f _ x*(y^2)+1.
	f1 _ x*y+1.
	f2 _ y+1.
	division _ f divisionBy: {f1. f2}.
	self assert: division quotients = {y. y one negated}.
	self assert: division remainder = (y one * 2).
	division validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 7/8/2022 11:07:14'!
testMultivariateDivision2
	"Example from Cox IVA 63 p."
	| Zxy x y f f1 f2 division |
	Zxy _ ℤ polynomialsIn: #(x y) ordering: (MonomialOrdering lex: #(2 1)).
	x _ Zxy x.
	y _ Zxy y.
	f _ x^2*y + (x*(y^2)) + (y^2).
	f1 _ x*y-1.
	f2 _ y^2-1.
	division _ f divisionBy: {f1. f2}.
	self assert: division quotients = {x+y. y one}.
	self assert: division remainder = (x+y+1).
	division validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 7/8/2022 11:07:21'!
testMultivariateDivision3
	"Example from Cox IVA 67 p."
	| Zxy x y f f1 f2 division |
	Zxy _ ℤ polynomialsIn: #(x y) ordering: (MonomialOrdering lex: #(2 1)).
	x _ Zxy x.
	y _ Zxy y.
	f _ x^2*y + (x*(y^2)) + (y^2).
	f1 _ x*y-1.
	f2 _ y^2-1.
	division _ f divisionBy: {f2. f1}.
	self assert: division quotients = {x+1. x}.
	self assert: division remainder = (x*2+1).
	division validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 7/8/2022 11:07:28'!
testPseudoDivision1
	| Zxy x y division |
	Zxy _ ℤ polynomialsIn: #(x y).
	x _ Zxy x.
	y _ Zxy y.
	division _ x^2 + (y^2) pseudoDivisionBy: y - x in: 2.
	self assert: division quotient asMultivariate = (x + y).
	self assert: division remainder asMultivariate = (x^2 * 2).
	division validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 7/8/2022 11:07:35'!
testPseudoDivision2
	| Zxy x y division |
	Zxy _ ℚ polynomialsIn: #(x y).
	x _ Zxy x.
	y _ Zxy y.
	division _ x^2 + (y^2) pseudoDivisionBy: (y-x)*2 in: 2.
	self assert: division quotient asMultivariate = ((x+y) * 2).
	self assert: division remainder asMultivariate = (x^2 * 8).
	division validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 7/8/2022 11:07:46'!
testPseudoDivision3
	| Zxy x y division |
	Zxy _ ℤ polynomialsIn: #(x y).
	x _ Zxy x.
	y _ Zxy y.
	division _ x^2 + (y^2) pseudoDivisionBy: y - x in: 2.
	self assert: division quotient asMultivariate = (x + y).
	self assert: division remainder asMultivariate = (x^2 * 2).
	division validate.

	division _ x^2 + (y^2) pseudoDivisionBy: x - y in: 2.
	self assert: division quotient asMultivariate = (x + y) negated.
	self assert: division remainder asMultivariate = (x^2 * 2).
	division validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 7/8/2022 11:07:54'!
testPseudoDivision: R
	| Rx f g division |
	Rx _ R polynomials.
	f _ Rx atRandomBits: 50 maxDegree: 5 atRandom.
	[g _ Rx atRandomBits: 50 maxDegree: 5 atRandom. g isZero] whileTrue.
	division _ f pseudoDivisionBy: g.
	division validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testPseudoDivisionOverPolynomialRing1
	self testPseudoDivision: ℚ polynomials! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testPseudoDivisionOverPolynomialRing2
	self testPseudoDivision: (ℚ polynomialsIn: 2)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 7/8/2022 11:08:00'!
testPseudoDivisionUnivariate
	| Zx x division |
	Zx _ ℤ polynomials.
	x _ Zx x.
	division _ x^3 + x + 1 pseudoDivisionBy: (x^2 * 3) + x + 1.
	self assert: division quotient = (x*3 - 1).
	self assert: division remainder = (x*7 + 10).
	self assert: division delta = 2.
	division validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/8/2023 16:38:46'!
testPseudoDivisionUnivariate2
	"Example from Zippel book"
	| Zx a b |
	Zx _ ℤ polynomials.
	a _ Zx !! #(1 0 1 0 -3 -3 8 2 -5) reversed.
	b _ Zx !! #(3 0 5 0 -4 -9 21) reversed.
	(a pseudoDivisionBy: b) validate! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionZ
	self testPseudoDivision: ℤ! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionZ12
	self testPseudoDivision: ℤ / 12! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionZ6
	self testPseudoDivision: ℤ / 6! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionZ8
	self testPseudoDivision: ℤ / 8! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/8/2023 16:38:53'!
testPseudoRemainder
	| Zx x a b r |
	Zx _ ℤ polynomials.
	x _ Zx x.
	a _ x^8 + (x^6) - (3*(x^4)) - (3*(x^3)) + (8*(x^2)) + (2*x) - 5.
	b _ 3*(x^6) + (5*(x^4)) - (4*(x^2)) - (9*x) + 21.
	r _ Zx !! #(-9 0 3 0 -15).
	self assert: (a pseudoRem: b) = r.
	self assert: (a pseudoDivisionBy: b) remainder = r! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 4/14/2021 05:58:31'!
testCRT
	| x f1 f2 f3 crt r1 r2 r3 h |
	x _ ℚ polynomials x.
	f1 _ x^5 + 1.
	f2 _ x^2 + 1.
	f3 _ x - 1.
	crt _ CRT moduli: {f1. f2. f3}.
	r1 _ x-1.
	r2 _ x+1.
	r3 _ x-x. "zero"
	h _ crt solve: {r1. r2. r3}.
	self assert: h \\ f1 = r1.
	self assert: h \\ f2 = r2.
	self assert: h \\ f3 = r3! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 12/19/2022 11:23:04'!
testDomainError
	| f |
	f _ ℤ polynomials atRandomBits: 10.
	self should: [f / 10007] raise: Error.
"	g _ ℚ polynomials atRandomBits: 100 maxDegree: 5. ""will have denominators""
	self should: [f + g] raise: Error.
	self should: [f * g] raise: Error"! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 6/7/2023 16:40:07'!
testFlatten
	| R f |
	R := ℚ polynomialsIn: 3 atRandom :: polynomialsIn: 3 atRandom.
	50 timesRepeat:
		[f := R atRandom.
		self assert: (R !! (R flatten !! f)) = f.
		f := R scalars atRandom.
		self assert: (R !! (R flatten !! f)) = (R constant: f)]! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testGaussLemma
	"Lemma (Gauss): The product of two primitive polynomials is primitive."
	| Zx f g |
	Zx _ ℤ polynomials.
	f _ Zx atRandomBits: 100 maxDegree: (5 to: 10) atRandom.
	g _ Zx atRandomBits: 100 maxDegree: (5 to: 10) atRandom.
	self assert: (f primitivePart * g primitivePart) isPrimitive! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testHeight
	| x |
	x _ ℚ polynomials x.
	self assert: (x * 2 - 5) height =  5.
	self assert: (x * 5 - 2) height =  5! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testInterpolation
	| Qx f |
	Qx _ ℚ polynomials.
	f _ Qx interpolate: {1 -> 2. 2 -> 3. 4 -> 5}.
	self assert: (f value: 1) = 2.
	self assert: (f value: 2) = 3.
	self assert: (f value: 4) = 5! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testInterpolation2
	| Qx f |
	Qx _ ℚ polynomials.
	f _ Qx interpolate: {1 -> 2. 2 -> 4. 3 -> 9}.
	self assert: (f value: 1) = 2.
	self assert: (f value: 2) = 4.
	self assert: (f value: 3) = 9! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/8/2023 16:38:15'!
testIsNilpotentOverZm
	| m R N U d f k |
	m _ (2 to: 100) atRandom.
	R _ ℤ/m.
	N _ R nilradical asSet.
	U _ R units asSet.
	d _ (1 to: 5) atRandom.
	f _ R polynomials !! ((1 to: d) collect: [:i| N atRandom]).
	self assert: f isNilpotent.
	k _ (1 to: d) atRandom.
	f _ R polynomials !! ((1 to: d) collect: [:i| i=k ifTrue: [U atRandom] ifFalse: [N atRandom]]).
	self assert: f isNilpotent not! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testIsSeparableNot
	"A tipical example of a polynomial that is NOT separable."
	| p F x Fy y f |
	p _ 5. "any prime p"
	F _ ℤ/p :: polynomials fractions.
	x _ F x.
	Fy _ F polynomialsIn: #(y).
	y _ Fy x.
	f _ y^p - x.
	self assert: f isSeparable not! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/8/2023 16:38:29'!
testIsUnitOverZm
	| m R N U d f k |
	m _ (2 to: 100) atRandom.
	R _ ℤ/m.
	N _ R nilradical asSet.
	U _ R units asSet.
	d _ (0 to: 5) atRandom.
	f _ R polynomials !! ({U atRandom}, ((1 to: d) collect: [:i| N atRandom])).
	self assert: f isUnit.
	f _ R polynomials !! ({N atRandom}, ((1 to: d) collect: [:i| N atRandom])).
	self assert: f isUnit not.
	d _ (1 to: 5) atRandom.
	k _ (1 to: d) atRandom.
	f _ R polynomials !! ({U atRandom}, ((1 to: d) collect: [:i| i=k ifTrue: [U atRandom] ifFalse: [N atRandom]])).
	self assert: f isUnit not! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testQuotientRingBug
	"This is fixed by making ideals of Z[x] have generators with positive leading coefficient. But it could be still a problem for polynomials over other rings."
	| R f Q pi a b |
	R _ ℤ polynomials.
	f _ R !! [:x| x^2*-1 - 1].
	Q _ R/f.
	pi _ Q projection.
	a _ R x - 1.
	b _ R x^2 + 1.
	self assert: (pi value: a) + (pi value: b) = (pi value: a+b)! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 7/15/2021 12:36:36'!
testShift
	| Zx x f |
	Zx _ ℤ polynomials.
	x _ Zx x.
	f _ Zx atRandomBits: 10.
	self assert: (f << 10) degree = (f degree + 10).
	self assert: ((f << 10) << -10) = f.
	self assert: f * (x^3) = (f << 3).
	self assert: (f * (x^3) << -3) = f! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 10/24/2023 16:07:20'!
testSquarefreePart
	| P x f1 f2 f3 f |
	P := ℚ polynomials.
	x := P x.
	f1 := x^2 + x + 1.
	f2 := 1 - x.
	f3 := x - 2.
	f := f1*f1*f2*f3*f3*f3.
	self assert: f radical = (f1 * f2 * f3)! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 10/24/2023 16:07:25'!
testSquarefreePartMultivariate
	| P x y f1 f2 f |
	P := ℚ polynomialsIn: #(x y).
	x := P x.
	y := P y.
	f1 := x + 1.
	f2 := y + 1.
	f := f1*f1*f2*f2.
	self assert: f radical = (f1 * f2)! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 3/6/2021 15:10:54'!
testUnivariateMultivariateConversion
	| Qx x y f |
	Qx _ ℚ polynomialsIn: #(x y).
	x _ Qx x.
	y _ Qx y.
	f _ x^2*y.
	self assert: f asUnivariate asMultivariate = f.
	self assert: (f asUnivariateIn: 1 :: asMultivariate) = f.
	self assert: (f asUnivariateIn: 2 :: asMultivariate) = f! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testUnivariateMultivariateConversion2
	| Qx x y f f2 x0 y0 |
	Qx _ ℚ polynomialsIn: #(x y).
	x _ Qx x.
	y _ Qx y.
	f _ Qx zero.
	10 timesRepeat: [f _ f + (10 atRandom - 5 * (x ^ (3 atRandom - 1) * (y ^ (3 atRandom - 1))))].
	f2 _ f asUnivariate.
	self assert: f2 asMultivariate = f.
	x0 _ (-100 to: 100) atRandom.
	y0 _ (-100 to: 100) atRandom.
	self assert: ((f2 value: y0) value: x0) = (f value: {x0. y0})! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 5/23/2020 09:34:55'!
testEvaluationHomomorphism
	| Zx f g a |
	Zx _ ℤ polynomials.
	f _ Zx atRandomBits: 1000 maxDegree: 10.
	g _ Zx atRandomBits: 1000 maxDegree: 10.
	a _ ℤ atRandomBits: 100.
	self assert: (f+g value: a) = ((f value: a) + (g value: a)).
	self assert: (f-g value: a) = ((f value: a) - (g value: a)).
	self assert: (f*g value: a) = ((f value: a) * (g value: a)).
	self assert: (f^3 value: a) = ((f value: a)^3)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 3/31/2016 06:56'!
testEvaluationHomomorphism: F
	| Fx f g a |
	Fx _ F polynomials.
	f _ Fx atRandomDegree: 10 atRandom.
	g _ Fx atRandomDegree: 10 atRandom.
	a _ F atRandom.
	self assert: (f+g value: a) = ((f value: a) + (g value: a)).
	self assert: (f-g value: a) = ((f value: a) - (g value: a)).
	self assert: (f*g value: a) = ((f value: a) * (g value: a)).
	self assert: (f^3 value: a) = ((f value: a)^3)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 6/1/2020 13:37:41'!
testEvaluationHomomorphismMultivariate
	| Zxyz f g a |
	Zxyz _ ℤ polynomialsIn: #(x y z).
	f _ Zxyz atRandomBits: 1000 maxDegree: 10.
	g _ Zxyz atRandomBits: 1000 maxDegree: 10.
	a _ ℤ^3 atRandomBits: 100.
	self assert: (f+g value: a) = ((f value: a) + (g value: a)).
	self assert: (f-g value: a) = ((f value: a) - (g value: a)).
	self assert: (f*g value: a) = ((f value: a) * (g value: a)).
	self assert: (f^3 value: a) = ((f value: a)^3)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 10/18/2016 21:01'!
testEvaluationHomomorphismZ2
	self testEvaluationHomomorphism: (PrimeField new: 2)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 5/23/2020 09:34:55'!
testEvaluationHomomorphismZm
	self testEvaluationHomomorphism: ℤ / (2 + 100 atRandom).
	self testEvaluationHomomorphism: ℤ / (2^13 - 2).
	self testEvaluationHomomorphism: ℤ / (2^31 - 2)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 12/2/2016 11:17:17'!
testEvaluationHomomorphismZp
	self testEvaluationHomomorphism: (PrimeField new: 3).
	self testEvaluationHomomorphism: (PrimeField new: 5).
	self testEvaluationHomomorphism: (PrimeField new: 2^13 - 1).
	self testEvaluationHomomorphism: (PrimeField new: 2^31 - 1)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 6/1/2020 13:37:52'!
testEvaluationMultivariate
	| Zxyz f a x y z |
	Zxyz _ ℤ polynomialsIn: #(x y z).
	x _ Zxyz x. y _ Zxyz y. z _ Zxyz z.
	f _ x^2 * y - z.
	a _ ℤ^3 atRandomBits: 100.
	self assert: (f value: a) = ((a at: 1)^2 * (a at: 2) - (a at: 3))! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/5/2021 08:41:04'!
testFiniteFieldFactorization3
	| F9 x f factors |
	F9 _ GaloisField new: (3^2).
	x _ F9 polynomials x.
	f _ (x-1)^2*(x-2).
	factors _ f factorization.
	self assert: factors size = 3.
	self assert: (factors occurrencesOf: x-1) = 2.
	self assert: (factors occurrencesOf: x-2) = 1! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/4/2022 22:05:52'!
testContentAndPrimitive
	| Zx Zxy x y f |
	Zx _ ℤ polynomialsIn: #(x).
	Zxy _ Zx polynomialsIn: #(y).
	x _ Zx x.
	y _ Zxy x.
	f _ y*(2*x) + (2*(x^2)).
	self assert: f primitivePart * f content = f! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:34:55'!
testGCDMultivariate
	| Zxy x y f g |
	Zxy _ ℤ polynomialsIn: #(x y) ordering: #lex.
	x _ Zxy x.
	y _ Zxy y.
	f _ (x+y)*(y-x).
	g _ (y-x)^2.
	self assert: (f gcd: g) = (x-y)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testGCDMultivariate2
	"Example from Sage documentation."
	| R x y f g |
	R _ ℚ polynomialsIn: #(x y).
	x _ R x. y _ R y.
	f _ x^2*y*(x+y)*3.
	g _ x*(y^2 - (x^2))*9.
	self assert: (f gcd: g) = (x^2 + (x*y))! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testGCDMultivariateOverQ
	| R x y f g |
	R _ ℚ polynomialsIn: #(x y).
	x _ R x. y _ R y.
	f _ x^2*y*6.
	g _ x*(y^2)*15 + (x^3*(y^2)*21).
	self assert: (f gcd: g) = (x*y)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:34:55'!
testGCDMultivariateOverZ
	| R x y f g |
	R _ ℤ polynomialsIn: #(x y).
	x _ R x. y _ R y.
	f _ x^2*y*6.
	g _ x*(y^2)*15 + (x^3*(y^2)*21).
	self assert: (f gcd: g) = (x*y*3)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testGCDOverRationalFunctions
	| R x y f g |
	R _ ℚ polynomialsIn: #(y) :: fractions polynomials.
	x _ R x. y _ R scalars x.
	f _ x^2*y*6.
	g _ x*(y^2)*15 + (x^3*(y^2)*21).
	self assert: (f gcd: g) = x! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateGCDBug
	| x |
	x _ ℚ polynomialsIn: 3 :: x.
	self assert: (x^2 gcd: x) = x! !

!PolynomialTest methodsFor: 'roots' stamp: 'len 7/15/2021 16:12:29'!
testCyclotomicFieldRoots
	| K f roots x |
	K _ CyclotomicField new: (3 to: 5) atRandom.
	roots _ (1 to: 3 atRandom) collect: [:i| K atRandomBits: 10].
	x _ K polynomials x.
	f _ roots product: [:α| x - α].
	self assert: f allRoots = roots asBag! !

!RandomizedTestCase methodsFor: 'debugging' stamp: 'len 5/25/2023 23:58:23'!
debugInstance
	^ super debugInstance setSeed: seed! !

!RandomizedTestCase methodsFor: 'setUp/tearDown' stamp: 'len 6/20/2023 20:50:50'!
setUp
	super setUp.
	random := Random seed: seed! !

!RandomizedTestCase methodsFor: 'setUp/tearDown' stamp: 'len 4/20/2021 06:56:32'!
tearDown
	super tearDown.
	random _ nil! !

!RandomizedTestCase methodsFor: 'private' stamp: 'len 5/20/2023 16:45:01'!
initialize
	super initialize.
	seed := 16rFFFFFFFF atRandom! !

!RandomizedTestCase methodsFor: 'private' stamp: 'len 5/25/2023 07:44:41'!
setSeed: anInteger
	seed := anInteger! !

!RandomizedTestCase class methodsFor: 'testing' stamp: 'len 5/25/2023 07:43:42'!
isAbstract
	^ thisContext methodClass == self class or: [self name beginsWith: 'Abstract']! !

!RandomizedTestCase class methodsFor: 'testing' stamp: 'len 6/5/2023 17:50:54'!
shouldInheritSelectors
	^ true! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/1/2023 16:59:31'!
newCospan
	| α β |
	α := self newMorphism.
	β := self newMorphismTo: α codomain.
	^ Cospan arrow: α coarrow: β! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/1/2023 16:57:36'!
newSpan
	| α β |
	α := self newMorphism.
	β := self newMorphismFrom: α domain.
	^ Span arrow: α coarrow: β! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/2/2023 00:20:52'!
newThreeArrow
	| α ι π |
	α := self newMorphism.
	ι := self newMorphismFrom: α domain.
	π := self newMorphismTo: α codomain.
	^ ThreeArrow aid: ι arrow: α coaid: π! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 12:20:31'!
testCospanAdditiveInverse
	| φ |
	φ := self newCospan.
	self assert: φ negated negated = φ.
	self assert: (φ + φ negated) isZero! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/1/2023 16:49:01'!
testCospanCanonicalObjects
	| γ |
	γ := self newMorphism.
	self assert: γ image = γ asCospan generalizedImage.
	self assert: γ coimage = γ asCospan generalizedCoimage.
	self assert: γ kernel = γ asCospan generalizedKernel.
	self assert: γ cokernel = γ asCospan generalizedCokernel! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/1/2023 17:17:09'!
testCospanCompositionWithIdentity
	| φ |
	φ := self newCospan.
	self assert: φ · φ domain id asCospan = φ.
	self assert: φ codomain id asCospan · φ = φ! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 12:18:56'!
testCospanIdentity
	| A φ |
	A := self newObject.
	φ := A id asCospan.
	self assert: φ isIdentity! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 15:42:28'!
testCospanPseudoInverse
	| φ |
	φ := self newCospan.
	self assert: φ · φ pseudoInverse · φ = φ.
	self assert: φ pseudoInverse · φ · φ pseudoInverse = φ pseudoInverse! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 12:20:54'!
testGeneralizedMorphismsEmbeddingFunctors
	| γ |
	γ := self newMorphism.
	self assert: γ asSpan representative = γ.
	self assert: γ asCospan representative = γ.
	self assert: γ asThreeArrow representative = γ.
	self assert: γ asSpan asCospan representative = γ.
	self assert: γ asSpan asThreeArrow representative = γ.
	self assert: γ asCospan asSpan representative = γ.
	self assert: γ asCospan asThreeArrow representative = γ.
	self assert: γ asThreeArrow asSpan representative = γ.
	self assert: γ asThreeArrow asCospan representative = γ! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 12:20:59'!
testSpanAdditiveInverse
	| φ |
	φ := self newSpan.
	self assert: φ negated negated = φ.
	self assert: (φ + φ negated) isZero! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/1/2023 16:48:27'!
testSpanCanonicalObjects
	| γ |
	γ := self newMorphism.
	self assert: γ image = γ asSpan generalizedImage.
	self assert: γ coimage = γ asSpan generalizedCoimage.
	self assert: γ kernel = γ asSpan generalizedKernel.
	self assert: γ cokernel = γ asSpan generalizedCokernel! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/1/2023 17:16:48'!
testSpanCompositionWithIdentity
	| φ |
	φ := self newSpan.
	self assert: φ · φ domain id asSpan = φ.
	self assert: φ codomain id asSpan · φ = φ! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 12:19:04'!
testSpanIdentity
	| A φ |
	A := self newObject.
	φ := A id asSpan.
	self assert: φ isIdentity! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 15:42:19'!
testSpanPseudoInverse
	| φ |
	φ := self newSpan.
	self assert: φ · φ pseudoInverse · φ = φ.
	self assert: φ pseudoInverse · φ · φ pseudoInverse = φ pseudoInverse! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 12:21:48'!
testThreeArrowAdditiveInverse
	| φ |
	φ := self newThreeArrow.
	self assert: φ negated negated = φ.
	self assert: (φ + φ negated) isZero! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 12:28:16'!
testThreeArrowCanonicalObjects
	| γ |
	γ := self newMorphism.
	self assert: γ image = γ asThreeArrow generalizedImage.
	self assert: γ coimage = γ asThreeArrow generalizedCoimage.
	self assert: γ kernel = γ asThreeArrow generalizedKernel.
	self assert: γ cokernel = γ asThreeArrow generalizedCokernel! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/1/2023 20:53:25'!
testThreeArrowCompositionWithIdentity
	| φ |
	φ := self newThreeArrow.
	self assert: φ · φ domain id asThreeArrow = φ.
	self assert: φ codomain id asThreeArrow · φ = φ! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 12:19:38'!
testThreeArrowIdentity
	| A φ |
	A := self newObject.
	φ := A id asThreeArrow.
	self assert: φ isIdentity! !

!AbstractAbelianCategoryTest methodsFor: 'generalized morphisms' stamp: 'len 7/5/2023 15:42:07'!
testThreeArrowPseudoInverse
	| φ |
	φ := self newThreeArrow.
	self assert: φ · φ pseudoInverse · φ = φ.
	self assert: φ pseudoInverse · φ · φ pseudoInverse = φ pseudoInverse! !

!AbstractAbelianCategoryTest methodsFor: 'private' stamp: 'len 7/1/2023 16:56:14'!
newMorphism
	^ self newObject ⇒ self newObject atRandom: random! !

!AbstractAbelianCategoryTest methodsFor: 'private' stamp: 'len 7/1/2023 16:56:00'!
newMorphismFrom: aDomain
	^ aDomain ⇒ self newObject atRandom: random! !

!AbstractAbelianCategoryTest methodsFor: 'private' stamp: 'len 7/1/2023 16:56:07'!
newMorphismTo: aCodomain
	^ self newObject ⇒ aCodomain atRandom: random! !

!IntegerModuleAbelianCategoryTest methodsFor: 'as yet unclassified' stamp: 'len 7/2/2023 16:51:10'!
newFreeModule
	| M |
	M := (3 atRandom: random) = 1 ifTrue: [ℤ asIdeal] ifFalse: [ℤ ^ ((0 to: 3) atRandom: random)].
	(5 atRandom: random) = 1 ifTrue: [^ M ⇒ self newSmallFreeModule].
	(5 atRandom: random) = 1 ifTrue: [^ M dual].
	^ M! !

!IntegerModuleAbelianCategoryTest methodsFor: 'as yet unclassified' stamp: 'len 7/2/2023 16:50:34'!
newObject
	| M N |
	M := self newFreeModule.
	(3 atRandom: random) = 1 ifTrue: [^ M].
	N := M sub: ((1 to: ((0 to: M rank) atRandom: random)) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	(3 atRandom: random) = 1 ifTrue: [^ N].
	^ M / N! !

!IntegerModuleAbelianCategoryTest methodsFor: 'as yet unclassified' stamp: 'len 7/2/2023 16:51:23'!
newSmallFreeModule
	| M |
	M := (3 atRandom: random) = 1 ifTrue: [ℤ asIdeal] ifFalse: [ℤ ^ ((0 to: 3) atRandom: random)].
	(3 atRandom: random) = 1 ifTrue: [^ M dual].
	^ M! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 15:24:58'!
testBilinearFormDecomposition
	| V A B |
	V := ℚ^(5 atRandom: random).
	A := V endomorphisms atRandom: random bits: 50.
	B := BilinearForm on: V matrix: A.
	self assert: B symmetricPart isSymmetric.
	self assert: B skewSymmetricPart isSkewSymmetric.
	self assert: B symmetricPart + B skewSymmetricPart = B! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/4/2023 01:17:03'!
testDotCrossAlternating
	"This is an example of an alternating bilinear form."
	| V u B |
	V := ℤ^3.
	u := V atRandom: random bits: 10.
	B := BilinearForm on: V mapWithArguments: [:x :y| u · (x × y)].
	self assert: B isAlternating! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 15:25:12'!
testDotProductIsSymmetric
	| V B |
	V := ℤ^(5 atRandom: random).
	B := BilinearForm on: V mapWithArguments: [:x :y| x · y].
	self assert: B isSymmetric! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 15:25:26'!
testMatrixAndLeft
	"The matrix of the bilinear form matches the matrix of the left part because the convention we follow when defining B(v,w) := v · (A * w)."
	| V A B |
	V := ℚ^(5 atRandom: random).
	A := V endomorphisms atRandom: random bits: 50.
	B := BilinearForm on: V matrix: A.
	self assert: A = B left asMatrix! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 08:19:22'!
testScalarSpaceMultiplication
	| V B |
	V _ ℤ asIdeal.
	B _ BilinearForm on: V mapWithArguments: [:x :y| x*y].
	self assert: B isSymmetric.
	self assert: B isAlternating not.
	V _ (ℤ/2) asIdeal.
	B _ BilinearForm on: V mapWithArguments: [:x :y| x*y].
	self assert: B isSkewSymmetric! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:11:58'!
smallGroupAtRandom
	^ ({[SymmetricGroup new: 3].
	[AlternatingGroup new: 3].
	[PermutationGroup klein].
	[LinearGroup klein]} atRandom: random) value! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 2/1/2018 23:02:45'!
testActionOrbitOf
	| G |
	G _ PermutationGroup on: (1 to: 8) generators: {#((1 3 2) (4 6 5)). #((1 2 3) (4 5 6)). #((7 8))}.
	self assert: (G action orbitOf: 1) = #(1 2 3) asSet.
	self assert: (G action orbitOf: 4) = #(4 6 5) asSet.
	self assert: (G action orbitOf: 7) = #(7 8) asSet! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:48:28'!
testActionStabilizerOf
	| G g1 g2 g3 id H |
	G := SymmetricGroup new: 8.
	g1 := G !! #((1 3 2) (4 6 5)).
	g2 := G !! #((1 2 3) (4 5 6)).
	g3 := G !! #((7 8)).
	id := G identity.
	H := G sub: {g1. g2. g3}.
	self assert: (H action stabilizerOf: 1) asSet = {id. g3} asSet.
	self assert: (H action stabilizerOf: 7) asSet = {id. g1. g2} asSet! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 4/25/2020 08:59:32'!
testAlternatingIsSymmetricCommutator
	| S A |
	S _ SymmetricGroup new: 3.
	A _ S alternatingSubgroup.
	self assert: A = S commutator.
	self assert: (S indexOf: A) = 2! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 15:40:39'!
testAlternatingOrder
	| A |
	A _ AlternatingGroup new: 3.
	self assert: A size = (3 factorial / 2)! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 15:40:46'!
testBigOrder1
	"Test fast computation of group order with Schreier-Sims algorithm.
	This is Sym(10), with order 10!!."
	| G |
	G _ PermutationGroup new: 10 generators: {#((1 2)). #(10 1 2 3 4 5 6 7 8 9)}.
	self assert: G size = 10 factorial! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 15:40:52'!
testBigOrder2
	"Test fast computation of group order with Schreier-Sims algorithm.
	This is the Mathieu group on 12 points, the second smallest of the 26 sporadic simple groups, with order 95040."
	self assert: PermutationGroup M12 size = 95040! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/14/2022 08:40:37'!
testCauchyFrobeniusLemma
	"Also called Brunside's lemma, Burnside counting theorem, or orbit-counting theorem."
	| X G φ sum |
	X _ 1 to: 3.
	G _ AlternatingGroup on: X.
	φ _ GroupAction from: (G, X) to: X mapWithArguments: [:f :x| f value: x].
	sum _ G sum: [:g| (φ fixedPointsOf: g) size].
	self assert: φ orbits size * G size = sum! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 1/28/2018 08:37:03'!
testConjugacyClasses
	"From wikipedia article on conjugacy classes."
	self assert: (SymmetricGroup new: 3) conjugacyClasses size = 3.
	self assert: (SymmetricGroup new: 4) conjugacyClasses size = 5! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 11/6/2016 10:04'!
testD3
	| D S A |
	D _ PermutationGroup dihedral: 2*3.
	self testGroup: D.
	S _ SymmetricGroup new: 3.
	self assert: D = S.
	A _ AlternatingGroup new: 3.
	self assert: D commutator = A.
	self assert: D center isTrivial! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/23/2023 16:20:41'!
testElementOrderExample1
	| K a b n |
	K := GaloisField new: 5 to: 5.
	a := K x.
	b := a^4.
	n := K units orderOf: b multiple: 5^5-1.
	self assert: n = 781! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/23/2023 16:23:07'!
testElementOrderExample2
	| K z w n |
	K := CyclotomicField new: 230.
	z := K x.
	w := z^50.
	n := K units orderOf: w multiple: 230.
	self assert: n = 23! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/23/2023 17:08:56'!
testElementOrderExample3
	| K a n |
	K := GaloisField new: 3 to: 60.
	a := K primitiveElement.
	n := K units orderOf: a multiple: 3^60 - 1.
	self assert: n = 42391158275216203514294433200! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:13:01'!
testGroup: G
	| e a b c |
	e := G identity.
	a := G atRandom: random bits: 100.
	b := G atRandom: random bits: 100.
	c := G atRandom: random bits: 100.
	self assert: e · a = a.
	self assert: a · e = a.
	self assert: a · a inverse = e.
	self assert: a inverse · a = e.
	self assert: (a · b) · c = (a · (b · c))! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:13:26'!
testGroupAction: φ
	| G X e g h x |
	G := φ group.
	X := φ space.
	e := G identity.
	g := G atRandom: random bits: 100.
	h := G atRandom: random bits: 100.
	x := X atRandom: random bits: 100.
	self assert: (φ value: {e. x}) = x.
	self assert: (φ value: {g. φ value: {h. x}}) = (φ value: {g · h. x})! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:13:41'!
testGroupAlgebra
	| G R a b c xa xb xc |
	G := self smallGroupAtRandom.
	R := G groupAlgebra.
	a := G atRandom: random.
	b := G atRandom: random.
	c := G atRandom: random.
	xa := R !! a.
	xb := R !! b.
	xc := R !! c.
	self assert: xa*xb = (R !! (a·b)).
	self assert: xa*2 + xb*xc - (xa*xc) - (xb*xc) = (R !! (a·c)).
	self assert: (xa + xb) * (xa - xb) = (xa^2 - (xb^2) + (xb*xa) - (xa*xb))! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:14:15'!
testGroupAlgebraIsARing
	| G R a a2 b b2 c c2 e e2 |
	G := self smallGroupAtRandom.
	R := G groupAlgebra.
	e := G identity.
	a := G atRandom: random.
	b := G atRandom: random.
	c := G atRandom: random.
	e2 := R !! e.
	a2 := R !! a.
	b2 := R !! b.
	c2 := R !! c.
	self assert: e2 = R one.
	self assert: e2 * a2 = a2.
	self assert: a2 * e2 = a2.
	self assert: (a2 * b2 * c2) = (a2 * (b2 * c2)).
	G isCommutative ifTrue: [self assert: a2 * b2 = (b2 * a2)].! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:14:38'!
testGroupHomomorphism: φ
	| G H a b |
	G := φ domain.
	H := φ codomain.
	a := G atRandom: random bits: 100.
	b := G atRandom: random bits: 100.
	self assert: (φ value: G identity) = H identity.
	self assert: (φ value: a · b) = ((φ value: a) · (φ value: b))! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/11/2022 10:40:06'!
testHessian
	| H |
	H _ PermutationGroup hessian.
	self assert: H size = 216.
	self testGroup: H! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2019 11:51:16'!
testIsSolvable
	| G |
	G _ SymmetricGroup new: 3.
	self assert: G isSolvable! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2019 11:52:58'!
testIsSolvableNot
	| G |
	G _ AlternatingGroup new: 5.
	self assert: G isSolvable not! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2020 06:38:34'!
testKleinFour
	| V S A |
	V _ PermutationGroup klein.
	self testGroup: V.
	S _ V ambient.
	self assert: (V isNormalIn: S).
	A _ S commutator.
	self assert: (V isNormalIn: A)! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:48:35'!
testLagrangeTheorem
	"G finite group, H subgroup of G, then |H| divides |G|."
	| G H |
	G := self smallGroupAtRandom.
	H := G sub: ((1 to: (3 atRandom: random)) collect: [:i| G atRandom: random]) asSet.
	self assert: H size | G size! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:15:25'!
testModularIntegersUnits
	| m R U |
	m := (2 to: 100) atRandom: random.
	R := ℤ / m.
	U := R units.
	self assert: U size = (Sequence phi value: m).
	self testGroup: U! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:15:32'!
testOrbitStabilizerTheorem
	| X G φ p |
	X := 1 to: 4.
	G := AlternatingGroup on: X.
	φ := GroupAction from: (G, X) to: X mapWithArguments: [:f :x| f value: x].
	p := X atRandom: random.
	self assert: (φ stabilizerOf: p) size * (φ orbitOf: p) size = G size! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 1/30/2018 21:34:39'!
testPermutationGroupAction
	self testGroupAction: (SymmetricGroup new: 10) action.
	self testGroupAction: (PermutationGroup cyclic: 15) action! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 20:31:57'!
testPermutationGroupHomomorphism1
	"Magma Handbook, Example H62E2."
	| C15 A15 f |
	C15 := PermutationGroup cyclic: 15.
	A15 := (ℤ/15) asAbelianGroup.
	f := C15 to: A15 images: (C15 generators collect: [:each| A15 generator * 11]).
	self assert: A15 = f image.
	self assert: A15 = (C15 apply: f).
	C15 do: [:c|
		C15 do: [:d|
			self assert: (f value: c) + (f value: d) = (f value: c*d)]]! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:38:30'!
testPermutationGroupHomomorphism2
	"Magma Handbook, Example H62E2."
	| C15 h im |
	C15 _ PermutationGroup cyclic: 15.
	h _ C15 to: C15 map: [:g| g ^ 3].
	im _ h image.
	self assert: im space size = 15. "a permutation group acting on a set of cardinality 15"
	self assert: im size = 5. "of order 5"
	self assert: im isCyclic! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 15:41:22'!
testSymmetricOrder
	| G |
	G _ SymmetricGroup new: 4.
	self assert: G size = 4 factorial! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 11/14/2016 08:35'!
testTrivial
	| G |
	G _ PermutationGroup new: 4 generators: #().
	self assert: G size = 1.
	self assert: G isTrivial! !

!LinearGroupsTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:24:06'!
testGeneralLinearAtRandom
	| V G |
	V := ℚ ^ 3.
	G := V automorphisms.
	self assert: (G atRandom: random bits: 100) isAutomorphism.
	self assert: (G includes: (G atRandom: random bits: 100))! !

!LinearGroupsTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:24:16'!
testOrthogonalAtRandom
	| V O |
	V := ℚ ^ 3.
	O := V innerProduct automorphisms.
	self assert: (O atRandom: random bits: 100) isOrthogonal.
	self assert: (O includes: (O atRandom: random bits: 100))! !

!LinearGroupsTest methodsFor: 'as yet unclassified' stamp: 'len 10/15/2023 13:23:18'!
testRotationsBy90Degrees
	"Group of rotations by 90 degrees in the plane, isomorphic to Z/4Z."
	| V G |
	V := ℚ ^ 2.
	G := LinearGroup on: V generators: {V endomorphisms !! #(0  1 -1 0)}.
	self assert: G size = 4.
	self assert: G isCyclic.
	self flag: #todo.
	"test invariant ring K[x,y]^G = K[x^2+y^2, x^2y^2, x^3y-xy^3], isomorphic to K[a,b,c]/<c^2-a^2b+4b^2>"
	"test hilbert series is 1-z^8 / ((1-z^2)^2(1-z^4)) = 1 + z^2 + 3z^4 + 3z^6 + ..."! !

!LinearGroupsTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2022 12:05:15'!
testSpecialLinearAtRandom
	| V G g |
	V _ ℚ ^ 3.
	G _ V automorphisms special.
	g _ G atRandomBits: 100.
	self assert: g isAutomorphism.
	g determinant = V scalars one.
	self assert: (G includes: g)! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:24:50'!
newQuadraticForm
	^ self newQuadraticForm: ((2 to: 5) atRandom: random)! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:25:01'!
newQuadraticForm: n
	| V |
	V := ℚ^n.
	^ QuadraticForm from: V coefficients: (ℚ^(n + 1 * n / 2) atRandom: random bits: 100)! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:25:19'!
testArithmetic
	| n Q1 Q2 p a |
	n := (1 to: 5) atRandom: random.
	Q1 := self newQuadraticForm: n.
	Q2 := self newQuadraticForm: n.
	p := Q1 domain atRandom: random bits: 100.
	a := ℚ atRandom: random bits: 100.
	self assert: (Q1 value: p) + (Q2 value: p) = (Q1 + Q2 value: p).
	self assert: (Q1 * a value: p) = ((Q1 value: p) * a).
	self assert: Q1 + Q2 + Q2 negated = Q1! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:25:26'!
testAsPolynomial
	| Q f p |
	Q := self newQuadraticForm.
	f := Q asPolynomial.
	p := Q domain atRandom: random bits: 100.
	self assert: (Q value: p) = (f value: p).
	self assert: (f degree = 2 and: [f isHomogeneous])! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/6/2020 11:00:14'!
testAsPolynomialAndBack
	| Q f |
	Q _ self newQuadraticForm.
	f _ Q asPolynomial.
	self assert: f asQuadraticForm = Q! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/6/2020 09:29:51'!
testAssociatedBilinearForm
	"Example from Sage."
	| Q B |
	Q _ QuadraticForm coefficients: (1,4,0,1,4,1).
	B _ Q polarization.
	self assert: (B value: {(1,2,0). (0,1,1)}) = 8.
	self assert: B depolarization = Q! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/6/2020 09:29:57'!
testAssociatedBilinearForm2
	"Example from Sage."
	| Q B |
	Q _ QuadraticForm coefficients: (1/2, 2, 1).
	B _ Q polarization.
	self assert: (B value: {(1,1). (1/2,2)}) = (19/4).
	self assert: B depolarization = Q! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 6/6/2023 00:14:24'!
testDiscriminant
	"Test the discriminant of a binary quadratic form matches the definition in Magma."
	| a b c f Q |
	[a := (-10 to: 10) atRandom: random.
	b := (-10 to: 10) atRandom: random.
	c := (-10 to: 10) atRandom: random.
	a isZero and: [b isZero and: [c isZero]]] whileTrue. "must be not all zero"
	f := (ℚ polynomialsIn: #(x y)) !! [:x :y| x^2*a + (x*y*b) + (y^2*c)].
	Q := f asQuadraticForm.
	self assert: Q discriminant = (b^2 - (4*a*c))! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testEvaluation
	"Example from Sage."
	| Q |
	Q _ QuadraticForm from: ℚ^3 coefficients: (0 to: 5).
	self assert: (Q value: (1,2,3)) = 89.
	self assert: (Q value: (1,0,0)) = 0.
	self assert: (Q value: (1,1,1)) = 15! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:25:58'!
testExternalOrthogonalSum
	| Q1 Q2 S v1 v2 embeddings w |
	Q1 := self newQuadraticForm.
	Q2 := self newQuadraticForm.
	S := Q1 ⊕ Q2.
	v1 := Q1 domain atRandom: random bits: 100.
	v2 := Q2 domain atRandom: random bits: 100.
	embeddings := S domain ⇇ (Q1 domain, Q2 domain).
	w := (embeddings first value: v1) + (embeddings second value: v2).
	self assert: (Q1 value: v1) + (Q2 value: v2) = (S value: w)! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/6/2020 09:32:19'!
testFromGramian
	| Q P |
	Q _ self newQuadraticForm.
	P _ QuadraticForm from: Q domain gramian: Q gramian.
	self assert: P = Q! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/6/2020 09:32:37'!
testFromHessian
	| Q P |
	Q _ self newQuadraticForm.
	P _ QuadraticForm from: Q domain hessian: Q hessian.
	self assert: P = Q! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 21:26:15'!
testGramian
	| Q M x |
	Q := QuadraticForm from: ℚ^3 coefficients: (0 to: 5).
	M := Q gramian.
	10 timesRepeat:
		[x := Q domain atRandom: random bits: (100 atRandom: random).
		self assert: (Q value: x) = (x · (M * x))]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:47:50'!
newElement
	^ R atRandom: random bits: 30! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:42:08'!
newScalar
	^ R scalars atRandom: random bits: 20! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:48:58'!
newUnitElement
	^ R units atRandom: random! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:48:04'!
testDivisibility
	20 timesRepeat:
		[| a b |
		a := self newElement.
		b := self newElement.
		self assert: (a isZero or: [a | (a*b)])]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:48:50'!
testDivisionByUnits
	10 timesRepeat:
		[| x u |
		x := self newElement.
		u := self newUnitElement.
		self assert: x * u / u = x.
		self assert: x / u = (x * u inverse)]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:51:18'!
testDivisionLeft
	20 timesRepeat:
		[| a b |
		a := self newElement.
		b := a * self newElement.
		self assert: (a isZero or: [a * (a \ b) = b])]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:51:27'!
testDivisionRight
	20 timesRepeat:
		[| a b |
		a := self newElement.
		b := self newElement * a.
		self assert: (a isZero or: [(b / a) * a = b])]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:49:26'!
testExactDivision
	20 timesRepeat:
		[| a b |
		a := self newElement.
		b := a * self newElement.
		self assert: (a isZero or: [a * (b ÷ a) = b])]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 13:53:19'!
testIntegersCoercion
	| a |
	self testRingHomomorphism: ℤ → R.
	a := ℤ atRandom: random bits: 20.
	self assert: (ℤ → R value: a) = (R one * a).
	self assert: R !! a = (R one * a)! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 8/12/2022 12:36:42'!
testLiftColiftOfZero
	self assert: (R zero colift: R zero) = R zero.
	self assert: (R zero lift: R zero) = R zero! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:50:36'!
testPower
	| x |
	self assert: R zero ^ 2 = R zero.
	self assert: R zero ^ 0 = R one.
	x := self newElement.
	self assert: x^0 = R one.
	self assert: x^1 = x.
	self assert: x^2 = (x*x).
	self assert: x^3 = (x*x*x).
	x := self newUnitElement.
	self assert: x^(-1) = x inverse.
	self assert: x^(-2) = x inverse squared.
	self assert: x^(-3) = (x inverse squared * x inverse)! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:28:07'!
testRingAxioms
	| one zero a b c |
	one := R one.
	zero := R zero.
	20 timesRepeat:
		[a := self newElement.
		b := self newElement.
		c := self newElement.
		self assert: a * one = a.
		self assert: one * a = a.
		self assert: zero * a = zero.
		self assert: a * zero = zero.
		self assert: zero + a = a.
		self assert: a + zero = a.
		"additive inverse"
		self assert: a - a = zero.
		self assert: a + b - b = a. "not really necessary to test this, but there was a bug before"
		"commutativity of addition:"
		self assert: a + b = (b + a).
		"associativity of addition:"
		self assert: a + b + c = (a + (b + c)).
		"associativity of multiplication:"
		self assert: a * b * c = (a * (b * c)).
		"distributivity:"
		self assert: (a + b) * c = (a * c + (b * c)).
		"commutativity of multiplication:"
		R isCommutative ifTrue: [self assert: a * b = (b * a)]]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 14:33:13'!
testRingCanonicalAssociates
	R hasCanonicalAssociates ifFalse: [^ self].
	20 timesRepeat:
		[| x u |
		x := self newElement.
		u := self newUnitElement.
		self assert: x normalization isUnit.
		self assert: u normalization = u inverse.
		self assert: u * u inverse = R one.
		self assert: u normalized = R one.
		self assert: x * x normalization = x normalized.
		self assert: x normalized = (x * u) normalized]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 19:18:25'!
testRingHomomorphism: φ
	| A B x y |
	A := φ domain.
	B := φ codomain.
	self assert: (φ value: A zero) = B zero.
	self assert: (φ value: A one) = B one.
	20 timesRepeat:
		[x := A atRandom: random bits: 10.
		y := A atRandom: random bits: 10.
		self assert: (φ value: x + y) = ((φ value: x) + (φ value: y)).
		self assert: (φ value: x * y) = ((φ value: x) * (φ value: y))]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 14:33:20'!
testRingInvolution
	| a b |
	R isInvolutive ifFalse: [^ self].
	self assert: R one conjugate = R one.
	20 timesRepeat:
		[a := self newElement.
		b := self newElement.
		self assert: (a + b) conjugate = (a conjugate + b conjugate).
		self assert: (a * b) conjugate = (b conjugate * a conjugate).
		self assert: a conjugate conjugate = a]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/9/2023 09:29:22'!
testRingPropertiesHierarchy
	R isField ifTrue: [self assert: R isEuclidean & R isIntegralDomain & R isDivisionRing].
	R isIntegralDomain ifTrue: [self assert: R isReduced].
	R isPID ifTrue: [self assert: R isIntegralDomain & R isPIR & R isUFD & R isNoetherian & R isDedekindDomain].
	R isEuclidean ifTrue: [self assert: R isPIR & R isBezout & R hasGCD & R isNoetherian].
	R isEuclideanDomain ifTrue: [self assert: R isIntegralDomain & R isEuclidean & R isBezoutDomain].
	R isDedekindDomain ifTrue: [self assert: R isIntegralDomain & R isNoetherian].
	R isFinite ifTrue: [self assert: R isArtinian].
	! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:54:12'!
testRingUnits
	10 timesRepeat:
		[| u |
		u := self newUnitElement.
		self assert: u isUnit.
		self assert: u * u inverse = R one]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:54:28'!
testUnitsAreDivisorsOfEveryElement
	10 timesRepeat:
		[| x u |
		x := self newElement.
		u := self newUnitElement.
		self assert: u | x]! !

!AbstractAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:55:56'!
testAlgebraAxioms
	| one zero x y z a |
	one := R one.
	zero := R zero.
	100 timesRepeat:
		[x := self newElement.
		y := self newElement.
		z := self newElement.
		a := self newScalar.
		self assert: x * one = x.
		self assert: one * x = x.
		self assert: zero * x = zero.
		self assert: x * zero = zero.
		self assert: zero + x = x.
		self assert: x + zero = x.
		"commutativity of addition:"
		self assert: x + y = (y + x).
		"associativity of addition:"
		self assert: x + y + z = (x + (y + z)).
		"associativity of multiplication:"
		self assert: x * y * z = (x * (y * z)).
		"distributivity:"
		self assert: (x + y) * z = (x * z + (y * z)).
		self assert: z * (x + y) = (z * x + (z * y)).
		"biliniarity of multiplication:"
		self assert: x * y * a = (x * (y * a)).
		self assert: x * y * a = (x * a * y).
		"commutativity of multiplication:"
		R isCommutative ifTrue: [self assert: x * y = (y * x)]]! !

!AbstractAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:56:33'!
testScalarEmbedding
	| a |
	self testRingHomomorphism: R scalars → R.
	a := self newScalar.
	self assert: (R scalars → R value: a) = (R one * a).
	self assert: R !! a = (R one * a)! !

!AbstractFiniteAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:57:01'!
testNormAndTrace
	| x y a |
	x := self newElement.
	y := self newElement.
	a := self newScalar.
	self assert: (x * a + y) trace = (x trace * a + y trace).
	self assert: (x * y) norm = (x norm * y norm)! !

!AbstractMaximalOrderTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 10:57:24'!
testCoercionToAndFromFractions
	| x |
	x := self newElement.
	self assert: R !! (R fractions !! x) = x! !

!AbstractMaximalOrderTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:34:34'!
testHomomorphismToFractions
	self testRingHomomorphism: R → R fractions! !

!AbstractMaximalOrderTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 00:49:44'!
testIsDedekindDomain
	self assert: R isDedekindDomain! !

!AbstractMaximalOrderTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 00:49:30'!
testIsIntegralDomain
	self assert: R isIntegralDomain! !

!CyclotomicRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:27:55'!
setUp
	super setUp.
	R := (CyclotomicField new: (3 atRandom: random) + 2) integers! !

!QuadraticRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:22:51'!
setUp
	| n |
	super setUp.
	[((n := (2 to: 100) atRandom: random) squarefreePart) > 1] whileFalse.
	random nextBoolean ifTrue: [n := n negated].
	R := (QuadraticField new: n) integers! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:28:39'!
setUp
	super setUp.
	R := CliffordAlgebra on: (QuadraticForm from: ℚ^3 coefficients: #(1 0 0 1 0 1))! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:19:57'!
testCliffordAlgebraDualGrade
	| x k |
	k _ (0 to: R degree) atRandom: random.
	x _ (R grade: k) atRandom: random bits: 10.
	self assert: ((R grade: R degree - k) includes: x dual)! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:03:35'!
testCliffordAlgebraDualProperties
	| x y |
	x := self newElement.
	y := self newElement.
	self assert: x ·| y dual = (x ∧ y) dual.
	self assert: x ∧ y dual = (x ·| y) dual! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:20:22'!
testCliffordAlgebraEvenSubalgebra
	| E x |
	E _ R evenSubalgebra.
	x _ E atRandom: random bits: 10.
	self assert: x = x reflection! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:04:15'!
testCliffordAlgebraGradeInvolution
	| x y v |
	x := self newElement.
	y := self newElement.
	v := R !! (R space atRandom: random bits: 10).
	self assert: (x*y) reflection = (x reflection * y reflection).
	self assert: v reflection = v negated! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:04:36'!
testCliffordAlgebraGradeProjection
	| x k j |
	k := (0 to: R degree) atRandom: random.
	[j := (0 to: R degree) atRandom: random. j = k] whileTrue.
	x := self newElement.
	self assert: ((R grade: k) includes: (x grade: k)).
	self assert: (((R grade: j) includes: (x grade: k)) not or: [(x grade: k) isZero]).
	x := (R grade: k) atRandom: random bits: 10.
	self assert: (x grade: k) = x.
	self assert: (x grade: j) isZero! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:23:20'!
testCliffordAlgebraHomogeneousElements
	| x |
	x _ (R grade: 0) atRandom: random bits: 10.
	self assert: x isScalar.
	x _ (R grade: 1) atRandom: random bits: 10.
	self assert: x isVector.
	x _ (R grade: 2) atRandom: random bits: 10.
	self assert: x isBivector! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:23:43'!
testCliffordAlgebraInverse
	| a |
	[a _ R atRandom: random bits: 10.
	a isUnit] whileFalse.
	self assert: a * a inverse = R one.
	self assert: a inverse * a = R one! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:24:20'!
testCliffordAlgebraMultiplication
	| Q V C x y z |
	V _ ℚ^3.
	Q _ QuadraticForm from: V coefficients: (1 to: 6).
	C _ CliffordAlgebra on: Q.
	x _ C !! (V x: 1).
	y _ C !! (V x: 2).
	z _ C !! (V x: 3).
	self assert: y*x = ((x*y) negated + 2).
	self assert: z*x = ((x*z) negated + 3).
	self assert: z*z = (C !! 6).
	self assert: (x*0) isZero.
	self assert: (0*x) isZero! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:05:06'!
testCliffordAlgebraProductsProperties
	| x y z one |
	x := self newElement.
	y := self newElement.
	z := self newElement.
	"Associativity of wedge product:"
	self assert: x ∧ (y ∧ z) = (x ∧  y ∧ z).
	"Associativity of left and right interior products:"
	self assert: x ·| (y |· z) = (x ·| y |· z).
	self assert: x ·| (y ·| z) = (x ∧ y ·| z).
	self assert: x · (y ·| z) = (x ∧ y · z).
	one := R one.
	self assert: one ∧ x = x.
	self assert: x ∧ one = x.
	self assert: one ·| x = x.
	self assert: x |· one = x! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:05:19'!
testCliffordAlgebraProductsPropertiesWithVectors
	| x y v |
	x := self newElement.
	y := self newElement.
	v := R !! (R space atRandom: random bits: 10).
	self assert: v * x = (v ·| x + (v ∧ x)).
	self assert: v ·| x = (v*x - (x reflection * v) / 2).
	self assert: v ·| x = (x reflection negated |· v).
	self assert: v ∧ x = (v*x + (x reflection * v) / 2).
	self assert: v ∧ x = (x reflection ∧ v).
	self assert: v ·| (x*y) = ((v ·| x)*y + (x reflection * (v ·| y)))! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 1/3/2022 11:47:12'!
testCliffordAlgebraProperties
	self assert: R isCommutative not.
	self assert: R rank = (2^R space rank).
	self assert: R scalars = ℚ.
	self assert: R zero isZero.
	self assert: R one isOne.
	self assert: R id isIdentity! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:05:33'!
testCliffordAlgebraReversion
	| x y v |
	x := self newElement.
	y := self newElement.
	v := R !! (R space atRandom: random bits: 10).
	self assert: (x*y) transpose = (y transpose * x transpose).
	self assert: v transpose = v! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:05:57'!
testCliffordAlgebraScalarEmbedding
	| a x |
	a := self newScalar.
	x := R !! a.
	self assert: x isScalar.
	self assert: ((R grade: 0) includes: x).
	self assert: R one * a = x.
	self assert: x asScalar = a! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/18/2022 20:13:19'!
testCliffordAlgebraScalarEmbedding2
	| ι |
	ι _ R scalars asIdeal ⇒ R asVectorSpace !! (R scalars → R).
	self assert: ι isInjective.
	self assert: ι image = (R grade: 0) asVectorSpace! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:16:41'!
testCliffordAlgebraSpaceEmbedding
	| V v x |
	V := R space.
	v := V atRandom: random bits: 10.
	x := R !! v.
	self assert: x isVector.
	self assert: ((R grade: 1) includes: x).
	self assert: x asVector = v! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/18/2022 20:15:41'!
testCliffordAlgebraSpaceEmbedding2
	| ι |
	ι _ R space ⇒ R asVectorSpace !! (R space carrier → R carrier).
	self assert: ι isInjective.
	self assert: ι image = (R grade: 1) asVectorSpace! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:16:48'!
testCliffordAlgebraUniversalProperty
	| Q v |
	Q := R quadraticForm.
	v := R space atRandom: random bits: 10.
	self assert: (R !! v) squared = (R one * (Q value: v))! !

!MatrixAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:29:27'!
setUp
	| F |
	super setUp.
	F := random nextBoolean ifTrue: [ℚ] ifFalse: [ℤ/(64 atRandom: random) nextPrime].
	R := (F^((0 to: 3) atRandom: random)) endomorphisms! !

!MatrixAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:37:45'!
testMatrixAlgebraRank
	self assert: R rank = R space rank squared! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:29:41'!
setUp
	super setUp.
	R := QuaternionAlgebra over: ℚ invariants: #(1 1)! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/7/2022 07:27:25'!
testQuaternionAlgebraAnnihilatorIdeal
	| H a b I |
	H _  QuaternionAlgebra over: ℚ invariants: #(1 1).
	a _ 1 + H i.
	self assert: a isZeroDivisor.
	I _ a annihilatorIdeal.
	self assert: (I includes: 1 - H i).
	self assert: (I includes: H j - H k).
	self assert: (I generators allSatisfy: [:each| (a * each) isZero]).
	b _ H j.
	self assert: b isZeroDivisor not.
	I _ b annihilatorIdeal.
	self assert: I isTrivial! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:31:55'!
testQuaternionAlgebraCoercionToAndFromModule
	| A x |
	A _ QuaternionAlgebra over: ℚ invariants: #(-17 -271).
	x _ A !! #(1 -2 3 0).
	self assert: A asModule = (ℚ^4).
	self assert: A asModule !! x = (ℚ^4 !! #(1 -2 3 0)).
	self assert: A !! (A asModule !! #(1 -2 3 0)) = x! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:16:55'!
testQuaternionAlgebraInverse
	| A a |
	A := QuaternionAlgebra over: ℚ invariants: #(-5 -2).
	[a := A atRandom: random bits: 20.
	a isUnit] whileFalse.
	self assert: a * a inverse = A one.
	self assert: a inverse * a = A one! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:48:42'!
testQuaternionAlgebraIsCentral
	| A |
	A := QuaternionAlgebra over: ℚ invariants: #(-5 -2).
	self assert: A isCentral.
	self assert: A center = (A sub: {A one})! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:32:53'!
testQuaternionAlgebraNormAndTraceExample
	"Example from Magma handbook, H92E9."
	| A x |
	A _ QuaternionAlgebra over: ℚ invariants: #(-17 -271).
	x _ A !! #(1 -2 3 0).
	self assert: x trace = 2.
	self assert: x norm = 2508. 
	self assert: x representation trace = 4.
	self assert: (x^2 - (x * x trace) + x norm) isZero! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:33:07'!
testQuaternionAlgebraTraceExample
	"Example from Sage."
	| A |
	A _ QuaternionAlgebra over: ℚ invariants: #(-5 -2).
	self assert: A i trace isZero.
	self assert: A j trace isZero.
	self assert: (A !! {1/3. 1/5. 1/7. 1}) trace = (2/3)! !

!RationalAffineAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:30:32'!
setUp
	| P |
	super setUp.
	P := ℚ polynomialsIn: (3 atRandom: random).
	[R := P / ((1 to: (4 atRandom: random) - 1) collect: [:i| P atRandom: random bits: 20]).
	R isTrivial] whileTrue! !

!AbstractCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/4/2022 18:12:21'!
testIsCommutative
	self assert: R isCommutative! !

!AbstractCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:25:25'!
testMatrixAlgebra
	| n M |
	self flag: #fixme. "test 3x3 or larger, now over some rings this is slow"
	n := #(1 2) atRandom: random.
	M := (R^n) endomorphisms.
	self testRingHomomorphism: R → M! !

!AbstractCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:25:30'!
testPolynomialAlgebra
	| P |
	P := R polynomialsIn: (3 atRandom: random).
	self testRingHomomorphism: R → P! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:25:58'!
testFractions
	self testRingHomomorphism: R → R fractions! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:07:59'!
testGCD
	R size < 60 ifTrue:
		[R do: [:a| R do: [:b| self testGCD: a with: b]]. ^ self].
	100 timesRepeat:
		[ | a b |
		[(a := self newElement) isZero] whileTrue.
		[(b := self newElement) isZero] whileTrue.
		self testGCD: a with: b]! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:08:28'!
testGCD2
	100 timesRepeat:
		[ | a b c gcd |
		[(a := self newElement) isZero] whileTrue.
		[(b := self newElement) isZero] whileTrue.
		[(c := self newElement) isZero] whileTrue.
		gcd := a*c gcd: b*c.
		self assert: c | gcd]! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/16/2022 12:06:07'!
testGCD: a with: b
	| gcd |
	gcd _ a gcd: b.
	self assert: gcd | a.
	self assert: gcd | b.
	self assert: gcd = gcd normalized! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:08:39'!
testGCDZero
	| a gcd |
	a := self newElement.
	gcd := a gcd: R zero.
	self assert: gcd = (R zero gcd: a).
	self assert: gcd = a normalized.
	self assert: (R zero gcd: R zero) = R zero! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/16/2022 12:07:21'!
testHasGCD
	self assert: R hasGCD! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:08:54'!
testLCM
	R size < 60 ifTrue:
		[R do: [:a| R do: [:b| self testLCM: a with: b]]. ^ self].
	100 timesRepeat:
		[| a b |
		[(a := self newElement) isZero] whileTrue.
		[(b := self newElement) isZero] whileTrue.
		self testLCM: a with: b]! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/16/2022 12:06:43'!
testLCM: a with: b
	| lcm |
	lcm _ a lcm: b.
	self assert: a | lcm.
	self assert: b | lcm.
	self assert: lcm = lcm normalized! !

!AbstractGCDRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:09:05'!
testLCMZero
	| a lcm |
	a := self newElement.
	lcm := a lcm: R zero.
	self assert: lcm = (R zero lcm: a).
	self assert: lcm = R zero.
	self assert: (R zero lcm: R zero) = R zero! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:09:16'!
testAnnihilator
	100 timesRepeat:
		[| x a |
		x := self newElement.
		a := x annihilator.
		self assert: (x * a) isZero.
		self assert: x isZeroDivisor not = a isZero]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2022 09:02:25'!
testAnnihilatorOfZero
	self assert: R zero annihilator isOne! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:09:29'!
testEuclideanDivision
	"Test division on a pair of elements:"
	R isTrivial ifTrue: [^ self].
	R size < 300 ifTrue:
		[R do: [:a| R do: [:b| self testEuclideanDivisionOf: a by: b]]. ^ self].
	100 timesRepeat:
		[| a b |
		a := self newElement.
		[b := self newElement. b isZero] whileTrue.
		self testEuclideanDivisionOf: a by: b]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/16/2022 10:44:43'!
testEuclideanDivisionOf: a by: b
	| q r |
	b isZero ifTrue:
		[self should: [a // b] raise: Error.
		self should: [a \\ b] raise: Error.
		^ self].
	q _ a // b.
	r _ a \\ b.
	self assert: a = (q * b + r).
	self assert: (r isZero or: [r gauge < b gauge])! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:09:42'!
testExactDivisionCompatibleWithEuclideanDivision
	R isTrivial ifTrue: [^ self].
	100 timesRepeat:
		[| x y xy q |
		x := self newElement.
		[y := self newElement. y isZero] whileTrue.
		xy := x * y.
		q := xy // y.
		self assert: xy / y = q.
		self assert: xy ÷ y = q.
		self assert: (xy \\ y) isZero]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 17:55:26'!
testHasCanonicalAssociates
	self assert: R hasCanonicalAssociates! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 00:47:18'!
testIsEuclidean
	self assert: R isEuclidean! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 00:46:56'!
testIsPIR
	self assert: R isPIR! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/18/2022 06:13:00'!
testNormalization
	| a u |
	100 timesRepeat:
		[a _ R atRandom: random.
		a _ a normalized.
		self assert: a normalized = a.
		u _ R units atRandom: random.
		self assert: (a*u) normalized = a]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:38:11'!
testXXGCD
	R size < 60 ifTrue:
		[R do: [:a| R do: [:b| self testXXGCD: a with: b]]. ^ self].
	20 timesRepeat:
		[| a b |
		a := self newElement.
		b := self newElement.
		self testXXGCD: a with: b]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 4/20/2021 06:48:37'!
testXXGCD: a with: b
	| result g s t u v |
	result _ a xxgcd: b.
	g _ result at: 1.
	s _ result at: 2.
	t _ result at: 3.
	u _ result at: 4.
	v _ result at: 5.
	self assert: g = (a gcd: b).
	self assert: s*a + (t*b) = g.
	self assert: (u*a + (v*b)) isZero.
	self assert: (s*v - (t*u)) isUnit! !

!AbstractEuclideanDomainTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 01:00:17'!
testIsIntegralDomain
	self assert: R isIntegralDomain! !

!AbstractEuclideanDomainTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 01:07:46'!
testIsPID
	self assert: R isPID! !

!AbstractEuclideanDomainTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 01:09:17'!
testIsUFD
	self assert: R isUFD! !

!AbstractDVRTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:25:31'!
testIsDVR
	self assert: R isDVR! !

!AbstractDVRTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:27:13'!
testIsDedekindDomain
	self assert: R isDedekindDomain! !

!AbstractDVRTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:28:41'!
testIsLocal
	self assert: R isLocal.
	self assert: R maximalIdeal = (R * R uniformizer)! !

!AbstractDVRTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 00:48:04'!
testIsNotField
	self assert: R isField not! !

!AbstractDVRTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:27:34'!
testIsUFD
	self assert: R isUFD! !

!AbstractDVRTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 21:52:15'!
testProjectionToResidueField
	self testRingHomomorphism: R → R residueField! !

!AbstractDVRTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 11:10:19'!
testValuation
	| p a b |
	self assert: R one valuation = 0.
	p := R uniformizer.
	self assert: p valuation = 1.
	a := self newElement.
	self assert: (a*p) valuation = (a valuation + 1).
	b := self newElement.
	self assert: (a*b) valuation = (a valuation + b valuation)! !

!LocalIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:36:48'!
setUp
	super setUp.
	R := ℤ @ (ℤ * (64 atRandom: random) nextPrime)! !

!FiniteFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:35:45'!
setUp
	super setUp.
	R := (	GaloisField new: (5 atRandom: random) nextPrime to: (3 atRandom: random)) polynomials! !

!FiniteFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/6/2023 10:20:07'!
testFactorization
	| factors f factorization |
	factors := (1 to: (4 atRandom: random)) collect: [:i| (R atRandom: random maxDegree: 4) ^ (3 atRandom: random)].
	f := factors product normalized.
	factorization := f factorization.
	self assert: factorization product = f.
	factorization withOccurrencesDo: [:g :k|
		self assert: g isMonic.
		self assert: g isIrreducible.
		self assert: (factors anySatisfy: [:h| g | h])]! !

!FiniteFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/9/2023 23:15:48'!
testModularComposition
	| d f g m |
	d := 10 atRandom: random.
	f := R atRandom: random maxDegree: d.
	g := R atRandom: random maxDegree: d.
	m := R atRandom: random degree: d + 1.
	self assert: (f value: g) \\ m = (f composeWith: g modulo: m)! !

!BinaryExtensionFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2023 08:18:56'!
setUp
	super setUp.
	R := (	GaloisField new: 2 to: (8 atRandom: random) + 1) polynomials! !

!BinaryExtensionFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/6/2023 10:11:01'!
testFactorizationExample
	| x f factors |
	x := R x.
	f := x^4 - 1.
	factors := f factorization.
	self assert: factors size = 4.
	self assert: (factors occurrencesOf: x+1) = 4! !

!BinaryExtensionFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/6/2023 10:39:53'!
testSquarefreeFactorizationBug
	| f g h |
	100 timesRepeat:
		[[f := R atRandom: random maxDegree: 1. f isConstant] whileTrue.
		[g := R atRandom: random maxDegree: 1. g isConstant] whileTrue.
		h := (f^2*g) normalized.
		self assert: h squarefreeFactorization product = h]! !

!PrimeFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:28:23'!
setUp
	super setUp.
	R := (PrimeField new: (1000 atRandom: random) nextPrime) polynomials! !

!PrimeFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:28:44'!
testIsIrreducibleExample
	"The polynomial x^4 + 1 is irreducible in Z[x] and Q[x], and it is reducible Z/p[x] for all prime numbers p."
	self deny: (R !! [:x| x^4 + 1]) isIrreducible! !

!BinaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:33:34'!
setUp
	super setUp.
	R := (ℤ/2) polynomials! !

!BinaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:43:05'!
testFactorizationExample
	| x f factors |
	x := R x.
	f := x^24 - 1.
	factors := f factorization.
	self assert: factors size = 16.
	self assert: (factors occurrencesOf: x+1) = 8.
	self assert: (factors occurrencesOf: x^2+x+1) = 8! !

!BinaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:43:51'!
testFactorizationExample2
	| x |
	x := R x.
	self assert: (x^15 - 1) factorization = {x + 1. x^2 + x + 1. x^4 + (x^3) + (x^2) + x + 1. x^4 + x + 1. x^4 + (x^3) + 1} asBag! !

!BinaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:44:15'!
testGCDExample
	| x f g h |
	x := R x.
	h := x^3 - x + 1.
	f := h * (x + 1) * x.
	g := h * (x + 1) * (x^2 + 1).
	self assert: (f gcd: g) = (h * (x + 1))! !

!TernaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:38:29'!
setUp
	super setUp.
	R := (ℤ/3) polynomials! !

!TernaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:51:58'!
testFactorizationExample
	| x f factors |
	x := R x.
	f := (x-1)^2*(x-2).
	factors := f factorization.
	self assert: factors size = 3.
	self assert: (factors occurrencesOf: x-1) = 2.
	self assert: (factors occurrencesOf: x-2) = 1! !

!TernaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:42:19'!
testFactorizationExample2
	| x f factors |
	x := R x.
	f := x^11 + (x^9 * 2) + (x^8 * 2) + (x^6) + (x^5) + (x^3 * 2) + (x^2 * 2) + 1.
	factors := f factorization.
	self assert: factors size = 8.
	self assert: (factors occurrencesOf: x + 2) = 4.
	self assert: (factors occurrencesOf: x^2 + 1) = 3.
	self assert: (factors occurrencesOf: x + 1) = 1.
	self assert: (factors inject: R one into: [:a :b| a*b]) = f! !

!TernaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:40:17'!
testIrreduciblePolynomialsCount
	"ℤ/3ℤ[x] has 8 irreducible monic polynomials of degree 3, out of 27 monic polynomials of degree 3."
	| f count |
	count := 0.
	R scalars^3 do: [:each|
		f := R !! (each coefficients copyWith: R scalars one).
		f isIrreducible
			ifTrue:
				[count := count + 1.
				self assert: f factorization size = 1]
			ifFalse:
				[self assert: f factorization size > 1]].
	self assert: count = 8! !

!TernaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:39:02'!
testIsIrreducibleExample
	| x f |
	x := R x.
	f := x^3 + (x*2) + 1.
	self assert: f factorization size = 1.
	self assert: f isIrreducible! !

!TernaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:52:07'!
testRootsExample
	| x f roots |
	x := R x.
	f := (x-1)^2*(x-2).
	roots := f allRoots.
	self assert: roots size = 3.
	self assert: (roots occurrencesOf: (R scalars !! 1)) = 2.
	self assert: (roots occurrencesOf: (R scalars !! 2)) = 1! !

!TernaryFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:37:34'!
testSquarefreeFactorizationExample
	| x f factors |
	x := R x.
	f := x^11 + (x^9 * 2) + (x^8 * 2) + (x^6) + (x^5) + (x^3 * 2) + (x^2 * 2) + 1.
	factors := f squarefreeFactorization.
	self assert: factors size = 8.
	self assert: (factors occurrencesOf: x + 2) = 4.
	self assert: (factors occurrencesOf: x^2 + 1) = 3.
	self assert: (factors occurrencesOf: x + 1) = 1! !

!TernaryExtensionFieldPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2023 08:18:10'!
setUp
	super setUp.
	R := (	GaloisField new: 3 to: (8 atRandom: random) + 1) polynomials! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:37:57'!
setUp
	super setUp.
	R := ℚ polynomials! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:16:34'!
testCompanionMatrixExample
	| x p |
	x := R x.
	p := x^2 + (x*2) - 1.
	self assert: p companion characteristicPolynomial = p.
	self assert: p companion minimalPolynomial = p! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:18:03'!
testExtendedEuclidBug
	| x f1 f2 |
	x := R x.
	f1 := x^2 + 1.
	f2 := x - 1.
	self assert: (f1 xgcd: f2) first = (f1 gcd: f2)! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:18:41'!
testGCDExample
	| x f g |
	x := R x.
	f := x^4 - 1.
	g := x^6 - 1.
	self assert: (f gcd: g) = (x^2 - 1)! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:26:02'!
testIsIrreducibleExample
	| x f |
	x := R x.
	f := x^4 + 1.
	self assert: f isIrreducible.
	self assert: f negated isIrreducible.
	self assert: (f*2) isIrreducible.
	self assert: (f*(x+1)) isIrreducible not! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:15:21'!
testRationalFactorizationExample
	| x f factors |
	x := ℚ polynomials x.
	f := (x-(1/2))^2*(x-2)*(x^2 + 1)*(x^2-2).
	factors := f factorization.
	self assert: factors size = 5.
	self assert: factors asSet size = 4.
	self assert: factors product normalized = f normalized "up to a unit"! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:14:39'!
testRationalRootsBug
	| x f roots |
	x := R x.
	f := x^3*-1 + (3*x) + 2.
	roots := f allRoots.
	self assert: roots size = 3.
	self assert: (roots occurrencesOf: -1) = 2.
	self assert: (roots occurrencesOf: 2) = 1! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:15:03'!
testRationalRootsExample
	| x f roots |
	x := R x.
	f := (x-(1/2))^2*(x-2)*(x^2 + 1)*(x^2-2).
	roots := f allRoots.
	self assert: roots size = 3.
	self assert: (roots occurrencesOf: 1/2) = 2.
	self assert: (roots occurrencesOf: 2) = 1! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:14:17'!
testRemainderInconsistencyBug
	"In Sage these two give the same result (1), same in Magma."
	| X x |
	X := ℤ polynomials x.
	x := R x.
	self assert: (x \\ (1 - x)) leadingCoefficient = (X \\ (1 - X)) leadingCoefficient! !

!RationalPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:12:35'!
testZeroCoefficientBug
	self assert: (R one * 0) isZero! !

!AbstractFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2022 12:23:12'!
testIsDivisionRing
	self assert: R isDivisionRing! !

!AbstractFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2022 11:51:45'!
testIsField
	self assert: R isField! !

!AbstractFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2022 11:51:32'!
testIsIntegralDomain
	self assert: R isIntegralDomain! !

!AbstractFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2022 11:52:48'!
testZeroIdealIsPrimeAndMinimalPrime
	| P |
	P _ R * 0.
	self assert: P isPrime.
	self assert: P isMinimalPrime! !

!AbstractFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/31/2023 10:02:20'!
testEmbeddingIntoOverfield
	"Embedding from GF(p^n) to GF(p^(n*k))."
	| p n k S |
	p := R characteristic.
	n := R degree.
	k := 3 atRandom: random.
	S := GaloisField new: p to: n * k.
	self testRingHomomorphism:  R → S! !

!AbstractFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/5/2023 17:41:39'!
testIsFinite
	self assert: R isFinite! !

!AbstractFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 23:52:32'!
testPrimitiveRoot
	| ξ i x |
	ξ := R primitiveRoot.
	self assert: ξ multiplicativeOrder = (R size - 1).
	i := 1.
	x := ξ.
	[x = R one] whileFalse: [x := x*ξ. i := i+1].
	self assert: i = (R size - 1)! !

!AbstractFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/5/2023 18:43:07'!
testUnitsSize
	self assert: R units size = (R size - 1)! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2023 07:53:57'!
setUp
	super setUp.
	R := GaloisField new: (5 atRandom: random) nextPrime to: (3 atRandom: random) + 1! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2023 07:55:41'!
testFrobenius
	self testRingHomomorphism: R frobenius! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/26/2023 10:32:14'!
testFrobeniusFixesScalars
	| a |
	a := R !! (R scalars atRandom: random).
	self assert: a frobenius = a! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/26/2023 10:33:14'!
testFrobeniusOrder
	self assert: (R frobenius ^ R degree) isIdentity! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/26/2023 14:27:10'!
testGeneralizedFrobeniusIsFrobenius
	| x |
	100 timesRepeat:
		[x := R atRandom: random.
		self assert: x generalizedFrobenius = x frobenius]! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/26/2023 14:26:35'!
testGeneralizedTraceIsTrace
	| x |
	100 timesRepeat:
		[x := R atRandom: random.
		self assert: x generalizedTrace = x trace]! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:41:27'!
testNormAndTrace
	| x y a |
	x := self newElement.
	y := self newElement.
	a := self newScalar.
	self assert: (x * a + y) trace = (x trace * a + y trace).
	self assert: (x * y) norm = (x norm * y norm)! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:46:58'!
testNormTraceAndConjugates
	| x |
	x := self newElement.
	self assert: R !! x trace = x conjugates sum.
	self assert: R !! x norm = x conjugates product! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2023 07:55:24'!
testNormTraceAndFrobenius
	| x |
	x := R atRandom: random.
	self assert: x frobenius trace = x trace.
	self assert: x frobenius norm = x norm! !

!GaloisFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/6/2023 11:18:22'!
testPrimeSubfield
	self assert: R primeSubfield = R scalars "because they are always absolute extensions of the prime field"! !

!BinaryExtensionFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2023 07:54:08'!
setUp
	super setUp.
	R := GaloisField new: 2 to: (8 atRandom: random) + 1! !

!BinaryExtensionFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/21/2023 18:08:56'!
testZeroPowerBug
	self assert: ((GaloisField new: 2 to: 2) zero^3) isZero! !

!TernaryExtensionFieldTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2023 08:18:22'!
setUp
	super setUp.
	R := GaloisField new: 3 to: (8 atRandom: random) + 1! !

!PrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 21:26:18'!
setUp
	super setUp.
	R := ℤ/(256 atRandom: random) nextPrime! !

!PrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:08:20'!
testEulerTheorem
	| a |
	[a := R atRandom: random. a isZero] whileTrue.
	self assert: a ^ (R modulus - 1) = R one! !

!PrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:08:26'!
testLogarithm
	| g a b p |
	p := R modulus.
	g := R primitiveRoot.
	[(a := R atRandom: random) isZero] whileTrue.
	[(b := R atRandom: random) isZero] whileTrue.
	self assert: g ^ (a log: g) = a.
	self assert: (a log: g) + (b log: g) \\ (p-1) = ((a*b log: g) \\ (p-1)).
	self assert: (a log: g) - (b log: g) \\ (p-1) = ((a/b log: g) \\ (p-1))! !

!PrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:08:32'!
testPrimeFieldBasicProperties
	self assert: R characteristic = R modulus.
	self testRingHomomorphism: R projection! !

!PrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/6/2023 11:18:42'!
testPrimeSubfield
	self assert: R primeSubfield = R! !

!PrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 21:30:53'!
testSquareRoot
	| a x |
	100 timesRepeat:
		[a := (R atRandom: random) squared.
		x := a squareRoot.
		self assert: x squared = a]! !

!BinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:31:04'!
setUp
	super setUp.
	R := ℤ/2! !

!BinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 17:41:46'!
testSquareRoot
	self assert: R zero squareRoot = R zero.
	self assert: R one squareRoot = R one! !

!TernaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:33:06'!
setUp
	super setUp.
	R := ℤ/3! !

!AbstractNumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:41:38'!
testNormAndTrace
	| x y a |
	x := self newElement.
	y := self newElement.
	a := self newScalar.
	self assert: (x * a + y) trace = (x trace * a + y trace).
	self assert: (x * y) norm = (x norm * y norm)! !

!AbstractNumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/6/2023 11:18:56'!
testPrimeSubfield
	self assert: R primeSubfield = ℚ! !

!CyclotomicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:31:19'!
setUp
	super setUp.
	R := CyclotomicField new: (3 atRandom: random) + 2! !

!CyclotomicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/17/2023 10:28:12'!
testComplexConjugation
	| a |
	self assert: R zero conjugate = R zero.
	self assert: R one conjugate = R one.
	10 timesRepeat:
		[a := self newElement.
		self assert: a conjugate conjugate = a.
		self assert: (ℂ !! a conjugate) = (ℂ !! a) conjugate]! !

!CyclotomicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/17/2023 16:53:07'!
testComplexEmbedding
	| a b |
	20 timesRepeat:
		[a := self newElement.
		b := self newElement.
		self assert: (ℂ !! a) * (ℂ !! b) = (ℂ !! (a*b)).
		self assert: (ℂ !! a) + (ℂ !! b) = (ℂ !! (a+b))]! !

!CyclotomicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 19:03:34'!
testComplexEmbeddingCompatibilityWithMaximalRealSubfield
	| S a |
	S := R maximalRealSubfield.
	100 timesRepeat:
		[a := S atRandom: random.
		self assert: (ℂ !! a) = (ℂ !! (R !! a))]! !

!CyclotomicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 21:39:09'!
testCyclotomicFieldDiscriminantExamples
	self assert: (CyclotomicField new: 20) discriminant = 4000000.
	self assert: (CyclotomicField new: 18) discriminant = -19683! !

!GaussianFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/5/2023 21:30:46'!
setUp
	super setUp.
	R := CyclotomicField new: 4! !

!GaussianFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/17/2023 10:32:51'!
testComplexConjugation
	| a |
	self assert: R zero conjugate = R zero.
	self assert: R one conjugate = R one.
	10 timesRepeat:
		[a := self newElement.
		self assert: a conjugate conjugate = a.
		self assert: (ℂ !! a conjugate) = (ℂ !! a) conjugate]! !

!GaussianFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 19:04:16'!
testComplexEmbedding
	| a b |
	100 timesRepeat:
		[a := self newElement.
		b := self newElement.
		self assert: (ℂ !! a) * (ℂ !! b) = (ℂ !! (a*b)).
		self assert: (ℂ !! a) + (ℂ !! b) = (ℂ !! (a+b))]! !

!QuadraticFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:22:58'!
setUp
	| n |
	super setUp.
	[(n := ((2 to: 100) atRandom: random) squarefreePart) > 1] whileFalse.
	random nextBoolean ifTrue: [n := n negated].
	R := QuadraticField new: n! !

!QuadraticFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 19:04:19'!
testComplexEmbedding
	| a b |
	100 timesRepeat:
		[a := self newElement.
		b := self newElement.
		self assert: (ℂ !! a) * (ℂ !! b) = (ℂ !! (a*b)).
		self assert: (ℂ !! a) + (ℂ !! b) = (ℂ !! (a+b))]! !

!QuadraticFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:23:10'!
testQuadraticFieldFundamentalDiscriminantExamples
	self assert: (QuadraticField new: 102) discriminant = 408.
	self assert: (QuadraticField new: 720) discriminant = 5.
	self assert: (QuadraticField new: 2) discriminant = 8! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:33:40'!
newElement
	^ R atRandom: random bits: 5! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/11/2023 13:44:27'!
setUp
	super setUp.
	R := ℂ! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:44:40'!
testArithmeticExamples
	| sqrt2 sqrt3 sqrt5 sqrt6 |
	sqrt2 := (ℂ !! 2) sqrt.
	sqrt3 := (ℂ !! 3) sqrt.
	sqrt5 := (ℂ !! 5) sqrt.
	sqrt6 := (ℂ !! 6) sqrt.
	self assert: sqrt2 squared asRational = 2.
	self assert: (sqrt2 + sqrt3) * sqrt2 = (sqrt6 + 2).
	self assert: (sqrt3 / sqrt5) squared inverse asRational = (5/3).
"	self assert: (((5 root: 3) + sqrt2 - sqrt2) ^ 3) asRational = 5"! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/17/2023 10:34:22'!
testConjugation
	| α |
	α := self newElement.
	self assert: α conjugate conjugate = α.
	self assert: α conjugate = (α real asComplex - (α imaginary asComplex * ℂ i))! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:44:26'!
testDivisionExample
	| sqrt3 sqrt5 |
	sqrt3 := (ℂ !! 3) sqrt.
	sqrt5 := (ℂ !! 5) sqrt.
	self assert: (sqrt3 / sqrt5 / sqrt3 / sqrt5) asRational = (1/5)! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:44:07'!
testMinimalPolynomialExample
	"Example from 'Algebraic Number Theory: A Computational Approach' by William Stein (29 pp)."
	| x α |
	α := 1 + (ℂ !! 5) sqrt / 2.
	x := ℚ polynomials x.
	self assert: α minimalPolynomial = (x^2 - x - 1)! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:40:53'!
testNormAndTrace
	| x y a |
	x := self newElement.
	y := self newElement.
	a := self newScalar.
	self assert: (x * a + y) trace = (x trace * a + y trace).
	self assert: (x * y) norm = (x norm * y norm)! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:46:53'!
testNormTraceAndConjugates
	| x |
	x := self newElement.
	self assert: R !! x trace = x conjugates sum.
	self assert: R !! x norm = x conjugates product! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:42:58'!
testProductExample
	| sqrt3 sqrt5 |
	sqrt3 := (ℂ !! 3) sqrt.
	sqrt5 := (ℂ !! 5) sqrt.
	self assert: (sqrt3 * sqrt5 * sqrt3 * sqrt5) asRational = 15! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:45:13'!
testSquareExample
	| sqrt2 |
	sqrt2 := (ℂ !! 2) sqrt.
	self assert: sqrt2 squared asRational = 2! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:46:07'!
testSquareRootExample
	| α |
	α := (ℂ !! 2) sqrt.
	self assert: α sqrt squared = α! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:38:46'!
testSquaredBug
	self assert: (ℂ !! 2) squared = (ℂ !! 4)! !

!ComplexAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:46:18'!
testSumExample
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 := (ℂ !! 2) sqrt.
	sqrt3 := (ℂ !! 3) sqrt.
	sqrt5 := (ℂ !! 5) sqrt.
	self assert: sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5 = sqrt2! !

!FiniteFieldRationalFunctionFieldTest methodsFor: 'as yet unclassified' stamp: 'len 6/17/2023 17:13:07'!
setUp
	super setUp.
	R := ((	GaloisField new: (5 atRandom: random) nextPrime to: (3 atRandom: random)) polynomialsIn: (2 atRandom: random)) fractions! !

!RationalFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:32:28'!
setUp
	super setUp.
	R := ℚ! !

!RationalRationalFunctionFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:32:45'!
setUp
	super setUp.
	R := (ℚ polynomialsIn: (3 atRandom: random)) fractions! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:33:45'!
newElement
	^ R atRandom: random bits: 5! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/11/2023 13:44:15'!
setUp
	super setUp.
	R := ℝ! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:48:00'!
testArithmeticExamples
	| sqrt2 sqrt3 sqrt5 sqrt6 |
	sqrt2 := (ℝ !! 2) sqrt.
	sqrt3 := (ℝ !! 3) sqrt.
	sqrt5 := (ℝ !! 5) sqrt.
	sqrt6 := (ℝ !! 6) sqrt.
	self assert: sqrt2 squared asRational = 2.
	self assert: (sqrt2 + sqrt3) * sqrt2 = (2 + sqrt6).
	self assert: (sqrt3 / sqrt5) squared inverse asRational = (5/3).
"	self assert: (((five root: 3) + sqrt2 - sqrt2) ^ 3) asRational = 5"! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:48:14'!
testDivisionExample
	| sqrt3 sqrt5 |
	sqrt3 := (ℝ !! 3) sqrt.
	sqrt5 := (ℝ !! 5) sqrt.
	self assert: (sqrt3 / sqrt5 / sqrt3 / sqrt5) asRational = (1/5)! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:42:37'!
testNormAndTrace
	| x y a |
	x := self newElement.
	y := self newElement.
	a := self newScalar.
	self assert: (x * a + y) trace = (x trace * a + y trace).
	self assert: (x * y) norm = (x norm * y norm)! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:47:07'!
testNormTraceAndConjugates
	| x |
	x := self newElement.
	self assert: R !! x trace = x conjugates sum.
	self assert: R !! x norm = x conjugates product! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:48:25'!
testProductExample
	| sqrt3 sqrt5 |
	sqrt3 := (ℝ !! 3) sqrt.
	sqrt5 := (ℝ !! 5) sqrt.
	self assert: (sqrt3 * sqrt5 * sqrt3 * sqrt5) asRational = 15! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:48:36'!
testSquareExample
	| sqrt2 |
	sqrt2 := (ℝ !! 2) sqrt.
	self assert: sqrt2 squared asRational = 2! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:48:52'!
testSquareRootExample
	| a |
	a := (ℝ !! 2) sqrt.
	self assert: a sqrt squared = a! !

!RealAlgebraicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 11/12/2023 16:49:03'!
testSumExample
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 := (ℝ !! 2) sqrt.
	sqrt3 := (ℝ !! 3) sqrt.
	sqrt5 := (ℝ !! 5) sqrt.
	self assert: sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5 = sqrt2! !

!EisensteinRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:33:44'!
setUp
	super setUp.
	R := (CyclotomicField new: 3) integers! !

!EisensteinRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:35:20'!
testIsPrimary
	| a |
	200 timesRepeat:
		[a := R atRandom: random.
		a isPrimary ifTrue: [self assert: a norm \\ 3 = 1]]! !

!GaussianRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/5/2023 21:31:00'!
setUp
	super setUp.
	R := (CyclotomicField new: 4) integers! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:36:33'!
setUp
	super setUp.
	R := ℤ! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:51:03'!
testBitCount
	| v |
	v := 1 << 3000 :: atRandom: random.
	self assert: (v printStringBase: 2 :: count: [:x| x = $1]) = v bitCount! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:50:52'!
testBitParity
	| v |
	v := 1 << 3000 :: atRandom: random.
	self assert: ((v printStringBase: 2 :: count: [:x| x = $1]) bitAnd: 1) = v bitParity! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:52:09'!
testIntegerDivisorsExample
	self assert: 12 numberOfDivisors = 6.
	self assert: 12 divisors size = 6! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:52:26'!
testIntegerIdealsPrimePrimarySemiprime
	self assert: (ℤ*5) isPrime.
	self assert: (ℤ*6) isPrime not.
	self assert: (ℤ*25) isPrimary.
	self assert: (ℤ*30) isPrimary not.
	self assert: (ℤ*30) isSemiprime.
	self assert: (ℤ*12) isSemiprime not! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:53:29'!
testIntegerSplit
	"Split algorithm from 'Fast Algorithms for Linear Algebra Modulo N', Arne Storjohann, Thom Mulders."
	| D a M |
	10000 timesRepeat:
		[D := 100 atRandom.
		a := D atRandom - 1.
		M := a split: D.
		self assert: M > 0.
		self assert: M | D.
		M factorization asSet do: [:p| self deny: p | a].
		(D / M) factorization asSet do: [:p| self assert: p | a]]! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:51:50'!
testIsFundamentalDiscriminant
	self assert: ((1 to: 33) select: [:each| each isFundamentalDiscriminant]) =  #(1 5 8 12 13 17 21 24 28 29 33).
	self assert: ((0 to: -31 by: -1) select: [:each| each isFundamentalDiscriminant]) = #(-3 -4 -7 -8 -11 -15 -19 -20 -23 -24 -31) 
! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:43:55'!
testIsPrimePower
	self assert: 0 isPrimePower not.
	self assert: 1 isPrimePower.
	2 to: 10000 do: [:each|
		self assert: each isPrimePower = (each factorization asSet size = 1)]! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:54:25'!
testJacobiAndLegendreSymbols
	| a p |
	10 timesRepeat:
		[p := ((1000 atRandom: random) + 2) nextPrime. "an odd prime"
		a := (1000 atRandom: random) - 1.
		self assert: (a legendre: p) = (a jacobi: p)]! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:54:49'!
testKroneckerSymbol
	| a b c |
	a := (-100 to: 100) atRandom: random.
	b := (-100 to: 100) atRandom: random.
	self assert: ((a kronecker: b) = 0) = ((a gcd: b) ~= 1).
	c := (-100 to: 100) atRandom: random.
	self assert: (a*b kronecker: c) = ((a kronecker: c) * (b kronecker: c)).
	b*c = 0 ifFalse: [self assert: (a kronecker: b*c) = ((a kronecker: b) * (a kronecker: c))]! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:54:58'!
testLegendreSymbol
	self assert: ((1 to: 10) collect: [:k| k legendre: 43]) = #(1 -1 -1 1 -1 1 -1 -1 1 1)! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 21:33:24'!
testNilradical
	self assert: ℤ nilradical = (ℤ*0)! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:55:59'!
testPartitions
	self assert: 42 partitions size =  53174! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 17:43:05'!
testPrimeAndMaximalIdeal
	| I |
	I := ℤ * 64 atRandom nextPrime.
	self assert: I isPrime.
	self assert: I isMaximal.
	I := ℤ * 6.
	self assert: I isPrime not.
	self assert: I isMaximal not.
	I := ℤ * 1.
	self assert: I isPrime not.
	self assert: I isMaximal not! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 17:42:52'!
testPrimeNotMaximalIdeal
	| I |
	I := ℤ * 0.
	self assert: I isPrime.
	self assert: I isMaximal not! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:46:40'!
testPrimePowerFactorization
	2 to: 10000 do: [:each|
		each isPrimePower
			ifTrue: [self assert: each primePowerFactorizationOrNil = each factorization]
			ifFalse: [self assert: each primePowerFactorizationOrNil isNil]]! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/24/2023 18:56:13'!
testPrimeSignature
	self assert: 1 primeSignature = #().
	self assert: 7 primeSignature = #(1).
	self assert: (7*5) primeSignature = #(1 1).
	self assert: 12 primeSignature = #(2 1).
	self assert: 100 primeSignature = #(2 2).
	self assert: 42 primeSignature = #(1 1 1).
	self assert: 16 primeSignature = #(4)! !

!IntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/24/2023 16:07:34'!
testRadicalAndIsSquarefree
	| n |
	n := (-1000 to: 1000) atRandom: random.
	self assert: n isSquarefree = (n radical = n)! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/23/2023 14:52:24'!
testAdicExpansion
	| a p |
	p := R base.
	a := R atRandom: random.
	self assert: a = (a expansion reversed inject: R zero into: [:total :each| total * p + each])! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:08:43'!
testCharacteristicAndSize
	| p n r |
	p := R base.
	n := R exponent.
	r := R degree.
	self assert: R characteristic = (p^n).
	self assert: R size = (p^n^r)! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 11:30:11'!
testDivisionBug
	| A a |
	A := GaloisRing new: 3 to: 2 to: 2.
	a := A x * 3.
	self assert: (a / a) isOne! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/20/2023 10:47:49'!
testEmbeddingIntoOverring
	"Canonical homomorphism from GR(p^n,m) to GR(p^n,m*k)."
	| p n m k S |
	p := R base.
	n := R exponent.
	m := R degree.
	k := 3 atRandom: random.
	S := GaloisRing new: p to: n to: m * k.
	self testRingHomomorphism:  R → S! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:08:55'!
testIsLocal
	self assert: R isLocal.
	self assert: R maximalIdeal = (R * R base)! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:09:09'!
testNilradical
	| a |
	self assert: R nilradical = (R * R base).
	self assert: (R nilradical atRandom: random) isNilpotent.
	[a := R atRandom: random. a isNilpotent] whileFalse.
	self assert: (R nilradical includes: a)! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:09:17'!
testProjectionFromHigherExponent
	| S |
	S := GaloisRing new: R base to: R exponent + (5 atRandom: random) to: R degree.
	self testRingHomomorphism: S → R! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 20:51:43'!
testProjectionToResidueField
	"Canonical homomorphism from GR(p^n,m) to GF(p,m).
	This sends 'a' in ℤ/pⁿℤ[x]/f to 'a mod p' in ℤ/pℤ[x]/(f mod p) where f mod p means reducing the coefficients mod p."
	self testRingHomomorphism: R → R residueField! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:09:25'!
testUnitsSize
	| p n r |
	p := R base.
	n := R exponent.
	r := R degree.
	self assert: p ^ r - 1 * (p ^ (n - 1 * r)) = R units size! !

!AbstractGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 9/8/2023 17:09:32'!
testValuation
	| p a b |
	self assert: R one valuation = 0.
	self assert: R zero valuation isPositiveInfinity.
	p := R !! R base.
	p isZero ifFalse: [self assert: p valuation = 1].
	a := R atRandom: random bits: 100.
	b := R atRandom: random bits: 100.
	(a*b) isZero ifFalse: [self assert: (a*b) valuation = (a valuation + b valuation)].! !

!GaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/24/2023 10:49:50'!
setUp
	super setUp.
	R := GaloisRing new: (3 atRandom: random) nextPrime to: (2 atRandom: random) to: (2 atRandom: random) + 1! !

!GaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/24/2023 10:50:34'!
testGeneralizedFrobenius
	self testRingHomomorphism: R generalizedFrobenius! !

!GaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/24/2023 10:50:40'!
testGeneralizedFrobeniusFixesScalars
	| a |
	a := R !! (R scalars atRandom: random).
	self assert: a generalizedFrobenius = a! !

!GaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/24/2023 10:50:57'!
testGeneralizedFrobeniusOrder
	self assert: (R generalizedFrobenius ^ R degree) isIdentity! !

!GaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 10/24/2023 10:59:11'!
testGeneralizedTrace
	| x y a |
	x := R atRandom: random.
	y := R atRandom: random.
	a := R scalars atRandom: random.
	self assert: (x * a + y) generalizedTrace = (x generalizedTrace * a + y generalizedTrace)! !

!PrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:54:00'!
setUp
	| k p |
	super setUp.
	p := (10 atRandom: random) nextPrime.
	k := 10 atRandom: random.
	R := PrimePowerRing new: p to: k! !

!PrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:53:48'!
testEuclideanDivisionCompatibleWithModularIntegerRing
	| S r1 r2 s1 s2 |
	"Hacky way to instantiate a ModularIntegerRing, otherwise we might get the cached original one"
	S := ModularIntegerRing new modulus: R modulus.
	100 timesRepeat:
		[r1 := R atRandom: random.
		[r2 := R atRandom: random. r2 isZero] whileTrue.
		s1 := S !! r1 representative.
		s2 := S !! r2 representative.
		self assert: (r1 // r2) representative = (s1 // s2) representative]! !

!PrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:53:48'!
testEuclideanFunctionCompatibleWithModularIntegerRing
	| S r s |
	"Hacky way to instantiate a ModularIntegerRing, otherwise we might get the cached original one"
	S := ModularIntegerRing new modulus: R modulus.
	100 timesRepeat:
		[r := R atRandom: random.
		s := S !! r representative.
		self assert: r gauge = s gauge]! !

!PrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:53:48'!
testGCDCompatibleWithModularIntegerRing
	| S r1 r2 s1 s2 |
	"Hacky way to instantiate a ModularIntegerRing, otherwise we might get the cached original one"
	S := ModularIntegerRing new modulus: R modulus.
	100 timesRepeat:
		[r1 := R atRandom: random.
		r2 := R atRandom: random.
		s1 := S !! r1 representative.
		s2 := S !! r2 representative.
		self assert: (r1 gcd: r2) representative = (s1 gcd: s2) representative]! !

!PrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:53:48'!
testLCMCompatibleWithModularIntegerRing
	| S r1 r2 s1 s2 |
	"Hacky way to instantiate a ModularIntegerRing, otherwise we might get the cached original one"
	S := ModularIntegerRing new modulus: R modulus.
	100 timesRepeat:
		[r1 := R atRandom: random.
		r2 := R atRandom: random.
		s1 := S !! r1 representative.
		s2 := S !! r2 representative.
		self assert: (r1 lcm: r2) representative = (s1 lcm: s2) representative]! !

!PrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:53:48'!
testNormalizationCompatibleWithModularIntegerRing
	| S r s |
	"Hacky way to instantiate a ModularIntegerRing, otherwise we might get the cached original one"
	S := ModularIntegerRing new modulus: R modulus.
	100 timesRepeat:
		[r := R atRandom: random.
		s := S !! r representative.
		self assert: r normalization representative = s normalization representative]! !

!PrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 21:30:32'!
testSquareRoot
	| a x |
	100 timesRepeat:
		[a := (R atRandom: random) squared.
		x := a squareRoot.
		self assert: x squared = a]! !

!BinaryPowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:54:00'!
setUp
	super setUp.
	R := PrimePowerRing new: 2 to: ((2 to: 64) atRandom: random)! !

!FiniteFieldDualNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 7/7/2023 17:38:40'!
setUp
	| K P |
	super setUp.
	K := GaloisField new: ((#(3 16rFF 16rFFFF) atRandom: random) atRandom: random) nextPrime to: (5 atRandom: random).
	P := K polynomials.
	R := P / {P x ^ 2}! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:37:16'!
setUp
	super setUp.
	R := ℤ/((2 to: ({2^8. 2^16.  2^32. 2^40} atRandom: random)) atRandom: random)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 12:04:12'!
testCongruenceSolvingExample1
	| A a b solutions |
	A _ ℤ/20.
	a _ A !! 11.
	b _ A !! 15.
	solutions _ a solve: b.
	self assert: solutions first = (A !! 5).
	self assert: solutions size = 1! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 12:03:44'!
testCongruenceSolvingExample2
	| A a b solutions |
	A _ ℤ/12167.
	a _ A !! 230.
	b _ A !! 1081.
	solutions _ a solve: b.
	self assert: (solutions includes: (A !! 2491)).
	self assert: solutions size = 23.
	self assert: (solutions allSatisfy: [:x| a*x = b])! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:27:44'!
testIsFieldIffIsIntegralDomain
	self assert: R isField = R isIntegralDomain! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:27:07'!
testIsLocalIffModulusIsPrimePower
	self assert: R modulus isPrimePower = R isLocal! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:26:46'!
testIsPrimeIffModulusIsPrime
	self assert: R modulus isPrime = R isField! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:30:51'!
testModularIntegersEuclideanExample
	"Some random example built in Magma online calculator."
	| A a b c |
	A := ℤ/12.
	a := A !! 2.
	b := A !! 9.
	c := a lcm: b.
	self assert: c = (A !! 6).
	self assert: c // a = (A !! 3).
	self assert: c // b = (A !! 10)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:31:08'!
testModularIntegersEuclideanExample2
	"Some random example built in Magma online calculator."
	| A a b |
	A := ℤ/39.
	a := A !! 36.
	b := A !! 3.
	self assert: a // b = (A !! 12)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:31:27'!
testModularIntegersEuclideanExample3
	"Some random example built in Magma online calculator."
	| A a b |
	A := ℤ/78.
	a := A !! 9.
	b := A !! 39.
	self assert: a // b = (A !! 0)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:31:42'!
testModularIntegersEuclideanExample4
	"Some random example built in Magma online calculator."
	| A a b |
	A := ℤ/230.
	a := A !! 205.
	b := A !! 128.
	self assert: (a gcd: b) = (A !! 1).
	self assert: (a lcm: b) = (A !! 10).! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:32:05'!
testModularIntegersEuclideanExample5
	"Some random example built in Magma online calculator."
	| A a b |
	A := ℤ/10.
	a := A !! 4.
	b := A !! 8.
	self assert: b normalized = (A !! 2).
	self assert: b normalization inverse = (A !! 9).
	self assert: (a gcd: b) = (A !! 2).
	self assert: (a lcm: b) = (A !! 2)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:32:22'!
testModularIntegersEuclideanExample6
	"Some random example built in Magma online calculator."
	| A a b |
	A := ℤ/12.
	a := A !! 4.
	b := A !! 8.
	self assert: a // b = (A !! 11).
	self assert: b // a = (A !! 2)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:32:35'!
testModularIntegersEuclideanExample7
	"Some random example built in Magma online calculator."
	| A a b |
	A := ℤ/6.
	a := A !! 4.
	b := A !! 3.
	self assert: (a gcd: b) = (A !! 1).
	self assert: (a lcm: b) = (A !! 0)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:32:49'!
testModularIntegersEuclideanExample8
	"Some random example built in Magma online calculator."
	| A a b |
	A := ℤ/10.
	a := A !! 6.
	b := A !! 4.
	self assert: a // b = (A !! 4).
	self assert: b // a = (A !! 4)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:33:04'!
testModularIntegersEuclideanExample9
	"Some random example built in Magma online calculator."
	| A a b |
	A := ℤ/10.
	a := A !! 2.
	b := A !! 4.
	self assert: a // b = (A !! 8).
	self assert: b // a = (A !! 2)! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:24:35'!
testNilradical
	self assert: R nilradical = (R * (R modulus factorization asSet product))! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:25:28'!
testPrimesExamples
	| A |
	A := ℤ/6.
	self assert: (A primes asSet collect: [:each| each representative]) = #(2 3 4) asSet.
	A := ℤ/12.
	self assert: (A primes asSet collect: [:each| each representative]) = #(2 3 9 10) asSet! !

!RationalDualNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 7/7/2023 14:53:30'!
setUp
	| P |
	super setUp.
	P := ℚ polynomials.
	R := P / {P x ^ 2}! !

!FiniteFieldMultivariatePolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:39:01'!
setUp
	super setUp.
	R := (	GaloisField new: (5 atRandom: random) nextPrime to: (3 atRandom: random)) polynomialsIn: (3 atRandom: random)! !

!IntegerMultivariatePolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/7/2023 16:45:45'!
setUp
	super setUp.
	R := ℤ polynomialsIn: (3 atRandom: random)! !

!IntegerMultivariatePolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/10/2023 18:08:09'!
testExtractCoercionBug
	| S x₁ f g |
	S := ℤ polynomialsIn: 2.
	x₁ := S x: 1.
	f := S → (S extract: #(1)) value: x₁.
	g := (S extract: #(1)) !! x₁.
	self assert: f = g! !

!IntegerMultivariatePolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/7/2023 16:46:04'!
testPolynomialFlatten
	| S f |
	S := R polynomialsIn: (3 atRandom: random).
	50 timesRepeat:
		[f := S atRandom: random.
		self assert: (S !! (S flatten !! f)) = f.
		f := R atRandom: random.
		self assert: (S !! (S flatten !! f)) = (S constant: f)]! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 17:37:53'!
setUp
	super setUp.
	R := ℤ polynomials! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:34:03'!
testContentAndPrimitive
	| f |
	f := R atRandom: random bits: 100 maxDegree: (1 to: 10) atRandom.
	self assert: f primitivePart * f content = f! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:34:36'!
testDiscriminantExample
	| x f g |
	x := R x.
	f := x^2 + x + 1.
	g := x^3 + 1.
	self assert: (f*g) discriminant = (f discriminant * g discriminant * (f resultant: g) squared)! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:34:56'!
testDiscriminantExample2
	| x f |
	x := R x.
	f := x^2 - x - 36042.
	self assert: f discriminant = 144169 "this is a prime, although looks like a square :)"! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:36:02'!
testEPRSExample
	"Example from Zippel."
	| a b eprs |
	a := R !! #(1 0 1 0 -3 -3 8 2 -5) reversed.
	b := R !! #(3 0 5 0 -4 -9 21) reversed.
	eprs := a eprs: b :: collect: [:each| each coefficients reversed asArray] :: asArray.
	self assert: eprs = #((1 0 1 0 -3 -3 8 2 -5)
		(3 0 5 0 -4 -9 21)
		(-15 0 3 0 -9) 
		(15795 30375 -59535) 
		(1254542875143750 -1654608338437500) (12593338795500743100931141992187500))! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:45:08'!
testFactorizationExample
	| x f factors |
	x := R x.
	f := (x*2-1)^2*(x-2)*(x^2 + 1)*(x^2-2)*18.
	factors := f factorization.
	self assert: factors size = 8.
	self assert: factors asSet size = 6.
	self assert: factors product = f! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:44:44'!
testGCDExample
	| x f g |
	x := R x.
	f := (x^4 - 1)*6.
	g := (x^6 - 1)*9.
	self assert: (f gcd: g) = ((x^2 - 1)*3)! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:26:25'!
testIsIrreducibleExample
	| x f |
	x := R x.
	f := x^4 + 1.
	self assert: f isIrreducible.
	self assert: f negated isIrreducible.
	self assert: (f*2) isIrreducible not.
	self assert: (f*(x+1)) isIrreducible not.
! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/17/2023 11:18:18'!
testOddEvenDecomposition
	| x f |
	x := R x.
	f := R atRandom: random maxDegree: (#(2 10) atRandom: random).
	self assert: (f oddPart value: x^2) * x + (f evenPart value: x^2) = f! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:46:03'!
testPPRSExample
	"Example from Zippel."
	| a b pprs |
	a := R !! #(1 0 1 0 -3 -3 8 2 -5) reversed.
	b := R !! #(3 0 5 0 -4 -9 21) reversed.
	pprs := a pprs: b :: collect: [:each| each coefficients reversed asArray] :: asArray.
	self assert: pprs = #((1 0 1 0 -3 -3 8 2 -5) 
		(3 0 5 0 -4 -9 21)
		(-5 0 1 0 -3)
		(13 25 -49)
		(4663 -6150)
		(1))! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/7/2023 16:44:51'!
testPolynomialFlatten
	| S f |
	S := R polynomialsIn: (3 atRandom: random).
	50 timesRepeat:
		[f := S atRandom: random.
		self assert: (S !! (S flatten !! f)) = f.
		f := R atRandom: random.
		self assert: (S !! (S flatten !! f)) = (S constant: f)]! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 17:40:06'!
testPrimeNotMaximalIdeal
	"Z[x]/<x> is isomorphic to Z, which is an integral domain but not a field. So <x> is prime but not maximal."
	| I |
	I := R * R x.
	self assert: I isPrime.
	self assert: I isMaximal not! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:48:21'!
testResultant
	| x f g h a b |
	x := R x.
	[(f := R atRandom: random bits: 10 maxDegree: 4) isZero] whileTrue.
	[(g := R atRandom: random bits: 10 maxDegree: 4) isZero] whileTrue.
	h := R atRandom: random bits: 10 maxDegree: 4.
	a := ℤ atRandom: random bits: 5.
	b := ℤ atRandom: random bits: 5.
	self assert: (f * h resultant: g * h) isZero.
	self assert: (-1)^(f degree * g degree) * (f resultant: g) = (g resultant: f).
	self assert: (f * g resultant: h) = ((f resultant: h) * (g resultant: h)).
	self assert: (R !! a resultant: f) = (a ^ f degree).
	self assert: (f resultant: R !! a) = (a ^ f degree).
	self assert: (x + a resultant: x + b) = (b - a)! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:48:39'!
testResultantExample
	| x f g f2 g2 h |
	x := R x.
	f := x^2 + x + 1.
	f2 := f * (x + 2).
	g := x^3 + 1.
	g2 := g * (x + 2).
	h := x^4 + 1.
	self assert: (f resultant: g) isZero not. "not 0 because f and g have no common roots"
	self assert: (f2 resultant: g2) isZero. "res(f2,g2) = 0 because f2 and g2 have a common root -2."
	self assert: (-1)^(f degree * g degree) * (f resultant: g) = (g resultant: f).
	self assert: (f * g resultant: h) = ((f resultant: h) * (g resultant: h))! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:48:54'!
testResultantExample2
	| x f g |
	x := R x.
	f := 2 * (x - 5) * (x - 11).
	g := 3 * (x - 7).
	self assert: (f resultant: g) = ((2 ^ 1) * (3^2) * (5 - 7) * (11 - 7))! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:47:16'!
testSPRSExample
	"Example from Zippel."
	| a b sprs |
	a := R !! #(1 0 1 0 -3 -3 8 2 -5) reversed.
	b := R !! #(3 0 5 0 -4 -9 21) reversed.
	sprs := a sprs: b :: collect: [:each| each coefficients reversed asArray] :: asArray.
	self assert: sprs = #((1 0 1 0 -3 -3 8 2 -5)
		#(3 0 5 0 -4 -9 21)
		#(15 0 -3 0 9)
		#(65 125 -245)
		#(9326 -12300)
		#(260708))
! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:46:31'!
testSubresultantExample
	"deg(gcd(f,g)) = d iff subresultant(f,g,k) = 0 for all k < d and subresultant(f,g,d) ~= 0, and the d-th subresultant coefficient is in fact a gcd of f and g."
	| x f g |
	x := R x.
	f := (x^4 - 1)*6.
	g := (x^6 - 1)*9.
	"self assert: (f gcd: g) = ((x^2 - 1)*3)"
	self assert: (f subresultant: 0 with: g) isZero.
	self assert: (f subresultant: 1 with: g) isZero.
	self assert: (f subresultant: 2 with: g) isZero not.
	self assert: (f subresultant: 2 with: g) primitivePart = (x^2 - 1)! !

!IntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:46:52'!
testSubresultantExample2
	| x f g |
	x := R x.
	f := 2 * (x - 5) * (x - 11).
	g := 3 * (x - 7).
	self assert: (f resultant: g) = (f subresultant: 0 with: g) asScalar! !

!ProductRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:19:27'!
setUp
	| A B C |
	super setUp.
	A := {ℤ. ℚ. ℤ/(64 atRandom: random)} atRandom: random.
	B := {ℤ. ℚ. ℤ/(64 atRandom: random)} atRandom: random.
	C := {ℤ. ℚ. ℤ/(64 atRandom: random)} atRandom: random.
	R := A × B × C! !

!ProductRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/1/2023 15:17:28'!
testProjections
	1 to: R arity do: [:i| self testRingHomomorphism: (R projection: i)]! !

!RationalMultivariatePolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:39:23'!
setUp
	super setUp.
	R := ℚ polynomialsIn: (3 atRandom: random)! !

!RationalMultivariatePolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 17:39:37'!
testPrimeNotMaximalIdeal
	"Q[x,y]/<x> is isomorphic to Q[y], which is an integral domain but not a field. So <x> is prime but not maximal."
	| P I |
	P := ℚ polynomialsIn: 2.
	I := P * P x.
	self assert: I isPrime.
	self assert: I isMaximal not! !

!FiniteRingExample1Test methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 16:40:49'!
setUp
	super setUp.
	R := (ℤ/2) polynomials.
	R := R / (R x^3 - R x)! !

!FiniteRingExample1Test methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 16:42:36'!
testThisRingProperties
	self assert: R isField not.
	self assert: R size = 8.
	self assert: R units size = 2.
	self assert: R zeroDivisors size = 6.
	self assert: R radical asSet = {R zero. R x^2 + R x} asSet
	"also, this ring has two maximal ideals"! !

!FiniteRingExample2Test methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 16:43:39'!
setUp
	super setUp.
	"this ring is isomorphic to 	ℤ/2 × (ℤ/2)"
	R := (ℤ/2) polynomials.
	R := R / (R x * (R x + 1))! !

!FiniteRingExample2Test methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 16:42:51'!
testThisRingProperties
	self assert: R isField not.
	self assert: R size = 4.
	self assert: R zeroDivisors size = 3.
	self assert: R characteristic = 2.
	self assert: R isLocal not.
	"more about this ring: two maximal (and princial as well) ideals (so not a local ring)"! !

!FiniteRingExample3Test methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 16:44:42'!
setUp
	super setUp.
	"this ring is isomorphic to ℤ/2ℤ[x]/⟨x² + x⟩"
	R := ℤ/2 × (ℤ/2)! !

!FiniteRingExample3Test methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 16:45:06'!
testThisRingProperties
	self assert: R isField not.
	self assert: R size = 4.
	self assert: R zeroDivisors size = 3.
	self assert: R characteristic = 2.
	self assert: R isLocal not.
	"more about this ring: two maximal (and principal as well) ideals (so not a local ring)"! !

!FiniteRingExample4Test methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 16:45:48'!
setUp
	super setUp.
	R := ℤ/4 × (ℤ/4)! !

!FiniteRingExample4Test methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 16:46:00'!
testThisRingProperties
	self assert: R characteristic = 4.
	self assert: R isField not.
	self assert: R size = 16.
	self assert: R zeroDivisors size = 12.
	self assert: R units size = 4.
	self assert: R radical isTrivial not.
	"more about this ring: two maximal ideals (so not a local ring), and non-trivial jacobson radical"! !

!GaloisRingMultivariatePolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:40:02'!
setUp
	super setUp.
	R := (	GaloisRing new: (5 atRandom: random) nextPrime to: (3 atRandom: random) to: (2 atRandom: random)) polynomialsIn: (3 atRandom: random)! !

!GaloisRingPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 6/5/2023 17:33:08'!
setUp
	super setUp.
	R := (	GaloisRing new: (5 atRandom: random) nextPrime to: (3 atRandom: random) to: (2 atRandom: random)) polynomials! !

!PrimePowerRingPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:54:00'!
setUp
	| k p |
	super setUp.
	p := (10 atRandom: random) nextPrime.
	k := 10 atRandom: random.
	R := (PrimePowerRing new: p to: k) polynomials! !

!BinaryPowerRingPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:54:00'!
setUp
	super setUp.
	R := (PrimePowerRing new: 2 to: ((2 to: 64) atRandom: random)) polynomials! !

!ModularIntegerMultivariatePolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:40:33'!
setUp
	super setUp.
	R := ℤ/((2 to: 256) atRandom: random) polynomialsIn: (3 atRandom: random)! !

!ModularIntegerPolynomialRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:41:07'!
setUp
	super setUp.
	R := (ℤ/((2 to: ({2^8. 2^16.  2^32. 2^40} atRandom: random)) atRandom: random)) polynomials! !

!QuasiGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/30/2023 23:31:25'!
setUp
	super setUp.
	p := #(2 3) atRandom: random.
	r := 3 atRandom: random.
	n := (2 atRandom: random) + 1.
	R := QuasiGaloisRing new: p to: r to: n! !

!QuasiGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 7/12/2023 19:54:08'!
testNilradical
	| a |
	self assert: R nilradical = (R * R x).
	self assert: (R nilradical atRandom: random) isNilpotent.
	[a := R atRandom: random. a isNilpotent] whileFalse.
	self assert: (R nilradical includes: a)! !

!QuasiGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/30/2023 23:27:55'!
testQuasiGaloisRingBasicProperties
	self assert: R characteristic = p.
	self assert: R size = (p^(r*n)).
	self assert: R isLocal.
	self assert: R isFinite.
	self assert: R isField not! !

!AbstractArrayTest methodsFor: 'private' stamp: 'len 5/6/2023 17:09:00'!
newArray
	^ self newArray: (200 atRandom: random)! !

!AbstractArrayTest methodsFor: 'private' stamp: 'len 5/11/2023 16:17:24'!
newArray: n
	^ self newArray: n fill: [:each| self newScalar]! !

!AbstractArrayTest methodsFor: 'private' stamp: 'len 5/11/2023 16:14:04'!
newArray: n fill: aBlock
	^ scalars new: n fill: aBlock! !

!AbstractArrayTest methodsFor: 'private' stamp: 'len 5/6/2023 17:06:46'!
newScalar
	^ scalars atRandom: random bits: (60 atRandom: random)! !

!AbstractArrayTest methodsFor: 'misc' stamp: 'len 5/6/2023 17:10:48'!
testAccessing
	| a i x |
	a _ self newArray.
	x _ self newScalar.
	i _ (1 to: a size) atRandom: random.
	a at: i put: x.
	self assert: (a at: i) = x! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:13:06'!
testAddition
	| n a b c |
	n _ 5 atRandom: random.
	a _ self newArray: n.
	b _ self newArray: n.
	c _ a + b.
	self assert: c size = n.
	1 to: n do: [:i| self assert: (a at: i) + (b at: i) = (c at: i)].
	self assert: (a + a negated) isZero.
	self assert: a + b = (b + a).
	self assert: a + a = (a*2).
	self assert: a - b = (a + b negated)! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/11/2023 23:11:33'!
testArithmeticWithArraysOfDifferentSize
	| a b c d zeros |
	a _ self newArray.
	b _ self newArray: a size.
	c _ self newArray.
	d _ self newArray: c size.
	zeros _ self newArray: (a size atRandom: random).
	zeros atAllPut: scalars zero.
	self assert: a + (b,zeros) = (a,zeros + b).
	self assert: (a,zeros) + b = (a + b , zeros).
	self assert: a - (b,zeros) = (a,zeros - b).
	self assert: (a,zeros) - b = (a - b , zeros).
	self assert: a * (b,c) = (a * b).
	self assert: (a,c) * b = (a * b).
	self assert: (a,c) * (b,d) = (a * b , (c * d)).
	self assert: a · (b,c) = (a · b).
	self assert: a,c · b = (a · b)! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:33:07'!
testArrayMinusArray
	| a b c |
	a _ self newArray.
	b _ self newArray: a size.
	c _ a - b.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) - (b at: i) = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:33:39'!
testArrayMinusArrayInplace
	| a b c |
	a _ self newArray.
	b _ self newArray: a size.
	c _ a copy.
	c -= b.
	1 to: a size do: [:i| self assert: (a at: i) - (b at: i) = (c at: i)]	! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:20:45'!
testArrayMinusScalar
	| a c x |
	a _ self newArray.
	x _ self newScalar.
	c _ a - x.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) - x = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:21:05'!
testArrayMinusScalarInplace
	| a c x |
	a _ self newArray.
	x _ self newScalar.
	c _ a copy.
	c -= x.
	1 to: a size do: [:i| self assert: (a at: i) - x = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:24:38'!
testArrayPlusArray
	| a b c |
	a _ self newArray.
	b _ self newArray: a size.
	c _ a + b.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) + (b at: i) = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:25:02'!
testArrayPlusArrayInplace
	| a b c |
	a _ self newArray.
	b _ self newArray: a size.
	c _ a copy.
	c += b.
	1 to: a size do: [:i| self assert: (a at: i) + (b at: i) = (c at: i)]	! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:21:22'!
testArrayPlusScalar
	| a c x |
	a _ self newArray.
	x _ self newScalar.
	c _ a + x.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) + x = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:21:37'!
testArrayPlusScalarInplace
	| a c x |
	a _ self newArray.
	x _ self newScalar.
	c _ a copy.
	c += x.
	1 to: a size do: [:i| self assert: (a at: i) + x = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/10/2023 10:52:47'!
testArrayTimesArray
	| a b c |
	a _ self newArray.
	b _ self newArray: a size.
	c _ a * b.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) * (b at: i) = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/10/2023 10:53:36'!
testArrayTimesArrayInplace
	| a b c |
	a _ self newArray.
	b _ self newArray: a size.
	c _ a copy.
	c *= b.
	1 to: a size do: [:i| self assert: (a at: i) * (b at: i) = (c at: i)]	! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/10/2023 10:53:07'!
testArrayTimesScalar
	| a c x |
	a _ self newArray.
	x _ self newScalar.
	c _ a * x.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) * x = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/10/2023 10:53:23'!
testArrayTimesScalarInplace
	| a c x |
	a _ self newArray.
	x _ self newScalar.
	c _ a copy.
	c *= x.
	1 to: a size do: [:i| self assert: (a at: i) * x = (c at: i)]! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:13:29'!
testAutoconvolution
	| a |
	a _ self newArray.
	self assert: a autoconvolution asArray = a asArray autoconvolution.
	self assert: a autoconvolution = (a convolution: a)! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:14:38'!
testConvolution
	| a b |
	a _ self newArray.
	b _ self newArray.
	self assert: (a convolution: b) asArray = (a asArray convolution: b asArray)! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:15:12'!
testDotProduct
	| a b |
	a _ self newArray.
	b _ self newArray.
	self assert: a · b = (a asArray · b asArray)! !

!AbstractArrayTest methodsFor: 'arithmetic' stamp: 'len 5/6/2023 17:20:01'!
testSubstraction
	| a b c |
	a _ self newArray.
	b _ self newArray: a size.
	c _ a - b.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) - (b at: i) = (c at: i)]! !

!AbstractArrayTest methodsFor: 'operations' stamp: 'len 5/6/2023 17:13:55'!
testCircularShift
	| a b n |
	a _ self newArray.
	n _ (a size negated to: a size) atRandom: random.
	b _ a >>> n.
	self assert: b size = a size.
	1 to: b size do: [:i| self assert: (b at: i) = (a atWrap: i-n)]! !

!AbstractArrayTest methodsFor: 'operations' stamp: 'len 5/6/2023 17:14:25'!
testConcatenation
	| a b c |
	a _ self newArray.
	b _ self newArray.
	c _ a, b.
	self assert: a size + b size = c size.
	1 to: a size do: [:i| self assert: (c at: i) = (a at: i)].
	1 to: b size do: [:i| self assert: (c at: a size + i) = (b at: i)]! !

!AbstractArrayTest methodsFor: 'operations' stamp: 'len 5/9/2023 21:14:34'!
testHammingWeight
	| a |
	a _ self newArray.
	a size // 2 timesRepeat: [a at: (a size atRandom: random) put: scalars zero].
	self assert: a support size = a weight! !

!AbstractArrayTest methodsFor: 'operations' stamp: 'len 5/6/2023 17:25:33'!
testReversed
	| v i |
	v _ self newArray.
	i _ v size atRandom: random.
	self assert: (v reversed at: i) = (v at: v size - i + 1).
	self assert: v reversed reversed = v! !

!AbstractArrayTest methodsFor: 'operations' stamp: 'len 5/6/2023 17:19:44'!
testShift
	| a b n |
	a _ self newArray.
	n _ (a size negated to: a size) atRandom: random.
	b _ a >> n.
	self assert: b size = a size.
	1 to: b size do: [:i|
		self assert: (b at: i) = ((i-n between: 1 and: a size) ifTrue: [a at: i-n] ifFalse: [scalars zero])]! !

!AbstractArrayTest methodsFor: 'operations' stamp: 'len 5/9/2023 21:14:06'!
testSupport
	| a |
	a _ self newArray.
	a size // 2 timesRepeat: [a at: (a size atRandom: random) put: scalars zero].
	self assert: a support asArray = ((1 to: a size) select: [:i| (a at: i) isZero not])! !

!AbstractArrayTest methodsFor: 'collection protocol' stamp: 'len 5/9/2023 22:07:16'!
testAnySatisfyAndAllSatisfy
	| a x y |
	a _ self newArray.
	x _ scalars atRandom: random.
	[y _ scalars atRandom: random. y = x] whileTrue.
	a atAllPut: x.
	self assert: (a allSatisfy: [:each| each = x]).
	self assert: (a anySatisfy: [:each| each = y]) not.
	a at: (a size atRandom: random) put: y.
	self assert: (a allSatisfy: [:each| each = x]) not.
	self assert: (a anySatisfy: [:each| each = y])! !

!AbstractArrayTest methodsFor: 'collection protocol' stamp: 'len 5/6/2023 17:14:55'!
testCopyFromTo
	| a b start stop |
	a _ self newArray.
	start _ (1 to: a size) atRandom: random.
	stop _ (1 to: a size) atRandom: random.
	b _ a copyFrom: start to: stop.
	self assert: b size = (stop-start+1 max: 0).
	start to: stop do: [:i| self assert: (a at: i) = (b at: i-start+1)]! !

!AbstractArrayTest methodsFor: 'collection protocol' stamp: 'len 5/11/2023 16:16:08'!
testDetect
	| x a |
	x _ scalars atRandom: random.
	a _ self newArray: (100 atRandom: random) fill: [:i| scalars atRandom: random satisfying: [:y| (y = x) not]].
	self assert: (a detect: [:one| one = x] ifNone: [#notFound]) == #notFound.
	a at: (a size atRandom: random) put: x.
	self assert: (a detect: [:one| one = x] ifNone: [#notFound]) = x! !

!AbstractArrayTest methodsFor: 'collection protocol' stamp: 'len 5/10/2023 10:48:01'!
testIndexOf
	| x a k |
	x _ scalars atRandom: random.
	a _ scalars new: (100 atRandom: random) fill: [:i| scalars atRandom: random satisfying: [:y| (y = x) not]].
	self assert: (a indexOf: x) = 0.
	a at: (k _ a size atRandom: random) put: x.
	self assert: (a indexOf: x) = k! !

!AbstractArrayTest methodsFor: 'collection protocol' stamp: 'len 5/6/2023 17:17:29'!
testReplaceFromToWithStartingAt
	| a b start stop aCopy start2 stop2 |
	a _ self newArray.
	b _ self newArray.
	start _ (1 to: a size) atRandom: random.
	start2 _ (1 to: b size) atRandom: random.
	stop _ (1 to: a size) atRandom: random.
	stop _ stop min: b size - start2 +1.
	stop2 _ start2 + stop - start.
	aCopy _ a copy.
	a replaceFrom: start to: stop with: b startingAt: start2.
	self assert: (a copyFrom: 1 to: start - 1) = (aCopy copyFrom: 1 to: start - 1).
	self assert: (a copyFrom: start to: stop) = (b copyFrom: start2 to: stop2).
	self assert: (a copyFrom: stop + 1 to: a size) = (aCopy copyFrom: stop + 1 to: a size)! !

!AbstractArrayTest methodsFor: 'collection protocol' stamp: 'len 5/10/2023 10:52:09'!
testSumAndProduct
	| a |
	a _ self newArray.
	self assert: a sum = a asArray sum.
	self assert: a product = a asArray product! !

!AbstractArrayTest methodsFor: 'collection protocol' stamp: 'len 5/10/2023 10:57:26'!
testSumAndProductWithArgument
	| a dict |
	a _ self newArray.
	dict _ Dictionary new.
	a do: [:each| dict at: each put: (10 atRandom: random)].
	self assert: (a sum: [:each| dict at: each]) = (a asArray sum: [:each| dict at: each]).
	self assert: (a product: [:each| dict at: each]) = (a asArray product: [:each| dict at: each])! !

!AbstractArrayTest methodsFor: 'low level' stamp: 'len 6/6/2023 08:51:49'!
testFirstNonzeroIndex
	| a i |
	a := self newArray.
	random nextBoolean ifTrue: [a := (self newArray: (100 atRandom: random)) zeros, a].
	i := a findFirst: [:x| x isZero not].
	i = 0 ifTrue: [i := nil].
	self assert: a firstNonzeroIndex = i! !

!AbstractArrayTest methodsFor: 'low level' stamp: 'len 5/6/2023 17:16:16'!
testIsZeroAt
	| a x index |
	a _ self newArray.
	x _ scalars zero.
	index _ (1 to: a size) atRandom: random.
	a at: index put: x.
	self assert: (a isZeroAt: index).
	1 to: a size do: [:i| self assert: (a isZeroAt: i) = (a at: i) isZero].! !

!AbstractArrayTest methodsFor: 'low level' stamp: 'len 6/6/2023 08:52:48'!
testLastNonzeroIndex
	| a i |
	a := self newArray.
	random nextBoolean ifTrue: [a := a, (self newArray: (100 atRandom: random)) zeros].
	i := a findLast: [:x| x isZero not].
	i = 0 ifTrue: [i := nil].
	self assert: a lastNonzeroIndex = i! !

!AbstractArrayTest methodsFor: 'low level' stamp: 'len 6/6/2023 13:19:02'!
testPolynomialEvaluation
	| a x |
	a := self newArray.
	x := self newScalar.
	self assert: (a evaluateFrom: 1 to: a size at: x) = ((1 to: a size) sum: [:i| x^(i-1) * (a at: i)])! !

!AbstractArrayTest methodsFor: 'low level' stamp: 'len 5/6/2023 17:20:33'!
testSwapFromToWithStartingAt
	| a b start stop aCopy bCopy start2 stop2 |
	a _ self newArray.
	b _ self newArray.
	start _ (1 to: a size) atRandom: random.
	start2 _ (1 to: b size) atRandom: random.
	stop _ (1 to: a size) atRandom: random.
	stop _ stop min: b size - start2 +1.
	stop2 _ start2 + stop - start.
	aCopy _ a copy.
	bCopy _ b copy.
	a swapFrom: start to: stop with: b startingAt: start2.
	self assert: (a copyFrom: start to: stop) = (bCopy copyFrom: start2 to: stop2).
	self assert: (b copyFrom: start2 to: stop2) = (aCopy copyFrom: start to: stop).
	a swapFrom: start to: stop with: b startingAt: start2.
	self assert: a = aCopy.
	self assert: b = bCopy! !

!ArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2023 23:59:26'!
setUp
	super setUp.
	scalars := ℤ! !

!BinaryArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2023 23:59:43'!
setUp
	super setUp.
	scalars := ℤ/2! !

!BinaryArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/13/2023 12:56:50'!
testIsAllOnes
	| a b |
	0 to: 64 do: [:n|
		a _ BinaryArray new: n.
		1 to: n do: [:i| a bitAt: i put: 1].
		self assert: a isAllOnes.
		1 to: n do: [:i|
			b _ a copy.
			b bitAt: i put: 0.
			self assert: b isAllOnes not]]! !

!BinaryArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/17/2023 16:21:49'!
testReadAndWriteBits
	| bits i length x y |
	x _ self newArray: ((10 to: 100) atRandom: random).
	y _ x copy.
	length _ (32 min: x size) atRandom: random.
	i _ (x size - length + 1) atRandom: random.
	bits _ random nextBits: length.
	self assert: bits highBit <= length.
	x bits: length at: i put: bits.
	self assert: (x bits: length at: i) = bits.
	x bits: length at: i put: (y bits: length at: i).
	self assert: x = y! !

!BinaryArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/13/2023 12:57:13'!
testSumBug
	self assert: (BinaryArray newFrom: 2r1011101) sum representative = 1.
	self assert: (BinaryArray newFrom: 2r10000100100010) sum representative = 0! !

!BinaryExtensionSlicedArrayTest methodsFor: 'as yet unclassified' stamp: 'len 6/11/2023 21:38:55'!
setUp
	super setUp.
	scalars := GaloisField new: 2 to: ((2 to: 7) atRandom: random)! !

!BinaryPowerArrayTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:54:00'!
setUp
	super setUp.
	scalars := PrimePowerRing new: 2 to: ((2 to: 64) atRandom: random)! !

!GaloisFieldArrayTest methodsFor: 'as yet unclassified' stamp: 'len 6/11/2023 21:37:31'!
setUp
	super setUp.
	scalars := GaloisField new: ((#(16rFF 16rFFFF 16rFFFFFFFF) atRandom: random) atRandom: random) nextPrime to: (10 atRandom: random)! !

!GaloisRingArrayTest methodsFor: 'as yet unclassified' stamp: 'len 6/12/2023 09:27:09'!
setUp
	super setUp.
	scalars := GaloisRing new: (3 atRandom: random) nextPrime to: (2 atRandom: random) to: (2 atRandom: random)! !

!ModularArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:01:54'!
setUp
	super setUp.
	scalars := ℤ / ((16r100000001 to: 16r100000001 + 1000) atRandom: random)! !

!ModularByteArrayTest methodsFor: 'as yet unclassified' stamp: 'len 7/11/2023 18:16:13'!
setUp
	super setUp.
	scalars := ℤ / ((4 to: 256) atRandom: random)! !

!ModularByteArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/12/2023 14:38:04'!
testRepresentativesUseByteArray
	self assert: self newArray representatives class = ByteArray! !

!ModularDoubleByteArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:02:32'!
setUp
	super setUp.
	scalars := ℤ / ((16rFF+2 to: 16rFFFF+1) atRandom: random)! !

!ModularDoubleByteArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/12/2023 14:38:18'!
testRepresentativesUseDoubleByteArray
	self assert: self newArray representatives class = DoubleByteArray! !

!ModularWordArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:02:53'!
setUp
	super setUp.
	scalars := ℤ / ((16rFFFF+2 to: 16rFFFFFFFF+1) atRandom: random)! !

!ModularWordArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/12/2023 14:38:31'!
testRepresentativesUseWordArray
	self assert: self newArray representatives class = WordArray! !

!ProductArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:03:27'!
setUp
	super setUp.
	scalars := ℤ × (ℤ/((64 atRandom: random) + 1))! !

!TernaryArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:03:46'!
setUp
	super setUp.
	scalars := ℤ/3! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:14:20'!
newScalar
	^ scalars atRandom: random bits: (60 atRandom: random)! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:13:57'!
newTuple
	^ self newTuple: (200 atRandom: random)! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 17:18:32'!
newTuple: n
	^ scalars ^ n atRandom: random! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 17:10:58'!
testAccessing
	| a i x |
	a _ self newTuple.
	x _ self newScalar.
	i _ (1 to: a size) atRandom: random.
	a at: i put: x.
	self assert: (a at: i) = x! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 17:41:36'!
testAddition
	| a b c |
	a _ self newTuple.
	b _ self newTuple: a size.
	c _ a + b.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) + (b at: i) = (c at: i)].
	self assert: (a + a negated) isZero.
	self assert: a + b = (b + a).
	self assert: a + a = (a*2).
	self assert: a - b = (a + b negated)! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 17:57:46'!
testCircularShift
	| a b n |
	a _ self newTuple.
	n _ (a size negated to: a size) atRandom: random.
	b _ a >>> n.
	self assert: b size = a size.
	1 to: b size do: [:i| self assert: (b at: i) = (a coefficients atWrap: i-n)]! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:22:05'!
testConcatenation
	| a b c |
	a _ self newTuple.
	b _ self newTuple.
	c _ a, b.
	self assert: a size + b size = c size.
	1 to: a size do: [:i| self assert: (c at: i) = (a at: i)].
	1 to: b size do: [:i| self assert: (c at: a size + i) = (b at: i)]! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:22:42'!
testCopyFromTo
	| a b start stop |
	a _ self newTuple.
	start _ (1 to: a size) atRandom: random.
	stop _ (1 to: a size) atRandom: random.
	b _ a copyFrom: start to: stop.
	self assert: b size = (stop-start+1 max: 0).
	start to: stop do: [:i| self assert: (a at: i) = (b at: i-start+1)]! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:15:45'!
testDotProduct
	| a b |
	a _ self newTuple.
	b _ self newTuple.
	self assert: a · b = (a asArray · b asArray)! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 14:59:00'!
testFirstNonzeroIndex
	| a i |
	a := self newTuple.
	random nextBoolean ifTrue: [a := (self newTuple: (100 atRandom: random)) zero, a].
	i := a coefficients findFirst: [:x| x isZero not].
	i = 0 ifTrue: [i := nil].
	self assert: a firstNonzeroIndex = i! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:23:14'!
testIsZeroAt
	| a x index |
	a _ self newTuple.
	x _ scalars zero.
	index _ (1 to: a size) atRandom: random.
	a at: index put: x.
	self assert: (a isZeroAt: index).
	1 to: a size do: [:i| self assert: (a isZeroAt: i) = (a at: i) isZero].! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 14:59:17'!
testLastNonzeroIndex
	| a i |
	a := self newTuple.
	random nextBoolean ifTrue: [a := a, (self newTuple: (100 atRandom: random)) zero].
	i := a coefficients findLast: [:x| x isZero not].
	i = 0 ifTrue: [i := nil].
	self assert: a lastNonzeroIndex = i! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:20:30'!
testProduct
	| a b c |
	a _ self newTuple.
	b _ self newTuple: a size.
	c _ a * b.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) * (b at: i) = (c at: i)]! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 17:27:13'!
testProductByScalar
	| a c x |
	a _ self newTuple.
	x _ self newScalar.
	c _ a * x.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) * x = (c at: i)]! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 17:27:46'!
testReversed
	| v i |
	v _ self newTuple.
	i _ v size atRandom: random.
	self assert: (v reversed at: i) = (v at: v size - i + 1).
	self assert: v reversed reversed = v! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:24:00'!
testShift
	| a b n |
	a _ self newTuple.
	n _ (a size negated to: a size) atRandom: random.
	b _ a >> n.
	self assert: b size = a size.
	1 to: b size do: [:i|
		self assert: (b at: i) = ((i-n between: 1 and: a size) ifTrue: [a at: i-n] ifFalse: [scalars zero])]! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/24/2022 11:16:36'!
testSubstraction
	| a b c |
	a _ self newTuple.
	b _ self newTuple: a size.
	c _ a - b.
	self assert: c size = a size.
	1 to: a size do: [:i| self assert: (a at: i) - (b at: i) = (c at: i)]! !

!AbstractCommutativeRingTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/2/2021 12:30:57'!
testCrossProduct
	| V a b c r |
	V _ scalars^3.
	a _ V atRandom: random bits: 20.
	b _ V atRandom: random bits: 20.
	c _ V atRandom: random bits: 20.
	r _ V scalars atRandom: random bits: 10.
	self assert: (a × a) isZero.
	self assert: a × b = (b × a) negated.
	self assert: a × (b + c) = (a × b + (a × c)).
	self assert: (a*r) × b = (a × (b*r)).
	self assert: (a*r) × b = (a × b * r).
	self assert: (a × (b × c) + (b × (c × a)) + (c × (a × b))) isZero! !

!AbstractCommutativeRingTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/2/2021 12:31:07'!
testCrossProductAndDotProduct
	| V a b c d |
	V _ scalars^3.
	a _ V atRandom: random bits: 20.
	b _ V atRandom: random bits: 20.
	c _ V atRandom: random bits: 20.
	d _ V atRandom: random bits: 20.
	self assert: a · (b × c) = (b · (c × a)).
	self assert: a · (b × c) = (c · (a × b)).
	"Lagrange identity:"
	self assert: (a × b) norm = (a norm * b norm - (a · b) squared).
	"Binet-Chauchy identity:"
	self assert: a × b · (c × d) = (a · c *(b · d) - (a · d * (b · c)))! !

!AbstractCommutativeRingTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2022 20:02:18'!
testHomogeneousTuples
	| n a b c d |
	n _ 5 atRandom: random.
	[a _ scalars^n atRandom: random bits: 20. a isZero] whileTrue.
	b _ a * (scalars units atRandom: random bits: 10).
	self assert: a ~ b.
	n >= 2 ifFalse: [n _ (2 to: 5) atRandom: random].
	c _ scalars^n x: 1.
	d _ scalars^n x: 2.
	self deny: c ~ d! !

!AbstractModularIntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:17:21'!
testModTupleOperations
	| n a b modulus |
	modulus := scalars modulus.
	n := 5 atRandom: random.
	a := ℤ^n atRandom: random bits: 30.
	b := ℤ^n atRandom: random bits: 30.
	#(+ - · *) do: [:op|
		self testTuple: a with: b operation: op modulo: modulus].
	#(negated reversed copy sum product) do: [:op|
		self testTuple: a operation: op modulo: modulus].! !

!AbstractModularIntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:28:45'!
testTuple: a operation: aSymbol modulo: modulus
	self assert: (a perform: aSymbol) % modulus = (a % modulus perform: aSymbol)! !

!AbstractModularIntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:28:53'!
testTuple: a with: b operation: aSymbol modulo: modulus
	self assert: (a perform: aSymbol with: b) % modulus = (a % modulus perform: aSymbol with: b % modulus)! !

!BinaryPowerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:54:00'!
setUp
	super setUp.
	scalars := PrimePowerRing new: 2 to: ((2 to: 64) atRandom: random)! !

!BinaryTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:41:19'!
setUp
	super setUp.
	scalars := ℤ/2! !

!BinaryTupleTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:29:12'!
testBitTupleBug
	| Z2 |
	Z2 _ PrimeField new: 2.
	self assert: ((1,0,0) % 2 :: sum = Z2 one).
	self assert: ((0,1,0) % 2 :: sum = Z2 one).
	self assert: ((1,1,0) % 2 :: sum = Z2 zero).
	self assert: ((1,1,1) % 2 :: sum = Z2 one)! !

!ModularIntegerByteTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:42:12'!
setUp
	super setUp.
	scalars := ℤ / ((4 to: 256) atRandom: random)! !

!ModularIntegerDoubleByteTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:41:39'!
setUp
	super setUp.
	scalars := ℤ / ((2^8 + 1 to: 2^16) atRandom: random)! !

!ModularIntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:42:30'!
setUp
	super setUp.
	scalars := ℤ / ((16r100000001 to: 16r100000001 + 1000) atRandom: random)! !

!ModularIntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/30/2023 23:06:28'!
testHomogeneousTuplesNonExample
	| P Q |
	P := (2,4,1) % 6.
	Q := (0,1,3) % 6.
	self deny: P ~ Q! !

!ModularIntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/30/2023 23:08:30'!
testTupleUnitsAndAssociatesExamples
	"Examples from Magma."
	| R |
	R := ℤ/6.
	self assert: (R !!!! #(0 4)) ~ (R !!!! #(0 2)).
	self assert: (R !!!! #(4 3)) ~ (R !!!! #(2 3)).
	R := ℤ/8.
	self assert: (R !!!! #(3 2)) ~ (R !!!! #(1 6)).
	self assert: (R !!!! #(7 2)) ~ (R !!!! #(1 6)).
	self assert: (R !!!! #(2 7)) ~ (R !!!! #(2 3))

"this fails, but anyway canonical associates for tuples over a ring with zero divisors is not well defined:
	R _ ℤ/6.
	self assert: (R !!!! #(0 2)) normalized = (R !!!! #(0 2)).
	self assert: (R !!!! #(0 4)) normalized = (R !!!! #(0 2)).
	self assert: (R !!!! #(2 3)) normalized = (R !!!! #(2 3)).
	self assert: (R !!!! #(4 3)) normalized = (R !!!! #(2 3)).
	R _ ℤ/8.
	self assert: (R !!!! #(3 2)) normalized = (R !!!! #(1 6)).
	self assert: (R !!!! #(7 2)) normalized = (R !!!! #(1 6)).
	self assert: (R !!!! #(2 3)) normalized = (R !!!! #(2 3)).
	self assert: (R !!!! #(2 7)) normalized = (R !!!! #(2 3))
"! !

!ModularIntegerWordTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:41:52'!
setUp
	super setUp.
	scalars := ℤ / ((2^16 + 1 to: 2^32) atRandom: random)! !

!TernaryTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:42:39'!
setUp
	super setUp.
	scalars := ℤ/3! !

!BinaryExtensionFieldTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:42:57'!
setUp
	super setUp.
	scalars := GaloisField new: 2 to: (8 atRandom: random) + 1! !

!FiniteFieldTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:43:33'!
setUp
	super setUp.
	scalars := GaloisField new: ((#(16rFF 16rFFFF 16rFFFFFFFF) atRandom: random) atRandom: random) nextPrime to: (10 atRandom: random)! !

!GaloisRingTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:44:04'!
setUp
	super setUp.
	scalars := GaloisRing new: (3 atRandom: random) nextPrime to: (2 atRandom: random) to: (2 atRandom: random)! !

!IntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:44:14'!
setUp
	super setUp.
	scalars := ℤ! !

!IntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/16/2022 19:45:51'!
testDomainError
	| v |
	v _ ℤ^3 !! #(1 2 3).
	self should: [v / 2] raise: Error! !

!IntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/30/2023 23:05:33'!
testHomogeneousTuplesExample
	self assert: (1,2) ~ (2,4).
	self deny: (1,2) ~ (1,0)! !

!IntegerTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/30/2023 23:07:20'!
testIntegerTupleSumExample
	self assert: (1,2,3) sum = (1 + 2 + 3)! !

!MultivariateTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:56:26'!
setUp
	super setUp.
	scalars := ℚ polynomialsIn: #(x y) ordering: #invlex! !

!MultivariateTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:57:37'!
testPOTOrderingExample
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Definition 3.5.3."
	| R x y M e1 e2 |
	R := ℚ polynomialsIn: #(x y) ordering: #invlex.
	x := R x. y := R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M := R^2 orderedBy: #pot.
	e1 := M x: 1. e2 := M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	self assert: (e1*x) leadingMonomial < (e1*y) leadingMonomial.
	self assert: (e1*y) leadingMonomial < (e1*x*y) leadingMonomial.
	self assert: (e1*x*y) leadingMonomial < (e2*x) leadingMonomial! !

!MultivariateTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:57:25'!
testSTupleExample
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.5.18."
	| R x y M e1 e2 f g h |
	R := ℚ polynomialsIn: #(x y) ordering: #invlex.
	x := R x. y := R y.
	x leadingMonomial < y leadingMonomial.
	M := R^2 orderedBy: #top.
	e1 := M x: 1. e2 := M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f := M !! {x^2 + 1. x*(y^3)*5 + x}.
	g := M !! {x^2*y. x^3*y*3 + y}.
	h := M !! {x^4/5 + (x^2/5) - (x^2*(y^3)/3). x^3/5 - (y^3/3)}.
	self assert: (f s: g) = h! !

!MultivariateTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:58:29'!
testSchreyerOrderingExample
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.7.11."
	| R x y M e1 e2 e3 g1 g2 g3 a b c |
	R := ℚ polynomialsIn: #(x y) ordering: #invlex.
	x := R x. y := R y.
	self assert: x leadingMonomial < y leadingMonomial.
	g1 := x^2*(y^2) - (x^3*y).
	g2 := x*(y^3) - (x^2*(y^2)).
	g3 := y^4 - (x^3).
	M := R^3 orderedBy: (SchreyerMonomialOrdering weights: {g1. g2. g3}).
	e1 := M x: 1. e2 := M x: 2. e3 := M x: 3.
	a := e2*x.
	b := e1*y.
	c := e3*x.
	self assert: a leadingMonomial < b leadingMonomial.
	self assert: b leadingMonomial < c leadingMonomial.
	self assert: a leadingMonomial < c leadingMonomial! !

!MultivariateTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:57:47'!
testTOPOrderingExample
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Definition 3.5.2."
	| R x y M e1 e2 |
	R := ℚ polynomialsIn: #(x y) ordering: #invlex.
	x := R x. y := R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M := R^2 orderedBy: #top.
	e1 := M x: 1. e2 := M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	self assert: (e1*x) leadingMonomial < (e2*x) leadingMonomial.
	self assert: (e2*x) leadingMonomial < (e1*y) leadingMonomial.
	self assert: (e1*y) leadingMonomial < (e1*x*y) leadingMonomial! !

!MultivariateTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:57:58'!
testTupleOrderingsExamples
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.5.5."
	| R x y M e1 e2 f |
	R := ℚ polynomialsIn: #(x y) ordering: #invlex.
	x := R x. y := R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M := R^2 orderedBy: #top.
	e1 := M x: 1. e2 := M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f := M !! {x^3*y*2 - (y^3) + (x*5). x*(y^2)*3 + (y^2) + (x*4)}.

	self assert: f leadingMonomial position = 1.
	self assert: f leadingMonomial monomial = (y^3) leadingMonomial.
	self assert: f leadingCoefficient = (-1).
	self assert: f leadingTerm = (e1 * (y^3) negated).

	M := M orderedBy: #pot.
	e1 := M x: 1. e2 := M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f := M !! f.

	self assert: f leadingMonomial position = 2.
	self assert: f leadingMonomial monomial = (y^2*x) leadingMonomial.
	self assert: f leadingCoefficient = 3.
	self assert: f leadingTerm = (e2 * (y^2*x*3))! !

!MultivariateTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:58:09'!
testTupleReductionExample
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.5.7."
	|R x y M e1 e2 f g h |
	R := ℚ polynomialsIn: #(x y) ordering: #invlex.
	x := R x. y := R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M := R^2 orderedBy: #top.
	e1 := M x: 1. e2 := M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f := M !! {x^3*y*2 - (y^3). x*(y^2)*3 + (y^2) + (x*4)}.
	g := M !! {x + 1. y^2 + x}.
	h := M !! {x^3*y*2 - (y^3) - (x^2*3) - (x*4) - 1. x*3 - (x^2*3)}.
	self assert: f \\ {g} = h! !

!MultivariateTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:58:18'!
testTupleReductionExample2
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.5.9."
	|R x y M e1 e2 f1 f2 f h |
	R := ℚ polynomialsIn: #(x y) ordering: #invlex.
	x := R x. y := R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M := R^2 orderedBy: #top.
	e1 := M x: 1. e2 := M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f1 := M !! {x*y - y. x^2}.
	f2 := M !! {x. y^2 - x}.
	f := M !! {y^2 + (x^2*y*2). y^2}.
	h := M !! {y^2 + (y*2) - x. x^3*(-2) - (x^2*2) + x}.
	self assert: f \\ {f1. f2} = h! !

!ProductRingTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:44:31'!
setUp
	super setUp.
	scalars := ℤ × (ℤ/((64 atRandom: random) + 1))! !

!RationalPolynomialRingTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:44:40'!
setUp
	super setUp.
	scalars := ℚ polynomials! !

!RationalPolynomialRingTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/30/2023 23:04:07'!
testHomogeneousTuplesExample
	| x P Q |
	x := scalars x.
	P := (x, (x^2 + 1)).
	Q := (x^2, (x^3 + x)).
	self assert: P ~ Q! !

!RationalTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:44:50'!
setUp
	super setUp.
	scalars := ℚ! !

!SmallFiniteFieldTupleTest methodsFor: 'initialization' stamp: 'len 5/26/2023 00:55:50'!
setUp
	| p |
	super setUp.
	p := (13 atRandom: random) nextPrime.
	scalars := GaloisField new: p to: (((256 log: p) floor + 1) atRandom: random) + 1! !

!TernaryExtensionFieldTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:45:39'!
setUp
	super setUp.
	scalars := GaloisField new: 3 to: (8 atRandom: random) + 1! !

!MatrixAlgebraTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:52:26'!
setUp
	super setUp.
	scalars := (ℚ^((0 to: 3) atRandom: random)) endomorphisms! !

!QuaternionAlgebraTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:46:21'!
setUp
	super setUp.
	scalars := QuaternionAlgebra over: ℚ invariants: #(1 1)! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/20/2023 15:21:11'!
testAddition
	| A B |
	A := self newMatrix.
	B := self newMatrix: A height @ A width.
	self testAddition: A with: B! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/20/2023 15:20:59'!
testAddition: A with: B
	| x |
	self assert: (A + A negated) isZero.
	self assert: A + A zero = A.
	self assert: A + B = (B + A).
	x := A domain atRandom: random bits: 10.
	self assert: (A+B)*x = (A*x + (B*x)).
	x := A codomain atRandom: random bits: 10.
	self assert: x*(A+B) = (x*A + (x*B))! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/20/2023 16:38:20'!
testInplaceAddition
	| A B C |
	A := self newMatrix.
	B := self newMatrix: A height @ A width.
	C := self newMatrix: A height @ A width.
	C replaceWith: A plus: B.
	self assert: A + B = C! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/20/2023 18:54:46'!
testInplaceMultiplication
	| A B C |
	A := self newMatrix.
	B := self newMatrix: A width @ (5 atRandom: random).
	C := self newMatrix: A height @ B width.
	C replaceWith: A times: B.
	self assert: A * B = C! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/20/2023 16:39:06'!
testInplaceSubstraction
	| A B C |
	A := self newMatrix.
	B := self newMatrix: A height @ A width.
	C := self newMatrix: A height @ A width.
	C replaceWith: A minus: B.
	self assert: A - B = C! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/22/2023 09:47:17'!
testMatrixTupleMultiplication
	| A v |
	A := self newMatrix.
	v := A domain atRandom: random.
	self assert: (A * v) asMatrix transpose = (A * v asMatrix)! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/20/2023 15:22:09'!
testMultiplication
	| A B |
	A := self newMatrix.
	B := self newMatrix: A width @ (5 atRandom: random).
	self testMultiplication: A with: B! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/20/2023 15:21:48'!
testMultiplication: A with: B
	| x |
	self assert: A * A domain id = A.
	self assert: A codomain id * A = A.
	x := B domain atRandom: random bits: 10.
	self assert: A*B*x = (A*(B*x)).
	x := A codomain atRandom: random bits: 10.
	self assert: x*A*B = (x*(A*B))! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/20/2023 21:17:29'!
testScalarAction
	| A b B |
	A := self newMatrix.
	b := scalars atRandom: random bits: 20.
	B := A*b.
	A withIndicesDo: [:each :i :j| self assert: each*b = (B at: i@j)]! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/26/2023 18:47:21'!
testStrassenWinogradSquareEven
	| n A B C |
	n := (10 atRandom: random) bitAnd: 1 bitInvert.
	A := self newMatrix: n @ n.
	B := self newMatrix: n @ n.
	C := A copy.
	C replaceStrassenWinogradWith: A times: B.
	self assert: A * B = C! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 5/26/2023 18:47:43'!
testStrassenWinogradSquareOddPeeling
	| n A B C |
	n := (10 atRandom: random) bitOr: 1.
	A := self newMatrix: n @ n.
	B := self newMatrix: n @ n.
	C := A copy.
	C replacePeelingStrassenWinogradWith: A times: B.
	self assert: A * B = C! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 15:10:56'!
newSubmatrix: extent of: aMatrix
	| i₀ j₀ |
	i₀ := aMatrix height - extent x + 1 atRandom: random.
	j₀ := aMatrix width - extent y + 1 atRandom: random.
	^ aMatrix from: i₀ @ j₀ to: i₀ @ j₀ + extent - (1@1)! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 15:10:02'!
newSubmatrixOf: aMatrix
	^ self newSubmatrix: (aMatrix height atRandom: random) @ (aMatrix width atRandom: random) of: aMatrix! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 15:21:17'!
testMatrixSubmatrixAddition
	| A B S |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	B := self newMatrix: S height @ S width.
	self testAddition: S with: B! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 16:53:53'!
testMatrixSubmatrixInplaceAdditionIntoMatrix
	| A B S C |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	B := self newMatrix: S height @ S width.
	C := self newMatrix: S height @ S width.
	C replaceWith: S plus: B.
	self assert: S + B = C.
	C := self newMatrix: S height @ S width.
	C replaceWith: B plus: S.
	self assert: S + B = C! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 16:54:02'!
testMatrixSubmatrixInplaceSubstractionIntoMatrix
	| A B S C |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	B := self newMatrix: S height @ S width.
	C := self newMatrix: S height @ S width.
	C replaceWith: S minus: B.
	self assert: S - B = C.
	C := self newMatrix: S height @ S width.
	C replaceWith: B minus: S.
	self assert: B - S = C! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 15:22:01'!
testMatrixSubmatrixMultiplication
	| A B S |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	B := self newMatrix: S width @ (5 atRandom: random).
	self testMultiplication: S with: B.
	B := self newMatrix: (5 atRandom: random) @ S height.
	self testMultiplication: B with: S! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 15:13:09'!
testSubmatrix
	| A i₀ i₁ j₀ j₁ start stop B S |
	A := self newNonemptyMatrix.
	i₀ := A height atRandom: random.
	i₁ := (i₀ to: A height) atRandom: random.
	j₀ := A width atRandom: random.
	j₁ := (j₀ to: A width) atRandom: random.
	start := i₀ @ j₀.
	stop := i₁ @ j₁.
	B := A copyFrom: start to: stop.
	S := A from: start to: stop.
	self assert: B = S.
	self assert: S = B! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 15:21:23'!
testSubmatrixSubmatrixAddition
	| A S T |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	T := self newSubmatrix: S height @ S width of: A.
	self testAddition: S with: T! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 16:56:48'!
testSubmatrixSubmatrixInplaceAdditionIntoMatrix
	| A S T C |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	T := self newSubmatrix: S height @ S width of: A.
	C := self newMatrix: S height @ S width.
	C replaceWith: S plus: T.
	self assert: S + T = C.
! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 16:57:10'!
testSubmatrixSubmatrixInplaceSubstractionIntoMatrix
	| A S T C |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	T := self newSubmatrix: S height @ S width of: A.
	C := self newMatrix: S height @ S width.
	C replaceWith: S minus: T.
	self assert: S - T = C.
! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/20/2023 15:22:15'!
testSubmatrixSubmatrixMultiplication
	| A S T |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	T := self newSubmatrix: S width @ (A height atRandom: random) of: A transpose.
	self testMultiplication: S with: T! !

!AbstractMatrixTest methodsFor: 'submatrices' stamp: 'len 5/22/2023 09:48:27'!
testSubmatrixTupleMultiplication
	| A S v |
	A := self newNonemptyMatrix.
	S := self newSubmatrixOf: A.
	v := S domain atRandom: random.
	self assert: (S * v) asMatrix transpose = (S * v asMatrix)! !

!AbstractMatrixTest methodsFor: 'other' stamp: 'len 5/20/2023 11:51:00'!
testCopyFromTo
	| A i₀ i₁ j₀ j₁ start stop B |
	A := self newMatrix.
	i₀ := A height > 0 ifTrue: [A height atRandom: random] ifFalse: [0].
	i₁ := A height > 0 ifTrue: [A height atRandom: random] ifFalse: [-1].
	j₀ := A width > 0 ifTrue: [A width atRandom: random] ifFalse: [0].
	j₁ := A width > 0 ifTrue: [A width atRandom: random] ifFalse: [-1].
	start := i₀ @ j₀.
	stop := i₁ @ j₁.
	B := A copyFrom: start to: stop.
	start x to: stop x do: [:i|
		start y to: stop y do: [:j|
			self assert: (A at: i@j) = (B at: i - start x + 1 @ (j - start y + 1))]	]! !

!AbstractMatrixTest methodsFor: 'other' stamp: 'len 5/16/2022 11:41:01'!
testDirectSumMatrix
	| A B C |
	A _ self newMatrix.
	B _ self newMatrix.
	C _ A ⊕ B.
	self assert: A width + B width = C width.
	self assert: A height + B height = C height.
	self assert: (C copyFrom: 1 @ 1 to: A height @ A width) = A.
	self assert: (C copyFrom: A height + 1 @ (A width + 1) to: C height @ C width) = B.
	self assert: (C copyFrom: A height + 1 @ 1 to: C height @ A width) isZero.
	self assert: (C copyFrom: 1 @ (A width + 1) to: A height @ C width) isZero! !

!AbstractMatrixTest methodsFor: 'other' stamp: 'len 10/5/2022 17:08:41'!
testLeftRightIdentity
	| A |
	A _ scalars^(5 atRandom: random) ⇒ (scalars^(7 atRandom: random)) atRandom: random bits: 100.
	self assert: A codomain id * A = A.
	self assert: A * A domain id = A! !

!AbstractMatrixTest methodsFor: 'other' stamp: 'len 5/19/2022 12:56:56'!
testMatrixConcatenation
	| A B C |
	A _ self newMatrix.
	B _ self newMatrix: A height @ ((0 to: 10) atRandom: random).
	C _ A ⊔ B.
	self assert: (C copyFromColumn: 1 to: A width) = A.
	self assert: (C copyFromColumn: A width + 1 to: C width) = B! !

!AbstractMatrixTest methodsFor: 'other' stamp: 'len 5/19/2022 12:56:39'!
testMatrixStacking
	| A B C |
	A _ self newMatrix.
	B _ self newMatrix: ((0 to: 10) atRandom: random) @ A width.
	C _ A ⊓ B.
	self assert: (C copyFromRow: 1 to: A height) = A.
	self assert: (C copyFromRow: A height + 1 to: C height) = B! !

!AbstractMatrixTest methodsFor: 'other' stamp: 'len 3/26/2021 13:23:21'!
testTranspose
	| A |
	A _ self newMatrix.
	self assert: A columns = A transpose rows.
	self assert: A rows = A transpose columns.
	self assert: A transpose transpose = A! !

!AbstractMatrixTest methodsFor: 'row/column operations' stamp: 'len 5/12/2023 20:47:17'!
testAddRowTimesScalar
	| A B i1 i2 j x |
	[A _ self newMatrix. A height > 0 and: [A width > 0]] whileFalse.
	i1 _ A height atRandom: random.
	i2 _ A height atRandom: random.
	j _ A width atRandom: random.
	B _ A copy.
	x _ scalars atRandom: random bits: 50.
	B addRow: i1 times: x to: i2 startingAt: j.
	self assert: ((B rowAt: i2) copyFrom: j to: A width) = ((A rowAt: i1) * x + (A rowAt: i2) copyFrom: j to: A width).
	self assert: ((B rowAt: i2) copyFrom: 1 to: j-1) = ((A rowAt: i2) copyFrom: 1 to: j-1)! !

!AbstractMatrixTest methodsFor: 'row/column operations' stamp: 'len 5/12/2023 20:47:23'!
testMultiplyRowByScalar
	| A B i j x |
	[A _ self newMatrix. A height > 0 and: [A width > 0]] whileFalse.
	i _ A height atRandom: random.
	j _ A width atRandom: random.
	B _ A copy.
	x _ scalars atRandom: random bits: 50.
	B multiplyRow: i by: x startingAt: j.
	self assert: ((B rowAt: i) copyFrom: j to: A width) = ((A rowAt: i) * x copyFrom: j to: A width).
	self assert: ((B rowAt: i) copyFrom: 1 to: j-1) = ((A rowAt: i) copyFrom: 1 to: j-1)! !

!AbstractMatrixTest methodsFor: 'row/column operations' stamp: 'len 5/12/2023 20:47:29'!
testSwapRows
	| A B i1 i2 |
	[A _ self newMatrix. A height > 0 and: [A width > 0]] whileFalse.
	i1 _ A height atRandom: random.
	i2 _ A height atRandom: random.
	B _ A copy.
	B swapRow: i1 with: i2.
	self assert: (B rowAt: i1) = (A rowAt: i2).
	self assert: (B rowAt: i2) = (A rowAt: i1).
	B swapRow: i1 with: i2.
	self assert: A = B! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 10/5/2022 17:08:01'!
testBackslashMatrix
	| n m n2 A B X |
	50 timesRepeat:
		[n _ 4 atRandom: random.
		m _ 4 atRandom: random.
		A _ scalars^n ⇒ (scalars^m) atRandom: random bits: m*n*5.
		n2 _ 4 atRandom: random.
		B _ A*(scalars^n2 ⇒ (scalars^n) atRandom: random bits: n*n2*5).
		X _ A \ B.
		self assert: A*X = B]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 10/5/2022 17:08:14'!
testBackslashOneRowMatrix
	| n A v b x |
	50 timesRepeat:
		[n _ 5 atRandom: random.
		A _ scalars^n ⇒ (scalars^1) atRandom: random bits: n*5.
		v _ scalars^n atRandom: random bits: n*5.
		b _ A*v.
		x _ A \ b.
		self assert: A*x = b]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 10/5/2022 17:08:20'!
testBackslashTuple
	| n m A v b x |
	50 timesRepeat:
		[n _ 4 atRandom: random.
		m _ 4 atRandom: random.
		A _ scalars^n ⇒ (scalars^m) atRandom: random bits: m*n*5.
		v _ scalars^n atRandom: random bits: n*5.
		b _ A*v.
		x _ A \ b.
		self assert: A*x = b]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 10/5/2022 17:08:29'!
testBackslashZeroMatrix
	| n m n2 A B X |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ (scalars^n ⇒ (scalars^m)) zero.
	n2 _ 5 atRandom: random.
	B _ A*(scalars^n2 ⇒ (scalars^n) atRandom: random bits: n*n2*5).
	X _ A \ B.
	self assert: A*X = B! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 5/26/2023 15:00:01'!
testMatrixDivision
	| A B n result N X combinations |
	n := 5 atRandom: random.
	A := scalars^5 ⇒ (scalars^n) atRandom: random bits: 100.
	combinations := (1 to: n) collect: [:each| random nextBoolean].
	B := Matrix columns:
		(combinations collect: [:isCombination|
			isCombination
				ifTrue: [A * (A domain atRandom: random bits: 20)]
				ifFalse: [A codomain atRandom: random bits: 20]]).
	result := B divisionBy: A.
	X := result first.
	N := result second.
	self assert: A*X + N = B.
	combinations withIndexDo: [:isCombination :i| isCombination ifTrue: [self assert: (N columnAt: i) isZero]]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 10/5/2022 17:08:56'!
testSlashMatrix
	| n m n2 A B X |
	50 timesRepeat:
		[n _ 4 atRandom: random.
		m _ 4 atRandom: random.
		A _ scalars^n ⇒ (scalars^m) atRandom: random bits: m*n*5.
		n2 _ 4 atRandom: random.
		B _ (scalars^m ⇒ (scalars^n2) atRandom: random bits: n*n2*5)*A.
		X _ B / A.
		self assert: X*A = B]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 10/5/2022 17:09:04'!
testSlashTuple
	| n m A v b x |
	50 timesRepeat:
		[n _ 4 atRandom: random.
		m _ 4 atRandom: random.
		A _ scalars^n ⇒ (scalars^m) atRandom: random bits: m*n*5.
		v _ scalars^m atRandom: random bits: n*5.
		b _ v*A.
		x _ b / A.
		self assert: x*A = b]! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 3/26/2021 13:16:39'!
testCokernelAndCoequalizer
	| A |
	A _ self newMatrix.
	self assert: A cokernel = (A coequalizer: A zero)! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 7/25/2021 10:14:36'!
testCosyzygies
	| A X |
	A _ self newMatrix.
	X _ A cosyzygies.
	self assert: (X · A) isZero! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 10/5/2022 17:08:34'!
testCosyzygiesZeroMatrix
	| n m A X |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ (scalars^n ⇒ (scalars^m)) zero.
	X _ A cosyzygies.
	self assert: (X · A) isZero! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 6/26/2023 12:48:48'!
testImage
	| A |
	A := self newMatrix.
	self assert: A image = (scalars^A height sub: A columns)! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 3/26/2021 13:18:21'!
testKernelAndEqualizer
	| A |
	A _ self newMatrix.
	self assert: A kernel = (A equalizer: A zero)! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 7/25/2021 09:35:46'!
testSyzygies
	| A X |
	A _ self newMatrix.
	X _ A syzygies.
	self assert: (A · X) isZero.
"	self assert: K isMonomorphism."
	self assert: A kernel = X image! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 10/5/2022 17:09:09'!
testSyzygiesZeroMatrix
	| n m A X |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ (scalars^n ⇒ (scalars^m)) zero.
	X _ A syzygies.
	self assert: (A · X) isZero.
	self assert: A kernel = X image! !

!AbstractMatrixTest methodsFor: 'private' stamp: 'len 10/5/2022 17:07:27'!
newMatrix
	| n m |
	n _ (0 to: 5) atRandom: random.
	m _ (0 to: 5) atRandom: random.
	^ scalars^n ⇒ (scalars^m) atRandom: random bits: (m*n*5+1 atRandom: random)! !

!AbstractMatrixTest methodsFor: 'private' stamp: 'len 5/26/2023 14:59:33'!
newMatrix: shape
	| samples hom |
	hom := scalars^shape y ⇒ (scalars^shape x).
	random nextBoolean ifTrue: [^ hom atRandom: random bits: 100].
	samples := {scalars zero. scalars one. scalars one negated}.
	^ hom fill: [:i :j| (2 atRandom: random) > 1 ifTrue: [scalars zero] ifFalse: [samples atRandom: random]]! !

!AbstractMatrixTest methodsFor: 'private' stamp: 'len 5/22/2023 11:25:22'!
newNonemptyMatrix
	| n m |
	n := (1 to: 7) atRandom: random.
	m := (1 to: 7) atRandom: random.
	^ scalars^n ⇒ (scalars^m) atRandom: random bits: (m*n*5+1 atRandom: random)! !

!AbstractMatrixTest methodsFor: 'private' stamp: 'len 8/4/2021 14:42:41'!
newSquareMatrix
	| n |
	n _ (0 to: 5) atRandom: random.
	^ (scalars^n) endomorphisms atRandom: random bits: (n*n*5+1 atRandom: random)! !

!AbstractCommutativeRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:14:51'!
testAdjugateCramerRule
	| A I |
	A _ scalars^3 :: endomorphisms atRandom: random bits: 10.
	I _ A identity.
	self assert: A * A adjugate = (I * A determinant)! !

!AbstractCommutativeRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 13:03:14'!
testCayleyHamiltonTheorem
	| A |
	A _ (scalars^(6 atRandom: random)) endomorphisms atRandom: random bits: 100.
	self assert: (A characteristicPolynomial value: A) isZero! !

!AbstractCommutativeRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 12:58:42'!
testCharacteristicMatrixAndPolynomial
	| A |
	A _ self newSquareMatrix.
	self assert: A characteristicPolynomial = A characteristicMatrix determinant! !

!AbstractCommutativeRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 13:04:31'!
testCharacteristicPolynomial
	| A p B |
	A _ self newSquareMatrix.
	p _ A characteristicPolynomial.
	self assert: (p value: A) isZero.
	self assert: p degree = A width.
	self assert: p isMonic.
	self assert: A transpose characteristicPolynomial = p.
	B _ A parent atRandom: random bits: 100.
	self assert: (A*B) characteristicPolynomial = (B*A) characteristicPolynomial! !

!AbstractCommutativeRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:17:39'!
testDeterminant
	| n V I A B c |
	n := (0 to: 4) atRandom: random.
	V := scalars^n.
	I := V id.
	self assert: I determinant = scalars one.
	A := V endomorphisms fill: [:i :j| scalars atRandom: random bits: 10].
	B := V endomorphisms fill: [:i :j| scalars atRandom: random bits: 10].
	self assert: (A * B) determinant = (A determinant * B determinant).
	c := scalars atRandom: random bits: 10.
	self assert: (A * c) determinant = (c^n * A determinant).
	self assert: A transpose determinant = A determinant! !

!CyclotomicIntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:11:32'!
setUp
	super setUp.
	scalars := (CyclotomicField new: (3 atRandom: random) + 2) integers! !

!QuadraticRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:23:23'!
setUp
	| n |
	super setUp.
	[(n := ((2 to: 100) atRandom: random) squarefreePart) > 1] whileFalse.
	random nextBoolean ifTrue: [n := n negated].
	scalars := (QuadraticField new: n) integers! !

!AbstractStandardRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 11/7/2023 10:54:51'!
testCanonicalBases
	| basisAndTransformation A G T U |
	A := self newMatrix: (5 atRandom: random) @ (5 atRandom: random).
	basisAndTransformation := A basisAndTransformation.
	G := basisAndTransformation first.
	T := basisAndTransformation second.
"	self assert: G isStandardBasis."
	self assert: A * T = G.
	10 timesRepeat:
		[U := A domain automorphisms atRandom: random.
		self assert: (A * U) basis = G]! !

!AbstractStandardRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/17/2023 13:04:55'!
testHasCanonicalBases
	self assert: scalars hasCanonicalBases! !

!AbstractEuclideanRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/17/2023 12:54:28'!
testEchelonization
	| A E T U |
	A := self newMatrix: (6 atRandom: random) @ (6 atRandom: random).
	T := A echelonTransformation.
	E := A echelon.
	self assert: E isEchelon.
"	self assert: E isReducedEchelon."
	self assert: T * A = E.
	U := A codomain automorphisms atRandom: random.
	self assert: (U * A) echelon = E.
	T := A coechelonTransformation.
	E := A coechelon.
	self assert: E transpose isEchelon.
"	self assert: E transpose isReducedEchelon."
	self assert: A * T = E.
	U := A domain automorphisms atRandom: random.
	self assert: (A * U) coechelon = E! !

!AbstractEuclideanRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 7/10/2023 20:05:03'!
testEchelonizationUniqueness
	| A U |
	10 timesRepeat:
		[A := self newMatrix: (3 atRandom: random) @ (3 atRandom: random).
		20 timesRepeat:
			[U := A codomain automorphisms atRandom: random.
			self assert: A echelon = (U * A) echelon]]! !

!AbstractEuclideanRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2023 14:54:56'!
testSmithNormalForm
	| A S |
	A := self newMatrix: (6 atRandom: random) @ (6 atRandom: random).
	S := A SNF.
	self assert: S isSmithNormalForm! !

!AbstractEuclideanRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2023 14:54:30'!
testSmithNormalFormAndTransformations
	| A SPQ S P Q |
	A := self newMatrix: (6 atRandom: random) @ (6 atRandom: random).
	SPQ := A SNFAndTransformations.
	S := SPQ first.
	P := SPQ second.
	Q := SPQ third.
	self assert: S isSmithNormalForm.
	self assert: P isUnimodular.
	self assert: Q isUnimodular.
	self assert: P*A*Q = S! !

!AbstractEuclideanRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 7/10/2023 20:03:25'!
testSmithNormalFormUniqueness
	| A U V |
	5 timesRepeat:
		[A := self newMatrix: (3 atRandom: random) @ (3 atRandom: random).
		5 timesRepeat:
			[U := A codomain automorphisms atRandom: random.
			V := A domain automorphisms atRandom: random.
			self assert: A SNF = (U * A * V) SNF]]! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:09:14'!
testAbstractRankNullity
	| n m A |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ scalars^n ⇒ (scalars^m) atRandom: random bits: (100 atRandom: random).
	self assert: A kernel = A transpose image complement.
	self assert: A image = A transpose kernel complement! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 12/11/2022 10:12:30'!
testColumnEquivalence
	| n m A P |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ scalars^n ⇒ (scalars^m) atRandom: random bits: 10*n*m.
	P _ A domain automorphisms atRandom: random bits: 10*n*n.
	self assert: A * P ~ A! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 7/12/2021 16:52:25'!
testColumnExtended
	| A B |
	A _ self newMatrix.
	B _ A columnExtended.
	self assert: B height = A height.
	self assert: B isSquare.
	self assert: A image <= B image! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:45:50'!
testCongruence
	| n A P |
	n := 5 atRandom: random.
	"at the moment we can test congruence of invertible matrices only:"
	A := (scalars^n) automorphisms "endomorphisms" atRandom: random bits: 10*n*n.
	P := A domain automorphisms atRandom: random bits: 10*n*n.
	self assert: P transpose * A * P ≅ A! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 12/11/2022 10:11:23'!
testEquivalence
	| n m A Q P |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ scalars^n ⇒ (scalars^m) atRandom: random bits: 10*n*m.
	P _ A domain automorphisms atRandom: random bits: 10*n*n.
	Q _ A codomain automorphisms atRandom: random bits: 10*m*m.
	self assert: Q * A * P ≡ A! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/27/2021 12:08:32'!
testFundamentalSubspacesDimensions
	| m n A r |
	m _ 6 atRandom: random.
	n _ 6 atRandom: random.
	A _ self newMatrix: m@n.
	r _ A rank.
	self assert: A image rank = r.
	self assert: A kernel rank = (n - r).
	self assert: A nullity = (n - r). "definition"
	self assert: A coimage rank = r.
	self assert: A cokernel rank = (m - r)! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/26/2021 12:45:46'!
testKernelImageTheorem
	| n A |
	n _ 6 atRandom: random.
	A _ self newMatrix: n@n.
	self assert: A kernel rank + A image rank = n! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 12:49:12'!
testMinimalPolynomial
	| A f |
	A _ self newSquareMatrix.
	f _ A minimalPolynomial.
	self assert: f | A characteristicPolynomial.
	self assert: (f value: A) isZero! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/26/2021 12:46:05'!
testRankNullityTheorem
	| n A |
	n _ 6 atRandom: random.
	A _ self newMatrix: n@n.
	self assert: A rank + A nullity = n! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 7/12/2021 16:52:32'!
testRowExtended
	| A B |
	A _ self newMatrix.
	B _ A rowExtended.
	self assert: B = A transpose columnExtended transpose! !

!AbstractFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 12/11/2022 09:57:52'!
testSimilarity
	| n A P |
	n _ 5 atRandom: random.
	A _ (scalars^n) endomorphisms atRandom: random bits: 10*n*n.
	P _ A domain automorphisms atRandom: random bits: 10*n*n.
	self assert: P inverse * A * P ≃ A! !

!BinaryExtensionFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:06:45'!
setUp
	super setUp.
	scalars := GaloisField new: 2 to: (8 atRandom: random) + 1! !

!BinaryMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:06:57'!
setUp
	super setUp.
	scalars := ℤ/2! !

!CyclotomicFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:07:17'!
setUp
	super setUp.
	scalars := CyclotomicField new: (3 atRandom: random) + 2! !

!FiniteFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:07:54'!
setUp
	super setUp.
	scalars := GaloisField new: ((#(16rFF 16rFFFF 16rFFFFFFFF) atRandom: random) atRandom: random) nextPrime to: (10 atRandom: random)! !

!FiniteFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 11:36:26'!
testCharacteristicPolynomialBug
	"Using the Berkowitz algorithm this test fails."
	| A |
	A _ (ℤ/2^3) endomorphisms !! #(1 1 1 1 0 0 1 1 1).
	self assert: (A characteristicPolynomial value: A) isZero! !

!FiniteFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:09:19'!
testEchelonExample
	"From Magma Handbook example H26E10."
	| K w A |
	K _ GaloisField new: 8.
	w _ K x.
	A _ K^3 ⇒ (K^4) !! {1. w. w^5. 0. w^3. w^4. w. 1. w^6. w^3. 1. w^4}.
	self assert: A echelon = (A parent !! {1. 0. 0. 0. 1. 0. 0. 0. 1. 0. 0. 0})! !

!GaussianFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 11/5/2023 21:31:15'!
setUp
	super setUp.
	scalars := CyclotomicField new: 4! !

!QuadraticFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:23:16'!
setUp
	| n |
	super setUp.
	[(n := ((2 to: 100) atRandom: random) squarefreePart) > 1] whileFalse.
	random nextBoolean ifTrue: [n := n negated].
	scalars := QuadraticField new: n! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:09:07'!
setUp
	super setUp.
	scalars := ℚ! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:06:20'!
testAdjugateExampleProperties
	| A B I c |
	A _ ℚ !!!!!! { (-3,2,-5). (-1,0,-2). (3,-4,1) }.
	B _ ℚ !!!!!! { (-1,2,3). (1,-2,1). (2,2,-1) }.
	c _ 7.
	I _ (ℚ^3) id.
	self assert: I adjugate = I.
	self assert: (A * B) adjugate = (B adjugate * A adjugate).
	self assert: (A*c) adjugate = (A adjugate * (c^2)).
	self assert: (A^5) adjugate = (A adjugate ^ 5).
	self assert: A transpose adjugate = A adjugate transpose! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:23:15'!
testBackslashExample
	"Example from Sage documentation. Non-singular and square."
	| A B X |
	A _ (ℚ^3) endomorphisms !! #(1 2 4 5 3 1 1 2 -1).
	B _ ℚ^2 ⇒ (ℚ^3) !! #(1 5 1 2 1 5).
	X _ A \ B.
	self assert: X = (ℚ^2 ⇒ (ℚ^3) !! {-1/7. -11/7. 4/7. 23/7. 0. 0}).
	self assert: A * X = B! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/29/2023 16:49:14'!
testBackslashExampleNoSolution
	"Example from Sage documentation."
	| A v |
	A := ℚ^3 ⇒ (ℚ^2) !! #(1 2 3 2 4 6).
	v := ℚ^2 !! #(1 1).
	self should: [A \ v] raise: DivisibilityError! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:09:40'!
testBackslashExampleNonSquare
	"Example from Sage documentation."
	| A B X |
	A _ ℚ^4 ⇒ (ℚ^2) !! #(0 -1 1 0 -2 2 1 0).
	B _ (ℚ^2) endomorphisms !! #(1 0 1 -1).
	X _ A \ B.
	self assert: X = (ℚ^2 ⇒ (ℚ^4) !! {-3/2. 1/2. -1. 0. 0. 0. 0. 0}).
	self assert: A * X = B! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:09:45'!
testBackslashExampleNonSquare2
	| A B X |
	A _ ℚ^4 ⇒ (ℚ^2) !! #(0 0 -1 1 0 -2 2 1).
	B _ (ℚ^2) endomorphisms !! #(1 0 1 -1).
	X _ A \ B.
	self assert: A * X = B! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:09:56'!
testBackslashExampleSingular
	"Example from Sage documentation."
	| A B X |
	A _ ℚ^3 ⇒ (ℚ^2) !! #(1 2 3 2 4 6).
	B _ ℚ^2 ⇒ (ℚ^2) !! #(6 -6 12 -12).
	X _ A \ B.
	self assert: X = (ℚ^2 ⇒ (ℚ^3) !! #(6 -6 0 0 0 0)).
	self assert: A * X = B! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:28:32'!
testBackslashRationalTuple
	"Example from Sage documentation."
	| A v x |
	A _ (ℚ^3) endomorphisms !! #(1 2 3 -1 2 5 2 3 1).
	v _ ℚ^3 !! #(1 2 3).
	x _ A \ v.
	self assert: x = (-13/12, (23/12), (-7/12)).
	self assert: A * x = v! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 18:30:48'!
testBackslashRationalTuple2
	"Example from GAP manual."
	| A b x |
	A := ℚ !!!!!! #((1 2 7) (4 5 6) (7 8 9) (10 11 19) (5 7 12)).
	b := ℚ !!!! #(95 115 182).
	self flag: #fixme.
	"in GAP manual they use this instead, but both are correct solutions: x := ℚ !!!! {47/4. -17/2. 67/4. 0. 0}."
	x := ℚ !!!! #(62 92 0 0 -67).
	self assert: A transpose \ b = x! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:10:01'!
testBackslashRationalTupleNonSquare
	"Example from Sage documentation."
	| A v x |
	A _ ℚ^3 ⇒ (ℚ^2) !! #(1 2 3 2 4 6).
	v _ (-1/2, -1).
	x _ (-1/2, 0, 0).
	self assert: A \ v = x! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:06:39'!
testCauchyDeterminantProductFormulaExample
	| A B |
	A _ ℚ !!!!!! {(1,1,1). (3,5,7). (9,25,49)}.
	B _ ℚ !!!!!! {(1,0,4). (1,1,1). (2,-2,-3)}.
	self assert: A determinant * B determinant = (A * B) determinant! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:06:50'!
testCokernelMatrixRational
	"From Sage."
	| A B |
	A _ ℚ !!!!!! {(1,2,4,-7,4). (1,1,0,2,-1). (1,0,3,-3,1). (0,-1,-1,3,-2). (0,0,-1,2,-1)}.
	B _ ℚ !!!!!! {(1,0,-1,2,-1). (0,1,-1,1,-4)}.
	self assert: A cosyzygies echelon = B "we have to perform an additional echelonization"! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:06:55'!
testDeterminantExample
	| m |
	m _ ℚ !!!!!! { (2,1). (-1,0) }.
	self assert: m determinant = 1! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:07:00'!
testDeterminantExample2
	| vandermonde |
	vandermonde _ ℚ !!!!!! {(1,1,1). (3,5,7). (9,25,49)}.
	self assert: vandermonde determinant = 16! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2022 11:45:21'!
testInverseBug
	| X |
	X _ (ℚ^2) endomorphisms !! #(1 2 0 0).
	self should: [X inverse] raise: Error! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:07:07'!
testInverseExample
	| A I |
	A _ ℚ !!!!!! { (2,1,1). (-1,0,1). (0,2,1) }.
	I _ (ℚ^3) id.
	self assert: A inverse * A = I.
	self assert: A * A inverse = I! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:16:50'!
testInverseExample2
	| Qx x f A |
	Qx _ ℚ polynomials.
	x _ Qx x.
	f _ x^8 + (x^6) - (3*(x^4)) - (3*(x^3)) + (8*(x^2)) + (2*x) - 5.
	A _ f companion.
	self assert: (A * A inverse) isIdentity! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:07:12'!
testJordanNormalFormExample
	| A jordan B |
"	<timeout: 2>"
	A _ ℚ !!!!!! { (5,4,2,1). (0,1,-1,-1). (-1,-1,3,0). (1,1,-1,2) }.
	jordan _ A jordan.
	self assert: jordan size = 3.
	B _ (ℚ^1) id.
	self assert: (jordan includes: B).
	B _ (ℚ^1) id * 2.
	self assert: (jordan includes: B).
	B _ (ℚ^2) id * 4 :: at: 1@2 put: 1; yourself.
	self assert: (jordan includes: B)! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:48:55'!
testKernelExample
	| A K |
	A := ℚ !!!!!! {(1, 0, -3, 0, 2, -8). (0, 1, 5, 0, -1, 4). (0, 0, 0, 1, 7, -9). (0, 0, 0, 0, 0, 0)}.
	K := ℚ^6 sub: {(3,-5,1,0,0,0). (-2,1,0,-7,1,0). (8,-4,0,9,0,1)}.
	self assert: A kernel = K! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:49:01'!
testKernelExample2
	"Example from GAP manual."
	| A K |
	A := ℚ !!!!!! #((1 2 7) (4 5 6) (7 8 9) (10 11 19) (5 7 12)).
	A := A transpose.
	K := ℚ^5 sub: {{-7/4. 9/2. -15/4. 1. 0}. {-3/4. -3/2. 1/4. 0. 1}}.
	self assert: A kernel = K! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:10:06'!
testLeftInverseRational
	| A |
	[A _ ℚ^3 ⇒ (ℚ^5) atRandom: random bits: 1000. A rank = 3] whileFalse.
	self assert: (A retraction * A) isIdentity! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:22:53'!
testRCFAndInvariantFactorsExample
	"Example from Magma documentation."
	| K A R x |
	K _ ℤ/5.
	A _ (K^5) endomorphisms !! #(0 2 4 2 0 2 2 2 3 3 3 4 4 1 3 0 0 0 0 1 0 0 0 1 0).
	R _ A parent !! #(1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0).
	self assert: A RCF = R.
	x _ K polynomials x.
	self assert: A invariantFactors = {x - 1. x^2 - 1. x^2 - 1}
	"TODO: test transformation T such that T*A*T inverse = R"! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:07:30'!
testRCFExample
	| A |
	A _ ℚ !!!!!! #((-2 0 0) (-1 -4 -1) (2 4 0)).
	self assert: A RCF = (ℚ !!!!!! #((-2 0 0) (0 0 -4) (0 1 -4)))! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:07:37'!
testRCFExample2
	"Example from wikipedia:"
	| A C |
	A _ ℚ !!!!!! #(
		(-1 3 -1 0 -2 0 0 -2)
		(-1 -1 1 1 -2 -1 0 -1)
		(-2 -6 4 3 -8 -4 -2 1)
		(-1 8 -3 -1 5 2 3 -3)
		(0 0 0 0 0 0 0 1)
		(0 0 0 0 -1 0 0 0)
		(1 0 0 0 2 0 0 0)
		(0 0 0 0 4 0 1 0)).
	C _ ℚ !!!!!! #(
		(0 1 0 0 0 0 0 0)
		(1 1 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 -1)
		(0 0 1 0 0 0 0 -4)
		(0 0 0 1 0 0 0 -4)
		(0 0 0 0 1 0 0 2)
		(0 0 0 0 0 1 0 4)
		(0 0 0 0 0 0 1 0)).
	self assert: A RCF = C! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:07:44'!
testRankExample
	| vandermonde |
	vandermonde _ ℚ !!!!!! {(1,1,1). (3,5,7). (9,25,49)}.
	self assert: vandermonde rank = 3.
	vandermonde _ ℤ !!!!!! {(1,1,1). (3,5,7). (9,25,49)}.
	self assert: vandermonde rank = 3! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:07:48'!
testRankExample2
	| m |
	m _ ℚ !!!!!! {(1,1,1). (2,2,0). (3,3,1)}.
	self assert: m rank = 2! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:10:11'!
testRightInverseRational
	| A |
	[A _ ℚ^5 ⇒ (ℚ^3) atRandom: random bits: 1000. A rank = 3] whileFalse.
	self assert: (A * A section) isIdentity! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:07:58'!
testSimilarityExample
	"Examples from wikipedia article on SNF"
	| A B C |
	A _ ℚ !!!!!! {(1,2). (0,1)}.
	B _ ℚ !!!!!! {(3,-4). (1,-1)}.
	C _ ℚ !!!!!! {(1,0). (1,2)}.
	self assert: A ≃ B.
	self deny: A ≃ C! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:10:16'!
testSlashWithMatrixExample
	| A B X  |
	A _ (ℚ^3) endomorphisms !! #(1 1 3 2 0 4 -1 6 -1).
	B _ ℚ^3 ⇒ (ℚ^1) !! #(2 19 8).
	X _ B / A.
	self assert: X = (B parent !! #(1 2 3)).
	self assert: X * A = B! !

!RationalMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:25:07'!
testSlashWithTupleExample
	| A b x  |
	A _ (ℚ^3) endomorphisms !! #(1 1 3 2 0 4 -1 6 -1).
	b _ ℚ^3 !! #(2 19 8).
	x _ b / A.
	self assert: x = (ℚ^3 !! #(1 2 3)).
	self assert: x * A = b! !

!SmallFiniteFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 01:00:03'!
setUp
	| p |
	super setUp.
	p := (13 atRandom: random) nextPrime.
	scalars := GaloisField new: p to: (((256 log: p) floor + 1) atRandom: random) + 1! !

!TernaryExtensionFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:10:32'!
setUp
	super setUp.
	scalars := GaloisField new: 3 to: (8 atRandom: random) + 1! !

!TernaryMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:10:56'!
setUp
	super setUp.
	scalars := ℤ/3! !

!EisensteinIntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:11:46'!
setUp
	super setUp.
	scalars := (CyclotomicField new: 3) integers! !

!GaussianIntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 11/5/2023 21:31:24'!
setUp
	super setUp.
	scalars := (CyclotomicField new: 4) integers! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:12:12'!
setUp
	super setUp.
	scalars := ℤ! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:03:53'!
testAdjugateExample
	| A I |
	A _ Matrix rows: { (2,1,1). (-1,0,1). (0,2,1) }.
	I _ A identity.
	self assert: A * A adjugate = (I * A determinant)! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:04:04'!
testAdjugateExample2
	| A B |
	A _ Matrix rows: { (-3,2,-5). (-1,0,-2). (3,-4,1) }.
	B _ Matrix rows: { (-8,18,-4). (-5,12,-1). (4,-6,2) }.
	self assert: A adjugate = B! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:10:21'!
testBackslashIntegerTuple
	"Example from Sage documentation."
	| A v x |
	A _ ℤ^3 ⇒ (ℤ^2) !! #(1 2 3 2 4 6).
	v _ (-1, -2).
	x _ A \ v.
"	self assert: x = (-1, 0, 0)."
	self assert: A * x = v! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:03'!
testBackslashIntegerTuple2
	"Example from GAP manual."
	| A b |
	A _ ℤ !!!!!! #((1 2 7) (4 5 6) (7 8 9) (10 11 19) (5 7 12)).
	A _ A transpose.
	b _ ℤ !!!! #(95 115 182).
"	x _ ℤ tuple: #(2285 -5854 4888 -1299 0)." "<- GAP comes up with this solution, but this is not very small"
	self assert: A * (A \ b) = b! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:08'!
testBackslashIntegerTuple3
	| A b |
	A _ ℤ !!!!!! #((-8 1 -7 -9 -2 -1) (5 -2 3 -3 1 1) (7 0 6 4 -5 -8) (-7 -10 5 9 -4 4) (3 -4 1 -2 3 -8) (-7 3 2 6 7 -1) (4 8 5 1 -8 -9) (9 5 0 -10 -8 8) (-6 2 -6 -9 -5 6)).
	A _ A transpose.
	b _ ℤ !!!! #(3 -1 -1 -7 9 8).
	"x _ ℤ !!!! #(38 25 -6 1 -34 -12 3 -3 -44)." "shortest solution"
	self assert: A * (A \ b) = b! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:29:40'!
testBackslashIntegerTupleBug
	| A b x |
	A _ (ℤ^3) endomorphisms !! #(1 0 0 0 1 0 0 0 10).
	b _ (0, 0, 10).
	x _ A \ b.
	self assert: A * x = b! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:30:02'!
testBackslashPascal3
	"Example from Matlab documentation. Non-singular and square."
	| A u x |
	A _ (ℤ^3) endomorphisms !! #(1 1 1 1 2 3 1 3 6). "pascal(3)"
	u _ (3, 1, 4).
	x _ A \ u.
	self assert: x = (10, -12, 5).
	self assert: A * x = u! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:30:06'!
testBackslashPascal3Magic3
	"Example from Matlab documentation. Non-singular and square."
	| A B X |
	A _ (ℤ^3) endomorphisms !! #(1 1 1 1 2 3 1 3 6). "pascal(3)"
	B _ (ℤ^3) endomorphisms !! #(8 1 6 3 5 7 4 9 2). "magic(3)"
	X _ A \ B.
	self assert: X = ((ℤ^3) endomorphisms !! #(19 -3 -1 -17 4 13 6 0 -6)).
	self assert: A * X = B! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:14'!
testCauchyDeterminantProductFormulaExample
	| A B |
	A _ ℤ !!!!!! {(1,1,1). (3,5,7). (9,25,49)}.
	B _ ℤ !!!!!! {(1,0,4). (1,1,1). (2,-2,-3)}.
	self assert: A determinant * B determinant = (A * B) determinant! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:24'!
testCharacteristicPolynomialExample1x1
	| A x |
	A _ ℤ !!!!!! #((42)).
	x _ ℤ polynomials x.
	self assert: A characteristicPolynomial = (x - 42)! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:28'!
testCharacteristicPolynomialExample2x2
	| A x |
	A _ ℤ !!!!!! #((1 2) (3 4)).
	x _ ℤ polynomials x.
	self assert: A characteristicPolynomial = (x^2 - (x*5) - 2)! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:32'!
testCharacteristicPolynomialExample3x3
	| A x |
	A _ ℤ !!!!!! #((1 2 3) (4 5 6) (7 8 9)).
	x _ ℤ polynomials x.
	self assert: A characteristicPolynomial = (x^3 - (x^2*15) - (x*18))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:36'!
testCharacteristicPolynomialExample4x4
	| A x |
	A _ ℤ !!!!!! #((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)).
	x _ ℤ polynomials x.
	self assert: A characteristicPolynomial = (x^4 - (x^3*34) - (x^2*80))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:43'!
testCokernelMatrixInteger
	"From Sage."
	| A B |
	A _ ℤ !!!!!! {(1,2,4,-7,4). (1,1,0,2,-1). (1,0,3,-3,1). (0,-1,-1,3,-2). (0,0,-1,2,-1)}.
	B _ ℤ !!!!!! {(1,0,-1,2,-1). (0,1,-1,1,-4)}.
	self assert: A cosyzygies echelon = B! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:47'!
testDeterminantExample
	| m |
	m _ ℤ !!!!!! { (2,1). (-1,0) }.
	self assert: m determinant = 1! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:51'!
testDeterminantExample2
	| vandermonde |
	vandermonde _ ℤ !!!!!! {(1,1,1). (3,5,7). (9,25,49)}.
	self assert: vandermonde determinant = 16! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/29/2022 19:39:19'!
testDomainError
	| A B |
	A _ (ℤ^4) id.
	self should: [A / 2] raise: DivisibilityError.
	B _ (ℚ^4) id.
	self should: [A + B] raise: Error.
	self should: [A * B] raise: Error! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:08:57'!
testHermiteNormalFormBug
	"Bug."
	| A H |
	A _ ℤ !!!!!! #((1 2 3 0) (1 2 3 1)).
	H _ ℤ !!!!!! #((1 2 3 0) (0 0 0 1)).
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:03'!
testHermiteNormalFormExample
	"Example from wikipedia."
	| A H |
	A _ ℤ !!!!!! { (3,3,1,4). (0,1,0,0). (0,0,19,16). (0,0,0,3) }.
	H _ ℤ !!!!!! { (3,0,1,1). (0,1,0,0). (0,0,19,1). (0,0,0,3) }.
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:08'!
testHermiteNormalFormExample2
	| A H |
	A _ ℤ !!!!!! { (9,-36,30). (-36,192,-180). (30,-180,180) }.
	H _ ℤ !!!!!! { (3,0,30). (0,12,0). (0,0,60) }.
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:15'!
testHermiteNormalFormExample3
	"Example from wikipedia"
	| A H |
	A _ ℤ !!!!!! { (2,3,6,2). (5,6,1,6). (8,3,1,1) }.
	H _ ℤ !!!!!! { (1,0,50,-11). (0,3,28,-2). (0,0,61,-13) }.
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:20'!
testHermiteNormalFormExample4
	"Example from Sage"
	| A B |
	A _ ℤ !!!!!! {(1,2,3,4). (5,6,7,8). (9,10,11,12)}.
	B _ ℤ !!!!!! {(1,2,3,4). (0,4,8,12). (0,0,0,0)}.
	self assert: A HNF = B.
	self assert: A echelon = B "the echelon form of integers matrices is the hermite normal form"! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:25'!
testHermiteNormalFormExample5
	"From Magma Handbook example H26E10."
	| A H |
	A _ ℤ !!!!!! #((2 -4 12 7 0) (3 -3 5 -1 4) (2 -1 -4 -5 -12) (0 3 6 -2 0)).
	H _ ℤ !!!!!! #((1 1 1 6 -164) (0 3 0 16 -348) (0 0 2 13 -200) (0 0 0 19 -316)).
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:35'!
testHermiteNormalFormExample6
	"From MuPAD documentation."
	| A H |
	A _ ℤ !!!!!! #(
		(25 -300 1050 -1400 630)
		(-300 4800 -18900 26880 -12600)
		(1050 -18900 79380 -117600 56700)
		(-1400 26880 -117600 179200 -88200)
		(630 -12600 56700 -88200 44100)).
	H _ ℤ !!!!!! #(
		(5 0 210 560 630)
		(0 60 0 0 0)
		(0 0 420 0 0)
		(0 0 0 840 0)
		(0 0 0 0 2520)).
	"The original from MuPAD documentation is:
	H _ ℤ !!!!!! #(
		(5 0 -210 -280 630)
		(0 60 0 0 0)
		(0 0 420 0 0)
		(0 0 0 840 0)
		(0 0 0 0 2520)).
	but this is wrong, this is a bug in MuPAD"
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:41'!
testHermiteNormalFormExample7
	"Example from GAP manual."
	| A H |
	A _ ℤ !!!!!! #((1 15 28) (4 5 6) (7 8 9)).
	H _ ℤ !!!!!! #((1 0 1) (0 1 1) (0 0 3)).
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:47'!
testHermiteNormalFormExample8
	"Example from Pernet-Stein 'Fast computation of Hermite normal forms of random integer matrices'."
	| A H |
	A _ ℤ !!!!!! #((-5 8 -3 -9 5 5) (-2 8 -2 -2 8 5) (7 -5 -8 4 3 -4) (1 -1 6 0 8 -3)).
	H _ ℤ !!!!!! #((1 0 3 237 -299 90) (0 1 1 103 -130 40) (0 0 4 352 -450 135) (0 0 0 486 -627 188)).
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:52'!
testHermiteNormalFormExample9
	"Example from Pernet-Stein 'Fast computation of Hermite normal forms of random integer matrices'."
	| A H |
	A _ ℤ !!!!!! #((-1 2 5 65 2) (4 -1 -3 1 -2) (-1 -2 1 -1 1)).
	H _ ℤ !!!!!! #((1 0 17 259 7) (0 1 31 453 13) (0 0 40 582 17)).
	self assert: A HNF = H! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:09:59'!
testIsHermiteNormalForm
	"Example from wikipedia."
	| A H |
	A _ ℤ !!!!!! { (3,3,1,4). (0,1,0,0). (0,0,19,16). (0,0,0,3) }.
	H _ ℤ !!!!!! { (3,0,1,1). (0,1,0,0). (0,0,19,1). (0,0,0,3) }.
	self assert: A isHermiteNormalForm not.
	self assert: H isHermiteNormalForm! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:08'!
testIsHermiteNormalForm2
	| M |
	M _ ℤ !!!!!! {(2,0,1,11). (0,1,5,12). (0,0,6,12). (0,0,0,33)}.
	self assert: M isHermiteNormalForm.
	self assert: M transpose isHermiteNormalForm not.
	M _ ℤ !!!!!! {(2,0,1,11). (0,1,5,12). (0,0,6,12). (0,0,0,33). (0,0,0,0)}.
	self assert: M isHermiteNormalForm.
	M _ ℤ !!!!!! {(2,0,1,11). (0,1,5,12). (0,0,6,12). (0,0,0,33). (0,0,0,1)}.
	self assert: M isHermiteNormalForm not! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 7/2/2023 18:22:47'!
testKernelExample
	"Example from GAP manual."
	| A K |
	A := ℤ !!!!!! #((1 2 7) (4 5 6) (7 8 9) (10 11 19) (5 7 12)).
	A := A transpose. "because in GAP matrices act on rows"
	K := ℤ !!!!!! #((1 18 -9 2 -6) (0 24 -13 3 -7)).
	K := K transpose.
	self assert: (A kernel → A domain) asMatrix = K! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:21'!
testKroneckerProductExample
	| A B C |
	A _ ℤ !!!!!! #((1 2) (3 4)).
	B _ ℤ !!!!!! #((0 5) (6 7)).
	C _ ℤ !!!!!! #((0 5 0 10) (6 7 12 14) (0 15 0 20) (18 21 24 28)).
	self assert: A ⊗ B = C! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:25'!
testMinorExample
	| A |
	A _ ℤ !!!!!! #((1 2 3) (4 5 6) (7 8 9)).
	self assert: (A minor: 2@3) = -6! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:10:25'!
testMinorsExample
	| A |
	A _ ℤ^3 ⇒ (ℤ^2) !! (1 to: 6).
	self assert: (A minors: 2) asArray = #(-3 -6 -3)! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:21:22'!
testPermanentExample
	"Example from https://en.wikipedia.org/wiki/Permanent_(mathematics)"
	| A |
	A _ (ℤ^4) endomorphisms !! #(1 1 1 1 2 1 0 0 3 0 1 0 4 0 0 1).
	self assert: A permanent = 10! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:30'!
testPivots
	| A |
	A _ ℤ !!!!!! {(0,-1,-1). (0,-20,1). (0,1,2)}.
	self assert: A echelon pivots asArray = {1@2. 2@3}! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:21:33'!
testPositiveDefiniteExample
	| M |
	M _ (ℤ^3) endomorphisms !! #(3 -1 1 -1 3 1 1 1 3).
	self assert: M isPositiveDefinite! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:34'!
testRankExample
	| m |
	m _ ℤ !!!!!! {(1,1,1). (2,2,2). (5,5,5)}.
	self assert: m rank = 1! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:38'!
testSmithNormalFormExample1
	| A |
	A _ ℤ !!!!!! { (2,4,4). (-6,6,12). (10,-4,-16) }.
	self assert: A SNF = ((ℤ^3) endomorphisms diagonal: #(2  6 12))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:42'!
testSmithNormalFormExample2
	| A |
	"Reduced Laplacian matrix of K_4:"
	A _ ℤ !!!!!! { (3,-1,-1). (-1,3,-1). (-1,-1,3) }.
	self assert: A SNF = ((ℤ^3) endomorphisms diagonal: #(1  4  4))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:46'!
testSmithNormalFormExample3
	"Example from Willian Stein, 'Algebraic Number Theory, A Computational Approach'."
	| A |
	A _ ℤ !!!!!! { (1,4,9). (16,25,36). (49,64,81) }.
	self assert: A SNF = ((ℤ^3) endomorphisms diagonal: #(1 3 72))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:50'!
testSmithNormalFormExample4
	"Smith form of a 3x3 matrix of rank 2.
	Example from Willian Stein, 'Algebraic Number Theory, A Computational Approach'."
	| A |
	A _ ℤ !!!!!! #((2 3 4) (5 6 7) (8 9 10)).
	self assert: A SNF = ((ℤ^3) endomorphisms diagonal: #(1 3 0)) "this fails with SNF because Cohen defines SNF only for nonsingular matrices, so use SNF2"! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:10:56'!
testSmithNormalFormExample5
	"Example from youtube video: 'Derek Holt: Algorithms for finitely presented groups' @ 25:49."
	| A |
	A _ ℤ !!!!!! #((1 2 0 -2) (0 2 2 0) (-1 -4 4 2)).
	self assert: A SNF = (ℤ !!!!!! #((1 0 0 0) (0 2 0 0) (0 0 6 0)))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:11:01'!
testSmithNormalFormExample6
	"From Magma Handbook example H26E10."
	| A |
	A _ ℤ !!!!!! #((2 -4 12 7 0) (3 -3 5 -1 4) (2 -1 -4 -5 -12) (0 3 6 -2 0)).
	self assert: A elementaryDivisors = #(1 1 1 2)! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:11:08'!
testSmithNormalFormExample7
	"From GAP manual."
	| A |
	A _ ℤ !!!!!! #((1 15 28) (4 5 6) (7 8 9)).
	self assert: A SNF = ((ℤ^3) endomorphisms diagonal: #(1 1 3))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:11:12'!
testSmithNormalFormExample8
	| A |
	A _ ℤ !!!!!! #((2 1 -3) (3 -2 -1) (1 -3 2)).
	self assert: A SNF = ((ℤ^3) endomorphisms diagonal: #(1 7 0))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:11:18'!
testSmithNormalFormExample9
	| A |
	A _ ℤ !!!!!! #((1 2 3 4 5 6 7) (1 0 1 0 1 0 1) (2 4 5 6 1 1 1) (1 4 2 5 2 0 0) (0 0 1 1 2 2 3)).
	self assert: A SNF = (ℤ !!!!!! #((1 0 0 0 0 0 0) (0 1 0 0 0 0 0) (0 0 1 0 0 0 0) (0 0 0 1 0 0 0) (0 0 0 0 2 0 0)))! !

!IntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:11:24'!
testSmithNormalFormExampleDiagonal
	| A |
	A _ ℤ !!!!!! #((2 0 0) (0 1 0) (0 0 0)).
	self assert: A SNF = ((ℤ^3) endomorphisms diagonal: #(1 2 0)).
	A _ ℤ !!!!!! #((5 0 0) (0 2 0) (0 0 0)).
	self assert: A SNF = ((ℤ^3) endomorphisms diagonal: #(1 10 0))! !

!PrimeFieldPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:12:59'!
setUp
	super setUp.
	scalars := ℤ/5! !

!PrimeFieldPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:16:20'!
testRankOverPolynomialsOverZ5
	"Example form Magma handbook (H88E7)."
	| K P x a |
	K _ ℤ/5.
	P _ K polynomials.
	x _ P x.
	a _ (P^5) endomorphisms fill: [:i :j| x^i + (x^j)].
	self assert: a rank = 2! !

!PrimeFieldPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:16:36'!
testSmithNormalFormOverPolynomialsOverPrimeField1
	"Example form Magma handbook (H88E7)."
	| K P x a |
	K _ ℤ/5.
	P _ K polynomials.
	x _ P x.
	a _ (P^5) endomorphisms fill: [:i :j| x^i + (x^j)].
	self assert: a elementaryDivisors = {x. x^3 + (x^2*3) + x}! !

!PrimeFieldPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:05:31'!
testSmithNormalFormOverPolynomialsOverPrimeField2
	"Example form Magma handbook (H88E7)."
	| K P x a ax |
	K _ ℤ/5.
	P _ K polynomials.
	x _ P x.
	a _ (ℤ^5) endomorphisms !! #(0 2 4 2 0 2 2 2 3 3 3 4 4 1 3 0 0 0 0 1 0 0 0 1 0).
	a _ a over: P.
	ax _ a identity * x - a.
	self assert: ax SNF = ((P^5) endomorphisms diagonal: {P one. P one. x - 1. x^2 - 1. x^2 - 1}).
	self assert: ax elementaryDivisors = {P one. P one. x - 1. x^2 - 1. x^2 - 1}! !

!RationalPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:13:57'!
setUp
	super setUp.
	scalars := ℚ polynomials! !

!RationalPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:15:39'!
testHermiteNormalFormExample
	"From MuPAD documentation."
	| R x A H |
	R _ ℚ polynomials.
	x _ R x.
	A _ (R^2) endomorphisms !! {x^2 + 3. (x*2 - 1)^2. (x + 2)^2. x^2*3 + 5}.
	H _ (R^2) endomorphisms !! {R one. (x^3*4 + (x^2*47) - (x*76) + 20) / 49. R zero. x^4 + (x^3*12) - (x^2*13) - (x*12) - 11}.
	self assert: A HNF = H! !

!RationalPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2023 14:49:46'!
testSmithNormalFormExample
	"Example from Sage documentation."
	| R x A |
	R := ℚ polynomials.
	x := R x.
	A := (R^2) id * x - (R !!!!!! #((3 -4) (1 -1))).
	self assert: A SNF = ((R^2) endomorphisms diagonal: {R one. x^2 - (x*2) + 1})! !

!BinaryPowerRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:54:00'!
setUp
	super setUp.
	scalars := PrimePowerRing new: 2 to: ((2 to: 64) atRandom: random)! !

!GaloisRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:14:34'!
setUp
	super setUp.
	scalars := GaloisRing new: (3 atRandom: random) nextPrime to: (2 atRandom: random) to: (2 atRandom: random)! !

!LocalIntegerRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:12:37'!
setUp
	super setUp.
	scalars := ℤ @ (ℤ * (64 atRandom: random) nextPrime)! !

!ModularIntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:15:33'!
setUp
	super setUp.
	scalars := ℤ/((2 to: 64) atRandom: random)! !

!ModularIntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:11:42'!
testHowellNormalForm1
	"Examples from 'Fast Algorithms for Linear Algebra Modulo N', Arne Storjohann, Thom Mulders."
	| R A B C D H |
	R _ ℤ/12.
	A _ R !!!!!! #((4 1 0) (0 0 5) (0 0 0)).
	B _ R !!!!!! #((8 5 5) (0 9 8) (0 0 10)).
	C _ R !!!!!! #((4 1 10) (0 0 5) (0 0 0)).
	D _ R !!!!!! #((4 1 0) (0 0 1) (0 0 0)).
	H _ R !!!!!! #((4 1 0) (0 3 0) (0 0 1)).
	self assert: A echelon = H.
	self assert: B echelon = H.
	self assert: C echelon = H.
	self assert: D echelon = H! !

!ModularIntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 21:47:55'!
testHowellNormalForm2
	| m R t u |
	100 timesRepeat:
		[[m _ (256 atRandom: random) + 1. m isPrime] whileTrue.
		R _ ℤ/m.
		t _ R^(5 atRandom: random) atRandom: random.
		u _ R units atRandom: random.
		self assert: t asRowMatrix echelon = (t*u) asRowMatrix echelon]! !

!ModularIntegerMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:11:49'!
testHowellNormalForm3
	"Example from 'Spans in the module (Z/mZ)^s', John A. Howell (1986)."
	| R A H |
	R _ ℤ/6.
	A _ R !!!!!! #((2 1)).
	H _ R !!!!!! #((2 1) (0 3)).
	self assert: A echelon = H! !

!PrimePowerRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2023 21:54:00'!
setUp
	super setUp.
	scalars := PrimePowerRing new: (10 atRandom: random) nextPrime to: (3 atRandom: random)! !

!PrimePowerRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 18:11:54'!
testSyzygiesBug
	| R A X |
	R _ ℤ/(7^3).
	A _ R !!!!!! #((15 48 278 40)).
	X _ A syzygies.
	self assert: (A*X) isZero! !

!GaloisRingMultivariatePolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:15:14'!
setUp
	super setUp.
	scalars := (GaloisRing new: (3 atRandom: random) nextPrime to: (2 atRandom: random) to: (2 atRandom: random)) polynomialsIn: (3 atRandom: random)! !

!ModularIntegerMultivariatePolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:16:01'!
setUp
	super setUp.
	scalars := ℤ/(1024 atRandom: random) polynomialsIn: (3 atRandom: random)! !

!ModularIntegerPolynomialRingPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/17/2023 15:00:13'!
setUp
	super setUp.
	scalars := ℤ/(1024 atRandom: random) :: polynomials polynomials! !

!PrimeFieldMultivariatePolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:12:49'!
setUp
	super setUp.
	scalars := ℤ/5 polynomialsIn: #(x y z)! !

!RationalMultivariatePolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:13:46'!
setUp
	super setUp.
	scalars := ℚ polynomialsIn: #(x y z)! !

!RationalPolynomialRingPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/10/2023 11:00:12'!
setUp
	super setUp.
	scalars := ℚ polynomials polynomials! !

!PrimeFieldAffineAlgebraMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:16:59'!
setUp
	| P |
	super setUp.
	P := ℤ/(64 atRandom: random) nextPrime polynomialsIn: (3 atRandom: random).
	[scalars := P / ((1 to: (4 atRandom: random) - 1) collect: [:i| P atRandom: random bits: 20]).
	scalars isTrivial] whileTrue! !

!ProductRingMatrixTest methodsFor: 'initialization' stamp: 'len 5/26/2023 00:17:47'!
setUp
	super setUp.
	scalars := ℤ × (ℤ/((64 atRandom: random) + 1))! !

!RationalAffineAlgebraMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:18:20'!
setUp
	| P |
	super setUp.
	P := ℚ polynomialsIn: (3 atRandom: random).
	[scalars := P / ((1 to: (4 atRandom: random) - 1) collect: [:i| P atRandom: random bits: 20]).
	scalars isTrivial] whileTrue! !

!CliffordAlgebraMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 15:54:34'!
setUp
	super setUp.
	self flag: #fixme. "use ℚ^3 or more"
	scalars := CliffordAlgebra on: (QuadraticForm from: ℚ^2 coefficients: #(1 0 0 1 0 1))! !

!GroupAlgebraMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:18:50'!
setUp
	super setUp.
	scalars := (SymmetricGroup new: ((0 to: 3) atRandom: random)) groupAlgebraOver: ℤ/2! !

!MatrixAlgebraMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:19:08'!
setUp
	super setUp.
	scalars := (ℚ^((0 to: 3) atRandom: random)) endomorphisms! !

!QuaternionAlgebraMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:19:19'!
setUp
	super setUp.
	scalars := QuaternionAlgebra over: ℚ invariants: #(1 1)! !

!AbstractModuleTest methodsFor: 'isomorphism theorems' stamp: 'len 10/5/2022 17:11:54'!
testFirstIsomorphismTheorem
	| M N f |
	M _ self newFreeModule.
	N _ self newFreeModule.
	f _ M ⇒ N atRandom: random bits: 100.
	self assert: f codomain >= f image.
	self assert: f domain >= f kernel.
	self assert: f domain / f kernel ~ f image! !

!AbstractModuleTest methodsFor: 'isomorphism theorems' stamp: 'len 6/26/2023 12:49:45'!
testSecondIsomorphismTheorem
	| M k S T |
	M := self newFreeModule.
	k := M rank > 0 ifTrue: [M rank atRandom: random] ifFalse: [0].
	S := M sub: ((1 to: k) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	k := M rank > 0 ifTrue: [M rank atRandom: random] ifFalse: [0].
	T := M sub: ((1 to: k) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	self assert: S+T <= M.
	self assert: S ∩ T <= M.
	self assert: (S+T)/T ~ (S/(S ∩ T))! !

!AbstractModuleTest methodsFor: 'isomorphism theorems' stamp: 'len 6/26/2023 12:50:17'!
testThirdIsomorphismTheorem
	| M S T k |
	M := self newFreeModule.
	k := M rank > 0 ifTrue: [M rank atRandom: random] ifFalse: [0].
	S := M sub: ((1 to: k) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	T := S sub: ((1 to: (k > 0 ifTrue: [k atRandom: random] ifFalse: [0])) collect: [:i| S atRandom: random bits: (10 atRandom: random)]).
	self assert: S/T <= (M/T).
	self assert: (M/T)/(S/T) ~ (M/S)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:36:58'!
testCoequalizer
	| A B f g Q |
	A _ self newFreeModule.
	B _ self newFreeModule.
	f _ (A ⇒ B) atRandom: random bits: (50 atRandom: random).
	g _ (A ⇒ B) atRandom: random bits: (50 atRandom: random).
	Q _ f coequalizer: g.
	self assert: (f → Q) notNil.
	self assert: f → Q = (g → Q)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/2/2023 18:26:00'!
testCoimage
	| A J |
	A := self newModuleMap.
	J := A domain → A coimage.
	self assert: A coimage = J image.
	self assert: J isEpimorphism! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/2/2023 18:25:26'!
testCokernel
	| A Q |
	A := self newModuleMap.
	Q := A codomain → A cokernel.
	self assert: (Q · A) isZero.
	self assert: Q isEpimorphism.
	self assert: A cokernel = Q image! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:10:55'!
testCokernelAndCoequalizer
	| A B f |
	A _ self newFreeModule.
	B _ self newFreeModule.
	f _ (A ⇒ B) atRandom: random bits: (100 atRandom: random).
	self assert: f cokernel = (f coequalizer: f zero)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/5/2023 22:10:36'!
testColift
	| X Y Z i f g |
	X := self newSmallModule.
	Y := self newSmallModule.
	Z := self newSmallModule.
	g := Y ⇒ Z atRandom: random bits: 20.
	i := X ⇒ Y atRandom: random bits: 20.
	f := g · i.
	g := f / i.
	self assert: g · i = f! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/5/2023 22:11:26'!
testColiftFree
	| X Y Z i f g |
	X := self newFreeModule.
	Y := self newFreeModule.
	Z := self newFreeModule.
	g := Y ⇒ Z atRandom: random bits: 20.
	i := X ⇒ Y atRandom: random bits: 20.
	f := g · i.
	g := f / i.
	self assert: g · i = f! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:11:25'!
testComposition
	| X Y Z f g x |
	X _ self newModule.
	Y _ self newModule.
	Z _ self newModule.
	f _ X ⇒ Y atRandom: random bits: 1000.
	g _ Y ⇒ Z atRandom: random bits: 1000.
	x _ X atRandom: random bits: 1000.
	self assert: (X ⇒ Z includes: g · f).
	self assert: (g · f value: x) = (g value: (f value: x)).
	X >= Z ifFalse: [self should: [f · g] raise: Error]! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:11:32'!
testCompositionError
	| X Y Z f g |
	X _ self newModule.
	Y _ self newModule.
	[Z _ self newModule. X >= Z] whileTrue.
	f _ X ⇒ Y atRandom: random bits: 1000.
	g _ Y ⇒ Z atRandom: random bits: 1000.
	self should: [f · g] raise: DomainError! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:37:35'!
testEqualizer
	| A B f g Q |
	A _ self newFreeModule.
	B _ self newFreeModule.
	f _ A ⇒ B atRandom: random bits: (100 atRandom: random).
	g _ A ⇒ B atRandom: random bits: (100 atRandom: random).
	Q _ f equalizer: g.
	self assert: (Q → f) notNil.
	self assert: Q → f = (Q → g)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 5/10/2022 10:48:02'!
testIdenittyImage
	| M id |
	M _ self newModule.
	id _ M id.
	self assert: id image = M.
	self assert: id isSurjective! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 07:57:09'!
testImage
	| f image v w |
	f _ self newModuleMap.
	image _ f image.
	v _ f domain atRandom: random bits: 100.
	w _ f value: v.
	self assert: (image includes: w)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/2/2023 18:25:02'!
testImageAndCoimage
	| A |
	A := self newModuleMap.
	self assert: (A codomain → A cokernel) kernel → A codomain = (A image → A codomain).
	self assert: A domain → (A kernel → A domain) cokernel = (A domain → A coimage)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/2/2023 18:17:19'!
testKernel
	| A K |
	A := self newModuleMap.
	K := A kernel → A domain.
	self assert: (A · K) isZero.
	self assert: K isMonomorphism.
	self assert: A kernel = K image! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 4/4/2021 10:23:04'!
testKernel2
	| f kernel v |
	f _ self newModuleMap.
	kernel _ f kernel.
	v _ kernel atRandom: random bits: 100.
	self assert: (f value: v) isZero! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:12:01'!
testKernelAndEqualizer
	| A B f |
	A _ self newFreeModule.
	B _ self newFreeModule.
	f _ A ⇒ B atRandom: random bits: (100 atRandom: random).
	self assert: f kernel = (f equalizer: f zero)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/5/2023 22:10:43'!
testLift
	| X Y Z p f g |
	X := self newSmallModule.
	Y := self newSmallModule.
	Z := self newSmallModule.
	g := X ⇒ Y atRandom: random bits: 20.
	p := Y ⇒ Z atRandom: random bits: 20.
	f := p · g.
	g := p \ f.
	self assert: p · g = f! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/5/2023 22:11:11'!
testLiftFree
	| X Y Z p f g |
	X := self newFreeModule.
	Y := self newFreeModule.
	Z := self newFreeModule.
	g := X ⇒ Y atRandom: random bits: 20.
	p := Y ⇒ Z atRandom: random bits: 20.
	f := p · g.
	g := p \ f.
	self assert: p · g = f! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:12:29'!
testMapToBiggerDimensionIsNotSurjective
	| V W f |
	 V _ scalars^(4 atRandom: random).
	W _ scalars^((V rank atRandom: random) - 1).
	f _ W ⇒ V atRandom: random bits: 100.
	self assert: f isSurjective not! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:12:33'!
testMapToSmallerDimensionIsNotInjective
	| V W f |
	 V _ scalars^(4 atRandom: random).
	W _ scalars^((V rank atRandom: random) - 1).
	f _ V ⇒ W atRandom: random bits: 100.
	self assert: f isInjective not! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/23/2021 13:55:24'!
testModuleMap
	| f |
	f _ self newModuleMap.
	self testModuleMap: f! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 07:57:30'!
testModuleMap: f
	| a1 a2 b1 b2 x |
	a1 _ f domain atRandom: random bits: 20.
	a2 _ f domain atRandom: random bits: 20.
	b1 _ f value: a1.
	b2 _ f value: a2.
	self assert: (f value: f domain zero) isZero.
	x _ f domain scalars atRandom: random bits: 20.
	self assert: (f value: a1 * x + a2) = (b1 * x + b2)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 10/14/2023 15:54:45'!
testPreimage
	| f w v |
	f := self newModuleMap.
	w := f image atRandom: random bits: 100.
	v := f preimageElement: w.
	self assert: (f value: v) = w! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 08:10:07'!
testProjection: f
	| v |
	self testModuleMap: f.
	v _ f domain atRandom: random bits: 20.
	self assert: (f value: v) = (f value: (f value: v)). "idempotent at v"
	self assert: f isIdempotent.
	self assert: (f image apply: f) = f image.
	self assert: (f codomain apply: f) = f image! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/5/2023 22:15:21'!
testPullback
	| X Y Z f g P π |
	Z := self newSmallModule.
	X := self newSmallModule.
	Y := self newSmallModule.
	f := X ⇒ Z atRandom: random bits: 20.
	g := Y ⇒ Z atRandom: random bits: 20.
	P := f ∧ g.
	π := P ⇉ (X, Y).
	π do: [:each| self testModuleMap: each].
	self assert: f · (π at: 1) = (g · (π at: 2))! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/5/2023 22:15:27'!
testPullbackFree
	| X Y Z f g P π |
	Z := self newFreeModule.
	X := self newFreeModule.
	Y := self newFreeModule.
	f := X ⇒ Z atRandom: random bits: 20.
	g := Y ⇒ Z atRandom: random bits: 20.
	P := f ∧ g.
	π := P ⇉ (X, Y).
	π do: [:each| self testModuleMap: each].
	self assert: f · (π at: 1) = (g · (π at: 2))! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/5/2023 22:15:10'!
testPushout
	| X Y Z f g S ι |
	Z := self newSmallModule.
	X := self newSmallModule.
	Y := self newSmallModule.
	f := Z ⇒ X atRandom: random bits: 20.
	g := Z ⇒ Y atRandom: random bits: 20.
	S := f ∨ g.
	ι := S ⇇ (X, Y).
	ι do: [:each| self testModuleMap: each].
	self assert: (ι at: 1) · f = ((ι at: 2) · g)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 7/5/2023 22:15:37'!
testPushoutFree
	| X Y Z f g S ι |
	Z := self newFreeModule.
	X := self newFreeModule.
	Y := self newFreeModule.
	f := Z ⇒ X atRandom: random bits: 20.
	g := Z ⇒ Y atRandom: random bits: 20.
	S := f ∨ g.
	ι := S ⇇ (X, Y).
	ι do: [:each| self testModuleMap: each].
	self assert: (ι at: 1) · f = ((ι at: 2) · g)! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 6/16/2023 13:43:41'!
testAnnihilator
	| M A |
	M := self newModule.
	A := M annihilator.
	self assert: (M * (A atRandom: random)) isTrivial! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 6/26/2023 12:49:23'!
testCanonicalMorphisms
	| V v W w |
	V := self newModule.
	v := V atRandom: random bits: 100.
	W := V sub: {v}.
	w := W atRandom: random bits: 100.
	self assert: (V null → W) isZero. "canonical morphism from initial object"
	self assert: (V → W null) isZero. "canonical morphism to terminal object"
	self assert: ((W → W null) value: w) isZero.
	self assert: (W → V value: w) = w. "canonical morphism to overobject"! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 10/5/2022 16:53:44'!
testDirectSum
	| M N S |
	M _ self newModule.
	N _ self newModule.
	S _ M ⊕ N.
	S ⇇ (M, N) do: [:each| self testModuleMap: each].
	S ⇉ (M, N) do: [:each| self testModuleMap: each]! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 10/5/2022 16:54:24'!
testDirectSumFree
	| M N S |
	M _ self newFreeModule.
	N _ self newFreeModule.
	S _ M ⊕ N.
	self testFreeModule: S.
	S ⇇ (M, N) do: [:each| self testModuleMap: each].
	S ⇉ (M, N) do: [:each| self testModuleMap: each]! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 4/26/2021 11:38:09'!
testFreeModule: M
	| c t v x y |
	x _ M atRandom: random bits: 10 * M rank.
	self assert: x = (M elementAt: M \ x).
	y _ M atRandom: random bits: 10 * M rank.
	c _ M scalars atRandom: random bits: 20.
	v _ x * c + y.
	t _ (M \ x) * c + (M \ y).
	self assert: v = (M elementAt: t)! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 6/26/2023 12:49:54'!
testSubmoduleInclusion
	| V S P v w |
	[[V := self newModule.
	v := V atRandom: random bits: 10. v isZero] whileTrue.
	S := V sub: {v}.
	w := V atRandom: random bits: 10. S includes: w] whileTrue.
	P := V sub: {w*(V scalars atRandom: random bits: 10)-v. w}.
	self assert: S < P.
	self assert: P > S.
	self assert: S ~= P.
	self assert: P ~= S.
	self assert: (S > P) not.
	self assert: (P < S) not! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 6/26/2023 12:50:08'!
testSubmoduleSum
	| V v w S T P |
	[[V := self newModule.
	v := V atRandom: random bits: 10. v isZero not] whileFalse.
	w := V atRandom: random bits: 10. w isZero not and: [v ~= w]] whileFalse.
	S := V sub: {v}.
	T := V sub: {w}.
	P := V sub: {v. w}.
	self assert: S + T = P.
	self assert: T + S = P.
	T := V sub: {v - w. v + w}.
	self assert: S + T = P! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 3/24/2021 09:15:57'!
testTuples
	self testFreeModule: (scalars ^ ((0 to: 5) atRandom: random))! !

!AbstractModuleTest methodsFor: 'quotients' stamp: 'len 6/26/2023 12:49:36'!
testQuotientMaps
	| M N Q π v s |
	M := self newModule.
	N := M sub: ((1 to: (3 atRandom: random) - 1) collect: [:i| M atRandom: random bits: 20]).
	Q := M / N.
	π := M → Q.
	v := M atRandom: random bits: 100.
	self assert: (Q includes: (π value: v)).
	s := N atRandom: random bits: 100.
	self assert: (π value: s) isZero.
	self assert: (π value: v) = (π value: v+s)! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 10/5/2022 17:10:30'!
newFreeModule
	| M |
	M _ (3 atRandom: random) = 1 ifTrue: [scalars asIdeal] ifFalse: [scalars ^ ((0 to: 3) atRandom: random)].
	scalars isCommutative ifFalse: [^ M].
	(5 atRandom: random) = 1 ifTrue: [^ M ⇒ self newSmallFreeModule].
	(5 atRandom: random) = 1 ifTrue: [^ M dual].
	^ M! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 5/26/2023 15:00:19'!
newMatrix: shape
	| hom samples |
	hom := scalars^shape y ⇒ (scalars^shape x).
	random nextBoolean ifTrue: [^ hom atRandom: random bits: 100].
	samples := {scalars zero. scalars one. scalars one negated}.
	^ hom fill: [:i :j| (2 atRandom: random) > 1 ifTrue: [scalars zero] ifFalse: [samples atRandom: random]]! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 6/26/2023 12:49:07'!
newModule
	| M N |
	M := self newFreeModule.
	(3 atRandom: random) = 1 ifTrue: [^ M].
	N := M sub: ((1 to: ((0 to: M rank) atRandom: random)) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	(3 atRandom: random) = 1 ifTrue: [^ N].
	^ M / N! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 10/5/2022 17:10:41'!
newModuleMap
	| V W |
	V _ self newFreeModule.
	W _ self newFreeModule.
	^ V ⇒ W !! (self newMatrix: W rank @ V rank)! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 12/25/2021 10:54:29'!
newSmallFreeModule
	| M |
	M _ (3 atRandom: random) = 1 ifTrue: [scalars asIdeal] ifFalse: [scalars ^ ((0 to: 3) atRandom: random)].
	scalars isCommutative ifFalse: [^ M].
	(3 atRandom: random) = 1 ifTrue: [^ M dual].
	^ M! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 6/26/2023 12:49:17'!
newSmallModule
	| M N |
	M := self newSmallFreeModule.
	(3 atRandom: random) = 1 ifTrue: [^ M].
	N := M sub: ((1 to: ((0 to: M rank) atRandom: random)) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	(2 atRandom: random) = 1 ifTrue: [^ N].
	^ M / N! !

!AbstractModuleTest methodsFor: 'bugs' stamp: 'len 6/26/2023 12:49:30'!
testQuotientBySelfBug
	| V M |
	V := scalars ^ 3.
	M := V sub: {V atRandom: random. V atRandom: random}.
	self assert: (M / M) isTrivial! !

!AbstractCommutativeRingModuleTest methodsFor: 'adjoint' stamp: 'len 10/14/2022 08:38:59'!
testAdjoint
	| V W f fAdjoint φ |
	V _ self newFreeModule.
	W _ self newFreeModule.
	f _ V ⇒ W atRandom: random bits: 50.
	fAdjoint _ f adjoint.
	self assert: fAdjoint domain = f codomain dual.
	self assert: fAdjoint codomain = f domain dual.
	φ _ W dual atRandom: random bits: 20.
	self assert: (fAdjoint value: φ) = (φ · f)! !

!AbstractCommutativeRingModuleTest methodsFor: 'adjoint' stamp: 'len 10/5/2022 17:14:14'!
testAdjointAndHomFunctor
	"Given a R-linear map f, applying the Hom(-, R) produces the adjoint (the dual map).
	Example from 'A Singular Introduction To Commutative Algebra, pp. 115, procedure kontraHom."
	| f |
	f _ self newFreeModule ⇒ self newFreeModule :: atRandom: random bits: 100.
	self assert: (f ⇒ scalars asIdeal) = f adjoint! !

!AbstractCommutativeRingModuleTest methodsFor: 'adjoint' stamp: 'len 10/5/2022 17:14:26'!
testAdjointProperties
	| V W U f g a |
	V _ self newFreeModule.
	W _ self newFreeModule.
	f _ V ⇒ W atRandom: random bits: 100.
	g _ V ⇒ W atRandom: random bits: 100.
	a _ scalars atRandom: random bits: 10.
	self assert: (f + g) adjoint = (f adjoint + g adjoint).
	self assert: (f * a) adjoint = (f adjoint * a).
	U _ self newFreeModule.
	f _ U ⇒ V atRandom: random bits: 100.
	self assert: (g · f) adjoint = (f adjoint · g adjoint)! !

!AbstractCommutativeRingModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:13'!
testTensorProductAssociativity
	| V1 V2 V3 |
	V1 _ self newSmallModule.
	V2 _ self newSmallModule.
	V3 _ self newSmallModule.
	self assert: (V1 ⊗ V2) ⊗ V3 ~ (V1 ⊗ (V2 ⊗ V3))! !

!AbstractCommutativeRingModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:19'!
testTensorProductCommutativity
	| V W |
	V _ self newSmallModule.
	W _ self newSmallModule.
	self assert: V ⊗ W ~ (W ⊗ V)! !

!AbstractCommutativeRingModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:23'!
testTensorProductDimension
	| V1 V2 |
	V1 _ self newSmallFreeModule.
	V2 _ self newSmallFreeModule.
	self assert: (V1 ⊗ V2) rank = (V1 rank * V2 rank)! !

!AbstractCommutativeRingModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:28'!
testTensorProductDistributivity
	| A B C |
	A _ self newSmallFreeModule.
	B _ self newSmallFreeModule.
	C _ self newSmallFreeModule.
	self assert: A ⊕ B ⊗ C ~ (A ⊗ C ⊕ (B ⊗ C))! !

!AbstractCommutativeRingModuleTest methodsFor: 'tensor product' stamp: 'len 10/5/2022 17:14:33'!
testTensorProductDualHomIsomorphism
	| V W A B |
	V _ self newFreeModule.
	W _ self newFreeModule.
	A _ V dual ⊗ W.
	B _ V ⇒ W.
	self assert: A ~ B.
"	phi _ A to: B evaluating: [:t| B map: [:v| t second * (t first value: v)]].
	self testLinearMap: phi.
	self assert: phi isIsomorphism"! !

!AbstractCommutativeRingModuleTest methodsFor: 'tensor product' stamp: 'len 9/18/2022 07:15:04'!
testTensorProductDualIsomorphism
	| V W A B |
	V _ self newFreeModule.
	W _ self newFreeModule.
	A _ V dual ⊗ W dual.
	B _ (V ⊗ W) dual.
	self assert: A ~ B.
"	phi _ A to: B evaluating: [:s| B map: [:t| (s first value: t first) * (s second value: t second)]].
	self testLinearMap: phi.
	self assert: phi isIsomorphism"! !

!AbstractCommutativeRingModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:43'!
testTensorProductRelations
	| V W T v v1 v2 w w1 w2 c |
	V _ self newFreeModule.
	W _ self newFreeModule.
	T _ V ⊗ W.
	v _ V atRandom: random bits: 40.
	v1 _ V atRandom: random bits: 40.
	v2 _ V atRandom: random bits: 40.
	w _ W atRandom: random bits: 40.
	w1 _ W atRandom: random bits: 40.
	w2 _ W atRandom: random bits: 40.
	c _ V scalars atRandom: random bits: 40.
	self assert: T !! {v1. w} + (T !! {v2. w}) = (T !! {v1 + v2. w}).
	self assert: T !! {v. w1} + (T !! {v. w2}) = (T !! {v. w1 + w2}).
	self assert: (T !! {v. w})*c = (T !! {v*c. w}).
	self assert: (T !! {v. w})*c = (T !! {v. w*c})! !

!AbstractCommutativeRingModuleTest methodsFor: 'bugs' stamp: 'len 1/3/2022 11:24:40'!
testMapDirectSumAndTensorProductBug
	| V W f g |
	V _ self newFreeModule.
	W _ self newFreeModule.
	f _ V endomorphisms atRandom: random bits: 100.
	g _ W endomorphisms atRandom: random bits: 100.
	"doing it twice should give the same thing:"
	self assert: (f⊗g) domain = (f⊗g) domain.
	self assert: (f⊕g) domain = (f⊕g) domain! !

!AbstractCommutativeRingModuleTest methodsFor: 'other' stamp: 'len 6/15/2023 14:22:52'!
testFittingIdealsAndAnnihilator
	| M A i |
	M :=self newModule.
	A := M annihilator.
	self assert: (M fittingIdeal: 0) <= A.
	i := 3 atRandom: random.
	A * (M fittingIdeal: i) <= (M fittingIdeal: i-1)! !

!AbstractCommutativeRingModuleTest methodsFor: 'other' stamp: 'len 6/15/2023 13:39:13'!
testFittingIdealsIncreasing
	| M |
	M := self newModule.
	0 to: 2 do: [:i| self assert: (M fittingIdeal: i) <= (M fittingIdeal: i+1)]! !

!AbstractCommutativeRingModuleTest methodsFor: 'other' stamp: 'len 6/15/2023 14:20:53'!
testFittingIdealsOfFreeModule
	| M |
	M := scalars^(5 atRandom: random).
	"Fittᵢ(Rⁿ) must be 0 for i < n and R for i >= n:"
	(0 to: M rank - 1) do: [:i| self assert: (M fittingIdeal: i) isTrivial].
	(M rank to: M rank + 2) do: [:i| self assert: (M fittingIdeal: i) = scalars asIdeal].! !

!AbstractVectorSpaceTest methodsFor: 'maps' stamp: 'len 3/24/2021 08:02:36'!
testInverse
	| V f |
	V _ self newVectorSpace.
	[(f _ V endomorphisms atRandom: random bits: 1000) isIsomorphism] whileFalse.
	self assert: (f · f inverse) isIdentity.
	self assert: (f inverse · f) isIdentity! !

!AbstractVectorSpaceTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:14:44'!
testRetraction
	| V W f r |
	[V _ self newVectorSpace.
	W _ self newVectorSpace.
	f _ V ⇒ W atRandom: random bits: 100. f isInjective] whileFalse.
	r _ f retraction.
	self assert: r isSurjective.
	self assert: r · f = V id! !

!AbstractVectorSpaceTest methodsFor: 'maps' stamp: 'len 10/5/2022 17:14:49'!
testSection
	| V W f s |
	[V _ self newVectorSpace.
	W _ self newVectorSpace.
	f _ V ⇒ W atRandom: random bits: 50. f isSurjective] whileFalse.
	s _ f section.
	self assert: s isInjective.
	self assert: f · s = W id! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 6/16/2023 13:43:12'!
testAdjointAndAnnihilatorSpace
	| V W f a |
	V := scalars ^ (3 atRandom: random).
	W := scalars ^ (5 atRandom: random).
	f := V ⇒ W atRandom: random bits: 100.
	a := f adjoint.
	self assert: a kernel = f image annihilatorSpace.
	self assert: a image = f kernel annihilatorSpace.
	self assert: f isSurjective = a isInjective.
	self assert: f isInjective = a isSurjective! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 6/26/2023 12:50:23'!
testAnnihilatorSpace
	| V v S S0 |
	[(V := self newVectorSpace) isTrivial] whileTrue.
	[(v := V atRandom: random bits: 100) isZero] whileTrue.
	S := V sub: {v}.
	S0 := S annihilatorSpace.
	self assert: S0 rank = S corank.
	S0 generators do: [:f| self assert: (f value: v) isZero]! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 6/16/2023 13:43:20'!
testAnnihilatorSpaceProperties
	| V n m S T |
	V := self newVectorSpace.
	n := (0 to: V rank) atRandom: random.
	S := (V grassmannian: n) atRandom: random bits: (100 atRandom: random).
	m := (0 to: n) atRandom: random.
	T := (S grassmannian: m) atRandom: random bits: (100 atRandom: random).
	self assert: T <= S.
	self assert: T annihilatorSpace >= S annihilatorSpace. "reverses inclusions"
	self assert: (S + T) annihilatorSpace = (S annihilatorSpace ∩ T annihilatorSpace).
	self assert: S rank + S annihilatorSpace rank = V rank.
	self assert: V annihilatorSpace isTrivial.
"	self assert: S annihilator annihilator = S dual dual"! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 8/12/2022 13:13:49'!
testGrassmannRelation
	| V S T |
	V _ self newVectorSpace.
	S _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (20 atRandom: random).
	T _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (20 atRandom: random).
	self assert: S rank + T rank = ((S + T) rank + (S ∩ T) rank)! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 3/26/2021 14:23:01'!
testGrassmannian
	| V r G S |
	V _ self newVectorSpace.
	r _ (0 to: V rank) atRandom: random.
	G _ V grassmannian: r.
	S _ G atRandom: random bits: (20 atRandom: random).
	self assert: S rank = r.
	self assert: (G includes: S).
	self assert: (G dual includes: S annihilatorSpace)! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 5/30/2023 22:58:11'!
testIsSemisimple
	| V |
	V := self newVectorSpace.
	self assert: V isSemisimple! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 4/6/2021 14:55:41'!
testSubspaceOfSubspace
	| V S s T t |
	[V _ self newVectorSpace. V rank > 2] whileFalse.
	S _ (V grassmannian: 2) atRandom: random bits: (100 atRandom: random).
	T _ (S grassmannian: 1) atRandom: random bits: (100 atRandom: random).
	self assert: S < V.
	self assert: T < S.
	self assert: T < V.
	[s _ S atRandom: random bits: (100 atRandom: random). T includes: s] whileTrue.
	[t _ T atRandom: random bits: (100 atRandom: random). t isZero] whileTrue.
	self assert: (S includes: s).
	self assert: (S includes: t).
	self assert: (T includes: t)! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 4/5/2021 04:34:32'!
testSubspaceSumRandom
	| V S T U |
	V _ self newVectorSpace.
	S _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (100 atRandom: random).
	T _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (100 atRandom: random).
	U _ S + T.
	self assert: S <= U.
	self assert: T <= U! !

!AbstractVectorSpaceTest methodsFor: 'quotients' stamp: 'len 3/24/2021 09:06:07'!
testOrthogonalProjection
	| V S p v |
	V _ self newVectorSpace.
	S _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (30 atRandom: random).
	p _ S orthogonalProjection.
	self testProjection: p.
	v _ V atRandom: random bits: (50 atRandom: random).
	self assert: (p value: v) = (p value: (p value: v)). "idempotent at v"
	self assert: p isIdempotent.
	self assert: p kernel = S complement.
	self assert: p image = S.
	self assert: (S apply: p) = S! !

!AbstractVectorSpaceTest methodsFor: 'quotients' stamp: 'len 12/10/2021 19:55:03'!
testQuotientSpace
	| V S Q |
	[(V _ self newVectorSpace) isTrivial] whileTrue.
	S _ V grassmannian: (V rank atRandom: random) :: atRandom: random bits: V rank * 10.
	Q _ V/S.
	self assert: Q rank = (V rank - S rank).
"	self assert: (Q includes: S + (V atRandomBits: V rank * 10))."
	self assert: (V / V null) rank = V rank.
	self assert: (V / V) rank = 0! !

!AbstractVectorSpaceTest methodsFor: 'quotients' stamp: 'len 6/12/2023 19:58:19'!
testQuotientSpaceBug
	| V v Q |
	V := scalars^3.
	v := V atRandom.
	Q := V / {v}.
	"The number of generators should be minimal (a basis):"
	self assert: Q rank = Q generators size.
	"The rank and basis size should match:"
	self assert: Q rank = Q basis size! !

!AbstractVectorSpaceTest methodsFor: 'quotients' stamp: 'len 10/5/2022 17:38:51'!
testQuotientSpaceMaps
	| V S Q π v s |
	V _ self newVectorSpace.
	S _ V grassmannian: ((0 to: V rank) atRandom: random) :: atRandom: random bits: 50.
	Q _ V/S.
	π _ V → Q.
	v _ V atRandom: random bits: 100.
	self assert: (Q includes: (π value: v)).
	s _ S atRandom: random bits: 100.
	self assert: (π value: s) isZero.
	self assert: (π value: v) = (π value: v+s)! !

!AbstractVectorSpaceTest methodsFor: 'tensor product' stamp: 'len 10/14/2022 08:39:18'!
testTensorHomAdjunction
	| V W U A B φ |
	V _ self newVectorSpace.
	W _ self newVectorSpace.
	U _ self newVectorSpace.
	A _ V⊗W ⇒ U.
	B _ V ⇒ (W ⇒ U).
	self assert: A ~ B.
	φ _ A to: B map: [:f| B map: [:v| W to: U map: [:w| f value: A domain !! {v. w}]]].
	self testModuleMap: φ.
	self assert: φ isIsomorphism! !

!AbstractVectorSpaceTest methodsFor: 'tensor product' stamp: 'len 12/15/2021 14:29:26'!
testTensorProductOfLinearForms
	| V W f g t v w |
	V _ self newFreeModule.
	W _ self newFreeModule.
	f _ V dual atRandom: random bits: 10.
	g _ W dual atRandom: random bits: 10.
	t _ f ⊗ g.
	self assert: t domain = (V ⊗ W).
	self assert: t codomain = (f codomain ⊗ g codomain).
	v _ V atRandom: random bits: 10.
	w _ W atRandom: random bits: 10.
	self assert: (t value: t domain !! {v. w}) = (t codomain !! {f value: v. g value: w})! !

!AbstractVectorSpaceTest methodsFor: 'private' stamp: 'len 3/24/2021 08:02:48'!
newVectorSpace
	^ self newFreeModule! !

!BinaryVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:19:40'!
setUp
	super setUp.
	scalars := ℤ/2! !

!BinaryVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:42:58'!
testKernelZ2
	| V f kernel |
	V _ (PrimeField new: 2) ^ 3.
	f _ V dual map: [:v| v sum].
	kernel _ f kernel.
	self assert: (kernel apply: f) isTrivial.
	self assert: kernel rank = 2.
	self assert: (kernel includes: V !! (0,1,1))! !

!BinaryVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:04'!
testLinearFormZ2
	| V f |
	V _ (PrimeField new: 2) ^ 4.
	f _ V dual map: [:v| v sum].
	self assert: f domain = V.
	self assert: f kernel rank = 3.
	self assert: f image rank = 1.
	self assert: (f kernel includes: V !! (1,1,1,1))! !

!BinaryVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:40:45'!
testModuleMapZ2
	| V f kernel v |
	V _ (PrimeField new: 2) ^ ((5 atRandom: random) + 1).
	[f _ V endomorphisms atRandom: random.
	(kernel _ f kernel) isTrivial or: [kernel = V]] whileTrue. "find one with non-trivial kernel"
	[v _ V atRandom: random. kernel includes: v] whileTrue.
	self assert: (f value: v) isZero not.
	v _ kernel atRandom: random.
	self assert: (f value: v) isZero! !

!BinaryVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:50:28'!
testPerpZ2
	| V S perp |
	V := (ℤ / 2) ^ 3.
	S := V sub: {(1,1,1) % 2}.
	perp := S complement.
	self assert: perp rank = 2.
	self assert: (S basis first · perp basis first) isZero.
	self assert: (S basis first · perp basis second) isZero! !

!CyclotomicFieldVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:20:03'!
setUp
	super setUp.
	scalars := CyclotomicField new: (3 atRandom: random) + 2! !

!FiniteFieldVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:20:27'!
setUp
	super setUp.
	scalars := GaloisField new: (10 atRandom: random) nextPrime to: (3 atRandom: random)! !

!GaussianFieldVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 11/5/2023 21:31:30'!
setUp
	super setUp.
	scalars := CyclotomicField new: 4! !

!QuadraticFieldVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:23:30'!
setUp
	super setUp.
	scalars := QuadraticField new: -5! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:20:51'!
setUp
	super setUp.
	scalars := ℚ! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:10'!
testLinearForm1
	| V f |
	V _ ℚ ^ 3.
	f _ V dual map: [:v| v x + v y - v z].
	self assert: (f value: (1,1,1)) = 1.
	self assert: (f value: (2,2,1)) = 3.
	self assert: (f value: (3,1, -1)) = 5! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:15'!
testLinearForm2
	| V f |
	V _ ℚ ^ 3.
	f _ V dual map: [:v| v x + v y - v z].
	self assert: f domain = V.
	self assert: f kernel rank = 2.
	self assert: f image rank = 1! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:41:14'!
testLinearFormAsPolynomial
	| V f x |
	V _ ℚ ^ (5 atRandom: random).
	f _ V dual atRandom: random bits: 100.
	x _ V atRandom: random bits: 100.
	self assert: (f value: x) = (f asPolynomial value: x).
	self assert: f parent !! f asPolynomial = f! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:23:57'!
testMapsInducedByHom
	"Example from 'A Singular Introduction to Commutative Algebra', Example 2.1.7 (Maps Induced by Hom)."
	| X Y Z f g h yz zx |
	X _ ℚ^3.
	Y _ ℚ^3.
	Z _ ℚ^2.
	f _ X to: Y matrix: (ℚ^3) endomorphisms !! (1 to: 9).
	g _ f ⇒ Z. "Hom(-,Z)"
	self assert: g domain = (Y ⇒ Z). "it is contravariant"
	self assert: g codomain = (X ⇒ Z).
	self assert: g asMatrix = ((ℚ^2) id ⊗ f asMatrix transpose).
	yz _ Y ⇒ Z atRandom: random bits: 100.
	self assert: (g value: yz) = (yz · f).
	h _ Z ⇒ f. "Hom(Z,-)"
	self assert: h domain = (Z ⇒ X). "it is covariant"
	self assert: h codomain = (Z ⇒ Y).
	self assert: h asMatrix = (f asMatrix ⊗ (ℚ^2) id).
	zx _ Z ⇒ X atRandom: random bits: 100.
	self assert: (h value: zx) = (f · zx)! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:50:35'!
testPerpExample
	| V S p perp |
	V := ℚ ^ 3.
	S := V sub: {(1, 1, 1). (2, 0, -2)}.
	perp := S complement.
	self assert: perp rank = 1.
	self assert: (perp basis first · S basis first) isZero.
	self assert: (perp basis first · S basis second) isZero.
	p := S orthogonalProjection.
	self assert: p kernel = perp! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:50:41'!
testPerpProperties
	| V S T |
	V := ℚ ^ 3.
	S := V sub: {(1, 1, 1). (2, 0, -2)}.
	T := V sub: {(1, -1, -3)}. "subspace of S"
	self assert: S + S complement = V.
	self assert: T + T complement = V.
	self assert: (S ∩ S complement) isTrivial.
	self assert: S <= S complement complement.
	self assert: S complement < T complement "because S > T"! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:38:55'!
testQuotientSpaceExample
	| V M x |
	V _ ℚ^3.
	M _ V / {V !! (1,2,3)}.
	x _ V → M value: (1,2,3).
	self assert: x isZero.
"	self assert: (M lifting value: x) = (0,0,0)"! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:50:52'!
testSubspaceIntersectionAndSumExample
	"Example from wikipedia Zassenhaus algorithm"
	| V U W |
	V := ℚ ^ 4.
	U := V sub: {(1,-1,0,1). (0,0,1,-1)}.
	W := V sub: {(5,0,-3,3). (0,5,-3,-2)}.
	self assert: U + W = (V sub: {(1,0,0,0). (0,1,0,-1). (0,0,1,-1)}).
	self assert: U ∩ W = (V sub: {(1,-1,0,1)})! !

!RationalVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:51:01'!
testSubspaceIntersectionExample
	| V S T T2 |
	V := ℚ ^ 3.
	S := V sub: {(1, 1, 1). (2, 0, -2)}.
	self assert: S ∩ V = S.
	self assert: S ∩ S = S.
	T := V sub: {(3,1,-1)}.
	self assert: S ∩ T = T.
	T2 := V sub: {(3,1,-1). (0,1,0)}.
	self assert: S ∩ T2 = T! !

!TernaryVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:21:07'!
setUp
	super setUp.
	scalars := ℤ/3! !

!TernaryVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:51:07'!
testQuotientGolay1
	"Construct the quotient (Z/3Z)^11 by the Golay code.
	(Example from Magma H28E10.)"
	| V S Q |
	V := ℤ/3 ^ 11.
	S := V sub: {(1,0,0,0,0,0,1,1,1,1,1). (0,1,0,0,0,0,0,1,2,2,1). (0,0,1,0,0,0,1,0,1,2,2). (0,0,0,1,0,0,2,1,0,1,2). (0,0,0,0,1,0,2,2,1,0,1). (0,0,0,0,0,1,1,2,2,1,0)}.
	Q := V/S.
	self assert: Q rank = 5.
	self assert: Q ambient rank = 5! !

!TernaryVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2023 16:45:51'!
testQuotientGolay2
	"Construct the quotient (Z/3Z)^11 by the Golay code as a subspace (the orthogonal complement of the Golay code).
	(Example from Magma H28E11.)"
	| V S Q |
	V := ℤ/3 ^ 11.
	S := V sub: ({(1,0,0,0,0,0,1,1,1,1,1). (0,1,0,0,0,0,0,1,2,2,1). (0,0,1,0,0,0,1,0,1,2,2). (0,0,0,1,0,0,2,1,0,1,2). (0,0,0,0,1,0,2,2,1,0,1). (0,0,0,0,0,1,1,2,2,1,0)} collect: [:each| V !! each]).
	Q := S complement.
	self assert: Q rank = 5.
	self assert: Q ambient rank = 11! !

!CyclotomicIntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:21:30'!
setUp
	super setUp.
	scalars := (CyclotomicField new: (3 atRandom: random) + 2) integers! !

!EisensteinIntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:21:47'!
setUp
	super setUp.
	scalars := (CyclotomicField new: 3) integers! !

!FiniteFieldAffineAlgebraModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/7/2023 17:39:22'!
setUp
	| K P |
	super setUp.
	K := GaloisField new: ((#(3 16rFF 16rFFFF) atRandom: random) atRandom: random) nextPrime to: (10 atRandom: random).
	P := K polynomialsIn: (3 atRandom: random).
	[scalars := P / ((1 to: (4 atRandom: random) - 1) collect: [:i| P atRandom: random bits: 20]).
	scalars isTrivial] whileTrue! !

!FiniteFieldDualNumbersModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/7/2023 17:39:12'!
setUp
	| K P |
	super setUp.
	K := GaloisField new: ((#(3 16rFF 16rFFFF) atRandom: random) atRandom: random) nextPrime to: (5 atRandom: random).
	P := K polynomials.
	scalars := P / {P x ^ 2}! !

!GaloisRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:22:19'!
setUp
	super setUp.
	scalars := GaloisRing new: (3 atRandom: random) nextPrime to: (2 atRandom: random) to: (2 atRandom: random)! !

!GaloisRingMultivariatePolynomialRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:22:56'!
setUp
	super setUp.
	scalars := (GaloisRing new: (3 atRandom: random) nextPrime to: (2 atRandom: random) to: (2 atRandom: random)) polynomialsIn: 2! !

!GaussianIntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 11/5/2023 21:31:37'!
setUp
	super setUp.
	scalars := (CyclotomicField new: 4) integers! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:23:23'!
setUp
	super setUp.
	scalars := ℤ! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:51:21'!
testFreeModuleDiscriminant
	"From Sage documentation of FreeModule."
	| M |
	M := ℤ^3.
	self assert: M discriminant = 1.
	self assert: (M sub: {(1,2,3)}) discriminant = 14.
	self assert: (M sub: {(1,2,3). (1,1,1)}) discriminant = 6! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:22:19'!
testHomBug
	| V |
	V _ ℤ^1 / {5}.
	self assert: V dual isTrivial.
	self assert: V dual invariants isEmpty! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:15:38'!
testHomBug2
	| V W |
	V _ ℤ^1 / {2}.
	W _ ℤ^(5 atRandom: random).
	self assert: (V ⇒ W) isTrivial! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 10/30/2023 19:28:05'!
testHomExample
	| A B H |
	A := (ℤ/4) asAbelianGroup.
	B := (ℤ/2) asAbelianGroup.
	H := A ⇒ B.
	self assert: H generators size = 1.
	self assert: H generator = (A to: B images: {B generator}).
	self assert: H size = 2.
	H := B ⇒ A.
	self assert: H generators size = 1.
	self assert: H generator = (B to: A images: {A generator * 2}).
	self assert: H size = 2! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:10:57'!
testInvariantsAndIsomorphismExample
	| M |
	M _ ℤ^3 / {(0,-2,1). (-2,0,1)}.
	self assert: M invariants = #(0 2).
	self assert: M ~ (ℤ^1 / {2} ⊕ (ℤ^1))! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:11:08'!
testInvariantsAndIsomorphismExample2
	| M |
	M _ ℤ^3 / {(2,1,0)}.
	self assert: M invariants = #(0 0).
	self assert: M ~ (ℤ^2)! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:11:19'!
testInvariantsAndIsomorphismExample3
	| M |
	M _ ℤ^1 / {2} ⊕ (ℤ^1) / {(1,2)}.
	self assert: M invariants = #(4).
	self assert: M ~ (ℤ^1/{4})! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:42:48'!
testModuleMapBug
	| M f |
	M _ ℤ^1 / {2}.
	f _ M dual atRandom: random bits: 100.
	self testModuleMap: f! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 9/18/2022 06:08:16'!
testModuleMapBug2
	| M N |
	M _ ℤ^1/{3}.
	N _ ℤ^1.
	"these are not valid module homomorphisms:"
	self should: [M to: N images: #(1)] raise: DomainError withMessageText: 'invalid morphism'.
	self should: [M to: N images: #(2)] raise: DomainError withMessageText: 'invalid morphism'! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:51:33'!
testQuotientModuleAnnihilator
	"Example from Sage (removed denominators)"
	| v1 v2 v3 V W |
	v1 := (1,0,0). v2 := (3,4,2). v3 := (0,0,2).
	V := ℤ^3 sub: {v1. v2. v3}.
	W := V ambient sub: {v1 + (v2*2). v1*9 + (v2*2). v3*4}.
	self assert: (V/W) annihilator = (ℤ*16)
	
"original with denominators:
	| v1 v2 v3 V W |
	v1 _ (1/2,0,0). v2 _ (3/2,2,1). v3 _ (0,0,1).
	V _ QQ^3 sub: {v1. v2. v3} over: ℤ.
	W _ QQ^3 sub: {v1 + (v2*2). v1*9 + (v2*2). v3*4} over: ℤ.
	self assert: (V/W) annihilator = (ℤ*16)
"! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/18/2021 12:46:45'!
testQuotientModuleBug
	| M a |
	M _ ℤ^2/{(0,2)}.
	a _ M !! (3,0).
	self assert: a representative = (3,0).
	self assert: (a*2) representative = (6,0)! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:51:43'!
testQuotientModuleInvariants
	"Example from Sage (removed denominators)"
	| v1 v2 v3 V W |
	v1 := (1,2,2). v2 := (3,4,2). v3 := (0,0,2).
	V := ℤ^3 sub: {v1. v2. v3}.
	W := V ambient sub: {v1*2 + (v2*4). v1*9 + (v2*12). v3*4}.
	self assert: (V/W) invariants = #(4 12)

"original with denominators:
	| v1 v2 v3 V W |
	v1 _ (1/2,1,1). v2 _ (3/2,2,1). v3 _ (0,0,1).
	V _ QQ^3 sub: {v1. v2. v3} over: ℤ.
	W _ QQ^3 sub: {v1*2 + (v2*4). v1*9 + (v2*12). v3*4} over: ℤ.
	self assert: (V/W) invariants = #(4 12)
"! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:51:47'!
testQuotientModuleInvariants2
	"Example from Sage"
	| V W |
	V := ℤ^3.
	W := V sub: {(1,2,0). (0,1,0). (0,2,0)}.
	self assert: (V/W) invariants = #(0) "from example in sage fgp_module.py"! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:51:54'!
testQuotientModuleIsFinite
	"Example from Sage (removed denominators)"
	| v1 v2 v3 V W |
	v1 := (1,0,0). v2 := (3,4,2). v3 := (0,0,2).
	V := ℤ^3 sub: {v1. v2. v3}.
	W := V ambient sub: {v1 + (v2*2). v1*9 + (v2*2). v3*4}.
	self assert: (V/W) isFinite.
	W := V null.
	self assert: (V/W) isFinite not

"original with denominators:
	| v1 v2 v3 V W |
	v1 _ (1/2,0,0). v2 _ (3/2,2,1). v3 _ (0,0,1).
	V _ QQ^3 span: {v1. v2. v3} over: ℤ.
	W _ QQ^3 span: {v1 + (v2*2). v1*9 + (v2*2). v3*4} over: ℤ.
	self assert: (V/W) isFinite.
	W _ V null.
	self assert: (V/W) isFinite not
"! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:51:59'!
testQuotientModuleSpanBug
	"The span is the original module, but it answers a submodule."
	| M |
	M := ℤ^2 / {(1,2)}.
	self assert: M = (M sub: M generators)! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:52:07'!
testSubmoduleIntersectionExample
	| M S T T2 |
	M := ℤ ^ 3.
	S := M sub: {(1, 1, 1). (2, 0, -2)}.
	self assert: S ∩ M = S.
	self assert: M ∩ S = S.
	self assert: S ∩ S = S.
	T := M sub: {(3,1,-1)}.
	self assert: S ∩ T = T.
	T2 := M sub: {(3,1,-1). (0,1,0)}.
	self assert: S ∩ T2 = T! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:52:11'!
testSubquotientContainsBug
	| V v S |
	V := ℤ / 5 :: asAbelianGroup.
	v := V elementAt: #(4).
	S := V sub: {v}.
	self assert: (S contains: v * 3)! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/10/2022 11:22:25'!
testSyzygies1
	| V |
	V _ ℤ^2 / {(2,0)}.
	self assert: (V elementAt: (V relations atRandom: random bits: 100)) isZero! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 10/5/2022 17:15:49'!
testSyzygies2
	| V W X |
	V _ ℤ^2 / {(2,0)}.
	W _ ℤ^3 / {(2,2,2)}.
	X _ V ⇒ W.
	self assert: (X elementAt: (X relations atRandom: random bits: 100)) isZero! !

!IntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:13:27'!
testTensorProductExample
	self assert: ℤ^1 ⊗ (ℤ^1/{2}) ~ (ℤ^1/{2})! !

!LocalIntegerRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:23:54'!
setUp
	super setUp.
	scalars := ℤ @ (ℤ * (64 atRandom: random) nextPrime)! !

!ModularIntegerModuleTest methodsFor: 'initialization' stamp: 'len 5/26/2023 00:24:13'!
setUp
	super setUp.
	scalars := ℤ/((2 to: 64) atRandom: random)! !

!ModularIntegerModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:52:21'!
testExampleModulesOverZ12
	| M A B |
	M := ℤ/12^3.
	A := M sub: {(4,1,0). (0,0,5)}.
	B := M sub: {(8,5,5). (0,9,8). (0,0,10)}.
	self assert: A = B! !

!ModularIntegerMultivariatePolynomialRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/17/2023 22:46:05'!
setUp
	super setUp.
	scalars := (ℤ/((2 to: 64) atRandom: random)) polynomialsIn: 2! !

!ProductRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:24:55'!
setUp
	super setUp.
	scalars := ℤ × (ℤ/((64 atRandom: random) + 1))! !

!QuadraticRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 11:23:36'!
setUp
	| n |
	super setUp.
	[(n := ((2 to: 100) atRandom: random) squarefreePart) > 1] whileFalse.
	random nextBoolean ifTrue: [n := n negated].
	scalars := (QuadraticField new: n) integers! !

!RationalAffineAlgebraModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/7/2023 14:39:20'!
setUp
	| P |
	super setUp.
	P := ℚ polynomialsIn: (3 atRandom: random).
	[scalars := P / ((1 to: (4 atRandom: random) - 1) collect: [:i| P atRandom: random bits: 20]).
	scalars isTrivial] whileTrue! !

!RationalDualNumbersModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/7/2023 14:45:33'!
setUp
	| P |
	super setUp.
	P := ℚ polynomials.
	scalars := P / {P x ^ 2}! !

!RationalMultivariatePolynomialRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:26:02'!
setUp
	super setUp.
	scalars := ℚ polynomialsIn: 2! !

!RationalMultivariatePolynomialRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/28/2023 16:59:33'!
testModuleGroebnerBasisReducedExample
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.6.1."
	| R x y M f1 f2 f3 f4 A |
	R := ℚ polynomialsIn: #(x y) ordering: #invlex.
	x := R x. y := R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M := R^3 orderedBy: #top.
"	e1 _ M x: 3. e2 _ M x: 2. e3 _ M x: 1."
	f1 := M !! {x*y. y. x} reversed.
	f2 := M !! {x^2 + x. y + (x^2). y} reversed.
	f3 := M !! {y negated. x. y} reversed.
	f4 := M !! {x^2. x. y} reversed.
	A := Matrix rows: {f1. f2. f3. f4}.
	self assert: A basis "reduced" width = 6 "it's already reduced"
	"TODO: verify that it matches the book"! !

!RationalMultivariatePolynomialRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/3/2023 21:30:33'!
testTorsionFreeNotFlatExample
	"The ideal ⟨x,y⟩ over K[x,y] is torsion-free but not flat."
	| R I |
	R := ℚ polynomialsIn: 2.
	I := R * R generators.
	self assert: I isTorsionFree.
	self assert: I isFlat not! !

!RationalPolynomialRingModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:26:13'!
setUp
	super setUp.
	scalars := ℚ polynomials! !

!CliffordAlgebraModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 15:58:08'!
setUp
	super setUp.
	self flag: #fixme. "should use ℚ^3 or more, but now it's too slow"
	scalars := CliffordAlgebra on: (QuadraticForm from: ℚ^2 coefficients: #(1 0 0 1 0 1))! !

!MatrixAlgebraModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:27:23'!
setUp
	super setUp.
	scalars := (ℤ/(256 atRandom: random) nextPrime^(2 atRandom: random)) endomorphisms! !

!QuaternionAlgebraModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:27:33'!
setUp
	super setUp.
	scalars := QuaternionAlgebra over: ℚ invariants: #(1 1)! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 6/17/2023 12:13:56'!
testExtendedBuchbergerWithCyclic3
	| R I A basisAndTransformation G T |
	R := scalars polynomialsIn: 3.
	I := R cyclicIdeal.
	I := I orderedBy: #grevlex.
	A := I generatorMatrix.
	basisAndTransformation := A basisAndTransformation.
	G := basisAndTransformation first.
	T := basisAndTransformation second.
	self assert: A * T = G.
	self assert: G isMinimalStandardBasis.
	self assert: G isReducedStandardBasis! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 6/17/2023 12:15:23'!
testGroebnerBasis: G ideal: I
	"Test G is a Groebner basis of the ideal I."
	| g division |
	I isTrivial ifTrue: [^ self assert: G isEmpty].
	I generators do: [:each| self assert: (each reductionBy: G) isZero].
	G do: [:each| self assert: (I includes: each)]. "but this will use Groebner bases.."
	[g := I atRandom: random bits: 200. g isZero] whileTrue.
	self assert: (g reductionBy: G) isZero.
	"The leading term of any polynomial in I is divisible by the leading term of some polynomial in G:"
	self assert: (G anySatisfy: [:any| any leadingMonomial | g leadingMonomial]).
	"The multivariate division of any polynomial in I by G gives the remainder 0:"
	division := g divisionBy: G.
	self assert: division remainder isZero.
	division validate.
"	self assert: G isMinimalStandardBasis.
	self assert: G isReducedStandardBasis"! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2022 19:06:13'!
testGroebnerCyclic3
	| I G |
	I _ (scalars polynomialsIn: 3) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2022 19:07:18'!
testGroebnerCyclic3Lexicographic
	| I G |
	I _ (scalars polynomialsIn: 3 ordering: #lex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2022 19:07:35'!
testGroebnerCyclic3TotalLexicographic
	| I G |
	I _ (scalars polynomialsIn: 3 ordering: #glex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2022 19:07:46'!
testGroebnerCyclic3TotalReverseLexicographic
	| I G |
	I _ (scalars polynomialsIn: 3 ordering: #grevlex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/29/2022 19:37:28'!
testGroebnerKatsura2
	| I G |
	I _ (scalars polynomialsIn: 2) katsuraIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/29/2022 19:37:35'!
testGroebnerKatsura3
	| I G |
	I _ (scalars polynomialsIn: 3) katsuraIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:15:58'!
testIdealMembership
	| R f1 f2 f3 I g |
	R := scalars polynomialsIn: (3 atRandom: random).
	[(f1 := R atRandom: random bits: 10) isZero] whileTrue.
	[(f2 := R atRandom: random bits: 10) isZero] whileTrue.
	[(f3 := R atRandom: random bits: 10) isZero] whileTrue.
	I := R * {f1. f2. f3}.
	g := I atRandom: random bits: 100.
	self assert: (I includes: g)! !

!AbstractGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2022 19:05:01'!
testZeroIdeal
	| R I f |
	R _ scalars polynomialsIn: (3 atRandom: random).
	I _ R * #(). "the zero ideal {0}"
	self assert: I isTrivial.
	self assert: (I includes: R zero).
	[f _ R atRandom: random bits: 10. f isZero] whileTrue.
	self deny: (I includes: f)! !

!FiniteFieldGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:04:47'!
setUp
	super setUp.
	scalars := GaloisField new: (5 atRandom: random) nextPrime to: (3 atRandom: random)! !

!GaloisRingGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:05:20'!
setUp
	super setUp.
	scalars := GaloisRing new: (5 atRandom: random) nextPrime to: (3 atRandom: random) to: (2 atRandom: random)! !

!IntegerGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:05:30'!
setUp
	super setUp.
	scalars := ℤ! !

!ModularIntegerGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:05:47'!
setUp
	super setUp.
	scalars := ℤ/(1024 atRandom: random)! !

!RationalGroebnerBasisTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:06:21'!
setUp
	super setUp.
	scalars := ℚ! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testFiniteRingJacobsonRadical
	| R |
	R _ ℤ/12.
	self assert: R radical = (R*6)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/7/2022 07:36:06'!
testFiniteRingLocalAndNotLocal
	| R S |
	R _ ℤ/6. "this is not a Galois ring, the zero divisors dont form an ideal"
	S _ ℤ/8. "this is a Galois ring"
	self assert: R isLocal not.
	self assert: S isLocal! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testFiniteRingsAreArtinian
	| R |
	R _ ℤ/6. "not local"
	self assert: R isArtinian.
	R _ ℤ/8. "local, prime power ring, Galois ring"
	self assert: R isArtinian.
	R _ ℤ/4 × (ℤ/4).
	self assert: R isArtinian.
	R _ GaloisRing new: 3 to: 2 to: 4.
	self assert: R isArtinian! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:21:42'!
testGaloisField
	| p k F |
	p _ #(2 3 5) atRandom.
	k _ #(1 2 3) atRandom.
	F _ GaloisField new: p to: k.
	self assert: F isField.
	self assert: F characteristic = p.
	self testRing: F.
	self testRingHomomorphism: F projection.
	self testSmallRingUnitsAndAssociates: F! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:18:36'!
testGaloisField2
	| GF pi x one a |
	GF _ GaloisField new: 3^3.
	self assert: GF isField.
	self assert: GF characteristic = 3.
	pi _ GF projection.
	x _ pi domain x.
	one _ pi domain one.
	a _ pi value: x^2 + one.
	self assert: a * a inverse = GF one.
	[(a _ GF atRandom) isZero] whileTrue.
	self assert: a * a inverse = GF one! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:13:29'!
testGaloisRing
	| R |
	R _ GaloisRing new: 3 to: 3 to: 2.
	self testRing: R.
	self testSmallRingUnitsAndAssociates: R.
	self assert: R characteristic = (3^3).
	self assert: R size = (3^3^2).
	self assert: R isLocal.
	self assert: R isEuclidean! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/22/2018 03:53:54'!
testGaloisRingNilradical
	| R |
	R _ GaloisRing new: 3 to: 3 to: 2.
	self assert: R nilradical = (R * 3)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/4/2020 06:27:21'!
testGaloisRingUnitsSize
	| R |
	R _ GaloisRing new: 3 to: 2 to: 2.
	self assert: R units asSet size = 72! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testIntegersModuloOneIsTrivial
	self assert: (ℤ/1) isTrivial! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegers
	| R |
	R _ ℤ / 256 atRandom.
	self testRing: R.
	self testSmallRingUnitsAndAssociates: R.
	self testRingHomomorphism: R projection! !

!RingTest methodsFor: 'finite rings' stamp: 'len 7/15/2021 10:40:39'!
testModularIntegersAnnihilator
	| R |
	R _ ℤ/64 atRandom.
	R do: [:x|
		| a A |
		a _ x annihilator.
		A _ x annihilatorIdeal.
		self assert: (x * a) isZero.
		R do: [:y| (x*y) isZero ifTrue: [self assert: (A includes: y)]]]! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:22:07'!
testModularIntegersLift
	self assert: ((2 to: 100) allSatisfy: [:m| (1 to: m) allSatisfy: [:x| (x % m) lift % m = (x % m)]])! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersPrimitiveRoots
	| X |
	"number of primitive roots in Z/nZ for n=2 to 17:"
	X _ #(1 1 1 2 1 2 0 2 2 4 0 4 2 0 0 8).
	self assert: ((2 to: 17) collect: [:m| (ℤ/m) count: [:x| x isPrimitiveRoot]]) = X.
	X withIndexDo: [:n :i|
		n > 0
			ifTrue: [self assert: (ℤ/(i+1)) primitiveRoot isPrimitiveRoot]
			ifFalse: [self should: [(ℤ/(i+1)) primitiveRoot] raise: Error]]! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersTrivial
	| R |
	R _ ℤ/1.
	self assert: R isTrivial.
	self assert: R size = 1.
	self assert: R zero = R one! !

!RingTest methodsFor: 'finite rings' stamp: 'len 8/5/2021 08:42:57'!
testModularIntegersUnitsIrreduciblesAndReducibles
	"From 'A Special Subring Associated with Irreducible Elements in the Ring of Z/nZ', Augustine Musukwa, Khumbo Kumwenda, Journal of Mathematics (2017)."
	| m R factors p |
	m _ (2 to: 100) atRandom.
	R _ ℤ/m.
	self assert: R units asSet size = m phi.
	factors _ m factorization.
	self assert: R irreducibles size = (m phi * ((factors asSet select: [:each| (factors occurrencesOf: each) > 1]) sum: [:each| 1/each] ifEmpty: [0])).
	p _ 10 atRandom nextPrime.
	m _ p ^ (2 to: 4) atRandom.
	R _ ℤ/m.
	self assert: R reducibles size = (R irreducibles size / (p - 1) - 1)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersUnitsIrreduciblesAndReduciblesExamples
	"Examples from 'A Special Subring Associated with Irreducible Elements in the Ring of Z/nZ', Augustine Musukwa, Khumbo Kumwenda, Journal of Mathematics (2017)."
	| R |
	R _ ℤ/8.
	self assert: (R units asSet collect: [:each| each representative]) = #(1 3 5 7) asSet.
	self assert: (R irreducibles asSet collect: [:each| each representative]) = #(2 6) asSet.
	R _ ℤ/36.
	self assert: (R units asSet collect: [:each| each representative]) = #(1 5 7 11 13 17 19 23 25 29 31 35) asSet.
	self assert: (R irreducibles asSet collect: [:each| each representative]) = #(2 3 10 14 15 21 22 26 33 34) asSet.
	R _ ℤ/25.
	self assert: R irreducibles size = 4.
	self assert: R reducibles isEmpty.
	R _ ℤ/32.
	self assert: R irreducibles size = 8.
	self assert: R reducibles size = 7.
	R _ ℤ/343.
	self assert: R irreducibles size = 42.
	self assert: R reducibles size = 6! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/30/2023 22:20:36'!
testPrimeFieldQuadraticResidues
	| F |
	F := PrimeField new: 1000 atRandom nextPrime.
	self assert: F atRandom squared isSquare.
	self assert: ((1000 count: [F atRandom isSquare]) between: 400 and: 600)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 10/14/2022 08:43:07'!
testPrimePowerRingCanonicalProjections
	"Canonical homomorphism from ℤ/<p^n> to ℤ/<p^m> with 1 <= m <= n."
	| p n m A B φ a b |
	p _ 100 atRandom nextPrime.
	n _ 4 atRandom.
	A _ ℤ/(p^n).
	m _ n atRandom.
	B _ ℤ/(p^m).
	φ _ A → B.
	a _ A atRandom.
	b _ φ value: a.
	self assert: (B includes: b).
	self assert: a representative \\ (p^m) = b representative.
	self testRingHomomorphism: φ! !

!RingTest methodsFor: 'finite rings' stamp: 'len 11/16/2023 21:54:00'!
testPrimePowerRingNilradicalAndUnits
	| R |
	R := PrimePowerRing new: #(2 3 5) atRandom to: 5 atRandom.
	self assert: R units size + R nilradical size = R size	! !

!RingTest methodsFor: 'finite rings' stamp: 'len 11/16/2023 21:54:00'!
testPrimePowerRingValuationAndUnit
	| R x |
	R _ PrimePowerRing new: 3 to: 5.
	x _ R !! 5.
	self assert: x valuation = 0.
	self assert: x normalization inverse = x.
	x _ R !! (3*3*5*2).
	self assert: x valuation = 2.
	self assert: x normalization inverse = (R !! 10).
	"this is undefined for an Euclidean valuation, but we define it at 0 to be +infinity in order to make it compatible with the p-adic valuation:"
	x _ R !! 0.
	self assert: x valuation = Infinity positive! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testZ6IsNotPID
	self assert: (ℤ/6) isPID not! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testZ6PrimeNeedNotBeIrreducible
	| R a |
	R _ ℤ/6.
	a _ R!!2.
	self assert: a isPrime.
	self assert: a isIrreducible not! !

!RingTest methodsFor: 'general' stamp: 'len 10/5/2022 17:39:40'!
testCanonicalMorphisms
	| R x Q |
	R _ ℚ polynomials.
	x _ R x.
	Q _ R / (x^2 + 1).
	self assert: (ℤ → R value: 4) = (R !! 4). "canonical morphism from initial object"
	self assert: (R → Q value: x^2 + 2) = Q one. "canonical morphism to quotient object"! !

!RingTest methodsFor: 'general' stamp: 'len 5/13/2020 06:06:15'!
testDivisionByUnits: R
	100 timesRepeat:
		[| x u |
		x _ R atRandomBits: 30.
		u _ R units atRandomBits: 30.
		self assert: x*u // u = x.
		self assert: x // u = (x * u inverse)]! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:34:55'!
testKrullDimension
	| R I |
	self assert: ℤ dimension = 1.
	self assert: ℚ dimension = 0.
	self assert: (ℤ polynomialsIn: 2) dimension = 3.
	R _ ℚ polynomialsIn: #(x y).
	I _ R * {R x ^ 2 + (R y ^ 2) - 1}.
	self assert: (R / I) dimension = 1! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:27'!
testQQ
	self testRing: ℚ! !

!RingTest methodsFor: 'general' stamp: 'len 5/13/2020 06:07:00'!
testRing: R
	self testRingAxioms: R.
	self testRingUnitsAndAssociates: R.
	self testDivisionByUnits: R! !

!RingTest methodsFor: 'general' stamp: 'len 5/13/2020 06:44:18'!
testRingAxioms: R
	| one zero a b c |
	one _ R one.
	zero _ R zero.
	100 timesRepeat:
		[a _ R atRandomBits: 20.
		b _ R atRandomBits: 20.
		c _ R atRandomBits: 20.
		self assert: a * one = a.
		self assert: one * a = a.
		self assert: zero * a = zero.
		self assert: a * zero = zero.
		self assert: zero + a = a.
		self assert: a + zero = a.
		"commutativity of addition:"
		self assert: a + b = (b + a).
		"associativity of addition:"
		self assert: a + b + c = (a + (b + c)).
		"associativity of multiplication:"
		self assert: a * b * c = (a * (b * c)).
		"distributivity:"
		self assert: (a + b) * c = (a * c + (b * c)).
		"commutativity of multiplication:"
		R isCommutative ifTrue: [self assert: a * b = (b * a)]]! !

!RingTest methodsFor: 'general' stamp: 'len 10/14/2022 08:43:37'!
testRingHomomorphism: φ
	| R S a b |
	R _ φ domain.
	S _ φ codomain.
	a _ R atRandomBits: 10.
	b _ R atRandomBits: 10.
	self assert: (φ value: R zero) = S zero.
	self assert: (φ value: R one) = S one.
	self assert: (φ value: a + b) = ((φ value: a) + (φ value: b)).
	self assert: (φ value: a * b) = ((φ value: a) * (φ value: b)) "this sometimes fails because f and -f are not equal regular functions, for example"! !

!RingTest methodsFor: 'general' stamp: 'len 7/30/2021 10:50:41'!
testRingUnitsAndAssociates: R
"	R size < 100 ifTrue: [^ self testSmallRingUnitsAndAssociates: R]."
	100 timesRepeat:
		[| x u |
		x _ R atRandomBits: 30.
		u _ (R atRandomBits: 30) normalization.
		self assert: x normalization isUnit.
		self assert: u normalization = u inverse.
		self assert: u * u normalization = R one.
		self assert: x normalized * x normalization inverse = x.
		self assert: x normalized = (x * u) normalized]! !

!RingTest methodsFor: 'general' stamp: 'len 7/30/2021 10:52:04'!
testSmallRingUnitsAndAssociates: R
	R do: [:x| self assert: x normalization isUnit].
	R units do: [:u| self assert: u normalization = u inverse].
	R units do: [:u| self assert: u * u inverse = R one].
	R do: [:x| self assert: x * x normalization = x normalized].
	R size < 80
		ifTrue:
			[R units do: [:u|
				R do: [:x| self assert: x normalized = (x * u) normalized]]]
		ifFalse:
			[R do: [:x|
				| u |
				u _ R atRandom normalization.
				self assert: x normalized = (x * u) normalized]]! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:35:15'!
testZZ
	self testRing: ℤ! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:35:28'!
testZZIdeals
	self assert: (ℤ * 3 includes: 6).
	self deny: (ℤ * 3 includes: 7).
	self assert: (ℤ * 0) isTrivial.
	self assert: (ℤ * 1) isOne! !

!RingTest methodsFor: 'local rings' stamp: 'len 9/16/2022 07:39:46'!
testLocalHomomorphism
	| f |
	f _ ℤ/4 to: ℤ/2 map: [:x| ℤ/2 !! x representative].
	self assert: f isLocal
! !

!RingTest methodsFor: 'localizations' stamp: 'len 7/23/2022 13:19:11'!
testLocalizationMaximalIdeal
	| Z2 |
	Z2 _ ℤ @ (ℤ * 2).
	self assert: Z2 maximalIdeal = (Z2 * (Z2 !! 2))! !

!RingTest methodsFor: 'localizations' stamp: 'len 12/22/2021 14:20:01'!
testLocalizationOfModularIntegers1
	| R |
	R _ ℤ/60.
	self assert: (R @ (R*2)) = (ℤ/4).
	self assert: (R @ (R*3)) = (ℤ/3).
	self assert: (R @ (R*5)) = (ℤ/5)! !

!RingTest methodsFor: 'localizations' stamp: 'len 10/14/2022 08:42:36'!
testLocalizationOfModularIntegers2
	| R P L φ |
	R _ ℤ/60.
	P _ R*2.
	L _ R @ P. "the localization is actually the prime power ring Z/4Z"
	"localization map:"
	φ _ R → L.
	self testRingHomomorphism: φ.
	"everything not in P maps to a unit:"
	R do: [:each| self assert: ((P includes: each) or: [(φ value: each) isUnit])]! !

!RingTest methodsFor: 'localizations' stamp: 'len 10/14/2022 08:42:47'!
testLocalizationOfModularIntegers3
	| R P L φ |
	R _ ℤ/60.
	P _ R*3.
	L _ R @ P. "the localization is actually the field GF(3)"
	"localization map:"
	φ _ R → L.
	self testRingHomomorphism: φ.
	"everything not in P maps to a unit:"
	R do: [:each| self assert: ((P includes: each) or: [(φ value: each) isUnit])]! !

!RingTest methodsFor: 'localizations' stamp: 'len 10/5/2022 17:40:57'!
testLocalizationOfZAt2
	| L |
	L _ ℤ @ (ℤ * 2).
	self assert: ℚ !! (L !! (1/3)) = (1/3).
	self should: [L !! (3/2)] raise: Error.
	self assert: ℚ !! (L !! 2) = 2.
	self assert: ℚ !! (L !! 3) = 3.
	self assert: (L atRandomBits: 10) denominator odd.
	self testRing: L.
	self testRingHomomorphism: ℤ → L! !

!RingTest methodsFor: 'polynomials' stamp: 'len 7/16/2021 09:39:46'!
testRationalFunctionReconstruction
	"Example taken from 'Fast Rational Function Reconstruction', Sara Khodadad, 2005."
	| F x f g h |
	F _ ℤ/11.
	x _ F polynomials x.
	f _ (1 to: 7) product: [:i| x - i].
	g _ x^6 + (x^5*3) + (x^4*8) + (x^3*4) + (x^2*6) + x + 9.
	h _ ((F polynomials / f) !! g) asRationalFunction.
	self assert: h numerator = (x^3*10 + 9).
	self assert: h denominator = (x + 2)
! !

!RingTest methodsFor: 'polynomials' stamp: 'len 10/5/2022 17:41:14'!
testRationalFunctions
	| R P F |
	R _ self newField.
	P _ R polynomials.
	F _ P fractions.
	self testRing: F.
	self testRingHomomorphism: R → F.
	self testRingHomomorphism: P → F! !

!RingTest methodsFor: 'polynomials' stamp: 'len 3/4/2021 15:03:00'!
testTensorProductOfPolynomialsRings
	self assert: (ℚ polynomialsIn: 2) ⊗ (ℚ polynomialsIn: 3) = (ℚ polynomialsIn: 5).
	self assert: ℤ polynomials ⊗ ℚ = ℚ polynomials.
	self assert: ℤ polynomials ⊗ ℚ polynomials = (ℚ polynomialsIn: 2).
	self assert: ℚ polynomials ⊗ ℤ polynomials = (ℚ polynomialsIn: 2).
	"TODO: test tensor map and coprojections"! !

!RingTest methodsFor: 'quotients' stamp: 'len 10/6/2016 20:32'!
testQuotientRing
	| Q |
	Q _ self newQuotientRing.
	self testRing: Q.
	self testRingHomomorphism: Q projection! !

!RingTest methodsFor: 'quotients' stamp: 'len 6/10/2022 11:38:50'!
testQuotientRing2
	| Q pi f g |
	Q _ self newQuotientRing.
	f _ Q cover atRandomBits: 10.
	g _ Q relations atRandomBits: 10.
	pi _ Q projection.
	self assert: (pi value: g) isZero.
	self assert: (Q includes: (pi value: f)).
	self testRingHomomorphism: pi.
"	self assert: (pi value: f) = (pi value: f + g)."
"	h _ Q atRandomBits: 100.
	self assert: (pi value: (pi section value: h)) = h"! !

!RingTest methodsFor: 'quotients' stamp: 'len 5/23/2020 09:33:01'!
testQuotientRingNotIntegralDomain
	| R |
	R _ ℚ polynomials.
	R _ R / (R x^2 - 1).
	self assert: R isField not.
	self assert: R isIntegralDomain not! !

!RingTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newField
	| R |
	R _ {ℚ. ℤ / 256 atRandom nextPrime} atRandom.
	"TODO: function fields, p-adics, power series"
"	R _ 2 atRandom = 1 ifTrue: [R extensionDegree: 3 atRandom + 1]."
"	2 atRandom = 1 ifTrue: [^ R polynomials fractions]."
	^ R! !

!RingTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newQuotientRing
	| R I |
	2 atRandom = 1 ifTrue: [^ ℤ / (2 to: 10) atRandom].
	R _ ℚ polynomials.
	I _ R * (R atRandomBits: 10 maxDegree: 5).
	^ R / I! !

!RingTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newRing
	4 atRandom = 1 ifTrue: [^ self newQuotientRing].
	2 atRandom = 1 ifTrue: [^ {ℤ. ℚ. ℤ / 64 atRandom} atRandom polynomialsIn: (1 to: 3) atRandom].
	^ {ℤ. ℚ. ℤ / 64 atRandom} atRandom! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/26/2022 08:04:25'!
test2Sphere
	| X |
	X _ SimplicialComplex sphere: 2.
	self assert: X dimension = 2.
	self assert: X fVector = #(1 4 6 4)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/26/2022 08:04:41'!
test5Sphere
	| X |
	X _ SimplicialComplex sphere: 5.
	self assert: X dimension = 5.
	self assert: X fVector = #(1 7 21 35 35 21 7)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 10:28:26'!
testAlexanderDual
	| X Y |
	X _ SimplicialComplex faces: #((0 1) (1 2) (2 3) (3 0)).
	self assert: X dual dual = X.
	Y _ SimplicialComplex faces: #((1 3) (0 2)).
	self assert: X dual = Y.
	self assert: X dual nonfaces = X complement facets! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersCircle
	| X |
	X _ SimplicialComplex sphere: 1.
	self assert: (X chainComplexOver: ℚ) betti = #(1 1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersKleinBottle
	| X |
	X _ SimplicialComplex kleinBottle.
	self assert: (X chainComplexOver: ℚ) betti = #(1 1 0)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersProjectivePlane
	| X |
	X _ SimplicialComplex realProjectivePlane.
	self assert: (X chainComplexOver: ℚ) betti = #(1 0 0)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersSphere
	| X |
	X _ SimplicialComplex sphere: 2.
	self assert: (X chainComplexOver: ℚ) betti = #(1 0 1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersTorus
	| X |
	X _ SimplicialComplex torus.
	self assert: (X chainComplexOver: ℚ) betti = #(1 2 1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testChainComplex
	"Jeff Erickson, 'Computational Topology', Example 17.7."
	| S2 C |
	S2 _ SimplicialComplex facets: ((Simplex vertices: #(w x y z)) faces: 2).
	C _ S2 chainComplexOver: ℤ.
	self assert: (C cycles: 1) rank = 3.
	self assert: (C boundaries: 0) rank = 3.
	self assert: (C boundaries: 1) = (C cycles: 1).
	self assert: C betti = #(1 0 1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/20/2017 08:37:27'!
testEulerCharacteristic
	| X |
	X _ SimplicialComplex torus.
	self assert: X euler = 0.
	X _ SimplicialComplex sphere: 2.
	self assert: X euler = 2.
	X _ SimplicialComplex kleinBottle.
	self assert: X euler = 0.
	X _ SimplicialComplex realProjectivePlane.
	self assert: X euler = 1! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 08:23:09'!
testJoin
	| S X |
	S _ SimplicialComplex faces: #((0) (1)).
	X _ S * S * S * S * S * S * S * S.
	self assert: X vertices size = 16.
	self assert: X facets size = 256
! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 08:23:15'!
testMinimalNonFaces
	| X |
	X _ SimplicialComplex faces: #((1 3) (1 2)).
	self assert: X nonfaces = {Simplex vertices: #(2 3)} asSet.
	X _ SimplicialComplex faces: #((0 1) (1 2) (2 3) (3 0)).
	self assert: X nonfaces = {Simplex vertices: #(1 3). Simplex vertices: #(0 2)} asSet
! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 08:23:18'!
testMinimalNonFaces2
	| X MNF |
	X _ SimplicialComplex faces: #((0 1 2) (0 2 3) (2 3 4) (1 2 4) (1 4 5) (0 3 6) (3 6 7) (4 5 7)).
	MNF _ (#((3 4 7) (0 7) (0 4) (0 5) (3 5) (1 7) (2 5) (5 6) (1 3) (4 6) (2 7) (2 6) (1 6)) collect: [:each| Simplex vertices: each]) asSet.
	self assert: X nonfaces = MNF! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 08:23:21'!
testProduct
	| C T |
	"Circle:"
	C _ SimplicialComplex faces: #((0 1) (1 2) (0 2)).
	"Torus:"
	T _ C × C.
	self assert: T euler = 0.
	self assert: T vertices size = 9.
	self assert: T facets size = 18! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/25/2017 10:08:54'!
testSimplex
	| tetrahedron |
	tetrahedron _ Simplex vertices: #(a b c d).
	self assert: tetrahedron dimension = 3.
	self assert: (tetrahedron faces: 2) size = 4.
	self assert: tetrahedron edges size = 6.
	self assert: (tetrahedron faces: 3) size = 1! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 08:37:30'!
testSimplexComplement
	| V |
	V _ (0 to: 5).
	self assert: ((Simplex vertices: #(1 2 3)) complementIn: V) = (Simplex vertices: #(0 4 5)).
	self assert: ((Simplex vertices: #(0 1 3 4)) complementIn: V) = (Simplex vertices: #(2 5)).
	self assert: ((Simplex vertices: #(0 4 1 3 )) complementIn: V) = (Simplex vertices: #(2 5))! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 3/7/2017 07:30:50'!
testSimplexInclusion
	| S S2 |
	S _ Simplex new: (2 to: 10) atRandom.
	S2 _ Simplex new: (0 to: S dimension) atRandom.
	self assert: S2 <= S! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/15/2017 16:21:54'!
testSimplexJoin
	| A B |
	A _ Simplex new: 2.
	B _ Simplex new: 3.
	self assert: A*B ~ (Simplex new: 2+1+3+1-1)
! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testSimplicialChainsBoundaryMap
	| X C d1 d2 |
	X _ SimplicialComplex sphere: 2.
	C _ X chainComplexOver: ℚ.
	d2 _ C at: 2.
	d1 _ C at: 1.
	self assert: d2 isZero not.
	self assert: d1 isZero not.
	self assert: (d1 · d2) isZero! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/21/2020 12:18:08'!
testSimplicialComplex
	| S |
	S _ SimplicialComplex facets: {{1. 2. 3}. {2. 4}. {3. 4}. {5}}.
	self assert: (S faces: 2) asSet size = 1.
	self assert: (S faces: 1) asSet size = 5.
	self assert: (S faces: 0) asSet size = 5! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/26/2022 07:45:36'!
testSimplicialComplexExample
	"Example from Herzog, 'Monomial Ideals', pag. 16."
	| X |
	X _ SimplicialComplex on: (1 to: 5) faces: #((1 2 4) (1 2 5) (2 3) (3 4)).
	self assert: X facets = (#((1 2 4) (1 2 5) (2 3) (3 4)) collect: [:each| Simplex vertices: each]) asSet.
	self assert: X nonfaces = (#((1 3) (3 5) (4 5) (2 3 4)) collect: [:each| Simplex vertices: each]) asSet.
	self assert: X fVector = #(1 5 7 2).
	self assert: X hVector = #(1 2 0 -1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 13:05:32'!
testSimplicialMap
	| S T f |
	S _ SimplicialComplex faces: #((0 2) (1 5) (3 4)).
	T _ SimplicialComplex faces: #((0 2) (1 3)).
	f _ S to: T vertexMap: ({0 -> 0. 1 -> 1. 2 -> 2. 3 -> 1. 4 -> 3. 5 -> 3} as: Dictionary).
	self assert: f image = T.
	self assert: f isSurjective.
	self assert: f isInjective not.
	self assert: f isIdentity not! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 10:49:02'!
testStanleyReisnerIdealAndBack
	| X |
	X _ SimplicialComplex on: (1 to: 5) faces: #((1 2 4) (1 2 5) (2 3) (3 4)).
	self assert: X stanleyReisnerIdeal asSimplicialComplex = X! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 18:14:18'!
testStanleyReisnerIdealAndBack2
	| X |
	X _ SimplicialComplex on: (1 to: 5) faces: #((1 3 2) (3 4) (2 4) (5)).
	self assert: X stanleyReisnerIdeal asSimplicialComplex = X! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 10:27:08'!
testStanleyReisnerIdealAndFacetIdealDuality
	| X |
	X _ SimplicialComplex on: (1 to: 5) faces: #((1 2 4) (1 2 5) (2 3) (3 4)).
	self assert: X dual stanleyReisnerIdeal = X complement facetIdeal! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 7/26/2022 13:44:41'!
testStanleyReisnerRing
	| X R |
	X _ SimplicialComplex faces: #((0 1) (1 2) (2 3) (3 0)).
	R _ ℚ polynomialsIn: #(x y z w).
	self assert: X stanleyReisnerRing relations generators asSet = (R !! [:x :y :z :w| {y*w. x*z}]) asSet! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/26/2022 08:04:52'!
testTorus
	| X |
	X _ SimplicialComplex torus.
	self assert: X dimension = 2.
	self assert: X fVector = #(1 7 21 14)! !

!WordArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/22/2023 17:07:20'!
testBitParityParallel
	| array parities |
	array := WordArray new: 32.
	1 to: 32 do: [:i| array at: i put: (0 to: 16rFFFFFFFF) atRandom].
	parities := 0.
	array reverseDo: [:each| parities := parities << 1 bitAnd: each bitParity].
	self assert: parities = array bitParityParallel! !

!WordArrayTest methodsFor: 'as yet unclassified' stamp: 'len 5/22/2023 17:10:47'!
testBitTranspose32x32
	| array1 array2 |
	array1 := (WordArray new: 32) at: 1 put: 3392231422; at: 2 put: 2917542836; at: 3 put: 2016986704; at: 4 put: 4023284951; at: 5 put: 3319131826; at: 6 put: 2609918512; at: 7 put: 2934033904; at: 8 put: 558419939; at: 9 put: 2466942702; at: 10 put: 3907522; at: 11 put: 2461608457; at: 12 put: 682014126; at: 13 put: 285397452; at: 14 put: 3023449861; at: 15 put: 3701423065; at: 16 put: 395901000; at: 17 put: 857111479; at: 18 put: 1789712607; at: 19 put: 1923901516; at: 20 put: 2388693829; at: 21 put: 828380017; at: 22 put: 1053284415; at: 23 put: 3860620701; at: 24 put: 2372246648; at: 25 put: 4191609509; at: 26 put: 3996225886; at: 27 put: 504614510; at: 28 put: 4191451967; at: 29 put: 2400004646; at: 30 put: 2158500940; at: 31 put: 287393905; at: 32 put: 269399837; yourself.
	array2 := (WordArray new: 32) at: 1 put: 3380307080; at: 2 put: 505613209; at: 3 put: 3211737355; at: 4 put: 2934365441; at: 5 put: 3404939391; at: 6 put: 1571883507; at: 7 put: 1721684941; at: 8 put: 21191643; at: 9 put: 2321119939; at: 10 put: 2635687863; at: 11 put: 2764057565; at: 12 put: 2091378642; at: 13 put: 2864437801; at: 14 put: 2571496506; at: 15 put: 1298632925; at: 16 put: 2276105172; at: 17 put: 1453344337; at: 18 put: 2233563914; at: 19 put: 2582079514; at: 20 put: 2957428620; at: 21 put: 255977013; at: 22 put: 1654533703; at: 23 put: 163053786; at: 24 put: 690408570; at: 25 put: 1502712250; at: 26 put: 376407401; at: 27 put: 384360538; at: 28 put: 531253359; at: 29 put: 3442865444; at: 30 put: 192358606; at: 31 put: 189153309; at: 32 put: 1002988923; yourself.
	self assert: array1 copy bitTranspose32x32 = array2! !

!DisabledTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 19:27:19'!
testIdealIsPrimeExample1
	| R x y I |
	R := ℚ polynomialsIn: #(x y).
	x := R x: 1. y := R x: 2.
	I := R * {x^2 - (y^2) - 1}.
	self assert: I isPrime! !

!DisabledTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 19:27:32'!
testIdealIsPrimeExample2
	"[AL94, Example 4.4.10]"
	| R x y z I |
	R := ℚ polynomialsIn: #(x y z).
	x := R x: 1. y := R x: 2. z := R x: 3.
	I := R * {x*z - (y^2). x^3 - (y*z). x^2*y - (z^2)}.
	self assert: I isPrime! !

!DisabledTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 19:27:43'!
testIdealIsPrimeExample3
	"[AL94, Exercise 4.4.4]"
	| R x y z I |
	R := ℚ polynomialsIn: #(x y z).
	x := R x: 1. y := R x: 2. z := R x: 3.
	I := R * {y^4 - (z^3). y^2 - (x*z). x*(y^2) - (z^2). x^2 - z}.
	self assert: I isPrime! !

!DisabledTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 19:27:57'!
testIdealIsPrimeExample4
	"[AL94, Exercise 4.4.3]"
	| R x y z I |
	R := ℚ polynomialsIn: #(x y z).
	x := R x: 1. y := R x: 2. z := R x: 3.
	I := R * {x*z - (y^3). y*z - (x^2)}.
	self assert: I isPrime not! !

!DisabledTest methodsFor: 'as yet unclassified' stamp: 'len 10/24/2023 16:10:00'!
testIdealRadicalZeroDimNotPerfectField
	"Counterexample that shows how Seidenberg algorithm fails over a non-perfect field.
	From Becker and Weispfenning 'Groebner Bases', Springer-Verlag, 1993. Example 8.16."
	| p F R t x y I |
	p := #(3 5 7 11) atRandom.
	F := (ℤ / p) polynomialsIn: #(t) :: fractions.
	R := F polynomialsIn: #(x y).
	t := F x: 1.
	x := R x.
	y := R y.
	I := R * {x^p - t. y^p - t}.
	self assert: (I includes: x^p - (y^p)).
	self deny: (I includes: x - y).
	self assert: (I radical includes: x - y)! !

!DisabledTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 16:46:15'!
testLocalAndGlobalDimension
	"'A Singluar Introduction to Commutative Algebra', Example A.8.4 (local and global dimension)."
	| R I X |
	R := ℚ polynomialsIn: #(x y z).
	I := R * (R !! [:x :y :z| {x*z. y*z}]).
	X := (R / I) spec.
	self assert: X dimension = 2. "global dimension of X"
	self assert: (X @ (0,0,0)) dimension = 2. "dimension of X at (0,0,0)"
	self assert: (X @ (1,0,0)) dimension = 2. "dimension of X at (1,0,0)"
	self assert: (X @ (0,0,1)) dimension = 1. "dimension of X at (0,0,1)"! !

!DisabledTest methodsFor: 'as yet unclassified' stamp: 'len 7/5/2023 16:46:41'!
testProjectivePlaneCurveArithmeticGenus
	| R f C |
	R :=  ℤ/5 polynomialsIn: 3.
	f := R !! [:x :y :z| y^2*(z^7) - (x^9) - (x*(z^8))].
	C := ProjectivePlaneCurve polynomial: f.
	self assert: C arithmeticGenus = 28.
	"self assert: C genus = 4"! !

!FiniteFieldRationalFunctionFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/9/2023 19:10:37'!
setUp
	super setUp.
	self flag: #fixme. "test 2 or 3 variables"
	scalars := ((	GaloisField new: (5 atRandom: random) nextPrime to: (3 atRandom: random)) polynomials"In: 3 atRandom: random") fractions! !

!RationalLocalPolynomialRingMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 10/18/2022 12:31:01'!
initialize
	super initialize.
	scalars _
		{[| R x y |
		R _ ℚ polynomialsIn: #(x y).
		x _ R x: 1. y _ R x: 2.
		R @ (R * {x. y})].
"		R @ (R * {x^2 - (y^2) - 1})]."

"		[| R x y z |
		R _ ℚ polynomialsIn: #(x y z).
		x _ R x: 1. y _ R x: 2. z _ R x: 3.
		R @ (R * {x*z - (y^2). x^3 - (y*z). x^2*y - (z^2)})].

		[| R x y z |
		R _ ℚ polynomialsIn: #(x y z).
		x _ R x: 1. y _ R x: 2. z _ R x: 3.
		R @ (R * {y^4 - (z^3). y^2 - (x*z). x*(y^2) - (z^2). x^2 - z})]"} atRandom value! !

!RationalRationalFunctionFieldMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 6/9/2023 19:16:36'!
setUp
	super setUp.
	scalars := (ℚ polynomialsIn: (3 atRandom: random)) fractions! !

!Benchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2016 04:20'!
run
	| output |
	output _ WriteStream on: (String new: 1000).
	(self class selectors asArray select: [:each|
		(each beginsWith: 'test') and: [each numArgs isZero]]) sort
		do: [:each|
			output nextPutAll: each; tab.
			self setUp.
			Smalltalk garbageCollect.
			[output nextPutAll: [self perform: each] bench] on: Error do: [output nextPutAll: 'FAILED'].
			self tearDown]
		separatedBy: [output newLine].
	^ output contents! !

!Benchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2016 04:16'!
setUp
	^ self! !

!Benchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2016 04:16'!
tearDown
	^ self! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/10/2023 14:03:56'!
testComplexAlgebraicNumberAddition
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 := (ℂ !! 2) sqrt.
	sqrt3 := (ℂ !! 3) sqrt.
	sqrt5 := (ℂ !! 5) sqrt.
	10 timesRepeat: [sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/10/2023 14:04:07'!
testComplexAlgebraicNumberDivision
	| sqrt3 sqrt5 |
	sqrt3 := (ℂ !! 3) sqrt.
	sqrt5 := (ℂ !! 5) sqrt.
	10 timesRepeat: [sqrt3 / sqrt5 / sqrt3 / sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/10/2023 14:04:18'!
testComplexAlgebraicNumberProduct
	| sqrt3 sqrt5 |
	sqrt3 := (ℂ !! 3) sqrt.
	sqrt5 := (ℂ !! 5) sqrt.
	10 timesRepeat: [sqrt3 * sqrt5 * sqrt3 * sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/10/2023 14:04:24'!
testComplexAlgebraicNumberSquare
	| sqrt2 |
	sqrt2 := (ℂ !! 2) sqrt.
	10 timesRepeat: [sqrt2 squared]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/10/2023 14:10:51'!
testRealAlgebraicNumberAddition
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 := (ℝ !! 2) sqrt.
	sqrt3 := (ℝ !! 3) sqrt.
	sqrt5 := (ℝ !! 5) sqrt.
	10 timesRepeat: [sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/10/2023 14:10:59'!
testRealAlgebraicNumberDivision
	| sqrt3 sqrt5 |
	sqrt3 := (ℝ !! 3) sqrt.
	sqrt5 := (ℝ !! 5) sqrt.
	10 timesRepeat: [sqrt3 / sqrt5 / sqrt3 / sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/10/2023 14:11:05'!
testRealAlgebraicNumberProduct
	| sqrt3 sqrt5 |
	sqrt3 := (ℝ !! 3) sqrt.
	sqrt5 := (ℝ !! 5) sqrt.
	10 timesRepeat: [sqrt3 * sqrt5 * sqrt3 * sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/10/2023 14:11:12'!
testRealAlgebraicNumberSquare
	| sqrt2 |
	sqrt2 := (ℝ !! 2) sqrt.
	10 timesRepeat: [sqrt2 squared]! !

!CharacteristicPolynomialOverIntegersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/25/2022 10:46:22'!
setUp
	M _ ℤ matrix: 80 @ 80 fill: [:i :j| (-20 to: 20) atRandom]! !

!CharacteristicPolynomialOverIntegersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/25/2022 10:49:34'!
testCharacteristicPolynomial
	M copy characteristicPolynomial! !

!DeterminantOverCommutativeRingBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/25/2022 10:57:20'!
setUp
	| n R |
	n _ 2003*1009.
	R _ (ℤ/n) polynomials.
	M _ R matrix: 80 @ 80 fill: [:i :j| R coefficients: ((1 to: 6) collect: [:k| R scalars !! (-100 to: 100) atRandom])]! !

!DeterminantOverCommutativeRingBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/25/2022 10:55:39'!
testDeterminant
	M copy determinant! !

!DeterminantOverPolynomialRingBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/25/2022 10:42:27'!
setUp
	| R |
	R _ ℤ polynomials.
	M _ R matrix: 40 @ 40 fill: [:i :j| R coefficients: ((1 to: 3) collect: [:k| (-20 to: 20) atRandom])]! !

!DeterminantOverPolynomialRingBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/25/2022 10:43:00'!
testDeterminant
	M copy determinant! !

!FatemanBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/25/2022 10:30:50'!
setUp
	| R |
	R _ ℤ polynomialsIn: #(x y z t).
	f _ 1 + R generators sum.
	p _ f^30! !

!FatemanBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/25/2022 10:31:11'!
testFateman
	p * (p + 1)! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:02'!
testCyclic3
	(ℚ polynomialsIn: 3) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:06'!
testCyclic4
	(ℚ polynomialsIn: 4) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:13'!
testCyclic5
	(ℚ polynomialsIn: 5) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testEliminationExample
	| Qx x y z I |
	Qx _ ℚ polynomialsIn: #(x y z).
	x _ Qx x. y _ Qx y. z _ Qx z.
	I _ Qx * {x^2 + y + z - 1. x + (y^2) + z - 1. x + y + (z^2) - 1}.
	I eliminateAll: #(1 2)! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:17'!
testKatsura2
	(ℚ polynomialsIn: 2) katsuraIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:21'!
testKatsura3
	(ℚ polynomialsIn: 3) katsuraIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:25'!
testKatsura4
	(ℚ polynomialsIn: 4) katsuraIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:29'!
testModularCyclic3
	((PrimeField new: 32003) polynomialsIn: 3) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:33'!
testModularCyclic4
	((PrimeField new: 32003) polynomialsIn: 4) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:38'!
testModularCyclic5
	((PrimeField new: 32003) polynomialsIn: 5) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:42'!
testModularKatsura2
	((PrimeField new: 32003) polynomialsIn: 2) katsuraIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:46'!
testModularKatsura3
	((PrimeField new: 32003) polynomialsIn: 3) katsuraIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:50'!
testModularKatsura4
	((PrimeField new: 32003) polynomialsIn: 4) katsuraIdeal basis! !

!GroupBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/15/2016 06:42'!
testJ1Order
	PermutationGroup J1 size! !

!GroupBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/15/2016 06:43'!
testM12Order
	PermutationGroup M12 size! !

!IntegersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 7/3/2016 04:28'!
test1000Factorial
	1000 factorial! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 6/9/2018 20:09:17'!
field: aField
	field _ aField! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/23/2022 21:40:24'!
setUp
	| random |
	random _ Random seed: 1234.
	field isNil ifTrue: [field _ 	PrimeField new: 32003].
	matrix _ field matrix: 300@300 fill: [:i :j| field atRandom: random]! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/22/2016 11:55:20'!
testAddition
	matrix + matrix! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/16/2022 20:14:37'!
testGaussianElimination
	matrix properties removeKey: #echelon ifAbsent: [].
	matrix echelon! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/16/2022 20:13:00'!
testImage
	matrix properties removeKey: #image ifAbsent: [].
	matrix image! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/16/2022 20:24:27'!
testKernel
	matrix properties removeKey: #coechelonAndTransformation ifAbsent: [].
	matrix kernel! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/22/2016 11:54:33'!
testSquared
	matrix squared! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 8/9/2020 07:55:46'!
testTransposition
	matrix transpose! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testAdditionOverZ
	| P f g |
	P _ ℤ polynomials.
	f _ P atRandomBits: 100 maxDegree: 50.
	g _ P atRandomBits: 100 maxDegree: 50.
	10000 timesRepeat: [f + g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testAdditionOverZ2
	| P f g |
	P _ ℤ / 2 :: polynomials.
	f _ P atRandomDegree: 50.
	g _ P atRandomDegree: 50.
	10000 timesRepeat: [f + g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testDivisionOverQ
	| P x f g |
	P _ ℚ polynomials.
	x _ P x.
	f _ x^2 + x + 1 ^ 10.
	g _ 1 - x ^ 10.
	2000 timesRepeat: [f // g. f \\ g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testDivisionOverZ
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x^2 + x + 1 ^ 10.
	g _ 1 - x ^ 10.
	2000 timesRepeat: [f // g. f \\ g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:40:46'!
testFactorizationOverZ17Degree100
	| n f |
	n _ 100.
	f _ (ℤ/17) polynomials !! [:x| ((1 to: n) sum: [:i| x^i*i]) + 1].
	f factorization! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:40:55'!
testFactorizationOverZ3Degree11Times20
	| F Fx x one f |
	F _ ℤ / 3.
	Fx _ F polynomials.
	x _ Fx x.
	one _ Fx one.
	f _ x^11 + (x^9 * 2) + (x^8 * 2) + (x^6) + (x^5) + (x^3 * 2) + (x^2 * 2) + one.
	20 timesRepeat: [f factorization]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/26/2018 19:28:38'!
testFiniteField
	"Benchmark Polynomial>>#= and ResidueClass#>>="
	| F |
	F _ GaloisField new: 9.
	((F^3) upTo: 9^3) asSet size = (9^3) ifFalse: [self halt]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testGCDOverQ
	| P x f g |
	P _ ℚ polynomials.
	x _ P x.
	f _ x^4 - 1 ^ 5.
	g _ x^6 - 1 ^ 5.
	2000 timesRepeat: [f gcd: g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testGCDOverZ
	| P x f g |
	P _ ℤ polynomials.
	x _ P x.
	f _ x^4 - 1 ^ 5.
	g _ x^6 - 1 ^ 5.
	2000 timesRepeat: [f gcd: g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testGCDOverZ2
	| P x f g |
	P _ ℤ / 2 :: polynomials.
	x _ P x.
	f _ x^4 - 1 ^ 5.
	g _ x^6 - 1 ^ 5.
	2000 timesRepeat: [f gcd: g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMultiplication2OverZ
	| P f g |
	P _ ℤ polynomials.
	f _ P atRandomBits: 100 maxDegree: 50.
	g _ P atRandomBits: 100 maxDegree: 50.
	100 timesRepeat: [f * g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/26/2018 18:13:05'!
testMultiplication2OverZ2
	| P f g |
	P _ PrimeField new: 2 :: polynomials.
	f _ P atRandomBits: 100 maxDegree: 50.
	g _ P atRandomBits: 100 maxDegree: 50.
	100 timesRepeat: [f * g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testMultiplicationOverZ
	| P x f |
	P _ ℚ polynomials.
	x _ P x.
	f _ x - 1.
	1 to: 9 do: [:i| f _ x - i * f * f]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/26/2018 18:13:19'!
testMultiplicationOverZ2
	| P x f |
	P _ PrimeField new: 2 :: polynomials.
	x _ P x.
	f _ x - (1 % 2).
	1 to: 9 do: [:i| f _ x - (i % 2) * f * f]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMultivariatePolynomialAdditionOverZ
	| P f g |
	P _ ℤ polynomialsIn: #(x y z).
	f _ P atRandomBits: 100 maxDegree: 50.
	g _ P atRandomBits: 100 maxDegree: 50.
	10000 timesRepeat: [f + g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivision
	| Zx x a b |
	Zx _ ℤ polynomials.
	x _ Zx x.
	a _ x^8 + (x^6) - (3*(x^4)) - (3*(x^3)) + (8*(x^2)) + (2*x) - 5.
	b _ 3*(x^6) + (5*(x^4)) - (4*(x^2)) - (9*x) + 21.
	20 timesRepeat: [(a pseudoDivisionBy: b) quotient]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testPseudoRemainder
	| Zx x a b |
	Zx _ ℤ polynomials.
	x _ Zx x.
	a _ x^8 + (x^6) - (3*(x^4)) - (3*(x^3)) + (8*(x^2)) + (2*x) - 5.
	b _ 3*(x^6) + (5*(x^4)) - (4*(x^2)) - (9*x) + 21.
	20 timesRepeat: [a pseudoRem: b]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 10/2/2023 19:44:49'!
testRationalRoots
	| x f |
	x := ℚ polynomials x.
	f := (x-(1/2))^2*(x-2)*(x^2 + 1)*(x^2-2).
	20 timesRepeat: [f roots]! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
setUp
	integerMatrix _ ℤ ^ (64@64) :: atRandomBits: 10000.
	binaryMatrix _ (PrimeField new: 2) ^ (64@64) :: atRandom.
	modularMatrix _ (PrimeField new: 32003) ^ (64@64) :: atRandom! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 7/8/2016 02:48'!
testAdditionZ
	integerMatrix + integerMatrix! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 7/8/2016 02:48'!
testAdditionZ2
	binaryMatrix + binaryMatrix! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 12/2/2016 09:24:56'!
testAdditionZp
	modularMatrix + modularMatrix! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 7/8/2016 02:49'!
testCopyZ
	integerMatrix copy! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 7/8/2016 02:50'!
testCopyZ2
	binaryMatrix copy! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 12/2/2016 09:24:46'!
testCopyZp
	modularMatrix copy! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/26/2018 20:41:21'!
testImageZ
	integerMatrix image! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2017 09:36:24'!
testImageZ2
	binaryMatrix image! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2017 09:36:52'!
testImageZp
	modularMatrix image! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/26/2018 20:41:50'!
testKernelZ
	integerMatrix kernel! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2017 09:36:17'!
testKernelZ2
	binaryMatrix kernel! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2017 09:36:46'!
testKernelZp
	modularMatrix kernel! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 7/8/2016 02:49'!
testMultiplicationZ
	integerMatrix * integerMatrix! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 7/8/2016 02:58'!
testMultiplicationZ2
	binaryMatrix * binaryMatrix! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 12/2/2016 09:24:37'!
testMultiplicationZp
	modularMatrix * modularMatrix! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 8/9/2020 07:56:33'!
testTranspositionZ
	integerMatrix transpose! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 8/9/2020 07:56:36'!
testTranspositionZ2
	binaryMatrix transpose! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 8/9/2020 07:56:39'!
testTranspositionZp
	modularMatrix transpose! !

!SparsePolynomialMultiplicationBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
setUp
	| P x y z |
	P _ ℤ polynomialsIn: #(x y z).
	x _ P x.
	y _ P y.
	z _ P z.
	p _ (1+x+y+z)^20.
	pBigCoefficients _ 10000000001*(1+x+y+z)^20. "big coefficients"
	pBigExponents _ 1 + (x^2147483647) + (y^2147483647) + (z^2147483647) ^ 20. "big exponents"
	pPlusOne _ p + 1.
	pBigCoefficientsPlusOne _ pBigCoefficients + 1.
	pBigExponentsPlusOne _ pBigExponents + 1! !

!SparsePolynomialMultiplicationBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/22/2016 11:48:43'!
testBigCoefficientsProduct
	pBigCoefficients * pBigCoefficientsPlusOne! !

!SparsePolynomialMultiplicationBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/22/2016 11:49:00'!
testBigExponentsProduct
	pBigExponents * pBigExponentsPlusOne! !

!SparsePolynomialMultiplicationBenchmark methodsFor: 'as yet unclassified' stamp: 'len 6/5/2016 08:44'!
testProduct
	p * pPlusOne! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 11:05:43'!
run
	| output |
	output _ WriteStream on: (String new: 1000).
	(self class selectors asArray select: [:each|
		(each beginsWith: 'test') and: [each numArgs isZero]]) sort
		do: [:each|
			output nextPutAll: each; tab.
			"self setUp."
			Smalltalk garbageCollect.
			[output nextPutAll: (self perform: each)] on: Error do: [output nextPutAll: 'FAILED'].
			"self tearDown"]
		separatedBy: [output newLine].
	^ output contents! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 18:35:51'!
testConvolution: a with: b into: c modulo: m
	c atAllPut: 0.
	1 to: a size do: [:i|
		1 to: b size do: [:j| | index |
			index _ i+j-1.
			c at: index put: (a at: i) * (b at: j) + (c at: index) \\ m]].
	^ c! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 18:02:38'!
testDotProduct: a with: b modulo: m
	| answer |
	answer _ 0.
	1 to: a size do: [:i| answer _ (a at: i) * (b at: i) + answer \\ m].
	^ answer! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 17:55:38'!
testEqualsBytes
	| random a b |
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: 256) - 1]) asByteArray.
	b _ a copy.
	^ [a = b] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 17:57:31'!
testEqualsWords
	| random a b |
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: 256) - 1]) asWordArray.
	b _ a copy.
	^ [a = b] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 17:57:05'!
testHashBytes
	| random a |
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: 256) - 1]) asByteArray.
	^ [a hash] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 17:57:21'!
testHashWords
	| random a |
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: 256) - 1]) asWordArray.
	^ [a hash] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 18:01:16'!
testModularConvolutionBytes
	| random p a b c |
	p _ 251.
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asByteArray.
	b _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asByteArray.
	c _ ByteArray new: 2000.
	^ [self testConvolution: a with: b into: c modulo: p] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 17:59:23'!
testModularConvolutionWords
	| random p a b c |
	p _ 32003.
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asWordArray.
	b _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asWordArray.
	c _ WordArray new: 2000.
	^ [self testConvolution: a with: b into: c modulo: p] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 18:03:18'!
testModularDotProductBytes
	| random p a b |
	p _ 251.
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asByteArray.
	b _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asByteArray.
	^ [self testDotProduct: a with: b modulo: p] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 18:03:50'!
testModularDotProductWords
	| random p a b |
	p _ 32003.
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asWordArray.
	b _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asWordArray.
	^ [self testDotProduct: a with: b modulo: p] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 18:01:23'!
testModularSumBytes
	| random p a b c |
	p _ 251.
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asByteArray.
	b _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asByteArray.
	c _ ByteArray new: 1000.
	^ [self testSum: a with: b into: c modulo: p] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 17:59:15'!
testModularSumWords
	| random p a b c |
	p _ 32003.
	random _ Random seed: 1234.
	a _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asWordArray.
	b _ ((1 to: 1000) collect: [:each| (random nextInteger: p) - 1]) asWordArray.
	c _ WordArray new: 1000.
	^ [self testSum: a with: b into: c modulo: p] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 19:09:54'!
testPermutate: a with: b into: c
	1 to: a size do: [:i| c at: i put: (b at: (a at: i))]! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 19:08:57'!
testPermutationCompositionArray
	| n a b c |
	n _ 1000.
	a _ (1 to: n) shuffled.
	b _ (1 to: n) shuffled.
	c _ Array new: n.
	^ [self testPermutate: a with: b into: c] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 11:11:52'!
testPermutationCompositionWords
	| n a b c |
	n _ 1000.
	a _ (1 to: n) shuffled asWordArray.
	b _ (1 to: n) shuffled asWordArray.
	c _ WordArray new: n.
	^ [self testPermutate: a with: b into: c] bench! !

!CASBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/19/2016 10:57:34'!
testSum: a with: b into: c modulo: m
	1 to: a size do: [:i| c at: i put: (a at: i) + (b at: i) \\ m].
	^ c! !
