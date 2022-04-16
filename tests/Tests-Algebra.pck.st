'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 2:18:23 pm'!
'Description '!
!provides: 'Tests-Algebra' 1 0!
SystemOrganization addCategory: #'Tests-Algebra'!
SystemOrganization addCategory: #'Tests-Algebra-Tuples'!
SystemOrganization addCategory: #'Tests-Algebra-Matrices'!
SystemOrganization addCategory: #'Tests-Algebra-Modules'!
SystemOrganization addCategory: #'Tests-Algebra-Rings'!
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

!classDefinition: #AbstractTestCase category: #'Tests-Algebra'!
TestCase subclass: #AbstractTestCase
	instanceVariableNames: 'random'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'AbstractTestCase class' category: #'Tests-Algebra'!
AbstractTestCase class
	instanceVariableNames: ''!

!classDefinition: #AbstractTupleTest category: #'Tests-Algebra-Tuples'!
AbstractTestCase subclass: #AbstractTupleTest
	instanceVariableNames: 'scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'AbstractTupleTest class' category: #'Tests-Algebra-Tuples'!
AbstractTupleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractTupleOverCommutativeRingTest category: #'Tests-Algebra-Tuples'!
AbstractTupleTest subclass: #AbstractTupleOverCommutativeRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'AbstractTupleOverCommutativeRingTest class' category: #'Tests-Algebra-Tuples'!
AbstractTupleOverCommutativeRingTest class
	instanceVariableNames: ''!

!classDefinition: #TuplesOverGaloisRingTest category: #'Tests-Algebra-Tuples'!
AbstractTupleOverCommutativeRingTest subclass: #TuplesOverGaloisRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TuplesOverGaloisRingTest class' category: #'Tests-Algebra-Tuples'!
TuplesOverGaloisRingTest class
	instanceVariableNames: ''!

!classDefinition: #TuplesOverIntegersTest category: #'Tests-Algebra-Tuples'!
AbstractTupleOverCommutativeRingTest subclass: #TuplesOverIntegersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TuplesOverIntegersTest class' category: #'Tests-Algebra-Tuples'!
TuplesOverIntegersTest class
	instanceVariableNames: ''!

!classDefinition: #TuplesOverModularIntegersTest category: #'Tests-Algebra-Tuples'!
AbstractTupleOverCommutativeRingTest subclass: #TuplesOverModularIntegersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TuplesOverModularIntegersTest class' category: #'Tests-Algebra-Tuples'!
TuplesOverModularIntegersTest class
	instanceVariableNames: ''!

!classDefinition: #TuplesOverBinaryFieldTest category: #'Tests-Algebra-Tuples'!
TuplesOverModularIntegersTest subclass: #TuplesOverBinaryFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TuplesOverBinaryFieldTest class' category: #'Tests-Algebra-Tuples'!
TuplesOverBinaryFieldTest class
	instanceVariableNames: ''!

!classDefinition: #TuplesOverPolynomialsOverRationalsTest category: #'Tests-Algebra-Tuples'!
AbstractTupleOverCommutativeRingTest subclass: #TuplesOverPolynomialsOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TuplesOverPolynomialsOverRationalsTest class' category: #'Tests-Algebra-Tuples'!
TuplesOverPolynomialsOverRationalsTest class
	instanceVariableNames: ''!

!classDefinition: #TuplesOverMatricesTest category: #'Tests-Algebra-Tuples'!
AbstractTupleTest subclass: #TuplesOverMatricesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TuplesOverMatricesTest class' category: #'Tests-Algebra-Tuples'!
TuplesOverMatricesTest class
	instanceVariableNames: ''!

!classDefinition: #TuplesOverQuaternionsTest category: #'Tests-Algebra-Tuples'!
AbstractTupleTest subclass: #TuplesOverQuaternionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Tuples'!
!classDefinition: 'TuplesOverQuaternionsTest class' category: #'Tests-Algebra-Tuples'!
TuplesOverQuaternionsTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractMatrixTest category: #'Tests-Algebra-Matrices'!
AbstractTestCase subclass: #AbstractMatrixTest
	instanceVariableNames: 'scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractMatrixTest class' category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractMatrixOverCommutativeRingTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #AbstractMatrixOverCommutativeRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractMatrixOverCommutativeRingTest class' category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverCommutativeRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractMatrixOverIntegralDomainTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverCommutativeRingTest subclass: #AbstractMatrixOverIntegralDomainTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractMatrixOverIntegralDomainTest class' category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractMatrixOverFieldTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #AbstractMatrixOverFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'AbstractMatrixOverFieldTest class' category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverFieldTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverCyclotomicFieldTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverFieldTest subclass: #MatrixOverCyclotomicFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverCyclotomicFieldTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverCyclotomicFieldTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverFiniteFieldTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverFieldTest subclass: #MatrixOverFiniteFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverFiniteFieldTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverFiniteFieldTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverQuadraticFieldTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverFieldTest subclass: #MatrixOverQuadraticFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverQuadraticFieldTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverQuadraticFieldTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverRationalsTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverFieldTest subclass: #MatrixOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverRationalsTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverRationalsTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverCyclotomicRingTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #MatrixOverCyclotomicRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverCyclotomicRingTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverCyclotomicRingTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverGaussianIntegersTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #MatrixOverGaussianIntegersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverGaussianIntegersTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverGaussianIntegersTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverIntegersTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #MatrixOverIntegersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverIntegersTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverIntegersTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverMultivariatePolynomialsOverPrimeFieldTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #MatrixOverMultivariatePolynomialsOverPrimeFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverMultivariatePolynomialsOverPrimeFieldTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverMultivariatePolynomialsOverPrimeFieldTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverMultivariatePolynomialsOverRationalsTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #MatrixOverMultivariatePolynomialsOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverMultivariatePolynomialsOverRationalsTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverMultivariatePolynomialsOverRationalsTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverPolynomialsOverPrimeFieldTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #MatrixOverPolynomialsOverPrimeFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverPolynomialsOverPrimeFieldTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverPolynomialsOverPrimeFieldTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverPolynomialsOverRationalsTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #MatrixOverPolynomialsOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverPolynomialsOverRationalsTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverPolynomialsOverRationalsTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverQuadraticRingTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverIntegralDomainTest subclass: #MatrixOverQuadraticRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverQuadraticRingTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverQuadraticRingTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverGaloisRingTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverCommutativeRingTest subclass: #MatrixOverGaloisRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverGaloisRingTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverGaloisRingTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverModularIntegersTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverCommutativeRingTest subclass: #MatrixOverModularIntegersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverModularIntegersTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverModularIntegersTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverPrimePowerRingTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverCommutativeRingTest subclass: #MatrixOverPrimePowerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverPrimePowerRingTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverPrimePowerRingTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverProductRingTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixOverCommutativeRingTest subclass: #MatrixOverProductRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverProductRingTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverProductRingTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverCliffordAlgebraTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #MatrixOverCliffordAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverCliffordAlgebraTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverCliffordAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverGroupAlgebraTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #MatrixOverGroupAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverGroupAlgebraTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverGroupAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverMatricesTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #MatrixOverMatricesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverMatricesTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverMatricesTest class
	instanceVariableNames: ''!

!classDefinition: #MatrixOverQuaternionsTest category: #'Tests-Algebra-Matrices'!
AbstractMatrixTest subclass: #MatrixOverQuaternionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Matrices'!
!classDefinition: 'MatrixOverQuaternionsTest class' category: #'Tests-Algebra-Matrices'!
MatrixOverQuaternionsTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractModuleTest category: #'Tests-Algebra-Modules'!
AbstractTestCase subclass: #AbstractModuleTest
	instanceVariableNames: 'scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'AbstractModuleTest class' category: #'Tests-Algebra-Modules'!
AbstractModuleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractCommutativeModuleTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #AbstractCommutativeModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'AbstractCommutativeModuleTest class' category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractVectorSpaceTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #AbstractVectorSpaceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'AbstractVectorSpaceTest class' category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest class
	instanceVariableNames: ''!

!classDefinition: #VectorSpaceOverBinaryFieldTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #VectorSpaceOverBinaryFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'VectorSpaceOverBinaryFieldTest class' category: #'Tests-Algebra-Modules'!
VectorSpaceOverBinaryFieldTest class
	instanceVariableNames: ''!

!classDefinition: #VectorSpaceOverCyclotomicFieldTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #VectorSpaceOverCyclotomicFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'VectorSpaceOverCyclotomicFieldTest class' category: #'Tests-Algebra-Modules'!
VectorSpaceOverCyclotomicFieldTest class
	instanceVariableNames: ''!

!classDefinition: #VectorSpaceOverFiniteFieldTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #VectorSpaceOverFiniteFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'VectorSpaceOverFiniteFieldTest class' category: #'Tests-Algebra-Modules'!
VectorSpaceOverFiniteFieldTest class
	instanceVariableNames: ''!

!classDefinition: #VectorSpaceOverQuadraticFieldTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #VectorSpaceOverQuadraticFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'VectorSpaceOverQuadraticFieldTest class' category: #'Tests-Algebra-Modules'!
VectorSpaceOverQuadraticFieldTest class
	instanceVariableNames: ''!

!classDefinition: #VectorSpaceOverRationalsTest category: #'Tests-Algebra-Modules'!
AbstractVectorSpaceTest subclass: #VectorSpaceOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'VectorSpaceOverRationalsTest class' category: #'Tests-Algebra-Modules'!
VectorSpaceOverRationalsTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverCyclotomicRingTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #ModuleOverCyclotomicRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverCyclotomicRingTest class' category: #'Tests-Algebra-Modules'!
ModuleOverCyclotomicRingTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverGaloisRingTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #ModuleOverGaloisRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverGaloisRingTest class' category: #'Tests-Algebra-Modules'!
ModuleOverGaloisRingTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverGaussianIntegersTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #ModuleOverGaussianIntegersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverGaussianIntegersTest class' category: #'Tests-Algebra-Modules'!
ModuleOverGaussianIntegersTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverIntegersTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #ModuleOverIntegersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverIntegersTest class' category: #'Tests-Algebra-Modules'!
ModuleOverIntegersTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverModularIntegersTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #ModuleOverModularIntegersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverModularIntegersTest class' category: #'Tests-Algebra-Modules'!
ModuleOverModularIntegersTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverMultivariatePolynomialsOverRationalsTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #ModuleOverMultivariatePolynomialsOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverMultivariatePolynomialsOverRationalsTest class' category: #'Tests-Algebra-Modules'!
ModuleOverMultivariatePolynomialsOverRationalsTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverPolynomialsOverRationalsTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #ModuleOverPolynomialsOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverPolynomialsOverRationalsTest class' category: #'Tests-Algebra-Modules'!
ModuleOverPolynomialsOverRationalsTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverQuadraticRingTest category: #'Tests-Algebra-Modules'!
AbstractCommutativeModuleTest subclass: #ModuleOverQuadraticRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverQuadraticRingTest class' category: #'Tests-Algebra-Modules'!
ModuleOverQuadraticRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractModuleTODOTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #AbstractModuleTODOTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'AbstractModuleTODOTest class' category: #'Tests-Algebra-Modules'!
AbstractModuleTODOTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverCliffordAlgebraTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #ModuleOverCliffordAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverCliffordAlgebraTest class' category: #'Tests-Algebra-Modules'!
ModuleOverCliffordAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverMatricesTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #ModuleOverMatricesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverMatricesTest class' category: #'Tests-Algebra-Modules'!
ModuleOverMatricesTest class
	instanceVariableNames: ''!

!classDefinition: #ModuleOverQuaternionsTest category: #'Tests-Algebra-Modules'!
AbstractModuleTest subclass: #ModuleOverQuaternionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Modules'!
!classDefinition: 'ModuleOverQuaternionsTest class' category: #'Tests-Algebra-Modules'!
ModuleOverQuaternionsTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractRingTest category: #'Tests-Algebra-Rings'!
AbstractTestCase subclass: #AbstractRingTest
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

!classDefinition: #AffineAlgebraTest category: #'Tests-Algebra-Rings'!
AbstractAlgebraTest subclass: #AffineAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AffineAlgebraTest class' category: #'Tests-Algebra-Rings'!
AffineAlgebraTest class
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

!classDefinition: #AbstractEuclideanRingTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #AbstractEuclideanRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractEuclideanRingTest class' category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest class
	instanceVariableNames: ''!

!classDefinition: #EuclideanProductRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #EuclideanProductRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'EuclideanProductRingTest class' category: #'Tests-Algebra-Rings'!
EuclideanProductRingTest class
	instanceVariableNames: ''!

!classDefinition: #GaloisRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #GaloisRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'GaloisRingTest class' category: #'Tests-Algebra-Rings'!
GaloisRingTest class
	instanceVariableNames: ''!

!classDefinition: #GaussianRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #GaussianRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'GaussianRingTest class' category: #'Tests-Algebra-Rings'!
GaussianRingTest class
	instanceVariableNames: ''!

!classDefinition: #LocalIntegerRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #LocalIntegerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'LocalIntegerRingTest class' category: #'Tests-Algebra-Rings'!
LocalIntegerRingTest class
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

!classDefinition: #PolynomialRingOverFiniteFieldTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #PolynomialRingOverFiniteFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'PolynomialRingOverFiniteFieldTest class' category: #'Tests-Algebra-Rings'!
PolynomialRingOverFiniteFieldTest class
	instanceVariableNames: ''!

!classDefinition: #PolynomialRingOverRationalsTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #PolynomialRingOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'PolynomialRingOverRationalsTest class' category: #'Tests-Algebra-Rings'!
PolynomialRingOverRationalsTest class
	instanceVariableNames: ''!

!classDefinition: #PrimePowerRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #PrimePowerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'PrimePowerRingTest class' category: #'Tests-Algebra-Rings'!
PrimePowerRingTest class
	instanceVariableNames: ''!

!classDefinition: #RationalIntegerRingTest category: #'Tests-Algebra-Rings'!
AbstractEuclideanRingTest subclass: #RationalIntegerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'RationalIntegerRingTest class' category: #'Tests-Algebra-Rings'!
RationalIntegerRingTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractIntegralDomainTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #AbstractIntegralDomainTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'AbstractIntegralDomainTest class' category: #'Tests-Algebra-Rings'!
AbstractIntegralDomainTest class
	instanceVariableNames: ''!

!classDefinition: #MultivariatePolynomialRingOverFiniteFieldTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #MultivariatePolynomialRingOverFiniteFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'MultivariatePolynomialRingOverFiniteFieldTest class' category: #'Tests-Algebra-Rings'!
MultivariatePolynomialRingOverFiniteFieldTest class
	instanceVariableNames: ''!

!classDefinition: #MultivariatePolynomialRingOverRationalsTest category: #'Tests-Algebra-Rings'!
AbstractCommutativeRingTest subclass: #MultivariatePolynomialRingOverRationalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Rings'!
!classDefinition: 'MultivariatePolynomialRingOverRationalsTest class' category: #'Tests-Algebra-Rings'!
MultivariatePolynomialRingOverRationalsTest class
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

!classDefinition: #AlgebraicNumbersTest category: #'Tests-Algebra'!
TestCase subclass: #AlgebraicNumbersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'AlgebraicNumbersTest class' category: #'Tests-Algebra'!
AlgebraicNumbersTest class
	instanceVariableNames: ''!

!classDefinition: #BilinearFormTest category: #'Tests-Algebra'!
TestCase subclass: #BilinearFormTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'BilinearFormTest class' category: #'Tests-Algebra'!
BilinearFormTest class
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

!classDefinition: #GroupTest category: #'Tests-Algebra'!
TestCase subclass: #GroupTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'GroupTest class' category: #'Tests-Algebra'!
GroupTest class
	instanceVariableNames: ''!

!classDefinition: #LinearGroupsTest category: #'Tests-Algebra'!
TestCase subclass: #LinearGroupsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'LinearGroupsTest class' category: #'Tests-Algebra'!
LinearGroupsTest class
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

!classDefinition: #ModuleTest category: #'Tests-Algebra'!
TestCase subclass: #ModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'ModuleTest class' category: #'Tests-Algebra'!
ModuleTest class
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

!classDefinition: #MultivariateModuleTest category: #'Tests-Algebra'!
TestCase subclass: #MultivariateModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'MultivariateModuleTest class' category: #'Tests-Algebra'!
MultivariateModuleTest class
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

!classDefinition: #PAdicsTest category: #'Tests-Algebra'!
TestCase subclass: #PAdicsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'PAdicsTest class' category: #'Tests-Algebra'!
PAdicsTest class
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

!classDefinition: #PowerSeriesTest category: #'Tests-Algebra'!
TestCase subclass: #PowerSeriesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'PowerSeriesTest class' category: #'Tests-Algebra'!
PowerSeriesTest class
	instanceVariableNames: ''!

!classDefinition: #QuadraticFormTest category: #'Tests-Algebra'!
TestCase subclass: #QuadraticFormTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'QuadraticFormTest class' category: #'Tests-Algebra'!
QuadraticFormTest class
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

!classDefinition: #SequenceTest category: #'Tests-Algebra'!
TestCase subclass: #SequenceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'SequenceTest class' category: #'Tests-Algebra'!
SequenceTest class
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

!classDefinition: #TupleTest category: #'Tests-Algebra'!
TestCase subclass: #TupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra'!
!classDefinition: 'TupleTest class' category: #'Tests-Algebra'!
TupleTest class
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

!classDefinition: #TupleConvolutionBenchmark category: #'Tests-Algebra-Benchmarks'!
Benchmark subclass: #TupleConvolutionBenchmark
	instanceVariableNames: 'a b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Algebra-Benchmarks'!
!classDefinition: 'TupleConvolutionBenchmark class' category: #'Tests-Algebra-Benchmarks'!
TupleConvolutionBenchmark class
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

!SparsePolynomialMultiplicationBenchmark commentStamp: '<historical>' prior: 0!
Timings giving in [1] on computers with 0.8ghz - 1ghz intel cpus: general purpose system 20s-100s, tuned systems 2.3s-10s. For big coefficients: singular 28s, pari 77s.

[1] R.J. Fateman, Comparing the speed of sparse polynomial multiplication.


self new run

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

!TupleConvolutionBenchmark commentStamp: 'len 6/6/2019 09:09:54' prior: 0!
self new run



testConvolution	3.02  runs per second!

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

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2020 08:10:13'!
testAbelianGroupHomomorphism: phi
	| G H a b |
	G _ phi domain.
	H _ phi codomain.
	a _ G atRandomBits: 100.
	b _ G atRandomBits: 100.
	self assert: (phi value: G identity) = H identity.
	self assert: (phi value: a + b) = ((phi value: a) + (phi value: b))! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:14:44'!
testAsPermutationGroup
	| A |
	A _ (É/2 ◊ (É/5) ◊ (É/4) ◊ (É/2)) asAbelianGroup.
	self assert: A asPermutationGroup asAbelianGroup invariants = A invariants! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2020 11:54:27'!
testDirectSum
	| A B C |
	A _ É asAbelianGroup.
	B _ (É / (2 to: 10) atRandom) asAbelianGroup.
	C _ A ê B.
	self testAbelianGroup: C.
	C -< (A, B) do: [:each| self testAbelianGroupHomomorphism: each].
	(A, B) >- C do: [:each| self testAbelianGroupHomomorphism: each]! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:15:41'!
testExponent
	| G |
	G _ (É/2 ◊ (É/3) ◊ (É/7)) asAbelianGroup.
	self assert: G exponent = 42.
	G _ (É/2 ◊ (É/4) ◊ (É/6)) asAbelianGroup.
	self assert: G exponent = 12! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:16:12'!
testFreeAbelianGroup
	| G |
	G _ É^2.
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
	M _ É ^ (1 to: 10) atRandom.
	self testAbelianGroup: M asAbelianGroup.
	self testAbelianGroupHomomorphism: (M projection: (1 to: M rank) atRandom) asAbelianGroupMap! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2020 08:16:40'!
testIntegers
	| G x1 x2 g1 g2 |
	G _ É asAbelianGroup.
	self testAbelianGroup: G.
	x1 _ É atRandomBits: 100.
	x2 _ É atRandomBits: 100.
	g1 _ G !! x1.
	g2 _ G !! x2.
	self assert: É !! g1 = x1.
	self assert: É !! G identity = 0.
	self assert: É !! (g1 + g2) = (x1 + x2).
	self assert: É !! g1 negated = x1 negated.! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/22/2020 06:50:23'!
testInvariants
	self assert: (É/1 ◊ (É/3) ◊ (É/5) ◊ (É/12)) asAbelianGroup invariants = #(3 60).
	self assert: (É/2 ◊ (É/3)) asAbelianGroup invariants = #(6).
	self assert: (É/6) asAbelianGroup invariants = #(6).
	self assert: (É/2 ◊ (É/6)) asAbelianGroup invariants = #(2 6).
	self assert: (É/3 ◊ (É/4) ◊ (É/5)) asAbelianGroup invariants = #(60).
	self assert: (É ◊ (É/6)) asAbelianGroup invariants = #(0 6)! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2020 09:02:52'!
testInvariantsAndPrimaryInvariants
	| invariants primaryInvariants A |
	invariants _ {2^2*3*(5^2). 2^5*(3^3)*(5^2)}.
	primaryInvariants _ {2^2. 2^5. 3. 3^3. 5^2. 5^2} asBag.
	A _ É^0.
	A _ invariants shuffled inject: É^0 into: [:sum :each| sum ê (É/each) asAbelianGroup].
	self assert: A invariants = invariants.
	self assert: A primaryInvariants = primaryInvariants.
	A _ primaryInvariants asArray shuffled inject: É^0 into: [:sum :each| sum ê (É/each) asAbelianGroup].
	self assert: A invariants = invariants.
	self assert: A primaryInvariants = primaryInvariants! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/22/2020 06:47:24'!
testIsCyclic
	| G |
	G _ (É/2 ◊ (É/3)) asAbelianGroup.
	self assert: G isCyclic.
	G _ (É/6) asAbelianGroup.
	self assert: G isCyclic.
	G _ (É/2 ◊ (É/2)) asAbelianGroup.
	self assert: G isCyclic not.
	G _ (É/2 ◊ (É/4)) asAbelianGroup.
	self assert: G isCyclic not.
	G _ É^0.
	self assert: G isCyclic.
	G _ (É/1) asAbelianGroup.
	self assert: G isCyclic.
	G _ (É/3 ◊ (É/4) ◊ (É/5)) asAbelianGroup.
	self assert: G isCyclic! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/22/2020 06:45:43'!
testIsTrivial
	| G |
	G _ É^0.
	self assert: G isTrivial.
	G _ (É/1) asAbelianGroup.
	self assert: G isTrivial.
	G _ (É/1 ◊ (É/1)) asAbelianGroup.
	self assert: G isTrivial.
	G _ (É/1 ◊ (É/1) ◊ (É/2)) asAbelianGroup.
	self assert: G isTrivial not.
	G _ (É/1 ◊ (É/1) ◊ É) asAbelianGroup.
	self assert: G isTrivial not.
	G _ (É/2) asAbelianGroup.
	self assert: G isTrivial not.
	G _ É asAbelianGroup.
	self assert: G isTrivial not! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:24:31'!
testMixedAbelianGroup
	| G |
	G _ (É ◊ É ◊ (É/2) ◊ (É/3)) asAbelianGroup.
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
	R _ É / (2 to: 10) atRandom.
	self testAbelianGroup: R asAbelianGroup.
	self testAbelianGroupHomomorphism: R projection asAbelianGroupMap! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:39:42'!
testOrder
	self assert: (É/2 ◊ (É/3)) asAbelianGroup size = 6.
	self assert: (É ◊ (É/2) ◊ (É/3)) asAbelianGroup size isInfinite! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/25/2020 09:10:40'!
testOrderOfElements
	| G g |
	G _ (É ◊ (É/2) ◊ (É/3) ◊ (É/4) ◊ (É/5)) asAbelianGroup.
	g _ G !! [:a :b :c :d :e| a + (b*2) + (d*4)].
	self assert: g order = 30.
	g _ G !! [:a :b :c :d :e| a + e].
	self assert: g order = 0! !

!AbelianGroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 19:26:02'!
testTorsionAbelianGroup
	| G |
	G _ (É/2 ◊ (É/3)) asAbelianGroup.
	self assert: G rank = 0.
	self assert: G primaryInvariants = #(2 3) asBag.
	self assert: G = G torsion.
	self assert: G isTorsion.
	self assert: G isFree not.
	self assert: G isMixed not.
	self assert: G torsionFree isTrivial! !

!AbstractTestCase methodsFor: 'setUp/tearDown' stamp: 'len 4/20/2021 06:52:42'!
setUp
	super setUp.
	random _ self random! !

!AbstractTestCase methodsFor: 'setUp/tearDown' stamp: 'len 4/20/2021 06:56:32'!
tearDown
	super tearDown.
	random _ nil! !

!AbstractTestCase class methodsFor: 'testing' stamp: 'len 4/20/2021 06:53:33'!
isAbstract
	^ self name beginsWith: 'Abstract'! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:17:19'!
testAddition
	| n a b c |
	n _ 5 atRandom: random.
	a _ scalars^n atRandom: random bits: 30.
	b _ scalars^n atRandom: random bits: 30.
	c _ a + b.
	self assert: c size = n.
	1 to: n do: [:i| self assert: (a at: i) + (b at: i) = (c at: i)].
	self assert: (a + a negated) isZero.
	self assert: a + b = (b + a).
	self assert: a + a = (a*2).
	self assert: a - b = (a + b negated)! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:16:59'!
testScalarAction
	| n a b x |
	n _ 5 atRandom: random.
	a _ scalars^n atRandom: random bits: 30.
	x _ scalars atRandom: random bits: 10.
	b _ a * x.
	self assert: b size = n.
	1 to: n do: [:i| self assert: (a at: i)*x = (b at: i)]! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:25:15'!
testTupleReversed
	| v i |
	v _ (scalars^(50 atRandom: random)) atRandom: random bits: 200.
	i _ v size atRandom: random.
	self assert: (v reversed at: i) = (v at: v size - i + 1).
	self assert: v reversed reversed = v! !

!AbstractTupleTest methodsFor: 'as yet unclassified' stamp: 'len 4/19/2021 10:18:33'!
testTuplesOfDifferentSize
	| a b zeros |
	a _ scalars^5 atRandom atRandomBits: 10.
	b _ scalars^5 atRandom atRandomBits: 10.
	zeros _ (scalars^5 atRandom) zero.
	self assert: a + (b,zeros) = (a + b).
	self assert: (a,zeros) + b = (a + b).
	self assert: a - (b,zeros) = (a - b).
	self assert: (a,zeros) - b = (a - b).
	self assert: a = (a,zeros).
	self assert: a,zeros = a.
	self assert: a hash = (a,zeros) hash.
	self assert: a ∑ (b,zeros) = (a ∑ b).
	self assert: a,zeros ∑ b = (a ∑ b)! !

!AbstractTupleOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/2/2021 12:30:57'!
testCrossProduct
	| V a b c r |
	V _ scalars^3.
	a _ V atRandom: random bits: 20.
	b _ V atRandom: random bits: 20.
	c _ V atRandom: random bits: 20.
	r _ V scalars atRandom: random bits: 10.
	self assert: (a ◊ a) isZero.
	self assert: a ◊ b = (b ◊ a) negated.
	self assert: a ◊ (b + c) = (a ◊ b + (a ◊ c)).
	self assert: (a*r) ◊ b = (a ◊ (b*r)).
	self assert: (a*r) ◊ b = (a ◊ b * r).
	self assert: (a ◊ (b ◊ c) + (b ◊ (c ◊ a)) + (c ◊ (a ◊ b))) isZero! !

!AbstractTupleOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/2/2021 12:31:07'!
testCrossProductAndDotProduct
	| V a b c d |
	V _ scalars^3.
	a _ V atRandom: random bits: 20.
	b _ V atRandom: random bits: 20.
	c _ V atRandom: random bits: 20.
	d _ V atRandom: random bits: 20.
	self assert: a ∑ (b ◊ c) = (b ∑ (c ◊ a)).
	self assert: a ∑ (b ◊ c) = (c ∑ (a ◊ b)).
	"Lagrange identity:"
	self assert: (a ◊ b) norm = (a norm * b norm - (a ∑ b) squared).
	"Binet-Chauchy identity:"
	self assert: a ◊ b ∑ (c ◊ d) = (a ∑ c *(b ∑ d) - (a ∑ d * (b ∑ c)))! !

!AbstractTupleOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/2/2021 12:31:23'!
testHomogeneousTuples
	| n a b c d |
	n _ 5 atRandom: random.
	[a _ scalars^n atRandom: random bits: 20. a isZero] whileTrue.
	b _ a * (scalars units atRandom: random bits: 20).
	self assert: a ~ b.
	n >= 2 ifFalse: [n _ (2 to: 5) atRandom: random].
	c _ scalars^n x: 1.
	d _ scalars^n x: 2.
	self deny: c ~ d! !

!TuplesOverGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 10:50:30'!
initialize
	super initialize.
	scalars _ GaloisRing new: 3 atRandom nextPrime to: 2 atRandom to: 2 atRandom! !

!TuplesOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:23:35'!
initialize
	super initialize.
	scalars _ É! !

!TuplesOverModularIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/26/2021 11:48:50'!
initialize
	super initialize.
	scalars _ É/(2 to: 64) atRandom! !

!TuplesOverModularIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:28:36'!
testModTupleOperations
	| n a b modulus |
	modulus _ scalars modulus.
	n _ 5 atRandom.
	a _ É^n atRandomBits: 30.
	b _ É^n atRandomBits: 30.
	#(+ - ∑ convolution: hadamard:) do: [:op|
		self testTuple: a with: b operation: op modulo: modulus].
	#(negated reversed copy sum product) do: [:op|
		self testTuple: a operation: op modulo: modulus].! !

!TuplesOverModularIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:28:45'!
testTuple: a operation: aSymbol modulo: modulus
	self assert: (a perform: aSymbol) % modulus = (a % modulus perform: aSymbol)! !

!TuplesOverModularIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:28:53'!
testTuple: a with: b operation: aSymbol modulo: modulus
	self assert: (a perform: aSymbol with: b) % modulus = (a % modulus perform: aSymbol with: b % modulus)! !

!TuplesOverBinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:26:20'!
initialize
	super initialize.
	scalars _ É/2! !

!TuplesOverBinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:29:12'!
testBitTupleBug
	| Z2 |
	Z2 _ PrimeField new: 2.
	self assert: ((1,0,0) % 2 :: sum = Z2 one).
	self assert: ((0,1,0) % 2 :: sum = Z2 one).
	self assert: ((1,1,0) % 2 :: sum = Z2 zero).
	self assert: ((1,1,1) % 2 :: sum = Z2 one)! !

!TuplesOverBinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:29:35'!
testBitTupleSum
	self assert: (BitTuple newFrom: 2r1011101) sum representative = 1.
	self assert: (BitTuple newFrom: 2r10000100100010) sum representative = 0! !

!TuplesOverPolynomialsOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:31:32'!
initialize
	super initialize.
	scalars _ Ñ polynomials! !

!TuplesOverMatricesTest methodsFor: 'as yet unclassified' stamp: 'len 7/24/2021 14:08:40'!
initialize
	super initialize.
	scalars _ (Ñ^(0 to: 3) atRandom) endomorphisms! !

!TuplesOverQuaternionsTest methodsFor: 'as yet unclassified' stamp: 'len 4/23/2021 11:00:55'!
initialize
	super initialize.
	scalars _ QuaternionAlgebra over: Ñ invariants: #(1 1)! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 8/4/2021 14:29:25'!
testMatrixAddition
	| A B x |
	A _ self newMatrix.
	self assert: (A + A negated) isZero.
	self assert: A + A zero = A.
	B _ self newMatrix: A extent.
	self assert: A + B = (B + A).
	x _ A domain atRandom: random bits: 10.
	self assert: (A+B)*x = (A*x + (B*x)).
	x _ A codomain atRandom: random bits: 10.
	self assert: x*(A+B) = (x*A + (x*B))! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 8/4/2021 14:19:47'!
testMatrixMultiplication
	| A B x |
	A _ self newMatrix.
	self assert: A * A domain id = A.
	self assert: A codomain id * A = A.
	B _ self newMatrix: A width @ (5 atRandom: random).
	x _ B domain atRandom: random bits: 10.
	self assert: A*B*x = (A*(B*x)).
	x _ A codomain atRandom: random bits: 10.
	self assert: x*A*B = (x*(A*B))! !

!AbstractMatrixTest methodsFor: 'arithmetic' stamp: 'len 7/25/2021 12:14:36'!
testScalarAction
	| A b B |
	A _ self newMatrix.
	b _ scalars atRandomBits: 20.
	B _ A*b.
	A withIndicesDo: [:each :i :j| self assert: each*b = (B at: i@j)]! !

!AbstractMatrixTest methodsFor: 'other' stamp: 'len 12/23/2021 17:49:39'!
testLeftRightIdentity
	| A |
	A _ scalars^(5 atRandom: random) => (scalars^(7 atRandom: random)) atRandom: random bits: 100.
	self assert: A codomain id * A = A.
	self assert: A * A domain id = A! !

!AbstractMatrixTest methodsFor: 'other' stamp: 'len 3/26/2021 13:23:21'!
testTranspose
	| A |
	A _ self newMatrix.
	self assert: A columns = A transpose rows.
	self assert: A rows = A transpose columns.
	self assert: A transpose transpose = A! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 8/1/2021 10:30:23'!
testBackslashMatrix
	| n m n2 A B X |
	100 timesRepeat:
		[n _ 5 atRandom: random.
		m _ 5 atRandom: random.
		A _ scalars^n => (scalars^m) atRandom: random bits: m*n*5.
		n2 _ 5 atRandom: random.
		B _ A*(scalars^n2 => (scalars^n) atRandom: random bits: n*n2*5).
		X _ A \ B.
		self assert: A*X = B]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 8/1/2021 10:30:28'!
testBackslashOneRowMatrix
	| n A v b x |
	100 timesRepeat:
		[n _ 5 atRandom: random.
		A _ scalars^n => (scalars^1) atRandom: random bits: n*5.
		v _ scalars^n atRandom: random bits: n*5.
		b _ A*v.
		x _ A \ b.
		self assert: A*x = b]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 8/1/2021 10:30:33'!
testBackslashTuple
	| n m A v b x |
	100 timesRepeat:
		[n _ 5 atRandom: random.
		m _ 5 atRandom: random.
		A _ scalars^n => (scalars^m) atRandom: random bits: m*n*5.
		v _ scalars^n atRandom: random bits: n*5.
		b _ A*v.
		x _ A \ b.
		self assert: A*x = b]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 8/1/2021 10:30:39'!
testBackslashZeroMatrix
	| n m n2 A B X |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ (scalars^n => (scalars^m)) zero.
	n2 _ 5 atRandom: random.
	B _ A*(scalars^n2 => (scalars^n) atRandom: random bits: n*n2*5).
	X _ A \ B.
	self assert: A*X = B! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 12/31/2021 09:31:37'!
testCodivision
	| A B n result N X combinations |
	n _ 5 atRandom: random.
	A _ scalars^5 => (scalars^n) atRandom: random bits: 100.
	combinations _ (1 to: n) collect: [:each| (2 atRandom: random) = 1].
	B _ Matrix rows:
		(combinations collect: [:isCombination|
			isCombination
				ifTrue: [(A codomain atRandom: random bits: 20) * A]
				ifFalse: [A domain atRandom: random bits: 20]]).
	result _ B codivisionBy: A.
	X _ result first.
	N _ result second.
	self assert: X*A + N = B.
	combinations withIndexDo: [:isCombination :i| isCombination ifTrue: [self assert: (N rowAt: i) isZero]]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 12/31/2021 09:31:59'!
testDivision
	| A B n result N X combinations |
	n _ 5 atRandom: random.
	A _ scalars^5 => (scalars^n) atRandom: random bits: 100.
	combinations _ (1 to: n) collect: [:each| (2 atRandom: random) = 1].
	B _ Matrix columns:
		(combinations collect: [:isCombination|
			isCombination
				ifTrue: [A * (A domain atRandom: random bits: 20)]
				ifFalse: [A codomain atRandom: random bits: 20]]).
	result _ B divisionBy: A.
	X _ result first.
	N _ result second.
	self assert: A*X + N = B.
	combinations withIndexDo: [:isCombination :i| isCombination ifTrue: [self assert: (N columnAt: i) isZero]]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 7/22/2021 11:25:00'!
testSlashMatrix
	| n m n2 A B X |
	100 timesRepeat:
		[n _ 5 atRandom: random.
		m _ 5 atRandom: random.
		A _ scalars^n => (scalars^m) atRandom: random bits: m*n*5.
		n2 _ 5 atRandom: random.
		B _ (scalars^m => (scalars^n2) atRandom: random bits: n*n2*5)*A.
		X _ B / A.
		self assert: X*A = B]! !

!AbstractMatrixTest methodsFor: 'solving' stamp: 'len 7/22/2021 11:24:11'!
testSlashTuple
	| n m A v b x |
	100 timesRepeat:
		[n _ 5 atRandom: random.
		m _ 5 atRandom: random.
		A _ scalars^n => (scalars^m) atRandom: random bits: m*n*5.
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
	self assert: (X ∑ A) isZero! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 7/25/2021 09:36:52'!
testCosyzygiesZeroMatrix
	| n m A X |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ (scalars^n => (scalars^m)) zero.
	X _ A cosyzygies.
	self assert: (X ∑ A) isZero! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 3/29/2021 11:08:38'!
testImage
	| A |
	A _ self newMatrix.
	self assert: A image = (scalars^A height span: A columns)! !

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
	self assert: (A ∑ X) isZero.
"	self assert: K isMonomorphism."
	self assert: A kernel = X image! !

!AbstractMatrixTest methodsFor: 'subobjects' stamp: 'len 7/25/2021 10:14:24'!
testSyzygiesZeroMatrix
	| n m A X |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ (scalars^n => (scalars^m)) zero.
	X _ A syzygies.
	self assert: (A ∑ X) isZero.
	self assert: A kernel = X image! !

!AbstractMatrixTest methodsFor: 'private' stamp: 'len 8/4/2021 14:42:51'!
newMatrix
	| n m |
	n _ (0 to: 5) atRandom: random.
	m _ (0 to: 5) atRandom: random.
	^ scalars^n => (scalars^m) atRandom: random bits: (m*n*5+1 atRandom: random)! !

!AbstractMatrixTest methodsFor: 'private' stamp: 'len 3/26/2021 12:41:44'!
newMatrix: shape
	| samples hom |
	hom _ scalars^shape y => (scalars^shape x).
	(2 atRandom: random) = 1 ifTrue: [^ hom atRandom: random bits: 100].
	samples _ {scalars zero. scalars one. scalars one negated}.
	^ hom filling: [:i :j| (2 atRandom: random) > 1 ifTrue: [scalars zero] ifFalse: [samples atRandom: random]]! !

!AbstractMatrixTest methodsFor: 'private' stamp: 'len 8/4/2021 14:42:41'!
newSquareMatrix
	| n |
	n _ (0 to: 5) atRandom: random.
	^ (scalars^n) endomorphisms atRandom: random bits: (n*n*5+1 atRandom: random)! !

!AbstractMatrixOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 12/18/2021 20:27:24'!
testAdjugateCramerRule
	| A I |
	A _ scalars matrix: 3 filling: [:i :j| scalars atRandom: random bits: 10].
	I _ A identity.
	self assert: A * A adjugate = (I * A determinant)! !

!AbstractMatrixOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 13:03:14'!
testCayleyHamiltonTheorem
	| A |
	A _ (scalars^(6 atRandom: random)) endomorphisms atRandom: random bits: 100.
	self assert: (A characteristicPolynomial value: A) isZero! !

!AbstractMatrixOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 12:58:42'!
testCharacteristicMatrixAndPolynomial
	| A |
	A _ self newSquareMatrix.
	self assert: A characteristicPolynomial = A characteristicMatrix determinant! !

!AbstractMatrixOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 13:04:31'!
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

!AbstractMatrixOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 12:56:13'!
testDeterminant
	| n I A B c |
	n _ (0 to: 4) atRandom: random.
	I _ (scalars^n) id.
	self assert: I determinant = scalars one.
	A _ scalars matrix: n filling: [:i :j| scalars atRandom: random bits: 10].
	B _ scalars matrix: n filling: [:i :j| scalars atRandom: random bits: 10].
	self assert: (A * B) determinant = (A determinant * B determinant).
	c _ scalars atRandomBits: 10.
	self assert: (A * c) determinant = (c^n * A determinant).
	self assert: A transpose determinant = A determinant! !

!AbstractMatrixOverCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 12/31/2021 11:20:33'!
testEchelonization
	| A E T |
	scalars hasEchelonization ifFalse: [^ self].
	A _ self newMatrix: (6 atRandom: random) @ (6 atRandom: random).
	T _ A echelonization.
	E _ A echelon.
	self assert: E isEchelon.
"	self assert: E isReducedEchelon."
	self assert: T*A = E.
	T _ A coechelonization.
	E _ A coechelon.
	self assert: E transpose isEchelon.
"	self assert: E isReducedEchelon."
	self assert: A*T = E! !

!AbstractMatrixOverFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/29/2021 11:10:53'!
testAbstractRankNullity
	| n m A |
	n _ 5 atRandom: random.
	m _ 5 atRandom: random.
	A _ scalars^n => (scalars^m) atRandom: random bits: (100 atRandom: random).
	self assert: A kernel = A transpose image complement.
	self assert: A image = A transpose kernel complement! !

!AbstractMatrixOverFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/12/2021 16:52:25'!
testColumnExtended
	| A B |
	A _ self newMatrix.
	B _ A columnExtended.
	self assert: B height = A height.
	self assert: B isSquare.
	self assert: A image <= B image! !

!AbstractMatrixOverFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/27/2021 12:08:32'!
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

!AbstractMatrixOverFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/26/2021 12:45:46'!
testKernelImageTheorem
	| n A |
	n _ 6 atRandom: random.
	A _ self newMatrix: n@n.
	self assert: A kernel rank + A image rank = n! !

!AbstractMatrixOverFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 12:49:12'!
testMinimalPolynomial
	| A f |
	A _ self newSquareMatrix.
	f _ A minimalPolynomial.
	self assert: f | A characteristicPolynomial.
	self assert: (f value: A) isZero! !

!AbstractMatrixOverFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/26/2021 12:46:05'!
testRankNullityTheorem
	| n A |
	n _ 6 atRandom: random.
	A _ self newMatrix: n@n.
	self assert: A rank + A nullity = n! !

!AbstractMatrixOverFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/12/2021 16:52:32'!
testRowExtended
	| A B |
	A _ self newMatrix.
	B _ A rowExtended.
	self assert: B = A transpose columnExtended transpose! !

!MatrixOverCyclotomicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/21/2021 17:35:18'!
initialize
	super initialize.
	scalars _ CyclotomicField new: 3 atRandom + 2! !

!MatrixOverFiniteFieldTest methodsFor: 'initialization' stamp: 'len 3/26/2021 12:49:47'!
initialize
	super initialize.
	scalars _ GaloisField new: 5 atRandom nextPrime to: 3 atRandom! !

!MatrixOverFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 11:36:26'!
testCharacteristicPolynomialBug
	"Using the Berkowitz algorithm this test fails."
	| A |
	A _ (É/2^3) endomorphisms !! #(1 1 1 1 0 0 1 1 1).
	self assert: (A characteristicPolynomial value: A) isZero! !

!MatrixOverFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:12:34'!
testEchelonExample
	"From Magma Handbook example H26E10."
	| K w A |
	K _ GaloisField new: 8.
	w _ K x.
	A _ K^3 => (K^4) !! {1. w. w^5. 0. w^3. w^4. w. 1. w^6. w^3. 1. w^4}.
	self assert: A echelon = (A parent !! {1. 0. 0. 0. 1. 0. 0. 0. 1. 0. 0. 0})! !

!MatrixOverQuadraticFieldTest methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 08:34:10'!
initialize
	| n |
	super initialize.
	[(n _ (2 to: 100) atRandom squarefreePart) > 1] whileFalse.
	2 atRandom = 1 ifTrue: [n _ n negated].
	scalars _ QuadraticField sqrt: n! !

!MatrixOverRationalsTest methodsFor: 'initialization' stamp: 'len 3/26/2021 12:54:30'!
initialize
	super initialize.
	scalars _ Ñ! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:04:44'!
testAdjugateExampleProperties
	| A B I c |
	A _ Ñ matrix: { (-3,2,-5). (-1,0,-2). (3,-4,1) }.
	B _ Ñ matrix: { (-1,2,3). (1,-2,1). (2,2,-1) }.
	c _ 7.
	I _ (Ñ^3) id.
	self assert: I adjugate = I.
	self assert: (A * B) adjugate = (B adjugate * A adjugate).
	self assert: (A*c) adjugate = (A adjugate * (c^2)).
	self assert: (A^5) adjugate = (A adjugate ^ 5).
	self assert: A transpose adjugate = A adjugate transpose! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:27:55'!
testBackslashExample
	"Example from Sage documentation. Non-singular and square."
	| A B X |
	A _ (Ñ^3) endomorphisms !! #(1 2 4 5 3 1 1 2 -1).
	B _ Ñ^2 => (Ñ^3) !! #(1 5 1 2 1 5).
	X _ A \ B.
	self assert: X = (Ñ^2 => (Ñ^3) !! {-1/7. -11/7. 4/7. 23/7. 0. 0}).
	self assert: A * X = B! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:28:00'!
testBackslashExampleNoSolution
	"Example from Sage documentation."
	| A v |
	A _ Ñ^3 => (Ñ^2) !! #(1 2 3 2 4 6).
	v _ Ñ^2 !! #(1 1).
	self assert: (A \ v) isNil! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:28:09'!
testBackslashExampleNonSquare
	"Example from Sage documentation."
	| A B X |
	A _ Ñ^4 => (Ñ^2) !! #(0 -1 1 0 -2 2 1 0).
	B _ (Ñ^2) endomorphisms !! #(1 0 1 -1).
	X _ A \ B.
	self assert: X = (Ñ^2 => (Ñ^4) !! {-3/2. 1/2. -1. 0. 0. 0. 0. 0}).
	self assert: A * X = B! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:28:16'!
testBackslashExampleNonSquare2
	| A B X |
	A _ Ñ^4 => (Ñ^2) !! #(0 0 -1 1 0 -2 2 1).
	B _ (Ñ^2) endomorphisms !! #(1 0 1 -1).
	X _ A \ B.
	self assert: A * X = B! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:28:23'!
testBackslashExampleSingular
	"Example from Sage documentation."
	| A B X |
	A _ Ñ^3 => (Ñ^2) !! #(1 2 3 2 4 6).
	B _ Ñ^2 => (Ñ^2) !! #(6 -6 12 -12).
	X _ A \ B.
	self assert: X = (Ñ^2 => (Ñ^3) !! #(6 -6 0 0 0 0)).
	self assert: A * X = B! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:28:32'!
testBackslashRationalTuple
	"Example from Sage documentation."
	| A v x |
	A _ (Ñ^3) endomorphisms !! #(1 2 3 -1 2 5 2 3 1).
	v _ Ñ^3 !! #(1 2 3).
	x _ A \ v.
	self assert: x = (-13/12, (23/12), (-7/12)).
	self assert: A * x = v! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:28:38'!
testBackslashRationalTuple2
	"Example from GAP manual."
	| A b x |
	A _ Ñ matrix: #((1 2 7) (4 5 6) (7 8 9) (10 11 19) (5 7 12)).
	b _ Ñ tuple: #(95 115 182).
	x _ Ñ tuple: {47/4. -17/2. 67/4. 0. 0}.
	self assert: A transpose \ b = x! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:28:44'!
testBackslashRationalTupleNonSquare
	"Example from Sage documentation."
	| A v x |
	A _ Ñ^3 => (Ñ^2) !! #(1 2 3 2 4 6).
	v _ (-1/2, -1).
	x _ (-1/2, 0, 0).
	self assert: A \ v = x! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:08:44'!
testCauchyDeterminantProductFormulaExample
	| A B |
	A _ Ñ matrix: {(1,1,1). (3,5,7). (9,25,49)}.
	B _ Ñ matrix: {(1,0,4). (1,1,1). (2,-2,-3)}.
	self assert: A determinant * B determinant = (A * B) determinant! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/6/2021 11:34:29'!
testCokernelMatrixRational
	"From Sage."
	| A B |
	A _ Ñ matrix: {(1,2,4,-7,4). (1,1,0,2,-1). (1,0,3,-3,1). (0,-1,-1,3,-2). (0,0,-1,2,-1)}.
	B _ Ñ matrix: {(1,0,-1,2,-1). (0,1,-1,1,-4)}.
	self assert: A cosyzygies echelon = B "we have to perform an additional echelonization"! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:10:49'!
testDeterminantExample
	| m |
	m _ Ñ matrix: { (2,1). (-1,0) }.
	self assert: m determinant = 1! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:11:09'!
testDeterminantExample2
	| vandermonde |
	vandermonde _ Ñ matrix: {(1,1,1). (3,5,7). (9,25,49)}.
	self assert: vandermonde determinant = 16! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/25/2021 18:30:56'!
testInverseBug
	| X |
	X _ (Ñ^2) endomorphisms !! #(1 2 0 0).
	self assert: X inverse isNil! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:16:38'!
testInverseExample
	| A I |
	A _ Ñ matrix: { (2,1,1). (-1,0,1). (0,2,1) }.
	I _ (Ñ^3) id.
	self assert: A inverse * A = I.
	self assert: A * A inverse = I! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:16:50'!
testInverseExample2
	| Qx x f A |
	Qx _ Ñ polynomials.
	x _ Qx x.
	f _ x^8 + (x^6) - (3*(x^4)) - (3*(x^3)) + (8*(x^2)) + (2*x) - 5.
	A _ f companion.
	self assert: (A * A inverse) isIdentity! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:17:57'!
testJordanNormalFormExample
	| A jordan B |
"	<timeout: 2>"
	A _ Ñ matrix: { (5,4,2,1). (0,1,-1,-1). (-1,-1,3,0). (1,1,-1,2) }.
	jordan _ A jordan.
	self assert: jordan size = 3.
	B _ (Ñ^1) id.
	self assert: (jordan includes: B).
	B _ (Ñ^1) id * 2.
	self assert: (jordan includes: B).
	B _ (Ñ^2) id * 4 :: at: 1@2 put: 1; yourself.
	self assert: (jordan includes: B)! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:18:07'!
testKernelExample
	| A K |
	A _ Ñ matrix: {(1, 0, -3, 0, 2, -8). (0, 1, 5, 0, -1, 4). (0, 0, 0, 1, 7, -9). (0, 0, 0, 0, 0, 0)}.
	K _ Ñ^6 span: {(3,-5,1,0,0,0). (-2,1,0,-7,1,0). (8,-4,0,9,0,1)}.
	self assert: A kernel = K! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:18:44'!
testKernelExample2
	"Example from GAP manual."
	| A K |
	A _ Ñ matrix: #((1 2 7) (4 5 6) (7 8 9) (10 11 19) (5 7 12)).
	A _ A transpose.
	K _ Ñ^5 span: {{-7/4. 9/2. -15/4. 1. 0}. {-3/4. -3/2. 1/4. 0. 1}}.
	self assert: A kernel = K! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:42:24'!
testLeftInverseRational
	| A |
	[A _ Ñ^3 => (Ñ^5) atRandom: random bits: 1000. A rank = 3] whileFalse.
	self assert: (A retraction * A) isIdentity! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:22:53'!
testRCFAndInvariantFactorsExample
	"Example from Magma documentation."
	| K A R x |
	K _ É/5.
	A _ (K^5) endomorphisms !! #(0 2 4 2 0 2 2 2 3 3 3 4 4 1 3 0 0 0 0 1 0 0 0 1 0).
	R _ A parent !! #(1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0).
	self assert: A RCF = R.
	x _ K polynomials x.
	self assert: A invariantFactors = {x - 1. x^2 - 1. x^2 - 1}
	"TODO: test transformation T such that T*A*T inverse = R"! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:21:49'!
testRCFExample
	| A |
	A _ Ñ matrix: #((-2 0 0) (-1 -4 -1) (2 4 0)).
	self assert: A RCF = (Ñ matrix: #((-2 0 0) (0 0 -4) (0 1 -4)))! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:21:59'!
testRCFExample2
	"Example from wikipedia:"
	| A C |
	A _ Ñ matrix: #(
		(-1 3 -1 0 -2 0 0 -2)
		(-1 -1 1 1 -2 -1 0 -1)
		(-2 -6 4 3 -8 -4 -2 1)
		(-1 8 -3 -1 5 2 3 -3)
		(0 0 0 0 0 0 0 1)
		(0 0 0 0 -1 0 0 0)
		(1 0 0 0 2 0 0 0)
		(0 0 0 0 4 0 1 0)).
	C _ Ñ matrix: #(
		(0 1 0 0 0 0 0 0)
		(1 1 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 -1)
		(0 0 1 0 0 0 0 -4)
		(0 0 0 1 0 0 0 -4)
		(0 0 0 0 1 0 0 2)
		(0 0 0 0 0 1 0 4)
		(0 0 0 0 0 0 1 0)).
	self assert: A RCF = C! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:23:05'!
testRankExample
	| vandermonde |
	vandermonde _ Ñ matrix: {(1,1,1). (3,5,7). (9,25,49)}.
	self assert: vandermonde rank = 3.
	vandermonde _ É matrix: {(1,1,1). (3,5,7). (9,25,49)}.
	self assert: vandermonde rank = 3! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:23:20'!
testRankExample2
	| m |
	m _ Ñ matrix: {(1,1,1). (2,2,0). (3,3,1)}.
	self assert: m rank = 2! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:42:30'!
testRightInverseRational
	| A |
	[A _ Ñ^5 => (Ñ^3) atRandom: random bits: 1000. A rank = 3] whileFalse.
	self assert: (A * A section) isIdentity! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:24:23'!
testSimilarityExample
	"Examples from wikipedia article on SNF"
	| A B C |
	A _ Ñ matrix: {(1,2). (0,1)}.
	B _ Ñ matrix: {(3,-4). (1,-1)}.
	C _ Ñ matrix: {(1,0). (1,2)}.
	self assert: A ~ B.
	self deny: A ~ C! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/25/2021 13:02:09'!
testSimilarityRational
	| n A P |
	n _ 5 atRandom: random.
	A _ (Ñ^n) endomorphisms atRandom: random bits: 10*n*n.
	[(P _ A parent atRandom: random bits: 10*n*n) isSingular] whileTrue.
	self assert: A ~ (P inverse * A * P)! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:24:55'!
testSlashWithMatrixExample
	| A B X  |
	A _ (Ñ^3) endomorphisms !! #(1 1 3 2 0 4 -1 6 -1).
	B _ Ñ^3 => (Ñ^1) !! #(2 19 8).
	X _ B / A.
	self assert: X = (B parent !! #(1 2 3)).
	self assert: X * A = B! !

!MatrixOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:25:07'!
testSlashWithTupleExample
	| A b x  |
	A _ (Ñ^3) endomorphisms !! #(1 1 3 2 0 4 -1 6 -1).
	b _ Ñ^3 !! #(2 19 8).
	x _ b / A.
	self assert: x = (Ñ^3 !! #(1 2 3)).
	self assert: x * A = b! !

!MatrixOverCyclotomicRingTest methodsFor: 'as yet unclassified' stamp: 'len 7/21/2021 17:35:09'!
initialize
	super initialize.
	scalars _ (CyclotomicField new: 3 atRandom + 2) integers! !

!MatrixOverGaussianIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 1/7/2022 13:52:55'!
initialize
	super initialize.
	scalars _ GaussianField new integers! !

!MatrixOverIntegersTest methodsFor: 'initialization' stamp: 'len 3/26/2021 12:50:25'!
initialize
	super initialize.
	scalars _ É! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:03:53'!
testAdjugateExample
	| A I |
	A _ Matrix rows: { (2,1,1). (-1,0,1). (0,2,1) }.
	I _ A identity.
	self assert: A * A adjugate = (I * A determinant)! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:04:04'!
testAdjugateExample2
	| A B |
	A _ Matrix rows: { (-3,2,-5). (-1,0,-2). (3,-4,1) }.
	B _ Matrix rows: { (-8,18,-4). (-5,12,-1). (4,-6,2) }.
	self assert: A adjugate = B! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:29:20'!
testBackslashIntegerTuple
	"Example from Sage documentation."
	| A v x |
	A _ É^3 => (É^2) !! #(1 2 3 2 4 6).
	v _ (-1, -2).
	x _ A \ v.
"	self assert: x = (-1, 0, 0)."
	self assert: A * x = v! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:29:26'!
testBackslashIntegerTuple2
	"Example from GAP manual."
	| A b |
	A _ É matrix: #((1 2 7) (4 5 6) (7 8 9) (10 11 19) (5 7 12)).
	A _ A transpose.
	b _ É tuple: #(95 115 182).
"	x _ É tuple: #(2285 -5854 4888 -1299 0)." "<- GAP comes up with this solution, but this is not very small"
	self assert: A * (A \ b) = b! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:29:34'!
testBackslashIntegerTuple3
	| A b |
	A _ É matrix: #((-8 1 -7 -9 -2 -1) (5 -2 3 -3 1 1) (7 0 6 4 -5 -8) (-7 -10 5 9 -4 4) (3 -4 1 -2 3 -8) (-7 3 2 6 7 -1) (4 8 5 1 -8 -9) (9 5 0 -10 -8 8) (-6 2 -6 -9 -5 6)).
	A _ A transpose.
	b _ É tuple: #(3 -1 -1 -7 9 8).
	"x _ É tuple: #(38 25 -6 1 -34 -12 3 -3 -44)." "shortest solution"
	self assert: A * (A \ b) = b! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:29:40'!
testBackslashIntegerTupleBug
	| A b x |
	A _ (É^3) endomorphisms !! #(1 0 0 0 1 0 0 0 10).
	b _ (0, 0, 10).
	x _ A \ b.
	self assert: A * x = b! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:30:02'!
testBackslashPascal3
	"Example from Matlab documentation. Non-singular and square."
	| A u x |
	A _ (É^3) endomorphisms !! #(1 1 1 1 2 3 1 3 6). "pascal(3)"
	u _ (3, 1, 4).
	x _ A \ u.
	self assert: x = (10, -12, 5).
	self assert: A * x = u! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:30:06'!
testBackslashPascal3Magic3
	"Example from Matlab documentation. Non-singular and square."
	| A B X |
	A _ (É^3) endomorphisms !! #(1 1 1 1 2 3 1 3 6). "pascal(3)"
	B _ (É^3) endomorphisms !! #(8 1 6 3 5 7 4 9 2). "magic(3)"
	X _ A \ B.
	self assert: X = ((É^3) endomorphisms !! #(19 -3 -1 -17 4 13 6 0 -6)).
	self assert: A * X = B! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:09:06'!
testCauchyDeterminantProductFormulaExample
	| A B |
	A _ É matrix: {(1,1,1). (3,5,7). (9,25,49)}.
	B _ É matrix: {(1,0,4). (1,1,1). (2,-2,-3)}.
	self assert: A determinant * B determinant = (A * B) determinant! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:09:20'!
testCharacteristicPolynomialExample1x1
	| A x |
	A _ É matrix: #((42)).
	x _ É polynomials x.
	self assert: A characteristicPolynomial = (x - 42)! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:09:28'!
testCharacteristicPolynomialExample2x2
	| A x |
	A _ É matrix: #((1 2) (3 4)).
	x _ É polynomials x.
	self assert: A characteristicPolynomial = (x^2 - (x*5) - 2)! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:09:35'!
testCharacteristicPolynomialExample3x3
	| A x |
	A _ É matrix: #((1 2 3) (4 5 6) (7 8 9)).
	x _ É polynomials x.
	self assert: A characteristicPolynomial = (x^3 - (x^2*15) - (x*18))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:09:59'!
testCharacteristicPolynomialExample4x4
	| A x |
	A _ É matrix: #((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)).
	x _ É polynomials x.
	self assert: A characteristicPolynomial = (x^4 - (x^3*34) - (x^2*80))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/6/2021 11:34:14'!
testCokernelMatrixInteger
	"From Sage."
	| A B |
	A _ É matrix: {(1,2,4,-7,4). (1,1,0,2,-1). (1,0,3,-3,1). (0,-1,-1,3,-2). (0,0,-1,2,-1)}.
	B _ É matrix: {(1,0,-1,2,-1). (0,1,-1,1,-4)}.
	self assert: A cosyzygies echelon = B! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:11:59'!
testDeterminantExample
	| m |
	m _ É matrix: { (2,1). (-1,0) }.
	self assert: m determinant = 1! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:11:39'!
testDeterminantExample2
	| vandermonde |
	vandermonde _ É matrix: {(1,1,1). (3,5,7). (9,25,49)}.
	self assert: vandermonde determinant = 16! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 1/6/2022 19:57:19'!
testDomainError
	| A B |
	A _ (É^4) id.
	self should: [A / 2] raise: InversionError.
	B _ (Ñ^4) id.
	self should: [A + B] raise: DomainError.
	self should: [A * B] raise: DomainError! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:39:23'!
testHNFvsCHNF
	| A |
	A _ (É^(4 atRandom: random)) endomorphisms atRandom: random bits: 50.
	self assert: A HNF = A antitranspose CHNF antitranspose! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:13:46'!
testHermiteNormalFormBug
	"Bug."
	| A H |
	A _ É matrix: #((1 2 3 0) (1 2 3 1)).
	H _ É matrix: #((1 2 3 0) (0 0 0 1)).
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:13:26'!
testHermiteNormalFormExample
	"Example from wikipedia."
	| A H |
	A _ É matrix: { (3,3,1,4). (0,1,0,0). (0,0,19,16). (0,0,0,3) }.
	H _ É matrix: { (3,0,1,1). (0,1,0,0). (0,0,19,1). (0,0,0,3) }.
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:13:59'!
testHermiteNormalFormExample2
	| A H |
	A _ É matrix: { (9,-36,30). (-36,192,-180). (30,-180,180) }.
	H _ É matrix: { (3,0,30). (0,12,0). (0,0,60) }.
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:14:09'!
testHermiteNormalFormExample3
	"Example from wikipedia"
	| A H |
	A _ É matrix: { (2,3,6,2). (5,6,1,6). (8,3,1,1) }.
	H _ É matrix: { (1,0,50,-11). (0,3,28,-2). (0,0,61,-13) }.
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:14:24'!
testHermiteNormalFormExample4
	"Example from Sage"
	| A B |
	A _ É matrix: {(1,2,3,4). (5,6,7,8). (9,10,11,12)}.
	B _ É matrix: {(1,2,3,4). (0,4,8,12). (0,0,0,0)}.
	self assert: A HNF = B.
	self assert: A echelon = B "the echelon form of integers matrices is the hermite normal form"! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:14:34'!
testHermiteNormalFormExample5
	"From Magma Handbook example H26E10."
	| A H |
	A _ É matrix: #((2 -4 12 7 0) (3 -3 5 -1 4) (2 -1 -4 -5 -12) (0 3 6 -2 0)).
	H _ É matrix: #((1 1 1 6 -164) (0 3 0 16 -348) (0 0 2 13 -200) (0 0 0 19 -316)).
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:14:45'!
testHermiteNormalFormExample6
	"From MuPAD documentation."
	| A H |
	A _ É matrix: #(
		(25 -300 1050 -1400 630)
		(-300 4800 -18900 26880 -12600)
		(1050 -18900 79380 -117600 56700)
		(-1400 26880 -117600 179200 -88200)
		(630 -12600 56700 -88200 44100)).
	H _ É matrix: #(
		(5 0 210 560 630)
		(0 60 0 0 0)
		(0 0 420 0 0)
		(0 0 0 840 0)
		(0 0 0 0 2520)).
	"The original from MuPAD documentation is:
	H _ É matrix: #(
		(5 0 -210 -280 630)
		(0 60 0 0 0)
		(0 0 420 0 0)
		(0 0 0 840 0)
		(0 0 0 0 2520)).
	but this is wrong, this is a bug in MuPAD"
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:14:55'!
testHermiteNormalFormExample7
	"Example from GAP manual."
	| A H |
	A _ É matrix: #((1 15 28) (4 5 6) (7 8 9)).
	H _ É matrix: #((1 0 1) (0 1 1) (0 0 3)).
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:15:05'!
testHermiteNormalFormExample8
	"Example from Pernet-Stein 'Fast computation of Hermite normal forms of random integer matrices'."
	| A H |
	A _ É matrix: #((-5 8 -3 -9 5 5) (-2 8 -2 -2 8 5) (7 -5 -8 4 3 -4) (1 -1 6 0 8 -3)).
	H _ É matrix: #((1 0 3 237 -299 90) (0 1 1 103 -130 40) (0 0 4 352 -450 135) (0 0 0 486 -627 188)).
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:15:13'!
testHermiteNormalFormExample9
	"Example from Pernet-Stein 'Fast computation of Hermite normal forms of random integer matrices'."
	| A H |
	A _ É matrix: #((-1 2 5 65 2) (4 -1 -3 1 -2) (-1 -2 1 -1 1)).
	H _ É matrix: #((1 0 17 259 7) (0 1 31 453 13) (0 0 40 582 17)).
	self assert: A HNF = H! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:17:21'!
testIsHermiteNormalForm
	"Example from wikipedia."
	| A H |
	A _ É matrix: { (3,3,1,4). (0,1,0,0). (0,0,19,16). (0,0,0,3) }.
	H _ É matrix: { (3,0,1,1). (0,1,0,0). (0,0,19,1). (0,0,0,3) }.
	self assert: A isHermiteNormalForm not.
	self assert: H isHermiteNormalForm! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:17:28'!
testIsHermiteNormalForm2
	| M |
	M _ É matrix: {(2,0,1,11). (0,1,5,12). (0,0,6,12). (0,0,0,33)}.
	self assert: M isHermiteNormalForm.
	self assert: M transpose isHermiteNormalForm not.
	M _ É matrix: {(2,0,1,11). (0,1,5,12). (0,0,6,12). (0,0,0,33). (0,0,0,0)}.
	self assert: M isHermiteNormalForm.
	M _ É matrix: {(2,0,1,11). (0,1,5,12). (0,0,6,12). (0,0,0,33). (0,0,0,1)}.
	self assert: M isHermiteNormalForm not! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/6/2021 11:16:40'!
testKernelExample
	"Example from GAP manual."
	| A K |
	A _ É matrix: #((1 2 7) (4 5 6) (7 8 9) (10 11 19) (5 7 12)).
	A _ A transpose. "because in GAP matrices act on rows"
	K _ É matrix: #((1 18 -9 2 -6) (0 24 -13 3 -7)).
	K _ K transpose.
	self assert: A kernelMap asMatrix = K! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:19:04'!
testKroneckerProductExample
	| A B C |
	A _ É matrix: #((1 2) (3 4)).
	B _ É matrix: #((0 5) (6 7)).
	C _ É matrix: #((0 5 0 10) (6 7 12 14) (0 15 0 20) (18 21 24 28)).
	self assert: A ë B = C! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:21:02'!
testMinorExample
	| A |
	A _ É matrix: #((1 2 3) (4 5 6) (7 8 9)).
	self assert: (A minor: 2@3) = -6! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:21:12'!
testMinorsExample
	| A |
	A _ É^3 => (É^2) !! (1 to: 6).
	self assert: (A minors: 2) asArray = #(-3 -6 -3)! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:21:22'!
testPermanentExample
	"Example from https://en.wikipedia.org/wiki/Permanent_(mathematics)"
	| A |
	A _ (É^4) endomorphisms !! #(1 1 1 1 2 1 0 0 3 0 1 0 4 0 0 1).
	self assert: A permanent = 10! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/12/2021 12:38:24'!
testPivots
	| A |
	A _ É matrix: {(0,-1,-1). (0,-20,1). (0,1,2)}.
	self assert: A echelon pivots asArray = {1@2. 2@3}! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:21:33'!
testPositiveDefiniteExample
	| M |
	M _ (É^3) endomorphisms !! #(3 -1 1 -1 3 1 1 1 3).
	self assert: M isPositiveDefinite! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:23:41'!
testRankExample
	| m |
	m _ É matrix: {(1,1,1). (2,2,2). (5,5,5)}.
	self assert: m rank = 1! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:30:04'!
testSmithNormalFormExample1
	| A |
	A _ É matrix: { (2,4,4). (-6,6,12). (10,-4,-16) }.
	self assert: A SNF = ((É^3) endomorphisms diagonal: #(2  6 12))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:30:14'!
testSmithNormalFormExample2
	| A |
	"Reduced Laplacian matrix of K_4:"
	A _ É matrix: { (3,-1,-1). (-1,3,-1). (-1,-1,3) }.
	self assert: A SNF = ((É^3) endomorphisms diagonal: #(1  4  4))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:30:23'!
testSmithNormalFormExample3
	"Example from Willian Stein, 'Algebraic Number Theory, A Computational Approach'."
	| A |
	A _ É matrix: { (1,4,9). (16,25,36). (49,64,81) }.
	self assert: A SNF = ((É^3) endomorphisms diagonal: #(1 3 72))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:30:36'!
testSmithNormalFormExample4
	"Smith form of a 3x3 matrix of rank 2.
	Example from Willian Stein, 'Algebraic Number Theory, A Computational Approach'."
	| A |
	A _ É matrix: #((2 3 4) (5 6 7) (8 9 10)).
	self assert: A SNF = ((É^3) endomorphisms diagonal: #(1 3 0)) "this fails with SNF because Cohen defines SNF only for nonsingular matrices, so use SNF2"! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:30:47'!
testSmithNormalFormExample5
	"Example from youtube video: 'Derek Holt: Algorithms for finitely presented groups' @ 25:49."
	| A |
	A _ É matrix: #((1 2 0 -2) (0 2 2 0) (-1 -4 4 2)).
	self assert: A SNF = (É matrix: #((1 0 0 0) (0 2 0 0) (0 0 6 0)))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:30:58'!
testSmithNormalFormExample6
	"From Magma Handbook example H26E10."
	| A |
	A _ É matrix: #((2 -4 12 7 0) (3 -3 5 -1 4) (2 -1 -4 -5 -12) (0 3 6 -2 0)).
	self assert: A elementaryDivisors = #(1 1 1 2)! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:31:05'!
testSmithNormalFormExample7
	"From GAP manual."
	| A |
	A _ É matrix: #((1 15 28) (4 5 6) (7 8 9)).
	self assert: A SNF = ((É^3) endomorphisms diagonal: #(1 1 3))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:31:15'!
testSmithNormalFormExample8
	| A |
	A _ É matrix: #((2 1 -3) (3 -2 -1) (1 -3 2)).
	self assert: A SNF = ((É^3) endomorphisms diagonal: #(1 7 0))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:31:24'!
testSmithNormalFormExample9
	| A |
	A _ É matrix: #((1 2 3 4 5 6 7) (1 0 1 0 1 0 1) (2 4 5 6 1 1 1) (1 4 2 5 2 0 0) (0 0 1 1 2 2 3)).
	self assert: A SNF = (É matrix: #((1 0 0 0 0 0 0) (0 1 0 0 0 0 0) (0 0 1 0 0 0 0) (0 0 0 1 0 0 0) (0 0 0 0 2 0 0)))! !

!MatrixOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:31:39'!
testSmithNormalFormExampleDiagonal
	| A |
	A _ É matrix: #((2 0 0) (0 1 0) (0 0 0)).
	self assert: A SNF = ((É^3) endomorphisms diagonal: #(1 2 0)).
	A _ É matrix: #((5 0 0) (0 2 0) (0 0 0)).
	self assert: A SNF = ((É^3) endomorphisms diagonal: #(1 10 0))! !

!MatrixOverMultivariatePolynomialsOverPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 12/30/2021 12:23:00'!
initialize
	super initialize.
	scalars _ É/5 polynomialsIn: #(x y z)! !

!MatrixOverMultivariatePolynomialsOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/15/2021 07:37:23'!
initialize
	super initialize.
	scalars _ Ñ polynomialsIn: #(x y z)! !

!MatrixOverPolynomialsOverPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:06:29'!
initialize
	super initialize.
	scalars _ É/5! !

!MatrixOverPolynomialsOverPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 12/18/2021 20:29:45'!
testRankOverPolynomialsOverZ5
	"Example form Magma handbook (H88E7)."
	| K P x a |
	K _ É/5.
	P _ K polynomials.
	x _ P x.
	a _ P matrix: 5 filling: [:i :j| x^i + (x^j)].
	self assert: a rank = 2! !

!MatrixOverPolynomialsOverPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 12/18/2021 20:29:53'!
testSmithNormalFormOverPolynomialsOverPrimeField1
	"Example form Magma handbook (H88E7)."
	| K P x a |
	K _ É/5.
	P _ K polynomials.
	x _ P x.
	a _ P matrix: 5 filling: [:i :j| x^i + (x^j)].
	self assert: a elementaryDivisors = {x. x^3 + (x^2*3) + x}! !

!MatrixOverPolynomialsOverPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:05:31'!
testSmithNormalFormOverPolynomialsOverPrimeField2
	"Example form Magma handbook (H88E7)."
	| K P x a ax |
	K _ É/5.
	P _ K polynomials.
	x _ P x.
	a _ (É^5) endomorphisms !! #(0 2 4 2 0 2 2 2 3 3 3 4 4 1 3 0 0 0 0 1 0 0 0 1 0).
	a _ a over: P.
	ax _ a identity * x - a.
	self assert: ax SNF = ((P^5) endomorphisms diagonal: {P one. P one. x - 1. x^2 - 1. x^2 - 1}).
	self assert: ax elementaryDivisors = {P one. P one. x - 1. x^2 - 1. x^2 - 1}! !

!MatrixOverPolynomialsOverRationalsTest methodsFor: 'initialization' stamp: 'len 3/26/2021 12:58:50'!
initialize
	super initialize.
	scalars _ Ñ polynomials! !

!MatrixOverPolynomialsOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:15:39'!
testHermiteNormalFormExample
	"From MuPAD documentation."
	| R x A H |
	R _ Ñ polynomials.
	x _ R x.
	A _ (R^2) endomorphisms !! {x^2 + 3. (x*2 - 1)^2. (x + 2)^2. x^2*3 + 5}.
	H _ (R^2) endomorphisms !! {R one. (x^3*4 + (x^2*47) - (x*76) + 20) / 49. R zero. x^4 + (x^3*12) - (x^2*13) - (x*12) - 11}.
	self assert: A HNF = H! !

!MatrixOverPolynomialsOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:32:25'!
testSmithNormalFormOverExample1
	"Example from Sage documentation."
	| R x A |
	R _ Ñ polynomials.
	x _ R x.
	A _ (R^2) id * x - (R matrix: #((3 -4) (1 -1))).
	self assert: A SNF = ((R^2) endomorphisms diagonal: {R one. x^2 - (x*2) + 1})! !

!MatrixOverQuadraticRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 08:34:13'!
initialize
	| n |
	super initialize.
	[(n _ (2 to: 100) atRandom squarefreePart) > 1] whileFalse.
	2 atRandom = 1 ifTrue: [n _ n negated].
	scalars _ (QuadraticField sqrt: n) integers! !

!MatrixOverGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 10:51:06'!
initialize
	super initialize.
	scalars _ GaloisRing new: 3 atRandom nextPrime to: 2 atRandom to: 2 atRandom! !

!MatrixOverModularIntegersTest methodsFor: 'initialization' stamp: 'len 4/26/2021 11:49:04'!
initialize
	super initialize.
	scalars _ É/(2 to: 64) atRandom! !

!MatrixOverModularIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:15:59'!
testHowellNormalForm1
	"Examples from 'Fast Algorithms for Linear Algebra Modulo N', Arne Storjohann, Thom Mulders."
	| R A B C D H |
	R _ É/12.
	A _ R matrix: #((4 1 0) (0 0 5) (0 0 0)).
	B _ R matrix: #((8 5 5) (0 9 8) (0 0 10)).
	C _ R matrix: #((4 1 10) (0 0 5) (0 0 0)).
	D _ R matrix: #((4 1 0) (0 0 1) (0 0 0)).
	H _ R matrix: #((4 1 0) (0 3 0) (0 0 1)).
	self assert: A echelon = H.
	self assert: B echelon = H.
	self assert: C echelon = H.
	self assert: D echelon = H! !

!MatrixOverModularIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 21:47:55'!
testHowellNormalForm2
	| m R t u |
	100 timesRepeat:
		[[m _ (256 atRandom: random) + 1. m isPrime] whileTrue.
		R _ É/m.
		t _ R^(5 atRandom: random) atRandom: random.
		u _ R units atRandom: random.
		self assert: t asRowMatrix echelon = (t*u) asRowMatrix echelon]! !

!MatrixOverModularIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2021 11:16:19'!
testHowellNormalForm3
	"Example from 'Spans in the module (Z/mZ)^s', John A. Howell (1986)."
	| R A H |
	R _ É/6.
	A _ R matrix: #((2 1)).
	H _ R matrix: #((2 1) (0 3)).
	self assert: A echelon = H! !

!MatrixOverPrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 3/26/2021 12:57:12'!
initialize
	super initialize.
	scalars _ PrimePowerRing new: 10 atRandom nextPrime to: 3 atRandom! !

!MatrixOverProductRingTest methodsFor: 'initialization' stamp: 'len 4/2/2021 10:22:25'!
initialize
	super initialize.
	scalars _ É ◊ É "(É/(64 atRandom + 1))"! !

!MatrixOverCliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 7/22/2021 11:17:19'!
initialize
	super initialize.
	scalars _ CliffordAlgebra on: (QuadraticForm from: Ñ^3 coefficients: #(1 0 0 1 0 1))! !

!MatrixOverGroupAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 7/22/2021 18:30:01'!
initialize
	super initialize.
	scalars _ (SymmetricGroup new: (0 to: 3) atRandom) groupAlgebraOver: É/2! !

!MatrixOverMatricesTest methodsFor: 'as yet unclassified' stamp: 'len 7/21/2021 18:12:12'!
initialize
	super initialize.
	scalars _ (Ñ^(0 to: 3) atRandom) endomorphisms! !

!MatrixOverQuaternionsTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:37:20'!
initialize
	super initialize.
	scalars _ QuaternionAlgebra over: Ñ invariants: #(1 1)! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 12/25/2021 10:54:14'!
newFreeModule
	| M |
	M _ (3 atRandom: random) = 1 ifTrue: [scalars asIdeal] ifFalse: [scalars ^ ((0 to: 3) atRandom: random)].
	scalars isCommutative ifFalse: [^ M].
	(5 atRandom: random) = 1 ifTrue: [^ M => self newSmallFreeModule].
	(5 atRandom: random) = 1 ifTrue: [^ M dual].
	^ M! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 3/24/2021 09:15:31'!
newMatrix: shape
	| hom samples |
	hom _ scalars^shape y => (scalars^shape x).
	(2 atRandom: random) = 1 ifTrue: [^ hom atRandom: random bits: 100].
	samples _ {scalars zero. scalars one. scalars one negated}.
	^ hom filling: [:i :j| (2 atRandom: random) > 1 ifTrue: [scalars zero] ifFalse: [samples atRandom: random]]! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 8/22/2021 12:27:37'!
newModule
	| M N |
	M _ self newFreeModule.
	(3 atRandom: random) = 1 ifTrue: [^ M].
	N _ M span: ((1 to: ((0 to: M rank) atRandom: random)) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	(3 atRandom: random) = 1 ifTrue: [^ N].
	^ M / N! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 3/23/2021 13:27:06'!
newModuleMap
	| V W |
	V _ self newFreeModule.
	W _ self newFreeModule.
	^ V => W !! (self newMatrix: W rank @ V rank)! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 12/25/2021 10:54:29'!
newSmallFreeModule
	| M |
	M _ (3 atRandom: random) = 1 ifTrue: [scalars asIdeal] ifFalse: [scalars ^ ((0 to: 3) atRandom: random)].
	scalars isCommutative ifFalse: [^ M].
	(3 atRandom: random) = 1 ifTrue: [^ M dual].
	^ M! !

!AbstractModuleTest methodsFor: 'private' stamp: 'len 1/6/2022 14:38:20'!
newSmallModule
	| M N |
	M _ self newSmallFreeModule.
	(3 atRandom: random) = 1 ifTrue: [^ M].
	N _ M span: ((1 to: ((0 to: M rank) atRandom: random)) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	(2 atRandom: random) = 1 ifTrue: [^ N].
	^ M / N! !

!AbstractModuleTest methodsFor: 'isomorphism theorems' stamp: 'len 3/23/2021 14:05:03'!
testFirstIsomorphismTheorem
	| M N f |
	M _ self newFreeModule.
	N _ self newFreeModule.
	f _ M => N atRandom: random bits: 100.
	self assert: f codomain >= f image.
	self assert: f domain >= f kernel.
	self assert: f domain / f kernel ~ f image! !

!AbstractModuleTest methodsFor: 'isomorphism theorems' stamp: 'len 7/26/2021 18:36:51'!
testSecondIsomorphismTheorem
	| M S T |
	M _ self newFreeModule.
	S _ M span: ((1 to: M rank atRandom) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	T _ M span: ((1 to: M rank atRandom) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	self assert: S+T <= M.
	self assert: S /\ T <= M.
	self assert: (S+T)/T ~ (S/(S /\ T))! !

!AbstractModuleTest methodsFor: 'isomorphism theorems' stamp: 'len 7/26/2021 18:38:37'!
testThirdIsomorphismTheorem
	| M S T k |
	M _ self newFreeModule.
	k _ M rank atRandom.
	S _ M span: ((1 to: k) collect: [:i| M atRandom: random bits: (10 atRandom: random)]).
	T _ S span: ((1 to: k atRandom) collect: [:i| S atRandom: random bits: (10 atRandom: random)]).
	self assert: S/T <= (M/T).
	self assert: (M/T)/(S/T) ~ (M/S)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 07:58:17'!
testCoequalizer
	| A B f g Q |
	A _ self newFreeModule.
	B _ self newFreeModule.
	f _ (A => B) atRandom: random bits: (50 atRandom: random).
	g _ (A => B) atRandom: random bits: (50 atRandom: random).
	Q _ f coequalizer: g.
	self assert: (f -> Q) notNil.
	self assert: f -> Q = (g -> Q)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/29/2021 04:22:14'!
testCoimage
	| A J |
	A _ self newModuleMap.
	J _ A coimageMap.
	self assert: A coimage = J image.
	self assert: J isEpimorphism! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/29/2021 04:26:42'!
testCokernel
	| A Q |
	A _ self newModuleMap.
	Q _ A cokernelMap.
	self assert: (Q ∑ A) isZero.
	self assert: Q isEpimorphism.
	self assert: A cokernel = Q coimage "what?"! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 07:58:37'!
testCokernelAndCoequalizer
	| A B f |
	A _ self newFreeModule.
	B _ self newFreeModule.
	f _ (A => B) atRandom: random bits: (100 atRandom: random).
	self assert: f cokernel = (f coequalizer: f zero)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/1/2021 12:29:42'!
testColift
	| M X A i Y f g |
	M _ self newSmallModule.
	X _ self newSmallModule.
	A _ (X => M atRandom: random bits: 20) kernel.
	i _ A -> X.
	self assert: i isInjective.
	Y _ self newSmallModule.
	f _ A => Y atRandom: random bits: 20.
	g _ f / i.
	self assert: (X => Y includes: g).
	self assert: i -> g = f

"	| M N phi cokernel L eta eta0 |
	M _ Ñ^3.
	N _ Ñ^4.
	phi _ (M => N) atRandomBits: 20.
	cokernel _ phi cokernel.
	L _ Ñ^5.
	eta _ N to: L evaluating: [:x| L atRandomBits: 20]. ???
	self assert: (phi -> eta) isZero.
	eta0 _ eta / (M ->> cokernel).
	self assert: (cokernel => L includes: eta0).
	self assert: M ->> cokernel -> eta0 = eta"! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/1/2021 12:21:22'!
testColiftFree
	| M X A i Y f g |
	M _ self newFreeModule.
	X _ self newFreeModule.
	A _ (X => M atRandom: random bits: 20) kernel.
	i _ A -> X.
	self assert: i isInjective.
	Y _ self newFreeModule.
	f _ A => Y atRandom: random bits: 20.
	g _ f / i.
	self assert: (X => Y includes: g).
	self assert: i -> g = f

"	| M N phi cokernel L eta eta0 |
	M _ Ñ^3.
	N _ Ñ^4.
	phi _ (M => N) atRandomBits: 20.
	cokernel _ phi cokernel.
	L _ Ñ^5.
	eta _ N to: L evaluating: [:x| L atRandomBits: 20]. ???
	self assert: (phi -> eta) isZero.
	eta0 _ eta / (M ->> cokernel).
	self assert: (cokernel => L includes: eta0).
	self assert: M ->> cokernel -> eta0 = eta"! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/22/2021 12:24:37'!
testComposition
	| X Y Z f g x |
	X _ self newModule.
	Y _ self newModule.
	Z _ self newModule.
	f _ X => Y atRandom: random bits: 1000.
	g _ Y => Z atRandom: random bits: 1000.
	x _ X atRandom: random bits: 1000.
	self assert: (X => Z includes: g ∑ f).
	self assert: (g ∑ f value: x) = (g value: (f value: x)).
	X = Z ifFalse: [self should: [f ∑ g] raise: DomainError]! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/22/2021 12:24:50'!
testCompositionError
	| X Y Z f g |
	X _ self newModule.
	Y _ self newModule.
	[Z _ self newModule. X = Z] whileTrue.
	f _ X => Y atRandom: random bits: 1000.
	g _ Y => Z atRandom: random bits: 1000.
	self should: [f ∑ g] raise: DomainError! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 08:01:55'!
testEqualizer
	| A B f g Q |
	A _ self newFreeModule.
	B _ self newFreeModule.
	f _ A => B atRandom: random bits: (100 atRandom: random).
	g _ A => B atRandom: random bits: (100 atRandom: random).
	Q _ f equalizer: g.
	self assert: (Q -> f) notNil.
	self assert: Q -> f = (Q -> g)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 07:57:09'!
testImage
	| f image v w |
	f _ self newModuleMap.
	image _ f image.
	v _ f domain atRandom: random bits: 100.
	w _ f value: v.
	self assert: (image includes: w)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/29/2021 04:24:51'!
testImageAndCoimage
	| A |
	A _ self newModuleMap.
	self assert: A cokernelMap kernelMap = A imageMap.
	self assert: A kernelMap cokernelMap = A coimageMap! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 4/4/2021 10:22:02'!
testKernel
	| A K |
	A _ self newModuleMap.
	K _ A kernelMap.
	self assert: (A ∑ K) isZero.
	self assert: K isMonomorphism.
	self assert: A kernel = K image! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 4/4/2021 10:23:04'!
testKernel2
	| f kernel v |
	f _ self newModuleMap.
	kernel _ f kernel.
	v _ kernel atRandom: random bits: 100.
	self assert: (f value: v) isZero! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 09:15:41'!
testKernelAndEqualizer
	| A B f |
	A _ self newFreeModule.
	B _ self newFreeModule.
	f _ A => B atRandom: random bits: (100 atRandom: random).
	self assert: f kernel = (f equalizer: f zero)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/1/2021 12:30:04'!
testLift
	| M X B Y p f g |
	M _ self newSmallModule.
	X _ self newSmallModule.
	B _ (M => X atRandom: random bits: 20) cokernel.
	p _ X -> B.
	self assert: p isSurjective.
	Y _ self newSmallModule.
	f _ Y => B atRandom: random bits: 20.
	g _ p \ f.
	self assert: (Y => X includes: g).
	self assert: g -> p = f

"	| M N phi kernel L tau tau0 |
	M _ Ñ^3.
	N _ Ñ^4.
	phi _ (M => N) atRandomBits: 20.
	kernel _ phi kernel.
	L _ Ñ^5.
	tau _ L to: M evaluating: [:x| kernel atRandomBits: 20].
	self assert: (tau -> phi) isZero.
	tau0 _ kernel >-> M \ tau.
	self assert: (L => kernel includes: tau0).
	self assert: tau0 -> (kernel >-> M) = tau"! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/1/2021 12:20:38'!
testLiftFree
	| M X B Y p f g |
	M _ self newFreeModule.
	X _ self newFreeModule.
	B _ (M => X atRandom: random bits: 20) cokernel.
	p _ X -> B.
	self assert: p isSurjective.
	Y _ self newFreeModule.
	f _ Y => B atRandom: random bits: 20.
	g _ p \ f.
	self assert: (Y => X includes: g).
	self assert: g -> p = f

"	| M N phi kernel L tau tau0 |
	M _ Ñ^3.
	N _ Ñ^4.
	phi _ (M => N) atRandomBits: 20.
	kernel _ phi kernel.
	L _ Ñ^5.
	tau _ L to: M evaluating: [:x| kernel atRandomBits: 20].
	self assert: (tau -> phi) isZero.
	tau0 _ kernel >-> M \ tau.
	self assert: (L => kernel includes: tau0).
	self assert: tau0 -> (kernel >-> M) = tau"! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 08:09:14'!
testMapToBiggerDimensionIsNotSurjective
	| V W f |
	 V _ scalars^(4 atRandom: random).
	W _ scalars^((V rank atRandom: random) - 1).
	f _ W => V atRandom: random bits: 100.
	self assert: f isSurjective not! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 08:08:52'!
testMapToSmallerDimensionIsNotInjective
	| V W f |
	 V _ scalars^(4 atRandom: random).
	W _ scalars^((V rank atRandom: random) - 1).
	f _ V => W atRandom: random bits: 100.
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

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 3/24/2021 07:57:36'!
testPreimage
	| f w v |
	f _ self newModuleMap.
	w _ f image atRandom: random bits: 100.
	v _ f preimage: w.
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

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/1/2021 12:30:26'!
testPullback
	| X Y Z f g P projections |
	Z _ self newSmallModule.
	X _ self newSmallModule.
	Y _ self newSmallModule.
	f _ X => Z atRandom: random bits: 20.
	g _ Y => Z atRandom: random bits: 20.
	P _ f /\ g.
	projections _ P -< (X, Y).
	self assert: projections first -> f = (projections second -> g)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/1/2021 12:22:15'!
testPullbackFree
	| X Y Z f g P projections |
	Z _ self newFreeModule.
	X _ self newFreeModule.
	Y _ self newFreeModule.
	f _ X => Z atRandom: random bits: 20.
	g _ Y => Z atRandom: random bits: 20.
	P _ f /\ g.
	projections _ P -< (X, Y).
	self assert: projections first -> f = (projections second -> g)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/1/2021 12:30:40'!
testPushout
	| X Y Z f g P coprojections |
	Z _ self newSmallModule.
	X _ self newSmallModule.
	Y _ self newSmallModule.
	f _ Z => X atRandom: random bits: 20.
	g _ Z => Y atRandom: random bits: 20.
	P _ f \/ g.
	coprojections _ (X, Y) >- P.
	self assert: f -> coprojections first = (g -> coprojections second)! !

!AbstractModuleTest methodsFor: 'maps' stamp: 'len 8/1/2021 12:22:21'!
testPushoutFree
	| X Y Z f g P coprojections |
	Z _ self newFreeModule.
	X _ self newFreeModule.
	Y _ self newFreeModule.
	f _ Z => X atRandom: random bits: 20.
	g _ Z => Y atRandom: random bits: 20.
	P _ f \/ g.
	coprojections _ (X, Y) >- P.
	self assert: f -> coprojections first = (g -> coprojections second)! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 3/24/2021 08:26:32'!
testCanonicalMorphisms
	| V v W w |
	V _ self newModule.
	v _ V atRandom: random bits: 100.
	W _ V span: {v}.
	w _ W atRandom: random bits: 100.
	self assert: (V null -> W) isZero. "canonical morphism from initial object"
	self assert: (V -> W null) isZero. "canonical morphism to terminal object"
	self assert: ((W -> W null) value: w) isZero.
	self assert: (W -> V value: w) = w. "canonical morphism to overobject"! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 3/24/2021 08:26:53'!
testDirectSum
	| M N S |
	M _ self newModule.
	N _ self newModule.
	S _ M ê N.
	(M, N) >- S do: [:each| self testModuleMap: each].
	S -< (M, N) do: [:each| self testModuleMap: each]! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 3/24/2021 08:27:15'!
testDirectSumFree
	| M N S |
	M _ self newFreeModule.
	N _ self newFreeModule.
	S _ M ê N.
	self testFreeModule: S.
	(M, N) >- S do: [:each| self testModuleMap: each].
	S -< (M, N) do: [:each| self testModuleMap: each]! !

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

!AbstractModuleTest methodsFor: 'other' stamp: 'len 3/24/2021 08:32:58'!
testSubmoduleInclusion
	| V S P v w |
	[[V _ self newModule.
	v _ V atRandom: random bits: 10. v isZero] whileTrue.
	S _ V span: {v}.
	w _ V atRandom: random bits: 10. S includes: w] whileTrue.
	P _ V span: {w*(V scalars atRandom: random bits: 10)-v. w}.
	self assert: S < P.
	self assert: P > S.
	self assert: S ~= P.
	self assert: P ~= S.
	self assert: (S > P) not.
	self assert: (P < S) not! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 3/24/2021 09:17:06'!
testSubmoduleSum
	| V v w S T P |
	[[V _ self newModule.
	v _ V atRandom: random bits: 10. v isZero not] whileFalse.
	w _ V atRandom: random bits: 10. w isZero not and: [v ~= w]] whileFalse.
	S _ V span: {v}.
	T _ V span: {w}.
	P _ V span: {v. w}.
	self assert: S + T = P.
	self assert: T + S = P.
	T _ V span: {v - w. v + w}.
	self assert: S + T = P! !

!AbstractModuleTest methodsFor: 'other' stamp: 'len 3/24/2021 09:15:57'!
testTuples
	self testFreeModule: (scalars ^ ((0 to: 5) atRandom: random))! !

!AbstractCommutativeModuleTest methodsFor: 'adjoint' stamp: 'len 7/26/2021 18:08:35'!
testAdjoint
	| V W f fAdjoint phi |
	V _ self newFreeModule.
	W _ self newFreeModule.
	f _ V => W atRandom: random bits: 50.
	fAdjoint _ f adjoint.
	self assert: fAdjoint domain = f codomain dual.
	self assert: fAdjoint codomain = f domain dual.
	phi _ W dual atRandom: random bits: 20.
	self assert: (fAdjoint value: phi) = (phi ∑ f)! !

!AbstractCommutativeModuleTest methodsFor: 'adjoint' stamp: 'len 7/26/2021 18:08:47'!
testAdjointAndHomFunctor
	"Given a R-linear map f, applying the Hom(-, R) produces the adjoint (the dual map).
	Example from 'A Singular Introduction To Commutative Algebra, pp. 115, procedure kontraHom."
	| f |
	f _ self newFreeModule => self newFreeModule :: atRandom: random bits: 100.
	self assert: (f => scalars asIdeal) = f adjoint! !

!AbstractCommutativeModuleTest methodsFor: 'adjoint' stamp: 'len 7/26/2021 18:08:53'!
testAdjointProperties
	| V W U f g a |
	V _ self newFreeModule.
	W _ self newFreeModule.
	f _ V => W atRandom: random bits: 100.
	g _ V => W atRandom: random bits: 100.
	a _ scalars atRandom: random bits: 10.
	self assert: (f + g) adjoint = (f adjoint + g adjoint).
	self assert: (f * a) adjoint = (f adjoint * a).
	U _ self newFreeModule.
	f _ U => V atRandom: random bits: 100.
	self assert: (g ∑ f) adjoint = (f adjoint ∑ g adjoint)! !

!AbstractCommutativeModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:13'!
testTensorProductAssociativity
	| V1 V2 V3 |
	V1 _ self newSmallModule.
	V2 _ self newSmallModule.
	V3 _ self newSmallModule.
	self assert: (V1 ë V2) ë V3 ~ (V1 ë (V2 ë V3))! !

!AbstractCommutativeModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:19'!
testTensorProductCommutativity
	| V W |
	V _ self newSmallModule.
	W _ self newSmallModule.
	self assert: V ë W ~ (W ë V)! !

!AbstractCommutativeModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:23'!
testTensorProductDimension
	| V1 V2 |
	V1 _ self newSmallFreeModule.
	V2 _ self newSmallFreeModule.
	self assert: (V1 ë V2) rank = (V1 rank * V2 rank)! !

!AbstractCommutativeModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:28'!
testTensorProductDistributivity
	| A B C |
	A _ self newSmallFreeModule.
	B _ self newSmallFreeModule.
	C _ self newSmallFreeModule.
	self assert: A ê B ë C ~ (A ë C ê (B ë C))! !

!AbstractCommutativeModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:34'!
testTensorProductDualHomIsomorphism
	| V W A B |
	V _ self newFreeModule.
	W _ self newFreeModule.
	A _ V dual ë W.
	B _ V => W.
	self assert: A ~ B.
"	phi _ A to: B evaluating: [:t| B evaluating: [:v| t second * (t first value: v)]].
	self testLinearMap: phi.
	self assert: phi isIsomorphism"! !

!AbstractCommutativeModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:39'!
testTensorProductDualIsomorphism
	| V W A B |
	V _ self newFreeModule.
	W _ self newFreeModule.
	A _ V dual ë W dual.
	B _ (V ë W) dual.
	self assert: A ~ B.
"	phi _ A to: B evaluating: [:s| B evaluating: [:t| (s first value: t first) * (s second value: t second)]].
	self testLinearMap: phi.
	self assert: phi isIsomorphism"! !

!AbstractCommutativeModuleTest methodsFor: 'tensor product' stamp: 'len 12/27/2021 08:09:43'!
testTensorProductRelations
	| V W T v v1 v2 w w1 w2 c |
	V _ self newFreeModule.
	W _ self newFreeModule.
	T _ V ë W.
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

!AbstractCommutativeModuleTest methodsFor: 'bugs' stamp: 'len 1/3/2022 11:24:40'!
testMapDirectSumAndTensorProductBug
	| V W f g |
	V _ self newFreeModule.
	W _ self newFreeModule.
	f _ V endomorphisms atRandom: random bits: 100.
	g _ W endomorphisms atRandom: random bits: 100.
	"doing it twice should give the same thing:"
	self assert: (fëg) domain = (fëg) domain.
	self assert: (fêg) domain = (fêg) domain! !

!AbstractVectorSpaceTest methodsFor: 'maps' stamp: 'len 3/24/2021 08:02:36'!
testInverse
	| V f |
	V _ self newVectorSpace.
	[(f _ V endomorphisms atRandom: random bits: 1000) isIsomorphism] whileFalse.
	self assert: (f ∑ f inverse) isIdentity.
	self assert: (f inverse ∑ f) isIdentity! !

!AbstractVectorSpaceTest methodsFor: 'maps' stamp: 'len 3/24/2021 08:11:49'!
testRetraction
	| V W f r |
	[V _ self newVectorSpace.
	W _ self newVectorSpace.
	f _ V => W atRandom: random bits: 100. f isInjective] whileFalse.
	r _ f retraction.
	self assert: r isSurjective.
	self assert: r ∑ f = V id! !

!AbstractVectorSpaceTest methodsFor: 'maps' stamp: 'len 3/24/2021 08:12:10'!
testSection
	| V W f s |
	[V _ self newVectorSpace.
	W _ self newVectorSpace.
	f _ V => W atRandom: random bits: 50. f isSurjective] whileFalse.
	s _ f section.
	self assert: s isInjective.
	self assert: f ∑ s = W id! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 3/24/2021 08:23:12'!
testAnnihilator
	| V v S S0 |
	[(V _ self newVectorSpace) isTrivial] whileTrue.
	[(v _ V atRandom: random bits: 100) isZero] whileTrue.
	S _ V span: {v}.
	S0 _ S annihilatorSpace.
	self assert: S0 rank = S corank.
	S0 generators do: [:f| self assert: (f value: v) isZero]! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 3/24/2021 09:04:59'!
testAnnihilatorProperties
	| V n m S T |
	V _ self newVectorSpace.
	n _ (0 to: V rank) atRandom: random.
	S _ (V grassmannian: n) atRandom: random bits: (100 atRandom: random).
	m _ (0 to: n) atRandom: random.
	T _ (S grassmannian: m) atRandom: random bits: (100 atRandom: random).
	self assert: T <= S.
	self assert: T annihilatorSpace >= S annihilatorSpace. "reverses inclusions"
	self assert: (S + T) annihilatorSpace = (S annihilatorSpace /\ T annihilatorSpace).
	self assert: S rank + S annihilatorSpace rank = V rank.
	self assert: V annihilatorSpace isTrivial.
"	self assert: S annihilator annihilator = S dual dual"! !

!AbstractVectorSpaceTest methodsFor: 'other' stamp: 'len 3/24/2021 09:16:15'!
testGrassmannRelation
	| V S T |
	V _ self newVectorSpace.
	S _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (20 atRandom: random).
	T _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (20 atRandom: random).
	self assert: S rank + T rank = ((S + T) rank + (S /\ T) rank)! !

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

!AbstractVectorSpaceTest methodsFor: 'quotients' stamp: 'len 8/22/2021 17:19:44'!
testQuotientMaps
	| V S Q é v s |
	V _ self newVectorSpace.
	S _ V grassmannian: ((0 to: V rank) atRandom: random) :: atRandom: random bits: 50.
	Q _ V/S.
	é _ V -> Q.
	v _ V atRandom: random bits: 100.
	self assert: (Q includes: (é value: v)).
	s _ S atRandom: random bits: 100.
	self assert: (é value: s) isZero.
	self assert: (é value: v) = (é value: v+s)! !

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

!AbstractVectorSpaceTest methodsFor: 'tensor product' stamp: 'len 7/21/2021 18:05:27'!
testTensorHomAdjunction
	| V W U A B phi |
	V _ self newVectorSpace.
	W _ self newVectorSpace.
	U _ self newVectorSpace.
	A _ VëW => U.
	B _ V => (W => U).
	self assert: A ~ B.
	phi _ A to: B evaluating: [:f| B evaluating: [:v| W to: U evaluating: [:w| f value: A domain !! {v. w}]]].
	self testModuleMap: phi.
	self assert: phi isIsomorphism! !

!AbstractVectorSpaceTest methodsFor: 'tensor product' stamp: 'len 12/15/2021 14:29:26'!
testTensorProductOfLinearForms
	| V W f g t v w |
	V _ self newFreeModule.
	W _ self newFreeModule.
	f _ V dual atRandom: random bits: 10.
	g _ W dual atRandom: random bits: 10.
	t _ f ë g.
	self assert: t domain = (V ë W).
	self assert: t codomain = (f codomain ë g codomain).
	v _ V atRandom: random bits: 10.
	w _ W atRandom: random bits: 10.
	self assert: (t value: t domain !! {v. w}) = (t codomain !! {f value: v. g value: w})! !

!AbstractVectorSpaceTest methodsFor: 'private' stamp: 'len 3/24/2021 08:02:48'!
newVectorSpace
	^ self newFreeModule! !

!AbstractVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 10:52:43'!
testAdjointAndAnnihilator
	| V W f a |
	V _ scalars ^ (3 atRandom: random).
	W _ scalars ^ (5 atRandom: random).
	f _ V => W atRandom: random bits: 100.
	a _ f adjoint.
	self assert: a kernel = f image annihilatorSpace.
	self assert: a image = f kernel annihilatorSpace.
	self assert: f isSurjective = a isInjective.
	self assert: f isInjective = a isSurjective! !

!AbstractVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 4/6/2021 14:55:41'!
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

!AbstractVectorSpaceTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:34:32'!
testSubspaceSumRandom
	| V S T U |
	V _ self newVectorSpace.
	S _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (100 atRandom: random).
	T _ (V grassmannian: ((0 to: V rank) atRandom: random)) atRandom: random bits: (100 atRandom: random).
	U _ S + T.
	self assert: S <= U.
	self assert: T <= U! !

!VectorSpaceOverBinaryFieldTest methodsFor: 'initialization' stamp: 'len 3/24/2021 08:37:49'!
initialize
	super initialize.
	scalars _ É/2! !

!VectorSpaceOverBinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:21:35'!
testKernelZ2
	| V f kernel |
	V _ (PrimeField new: 2) ^ 3.
	f _ V dual evaluating: [:v| v sum].
	kernel _ f kernel.
	self assert: (kernel apply: f) isTrivial.
	self assert: kernel rank = 2.
	self assert: (kernel includes: V !! (0,1,1))! !

!VectorSpaceOverBinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:21:13'!
testLinearFormZ2
	| V f |
	V _ (PrimeField new: 2) ^ 4.
	f _ V dual evaluating: [:v| v sum].
	self assert: f domain = V.
	self assert: f kernel rank = 3.
	self assert: f image rank = 1.
	self assert: (f kernel includes: V !! (1,1,1,1))! !

!VectorSpaceOverBinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:40:45'!
testModuleMapZ2
	| V f kernel v |
	V _ (PrimeField new: 2) ^ ((5 atRandom: random) + 1).
	[f _ V endomorphisms atRandom: random.
	(kernel _ f kernel) isTrivial or: [kernel = V]] whileTrue. "find one with non-trivial kernel"
	[v _ V atRandom: random. kernel includes: v] whileTrue.
	self assert: (f value: v) isZero not.
	v _ kernel atRandom: random.
	self assert: (f value: v) isZero! !

!VectorSpaceOverBinaryFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:18:13'!
testPerpZ2
	| V S perp |
	V _ (É / 2) ^ 3.
	S _ V span: {(1,1,1) % 2}.
	perp _ S complement.
	self assert: perp rank = 2.
	self assert: (S basis first ∑ perp basis first) isZero.
	self assert: (S basis first ∑ perp basis second) isZero! !

!VectorSpaceOverCyclotomicFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/15/2021 12:47:15'!
initialize
	super initialize.
	scalars _ CyclotomicField new: 3 atRandom + 2! !

!VectorSpaceOverFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/24/2021 07:51:41'!
initialize
	super initialize.
	scalars _ GaloisField new: 10 atRandom nextPrime to: 3 atRandom! !

!VectorSpaceOverQuadraticFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/23/2021 13:51:16'!
initialize
	super initialize.
	scalars _ QuadraticField sqrt: -5! !

!VectorSpaceOverRationalsTest methodsFor: 'initialization' stamp: 'len 3/23/2021 13:49:39'!
initialize
	super initialize.
	scalars _ Ñ! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:23:17'!
testLinearForm1
	| V f |
	V _ Ñ ^ 3.
	f _ V dual evaluating: [:v| v x + v y - v z].
	self assert: (f value: (1,1,1)) = 1.
	self assert: (f value: (2,2,1)) = 3.
	self assert: (f value: (3,1, -1)) = 5! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:23:24'!
testLinearForm2
	| V f |
	V _ Ñ ^ 3.
	f _ V dual evaluating: [:v| v x + v y - v z].
	self assert: f domain = V.
	self assert: f kernel rank = 2.
	self assert: f image rank = 1! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:41:14'!
testLinearFormAsPolynomial
	| V f x |
	V _ Ñ ^ (5 atRandom: random).
	f _ V dual atRandom: random bits: 100.
	x _ V atRandom: random bits: 100.
	self assert: (f value: x) = (f asPolynomial value: x).
	self assert: f parent !! f asPolynomial = f! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:43:40'!
testMapsInducedByHom
	"Example from 'A Singular Introduction to Commutative Algebra', Example 2.1.7 (Maps Induced by Hom)."
	| X Y Z f g h yz zx |
	X _ Ñ^3.
	Y _ Ñ^3.
	Z _ Ñ^2.
	f _ X to: Y matrix: (Ñ^3) endomorphisms !! (1 to: 9).
	g _ f => Z. "Hom(-,Z)"
	self assert: g domain = (Y => Z). "it is contravariant"
	self assert: g codomain = (X => Z).
	self assert: g asMatrix = ((Ñ^2) id ë f asMatrix transpose).
	yz _ Y => Z atRandom: random bits: 100.
	self assert: (g value: yz) = (yz ∑ f).
	h _ Z => f. "Hom(Z,-)"
	self assert: h domain = (Z => X). "it is covariant"
	self assert: h codomain = (Z => Y).
	self assert: h asMatrix = (f asMatrix ë (Ñ^2) id).
	zx _ Z => X atRandom: random bits: 100.
	self assert: (h value: zx) = (f ∑ zx)! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:25:50'!
testPerpExample
	| V S p perp |
	V _ Ñ ^ 3.
	S _ V span: {(1, 1, 1). (2, 0, -2)}.
	perp _ S complement.
	self assert: perp rank = 1.
	self assert: (perp basis first ∑ S basis first) isZero.
	self assert: (perp basis first ∑ S basis second) isZero.
	p _ S orthogonalProjection.
	self assert: p kernel = perp! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:18:35'!
testPerpProperties
	| V S T |
	V _ Ñ ^ 3.
	S _ V span: {(1, 1, 1). (2, 0, -2)}.
	T _ V span: {(1, -1, -3)}. "subspace of S"
	self assert: S + S complement = V.
	self assert: T + T complement = V.
	self assert: (S /\ S complement) isTrivial.
	self assert: S <= S complement complement.
	self assert: S complement < T complement "because S > T"! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:24:53'!
testQuotientSpaceExample
	| V M x |
	V _ Ñ^3.
	M _ V / {V !! (1,2,3)}.
	x _ V -> M value: (1,2,3).
	self assert: x isZero.
"	self assert: (M lifting value: x) = (0,0,0)"! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:25:40'!
testSubspaceIntersectionAndSumExample
	"Example from wikipedia Zassenhaus algorithm"
	| V U W |
	V _ Ñ ^ 4.
	U _ V span: {(1,-1,0,1). (0,0,1,-1)}.
	W _ V span: {(5,0,-3,3). (0,5,-3,-2)}.
	self assert: U + W = (V span: {(1,0,0,0). (0,1,0,-1). (0,0,1,-1)}).
	self assert: U /\ W = (V span: {(1,-1,0,1)})! !

!VectorSpaceOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:25:31'!
testSubspaceIntersectionExample
	| V S T T2 |
	V _ Ñ ^ 3.
	S _ V span: {(1, 1, 1). (2, 0, -2)}.
	self assert: S /\ V = S.
	self assert: S /\ S = S.
	T _ V span: {(3,1,-1)}.
	self assert: S /\ T = T.
	T2 _ V span: {(3,1,-1). (0,1,0)}.
	self assert: S /\ T2 = T! !

!ModuleOverCyclotomicRingTest methodsFor: 'as yet unclassified' stamp: 'len 7/26/2021 10:20:31'!
initialize
	super initialize.
	scalars _ (CyclotomicField new: 3 atRandom + 2) integers! !

!ModuleOverGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 10:56:30'!
initialize
	super initialize.
	scalars _ GaloisRing new: 3 atRandom nextPrime to: 2 atRandom to: 2 atRandom! !

!ModuleOverGaussianIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 1/7/2022 13:52:03'!
initialize
	super initialize.
	scalars _ GaussianField new integers! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/23/2021 13:49:57'!
initialize
	super initialize.
	scalars _ É! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 3/24/2021 08:28:50'!
testFreeModuleDiscriminant
	"From Sage documentation of FreeModule."
	| M |
	M _ É^3.
	self assert: M discriminant = 1.
	self assert: (M span: {(1,2,3)}) discriminant = 14.
	self assert: (M span: {(1,2,3). (1,1,1)}) discriminant = 6! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:22:19'!
testHomBug
	| V |
	V _ É^1 / {5}.
	self assert: V dual isTrivial.
	self assert: V dual invariants isEmpty! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:37:39'!
testHomBug2
	| V W |
	V _ É^1 / {2}.
	W _ É^(5 atRandom: random).
	self assert: (V => W) isTrivial! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 7/17/2021 19:40:40'!
testHomExample
	| A B H |
	A _ (É/4) asAbelianGroup.
	B _ (É/2) asAbelianGroup.
	H _ A => B.
	self assert: H generators size = 1.
	self assert: H generator = (A to: B evaluating: [:x| B elementAt: {x representative \\ 2}]).
	self assert: H size = 2.
	H _ B => A.
	self assert: H generators size = 1.
	self assert: H generator = (B to: A evaluating: [:x| A elementAt: {x representative * 2}]).
	self assert: H size = 2! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:10:57'!
testInvariantsAndIsomorphismExample
	| M |
	M _ É^3 / {(0,-2,1). (-2,0,1)}.
	self assert: M invariants = #(0 2).
	self assert: M ~ (É^1 / {2} ê (É^1))! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:11:08'!
testInvariantsAndIsomorphismExample2
	| M |
	M _ É^3 / {(2,1,0)}.
	self assert: M invariants = #(0 0).
	self assert: M ~ (É^2)! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:11:19'!
testInvariantsAndIsomorphismExample3
	| M |
	M _ É^1 / {2} ê (É^1) / {(1,2)}.
	self assert: M invariants = #(4).
	self assert: M ~ (É^1/{4})! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:42:48'!
testModuleMapBug
	| M f |
	M _ É^1 / {2}.
	f _ M dual atRandom: random bits: 100.
	self testModuleMap: f! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:20:16'!
testModuleMapBug2
	| M N |
	M _ É^1/{3}.
	N _ É^1.
	"these are not valid module homomorphisms:"
	self should: [M to: N mapping: [:i| i]] raise: DomainError withMessageText: 'invalid morphism'.
	self should: [M to: N mapping: [:i| i*2]] raise: DomainError withMessageText: 'invalid morphism'! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:24:01'!
testQuotientModuleAnnihilator
	"Example from Sage (removed denominators)"
	| v1 v2 v3 V W |
	v1 _ (1,0,0). v2 _ (3,4,2). v3 _ (0,0,2).
	V _ É^3 span: {v1. v2. v3}.
	W _ V ambient span: {v1 + (v2*2). v1*9 + (v2*2). v3*4}.
	self assert: (V/W) annihilator = (É*16)
	
"original with denominators:
	| v1 v2 v3 V W |
	v1 _ (1/2,0,0). v2 _ (3/2,2,1). v3 _ (0,0,1).
	V _ QQ^3 span: {v1. v2. v3} over: É.
	W _ QQ^3 span: {v1 + (v2*2). v1*9 + (v2*2). v3*4} over: É.
	self assert: (V/W) annihilator = (É*16)
"! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 7/18/2021 12:46:45'!
testQuotientModuleBug
	| M a |
	M _ É^2/{(0,2)}.
	a _ M !! (3,0).
	self assert: a representative = (3,0).
	self assert: (a*2) representative = (6,0)! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:24:11'!
testQuotientModuleInvariants
	"Example from Sage (removed denominators)"
	| v1 v2 v3 V W |
	v1 _ (1,2,2). v2 _ (3,4,2). v3 _ (0,0,2).
	V _ É^3 span: {v1. v2. v3}.
	W _ V ambient span: {v1*2 + (v2*4). v1*9 + (v2*12). v3*4}.
	self assert: (V/W) invariants = #(4 12)

"original with denominators:
	| v1 v2 v3 V W |
	v1 _ (1/2,1,1). v2 _ (3/2,2,1). v3 _ (0,0,1).
	V _ QQ^3 span: {v1. v2. v3} over: É.
	W _ QQ^3 span: {v1*2 + (v2*4). v1*9 + (v2*12). v3*4} over: É.
	self assert: (V/W) invariants = #(4 12)
"! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:24:19'!
testQuotientModuleInvariants2
	"Example from Sage"
	| V W |
	V _ É^3.
	W _ V span: {(1,2,0). (0,1,0). (0,2,0)}.
	self assert: (V/W) invariants = #(0) "from example in sage fgp_module.py"! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:24:30'!
testQuotientModuleIsFinite
	"Example from Sage (removed denominators)"
	| v1 v2 v3 V W |
	v1 _ (1,0,0). v2 _ (3,4,2). v3 _ (0,0,2).
	V _ É^3 span: {v1. v2. v3}.
	W _ V ambient span: {v1 + (v2*2). v1*9 + (v2*2). v3*4}.
	self assert: (V/W) isFinite.
	W _ V null.
	self assert: (V/W) isFinite not

"original with denominators:
	| v1 v2 v3 V W |
	v1 _ (1/2,0,0). v2 _ (3/2,2,1). v3 _ (0,0,1).
	V _ QQ^3 span: {v1. v2. v3} over: É.
	W _ QQ^3 span: {v1 + (v2*2). v1*9 + (v2*2). v3*4} over: É.
	self assert: (V/W) isFinite.
	W _ V null.
	self assert: (V/W) isFinite not
"! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:24:39'!
testQuotientModuleSpanBug
	"The span is the original module, but it answers a submodule."
	| M |
	M _ É^2 / {(1,2)}.
	self assert: M = (M span: M generators)! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:25:11'!
testSubmoduleIntersectionExample
	| M S T T2 |
	M _ É ^ 3.
	S _ M span: {(1, 1, 1). (2, 0, -2)}.
	self assert: S /\ M = S.
	self assert: M /\ S = S.
	self assert: S /\ S = S.
	T _ M span: {(3,1,-1)}.
	self assert: S /\ T = T.
	T2 _ M span: {(3,1,-1). (0,1,0)}.
	self assert: S /\ T2 = T! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:17:27'!
testSubquotientContainsBug
	| V v S |
	V _ É / 5 :: asAbelianGroup.
	v _ V elementAt: #(4).
	S _ V span: {v}.
	self assert: (S contains: v * 3)! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:42:58'!
testSyzygies1
	| V |
	V _ É^2 / {(2,0)}.
	self assert: (V elementAt: (V syzygies atRandom: random bits: 100)) isZero! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:43:04'!
testSyzygies2
	| V W X |
	V _ É^2 / {(2,0)}.
	W _ É^3 / {(2,2,2)}.
	X _ V => W.
	self assert: (X elementAt: (X syzygies atRandom: random bits: 100)) isZero! !

!ModuleOverIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:13:27'!
testTensorProductExample
	self assert: É^1 ë (É^1/{2}) ~ (É^1/{2})! !

!ModuleOverModularIntegersTest methodsFor: 'initialization' stamp: 'len 4/2/2021 11:13:47'!
initialize
	super initialize.
	scalars _ É/(2 to: 64) atRandom! !

!ModuleOverModularIntegersTest methodsFor: 'as yet unclassified' stamp: 'len 4/5/2021 04:22:53'!
testExampleModulesOverZ12
	| M A B |
	M _ É/12^3.
	A _ M span: {(4,1,0). (0,0,5)}.
	B _ M span: {(8,5,5). (0,9,8). (0,0,10)}.
	self assert: A = B! !

!ModuleOverMultivariatePolynomialsOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2021 14:16:08'!
initialize
	super initialize.
	scalars _ Ñ polynomialsIn: 2! !

!ModuleOverPolynomialsOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 3/23/2021 13:50:37'!
initialize
	super initialize.
	scalars _ Ñ polynomials! !

!ModuleOverQuadraticRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 08:34:16'!
initialize
	| n |
	super initialize.
	[(n _ (2 to: 100) atRandom squarefreePart) > 1] whileFalse.
	2 atRandom = 1 ifTrue: [n _ n negated].
	scalars _ (QuadraticField sqrt: n) integers! !

!AbstractModuleTODOTest methodsFor: 'as yet unclassified' stamp: 'len 7/25/2021 13:09:01'!
testPullbackSquaresSharingMorphism
	| A B C D f g h P r s Q t u |
	C _ self newSmallFreeModule.
	A _ self newSmallFreeModule.
	B _ self newSmallFreeModule.
	D _ self newSmallFreeModule.
	f _ A => C atRandom: random bits: 20.
	g _ B => C atRandom: random bits: 20.
	h _ D => B atRandom: random bits: 20.
	P _ f /\ g.
	r _ P -< (A, B) at: 1.
	s _ P -< (A, B) at: 2.
	Q _ s /\ h.
	t _ Q -< (P, D) at: 1.
	u _ Q -< (P, D) at: 2.
	self assert: (r∑t) -> f = (u -> (g∑h))! !

!AbstractModuleTODOTest methodsFor: 'as yet unclassified' stamp: 'len 7/25/2021 13:09:18'!
testPushoutSquaresSharingMorphism
	| A B C D f g h P i j Q k l |
	C _ self newSmallFreeModule.
	A _ self newSmallFreeModule.
	B _ self newSmallFreeModule.
	D _ self newSmallFreeModule.
	f _ C => A atRandom: random bits: 20.
	g _ C => B atRandom: random bits: 20.
	h _ B => D atRandom: random bits: 20.
	P _ f \/ g.
	i _ (A, B) >- P at: 1.
	j _ (A, B) >- P at: 2.
	Q _ j \/ h.
	k _ (P, D) >- Q at: 1.
	l _ (P, D) >- Q at: 2.
	self assert: f -> (k∑i) = (h∑g -> l)! !

!ModuleOverCliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 7/26/2021 10:19:57'!
initialize
	super initialize.
	scalars _ CliffordAlgebra on: (QuadraticForm from: Ñ^3 coefficients: #(1 0 0 1 0 1))! !

!ModuleOverMatricesTest methodsFor: 'as yet unclassified' stamp: 'len 8/22/2021 12:38:15'!
initialize
	super initialize.
	scalars _ (É/256 atRandom nextPrime^2 atRandom) endomorphisms! !

!ModuleOverQuaternionsTest methodsFor: 'as yet unclassified' stamp: 'len 7/25/2021 12:09:23'!
initialize
	super initialize.
	scalars _ QuaternionAlgebra over: Ñ invariants: #(1 1)! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/3/2022 10:32:18'!
testDivisionByUnits: aRing
	100 timesRepeat:
		[| x u |
		x _ aRing atRandomBits: 30.
		u _ aRing units atRandomBits: 30.
		self assert: x*u / u = x.
		self assert: x / u = (x * u inverse)]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 12/26/2021 12:17:45'!
testRing
	self testRing: R! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/3/2022 10:31:23'!
testRing: aRing
	self testRingAxioms: aRing.
	self testRingUnits: aRing.
	aRing hasCanonicalAssociates ifTrue: [self testRingCanonicalAssociates: aRing].
	aRing isInvolutive ifTrue: [self testRingInvolution: aRing]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 21:51:54'!
testRingAxioms: aRing
	| one zero a b c |
	one _ aRing one.
	zero _ aRing zero.
	100 timesRepeat:
		[a _ aRing atRandom: random bits: 20.
		b _ aRing atRandom: random bits: 20.
		c _ aRing atRandom: random bits: 20.
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

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/3/2022 14:12:31'!
testRingCanonicalAssociates: aRing
	100 timesRepeat:
		[| x u |
		x _ aRing atRandom: random bits: 30.
		u _ (aRing atRandom: random bits: 30) normalization.
		self assert: x normalization isUnit.
		self assert: u normalization = u inverse.
		self assert: u * u inverse = aRing one.
		self assert: u normalized = aRing one.
		self assert: x normalization * x = x normalized.
		self assert: x normalized = (x * u) normalized]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 7/6/2021 10:53:01'!
testRingHomomorphism: phi
	| A B x y |
	A _ phi domain.
	B _ phi codomain.
	x _ A atRandom: random bits: 10.
	y _ A atRandom: random bits: 10.
	self assert: (phi value: A zero) = B zero.
	self assert: (phi value: A one) = B one.
	self assert: (phi value: x + y) = ((phi value: x) + (phi value: y)).
	self assert: (phi value: x * y) = ((phi value: x) * (phi value: y))! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 7/25/2021 12:30:01'!
testRingInvolution: aRing
	| a b |
	self assert: aRing one conjugate = aRing one.
	100 timesRepeat:
		[a _ aRing atRandom: random bits: 20.
		b _ aRing atRandom: random bits: 20.
		self assert: (a + b) conjugate = (a conjugate + b conjugate).
		self assert: (a * b) conjugate = (b conjugate * a conjugate).
		self assert: a conjugate conjugate = a]! !

!AbstractRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/3/2022 10:30:09'!
testRingUnits: aRing
	10 timesRepeat:
		[| u |
		u _ aRing units atRandom: random bits: 30.
		self assert: u isUnit.
		self assert: u * u inverse = aRing one]! !

!AbstractAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:42:41'!
testAlgebra
	self testAlgebraAxioms: R! !

!AbstractAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/15/2021 19:41:50'!
testAlgebraAxioms: anAlgebra
	| one zero x y z a |
	one _ anAlgebra one.
	zero _ anAlgebra zero.
	100 timesRepeat:
		[x _ anAlgebra atRandom: random bits: 20.
		y _ anAlgebra atRandom: random bits: 20.
		z _ anAlgebra atRandom: random bits: 20.
		a _ anAlgebra scalars atRandom: random bits: 20.
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
		anAlgebra isCommutative ifTrue: [self assert: x * y = (y * x)]]! !

!AbstractAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/21/2021 13:40:02'!
testScalarEmbedding
	| a |
	self testRingHomomorphism: R scalars -> R.
	a _ R scalars atRandom: random bits: 20.
	self assert: (R scalars -> R value: a) = (R one * a).
	self assert: R !! a = (R one * a)! !

!AbstractFiniteAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/21/2021 13:59:18'!
testCharacteristicPolynomialAndMinimalPolynomial
	| x f µ |
	x _ R atRandom: random bits: 20.
	f _ x characteristicPolynomial.
	self assert: f isMonic.
	self assert: (f value: x) isZero.
	R scalars isIntegralDomain ifTrue:
		[µ _ x minimalPolynomial.
		self assert: µ isMonic.
		self assert: (µ value: x) isZero.
		self assert: µ | f]! !

!AbstractFiniteAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/21/2021 14:02:15'!
testNormAndTrace
	| x y a |
	x _ R atRandom: random bits: 100.
	y _ R atRandom: random bits: 100.
	a _ R scalars atRandom: random bits: 10.
	self assert: (x * a + y) trace = (x trace * a + y trace).
	self assert: (x * y) norm = (x norm * y norm)! !

!AbstractMaximalOrderTest methodsFor: 'as yet unclassified' stamp: 'len 12/17/2021 12:15:56'!
testCoercionToAndFromFractions
	| x |
	x _ R atRandomBits: 100.
	self assert: R !! (R fractions !! x) = x! !

!AbstractMaximalOrderTest methodsFor: 'as yet unclassified' stamp: 'len 12/17/2021 12:25:21'!
testHomomorphismToFractions
	self testRingHomomorphism: R -> R fractions! !

!CyclotomicRingTest methodsFor: 'as yet unclassified' stamp: 'len 12/17/2021 12:13:25'!
initialize
	super initialize.
	R _ (CyclotomicField new: 3 atRandom + 2) integers! !

!QuadraticRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 08:34:42'!
initialize
	| n |
	super initialize.
	[(n _ (2 to: 100) atRandom squarefreePart) > 1] whileFalse.
	2 atRandom = 1 ifTrue: [n _ n negated].
	R _ (QuadraticField sqrt: n) integers! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 7/25/2021 12:21:35'!
initialize
	super initialize.
	R _ CliffordAlgebra on: (QuadraticForm from: Ñ^3 coefficients: #(1 0 0 1 0 1))! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:19:57'!
testCliffordAlgebraDualGrade
	| x k |
	k _ (0 to: R degree) atRandom: random.
	x _ (R grade: k) atRandom: random bits: 10.
	self assert: ((R grade: R degree - k) includes: x dual)! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:19:00'!
testCliffordAlgebraDualProperties
	| x y |
	x _ R atRandom: random bits: 10.
	y _ R atRandom: random bits: 10.
	self assert: x <∑ y dual = (x /\ y) dual.
	self assert: x /\ y dual = (x <∑ y) dual! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:20:22'!
testCliffordAlgebraEvenSubalgebra
	| E x |
	E _ R evenSubalgebra.
	x _ E atRandom: random bits: 10.
	self assert: x = x reflection! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:21:03'!
testCliffordAlgebraGradeInvolution
	| x y v |
	x _ R atRandom: random bits: 10.
	y _ R atRandom: random bits: 10.
	v _ R !! (R space atRandom: random bits: 10).
	self assert: (x*y) reflection = (x reflection * y reflection).
	self assert: v reflection = v negated! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:21:54'!
testCliffordAlgebraGradeProjection
	| x k j |
	k _ (0 to: R degree) atRandom: random.
	[j _ (0 to: R degree) atRandom: random. j = k] whileTrue.
	x _ R atRandom: random bits: 20.
	self assert: ((R grade: k) includes: (x grade: k)).
	self assert: (((R grade: j) includes: (x grade: k)) not or: [(x grade: k) isZero]).
	x _ (R grade: k) atRandom: random bits: 10.
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
	V _ Ñ^3.
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

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:24:46'!
testCliffordAlgebraProductsProperties
	| x y z one |
	x _ R atRandomBits: 10.
	y _ R atRandomBits: 10.
	z _ R atRandomBits: 10.
	"Associativity of wedge product:"
	self assert: x /\ (y /\ z) = (x /\ y /\ z).
	"Associativity of left and right interior products:"
	self assert: x <∑ (y ∑> z) = (x <∑ y ∑> z).
	self assert: x <∑ (y <∑ z) = (x /\ y <∑ z).
	self assert: x ∑ (y <∑ z) = (x /\ y ∑ z).
	one _ R one.
	self assert: one /\ x = x.
	self assert: x /\ one = x.
	self assert: one <∑ x = x.
	self assert: x ∑> one = x! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:25:34'!
testCliffordAlgebraProductsPropertiesWithVectors
	| x y v |
	x _ R atRandom: random bits: 10.
	y _ R atRandom: random bits: 10.
	v _ R !! (R space atRandom: random bits: 10).
	self assert: v*x = (v <∑ x + (v /\ x)).
	self assert: v <∑ x = (v*x - (x reflection * v) / 2).
	self assert: v <∑ x = (x reflection negated ∑> v).
	self assert: v /\ x = (v*x + (x reflection * v) / 2).
	self assert: v /\ x = (x reflection /\ v).
	self assert: v <∑ (x*y) = ((v <∑ x)*y + (x reflection * (v <∑ y)))! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 1/3/2022 11:47:12'!
testCliffordAlgebraProperties
	self assert: R isCommutative not.
	self assert: R rank = (2^R space rank).
	self assert: R scalars = Ñ.
	self assert: R zero isZero.
	self assert: R one isOne.
	self assert: R id isIdentity! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:25:58'!
testCliffordAlgebraReversion
	| x y v |
	x _ R atRandom: random bits: 10.
	y _ R atRandom: random bits: 10.
	v _ R !! (R space atRandomBits: 10).
	self assert: (x*y) transpose = (y transpose * x transpose).
	self assert: v transpose = v! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:26:41'!
testCliffordAlgebraScalarEmbedding
	| F a x |
	F _ R scalars.
	a _ F atRandomBits: 10.
	x _ R !! a.
	self assert: x isScalar.
	self assert: ((R grade: 0) includes: x).
	self assert: R one * a = x.
	self assert: x asScalar = a! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:27:03'!
testCliffordAlgebraScalarEmbedding2
	| i |
	i _ R scalars asIdeal -> R asVectorSpace.
	self assert: i isInjective.
	self assert: i image = (R grade: 0) asVectorSpace! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:27:23'!
testCliffordAlgebraSpaceEmbedding
	| V v x |
	V _ R space.
	v _ V atRandomBits: 10.
	x _ R !! v.
	self assert: x isVector.
	self assert: ((R grade: 1) includes: x).
	self assert: x asVector = v! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:27:42'!
testCliffordAlgebraSpaceEmbedding2
	| i |
	i _ R space -> R asVectorSpace.
	self assert: i isInjective.
	self assert: i image = (R grade: 1) asVectorSpace! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:28:05'!
testCliffordAlgebraUniversalProperty
	| Q v |
	Q _ R quadraticForm.
	v _ R space atRandomBits: 10.
	self assert: (R !! v) squared = (R one * (Q value: v))! !

!MatrixAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:36:45'!
initialize
	| F |
	super initialize.
	F _ 2 atRandom = 1 ifTrue: [Ñ] ifFalse: [É/64 atRandom nextPrime].
	R _ (F^(0 to: 3) atRandom) endomorphisms! !

!MatrixAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:37:45'!
testMatrixAlgebraRank
	self assert: R rank = R space rank squared! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 7/25/2021 12:20:24'!
initialize
	super initialize.
	R _ QuaternionAlgebra over: Ñ invariants: #(1 1)! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 1/6/2022 09:25:53'!
testQuaternionAlgebraAnnihilatorIdeal
	| H a b I |
	H _  QuaternionAlgebra over: Ñ invariants: #(1 1).
	a _ 1 + H i.
	self assert: a isZerodivisor.
	I _ a annihilatorIdeal.
	self assert: (I includes: 1 - H i).
	self assert: (I includes: H j - H k).
	self assert: (I generators allSatisfy: [:each| (a * each) isZero]).
	b _ H j.
	self assert: b isZerodivisor not.
	I _ b annihilatorIdeal.
	self assert: I isTrivial! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:31:31'!
testQuaternionAlgebraAsJordanAlgebra
	| A J |
	A _ QuaternionAlgebra over: Ñ invariants: #(-5 -2).
	J _ A asJordanAlgebra.
	self assert: J isCommutative.
	self assert: J isAssociative not.
	self assert: J isJordan! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:31:55'!
testQuaternionAlgebraCoercionToAndFromModule
	| A x |
	A _ QuaternionAlgebra over: Ñ invariants: #(-17 -271).
	x _ A !! #(1 -2 3 0).
	self assert: A asModule = (Ñ^4).
	self assert: A asModule !! x = (Ñ^4 !! #(1 -2 3 0)).
	self assert: A !! (A asModule !! #(1 -2 3 0)) = x! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:32:27'!
testQuaternionAlgebraInverse
	| A a |
	A _ QuaternionAlgebra over: Ñ invariants: #(-5 -2).
	[a _ A atRandomBits: 20.
	a isUnit] whileFalse.
	self assert: a * a inverse = A one.
	self assert: a inverse * a = A one! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:32:41'!
testQuaternionAlgebraIsCentral
	| A |
	A _ QuaternionAlgebra over: Ñ invariants: #(-5 -2).
	self assert: A isCentral.
	self assert: A center = (A span: {A one})! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:32:53'!
testQuaternionAlgebraNormAndTraceExample
	"Example from Magma handbook, H92E9."
	| A x |
	A _ QuaternionAlgebra over: Ñ invariants: #(-17 -271).
	x _ A !! #(1 -2 3 0).
	self assert: x trace = 2.
	self assert: x norm = 2508. 
	self assert: x representation trace = 4.
	self assert: (x^2 - (x * x trace) + x norm) isZero! !

!QuaternionAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 16:33:07'!
testQuaternionAlgebraTraceExample
	"Example from Sage."
	| A |
	A _ QuaternionAlgebra over: Ñ invariants: #(-5 -2).
	self assert: A i trace isZero.
	self assert: A j trace isZero.
	self assert: (A !! {1/3. 1/5. 1/7. 1}) trace = (2/3)! !

!AffineAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 15:45:46'!
initialize
	| P |
	super initialize.
	P _ Ñ polynomialsIn: 3 atRandom.
	R _ P / ((1 to: 3 atRandom - 1) collect: [:i| P atRandomBits: 10])! !

!AbstractCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 7/25/2021 12:17:38'!
testFractions
	| F |
	F _ R fractions.
	self testRing: F.
	self testRingHomomorphism: R -> F! !

!AbstractCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/4/2022 18:12:21'!
testIsCommutative
	self assert: R isCommutative! !

!AbstractCommutativeRingTest methodsFor: 'as yet unclassified' stamp: 'len 7/25/2021 12:17:51'!
testPolynomials
	| P |
	P _ R polynomialsIn: (3 atRandom: random).
	self testRingAxioms: P.
	self testRingHomomorphism: R -> P! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/12/2022 18:44:59'!
testEuclideanDivision
	"Test division on a pair of elements:"
	R isTrivial ifTrue: [^ self].
	R size < 300 ifTrue:
		[R do: [:a| R do: [:b| self testEuclideanDivisionOf: a by: b]]. ^ self].
	100 timesRepeat:
		[| a b |
		a _ R atRandom: random bits: 30.
		[b _ R atRandom: random bits: 30. b isZero] whileTrue.
		self testEuclideanDivisionOf: a by: b]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/12/2022 18:42:37'!
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

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/3/2022 11:04:18'!
testExactDivisionCompatibleWithEuclideanDivision
	R isTrivial ifTrue: [^ self].
	100 timesRepeat:
		[| x y xy |
		x _ R atRandom: random bits: 30.
		[y _ R atRandom: random bits: 30. y isZero] whileTrue.
		xy _ x * y.
		self assert: xy / y = (xy // y).
		self assert: (xy \\ y) isZero]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 05:33:19'!
testGCD
	R size < 60 ifTrue:
		[R do: [:a| R do: [:b| self testGCD: a with: b]]. ^ self].
	100 timesRepeat:
		[ | a b |
		[(a _ R atRandom: random bits: 30) isZero] whileTrue.
		[(b _ R atRandom: random bits: 30) isZero] whileTrue.
		self testGCD: a with: b]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 05:32:36'!
testGCD: a with: b
	| gcd |
	gcd _ a gcd: b.
	self assert: gcd | a.
	self assert: gcd | b.
	self assert: gcd = gcd normalized! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 05:33:06'!
testGCDZero
	| a gcd |
	a _ R atRandom: random bits: 100.
	gcd _ a gcd: R zero.
	self assert: gcd = (R zero gcd: a).
	self assert: gcd = a normalized.
	self assert: (R zero gcd: R zero) = R zero! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/4/2022 18:12:54'!
testIsEuclidean
	self assert: R isEuclidean.
	self assert: R isPIR! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 05:34:01'!
testLCM
	R size < 60 ifTrue:
		[R do: [:a| R do: [:b| self testLCM: a with: b]]. ^ self].
	100 timesRepeat:
		[| a b |
		[(a _ R atRandom: random bits: 30) isZero] whileTrue.
		[(b _ R atRandom: random bits: 30) isZero] whileTrue.
		self testLCM: a with: b]! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 05:33:40'!
testLCM: a with: b
	| lcm |
	lcm _ a lcm: b.
	self assert: a | lcm.
	self assert: b | lcm.
	self assert: lcm = lcm normalized! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 05:33:35'!
testLCMZero
	| a lcm |
	a _ R atRandom: random bits: 100.
	lcm _ a lcm: R zero.
	self assert: lcm = (R zero lcm: a).
	self assert: lcm = R zero.
	self assert: (R zero lcm: R zero) = R zero! !

!AbstractEuclideanRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 05:32:16'!
testXXGCD
	R size < 60 ifTrue:
		[R do: [:a| R do: [:b| self testXXGCD: a with: b]]. ^ self].
	100 timesRepeat:
		[| a b |
		a _ R atRandom: random bits: 30.
		b _ R atRandom: random bits: 30.
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

!EuclideanProductRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/4/2022 11:39:20'!
initialize
	super initialize.
	R _ 2 atRandom = 1 ifTrue: [É ◊ É] ifFalse: [É/4 ◊ (É/6)]! !

!GaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 4/21/2021 08:18:32'!
initialize
	super initialize.
	R _ GaloisRing new: 3 atRandom nextPrime to: 2 atRandom to: 2 atRandom! !

!GaussianRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/4/2022 15:21:01'!
initialize
	super initialize.
	R _ GaussianField new integers! !

!LocalIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 12/29/2021 14:17:29'!
initialize
	super initialize.
	R _ É @ 5! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:33:09'!
initialize
	super initialize.
	R _ É/64 atRandom! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 12:04:12'!
testCongruenceSolvingExample1
	| A a b solutions |
	A _ É/20.
	a _ A !! 11.
	b _ A !! 15.
	solutions _ a solve: b.
	self assert: solutions first = (A !! 5).
	self assert: solutions size = 1! !

!ModularIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 12:03:44'!
testCongruenceSolvingExample2
	| A a b solutions |
	A _ É/12167.
	a _ A !! 230.
	b _ A !! 1081.
	solutions _ a solve: b.
	self assert: (solutions includes: (A !! 2491)).
	self assert: solutions size = 23.
	self assert: (solutions allSatisfy: [:x| a*x = b])! !

!PolynomialRingOverFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 12:09:15'!
initialize
	super initialize.
	R _ (	GaloisField new: 5 atRandom nextPrime to: 3 atRandom) polynomials! !

!PolynomialRingOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2021 12:08:03'!
initialize
	super initialize.
	R _ Ñ polynomials! !

!PrimePowerRingTest methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 05:28:18'!
initialize
	| k p |
	super initialize.
	p _ 10 atRandom nextPrime.
	k _ 10 atRandom.
	R _ PrimePowerRing new: p to: k! !

!RationalIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 4/22/2021 10:32:40'!
initialize
	super initialize.
	R _ É! !

!AbstractIntegralDomainTest methodsFor: 'as yet unclassified' stamp: 'len 12/20/2021 17:43:37'!
testZeroIdeal
	| P |
	P _ R * 0.
	self assert: P isPrime.
	self assert: P isMinimalPrime! !

!MultivariatePolynomialRingOverFiniteFieldTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 15:32:40'!
initialize
	super initialize.
	R _ (	GaloisField new: 5 atRandom nextPrime to: 3 atRandom) polynomialsIn: 3 atRandom! !

!MultivariatePolynomialRingOverRationalsTest methodsFor: 'as yet unclassified' stamp: 'len 8/20/2021 15:33:31'!
initialize
	super initialize.
	R _ Ñ polynomialsIn: 3 atRandom! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 5/23/2020 09:33:01'!
testAffinePlaneCurveMultiplicity
	| A C |
	A _ AffineSpace new: #(x y) over: Ñ.
	C _ A subscheme: A coordinateRing !! [:x :y| y^2 - (x^3)].
	self assert: (C multiplicityAt: (1,1)) = 1.
	self assert: (C multiplicityAt: (0,0)) = 2! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 7/8/2020 12:01:44'!
testAffineSchemeCartesianProduct
	| X1 X2 P |
	X1 _ AffineVariety circle.
	X2 _ AffineVariety romanSurface.
	P _ X1 ◊ X2.
	self assert: (P ambient -< (X1 ambient, X2 ambient) collect: [:each| P apply: each]) = {X1. X2}.
	self assert: X1 dimension + X2 dimension = P dimension.
	self assert: X1 ambient dimension + X2 ambient dimension = P ambient dimension.
	self assert: (X1 pointsCountOver: É/3) * (X2 pointsCountOver: É/3) = (P pointsCountOver: É/3).
	self assert: (P satisfies: (5,2,6,2,6) % 7)! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 5/23/2020 09:33:01'!
testAffineSchemeCreation
	| A x y z X Y |
	A _ AffineSpace new: 3 over: Ñ.
	x _ A coordinateRing x.
	y _ A coordinateRing y.
	z _ A coordinateRing z.
	X _ A subscheme: x - y.
	Y _ X subscheme: {x^2 - (z^3). y^3 - (z^4)}.
	self assert: X ambient = A.
	"Y is defined as subscheme of X, so it contains the equations of X plus the two new equations, but its ambient space is still A (not X):"
	self assert: Y ambient = A! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 10/18/2016 20:58'!
testAffineSpace
	| K A |
	K _ PrimeField new: 23.
	A _ AffineSpace new: 3 over: K.
	self assert: A dimension = 3.
	self assert: A scalars = K.
	self assert: A coordinateRing = (K polynomialsIn: 3).
	self assert: A ambient = A.
	self assert: A coordinateRing spec = A! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 5/23/2020 08:56:11'!
testProjectiveClosure
	"From Magma Handbook example Scheme_projective-closure (H118E23)."
	| A1 u v X PX X2 |
	A1 _ AffineSpace new: #(u v) over: É/5.
	u _ A1 coordinateRing x: #u.
	v _ A1 coordinateRing x: #v.
	X _ A1 subscheme: u^2 - (v^5).
	PX _ X projectiveClosure.
	self assert: (PX patch: 3) = X.
	X2 _ PX patch: 2.
	self assert: X2 projectiveClosure = X projectiveClosure! !

!AlgebraicGeometryTest methodsFor: 'affine' stamp: 'len 1/14/2022 11:28:54'!
testProjectiveEmbedding
	| A1 u v X PX affinePointsEmbedded projectivePoints |
	A1 _ AffineSpace new: #(u v) over: É/5.
	u _ A1 coordinateRing x: #u.
	v _ A1 coordinateRing x: #v.
	X _ A1 subscheme: u^2 - (v^5).
	PX _ X projectiveClosure.
	affinePointsEmbedded _ X points asSet apply: X projectiveEmbedding.
	projectivePoints _ PX points asSet.
	self assert: (projectivePoints includesAllOf: affinePointsEmbedded).
	"the projective scheme has one more point at infinity:"
	self assert: affinePointsEmbedded size + 1 = projectivePoints size! !

!AlgebraicGeometryTest methodsFor: 'conics' stamp: 'len 5/23/2020 09:33:01'!
testConicCoefficients
	self assert: (Conic coefficients: (1,2,3,4,5,6)) coefficients = (1,2,3,4,5,6).
	self assert: (Conic polynomial: (Ñ polynomialsIn: 3) !! [:x :y :z| x^2 + (x*y*5) + (y^2) + (z^2)]) coefficients = (1,5,0,1,0,1)! !

!AlgebraicGeometryTest methodsFor: 'conics' stamp: 'len 5/23/2020 09:33:01'!
testConicDeterminant
	self assert: (Conic coefficients: ((1,2,3,4,5,6) over: Ñ)) determinant = (41/4)! !

!AlgebraicGeometryTest methodsFor: 'conics' stamp: 'len 5/23/2020 09:33:01'!
testConicIsSmooth
	self assert: (Conic coefficients: ((1,-1,0) over: Ñ)) isSmooth not.
	self assert: (Conic coefficients: (1,1,1,1,1,0) % 2) isSmooth! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:16'!
testProjectiveLineOverFiniteRing
	| P |
	P _ ProjectiveLine over: É/6.
	"P(Z/6Z) has 12 points: [0:1], [1:1], [2:1], [3:1], [4:1], [5:1], [1:2], [3:2], [5:2], [1:3], [2:3] and [1:0] (Henri Cohen, 'A Course in Computational Algebraic Number Theory', 478p)"
	self assert: P points size = 12.
	self assert: (Iterator on: P points performing: #do:) size = 12! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:27'!
testProjectiveLineOverFiniteRing2AsDirectProduct
	| P |
	P _ ProjectiveLine over: É/2 ◊ (É/2). "isomorphic to É/2[x]/<x(x+1)>"
	self assert: P points size = 9.
	self assert: (Iterator on: P points performing: #do:) size = 9! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:37'!
testProjectiveLineOverFiniteRing2AsQuotient
	| R P |
	R _ (É/2) polynomials.
	R _ R / (R x * (R x + 1)). "isomorphic to É/2 ◊ (É/2)"
	P _ ProjectiveLine over: R.
	self assert: P points size = 9.
	self assert: (Iterator on: P points performing: #do:) size = 9! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:42'!
testProjectiveLineOverFiniteRing4
	| P |
	P _ ProjectiveLine over: É/4 ◊ (É/4).
	self assert: P points size = 36.
	self assert: (Iterator on: P points performing: #do:) size = 36! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:49'!
testProjectiveLineOverFiniteRing5
	| P |
	P _ ProjectiveLine over: É/2 ◊ (É/2) ◊ (É/2).
	self assert: P points size = 27.
	self assert: (Iterator on: P points performing: #do:) size = 27! !

!AlgebraicGeometryTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 08:56:53'!
testProjectiveLineOverFiniteRing6
	| R P |
	R _ (É/2) polynomials.
	R _ R / (R x^3 - R x).
	P _ ProjectiveLine over: R.
	self assert: P points size = 18.
	self assert: (Iterator on: P points performing: #do:) size = 18! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 12/15/2021 12:00:00'!
testDifference
	| A x y C X2 D O |
	A _ AffineSpace new: 2 over: Ñ.
	x _ A coordinateRing x.
	y _ A coordinateRing y.
	C _ A subscheme: x * y. "union of x-axis and y-axis"
	X2 _ A subscheme: x^2. "y-axis with double multiplicity"
	D _ X2 - C. "y-axis with multiplicity 1"
	self assert: D asIdeal = (A coordinateRing * x).
	O _ A subscheme: {x. y}.
	D _ C - O.
	self assert: D = C.
	self assert: (C - A) isEmpty "removing the ambient space removes all"! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testEllipticCurvesAreSmooth
	| R x y C |
	R _ Ñ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	C _ R / (x^3 - (x*3) + 3 - (y^2) homogenizedIn: 3) :: proj.
	self assert: C isSmooth.
	C _ R / (x^3 + x - (y^2) homogenizedIn: 3) :: proj.
	self assert: C isSmooth.
	C _ R / (x^3 - x - (y^2) homogenizedIn: 3) :: proj.
	self assert: C isSmooth! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testEmpty
	| A X |
	A _ Ñ polynomialsIn: #(x y z).
	X _ A / {A continuantOfOrder: 2. A continuantOfOrder: 3} :: spec.
	self assert: X isEmpty! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testEqualVarietiesDifferentIdeals
	| A x I I2 V V2 |
	A _ Ñ polynomials.
	x _ A x.
	I _ A * {x}.
	I2 _ A * {x^2}.
	V _ I variety.
	V2 _ I2 variety.
	self assert: V = V2.
	self assert: I ~= I2! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testEqualVarietiesDifferentIdeals2
	| A x y I I2 V V2 |
	A _ Ñ polynomialsIn: #(x y).
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
	A _ Ñ polynomialsIn: #(x y).
	x _ A x.
	y _ A y.
	X _ A / {x*x - 1. y*y - (x*x) - 3} :: spec.
	self assert: X isFinite! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testIntersection
	| A x y V V2 W |
	A _ Ñ polynomialsIn: #(x y).
	x _ A x.
	y _ A y.
	V _ A / {y} :: spec.
	V2 _ A / {x*x - 1} :: spec.
	W _ V /\ V2.
	self assert: (W satisfies: (1, 0)).
	self assert: (W satisfies: (0, 1)) not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/28/2020 18:15:56'!
testLinearSubspaceAsAlgebraicVariety
	| S V |
	S _ Ñ ^ 3 grassmannian: 2 :: atRandomBits: 10.
	V _ S asAlgebraicVariety.
	self assert: V dimension + 1 = S rank. "because it's projective"
	self assert: (V satisfies: (S x: 1)).
	self assert: (V satisfies: (S x: 2))! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 12/18/2021 07:02:58'!
testLocalAndGlobalDimension
	"'A Singluar Introduction to Commutative Algebra', Example A.8.4 (local and global dimension)."
	| R I X |
	R _ Ñ polynomialsIn: #(x y z).
	I _ R * (R generate: [:x :y :z| {x*z. y*z}]).
	X _ (R / I) spec.
	self assert: X dimension = 2. "global dimension of X"
	self assert: (X !! (0,0,0)) localRing dimension = 2. "dimension of X at (0,0,0)"
	self assert: (X !! (1,0,0)) localRing dimension = 2. "dimension of X at (1,0,0)"
	self assert: (X !! (0,0,1)) localRing dimension = 1. "dimension of X at (0,0,1)"! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 12/15/2021 12:55:03'!
testMorphismsOfProjectiveVarieties
	"'A Singular Introduction To Commutative Algebra', Example A.6.8 (morphisms of projective varieties)."
	| P A x y z X f1 f2 f3 I J Y phi Z |
	P _ ProjectiveSpace new: #(x y z) over: Ü. "å^2à"
	A _ P coordinateRing. "à[x,y,z]"
	x _ A x. y _ A y. z _ A z.
	X _ P subscheme: {z^3 - (x*(y^2)) + (y^3)}.
	f1 _ x*z. f2 _ x*y. f3 _ x^2 + (y*z).
	"In order for (f1,f2,f3) to define a projective morphism (not just a rational map) it must never be zero at any point of X, this means the intersection of X with V(J) must be empty:"
	I _ X asIdeal.
	J _ A * {f1. f2. f3}.
	self assert: (A / (I + J)) dimension = 0.
	Y _ P subscheme: J.
	self assert: (X /\ Y) isEmpty.
	phi _ RationalMap from: X to: P representatives: {f1. f2. f3}.
	"The image is a curve of degree 6:"
	Z _ phi image.
	self assert: Z isCurve.
	self assert: Z degree = 6! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/31/2020 20:38:36'!
testMultiplicity
	"Example from Sage documentation."
	| A X q1 q2 |
	A _ AffineSpace new: 4 over: Ñ.
	X _ A subscheme: (A coordinateRing !! [:x :y :z :w| {z*y - (x^7). w - (2*z)}]).
	q1 _ X !! (Ñ^4 !! {1. (1/3). 3. 6}).
	q2 _ X !! (Ñ^4) zero.
	self assert: q1 multiplicity = 1.
	self assert: q2 multiplicity = 2! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testMultiplicity2
	"Example from Sage documentation."
	| P X q1 q2 q3 |
	P _ ProjectiveSpace new: 4 over: Ñ.
	X _ P subscheme: (P coordinateRing !! [:x :y :z :w :t| {y^2 - (x*t). w^7 - (t*w*(x^5)) - (z^7)}]).
	q1 _ X !! (0,0,1,1,1).
	self assert: q1 multiplicity = 1.
	q2 _ X !! (1,0,0,0,0).
	self assert: q2 multiplicity = 3.
	q3 _ X !! (0,0,0,0,1).
	self assert: q3 multiplicity = 7! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:31:47'!
testMultiplicity3
	"Example from Sage documentation."
	| P X q |
	P _ ProjectiveSpace new: 3 over: Ü.
	X _ P subscheme: (P coordinateRing !! [:x :y :z :w| z^5*(x^2)*w - (y^8)]).
	q _ X !! (2,0,0,1).
	self assert: q multiplicity = 5! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testMultiplicityAndTangentCone
	"'A Singular Introduction to Commutative Algebra', Example A.8.5."
	| R C T p |
	R _ Ñ polynomialsIn: #(x y).
	C _ (R / (R !! [:x :y| x^3 - y squared])) spec.
	p _ C !! (0,0).
	self assert: p multiplicity = 2.
	T _ (R / (R !! [:x :y| y^2])) spec.
	self assert: p tangentCone = T.
"	L _ (R / (R !! [:x :y| 100 atRandom * x + (100 atRandom * y)])) spec
	...
"! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testNotEmpty
	| A x y X |
	A _ Ñ polynomialsIn: #(x y).
	x _ A x.
	y _ A y.
	X _ A  / {x*x - 1. y*y - (x*x) - 3} :: spec.
	self assert: X isEmpty not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testNotFinite
	| A x y X |
	A _ Ñ polynomialsIn: #(x y).
	x _ A x.
	y _ A y.
	X _ A / {y*y - (x*x) - 3} :: spec.
	self assert: X isFinite not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testReduced
	| A R x y z X |
	A _ AffineSpace new: 3 over: Ñ.
	R _ A coordinateRing.
	x _ R x.
	y _ R y.
	z _ R z.
	X _ A subscheme: {x*(y^3). x^2*z}.
	self assert: X isReduced not.
	self assert: X reduced = (A subscheme: {x*y. x*z})! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testRingHomomorphismSpec
	| A B x y phi |
	A _ Ñ polynomialsIn: #(x y z).
	B _ Ñ polynomialsIn: #(x y).
	x _ B x.
	y _ B y.
	phi _ A to: B evaluating: [:f| f value: {x-y. x. y}].
	self assert: phi spec domain = B spec.
	self assert: phi spec codomain = A spec.
	self assert: phi spec asRingMap = phi! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testSingularCubics
	| R x y C |
	R _ Ñ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	C _ R / (x^3 - (y^2) homogenizedIn: 3) :: proj.
	self assert: (C !! (1,1,1)) isSmooth.
	self assert: (C !! (0,0,1)) isSmooth not.
	self assert: C isSingular.
	C _ R / (x^3 + (x^2) - (y^2) homogenizedIn: 3) :: proj.
	self assert: C isSingular! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:01'!
testSmoothness
	| R C |
	R _ Ñ polynomialsIn: #(x y).
	"cuspidal curve:"
	C _ (R / (R !! [:x :y| y^2 - (x^3)])) spec.
	self assert: C isSmooth not.
	self assert: C isSingular.
	self assert: (C !! (1,1)) isSmooth.
	self assert: (C !! (0,0)) isSmooth not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:32:13'!
testSmoothnessOverCC
	| P R C |
	P _ ProjectiveSpace new: 2 over: Ü.
	R _ P coordinateRing.
	C _ P subscheme: R !! [:x :y :z| x].
	self assert: C isSingular not.
	C _ P subscheme: R !! [:x :y :z| y^2*z - (x^3)].
	self assert: C isSingular.
	C _ P subscheme: R !! [:x :y :z| y^2*z - (x^3) + (z^3)].
	self assert: C isSingular not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 08:57:20'!
testSmoothnessOverFiniteField
	| P R C |
	P _ ProjectiveSpace new: 2 over: É/19.
	R _ P coordinateRing.
	C _ P subscheme: R !! [:x :y :z| x^3 + (y^3) + (z^3)].
	self assert: C isSingular not.
	C _ P subscheme: R !! [:x :y :z| x^4 - (x*(z^3))].
	self assert: C isSingular.
	C _ P subscheme: R !! [:x :y :z| x^5 + (y^5*19) + (z^5)].
	self assert: C isSingular.
	C _ P subscheme: R !! [:x :y :z| x^5 + (y^5*9) + (z^5)].
	self assert: C isSingular not! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:17'!
testSmoothnessOverQQ
	| P C |
	P _ ProjectiveSpace new: 2 over: Ñ.
	C _ P subscheme: P coordinateRing !! [:x :y :z| x^3 - (y^2*z)].
	self assert: C isSingular! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 10/26/2020 14:31:04'!
testSpecZ
	self assert: É spec dimension = 1.
	self assert: É spec isNoetherian.
	self assert: (É spec includes: É*5).
	self deny: (É spec includes: É*6)! !

!AlgebraicGeometryTest methodsFor: 'general' stamp: 'len 5/23/2020 08:57:54'!
testSpecZSubschemes
	self assert: É spec >= (É / 2) spec.
	self assert: (É / 10) spec >= (É / 5) spec.
	self deny: (É / 10) spec >= (É / 3) spec.
	self assert: (É / 10) spec /\ (É / 15) spec = (É / 5) spec.
	self assert: (É / 10) spec \/ (É / 15) spec = (É / 30) spec! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 08:56:56'!
testProjectivePlaneCurveArithmeticGenus
	| R f C |
	R _  (É/5) polynomialsIn: 3.
	f _ R !! [:x :y :z| y^2*(z^7) - (x^9) - (x*(z^8))].
	C _ ProjectivePlaneCurve polynomial: f.
	self assert: C arithmeticGenus = 28.
	"self assert: C genus = 4"! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 08:56:59'!
testProjectivePlaneOverFiniteField
	| P |
	P _ ProjectiveSpace new: 2 over: É/5.
	self assert: P points size = 31.
	self assert: (Iterator on: P points performing: #do:) size = 31! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 08:57:01'!
testProjectivePlaneOverFiniteRing
	| P |
	P _ ProjectiveSpace new: 2 over: É/4.
	self assert: P points size = 28
	"this plane has 28 points and 28 lines (falling into 7 neighbour classes of size 4), each line has 6 points and each point is on 6 lines"! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 4/13/2022 13:34:43'!
testProjectiveSchemeAffinePatches
	| P E p a |
	P _ ProjectivePlane over: Ñ.
	"cremona37a elliptic curve:"
	E _ P subscheme: (P coordinateRing !! [:x :y :z| y^2*z + (z^2*x) + (z^2*y) - (x^3)]).
	p _ E !! ((2,-3) over: Ñ).
	a _ p dehomogenizedIn: 2.
	"the affine point is in the second standard affine patch:"
	self assert: (E patch: 2) == a codomain.
	"and when we homogenize again, the same projective embedding is used and come back to the same projective point on exactly the same projective curve:"
	self assert: a homogenized = p.
	self assert: a homogenized codomain == E! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 09:33:01'!
testProjectiveSchemeDegree
	"Example from Sage documentation."
	| P X |
	P _ ProjectiveSpace new: 5 over: Ñ.
	X _ P subscheme: (P coordinateRing !! [:x :y :z :w :t :u| x^7 + (x*y*z*(t^4)) - (u^7)]).
	self assert: X degree = 7! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 08:57:04'!
testProjectiveSchemeDegree2
	"Example from Sage documentation."
	| P X |
	P _ ProjectiveSpace new: 3 over: É/13.
	X _ P subscheme: (P coordinateRing !! [:x :y :z :w| {y^3 - (w^3). x + (7*z)}]).
	self assert: X degree = 3! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 5/23/2020 09:33:01'!
testProjectiveSchemeDegree3
	"Example from Sage documentation."
	| P X |
	P _ ProjectiveSpace new: 4 over: Ñ.
	X _ P subscheme: (P coordinateRing !! [:x :y :z :w :u| {x^7 - (y*(z^3)*(w^2)*u). w*(z^2) - (y*(u^2)). z^3 + (y^3)}]).
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
	P _ ProjectiveSpace new: 1 over: É/3.
	S _ P points.
	self assert: S size = 4.
	self assert: S asSet size = 4.
	S _ P pointsOver: (É/3 extensionDegree: 2).
	self assert: S size = 10.
	self assert: S asSet size = 10! !

!AlgebraicGeometryTest methodsFor: 'projective' stamp: 'len 8/5/2021 08:37:07'!
testProjectiveSpaceOverModularIntegers
	| N n P numberOfPoints |
	N _ (2 to: 6) atRandom.
	n _ (1 to: 3) atRandom.
	P _ ProjectiveSpace new: n over: É/N.
	"formula from Cohen's 'A course in computational algebraic number theory' page 486:"
	numberOfPoints _ N^n * (N factorization asSet product: [:p| (0 to: n) sum: [:i| 1/(p^i)]]).
	self assert: P points asSet size = numberOfPoints.
	self assert: P points size = numberOfPoints! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 12/15/2021 13:21:53'!
testRationalMap
	"Example from Magma handbook Scheme_map-creation (H118E32)."
	| A1 A2 x y f X |
	A1 _ AffineSpace new: #(t) over: Ñ.
	A2 _ AffineSpace new: #(x y) over: Ñ.

	f _ RationalMap from: A1 to: A2 evaluatingWithArguments: [:t| {t^3 + t. (t^2 - 3)}].

	self assert: f domain = A1.
	self assert: f codomain = A2.

	x _ A2 coordinateRing x.
	y _ A2 coordinateRing y.
	X _ A2 subscheme: {x^2*-1 + (y^3) + (y^2*11) + (y*40) + 48}.
	self assert: f image = X! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 12/15/2021 13:22:43'!
testRationalMap2
	"Example from Cox's IVA pag. 234. Parametrization of the tangent surface of the twisted cubic."
	| A2 A3 x y z f X |
	A2 _ AffineSpace new: #(t u) over: Ñ.
	A3 _ AffineSpace new: 3 over: Ñ.

	f _ RationalMap from: A2 to: A3 evaluatingWithArguments: [:t :u| {t + u. t^2 + (t*u*2). t^3 + (t^2*u*3)}].

	x _ A3 coordinateRing x.
	y _ A3 coordinateRing y.
	z _ A3 coordinateRing z.
	X _ A3 subscheme: {x^3*z - (x*y^2*(3/4)) - (x*y*z*(3/2)) + (y^3) + (z^2*(1/4))}.
	self assert: f image = X! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 12/15/2021 13:23:05'!
testRationalMapAffineExample
	"From lecture notes MIT 18.782 Introduction to Arithmetic Geometry, Lecture #15, Fall 2013, 10/29/2013, Example 15.19."
	| A2 x y X A1 phi phi2 phiInv |
	A2 _ AffineSpace new: 2 over: Ñ.
	x _ A2 coordinateRing x.
	y _ A2 coordinateRing y.
	X _ A2 subscheme: x^2 + (y^2) - 1.
	A1 _ AffineSpace new: 1 over: Ñ.
	phi _ RationalMap from: X to: A1 evaluatingWithArguments: [:x0 :y0| {y0 / (x0 + 1)}].
	phi2 _ RationalMap from: X to: A1 evaluatingWithArguments: [:x0 :y0| {1 - x0 / y0}].
	self assert: phi = phi2.
	phiInv _ RationalMap from: A1 to: X evaluatingWithArguments: [:t| {1 - (t^2) / (1 + (t^2)). t*2 / (1 + (t^2))}].
	self assert: (phi ∑ phiInv) isIdentity.
	self assert: phi isRegular not.
	self assert: phi isDominant.
"	self assert: phi isSurjective."
	self assert: phiInv isRegular not.
	self assert: phiInv isDominant.
"	self assert: phiInv isSurjective not"! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 12/15/2021 13:23:23'!
testRationalMapImage
	| A X X2 f |
	A _ AffineSpace new: #(x y) over: Ñ.
	"circle centered at (0,0):"
	X _ A subscheme: A coordinateRing !! [:x :y| x^2 + (y^2) - 1].
	"circle centered at: (1,2):"
	X2 _ A subscheme: A coordinateRing !! [:x :y| (x-1)^2 + ((y-2)^2) - 1].
	"translation by (1,2):"
	f _ RationalMap from: X to: A evaluatingWithArguments: [:x :y| {x+1. y+2}].
	self assert: f image = X2.
	"global translation by (1,2):"
	f _ RationalMap from: A to: A evaluatingWithArguments: [:x :y| {x+1. y+2}].
	self assert: (X apply: f) = X2! !

!AlgebraicGeometryTest methodsFor: 'rational maps' stamp: 'len 6/7/2020 12:57:04'!
testRationalMapLinearMapConversion
	| A2 A3 V2 V3 f f2 p f3 |
	A2 _ AffineSpace new: 2 over: Ñ.
	A3 _ AffineSpace new: 3 over: Ñ.
	V2 _ Ñ^2.
	V3 _ Ñ^3.

	f _ V2 => V3 atRandomBits: 100.
	f2 _ f asRationalMap.
	self assert: (A2 => A3 includes: f2).

	p _ V2 atRandomBits: 100.
	self assert: (f value: p) = (f2 value: p).

	f3 _ V2 => V3 !! f2.
	self assert: f = f3! !

!AlgebraicGeometryTest methodsFor: 'rational points' stamp: 'len 5/23/2020 08:57:14'!
testRationalPointsOverFiniteField
	| P C |
	P _ ProjectiveSpace new: 2 over: É/7.
	C _ P subscheme: P coordinateRing !! [:x :y :z| x^3 + (y^3) - (z^3)].
	self assert: C points size = 9.
	self assert: (Iterator on: C points performing: #do:) size = 9! !

!AlgebraicGeometryTest methodsFor: 'rational points' stamp: 'len 1/14/2022 11:32:38'!
testSturmfelsExample1
	"Bernd Sturmfels, 'Ideals, Varieties and Macaulay 2', 1. A Curve in Affine 3-Space."
	| R x y I C C1 C2 |
	R _ Ñ polynomialsIn: #(x y z). "default ordering is trevlex"
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

!AlgebraicGeometryTest methodsFor: 'rational points' stamp: 'len 12/15/2021 12:00:19'!
testSturmfelsExample2
	"Bernd Sturmfels, 'Ideals, Varieties and Macaulay 2', 2. Intersecting Our Curve With a Surface."
	| R x y z C S theirunion ourpoints |
	R _ Ñ polynomialsIn: #(x y z). "default ordering is trevlex"
	x _ R x.
	y _ R y.
	z _ R z.
	C _ (R / {x^4 - (y^5). x^3 - (y^7)}) spec.
	C _ C \ (R / R x) spec. "this is C1 from example1"
	S _ (R / (x^5 + (y^5) + (z^5) - 1)) spec.
	theirunion _ C \/ S.
	"In this case, the union's ideal coincides with the product of the ideals:"
	self assert: theirunion asIdeal = (C asIdeal * S asIdeal).
	"The intersection of the curve and the surface (whose defining ideal is the sum of the respective ideals):"
	ourpoints _ C /\ S.
	"We get a finite set of points:"
	self assert: ourpoints dimension = 0.
	"The number of points is 65:"
	self assert: ourpoints projectiveClosure degree = 65.
	"Each of the points is multiplicity-free:"
	self assert: ourpoints isReduced. "equivalent to the original: ourpoints reduced projectiveClosure degree = 65."! !

!AlgebraicGeometryTest methodsFor: 'rational points' stamp: 'len 3/6/2021 07:07:05'!
testTwistedCubic
	| P R M C |
	P _ ProjectiveSpace new: 3 over: Ñ.
	R _ P coordinateRing.
	"R indeterminateNames: #(u v w t)"
	M _ R^3 => (R^2) !! (#(1 2 3 2 3 4) collect: [:i| R x: i]).
	C _ P subscheme: (M minors: 2).
	self assert: C dimension = 1.
	self assert: C isSmooth! !

!AlgebraicGeometryTest methodsFor: 'rational points' stamp: 'len 5/23/2020 09:33:01'!
testTwistedCubicDegree
	"Twisted cubic in P^3 with homogeneous coordinates x,y,w,z."
	| A X |
	A _ Ñ polynomialsIn: #(x y z w).
	X _ (A / [:x :y :z :w| {x^2 - (y*w). x*z - (y^2)}]) proj.
	"At this point X is still reducible, it is the union of a line x=y=0 and the twisted cubic:"
	self assert: X degree = 4.
	"To cut out X, we impose an additional equation x^3 - (z * (w^2)) = 0:"
	X _ (A / [:x :y :z :w| {x^2 - (y*w). x*z - (y^2). x^3 - (z*(w^2))}]) proj.
	"But we get the twisted cubic (in some sense) 'counted with multiplicity 2', so the degree is 6
	(BUT NOT REALLY, MAGMA GIVES 3 LIKE US, I DONT REMEMBER WHERE I GOT THIS EXAMPLE FROM BUT THE CORRECT ANSWER IS 3)"
	self assert: X degree = 3! !

!AlgebraicGeometryTest methodsFor: 'tangent cone' stamp: 'len 12/15/2021 12:00:25'!
testTangentCone
	"'A Singular Introduction to Commutative Algebra', Example 5.5.13."
	| R I J X |
	R _ Ñ polynomialsIn: #(x y z).
	I _ R * (R !! [:x :y :z| {y*z + (z^2) + (x^3). y^2 + (x*z) + (y^4)}]).
	X _ (R/I) spec.
	J _ R * (R !! [:x :y :z| {y^2 + (x*z). y*z + (z^2). x*(z^2) - (y*(z^2))}]).
	self assert: (X !! (0,0,0)) tangentCone asIdeal = J! !

!AlgebraicGeometryTest methodsFor: 'tangent cone' stamp: 'len 12/15/2021 12:00:32'!
testTangentCone2
	"M. Kreuzer, 'Computational Commutative Algebra', Example 6.5.27."
	| R J T I |
	R _ Ñ polynomialsIn: 4.
	J _ R * (R generate: [:x1 :x2 :x3 :x4| {x1*x2 - (x3^2). x2^2 - (x4^5)}]).
	T _ (R/J) spec tangentConeAt: (0,0,0,0).
	I _ R * (R generate: [:x1 :x2 :x3 :x4| {x1*x2 - (x3^2). x2^2. x2*(x3^2). x3^4}]).
	self assert: T asIdeal = I! !

!AlgebraicGeometryTest methodsFor: 'tangent cone' stamp: 'len 5/23/2020 09:33:01'!
testTangentConeAndTangentSpace
	"Example from https://en.wikipedia.org/wiki/Tangent_cone."
	| A x y S o |
	A _ AffineSpace new: #(x y) over: Ñ.
	x _ A coordinateRing x.
	y _ A coordinateRing y.
	"this curve is singular at (0,0):"
	S _ A subscheme: x^2 + (x^3) - (y^2).
	o _ S !! (0,0).
	"the tangent cone is the union of the two lines x = y and x = -y:"
	self assert: o tangentCone = (A subscheme: (x + y) * (x - y)).
	"the tangent space is the whole plane:"
	self assert: o tangentSpace = A! !

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
	x _ É polynomials x.
	self assert: (Graph K: 3) chromaticPolynomial = (x*(x-1)*(x-2))! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:09:03'!
testChromaticPolynomialCylic
	| x n |
	x _ É polynomials x.
	n _ 4 atRandom.
	self assert: (Graph C: n) chromaticPolynomial = (x-1^n + ((-1)^n * (x-1)))! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:09:12'!
testChromaticPolynomialPath
	| x n |
	x _ É polynomials x.
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
	P _ É polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	T _ x*(x^2 + (x+y)) + (x^2 + (x+y)) + (x*y + (y^2)).
	self assert: G tuttePolynomial = T! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:10:14'!
testTuttePolynomial2
	| G P x y T |
	G _ Graph ordered addEdges: {1->2. 2->3. 3->4. 4->1. 1->3}; yourself.
	P _ É polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	T _ x*(x^2 + (x+y)) + (x^2 + (x+y)) + (x*y + (y^2)).
	self assert: G tuttePolynomial = T! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:10:19'!
testTuttePolynomialK3
	| G P x y T |
	G _ Graph K: 3.
	P _ É polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	T _ x^2 + x + y.
	self assert: G tuttePolynomial = T! !

!AlgebraicGraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 13:10:25'!
testTuttePolynomialK4
	| G P x y T |
	G _ Graph K: 4.
	P _ É polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	T _ x^3 + (x^2*3) + (x*2) + (x*y*4) + (y*2) + (y^2*3) + (y^3).
	self assert: G tuttePolynomial = T! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 05:39:40'!
testArithmetic
	| sqrt2 sqrt3 sqrt5 sqrt6 |
	sqrt2 _ ComplexAlgebraicNumber sqrt2.
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	sqrt6 _ ComplexAlgebraicNumber sqrt: 6.
	self assert: sqrt2 squared = 2.
	self assert: (sqrt2 + sqrt3) * sqrt2 = (2 + sqrt6).
	self assert: (sqrt3 / sqrt5) squared reciprocal = (5/3).
"	self assert: (((5 root: 3) + sqrt2 - sqrt2) ^ 3) asRational = 5"! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 05:39:54'!
testDivision
	| sqrt3 sqrt5 |
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	self assert: sqrt3 / sqrt5 / sqrt3 / sqrt5 = (1/5)! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 23:50:51'!
testMultiplicationByi
	| a |
	a _ ComplexAlgebraicNumber sqrt2.
	self assert: a i i = a negated! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 04:54:12'!
testNorm
	| a |
	a _ ComplexAlgebraicNumber phi.
	self assert: a norm = a conjugates product! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 05:40:10'!
testProduct
	| sqrt3 sqrt5 |
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	self assert: sqrt3 * sqrt5 * sqrt3 * sqrt5 = 15! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 18:49:57'!
testRealArithmetic
	| sqrt2 sqrt3 sqrt5 sqrt6 |
	sqrt2 _ RealAlgebraicNumber sqrt2.
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	sqrt6 _ RealAlgebraicNumber sqrt: 6.
	self assert: sqrt2 squared asRational = 2.
	self assert: (sqrt2 + sqrt3) * sqrt2 = (2 + sqrt6).
	self assert: (sqrt3 / sqrt5) squared reciprocal = (5/3).
"	self assert: (((five root: 3) + sqrt2 - sqrt2) ^ 3) asRational = 5"! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 21:59'!
testRealDivision
	| sqrt3 sqrt5 |
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	self assert: (sqrt3 / sqrt5 / sqrt3 / sqrt5) asRational = (1/5)! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testRealNormalizationBug
	| f interval x |
	x _ Ñ polynomials x.
	f _ x^4 * 1576875 - (x^2 * 877250) + 43923.
	interval _ RealInterval open: (-260139/1345600) closed: (81631/134560).
	RealAlgebraicNumber polynomial: f bounds: interval! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 21:59'!
testRealProduct
	| sqrt3 sqrt5 |
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	self assert: (sqrt3 * sqrt5 * sqrt3 * sqrt5) asRational = 15! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 21:59'!
testRealSquare
	| sqrt2 |
	sqrt2 _ RealAlgebraicNumber sqrt2.
	self assert: sqrt2 squared asRational = 2! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 18:48:17'!
testRealSquareRoot
	| a |
	a _ RealAlgebraicNumber sqrt2.
	self assert: a squareRoot squared = a! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 22:00'!
testRealSum
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 _ RealAlgebraicNumber sqrt: 2.
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	self assert: sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5 = sqrt2! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 05:40:24'!
testSquare
	| sqrt2 |
	sqrt2 _ ComplexAlgebraicNumber sqrt2.
	self assert: sqrt2 squared = 2! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 18:49:16'!
testSquareRoot
	| a |
	a _ ComplexAlgebraicNumber sqrt2.
	self assert: a squareRoot squared = a! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 1/20/2018 19:18:17'!
testSum
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 _ ComplexAlgebraicNumber sqrt: 2.
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	self assert: sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5 = sqrt2! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 04:53:56'!
testTrace
	| a |
	a _ ComplexAlgebraicNumber phi.
	self assert: a trace = a conjugates sum! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/27/2020 19:19:26'!
testBilinearFormDecomposition
	| V A B |
	V _ Ñ^5 atRandom.
	A _ V endomorphisms atRandomBits: 50.
	B _ BilinearForm on: V matrix: A.
	self assert: B symmetricPart isSymmetric.
	self assert: B skewSymmetricPart isSkewSymmetric.
	self assert: B symmetricPart + B skewSymmetricPart = B! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/27/2020 19:24:54'!
testDotCrossAlternating
	"This is an example of an alternating bilinear form."
	| V u B |
	V _ É^3.
	u _ V atRandomBits: 10.
	B _ BilinearForm on: V evaluatingWithArguments: [:x :y| u ∑ (x ◊ y)].
	self assert: B isAlternating! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/27/2020 19:10:08'!
testDotProductIsSymmetric
	| V B |
	V _ É^5 atRandom.
	B _ BilinearForm on: V evaluatingWithArguments: [:x :y| x ∑ y].
	self assert: B isSymmetric! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 4/1/2021 06:46:48'!
testMatrixAndLeft
	"The matrix of the bilinear form matches the matrix of the left part because the convention we follow when defining B(v,w) := v ∑ (A * w)."
	| V A B |
	V _ Ñ^5 atRandom.
	A _ V endomorphisms atRandomBits: 50.
	B _ BilinearForm on: V matrix: A.
	self assert: A = B left asMatrix! !

!BilinearFormTest methodsFor: 'as yet unclassified' stamp: 'len 4/8/2021 12:20:22'!
testScalarSpaceMultiplication
	| V B |
	V _ É asIdeal.
	B _ BilinearForm on: V evaluatingWithArguments: [:x :y| x*y].
	self assert: B isSymmetric.
	self assert: B isAlternating not.
	V _ (É/2) asIdeal.
	B _ BilinearForm on: V evaluatingWithArguments: [:x :y| x*y].
	self assert: B isSkewSymmetric! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 08:59:39'!
newChainComplex
	^ self newChainComplexOver: {É. É/64 atRandom nextPrime. Ñ} atRandom! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/23/2020 00:35:38'!
newChainComplexOver: aRing
	| X |
	X _ SimplicialComplex perform: #(kleinBottle realProjectivePlane torus) atRandom.
	^ X chainComplexOver: aRing! !

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2020 12:43:53'!
testChainComplexExample
	self assert: self newChainComplex verify! !

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

!ChainComplexTest methodsFor: 'as yet unclassified' stamp: 'len 11/4/2020 12:29:06'!
testShift
	| C k i |
	C _ self newChainComplex.
	self assert: C >> 0 = C.
	k _ (-5 to: 5) atRandom.
	self assert: (C >> k) verify.
	self assert: C >> k << k = C.
	self assert: C >> k = (C << k negated).
	self assert: (C >> k) minDegree = (C minDegree + k).
	self assert: (C >> k) maxDegree = (C maxDegree + k).
	i _ (-5 to: 5) atRandom.
	self assert: (C >> k at: i+k) = ((C at: i) * ((-1)^k))! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 5/23/2020 09:33:01'!
testAffineRingElementOperations
	"Magma handbook example H105E1."
	| R A x y |
	R _ Ñ polynomialsIn: #(x y).
	A _ R / (R generate: [:x1 :y1| {x1^2 - (y1^2) + 2. y1^3 - 5}]).
	x _ A x. y _ A y.
	self assert: x^2 = (y^2 - 2).
	"self assert: x reciprocal = ..."! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 8/1/2021 12:00:06'!
testAffineRingIdealIntersectionBug
	| P x y A I J |
	P _ Ñ polynomialsIn: #(x y).
	x _ P x. y _ P y.
	A _ P / {y^2 - x + 1. y - 1}.
	I _ A * {1. 2}.
	J _ A * {-1. 2}.
	self assert: I /\ J = A asIdeal! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 5/23/2020 09:33:01'!
testAffineRingIdealIntersectionRandom
	| R f1 f2 J A |
	R _ Ñ polynomialsIn: #(x y).
	[(f1 _ R atRandomBits: 10) isZero] whileTrue.
	[(f2 _ R atRandomBits: 10) isZero] whileTrue.
	J _ R * {f1. f2}.
	A _ R/J.
	self testIdealIntersectionRandom: A! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 5/23/2020 09:33:01'!
testAffineRingIdealMembershipRandom
	| R f1 f2 J A g I h |
	R _ Ñ polynomialsIn: #(x y z).
	[(f1 _ R atRandomBits: 10) isZero] whileTrue.
	[(f2 _ R atRandomBits: 10) isZero] whileTrue.
	J _ R * {f1. f2}.
	A _ R/J.
	g _ A atRandomBits: 10.
	I _ A * g.
	h _ I atRandomBits: 10.
	self assert: (I includes: h)! !

!CommutativeAlgebraTest methodsFor: 'affine ring' stamp: 'len 5/23/2020 09:33:01'!
testAffineRingIdealOperations
	"Magma handbook, example H105E2."
	| R A I J |
	R _ Ñ polynomialsIn: #(x y z) "ordering: #lex".
	A _ R / (R generate: [:x :y :z| {x^2 - y + 1. y^3 + z - 1}]).
	I _ A * (A generate: [:x :y :z| x^3*y*(z^2)]).
	self deny: I isRadical.
	J _ A * (A generate: [:x :y :z| {x*(y^2) + (x*y) - (x*z) + x. y*z. z^2 - z}]).
	self assert: I radical = J. "this fails probably because PolynomialIdeal>>radical is broken"
	"... and then some tests of primary decomposition"! !

!CommutativeAlgebraTest methodsFor: 'algorithms' stamp: 'len 1/13/2022 12:02:33'!
testExtendedBuchberger
	| R I A basisAndTransformation G T |
	R _ Ñ polynomialsIn: #(a b c d).
	I _ R cyclicIdeal.
	I _ I orderedBy: #grevlex.
	A _ I generatorMatrix.
	basisAndTransformation _ A basisAndTransformation.
	G _ basisAndTransformation first.
	T _ basisAndTransformation second.
	self assert: (A * T) = G! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 1/13/2022 10:45:56'!
testBuchbergerNormalForm
	"A Singular Introduction to Commutative Algebra, Example 1.6.12."
	| R x y z f G h |
	R _ Ñ polynomialsIn: #(x y z) ordering: #glex.
	x _ R x. y _ R y. z _ R z.
	f _ x^3 + (y^2) + (z^2*2) + x + y + 1.
	G _ {x. y}.
	h _ f reductionBy: G.
	self assert: h = (z^2*2 + x + y + 1).
	h _ f reductionFullBy: G.
	self assert: h = (z^2 + (1/2))! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 1/13/2022 10:41:54'!
testMoraNormalForm
	"A Singular Introduction to Commutative Algebra, Example 1.7.7."
	| R f G h x y z |
	R _ Ñ polynomialsIn: #(x y z) ordering: #lgrevlex.
	x _ R x. y _ R y. z _ R z.
	f _ x^2 + (y^2) + (z^3) + (x^4) + (y^5).
	G _ {x. y}.
	h _ f reductionBy: G.
	self assert: h = (z^3 + (x^4) + (y^5))! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateDivisionExample
	| P x y z F f |
	P _ Ñ polynomialsIn: #(x y z) ordering: #glex.
	x _ P x.
	y _ P y.
	z _ P z.
	F _ {x^2 - (y*z). y^2 - (z*x). z^2 - (x*y)}.
	f _ x^3 + (y^3 * 3).
	self assert: (f divisionBy: F) remainder = (y^3 * 4)! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateDivisionExample2
	| R f f1 f2 |
	R _ Ñ polynomialsIn: #(x y z) ordering: #lex.
	f _ R !! [:x :y :z| x^5*(y^3)].
	f1 _ R !! [:x :y :z| x^3*(y^2) - ((y^2)*z)].
	f2 _ R !! [:x :y :z| x*(y^2) - (y*z)].
	self assert: (f divisionBy: {f1. f2}) printString = 'x^5y^3 = (x^2y)(x^3y^2 - y^2z) + (xyz + z^2)(xy^2 - yz) + yz^3'! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateDivisionExample3
	| R f f1 f2 |
	R _ Ñ polynomialsIn: #(x y z) ordering: #lex.
	f _ R !! [:x :y :z| x^3*(y^2) + (x*y) + x + 1].
	f1 _ R !! [:x :y :z| x^3 + 1].
	f2 _ R !! [:x :y :z| y^2 + 1].
	self assert: (f divisionBy: {f1. f2}) printString = 'x^3y^2 + xy + x + 1 = (y^2)(x^3 + 1) + (-1)(y^2 + 1) + xy + x + 2'! !

!CommutativeAlgebraTest methodsFor: 'division and normal forms' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateDivisionExample4
	| R f f1 f2 |
	R _ Ñ polynomialsIn: #(x y) ordering: #lex.
	f _ R !! [:x :y| x^2*(y^3) - (x*(y^2)*2)].
	f1 _ R !! [:x :y| x^2*y - (x*2)].
	f2 _ R !! [:x :y| y^3 + 4].
	self assert: (f divisionBy: {f1. f2}) printString = 'x^2y^3 - 2xy^2 = (y^2)(x^2y - 2x) + (0)(y^3 + 4) + 0'.
	self assert: (f divisionBy: {f2. f1}) printString = 'x^2y^3 - 2xy^2 = (x^2)(y^3 + 4) + (0)(x^2y - 2x) + -4x^2 - 2xy^2'! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:52:22'!
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
	self assert: division verify.
"	self assert: G reduced isReduced.
	self assert: G minimal isMinimal"! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:38'!
testGroebnerCyclic3
	| I G |
	I _ (Ñ polynomialsIn: 3) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:43'!
testGroebnerCyclic3Lexicographic
	| I G |
	I _ (Ñ polynomialsIn: 3 ordering: #lex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:53'!
testGroebnerCyclic3OverZ2
	| I G |
	I _ (É/2 polynomialsIn: 3) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 12:03:25'!
testGroebnerCyclic3Reduced
	| I G |
	I _ (Ñ polynomialsIn: #(x y z)) cyclicIdeal.
	I _ I orderedBy: #lex.
	G _ I basis "reduced".
	self assert: G printString = '(x + y + z, y^2 + yz + z^2, z^3 - 1)'! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:56'!
testGroebnerCyclic3TotalLexicographic
	| I G |
	I _ (Ñ polynomialsIn: 3 ordering: #glex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:33:59'!
testGroebnerCyclic3TotalReverseLexicographic
	| I G |
	I _ (Ñ polynomialsIn: 3 ordering: #grevlex) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:02'!
testGroebnerCyclic4
	| I G |
	I _ (Ñ polynomialsIn: 4) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:04'!
testGroebnerCyclic4OverZ127
	| I G |
	I _ (É/127 polynomialsIn: 4) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:06'!
testGroebnerCyclic4OverZ2
	| I G |
	I _ (É/2 polynomialsIn: 4) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:09'!
testGroebnerCyclic5
	| I G |
	I _ (Ñ polynomialsIn: 5) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:11'!
testGroebnerCyclic5OverZ127
	| I G |
	I _ (É/127 polynomialsIn: 5) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:14'!
testGroebnerCyclic5OverZ2
	| I G |
	I _ (É/2 polynomialsIn: 5) cyclicIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:56:59'!
testGroebnerCyclic5Reduced
	| I G |
	I _ (Ñ polynomialsIn: 5) cyclicIdeal.
	G _ I basis "reduced".
	self assert: I basisMatrix isReducedStandardBasis.
	self testGroebnerBasis: G ideal: I.
	self assert: (I scalars * G) basis "reduced" = G! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:34:43'!
testGroebnerExample
	| P x y z I G |
	P _ Ñ polynomialsIn: #(x y z) ordering: #lex.
	x _ P x.
	y _ P y.
	z _ P z.
	I _ P * {x^2 - (y*z). y^2 - (z*x). z^2 - (x*y)}.
	G _ I basis "minimal".
	self assert: G asSet = {x^2 - (y*z). x*z - (y^2). x*y - (z^2). y^3 - (z^3)} asSet! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 12:00:51'!
testGroebnerExample2
	| R x y f1 f2 I G |
	R _ Ñ polynomialsIn: #(x y) ordering: #lex.
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
	R _ Ñ polynomialsIn: #(x y) ordering: #glex.
	I _ R * (R !! [:x :y| {x^2*y + x + 1. x*(y^2) + y + 1. x - y}]).
	G _ I basis "reduced".
	self assert: G printString = '(y^3 + y + 1, x - y)' "this is as computed by Singular"! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 12:01:43'!
testGroebnerExample4Minimal
	"Example from Cox, Little, O'Shea, 'Ideals, Varieties and Algorithms', p. 90, continued in p. 93."
	| R I G |
	R _ Ñ polynomialsIn: #(x y) ordering: #grevlex.
	I _ R * (R !! [:x :y| {x^3 - (x*y*2). x^2*y - (y^2*2) + x}]).
	G _ I basis "minimal".
	self assert: G printString = '(x^2, xy, y^2 - (1/2)x)'! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:35:21'!
testGroebnerKatsura2
	| I G |
	I _ (Ñ polynomialsIn: 2) katsuraIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:35:25'!
testGroebnerKatsura3
	| I G |
	I _ (Ñ polynomialsIn: 3) katsuraIdeal.
	G _ I basis.
	self testGroebnerBasis: G ideal: I! !

!CommutativeAlgebraTest methodsFor: 'groebner' stamp: 'len 1/13/2022 11:35:53'!
testGroebnerTrivial
	"The 3 polynomials have no common zeroes, the Groebner basis should be {1}."
	| Qx x y I |
	Qx _ Ñ polynomialsIn: #(x y) ordering: #lex.
	x _ Qx x.
	y _ Qx y.
	I _ Qx * {x + y. x^2 - 1. y^2 - (x*2)}.
	self assert: I isOne! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 12/30/2021 17:28:47'!
testAffineRingDimensionAsVectorSpace
	"Example from answer in math.stackexchange 'What does Hilbert series of monomial ideals describe?'."
	| S a b I |
	S _ É/17 polynomialsIn: #(a b).
	a _ S x: 1.
	b _ S x: 2.
	I _ S * {a^6. a^3*b. a^2*(b^4). b^5}.
	self assert: (S/I) vectorSpaceDimension = 17! !

!CommutativeAlgebraTest methodsFor: 'hilbert series' stamp: 'len 5/23/2020 09:33:01'!
testHilbertSeriesOfPolynomialIdeal
	"Derksen, Kemper 'Computational Invariant Theory', Example 1.4.9."
	| t R x y z w I H |
	t _ Ñ polynomials fractions x.
	R _ Ñ polynomialsIn: #(x y z w) "ordering: #lex".
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
	t _ Ñ polynomials fractions x.
	R _ Ñ polynomialsIn: #(x y z w) "ordering: #lex".
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
	t _ Ñ polynomials fractions x.
	R _ Ñ polynomialsIn: #(x y z w).
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
	t _ Ñ polynomials fractions x.
	R _ Ñ polynomialsIn: 4.
	H _ R hilbertSeriesAt: t.
	self assert: H = ((1 - t) ^ -4)! !

!CommutativeAlgebraTest methodsFor: 'ideal basics' stamp: 'len 5/23/2020 09:33:01'!
testIdealElementOperations
	"Magma handbook example H103E2."
	| R x y z I |
	R _ Ñ polynomialsIn: #(x y z) "ordering: #lex".
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

!CommutativeAlgebraTest methodsFor: 'ideal basics' stamp: 'len 4/6/2021 13:49:14'!
testMonomialIdeal
	"Test ideals generated by monomials are automatically created as MonomialIdeal."
	| R f g h |
	R _ Ñ polynomialsIn: 3.
	f _ R !! [:x :y :z| x^2 + y].
	g _ R !! [:x :y :z| z^3].
	h _ R !! [:x :y :z| x^2*y].
	self assert: (R * {f. g. h}) isMonomial not.
	self assert: (R * {g. h}) isMonomial! !

!CommutativeAlgebraTest methodsFor: 'ideal basics' stamp: 'len 5/23/2020 09:34:55'!
testZeroIdeal
	| zero |
	zero _ É * #(). "the zero ideal {0}"
	self assert: zero isZero.
	self assert: (zero includes: 0).
	self deny: (zero includes: 42)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealColon
	| R x y z I J |
	R _ Ñ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	z _ R z.
	I _ R * {x*y - 1. x^3*(z^2) - (y^2). x*(z^3) - x - 1}.
"	J _ R * {x*y - 1. x^2*z - y. x*(z^3) - x - 1}."
	self assert: I : R asIdeal = I.
	self assert: R asIdeal : I = R asIdeal.
	J _ R * {I atRandomBits: 10. I atRandomBits: 10}. "J <= I"
	self assert: I : J = R asIdeal! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealColon2
	"Example from Cox IVA 205p."
	| R x y z I J Q |
	R _ Ñ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	z _ R z.
	I _ R * {x*z - (y^2). x^3 - (y*z)}.
	J _ R * {x. y}.
	Q _ I : J.
	self assert: Q = (I : (R * x) /\ (I : (R * y))).
	self assert: Q = (R * {x*z - (y^2). x^3 - (y*z). x^2*y - (z^2)})! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealElimination
	"Example from Cox IVA 123 p. NOTE: this is a little slow."
	| Qx x y z I |
	Qx _ Ñ polynomialsIn: #(x y z).
	x _ Qx x. y _ Qx y. z _ Qx z.
	I _ Qx * {x^2 + y + z - 1. x + (y^2) + z - 1. x + y + (z^2) - 1}.
	self assert: (I eliminateAll: #(1 2)) generator = (z^6 - (z^4*4) + (z^3*4) - (z^2))! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 4/8/2021 12:35:55'!
testIdealEqualsUnitIdeal
	| R x y I |
	R _ É/7 polynomialsIn: #(x y) ordering: #glex.
	x _ R x.
	y _ R y.
	I _ R * {x + 1. x - 1. y - 2}.
	self assert: I = R asIdeal! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 3/4/2021 18:21:29'!
testIdealHomogenization
	"The homogenization of an ideal can be bigger than the ideal generated by the homogenized generators."
	| P x y z f1 f2 I f3 J |
	P _ Ñ polynomialsIn: #(x y z).
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
	J _ (Ñ polynomialsIn: 4) * {f1 homogenized. f2 homogenized}.
	self deny: (J includes: f3 homogenized)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealIntersection
	"Example from Cox IVA 194 p."
	| Qxy x y I J K |
	Qxy _ Ñ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	I _ Qxy * {x^2 * y}.
	J _ Qxy * {x * (y^2)}.
	K _ Qxy * {x^2*(y^2)}.
	self assert: I /\ J = K! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealIntersectionRandom
	self testIdealIntersectionRandom: (Ñ polynomialsIn: #(x y))! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/10/2019 04:31:29'!
testIdealIntersectionRandom: aRing
	| R f1 f2 g I J K |
	R _ aRing.
	f1 _ R atRandomBits: 10.
	f2 _ R atRandomBits: 10.
	g _ R atRandomBits: 10.
	I _ R * {f1. g}.
	J _ R * {f2. g}.
	K _ R * {g}.
	self assert: I /\ J >= K! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealIsPrime
	| R x y I |
	R _ Ñ polynomialsIn: #(x y).
	x _ R x: #x.
	y _ R x: #y.
	I _ R * {x^2 - (y^2) - 1}.
	self assert: I isPrime! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealIsRadical
	"Becker, Weispfenning, 'Groebner Bases', Exercise 8.21."
	| R x y I |
	R _ Ñ polynomialsIn: #(x y).
	x _ R x: #x.
	y _ R x: #y.
	I _ R * {x^2 + y. y^2 + x}.
	self assert: (R / I) dimension = 0.
	self assert: I isRadical! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealMembership
	| Qxy x y f g I J h k |
	Qxy _ Ñ polynomialsIn: #(x y).
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
	Qxy _ Ñ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	I _ Qxy * {x*x. y*2 + x}.
	self assert: (I includes: (x + y) squared)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealMembership3
	| Qx x y z w f g I |
	Qx _ Ñ polynomialsIn: #(x y z w).
	x _ Qx x.
	y _ Qx y.
	z _ Qx z.
	w _ Qx x: #w.
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
	R _ É/127 polynomialsIn: #(x y z) ordering: #glex.
	x _ R x.
	y _ R y.
	z _ R z.
	I _ R * {x*y + z. y^3 + 1. z^2 - (x*5) - 1}.
	self assert: (I includes: (x*y + z) + (y^3 + 1)).
	self assert: (I includes: x*z*(z^2 - (x*5) - 1))! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:34:55'!
testIdealMembership5
	| R x y I |
	R _ É/7 polynomialsIn: #(x y) ordering: #glex.
	x _ R x.
	y _ R y.
	I _ R * {x*y + 1. y^2 - 1}.
	self assert: (I includes: x+y)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealMembershipRandom
	self testIdealMembershipRandom: (Ñ polynomialsIn: #(x y z))! !

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

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealRadical
	| R x y I J |
	R _ Ñ polynomialsIn: #(x y).
	x _ R x: #x.
	y _ R x: #y.
	I _ R * {x^3. y^2}.
	J _ R * {x. y}.
	self assert: I radical = J! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:34:55'!
testIdealRadicalZeroDimNotPerfectField
	"Counterexample that shows how Seidenberg algorithm fails over a non-perfect field.
	From Becker and Weispfenning 'Groebner Bases', Springer-Verlag, 1993. Example 8.16."
	| p F R t x y I |
	p _ #(3 5 7 11) atRandom.
	F _ (É / p) polynomialsIn: #(t) :: fractions.
	R _ F polynomialsIn: #(x y).
	t _ F x: #t.
	x _ R x.
	y _ R y.
	I _ R * {x^p - t. y^p - t}.
	self assert: (I includes: x^p - (y^p)).
	self deny: (I includes: x - y).
	self assert: (I radical includes: x - y)! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealRadicalZeroDimPerfectField
	"Seidenberg algorithm works for 0-dimensional ideals over a perfect field." 
	| R y z I J |
	R _ Ñ polynomialsIn: #(y z).
	y _ R x: #y.
	z _ R x: #z.
	I _ R * {y + z. z^2}.
	J _ R * {y. z}.
	self assert: I radical = J! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testIdealSaturation
	"Example from SageMath reference."
	| R x y z I J S |
	R _ Ñ polynomialsIn: #(x y z).
	x _ R x.
	y _ R y.
	z _ R z.
	I _ R * {x^5*(z^3). x*y*z. y*(z^4)}.
	J _ R * z.
	S _ R * {y. x^5}.
	self assert: (I saturation: J) = S! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 7/19/2021 11:20:33'!
testLocalIdeals
	"Magma handbook, example H104E4."
	| R I J A M |
	R _ Ñ polynomialsIn: #(x y z) ordering: #llex.
	I _ R * (R generate: [:x :y :z| {x*y - z. x^3*(z^2) - (y^2). x*(z^3) - x - y}]).
	J _ R * (R generate: [:x :y :z| {x*y - z. x^2*z - y. x*(z^3) - x - y}]).
	A _ R * (R generate: [:x :y :z|
		{x^2 - (y^2) + (2*(x^3)*z).
		x*y + (y^2) - (x^3*z).
		y^3.
		x*z + (y*z).
		y*z.
		z^2}]).
	self assert: I * J = A.
	M _ R * (R generate: [:x :y :z| {x + y. y^2. z}]).
	self assert: I /\ J = M.
	self deny: A = M.
	self assert: A <= M! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/2/2021 12:34:57'!
testPolynomialRingHomomorphismKernel
	| A B x y phi |
	A _ Ñ polynomialsIn: #(x y z).
	B _ Ñ polynomialsIn: #(x y).
	x _ B x.
	y _ B y.
	phi _ A to: B mapping: [:i| {x-y. x. y} at: i].
	self assert: phi kernel = (A * (A x - A y + A z)).
	self assert: (phi value: (phi kernel atRandomBits: 15)) isZero! !

!CommutativeAlgebraTest methodsFor: 'ideal operations' stamp: 'len 5/23/2020 09:33:01'!
testRadicalMembership
	| R x y z g1 g2 f I |
	R _ Ñ polynomialsIn: #(x y z).
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
	R _ Ñ polynomialsIn: #(a b c d e).
	a _ R x: 1. b _ R x: 2. c _ R x: 3. d _ R x: 4. e _ R x: 5.
	I _ R * {a*d. a*e. b*c*d. b*e. c*e. d*e}.
	self assert: I primaryDecomposition = {R * {d. e}. R * {a. b. e}. R * {a. c. e}. R * {a. b. c. d}} asSet! !

!CommutativeAlgebraTest methodsFor: 'krull dimension' stamp: 'len 5/23/2020 09:33:01'!
testKrullDimension
	| R I Q |
	R _ Ñ polynomialsIn: #(x y).
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
	R _ Ñ polynomialsIn: #(x y).
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
	R _ É/32003 polynomialsIn: #(x y z).
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
	R _ É/2147483659 polynomialsIn: #(x y).
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
	R _ É polynomialsIn: #(x y).
	self assert: R dimension = 3.
"	I _ R * {R x. R y}.
	Q _ R / I.
	self assert: Q dimension = 0.
"	I _ R * R zero. "zero ideal"
	Q _ R / I.
	self assert: Q dimension = 3! !

!CommutativeAlgebraTest methodsFor: 'testing examples' stamp: 'len 5/23/2020 09:33:01'!
testCyclic7Correctness
	"Test correctness of the ideal of cyclic 7-roots checking against one generated by Singular."
	| txt |
	txt _ ((Ñ polynomialsIn: 7) cyclicIdeal generators sorted: [:a :b| a leadingMonomial >= b leadingMonomial]) printString.
	#('(' ')' '{' '}' ' ') do: [:each| txt _ txt copyReplaceAll: each with: ''].
	self assert: txt = 'x1x2x3x4x5x6x7-1,x1x2x3x4x5x6+x1x2x3x4x5x7+x1x2x3x4x6x7+x1x2x3x5x6x7+x1x2x4x5x6x7+x1x3x4x5x6x7+x2x3x4x5x6x7,x1x2x3x4x5+x2x3x4x5x6+x1x2x3x4x7+x1x2x3x6x7+x1x2x5x6x7+x1x4x5x6x7+x3x4x5x6x7,x1x2x3x4+x2x3x4x5+x3x4x5x6+x1x2x3x7+x1x2x6x7+x1x5x6x7+x4x5x6x7,x1x2x3+x2x3x4+x3x4x5+x4x5x6+x1x2x7+x1x6x7+x5x6x7,x1x2+x2x3+x3x4+x4x5+x5x6+x1x7+x6x7,x1+x2+x3+x4+x5+x6+x7'! !

!CommutativeAlgebraTest methodsFor: 'testing examples' stamp: 'len 7/19/2021 11:13:35'!
testKatsura7Correctness
	"Test correctness of Katsura-7 ideal checking against one generated by Singular."
	| txt |
	txt _ ((Ñ polynomialsIn: 7) katsuraIdeal generators sorted: [:a :b| a leadingMonomial >= b leadingMonomial]) printString.
	#('(' ')' '{' '}' ' ') do: [:each| txt _ txt copyReplaceAll: each with: ''].
	self assert: txt = 'x1^2+2x2^2+2x3^2+2x4^2+2x5^2+2x6^2+2x7^2-x1,2x1x2+2x2x3+2x3x4+2x4x5+2x5x6+2x6x7-x2,x2^2+2x1x3+2x2x4+2x3x5+2x4x6+2x5x7-x3,2x2x3+2x1x4+2x2x5+2x3x6+2x4x7-x4,x3^2+2x2x4+2x1x5+2x2x6+2x3x7-x5,2x3x4+2x2x5+2x1x6+2x2x7-x6,x1+2x2+2x3+2x4+2x5+2x6+2x7-1'! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 4/1/2021 12:19:16'!
smallGroupAtRandom
	^ {[SymmetricGroup new: 3].
	[AlternatingGroup new: 3].
	[PermutationGroup klein].
	[LinearGroup klein]} atRandom value! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 2/1/2018 23:02:45'!
testActionOrbitOf
	| G |
	G _ PermutationGroup on: (1 to: 8) generators: {#((1 3 2) (4 6 5)). #((1 2 3) (4 5 6)). #((7 8))}.
	self assert: (G action orbitOf: 1) = #(1 2 3) asSet.
	self assert: (G action orbitOf: 4) = #(4 6 5) asSet.
	self assert: (G action orbitOf: 7) = #(7 8) asSet! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 2/1/2018 23:04:44'!
testActionStabilizerOf
	| G g1 g2 g3 id H |
	G _ SymmetricGroup new: 8.
	g1 _ G !! #((1 3 2) (4 6 5)).
	g2 _ G !! #((1 2 3) (4 5 6)).
	g3 _ G !! #((7 8)).
	id _ G identity.
	H _ G span: {g1. g2. g3}.
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

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 15:41:00'!
testCauchyFrobeniusLemma
	"Also called Brunside's lemma, Burnside counting theorem, or orbit-counting theorem."
	| X G phi sum |
	X _ 1 to: 3.
	G _ AlternatingGroup on: X.
	phi _ GroupAction from: (G, X) to: X evaluatingWithArguments: [:f :x| f value: x].
	sum _ G elements sum: [:g| (phi fixedPointsOf: g) size].
	self assert: phi orbits size * G size = sum! !

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

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2020 11:58:41'!
testDirectProduct
	| S A G g |
	S _ self smallGroupAtRandom.
	A _ self smallGroupAtRandom.
	G _ S ◊ A.
	g _ G atRandom.
	self assert: g ∑ g inverse = G identity.
	self testGroup: G.
	(S, A) >- G do: [:each| self testGroupHomomorphism: each].
	G -< (S, A) do: [:each| self testGroupHomomorphism: each]! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 5/5/2019 18:14:45'!
testGroup: G
	| e a b c |
	e _ G identity.
	a _ G atRandomBits: 100.
	b _ G atRandomBits: 100.
	c _ G atRandomBits: 100.
	self assert: e ∑ a = a.
	self assert: a ∑ e = a.
	self assert: a ∑ a inverse = e.
	self assert: a inverse ∑ a = e.
	self assert: (a ∑ b) ∑ c = (a ∑ (b ∑ c))! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 5/5/2019 18:43:04'!
testGroupAction: phi
	| G X e g h x |
	G _ phi group.
	X _ phi space.
	e _ G identity.
	g _ G atRandomBits: 100.
	h _ G atRandomBits: 100.
	x _ X atRandomBits: 100.
	self assert: (phi value: {e. x}) = x.
	self assert: (phi value: {g. phi value: {h. x}}) = (phi value: {g ∑ h. x})! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 7/14/2020 18:17:25'!
testGroupAlgebra
	| G R a b c xa xb xc |
	G _ self smallGroupAtRandom.
	R _ G groupAlgebra.
	a _ G atRandom.
	b _ G atRandom.
	c _ G atRandom.
	xa _ R !! a.
	xb _ R !! b.
	xc _ R !! c.
	self assert: xa*xb = (R !! (a∑b)).
	self assert: xa*2 + xb*xc - (xa*xc) - (xb*xc) = (R !! (a∑c)).
	self assert: (xa + xb) * (xa - xb) = (xa^2 - (xb^2) + (xb*xa) - (xa*xb))! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 7/14/2020 18:17:49'!
testGroupAlgebraIsARing
	| G R a a2 b b2 c c2 e e2 |
	G _ self smallGroupAtRandom.
	R _ G groupAlgebra.
	e _ G identity.
	a _ G atRandom.
	b _ G atRandom.
	c _ G atRandom.
	e2 _ R !! e.
	a2 _ R !! a.
	b2 _ R !! b.
	c2 _ R !! c.
	self assert: e2 = R one.
	self assert: e2 * a2 = a2.
	self assert: a2 * e2 = a2.
	self assert: (a2 * b2 * c2) = (a2 * (b2 * c2)).
	G isCommutative ifTrue: [self assert: a2 * b2 = (b2 * a2)].! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 5/5/2019 18:43:49'!
testGroupHomomorphism: phi
	| G H a b |
	G _ phi domain.
	H _ phi codomain.
	a _ G atRandomBits: 100.
	b _ G atRandomBits: 100.
	self assert: (phi value: G identity) = H identity.
	self assert: (phi value: a ∑ b) = ((phi value: a) ∑ (phi value: b))! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 11/6/2016 10:03'!
testHessian
	| H |
	H _ PermutationGroup hessian.
	self assert: H elements size = 216.
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

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 15:41:07'!
testLagrangeTheorem
	"G finite group, H subgroup of G, then |H| divides |G|."
	| G H |
	G _ self smallGroupAtRandom.
	H _ G span: ((1 to: 3 atRandom) collect: [:i| G atRandom]) asSet.
	self assert: H size | G size! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersUnits
	| m R U |
	m _ (2 to: 100) atRandom.
	R _ É / m.
	U _ R units.
	self assert: U size = (Sequence phi value: m).
	self testGroup: U! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 10/20/2020 15:41:16'!
testOrbitStabilizerTheorem
	| X G phi p |
	X _ 1 to: 4.
	G _ AlternatingGroup on: X.
	phi _ GroupAction from: (G, X) to: X evaluatingWithArguments: [:f :x| f value: x].
	p _ X atRandom.
	self assert: (phi stabilizerOf: p) size * (phi orbitOf: p) size = G size! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 1/30/2018 21:34:39'!
testPermutationGroupAction
	self testGroupAction: (SymmetricGroup new: 10) action.
	self testGroupAction: (PermutationGroup cyclic: 15) action! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 3/13/2021 10:08:09'!
testPermutationGroupHomomorphism1
	"Magma Handbook, Example H62E2."
	| C15 A15 f |
	C15 _ PermutationGroup cyclic: 15.
	A15 _ (É/15) asAbelianGroup.
	f _ C15 to: A15 mapping: [:i| A15 generator * 11].
	self assert: A15 = f image.
	self assert: A15 = (C15 apply: f).
	C15 do: [:c|
		C15 do: [:d|
			self assert: (f value: c) * (f value: d) = (f value: c*d)]]! !

!GroupTest methodsFor: 'as yet unclassified' stamp: 'len 8/19/2020 15:30:31'!
testPermutationGroupHomomorphism2
	"Magma Handbook, Example H62E2."
	| C15 h im |
	C15 _ PermutationGroup cyclic: 15.
	h _ C15 to: C15 evaluating: [:g| g ^ 3].
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

!LinearGroupsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testGeneralLinearAtRandom
	| V G |
	V _ Ñ ^ 3.
	G _ V automorphisms.
	self assert: (G atRandomBits: 100) isAutomorphism.
	self assert: (G includes: (G atRandomBits: 100))! !

!LinearGroupsTest methodsFor: 'as yet unclassified' stamp: 'len 7/28/2020 09:37:01'!
testOrthogonalAtRandom
	| V O |
	V _ Ñ ^ 3.
	O _ V innerProduct automorphisms.
	self assert: (O atRandomBits: 100) isOrthogonal.
	self assert: (O includes: (O atRandomBits: 100))! !

!LinearGroupsTest methodsFor: 'as yet unclassified' stamp: 'len 4/1/2021 12:19:16'!
testRotationsBy90Degrees
	"Group of rotations by 90 degrees in the plane, isomorphic to Z/4Z."
	| V G |
	V _ Ñ ^ 2.
	G _ LinearGroup on: V generators: {V endomorphisms elementAt: (0, 1, -1, 0)}.
	self assert: G size = 4.
	self assert: G isCyclic.
	self flag: #todo.
	"test invariant ring K[x,y]^G = K[x^2+y^2, x^2y^2, x^3y-xy^3], isomorphic to K[a,b,c]/<c^2-a^2b+4b^2>"
	"test hilbert series is 1-z^8 / ((1-z^2)^2(1-z^4)) = 1 + z^2 + 3z^4 + 3z^6 + ..."! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testAlgebraicExtension
	| x E a b |
	x _ Ñ polynomials x.
	E _ Ñ extension: x^4 - 2.
	a _ E x.
	self assert: (a^4) representative asScalar = 2.
	b _ a^2 + (E embed: 3).
	self assert: b minimalPolynomial = (x^2 - (x*6) + 7)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 7/2/2016 05:48'!
testBitCount
	| v |
	v _ 1 << 3000 :: atRandom.
	self assert: (v printStringBase: 2 :: count: [:x| x = $1]) = v bitCount! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 7/2/2016 05:48'!
testBitParity
	| v |
	v _ 1 << 3000 :: atRandom.
	self assert: ((v printStringBase: 2 :: count: [:x| x = $1]) bitAnd: 1) = v bitParity! !

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

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/4/2015 01:46'!
testContinuedFractions
	| x |
	x _ ContinuedFraction fromFraction: 415/93.
	self assert: x coefficients = (4, 2, 6, 7).
	self assert: x asFraction = (415/93)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/4/2015 22:01'!
testContinuedFractions2
	-2 to: 2 by: 1/2 do:
		[:x| self assert: (ContinuedFraction fromFraction: x) asFraction = x]! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 3/27/2016 04:47'!
testContinuedFractionsE
	| x e |
	e _ ContinuedFraction e.
	x _ e convergentAt: 20.
	self assert: (x asFloat - 1 exp) abs < 0.0000001! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 3/27/2016 04:47'!
testContinuedFractionsGoldenRatio
	| x phi |
	phi _ ContinuedFraction phi.
	x _ phi convergentAt: 20.
	self assert: (x asFloat - 1.6180339887) abs < 0.000001! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/9/2020 10:16:07'!
testContinuedFractionsGoldenRatio2
	| phi denominators |
	phi _ ContinuedFraction phi.
	denominators _ phi convergents collect: [:each| each asRational denominator].
	self assert: (denominators equals: Sequence fibonacci << 1 upTo: 100)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/3/2015 02:18'!
testContinuedFractionsNegated
	| x |
	x _ ContinuedFraction fromFraction: 415/93.
	self assert: x negated asFraction = (-415/93)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/3/2015 02:19'!
testContinuedFractionsReciprocal
	| x |
	x _ ContinuedFraction fromFraction: 415/93.
	self assert: x reciprocal asFraction = (93/415)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testCyclotomicPolynomials
	| P |
	P _ Ñ polynomials.
	1 to: 20 do: [:n| self assert: (P cyclotomic: n) degree = n phi]! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testCyclotomicPolynomials2
	| P x n |
	P _ Ñ polynomials.
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
testFieldAutomorphisms
	self assert: Ñ automorphisms isTrivial! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testFreeAlgebra
	| A f g h |
	A _ Ñ freeAlgebraIn: #(x y z).
	f _ A atRandomBits: 100.
	g _ A atRandomBits: 100.
	h _ A atRandomBits: 100.
	self assert: (f*g)*h = (f*(g*h)). "associativity"
	self assert: (f+g)*h = (f*h + (g*h)). "distributivity"
	self assert: f * A one = f.
	self assert: A one * f = f! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testFunctionComposition
	| f g a |
	f _ É carrier to: É carrier evaluating: [:x| x * 2].
	g _ É carrier to: É carrier evaluating: [:x| x + 1].
	a _ 1000 atRandom.
	self assert: (f ∑ g value: a) = (f value: (g value: a)).
	self assert: (g ∑ f value: a) = (g value: (f value: a))! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 3/3/2021 07:31:22'!
testFunctionComposition2
	| f g a |
	f _ É carrier to: Ñ carrier evaluating: [:x| x asRational / 2].
	g _ Ñ carrier to: É carrier evaluating: [:x| x numerator].
	a _ 1000 atRandom.
	self assert: (f ∑ g value: a asRational) = (f value: (g value: a asRational)).
	self assert: (g ∑ f value: a) = (g value: (f value: a))! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testFunctionComposition3
	| f g |
	f _ É carrier to: Ñ carrier evaluating: [:x| x / 2].
	g _ É carrier to: É carrier evaluating: [:x| x + 1].
	self should: [g ∑ f] raise: DomainError! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/11/2016 20:01:51'!
testFundamentalDiscriminant
	self assert: ((1 to: 33) select: [:each| each isFundamentalDiscriminant]) =  #(1 5 8 12 13 17 21 24 28 29 33).
	self assert: ((0 to: -31 by: -1) select: [:each| each isFundamentalDiscriminant]) = #(-3 -4 -7 -8 -11 -15 -19 -20 -23 -24 -31) 
! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/13/2015 02:02'!
testIntegerDivisors
	self assert: 12 numberOfDivisors = 6.
	self assert: 12 divisors size = 6! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testIntegerIdealsPrimePrimarySemiprime
	self assert: (É*5) isPrime.
	self assert: (É*6) isPrime not.
	self assert: (É*25) isPrimary.
	self assert: (É*30) isPrimary not.
	self assert: (É*30) isSemiprime.
	self assert: (É*12) isSemiprime not! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:40:22'!
testIntegerSplit
	"Split algorithm from 'Fast Algorithms for Linear Algebra Modulo N', Arne Storjohann, Thom Mulders."
	| D a M |
	10000 timesRepeat:
		[D _ 100 atRandom.
		a _ D atRandom - 1.
		M _ a split: D.
		self assert: M > 0.
		self assert: M | D.
		M factorization asSet do: [:p| self deny: p | a].
		(D / M) factorization asSet do: [:p| self assert: p | a]]! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testIteratorCovering
	| F |
	F _ É / 3.
	self assert: (F^3 upTo: 3^3) asSet size = (3^3).
	self assert: (É^3 upTo: 100) asSet size = 100! !

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

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/26/2016 23:11:25'!
testJacobiAndLegendreSymbols
	| a p |
	10 timesRepeat:
		[p _ (1000 atRandom + 2) nextPrime. "an odd prime"
		a _ 1000 atRandom - 1.
		self assert: (a legendre: p) = (a jacobi: p)]! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2016 08:34:47'!
testKroneckerAndLegendreSymbols
	| a p |
	10 timesRepeat:
		[p _ (1000 atRandom + 2) nextPrime. "an odd prime"
		a _ 1000 atRandom - 1.
		self assert: (a legendre: p) = (a kronecker: p)]! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2016 08:51:30'!
testKroneckerSymbol
	| a b c |
	a _ (-100 to: 100) atRandom.
	b _ (-100 to: 100) atRandom.
	self assert: ((a kronecker: b) = 0) = ((a gcd: b) ~= 1).
	c _ (-100 to: 100) atRandom.
	self assert: (a*b kronecker: c) = ((a kronecker: c) * (b kronecker: c)).
	b*c = 0 ifFalse: [self assert: (a kronecker: b*c) = ((a kronecker: b) * (a kronecker: c))]! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/26/2016 23:13:26'!
testLegendreSymbol
	self assert: ((1 to: 10) collect: [:k| k legendre: 43]) = #(1 -1 -1 1 -1 1 -1 -1 1 1)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2020 09:42:31'!
testMapsInducedByHom
	| X Y Z f g h yz zx |
	X _ #(1 2 3) asSet asDomain.
	Y _ {$x. $y. $z} asSet asDomain.
	Z _ #(a b c) asSet asDomain.
	f _ X to: Y evaluating: [:each| Y atRandom: (Random seed: each hash)].
	g _ f => Z. "Hom(-,Z)"
	self assert: g domain = (Y => Z). "it is contravariant"
	self assert: g codomain = (X => Z).
	yz _ Y to: Z evaluating: [:each| Z atRandom: (Random seed: each hash)].
	self assert: (g value: yz) = (yz ∑ f).
	h _ Z => f. "Hom(Z,-)"
	self assert: h domain = (Z => X). "it is covariant"
	self assert: h codomain = (Z => Y).
	zx _ Z to: X evaluating: [:each| X atRandom: (Random seed: each hash)].
	self assert: (h value: zx) = (f ∑ zx)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 7/2/2016 21:04'!
testPartitions
	self assert: 42 partitions size =  53174! !

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

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2020 07:20:07'!
testPolynomialVectorSpace
	| Qx V S x |
	Qx _ Ñ polynomials.
	x _ Qx x.
	V _ Qx filtrationAt: 5.
	S _ V span: {Qx one. x^2. x*2}.
	self assert: (S includes: V !! (x^2 + 2)).
	self deny: (S includes: V !! (x^3 + 2)).
	self assert: Qx !! (S orthogonalProjection value: V !! (x^3 - (x^2) + (x*2) + 3)) = ((x^2) negated + (x*2) + 3)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 12/13/2015 01:54'!
testPrimeSignature
	self assert: 1 primeSignature = #().
	self assert: 7 primeSignature = #(1).
	self assert: (7*5) primeSignature = #(1 1).
	self assert: 12 primeSignature = #(2 1).
	self assert: 100 primeSignature = #(2 2).
	self assert: 42 primeSignature = #(1 1 1).
	self assert: 16 primeSignature = #(4)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 08:38:58'!
testRadicalAndIsSquarefree
	| n |
	n _ (-1000 to: 1000) atRandom.
	self assert: n isSquarefree = (n radical = n)! !

!MathematicsTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 21:48'!
testRealIntervalIntersects
	| I J |
	I _ RealInterval open: -1 closed: 0.
	J _ RealInterval open: 0 closed: 1.
	self assert: (I intersects: J) not.
	J _ RealInterval closed: 0 open: 1.
	self assert: (I intersects: J).
	J _ RealInterval closed: -2 closed: -1.
	self assert: (I intersects: J) not! !

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

!ModuleTest methodsFor: 'bugs' stamp: 'len 4/26/2021 11:43:09'!
testPIDCoordinatesOfBug
	| M S |
	M _ self newFreeModuleOver: self newScalarsPID.
	S _ M grassmannian: (1 to: M rank) atRandom :: atRandomBits: 50.
	self assert: (S generators allSatisfy: [:each| (S \ each) notNil])! !

!ModuleTest methodsFor: 'maps' stamp: 'len 3/22/2021 11:46:14'!
testModuleMap: f
	| a1 a2 b1 b2 x |
	a1 _ f domain atRandomBits: 20.
	a2 _ f domain atRandomBits: 20.
	b1 _ f value: a1.
	b2 _ f value: a2.
	self assert: (f value: f domain zero) isZero.
	x _ f domain scalars atRandomBits: 20.
	self assert: (f value: a1 * x + a2) = (b1 * x + b2)! !

!ModuleTest methodsFor: 'maps' stamp: 'len 3/22/2021 11:46:57'!
testProjection: f
	| v |
	self testModuleMap: f.
	v _ f domain atRandomBits: 20.
	self assert: (f value: v) = (f value: (f value: v)). "idempotent at v"
	self assert: f isIdempotent.
	self assert: (f image apply: f) = f image.
	self assert: (f codomain apply: f) = f image! !

!ModuleTest methodsFor: 'other' stamp: 'len 4/26/2021 11:42:54'!
testFreeModule: M
	| c t v x y |
	x _ M atRandomBits: 10 * M rank.
	self assert: x = (M elementAt: (M \ x)).
	y _ M atRandomBits: 10 * M rank.
	c _ M scalars atRandomBits: 20.
	v _ x * c + y.
	t _ (M \ x) * c + (M \ y).
	self assert: v = (M elementAt: t)! !

!ModuleTest methodsFor: 'other' stamp: 'len 4/8/2021 12:22:38'!
testIsSemisimple
	| V |
	V _ self newVectorSpace.
	self assert: V isSemisimple.
	V _ É asIdeal. "because Z is not artinian"
	self assert: V isSemisimple not! !

!ModuleTest methodsFor: 'quotient and complement' stamp: 'len 3/10/2021 14:12:48'!
testQuotientGolay1
	"Construct the quotient (Z/3Z)^11 by the Golay code.
	(Example from Magma H28E10.)"
	| V S Q |
	V _ É/3 ^ 11.
	S _ V span: {(1,0,0,0,0,0,1,1,1,1,1). (0,1,0,0,0,0,0,1,2,2,1). (0,0,1,0,0,0,1,0,1,2,2). (0,0,0,1,0,0,2,1,0,1,2). (0,0,0,0,1,0,2,2,1,0,1). (0,0,0,0,0,1,1,2,2,1,0)}.
	Q _ V/S.
	self assert: Q rank = 5.
	self assert: Q degree = 5! !

!ModuleTest methodsFor: 'quotient and complement' stamp: 'len 5/23/2020 09:34:55'!
testQuotientGolay2
	"Construct the quotient (Z/3Z)^11 by the Golay code as a subspace (the orthogonal complement of the Golay code).
	(Example from Magma H28E11.)"
	| V S Q |
	V _ É/3 ^ 11.
	S _ V span: ({(1,0,0,0,0,0,1,1,1,1,1). (0,1,0,0,0,0,0,1,2,2,1). (0,0,1,0,0,0,1,0,1,2,2). (0,0,0,1,0,0,2,1,0,1,2). (0,0,0,0,1,0,2,2,1,0,1). (0,0,0,0,0,1,1,2,2,1,0)} collect: [:each| each over: V scalars]).
	Q _ S complement.
	self assert: Q rank = 5.
	self assert: Q degree = 11! !

!ModuleTest methodsFor: 'private' stamp: 'len 4/21/2020 05:52:54'!
newFreeModule
	^ self newFreeModuleOver: self newScalarsRing! !

!ModuleTest methodsFor: 'private' stamp: 'len 4/8/2021 12:22:33'!
newFreeModuleOver: aRing
	| M N |
	M _ 3 atRandom = 1 ifTrue: [aRing asIdeal] ifFalse: [aRing ^ 3 atRandom].
	5 atRandom = 1 ifTrue:
		[N _ self newFreeModuleOver: aRing.
		2 atRandom = 1 ifTrue: [^ M => N].
		^ M ë N].
	5 atRandom = 1 ifTrue: [^ M dual].
	^ M! !

!ModuleTest methodsFor: 'private' stamp: 'len 6/1/2020 14:14:17'!
newMatrix: shape over: aRing
	| hom samples |
	hom _ aRing^shape y => (aRing^shape x).
	2 atRandom = 1 ifTrue: [^ hom atRandomBits: 100].
	samples _ {aRing zero. aRing one. aRing one negated}.
	^ hom filling: [:i :j| 2 atRandom > 1 ifTrue: [aRing zero] ifFalse: [samples atRandom]]! !

!ModuleTest methodsFor: 'private' stamp: 'len 3/22/2021 12:02:55'!
newModule
	^ self newModuleOver: self newScalarsRing! !

!ModuleTest methodsFor: 'private' stamp: 'len 3/22/2021 11:51:07'!
newModuleMap
	| R V W |
	R _ self newScalarsRing.
	V _ self newFreeModuleOver: R.
	W _ self newFreeModuleOver: R.
	^ V => W !! (self newMatrix: W rank @ V rank over: R)! !

!ModuleTest methodsFor: 'private' stamp: 'len 3/22/2021 12:03:17'!
newModuleOver: aRing
	| M N |
	M _ self newFreeModuleOver: aRing.
	2 atRandom = 1 ifTrue: [^ M].
	N _ M grassmannian: (0 to: M rank) atRandom :: atRandomBits: 50 atRandom.
	^ M / N! !

!ModuleTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newScalarsField
	^ {Ñ. É/10 atRandom nextPrime. Ñ polynomials fractions. (É/10 atRandom nextPrime) polynomials fractions. GaloisField new: 2 to: 2} atRandom! !

!ModuleTest methodsFor: 'private' stamp: 'len 8/4/2020 14:29:17'!
newScalarsPID
	^ {Ñ. É/10 atRandom nextPrime. É. Ñ polynomials. (É/10 atRandom nextPrime) polynomials} atRandom! !

!ModuleTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newScalarsRing
	^ {É. Ñ. É/10 atRandom nextPrime. Ñ polynomials. (É/10 atRandom nextPrime) polynomials. Ñ polynomials fractions. (É/10 atRandom nextPrime) polynomials fractions} atRandom! !

!ModuleTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newSmallVectorSpace
	^ self newFreeModuleOver: {Ñ. É / #(2 3 5 7) atRandom} atRandom! !

!ModuleTest methodsFor: 'private' stamp: 'len 4/21/2020 05:57:02'!
newVectorSpace
	^ self newFreeModuleOver: self newScalarsField! !

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

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 11/22/2016 08:31:09'!
testHash
	| m1 m2 |
	m1 _ Monomial exponents: #(1 2 0).
	m2 _ Monomial exponents: #(1 2 0 0 0 0 0).
	self assert: m1 hash = m2 hash! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testLocalMonomialOrderings
	"Examples from Magma handbook."
	| f |
	f _ É polynomialsIn: #(x y z) ordering: #llex :: generate: [:x :y :z| 1 + x + y + z + (x^7) + (x^8*(y^7)) + (y^5) + (z^10)].
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
	P _ É polynomialsIn: 3.

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
	R _ É polynomialsIn: #(a b c d).
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
	P _ É polynomialsIn: 4.
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
	f _ (Ñ polynomialsIn: #(x y z)) !! [:x :y :z| x^3*y*z + (x^3) + (x*(y^2)) + (y^5) + (z^4)].
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

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 3/6/2021 13:48:59'!
testWeightedMonomialOrderings
	| P x y z f lex glex grevlex revlex |
	P _ É polynomialsIn: 3.

	x _ P x.
	y _ P y.
	z _ P z.

	f _ 10*x - (7*(y^4)) + (11*(y^3)*z).

	lex _ WeightedMonomialOrdering lex: (1 to: 3).
	glex _ WeightedMonomialOrdering glex: (1 to: 3).
	grevlex _ WeightedMonomialOrdering grevlex: (1 to: 3).
	revlex _ WeightedMonomialOrdering revlex: (1 to: 3).

	self assert: (f orderedBy: lex) leadingCoefficient = 10.
	self assert: (f orderedBy: glex) leadingCoefficient = -7.
	self assert: (f orderedBy: grevlex) leadingCoefficient = -7. "this matches the definition in Singular"
	self assert: (f orderedBy: revlex) leadingCoefficient = 10! !

!MonomialTest methodsFor: 'as yet unclassified' stamp: 'len 3/6/2021 13:18:48'!
testWeightedMonomialOrderingsConsistency
	| P x y z f lex glex grevlex |
	P _ É polynomialsIn: 3.

	x _ P x.
	y _ P y.
	z _ P z.

	f _ 10*x - (7*(y^4)) + (11*(y^3)*z).

	lex _ WeightedMonomialOrdering lex: (1 to: 3).
	glex _ WeightedMonomialOrdering glex: (1 to: 3).
	grevlex _ WeightedMonomialOrdering grevlex: (1 to: 3).
"	revlex _ WeightedMonomialOrdering revlex: (1 to: 3)."

	self assert: (f orderedBy: lex) monomials asArray = (f orderedBy: #lex) monomials asArray.
	self assert: (f orderedBy: glex) monomials asArray = (f orderedBy: #glex) monomials asArray.
	self assert: (f orderedBy: grevlex) monomials asArray = (f orderedBy: #grevlex) monomials asArray.
"	self assert: (f orderedBy: revlex) monomials asArray = (f orderedBy: #revlex) monomials asArray"! !

!MultivariateModuleTest methodsFor: 'as yet unclassified' stamp: 'len 1/5/2022 11:43:23'!
testModuleGroebnerBasisReduced
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.6.1."
	| R x y M f1 f2 f3 f4 A |
	R _ Ñ polynomialsIn: #(x y) ordering: #invlex.
	x _ R x. y _ R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M _ R^3 orderedBy: #top.
"	e1 _ M x: 3. e2 _ M x: 2. e3 _ M x: 1."
	f1 _ M !! {x*y. y. x} reversed.
	f2 _ M !! {x^2 + x. y + (x^2). y} reversed.
	f3 _ M !! {y negated. x. y} reversed.
	f4 _ M !! {x^2. x. y} reversed.
	A _ Matrix rows: {f1. f2. f3. f4}.
	self assert: A basis "reduced" size = 6 "it's already reduced"
	"TODO: verify that it matches the book"! !

!MultivariateModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2021 13:28:14'!
testPOTOrdering
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Definition 3.5.3."
	| R x y M e1 e2 |
	R _ Ñ polynomialsIn: #(x y) ordering: #invlex.
	x _ R x. y _ R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M _ R^2 orderedBy: #pot.
	e1 _ M x: 1. e2 _ M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	self assert: (e1*x) leadingMonomial < (e1*y) leadingMonomial.
	self assert: (e1*y) leadingMonomial < (e1*x*y) leadingMonomial.
	self assert: (e1*x*y) leadingMonomial < (e2*x) leadingMonomial! !

!MultivariateModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2021 13:28:20'!
testSTuple
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.5.18."
	| R x y M e1 e2 f g h |
	R _ Ñ polynomialsIn: #(x y) ordering: #invlex.
	x _ R x. y _ R y.
	x leadingMonomial < y leadingMonomial.
	M _ R^2 orderedBy: #top.
	e1 _ M x: 1. e2 _ M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f _ M !! {x^2 + 1. x*(y^3)*5 + x}.
	g _ M !! {x^2*y. x^3*y*3 + y}.
	h _ M !! {x^4/5 + (x^2/5) - (x^2*(y^3)/3). x^3/5 - (y^3/3)}.
	(f s: g) = h
! !

!MultivariateModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2021 13:28:25'!
testSchreyerOrdering
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.7.11."
	| R x y M e1 e2 e3 g1 g2 g3 a b c |
	R _ Ñ polynomialsIn: #(x y) ordering: #invlex.
	x _ R x. y _ R y.
	self assert: x leadingMonomial < y leadingMonomial.
	g1 _ x^2*(y^2) - (x^3*y).
	g2 _ x*(y^3) - (x^2*(y^2)).
	g3 _ y^4 - (x^3).
	M _ R^3 orderedBy: (SchreyerOrdering weights: {g1. g2. g3}).
	e1 _ M x: 1. e2 _ M x: 2. e3 _ M x: 3.
	a _ e2*x.
	b _ e1*y.
	c _ e3*x.
	self assert: a leadingMonomial < b leadingMonomial.
	self assert: b leadingMonomial < c leadingMonomial.
	self assert: a leadingMonomial < c leadingMonomial! !

!MultivariateModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2021 13:28:30'!
testTOPOrdering
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Definition 3.5.2."
	| R x y M e1 e2 |
	R _ Ñ polynomialsIn: #(x y) ordering: #invlex.
	x _ R x. y _ R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M _ R^2 orderedBy: #top.
	e1 _ M x: 1. e2 _ M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	self assert: (e1*x) leadingMonomial < (e2*x) leadingMonomial.
	self assert: (e2*x) leadingMonomial < (e1*y) leadingMonomial.
	self assert: (e1*y) leadingMonomial < (e1*x*y) leadingMonomial! !

!MultivariateModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2021 13:28:35'!
testTupleOrderings
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.5.5."
	| R x y M e1 e2 f |
	R _ Ñ polynomialsIn: #(x y) ordering: #invlex.
	x _ R x. y _ R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M _ R^2 orderedBy: #top.
	e1 _ M x: 1. e2 _ M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f _ M !! {x^3*y*2 - (y^3) + (x*5). x*(y^2)*3 + (y^2) + (x*4)}.

	self assert: f leadingMonomial position = 1.
	self assert: f leadingMonomial monomial = (y^3) leadingMonomial.
	self assert: f leadingCoefficient = (-1).
	self assert: f leadingTerm = (e1 * (y^3) negated).

	M _ M orderedBy: #pot.
	e1 _ M x: 1. e2 _ M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f _ M !! f.

	self assert: f leadingMonomial position = 2.
	self assert: f leadingMonomial monomial = (y^2*x) leadingMonomial.
	self assert: f leadingCoefficient = 3.
	self assert: f leadingTerm = (e2 * (y^2*x*3))! !

!MultivariateModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2021 13:28:40'!
testTupleReduction
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.5.7."
	|R x y M e1 e2 f g h |
	R _ Ñ polynomialsIn: #(x y) ordering: #invlex.
	x _ R x. y _ R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M _ R^2 orderedBy: #top.
	e1 _ M x: 1. e2 _ M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f _ M !! {x^3*y*2 - (y^3). x*(y^2)*3 + (y^2) + (x*4)}.
	g _ M !! {x + 1. y^2 + x}.
	h _ M !! {x^3*y*2 - (y^3) - (x^2*3) - (x*4) - 1. x*3 - (x^2*3)}.
	self assert: f \\ {g} = h! !

!MultivariateModuleTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2021 13:28:45'!
testTupleReduction2
	"William W. Adams, Philippe Loustaunau, 'An Introduction to Groebner Bases' (1994), Example 3.5.9."
	|R x y M e1 e2 f1 f2 f h |
	R _ Ñ polynomialsIn: #(x y) ordering: #invlex.
	x _ R x. y _ R y.
	self assert: x leadingMonomial < y leadingMonomial.
	M _ R^2 orderedBy: #top.
	e1 _ M x: 1. e2 _ M x: 2.
	self assert: e1 leadingMonomial < e2 leadingMonomial.
	f1 _ M !! {x*y - y. x^2}.
	f2 _ M !! {x. y^2 - x}.
	f _ M !! {y^2 + (x^2*y*2). y^2}.
	h _ M !! {y^2 + (y*2) - x. x^3*(-2) - (x^2*2) + x}.
	self assert: f \\ {f1. f2} = h! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 21:58:12'!
testCyclotomicFieldDiscriminant
	self assert: (CyclotomicField new: 20) discriminant = 4000000.
	self assert: (CyclotomicField new: 18) discriminant = -19683! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testMinimalPolynomial
	"Example from 'Algebraic Number Theory: A Computational Approach' by William Stein (28 pp)."
	| x K a |
	x _ Ñ polynomials x.
	K _ Ñ extension: x^2 - 2.
	a _ K x.
	self assert: a minimalPolynomial = (x^2 - 2).
	self assert: (a/2 + 3) minimalPolynomial = (x^2 - (x*6) + (17/2))! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 3/22/2021 10:37:04'!
testMinimalPolynomial2
	"Example from 'Algebraic Number Theory: A Computational Approach' by William Stein (29 pp)."
	| x K a alpha |
	x _ Ñ polynomials x.
	K _ QuadraticField sqrt: 5.
	a _ K x.
	self assert: a squared = (K !! 5).
	alpha _ (1 + a) / 2.
	self assert: alpha minimalPolynomial = (x^2 - x - 1)! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testMinimalPolynomial3
	"Example from 'Algebraic Number Theory: A Computational Approach' by William Stein (29 pp)."
	| x alpha |
	x _ Ñ polynomials x.
	alpha _ 1 + 5 squareRoot / 2.
	self assert: alpha minimalPolynomial = (x^2 - x - 1)! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testNormAndTrace
	| F a b |
"	x _ Ñ polynomials x.
	F _ Ñ extension: x^4 - (x^2*420) + 40000."
	F _ Ñ extensionDegree: (2 to: 4) atRandom.
	a _ F atRandomBits: 100.
	b _ F atRandomBits: 100.
	self assert: (a + b) trace = (a trace + b trace).
	self assert: (a * b) norm = (a norm * b norm)! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/11/2021 19:16:06'!
testNormOfAlgebraicInteger
	| F a |
	F _ Ñ extensionDegree: (2 to: 4) atRandom.
	a _ F !! (F integers atRandomBits: 100).
	self assert: a norm isIntegral! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testNormTraceAndMinimalPolynomial
	"Example H38E17 from Magma documentation."
	| x F a |
	x _ Ñ polynomials x.
	F _ Ñ extension: x^4 - (x^2*420) + 40000.
	a _ F x / 2.
	self assert: a trace = 0.
	self assert: a norm = 2500.
	self assert: a minimalPolynomial = (x^4 - (x^2*105) + 2500)! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/18/2021 18:04:43'!
testQSqrt2
	| E x |
	x _ Ñ polynomials x.
	E _ Ñ adjoin: RealAlgebraicNumber sqrt2.
	self assert: E x minimalPolynomial = E modulus.
	self assert: E x squared minimalPolynomial = (x - 2)! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/16/2021 11:08:27'!
testQSqrt2RealEmbeddings
	| E embeddings roots |
	E _ Ñ adjoin: RealAlgebraicNumber sqrt2.
	embeddings _ E realEmbeddings.
	roots _ (E modulus rootsIn: Ö) asSet.
	self assert: (embeddings collect: [:f| f value: E x]) asSet = roots! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 12/14/2016 14:59:36'!
testQuadraticFieldFundamentalDiscriminant
	self assert: (QuadraticField sqrt: 102) discriminant = 408.
	self assert: (QuadraticField sqrt: 720) discriminant = 5.
	self assert: (QuadraticField sqrt: 2) discriminant = 8! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/16/2021 11:08:32'!
testQuadraticFieldSqrt2
	| E x |
	E _ QuadraticField sqrt: 2.
	x _ Ñ polynomials x.
	self assert: E x minimalPolynomial = E modulus.
	self assert: E x squared minimalPolynomial = (x - 2).
	self assert: E discriminant = 8! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/16/2021 11:08:37'!
testQuadraticFieldSqrtMinusFive
	| E |
	E _ QuadraticField sqrt: -5.
	self assert: E x minimalPolynomial = E modulus.
	self assert: E x squared = (E embed: -5).
	self assert: E x ^ 4 = (E embed: 25).
	self assert: E discriminant = -20! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/16/2021 10:26:19'!
testSignature
	| x F |
	x _ Ñ polynomials x.
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

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/16/2021 10:26:32'!
testTotallyImaginary
	| x E |
	x _ Ñ polynomials x.
	E _ NumberField modulus: x^2 + 2.
	self assert: E isImaginary.
	E _ NumberField modulus: x^2 - 2.
	self assert: E isImaginary not.
	E _ NumberField modulus: x^4 - 2.
	self assert: E isImaginary not! !

!NumberFieldTest methodsFor: 'as yet unclassified' stamp: 'len 7/16/2021 10:26:44'!
testTotallyReal
	| x E |
	x _ Ñ polynomials x.
	E _ NumberField modulus: x^2 + 2.
	self assert: E isReal not.
	E _ NumberField modulus: x^2 - 2.
	self assert: E isReal.
	E _ NumberField modulus: x^4 - 2.
	self assert: E isReal not! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testAddition
	| R a b c |
	R _ É adic: 5.
	a _ R !! 123.
	b _ R !! -445.
	c _ R !! (123 - 445).
	self assert: a+b = c.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 20).
	c _ a+b.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 20)! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testAddition2
	| Z7 a b c |
	Z7 _ É adic: 7.
	a _ Z7 !! 676.
	b _ Z7 !! 123.
	c _ Z7 !! (676+123).
	self assert: a+b = c! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testAddition3
	| Z7 a b c |
	Z7 _ É adic: 7.
	a _ Z7 !! -676.
	b _ Z7 !! 123.
	c _ Z7 !! (-676+123).
	self assert: a+b = c! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testAdditiveInverse
	| R a b |
	R _ É adic: 7.
	a _ R !! -676.
	b _ R !! 676.
	self assert: a = b negated.
	self assert: a negated negated = a! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testFractionsField
	| Z7 Q7 |
	Z7 _ É adic: 7.
	Q7 _ Ñ adic: 7.
	self assert: Z7 fractions = Q7! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testLocalProperties
	| R |
	R _ É adic: 7.
	self assert: R isLocal.
	self assert: R maximalIdeal = (R * 7)! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMultiplication
	| R a b c |
	R _ É adic: 5.
	a _ R !! 123.
	b _ R !! -445.
	c _ R !! (123 * -445).
	self assert: a * b = c.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 19)! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMultiplication2
	| R a b c |
	R _ É adic: 7.
	a _ R !! 676.
	b _ R !! 123.
	c _ R !! (676 * 123).
	self assert: a * b = c! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMultiplication3
	| R a b c p |
	R _ É adic: 5.
	p _ R uniformizer.
	a _ R !! 123.
	b _ p^8 + 1.
	c _ R !! (123 * (5^8 + 1)).
	self assert: a * b = c.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 20)! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMultiplicationBug
	| R zero |
	R _ É adic: 5.
	zero _ R zero.
	self assert: zero * zero = zero! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testNormAndValuation
	| R x |
	R _ É adic: 7.
	x _ R !! 13.
	self assert: x valuation = 0.
	self assert: x abs = 1.
	x _ x * (7*7).
	self assert: x valuation = 2.
	self assert: x abs = (1/(7*7)).
	R _ É adic: 7.
	x _ R !! 0.
	self assert: x valuation = Infinity positive.
	self assert: x abs = 0! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testPrecision
	"Some tests to match Magma."
	| R p a |
	R _ É adic: 5.
	p _ R uniformizer.
	self assert: p precision = 20.
	a _ p*p negated.
	self assert: {a precision. a absolutePrecision. a relativePrecision} = #(21 21 19).
	a _ R !! -25.
	self assert: {a precision. a absolutePrecision. a relativePrecision} = #(20 20 18)! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testProjectionToPrimePowerRingAndLifting
	| R a x |
	R _ É adic: 7.
	a _ (-1024 to: 1024) atRandom.
	x _ R !! a.
	self assert: ((É/(7^5)) !! x) lift = a.
	self assert: ((É/(7^(5 to: 10) atRandom)) !! x) lift = a! !

!PAdicsTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testSubstraction
	| R a b c |
	R _ É adic: 7.
	a _ R !! 676.
	b _ R !! 123.
	c _ R !! (676 - 123).
	self assert: a - b = c! !

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
	self testArithmeticOver: Ñ! !

!PolynomialTest methodsFor: 'arithmetic' stamp: 'len 5/23/2020 09:34:55'!
testArithmeticZ2
	self testArithmeticOver: É/2! !

!PolynomialTest methodsFor: 'arithmetic' stamp: 'len 5/23/2020 09:34:55'!
testArithmeticZ3
	self testArithmeticOver: É/3! !

!PolynomialTest methodsFor: 'arithmetic' stamp: 'len 5/23/2020 09:34:55'!
testArithmeticZm
	self testArithmeticOver: É/(2 + 100 atRandom)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/12/2020 12:33:23'!
testDivision: F
	| Fx f g |
	Fx _ F polynomials.
	f _ Fx atRandomBits: 50 maxDegree: 5 atRandom.
	[g _ Fx atRandomBits: 50 maxDegree: 5 atRandom. g isZero] whileTrue.
	self assert: (f // g) * g + (f \\ g) = f! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/10/2020 02:23:03'!
testDivisionPrimeField65535
	self testDivision: (PrimeField new: 2^17 - 1)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/10/2020 02:22:44'!
testDivisionPrimeFieldZ2
	self testDivision: (PrimeField new: 2)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testDivisionQ
	self testDivision: Ñ! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testDivisionQ1
	| P x f g |
	P _ Ñ polynomials.
	x _ P x.
	f _ x.
	g _ 1 - (x*3).
	self assert: f // g = (P !! (-1/3)).
	self assert: f \\ g = (P !! (1/3))! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testDivisionQ2
	| P x f g |
	P _ Ñ polynomials.
	x _ P x.
	f _ x.
	g _ 1 + (x*3).
	self assert: f // g = (P !! (1/3)).
	self assert: f \\ g = (P !! (-1/3))! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testDivisionRationalFunctions
	self testDivision: Ñ polynomials fractions! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionRationalFunctionsZ2
	self testDivision: (É/2) polynomials fractions! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/13/2020 08:53:01'!
testDivisionRationalFunctionsZ65535
	self testDivision: (PrimeField new: 2^17 - 1) polynomials fractions! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ1
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x.
	g _ 1 + (x*3).
	self assert: f // g = (P !! 0).
	self assert: f \\ g = x! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ2
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x.
	g _ 1 - (x*3).
	self assert: f // g = (P !! -1).
	self assert: f \\ g = (1 - (x*2))! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ3
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x.
	g _ 1 - x.
	self assert: f // g = (P !! -1).
	self assert: f \\ g = (P !! 1)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ4
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x.
	g _ 1 + x.
	self assert: f // g = (P !! 1).
	self assert: f \\ g = (P !! -1)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ5
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x*4.
	g _ x*2.
	self assert: f // g = (P !! 2).
	self assert: f \\ g = (P !! 0)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ6
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x*4.
	g _ x*3.
	self assert: f // g = (P !! 1).
	self assert: f \\ g = x! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ7
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x*4+1.
	g _ x^2*3.
	self assert: f // g = (P !! 0).
	self assert: f \\ g = f! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ8
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x*(-10).
	g _ x*(-9).
	self assert: f // g = (P !! 1).
	self assert: f \\ g = x negated! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testDivisionZ9
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x*(-10).
	g _ x*(-3).
	self assert: f // g = (P !! 3).
	self assert: f \\ g = (x*(-1))! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:17:56'!
testExactDivisionBug
	| Zx x f |
	Zx _ É polynomials.
	x _ Zx x.
	f _ x*2.
	self assert: f / f = Zx one! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:18:03'!
testExactDivisionMultivariate
	| Qxy x y f g |
	Qxy _ Ñ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	f _ x-y.
	g _ x+y.
	self assert: f * g / g = f.
	self assert: f * g / f = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testExactDivisionMultivariate2
	| Qxy x y f g |
	Qxy _ Ñ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	f _ x-y.
	g _ x+y.
	self assert: (f * g divisionBy: g) quotient = f.
	self assert: (f * g divisionBy: f) quotient = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:18:09'!
testExactDivisionQ
	| x f g |
	x _ Ñ polynomials x.
	f _ x^2 + x + 1.
	g _ 1 - x.
	self assert: f * g / g = f.
	self assert: f * g / f = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:18:14'!
testExactDivisionQRandom
	| Qx f g |
	Qx _ Ñ polynomials.
	f _ Qx atRandomBits: 100 maxDegree: (1 to: 10) atRandom.
	g _ Qx atRandomBits: 100 maxDegree: (1 to: 10) atRandom.
	self assert: f * g / g = f.
	self assert: f * g / f = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 1/9/2022 08:18:21'!
testExactDivisionZ
	| x f g |
	x _ É polynomials x.
	f _ x^2 + x + 1.
	g _ 1 - x.
	self assert: f * g / g = f.
	self assert: f * g / f = g! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testMultivariateDivision
	"Example from Cox IVA 62 p."
	| Zxy x y f f1 f2 division |
	Zxy _ É polynomialsIn: #(x y) ordering: (MonomialOrdering lex: #(2 1)).
	x _ Zxy x.
	y _ Zxy y.
	f _ x*(y^2)+1.
	f1 _ x*y+1.
	f2 _ y+1.
	division _ f divisionBy: {f1. f2}.
	self assert: division quotients = {y. y one negated}.
	self assert: division remainder = (y one * 2).
	self assert: division verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testMultivariateDivision2
	"Example from Cox IVA 63 p."
	| Zxy x y f f1 f2 division |
	Zxy _ É polynomialsIn: #(x y) ordering: (MonomialOrdering lex: #(2 1)).
	x _ Zxy x.
	y _ Zxy y.
	f _ x^2*y + (x*(y^2)) + (y^2).
	f1 _ x*y-1.
	f2 _ y^2-1.
	division _ f divisionBy: {f1. f2}.
	self assert: division quotients = {x+y. y one}.
	self assert: division remainder = (x+y+1).
	self assert: division verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testMultivariateDivision3
	"Example from Cox IVA 67 p."
	| Zxy x y f f1 f2 division |
	Zxy _ É polynomialsIn: #(x y) ordering: (MonomialOrdering lex: #(2 1)).
	x _ Zxy x.
	y _ Zxy y.
	f _ x^2*y + (x*(y^2)) + (y^2).
	f1 _ x*y-1.
	f2 _ y^2-1.
	division _ f divisionBy: {f2. f1}.
	self assert: division quotients = {x+1. x}.
	self assert: division remainder = (x*2+1).
	self assert: division verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivision1
	| Zxy x y division |
	Zxy _ É polynomialsIn: #(x y).
	x _ Zxy x.
	y _ Zxy y.
	division _ x^2 + (y^2) pseudoDivisionBy: y - x in: 2.
	self assert: division quotient asMultivariate = (x + y).
	self assert: division remainder asMultivariate = (x^2 * 2).
	self assert: division verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testPseudoDivision2
	| Zxy x y division |
	Zxy _ Ñ polynomialsIn: #(x y).
	x _ Zxy x.
	y _ Zxy y.
	division _ x^2 + (y^2) pseudoDivisionBy: (y-x)*2 in: 2.
	self assert: division quotient asMultivariate = ((x+y) * 2).
	self assert: division remainder asMultivariate = (x^2 * 8).
	self assert: division verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivision3
	| Zxy x y division |
	Zxy _ É polynomialsIn: #(x y).
	x _ Zxy x.
	y _ Zxy y.
	division _ x^2 + (y^2) pseudoDivisionBy: y - x in: 2.
	self assert: division quotient asMultivariate = (x + y).
	self assert: division remainder asMultivariate = (x^2 * 2).
	self assert: division verify.

	division _ x^2 + (y^2) pseudoDivisionBy: x - y in: 2.
	self assert: division quotient asMultivariate = (x + y) negated.
	self assert: division remainder asMultivariate = (x^2 * 2).
	self assert: division verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/12/2020 12:33:15'!
testPseudoDivision: R
	| Rx f g division |
	Rx _ R polynomials.
	f _ Rx atRandomBits: 50 maxDegree: 5 atRandom.
	[g _ Rx atRandomBits: 50 maxDegree: 5 atRandom. g isZero] whileTrue.
	division _ f pseudoDivisionBy: g.
	self assert: division verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testPseudoDivisionOverPolynomialRing1
	self testPseudoDivision: Ñ polynomials! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:33:01'!
testPseudoDivisionOverPolynomialRing2
	self testPseudoDivision: (Ñ polynomialsIn: 2)! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionUnivariate
	| Zx x division |
	Zx _ É polynomials.
	x _ Zx x.
	division _ x^3 + x + 1 pseudoDivisionBy: (x^2 * 3) + x + 1.
	self assert: division quotient = (x*3 - 1).
	self assert: division remainder = (x*7 + 10).
	self assert: division delta = 2.
	self assert: division verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionUnivariate2
	"Example from Zippel book"
	| Zx a b |
	Zx _ É polynomials.
	a _ Zx coefficients: #(1 0 1 0 -3 -3 8 2 -5) reversed.
	b _ Zx coefficients: #(3 0 5 0 -4 -9 21) reversed.
	^ self assert: (a pseudoDivisionBy: b) verify! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionZ
	self testPseudoDivision: É! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionZ12
	self testPseudoDivision: É / 12! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionZ6
	self testPseudoDivision: É / 6! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivisionZ8
	self testPseudoDivision: É / 8! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testPseudoRemainder
	| Zx x a b r |
	Zx _ É polynomials.
	x _ Zx x.
	a _ x^8 + (x^6) - (3*(x^4)) - (3*(x^3)) + (8*(x^2)) + (2*x) - 5.
	b _ 3*(x^6) + (5*(x^4)) - (4*(x^2)) - (9*x) + 21.
	r _ Zx coefficients: #(-9 0 3 0 -15).
	self assert: (a pseudoRem: b) = r.
	self assert: (a pseudoDivisionBy: b) remainder = r! !

!PolynomialTest methodsFor: 'division' stamp: 'len 5/23/2020 09:34:55'!
testRemainderInconsistency
	"In Sage these two give the same result (1), same in Magma."
	| X x |
	X _ É polynomials x.
	x _ Ñ polynomials x.
	self assert: (x \\ (1 - x)) leadingCoefficient = (X \\ (1 - X)) leadingCoefficient! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 4/14/2021 05:58:31'!
testCRT
	| x f1 f2 f3 crt r1 r2 r3 h |
	x _ Ñ polynomials x.
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

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testCompanionMatrix
	| x p |
	x _ Ñ polynomials x.
	p _ x^2 + (x*2) - 1.
	self assert: p companion characteristicPolynomial = p.
	self assert: p companion minimalPolynomial = p! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testDiscriminant
	| Zx x f g |
	Zx _ É polynomials.
	x _ Zx x.
	f _ x^2 + x + 1.
	g _ x^3 + 1.
	self assert: (f*g) discriminant = (f discriminant * g discriminant * (f resultant: g) squared)! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testDiscriminant2
	| Zx x f |
	Zx _ É polynomials.
	x _ Zx x.
	f _ x^2 - x - 36042.
	self assert: f discriminant = 144169 "this is a prime, although looks like a square :)"! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 1/7/2022 20:38:05'!
testDomainError
	| f g |
	f _ É polynomials atRandomBits: 10.
	self should: [f / 2] raise: Error.
	g _ Ñ polynomials atRandomBits: 100 maxDegree: 5. "will have denominators"
	self should: [f + g] raise: Error.
	self should: [f * g] raise: Error! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testGaussLemma
	"Lemma (Gauss): The product of two primitive polynomials is primitive."
	| Zx f g |
	Zx _ É polynomials.
	f _ Zx atRandomBits: 100 maxDegree: (5 to: 10) atRandom.
	g _ Zx atRandomBits: 100 maxDegree: (5 to: 10) atRandom.
	self assert: (f primitivePart * g primitivePart) isPrimitive! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testHeight
	| x |
	x _ Ñ polynomials x.
	self assert: (x * 2 - 5) height =  5.
	self assert: (x * 5 - 2) height =  5! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testInclusionInFractionField
	| R F |
	R _ Ñ polynomials.
	F _ R fractions.
	self assert: R <= F.
	self assert: F >= R.
	self deny: R >= F.
	self deny: F <= R! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testInterpolation
	| Qx f |
	Qx _ Ñ polynomials.
	f _ Qx interpolate: {1 -> 2. 2 -> 3. 4 -> 5}.
	self assert: (f value: 1) = 2.
	self assert: (f value: 2) = 3.
	self assert: (f value: 4) = 5! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testInterpolation2
	| Qx f |
	Qx _ Ñ polynomials.
	f _ Qx interpolate: {1 -> 2. 2 -> 4. 3 -> 9}.
	self assert: (f value: 1) = 2.
	self assert: (f value: 2) = 4.
	self assert: (f value: 3) = 9! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 4/14/2021 06:01:49'!
testIsIrreducible
	"The polynomial x^4 + 1 is irreducible in Z[x] and Q[x], and it is reducible Z/p[x] for all prime numbers p."
	| p |
	self assert: (É polynomials !! [:x| x^4 + 1]) isIrreducible.
	self assert: (Ñ polynomials !! [:x| x^4 + 1]) isIrreducible.
	p _ 1000 atRandom nextPrime.
	self deny: (É/p :: polynomials !! [:x| x^4 + 1]) isIrreducible! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testIsIrreducibleOverQ
	| x f |
	x _ Ñ polynomials x.
	f _ x^4 + 1.
	self assert: f isIrreducible.
	self assert: f negated isIrreducible.
	self assert: (f*2) isIrreducible.
	self assert: (f*(x+1)) isIrreducible not! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testIsIrreducibleOverZ
	| x f |
	x _ É polynomials x.
	f _ x^4 + 1.
	self assert: f isIrreducible.
	self assert: f negated isIrreducible.
	self assert: (f*2) isIrreducible not.
	self assert: (f*(x+1)) isIrreducible not.
! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testIsNilpotentOverZm
	| m R N U d f k |
	m _ (2 to: 100) atRandom.
	R _ É/m.
	N _ R nilradical asSet.
	U _ R units asSet.
	d _ (1 to: 5) atRandom.
	f _ R polynomials coefficients: ((1 to: d) collect: [:i| N atRandom]).
	self assert: f isNilpotent.
	k _ (1 to: d) atRandom.
	f _ R polynomials coefficients: ((1 to: d) collect: [:i| i=k ifTrue: [U atRandom] ifFalse: [N atRandom]]).
	self assert: f isNilpotent not! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testIsSeparableNot
	"A tipical example of a polynomial that is NOT separable."
	| p F x Fy y f |
	p _ 5. "any prime p"
	F _ É/p :: polynomials fractions.
	x _ F x.
	Fy _ F polynomialsIn: #(y).
	y _ Fy x.
	f _ y^p - x.
	self assert: f isSeparable not! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testIsUnitOverZm
	| m R N U d f k |
	m _ (2 to: 100) atRandom.
	R _ É/m.
	N _ R nilradical asSet.
	U _ R units asSet.
	d _ (0 to: 5) atRandom.
	f _ R polynomials coefficients: ({U atRandom}, ((1 to: d) collect: [:i| N atRandom])).
	self assert: f isUnit.
	f _ R polynomials coefficients: ({N atRandom}, ((1 to: d) collect: [:i| N atRandom])).
	self assert: f isUnit not.
	d _ (1 to: 5) atRandom.
	k _ (1 to: d) atRandom.
	f _ R polynomials coefficients: ({U atRandom}, ((1 to: d) collect: [:i| i=k ifTrue: [U atRandom] ifFalse: [N atRandom]])).
	self assert: f isUnit not! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testQuotientRingBug
	"This is fixed by making ideals of Z[x] have generators with positive leading coefficient. But it could be still a problem for polynomials over other rings."
	| R f Q pi a b |
	R _ É polynomials.
	f _ R !! [:x| x^2*-1 - 1].
	Q _ R/f.
	pi _ Q projection.
	a _ R x - 1.
	b _ R x^2 + 1.
	self assert: (pi value: a) + (pi value: b) = (pi value: a+b)! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 8/1/2021 11:46:00'!
testResultant
	| R x f g h a b |
	R _ É polynomials.
	x _ R x.
	[(f _ R atRandomBits: 10 maxDegree: 4) isZero] whileTrue.
	[(g _ R atRandomBits: 10 maxDegree: 4) isZero] whileTrue.
	h _ R atRandomBits: 10 maxDegree: 4.
	a _ É atRandomBits: 5.
	b _ É atRandomBits: 5.
	self assert: (f * h resultant: g * h) isZero.
	self assert: (-1)^(f degree * g degree) * (f resultant: g) = (g resultant: f).
	self assert: (f * g resultant: h) = ((f resultant: h) * (g resultant: h)).
	self assert: (R !! a resultant: f) = (a ^ f degree).
	self assert: (f resultant: R !! a) = (a ^ f degree).
	self assert: (x + a resultant: x + b) = (b - a).
	! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 8/1/2021 11:36:31'!
testResultant2
	| x f g f2 g2 h |
	x _ É polynomials x.
	f _ x^2 + x + 1.
	f2 _ f * (x + 2).
	g _ x^3 + 1.
	g2 _ g * (x + 2).
	h _ x^4 + 1.
	self assert: (f resultant: g) isZero not. "not 0 because f and g have no common roots"
	self assert: (f2 resultant: g2) isZero. "res(f2,g2) = 0 because f2 and g2 have a common root -2."
	self assert: (-1)^(f degree * g degree) * (f resultant: g) = (g resultant: f).
	self assert: (f * g resultant: h) = ((f resultant: h) * (g resultant: h))! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 8/1/2021 11:36:48'!
testResultant3
	| x f g |
	x _ É polynomials x.
	f _ 2 * (x - 5) * (x - 11).
	g _ 3 * (x - 7).
	self assert: (f resultant: g) = ((2 ^ 1) * (3^2) * (5 - 7) * (11 - 7))! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 7/15/2021 12:36:36'!
testShift
	| Zx x f |
	Zx _ É polynomials.
	x _ Zx x.
	f _ Zx atRandomBits: 10.
	self assert: (f << 10) degree = (f degree + 10).
	self assert: ((f << 10) << -10) = f.
	self assert: f * (x^3) = (f << 3).
	self assert: (f * (x^3) << -3) = f! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 1/9/2022 08:39:20'!
testSquarefreePart
	| P x f1 f2 f3 f |
	P _ Ñ polynomials.
	x _ P x.
	f1 _ x^2 + x + 1.
	f2 _ 1 - x.
	f3 _ x - 2.
	f _ f1*f1*f2*f3*f3*f3.
	self assert: f squarefreePart = (f1 * f2 * f3)! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 1/9/2022 08:39:32'!
testSquarefreePartMultivariate
	| P x y f1 f2 f |
	P _ Ñ polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	f1 _ x + 1.
	f2 _ y + 1.
	f _ f1*f1*f2*f2.
	self assert: f squarefreePart = (f1 * f2)! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testSubresultant
	"deg(gcd(f,g)) = d iff subresultant(f,g,k) = 0 for all k < d and subresultant(f,g,d) ~= 0, and the d-th subresultant coefficient is in fact a gcd of f and g."
	| x f g |
	x _ É polynomials x.
	f _ (x^4 - 1)*6.
	g _ (x^6 - 1)*9.
	"self assert: (f gcd: g) = ((x^2 - 1)*3)"
	self assert: (f subresultant: 0 with: g) isZero.
	self assert: (f subresultant: 1 with: g) isZero.
	self assert: (f subresultant: 2 with: g) isZero not.
	self assert: (f subresultant: 2 with: g) primitivePart = (x^2 - 1)! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:34:55'!
testSubresultant0
	| Zx x f g |
	Zx _ É polynomials.
	x _ Zx x.
	f _ 2 * (x - 5) * (x - 11).
	g _ 3 * (x - 7).
	self assert: (f resultant: g) = (f subresultant: 0 with: g) asScalar! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 3/6/2021 15:10:54'!
testUnivariateMultivariateConversion
	| Qx x y f |
	Qx _ Ñ polynomialsIn: #(x y).
	x _ Qx x.
	y _ Qx y.
	f _ x^2*y.
	self assert: f asUnivariate asMultivariate = f.
	self assert: (f asUnivariateIn: 1 :: asMultivariate) = f.
	self assert: (f asUnivariateIn: 2 :: asMultivariate) = f! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testUnivariateMultivariateConversion2
	| Qx x y f f2 x0 y0 |
	Qx _ Ñ polynomialsIn: #(x y).
	x _ Qx x.
	y _ Qx y.
	f _ Qx zero.
	10 timesRepeat: [f _ f + (10 atRandom - 5 * (x ^ (3 atRandom - 1) * (y ^ (3 atRandom - 1))))].
	f2 _ f asUnivariate.
	self assert: f2 asMultivariate = f.
	x0 _ (-100 to: 100) atRandom.
	y0 _ (-100 to: 100) atRandom.
	self assert: ((f2 value: y0) value: x0) = (f value: {x0. y0})! !

!PolynomialTest methodsFor: 'etc' stamp: 'len 5/23/2020 09:33:01'!
testZeroCoefficientBug
	| Qx |
	Qx _ Ñ polynomials.
	self assert: (Qx one * 0) isZero! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 5/23/2020 09:34:55'!
testEvaluationHomomorphism
	| Zx f g a |
	Zx _ É polynomials.
	f _ Zx atRandomBits: 1000 maxDegree: 10.
	g _ Zx atRandomBits: 1000 maxDegree: 10.
	a _ É atRandomBits: 100.
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
	Zxyz _ É polynomialsIn: #(x y z).
	f _ Zxyz atRandomBits: 1000 maxDegree: 10.
	g _ Zxyz atRandomBits: 1000 maxDegree: 10.
	a _ É^3 atRandomBits: 100.
	self assert: (f+g value: a) = ((f value: a) + (g value: a)).
	self assert: (f-g value: a) = ((f value: a) - (g value: a)).
	self assert: (f*g value: a) = ((f value: a) * (g value: a)).
	self assert: (f^3 value: a) = ((f value: a)^3)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 10/18/2016 21:01'!
testEvaluationHomomorphismZ2
	self testEvaluationHomomorphism: (PrimeField new: 2)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 5/23/2020 09:34:55'!
testEvaluationHomomorphismZm
	self testEvaluationHomomorphism: É / (2 + 100 atRandom).
	self testEvaluationHomomorphism: É / (2^13 - 2).
	self testEvaluationHomomorphism: É / (2^31 - 2)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 12/2/2016 11:17:17'!
testEvaluationHomomorphismZp
	self testEvaluationHomomorphism: (PrimeField new: 3).
	self testEvaluationHomomorphism: (PrimeField new: 5).
	self testEvaluationHomomorphism: (PrimeField new: 2^13 - 1).
	self testEvaluationHomomorphism: (PrimeField new: 2^31 - 1)! !

!PolynomialTest methodsFor: 'evaluation' stamp: 'len 6/1/2020 13:37:52'!
testEvaluationMultivariate
	| Zxyz f a x y z |
	Zxyz _ É polynomialsIn: #(x y z).
	x _ Zxyz x. y _ Zxyz y. z _ Zxyz z.
	f _ x^2 * y - z.
	a _ É^3 atRandomBits: 100.
	self assert: (f value: a) = ((a at: 1)^2 * (a at: 2) - (a at: 3))! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/5/2021 08:40:59'!
testFiniteFieldFactorization
	| F Fx x one f factors |
	F _ É / 3.
	Fx _ F polynomials.
	x _ Fx x.
	one _ Fx one.
	f _ x^11 + (x^9 * 2) + (x^8 * 2) + (x^6) + (x^5) + (x^3 * 2) + (x^2 * 2) + one.
	factors _ f factorization.
	self assert: factors size = 8.
	self assert: (factors occurrencesOf: x + (one*2)) = 4.
	self assert: (factors occurrencesOf: x^2 + one) = 3.
	self assert: (factors occurrencesOf: x + one) = 1.
	self assert: (factors inject: Fx one into: [:a :b| a*b]) = f! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/5/2021 08:41:02'!
testFiniteFieldFactorization2
	| Z3 x f factors |
	Z3 _ É / 3.
	x _ Z3 polynomials x.
	f _ (x-1)^2*(x-2).
	factors _ f factorization.
	self assert: factors size = 3.
	self assert: (factors occurrencesOf: x-1) = 2.
	self assert: (factors occurrencesOf: x-2) = 1! !

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

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/5/2021 08:41:07'!
testFiniteFieldFactorization4
	| R x f factors |
	R _ É / 2.
	x _ R polynomials x.
	f _ x^24 - 1.
	factors _ f factorization.
	self assert: factors size = 16.
	self assert: (factors occurrencesOf: x+1) = 8.
	self assert: (factors occurrencesOf: x^2+x+1) = 8! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/5/2021 08:41:09'!
testFiniteFieldFactorization5
	| R x |
	R _ É / 2.
	x _ R polynomials x.
	self assert: (x^15 - 1) factorization = {x + 1. x^2 + x + 1. x^4 + (x^3) + (x^2) + x + 1. x^4 + x + 1. x^4 + (x^3) + 1} asBag! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/5/2021 08:41:26'!
testFiniteFieldIrreduciblePolynomials
	"Z/<3>[x] has 8 irreducible monic polynomials of degree 3, out of 27 monic polynomials of degree 3."
	| F Fx f count |
	F _ É / 3.
	Fx _ F polynomials.
	count _ 0.
	F^3 do: [:each|
		f _ (Fx coefficients: each, F one).
		f isIrreducible
			ifTrue:
				[count _ count + 1.
				self assert: f factorization size = 1]
			ifFalse:
				[self assert: f factorization size > 1]].
	self assert: count = 8! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/5/2021 08:41:31'!
testFiniteFieldIsIrreducible
	| F Fx x f |
	F _ É / 3.
	Fx _ F polynomials.
	x _ Fx x.
	f _ x^3 + (x*2) + 1.
	self assert: f factorization size = 1.
	self assert: f isIrreducible! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 1/9/2022 08:35:52'!
testFiniteFieldSFF
	| F Fx x one f factors |
	F _ É / 3.
	Fx _ F polynomials.
	x _ Fx x.
	one _ Fx one.
	f _ x^11 + (x^9 * 2) + (x^8 * 2) + (x^6) + (x^5) + (x^3 * 2) + (x^2 * 2) + one.
	factors _ f squarefreeFactorization.
	self assert: factors size = 8.
	self assert: (factors occurrencesOf: x + (one*2)) = 4.
	self assert: (factors occurrencesOf: x^2 + one) = 3.
	self assert: (factors occurrencesOf: x + one) = 1! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/5/2021 08:41:33'!
testIntegerFactorization
	| x f factors |
	x _ É polynomials x.
	f _ (x*2-1)^2*(x-2)*(x^2 + 1)*(x^2-2)*18.
	factors _ f factorization.
	self assert: factors size = 8.
	self assert: factors asSet size = 6.
	self assert: factors product = f! !

!PolynomialTest methodsFor: 'factorization' stamp: 'len 8/10/2021 17:52:56'!
testRationalFactorization
	| x f factors |
	x _ Ñ polynomials x.
	f _ (x-(1/2))^2*(x-2)*(x^2 + 1)*(x^2-2).
	factors _ f factorization.
	self assert: factors size = 5.
	self assert: factors asSet size = 4.
	self assert: factors product normalized = f normalized "up to a unit"! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:34:55'!
testContentAndPrimitive
	| Zx Zxy x y f |
	Zx _ É polynomialsIn: #(x).
	Zxy _ Zx polynomialsIn: #(y).
	x _ Zx x.
	y _ Zxy x.
	f _ 2*x*y + (2*(x^2)).
	self assert: f primitivePart * f content = f! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:34:55'!
testContentAndPrimitiveAtRandom
	| Zx f |
	Zx _ É polynomials.
	f _ Zx atRandomBits: 100 maxDegree: (1 to: 10) atRandom.
	self assert: f primitivePart * f content = f! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testExtendedEuclidBug
	| x f1 f2 |
	x _ Ñ polynomials x.
	f1 _ x^2 + 1.
	f2 _ x - 1.
	self assert: (f1 xgcd: f2) first = (f1 gcd: f2)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testGCD
	| P x f g |
	P _ Ñ polynomials.
	x _ P x.
	f _ x^4 - 1.
	g _ x^6 - 1.
	self assert: (f gcd: g) = (x^2 - 1)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:34:55'!
testGCDMultivariate
	| Zxy x y f g |
	Zxy _ É polynomialsIn: #(x y) ordering: #lex.
	x _ Zxy x.
	y _ Zxy y.
	f _ (x+y)*(y-x).
	g _ (y-x)^2.
	self assert: (f gcd: g) = (x-y)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testGCDMultivariate2
	"Example from Sage documentation."
	| R x y f g |
	R _ Ñ polynomialsIn: #(x y).
	x _ R x. y _ R y.
	f _ x^2*y*(x+y)*3.
	g _ x*(y^2 - (x^2))*9.
	self assert: (f gcd: g) = (x^2 + (x*y))! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testGCDMultivariateOverQ
	| R x y f g |
	R _ Ñ polynomialsIn: #(x y).
	x _ R x. y _ R y.
	f _ x^2*y*6.
	g _ x*(y^2)*15 + (x^3*(y^2)*21).
	self assert: (f gcd: g) = (x*y)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:34:55'!
testGCDMultivariateOverZ
	| R x y f g |
	R _ É polynomialsIn: #(x y).
	x _ R x. y _ R y.
	f _ x^2*y*6.
	g _ x*(y^2)*15 + (x^3*(y^2)*21).
	self assert: (f gcd: g) = (x*y*3)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testGCDOverRationalFunctions
	| R x y f g |
	R _ Ñ polynomialsIn: #(y) :: fractions polynomials.
	x _ R x. y _ R scalars x.
	f _ x^2*y*6.
	g _ x*(y^2)*15 + (x^3*(y^2)*21).
	self assert: (f gcd: g) = x! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:34:55'!
testGCDOverZ
	| x f g |
	x _ É polynomials x.
	f _ (x^4 - 1)*6.
	g _ (x^6 - 1)*9.
	self assert: (f gcd: g) = ((x^2 - 1)*3)! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:34:55'!
testGCDOverZ2
	| x f g h |
	x _ É/2 :: polynomials x.
	h _ x^3 - x + 1.
	f _ h * (x + 1) * x.
	g _ h * (x + 1) * (x^2 + 1).
	self assert: (f gcd: g) = (h * (x + 1))! !

!PolynomialTest methodsFor: 'gcd' stamp: 'len 5/23/2020 09:33:01'!
testMultivariateGCDBug
	| x |
	x _ Ñ polynomialsIn: 3 :: x.
	self assert: (x^2 gcd: x) = x! !

!PolynomialTest methodsFor: 'remainder sequences' stamp: 'len 5/23/2020 09:34:55'!
testEPRS
	"Example from Zippel."
	| a b eprs |
	a _ É polynomials coefficients: #(1 0 1 0 -3 -3 8 2 -5) reversed.
	b _ É polynomials coefficients: #(3 0 5 0 -4 -9 21) reversed.
	eprs _ a eprs: b :: collect: [:each| each coefficients reversed asArray] :: asArray.
	self assert: eprs = #((1 0 1 0 -3 -3 8 2 -5)
		(3 0 5 0 -4 -9 21)
		(-15 0 3 0 -9) 
		(15795 30375 -59535) 
		(1254542875143750 -1654608338437500) (12593338795500743100931141992187500))! !

!PolynomialTest methodsFor: 'remainder sequences' stamp: 'len 5/23/2020 09:34:55'!
testPPRS
	"Example from Zippel."
	| a b pprs |
	a _ É polynomials coefficients: #(1 0 1 0 -3 -3 8 2 -5) reversed.
	b _ É polynomials coefficients: #(3 0 5 0 -4 -9 21) reversed.
	pprs _ a pprs: b :: collect: [:each| each coefficients reversed asArray] :: asArray.
	self assert: pprs = #((1 0 1 0 -3 -3 8 2 -5) 
		(3 0 5 0 -4 -9 21)
		(-5 0 1 0 -3)
		(13 25 -49)
		(4663 -6150)
		(1))! !

!PolynomialTest methodsFor: 'remainder sequences' stamp: 'len 5/23/2020 09:34:55'!
testSPRS
	"Example from Zippel."
	| a b sprs |
	a _ É polynomials coefficients: #(1 0 1 0 -3 -3 8 2 -5) reversed.
	b _ É polynomials coefficients: #(3 0 5 0 -4 -9 21) reversed.
	sprs _ a sprs: b :: collect: [:each| each coefficients reversed asArray] :: asArray.
	self assert: sprs = #((1 0 1 0 -3 -3 8 2 -5)
		#(3 0 5 0 -4 -9 21)
		#(15 0 -3 0 9)
		#(65 125 -245)
		#(9326 -12300)
		#(260708))
! !

!PolynomialTest methodsFor: 'roots' stamp: 'len 7/15/2021 16:12:29'!
testCyclotomicFieldRoots
	| K f roots x |
	K _ CyclotomicField new: (3 to: 5) atRandom.
	roots _ (1 to: 3 atRandom) collect: [:i| K atRandomBits: 10].
	x _ K polynomials x.
	f _ roots product: [:à| x - à].
	self assert: f allRoots = roots asBag! !

!PolynomialTest methodsFor: 'roots' stamp: 'len 5/23/2020 09:34:55'!
testFiniteFieldRoots
	| Z3 x f roots |
	Z3 _ É / 3.
	x _ Z3 polynomials x.
	f _ (x-1)^2*(x-2).
	roots _ f allRoots.
	self assert: roots size = 3.
	self assert: (roots occurrencesOf: (Z3 project: 1)) = 2.
	self assert: (roots occurrencesOf: (Z3 project: 2)) = 1! !

!PolynomialTest methodsFor: 'roots' stamp: 'len 5/23/2020 09:33:01'!
testRationalRoots
	| x f roots |
	x _ Ñ polynomials x.
	f _ (x-(1/2))^2*(x-2)*(x^2 + 1)*(x^2-2).
	roots _ f allRoots.
	self assert: roots size = 3.
	self assert: (roots occurrencesOf: 1/2) = 2.
	self assert: (roots occurrencesOf: 2) = 1! !

!PolynomialTest methodsFor: 'roots' stamp: 'len 5/23/2020 09:33:01'!
testRationalRootsBug
	| x f roots |
	x _ Ñ polynomials x.
	f _ x^3*-1 + (3*x) + 2.
	roots _ f allRoots.
	self assert: roots size = 3.
	self assert: (roots occurrencesOf: -1) = 2.
	self assert: (roots occurrencesOf: 2) = 1! !

!PolynomialTest methodsFor: 'roots' stamp: 'len 5/23/2020 09:33:01'!
testRealRoots
	| x f roots |
	x _ Ñ polynomials x.
	f _ (x-(1/2))^2*(x-2)*(x^2 + 1)*(x^2-2).
	roots _ f allRootsIn: Ö.
	self assert: roots size = 5.
	self assert: roots asSet size = 4.
	self assert: (roots occurrencesOf: 1/2) = 2.
	self assert: (roots occurrencesOf: 2) = 1! !

!PowerSeriesTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
cos
	"The power series expansion of cos(x)."
	^ Ñ powerSeries coefficients:
		(Sequence to: Ñ evaluating: [:n| n even ifTrue: [0] ifFalse: [|k| k _ n-1/2. (-1)^k / (2*k) factorial]])! !

!PowerSeriesTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
exp
	"The power series expansion of exp(x)."
	^ Ñ powerSeries
		coefficients: (Sequence to: Ñ evaluating: [:n| 1 / n factorial])! !

!PowerSeriesTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
sin
	"The power series expansion of sin(x)."
	^ Ñ powerSeries
		coefficients: (Sequence to: Ñ evaluating: [:n| n odd ifTrue: [0] ifFalse: [|k| k _ n/2-1. (-1)^k / (2*k+1) factorial]])! !

!PowerSeriesTest methodsFor: 'as yet unclassified' stamp: 'len 4/14/2020 18:10:55'!
testAddition
	| a b c |
	a _ self cos.
	b _ self sin.
	c _ a+b.
	self assert: c-a = b.
	self assert: c-b = a.
	self assert: (c + c negated) isZero.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 20)! !

!PowerSeriesTest methodsFor: 'as yet unclassified' stamp: 'len 4/14/2020 18:14:48'!
testAdditiveInverse
	| a zero |
	a _ self cos.
	self assert: a negated negated = a.
	self assert: (a + a negated) isZero.
	zero _ a - a.
	self assert: zero isZero.
	self assert: zero negated = zero! !

!PowerSeriesTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testLocalProperties
	| R |
	R _ Ñ powerSeries.
	self assert: R isLocal.
	self assert: R maximalIdeal = (R * R x)! !

!PowerSeriesTest methodsFor: 'as yet unclassified' stamp: 'len 5/3/2020 10:09:34'!
testMultiplicativeInverse
	| a one |
	a _ self cos.
	one _ a parent one.
	self assert: a * a inverse = one.
	self assert: one inverse = one! !

!PowerSeriesTest methodsFor: 'as yet unclassified' stamp: 'len 4/14/2020 18:11:21'!
testSinSquaredPlusCosSquared
	| cos sin |
	sin _ self sin.
	cos _ self cos.
	self assert: cos squared + sin squared = cos parent one! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 3/28/2018 15:13:47'!
newQuadraticForm
	^ self newQuadraticForm: (2 to: 5) atRandom! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
newQuadraticForm: n
	| V |
	V _ Ñ^n.
	^ QuadraticForm from: V coefficients: (Ñ^(n + 1 * n / 2) atRandomBits: 100)! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testArithmetic
	| n Q1 Q2 p a |
	n _ (1 to: 5) atRandom.
	Q1 _ self newQuadraticForm: n.
	Q2 _ self newQuadraticForm: n.
	p _ Q1 domain atRandomBits: 100.
	a _ Ñ atRandomBits: 100.
	self assert: (Q1 value: p) + (Q2 value: p) = (Q1 + Q2 value: p).
	self assert: (Q1 * a value: p) = ((Q1 value: p) * a).
	self assert: Q1 + Q2 + Q2 negated = Q1! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 3/27/2018 23:52:45'!
testAsPolynomial
	| Q f p |
	Q _ self newQuadraticForm.
	f _ Q asPolynomial.
	p _ Q domain atRandomBits: 100.
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

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/6/2020 11:08:06'!
testDiscriminant
	"Test the discriminant of a binary quadratic form matches the definition in Magma."
	| a b c f Q |
	a _ (-10 to: 10) atRandom.
	b _ (-10 to: 10) atRandom.
	c _ (-10 to: 10) atRandom.
	f _ (Ñ polynomialsIn: #(x y)) generate: [:x :y| x^2*a + (x*y*b) + (y^2*c)].
	Q _ f asQuadraticForm.
	self assert: Q discriminant = (b^2 - (4*a*c))! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testEvaluation
	"Example from Sage."
	| Q |
	Q _ QuadraticForm from: Ñ^3 coefficients: (0 to: 5).
	self assert: (Q value: (1,2,3)) = 89.
	self assert: (Q value: (1,0,0)) = 0.
	self assert: (Q value: (1,1,1)) = 15! !

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2020 12:00:03'!
testExternalOrthogonalSum
	| Q1 Q2 S v1 v2 embeddings w |
	Q1 _ self newQuadraticForm.
	Q2 _ self newQuadraticForm.
	S _ Q1 ê Q2.
	v1 _ Q1 domain atRandomBits: 100.
	v2 _ Q2 domain atRandomBits: 100.
	embeddings _ (Q1 domain, Q2 domain) >- S domain.
	w _ (embeddings first value: v1) + (embeddings second value: v2).
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

!QuadraticFormTest methodsFor: 'as yet unclassified' stamp: 'len 7/6/2020 06:31:50'!
testGramian
	| Q M x |
	Q _ QuadraticForm from: Ñ^3 coefficients: (0 to: 5).
	M _ Q gramian.
	10 timesRepeat:
		[x _ Q domain atRandomBits: 100 atRandom.
		self assert: (Q value: x) = (x ∑ (M * x))]! !

!RingTest methodsFor: 'euclidean' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersEuclideanExample
	"Some random example built in Magma online calculator."
	| R a b c |
	R _ É/12.
	a _ R !! 2.
	b _ R !! 9.
	c _ a lcm: b.
	self assert: c = (R !! 6).
	self assert: c // a = (R !! 3).
	self assert: c // b = (R !! 10)! !

!RingTest methodsFor: 'euclidean' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersEuclideanExample2
	"Some random example built in Magma online calculator."
	| R a b |
	R _ É/39.
	a _ R !! 36.
	b _ R !! 3.
	self assert: a // b = (R !! 12)! !

!RingTest methodsFor: 'euclidean' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersEuclideanExample3
	"Some random example built in Magma online calculator."
	| R a b |
	R _ É/78.
	a _ R !! 9.
	b _ R !! 39.
	self assert: a // b = (R !! 0)! !

!RingTest methodsFor: 'euclidean' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersEuclideanExample4
	"Some random example built in Magma online calculator."
	| R a b |
	R _ É/230.
	a _ R !! 205.
	b _ R !! 128.
	self assert: (a gcd: b) = (R !! 1).
	self assert: (a lcm: b) = (R !! 10).! !

!RingTest methodsFor: 'euclidean' stamp: 'len 7/30/2021 10:48:51'!
testModularIntegersEuclideanExample5
	"Some random example built in Magma online calculator."
	| R a b |
	R _ É/10.
	a _ R !! 4.
	b _ R !! 8.
	self assert: b normalized = (R !! 2).
	self assert: b normalization inverse = (R !! 9).
	self assert: (a gcd: b) = (R !! 2).
	self assert: (a lcm: b) = (R !! 2)! !

!RingTest methodsFor: 'euclidean' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersEuclideanExample6
	"Some random example built in Magma online calculator."
	| R a b |
	R _ É/12.
	a _ R !! 4.
	b _ R !! 8.
	self assert: a // b = (R !! 11).
	self assert: b // a = (R !! 2)! !

!RingTest methodsFor: 'euclidean' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersEuclideanExample7
	"Some random example built in Magma online calculator."
	| R a b |
	R _ É/6.
	a _ R !! 4.
	b _ R !! 3.
	self assert: (a gcd: b) = (R !! 1).
	self assert: (a lcm: b) = (R !! 0)! !

!RingTest methodsFor: 'euclidean' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersEuclideanExample8
	"Some random example built in Magma online calculator."
	| R a b |
	R _ É/10.
	a _ R !! 6.
	b _ R !! 4.
	self assert: a // b = (R !! 4).
	self assert: b // a = (R !! 4)! !

!RingTest methodsFor: 'euclidean' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersEuclideanExample9
	"Some random example built in Magma online calculator."
	| R a b |
	R _ É/10.
	a _ R !! 2.
	b _ R !! 4.
	self assert: a // b = (R !! 8).
	self assert: b // a = (R !! 2)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 1/6/2022 09:23:48'!
testFiniteRing1
	| R |
	R _ (É/2) polynomials.
	R _ R / (R x^3 - R x).
	self testRing: R.
	self testSmallRingUnitsAndAssociates: R.
	self assert: R isField not.
	self assert: R size = 8.
	self assert: R units size = 2.
	self assert: R zerodivisors size = 6.
	self assert: R radical asSet = {R zero. R x^2 + R x} asSet
	"also, this ring has two maximal ideals"! !

!RingTest methodsFor: 'finite rings' stamp: 'len 1/6/2022 09:23:53'!
testFiniteRing2
	| R |
	R _ (É/2) polynomials.
	R _ R / (R x * (R x + 1)).
	self testRing: R.
	self testSmallRingUnitsAndAssociates: R.
	self assert: R isField not.
	self assert: R size = 4.
	self assert: R zerodivisors size = 3.
	self assert: R characteristic = 2.
	self assert: R isLocal not.
	"more about this ring: two maximal (and princial as well) ideals (so not a local ring)"! !

!RingTest methodsFor: 'finite rings' stamp: 'len 1/6/2022 09:23:58'!
testFiniteRing2Alternative
	| R |
	R _ É/2 ◊ (É/2).
	self testRing: R.
	self testSmallRingUnitsAndAssociates: R.
	self assert: R isField not.
	self assert: R size = 4.
	self assert: R zerodivisors size = 3.
	self assert: R characteristic = 2.
	self assert: R isLocal not.
	"more about this ring: two maximal (and principal as well) ideals (so not a local ring)"! !

!RingTest methodsFor: 'finite rings' stamp: 'len 1/6/2022 09:24:02'!
testFiniteRing3
	| R |
	R _ É/4 ◊ (É/4).
	self testRing: R.
	self testSmallRingUnitsAndAssociates: R.
	self assert: R characteristic = 4.
	self assert: R isField not.
	self assert: R size = 16.
	self assert: R zerodivisors size = 12.
	self assert: R units size = 4.
	self assert: R radical isTrivial not.
	"more about this ring: two maximal ideals (so not a local ring), and non-trivial jacobson radical"! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testFiniteRingJacobsonRadical
	| R |
	R _ É/12.
	self assert: R radical = (R*6)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testFiniteRingLocalAndNotLocal
	| R S |
	R _ É/6. "this is not a Galois ring, the zero-divisors dont form an ideal"
	S _ É/8. "this is a Galois ring"
	self assert: R isLocal not.
	self assert: S isLocal! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testFiniteRingsAreArtinian
	| R |
	R _ É/6. "not local"
	self assert: R isArtinian.
	R _ É/8. "local, prime power ring, Galois ring"
	self assert: R isArtinian.
	R _ É/4 ◊ (É/4).
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

!RingTest methodsFor: 'finite rings' stamp: 'len 4/19/2019 04:55:59'!
testGaloisRingCanonicalEmbeddingIntoOverring
	"Canonical homomorphism from GR(p^n,m) to GR(p^n,m*k)."
	| p n m k A B phi |
	p _ #(2 3 5) atRandom.
	n _ 3 atRandom.
	m _ 3 atRandom.
	k _ 3 atRandom.
	A _ GaloisRing new: p to: n to: m.
	B _ GaloisRing new: p to: n to: m * k.
	phi _ A -> B.
	self testRingHomomorphism: phi! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/15/2019 07:04:02'!
testGaloisRingCanonicalProjectionOntoResidueField
	"Canonical homomorphism from GR(p^n,m) to GF(p,m).
	This sends 'a' in Z/(p^n)Z[x]/f to 'a mod p' in Z/pZ[x]/(f mod p) where f mod p means reducing the coefficients mod p."
	| p n m A B phi |
	p _ #(2 3 5) atRandom.
	n _ 3 atRandom.
	m _ 3 atRandom.
	A _ GaloisRing new: p to: n to: m.
	B _ GaloisField new: p to: m.
	phi _ A -> B.
	self testRingHomomorphism: phi! !

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
	self assert: (É/1) isTrivial! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegers
	| R |
	R _ É / 256 atRandom.
	self testRing: R.
	self testSmallRingUnitsAndAssociates: R.
	self testRingHomomorphism: R projection! !

!RingTest methodsFor: 'finite rings' stamp: 'len 7/15/2021 10:40:39'!
testModularIntegersAnnihilator
	| R |
	R _ É/64 atRandom.
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
testModularIntegersPrimesExamples
	| R |
	R _ É/6.
	self assert: (R primes asSet collect: [:each| each representative]) = #(2 3 4) asSet.
	R _ É/12.
	self assert: (R primes asSet collect: [:each| each representative]) = #(2 3 9 10) asSet! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersPrimitiveRoots
	| X |
	"number of primitive roots in Z/nZ for n=2 to 17:"
	X _ #(1 1 1 2 1 2 0 2 2 4 0 4 2 0 0 8).
	self assert: ((2 to: 17) collect: [:m| (É/m) count: [:x| x isPrimitiveRoot]]) = X.
	X withIndexDo: [:n :i|
		n > 0
			ifTrue: [self assert: (É/(i+1)) primitiveRoot isPrimitiveRoot]
			ifFalse: [self should: [(É/(i+1)) primitiveRoot] raise: Error]]! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersProperties
	| m R |
	m _ (2 to: 10) atRandom.
	R _ É/m.
	self assert: R isPIR.
	self assert: m isPrime = R isField.
	self assert: m isPrimePower = R isLocal.
	self assert: R isField = R isIntegralDomain.
	self assert: R isField = R isPID! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersTrivial
	| R |
	R _ É/1.
	self assert: R isTrivial.
	self assert: R size = 1.
	self assert: R zero = R one! !

!RingTest methodsFor: 'finite rings' stamp: 'len 8/5/2021 08:42:57'!
testModularIntegersUnitsIrreduciblesAndReducibles
	"From 'A Special Subring Associated with Irreducible Elements in the Ring of Z/nZ', Augustine Musukwa, Khumbo Kumwenda, Journal of Mathematics (2017)."
	| m R factors p |
	m _ (2 to: 100) atRandom.
	R _ É/m.
	self assert: R units asSet size = m phi.
	factors _ m factorization.
	self assert: R irreducibles size = (m phi * ((factors asSet select: [:each| (factors occurrencesOf: each) > 1]) sum: [:each| 1/each] ifEmpty: [0])).
	p _ 10 atRandom nextPrime.
	m _ p ^ (2 to: 4) atRandom.
	R _ É/m.
	self assert: R reducibles size = (R irreducibles size / (p - 1) - 1)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersUnitsIrreduciblesAndReduciblesExamples
	"Examples from 'A Special Subring Associated with Irreducible Elements in the Ring of Z/nZ', Augustine Musukwa, Khumbo Kumwenda, Journal of Mathematics (2017)."
	| R |
	R _ É/8.
	self assert: (R units asSet collect: [:each| each representative]) = #(1 3 5 7) asSet.
	self assert: (R irreducibles asSet collect: [:each| each representative]) = #(2 6) asSet.
	R _ É/36.
	self assert: (R units asSet collect: [:each| each representative]) = #(1 5 7 11 13 17 19 23 25 29 31 35) asSet.
	self assert: (R irreducibles asSet collect: [:each| each representative]) = #(2 3 10 14 15 21 22 26 33 34) asSet.
	R _ É/25.
	self assert: R irreducibles size = 4.
	self assert: R reducibles isEmpty.
	R _ É/32.
	self assert: R irreducibles size = 8.
	self assert: R reducibles size = 7.
	R _ É/343.
	self assert: R irreducibles size = 42.
	self assert: R reducibles size = 6! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testPrimeField
	| p F |
	p _ 256 atRandom nextPrime.
	F _ É / p.
	self assert: F isField.
	self assert: F characteristic = p.
	self testRing: F.
	self testRingHomomorphism: F projection.
	self testSmallRingUnitsAndAssociates: F! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testPrimeFieldEulerTheorem
	| p F a |
	p _ 256 atRandom nextPrime.
	F _ É / p.
	[a _ F atRandom. a isZero] whileTrue.
	self assert: a ^ (p - 1) = F one! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testPrimeFieldLogarithm
	| F g a b p |
	p _ 17.
	F _ É/p.
	g _ F primitiveRoot.
	[(a _ F atRandom) isZero] whileTrue.
	[(b _ F atRandom) isZero] whileTrue.
	self assert: g ^ (a log: g) = a.
	self assert: (a log: g) + (b log: g) \\ (p-1) = ((a*b log: g) \\ (p-1)).
	self assert: (a log: g) - (b log: g) \\ (p-1) = ((a/b log: g) \\ (p-1))! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testPrimeFieldPrimitiveRoot
	| p F g i x |
	p _ 100 atRandom nextPrime.
	F _ É/p.
	g _ F primitiveRoot.
	self assert: g multiplicativeOrder = (p-1).
	i _ 1.
	x _ g.
	[x = F one] whileFalse: [x _ x*g. i _ i+1].
	self assert: i = (p-1)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:26:33'!
testPrimeFieldQuadraticResidues
	| F |
	F _ PrimeField new: 1000 atRandom nextPrime.
	self assert: F atRandom squared isQuadraticResidue.
	self assert: ((1000 count: [F atRandom isQuadraticResidue]) between: 400 and: 600)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:28:46'!
testPrimeFieldSquareRoot
	| F a x |
	100 timesRepeat:
		[F _ PrimeField new: (1000 atRandom + 1) nextPrime.
		a _ F atRandom squared.
		x _ a squareRoot.
		self assert: x squared = a]! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testPrimeFieldUnitsSize
	| F |
	F _ É/256 atRandom nextPrime.
	self assert: F units size = (F size - 1)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:15:40'!
testPrimePowerRing
	| R |
	R _ PrimePowerRing new: 3 to: 3.
	self testRing: R.
	self testSmallRingUnitsAndAssociates: R.
	self assert: R characteristic = (3^3).
	self assert: R size = (3^3).
	self assert: R isLocal.
	self assert: R isEuclidean.
	self assert: R maximalIdeal = (R*3)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testPrimePowerRingCanonicalProjections
	"Canonical homomorphism from É/<p^n> to É/<p^m> with 1 <= m <= n."
	| p n m A B phi a b |
	p _ 100 atRandom nextPrime.
	n _ 4 atRandom.
	A _ É/(p^n).
	m _ n atRandom.
	B _ É/(p^m).
	phi _ A -> B.
	a _ A atRandom.
	b _ phi value: a.
	self assert: (B includes: b).
	self assert: a representative \\ (p^m) = b representative.
	self testRingHomomorphism: phi! !

!RingTest methodsFor: 'finite rings' stamp: 'len 6/5/2019 14:25:29'!
testPrimePowerRingNilradical
	| R |
	R _ PrimePowerRing new: 3 to: 3.
	self assert: R nilradical = (R * 3)! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testPrimePowerRingNilradicalAndUnits
	| p n R |
	p _ 30 atRandom nextPrime.
	n _ 4 atRandom.
	R _ É/(p^n).
	self assert: R units size + R nilradical size = R size! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:29:15'!
testPrimePowerRingSquareRoot
	| F a x |
	100 timesRepeat:
		[F _ PrimePowerRing new: 10 atRandom nextPrime to: 3 atRandom.
		a _ F atRandom squared.
		x _ a squareRoot.
		self assert: x squared = a]! !

!RingTest methodsFor: 'finite rings' stamp: 'len 7/30/2021 10:49:15'!
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

!RingTest methodsFor: 'finite rings' stamp: 'len 5/10/2020 19:58:36'!
testQuasiGaloisRing1
	| p r n A |
	p _ #(2 3) atRandom.
	r _ 3 atRandom.
	n _ 3 atRandom+1.
	A _ QuasiGaloisRing new: p to: r to: n.
	self testRing: A.
	self testRingUnitsAndAssociates: A.
	self assert: A characteristic = p.
	self assert: A size = (p^(r*n)).
	self assert: A isLocal.
	self assert: A isFinite.
	self assert: A isField not! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/9/2020 17:28:07'!
testSquareRootModulo2
	| F |
	F _ PrimeField new: 2.
	self assert: F zero squareRoot = F zero.
	self assert: F one squareRoot = F one! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testZ6IsNotPID
	self assert: (É/6) isPID not! !

!RingTest methodsFor: 'finite rings' stamp: 'len 5/23/2020 09:34:55'!
testZ6PrimeNeedNotBeIrreducible
	| R a |
	R _ É/6.
	a _ R!!2.
	self assert: a isPrime.
	self assert: a isIrreducible not! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:34:55'!
testCanonicalMorphisms
	| R x Q |
	R _ Ñ polynomials.
	x _ R x.
	Q _ R / (x^2 + 1).
	self assert: (É -> R value: 4) = (R !! 4). "canonical morphism from initial object"
	self assert: (R -> Q value: x^2 + 2) = Q one. "canonical morphism to quotient object"! !

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
	self assert: É dimension = 1.
	self assert: Ñ dimension = 0.
	self assert: (É polynomialsIn: 2) dimension = 3.
	R _ Ñ polynomialsIn: #(x y).
	I _ R * {R x ^ 2 + (R y ^ 2) - 1}.
	self assert: (R / I) dimension = 1! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:34:55'!
testNilradical
	self assert: É nilradical = (É*0).
	self assert: (É/12) nilradical = (É/12*6)! !

!RingTest methodsFor: 'general' stamp: 'len 9/22/2018 19:03:04'!
testProductRing
	| R S P |
	R _ self newRing.
	S _ self newRing.
	P _ R ◊ S.
	self testRing: P.
	self testRingHomomorphism: (P projection: 2 atRandom)! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:33:27'!
testQQ
	self testRing: Ñ! !

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

!RingTest methodsFor: 'general' stamp: 'len 3/13/2018 20:15:02'!
testRingHomomorphism: phi
	| R S a b |
	R _ phi domain.
	S _ phi codomain.
	a _ R atRandomBits: 10.
	b _ R atRandomBits: 10.
	self assert: (phi value: R zero) = S zero.
	self assert: (phi value: R one) = S one.
	self assert: (phi value: a + b) = ((phi value: a) + (phi value: b)).
	self assert: (phi value: a * b) = ((phi value: a) * (phi value: b)) "this sometimes fails because f and -f are not equal regular functions, for example"! !

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
	self testRing: É! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:35:28'!
testZZIdeals
	self assert: (É * 3 includes: 6).
	self deny: (É * 3 includes: 7).
	self assert: (É * 0) isTrivial.
	self assert: (É * 1) isOne! !

!RingTest methodsFor: 'general' stamp: 'len 6/7/2020 13:42:24'!
testZZPrimeAndMaximalIdeal
	| I |
	I _ É * 64 atRandom nextPrime.
	self assert: I isPrime.
	self assert: I isMaximal.
	I _ É * 6.
	self assert: I isPrime not.
	self assert: I isMaximal not.
	I _ É * 1.
	self assert: I isPrime not.
	self assert: I isMaximal not! !

!RingTest methodsFor: 'general' stamp: 'len 5/23/2020 09:35:33'!
testZZPrimeNotMaximalIdeal
	| I |
	I _ É * 0.
	self assert: I isPrime.
	self assert: I isMaximal not! !

!RingTest methodsFor: 'local rings' stamp: 'len 6/26/2019 13:00:09'!
testDVR: V
	| a b p |
	self assert: V isLocal.
	self assert: V isIntegralDomain.
	self assert: V isEuclidean.
	self assert: V one valuation = 0.
	p _ V uniformizer.
	self assert: p valuation = 1.
	a _ V atRandomBits: 100.
	self assert: (a*p) valuation = (a valuation + 1).
	b _ V atRandomBits: 100.
	self assert: (a*b) valuation = (a valuation + b valuation)! !

!RingTest methodsFor: 'local rings' stamp: 'len 5/23/2020 09:34:55'!
testLocalHomomorphism
	| f |
	f _ É/4 to: É/2 evaluating: [:x| É/2 !! x representative].
	self assert: f isLocal
! !

!RingTest methodsFor: 'local rings' stamp: 'len 12/22/2021 14:20:33'!
testValuationRing1
	| V |
	V _ É @ 5.
	self testRing: V.
	self testRingHomomorphism: V -> Ñ! !

!RingTest methodsFor: 'local rings' stamp: 'len 12/22/2021 14:20:38'!
testValuationRing2
	| V |
	V _ É @ 5.
	self testDVR: V! !

!RingTest methodsFor: 'localizations' stamp: 'len 12/22/2021 14:19:40'!
testLocalizationInclusions
	| Z2 Z3 Z6 |
	Z2 _ É @ 2.
	Z3 _ É @ 3.
	Z6 _ É @ 6. "<- this is not a prime ideal, it shouldn't be allowed"
	self assert: Z6 >= Z2.
	self assert: Z6 >= Z3.
	self deny: Z3 >= Z2.
	self deny: Z2 >= Z3.
	self deny: Z2 >= Z6! !

!RingTest methodsFor: 'localizations' stamp: 'len 12/22/2021 14:19:49'!
testLocalizationMaximalIdeal
	| Z2 |
	Z2 _ É @ 2.
	self assert: Z2 maximalIdeal = (Z2 * (Z2 !! 2))! !

!RingTest methodsFor: 'localizations' stamp: 'len 12/22/2021 14:20:01'!
testLocalizationOfModularIntegers1
	| R |
	R _ É/60.
	self assert: (R @ (R*2)) = (É/4).
	self assert: (R @ (R*3)) = (É/3).
	self assert: (R @ (R*5)) = (É/5)! !

!RingTest methodsFor: 'localizations' stamp: 'len 12/22/2021 14:20:08'!
testLocalizationOfModularIntegers2
	| R P L phi |
	R _ É/60.
	P _ R*2.
	L _ R @ P. "the localization is actually the prime power ring Z/4Z"
	"localization map:"
	phi _ R -> L.
	self testRingHomomorphism: phi.
	"everything not in P maps to a unit:"
	R do: [:each| self assert: ((P includes: each) or: [(phi value: each) isUnit])]! !

!RingTest methodsFor: 'localizations' stamp: 'len 12/22/2021 14:20:18'!
testLocalizationOfModularIntegers3
	| R P L phi |
	R _ É/60.
	P _ R*3.
	L _ R @ P. "the localization is actually the field GF(3)"
	"localization map:"
	phi _ R -> L.
	self testRingHomomorphism: phi.
	"everything not in P maps to a unit:"
	R do: [:each| self assert: ((P includes: each) or: [(phi value: each) isUnit])]! !

!RingTest methodsFor: 'localizations' stamp: 'len 12/22/2021 14:20:24'!
testLocalizationOfZAt2
	| L |
	L _ É @ 2.
	self assert: Ñ !! (L !! (1/3)) = (1/3).
	self should: [L !! (3/2)] raise: Error.
	self assert: Ñ !! (L !! 2) = 2.
	self assert: Ñ !! (L !! 3) = 3.
	self assert: (L atRandomBits: 10) denominator odd.
	self testRing: L.
	self testRingHomomorphism: É -> L! !

!RingTest methodsFor: 'polynomials' stamp: 'len 5/4/2020 06:30:23'!
testMultivariatePolynomialRing
	| R P |
	R _ self newRing.
	P _ R polynomialsIn: #(x y z).
	self testRing: R.
	self testRingHomomorphism: R -> P! !

!RingTest methodsFor: 'polynomials' stamp: 'len 5/4/2020 06:29:53'!
testPolynomialRing
	| R P |
	R _ self newRing.
	P _ R polynomials.
	self testRing: P.
	self testRingHomomorphism: R -> P! !

!RingTest methodsFor: 'polynomials' stamp: 'len 5/23/2020 09:34:55'!
testPolynomialRingPrimeNotMaximalIdeal
	"Z[x]/<x> is isomorphic to Z, which is an integral domain but not a field. So <x> is prime but not maximal."
	| R I |
	R _ É polynomials.
	I _ R * R x.
	self assert: I isPrime.
	self assert: I isMaximal not! !

!RingTest methodsFor: 'polynomials' stamp: 'len 5/23/2020 09:33:01'!
testPolynomialRingPrimeNotMaximalIdeal2
	"Q[x,y]/<x> is isomorphic to Q[y], which is an integral domain but not a field. So <x> is prime but not maximal."
	| R I |
	R _ Ñ polynomialsIn: 2.
	I _ R * R x.
	self assert: I isPrime.
	self assert: I isMaximal not! !

!RingTest methodsFor: 'polynomials' stamp: 'len 7/16/2021 09:39:46'!
testRationalFunctionReconstruction
	"Example taken from 'Fast Rational Function Reconstruction', Sara Khodadad, 2005."
	| F x f g h |
	F _ É/11.
	x _ F polynomials x.
	f _ (1 to: 7) product: [:i| x - i].
	g _ x^6 + (x^5*3) + (x^4*8) + (x^3*4) + (x^2*6) + x + 9.
	h _ ((F polynomials / f) !! g) asRationalFunction.
	self assert: h numerator = (x^3*10 + 9).
	self assert: h denominator = (x + 2)
! !

!RingTest methodsFor: 'polynomials' stamp: 'len 5/13/2020 06:28:58'!
testRationalFunctions
	| R P F |
	R _ self newField.
	P _ R polynomials.
	F _ P fractions.
	self testRing: F.
	self testRingHomomorphism: R -> F.
	self testRingHomomorphism: P -> F! !

!RingTest methodsFor: 'polynomials' stamp: 'len 3/4/2021 15:03:00'!
testTensorProductOfPolynomialsRings
	self assert: (Ñ polynomialsIn: 2) ë (Ñ polynomialsIn: 3) = (Ñ polynomialsIn: 5).
	self assert: É polynomials ë Ñ = Ñ polynomials.
	self assert: É polynomials ë Ñ polynomials = (Ñ polynomialsIn: 2).
	self assert: Ñ polynomials ë É polynomials = (Ñ polynomialsIn: 2).
	"TODO: test tensor map and coprojections"! !

!RingTest methodsFor: 'quotients' stamp: 'len 10/6/2016 20:32'!
testQuotientRing
	| Q |
	Q _ self newQuotientRing.
	self testRing: Q.
	self testRingHomomorphism: Q projection! !

!RingTest methodsFor: 'quotients' stamp: 'len 2/27/2017 20:51:24'!
testQuotientRing2
	| Q pi f g |
	Q _ self newQuotientRing.
	f _ Q base atRandomBits: 10.
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
	R _ Ñ polynomials.
	R _ R / (R x^2 - 1).
	self assert: R isField not.
	self assert: R isIntegralDomain not! !

!RingTest methodsFor: 'quotients' stamp: 'len 5/23/2020 09:33:01'!
testQuotientRingWhichIsAField
	| R |
	R _ Ñ polynomials.
	R _ R / (R x^2 + (R x * 2) + 2).
	self assert: R isField! !

!RingTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newField
	| R |
	R _ {Ñ. É / 256 atRandom nextPrime} atRandom.
	"TODO: function fields, p-adics, power series"
"	R _ 2 atRandom = 1 ifTrue: [R extensionDegree: 3 atRandom + 1]."
"	2 atRandom = 1 ifTrue: [^ R polynomials fractions]."
	^ R! !

!RingTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newQuotientRing
	| R I |
	2 atRandom = 1 ifTrue: [^ É / (2 to: 10) atRandom].
	R _ Ñ polynomials.
	I _ R * (R atRandomBits: 10 maxDegree: 5).
	^ R / I! !

!RingTest methodsFor: 'private' stamp: 'len 5/23/2020 09:34:55'!
newRing
	4 atRandom = 1 ifTrue: [^ self newQuotientRing].
	2 atRandom = 1 ifTrue: [^ {É. Ñ. É / 64 atRandom} atRandom polynomialsIn: (1 to: 3) atRandom].
	^ {É. Ñ. É / 64 atRandom} atRandom! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2019 18:09:49'!
testBellNumbers
	self assert: (Sequence bellNumbers beginsWith: #(1 1 2 5 15 52 203 877)).
	"The inverse binomial transform is the shift operator for the Bell numbers:"
	self assert: (Sequence bellNumbers binomialInverseTransform beginsWith: #(1 2 5 15 52 203 877)).! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 12/5/2015 01:13'!
testDirichletConvolution
	self assert: ((Sequence phi dirichlet: Sequence one) beginsWith: #(1 2 3 4 5)).
	self assert: ((Sequence moebius dirichlet: Sequence one) beginsWith: #(1 0 0 0 0))! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 10/31/2016 04:14'!
testLinearRecursiveSequenceBug
	| F |
	F _ CFiniteSequence fibonacci.
	self assert: ((F - F) at: 1) = 0! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testRecursive
	| f g |
	f _ Sequence fibonacci.
	g _ CFiniteSequence to: É evaluating: f maxOrder: 5.
	self assert: (f equals: g upTo: 20).
	self assert: g order = 2.
	self assert: g characteristicPolynomial coefficients asArray = #(-1 -1 1)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 10/31/2016 04:13'!
testRecursive2
	| a b c |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence lucasNumbers.
	c _ a + (b*2) - (b+b).
	self assert: a = c! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 3/7/2017 07:28:24'!
testRecursiveConvolution
	| a b c a2 b2 c2 |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence lucasNumbers.
	a2 _ Sequence evaluating: [:i| a at: i].
	b2 _ Sequence evaluating: [:i| b at: i].

	c _ a convolution: b.
	c2 _ a2 convolution: b2.
	
	self assert: (c equals: c2 upTo: 20)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testRecursiveEquals
	| a b c |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence to: É evaluating: [:i| a at: i] maxOrder: 10.
	c _ CFiniteSequence lucasNumbers.
	self assert: a = b.
	self assert: a ~= c! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 3/7/2017 07:28:49'!
testRecursiveIndefiniteSumation
	| a c a2 c2 |
	a _ CFiniteSequence fibonacci.
	a2 _ Sequence evaluating: [:i| a at: i].

	c _ a sum.
	c2 _ a2 sum.
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 10/31/2016 04:13'!
testRecursiveMinimal
	| a f |
	a _ CFiniteSequence initialValues: #(0 1 1) coefficients: #(0 1 1).
	f _ CFiniteSequence fibonacci.
	self assert: a isMinimal not.
	self assert: a order = 3.
	self assert: a minimal order = 2.
	self assert: a minimalPolynomial = f characteristicPolynomial! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 3/7/2017 07:29:10'!
testRecursiveProduct
	| a b c c2 |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence pellNumbers.
	c _ a * b.
	c2 _ Sequence evaluating: [:i| (a at: i) * (b at: i)].
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20).
	self assert: c order = 4! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 3/7/2017 07:29:52'!
testRecursiveScalarMultiplication
	| a c a2 c2 |
	a _ CFiniteSequence fibonacci.
	a2 _ Sequence evaluating: [:i| a at: i].

	c _ a * 7.
	c2 _ a2 * 7.
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 3/7/2017 07:30:12'!
testRecursiveShiftLeft
	| a c a2 c2 |
	a _ CFiniteSequence fibonacci.
	a2 _ Sequence evaluating: [:i| a at: i].

	c _ a << 5.
	c2 _ a2 << 5.
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 3/7/2017 07:30:25'!
testRecursiveSum
	| a b c c2 |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence lucasNumbers.
	c _ a + b.
	c2 _ Sequence evaluating: [:i| (a at: i) + (b at: i)].
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/20/2017 08:36:51'!
test2Sphere
	| X |
	X _ SimplicialComplex sphere: 2.
	self assert: X dimension = 2.
	self assert: X fVector = (1,4,6,4)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/20/2017 08:37:08'!
test5Sphere
	| X |
	X _ SimplicialComplex sphere: 5.
	self assert: X dimension = 5.
	self assert: X fVector = (1,7,21,35,35,21,7)! !

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
	self assert: (X chainComplexOver: Ñ) betti = #(1 1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersKleinBottle
	| X |
	X _ SimplicialComplex kleinBottle.
	self assert: (X chainComplexOver: Ñ) betti = #(1 1 0)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersProjectivePlane
	| X |
	X _ SimplicialComplex realProjectivePlane.
	self assert: (X chainComplexOver: Ñ) betti = #(1 0 0)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersSphere
	| X |
	X _ SimplicialComplex sphere: 2.
	self assert: (X chainComplexOver: Ñ) betti = #(1 0 1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testBettiNumbersTorus
	| X |
	X _ SimplicialComplex torus.
	self assert: (X chainComplexOver: Ñ) betti = #(1 2 1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testChainComplex
	"Jeff Erickson, 'Computational Topology', Example 17.7."
	| S2 C |
	S2 _ SimplicialComplex facets: ((Simplex vertices: #(w x y z)) faces: 2).
	C _ S2 chainComplexOver: É.
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
	T _ C ◊ C.
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
	C _ X chainComplexOver: Ñ.
	d2 _ C at: 2.
	d1 _ C at: 1.
	self assert: d2 isZero not.
	self assert: d1 isZero not.
	self assert: (d1 ∑ d2) isZero! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 4/21/2020 12:18:08'!
testSimplicialComplex
	| S |
	S _ SimplicialComplex facets: {{1. 2. 3}. {2. 4}. {3. 4}. {5}}.
	self assert: (S faces: 2) asSet size = 1.
	self assert: (S faces: 1) asSet size = 5.
	self assert: (S faces: 0) asSet size = 5! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/19/2017 10:00:52'!
testSimplicialComplexExample
	"Example from Herzog, 'Monomial Ideals', pag. 16."
	| X |
	X _ SimplicialComplex on: (1 to: 5) faces: #((1 2 4) (1 2 5) (2 3) (3 4)).
	self assert: X facets = (#((1 2 4) (1 2 5) (2 3) (3 4)) collect: [:each| Simplex vertices: each]) asSet.
	self assert: X nonfaces = (#((1 3) (3 5) (4 5) (2 3 4)) collect: [:each| Simplex vertices: each]) asSet.
	self assert: X fVector = (1,5,7,2).
	self assert: X hVector = (1,2,0,-1)! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/20/2017 18:52:39'!
testSimplicialMap
	| S T f |
	S _ SimplicialComplex faces: #((0 2) (1 5) (3 4)).
	T _ SimplicialComplex faces: #((0 2) (1 3)).
	f _ S to: T map: ({0 -> 0. 1 -> 1. 2 -> 2. 3 -> 1. 4 -> 3. 5 -> 3} as: Dictionary).
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

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testStanleyReisnerRing
	| X R |
	X _ SimplicialComplex faces: #((0 1) (1 2) (2 3) (3 0)).
	R _ Ñ polynomialsIn: #(x y z w).
	self assert: X stanleyReisnerRing relations generators asSet = (R generate: [:x :y :z :w| {y*w. x*z}]) asSet! !

!SimplicialComplexTest methodsFor: 'as yet unclassified' stamp: 'len 2/14/2017 19:45:41'!
testTorus
	| X |
	X _ SimplicialComplex torus.
	self assert: X dimension = 2.
	self assert: X fVector = (1,7,21,14)! !

!TupleTest methodsFor: 'bit tuples' stamp: 'len 11/6/2016 11:35'!
testBitTupleBug
	| Z2 |
	Z2 _ PrimeField new: 2.
	self assert: ((1,0,0) % 2 :: sum = Z2 one).
	self assert: ((0,1,0) % 2 :: sum = Z2 one).
	self assert: ((1,1,0) % 2 :: sum = Z2 zero).
	self assert: ((1,1,1) % 2 :: sum = Z2 one)! !

!TupleTest methodsFor: 'bit tuples' stamp: 'len 6/1/2020 13:38:19'!
testBitTupleOperations
	| n a b |
	n _ 120 atRandom.
	a _ É^n atRandomBits: n*2.
	b _ É^n atRandomBits: n*2.
	#(+ - ∑ convolution: hadamard:) do: [:op|
		self testTuple: a with: b operation: op modulo: 2].
	#(negated reversed copy sum product) do: [:op|
		self testTuple: a operation: op modulo: 2]! !

!TupleTest methodsFor: 'bit tuples' stamp: 'len 6/1/2020 13:30:24'!
testBitTupleReversed
	| v i |
	v _ (É/2^100 atRandom) atRandom.
	i _ v size atRandom.
	self assert: (v reversed at: i) = (v at: v size - i + 1).
	self assert: v reversed reversed = v! !

!TupleTest methodsFor: 'bit tuples' stamp: 'len 5/8/2019 04:24:02'!
testBitTupleSum
	self assert: (BitTuple newFrom: 2r1011101) sum representative = 1.
	self assert: (BitTuple newFrom: 2r10000100100010) sum representative = 0! !

!TupleTest methodsFor: 'bit tuples' stamp: 'len 7/27/2020 08:51:53'!
testBitTuplesOfDifferentSize
	self testTuplesOfDifferentSizeOver: É/2! !

!TupleTest methodsFor: 'integer tuples' stamp: 'len 7/27/2020 08:20:55'!
testIntegerTupleOperations
	| a b |
	a _ (1,2,3).
	b _ (2,3,4).
	self assert: a + b = (3,5,7).
	self assert: a * 2 = (2,4,6).
	self assert: a + b + b - (b * 2) = a.
	self assert: a ∑ b = (2 + 6 + 12)! !

!TupleTest methodsFor: 'integer tuples' stamp: 'len 7/27/2020 08:59:41'!
testIntegerTupleSum
	self assert: (1,2,3) sum = (1 + 2 + 3)! !

!TupleTest methodsFor: 'integer tuples' stamp: 'len 7/27/2020 08:51:24'!
testIntegerTuplesOfDifferentSize
	self testTuplesOfDifferentSizeOver: É! !

!TupleTest methodsFor: 'misc' stamp: 'len 7/4/2020 11:34:03'!
testCrossProduct
	| V a b c r |
	V _ É^3.
	a _ V atRandomBits: 20.
	b _ V atRandomBits: 20.
	c _ V atRandomBits: 20.
	r _ V scalars atRandomBits: 10.
	self assert: (a ◊ a) isZero.
	self assert: a ◊ b = (b ◊ a) negated.
	self assert: a ◊ (b + c) = (a ◊ b + (a ◊ c)).
	self assert: (a*r) ◊ b = (a ◊ (b*r)).
	self assert: (a*r) ◊ b = (a ◊ b * r).
	self assert: (a ◊ (b ◊ c) + (b ◊ (c ◊ a)) + (c ◊ (a ◊ b))) isZero! !

!TupleTest methodsFor: 'misc' stamp: 'len 7/9/2020 07:17:27'!
testCrossProductAndDotProduct
	| V a b c d |
	V _ É^3.
	a _ V atRandomBits: 20.
	b _ V atRandomBits: 20.
	c _ V atRandomBits: 20.
	d _ V atRandomBits: 20.
	self assert: a ∑ (b ◊ c) = (b ∑ (c ◊ a)).
	self assert: a ∑ (b ◊ c) = (c ∑ (a ◊ b)).
	"Lagrange identity:"
	self assert: (a ◊ b) norm = (a norm * b norm - (a ∑ b) squared).
	"Binet-Chauchy identity:"
	self assert: a ◊ b ∑ (c ◊ d) = (a ∑ c *(b ∑ d) - (a ∑ d * (b ∑ c)))! !

!TupleTest methodsFor: 'misc' stamp: 'len 4/11/2018 15:54:46'!
testHomogeneousTuples
	self assert: (1,2,3) ~ (2,4,6).
	self deny: (1,2,3) ~ (-2,4,6)! !

!TupleTest methodsFor: 'misc' stamp: 'len 5/23/2020 09:34:55'!
testHomogeneousTuples2
	| P Q |
	P _ (1,2) over: É.
	Q _ (2,4) over: É.
	self assert: P ~ Q.
	P _ (1,2) over: É.
	Q _ (1,0) over: É.
	self deny: P ~ Q! !

!TupleTest methodsFor: 'misc' stamp: 'len 5/23/2020 09:33:01'!
testHomogeneousTuples3
	| R x P Q |
	R _ Ñ polynomials.
	x _ R x.
	P _ R tuple: {x. x^2 + 1}.
	Q _ R tuple: {x^2. x^3 + x}.
	self assert: P ~ Q! !

!TupleTest methodsFor: 'misc' stamp: 'len 4/11/2018 17:27:20'!
testHomogeneousTuples4
	| P Q |
	P _ (2,4,1) % 6.
	Q _ (0,1,3) % 6.
	self deny: P ~ Q! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 11/6/2016 11:39'!
testModTupleConcatenation
	self assert: (1,2,3) % 7, ((2,3,4) % 7) = ((1,2,3,2,3,4) % 7)! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 11/6/2016 13:49'!
testModTupleConvolution
	| a b |
	a _ (1,2,3).
	b _ (2,3,4).
	self assert: (a % 7 convolution: b % 7) = ((a convolution: b) % 7)! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 6/1/2020 13:39:06'!
testModTupleOperations
	| n a b modulus |
	modulus _ 10 atRandom + 1.
	n _ 5 atRandom.
	a _ É^n atRandomBits: 30.
	b _ É^n atRandomBits: 30.
	#(+ - ∑ convolution: hadamard:) do: [:op|
		self testTuple: a with: b operation: op modulo: modulus].
	#(negated reversed copy sum product) do: [:op|
		self testTuple: a operation: op modulo: modulus].! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 5/23/2020 09:34:55'!
testModTupleSum
	| a b |
	a _ É tuple: ((1 to: 10) collect: [:i| 100 atRandom]).
	b _ a % 7.
	self assert: b sum = (a sum % 7)! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersTupleAssociateBug
	| R t |
	R _ É/8.
	t _ R tuple: #(3 2).
	self assert: t*5*5 = t.
	self assert: (t*5) normalized = t normalized! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersTupleAssociateBug1
	| R t |
	R _ É/6.
	t _ R tuple: #(2 3).
	self assert: t*5*5 = t.
	self assert: (t*5) normalized = t normalized! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersTupleAssociateBug2
	| R t |
	R _ É/6.
	t _ R tuple: #(3 2).
	self assert: t*5*5 = t.
	self assert: (t*5) normalized = t normalized! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 7/30/2021 10:54:45'!
testModularIntegersTupleUnitsAndAssociates
	| R t tt |
	100 timesRepeat:
		[R _ É/64 atRandom.
		t _ (R^5 atRandom) atRandom.
		tt _ t normalized.
		self assert: t * t normalization = tt.
		R unitsDo: [:u| self assert: (t*u) normalized = tt]]! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersTupleUnitsAndAssociates2
	"Examples from Magma."
	| R |
	R _ É/6.
	self assert: (R tuple: #(0 2)) normalized = (R tuple: #(0 2)).
	self assert: (R tuple: #(0 4)) normalized = (R tuple: #(0 2)).
	self assert: (R tuple: #(2 3)) normalized = (R tuple: #(2 3)).
	self assert: (R tuple: #(4 3)) normalized = (R tuple: #(2 3)).
	R _ É/8.
	self assert: (R tuple: #(3 2)) normalized = (R tuple: #(1 6)).
	self assert: (R tuple: #(7 2)) normalized = (R tuple: #(1 6)).
	self assert: (R tuple: #(2 3)) normalized = (R tuple: #(2 3)).
	self assert: (R tuple: #(2 7)) normalized = (R tuple: #(2 3))! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 5/23/2020 09:34:55'!
testModularIntegersTupleUnitsAndAssociates3
	"Examples from Magma."
	| R |
	R _ É/6.
	self assert: (R tuple: #(0 4)) ~ (R tuple: #(0 2)).
	self assert: (R tuple: #(4 3)) ~ (R tuple: #(2 3)).
	R _ É/8.
	self assert: (R tuple: #(3 2)) ~ (R tuple: #(1 6)).
	self assert: (R tuple: #(7 2)) ~ (R tuple: #(1 6)).
	self assert: (R tuple: #(2 7)) ~ (R tuple: #(2 3))! !

!TupleTest methodsFor: 'modular integer tuples' stamp: 'len 7/27/2020 08:52:29'!
testModularIntegersTuplesOfDifferentSize
	self testTuplesOfDifferentSizeOver: É/64 atRandom! !

!TupleTest methodsFor: 'private' stamp: 'len 11/6/2016 14:24'!
testTuple: a operation: aSymbol modulo: modulus
	self assert: (a perform: aSymbol) % modulus = (a % modulus perform: aSymbol)! !

!TupleTest methodsFor: 'private' stamp: 'len 11/6/2016 14:12'!
testTuple: a with: b operation: aSymbol modulo: modulus
	self assert: (a perform: aSymbol with: b) % modulus = (a % modulus perform: aSymbol with: b % modulus)! !

!TupleTest methodsFor: 'private' stamp: 'len 7/27/2020 08:50:46'!
testTuplesOfDifferentSizeOver: aRing
	| a b zeros |
	a _ aRing^5 atRandom atRandomBits: 10.
	b _ aRing^5 atRandom atRandomBits: 10.
	zeros _ (aRing^5 atRandom) zero.
	self assert: a + (b,zeros) = (a + b).
	self assert: (a,zeros) + b = (a + b).
	self assert: a - (b,zeros) = (a - b).
	self assert: (a,zeros) - b = (a - b).
	self assert: a = (a,zeros).
	self assert: a,zeros = a.
	self assert: a hash = (a,zeros) hash.
	self assert: a ∑ (b,zeros) = (a ∑ b).
	self assert: a,zeros ∑ b = (a ∑ b)! !

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

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:55'!
testComplexAlgebraicNumberAddition
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 _ ComplexAlgebraicNumber sqrt: 2.
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:34:35'!
testComplexAlgebraicNumberDivision
	| sqrt3 sqrt5 |
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 / sqrt5 / sqrt3 / sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:34:48'!
testComplexAlgebraicNumberProduct
	| sqrt3 sqrt5 |
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 * sqrt5 * sqrt3 * sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:28'!
testComplexAlgebraicNumberSquare
	| sqrt2 |
	sqrt2 _ ComplexAlgebraicNumber sqrt2.
	10 timesRepeat: [sqrt2 squared]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:01'!
testRealAlgebraicNumberAddition
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 _ RealAlgebraicNumber sqrt: 2.
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:06'!
testRealAlgebraicNumberDivision
	| sqrt3 sqrt5 |
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 / sqrt5 / sqrt3 / sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:11'!
testRealAlgebraicNumberProduct
	| sqrt3 sqrt5 |
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 * sqrt5 * sqrt3 * sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:15'!
testRealAlgebraicNumberSquare
	| sqrt2 |
	sqrt2 _ RealAlgebraicNumber sqrt2.
	10 timesRepeat: [sqrt2 squared]! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:02'!
testCyclic3
	(Ñ polynomialsIn: 3) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:06'!
testCyclic4
	(Ñ polynomialsIn: 4) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:13'!
testCyclic5
	(Ñ polynomialsIn: 5) cyclicIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testEliminationExample
	| Qx x y z I |
	Qx _ Ñ polynomialsIn: #(x y z).
	x _ Qx x. y _ Qx y. z _ Qx z.
	I _ Qx * {x^2 + y + z - 1. x + (y^2) + z - 1. x + y + (z^2) - 1}.
	I eliminateAll: #(1 2)! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:17'!
testKatsura2
	(Ñ polynomialsIn: 2) katsuraIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:21'!
testKatsura3
	(Ñ polynomialsIn: 3) katsuraIdeal basis! !

!GroebnerBasisBenchmark methodsFor: 'as yet unclassified' stamp: 'len 1/13/2022 11:36:25'!
testKatsura4
	(Ñ polynomialsIn: 4) katsuraIdeal basis! !

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

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 12/18/2021 20:30:00'!
setUp
	| random |
	random _ Random seed: 1234.
	field isNil ifTrue: [field _ 	É/32003].
	matrix _ field matrix: 300@300 filling: [:i :j| field atRandom: random]! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 11/22/2016 11:55:20'!
testAddition
	matrix + matrix! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/12/2021 12:36:48'!
testGaussianElimination
	matrix echelon! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2017 09:33:07'!
testImage
	matrix image! !

!ModularMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 3/1/2017 09:33:01'!
testKernel
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
	P _ É polynomials.
	f _ P atRandomBits: 100 maxDegree: 50.
	g _ P atRandomBits: 100 maxDegree: 50.
	10000 timesRepeat: [f + g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testAdditionOverZ2
	| P f g |
	P _ É / 2 :: polynomials.
	f _ P atRandomDegree: 50.
	g _ P atRandomDegree: 50.
	10000 timesRepeat: [f + g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testDivisionOverQ
	| P x f g |
	P _ Ñ polynomials.
	x _ P x.
	f _ x^2 + x + 1 ^ 10.
	g _ 1 - x ^ 10.
	2000 timesRepeat: [f // g. f \\ g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testDivisionOverZ
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x^2 + x + 1 ^ 10.
	g _ 1 - x ^ 10.
	2000 timesRepeat: [f // g. f \\ g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:40:46'!
testFactorizationOverZ17Degree100
	| n f |
	n _ 100.
	f _ (É/17) polynomials !! [:x| ((1 to: n) sum: [:i| x^i*i]) + 1].
	f factorization! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:40:55'!
testFactorizationOverZ3Degree11Times20
	| F Fx x one f |
	F _ É / 3.
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
	P _ Ñ polynomials.
	x _ P x.
	f _ x^4 - 1 ^ 5.
	g _ x^6 - 1 ^ 5.
	2000 timesRepeat: [f gcd: g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testGCDOverZ
	| P x f g |
	P _ É polynomials.
	x _ P x.
	f _ x^4 - 1 ^ 5.
	g _ x^6 - 1 ^ 5.
	2000 timesRepeat: [f gcd: g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testGCDOverZ2
	| P x f g |
	P _ É / 2 :: polynomials.
	x _ P x.
	f _ x^4 - 1 ^ 5.
	g _ x^6 - 1 ^ 5.
	2000 timesRepeat: [f gcd: g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testMultiplication2OverZ
	| P f g |
	P _ É polynomials.
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
	P _ Ñ polynomials.
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
	P _ É polynomialsIn: #(x y z).
	f _ P atRandomBits: 100 maxDegree: 50.
	g _ P atRandomBits: 100 maxDegree: 50.
	10000 timesRepeat: [f + g]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testPseudoDivision
	| Zx x a b |
	Zx _ É polynomials.
	x _ Zx x.
	a _ x^8 + (x^6) - (3*(x^4)) - (3*(x^3)) + (8*(x^2)) + (2*x) - 5.
	b _ 3*(x^6) + (5*(x^4)) - (4*(x^2)) - (9*x) + 21.
	20 timesRepeat: [(a pseudoDivisionBy: b) quotient]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testPseudoRemainder
	| Zx x a b |
	Zx _ É polynomials.
	x _ Zx x.
	a _ x^8 + (x^6) - (3*(x^4)) - (3*(x^3)) + (8*(x^2)) + (2*x) - 5.
	b _ 3*(x^6) + (5*(x^4)) - (4*(x^2)) - (9*x) + 21.
	20 timesRepeat: [a pseudoRem: b]! !

!PolynomialBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testRealRoots
	| x f |
	x _ Ñ polynomials x.
	f _ (x-(1/2))^2*(x-2)*(x^2 + 1)*(x^2-2).
	20 timesRepeat: [f rootsIn: Ö]! !

!SmallMatrixBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
setUp
	integerMatrix _ É ^ (64@64) :: atRandomBits: 10000.
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
	P _ É polynomialsIn: #(x y z).
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

!TupleConvolutionBenchmark methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
setUp
	| n R |
	n _ 80.
	R _ É^n.
	a _ R atRandomBits: n*10.
	b _ R atRandomBits: n*10! !

!TupleConvolutionBenchmark methodsFor: 'as yet unclassified' stamp: 'len 6/6/2019 09:31:24'!
testConvolution
	a convolution: b! !

!TupleConvolutionBenchmark methodsFor: 'as yet unclassified' stamp: 'len 6/6/2019 09:31:54'!
testConvolution2
	a convolution2: b! !

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
