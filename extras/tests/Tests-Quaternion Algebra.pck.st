'From Cuis6.3 [latest update: #6222] on 24 November 2024 at 8:37:58 pm'!
'Description '!
!provides: 'Tests-Quaternion Algebra' 1 0!
SystemOrganization addCategory: #'Tests-Quaternion Algebra'!


!classDefinition: #QuaternionAlgebraTest category: #'Tests-Quaternion Algebra'!
AbstractFiniteAlgebraTest subclass: #QuaternionAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Quaternion Algebra'!
!classDefinition: 'QuaternionAlgebraTest class' category: #'Tests-Quaternion Algebra'!
QuaternionAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #QuaternionAlgebraTupleTest category: #'Tests-Quaternion Algebra'!
AbstractTupleTest subclass: #QuaternionAlgebraTupleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Quaternion Algebra'!
!classDefinition: 'QuaternionAlgebraTupleTest class' category: #'Tests-Quaternion Algebra'!
QuaternionAlgebraTupleTest class
	instanceVariableNames: ''!

!classDefinition: #QuaternionAlgebraMatrixTest category: #'Tests-Quaternion Algebra'!
AbstractMatrixTest subclass: #QuaternionAlgebraMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Quaternion Algebra'!
!classDefinition: 'QuaternionAlgebraMatrixTest class' category: #'Tests-Quaternion Algebra'!
QuaternionAlgebraMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #QuaternionAlgebraModuleTest category: #'Tests-Quaternion Algebra'!
AbstractModuleTest subclass: #QuaternionAlgebraModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Quaternion Algebra'!
!classDefinition: 'QuaternionAlgebraModuleTest class' category: #'Tests-Quaternion Algebra'!
QuaternionAlgebraModuleTest class
	instanceVariableNames: ''!


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

!QuaternionAlgebraTupleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:46:21'!
setUp
	super setUp.
	scalars := QuaternionAlgebra over: ℚ invariants: #(1 1)! !

!QuaternionAlgebraMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:19:19'!
setUp
	super setUp.
	scalars := QuaternionAlgebra over: ℚ invariants: #(1 1)! !

!QuaternionAlgebraModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:27:33'!
setUp
	super setUp.
	scalars := QuaternionAlgebra over: ℚ invariants: #(1 1)! !
