'From Cuis6.3 [latest update: #6222] on 7 December 2024 at 7:53:40 pm'!
'Description '!
!provides: 'Tests-Clifford Algebra' 1 1!
SystemOrganization addCategory: #'Tests-Clifford Algebra'!


!classDefinition: #CliffordAlgebraTest category: #'Tests-Clifford Algebra'!
AbstractFiniteAlgebraTest subclass: #CliffordAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Clifford Algebra'!
!classDefinition: 'CliffordAlgebraTest class' category: #'Tests-Clifford Algebra'!
CliffordAlgebraTest class
	instanceVariableNames: ''!

!classDefinition: #CliffordAlgebraMatrixTest category: #'Tests-Clifford Algebra'!
AbstractMatrixTest subclass: #CliffordAlgebraMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Clifford Algebra'!
!classDefinition: 'CliffordAlgebraMatrixTest class' category: #'Tests-Clifford Algebra'!
CliffordAlgebraMatrixTest class
	instanceVariableNames: ''!

!classDefinition: #CliffordAlgebraModuleTest category: #'Tests-Clifford Algebra'!
AbstractModuleTest subclass: #CliffordAlgebraModuleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Clifford Algebra'!
!classDefinition: 'CliffordAlgebraModuleTest class' category: #'Tests-Clifford Algebra'!
CliffordAlgebraModuleTest class
	instanceVariableNames: ''!


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

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/1/2024 10:10:30'!
testCliffordAlgebraScalarEmbedding2
	| ι |
	ι := R scalars asIdeal ⇒ R carrier !! (R scalars → R).
	self assert: ι isInjective.
	self assert: ι image = (R grade: 0) carrier! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:16:41'!
testCliffordAlgebraSpaceEmbedding
	| V v x |
	V := R space.
	v := V atRandom: random bits: 10.
	x := R !! v.
	self assert: x isVector.
	self assert: ((R grade: 1) includes: x).
	self assert: x asVector = v! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/1/2024 10:10:48'!
testCliffordAlgebraSpaceEmbedding2
	| ι |
	ι := R space ⇒ R carrier !! (R space underlyingSet → R underlyingSet).
	self assert: ι isInjective.
	self assert: ι image = (R grade: 1) carrier! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:16:48'!
testCliffordAlgebraUniversalProperty
	| Q v |
	Q := R quadraticForm.
	v := R space atRandom: random bits: 10.
	self assert: (R !! v) squared = (R one * (Q value: v))! !

!CliffordAlgebraMatrixTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 15:54:34'!
setUp
	super setUp.
	self flag: #fixme. "use ℚ^3 or more"
	scalars := CliffordAlgebra on: (QuadraticForm from: ℚ^2 coefficients: #(1 0 0 1 0 1))! !

!CliffordAlgebraModuleTest methodsFor: 'as yet unclassified' stamp: 'len 5/27/2023 15:58:08'!
setUp
	super setUp.
	self flag: #fixme. "should use ℚ^3 or more, but now it's too slow"
	scalars := CliffordAlgebra on: (QuadraticForm from: ℚ^2 coefficients: #(1 0 0 1 0 1))! !
