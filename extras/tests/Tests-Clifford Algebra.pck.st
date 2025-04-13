'From Cuis7.3 [latest update: #7092] on 13 April 2025 at 12:41:40 pm'!
'Description '!
!provides: 'Tests-Clifford Algebra' 1 6!
SystemOrganization addCategory: #'Tests-Clifford Algebra'!


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

!classDefinition: #CliffordAlgebraTest category: #'Tests-Clifford Algebra'!
AbstractFiniteAlgebraTest subclass: #CliffordAlgebraTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Clifford Algebra'!
!classDefinition: 'CliffordAlgebraTest class' category: #'Tests-Clifford Algebra'!
CliffordAlgebraTest class
	instanceVariableNames: ''!


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

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/26/2023 00:28:39'!
setUp
	super setUp.
	R := CliffordAlgebra on: (QuadraticForm from: ℚ^3 coefficients: #(1 0 0 1 0 1))! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2024 10:16:16'!
testCliffordAlgebraDualGrade
	| x k |
	k := (0 to: R degree) atRandom: random.
	x := R !! ((R grade: k) atRandom: random bits: 10).
	self assert: ((R grade: R degree - k) includes: R carrier !! x dual)! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/9/2024 12:54:02'!
testCliffordAlgebraDualProperties
	| x y |
	x := self newElement.
	y := self newElement.
	self assert: x ∙| y dual = (x ∧ y) dual.
	self assert: x ∧ y dual = (x ∙| y) dual! !

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

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2024 10:15:38'!
testCliffordAlgebraGradeProjection
	| x k j |
	k := (0 to: R degree) atRandom: random.
	[j := (0 to: R degree) atRandom: random. j = k] whileTrue.
	x := self newElement.
	self assert: ((R grade: k) includes: R carrier !! (x grade: k)).
	self assert: (((R grade: j) includes: R carrier !! (x grade: k)) not or: [(x grade: k) isZero]).
	x := R !! ((R grade: k) atRandom: random bits: 10).
	self assert: (x grade: k) = x.
	self assert: (x grade: j) isZero! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2024 10:12:55'!
testCliffordAlgebraHomogeneousElements
	| x |
	x := (R grade: 0) atRandom: random bits: 10.
	self assert: (R !! x) isScalar.
	x := (R grade: 1) atRandom: random bits: 10.
	self assert: (R !! x) isVector.
	x := (R grade: 2) atRandom: random bits: 10.
	self assert: (R !! x) isBivector! !

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

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/9/2024 12:55:48'!
testCliffordAlgebraProductsProperties
	| x y z one |
	x := self newElement.
	y := self newElement.
	z := self newElement.
	"Associativity of wedge product:"
	self assert: x ∧ (y ∧ z) = (x ∧  y ∧ z).
	"Associativity of left and right interior products:"
	self assert: x ∙| (y |∙ z) = (x ∙| y |∙ z).
	self assert: x ∙| (y ∙| z) = (x ∧ y ∙| z).
	self assert: x ∙ (y ∙| z) = (x ∧ y ∙ z).
	one := R one.
	self assert: one ∧ x = x.
	self assert: x ∧ one = x.
	self assert: one ∙| x = x.
	self assert: x |∙ one = x! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/9/2024 12:56:13'!
testCliffordAlgebraProductsPropertiesWithVectors
	| x y v |
	x := self newElement.
	y := self newElement.
	v := R !! (R space atRandom: random bits: 10).
	self assert: v * x = (v ∙| x + (v ∧ x)).
	self assert: v ∙| x = (v*x - (x reflection * v) / 2).
	self assert: v ∙| x = (x reflection negated |∙ v).
	self assert: v ∧ x = (v*x + (x reflection * v) / 2).
	self assert: v ∧ x = (x reflection ∧ v).
	self assert: v ∙| (x*y) = ((v ∙| x)*y + (x reflection * (v ∙| y)))! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 4/11/2025 22:15:04'!
testCliffordAlgebraProperties
	self assert: R isCommutative not.
	self assert: R degree = (2^R space rank).
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

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2024 10:12:15'!
testCliffordAlgebraScalarEmbedding
	| a x |
	a := self newScalar.
	x := R !! a.
	self assert: x isScalar.
	self assert: ((R grade: 0) includes: R carrier !! x).
	self assert: R one * a = x.
	self assert: x asScalar = a! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2024 10:12:04'!
testCliffordAlgebraScalarEmbedding2
	| ι |
	ι := R scalars asIdeal ⇒ R carrier !! (R scalars → R).
	self assert: ι isInjective.
	self assert: ι image = (R grade: 0)! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2024 10:12:21'!
testCliffordAlgebraSpaceEmbedding
	| V v x |
	V := R space.
	v := V atRandom: random bits: 10.
	x := R !! v.
	self assert: x isVector.
	self assert: ((R grade: 1) includes: R carrier !! x).
	self assert: x asVector = v! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2024 10:11:28'!
testCliffordAlgebraSpaceEmbedding2
	| ι |
	ι := R space ⇒ R carrier !! (R space underlyingSet → R underlyingSet).
	self assert: ι isInjective.
	self assert: ι image = (R grade: 1)! !

!CliffordAlgebraTest methodsFor: 'as yet unclassified' stamp: 'len 5/20/2023 21:16:48'!
testCliffordAlgebraUniversalProperty
	| Q v |
	Q := R quadraticForm.
	v := R space atRandom: random bits: 10.
	self assert: (R !! v) squared = (R one * (Q value: v))! !
