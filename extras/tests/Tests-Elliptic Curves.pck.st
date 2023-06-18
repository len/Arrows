'From Cuis 6.0 [latest update: #5866] on 18 June 2023 at 2:40:19 pm'!
'Description '!
!provides: 'Tests-Elliptic Curves' 1 3!
!requires: 'Elliptic Curves' 1 2 nil!
SystemOrganization addCategory: 'Tests-Elliptic Curves'!


!classDefinition: #AbstractEllipticCurveTest category: 'Tests-Elliptic Curves'!
RandomizedTestCase subclass: #AbstractEllipticCurveTest
	instanceVariableNames: 'E'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'AbstractEllipticCurveTest class' category: 'Tests-Elliptic Curves'!
AbstractEllipticCurveTest class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveCremona389a1Test category: 'Tests-Elliptic Curves'!
AbstractEllipticCurveTest subclass: #EllipticCurveCremona389a1Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'EllipticCurveCremona389a1Test class' category: 'Tests-Elliptic Curves'!
EllipticCurveCremona389a1Test class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveSecp256k1Test category: 'Tests-Elliptic Curves'!
AbstractEllipticCurveTest subclass: #EllipticCurveSecp256k1Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'EllipticCurveSecp256k1Test class' category: 'Tests-Elliptic Curves'!
EllipticCurveSecp256k1Test class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurvesTest category: 'Tests-Elliptic Curves'!
RandomizedTestCase subclass: #EllipticCurvesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'EllipticCurvesTest class' category: 'Tests-Elliptic Curves'!
EllipticCurvesTest class
	instanceVariableNames: ''!


!AbstractEllipticCurveTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:29:58'!
testGroupLaw
	| O P Q R |
	O := E points identity.
	20 timesRepeat:
		[P := E points atRandom: random.
		Q := E points atRandom: random.
		R := E points atRandom: random.
		"identity element:"
		self assert: P + O = P.
		self assert: O + P = P.
		"associativity:"
		self assert: P + Q + R = (P + (Q + R)).
		"commutativity:"
		self assert: P + Q = (Q + P).
		"inverse:"
		self assert: P + P negated = O.
		self assert: P + Q - Q = P.
		"doubling:"
		self assert: P + P = P doubled.
		"scalar multiplication:"
		self assert: P * 0 = O.
		self assert: P * 1 = P.
		self assert: P * 2 = P doubled.
		self assert: P * 3 = (P + P + P).
		self assert: P * -3 = (P negated * 3)]! !

!AbstractEllipticCurveTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:34:28'!
testPointsAtX
	10 count: [(E pointsAtX: (E scalars atRandom: random)) allSatisfy: [:each| E satisfies: each coordinates]]! !

!AbstractEllipticCurveTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:30:18'!
testWeierstrassEquationInvariants
	self assert: E b₈ * 4 = (E b₂ * E b₆ - E b₄ squared).
	self assert: E discriminant * 1728 = (E c₄ ^ 3 - E c₆ squared)! !

!EllipticCurveCremona389a1Test methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:29:33'!
setUp
	super setUp.
	E := EllipticCurve cremona389a1! !

!EllipticCurveCremona389a1Test methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:30:49'!
testGroupLaw2
	| P Q O |
	P := E pointAt: (ℚ !!!! #(-1 1)).
	Q := E pointAt: (ℚ !!!! #(0 -1)).
	O := E pointAt: (ℚ !!!! #(0 1 0)).
	self assert: P+Q = (E pointAt: (ℚ !!!! #(4 8))).
	self assert: P-Q = (E pointAt: (ℚ !!!! #(1 0))).
	self assert: 3*P - (5*Q) = (E pointAt: (ℚ !!!! {328/361. -2800/6859})).
	self assert: O+P = P.
	self assert: P+O = P.
	self assert: P-P = O! !

!EllipticCurveSecp256k1Test methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:32:16'!
setUp
	super setUp.
	E := EllipticCurve secp256k1! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:25:34'!
testBInvariantsExample
	| E |
	E := EllipticCurve coefficients: (1,2,3,4,5).
	self assert: E b₂ = 9.
	self assert: E b₄ = 11.
	self assert: E b₆ = 29.
	self assert: E b₈ = 35! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:25:25'!
testCInvariantsExample
	| E |
	E := EllipticCurve coefficients: (0,-1,1,-10,-20).
	self assert: E c₄ = 496.
	self assert: E c₆ = 20008! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:35:24'!
testRandomCurveWeierstrassEquationInvariants
	| E |
	E := EllipticCurve coefficients: (ℤ ^ 5 atRandom: random bits: 100).
	self assert: E b₈ * 4 = (E b₂ * E b₆ - E b₄ squared).
	self assert: E discriminant * 1728 = (E c₄ ^ 3 - E c₆ squared)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:35:57'!
testRandomShortWeierstrassCurveWeierstrassEquationInvariants
	| E |
	E := EllipticCurve coefficients: (ℤ ^ 2 atRandom: random bits: 100).
	self assert: E b₈ * 4 = (E b₂ * E b₆ - E b₄ squared).
	self assert: E discriminant * 1728 = (E c₄ ^ 3 - E c₆ squared)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:34:42'!
testSECP256k1PointsAtX
	| E p |
	E := EllipticCurve secp256k1.
	p := E scalars modulus.
	10 count: [(E pointsAtX: p atRandom) allSatisfy: [:each| E satisfies: each coordinates]]! !
