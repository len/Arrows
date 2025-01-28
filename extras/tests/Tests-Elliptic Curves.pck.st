'From Cuis7.3 [latest update: #7034] on 28 January 2025 at 8:59:12 pm'!
'Description '!
!provides: 'Tests-Elliptic Curves' 1 9!
!requires: 'Elliptic Curves' 1 2 nil!
SystemOrganization addCategory: #'Tests-Elliptic Curves'!


!classDefinition: #AbstractEllipticCurveTest category: #'Tests-Elliptic Curves'!
RandomizedTestCase subclass: #AbstractEllipticCurveTest
	instanceVariableNames: 'E'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'AbstractEllipticCurveTest class' category: #'Tests-Elliptic Curves'!
AbstractEllipticCurveTest class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveBN254Test category: #'Tests-Elliptic Curves'!
AbstractEllipticCurveTest subclass: #EllipticCurveBN254Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'EllipticCurveBN254Test class' category: #'Tests-Elliptic Curves'!
EllipticCurveBN254Test class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveCremona389a1Test category: #'Tests-Elliptic Curves'!
AbstractEllipticCurveTest subclass: #EllipticCurveCremona389a1Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'EllipticCurveCremona389a1Test class' category: #'Tests-Elliptic Curves'!
EllipticCurveCremona389a1Test class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveSecp256k1Test category: #'Tests-Elliptic Curves'!
AbstractEllipticCurveTest subclass: #EllipticCurveSecp256k1Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'EllipticCurveSecp256k1Test class' category: #'Tests-Elliptic Curves'!
EllipticCurveSecp256k1Test class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurvesTest category: #'Tests-Elliptic Curves'!
RandomizedTestCase subclass: #EllipticCurvesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'EllipticCurvesTest class' category: #'Tests-Elliptic Curves'!
EllipticCurvesTest class
	instanceVariableNames: ''!


!AbstractEllipticCurveTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 23:20:11'!
testGroupLaw
	| O P Q R |
	O := E points identity.
	self assert: O + O = O.
	self assert: O doubled = O.
	self assert: O negated = O.
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
		self assert: (E satisfies: (P + Q) coordinates).
		"inverse:"
		self assert: P + P negated = O.
		self assert: P + Q - Q = P.
		self assert: (E satisfies: P negated coordinates).
		"doubling:"
		self assert: P + P = P doubled.
		self assert: P negated doubled = P doubled negated.
		self assert: (E satisfies: P doubled coordinates).
		"scalar multiplication:"
		self assert: P * 0 = O.
		self assert: P * 1 = P.
		self assert: P * 2 = P doubled.
		self assert: P * 3 = (P + P + P).
		self assert: P * 4 = P doubled doubled.
		self assert: P * -1 = P negated.
		self assert: P * -2 = P doubled negated.
		self assert: P * -3 = (P negated * 3)]! !

!AbstractEllipticCurveTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 16:52:21'!
testPointsWithX
	10 count: [(E points pointsWithX: (E scalars atRandom: random)) allSatisfy: [:each| E satisfies: each coordinates]]! !

!AbstractEllipticCurveTest methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 14:30:18'!
testWeierstrassEquationInvariants
	self assert: E b₈ * 4 = (E b₂ * E b₆ - E b₄ squared).
	self assert: E discriminant * 1728 = (E c₄ ^ 3 - E c₆ squared)! !

!EllipticCurveBN254Test methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 15:19:03'!
setUp
	super setUp.
	E := EllipticCurve bn254! !

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

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/21/2023 16:33:05'!
testArithmeticExample1
	"[Coh+05, Example 13.4]."
	| F E P Q |
	F := ℤ/2003.
	E := EllipticCurve coefficients: F !!!! #(2 5 8 1136 531).
	P := E points !! #(1118 269).
	P validate.
	Q := E points !! #(892 529).
	Q validate.
	self assert: P negated = (E points !! #(1118 1493)).
	self assert: P+Q = (E points !! #(1681 1706)).
	self assert: P*2 = (E points !! #(1465 677))
! !

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

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 11:50:44'!
testCurveFromCInvariantsExample
	| E |
	E := EllipticCurve c₄: 17 c₆: -2005.
	self assert: E c₄ = 17.
	self assert: E c₆ = -2005! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/22/2023 16:07:09'!
testHasseInvariantExamples
	"Examples from Sage documentation:"
	| F E |
	F := ℤ/101.
	E := EllipticCurve coefficients: F !!!! #(0 0 0 0 1).
	self assert: E hasseInvariant = (F !! 0).
	E := EllipticCurve coefficients: F !!!! #(0 0 0 1 1).
	self assert: E hasseInvariant = (F !! 98).
	F := ℤ/103.
	E := EllipticCurve coefficients: F !!!! #(0 0 0 0 1).
	self assert: E hasseInvariant = (F !! 20).
	E := EllipticCurve coefficients: F !!!! #(0 0 0 1 1).
	self assert: E hasseInvariant = (F !! 17).
	F := GaloisField new: 107 to: 2.
	E := EllipticCurve coefficients: F !!!! {0. 0. 0. F x. 1}.
	self assert: E hasseInvariant = (F x * 62 + 75).
	E := EllipticCurve coefficients: F !!!! {0. 0. 0. 0. F x}.
	self assert: E hasseInvariant = (F !! 0)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 19:36:33'!
testMillerAlgorithmExample1
	| F E P Q I |
	F := ℤ/7.
	E := EllipticCurve coefficients: F !!!! #(0 2).
	P := E !! (F !!!! #(5 1)).
	Q := E !! (F !!!! #(0 3)).
	I := E zero.
	self assert: (I miller: P torsion: 1) = (F !! 1).
	self assert: (I miller: Q torsion: 1) = (F !! 1)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 19:35:22'!
testMillerAlgorithmExample2
	"A 2-torsion example (from Sage):"
	| F E P Q |
	F := ℤ/7.
	E := EllipticCurve coefficients: F !!!! #(-1 0).
	P := E !! (F !!!! #(0 0)).
	Q := E !! (F !!!! #(1 0)).
	self assert: (P miller: P torsion: 2) = (F !! 0).
	self assert: (Q miller: Q torsion: 2) = (F !! 0).
	self assert: (P miller: Q torsion: 2) = (F !! 1).
	self assert: (Q miller: P torsion: 2) = (F !! 6)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 19:33:16'!
testMillerAlgorithmExample3
	"A 3-torsion example (from Sage):"
	| F E P Q |
	F := ℤ/7.
	E := EllipticCurve coefficients: F !!!! #(0 2).
	P := E !! (F !!!! #(5 1)).
	Q := E !! (F !!!! #(0 3)).
	self assert: (P miller: Q torsion: 3) = (F !! 4)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 19:38:55'!
testMillerAlgorithmExample4
	"A 4-torsion example (from Sage):"
	| F E P Q |
	F := ℤ/7.
	E := EllipticCurve coefficients: F !!!! #(-1 0).
	P := E !! (F !!!! #(5 1)).
	Q := E !! (F !!!! #(4 2)).
	self assert: (P miller: Q torsion: 4) = (F !! 3)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 19:39:39'!
testMillerAlgorithmExample5
	"A 5-torsion example (from Sage):"
	| F E P Q |
	F := ℤ/7.
	E := EllipticCurve coefficients: F !!!! #(-1 4).
	P := E !! (F !!!! #(4 1)).
	Q := E !! (F !!!! #(6 5)).
	self assert: (P miller: Q torsion: 5) = (F !! 1)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 19:40:24'!
testMillerAlgorithmExample6
	"A 6-torsion example (from Sage):"
	| F E P Q |
	F := ℤ/7.
	E := EllipticCurve coefficients: F !!!! #(3 1).
	P := E !! (F !!!! #(5 1)).
	Q := E !! (F !!!! #(3 3)).
	self assert: (P miller: Q torsion: 6) = (F !! 5)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 22:49:31'!
testMillerAlgorithmExample7
	"An example of embedding degree 6 (from Sage):"
	| q F E R m K EK P Qx Qy Q |
	q := 401.
	F := ℤ/q.
	E := EllipticCurve coefficients: F !!!! #(146 400).
	R := F polynomials.
	m := R !! #(3 51 81 115 4 0 1).
	K := GaloisField modulus: m.
	EK := E over: K.
	P := E !! (F !!!! #(338 227)).
	self assert: (E satisfies: P coordinates).
	Qx := K !! #(93 71 335 160 391 333).
	Qy := K !! #(210 340 342 26 273 343).
	Q := EK !! (K !!!! {Qx. Qy}).
	self assert: (EK satisfies: Q coordinates).
	self assert: (P miller: Q torsion: 127) = (K !! #(275 20 233 355 39 371))! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 12/12/2023 19:02:59'!
testMillerLineSubrutineExamples
	"Examples from Sage:"
	| F E O a P Q |
	F := GaloisField new: 2 to: 5.
	E := EllipticCurve coefficients: F !!!! {0. 0. 1. 1. 1}.
	O := E zero.
	a := F x.
	P := E !! (a^4 + 1, (a^3)).
	Q := E !! (a^4 , (a^4 + (a^3))).
	self assert: (P line: P at: P*-2) = (F !! 0).
	self assert: (P line: Q at: (P+Q) negated) = (F !! 0).
	self assert: (O line: O at: Q) = (F !! 1).
	self assert: (P line: O at: Q) = (F !! 1).
	self assert: (P line: P*13 at: Q) = (a^4).
	self assert: (P line: P at: Q) = (a^4 + (a^3) + (a^2) + 1)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/21/2023 16:37:03'!
testPointOrderExample1
	| p F E P |
	p := 103.
	F := ℤ/p.
	E := EllipticCurve coefficients: F !!!! #(1 18).
	P := E !! (F !!!! (33, 91)).
	self assert: P order = 19! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/21/2023 16:36:43'!
testPointOrderExample2
	"[Coh+05, Example 13.10]."
	| F E P Q |
	F := ℤ/2003.
	E := EllipticCurve coefficients: F !!!! #(2 5 8 1136 531).
	E points size = 1956.
	P := E points !! #(1118 269).
	self assert: P order = 1956.
	Q := E points !! #(892 529).
	self assert: Q order = 163! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/22/2023 14:46:00'!
testQuadraticTwistExample
	| F E |
	F := ℤ/1103.
	E := EllipticCurve coefficients: F !!!! #(1 0 0 107 340).
	self assert: (E quadraticTwistBy: -1) coefficients = (F !!!! #(0 1102 0 609 300))! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 01:40:03'!
testQuadraticTwistTraceOfFrobeniusExamples
	| F E |
	F := ℤ/1103.
	E := EllipticCurve coefficients: F !!!! #(1 0 0 107 340).
	self assert: (E quadraticTwistBy: -1) trace = E trace negated.
	F := GaloisField new: 7 to: 2.
	E := EllipticCurve coefficients: F !!!! #(-1 3).
	self assert: E quadraticTwist trace = E trace negated! !

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

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 16:52:28'!
testSECP256k1PointsWithX
	| E p |
	E := EllipticCurve secp256k1.
	p := E scalars modulus.
	10 count: [(E points pointsWithX: p atRandom) allSatisfy: [:each| E satisfies: each coordinates]]! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 23:32:34'!
testTatePairingExample
	"Example from Sage:"
	| p F E P Q n k |
	p := 103.
	F := ℤ/p.
	E := EllipticCurve coefficients: F !!!! #(1 18).
	P := E !! (F !!!! (33, 91)).
	Q := E !! (F !!!! (87, 51)).
	n := P order.
	self assert: n = 19.
	k := ((GaloisField new: n) !! p) multiplicativeOrder.
	self assert: k = 6.
	self assert: (P tatePairing: P torsion: n embeddingDegree: k) = (F !! 1).
	self assert: (P tatePairing: Q torsion: n embeddingDegree: k) = (F !! 1)! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 12/12/2023 19:03:11'!
testWeilPairingExample
	"Example from Sage:"
	| R F E a P Q x |
	R := (ℤ/19) polynomials.
	F := GaloisField modulus: (R !! #(2 11 2 0 1)).
	E := EllipticCurve coefficients: F !!!! #(-1 0).
	a := F x.
	P := E !! (F !!!! {a^3*15 + (a^2*17) + (a*14) + 13. a^3*16 + (a^2*7) + a + 18}).
	self assert: (E satisfies: P coordinates).
	Q := E !! (F !!!! {a^3*10 + (a^2*16) + (a*4) + 2. a^3*6 + (a^2*4) + (a*3) + 2}).
	self assert: (E satisfies: Q coordinates).
	x := P weilPairing: Q torsion: 360.
	self assert: (x^360) isOne! !
