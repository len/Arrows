'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 2:13:09 pm'!
'Description '!
!provides: 'Tests-Elliptic Curves' 1 1!
!requires: 'Elliptic Curves' 1 2 nil!
SystemOrganization addCategory: #'Tests-Elliptic Curves'!


!classDefinition: #EllipticCurvesTest category: #'Tests-Elliptic Curves'!
TestCase subclass: #EllipticCurvesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Elliptic Curves'!
!classDefinition: 'EllipticCurvesTest class' category: #'Tests-Elliptic Curves'!
EllipticCurvesTest class
	instanceVariableNames: ''!


!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 11/11/2016 10:14'!
testBInvariants
	| W |
	W _ WeierstrassEquation coefficients: (1,2,3,4,5).
	self assert: W b2 = 9.
	self assert: W b4 = 11.
	self assert: W b6 = 29.
	self assert: W b8 = 35! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 11/11/2016 10:30'!
testCInvariants
	| W |
	W _ WeierstrassEquation coefficients: (0,-1,1,-10,-20).
	self assert: W c4 = 496.
	self assert: W c6 = 20008! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testGroupLaw
	| E P Q O |
	E _ EllipticCurve cremona389a1.
	P _ E pointAt: (� tuple: #(-1 1)).
	Q _ E pointAt: (� tuple: #(0 -1)).
	O _ E pointAt: (� tuple: #(0 1 0)).
	self assert: P+Q = (E pointAt: (� tuple: #(4 8))).
	self assert: P-Q = (E pointAt: (� tuple: #(1 0))).
	self assert: 3*P - (5*Q) = (E pointAt: (� tuple: {328/361. -2800/6859})).
	self assert: O+P = P.
	self assert: P+O = P.
	self assert: P-P = O! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 8/27/2018 22:15:43'!
testPointAtX
	| E p |
	E _ EllipticCurve secp256k1.
	p _ E scalars modulus.
	10 count: [(E pointsAtX: p atRandom) allSatisfy: [:each| E satisfies: each coordinates]]! !

!EllipticCurvesTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:34:55'!
testWeierstrassEquationInvariants
	| W |
	W _ WeierstrassEquation coefficients: (� ^ 5 atRandomBits: 100).
	self assert: W b8 * 4 = (W b2 * W b6 - W b4 squared).
	self assert: W discriminant * 1728 = (W c4 ^ 3 - W c6 squared)! !