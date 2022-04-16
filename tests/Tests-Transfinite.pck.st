'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 2:13:19 pm'!
'Description '!
!provides: 'Tests-Transfinite' 1 0!
!requires: 'Transfinite' 1 0 nil!
SystemOrganization addCategory: #'Tests-Transfinite'!


!classDefinition: #InfinityTest category: #'Tests-Transfinite'!
TestCase subclass: #InfinityTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Transfinite'!
!classDefinition: 'InfinityTest class' category: #'Tests-Transfinite'!
InfinityTest class
	instanceVariableNames: ''!

!classDefinition: #OrdinalsTest category: #'Tests-Transfinite'!
TestCase subclass: #OrdinalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Transfinite'!
!classDefinition: 'OrdinalsTest class' category: #'Tests-Transfinite'!
OrdinalsTest class
	instanceVariableNames: ''!


!InfinityTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 12:37:29'!
testInfinity
	self assert: Infinity positive > Infinity negative.
	self assert: Infinity positive > 127934879182734.
	self assert: Infinity negative < -198273498712341.
	self assert: Infinity positive >= 127934879182734.
	self assert: Infinity negative <= -198273498712341.
	self assert: -918723948719 > Infinity negative.
	self assert: 91872123948719 < Infinity positive! !

!InfinityTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 12:37:36'!
testInfinityUndeterminates
	self should: [Infinity positive + Infinity negative] raise: Error.
	self should: [Infinity positive * 0] raise: Error.
	self should: [Infinity positive / Infinity positive] raise: Error.
	self should: [Infinity positive ^ 0] raise: Error
! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 12:03:01'!
newOrdinal
	| answer |
	answer _ (0 to: 2) atRandom asOrdinal.
	2 atRandom = 1 ifTrue: [answer _ answer exp].
	^ answer! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 12:41:47'!
testAdditionAssociativity
	| a b c |
	a _ self newOrdinal.
	b _ self newOrdinal.
	c _ self newOrdinal.
	self assert: a + (b + c) = ((a + b) + c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/29/2021 11:30:24'!
testAdditionExamples
	| � |
	� _ 1 asOrdinal exp.
	self assert: � + 1 + 1 = (� + 2).
	self assert: 1 + � = �.
	self assert: �^5*3 + (�^3*2) + (�^3*3) = (�^5*3 + (�^3*5)).
	self assert: �^5*3 + (�^3*2) + (�^4*2) = (�^5*3 + (�^4*2)).
	self assert: �^�^2*2 + (�^3*2) + (�^4*2) = (�^�^2*2 + (�^4*2)).
	self assert: 3 + (�^5*3) = (�^5*3).
	self assert: �^5*3 + 2 + 1 = (�^5*3 + 3)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 13:46:41'!
testAdditionIncreasing
	"Ordinal addition is strictly increasing in the addend."
	| a b c |
	a _ self newOrdinal.
	[b _ self newOrdinal.
	c _ self newOrdinal.
	b < c] whileFalse.
	self assert: a + b < (a + c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 13:44:15'!
testAdditionNeutral
	"The empty ordinal 0 is a two-sided neutral element for the ordinal addition."
	| a |
	a _ self newOrdinal.
	self assert: a + 0 asOrdinal = a.
	self assert: 0 asOrdinal + a = a! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 13:49:00'!
testAdditionNondecreasing
	"Ordinal addition is nondecreasing in the augend."
	| a b c |
	[a _ self newOrdinal.
	b _ self newOrdinal.
	a <= b] whileFalse.
	c _ self newOrdinal.
	self assert: a + c <= (b + c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 18:38:45'!
testAdditionTruncationProperty
	| a b |
	a _ self newOrdinal.
	b _ self newOrdinal.
	self assert: a + b = ((a truncatedAt: b degree) + b)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 14:21:04'!
testArithmeticExamples
	| � a b |
	� _ 1 asOrdinal exp.
	a _ �^4*2 + (�^2) + 3.
	b _ �^3*5 + (�*7).
	self assert: a + b = (�^4*2 + (�^3*5) + (�*7)).
	self assert: a * b = (�^7*5 + (�^5*7))! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 11:53:37'!
testConvertingIntegers
	self assert: 0 asOrdinal asInteger = 0.
	self assert: 1 asOrdinal asInteger = 1.
	self assert: 42 asOrdinal asInteger = 42! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 11:54:37'!
testExp
	self assert: 0 asOrdinal exp = 1 asOrdinal.
	self assert: 0 asOrdinal exp exp isFinite not! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 14:08:33'!
testExp2
	"If a < b, then w^a + w^b = w^b."
	| a b |
	[a _ self newOrdinal.
	b _ self newOrdinal.
	a < b] whileFalse.
	self assert: a exp + b exp = b exp! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 12:44:36'!
testExponentiation
	| a b c |
	a _ self newOrdinal.
	b _ self newOrdinal.
	c _ self newOrdinal.
	self assert: a^(b*c) = (a^b^c).
	self assert: a^(b+c) = (a^b * (a^c))! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 16:01:00'!
testExponentiationIncreasing
	"Ordinal exponentiation is strictly increasing in the exponent for bases exceeing 1."
	| a b c |
	[a _ self newOrdinal. a > 1 asOrdinal] whileFalse.
	[b _ self newOrdinal.
	c _ self newOrdinal.
	b < c] whileFalse.
	self assert: a^b < (a^c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 16:02:06'!
testExponentiationNondecreasing
	"Ordinal exponentiation is nondecreasing in the base."
	| a b c |
	[a _ self newOrdinal.
	b _ self newOrdinal.
	a <= b] whileFalse.
	c _ self newOrdinal.
	self assert: a^c <= (b^c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 11:52:34'!
testFinite
	self assert: 0 asOrdinal isFinite.
	self assert: 1 asOrdinal isFinite.
	self assert: (0 to: 10) atRandom asOrdinal isFinite! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 11:55:39'!
testFiniteOrdering
	self assert: 0 asOrdinal < 1 asOrdinal.
	self assert: 1 asOrdinal < 42 asOrdinal! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 12:31:53'!
testIncludesIntegers
	self assert: (1 asOrdinal includes: 0).
	self assert: (42 asOrdinal includes: 0).
	self assert: (42 asOrdinal includes: 41).
	self deny: (42 asOrdinal includes: 42).
	self deny: (42 asOrdinal includes: -1).
	self assert: (1 asOrdinal exp includes: 0).
	self assert: (1 asOrdinal exp includes: 1000)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 12:43:15'!
testMultiplicationAssociativity
	| a b c |
	a _ self newOrdinal.
	b _ self newOrdinal.
	c _ self newOrdinal.
	self assert: a*b*c = (a*(b*c))! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/29/2021 11:41:17'!
testMultiplicationExamples
	| � |
	� _ 1 asOrdinal exp.
	self assert: � * (� + 1) = (�*� + �).
	self assert: (� + 1) * (� + 1) = (�*� + � + 1).
	self assert: 1 * � = �.
	self assert: � * 1 = �.
	self assert: � * 0 = 0 asOrdinal.
	self assert: 0 * � = 0 asOrdinal.
	self assert: �^� * � * � = (� ^ (� + 2))! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 13:53:43'!
testMultiplicationIncreasing
	"Ordinal multiplication is strictly increasing in the multiplier provided the multiplicand is nonzero."
	| a b c |
	[a _ self newOrdinal. a isZero not] whileFalse.
	[b _ self newOrdinal.
	c _ self newOrdinal.
	b < c] whileFalse.
	self assert: a * b < (a * c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 12:54:59'!
testMultiplicationLeftDistributivityOverAddition
	| a b c |
	a _ self newOrdinal.
	b _ self newOrdinal.
	c _ self newOrdinal.
	self assert: a*(b + c) = (a*b + (a*c))! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 13:51:25'!
testMultiplicationNondecreasing
	"Ordinal multiplication is nondecreasing in the multiplicand."
	| a b c |
	[a _ self newOrdinal.
	b _ self newOrdinal.
	a <= b] whileFalse.
	c _ self newOrdinal.
	self assert: a * c <= (b * c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/31/2021 11:26:03'!
testOmega
	| � |
	� _ 1 asOrdinal exp.
	self assert: � isOmega! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/31/2021 11:25:22'!
testOne
	self assert: 1 asOrdinal isOne.
	self assert: 1 asOrdinal = 0 asOrdinal exp! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 11:51:55'!
testZero
	self assert: 0 asOrdinal isZero! !