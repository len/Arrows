'From Cuis 6.0 [latest update: #5801] on 3 June 2023 at 9:58:34 pm'!
'Description '!
!provides: 'Tests-Transfinites' 1 3!
!requires: 'Transfinites' 1 0 nil!
SystemOrganization addCategory: #'Tests-Transfinites'!


!classDefinition: #InfinityTest category: #'Tests-Transfinites'!
TestCase subclass: #InfinityTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Transfinites'!
!classDefinition: 'InfinityTest class' category: #'Tests-Transfinites'!
InfinityTest class
	instanceVariableNames: ''!

!classDefinition: #OrdinalsTest category: #'Tests-Transfinites'!
TestCase subclass: #OrdinalsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Transfinites'!
!classDefinition: 'OrdinalsTest class' category: #'Tests-Transfinites'!
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

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 21:20:13'!
newFiniteOrdinal
	^ (0 to: 2) atRandom asOrdinal! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 21:20:36'!
newOrdinal
	2 atRandom = 1 ifTrue: [self newFiniteOrdinal exp].
	^ self newFiniteOrdinal! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 12:41:47'!
testAdditionAssociativity
	| a b c |
	a _ self newOrdinal.
	b _ self newOrdinal.
	c _ self newOrdinal.
	self assert: a + (b + c) = ((a + b) + c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2022 12:21:17'!
testAdditionExamples
	| ω |
	ω _ 1 asOrdinal exp.
	self assert: ω + 1 + 1 = (ω + 2).
	self assert: 1 + ω = ω.
	self assert: ω^5*3 + (ω^3*2) + (ω^3*3) = (ω^5*3 + (ω^3*5)).
	self assert: ω^5*3 + (ω^3*2) + (ω^4*2) = (ω^5*3 + (ω^4*2)).
"	self assert: ω^ω^2*2 + (ω^3*2) + (ω^4*2) = (ω^ω^2*2 + (ω^4*2))."
	self assert: 3 + (ω^5*3) = (ω^5*3).
	self assert: ω^5*3 + 2 + 1 = (ω^5*3 + 3)! !

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
	| ω a b |
	ω _ 1 asOrdinal exp.
	a _ ω^4*2 + (ω^2) + 3.
	b _ ω^3*5 + (ω*7).
	self assert: a + b = (ω^4*2 + (ω^3*5) + (ω*7)).
	self assert: a * b = (ω^7*5 + (ω^5*7))! !

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

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 21:20:46'!
testExponentiation
	| a b c |
	a := self newOrdinal.
	b := self newFiniteOrdinal.
	c := self newFiniteOrdinal.
	self assert: a^(b*c) = (a^b^c).
	self assert: a^(b+c) = (a^b * (a^c))! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 21:20:59'!
testExponentiationIncreasing
	"Ordinal exponentiation is strictly increasing in the exponent for bases exceeing 1."
	| a b c |
	[a := self newOrdinal. a > 1 asOrdinal] whileFalse.
	[b := self newFiniteOrdinal.
	c := self newFiniteOrdinal.
	b < c] whileFalse.
	self assert: a^b < (a^c)! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2023 21:21:09'!
testExponentiationNondecreasing
	"Ordinal exponentiation is nondecreasing in the base."
	| a b c |
	[a := self newOrdinal.
	b := self newOrdinal.
	a <= b] whileFalse.
	c := self newFiniteOrdinal.
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
	| ω |
	ω _ 1 asOrdinal exp.
	self assert: ω * (ω + 1) = (ω*ω + ω).
	self assert: (ω + 1) * (ω + 1) = (ω*ω + ω + 1).
	self assert: 1 * ω = ω.
	self assert: ω * 1 = ω.
	self assert: ω * 0 = 0 asOrdinal.
	self assert: 0 * ω = 0 asOrdinal.
	self assert: ω^ω * ω * ω = (ω ^ (ω + 2))! !

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
	| ω |
	ω _ 1 asOrdinal exp.
	self assert: ω isOmega! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/31/2021 11:25:22'!
testOne
	self assert: 1 asOrdinal isOne.
	self assert: 1 asOrdinal = 0 asOrdinal exp! !

!OrdinalsTest methodsFor: 'as yet unclassified' stamp: 'len 12/28/2021 11:51:55'!
testZero
	self assert: 0 asOrdinal isZero! !
