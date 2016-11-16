'From Cuis 4.2 of 25 July 2013 [latest update: #2847] on 16 November 2016 at 10:12:02.701382 am'!
'Description This package implements Tarski''s first-order theory of the reals with the Collin''s CAD (Cylindrical Algebraic Decomposition) algorithm.'!
!provides: 'Tarski' 1 3!
!requires: 'Mathematics' 1 240 nil!
!classDefinition: #SemialgebraicGeometryTest category: #'Tarski-Tests'!
TestCase subclass: #SemialgebraicGeometryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Tests'!
!classDefinition: 'SemialgebraicGeometryTest class' category: #'Tarski-Tests'!
SemialgebraicGeometryTest class
	instanceVariableNames: ''!

!classDefinition: #SemialgebraicInterval category: #'Tarski-CAD'!
RealInterval subclass: #SemialgebraicInterval
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-CAD'!
!classDefinition: 'SemialgebraicInterval class' category: #'Tarski-CAD'!
SemialgebraicInterval class
	instanceVariableNames: ''!

!classDefinition: #SemialgebraicSet category: #'Tarski-CAD'!
Domain subclass: #SemialgebraicSet
	instanceVariableNames: 'cells'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-CAD'!
!classDefinition: 'SemialgebraicSet class' category: #'Tarski-CAD'!
SemialgebraicSet class
	instanceVariableNames: ''!

!classDefinition: #RealRootFunction category: #'Tarski-CAD'!
Function subclass: #RealRootFunction
	instanceVariableNames: 'polynomials index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-CAD'!
!classDefinition: 'RealRootFunction class' category: #'Tarski-CAD'!
RealRootFunction class
	instanceVariableNames: ''!

!classDefinition: #CylindricalAlgebraicDecomposition category: #'Tarski-CAD'!
Object subclass: #CylindricalAlgebraicDecomposition
	instanceVariableNames: 'polynomials components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-CAD'!
!classDefinition: 'CylindricalAlgebraicDecomposition class' category: #'Tarski-CAD'!
CylindricalAlgebraicDecomposition class
	instanceVariableNames: ''!

!classDefinition: #SemialgebraicCell category: #'Tarski-CAD'!
Object subclass: #SemialgebraicCell
	instanceVariableNames: 'projection left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-CAD'!
!classDefinition: 'SemialgebraicCell class' category: #'Tarski-CAD'!
SemialgebraicCell class
	instanceVariableNames: ''!

!classDefinition: #BooleanConnective category: #'Tarski-Sentences'!
Object subclass: #BooleanConnective
	instanceVariableNames: 'left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'BooleanConnective class' category: #'Tarski-Sentences'!
BooleanConnective class
	instanceVariableNames: ''!

!classDefinition: #ConjunctionConnective category: #'Tarski-Sentences'!
BooleanConnective subclass: #ConjunctionConnective
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'ConjunctionConnective class' category: #'Tarski-Sentences'!
ConjunctionConnective class
	instanceVariableNames: ''!

!classDefinition: #DisjunctionConnective category: #'Tarski-Sentences'!
BooleanConnective subclass: #DisjunctionConnective
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'DisjunctionConnective class' category: #'Tarski-Sentences'!
DisjunctionConnective class
	instanceVariableNames: ''!

!classDefinition: #ImplicationConnective category: #'Tarski-Sentences'!
BooleanConnective subclass: #ImplicationConnective
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'ImplicationConnective class' category: #'Tarski-Sentences'!
ImplicationConnective class
	instanceVariableNames: ''!

!classDefinition: #PolynomialEquation category: #'Tarski-Sentences'!
Object subclass: #PolynomialEquation
	instanceVariableNames: 'polynomial'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'PolynomialEquation class' category: #'Tarski-Sentences'!
PolynomialEquation class
	instanceVariableNames: ''!

!classDefinition: #PolynomialInequality category: #'Tarski-Sentences'!
Object subclass: #PolynomialInequality
	instanceVariableNames: 'polynomial'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'PolynomialInequality class' category: #'Tarski-Sentences'!
PolynomialInequality class
	instanceVariableNames: ''!

!classDefinition: #QuantifiedSentence category: #'Tarski-Sentences'!
Object subclass: #QuantifiedSentence
	instanceVariableNames: 'prefix matrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'QuantifiedSentence class' category: #'Tarski-Sentences'!
QuantifiedSentence class
	instanceVariableNames: ''!

!classDefinition: #Quantifier category: #'Tarski-Sentences'!
Object subclass: #Quantifier
	instanceVariableNames: 'indeterminate'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'Quantifier class' category: #'Tarski-Sentences'!
Quantifier class
	instanceVariableNames: ''!

!classDefinition: #ExistentialQuantifier category: #'Tarski-Sentences'!
Quantifier subclass: #ExistentialQuantifier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'ExistentialQuantifier class' category: #'Tarski-Sentences'!
ExistentialQuantifier class
	instanceVariableNames: ''!

!classDefinition: #UniversalQuantifier category: #'Tarski-Sentences'!
Quantifier subclass: #UniversalQuantifier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tarski-Sentences'!
!classDefinition: 'UniversalQuantifier class' category: #'Tarski-Sentences'!
UniversalQuantifier class
	instanceVariableNames: ''!


!SemialgebraicInterval commentStamp: '<historical>' prior: 0!
My instances are components of a cell complex of R^1. See SemialgebraicCell.
!

!SemialgebraicSet commentStamp: '<historical>' prior: 0!
My instances are semialgebraic sets, i.e. subsets of R^n expressable as finite union and intersection of sets of the form:
	{(x1,...,xn) | sign(f(x1,...,xn)) = s}
where f is a polynomial in R[x1,...,xn] and s is 1, -1 or 0.

Semialgebraic sets are closed under union, intersection, complementation and projection. Furthermore, the boundary and closure of a semialgebraic set is again a semialgebraic set. Semialgebraic sets are the natural generalization of algebraic sets (zeros of polynomials) in such a way to make them be closed under projection.

This implementation is based on the Cylindrical Algebraic Decomposition process. Each SemialgebraicSet is made of the union of SemialgebraicCells (or SemialgebraicIntervals in R^1). For more details on the implementation of semialgebraic sets, refer to the comment of class CylindricalAlgebraicDecomposition.

Some examples of semialgebraic sets are:
	open unitary circle in R^2:
		SemialgebraicSet circle.
	unitary circunference in R^2:
		SemialgebraicSet circunference.
!

!RealRootFunction commentStamp: '<historical>' prior: 0!
My instances are defined by an integer index and a set of rational multivariate polynomials. 

The RealRootFunction with index k and polynomial f in n+1 indeterminates represents the continuous real function going from a connected subset of R^n (a semialgebraic set) to R and whose value at (a1,...,an) is the k-th real root of f(a1,...,an, x) as univariate polynomial in x.

A RealRootFunction with two or more defining polynomials is equivalent to the one defined by the product of all the polynomials.!

!CylindricalAlgebraicDecomposition commentStamp: '<historical>' prior: 0!
My instances decompose R^n into connected components (SemialgebraicCells) such that a given set of polynomials keeps sign-invariant over each of the components. This decomposition is called Cylindrical Algebraic Decomposition (CAD), and what is obtainded is called a Cell Complex of R^n. Each of the components is a SemialgebraicCell (or SemialgebraicInterval in R^1).

The univariate case is easy. Suppose we have to find the CAD of R with respect to the polynomial set {f1, ..., fn}. Then we find all the real roots of f1, ..., fn, a1 < a2 < ... < am, and the CAD we were looking for is:
	[-infinity, a1), [a1, a1], (a1, a2), [a2, a2], (a2, a3), ... [am, am], (am, +infinity].
All this can be effectively computed when using Sturm theory when the polynomials have rational coefficients. See the comments in AlgebraicNumber and AlgebraicPolynomialRootFinder. Each component can be asked for a sample point in it, and the sample points can be constructed in this way:
	the sample point of [-infinity, a1) is a1 - 1;
	the sample point of (am, +infinity] is am + 1;
	the sampel point of [ai, ai] is ai;
	and the sample point of (ai, a{i+1}) is ai + a{i+1} / 2 (the middle).

An example: decide wether the sentence x^2 + 1 > 0 is true or false. First we find the CAD of R with respect to {x^2 + 1}. The roots of x^2 + 1 are -1 and 1, so the CAD is
	[-infinity, -1), [-1, -1], (-1, 1), [1, 1] and (1, +infinity]
and the respective sample points are
	-2, -1, 0, 1 and 2.
We know that the polynomial x^2 + 1 is sign invariant over each of the components of the CAD, so if it's positive in all the components, it's positive over all R. So we evaluate x^2 + 1 at each of the sample points and check that it's positive, and then we can conclude that the sentence is true.

The construction of the CAD in the multivariate case is performed inductively. Suppose we want the CAD of R^k with respect to the set of real polynomials F = {f1, ..., fn} in k indeterminates. Then we produce another set of polynomials Phi(F) = {g1, ..., gm} in k-1 indeterminates that have some nice properties that makes this construction valid (for more information on this see Algorithmic Algebra, B. Mishra, Springer-Verlag, 1993). Now we inductively compute K', the CAD of R^{k-1} with respect to Phi(F), and name the components C1, ... Ct. Each component can be asked for a sample point in it. Let f = f1 f2 ... fn (the product of the polynomials in F). Furthermore, we define the functions zj:R^{k-1} -> R to be at the point (a1, ..., a{k-1}) the j-th root of f(a1, ..., a{k-1}, x) as univariate polynomial in x. Phi(F) is defined in such a way to make the number of roots of f be invariant over each C in K', and to make each zj to be a continuous function over each C in K'. Finally we can construct K, the CAD of R^k. Each cell in K will be of two kinds:
	1. For each cell C in K', C x [-infinity, +infinity].
	2. For each cell C in K', suppose the number of roots of f are r. Then we have the following cells:
		C0* = {(p, x) | x < z1(p), p in C}
		C1 = {(p, x) | x = z1(p), p in C}
		C1* = {(p, x) | z1(p < x < z2(p), p in C}
		C2 = {(p, x) | x = z2(p), p in C}
			...
		Cm = {(p, x) | x = zm(p), p in C}
		Cm* = {(p, x) | zm(p) < x, p in C}.
Sample points for each of the cells constructed can be easyly computed.

Finally, we cite the Collin's Theorem. Given a finite set of multivariate polynomials F in Q[x1,...,xn] we can effectively construct the followings:
- A F-sign-invariat cylindrical algebraic decomposition K of R^n into semialgebraic connected cells. Each cell C in K is homeomorphic to R^d for some 0 <= d <= n;
- A sample algebraic point in each cell in K and defining polynomials for each sample point;
- Quantifier-free defining sentences for each cell C in K.

!

!SemialgebraicCell commentStamp: '<historical>' prior: 0!
My instances are components of a cell complex of R^n, where n>1. For the case n=1 SemialgebraicInterval is used. (See the comments in the class SemialgebraicSet and CylindricalAlgebraicDecomposition).

Each SemialgebraicCell in R^n is represented as the extension of a SemialgebraicCell or SemialgebraicInterval in R^{n-1}.
!

!BooleanConnective commentStamp: '<historical>' prior: 0!
I am an abstract superclass for boolean connectives that are employed in the building of Tarski sentences. My concrete subclases are ConjunctionConnective (AND), DisjunctionConnective (OR) and ImplicationConective.
!

!ConjunctionConnective commentStamp: 'len 5/31/2016 05:54' prior: 0!
My instances are conjunctions of two Tarski sentences.

For an example try this:
	| Fxy x y |
	Fxy _ Rationals new polynomialsIn: #(x y).
	x _ Fxy x.
	y _ Fxy y.
	ConjunctionConnective left: (x squared + y squared gneq: 1) right: (x eq: 1).
!

!DisjunctionConnective commentStamp: '<historical>' prior: 0!
My instances are disjunctions of two Tarski sentences.

For an example try this:
	| Fxy x y |
	Fxy _ Rationals new polynomialsIn: #(x y).
	x _ Fxy x.
	y _ Fxy y.
	DisjunctionConnective
		left: (x squared + y squared lneq: 1) right: (x squared + y squared eq: 1).
!

!ImplicationConnective commentStamp: '<historical>' prior: 0!
My instances are implications whose predicate and conclusion are Tarski sentences.

For an example try this:
	| Fxy x y |
	Fxy _ Rationals new polynomialsIn: #(x y).
	x _ Fxy x.
	y _ Fxy y.
	ImplicationConnective left: (x squared + y squared eq: 0) right: (x eq: 0).
!

!PolynomialEquation commentStamp: '<historical>' prior: 0!
My instances are polynomial equations.

For an example try:
	| Fxy x y |
	Fxy _ Rationals new polynomialsIn: #(x y).
	x _ Fxy x.
	y _ Fxy y.
	PolynomialEquation polynomial: x*x + (y*y) - 1.

which is equivalent to:
	| Fxy x y |
	Fxy _ Rationals new polynomialsIn: #(x y).
	x _ Fxy x.
	y _ Fxy y.
	x*x + (y*y) eq: 1.
!

!PolynomialInequality commentStamp: '<historical>' prior: 0!
My instances are polynomial inequalities.

For an example try:
	| Fxy x y |
	Fxy _ Rationals new polynomialsIn: #(x y).
	x _ Fxy x.
	y _ Fxy y.
	PolynomialInequality polynomial: x*x + (y*y) - 1.

which is equivalent to:
	| Fxy x y |
	Fxy _ Rationals new polynomialsIn: #(x y).
	x _ Fxy x.
	y _ Fxy y.
	x*x + (y*y) lneq: 1.
!

!QuantifiedSentence commentStamp: '<historical>' prior: 0!
My instances are Tarski sentences represented by a list of quantifiers (prefix) and by the sentence subject of the quantification (matrix).

For an example try this:
	| Qxy x y |
	Qxy _ Rationals new polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	(QuantifiedSentence matrix: (y squared gneq: x))
		add: (ExistentialQuantifier indeterminate: 1);
		add: (UniversalQuantifier indeterminate: 2);
		yourself.

Tarski sentences can be tested for truth (just sending the message #truth). The algorithm uses the mechinery of Cylindrical Algebraic Decomposition (CAD) of R^n and Semialgebraic Sets. and has a very bad time complexity, but it's useful in small examples.
!

!Quantifier commentStamp: '<historical>' prior: 0!
I am an abstract superclass for real quantifiers that are employed in the building of Tarski sentences. My concrete subclases are UniversalQuantifier and ExistentialQuantifier.
!

!ExistentialQuantifier commentStamp: '<historical>' prior: 0!
My instances are existential quantifiers in one indeterminate.

For an example try:
	ExistentialQuantifier indeterminate: 3.
!

!UniversalQuantifier commentStamp: '<historical>' prior: 0!
My instances are universal quantifiers in one indeterminate.

For an example try:
	UniversalQuantifier indeterminate: 3.
!

!Polynomial methodsFor: '*Tarski' stamp: 'len 11/22/97 19:20'!
eq: anObject
	"Answer the equation 'self = anObject'."

	^ PolynomialEquation polynomial: self - anObject! !

!Polynomial methodsFor: '*Tarski' stamp: 'len 12/18/97 19:18'!
geq: anObject
	"Answer the inequality 'self >= anObject'."

	^ (self gneq: anObject) | (self eq: anObject)! !

!Polynomial methodsFor: '*Tarski' stamp: 'len 1/12/98 23:59'!
gneq: anObject
	"Answer the inequality 'self > anObject'."

	^ self negated lneq: anObject negated! !

!Polynomial methodsFor: '*Tarski' stamp: 'len 12/18/97 19:18'!
leq: anObject
	"Answer the inequality 'self <= anObject'."

	^ (self lneq: anObject) | (self eq: anObject)! !

!Polynomial methodsFor: '*Tarski' stamp: 'len 12/18/97 19:17'!
lneq: anObject
	"Answer the inequality 'self < anObject'."

	^ PolynomialInequality polynomial: self - anObject! !

!Polynomial methodsFor: '*Tarski' stamp: 'len 12/18/97 19:18'!
neq: anObject
	"Answer the inequation 'self ~= anObject'."

	^ (self lneq: anObject) | (self gneq: anObject)! !

!SemialgebraicGeometryTest methodsFor: 'as yet unclassified' stamp: 'len 7/15/2016 18:37'!
testCAD
	| Qxy x y S |
	Qxy _ QQ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	S _ SemialgebraicSet cad: {x squared + y squared - 4. (x - 1) * (y - 1) - 1}.
	self assert: S cells size = 13.
	self assert: (S cells count: [:each| each dimension = 0]) = 2.
	self assert: (S cells count: [:each| each dimension = 1]) = 6.
	self assert: (S cells count: [:each| each dimension = 2]) = 5! !

!SemialgebraicGeometryTest methodsFor: 'as yet unclassified' stamp: 'len 6/2/2016 03:56'!
testDimension
	self assert: SemialgebraicSet circle dimension = 2.
	self assert: SemialgebraicSet circunference dimension = 1.
	self assert: SemialgebraicSet sphere dimension = 2.
	self assert: SemialgebraicSet ball dimension = 3! !

!SemialgebraicGeometryTest methodsFor: 'as yet unclassified' stamp: 'len 6/3/2016 20:06'!
testOpenClosed
	| S |
	S _ SemialgebraicSet circle.
	self assert: S isOpen.
	self assert: S isClosed not.
	S _ SemialgebraicSet circunference.
	self assert: S isClosed.
	self assert: S isOpen not.
	S _ SemialgebraicSet ball.
	self assert: S isOpen.
	self assert: S isClosed not.
	S _ SemialgebraicSet sphere.
	self assert: S isClosed.
	self assert: S isOpen not! !

!SemialgebraicGeometryTest methodsFor: 'as yet unclassified' stamp: 'len 6/2/2016 04:39'!
testSentence
	| S f |
	S _ SemialgebraicSet sphere.
	f _ S sentence.
	self assert: (S cells allSatisfy: [:each| f isSolution: each sample])! !

!SemialgebraicInterval methodsFor: 'operations' stamp: 'len 1/8/98 20:35'!
boundary
	"Answer the boundary of the receiver."

	^ SemialgebraicSet new
		add: (self class between: self left and: self left);
		add: (self class between: self right and: self right);
		yourself! !

!SemialgebraicInterval methodsFor: 'operations' stamp: 'len 1/8/98 20:29'!
closure
	"Answer the closure of the receiver."

	^ self boundary copy add: self; yourself! !

!SemialgebraicInterval methodsFor: 'operations' stamp: 'len 1/16/98 21:58'!
interior
	"Answer the interior of the receiver."

	^ self closure copyWithoutAll: self boundary! !

!SemialgebraicInterval methodsFor: 'testing' stamp: 'len 2/6/98 13:33'!
isCylindricalOver: aCell
	"Answer true if the receiver is cylindrical over aCell."

	^ self = aCell! !

!SemialgebraicInterval methodsFor: 'accessing' stamp: 'len 1/17/98 13:49'!
polynomials
	"Answer the polynomials defining the receiver."

	| answer |
	answer _ Set new.
	self left isInfinity ifFalse: [answer add: self left polynomial].
	self right isInfinity ifFalse: [answer add: self right polynomial].
	^ answer! !

!SemialgebraicInterval methodsFor: 'printing' stamp: 'len 6/2/2016 02:09'!
printOn: aStream
	"aStream nextPut: ${; print: self sentence; nextPut: $}"
	self dimension >= 0
		ifFalse: [aStream nextPut: Character emptySet. ^ self].
	self dimension = 0
		ifTrue: [aStream nextPut: ${; print: self sample; nextPut: $}. ^ self].
	super printOn: aStream! !

!SemialgebraicInterval methodsFor: 'accessing' stamp: 'len 2/6/98 12:38'!
projection
	"Answer the projection of the receiver."
	^ nil! !

!SemialgebraicInterval methodsFor: 'accessing' stamp: 'len 6/1/2016 23:32'!
rank
	"Answer the number of indeterminates involved to define the receiver."
	^ 1! !

!SemialgebraicInterval methodsFor: 'accessing' stamp: 'len 11/5/2016 09:59'!
sample
	"Answer a sample point in the receiver."

	self isEmpty ifTrue: [^ self error: 'empty interval'].
	self left = self right ifTrue: [^ self left].
	(self isLeftBounded and: [self isRightBounded])
		ifTrue: [[self left rightBound <= self right leftBound]
					whileFalse: [self left refineWith: self right].
				^ self left rightBound + self right leftBound / 2].
	self isLeftBounded ifTrue: [^ self left rightBound + 1].
	self isRightBounded ifTrue: [^ self right leftBound - 1].
	^ 0! !

!SemialgebraicInterval methodsFor: 'accessing' stamp: 'len 3/18/2016 06:44'!
sentence
	"Answer a quantifier-free defining sentence for the receiver."

	| answer sample sign sentence |
	sample _ self sample.
	self polynomials do: [:each|
		sentence :=
			(sign _ (each value: sample) sign) = 0
				ifTrue: [each eq: 0]
				ifFalse: [sign = -1 ifTrue: [each lneq: 0] ifFalse: [each gneq: 0]].
		answer _ answer isNil ifTrue: [sentence] ifFalse: [answer & sentence]].
	^ answer! !

!SemialgebraicInterval class methodsFor: 'instance creation' stamp: 'len 12/20/97 02:35'!
between: leftBound and: rightBound
	"Answer a new instance of the receiver representing the
	univariate cell which is bounded by the given bounds."

	(leftBound = rightBound or: [leftBound isInfinity and: [rightBound isInfinity]])
		ifTrue: [^ self closed: leftBound closed: rightBound].
	leftBound isInfinity
		ifTrue: [^ self closed: leftBound open: rightBound].
	rightBound isInfinity
		ifTrue: [^ self open: leftBound closed: rightBound].
	^ self open: leftBound open: rightBound! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 9/1/2016 07:54'!
/\ aSemialgebraicSet
	"Meet."
	^ self ˜ aSemialgebraicSet! !

!SemialgebraicSet methodsFor: 'comparing' stamp: 'len 6/2/2016 05:23'!
= aSemialgebraicSet
	self == aSemialgebraicSet ifTrue: [^ true].
	(aSemialgebraicSet isKindOf: SemialgebraicSet) ifFalse: [^ false].
	self cells do: [ :each | (aSemialgebraicSet cells includes: each) ifFalse: [^ false]].
	aSemialgebraicSet cells do: [ :each | (self cells includes: each) ifFalse: [^ false]].
	^ true! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 6/2/2016 08:11'!
\ aSemialgebraicSet
	"Answer the difference of the receiver with the argument,"
	^ self copyWithoutAll: aSemialgebraicSet cells! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 9/1/2016 07:51'!
\/ aSemialgebraicSet
	"Join."
	^ self ™ aSemialgebraicSet! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 9/1/2016 07:54'!
˜ aSemialgebraicSet
	self flag: #fix. "this is wrong"
	^ self class cells: self cells ˜ aSemialgebraicSet cells! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 9/1/2016 07:56'!
™ aSemialgebraicSet
	^ self class cells: self cells ™ aSemialgebraicSet cells! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 1/9/98 21:34'!
add: aCell
	"Add the argument to the cells of the receiver. Answer the argument."

	^ self cells add: aCell! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 12/14/97 21:48'!
addAll: aCollection
	"Add the elements in the argument to the cells of the receiver. Answer the argument."

	aCollection do: [ :each | self add: each].
	^ aCollection! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 6/2/2016 08:13'!
boundary
	| answer |
	answer _ self class new.
	self cells do: [:each| answer addAll: each boundary cells].
	^ answer! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 6/2/2016 04:48'!
boundingBox
	"Answer a tight bounding box of the receiver, assuming it's bounded."
	^ self notYetImplemented! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 5/30/2016 08:48'!
cells
	^ cells! !

!SemialgebraicSet methodsFor: 'accessing-private' stamp: 'len 12/14/97 21:46'!
cells: aCollection
	cells _ aCollection! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 6/2/2016 08:12'!
closure
	| answer |
	answer _ self class new.
	self cells do: [ :each | answer addAll: each closure cells].
	^ answer! !

!SemialgebraicSet methodsFor: 'iterating' stamp: 'len 2/6/98 12:37'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect the resulting values into a collection like the receiver. Answer 
	the new collection."

	| newCollection |
	newCollection _ self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 2/2/2016 22:26'!
complement
	^ self notYetImplemented! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 11/16/2016 10:09'!
connectivityGraph
	"Answer the connectivity graph of the cell complex of the receiver."
	| answer |
	answer _ Graph unordered.
	self cells do: [:v|
		| vBar |
		vBar _ v closure.
		self cells do: [:u|
			(vBar cells includes: u) ifTrue: [answer addEdgeFrom: u to: v]]].
	^ answer! !

!SemialgebraicSet methodsFor: 'testing' stamp: 'len 6/2/2016 05:18'!
connects: aTuple with: anotherTuple
	"Answer true if there exists a path in the receiver connecting the two points."

	| a b |
	a _ self cells detect: [:one| one includes: aTuple] ifNone: [^ false].
	b _ self cells detect: [:one| one includes: anotherTuple] ifNone: [^ false].
	self flag: #fix. "inefficient"
	self connectivityGraph componentsDo: [:each|
		(each includes: a)
			ifTrue: [^ each includes: b]
			ifFalse: [(each includes: b) ifTrue: [^ false]]].
	^ false! !

!SemialgebraicSet methodsFor: 'copying' stamp: 'len 6/2/2016 05:20'!
copy
	^ self class cells: self cells copy! !

!SemialgebraicSet methodsFor: 'copying' stamp: 'len 6/2/2016 05:20'!
copyWithoutAll: aCollection
	"Answer a copy of the receiver without all the cells in aCollection."

	^ self class cells: (self cells select: [:each| (aCollection includes: each) not])! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 6/2/2016 05:03'!
cylinderAt: aCell
	"Answer the subset of the receiver cylindrical over aCell."

	^ self class cells: (self cells select: [:each| each isCylindricalOver: aCell])! !

!SemialgebraicSet methodsFor: 'iterating' stamp: 'len 1/10/98 00:24'!
detect: aBlock ifNone: exceptionBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true. If none 
	evaluate to true, then evaluate the argument, exceptionBlock."

	self do: [:each | (aBlock value: each) ifTrue: [^ each]].
	^ exceptionBlock value! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 6/2/2016 04:42'!
dimension
	"Answer the topological dimension of the receiver."
	self isEmpty ifTrue: [^ -1].
	^ self cells max: [:each| each dimension]! !

!SemialgebraicSet methodsFor: 'iterating' stamp: 'len 8/26/2016 06:54'!
do: aBlock
	self cells do: aBlock! !

!SemialgebraicSet methodsFor: 'comparing' stamp: 'len 6/28/2016 09:36'!
hash
	^ self cells inject: self class hash into: [:answer :each| answer + each hash]! !

!SemialgebraicSet methodsFor: 'testing' stamp: 'len 6/2/2016 05:19'!
includes: anObject
	(anObject is: #Tuple)
		ifTrue: [^ self cells anySatisfy: [:any| any includes: anObject]].
	^ false! !

!SemialgebraicSet methodsFor: 'operations' stamp: 'len 6/2/2016 08:13'!
interior
	^ self closure \ self boundary! !

!SemialgebraicSet methodsFor: 'testing' stamp: 'len 6/2/2016 04:07'!
isBounded
	^ self cells allSatisfy: [:each| each isBounded]! !

!SemialgebraicSet methodsFor: 'testing' stamp: 'len 6/2/2016 04:43'!
isClosed
	^ self cells allSatisfy: [:each| each isClosed]! !

!SemialgebraicSet methodsFor: 'testing' stamp: 'len 6/2/2016 04:49'!
isConnected
	^ self connectivityGraph isConnected! !

!SemialgebraicSet methodsFor: 'testing' stamp: 'len 2/6/98 12:35'!
isEmpty
	"Answer true if the receiver is empty."

	^ self cells isEmpty! !

!SemialgebraicSet methodsFor: 'testing' stamp: 'len 6/2/2016 04:48'!
isFinite
	^ self cells allSatisfy: [:each| each isFinite]! !

!SemialgebraicSet methodsFor: 'testing' stamp: 'len 6/2/2016 04:43'!
isOpen
	^ self cells allSatisfy: [:each| each isOpen]! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 6/2/2016 04:01'!
polynomials
	^ self cells inject: Set new into: [:answer :each| answer addAll: each polynomials; yourself]! !

!SemialgebraicSet methodsFor: 'printing' stamp: 'len 11/10/2016 08:47'!
printOn: aStream
	"aStream nextPut: ${; print: self sentence; nextPut: $}"	
	self isEmpty ifTrue: [aStream nextPut: Character emptySet].
	self cells do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ' U ']! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 6/1/2016 22:58'!
projectionAt: anInteger
	"Answer the first anInteger components of the receiver."
	| answer |
	answer _ self class new.
	self cells do: [:each| answer addAll: (each projectionAt: anInteger)].
	^ answer! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 6/2/2016 04:25'!
rank
	^ self cells anyOne rank! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 6/2/2016 04:41'!
sample
	"Answer a sample point in the receiver."

	^ self cells anyOne sample! !

!SemialgebraicSet methodsFor: 'iterating' stamp: 'len 1/16/98 22:08'!
select: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for 
	which aBlock evaluates to true. Answer the new collection."

	| newCollection |
	newCollection _ self species new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection! !

!SemialgebraicSet methodsFor: 'accessing' stamp: 'len 7/15/2016 18:38'!
sentence
	"Answer a quantifier-free defining sentence for the receiver."

	self isEmpty ifTrue: [^ QQ polynomials x squared lneq: 0].
	^ self cells collect: [:each| each sentence] andFold: [:a :b| a | b]! !

!SemialgebraicSet class methodsFor: 'examples' stamp: 'len 7/15/2016 18:38'!
ball
	"Answer the open unitary ball in R^3."
	"SemialgebraicSet ball."
	| Qxyz x y z |
	Qxyz _ QQ polynomialsIn: #(x y z).
	x _ Qxyz x.
	y _ Qxyz y.
	z _ Qxyz z.
	^ self cad: {x squared + y squared + z squared - 1}
		:: select: [ :each | each sample norm2 < 1]! !

!SemialgebraicSet class methodsFor: 'instance creation' stamp: 'len 5/30/2016 08:44'!
cad: aPolynomialSet
	"Answer the CAD (Cylindrical Algebraic Decomposition) of R^n by the polynomials in aPolynomialSet."

	^ self cells: (CylindricalAlgebraicDecomposition polynomials: aPolynomialSet) components! !

!SemialgebraicSet class methodsFor: 'instance creation' stamp: 'len 5/30/2016 08:46'!
cells: aCollection
	^ super new cells: aCollection! !

!SemialgebraicSet class methodsFor: 'examples' stamp: 'len 7/15/2016 18:38'!
circle
	"Answer the open unitary circle in R^2."
	"SemialgebraicSet circle."
	| Qxy x y |
	Qxy _ QQ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	^ self cad: {x squared + y squared - 1}
		:: select: [ :each | each sample norm2 < 1]! !

!SemialgebraicSet class methodsFor: 'examples' stamp: 'len 7/15/2016 18:38'!
circunference
	"Answer the open unitary circunference in R^2."
	"SemialgebraicSet circunference."
	| Qxy x y |
	Qxy _ QQ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	^ self cad: {x squared + y squared - 1}
		:: select: [ :each | each sample norm2 = 1]! !

!SemialgebraicSet class methodsFor: 'examples' stamp: 'len 7/15/2016 18:38'!
closedCircle
	"Answer the closed unitary circle in R^2."
	| Qxy x y |
	Qxy _ QQ polynomialsIn: #(x y).
	x _ Qxy x.
	y _ Qxy y.
	^ self cad: {x squared + y squared - 1}
		:: select: [ :each | each sample norm2 <= 1]! !

!SemialgebraicSet class methodsFor: 'instance creation' stamp: 'len 5/30/2016 08:46'!
new
	^ self cells: Set new! !

!SemialgebraicSet class methodsFor: 'examples' stamp: 'len 7/15/2016 18:38'!
sphere
	"Answer the open unitary sphere in R^3."
	"SemialgebraicSet sphere."
	| Qxyz x y z |
	Qxyz _ QQ polynomialsIn: #(x y z).
	x _ Qxyz x.
	y _ Qxyz y.
	z _ Qxyz z.
	^ self cad: {x squared + y squared + z squared - 1}
		:: select: [ :each | each sample norm2 = 1]! !

!RealRootFunction methodsFor: 'as yet unclassified' stamp: 'len 5/30/2016 06:44'!
= aPolynomialRootFunction
	(aPolynomialRootFunction isKindOf: self class) ifFalse: [^ false].
	^ self polynomial = aPolynomialRootFunction polynomial and: [self index = aPolynomialRootFunction index]! !

!RealRootFunction methodsFor: 'accessing' stamp: 'len 4/17/2016 06:42'!
codomain
	^ RealAlgebraicNumbers new! !

!RealRootFunction methodsFor: 'accessing' stamp: 'len 5/30/2016 06:58'!
domain
	^ RealAlgebraicNumbers new ** (self polynomials anyOne species rank - 1)! !

!RealRootFunction methodsFor: 'as yet unclassified' stamp: 'len 6/28/2016 09:30'!
hash
	^ self polynomial hash hashMultiply + self index hash! !

!RealRootFunction methodsFor: 'accessing' stamp: 'len 3/17/2016 23:52'!
index
	^ index! !

!RealRootFunction methodsFor: 'accessing-private' stamp: 'len 12/14/97 21:11'!
index: anInteger
	index _ anInteger! !

!RealRootFunction methodsFor: 'accessing' stamp: 'len 5/30/2016 06:38'!
polynomial
	^ self polynomials product! !

!RealRootFunction methodsFor: 'accessing' stamp: 'len 3/17/2016 23:52'!
polynomials
	^ polynomials! !

!RealRootFunction methodsFor: 'accessing-private' stamp: 'len 5/30/2016 06:01'!
polynomials: aPolynomialSet
	(aPolynomialSet allSatisfy: [:each| each species scalars class == Rationals]) ifFalse: [self error: 'not polynomials in Q[x]'].
	polynomials _ aPolynomialSet! !

!RealRootFunction methodsFor: 'as yet unclassified' stamp: 'len 5/30/2016 07:02'!
printOn: aStream
	polynomials do: [:f|
		aStream nextPut: $(; print: f; nextPut: $)].
	aStream space; print: index; nextPutAll: '-root'.! !

!RealRootFunction methodsFor: 'operations' stamp: 'len 6/1/2016 22:48'!
value: aTuple
	| roots f polys Rx x |
	aTuple size = self domain rank ifFalse: [self error: 'the arity of the argument should match the number of indeterminates'].
	roots _ Set new.
	polys _ Set new.
	Rx _ RealAlgebraicNumbers new polynomials.
	x _  Rx x.
	self polynomials do: [:each|
		f _ (each value: ((aTuple collect: [:t| Rx constant: t]), x))
				coefficientsApply: [:coeff|
					coeff isAlgebraic ifTrue: [coeff asRational] ifFalse: [coeff]].
		polys do: [:some| f _ f // (f gcd: some)].
		roots addAll: (RealRootFinder polynomial: f) roots.
		polys add: f].
	^ roots asSortedCollection at: self index! !

!RealRootFunction class methodsFor: 'instance creation' stamp: 'len 3/17/2016 04:21'!
polynomial: aPolynomial index: anInteger
	"Answer a new instance of the receiver representing the anInteger-th root function for the polynomial aPolynomial."

	^ self polynomials: {aPolynomial} index: anInteger! !

!RealRootFunction class methodsFor: 'instance creation' stamp: 'len 12/14/97 21:50'!
polynomials: aPolynomialSet index: anInteger
	"Answer a new instance of the receiver representing the
	anInteger-th root function for the polynomials in aPolynomialSet."

	^ self new polynomials: aPolynomialSet; index: anInteger! !

!CylindricalAlgebraicDecomposition methodsFor: 'accessing' stamp: 'len 5/30/2016 03:40'!
components
	components isNil ifTrue: [self decompose].
	^ components! !

!CylindricalAlgebraicDecomposition methodsFor: 'private' stamp: 'len 6/7/2016 00:21'!
decompose
	"Perform the cylindrical algebraic decomposition."

	| x Rx cells roots point functions f |
	self dimension < 1 ifTrue: [^ components _ #()].
	self dimension = 1 ifTrue: [^ self decomposeUnivariate].
	Rx _ RealAlgebraicNumbers new polynomials.
	x _ Rx x.
	cells _ OrderedCollection new.
	(self class polynomials: self phi) do: [:each|
		point _ ((each sample collect: [:s| Rx constant: s]), x).
		roots _ Set new.
		self polynomials do: [:some|
			f _ (some value: point) coefficientsApply: [:coeff|
					coeff isAlgebraic ifTrue: [coeff asRational] ifFalse: [coeff]].
			roots addAll: (RealRootFinder polynomial: f) roots].
		roots isEmpty
			ifTrue: [cells add:
						(SemialgebraicCell
							projection: each
							between: Infinity negative and: Infinity positive)]
			ifFalse: [functions _ (1 to: roots size)
						collect: [ :some | RealRootFunction
											polynomials: self polynomials index: some].
					cells add: (SemialgebraicCell
								projection: each
								between: Infinity negative
								and: functions first).
					1 to: functions size - 1 do: [ :index |
						cells
							add: (SemialgebraicCell
									projection: each
									between: (functions at: index)
									and: (functions at: index));
							add: (SemialgebraicCell
									projection: each
									between: (functions at: index)
									and: (functions at: index + 1))].
					cells add: (SemialgebraicCell
								projection: each
								between: functions last
								and: functions last).
					cells add: (SemialgebraicCell
								projection: each
								between: functions last
								and: Infinity positive)]].
	^ components _ cells! !

!CylindricalAlgebraicDecomposition methodsFor: 'private' stamp: 'len 5/30/2016 18:57'!
decomposeUnivariate
	"Perform the cylindrical algebraic decomposition in the univariate case.
	It's easy. Suppose we have to find the CAD of R with respect to the polynomial set {f1, ..., fn}.
	Then we find all the real roots of f1, ..., fn, a1 < a2 < ... < am, and the CAD we were looking for is:
		[-infinity, a1), [a1, a1], (a1, a2), [a2, a2], ... [am, am], (am, +infinity]."

	| roots intervals |
	roots _ Set new.
	self polynomials do: [ :each |
		roots addAll: (RealRootFinder polynomial: each) roots].
	roots isEmpty
		ifTrue: [^ components _ {SemialgebraicInterval between: Infinity negative and: Infinity positive}].
	roots _ roots asSortedCollection.
	intervals _ OrderedCollection new.
	intervals add: (SemialgebraicInterval between: Infinity negative and: roots first).
	1 to: roots size - 1 do: [ :each |
		intervals
			add: (SemialgebraicInterval between: (roots at: each) and: (roots at: each));
			add: (SemialgebraicInterval between: (roots at: each) and: (roots at: each + 1))].
	intervals
		add: (SemialgebraicInterval between: roots last and: roots last);
		add: (SemialgebraicInterval between: roots last and: Infinity positive).
	^ components _ intervals! !

!CylindricalAlgebraicDecomposition methodsFor: 'accessing' stamp: 'len 5/30/2016 05:52'!
dimension
	"Answer the dimension of the ambient of the receiver."
	
	^ polynomials anyOne species rank! !

!CylindricalAlgebraicDecomposition methodsFor: 'iterating' stamp: 'len 12/12/97 20:11'!
do: aBlock
	"Enumerate the components of the receiver."

	self components do: aBlock! !

!CylindricalAlgebraicDecomposition methodsFor: 'private' stamp: 'len 11/5/2016 10:07'!
phi
	"Answer a set of polynomials in n-1 indeterminates which completly characterize the connected maximal F-delineable subsets of R^{n-1}. (where F is the polynomial set of the receiver and n is the number of indeterminates). See >>decompose."

	| answer f g |
	answer _ Set new.
	self polynomials do: [:each|
		| f` |
		f _ each asUnivariate.
		answer addAll: f coefficients.
		f` _ f derivative.
		0 to: f degree - 1 do: [:i| answer add: (f psc: i with: f`)]].
	1 to: self polynomials size - 1 do: [:i|
		i + 1 to: self polynomials size do: [:j|
			f _ (self polynomials at: i) asUnivariate.
			g _ (self polynomials at: j) asUnivariate.
			0 to: (f degree min: g degree) do: [:m|
				answer add: (f psc: m with: g)]]].
	^ ((answer select: [:each| each isZero not]) collect: [:each| each / each leadingCoefficient]) asArray! !

!CylindricalAlgebraicDecomposition methodsFor: 'accessing' stamp: 'len 12/12/97 23:32'!
polynomials
	"Answer the polynomial set defining the receiver."

	^ polynomials! !

!CylindricalAlgebraicDecomposition methodsFor: 'accessing-private' stamp: 'len 5/30/2016 05:31'!
polynomials: aPolynomialSet
	polynomials _ aPolynomialSet.
	aPolynomialSet isEmpty ifTrue: [self error: 'empty polynomial set']. "when could it be empty?"! !

!CylindricalAlgebraicDecomposition methodsFor: 'printing' stamp: 'len 11/5/2016 10:11'!
printOn: aStream
	aStream nextPutAll: 'CAD in '; print: RR; nextPutAll: self dimension printString super; nextPutAll: ' of {'.
	self polynomials do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $}! !

!CylindricalAlgebraicDecomposition class methodsFor: 'instance creation' stamp: 'len 5/30/2016 05:47'!
polynomials: aPolynomialSet
	"Answer a new instance of the receiver representing the cylindrical algebraic decomposition of R^n in cells sign-invariant by aPolynomialSet (of polynomials in R[x1,..,xn])."

	^ self new polynomials: aPolynomialSet! !

!SemialgebraicCell methodsFor: 'comparing' stamp: 'len 3/18/2016 06:43'!
= anObject
	^ self class == anObject class
		and: [self projection = anObject projection
			and: [self left = anObject left and: [self right = anObject right]]]! !

!SemialgebraicCell methodsFor: 'morphisms' stamp: 'len 6/2/2016 02:26'!
boundary
	| answer |
	answer _ SemialgebraicSet new.
	self projection boundary do: [ :each |
		answer
			add: (self class projection: each between: self left and: self left);
			add: (self class projection: each between: self left and: self right);
			add: (self class projection: each between: self right and: self right)].
	answer
		add: (self class projection: self projection between: self left and: self left);
		add: (self class projection: self projection between: self right and: self right).
	^ answer! !

!SemialgebraicCell methodsFor: 'accessing' stamp: 'len 5/30/2016 18:56'!
boundsAt: aTuple
	"Answer the bounds of the last component of the receiver when the first components are aTuple."

	| lbound rbound |
	lbound _ (self left isInfinity or: [self left isNumber])
		ifTrue: [self left]
		ifFalse: [self left value: aTuple].
	rbound _ (self right isInfinity or: [self right isNumber])
		ifTrue: [self right]
		ifFalse: [self right value: aTuple].
	^ SemialgebraicInterval between: lbound and: rbound! !

!SemialgebraicCell methodsFor: 'morphisms' stamp: 'len 6/2/2016 08:08'!
closure
	| answer |
	answer _ SemialgebraicSet new.
	self projection closure do: [:each|
		answer
			add: (self class projection: each between: self left and: self left);
			add: (self class projection: each between: self left and: self right);
			add: (self class projection: each between: self right and: self right)].
	answer
		add: (self class projection: self projection between: self left and: self left);
		add: self;
		add: (self class projection: self projection between: self right and: self right).
	^ answer! !

!SemialgebraicCell methodsFor: 'accessing' stamp: 'len 3/17/2016 03:44'!
dimension
	"Answer the dimension of the receiver."

	^ self left = self right
		ifTrue: [self projection dimension]
		ifFalse: [self projection dimension + 1]! !

!SemialgebraicCell methodsFor: 'comparing' stamp: 'len 6/28/2016 09:35'!
hash
	^ self projection hash + self left hash + self right hash hashMultiply! !

!SemialgebraicCell methodsFor: 'testing' stamp: 'len 1/17/98 14:23'!
includes: aTuple
	"Answer true if the receiver includes the argument."

	| pi |
	pi _ aTuple copyFrom: 1 to: aTuple size - 1.
	^ (self projection includes: pi) and: [(self boundsAt: pi) includes: aTuple last]! !

!SemialgebraicCell methodsFor: 'morphisms' stamp: 'len 6/2/2016 08:09'!
interior
	^ self closure \ self boundary! !

!SemialgebraicCell methodsFor: 'morphisms' stamp: 'len 6/2/2016 07:38'!
interior2
	"Answer the interior of the receiver."

	^ self closure copyWithoutAll: self boundary! !

!SemialgebraicCell methodsFor: 'testing' stamp: 'len 12/14/97 22:06'!
isBounded
	"Answer true if the receiver is bounded."

	^ self projection isBounded and: [(self left isInfinity or: [self right isInfinity]) not]! !

!SemialgebraicCell methodsFor: 'testing' stamp: 'len 6/2/2016 07:33'!
isClosed
	^ self projection isClosed and: [self left = self right]! !

!SemialgebraicCell methodsFor: 'testing' stamp: 'len 2/6/98 13:33'!
isCylindricalOver: aCell
	"Answer true if the receiver is cylindrical over aCell."

	^ self = aCell or: [self projection isCylindricalOver: aCell]! !

!SemialgebraicCell methodsFor: 'testing' stamp: 'len 6/2/2016 05:09'!
isFinite
	^ self projection isFinite and: [self left = self right]! !

!SemialgebraicCell methodsFor: 'testing' stamp: 'len 6/2/2016 07:35'!
isOpen
	^ self projection isOpen and: [self left ~= self right]! !

!SemialgebraicCell methodsFor: 'accessing-private' stamp: 'len 12/14/97 21:33'!
left
	^ left! !

!SemialgebraicCell methodsFor: 'accessing-private' stamp: 'len 12/14/97 21:33'!
left: anObject
	left _ anObject! !

!SemialgebraicCell methodsFor: 'accessing' stamp: 'len 7/15/2016 18:37'!
polynomials
	"Answer the polynomials defining the receiver."

	| R answer |
	answer _ Set new.
	self left isInfinity ifFalse: [answer addAll: self left polynomials].
	self right isInfinity ifFalse: [answer addAll: self right polynomials].
	R _ answer isEmpty ifTrue: [QQ polynomialsIn: self rank] ifFalse: [answer anyOne species].
	self projection polynomials do: [:each| answer add: (each as: R)].
	^ answer! !

!SemialgebraicCell methodsFor: 'printing' stamp: 'len 6/2/2016 02:09'!
printOn: aStream
	"aStream nextPut: ${; print: self sentence; nextPut: $}"
	self dimension >= 0
		ifFalse: [aStream nextPut: Character emptySet. ^ self].
	self dimension = 0
		ifTrue: [aStream nextPut: ${; print: self sample; nextPut: $}. ^ self].
	aStream print: self dimension; nextPutAll: '-cell'; print: self sample! !

!SemialgebraicCell methodsFor: 'accessing' stamp: 'len 12/14/97 21:01'!
projection
	"Answer the projection of the receiver (all but the last component)."

	^ projection! !

!SemialgebraicCell methodsFor: 'accessing-private' stamp: 'len 12/14/97 20:59'!
projection: aCell
	projection _ aCell! !

!SemialgebraicCell methodsFor: 'accessing' stamp: 'len 2/6/98 12:50'!
projectionAt: anInteger
	"Answer the first anInteger components of the receiver."

	| projections cell |
	projections _ OrderedCollection with: self.
	cell _ self.
	[(cell _ cell projection) notNil] whileTrue: [projections addFirst: cell].
	^ projections at: anInteger! !

!SemialgebraicCell methodsFor: 'accessing' stamp: 'len 6/1/2016 23:32'!
rank
	"Answer the number of indeterminates involved to define the receiver."
	^ self projection rank + 1! !

!SemialgebraicCell methodsFor: 'accessing-private' stamp: 'len 12/14/97 21:33'!
right
	^ right! !

!SemialgebraicCell methodsFor: 'accessing-private' stamp: 'len 12/14/97 21:33'!
right: anObject
	right _ anObject! !

!SemialgebraicCell methodsFor: 'accessing' stamp: 'len 1/17/98 14:23'!
sample
	"Answer a sample point in the receiver."

	| alpha |
	^ (alpha _ self projection sample), (self boundsAt: alpha) sample! !

!SemialgebraicCell methodsFor: 'accessing' stamp: 'len 5/30/2016 19:00'!
sentence
	"Answer a quantifier-free defining sentence for the receiver."
	
	| answer sample sign sentence |
	self flag: #fix. "this is the same as SemialgebraicInterval>>sentence, fix"
	sample _ self sample.
	self polynomials do: [:each|
		sentence _
			(sign _ (each value: sample) sign) = 0
				ifTrue: [each eq: 0]
				ifFalse: [sign = -1 ifTrue: [each lneq: 0] ifFalse: [each gneq: 0]].
		answer _ answer isNil ifTrue: [sentence] ifFalse: [answer & sentence]].
	^ answer! !

!SemialgebraicCell class methodsFor: 'instance creation' stamp: 'len 12/14/97 21:45'!
projection: aCell between: leftBound and: rightBound
	"Answer a new instance of the receiver representing the cell
	whose projection is aCell and whose last component is bounded
	by the given bounds."

	^ self new projection: aCell; left: leftBound; right: rightBound! !

!BooleanConnective methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:44'!
& anObject
	"Answer the conjunction of the receiver with the argument."

	^ ConjunctionConnective left: self right: anObject! !

!BooleanConnective methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:48'!
-> anObject
	"Answer the implication of the argument from the receiver."

	^ ImplicationConnective left: self right: anObject! !

!BooleanConnective methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:45'!
| anObject
	"Answer the disjunction of the receiver with the argument."

	^ DisjunctionConnective left: self right: anObject! !

!BooleanConnective methodsFor: 'accessing' stamp: 'len 12/31/97 02:44'!
bounded
	"Answer the collection of bounded indeterminates."

	^ self left bounded addAll: self right bounded; yourself! !

!BooleanConnective methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:24'!
exists: anInteger
	"Answer the quantification of the reciever by the
	existential quantifier in the anInteger-th indeterminate."

	^ QuantifiedSentence quantifier: (Quantifier exists: anInteger) matrix: self! !

!BooleanConnective methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:25'!
forAll: anInteger
	"Answer the quantification of the reciever by the
	universal quantifier in the anInteger-th indeterminate."

	^ QuantifiedSentence quantifier: (Quantifier forAll: anInteger) matrix: self! !

!BooleanConnective methodsFor: 'accessing' stamp: 'len 12/31/97 02:44'!
free
	"Answer the collection of free indeterminates."

	| answer |
	answer _ self indeterminates.
	self bounded do: [ :each | answer remove: each ifAbsent: []].
	^ answer! !

!BooleanConnective methodsFor: 'accessing' stamp: 'len 12/18/97 20:41'!
indeterminates
	"Answer the indeterminates of the receiver."

	^ self left indeterminates addAll: self right indeterminates; yourself! !

!BooleanConnective methodsFor: 'testing' stamp: 'len 12/18/97 20:27'!
isQuantifierFree
	"Answer true if the receiver is quantifier-free."

	^ self left isQuantifierFree and: [self right isQuantifierFree]! !

!BooleanConnective methodsFor: 'testing' stamp: 'len 12/18/97 19:43'!
isSolution: anObject
	"Answer true if the argument is solution of the receiver."

	self subclassResponsibility! !

!BooleanConnective methodsFor: 'accessing' stamp: 'len 12/18/97 19:42'!
left
	"Answer the left component of the receiver."

	^ left! !

!BooleanConnective methodsFor: 'accessing-private' stamp: 'len 12/18/97 19:42'!
left: anObject
	left _ anObject! !

!BooleanConnective methodsFor: 'operations' stamp: 'len 12/26/97 18:46'!
normalized
	"Answer a copy of the receiver with the indeterminates renamed
	such that the same indeterminate does not ocur as free and bound."

	| last bounded answer |
	last _ self indeterminates asSortedCollection last.
	bounded _ self right bounded.
	answer _ self copy.
	(self left free select: [ :some | bounded includes: some])
			do: [ :each | answer rename: each to: (last _ last + 1)].
	^ answer! !

!BooleanConnective methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:49'!
not
	"Answer the negation of the receiver."

	self subclassResponsibility! !

!BooleanConnective methodsFor: 'accessing' stamp: 'len 12/19/97 21:47'!
polynomials
	"Answer the polynomial set of the receiver."

	^ self left polynomials addAll: self right polynomials; yourself! !

!BooleanConnective methodsFor: 'operations' stamp: 'len 12/31/97 02:19'!
prenex
	"Answer the prenex form of the receiver."

	^ self reduced normalized privatePrenex! !

!BooleanConnective methodsFor: 'printing' stamp: 'len 11/10/2016 08:44'!
printOn: aStream
	left class = self class
		ifTrue: [aStream print: left]
		ifFalse: [aStream nextPut: $(; print: left; nextPut: $)].
	aStream space; nextPutAll: self operatorName; space.
	right class = self class
		ifTrue: [aStream print: right]
		ifFalse: [aStream nextPut: $(; print: right; nextPut: $)]
! !

!BooleanConnective methodsFor: 'private' stamp: 'len 12/31/97 02:26'!
privatePrenex
	"Answer the prenex form of the receiver.
	Assume the receiver is reduced and normalized."

	| lp rp prefix matrix |
	lp _ self left privatePrenex.
	rp _ self right privatePrenex.
	prefix _ lp prefix copy addAll: rp prefix; yourself.
	matrix _ self class left: lp matrix right: rp matrix.
	^ QuantifiedSentence prefix: prefix matrix: matrix! !

!BooleanConnective methodsFor: 'operations' stamp: 'len 12/26/97 18:38'!
reduced
	"Answer a copy of the receiver with the redundant quantifiers removed."

	^ self class left: self left reduced right: self right reduced! !

!BooleanConnective methodsFor: 'operations' stamp: 'len 12/26/97 18:48'!
rename: anInteger to: anotherInteger
	"Rename the anInteger-th indeterminate of the receiver to anotherInteger."

	self left rename: anInteger to: anotherInteger.
	self right rename: anInteger to: anotherInteger! !

!BooleanConnective methodsFor: 'accessing' stamp: 'len 12/18/97 19:42'!
right
	"Answer the right component of the receiver."

	^ right! !

!BooleanConnective methodsFor: 'accessing-private' stamp: 'len 12/18/97 19:42'!
right: anObject
	right _ anObject! !

!BooleanConnective class methodsFor: 'instance creation' stamp: 'len 12/18/97 19:46'!
left: anObject right: anotherObject
	"Answer a new instance of the receiver with left
	and right components given by the arguments."

	^ self new left: anObject; right: anotherObject! !

!ConjunctionConnective methodsFor: 'testing' stamp: 'len 12/18/97 19:43'!
isSolution: anObject
	"Answer true if the argument is solution of the receiver."

	^ (self left isSolution: anObject) and: [self right isSolution: anObject]! !

!ConjunctionConnective methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:49'!
not
	"Answer the negation of the receiver."

	^ self left not | self right not! !

!ConjunctionConnective methodsFor: 'printing' stamp: 'len 6/2/2016 01:34'!
operatorName
	^ '&'! !

!DisjunctionConnective methodsFor: 'testing' stamp: 'len 12/18/97 19:44'!
isSolution: anObject
	"Answer true if the argument is solution of the receiver."

	^ (self left isSolution: anObject) or: [self right isSolution: anObject]! !

!DisjunctionConnective methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:50'!
not
	"Answer the negation of the receiver."

	^ self left not & self right not! !

!DisjunctionConnective methodsFor: 'printing' stamp: 'len 6/2/2016 01:34'!
operatorName
	^ '|'! !

!ImplicationConnective methodsFor: 'testing' stamp: 'len 12/18/97 19:47'!
isSolution: anObject
	"Answer true if the argument is solution of the receiver."

	^ (self left isSolution: anObject) = (self right isSolution: anObject)! !

!ImplicationConnective methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:52'!
not
	"Answer the negation of the receiver."

	^ self left -> self right not! !

!ImplicationConnective methodsFor: 'printing' stamp: 'len 6/2/2016 00:28'!
operatorName
	^ '->'! !

!PolynomialEquation methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:56'!
& anObject
	"Answer the conjunction of the receiver with the argument."

	^ ConjunctionConnective left: self right: anObject! !

!PolynomialEquation methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:57'!
-> anObject
	"Answer the implication of the argument from the receiver."

	^ ImplicationConnective left: self right: anObject! !

!PolynomialEquation methodsFor: 'arithmetic' stamp: 'len 12/18/97 19:56'!
| anObject
	"Answer the disjunction of the receiver with the argument."

	^ DisjunctionConnective left: self right: anObject! !

!PolynomialEquation methodsFor: 'converting' stamp: 'len 3/24/2016 06:20'!
asLinearEquation
	| P V b |
	polynomial isLinear ifFalse: [^ DomainError signal: 'the polynomial is not linear'].
	P _ polynomial species.
	V _ P scalars ** P rank.
	b _ polynomial value: V zero.
	^ (LinearForm from: V evaluating: [:v| (polynomial value: v) - b]) eq: b negated! !

!PolynomialEquation methodsFor: 'accessing' stamp: 'len 12/21/97 21:06'!
bounded
	"Answer the collection of bounded indeterminates."

	^ Set new! !

!PolynomialEquation methodsFor: 'copying' stamp: 'len 12/19/97 02:48'!
copy
	"Answer a copy of the receiver."

	^ self class polynomial: self polynomial copy! !

!PolynomialEquation methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:26'!
exists: anInteger
	"Answer the quantification of the reciever by the
	existential quantifier in the anInteger-th indeterminate."

	^ QuantifiedSentence quantifier: (Quantifier exists: anInteger) matrix: self! !

!PolynomialEquation methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:26'!
forAll: anInteger
	"Answer the quantification of the reciever by the
	universal quantifier in the anInteger-th indeterminate."

	^ QuantifiedSentence quantifier: (Quantifier forAll: anInteger) matrix: self! !

!PolynomialEquation methodsFor: 'accessing' stamp: 'len 12/21/97 21:06'!
free
	"Answer the collection of free indeterminates."

	^ self indeterminates! !

!PolynomialEquation methodsFor: 'accessing' stamp: 'len 12/18/97 20:34'!
indeterminates
	"Answer the indeterminates of the receiver."

	^ self polynomial indeterminates! !

!PolynomialEquation methodsFor: 'testing' stamp: 'len 12/23/2015 00:14'!
isHomogeneous
	^ polynomial isHomogeneous! !

!PolynomialEquation methodsFor: 'testing' stamp: 'len 12/18/97 20:28'!
isQuantifierFree
	"Answer true if the receiver is quantifier-free."

	^ true! !

!PolynomialEquation methodsFor: 'testing' stamp: 'len 11/24/2015 06:16'!
isSolution: anObject
	"Answer true if the argument satisfies the receiver."

	^ (self polynomial value: anObject) isZero! !

!PolynomialEquation methodsFor: 'operations' stamp: 'len 12/26/97 18:46'!
normalized
	"Answer a copy of the receiver with the indeterminates renamed
	such that the same indeterminate does not ocur as free and bound."

	^ self copy! !

!PolynomialEquation methodsFor: 'arithmetic' stamp: 'len 11/19/2015 19:43'!
not
	"Answer the negation of the receiver."

	^ (self polynomial lneq: self polynomial zero)
		| (self polynomial gneq: self polynomial zero)! !

!PolynomialEquation methodsFor: 'accessing' stamp: 'len 7/30/97 23:47'!
polynomial
	"Answer the polynomial that defines the receiver."

	^ polynomial! !

!PolynomialEquation methodsFor: 'accessing-private' stamp: 'len 3/1/2016 19:24'!
polynomial: aPolynomial
	aPolynomial coefficients do: [:each|
		each isRational
			ifFalse: [polynomial _ aPolynomial. ^ self]].
	polynomial _ aPolynomial integerPrimitive! !

!PolynomialEquation methodsFor: 'accessing' stamp: 'len 3/17/2016 04:20'!
polynomials
	"Answer the polynomial set of the receiver."

	^ {self polynomial}! !

!PolynomialEquation methodsFor: 'operations' stamp: 'len 12/31/97 02:26'!
prenex
	"Answer the prenex form of the receiver."

	^ self reduced normalized privatePrenex! !

!PolynomialEquation methodsFor: 'printing' stamp: 'len 6/2/2016 01:43'!
printOn: aStream
	aStream print: self polynomial - self polynomial independentCoefficient; nextPutAll: ' = '; print: self polynomial independentCoefficient negated! !

!PolynomialEquation methodsFor: 'private' stamp: 'len 12/31/97 02:27'!
privatePrenex
	"Answer the prenex form of the receiver.
	Assume the receiver is reduced and normalized."

	^ QuantifiedSentence matrix: self! !

!PolynomialEquation methodsFor: 'operations' stamp: 'len 12/26/97 18:38'!
reduced
	"Answer a copy of the receiver with the redundant quantifiers removed."

	^ self copy! !

!PolynomialEquation methodsFor: 'operations' stamp: 'len 6/1/2016 23:18'!
rename: anInteger to: anotherInteger 
	"Rename the anInteger-th indeterminate of the receiver to anotherInteger."
	self polynomial: (self polynomial rename: anInteger to: anotherInteger)! !

!PolynomialEquation class methodsFor: 'instance creation' stamp: 'len 7/30/97 23:45'!
polynomial: aPolynomial
	"Answer an instance of the receiver representing
	the equation 'aPolynomial = 0'."

	^ self new polynomial: aPolynomial! !

!PolynomialInequality methodsFor: 'arithmetic' stamp: 'len 12/18/97 20:07'!
& anObject
	"Answer the conjunction of the receiver with the argument."

	^ ConjunctionConnective left: self right: anObject! !

!PolynomialInequality methodsFor: 'arithmetic' stamp: 'len 12/18/97 20:07'!
-> anObject
	"Answer the implication of the argument from the receiver."

	^ ImplicationConnective left: self right: anObject! !

!PolynomialInequality methodsFor: 'arithmetic' stamp: 'len 12/18/97 20:07'!
| anObject
	"Answer the disjunction of the receiver with the argument."

	^ DisjunctionConnective left: self right: anObject! !

!PolynomialInequality methodsFor: 'accessing' stamp: 'len 12/21/97 21:06'!
bounded
	"Answer the collection of bounded indeterminates."

	^ Set new! !

!PolynomialInequality methodsFor: 'copying' stamp: 'len 12/19/97 02:47'!
copy
	"Answer a copy of the receiver."

	^ self class polynomial: self polynomial copy! !

!PolynomialInequality methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:26'!
exists: anInteger
	"Answer the quantification of the reciever by the
	existential quantifier in the anInteger-th indeterminate."

	^ QuantifiedSentence quantifier: (Quantifier exists: anInteger) matrix: self! !

!PolynomialInequality methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:27'!
forAll: anInteger
	"Answer the quantification of the reciever by the
	universal quantifier in the anInteger-th indeterminate."

	^ QuantifiedSentence quantifier: (Quantifier forAll: anInteger) matrix: self! !

!PolynomialInequality methodsFor: 'accessing' stamp: 'len 12/21/97 21:07'!
free
	"Answer the collection of free indeterminates."

	^ self indeterminates! !

!PolynomialInequality methodsFor: 'accessing' stamp: 'len 12/18/97 20:34'!
indeterminates
	"Answer the indeterminates of the receiver."

	^ self polynomial indeterminates! !

!PolynomialInequality methodsFor: 'testing' stamp: 'len 12/18/97 20:28'!
isQuantifierFree
	"Answer true if the receiver is quantifier-free."

	^ true! !

!PolynomialInequality methodsFor: 'testing' stamp: 'len 11/24/2015 06:16'!
isSolution: anObject
	"Answer true if the argument satisfies the receiver."

	^ (self polynomial value: anObject) negative! !

!PolynomialInequality methodsFor: 'operations' stamp: 'len 12/26/97 18:46'!
normalized
	"Answer a copy of the receiver with the indeterminates renamed
	such that the same indeterminate does not ocur as free and bound."

	^ self copy! !

!PolynomialInequality methodsFor: 'arithmetic' stamp: 'len 11/19/2015 19:43'!
not
	"Answer the negation of the receiver."

	^ (self polynomial eq: self polynomial zero) |
		(self polynomial gneq: self polynomial zero)! !

!PolynomialInequality methodsFor: 'accessing' stamp: 'len 12/15/97 23:57'!
polynomial
	"Answer the polynomial that defines the receiver."

	^ polynomial! !

!PolynomialInequality methodsFor: 'accessing-private' stamp: 'len 3/1/2016 19:28'!
polynomial: aPolynomial
	aPolynomial coefficients do: [:each|
		each isRational
			ifFalse: [polynomial _ aPolynomial. ^ self]].
	polynomial _ aPolynomial / aPolynomial integerContent abs! !

!PolynomialInequality methodsFor: 'accessing' stamp: 'len 3/17/2016 04:20'!
polynomials
	"Answer the polynomial set of the receiver."

	^ {self polynomial}! !

!PolynomialInequality methodsFor: 'operations' stamp: 'len 12/31/97 02:26'!
prenex
	"Answer the prenex form of the receiver."

	^ self reduced normalized privatePrenex! !

!PolynomialInequality methodsFor: 'printing' stamp: 'len 6/2/2016 01:45'!
printOn: aStream
	"Print a representation of the receiver on the stream aStream."

	| f ineq |
	self polynomial leadingCoefficient negative
		ifTrue: [f _ self polynomial negated. ineq _ ' > ']
		ifFalse: [f _ self polynomial. ineq _ ' < '].
	aStream print: f - f independentCoefficient; nextPutAll: ineq; print: f independentCoefficient negated! !

!PolynomialInequality methodsFor: 'private' stamp: 'len 12/31/97 02:27'!
privatePrenex
	"Answer the prenex form of the receiver.
	Assume the receiver is reduced and normalized."

	^ QuantifiedSentence matrix: self! !

!PolynomialInequality methodsFor: 'operations' stamp: 'len 12/26/97 18:38'!
reduced
	"Answer a copy of the receiver with the redundant quantifiers removed."

	^ self copy! !

!PolynomialInequality methodsFor: 'operations' stamp: 'len 6/1/2016 23:19'!
rename: anInteger to: anotherInteger 
	"Rename the anInteger-th indeterminate of the receiver to anotherInteger."
	self polynomial: (self polynomial rename: anInteger to: anotherInteger)! !

!PolynomialInequality class methodsFor: 'instance creation' stamp: 'len 12/16/97 00:00'!
polynomial: aPolynomial
	"Answer an instance of the receiver representing
	the inequation 'aPolynomial < 0'."

	^ self new polynomial: aPolynomial! !

!QuantifiedSentence methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:09'!
& anObject
	"Answer the conjunction of the receiver with the argument."

	^ ConjunctionConnective left: self right: anObject! !

!QuantifiedSentence methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:09'!
-> anObject
	"Answer the implication of the argument from the receiver."

	^ ImplicationConnective left: self right: anObject! !

!QuantifiedSentence methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:09'!
| anObject
	"Answer the disjunction of the receiver with the argument."

	^ DisjunctionConnective left: self right: anObject! !

!QuantifiedSentence methodsFor: 'accessing' stamp: 'len 2/6/98 12:33'!
add: aQuantifier
	"Add the quantifier aQuantifier to the receiver. Answer aQuantifier."

	^ self prefix add: aQuantifier! !

!QuantifiedSentence methodsFor: 'accessing' stamp: 'len 12/31/97 02:46'!
bounded
	"Answer the collection of bounded indeterminates."

	| answer |
	answer _ self matrix bounded.
	self prefix do: [ :each | answer add: each indeterminate].
	^ answer! !

!QuantifiedSentence methodsFor: 'copying' stamp: 'len 2/14/98 13:31'!
copy
	"Answer a copy of the receiver."

	^ self class
		prefix: (self prefix collect: [ :each | each copy])
		matrix: self matrix copy! !

!QuantifiedSentence methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:24'!
exists: anInteger
	"Answer the quantification of the reciever by the
	existential quantifier in the anInteger-th indeterminate."

	^ self copy add: (Quantifier exists: anInteger); yourself! !

!QuantifiedSentence methodsFor: 'arithmetic' stamp: 'len 2/6/98 12:32'!
forAll: anInteger
	"Answer the quantification of the reciever by the
	universal quantifier in the anInteger-th indeterminate."

	^ self copy add: (Quantifier forAll: anInteger); yourself! !

!QuantifiedSentence methodsFor: 'accessing' stamp: 'len 12/31/97 02:45'!
free
	"Answer the collection of free indeterminates."

	| answer |
	answer _ self indeterminates.
	self bounded do: [ :each | answer remove: each ifAbsent: []].
	^ answer! !

!QuantifiedSentence methodsFor: 'accessing' stamp: 'len 12/19/97 22:00'!
indeterminates
	"Answer the indeterminates set of the receiver."

	| answer |
	answer _ self matrix indeterminates.
	self prefix do: [ :each | answer add: each indeterminate].
	^ answer! !

!QuantifiedSentence methodsFor: 'initialization' stamp: 'len 2/7/98 15:26'!
initialize
	self prefix: OrderedCollection new! !

!QuantifiedSentence methodsFor: 'testing' stamp: 'len 12/21/97 20:42'!
isQuantifierFree
	"Answer true if the receiver is a quantifier-free sentence."

	^ self prefix isEmpty and: [self matrix isQuantifierFree]! !

!QuantifiedSentence methodsFor: 'accessing' stamp: 'len 12/21/97 21:03'!
matrix
	"Answer the matrix of the receiver: the sentence being quantified."

	^ matrix! !

!QuantifiedSentence methodsFor: 'accessing-private' stamp: 'len 12/19/97 21:30'!
matrix: anObject
	matrix _ anObject! !

!QuantifiedSentence methodsFor: 'operations' stamp: 'len 12/26/97 18:47'!
normalized
	"Answer a copy of the receiver with the indeterminates renamed
	such that the same indeterminate does not ocur as free and bound."

	^ self class prefix: self prefix matrix: self matrix normalized! !

!QuantifiedSentence methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:10'!
not
	"Answer the negation of the receiver."

	^ self class prefix: (self prefix collect: [ :each | each not]) matrix: self matrix not! !

!QuantifiedSentence methodsFor: 'accessing' stamp: 'len 12/19/97 21:46'!
polynomials
	"Answer the polynomial set of the receiver."

	^ self matrix polynomials! !

!QuantifiedSentence methodsFor: 'accessing' stamp: 'len 12/21/97 21:02'!
prefix
	"Answer the prefix of the receiver: a collection with quantifiers."

	^ prefix! !

!QuantifiedSentence methodsFor: 'accessing-private' stamp: 'len 12/19/97 21:30'!
prefix: aCollection
	prefix _ aCollection! !

!QuantifiedSentence methodsFor: 'operations' stamp: 'len 12/31/97 02:26'!
prenex
	"Answer the prenex form of the receiver."

	^ self reduced normalized privatePrenex! !

!QuantifiedSentence methodsFor: 'printing' stamp: 'len 5/30/2016 21:36'!
printOn: aStream
	"Print a representation of the receiver on the stream aStream."

	| names |
	names _ self matrix polynomial species indeterminateNames.
	self prefix do: [ :each | | name | each printOn: aStream withNames: names].
	aStream nextPut: $[; print: self matrix; nextPut: $]! !

!QuantifiedSentence methodsFor: 'private' stamp: 'len 12/31/97 02:25'!
privatePrenex
	"Answer the prenex form of the receiver.
	Assume the receiver is reduced and normalized."

	| mp newPrefix newMatrix |
	mp _ self matrix privatePrenex.
	newPrefix _ self prefix copy addAll: mp prefix; yourself.
	newMatrix _ mp matrix.
	^ self class prefix: newPrefix matrix: newMatrix! !

!QuantifiedSentence methodsFor: 'operations' stamp: 'len 12/31/97 02:52'!
reduced
	"Answer a copy of the receiver with the redundant quantifiers removed."

	| indets quantifiers reducedMatrix |
	reducedMatrix _ self matrix reduced.
	indets _ reducedMatrix indeterminates.
	quantifiers _ self prefix select: [ :each | indets includes: each indeterminate].
	^ quantifiers isEmpty
		ifTrue: [reducedMatrix]
		ifFalse: [self class prefix: quantifiers matrix: reducedMatrix]! !

!QuantifiedSentence methodsFor: 'operations' stamp: 'len 12/26/97 18:52'!
rename: anInteger to: anotherInteger
	"Rename the anInteger-th indeterminate of the receiver to anotherInteger."

	self prefix do: [ :each | each rename: anInteger to: anotherInteger].
	self matrix rename: anInteger to: anotherInteger! !

!QuantifiedSentence methodsFor: 'operations' stamp: 'len 2/7/98 19:04'!
resorted
	"Answer a copy of the receiver with the indeterminates resorted and renamed."

	| answer last |
	answer _ self copy.
	last _ 0.
	self prefix do: [ :each | last _ each indeterminate max: last].
	last _ last + 1.
	1 to: self prefix size do: [ :each |
		answer rename: (self prefix at: each) indeterminate to: last + each].
	1 to: self prefix size do: [ :each |
		answer rename: last + each to: each].
	^ answer! !

!QuantifiedSentence methodsFor: 'accessing' stamp: 'len 2/7/98 19:16'!
truth
	"Answer the truth value of the receiver, true or false."

	| prenex cad |
	prenex _ self prenex resorted.
	cad _ SemialgebraicSet cad: prenex polynomials.
	^ prenex truthRestrictedTo: cad! !

!QuantifiedSentence methodsFor: 'private' stamp: 'len 2/7/98 18:52'!
truthRestrictedTo: aSemialgebraicSet
	"Answer the truth value of the receiver, true or false,
	when restricted to aSemialgebraicSet. Assume that the
	receiver is in prenex form."

	| sentence quantifier |
	aSemialgebraicSet isEmpty
		ifTrue: [self prefix do: [ :each | each isExistential ifTrue: [^ false]].
				^ true].
	self isQuantifierFree
		ifTrue: [aSemialgebraicSet do: [ :each |
					(self matrix isSolution: each sample) ifFalse: [^ false]].
				^ true].
	sentence _ self class prefix: (self prefix copy removeFirst; yourself) matrix: self matrix.
	quantifier _ self prefix first.
	^ quantifier isExistential
		ifTrue: [(aSemialgebraicSet projectionAt: quantifier indeterminate)
					do: [ :each |
						(sentence truthRestrictedTo: (aSemialgebraicSet cylinderAt: each))
							ifTrue: [^ true]].
				false]
		ifFalse: [(aSemialgebraicSet projectionAt: quantifier indeterminate)
					do: [ :each |
						(sentence truthRestrictedTo: (aSemialgebraicSet cylinderAt: each))
							ifFalse: [^ false]].
				true]! !

!QuantifiedSentence class methodsFor: 'instance creation' stamp: 'len 2/5/98 20:29'!
matrix: anObject
	"Answer a new instance of the receiver with no
	quantifiers and with the matrix given by the argument."

	^ self new matrix: anObject! !

!QuantifiedSentence class methodsFor: 'instance creation' stamp: 'len 12/19/97 21:32'!
new
	^ super new initialize! !

!QuantifiedSentence class methodsFor: 'instance creation' stamp: 'len 2/5/98 20:32'!
prefix: aCollection matrix: anObject
	"Answer a new instance of the receiver with
	the prefix and matrix given by the arguments."

	| answer |
	answer _ self matrix: anObject.
	answer prefix addAll: aCollection.
	^ answer! !

!QuantifiedSentence class methodsFor: 'instance creation' stamp: 'len 6/1/2016 23:15'!
quantifier: aQuantifier matrix: anObject
	"Answer a new instance of the receiver with the quantifier and matrix given by the arguments."

	^ self prefix: {aQuantifier} matrix: anObject! !

!Quantifier methodsFor: 'copying' stamp: 'len 2/14/98 13:30'!
copy
	"Answer a copy of the receiver."

	^ self class indeterminate: self indeterminate! !

!Quantifier methodsFor: 'accessing' stamp: 'len 12/18/97 20:35'!
indeterminate
	"Answer the quantified indeterminate of the reiceiver."

	^ indeterminate! !

!Quantifier methodsFor: 'accessing-private' stamp: 'len 12/18/97 20:18'!
indeterminate: anInteger
	indeterminate _ anInteger! !

!Quantifier methodsFor: 'testing' stamp: 'len 2/5/98 21:26'!
isExistential
	"Answer true if the receiver is an existential quantifier."
	^ false! !

!Quantifier methodsFor: 'testing' stamp: 'len 2/5/98 21:27'!
isUniversal
	"Answer true if the receiver is an universal quantifier."
	^ false! !

!Quantifier methodsFor: 'printing' stamp: 'len 5/30/2016 09:19'!
printOn: aStream
	"Print a representation of the receiver on the stream aStream."

	self printOn: aStream withNames: Dictionary new! !

!Quantifier methodsFor: 'printing' stamp: 'len 12/31/97 03:02'!
printOn: aStream withNames: aDictionary
	"Print a representation of the receiver on the stream aStream
	using the indeterminate names from aDictionary."

	self subclassResponsibility! !

!Quantifier methodsFor: 'operations' stamp: 'len 12/26/97 18:53'!
rename: anInteger to: anotherInteger
	"Rename the anInteger-th indeterminate of the receiver to anotherInteger."

	self indeterminate = anInteger ifTrue: [self indeterminate: anotherInteger]! !

!Quantifier class methodsFor: 'instance creation' stamp: 'len 12/19/97 21:50'!
exists: anInteger
	"Answer the existential quantifier in the anInteger-th indeterminate."

	^ ExistentialQuantifier indeterminate: anInteger! !

!Quantifier class methodsFor: 'instance creation' stamp: 'len 12/19/97 21:50'!
forAll: anInteger
	"Answer the universal quantifier in the anInteger-th indeterminate."

	^ UniversalQuantifier indeterminate: anInteger! !

!Quantifier class methodsFor: 'instance creation' stamp: 'len 12/19/97 21:35'!
indeterminate: anInteger
	"Answer a new instance of the receiver representing
	the quantifier in the anInteger-th indeterminate."

	^ self new indeterminate: anInteger! !

!ExistentialQuantifier methodsFor: 'testing' stamp: 'len 2/5/98 21:28'!
isExistential
	"Answer true if the receiver is an existential quantifier."
	^ true! !

!ExistentialQuantifier methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:13'!
not
	"Answer the negation of the receiver."

	^ UniversalQuantifier indeterminate: self indeterminate! !

!ExistentialQuantifier methodsFor: 'printing' stamp: 'len 5/31/2016 05:42'!
printOn: aStream withNames: aDictionary
	"Print a representation of the receiver on the stream aStream
	using the indeterminate names from aDictionary."

	| name |
	name _ aDictionary
				at: self indeterminate
				ifAbsent: ['x' asText, self indeterminate printString sub].
	aStream nextPut: Character exists; nextPutAll: name! !

!UniversalQuantifier methodsFor: 'testing' stamp: 'len 2/5/98 21:27'!
isUniversal
	"Answer true if the receiver is an universal quantifier."
	^ true! !

!UniversalQuantifier methodsFor: 'arithmetic' stamp: 'len 12/21/97 21:13'!
not
	"Answer the negation of the receiver."

	^ ExistentialQuantifier indeterminate: self indeterminate! !

!UniversalQuantifier methodsFor: 'printing' stamp: 'len 5/31/2016 05:43'!
printOn: aStream withNames: aDictionary
	"Print a representation of the receiver on the stream aStream
	using the indeterminate names from aDictionary."

	| name |
	name _ aDictionary
				at: self indeterminate
				ifAbsent: ['x' asText, self indeterminate printString sub].
	aStream nextPut: Character forAll; nextPutAll: name! !
