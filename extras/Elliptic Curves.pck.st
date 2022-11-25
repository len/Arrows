'From Cuis 6.0 [latest update: #5438] on 25 November 2022 at 10:26:08 am'!
'Description '!
!provides: 'Elliptic Curves' 1 8!
!requires: 'Algebra' 1 0 nil!
SystemOrganization addCategory: #'Elliptic Curves'!


!classDefinition: #EllipticCurveGroup category: #'Elliptic Curves'!
Group subclass: #EllipticCurveGroup
	instanceVariableNames: 'curve scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveGroup class' category: #'Elliptic Curves'!
EllipticCurveGroup class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurve category: #'Elliptic Curves'!
ProjectivePlaneCurve subclass: #EllipticCurve
	instanceVariableNames: 'equation'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurve class' category: #'Elliptic Curves'!
EllipticCurve class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveIsogeny category: #'Elliptic Curves'!
RationalMap subclass: #EllipticCurveIsogeny
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveIsogeny class' category: #'Elliptic Curves'!
EllipticCurveIsogeny class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurvePoint category: #'Elliptic Curves'!
ProjectivePoint subclass: #EllipticCurvePoint
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurvePoint class' category: #'Elliptic Curves'!
EllipticCurvePoint class
	instanceVariableNames: ''!

!classDefinition: #WeierstrassEquation category: #'Elliptic Curves'!
Object subclass: #WeierstrassEquation
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'WeierstrassEquation class' category: #'Elliptic Curves'!
WeierstrassEquation class
	instanceVariableNames: ''!


!EllipticCurveGroup commentStamp: '<historical>' prior: 0!
Abelian groups E(K) consisting of the K-rational points of an elliptic curve E.!

!EllipticCurve commentStamp: '<historical>' prior: 0!
Elliptic curves are the projective plane curves of genus 1. They can be defined by a Weierstrass equation (see WeierstrassEquation).

Elliptic curves are abelian varieties, i.e. the sets E(K) of K-rational points are abelian groups. See EllipticCurvePoint and EllipticCurveGroup.
!

!EllipticCurveIsogeny commentStamp: '<historical>' prior: 0!
Scheme morphisms of elliptic curves E -> E' that send the origin of E to the origin of E'. They are homomorphisms of group schemes.!

!EllipticCurvePoint commentStamp: '<historical>' prior: 0!
K-rational points on an elliptic curve E, seen as morphisms Spec(K) -> E. These objects implement the group law (with additive notation). The identity element (zero, or the point at infinity) is [0:1:0].

Following conventions, a point [a:b:1] is printed '(a,b)', and the point at infinity [0:1:0] is printed 'O'.

See also EllipticCurve.!

!WeierstrassEquation commentStamp: '<historical>' prior: 0!
Weierstrass equations are equations of the form
	y^2 + a1 xy + a3 y = x^3 + a2 x^2 + a4 x + a0
with coefficients (a1,a2,a3,a4,a6) in some field.

When the discriminant is not zero, they define an elliptic curve. See EllipticCurve.!

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 9/28/2018 06:40:54'!
ambient
	^ self! !

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 11/10/2016 08:06'!
curve
	^ curve! !

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 11/10/2016 08:06'!
scalars
	^ scalars! !

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 5/22/2022 11:08:16'!
torsion
	"Answer the torsion subgroup of the receiver."
	"[Coh96] Algorithm 7.5.5."
"	| E b2 b4 b6 b8 discriminant |
	E _ self curve equation.
	b2 _ E b2.
	b4 _ E b4.
	b6 _ E b6.
	b8 _ E b8.
	discriminant _ E discriminant.
	points add: self identity.
	..."
	self notYetImplemented! !

!EllipticCurveGroup methodsFor: 'elements' stamp: 'len 11/23/2022 17:08:03'!
identity
	"Answer the identity element of the receiver.
	This is the point at infinity."
	^ curve pointAt: scalars !!!! {scalars zero. scalars one. scalars zero}! !

!EllipticCurveGroup methodsFor: 'enumerating' stamp: 'len 11/10/2016 08:10'!
do: aBlock
	self curve pointsOver: self scalars do: aBlock! !

!EllipticCurveGroup methodsFor: 'printing' stamp: 'len 11/10/2016 08:13'!
printOn: aStream
	aStream nextPutAll: 'E('; print: self scalars; nextPut: $)! !

!EllipticCurveGroup methodsFor: 'random' stamp: 'len 8/27/2018 22:24:49'!
atRandom: aRandom
	^ curve pointAtRandom: aRandom! !

!EllipticCurveGroup methodsFor: 'testing-categories' stamp: 'len 7/13/2022 12:14:07'!
isAdditiveGroup
	^ true! !

!EllipticCurveGroup methodsFor: 'private' stamp: 'len 11/10/2016 08:11'!
curve: anEllipticCurve
	curve _ anEllipticCurve! !

!EllipticCurveGroup methodsFor: 'private' stamp: 'len 11/10/2016 08:11'!
scalars: aField
	scalars _ aField! !

!EllipticCurveGroup class methodsFor: 'instance creation' stamp: 'len 11/10/2016 08:11'!
on: anEllipticCurve over: aField
	^ self new curve: anEllipticCurve; scalars: aField! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 11/9/2016 13:11'!
equation
	^ equation! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 11/10/2016 08:03'!
genus
	"Answer the geometric genus of the receiver."
	^ 1! !

!EllipticCurve methodsFor: 'converting' stamp: 'len 5/2/2021 11:19:44'!
% anInteger
	"Answer the reduction of the receiver modulo anInteger."
	self scalars = ℚ ifFalse: [^ DomainError signal: 'not over the rationals'].
	^ self over: ℤ / anInteger! !

!EllipticCurve methodsFor: 'converting' stamp: 'len 1/20/2017 09:37:28'!
over: aRing
	^ self class equation: (self equation over: aRing)! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 8/5/2021 08:38:35'!
badPrimes
	"Answer the set of primes for which the receiver doesn't have 'good reduction'."
	^ self discriminant factorization asSet! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 11/11/2016 10:02'!
discriminant
	^ self equation discriminant! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 11/10/2016 09:43'!
divisionPolynomial: n
	"Answer the n-th division polynomial."
	^ self divisionPolynomial: n at: self scalars polynomials x! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 12/28/2016 09:23:12'!
divisionPolynomial: n at: x
	"Answer the n-th division polynomial of the receiver evaluated at x."
	^ self notYetImplemented! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 5/23/2020 09:13:12'!
divisionPolynomialNew: n
	"Answer the n-th division polynomial."
	| R w x y |
	R _ ℤ polynomialsIn: #(x y).
	n == 1 ifTrue: [^ R one].
	w _ self equation.
	x _ R x.
	y _ R y.
	n == 2 ifTrue: [^ y*2 + (w a1 * x) + w a3].
	n == 3 ifTrue: [^ x^4 * 3 + (x^3 * w b2) + (x^2 * 3 * w b4) + (x * 3 * w b6) + w b8].
	n == 4 ifTrue: [^ (self divisionPolynomial: 2) * (x^6 * 2 + (x^5 * w b2) + (x^4 * w b4 * 5) + (x^3 * 10 * w b6) + (x^2 * 10 * w b8) + (x * (w b2 * w b8 - (w b4 * w b6))) + (w b4 * w b8 - w b6 squared))].
"	n odd ifTrue: [
		m _ n - 1 // 2.
		^ (self divisionPolynomial: m+2) * ((self divisionPolynomial: m) ^ 3) - (self divisionPolynomial"! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 1/8/2017 20:55:09'!
divisionPolynomials
	"Answer the sequence of division polynomials.
	Reference: Silverman, 'The Arithmetic of Elliptic Curves', Exercise 3.7, pag. 105."
	| R x y phi1 phi2 phi3 phi4 a1 a3 b2 b4 b6 b8 |
	R _ self scalars polynomialsIn: #(x y). x _ R x. y _ R y.
	a1 _ self equation a1.
	a3 _ self equation a3.
	b2 _ self equation b2.
	b4 _ self equation b4.
	b6 _ self equation b6.
	b8 _ self equation b8.
	phi1 _ R one.
	phi2 _ y*2 + (a1 * x) + a3.
	phi3 _ x^4 * 3 + (x^3 * b2) + (x^2 * 3 * b4) + (x * 3 * b6) + b8.
	phi4 _ phi2 * (x^6 * 2 + (x^5 * b2) + (x^4 * b4 * 5) + (x^3 * 10 * b6) + (x^2 * 10 * b8) + (x * (b2 * b8 - (b4 * b6))) + (b4 * b8 - b6 squared)).
	^ Sequence to: R initialValues: {phi1. phi2. phi3. phi4} recurrence: [:phi :n|
		| m |
		n odd
			ifTrue:
				[m _ n - 1 // 2.
				(phi at: m+2) * ((phi at: m) ^ 3) - ((phi at: m - 1) * ((phi at: m + 1) ^ 3))]
			ifFalse:
				[m _ n // 2.
				(phi at: m-1) squared * (phi at: m) * (phi at: m+2) - ((phi at: m-2) * (phi at: m) * (phi at: m+1) squared // phi2)]]! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 10/14/2022 08:45:21'!
multiplicationBy: m
	| F f omega φ ψ x |
	ψ _ self divisionPolynomials.
	f _ ψ at: m.
	x _ ψ codomain x.
"	y _ psi codomain y."
	F _ self ambient functionField.
	φ _ x * f squared - ((ψ at: m+1) * (ψ at: m-1)).
	omega _ (ψ at: m+2) * (ψ at: m-1) squared - ((ψ at: m-2) * (ψ at: m+1) squared).
	f _ f value: {F x. F y}.
	omega _ (omega value: {F x. F y}) / (F y * 4).
	φ _ φ value: {F x. F y}.
	^ "self from: self to: self representatives:" {φ / f squared. omega / (f^3). F z}! !

!EllipticCurve methodsFor: 'points' stamp: 'len 7/4/2019 03:48:39'!
pointAt: aTuple
	aTuple size = 2 ifTrue: [^ EllipticCurvePoint on: self coordinates: (aTuple copyWith: aTuple scalars one)].
	^ EllipticCurvePoint on: self coordinates: aTuple! !

!EllipticCurve methodsFor: 'points' stamp: 'len 8/27/2018 22:25:47'!
pointAtRandom: aRandom
	"Answer randomly a point of the receiver. Assume the scalars are a finite field."
	| points |
	[(points _ self pointsAtX: (self scalars atRandom: aRandom)) isEmpty] whileTrue.
	^ points atRandom: aRandom! !

!EllipticCurve methodsFor: 'points' stamp: 'len 7/30/2019 04:38:31'!
pointsAtX: x
	"Answer the points with x coordinate as given by the argument."
	| P |
	P _ self scalars polynomials.
	^ (self polynomial value: {P !! x. P x. P one}) roots collect: [:y| self pointAt: (self scalars !! x, y)]! !

!EllipticCurve methodsFor: 'points' stamp: 'len 12/22/2016 23:19:49'!
pointsOver: aField
	"Answer the group of K-rational points on the receiver."
	^ EllipticCurveGroup on: self over: aField! !

!EllipticCurve methodsFor: 'printing' stamp: 'len 12/11/2016 20:43:15'!
printOn: aStream
	aStream "nextPutAll: 'E : ';" print: self equation! !

!EllipticCurve methodsFor: 'testing' stamp: 'len 10/25/2016 14:11'!
isSmooth
	^ self discriminant isZero not! !

!EllipticCurve methodsFor: 'private' stamp: 'len 11/10/2016 09:45'!
divisionPolynomial0: n at: x
	"Answer the n-th division polynomial, without the 2-torsion factor if n is even, evaluated at x."
	n = -2 ifTrue: [^ (self divisionPolynomial: -1 at: x) squared].
	n = -1 ifTrue: [^ x^3 * 4 + (x^2 * equation b2) + (x * equation b4 * 2) + equation b6].
	n <= 0 ifTrue: [DomainError signal].
	(n = 1 or: [n = 2]) ifTrue: [^ x one].
	n = 3 ifTrue: [^ x^4 + (x^3 * equation b2) + (x^2 * equation b4 * 3) + (x * equation b6 * 3) + equation b8].
	n = 4 ifTrue: [^ (x^2 * 6 + (x * equation b2) + equation b4) * (self divisionPolynomial: 3 at: x) + (self divisionPolynomial: -2 at: x)].
	^ n even
		ifTrue:
			[| m |
			m _ n - 2 // 2.
			(self divisionPolynomial: m+1 at: x) *
			((self divisionPolynomial: m at: x)^2 * (self divisionPolynomial: m+3 at: x) -
			((self divisionPolynomial: m+2 at: x)^2 * (self divisionPolynomial: m-1 at: x)))]
		ifFalse:
			[| m |
			m _ n - 1 // 2.
			m even
				ifTrue: [(self divisionPolynomial: m at: x)^3 * (self divisionPolynomial: -2 at: x) * (self divisionPolynomial: m+2 at: x) - ((self divisionPolynomial: m+1 at: x)^3 * (self divisionPolynomial: m-1 at: x))]
				ifFalse: [(self divisionPolynomial: m at: x)^3 * (self divisionPolynomial: m+2 at: x) - ((self divisionPolynomial: m+1 at: x)^3 * (self divisionPolynomial: m-1 at: x) * (self divisionPolynomial: -2 at: x))]]! !

!EllipticCurve methodsFor: 'private' stamp: 'len 7/31/2020 10:24:36'!
equation: aWeierstrassEquation
	| f |
	equation _ aWeierstrassEquation.
	f _ equation homogeneousPolynomial.
	self coordinateRing: f parent / f! !

!EllipticCurve class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
cremona11a1
	"Elliptic curve 11a1, first curve in the Cremona database."
	^ EllipticCurve coefficients: ((0,-1,1,-10,-20) over: ℚ)! !

!EllipticCurve class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
cremona11a2
	"Elliptic curve 11a2 from the Cremona database."
	^ EllipticCurve coefficients: ((0,-1,1,-7820,-263580) over: ℚ)! !

!EllipticCurve class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
cremona11a3
	"Elliptic curve 11a3 from the Cremona database."
	^ EllipticCurve coefficients: ((0,-1,1,0,0) over: ℚ)! !

!EllipticCurve class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
cremona37a
	"Elliptic curve 37a from the Cremona database.
	This is the elliptic curve of rank 1 with smallest conductor."
	^ EllipticCurve coefficients: ((0,0,1,-1,0) over: ℚ)! !

!EllipticCurve class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
cremona389a1
	"Elliptic curve 389a1 from the Cremona database."
	^ EllipticCurve coefficients: ((0,1,1,-2,0) over: ℚ)! !

!EllipticCurve class methodsFor: 'examples' stamp: 'len 8/24/2018 20:06:52'!
secp256k1
	"Answer the SECP256K1 elliptic curve, used by Bitcoin.
	The group of this curve has order 115792089237316195423570985008687907852837564279074904382605163141518161494337."
	| F |
	F _ PrimeField new: 16rFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F.
	^ self coefficients: ((0,0,0,0,7) over: F)! !

!EllipticCurve class methodsFor: 'instance creation' stamp: 'len 11/9/2016 15:12'!
coefficients: aTuple
	^ self equation: (WeierstrassEquation coefficients: aTuple)! !

!EllipticCurve class methodsFor: 'instance creation' stamp: 'len 11/9/2016 13:10'!
equation: aWeierstrassEquation
	^ self new equation: aWeierstrassEquation! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 3/6/2017 16:48:06'!
dual
	"Answer the dual of the receiver. If the receiver is f:E->E' of degree n, its dual is an isogeny g:E'->E such that the compositions are [n] (multiplication by n)."
	^ self notYetImplemented! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 12/25/2016 08:42:27'!
isSurjective
	^ self isZero not! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 12/25/2016 08:43:44'!
isZero
	^ self degree = 0! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 12/28/2016 11:16:46'!
kernelPolynomial
	^ self propertyAt: #kernelPolynomial ifAbsentPut: [self computeKernelPolynomial]! !

!EllipticCurvePoint methodsFor: 'accessing' stamp: 'len 11/11/2016 05:22'!
x
	^ self coordinates at: 1! !

!EllipticCurvePoint methodsFor: 'accessing' stamp: 'len 11/11/2016 05:22'!
y
	^ self coordinates at: 2! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 11/11/2016 09:58'!
* anInteger
	anInteger isInteger ifFalse: [^ DomainError signal: 'argument must be integer'].
	anInteger = 1 ifTrue: [^ self].
	anInteger = 0 ifTrue: [^ self zero].
	anInteger < 0 ifTrue: [^ (self * anInteger negated) negated].
	^ anInteger even
		ifTrue: [self doubled * (anInteger // 2)]
		ifFalse: [self doubled * (anInteger // 2) + self]! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 11/23/2022 17:13:14'!
+ aPoint
	"Answer the sum of the receiver and the argument.
	[Coh96] Proposition 7.1.7."
	| W x1 y1 x2 y2 m x3 y3 |
	aPoint isZero ifTrue: [^ self].
	self isZero ifTrue: [^ aPoint].
	W _ self codomain equation.
	x1 _ self x. y1 _ self y.
	x2 _ aPoint x. y2 _ aPoint y.
	(x1 = x2 and: [y1 = (y2 negated - (W a1 * x2) - W a3)]) "self = aPoint negated"
		ifTrue: [^ self zero].
	m _ (x1 = x2 and: [y1 = y2]) "self = aPoint"
		ifTrue: [x1 squared * 3 + (W a2 * x1 * 2) + W a4 - (W a1 * y1) / (y1 * 2 + (W a1 * x1) + W a3)]
		ifFalse: [y1 - y2 / (x1 - x2)].
	x3 _ x1 negated - x2 - W a2 + (m*(m + W a1)).
	y3 _ y1 negated - W a3 - (W a1 * x3) + (m*(x1 - x3)).
	^ self shallowCopy coordinates: self scalars !!!! {x3. y3. self scalars one}! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 11/9/2016 08:28'!
- aPoint
	^ self + aPoint negated! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 11/23/2022 17:13:24'!
doubled
	"Answer the receiver times 2, i.e. self + self.
	[Coh96] Proposition 7.1.7."
	| W x1 y1 m x3 y3 |
	self isZero ifTrue: [^ self].
	W _ self codomain equation.
	x1 _ self x. y1 _ self y.
	m _ x1 squared * 3 + (W a2 * x1 * 2) + W a4 - (W a1 * y1) / (y1 * 2 + (W a1 * x1) + W a3).
	x3 _ x1*(-2) - W a2 + (m*(m + W a1)).
	y3 _ y1 negated - W a3 - (W a1 * x3) + (m*(x1 - x3)).
	^ self shallowCopy coordinates: self scalars !!!! {x3. y3. self scalars one}! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 11/23/2022 17:13:32'!
negated
	| W x |
	self isZero ifTrue: [^ self].
	W _ self codomain equation.
	x _ self x.
	^ self shallowCopy coordinates: self scalars !!!! {x. self y negated + (W a1 * x) - W a3. self scalars one}! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 11/23/2022 17:13:43'!
zero
	| R |
	R _ self scalars.
	^ self shallowCopy coordinates: R !!!! {R zero. R one. R zero}! !

!EllipticCurvePoint methodsFor: 'converting' stamp: 'len 11/11/2016 08:24'!
adaptToInteger: anInteger andSend: selector
	selector = #* ifTrue: [^ self * anInteger].
	self error: 'undefined operation'! !

!EllipticCurvePoint methodsFor: 'printing' stamp: 'len 11/11/2016 05:21'!
printOn: aStream
	self isZero
		ifTrue: [aStream nextPutAll: 'O' italic]
		ifFalse: [aStream nextPut: $(; print: self x; nextPut: $,; print: self y; nextPut: $)]! !

!EllipticCurvePoint methodsFor: 'testing' stamp: 'len 11/9/2016 13:33'!
isZero
	^ (self coordinates at: 1) isZero and: [(self coordinates at: 3) isZero]! !

!WeierstrassEquation methodsFor: 'printing' stamp: 'len 2/12/2017 06:05:00'!
printOn: aStream
	| P x y |
	P _ self scalars polynomialsIn: #(x y) ordering: (MonomialOrdering glex: #(2 1)).
	x _ P x.
	y _ P y.
	aStream print: y^2 + (x*y * self a1) + (y * self a3).
	aStream
		nextPutAll: ' = ';
		print: (x^3) + (x^2 * self a2) + (x * self a4) + self a6! !

!WeierstrassEquation methodsFor: 'accessing' stamp: 'len 11/9/2016 13:01'!
curve
	^ EllipticCurve equation: self! !

!WeierstrassEquation methodsFor: 'accessing' stamp: 'len 5/23/2018 22:35:52'!
homogeneousPolynomial
	^ (self scalars polynomialsIn: #(x y z)) !! self polynomial homogenizedIn: 3! !

!WeierstrassEquation methodsFor: 'accessing' stamp: 'len 8/8/2019 05:00:57'!
polynomial
	^ (self scalars polynomialsIn: #(x y))
		!! [:x :y| y^2 + (x*y * self a1) + (y * self a3) - (x^3) - (x^2 * self a2) - (x * self a4) - self a6]! !

!WeierstrassEquation methodsFor: 'accessing' stamp: 'len 11/9/2016 07:16'!
scalars
	^ coefficients scalars! !

!WeierstrassEquation methodsFor: 'accessing-private' stamp: 'len 11/9/2016 07:08'!
coefficients: aTuple
	coefficients _ aTuple! !

!WeierstrassEquation methodsFor: 'converting' stamp: 'len 1/20/2017 09:36:51'!
over: aRing
	^ self class coefficients: (coefficients over: aRing)! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:08'!
a1
	^ coefficients at: 1! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:08'!
a2
	^ coefficients at: 2! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:08'!
a3
	^ coefficients at: 3! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:08'!
a4
	^ coefficients at: 4! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:08'!
a6
	^ coefficients at: 5! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/11/2016 10:16'!
b2
	^ self a1 squared + (self a2 * 4) "NOTE: this is wrong in Silverman 'The Arithmetic Of Elliptic Curves'"! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:23'!
b4
	^ self a4 * 2 + (self a1 * self a3)! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:23'!
b6
	^ self a3 squared + (self a6 * 4)! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:24'!
b8
	^ self a1 squared * self a6 + (self a2 * self a6 * 4) - (self a1 * self a3 * self a4) + (self a2 * self a3 squared) - self a4 squared! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:25'!
c4
	^ self b2 squared - (self b4 * 24)! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:26'!
c6
	^ (self b2 ^ 3) negated + (self b2 * self b4 * 36) - (self b6 * 216)! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:27'!
discriminant
	^ (self b2 squared * self b8) negated - (self b4 ^ 3 * 8) - (self b6 squared * 27) + (self b2 * self b4 * self b6 * 9)! !

!WeierstrassEquation methodsFor: 'invariants' stamp: 'len 11/9/2016 07:28'!
j
	^ self c4 ^ 3 / self discriminant! !

!WeierstrassEquation class methodsFor: 'instance creation' stamp: 'len 11/9/2016 07:08'!
coefficients: aTuple
	^ self new coefficients: aTuple! !
