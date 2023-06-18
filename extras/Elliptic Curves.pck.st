'From Cuis 6.0 [latest update: #5866] on 18 June 2023 at 2:40:13 pm'!
'Description '!
!provides: 'Elliptic Curves' 1 11!
!requires: 'Algebra' 1 0 nil!
SystemOrganization addCategory: 'Elliptic Curves'!


!classDefinition: #EllipticCurveGroup category: 'Elliptic Curves'!
Group subclass: #EllipticCurveGroup
	instanceVariableNames: 'curve scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveGroup class' category: 'Elliptic Curves'!
EllipticCurveGroup class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurve category: 'Elliptic Curves'!
ProjectivePlaneCurve subclass: #EllipticCurve
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurve class' category: 'Elliptic Curves'!
EllipticCurve class
	instanceVariableNames: ''!

!classDefinition: #ShortWeierstrassCurve category: 'Elliptic Curves'!
EllipticCurve subclass: #ShortWeierstrassCurve
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'ShortWeierstrassCurve class' category: 'Elliptic Curves'!
ShortWeierstrassCurve class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveIsogeny category: 'Elliptic Curves'!
RationalMap subclass: #EllipticCurveIsogeny
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveIsogeny class' category: 'Elliptic Curves'!
EllipticCurveIsogeny class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurvePoint category: 'Elliptic Curves'!
ProjectivePoint subclass: #EllipticCurvePoint
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurvePoint class' category: 'Elliptic Curves'!
EllipticCurvePoint class
	instanceVariableNames: ''!


!EllipticCurveGroup commentStamp: '<historical>' prior: 0!
Abelian group E(K) consisting of the K-rational points of an elliptic curve E.!

!EllipticCurve commentStamp: '<historical>' prior: 0!
Elliptic curves are the projective plane curves of genus 1. They can be defined by a Weierstrass equation of the form
	y² + a₁xy + a₃y = x³ + a₂x² + a₄x + a₀
with coefficients (a₁,a₂,a₃,a₄,a₆) in some field.

Elliptic curves are abelian varieties, i.e. the sets E(K) of K-rational points are abelian groups. See EllipticCurvePoint and EllipticCurveGroup.
!

!EllipticCurveIsogeny commentStamp: '<historical>' prior: 0!
Scheme morphisms of elliptic curves E -> E' that send the origin of E to the origin of E'. They are homomorphisms of group schemes.!

!EllipticCurvePoint commentStamp: '<historical>' prior: 0!
K-rational points on an elliptic curve E, seen as morphisms Spec(K) -> E. These objects implement the group law (with additive notation). The identity element (zero, or the point at infinity) is [0:1:0].

Following conventions, a point [a:b:1] is printed '(a,b)', and the point at infinity [0:1:0] is printed 'O'.

See also EllipticCurve.!

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

!EllipticCurveGroup methodsFor: 'enumerating' stamp: 'len 6/17/2023 23:18:20'!
do: aBlock
	curve pointsOver: self scalars do: aBlock! !

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

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 10:59:59'!
a₁
	^ coefficients at: 1! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:00:09'!
a₂
	^ coefficients at: 2! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:00:19'!
a₃
	^ coefficients at: 3! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:00:29'!
a₄
	^ coefficients at: 4! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:00:37'!
a₆
	^ coefficients at: 5! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:01:18'!
b₂
	^ self a₁ squared + (self a₂ * 4) "NOTE: this is misprinted in Silverman's 'The Arithmetic Of Elliptic Curves'"! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:01:40'!
b₄
	^ self a₄ * 2 + (self a₁ * self a₃)! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:02:08'!
b₆
	^ self a₃ squared + (self a₆ * 4)! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:03:10'!
b₈
	^ self a₁ squared * self a₆ + (self a₂ * self a₆ * 4) - (self a₁ * self a₃ * self a₄) + (self a₂ * self a₃ squared) - self a₄ squared! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:03:36'!
c₄
	^ self b₂ squared - (self b₄ * 24)! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:04:04'!
c₆
	^ (self b₂ ^ 3) negated + (self b₂ * self b₄ * 36) - (self b₆ * 216)! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:04:58'!
discriminant
	^ (self b₂ squared * self b₈) negated - (self b₄ ^ 3 * 8) - (self b₆ squared * 27) + (self b₂ * self b₄ * self b₆ * 9)! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 11/10/2016 08:03'!
genus
	"Answer the geometric genus of the receiver."
	^ 1! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:05:11'!
j
	^ self c₄ ^ 3 / self discriminant! !

!EllipticCurve methodsFor: 'arithmetic' stamp: 'len 6/18/2023 12:34:07'!
point: P plus: Q
	"Addition of two points [Coh96, Proposition 7.1.7]."
	| x₁ y₁ x₂ y₂ λ x₃ y₃ | 
	P isZero ifTrue: [^ Q].
	Q isZero ifTrue: [^ P].
	x₁ := P x. y₁ := P y.
	x₂ := Q x. y₂ := Q y.
	x₁ = x₂
		ifTrue:
			[y₁ = (y₂ negated - (self a₁ * x₂) - self a₃) "P = -Q?"
				ifTrue: [^ P zero].
			y₁ = y₂ "P = Q?"
				ifTrue: [λ := x₁ squared * 3 + (self a₂ * x₁ * 2) + self a₄ - (self a₁ * y₁) / (y₁ * 2 + (self a₁ * x₁) + self a₃)]]
		ifFalse:
			[λ := y₁ - y₂ / (x₁ - x₂)].
	x₃ := x₁ negated - x₂ - self a₂ + (λ*(λ + self a₁)).
	y₃ := y₁ negated - self a₃ - (self a₁ * x₃) + (λ*(x₁ - x₃)).
	^ P shallowCopy x: x₃ y: y₃! !

!EllipticCurve methodsFor: 'arithmetic' stamp: 'len 6/18/2023 12:43:15'!
pointDouble: P
	"Point doubling [Coh96, Proposition 7.1.7]."
	| x₁ y₁ λ x₃ y₃ |
	P isZero ifTrue: [^ P].
	x₁ := P x. y₁ := P y.
	λ := x₁ squared * 3 + (self a₂ * x₁ * 2) + self a₄ - (self a₁ * y₁) / (y₁ * 2 + (self a₁ * x₁) + self a₃).
	x₃ := x₁*(-2) - self a₂ + (λ*(λ + self a₁)).
	y₃ := y₁ negated - self a₃ - (self a₁ * x₃) + (λ*(x₁ - x₃)).
	^ P shallowCopy x: x₃ y: y₃! !

!EllipticCurve methodsFor: 'arithmetic' stamp: 'len 6/18/2023 12:19:35'!
pointNegate: P
	| x |
	P isZero ifTrue: [^ P].
	x := P x.
	^ P shallowCopy x: x y: P y negated + (self a₁ * x) - (self a₃)! !

!EllipticCurve methodsFor: 'converting' stamp: 'len 5/2/2021 11:19:44'!
% anInteger
	"Answer the reduction of the receiver modulo anInteger."
	self scalars = ℚ ifFalse: [^ DomainError signal: 'not over the rationals'].
	^ self over: ℤ / anInteger! !

!EllipticCurve methodsFor: 'converting' stamp: 'len 6/18/2023 11:10:36'!
over: aRing
	^ self class coefficients: (coefficients over: aRing)! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 8/5/2021 08:38:35'!
badPrimes
	"Answer the set of primes for which the receiver doesn't have 'good reduction'."
	^ self discriminant factorization asSet! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 11/10/2016 09:43'!
divisionPolynomial: n
	"Answer the n-th division polynomial."
	^ self divisionPolynomial: n at: self scalars polynomials x! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 12/28/2016 09:23:12'!
divisionPolynomial: n at: x
	"Answer the n-th division polynomial of the receiver evaluated at x."
	^ self notYetImplemented! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/18/2023 11:14:24'!
divisionPolynomialNew: n
	"Answer the n-th division polynomial."
	| R x y |
	R := ℤ polynomialsIn: #(x y).
	n == 1 ifTrue: [^ R one].
	x := R x.
	y := R y.
	n == 2 ifTrue: [^ y*2 + (self a₁ * x) + self a₃].
	n == 3 ifTrue: [^ x^4 * 3 + (x^3 * self b₂) + (x^2 * 3 * self b₄) + (x * 3 * self b₆) + self b₈].
	n == 4 ifTrue: [^ (self divisionPolynomial: 2) * (x^6 * 2 + (x^5 * self b₂) + (x^4 * self b₄ * 5) + (x^3 * 10 * self b₆) + (x^2 * 10 * self b₈) + (x * (self b₂ * self b₈ - (self b₄ * self b₆))) + (self b₄ * self b₈ - self b₆ squared))].
"	n odd ifTrue: [
		m _ n - 1 // 2.
		^ (self divisionPolynomial: m+2) * ((self divisionPolynomial: m) ^ 3) - (self divisionPolynomial"! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/18/2023 11:19:42'!
divisionPolynomials
	"Answer the sequence of division polynomials.
	Reference: Silverman, 'The Arithmetic of Elliptic Curves', Exercise 3.7, pag. 105."
	| R x y φ₁ φ₂ φ₃ φ₄ a₁ a₃ b₂ b₄ b₆ b₈ |
	R := self scalars polynomialsIn: #(x y). x := R x. y := R y.
	a₁ := self a₁.
	a₃ := self a₃.
	b₂ := self b₂.
	b₄ := self b₄.
	b₆ := self b₆.
	b₈ := self b₈.
	φ₁ := R one.
	φ₂ := y*2 + (a₁ * x) + a₃.
	φ₃ := x^4 * 3 + (x^3 * b₂) + (x^2 * 3 * b₄) + (x * 3 * b₆) + b₈.
	φ₄ := φ₂ * (x^6 * 2 + (x^5 * b₂) + (x^4 * b₄ * 5) + (x^3 * 10 * b₆) + (x^2 * 10 * b₈) + (x * (b₂ * b₈ - (b₄ * b₆))) + (b₄ * b₈ - b₆ squared)).
	^ Sequence to: R initialValues: {φ₁. φ₂. φ₃. φ₄} recurrence: [:phi :n|
		| m |
		n odd
			ifTrue:
				[m := n - 1 // 2.
				(phi at: m+2) * ((phi at: m) ^ 3) - ((phi at: m - 1) * ((phi at: m + 1) ^ 3))]
			ifFalse:
				[m := n // 2.
				(phi at: m-1) squared * (phi at: m) * (phi at: m+2) - ((phi at: m-2) * (phi at: m) * (phi at: m+1) squared // φ₂)]]! !

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

!EllipticCurve methodsFor: 'points' stamp: 'len 6/18/2023 12:05:20'!
pointAt: aTuple
	aTuple size = 2 ifTrue: [^ self pointClass on: self coordinates: aTuple, aTuple scalars one].
	^ self pointClass on: self coordinates: aTuple! !

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

!EllipticCurve methodsFor: 'printing' stamp: 'len 6/18/2023 11:09:27'!
printOn: aStream
	| P x y |
	P := self scalars polynomialsIn: #(x y) ordering: (MonomialOrdering glex: #(2 1)).
	x := P x.
	y := P y.
	aStream print: y^2 + (x*y * self a₁) + (y * self a₃).
	aStream
		nextPutAll: ' = ';
		print: (x^3) + (x^2 * self a₂) + (x * self a₄) + self a₆! !

!EllipticCurve methodsFor: 'testing' stamp: 'len 10/25/2016 14:11'!
isSmooth
	^ self discriminant isZero not! !

!EllipticCurve methodsFor: 'private' stamp: 'len 6/18/2023 11:08:14'!
coefficients: aTuple
	| f |
	self assert: aTuple size = 5.
	coefficients := aTuple.
	f := (aTuple scalars polynomialsIn: #(x y z))
		!! [:x :y :z| y^2 + (x*y * self a₁) + (y * self a₃) - (x^3) - (x^2 * self a₂) - (x * self a₄) - self a₆].
	f := f homogenizedIn: 3.
	self coordinateRing: f parent / f! !

!EllipticCurve methodsFor: 'private' stamp: 'len 6/18/2023 11:12:26'!
divisionPolynomial0: n at: x
	"Answer the n-th division polynomial, without the 2-torsion factor if n is even, evaluated at x."
	n = -2 ifTrue: [^ (self divisionPolynomial: -1 at: x) squared].
	n = -1 ifTrue: [^ x^3 * 4 + (x^2 * self b₂) + (x * self b₄ * 2) + self b₆].
	n <= 0 ifTrue: [DomainError signal].
	(n = 1 or: [n = 2]) ifTrue: [^ x one].
	n = 3 ifTrue: [^ x^4 + (x^3 * self b₂) + (x^2 * self b₄ * 3) + (x * self b₆ * 3) + self b₈].
	n = 4 ifTrue: [^ (x^2 * 6 + (x * self b₂) + self b₄) * (self divisionPolynomial: 3 at: x) + (self divisionPolynomial: -2 at: x)].
	^ n even
		ifTrue:
			[| m |
			m := n - 2 // 2.
			(self divisionPolynomial: m+1 at: x) *
			((self divisionPolynomial: m at: x)^2 * (self divisionPolynomial: m+3 at: x) -
			((self divisionPolynomial: m+2 at: x)^2 * (self divisionPolynomial: m-1 at: x)))]
		ifFalse:
			[| m |
			m := n - 1 // 2.
			m even
				ifTrue: [(self divisionPolynomial: m at: x)^3 * (self divisionPolynomial: -2 at: x) * (self divisionPolynomial: m+2 at: x) - ((self divisionPolynomial: m+1 at: x)^3 * (self divisionPolynomial: m-1 at: x))]
				ifFalse: [(self divisionPolynomial: m at: x)^3 * (self divisionPolynomial: m+2 at: x) - ((self divisionPolynomial: m+1 at: x)^3 * (self divisionPolynomial: m-1 at: x) * (self divisionPolynomial: -2 at: x))]]! !

!EllipticCurve methodsFor: 'private' stamp: 'len 6/18/2023 11:42:02'!
pointClass
	^ EllipticCurvePoint! !

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

!EllipticCurve class methodsFor: 'instance creation' stamp: 'len 6/18/2023 14:11:01'!
coefficients: aTuple
	(aTuple size = 2 or: [(1 to: 3) allSatisfy: [:i| aTuple isZeroAt: i]]) ifTrue: [^ ShortWeierstrassCurve coefficients: aTuple].
	^ self new coefficients: aTuple! !

!ShortWeierstrassCurve methodsFor: 'arithmetic' stamp: 'len 6/18/2023 13:59:39'!
point: P plus: Q
	| x₁ y₁ x₂ y₂ λ x₃ y₃ | 
	P isZero ifTrue: [^ Q].
	Q isZero ifTrue: [^ P].
	x₁ := P x. y₁ := P y.
	x₂ := Q x. y₂ := Q y.
	x₁ = x₂
		ifTrue:
			[y₁ = y₂ negated "P = -Q?"
				ifTrue: [^ P zero].
			y₁ = y₂ "P = Q?"
				ifTrue: [λ := x₁ squared * 3 + self a₄ / (y₁ * 2)]]
		ifFalse:
			[λ := y₁ - y₂ / (x₁ - x₂)].
	x₃ := λ squared - x₁ - x₂.
	y₃ := λ * (x₁ - x₃) - y₁.
	^ P shallowCopy x: x₃ y: y₃! !

!ShortWeierstrassCurve methodsFor: 'arithmetic' stamp: 'len 6/18/2023 13:59:45'!
pointDouble: P
	| x₁ y₁ λ x₃ y₃ | 
	P isZero ifTrue: [^ P].
	x₁ := P x. y₁ := P y.
	λ := x₁ squared * 3 + self a₄ / (y₁ * 2).
	x₃ := λ squared - (x₁*2).
	y₃ := λ * (x₁ - x₃) - y₁.
	^ P shallowCopy x: x₃ y: y₃! !

!ShortWeierstrassCurve methodsFor: 'arithmetic' stamp: 'len 6/18/2023 12:43:59'!
pointNegate: P
	^ P isZero ifTrue: [P] ifFalse: [P shallowCopy x: P x y: P y negated]! !

!ShortWeierstrassCurve class methodsFor: 'instance creation' stamp: 'len 6/18/2023 14:09:15'!
coefficients: aTuple
	aTuple size = 2 ifTrue: [^ self new coefficients: aTuple scalars !!!! {0. 0. 0} , aTuple].
	self assert: ((1 to: 3) allSatisfy: [:i| aTuple isZeroAt: i]).
	^ self new coefficients: aTuple! !

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

!EllipticCurvePoint methodsFor: 'accessing' stamp: 'len 5/8/2023 23:30:14'!
x
	^ coordinates at: 1! !

!EllipticCurvePoint methodsFor: 'accessing' stamp: 'len 5/8/2023 23:30:19'!
y
	^ coordinates at: 2! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 11/11/2016 09:58'!
* anInteger
	anInteger isInteger ifFalse: [^ DomainError signal: 'argument must be integer'].
	anInteger = 1 ifTrue: [^ self].
	anInteger = 0 ifTrue: [^ self zero].
	anInteger < 0 ifTrue: [^ (self * anInteger negated) negated].
	^ anInteger even
		ifTrue: [self doubled * (anInteger // 2)]
		ifFalse: [self doubled * (anInteger // 2) + self]! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 6/18/2023 11:34:42'!
+ aPoint
	^ codomain point: self plus: aPoint! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 11/9/2016 08:28'!
- aPoint
	^ self + aPoint negated! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 6/18/2023 11:38:50'!
doubled
	^ codomain pointDouble: self! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 6/18/2023 11:40:49'!
negated
	^ codomain pointNegate: self! !

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 6/18/2023 12:14:33'!
zero
	^ self shallowCopy coordinates: (coordinates zero at: 2 put: self scalars one; yourself)! !

!EllipticCurvePoint methodsFor: 'converting' stamp: 'len 11/11/2016 08:24'!
adaptToInteger: anInteger andSend: selector
	selector = #* ifTrue: [^ self * anInteger].
	self error: 'undefined operation'! !

!EllipticCurvePoint methodsFor: 'printing' stamp: 'len 11/11/2016 05:21'!
printOn: aStream
	self isZero
		ifTrue: [aStream nextPutAll: 'O' italic]
		ifFalse: [aStream nextPut: $(; print: self x; nextPut: $,; print: self y; nextPut: $)]! !

!EllipticCurvePoint methodsFor: 'testing' stamp: 'len 6/18/2023 12:26:49'!
isZero
	^ (coordinates isZeroAt: 1) and: [coordinates isZeroAt: 3]! !

!EllipticCurvePoint methodsFor: 'private' stamp: 'len 6/18/2023 12:10:17'!
x: x y: y
	coordinates := coordinates copy at: 1 put: x; at: 2 put: y; yourself! !
