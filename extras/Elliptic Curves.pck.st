'From Cuis 6.0 [latest update: #5875] on 21 June 2023 at 11:28:58 pm'!
'Description '!
!provides: 'Elliptic Curves' 1 14!
!requires: 'Algebra' 1 0 nil!
SystemOrganization addCategory: 'Elliptic Curves'!


!classDefinition: #EllipticCurveGroup category: 'Elliptic Curves'!
Group subclass: #EllipticCurveGroup
	instanceVariableNames: 'scalars codomain'
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

!classDefinition: #WeierstrassCurve category: 'Elliptic Curves'!
EllipticCurve subclass: #WeierstrassCurve
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'WeierstrassCurve class' category: 'Elliptic Curves'!
WeierstrassCurve class
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

!classDefinition: #EllipticCurveMorphism category: 'Elliptic Curves'!
SchemeMorphism subclass: #EllipticCurveMorphism
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveMorphism class' category: 'Elliptic Curves'!
EllipticCurveMorphism class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveIsogeny category: 'Elliptic Curves'!
EllipticCurveMorphism subclass: #EllipticCurveIsogeny
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveIsogeny class' category: 'Elliptic Curves'!
EllipticCurveIsogeny class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveIsomorphism category: 'Elliptic Curves'!
EllipticCurveMorphism subclass: #EllipticCurveIsomorphism
	instanceVariableNames: 'u r s t'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveIsomorphism class' category: 'Elliptic Curves'!
EllipticCurveIsomorphism class
	instanceVariableNames: ''!


!EllipticCurveGroup commentStamp: '<historical>' prior: 0!
Abelian group E(K) consisting of the K-rational points of an elliptic curve E.!

!EllipticCurve commentStamp: '<historical>' prior: 0!
Elliptic curves are smooth projective plane curves of genus 1 with at least one point. They can be defined by a long Weierstrass equation of the form
	y² + a₁xy + a₃y = x³ + a₂x² + a₄x + a₆
with coefficients (a₁,a₂,a₃,a₄,a₆) in a field, or in characteristic > 3 with a short Weierstrass equation
	y² = x³ + a₄x + a₆.

Elliptic curves are abelian varieties, i.e. the sets E(K) of K-rational points are abelian groups. See EllipticCurvePoint and EllipticCurveGroup.
!

!WeierstrassCurve commentStamp: '<historical>' prior: 0!
Elliptic curves defined by a short Weierstrass equation:
	y² = x³ + a₄x + a₆.
!

!EllipticCurvePoint commentStamp: '<historical>' prior: 0!
K-rational points on an elliptic curve E, seen as morphisms Spec(K) -> E. These objects implement the group law (with additive notation). The identity element (zero, or the point at infinity) is [0:1:0].

Following conventions, a point [a:b:1] is printed '(a,b)', and the point at infinity [0:1:0] is printed 'O'.

See also EllipticCurve.!

!EllipticCurveIsogeny commentStamp: '<historical>' prior: 0!
Scheme morphisms of elliptic curves E₁ -> E₂ over a field that send the identity of E₁ to the identity of E₂. They are homomorphisms of group schemes.!

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 9/28/2018 06:40:54'!
ambient
	^ self! !

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 6/19/2023 13:04:08'!
codomain
	^ codomain! !

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 6/19/2023 12:56:04'!
domain
	^ scalars spec! !

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 11/10/2016 08:06'!
scalars
	^ scalars! !

!EllipticCurveGroup methodsFor: 'accessing' stamp: 'len 6/21/2023 16:40:37'!
size
	^ self propertyAt: #size ifAbsentPut: [codomain pointsCountOver: scalars]! !

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

!EllipticCurveGroup methodsFor: 'comparing' stamp: 'len 6/19/2023 16:57:43'!
= anObject
	^ self class = anObject class and: [self scalars = anObject scalars and: [self codomain = anObject codomain]]! !

!EllipticCurveGroup methodsFor: 'comparing' stamp: 'len 6/19/2023 16:57:55'!
hash
	^ self codomain hash! !

!EllipticCurveGroup methodsFor: 'elements' stamp: 'len 6/19/2023 12:58:09'!
identity
	"Answer the identity element of the receiver.
	This is the point at infinity."
	^ codomain pointAt: scalars !!!! {scalars zero. scalars one. scalars zero}! !

!EllipticCurveGroup methodsFor: 'elements' stamp: 'len 6/21/2023 16:07:31'!
newFrom: anObject
	(anObject isSequenceable and: [anObject size = 2]) ifTrue: [^ self codomain pointAt: self scalars !!!! anObject].
	^ super newFrom: anObject! !

!EllipticCurveGroup methodsFor: 'elements' stamp: 'len 6/19/2023 16:52:05'!
pointsWithX: x₀
	"Answer the points with x coordinate as given by the argument."
	^ Iterator on: [:aBlock| self pointsWithX: x₀ do: aBlock]! !

!EllipticCurveGroup methodsFor: 'elements' stamp: 'len 6/19/2023 16:51:26'!
pointsWithX: x₀ do: aBlock
	"Answer the points with x coordinate as given by the argument."
	| P |
	P := scalars polynomials.
	^ (codomain polynomial value: {P !! x₀. P x. P one}) roots do: [:y| aBlock value: (codomain pointAt: (scalars !! x₀, y))]! !

!EllipticCurveGroup methodsFor: 'enumerating' stamp: 'len 6/19/2023 12:57:57'!
do: aBlock
	codomain pointsOver: self scalars do: aBlock! !

!EllipticCurveGroup methodsFor: 'morphisms' stamp: 'len 6/19/2023 13:00:16'!
to: aCodomain adapting: anObject
	((anObject isKindOf: EllipticCurveIsogeny) and: [anObject domain = codomain and: [anObject codomain = aCodomain codomain]]) ifTrue: [^ self to: aCodomain map: [:P| anObject value: P]].
	^ super to: aCodomain adapting: anObject! !

!EllipticCurveGroup methodsFor: 'operations' stamp: 'len 6/21/2023 15:09:55'!
elementaryDivisors
	"[Mil04, Algorithm 3]."
	| K N r factors N₀ N₁ P Q P₁ Q₁ s t m ζ d |
	K := self codomain scalars.
	N := self size.
	r := N gcd: K size - 1.
	factors := r factorization.
	N decompositionWith: r into: [:anInteger :anotherInteger| N₀ := anInteger. N₁ := anotherInteger].
	[P := self atRandom.
	Q := self atRandom.
	P₁ := P * N₁.
	Q₁ := Q * N₁.
	s := self orderOf: P₁ exponentBound: r factorization: factors.
	t := self orderOf: Q₁ exponentBound: r factorization: factors.
	m := s lcm: t.
	ζ := P₁ weilPairing: Q₁ torsion: m.
	d := ζ multiplicativeOrder.
	m*d = N₀ ifTrue: [^ {d. N / d squared}]] repeat! !

!EllipticCurveGroup methodsFor: 'operations' stamp: 'len 6/21/2023 13:02:23'!
elementaryDivisorsAndGenerators
	"[Mil04, Algorithm 2]."
	| N P Q s t m ζ d |
	N := self size.
	[P := self atRandom.
	Q := self atRandom.
	s := P order.
	t := Q order.
	m := s lcm: t.
	ζ := P weilPairing: Q torsion: m.
	d := ζ multiplicativeOrder.
	m*d = N ifTrue: [^ {d. m. P. Q}]] repeat! !

!EllipticCurveGroup methodsFor: 'printing' stamp: 'len 11/10/2016 08:13'!
printOn: aStream
	aStream nextPutAll: 'E('; print: self scalars; nextPut: $)! !

!EllipticCurveGroup methodsFor: 'random' stamp: 'len 6/20/2023 11:22:20'!
atRandom: aRandom
	"Answer a point choosen uniformly, assuming we're over a finite field.
	See [Mil04, Algorithm 6]."
	| points |
	[scalars isFinite ifTrue: [(scalars size + 1 * 2 atRandom: aRandom) = 1 ifTrue: [^ self identity]].
	[(points := (self pointsWithX: (scalars atRandom: aRandom)) asOrderedCollection) isEmpty] whileTrue.
	"There can be two distinct points or one double point.
	If there are two distinct points, choose one uniformy:"
	points size > 1 ifTrue: [^ points atRandom: aRandom].
	"Otherwise with probability 1/2 answer the double point, or try again:"
	(2 atRandom: aRandom) = 1 ifTrue: [^ points anyOne]] repeat! !

!EllipticCurveGroup methodsFor: 'testing' stamp: 'len 6/19/2023 16:52:44'!
isX: anElement
	"Answer true if anElement is the x coordinate of any point of the receiver."
	^ (self pointsWithX: anElement) notEmpty! !

!EllipticCurveGroup methodsFor: 'testing-categories' stamp: 'len 7/13/2022 12:14:07'!
isAdditiveGroup
	^ true! !

!EllipticCurveGroup methodsFor: 'private' stamp: 'len 6/19/2023 13:04:03'!
codomain: anEllipticCurve
	codomain := anEllipticCurve! !

!EllipticCurveGroup methodsFor: 'private' stamp: 'len 11/10/2016 08:11'!
scalars: aField
	scalars _ aField! !

!EllipticCurveGroup class methodsFor: 'instance creation' stamp: 'len 6/19/2023 12:58:44'!
on: anEllipticCurve over: aField
	^ self new codomain: anEllipticCurve; scalars: aField! !

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

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/19/2023 13:53:09'!
coefficients
	"Answer the Weierstrass coefficients of the receiver (a₁,a₂,a₃,a₄,a₆),
	such that the receiver is defined by the long Weierstrass equation
		y² + a₁xy + a₃y = x³ + a₂x² + a₄x + a₆."
	^ coefficients! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:03:36'!
c₄
	^ self b₂ squared - (self b₄ * 24)! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:04:04'!
c₆
	^ (self b₂ ^ 3) negated + (self b₂ * self b₄ * 36) - (self b₆ * 216)! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/19/2023 12:17:47'!
discriminant
	"Note: an elliptic curve is non-singular if the discriminant is a unit (non-zero over a field)."
	^ (self b₂ squared * self b₈) negated - (self b₄ ^ 3 * 8) - (self b₆ squared * 27) + (self b₂ * self b₄ * self b₆ * 9)! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 11/10/2016 08:03'!
genus
	"Answer the geometric genus of the receiver."
	^ 1! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/19/2023 13:57:16'!
hyperellipticPolynomials
	"Answer polynomials g and h such that the receiver can be defined by the standard hyperelliptic equation:
		y² + h(x)y = g(x)."
	| R |
	R := self scalars polynomials.
	^ {R !! {self a₆. self a₄. self a₂. 1}. R !! {self a₃. self a₁}}! !

!EllipticCurve methodsFor: 'accessing' stamp: 'len 6/18/2023 11:05:11'!
j
	^ self c₄ ^ 3 / self discriminant! !

!EllipticCurve methodsFor: 'comparing' stamp: 'len 6/19/2023 13:53:44'!
~ aScheme
	"Answer true if the receiver is isomorphic to the argument."
	(aScheme isKindOf: EllipticCurve) ifFalse: [^ super ~ aScheme].
	self = aScheme ifTrue: [^ true].
	self scalars = aScheme scalars ifFalse: [^ false].
	self j = aScheme j ifFalse: [^ false].
	self notYetImplemented! !

!EllipticCurve methodsFor: 'converting' stamp: 'len 5/2/2021 11:19:44'!
% anInteger
	"Answer the reduction of the receiver modulo anInteger."
	self scalars = ℚ ifFalse: [^ DomainError signal: 'not over the rationals'].
	^ self over: ℤ / anInteger! !

!EllipticCurve methodsFor: 'converting' stamp: 'len 6/18/2023 11:10:36'!
over: aRing
	^ self class coefficients: (coefficients over: aRing)! !

!EllipticCurve methodsFor: 'morphisms' stamp: 'len 6/20/2023 15:26:21'!
id
	^ EllipticCurveIsomorphism from: self to: self u: 1 r: 0 s: 0 t: 0! !

!EllipticCurve methodsFor: 'morphisms' stamp: 'len 6/18/2023 22:35:40'!
negationMap
	^ self notYetImplemented! !

!EllipticCurve methodsFor: 'morphisms' stamp: 'len 6/20/2023 15:30:12'!
u: u r: r s: s t: t
	"See [Sil08, Table 3.1 Change-of-variable formulas for Weierstrass equations]."
	| a₁ a₂ a₃ a₄ a₆ codomain α₁ α₂ α₃ α₄ α₆ |
	a₁ := self a₁.
	a₂ := self a₂.
	a₃ := self a₃.
	a₄ := self a₄.
	a₆ := self a₆.
	α₁ := a₁ + (s*2) / u.
	α₂ := a₂ - (s*a₁) + (r*3) - s squared / u squared.
	α₃ := a₃ + (r*a₁) + (t*2) / (u^3).
	α₄ := a₄ - (s*a₃) + (r*2*a₂) - (r*s + t * a₁) + (r squared * 3) - (s*t*2) / (u^4).
	α₆ := a₆ + (r*a₄) + (r squared * a₂) + (r^3) - (t*a₃) - t squared - (r*t*a₁) / (u^6).
	codomain := EllipticCurve coefficients: self scalars !!!! {α₁. α₂. α₃. α₄. α₆}.
	^ EllipticCurveIsomorphism from: self to: codomain u: u r: r s: s t: t! !

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

!EllipticCurve methodsFor: 'points' stamp: 'len 12/22/2016 23:19:49'!
pointsOver: aField
	"Answer the group of K-rational points on the receiver."
	^ EllipticCurveGroup on: self over: aField! !

!EllipticCurve methodsFor: 'points' stamp: 'len 6/18/2023 22:33:58'!
zero
	^ self points identity! !

!EllipticCurve methodsFor: 'points-arithmetic' stamp: 'len 6/21/2023 16:26:48'!
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

!EllipticCurve methodsFor: 'points-arithmetic' stamp: 'len 6/21/2023 22:11:51'!
pointDouble: P
	"Point doubling [Coh96, Proposition 7.1.7]."
	| x₁ y₁ v λ x₃ y₃ |
	P isZero ifTrue: [^ P].
	x₁ := P x. y₁ := P y.
	(v := y₁ * 2 + (self a₁ * x₁) + self a₃) isZero ifTrue: [^ P zero].
	λ := x₁ squared * 3 + (self a₂ * x₁ * 2) + self a₄ - (self a₁ * y₁) / v.
	x₃ := x₁*(-2) - self a₂ + (λ*(λ + self a₁)).
	y₃ := y₁ negated - self a₃ - (self a₁ * x₃) + (λ*(x₁ - x₃)).
	^ P shallowCopy x: x₃ y: y₃! !

!EllipticCurve methodsFor: 'points-arithmetic' stamp: 'len 6/21/2023 16:18:21'!
pointNegate: P
	| x |
	P isZero ifTrue: [^ P].
	x := P x.
	^ P shallowCopy x: x y: P y negated - (self a₁ * x) - (self a₃)! !

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

!EllipticCurve methodsFor: 'testing' stamp: 'len 6/19/2023 12:19:22'!
isSmooth
	self flag: #fixme. "assumes the curve is defined over a field, over a ring it should test isUnit"
	^ self discriminant isZero not! !

!EllipticCurve methodsFor: 'testing' stamp: 'len 6/21/2023 16:49:48'!
isSupersingular
	(self scalars isKindOf: PrimeField) ifFalse: [^ self notYetImplemented].
	self scalars characteristic <= 3 ifTrue: [^ self j isZero].
	^ self scalars characteristic + 1 = self points size! !

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

!EllipticCurve class methodsFor: 'examples' stamp: 'len 6/18/2023 15:15:48'!
bn254
	"bn254 pairing-friendly curve.
	p = 16r30644E72E131A029B85045B68181585D97816A916871CA8D3C208C16D87CFD47.
	n = 16r30644E72E131A029B85045B68181585D2833E84879B9709143E1F593F0000001.
	Field: GF(p)
	Generator: (1, 2)
	Order: n
	Cofactor: 1"
	| F |
	F := PrimeField new: 16r30644E72E131A029B85045B68181585D97816A916871CA8D3C208C16D87CFD47.
	^ self coefficients: ((0,0,0,0,3) over: F)! !

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

!EllipticCurve class methodsFor: 'examples' stamp: 'len 6/18/2023 15:12:56'!
pallas
	"Pallas curve from the Pasta curves. See also >>#vesta.
	p = 16r40000000000000000000000000000000224698FC094CF91B992D30ED00000001.
	q = 16r40000000000000000000000000000000224698FC0994A8DD8C46EB2100000001.
	Field: GF(p)
	Generator: (-1, 2)
	Order: q
	Cofactor: 1"
	| F |
	F := PrimeField new: 16r40000000000000000000000000000000224698FC094CF91B992D30ED00000001.
	^ self coefficients: ((0,0,0,0,5) over: F)! !

!EllipticCurve class methodsFor: 'examples' stamp: 'len 6/18/2023 14:56:23'!
secp256k1
	"Answer the secp256k1 elliptic curve, used by Bitcoin.
	The group of this curve has order 115792089237316195423570985008687907852837564279074904382605163141518161494337."
	| F |
	F := PrimeField new: 16rFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F.
	^ self coefficients: ((0,0,0,0,7) over: F)! !

!EllipticCurve class methodsFor: 'examples' stamp: 'len 6/18/2023 15:13:01'!
vesta
	"Vesta curve from the Pasta curves. See also >>#pallas.
	p = 16r40000000000000000000000000000000224698FC094CF91B992D30ED00000001.
	q = 16r40000000000000000000000000000000224698FC0994A8DD8C46EB2100000001.
	Field: GF(q)
	Generator: (-1, 2)
	Order: p
	Cofactor: 1"
	| F |
	F := PrimeField new: 16r40000000000000000000000000000000224698FC0994A8DD8C46EB2100000001.
	^ self coefficients: ((0,0,0,0,5) over: F)! !

!EllipticCurve class methodsFor: 'instance creation' stamp: 'len 6/19/2023 11:57:54'!
coefficients: aTuple
	(aTuple size = 2 or: [(1 to: 3) allSatisfy: [:i| aTuple isZeroAt: i]]) ifTrue: [^ WeierstrassCurve coefficients: aTuple].
	^ self new coefficients: aTuple! !

!EllipticCurve class methodsFor: 'instance creation' stamp: 'len 6/19/2023 11:48:22'!
c₄: c₄ c₆: c₆
	"Answer an elliptic curve with the given c₄ and c₆ invariants."
	| K a₄ a₆ |
	K := c₄ parent fractions.
	a₄ := (K !! -48) inverse * c₄.
	a₆ := (K !! -864) inverse * c₆.
	^ self coefficients: (K !!!! {a₄. a₆})! !

!WeierstrassCurve methodsFor: 'points-arithmetic' stamp: 'len 6/18/2023 13:59:39'!
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

!WeierstrassCurve methodsFor: 'points-arithmetic' stamp: 'len 6/21/2023 22:16:09'!
pointDouble: P
	| x₁ y₁ v λ x₃ y₃ | 
	P isZero ifTrue: [^ P].
	y₁ := P y.
	(v := y₁ * 2) isZero ifTrue: [^ P zero]. "P = -P"
	x₁ := P x.
	λ := x₁ squared * 3 + self a₄ / v.
	x₃ := λ squared - (x₁*2).
	y₃ := λ * (x₁ - x₃) - y₁.
	^ P shallowCopy x: x₃ y: y₃! !

!WeierstrassCurve methodsFor: 'points-arithmetic' stamp: 'len 6/18/2023 12:43:59'!
pointNegate: P
	^ P isZero ifTrue: [P] ifFalse: [P shallowCopy x: P x y: P y negated]! !

!WeierstrassCurve class methodsFor: 'instance creation' stamp: 'len 6/18/2023 14:09:15'!
coefficients: aTuple
	aTuple size = 2 ifTrue: [^ self new coefficients: aTuple scalars !!!! {0. 0. 0} , aTuple].
	self assert: ((1 to: 3) allSatisfy: [:i| aTuple isZeroAt: i]).
	^ self new coefficients: aTuple! !

!EllipticCurvePoint methodsFor: 'accessing' stamp: 'len 6/19/2023 00:55:55'!
order
	^ self parent orderOf: self! !

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

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 6/19/2023 00:54:05'!
/ anInteger
	^ (self divisionPoints: anInteger) anyOne! !

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

!EllipticCurvePoint methodsFor: 'testing' stamp: 'len 6/19/2023 17:14:36'!
isDivisibleBy: anInteger
	"Answer true if there's a point Q such that the receiver equals Q*anInteger."
	^ (self divisionPoints: anInteger) notEmpty! !

!EllipticCurvePoint methodsFor: 'testing' stamp: 'len 6/18/2023 12:26:49'!
isZero
	^ (coordinates isZeroAt: 1) and: [coordinates isZeroAt: 3]! !

!EllipticCurvePoint methodsFor: 'pairings' stamp: 'len 6/19/2023 23:31:37'!
atePairing: Q torsion: n embeddingDegree: k trace: t
	| K |
	K := self codomain scalars.
	K degree = k ifTrue: [^ self atePairing: Q torsion: n embeddingDegree: k trace: t order: K scalars size].
	^ self error: 'unexpected embedding degree'! !

!EllipticCurvePoint methodsFor: 'pairings' stamp: 'len 6/19/2023 23:31:57'!
atePairing: Q torsion: n embeddingDegree: k trace: t order: q
	| E π |
	E := self codomain.
	self assert: Q codomain = E.
	self assert: (self*n) isZero.
	π := E pointAt: {self x ^ q. self y ^ q}.
	(π - self) isZero ifFalse: [^ self error: 'not in the kernel of π - 1'].
	π := E pointAt: {Q x ^ q. Q y ^ q}.
	(π - (Q*q)) isZero ifFalse: [^ self error: 'not in the kernel of π - q'].
	^ (Q miller: self torsion: t - 1) ^ (q^k - 1 / n)! !

!EllipticCurvePoint methodsFor: 'pairings' stamp: 'len 6/19/2023 23:32:13'!
tatePairing: Q torsion: n embeddingDegree: k
	| K d |
	K := self codomain scalars.
	d := K degree.
	d = 1 ifTrue: [^ self tatePairing: Q torsion: n embeddingDegree: k order: K size].
	d = k ifTrue: [^ self tatePairing: Q torsion: n embeddingDegree: k order: K scalars size].
	^ self error: 'unexpected embedding degree'! !

!EllipticCurvePoint methodsFor: 'pairings' stamp: 'len 6/20/2023 19:27:57'!
tatePairing: Q torsion: n embeddingDegree: k order: q
	| R |
	self assert: Q codomain = self codomain.
	self assert: (self*n) isZero.
	"This can fail if Q is in one of the lines of the Miller algorithm:"
	(self miller: Q torsion: n) ifNotNil: [:anElement| ^ anElement ^ (q^k - 1 / n)].
	"If it failed, we translate by a random point and try again:"
	R := self parent atRandom.
	^ (self tatePairing: Q + R torsion: n embeddingDegree: k order: q) / (self tatePairing: R torsion: n embeddingDegree: k order: q)! !

!EllipticCurvePoint methodsFor: 'pairings' stamp: 'len 6/19/2023 23:32:53'!
weilPairing: Q torsion: n
	"Answer the Weil pairing of the receiver with aPoint."
	| P E |
	P := self.
	E := self codomain.
	self assert: Q codomain = E.
	self assert: (P*n) isZero.
	self assert: (Q*n) isZero.
	(P isZero or: [Q isZero or: [P = Q]]) ifTrue: [^ E scalars one].
	^ (P miller: Q torsion: n) / (Q miller: P torsion: n) * (-1 ^ (n ∧ 1))! !

!EllipticCurvePoint methodsFor: 'pairings-private' stamp: 'len 6/20/2023 20:25:18'!
line: R at: Q
	"Answer the value at Q of the line through the receiver and R."
	| P λ E denominator |
	P := self.
	(P isZero or: [R isZero]) ifTrue:
		[P = R ifTrue: [^ self codomain scalars one].
		P isZero ifTrue: [^ Q x - R x].
		"Then R is zero:"
		^ Q x - P x].
	P = R
		ifTrue:
			[E := self codomain.
			(denominator := P y * 2 + (P x * E a₁) + E a₃) isZero ifTrue: [^ Q x - P x].
			λ := P x squared * 3 + (P x * E a₂ * 2) + E a₄ - (P y * E a₁) * denominator inverse]
		ifFalse:
			[P x = R x ifTrue: [^ Q x - P x].
			λ := (R y - P y) * (R x - P x) inverse].
	^ Q y - P y - (Q x - P x * λ)! !

!EllipticCurvePoint methodsFor: 'pairings-private' stamp: 'len 6/20/2023 21:33:57'!
miller: Q torsion: anInteger
	"Miller's algorithm [Mil04]."
	| n answer i ℓ v V S |
	Q isZero ifTrue: [^ nil].
	self assert: anInteger ~= 0.
	n := anInteger abs.
	answer := self codomain scalars one.
	V := self.
	i := n highBit - 1.
	[i > 0] whileTrue:
		[S := V * 2.
		ℓ := V line: V at: Q.
		v := S line: S negated at: Q.
		v isZero ifTrue: [^ nil].
		answer := answer squared * ℓ * v inverse.
		V := S.
		(n bitAt: i) = 1 ifTrue:
			[S := V + self.
			ℓ := V line: self at: Q.
			v := S line: S negated at: Q.
			v isZero ifTrue: [^ nil].
			answer := answer * ℓ * v inverse.
			V := S].
		i := i - 1].
	anInteger < 0 ifTrue:
		[v := V line: V negated at: Q.
		answer := answer * v.
		answer isZero ifTrue: [^ nil].
		answer := answer inverse].
	^ answer! !

!EllipticCurvePoint methodsFor: 'private' stamp: 'len 6/19/2023 17:19:18'!
x: x y: y
	"Warning: this assumes the receiver is not the point at infinity, i.e. z = 1."
	coordinates := coordinates copy at: 1 put: x; at: 2 put: y; yourself! !

!EllipticCurvePoint methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 16:42:04'!
divisionPoints: anInteger
	"Answer all the points Q such that the receiver equals Q*anInteger."
	^ Iterator on: [:aBlock| self divisionPoints: anInteger do: aBlock]! !

!EllipticCurvePoint methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 16:53:35'!
divisionPoints: anInteger do: aBlock
	"Iterate over all the points Q such that the receiver equals Q*anInteger."
	| nP isTwoTorsion Q nQ mQ |
	(anInteger = 1 or: [anInteger = -1]) ifTrue: [aBlock value: self. ^ self].
	anInteger = 0 ifTrue: [self isZero ifTrue: [aBlock value: self]. ^ self].
	nP := self negated.
	isTwoTorsion := self = nP.
	(self divisionPolynomial: anInteger) roots do: [:x|
		Q := (self parent pointsWithX: x) anyOne.
		nQ := Q negated.
		mQ := Q * anInteger.
		self = mQ ifTrue: [aBlock value: Q].
		isTwoTorsion
			ifTrue: [nQ = Q ifFalse: [aBlock value: nQ]]
			ifFalse: [mQ = nP ifTrue: [aBlock value: nQ]]]! !

!EllipticCurvePoint methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 16:32:03'!
divisionPolynomial: anInteger
"	nP := self negated.
	is2Torsion := self = nP."
	self isZero ifTrue: [^ self codomain divisionPolynomial: anInteger].
	^ self notYetImplemented! !

!EllipticCurveMorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:50:56'!
dual
	^ self subclassResponsibility! !

!EllipticCurveMorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:07:14'!
isInjective
	self isSeparable ifTrue: [^ self degree = 1].
	self notYetImplemented! !

!EllipticCurveMorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:06:09'!
isSurjective
	^ self degree > 0! !

!EllipticCurveMorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:08:04'!
isZero
	^ self degree = 0! !

!EllipticCurveMorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:50:32'!
kernelPolynomial
	^ self subclassResponsibility! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 18:07:06'!
degree
	| Φ₁ |
	Φ₁ := self xRationalFunction.
	^ Φ₁ numerator degree max: Φ₁ denominator degree! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 3/6/2017 16:48:06'!
dual
	"Answer the dual of the receiver. If the receiver is f:E->E' of degree n, its dual is an isogeny g:E'->E such that the compositions are [n] (multiplication by n)."
	^ self notYetImplemented! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:41:47'!
isSeparable
	^ true! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 12/25/2016 08:42:27'!
isSurjective
	^ self isZero not! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 12/25/2016 08:43:44'!
isZero
	^ self degree = 0! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 12/28/2016 11:16:46'!
kernelPolynomial
	^ self propertyAt: #kernelPolynomial ifAbsentPut: [self computeKernelPolynomial]! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:36:28'!
value: aPoint
	aPoint isZero ifTrue: [^ self codomain zero].
	self notYetImplemented! !

!EllipticCurveIsogeny class methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:24:40'!
from: anEllipticCurve kernel: aCollection
	"Answer an isogeny from anEllipticCurve with kernel given by a list of points."
	^ self notYetImplemented! !

!EllipticCurveIsogeny class methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:24:01'!
from: anEllipticCurve kernelPolynomial: aPolynomial
	^ self notYetImplemented! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 12:03:31'!
= anObject
	self class = anObject class ifFalse: [^ super = anObject].
	^ self domain = anObject domain and: [self codomain = anObject codomain and: [u = anObject u and: [r = anObject r and: [s = anObject s and: [t = anObject t]]]]]! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:47:57'!
dual
	^ self inverse! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:40:41'!
inverse
	| r₂ s₂ u₂ |
	^ self class from: self codomain to: self domain u: (u₂ := u inverse) r: (r₂ := r negated * u₂ squared) s: (s₂ := s negated * u₂) t: r₂ * s₂ * (u₂ ^ 3)! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:33:03'!
r
	^ r! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 12:06:08'!
rationalFunctions
	^ self value: self domain functionField generators! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:33:07'!
s
	^ s! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:33:14'!
t
	^ t! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:33:18'!
u
	^ u! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:39:13'!
u: u₀ r: r₀ s: s₀ t: t₀
	u := u₀.
	r := r₀.
	s := s₀.
	t := t₀! !

!EllipticCurveIsomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 12:05:50'!
xRationalFunction
	^ (self domain functionField x: 1) - r / u squared! !

!EllipticCurveIsomorphism class methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:40:22'!
from: anEllipticCurve to: anotherEllipticCurve u: u₀ r: r₀ s: s₀ t: t₀
	^ self new domain: anEllipticCurve; codomain: anotherEllipticCurve; u: u₀ r: r₀ s: s₀ t: t₀! !
