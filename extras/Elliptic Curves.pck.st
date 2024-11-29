'From Cuis6.3 [latest update: #6222] on 24 November 2024 at 8:35:31 pm'!
'Description '!
!provides: 'Elliptic Curves' 1 27!
!requires: 'Algebra' 1 0 nil!
SystemOrganization addCategory: #'Elliptic Curves'!


!classDefinition: #EllipticCurveGroup category: #'Elliptic Curves'!
AbelianGroup subclass: #EllipticCurveGroup
	instanceVariableNames: 'scalars codomain'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveGroup class' category: #'Elliptic Curves'!
EllipticCurveGroup class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurve category: #'Elliptic Curves'!
ProjectivePlaneCurve subclass: #EllipticCurve
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurve class' category: #'Elliptic Curves'!
EllipticCurve class
	instanceVariableNames: ''!

!classDefinition: #WeierstrassCurve category: #'Elliptic Curves'!
EllipticCurve subclass: #WeierstrassCurve
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'WeierstrassCurve class' category: #'Elliptic Curves'!
WeierstrassCurve class
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

!classDefinition: #EllipticCurveMap category: #'Elliptic Curves'!
RationalMap subclass: #EllipticCurveMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveMap class' category: #'Elliptic Curves'!
EllipticCurveMap class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveFrobeniusIsogeny category: #'Elliptic Curves'!
EllipticCurveMap subclass: #EllipticCurveFrobeniusIsogeny
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveFrobeniusIsogeny class' category: #'Elliptic Curves'!
EllipticCurveFrobeniusIsogeny class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveIsogeny category: #'Elliptic Curves'!
EllipticCurveMap subclass: #EllipticCurveIsogeny
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveIsogeny class' category: #'Elliptic Curves'!
EllipticCurveIsogeny class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveIsomorphism category: #'Elliptic Curves'!
EllipticCurveMap subclass: #EllipticCurveIsomorphism
	instanceVariableNames: 'u r s t'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveIsomorphism class' category: #'Elliptic Curves'!
EllipticCurveIsomorphism class
	instanceVariableNames: ''!

!classDefinition: #EllipticCurveScalingEndomorphism category: #'Elliptic Curves'!
EllipticCurveMap subclass: #EllipticCurveScalingEndomorphism
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Elliptic Curves'!
!classDefinition: 'EllipticCurveScalingEndomorphism class' category: #'Elliptic Curves'!
EllipticCurveScalingEndomorphism class
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
K-rational points on an elliptic curve E, seen as morphisms Spec(K) → E. These objects implement the group law (with additive notation). The identity element (zero, or the point at infinity) is [0:1:0].

Following conventions, a point [a:b:1] is printed '(a,b)', and the point at infinity [0:1:0] is printed 'O'.

See also EllipticCurve.!

!EllipticCurveIsogeny commentStamp: '<historical>' prior: 0!
Scheme morphisms of elliptic curves E₁ → E₂ over a field that send the identity of E₁ to the identity of E₂. They are homomorphisms of group schemes.!

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

!EllipticCurveGroup methodsFor: 'enumerating' stamp: 'len 6/22/2023 02:15:46'!
do: aBlock
	aBlock value: self identity.
	scalars do: [:each| self pointsWithX: each do: aBlock]! !

!EllipticCurveGroup methodsFor: 'morphisms' stamp: 'len 4/6/2024 15:54:37'!
to: aCodomain adapt: anObject
	((anObject isKindOf: EllipticCurveIsogeny) and: [anObject domain = codomain and: [anObject codomain = aCodomain codomain]]) ifTrue: [^ self to: aCodomain map: [:P| anObject value: P]].
	^ super to: aCodomain adapt: anObject! !

!EllipticCurveGroup methodsFor: 'operations' stamp: 'len 6/22/2023 15:40:36'!
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
	s := self orderOf: P₁ multiple: r factorization: factors.
	t := self orderOf: Q₁ multiple: r factorization: factors.
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

!EllipticCurveGroup methodsFor: 'random' stamp: 'len 5/1/2024 12:24:55'!
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
	(2 atRandom: aRandom) = 1 ifTrue: [^ points any]] repeat! !

!EllipticCurveGroup methodsFor: 'testing' stamp: 'len 6/19/2023 16:52:44'!
isX: anElement
	"Answer true if anElement is the x coordinate of any point of the receiver."
	^ (self pointsWithX: anElement) notEmpty! !

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

!EllipticCurve methodsFor: 'accessing' stamp: 'len 11/23/2024 09:02:54'!
discriminant
	"Note: an elliptic curve is non-singular if the discriminant is a unit (nonzero over a field)."
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

!EllipticCurve methodsFor: 'converting' stamp: 'len 4/5/2024 19:03:48'!
% anInteger
	"Answer the reduction of the receiver modulo anInteger."
	self scalars = ℚ ifFalse: [^ self error: 'not over the rationals'].
	^ self over: ℤ / anInteger! !

!EllipticCurve methodsFor: 'converting' stamp: 'len 6/22/2023 16:00:40'!
asShortWeierstrassModel
	"Answer a curve of the form y² = x³ + a₄x + a₆ isomorphic to the receiver." 
	| K |
	K := self scalars.
	self assert: K characteristic ~= 2.
	self b₂ isZero ifTrue: [^ WeierstrassCurve coefficients: K !!!! {self b₄ * 8. self b₆ * 16}].
	K characteristic = 3 ifTrue: [^ self error: 'no short Weierstrass model for characteristic 3 with nonzero b₂'].
	^ WeierstrassCurve coefficients: K !!!! {self c₄ * -27. self c₆ * -54}! !

!EllipticCurve methodsFor: 'converting' stamp: 'len 6/18/2023 11:10:36'!
over: aRing
	^ self class coefficients: (coefficients over: aRing)! !

!EllipticCurve methodsFor: 'frobenius' stamp: 'len 5/1/2024 12:26:52'!
frobenius
	"Answer the Frobenius of the receiver as an element in the Frobenius quadratic order (or ℤ)."
	^ self frobeniusOrder degree = 1 ifTrue: [self frobeniusPolynomial roots any] ifFalse: [self frobeniusOrder x]! !

!EllipticCurve methodsFor: 'frobenius' stamp: 'len 6/26/2023 01:43:07'!
frobeniusDiscriminant
	^ self frobeniusPolynomial discriminant! !

!EllipticCurve methodsFor: 'frobenius' stamp: 'len 7/13/2023 11:44:24'!
frobeniusMap
	"Answer the Frobenius endomorphism as an isogeny."
	^ self notYetImplemented! !

!EllipticCurve methodsFor: 'frobenius' stamp: 'len 6/26/2023 01:46:43'!
frobeniusOrder
	self flag: #fixme.
	^ ℤ extension: self frobeniusPolynomial "or one of its factors?"! !

!EllipticCurve methodsFor: 'frobenius' stamp: 'len 6/26/2023 01:29:53'!
frobeniusPolynomial
	"Answer the characteristic polynomial of the Frobenius endomorphism."
	^ ℤ polynomials !! {self scalars size. self trace negated. 1}! !

!EllipticCurve methodsFor: 'frobenius' stamp: 'len 6/26/2023 01:24:30'!
trace
	"Answer the trace of Frobenius of this elliptic curve."
	^ 1 + self scalars size - self points size! !

!EllipticCurve methodsFor: 'morphisms' stamp: 'len 6/20/2023 15:26:21'!
id
	^ EllipticCurveIsomorphism from: self to: self u: 1 r: 0 s: 0 t: 0! !

!EllipticCurve methodsFor: 'morphisms' stamp: 'len 6/27/2023 13:33:00'!
multiplicationBy: anInteger
	| n K x y P numerator denominator f₁ f₂ |
	self assert: anInteger ~= 0.
	n := anInteger abs.
	P := {-1. n-1. n. n+1} collect: [:i| self divisionPolynomialXNoTwoTorsion: i].
	x := P first parent x.
	numerator := n even
		ifTrue: [x * (P at: 1) * (P at: 3) squared - ((P at: 2) * (P at: 4))]
		ifFalse: [x * (P at: 3) squared - ((P at: 1) * (P at: 2) * (P at: 4))].
	denominator := (P at: 3) squared.
	n even ifTrue: [denominator := denominator * (P at: 1)].
	K := self ambient functionField.
	x := K x: 1. y := K x: 2.
	f₁ := K !! numerator / (K !! denominator).
	f₂ := (y*2 + (x * self a₁) + self a₃) * (f₁ derivativeIn: 1) / anInteger - (f₁ * self a₁) - self a₃ / 2.
	^ self to: self representatives: {f₁. f₂}! !

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

!EllipticCurve methodsFor: 'operations' stamp: 'len 11/11/2024 14:34:57'!
divisionPolynomials
	"Answer the sequence of division polynomials.
	Reference: Silverman, 'The Arithmetic of Elliptic Curves', Exercise 3.7, pag. 105."
	| R x y ψ₁ ψ₂ ψ₃ ψ₄ a₁ a₃ b₂ b₄ b₆ b₈ |
	R := self scalars polynomialsIn: #(x y). x := R x. y := R y.
	a₁ := self a₁.
	a₃ := self a₃.
	b₂ := self b₂.
	b₄ := self b₄.
	b₆ := self b₆.
	b₈ := self b₈.
	ψ₁ := R one.
	ψ₂ := y*2 + (a₁ * x) + a₃.
	ψ₃ := x^4 * 3 + (x^3 * b₂) + (x^2 * 3 * b₄) + (x * 3 * b₆) + b₈.
	ψ₄ := ψ₂ * (x^6 * 2 + (x^5 * b₂) + (x^4 * b₄ * 5) + (x^3 * 10 * b₆) + (x^2 * 10 * b₈) + (x * (b₂ * b₈ - (b₄ * b₆))) + (b₄ * b₈ - b₆ squared)).
	^ Sequence initialValues: {ψ₁. ψ₂. ψ₃. ψ₄} recurrence: [:ψ :n|
		| m |
		n odd
			ifTrue:
				[m := n - 1 // 2.
				(ψ at: m+2) * ((ψ at: m) ^ 3) - ((ψ at: m - 1) * ((ψ at: m + 1) ^ 3))]
			ifFalse:
				[m := n // 2.
				(ψ at: m-1) squared * (ψ at: m) * (ψ at: m+2) - ((ψ at: m-2) * (ψ at: m) * (ψ at: m+1) squared / ψ₂)]]! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/22/2023 15:28:57'!
hasseInvariant
	"Answer the Hasse invariant of the receiver, which is zero if and only if the curve is supersingular.
	The Hasse invariant is only defined over fields of positive characteristic."
	| K p E x |
	K := self scalars.
	(p := K characteristic) > 0 ifFalse: [^ self error: 'Hasse invariant only defined in positive characteristic'].
	p = 2 ifTrue: [^ self a₁].
	p = 3 ifTrue: [^ self b₂].
	p = 5 ifTrue: [^ self c₄].
	p = 7 ifTrue: [^ self c₆ negated].
	E := self asShortWeierstrassModel.
	x := K polynomials x.
	^ (x^3 + (x * E a₄) + E a₆) ^ (p - 1 / 2) at: p - 1! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/22/2023 14:44:10'!
quadraticTwist
	"Assuming the base field is finite, answer the quadratic twist of the receiver (up to isomorphism)."
	| K D a x |
	K := self scalars.
	K isFiniteField ifFalse: [^ self error: 'infinite field'].
	K characteristic ~= 2 ifTrue:
		[[(D := K atRandom) isSquare] whileTrue.
		^ self quadraticTwistBy: D].
	K degree odd ifTrue: [^ self quadraticTwistBy: K one].
	D := K x.
	a := D squared.
	x := K polynomials x.
	[(x squared + x + D) roots notEmpty] whileTrue: [D := D * a].
	^ self quadraticTwistBy: D! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/22/2023 14:42:49'!
quadraticTwistBy: anElement
	"Answer the quadratic twist by the given twisting parameter D.
	In characteristic 2, the twist is always isomorphic to the original curve after adjoining a root of x² + x + D to the base field.
	Otherwise, D must be nonzero and the twist is isomorphic to the original curve after adjoining √D to the base field."
	| K D E₀ |
	K := self scalars.
	D := K !! anElement.
	K characteristic ~= 2 ifTrue:
		[D isZero ifTrue: [^ self error: 'twisting parameter must be nonzero'].
		^ EllipticCurve coefficients: K !!!! {0. self b₂ * D. 0. self b₄ * 8 * D squared. self b₆ * 16 * (D ^ 3)}].
	self j isZero ifTrue: [^ self notYetImplemented].
	E₀ := self u: self a₁ r: self a₃ / self a₁ s: 0 t: self a₁ squared * self a₄ + self a₃ squared / (self a₁ ^ 3).
	self assert: E₀ a₁ isOne.
	self assert: E₀ a₃ isZero.
	self assert: E₀ a₄ isZero.
	^ EllipticCurve coefficients: K !!!! {1. E₀ a₂ + D. 0. 0. E₀ a₆}! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/22/2023 15:12:15'!
quadraticTwistOf: anEllipticCurve
	"If the receiver is a quadratic twist of the other curve, answer the corresponding twisting parameter.
	Otherwise return zero."
	| K j |
	(K := self scalars) = anEllipticCurve scalars ifFalse: [^ K zero].
	(j := self j) = anEllipticCurve j ifFalse: [^ K zero].
"	self ~ anEllipticCurve ifTrue: [^ K one]."
	K characteristic = 2 ifTrue: [^ self notYetImplemented].
	K characteristic = 3 ifTrue:
		[j isZero ifTrue: [^ self notYetImplemented].
		^ self b₂ / anEllipticCurve b₂].
	j isZero ifTrue:
		[(K polynomials x^3 - (self c₆ / anEllipticCurve c₆)) roots do: [:any| ^ any].
		^ K zero].
	j = (K !! 1728) ifTrue:
		[(K polynomials x squared - (self c₄ / anEllipticCurve c₄)) roots do: [:any| ^ any].
		^ K zero].
	^ self c₆ * anEllipticCurve c₄ / (self c₄ * anEllipticCurve c₆)! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/22/2023 14:27:28'!
quarticTwistBy: anElement
	"Answer the quartic twist by the given parameter."
	| K D |
	K := self scalars.
	D := K !! anElement.
	(K characteristic = 2 or: [K characteristic = 3]) ifTrue: [^ self error: 'quartic twist not defined in characteristic 2 or 3'].
	self j = (K !! 1728) ifFalse: [^ self error: 'quartic twist only defined when j = 1728'].
	D isZero ifTrue: [^ self error: 'twisting parameter must be nonzero'].
	self assert: self c₆ isZero.
	^ EllipticCurve coefficients: K !!!! {0. 0. 0. self c₄*D*-27. 0}! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/22/2023 15:15:08'!
quarticTwistOf: anEllipticCurve
	"If the receiver is a quartic twist of the other curve, answer the corresponding twisting parameter.
	Otherwise return zero."
	| K j |
	(K := self scalars) = anEllipticCurve scalars ifFalse: [^ K zero].
	((j := self j) = anEllipticCurve j and: [j = (K !! 1728)]) ifFalse: [^ K zero].
"	self ~ anEllipticCurve ifTrue: [^ K one]."
	(K characteristic = 2 or: [K characteristic = 3]) ifTrue: [^ self notYetImplemented].
	^ anEllipticCurve c₄ / self c₄! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/22/2023 14:29:33'!
sexticTwistBy: anElement
	"Answer the sextic twist by the given parameter."
	| K D |
	K := self scalars.
	D := K !! anElement.
	(K characteristic = 2 or: [K characteristic = 3]) ifTrue: [^ self error: 'sextic twist not defined in characteristic 2 or 3'].
	self j isZero ifFalse: [^ self error: 'sextic twist only defined when j = 0'].
	D isZero ifTrue: [^ self error: 'twisting parameter must be nonzero'].
	self assert: self c₄ isZero.
	^ EllipticCurve coefficients: K !!!! {0. 0. 0. 0. self c₆*D*-54}! !

!EllipticCurve methodsFor: 'operations' stamp: 'len 6/22/2023 15:16:53'!
sexticTwistOf: anEllipticCurve
	"If the receiver is a sextic twist of the other curve, answer the corresponding twisting parameter.
	Otherwise return zero."
	| K |
	(K := self scalars) = anEllipticCurve scalars ifFalse: [^ K zero].
	(self j isZero and: [anEllipticCurve j isZero]) ifFalse: [^ K zero].
"	self ~ anEllipticCurve ifTrue: [^ K one]."
	(K characteristic = 2 or: [K characteristic = 3]) ifTrue: [^ self notYetImplemented].
	^ anEllipticCurve c₆ / self c₆! !

!EllipticCurve methodsFor: 'points' stamp: 'len 6/27/2023 15:32:05'!
pointAt: aTuple
	aTuple size = 2 ifTrue: [^ EllipticCurvePoint on: self coordinates: aTuple, aTuple scalars one].
	self assert: aTuple size = 3.
"	(aTuple at: 3) isOne ifFalse: [^ EllipticCurvePoint on: self coordinates: aTuple / (aTuple at: 3)]."
	^ EllipticCurvePoint on: self coordinates: aTuple! !

!EllipticCurve methodsFor: 'points' stamp: 'len 12/22/2016 23:19:49'!
pointsOver: aField
	"Answer the group of K-rational points on the receiver."
	^ EllipticCurveGroup on: self over: aField! !

!EllipticCurve methodsFor: 'points' stamp: 'len 6/22/2023 02:16:41'!
pointsOver: aRing do: aBlock
	(self pointsOver: aRing) do: aBlock! !

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

!EllipticCurve methodsFor: 'printing' stamp: 'len 6/21/2023 23:57:10'!
printOn: aStream
	| P x y |
	P := self scalars polynomialsIn: #(x y) ordering: (MonomialOrdering glex: #(2 1)).
	x := P x.
	y := P y.
	aStream print: y^2 + (x*y * self a₁) + (y * self a₃).
	aStream
		nextPutAll: ' = ';
		print: (x^3) + (x^2 * self a₂) + (x * self a₄) + self a₆! !

!EllipticCurve methodsFor: 'testing' stamp: 'len 6/26/2023 01:50:19'!
isOrdinary
	^ self isSupersingular not! !

!EllipticCurve methodsFor: 'testing' stamp: 'len 6/19/2023 12:19:22'!
isSmooth
	self flag: #fixme. "assumes the curve is defined over a field, over a ring it should test isUnit"
	^ self discriminant isZero not! !

!EllipticCurve methodsFor: 'testing' stamp: 'len 6/26/2023 01:49:26'!
isSupersingular
	self scalars characteristic > 0 ifTrue: [^ self hasseInvariant isZero].
	"equivalently: the trace of Frobenius is divisible by the characteristic"
	^ self notYetImplemented! !

!EllipticCurve methodsFor: 'private' stamp: 'len 6/18/2023 11:08:14'!
coefficients: aTuple
	| f |
	self assert: aTuple size = 5.
	coefficients := aTuple.
	f := (aTuple scalars polynomialsIn: #(x y z))
		!! [:x :y :z| y^2 + (x*y * self a₁) + (y * self a₃) - (x^3) - (x^2 * self a₂) - (x * self a₄) - self a₆].
	f := f homogenizedIn: 3.
	self coordinateRing: f parent / f! !

!EllipticCurve methodsFor: 'private' stamp: 'len 5/1/2024 12:26:34'!
divisionPolynomial: n
	"Answer the n-th division polynomial, as a bivariate polynomial including a factor 2y+a₁x+a₃ having simple roots at the 2-torsion points."
	| cache x y φ |
	cache := self propertyAt: #divisionPolynomials ifAbsentPut: [Dictionary new].
	x := cache notEmpty ifTrue: [cache any parent x] ifFalse: [(self scalars polynomialsIn: #(x y)) x].
	φ := self divisionPolynomial: n at: x cache: cache.
	n even ifTrue: [y := x parent y. φ := φ * (y * 2 + (x * self a₁) + self a₃)].
	^ φ! !

!EllipticCurve methodsFor: 'private' stamp: 'len 4/5/2024 19:03:28'!
divisionPolynomial: n at: x cache: cache
	"Answer the n-th division polynomial, without the 2-torsion factor if n is even, evaluated at x.
	When n = -1 answer B₆ := (2y+a₁x+a₃)², and for n = -2 answer B₆²."
	cache at: n ifPresent: [:aPolynomial| ^ aPolynomial].
	n = -2 ifTrue: [^ cache at: n put: (self divisionPolynomial: -1 at: x cache: cache) squared].
	n = -1 ifTrue: [^ cache at: n put: x^3 * 4 + (x^2 * self b₂) + (x * self b₄ * 2) + self b₆].
	n <= 0 ifTrue: [Error signal].
	(n = 1 or: [n = 2]) ifTrue: [^ cache at: n put: x one].
	n = 3 ifTrue: [^ cache at: n put: x^4 * 3 + (x^3 * self b₂) + (x^2 * self b₄ * 3) + (x * self b₆ * 3) + self b₈].
	n = 4 ifTrue: [^ cache at: n put: (x^2 * 6 + (x * self b₂) + self b₄) * (self divisionPolynomial: 3 at: x cache: cache) - (self divisionPolynomial: -2 at: x cache: cache)].
	^ cache at: n put:
		(n even
			ifTrue:
				[| m |
				m := n - 2 // 2.
				(self divisionPolynomial: m+1 at: x cache: cache) *
				((self divisionPolynomial: m at: x cache: cache) squared * (self divisionPolynomial: m+3 at: x cache: cache) -
				((self divisionPolynomial: m+2 at: x cache: cache) squared * (self divisionPolynomial: m-1 at: x cache: cache)))]
			ifFalse:
				[| m |
				m := n - 1 // 2.
				m even
					ifTrue: [(self divisionPolynomial: m at: x cache: cache)^3 * (self divisionPolynomial: -2 at: x cache: cache) * (self divisionPolynomial: m+2 at: x cache: cache) - ((self divisionPolynomial: m+1 at: x cache: cache)^3 * (self divisionPolynomial: m-1 at: x cache: cache))]
					ifFalse: [(self divisionPolynomial: m at: x cache: cache)^3 * (self divisionPolynomial: m+2 at: x cache: cache) - ((self divisionPolynomial: m+1 at: x cache: cache)^3 * (self divisionPolynomial: m-1 at: x cache: cache) * (self divisionPolynomial: -2 at: x cache: cache))]])! !

!EllipticCurve methodsFor: 'private' stamp: 'len 5/1/2024 12:26:39'!
divisionPolynomialX: n
	"Answer the n-th division polynomial, as a univariate polynomial including a factor of degree 3 whose roots are the x-coordinates of the 2-torsion points."
	| cache x φ |
	cache := self propertyAt: #divisionPolynomialsX ifAbsentPut: [Dictionary new].
	x := cache notEmpty ifTrue: [cache any parent x] ifFalse: [self scalars polynomials x].
	φ := self divisionPolynomial: n at: x cache: cache.
	n even ifTrue: [φ := φ * (self divisionPolynomial: -1 at: x cache: cache)].
	^ φ! !

!EllipticCurve methodsFor: 'private' stamp: 'len 5/1/2024 12:26:45'!
divisionPolynomialXNoTwoTorsion: n
	"Answer the n-th division polynomial, as a univariate polynomial omiting factors corresponding to the x-coordinates of the 2-torsion points."
	| cache x |
	cache := self propertyAt: #divisionPolynomialsXNoTwoTorsion ifAbsentPut: [Dictionary new].
	x := cache notEmpty ifTrue: [cache any parent x] ifFalse: [self scalars polynomials x].
	^ self divisionPolynomial: n at: x cache: cache! !

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

!WeierstrassCurve methodsFor: 'converting' stamp: 'len 6/22/2023 16:00:53'!
asShortWeierstrassModel
	"Answer a curve of the form y² = x³ + a₄x + a₆ isomorphic to the receiver." 
	^ self! !

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

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 4/5/2024 19:01:20'!
* anInteger
	self assert: anInteger isInteger.
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

!EllipticCurvePoint methodsFor: 'arithmetic' stamp: 'len 5/1/2024 12:27:29'!
/ anInteger
	^ (self divisionPoints: anInteger) any! !

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

!EllipticCurvePoint methodsFor: 'pairings' stamp: 'len 10/12/2024 09:06:22'!
weilPairing: Q torsion: n
	"Answer the Weil pairing of the receiver with aPoint."
	| P E |
	P := self.
	E := self codomain.
	self assert: Q codomain = E.
	self assert: (P*n) isZero.
	self assert: (Q*n) isZero.
	(P isZero or: [Q isZero or: [P = Q]]) ifTrue: [^ E scalars one].
	^ (P miller: Q torsion: n) / (Q miller: P torsion: n) * (-1 ^ (n bitAnd: 1))! !

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

!EllipticCurvePoint methodsFor: 'as yet unclassified' stamp: 'len 5/1/2024 12:27:35'!
divisionPoints: anInteger do: aBlock
	"Iterate over all the points Q such that the receiver equals Q*anInteger."
	| nP isTwoTorsion Q nQ mQ |
	(anInteger = 1 or: [anInteger = -1]) ifTrue: [aBlock value: self. ^ self].
	anInteger = 0 ifTrue: [self isZero ifTrue: [aBlock value: self]. ^ self].
	nP := self negated.
	isTwoTorsion := self = nP.
	(self divisionPolynomial: anInteger) roots do: [:x|
		Q := (self parent pointsWithX: x) any.
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

!EllipticCurvePoint methodsFor: 'as yet unclassified' stamp: 'len 7/13/2023 11:41:44'!
frobenius
	"Answer the Frobenius of this point."
	^ self frobenius: 1! !

!EllipticCurvePoint methodsFor: 'as yet unclassified' stamp: 'len 7/13/2023 11:41:28'!
frobenius: anInteger
	"Answer the n-th Frobenius of this point."
	| e |
	e := self scalars characteristic ^ anInteger.
	^ self shallowCopy coordinates: (coordinates apply: [:each| each ^ e])! !

!EllipticCurvePoint methodsFor: 'as yet unclassified' stamp: 'len 6/2/2024 10:39:00'!
parent
	^ self domain ⇒ self codomain! !

!EllipticCurveMap methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:50:56'!
dual
	^ self subclassResponsibility! !

!EllipticCurveMap methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:07:14'!
isInjective
	self isSeparable ifTrue: [^ self degree = 1].
	self notYetImplemented! !

!EllipticCurveMap methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:06:09'!
isSurjective
	^ self degree > 0! !

!EllipticCurveMap methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:08:04'!
isZero
	^ self degree = 0! !

!EllipticCurveMap methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 19:50:32'!
kernelPolynomial
	^ self subclassResponsibility! !

!EllipticCurveFrobeniusIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:11:42'!
isInjective
	^ true! !

!EllipticCurveFrobeniusIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:11:52'!
isSeparable
	^ self degree = 1! !

!EllipticCurveFrobeniusIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:14:42'!
kernelPolynomial
	^ self domain scalars polynomials one! !

!EllipticCurveFrobeniusIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:13:03'!
scalingFactor
	^ self degree = 1 ifTrue: [self domain scalars one] ifFalse: [self domain scalars zero]! !

!EllipticCurveFrobeniusIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:13:59'!
value: aPoint
	aPoint isZero ifTrue: [^ aPoint].
	^ aPoint shallowCopy x: aPoint x ^ self degree y: aPoint y ^ self degree! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/19/2023 18:07:06'!
degree
	| Φ₁ |
	Φ₁ := self xRationalFunction.
	^ Φ₁ numerator degree max: Φ₁ denominator degree! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/29/2023 19:49:16'!
dual
	"Answer the dual of the receiver. If the receiver is f:E→E' of degree n, its dual is an isogeny g:E'→E such that the compositions are [n] (multiplication by n)."
	^ self notYetImplemented! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 6/18/2023 22:41:47'!
isSeparable
	^ true! !

!EllipticCurveIsogeny methodsFor: 'as yet unclassified' stamp: 'len 12/25/2016 08:42:27'!
isSurjective
	^ self isZero not! !

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

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:00:58'!
degree
	^ self multiplier squared! !

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 17:59:08'!
dual
	^ self! !

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:06:06'!
isInjective
	self multiplier = 0 ifTrue: [^ false].
	^ (self multiplier abs isPowerOf: self domain scalars characteristic) and: [self domain isSupersingular]! !

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:04:48'!
isSeparable
	self multiplier = 0 ifTrue: [^ self error: 'not an isogeny'].
	^ self scalingFactor isZero not! !

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:03:35'!
kernelPolynomial
"	self multiplier = 0 ifTrue: [^ ... zero polynomial]."
	^ (self domain divisionPolynomial: self multiplier abs) radical! !

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:00:41'!
multiplier
	^ self propertyAt: #multiplier! !

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:06:47'!
negated
	^ self class on: self domain multiplier: self multiplier negated! !

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:00:31'!
scalingFactor
	^ self domain scalars !! self multiplier! !

!EllipticCurveScalingEndomorphism methodsFor: 'as yet unclassified' stamp: 'len 6/25/2023 18:01:19'!
value: aPoint
	^ aPoint * self multiplier! !
