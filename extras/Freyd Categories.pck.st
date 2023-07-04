'From Cuis 6.0 [latest update: #5882] on 4 July 2023 at 5:14:14 pm'!
'Description '!
!provides: 'Freyd Categories' 1 1!
SystemOrganization addCategory: #'Freyd Categories'!


!classDefinition: #FreydCategory category: #'Freyd Categories'!
Category subclass: #FreydCategory
	instanceVariableNames: 'objects'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Freyd Categories'!
!classDefinition: 'FreydCategory class' category: #'Freyd Categories'!
FreydCategory class
	instanceVariableNames: ''!

!classDefinition: #FreydMorphism category: #'Freyd Categories'!
AbelianMorphism subclass: #FreydMorphism
	instanceVariableNames: 'datum'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Freyd Categories'!
!classDefinition: 'FreydMorphism class' category: #'Freyd Categories'!
FreydMorphism class
	instanceVariableNames: ''!


!FreydCategory commentStamp: '<historical>' prior: 0!
The Freyd category A(C) of an additive category C with weak kernels, that equips C with cokernels in a universal way. The objects of A(C) are the morphisms of C, and the morphisms are FreydMorphisms. See [Pos17b].!

!FreydMorphism commentStamp: '<historical>' prior: 0!
Morphisms of a Freyd category.!

!FreydCategory methodsFor: 'as yet unclassified' stamp: 'len 6/30/2023 15:08:34'!
= anObject
	^ self class = anObject class and: [objects = anObject objects]! !

!FreydCategory methodsFor: 'as yet unclassified' stamp: 'len 6/30/2023 15:08:41'!
hash
	^ objects hash! !

!FreydCategory methodsFor: 'as yet unclassified' stamp: 'len 6/30/2023 15:06:34'!
isAbelian
	^ true! !

!FreydCategory methodsFor: 'as yet unclassified' stamp: 'len 6/30/2023 15:08:17'!
objects
	^ objects! !

!FreydCategory methodsFor: 'as yet unclassified' stamp: 'len 6/30/2023 15:04:09'!
objects: anAdditiveCategory
	objects := anAdditiveCategory! !

!FreydCategory methodsFor: 'as yet unclassified' stamp: 'len 7/1/2023 01:05:49'!
printOn: aStream
	aStream nextPutAll: 'Freyd('; print: objects; nextPut: $)! !

!FreydCategory class methodsFor: 'as yet unclassified' stamp: 'len 6/30/2023 15:08:03'!
on: anAdditiveCategory
	^ self new objects: anAdditiveCategory! !

!FreydMorphism methodsFor: 'accessing' stamp: 'len 6/30/2023 15:11:09'!
category
	^ self domain category Freyd! !

!FreydMorphism methodsFor: 'accessing' stamp: 'len 6/29/2023 17:08:03'!
datum
	^ datum! !

!FreydMorphism methodsFor: 'accessing' stamp: 'len 6/29/2023 19:37:14'!
witness
	^ self propertyAt: #witness ifAbsentPut: [self codomain \ (datum · self domain)]! !

!FreydMorphism methodsFor: 'comparing' stamp: 'len 6/29/2023 17:07:56'!
= anObject
	anObject class = self class ifFalse: [^ false].
	(self domain = anObject domain and: [self codomain = anObject codomain]) ifFalse: [^ false].
	^ (self codomain lift: datum - anObject datum) notNil! !

!FreydMorphism methodsFor: 'comparing' stamp: 'len 6/29/2023 17:08:27'!
hash
	^ self domain hash + self codomain hash hashMultiply! !

!FreydMorphism methodsFor: 'operations' stamp: 'len 6/29/2023 19:19:26'!
· aMorphism
	self checkCompatibleComposition: aMorphism.
	^ self class from: aMorphism domain to: self codomain datum: datum · aMorphism datum! !

!FreydMorphism methodsFor: 'operations' stamp: 'len 6/29/2023 18:14:00'!
cokernel
	"Answer the cokernel projection of the receiver."
	^ self class from: self codomain to: datum ⊔ self codomain datum: self codomain domain id! !

!FreydMorphism methodsFor: 'operations' stamp: 'len 6/29/2023 18:16:01'!
colift: aMorphism
	self notYetImplemented! !

!FreydMorphism methodsFor: 'operations' stamp: 'len 6/29/2023 18:14:10'!
kernel
	"Answer the kernel embedding of the receiver."
	^ self notYetImplemented! !

!FreydMorphism methodsFor: 'operations' stamp: 'len 6/29/2023 18:15:59'!
lift: aMorphism
	self notYetImplemented! !

!FreydMorphism methodsFor: 'testing' stamp: 'len 6/30/2023 00:33:29'!
isEpimorphism
	^ datum isIdentity or: [super isEpimorphism]! !

!FreydMorphism methodsFor: 'private' stamp: 'len 6/29/2023 17:27:27'!
datum: aMorphism
	datum := aMorphism! !

!FreydMorphism class methodsFor: 'instance creation' stamp: 'len 6/29/2023 17:26:53'!
from: aDomain to: aCodomain datum: aMorphism
	self assert: (aMorphism domain = aDomain codomain and: [aMorphism codomain = aCodomain codomain]).
	^ self new domain: aDomain; codomain: aCodomain; datum: aMorphism! !

!AbelianMorphism methodsFor: '*freyd categories' stamp: 'len 6/30/2023 15:37:54'!
id
	"Answer the identity morphisms of the receiver in the Freyd category."
	^ FreydMorphism from: self to: self datum: self codomain id! !
