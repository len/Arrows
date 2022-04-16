'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 1:26:13 pm'!
'Description '!
!provides: 'Collections-Iterator' 1 1!
SystemOrganization addCategory: #'Collections-Iterator'!


!classDefinition: #Iterator category: #'Collections-Iterator'!
Collection subclass: #Iterator
	instanceVariableNames: 'block'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Iterator'!
!classDefinition: 'Iterator class' category: #'Collections-Iterator'!
Iterator class
	instanceVariableNames: ''!


!Iterator commentStamp: '<historical>' prior: 0!
An Iterator is a read-only collection that evaluates a block to yield the elements of the collection.!

!Iterator methodsFor: 'printing' stamp: 'len 5/10/2020 10:16:19'!
printOn: aStream
	| count |
	aStream nextPut: ${.
	count _ 0.
	self do: [:element| aStream print: element] separatedBy: [(count _ count + 1) > 10 ifTrue: [aStream nextPutAll: '...)'. ^ self] ifFalse: [aStream nextPutAll: ', ']].
	aStream nextPut: $}! !

!Iterator methodsFor: 'accessing' stamp: 'len 4/12/2021 12:31:20'!
first
	self do: [:each| ^ each].
	self errorEmptyCollection! !

!Iterator methodsFor: 'accessing'!
identityIndexOf: anElement 
	"Answer the identity index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self identityIndexOf: anElement ifAbsent: [0]! !

!Iterator methodsFor: 'accessing'!
identityIndexOf: anElement ifAbsent: exceptionBlock 
	"Answer the identity index of anElement within the receiver.  If the receiver does
	not contain anElement, answer the result of evaluating the exceptionBlock."

	| index |
	index := 1.
	self do: [ :el | el == anElement ifTrue: [^index].  index := index + 1].
	^exceptionBlock value! !

!Iterator methodsFor: 'accessing'!
indexOf: anElement 
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self indexOf: anElement ifAbsent: [0]! !

!Iterator methodsFor: 'accessing'!
indexOf: anElement ifAbsent: exceptionBlock 
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer the result of evaluating the exceptionBlock."

	| index |
	index := 1.
	self do: [ :el | el = anElement ifTrue: [^index].  index := index + 1].
	^exceptionBlock value! !

!Iterator methodsFor: 'adding'!
add: anObject
	"Iterators are read-only"
	self shouldNotImplement! !

!Iterator methodsFor: 'copying' stamp: 'len 10/27/2016 15:39'!
first: n
	"Answer the first n elements of the receiver.
	Raise an error if there are not enough elements."
	| answer |
	answer _ self species new: n.
	self do: [:each|
		answer size = n ifTrue: [^ answer].
		answer add: each].
	^ self error: 'not enough elements'! !

!Iterator methodsFor: 'enumerating'!
do: aBlock
	block value: aBlock! !

!Iterator methodsFor: 'enumerating'!
findFirst: aBlock
	"Answer the index of the first element of the receiver
	for which aBlock evaluates as true."

	| index |
	index := 1.
	self do: [ :el | (aBlock value: el) ifTrue: [^index].  index := index + 1].
	^0! !

!Iterator methodsFor: 'enumerating'!
findLast: aBlock
	"Answer the index of the last element of the receiver
	for which aBlock evaluates as true."

	| index last |
	index := 1.
	last := 0.
	self do: [ :el | (aBlock value: el) ifTrue: [last := index].  index := index + 1].
	^last! !

!Iterator methodsFor: 'enumerating'!
keysAndValuesDo: aBlock  
	"Evaluate aBlock with each of the receiver's key/value pairs
	(e.g. indexes and elements) as the arguments."

	| index |
	index := 1.
	self do: [:el | aBlock value: index value: el.  index := index + 1]! !

!Iterator methodsFor: 'removing'!
remove: oldObject ifAbsent: anExceptionBlock 
	"Iterators are read-only."
	self shouldNotImplement! !

!Iterator methodsFor: 'private'!
block: aBlock
	block := aBlock! !

!Iterator methodsFor: 'private'!
species
	^OrderedCollection! !

!Iterator class methodsFor: 'instance creation' stamp: 'len 2/28/2016 20:32'!
on: aBlock
	^ self new block: aBlock! !

!Iterator class methodsFor: 'instance creation' stamp: 'len 2/14/2016 21:45'!
on: anObject performing: aSymbol
	^ self new block: [:aBlock| anObject perform: aSymbol with: aBlock]! !
