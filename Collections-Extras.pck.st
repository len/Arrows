'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 1:26:09 pm'!
'Description '!
!provides: 'Collections-Extras' 1 1!



!Collection class methodsFor: '*collections-extras'!
accumulate: iteratorBlock
	"Return a new instance of myself by evaluating the iteratorBlock.
	Examples:
		| x oc set |
		x := #( 1 2 3 4 1 ).
		oc := OrderedCollection accumulate: [ :incBlock | x do: incBlock].
		Transcript cr; show: oc printString.
		set := Set accumulate: [ :incBlock | x do: incBlock].
		Transcript cr; show: set printString"

	| collection | 
	collection := self new.
	iteratorBlock value: [ :element | collection add: element].
	^collection! !

!Set methodsFor: '*collections-extras' stamp: 'len 4/27/2016 02:28'!
add: newObject ifAbsent: aBlock
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: newObject.
	(array at: index) ifNil: [self atNewIndex: index put: newObject. aBlock value].
	^ newObject! !

!Set methodsFor: '*collections-extras' stamp: 'len 5/12/2016 23:24'!
add: newObject ifPresent: aBlock
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: newObject.
	(array at: index) isNil
		ifTrue: [self atNewIndex: index put: newObject]
		ifFalse: [aBlock value].
	^ newObject! !

!Set methodsFor: '*collections-extras' stamp: 'len 12/23/2021 13:58:57'!
at: anObject
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"
	| index |
	^(index _ self scanFor: anObject) = 0 ifFalse: [array at: index]! !

!Set methodsFor: '*collections-extras' stamp: 'len 12/23/2021 13:59:04'!
at: anObject ifAbsent: exceptionBlock
	| index |
	^ ((index _ self scanFor: anObject) = 0 ifFalse: [array at: index]) ifNil: exceptionBlock! !

!Set methodsFor: '*collections-extras' stamp: 'len 12/23/2021 13:59:10'!
at: anObject ifAbsentPut: aBlock
	"Answer an object in the receiver that is equal to anObject, or evaluate aBlock to get one and add it to the receiver,
	Relies heavily on hash properties, it is important that the following holds: aBlock value = anObject."
	| index |
	anObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: anObject.
	^ (array at: index) ifNil: [self atNewIndex: index put: aBlock value]! !
