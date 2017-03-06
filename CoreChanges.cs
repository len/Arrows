'From Cuis 5.0 of 7 November 2016 [latest update: #3059] on 6 March 2017 at 8:12:25 pm'!

!Fraction methodsFor: 'testing' stamp: 'len 2/14/2017 08:21:16'!
is: aSymbol
	^ aSymbol == #Fraction or: [super is: aSymbol]! !


!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/17/2016 23:02:17'!
CC
	^ $à! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/17/2016 23:02:47'!
Delta
	^ $Ü! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/17/2016 23:02:07'!
FF
	^ $â! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/17/2016 23:02:58'!
Gamma
	^ $Ö! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/11/2016 13:40:49'!
HH
	^ $ä! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/11/2016 13:40:56'!
NN
	^ $ã! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/17/2016 23:03:09'!
Omega
	^ $á! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/11/2016 13:41:07'!
PP
	^ $å! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/11/2016 13:42:03'!
QQ
	^ $ç! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/11/2016 13:42:09'!
RR
	^ $é! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/11/2016 13:42:16'!
ZZ
	^ $è! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:13'!
aleph
	^ $Å! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:17'!
cap
	^ $ï! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/24/2016 10:45:30'!
circ
	^ $î! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:21'!
cup
	^ $ñ! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:25'!
degree
	^ $∞! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:29'!
dot
	^ $∑! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:33'!
emptySet
	^ $ÿ! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:36'!
infinity
	^ $Ä! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:37:46'!
mu
	^ $µ! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:43'!
odot
	^ $ì! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:47'!
oplus
	^ $ê! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:40:51'!
otimes
	^ $í! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:38:45'!
pi
	^ $Ç! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:38:01'!
plusMinus
	^ $±! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:38:51'!
sqrt
	^ $ó! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/8/2016 20:38:56'!
times
	^ $◊! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 12/17/2016 23:02:30'!
zeta
	^ $Ñ! !


!Collection methodsFor: 'enumerating' stamp: 'len 12/8/2016 20:33:00'!
symmetricDifference: aSet 
	^ (self difference: aSet) ñ (aSet difference: self)! !

!Collection methodsFor: 'statistics' stamp: 'len 4/29/2016 21:58'!
argmax: aBlock
	"Answer the argument (element of the receiver) that maximizes aBlock."
	^ self detectMax: aBlock! !

!Collection methodsFor: 'statistics' stamp: 'len 6/7/2016 09:38'!
argmin: aBlock
	"Answer the argument (element of the receiver) that minimizes aBlock."
	^ self detectMin: aBlock! !


!SequenceableCollection methodsFor: 'copying' stamp: 'len 3/3/2016 23:07'!
copyWithoutIndex: index 
	| answer |
	answer _ self species new: self size - 1.
	answer replaceFrom: 1 to: index - 1 with: self startingAt: 1.
	answer replaceFrom: index to: answer size with: self startingAt: index + 1.
	^ answer! !


!FloatArray methodsFor: 'arithmetic' stamp: 'len 1/21/2017 13:07:40'!
normalized
	^ self copy normalize! !


!OrderedCollection methodsFor: 'converting' stamp: 'len 11/29/2016 08:54:14'!
asArray
	^ array copyFrom: firstIndex to: lastIndex! !


!SortedCollection methodsFor: 'converting' stamp: 'len 11/29/2016 08:56:57'!
asSortedArray
	^ self asArray! !


!Bag methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 11:26:24'!
withOccurrencesDo: aBlock
	contents associationsDo: [:each| aBlock value: each key value: each value]! !


!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'len 12/4/2016 20:59:47'!
cr
	self newLine! !


!StrikeFont methodsFor: 'character shapes' stamp: 'len 6/7/2016 06:20'!
takeAllGlyphFor: aCharacter from: sourceCharacter in: aFont
	self takeGlyphFor: aCharacter from: sourceCharacter in: aFont.
	self derivativeFonts do: [:each|
		(aFont derivativeFonts detect: [:one| one emphasis = each emphasis] ifNone: [])
			ifNotNil: [:otherFont| each takeGlyphFor: aCharacter from: sourceCharacter in: otherFont]].
! !


!Morph methodsFor: 'submorphs-accessing' stamp: 'len 12/5/2016 11:59:49'!
findA: aClass
	^ self findDeepSubmorphThat: [:each| each isKindOf: aClass] ifAbsent: [self error: 'not found']! !


!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'len 2/18/2016 06:42'!
fillEllipse: aRectangle color: aColor
	| displayRectangle |
	displayRectangle _ (currentTransformation displayBoundsOfTransformOf: aRectangle) truncated.
	self setPaintColor: aColor.
	port fillOval: displayRectangle! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'len 2/18/2016 06:43'!
fillEllipse: aRectangle color: aColor borderWidth: borderWidth borderColor: borderColor
	self fillEllipse: (aRectangle insetBy: borderWidth) color: aColor.
	(borderWidth > 0 and: [borderColor isTransparent not])
		ifTrue: [self frameEllipse: aRectangle borderWidth: borderWidth color: borderColor]! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'len 2/18/2016 06:43'!
frameEllipse: aRectangle borderWidth: borderWidth color: aColor
	| displayRectangle |
	displayRectangle _ (currentTransformation displayBoundsOfTransformOf: aRectangle) truncated.
	self setPaintColor: aColor.
	port frameOval: displayRectangle borderWidth: borderWidth! !


!Theme methodsFor: 'other options' stamp: 'len 6/9/2016 20:57'!
buttonHeight
	"Answer the user's preferred default height for buttons."

	^Preferences standardButtonFont height * 14 // 8! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 3/5/2016 22:40'!
blockLevelOne

	^ #(
		leftParenthesis1
		rightParenthesis1
		blockStart1
		blockEnd1
	)
! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 3/5/2016 22:40'!
blockLevelThree

	^ #(
		leftParenthesis3
		rightParenthesis3
		blockStart3
		blockEnd3
	)
! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 3/5/2016 22:40'!
blockLevelTwo

	^ #(
		leftParenthesis2
		rightParenthesis2
		blockStart2
		blockEnd2
	)
! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 6/9/2016 04:47'!
errors
	^ #(
		invalid
		excessCode
	)! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'len 2/17/2017 20:15:05'!
specialOperators
	"Answer the array of special binary operators that are not typeable by keyboard."
	^ #(  ê í î ï ñ ù û ∑ ◊ ˜)! !


!Workspace methodsFor: 'binding' stamp: 'len 11/23/2015 03:28'!
bindingOf: aString
	mustDeclareVariables ifTrue: [^ nil].
	(bindings includesKey: aString) ifFalse: [
		"aString first isUppercase
			ifTrue: [^nil]
			ifFalse: ["bindings at: aString put: nil]"]".
	^bindings associationAt: aString! !


!Fraction methodsFor: 'comparing' stamp: 'len 10/29/2016 16:03'!
= aNumber
	self == aNumber ifTrue: [ ^ true ].
	aNumber isFraction
		ifTrue: [numerator = 0 ifTrue: [^ aNumber numerator = 0].
				^ (numerator * aNumber denominator) =
					(aNumber numerator * denominator)
				"Note: used to just compare num and denom,
					but this fails for improper fractions"].
	^ aNumber adaptToFraction: self andSend: #=! !


!Duration methodsFor: 'ansi protocol' stamp: 'len 6/28/2016 09:12'!
hash
	^seconds bitXor: nanos! !


!Complex methodsFor: 'comparing' stamp: 'len 11/26/2016 08:34:46'!
= anObject
	self == anObject ifTrue: [ ^ true].
	^anObject isComplex
		ifTrue: [(real = anObject real) & (imaginary = anObject imaginary)]
		ifFalse: [anObject adaptToComplex: self andSend: #=]! !

!Complex methodsFor: 'printing' stamp: 'len 11/29/2016 11:42:24'!
printOn: aStream
	(real ~= 0 or: [imaginary = 0])
		ifTrue:
			[aStream print: real.
			imaginary = 0
				ifFalse:
					[aStream space.
					imaginary positive
						ifTrue: [aStream nextPut: $+]
						ifFalse: [aStream nextPut: $-].
					aStream space.
					imaginary abs = 1 ifFalse: [aStream print: imaginary abs].
					aStream nextPutAll: 'i' italic]]
		ifFalse:
			[imaginary negative
				ifTrue: [aStream nextPut: $-].
			imaginary abs = 1 ifFalse: [aStream print: imaginary abs].
			aStream nextPutAll: 'i' italic]! !


!Character methodsFor: 'testing' stamp: 'len 2/17/2017 20:13:18'!
isSpecial
	"Answer whether the receiver is one of the special characters that can be used as binary operator."

	^'+-/\*~<>=@,%|&?!!êëíìîïñùû∑◊˜' includes: self! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'len 11/22/2016 07:07:33'!
atAllPut: anObject 
	"Put anObject at every one of the receiver's indices."

	| size |
	(size _ self size) > 50 "first method faster from 50 accesses and on"
		ifTrue: [self from: 1 to: size put: anObject]
		ifFalse: [1 to: size do: [:index | self at: index put: anObject]]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'len 2/19/2017 18:58:55'!
combinations: k atATimeDo: aBlock
	"Take the items in the receiver, k at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size choose: k) combinations.

	 'abcde' combinations: 3 atATimeDo: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	k = 0 ifTrue: [aBlock value: #(). ^ self].
	aCollection _ Array new: k.
	self combinationsAt: 1 in: aCollection after: 0 do: aBlock! !


!String class methodsFor: 'initialization' stamp: 'len 3/6/2017 20:07:43'!
initialize
	"
	String initialize
	"

	| order newOrder lowercase |

	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126 183 215 247) do: [ :c | "\^|~∑◊˜"
		newOrder at: c + 1 put: (order _ order+1)].
	16r90 to: 16r9F do: [ :i |
		newOrder at: i + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order _ order+1.
		newOrder at: upperAndLowercase first numericValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second numericValue + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	CaseInsensitiveOrder _ newOrder asByteArray.
	
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	28 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126 183 215 247) do: [ :c | "\^|~∑◊˜"
		newOrder at: c + 1 put: (order _ order+1)].
	16r90 to: 16r9F do: [ :i |
		newOrder at: i + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first numericValue + 1 put: (order _ order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase _ upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase numericValue + 1 put: (order _ order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	CaseSensitiveOrder _ newOrder asByteArray.

	"a table for translating to lower case"
	LowercasingTable _ String withAll: (Character characterTable collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable _ String withAll: (Character characterTable collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish _ String withAll: (Character characterTable collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders _ CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators _ CharacterSet separators.
	CSNonSeparators _ CSSeparators complement! !


!Dictionary methodsFor: 'printing' stamp: 'len 3/6/2017 19:54:46'!
printElementsOn: aStream
	aStream nextPut: ${.
	self keysSortedSafely do:
		[:key | aStream print: key; nextPutAll: '->'; print: (self at: key)] separatedBy: [aStream space].
	aStream nextPut: $}! !


!TextAnchor methodsFor: 'copying' stamp: 'len 6/24/2016 22:08'!
postCopy

"	anchoredFormOrMorph _ anchoredFormOrMorph copy"! !


!StrikeFont methodsFor: 'character shapes' stamp: 'len 6/7/2016 06:02'!
takeGlyphFor: aCharacter from: sourceCharacter in: aFont
	"Copy characterForm over the glyph for the argument, character."
	| characterForm r newForm |
	characterForm _ aFont glyphAt: sourceCharacter.
	r _ 0@(0 max: aFont ascent - self ascent) extent: characterForm width @ glyphs height.
	(newForm _ Form extent: characterForm width @ glyphs height depth: glyphs depth)
		fillWhite;
		copyBits: r
		from: characterForm
		at: 0@0
		clippingBox: newForm boundingBox
		rule: Form over.
	self glyphAt: aCharacter put: newForm! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'len 6/15/2016 19:48'!
install: aString
"
StrikeFont install: 'DejaVu'.
StrikeFont buildLargerPunctuation: 'DejaVu'.
Character initialize

StrikeFont install: 'DejaVu Sans Mono'.
StrikeFont buildLargerPunctuation: 'DejaVu Sans Mono'.
Character initialize
"
"
StrikeFont install: 'Inconsolata'
StrikeFont install: '#PilGi'
StrikeFont install: 'Optima'
StrikeFont install: 'Herculanum'
StrikeFont install: 'Papyrus'
StrikeFont install: 'Handwriting - Dakota'
StrikeFont install: 'Times New Roman'
StrikeFont install: 'Apple Chancery'
StrikeFont install: 'Cochin'
StrikeFont install: 'Cracked'
StrikeFont install: 'Zapfino'
StrikeFont install: 'Brush Script MT'
StrikeFont install: 'Chalkboard'
"
	| fontDict |
	fontDict _ Dictionary new.
	"Just try a lot of sizes. Will ignore missing files."
	1 to: 200 do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			fontDict
				at: s
				put: font ]].
	fontDict notEmpty ifTrue: [
		AvailableFonts at: aString put: fontDict ].
"	Preferences restoreDefaultFonts"! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'len 6/30/2016 07:20'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	self layoutMorph
		addMorph: self buildMorphicMessageList proportionalHeight: 0.4;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
	model changed: #editSelection! !


!Theme methodsFor: 'other options' stamp: 'len 6/9/2016 20:57'!
buttonPaneHeight
	"Answer the user's preferred default height for button panes."

	^ self buttonHeight! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 6/8/2016 01:08'!
firstBlockLevel

	^ #(
		leftParenthesis
		rightParenthesis
"		leftBrace
		rightBrace
"		blockStart
		blockEnd
	)! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 6/10/2016 05:33'!
generateShoutConfig

	| styles colors |
	
	styles := OrderedCollection new.
	colors := self shout as: Dictionary.

	{
		{self undefined. colors at: #defaults . #normal}.
		{self errors . colors at: #defaults}.
		{self defaults . colors at: #defaults}.
		{self pseudoVariables . colors at: #pseudoVariables . #bold}.
		{self argumentTypes . colors at: #arguments . self italic}.
		{self instVar . colors at: #instVar}.
		{self tempBar . colors at: #tempBar}.
		{self tempVars . colors at: #tempVars . self italic}.
		{self blockTemps . colors at: #blockTemps ifAbsent: [colors at: #tempVars] . self italic}.
		{self messages . colors at: #messages}.
		{self incompleteMessages . colors at: #incompleteMessages . #normal "#underlined"}.
		{self literals . colors at: #literals ifAbsent: [colors at: #pseudoVariables]}.
		{self symbols . colors at: #messages . #bold}.
		{self globals . colors at: #defaults . #bold}.
		{self blockLevelOne . colors at: #blockLevelOne}.
		{self blockLevelTwo . colors at: #blockLevelTwo}.
		{self blockLevelThree . colors at: #blockLevelThree}.
		{self blockLevelFour . colors at: #blockLevelFour}.
		{self blockLevelFive . colors at: #blockLevelFive}.
		{self blockLevelSix . colors at: #blockLevelSix}.
		{self blockLevelSeven . colors at: #blockLevelSeven}.
		{self firstBlockLevel . colors at: #block}.
		{self methodTags . colors at: #methodTags . #bold}.
		{self pattern . nil . #bold}.
		{self ansiAssignment . nil . #bold}.
		{self assignment . nil . #(#bold #withST80Glyphs)}.
		{self return . nil . #(#bold #withST80Glyphs)}.
	} do: [ :style |
		styles addAll:
			(style first
				collect: [ :category | | elements |
					elements _ style asOrderedCollection.
					elements at: 1 put: category.
					Array withAll: elements ])].

	"Miscellaneous remainder after factoring out commonality:"
	styles addAll: {
		{#unfinishedString . colors at: #literals . #normal}.
		{#undefinedIdentifier . colors at: #undefined . #underlined}.
		{#unfinishedComment . colors at: #comment ifAbsent: [colors at: #pseudoVariables] . #normal }.
		{#comment . colors at: #comment ifAbsent: [colors at: #methodTags] . self italic}.
		{#string . colors at: #literals . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #defaults . #(italic underlined)}.
		{#classVar . colors at: #classVar ifAbsent: [colors at: #tempVars] . #bold}.
	}.

	^ styles! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 6/8/2016 23:48'!
literals

	^ #(
			character
			integer
			number
			-
"		#'$'"
			blockStart3
			blockEnd3
			leftParenthesis3
			rightParenthesis3
	)! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 6/10/2016 05:33'!
undefined

	^ #(
		undefinedKeyword
		undefinedBinary
		undefinedUnary
	)! !

!Theme methodsFor: 'shout' stamp: 'len 6/8/2016 00:13'!
shout
	"Color symbols as an association list."
	
	^ {
		#defaults 				-> #black.
		#undefined 				-> #red.
		#comment 				-> #(green muchDarker).
		#methodTags 			-> #(green muchDarker).
		#pseudoVariables 		-> #(red muchDarker).
		#messages 				-> #(blue darker).
		#arguments 				-> #(cyan muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#incompleteMessages -> #(gray veryMuchDarker).
		#blockLevelFour 		-> #(green darker).
		#blockLevelFive 		-> #(orange darker).
		#blockLevelSix 			-> #(magenta darker).
		#blockLevelSeven 		-> #blue.
		#tempBar 				-> #gray.
		#tempVars 				-> #(gray muchDarker).
	}! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'len 2/17/2017 20:16:57'!
handleKeystrokeBefore: kbEvent
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."
	| currentPos currentCharIsAlphaNumeric keyValue ctrl cmd tab colon alphanum backspace esc space return keyChar  |
	currentPos _ textMorph editor startIndex-1.
	currentCharIsAlphaNumeric _ currentPos > 0 and: [ model textSize >= currentPos and: [
			(model actualContents at: currentPos) isAlphaNumeric ]].
	keyValue _ kbEvent keyValue.
	keyChar _ kbEvent keyCharacter.
	ctrl _ kbEvent controlKeyPressed.
	cmd _ kbEvent commandAltKeyPressed.
	tab _ keyChar = Character tab.
	colon _ keyChar = $:.
	alphanum _ kbEvent keyCharacter isAlphaNumeric.
	backspace _ keyValue = 8.
	esc _ keyValue = 27.
	space _ #(0 32 160) includes: keyValue.
	return _ kbEvent isReturnKey.

	"Stuff to do if the menu is not open"
	menuMorph ifNil: [
		"Ctrl-Space or Tab for open"
		"Mac specific note: Using option-space (actually option+160) effectively disables the non-breaking space character 160"
		(space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [
			(self opensWithTab and: [tab]) and: [currentCharIsAlphaNumeric or: [currentPos > 0 and: [(model actualContents at: currentPos) == $\]]]])
				ifTrue: [self openCompletionMenu. ^ true].
		"Auto-open - currently deactivated"
"		(ctrl not & cmd not & alphanum) 
			ifTrue: [ self openCompletionMenu ]."
		^ false].

	"Starting here, stuff to do if the menu is open"
	menuMorph stillActive.
	"Escape"
	esc ifTrue: [ self closeMenu. ^ true].
	"Backspace"
	backspace ifTrue: [
		currentCharIsAlphaNumeric ifFalse: [ self closeMenu ].
		^ false].
	"Home"
	keyValue = 1 ifTrue: [ menuMorph home. ^ true ].
	"End"
	keyValue = 4 ifTrue: [ menuMorph end. ^ true].
	"?"
	keyChar = $? ifTrue: [ menuMorph help. ^true].
	"Arrow up"
	keyValue = 30 ifTrue: [ menuMorph moveUp. ^ true].
	"Arrow down"
	keyValue = 31 ifTrue: [ menuMorph moveDown. ^ true].
	"Page up"
	keyValue = 11 ifTrue: [ menuMorph pageUp. ^ true].
	"Page down"
	keyValue = 12 ifTrue: [ menuMorph pageDown. ^ true].
	"Return, Tab or Ctrl-Space"
	(return or: [ space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [ tab]]) ifTrue: [
		self insertSelected
			ifTrue: [^ true]].
	"All keys but the alphanumeric chars (without command and control ) 
	and the backspace key do close the menu"
	(ctrl not & cmd not and: [ alphanum | colon])
		ifFalse: [ self closeMenu ].
	^false! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'len 12/18/2016 12:38:16'!
computeEntries

	| allSource contextClass id p last3Ranges range prevRange receiverClass prevPrevRange |
	allSource _ model actualContents string.
	p _ (model is: #hasTextProvider)
		ifTrue: [ model textProvider ]
		ifFalse: [ model ].
	contextClass _ (p respondsTo: #selectedClassOrMetaClass) ifTrue: [
		p selectedClassOrMetaClass ].

	"Instead of creating a new string, maybe we could pass the last position to parse to Shout..."
	parser _ SHParserST80 new.
	parser
		workspace: ((model isMemberOf: Workspace) ifTrue: [ model ]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position).
	parser parse.
	last3Ranges _ parser last3Ranges.
	range _ last3Ranges third.
	range ifNil: [ ^entries _ #() ].

	"If parsing breaks before position, then we don't know what to suggest, therefore don't open Completion"
	range end = position ifFalse: [ ^entries _ #() ].

	prefix _ allSource copyFrom: range start to: range end.

	"The escape character $\ makes the completer show special binary operators:"
	prefix = '\' ifTrue: [ ^entries _ self specialOperators ].

	(parser isMessage: range rangeType) ifTrue: [
		"If previous range is a constant or a well known identifier, we might filter messages"
		prevRange _ last3Ranges second.
		prevPrevRange _ last3Ranges first.
		receiverClass _ nil.
		"3 if -> ifNil: but not ifTrue:
		3=4 -> ifNil: or ifTrue:"
		(prevRange notNil and: [ prevPrevRange isNil or: [ (#(binary keyword) includes: prevPrevRange rangeType) not]]) ifTrue: [
			id _ (allSource copyFrom: prevRange start to: prevRange end).
			receiverClass _ prevRange rangeType caseOf: {
				[ #globalVar ] -> [ (Smalltalk at: id asSymbol) class ].
				[ #self ] -> [ contextClass ].
				[ #super ] -> [ contextClass superclass ].
				[ #true ] -> [ True ].
				[ #false ] -> [ False ].
				[ #nil ] -> [ UndefinedObject ].
				[ #character ] -> [ id first class ].
				[ #number ] -> [ (Compiler evaluate: id) class ].
				[ #string ] -> [ (Compiler evaluate: id) class ].
				[ #symbol ] -> [ (Compiler evaluate: id) class ].
				[ #stringSymbol ] -> [ (Compiler evaluate: id) class ].
				"thisContext could mean ContextPart or BlockClosure..."
				"[ #thisContext ] -> [ ContextPart ]"
			} otherwise: [ nil ]
		].
		^self computeMessageEntries: receiverClass ].

	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [
		^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #()! !

!methodRemoval: Character class #bullet!
Character class removeSelector: #bullet!
!methodRemoval: Character class #circle!
Character class removeSelector: #circle!
!methodRemoval: Character class #contourIntegral!
Character class removeSelector: #contourIntegral!
!methodRemoval: Character class #doesNotExist!
Character class removeSelector: #doesNotExist!
!methodRemoval: Character class #exists!
Character class removeSelector: #exists!
!methodRemoval: Character class #forAll!
Character class removeSelector: #forAll!
!methodRemoval: Character class #greaterNotEqual!
Character class removeSelector: #greaterNotEqual!
!methodRemoval: Character class #greaterOrEqual!
Character class removeSelector: #greaterOrEqual!
!methodRemoval: Character class #greaterOverEqual!
Character class removeSelector: #greaterOverEqual!
!methodRemoval: Character class #identical!
Character class removeSelector: #identical!
!methodRemoval: Character class #integral!
Character class removeSelector: #integral!
!methodRemoval: Character class #lessNotEqual!
Character class removeSelector: #lessNotEqual!
!methodRemoval: Character class #lessOrEqual!
Character class removeSelector: #lessOrEqual!
!methodRemoval: Character class #lessOverEqual!
Character class removeSelector: #lessOverEqual!
!methodRemoval: Character class #notEqual!
Character class removeSelector: #notEqual!
!methodRemoval: Character class #notIdentical!
Character class removeSelector: #notIdentical!
!methodRemoval: Character class #partial!
Character class removeSelector: #partial!
!methodRemoval: Character class #strictlyEquivalent!
Character class removeSelector: #strictlyEquivalent!
!methodRemoval: Character class #summation!
Character class removeSelector: #summation!

!Character class reorganize!
('class initialization' accentedLetters initClassCachedState initialize initializeClassificationTable initializeUnicodeCodePoints releaseClassCachedState uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' asciiValue: codePoint: digitValue: immediateNumericValue: new nonImmediateNumericValue: numericValue: safeCodePoint: separators)
('accessing untypeable characters' arrowDown arrowLeft arrowRight arrowUp backspace cr crCharacter delete end enter escape euro home insert lf lfCharacter nbsp newLineCharacter newPage null pageDown pageUp space tab)
('constants' characterTable)
('converting' evaluate:withUtf8BytesOfUnicodeCodePoint: iso8859s15CodeForUnicodeCodePoint: latin1ToMacRoman: macRomanToLatin1: nextUnicodeCodePointFromUtf8: unicodeCodePointOfUtf8Bytes: utf8BytesOfUnicodeCodePoint:)
('fileIn/Out' definition)
('accessing mathematical symbols' CC Delta FF Gamma HH NN Omega PP QQ RR ZZ aleph cap circ cup degree dot emptySet infinity mu odot oplus otimes pi plusMinus sqrt times zeta)
!

String initialize!
