'From Cuis 5.0 [latest update: #4542] on 24 March 2021 at 2:09:15 pm'!
'Description '!
!provides: 'Morphic-Graphs' 1 1!
!requires: 'Domains' 1 614 nil!
SystemOrganization addCategory: #'Morphic-Graphs'!


!classDefinition: #GraphMorph category: #'Morphic-Graphs'!
KernelMorph subclass: #GraphMorph
	instanceVariableNames: 'graph nodeToMorph edgeToMorph focusNode dimension lastMousePosition components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Graphs'!
!classDefinition: 'GraphMorph class' category: #'Morphic-Graphs'!
GraphMorph class
	instanceVariableNames: ''!

!classDefinition: #SpringGraphMorph category: #'Morphic-Graphs'!
GraphMorph subclass: #SpringGraphMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Graphs'!
!classDefinition: 'SpringGraphMorph class' category: #'Morphic-Graphs'!
SpringGraphMorph class
	instanceVariableNames: ''!

!classDefinition: #GraphNodeMorph category: #'Morphic-Graphs'!
KernelMorph subclass: #GraphNodeMorph
	instanceVariableNames: 'node x dx shape label labelMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Graphs'!
!classDefinition: 'GraphNodeMorph class' category: #'Morphic-Graphs'!
GraphNodeMorph class
	instanceVariableNames: ''!

!classDefinition: #SpringNodeMorph category: #'Morphic-Graphs'!
GraphNodeMorph subclass: #SpringNodeMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Graphs'!
!classDefinition: 'SpringNodeMorph class' category: #'Morphic-Graphs'!
SpringNodeMorph class
	instanceVariableNames: ''!

!classDefinition: #CompositeGraphMorph category: #'Morphic-Graphs'!
LayoutMorph subclass: #CompositeGraphMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Graphs'!
!classDefinition: 'CompositeGraphMorph class' category: #'Morphic-Graphs'!
CompositeGraphMorph class
	instanceVariableNames: ''!

!classDefinition: #GraphEdgeMorph category: #'Morphic-Graphs'!
Morph subclass: #GraphEdgeMorph
	instanceVariableNames: 'color source target width drawArrowHead label labelMorph multiplicity'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Graphs'!
!classDefinition: 'GraphEdgeMorph class' category: #'Morphic-Graphs'!
GraphEdgeMorph class
	instanceVariableNames: ''!

!classDefinition: #SpringEdgeMorph category: #'Morphic-Graphs'!
GraphEdgeMorph subclass: #SpringEdgeMorph
	instanceVariableNames: 'desiredDistance'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Graphs'!
!classDefinition: 'SpringEdgeMorph class' category: #'Morphic-Graphs'!
SpringEdgeMorph class
	instanceVariableNames: ''!


!GraphMorph commentStamp: '<historical>' prior: 0!
A GraphMorph is composed of nodes and edges (GraphNodeMorph and GraphEdgeMorph or subclasses). Subclasses of GraphMorph implement different layouts.

Examples:
	| S a b |
	S _ SymmetricGroup on: (1 to: 4).
	a _ S !! #(2 1 4 3).
	b _ S !! #(2 3 1 4).
	(S alternatingSubgroup cayleyGraphMorph: {a.b})

	| G |
	G _ Graph exampleImplicitGraph.
	SpringGraphMorph new graph: G

	| G |
	G _ RootedDigraph exampleSmallLabelledTree.
	SpringGraphMorph new graph: G
!

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:45'!
dimension
	^ dimension! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:45'!
dimension: anInteger
	dimension _ anInteger.
	self nodesDo: [:each| each dimension: anInteger].
	self startStepping! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/31/2016 05:09'!
edgeAt: anEdge
	^ self edgeAt: anEdge ifAbsent: [self error: 'edge not found']! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/31/2016 05:09'!
edgeAt: anEdge ifAbsent: exceptionBlock
	| edge |
	edge _ anEdge key -> anEdge value.
	graph isDirected ifFalse: [edge _ edge as: MutualAssociation].
	^ edgeToMorph at: edge ifAbsent: exceptionBlock! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 5/4/2016 06:50'!
edges
	^ Iterator on: self performing: #edgesDo:! !

!GraphMorph methodsFor: 'accessing' stamp: 'Samir Saidani 12/16/2003 02:02'!
focusNode
	^focusNode! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 23:00'!
focusOn: aNode
	focusNode _ graph nodeAt: aNode.
	nodeToMorph at: focusNode ifPresent: [:aMorph| aMorph freeze].
	self startStepping! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 2/14/2016 22:45'!
graph
	^ graph! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/14/2016 05:42'!
graph: aGraph
	self clearAll.
	graph _ aGraph.
	graph nodesDo: [:each| self addNode: each].
	graph edgesAndLabelsDo: [:each :label| self addEdge: each label: label].
	graph
		when: #addNode: send: #addNode: to: self;
		when: #addEdge: send: #addEdge: to: self;
		when: #removeNode: send: #removeNode: to: self;
		when: #removeEdge: send: #removeEdge: to: self.
	(graph isKindOf: RootedDigraph)
		ifTrue: [self focusOn: graph rootNode]
		ifFalse: [self focusOn: graph anyOne]! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:45'!
hideLabels
	self nodesDo: [:each| each hideLabel].
	self edgesDo: [:each| each hideLabel]! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/31/2016 05:07'!
nodeAt: anObject
	^ nodeToMorph at: anObject! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/31/2016 05:06'!
nodeAt: anObject ifAbsent: exceptionBlock
	^ nodeToMorph at: anObject ifAbsent: exceptionBlock! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 2/17/2016 23:11'!
origin
	^ (nodeToMorph at: (focusNode ifNil: [^ self morphExtent // 2])) morphBounds center! !

!GraphMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:45'!
perturbate: n
	self nodesDo: [:each| each perturbate: n].
	self startStepping! !

!GraphMorph methodsFor: 'enumerating' stamp: 'len 2/29/2016 21:44'!
edgesAndLabelsDo: aBlock
	graph edgesAndLabelsDo: [:each :label| aBlock value: (self edgeAt: each) value: label]! !

!GraphMorph methodsFor: 'enumerating' stamp: 'len 2/15/2003 19:02'!
edgesDo: aBlock
	edgeToMorph do: aBlock! !

!GraphMorph methodsFor: 'enumerating' stamp: 'len 2/15/2003 18:58'!
nodesDo: aBlock
	nodeToMorph do: aBlock! !

!GraphMorph methodsFor: 'event handling' stamp: 'len 2/19/2016 02:05'!
acceptDroppingMorph: aMorph event: evt
	(aMorph isKindOf: self nodeClass)
		ifTrue: [self addMorph: aMorph.
				"self focusOn: aMorph node".
				^ self startStepping].
	^ super acceptDroppingMorph: aMorph event: evt! !

!GraphMorph methodsFor: 'event handling' stamp: 'len 7/9/2016 22:52'!
allowSubmorphExtraction
	" allow extraction so submorphs can be grabbed and dragged to a new position.
	  usefull to rearrange the vertices manually"
	^ true! !

!GraphMorph methodsFor: 'event handling' stamp: 'len 7/9/2016 22:51'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true! !

!GraphMorph methodsFor: 'event handling' stamp: 'len 7/9/2016 22:51'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	lastMousePosition _ localEventPosition! !

!GraphMorph methodsFor: 'event handling' stamp: 'len 7/9/2016 22:51'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	lastMousePosition isNil
		ifFalse:
			[aMouseMoveEvent shiftPressed
				ifTrue:
					[| bounds d |
					bounds _ self morphLocalBounds.
					d _ (localEventPosition dist: lastMousePosition) / (bounds width min: bounds height).
					localEventPosition y >= lastMousePosition y ifTrue: [d _ d negated].
					self scaleEdgesBy: 2.0 ^ d]
				ifFalse: [self rotateNodesBy: lastMousePosition - localEventPosition]].
	lastMousePosition _ localEventPosition! !

!GraphMorph methodsFor: 'event handling' stamp: 'len 6/2/2020 17:21:26'!
rotateNodesBy: aPoint
	| xSpeed ySpeed |
	self dimension < 3 ifTrue: [^ self].
	xSpeed _ Float pi / self morphExtent x.
	ySpeed _ Float pi / self morphExtent y.
"	self dimension = 2 ifTrue: [self nodesDo: [:each| |p pp| p _ each x. pp _ p x @ p y rotatedBy: aPoint y * ySpeed negated + (aPoint x * xSpeed). each x: (pp x, pp y)]. ^ self]."
	self nodesDo: [:each| | p xz yz |
		p _ each x.
		xz _ p x @ p z.
		xz _ xz rotatedBy: aPoint x * xSpeed.
		yz _ p y @ xz y.
		yz _ yz rotatedBy: aPoint y * ySpeed.
		p _ (xz x, yz x, yz y).
		each x: p.
		each dx: p * 0. "optimization, dont rotate the velocities"].
	self startStepping! !

!GraphMorph methodsFor: 'geometry' stamp: 'len 7/9/2016 22:53'!
privateExtent: aPoint
	^ (super privateExtent: aPoint) ifTrue: [self startStepping]; yourself! !

!GraphMorph methodsFor: 'initialization' stamp: 'len 7/14/2016 02:56'!
initialize
	super initialize.
	self morphExtent: 300 @ 300.
	self color: Color transparent.
	self initializeNodeToMorph.
	self initializeEdgeToMorph.
	dimension _ 3! !

!GraphMorph methodsFor: 'initialization' stamp: 'Samir Saidani 12/7/2003 19:25'!
initializeEdgeToMorph
	edgeToMorph _ Dictionary new! !

!GraphMorph methodsFor: 'initialization' stamp: 'Samir Saidani 12/7/2003 19:25'!
initializeNodeToMorph
	nodeToMorph _ Dictionary new! !

!GraphMorph methodsFor: 'layout' stamp: 'sam 6/1/2004 14:37'!
clearAll
	self clearEdges.
	self clearNodes.
	! !

!GraphMorph methodsFor: 'layout' stamp: 'len 2/14/2016 22:48'!
clearEdges
	self edgesDo: [:edgeMorph| edgeMorph delete].
	self initializeEdgeToMorph! !

!GraphMorph methodsFor: 'layout' stamp: 'len 2/14/2016 22:48'!
clearNodes
	self nodesDo: [:nodeMorph | nodeMorph delete].
	self initializeNodeToMorph! !

!GraphMorph methodsFor: 'stepping' stamp: 'len 6/2/2020 17:08:02'!
alignToCenter
	| centerOfMass delta |
	centerOfMass _ nodeToMorph anyOne x zero.
	self nodesDo: [:each| centerOfMass _ centerOfMass + each x].
	centerOfMass _ centerOfMass / graph size.
	centerOfMass norm2 < 1.0 ifTrue: [^ self].
	delta _ centerOfMass negated.
	self nodesDo: [:each| each translateBy: delta]! !

!GraphMorph methodsFor: 'stepping' stamp: 'len 7/9/2016 22:48'!
step
	| maxDelta |
	self nodesDo: [:each| each dampBy: 1.1]. "damping"
	self alignToCenter.
	maxDelta _ 0.
	self nodesDo: [:each| maxDelta _ maxDelta max: each doMove].
	self edgesDo: [:each| each adjustBounds].
	self redrawNeeded.
	maxDelta < 0.1 ifTrue: [self stopStepping]! !

!GraphMorph methodsFor: 'stepping' stamp: 'len 7/9/2016 22:48'!
stepTime
	^ 0! !

!GraphMorph methodsFor: 'stepping' stamp: 'len 7/9/2016 22:48'!
wantsSteps
	^ true! !

!GraphMorph methodsFor: 'private' stamp: 'len 2/12/2003 23:34'!
addEdge: anEdge
	^ self addEdge: anEdge label: nil! !

!GraphMorph methodsFor: 'private' stamp: 'len 7/14/2016 05:25'!
addEdge: anEdge label: label 
	| src dest |
	src _ self graph nodeAt: anEdge key.
	dest _ self graph nodeAt: anEdge value.
	^ self privateAddEdgeFrom: src to: dest label: label! !

!GraphMorph methodsFor: 'private' stamp: 'len 7/14/2016 05:42'!
addNode: aGraphNode 
	| morph |
	(nodeToMorph includesKey: aGraphNode)
		ifTrue: [^ nodeToMorph at: aGraphNode].
	morph _ self nodeClass new node: aGraphNode.
	morph dimension: dimension.
	nodeToMorph at: aGraphNode put: morph.
	morph morphPosition: self morphWidth asInteger atRandom @ self morphHeight asInteger atRandom.
	self addMorph: morph.
"	aGraphNode
		neighborsAndLabelsDo: [:each :label|
			self addNode: each.
			self privateAddEdgeFrom: aGraphNode to: each label: label]."
	^ morph! !

!GraphMorph methodsFor: 'private' stamp: 'len 7/9/2016 22:26'!
edgeClass
	^ GraphEdgeMorph! !

!GraphMorph methodsFor: 'private' stamp: 'len 7/9/2016 22:26'!
nodeClass
	^ GraphNodeMorph! !

!GraphMorph methodsFor: 'private' stamp: 'len 7/14/2016 05:38'!
privateAddEdgeFrom: source to: target label: label
	| edge morph |
	edge _ source -> target.
	graph isDirected ifFalse: [edge _ edge as: MutualAssociation].
	(edgeToMorph includesKey: edge)
		ifTrue: [^ (edgeToMorph at: edge) incrementMultiplicity].
	morph _ self edgeClass new.
	edgeToMorph at: edge put: morph.
	morph source: (nodeToMorph at: source); target: (nodeToMorph at: target).
	morph label: label.
	graph isDirected ifTrue: [morph showArrowHead] ifFalse: [morph hideArrowHead].
	self addMorphBack: morph.
	^ morph! !

!GraphMorph methodsFor: 'private' stamp: 'len 7/14/2016 05:39'!
privateRemoveEdgeFrom: source to: target label: label 
	| edge |
	edge _ source -> target.
	graph isDirected ifFalse: [edge _ edge as: MutualAssociation].
	(edgeToMorph removeKey: edge ifAbsent: [^ self]) delete! !

!GraphMorph methodsFor: 'private' stamp: 'timestamp) (no'!
removeEdge: anEdge 
	^ self removeEdge: anEdge label: nil! !

!GraphMorph methodsFor: 'private' stamp: 'len 2/17/2016 23:06'!
removeEdge: anEdge label: label 
	^ self privateRemoveEdgeFrom: anEdge key to: anEdge value label: label! !

!SpringGraphMorph methodsFor: 'accessing' stamp: 'len 4/26/2016 02:06'!
desiredDistance: aNumber
	^ self edgesDo: [:each| each desiredDistance: aNumber]! !

!SpringGraphMorph methodsFor: 'accessing' stamp: 'len 5/25/2016 03:15'!
graph: aGraph
	| morph desiredDistance |
	super graph: aGraph.
	(aGraph isKindOf: RootedDigraph)
		ifTrue: [morph _ nodeToMorph at: aGraph rootNode.
				morph morphAlign: morph morphBounds center with: self morphExtent // 2].
	aGraph size < 100
		ifTrue: [desiredDistance _ (self morphLocalBounds area / (aGraph radius min: 10) / 2) sqrt.
			components _ graph components asArray collect: [:each| each asArray collect: [:node| nodeToMorph at: node]]]
		ifFalse:
			[desiredDistance _ 10.
			components _ nil].
	self edgesDo: [:each| each desiredDistance: desiredDistance "/ each source node degree sqrt"]! !

!SpringGraphMorph methodsFor: 'event handling' stamp: 'len 2/8/2003 00:38'!
allowSubmorphExtraction
	" allow extraction so submorphs can be grabbed and dragged to a new position.
	  usefull to rearrange the vertices manually"
	^ true! !

!SpringGraphMorph methodsFor: 'event handling' stamp: 'len 4/27/2016 08:01'!
scaleEdgesBy: aNumber
	self edgesDo: [:each| each desiredDistance: each desiredDistance * aNumber].
	self startStepping! !

!SpringGraphMorph methodsFor: 'stepping' stamp: 'len 4/30/2016 02:22'!
approachComponents
	| componentsDesiredDistance |
	(components isNil or: [components size < 2])
		ifTrue: [^ self].
	componentsDesiredDistance _ 1.
	components do: [:each|
		components do: [:other|
			each == other
				ifFalse:
					[| source target f currentDistance diff delta |
					source _ each atRandom.
					target _ other atRandom.
					(currentDistance _ (diff _ source x + source dx - target x - target dx) norm) > componentsDesiredDistance
						ifTrue:
							[f _ (componentsDesiredDistance - currentDistance) asFloat / (currentDistance * 4).
							delta _ diff * f / 2.
							"target dx: target dx - delta."
							source dx: source dx + (delta/other size)]]]]! !

!SpringGraphMorph methodsFor: 'stepping' stamp: 'len 2/25/2016 16:36'!
aproachConnectedNodes
	self edgesDo: [:each| each approachNodes]! !

!SpringGraphMorph methodsFor: 'stepping' stamp: 'len 2/19/2016 04:17'!
separateAllNodes
	self nodesDo: [:each | each separateFromAllNodes]! !

!SpringGraphMorph methodsFor: 'stepping' stamp: 'len 6/2/2020 17:10:51'!
step
	| maxDelta |
	self nodesDo: [:each| each dampBy: 1.1]. "damping"
	self aproachConnectedNodes; separateAllNodes. "self approachComponents;"
	maxDelta _ 0.0.
	self nodesDo: [:each| maxDelta _ maxDelta max: each doMove].
	self alignToCenter.
	self edgesDo: [:each| each adjustBounds].
	self redrawNeeded.
	maxDelta < 0.1 ifTrue: [self stopStepping]! !

!SpringGraphMorph methodsFor: 'private' stamp: 'len 2/15/2003 18:10'!
edgeClass
	^ SpringEdgeMorph! !

!SpringGraphMorph methodsFor: 'private' stamp: 'len 2/25/2016 17:42'!
nodeClass
	^ SpringNodeMorph! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 6/2/2020 17:25:00'!
alpha
	(self dimension > 2 and: [owner notNil]) ifFalse: [^ 1.0].
	^ (x z / owner morphExtent y + 0.75) squared min: 1.0 max: 0.0! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:25'!
color
	^ color! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:25'!
color: aColor
	color _ aColor! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:39'!
dimension
	^ x size! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 6/2/2020 14:54:27'!
dimension: anInteger
	| n |
	(n _ x size) = anInteger
		ifTrue: [^ self].
	n > anInteger
		ifTrue:
			[x _ x copyFrom: 1 to: anInteger.
			dx _ dx copyFrom: 1 to: anInteger]
		ifFalse:
			[| z |
			z _ (…^(anInteger - n)) zero.
			x _ x, z.
			dx _ dx, z]! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:41'!
dx
	^ dx! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:41'!
dx: aVector
	dx _ aVector! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 2/18/2016 21:42'!
label
	^ label! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 2/18/2016 21:43'!
label: aString
	label _ aString.
	label notNil ifTrue: [self showLabel] ifFalse: [self hideLabel]! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 2/12/2003 23:20'!
node
	^ node! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 4/8/2016 23:02'!
node: aGraphNode 
	node _ aGraphNode.
	self label: node value printText! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 12/19/2016 12:38:16'!
radius
	^ extent x! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 12/19/2016 12:39:19'!
radius: aNumber
	self morphExtent: aNumber @ aNumber! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 2/18/2016 21:25'!
shape
	^ shape! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 2/18/2016 21:31'!
shape: aShapeSymbol
	shape _ aShapeSymbol.
	self redrawNeeded! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 2/18/2016 21:07'!
value
	^ node value! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:14'!
x
	^ x! !

!GraphNodeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:14'!
x: aPoint
	x _ aPoint! !

!GraphNodeMorph methodsFor: 'drawing' stamp: 'len 5/4/2016 06:37'!
drawOn: aCanvas
	(extent x abs < 2 and: [extent y abs < 2]) ifTrue: [^ self].
	shape == #circle
		ifTrue: [aCanvas fillEllipse: self morphLocalBounds color: self color. ^ self].
	shape == #square
		ifTrue: [aCanvas fillRectangle: self morphLocalBounds color: self color. ^ self].
	shape == #squareFrame
		ifTrue: [aCanvas frameRectangle: self morphLocalBounds borderWidth: 1 color: self color. ^ self].
	shape == #circleFrame
		ifTrue: [aCanvas frameEllipse: self morphLocalBounds borderWidth: 1 color: self color. ^ self].
	"otherwise do nothing"
	^ self! !

!GraphNodeMorph methodsFor: 'initialization' stamp: 'len 6/2/2020 13:12:27'!
initialize
	super initialize.
	self color: Color gray.
	x _ (0.0,0.0).
	dx _ x.
	shape _ #circle.
	extent _ 10@10! !

!GraphNodeMorph methodsFor: 'moving' stamp: 'len 6/2/2020 15:03:08'!
dampBy: aNumber
	aNumber > 1.0 ifFalse: [^ self].
	dx _ dx / aNumber! !

!GraphNodeMorph methodsFor: 'moving' stamp: 'len 6/2/2020 17:05:30'!
doMove
	| ownerBounds bounds delta |
	dx do: [:each| each isNaN ifTrue: [dx _ dx zero. ^ 0.0]].
	x _ x + dx.
	ownerBounds _ owner morphLocalBounds.
	self morphPosition: ownerBounds center + ((x at: 1) @ (x at: 2)).
	(ownerBounds containsRect: (bounds _ self morphBounds))
		ifFalse: [delta _ 2.0 * (bounds amountToTranslateWithin: ownerBounds).
				x _ x + (delta x, delta y).
				self morphPosition: bounds center + ((x at: 1) @ (x at: 2)).
				dx _ dx * 0.0].
	^ dx norm! !

!GraphNodeMorph methodsFor: 'moving' stamp: 'len 7/9/2016 22:54'!
freeze
	dx _ dx * 0.0! !

!GraphNodeMorph methodsFor: 'moving' stamp: 'len 7/9/2016 22:42'!
perturbate: n
	dx _ dx + (dx collect: [:xi| n atRandom - n])! !

!GraphNodeMorph methodsFor: 'moving' stamp: 'len 6/2/2020 16:01:50'!
perturbation
	^ x collect: [:xi| 3 atRandom - 2]! !

!GraphNodeMorph methodsFor: 'moving' stamp: 'len 7/9/2016 22:42'!
translateBy: delta
	dx _ dx + delta! !

!GraphNodeMorph methodsFor: 'private' stamp: 'len 2/18/2016 21:42'!
hideLabel
	labelMorph notNil ifTrue: [labelMorph delete. labelMorph _ nil]! !

!GraphNodeMorph methodsFor: 'private' stamp: 'len 4/30/2016 06:55'!
showLabel
	(labelMorph notNil or: [label isNil]) ifTrue: [^ self].
	labelMorph _ StringMorph contents: label font: Preferences standardMenuFont.
	labelMorph color: Color white.
	self addMorph: labelMorph.
	labelMorph morphAlign: labelMorph morphBounds leftCenter with: self morphBounds rightCenter + (1@0)! !

!SpringNodeMorph methodsFor: 'event handling testing' stamp: 'len 7/9/2016 22:10'!
aboutToBeGrabbedBy: aHand 
	super aboutToBeGrabbedBy: aHand.
	(owner isKindOf: GraphMorph) ifTrue: ["owner focusOn: node; stopStepping"]! !

!SpringNodeMorph methodsFor: 'event handling testing' stamp: 'sam 2/10/2004 01:56'!
handlesMouseDown: evt 
	evt shiftPressed
		ifFalse: [^ super handlesMouseDown: evt]
		ifTrue: [^ true]! !

!SpringNodeMorph methodsFor: 'event handling testing' stamp: 'len 2/18/2016 06:02'!
wantsToBeDroppedInto: aMorph
	^ aMorph isKindOf: SpringGraphMorph! !

!SpringNodeMorph methodsFor: 'moving' stamp: 'len 6/2/2020 16:03:40'!
approachPoint: aPoint
	| x1 x2 difference distanceSquared |
	x1 _ x.
	x2 _ aPoint.
	difference _ x1 - x2.
	distanceSquared _ difference norm2.
	distanceSquared > 0.1 ifFalse: [^ self].
	dx _ dx + (difference / difference norm * 2.0)! !

!SpringNodeMorph methodsFor: 'moving' stamp: 'len 6/2/2020 16:08:12'!
separateFromAllNodes
	| force x1 x2 magnitude |
	(owner isKindOf: SpringGraphMorph) ifFalse: [^ self].
	force _ dx zero.
	x1 _ x.
	owner nodesDo: [:each|
		x2 _ each x + each dx.
		self == each
			ifFalse:
				[| difference distanceSquared |
				difference _ x1 - x2.
				distanceSquared _ difference norm2.
				force _ force + (distanceSquared > 1.0 ifTrue: [difference / distanceSquared] ifFalse: [self perturbation])]].
	magnitude _ force norm.
	magnitude > 0.1 ifTrue: [dx _ dx + (force / magnitude * 2)]! !

!SpringNodeMorph methodsFor: 'private' stamp: 'len 2/29/2016 21:28'!
nearbyNodesDo: aBlock
	| count |
	count _ 0.
	node markDo: [:each |
		count _ count + 1.
		count > 40 ifTrue: [^ self].
		aBlock value: (owner nodeAt: each)]! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/30/2016 07:36'!
addComponent: aGraphMorph
	self addMorph: aGraphMorph proportionalWidth: 1! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/30/2016 07:41'!
componentsDo: aBlock
	self submorphsDo: [:each| (each isKindOf: GraphMorph) ifTrue: [aBlock value: each]]! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/30/2016 07:41'!
dimension: anInteger
	self componentsDo: [:each| each dimension: anInteger]! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 05:08'!
edgeAt: anEdge 
	^ self nodeAt: anEdge ifAbsent: [self error: 'edge not found']! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 05:08'!
edgeAt: anEdge ifAbsent: exceptionBlock
	self componentsDo: [:each| (each edgeAt: anEdge ifAbsent: []) ifNotNil: [:edge| ^edge]].
	^ exceptionBlock value! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 05:02'!
edgesAndLabelsDo: aBlock 
	self componentsDo: [:each| each edgesAndLabelsDo: aBlock]! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/30/2016 07:43'!
hideLabels
	self componentsDo: [:each| each hideLabels]! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/30/2016 07:38'!
initialize
	super initialize.
	self color: Color transparent.
	self morphExtent: 600 @ 300! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 05:06'!
nodeAt: anObject 
	^ self nodeAt: anObject ifAbsent: [self error: 'node not found']! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 05:05'!
nodeAt: anObject ifAbsent: exceptionBlock
	self componentsDo: [:each| (each nodeAt: anObject ifAbsent: []) ifNotNil: [:node| ^node]].
	^ exceptionBlock value! !

!CompositeGraphMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 05:03'!
nodesDo: aBlock
	self componentsDo: [:each| each nodesDo: aBlock]! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:11'!
alpha
	^ source alpha + target alpha / 2.0! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:26'!
color
	^ color alpha: self alpha! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 7/9/2016 22:27'!
color: aColor
	color _ aColor! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 2/19/2016 03:25'!
hideArrowHead
	drawArrowHead _ false! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 7/14/2016 05:33'!
incrementMultiplicity
	multiplicity _ multiplicity + 1! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 2/19/2016 19:00'!
label
	^ label! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 2/19/2016 19:00'!
label: aString
	label _ aString.
	label notNil ifTrue: [self showLabel] ifFalse: [self hideLabel]! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 2/19/2016 03:25'!
showArrowHead
	drawArrowHead _ true! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'Samir Saidani 6/23/2003 21:17'!
source
	^ source! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'Samir Saidani 6/23/2003 21:17'!
source: aMorph
	source _ aMorph! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 3/24/2021 13:58:57'!
sourcePoint
	^ self internalizeFromWorld: source displayBounds center! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 2/15/2016 22:56'!
target
	^ target! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 2/15/2016 22:56'!
target: aMorph
	target _ aMorph! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 3/24/2021 13:59:03'!
targetPoint
	^ self internalizeFromWorld: target displayBounds center! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 2/18/2016 06:16'!
width
	^ width! !

!GraphEdgeMorph methodsFor: 'accessing' stamp: 'len 2/18/2016 06:16'!
width: anInteger
	"Set the width of the line to draw."
	width _ anInteger! !

!GraphEdgeMorph methodsFor: 'drawing' stamp: 'len 3/1/2016 01:44'!
direction
	^ (self targetPoint - self sourcePoint) normalized! !

!GraphEdgeMorph methodsFor: 'drawing' stamp: 'len 1/23/2017 09:35:06'!
drawArcOn: aCanvas from: sourcePoint to: targetPoint
	multiplicity odd
		ifTrue: [aCanvas line: sourcePoint to: targetPoint width: width color: self color].
	1 to: multiplicity - (multiplicity \\ 2) do: [:i|
		self drawCurveOn: aCanvas from: sourcePoint to: targetPoint curvature: i+1//2 * (i odd ifTrue: [1] ifFalse: [-1])]! !

!GraphEdgeMorph methodsFor: 'drawing' stamp: 'len 12/19/2016 12:29:46'!
drawArrowOn: aCanvas from: aPoint to: anotherPoint
	| sourcePoint targetPoint direction |
	sourcePoint _ aPoint.
	targetPoint _ anotherPoint.
	direction _ (targetPoint - sourcePoint) normalized.
	target identityHash < source identityHash
		ifTrue:
			[ | shift |
			shift _ (direction rotatedBy: Float pi/2) * 4.
			sourcePoint _ sourcePoint + shift.
			targetPoint _ targetPoint + shift].
	targetPoint _ targetPoint - (direction * target radius).
	sourcePoint _ sourcePoint + (direction * source radius).
	self drawArcOn: aCanvas from: sourcePoint to: targetPoint.
	aCanvas line: targetPoint - (direction*3) - ((direction rotatedBy: Float pi / 4) * 4)
		to: targetPoint width: width color: self color.
	aCanvas line: targetPoint - (direction*2) - ((direction rotatedBy: Float pi / 4) * 4)
		to: targetPoint width: width color: self color.
	aCanvas line: targetPoint - (direction*3) - ((direction rotatedBy: Float pi * 7 / 4) * 4)
		to: targetPoint width: width color: self color.
	aCanvas line: targetPoint - (direction*2) - ((direction rotatedBy: Float pi * 7 / 4) * 4)
		to: targetPoint width: width color: self color.

"	self sourcePoint = self targetPoint
		ifFalse: [aCanvas
				drawThinArrowFrom: self sourcePoint
				to: self targetPoint
				width: 1
				color: self color]"
			
	"| circle string |  
	circle _ EllipseMorph new extent: 20 @ 20;  
	 
	color: (Color  
	r: 1.0  
	g: 0.452  
	b: 0.065).  
	circle align: circle center with: self center.  
	self addMorph: circle.  
	string _ StringMorph new contents: 'test'.  
	string align: string bounds leftCenter with: bounds rightCenter + (4 @  
	0). self addMorph: string"
"	| aa |
	aa _ PolygonMorph new.
	aa computeArrowFormAt: self sourcePoint from: self destinationPoint.
	self addMorph: aa"
! !

!GraphEdgeMorph methodsFor: 'drawing' stamp: 'len 1/23/2017 09:35:19'!
drawCurveOn: aCanvas from: sourcePoint to: targetPoint curvature: curvature
	| perp previous |
	perp _ targetPoint - sourcePoint / 10.0 rotatedBy: Float pi / 2.
	0.0 to: 1.0 count: 15 do: [:i|
		| current |
		current _ sourcePoint * i + (targetPoint * (1.0-i)).
		current _ current + ((0.5 - (0.5-i) abs) sqrt * curvature * perp).
		previous ifNotNil: [aCanvas line: previous to: current width: width color: self color].
		previous _ current]! !

!GraphEdgeMorph methodsFor: 'drawing' stamp: 'len 7/14/2016 02:51'!
drawLoopOn: aCanvas at: aPoint
	aCanvas frameEllipse: (aPoint - (10@20) extent: 20@20) borderWidth: width color: self color! !

!GraphEdgeMorph methodsFor: 'drawing' stamp: 'len 7/14/2016 05:21'!
drawOn: aCanvas 
	"frameRectangle: self bounds color: Color black;"
	| sourcePoint targetPoint |
	"aCanvas frameRectangle: self morphLocalBounds borderWidth: 1 color: Color red."
	[sourcePoint _ self sourcePoint.
	targetPoint _ self targetPoint] on: Error do: [^ self].
	targetPoint = sourcePoint ifTrue: [^ self drawLoopOn: aCanvas at: targetPoint].
	drawArrowHead ifTrue: [^ self drawArrowOn: aCanvas from: sourcePoint to: targetPoint].
	self drawArcOn: aCanvas from: sourcePoint to: targetPoint! !

!GraphEdgeMorph methodsFor: 'geometry testing' stamp: 'len 4/28/2016 05:50'!
morphContainsPoint: aLocalPoint
	^ super morphContainsPoint: aLocalPoint! !

!GraphEdgeMorph methodsFor: 'initialization' stamp: 'len 7/14/2016 05:37'!
initialize
	super initialize.
	self color: Color gray.
	width _ 1.
	drawArrowHead _ true.
	multiplicity _ 1! !

!GraphEdgeMorph methodsFor: 'stepping' stamp: 'len 2/26/2016 03:03'!
step
	(source owner isNil or: [target owner isNil]) ifTrue: [^ self delete].
	self adjustBounds! !

!GraphEdgeMorph methodsFor: 'stepping' stamp: 'Samir Saidani 6/23/2003 21:17'!
stepTime
	^ 0! !

!GraphEdgeMorph methodsFor: 'stepping' stamp: 'len 2/17/2016 22:15'!
wantsSteps
	^ true! !

!GraphEdgeMorph methodsFor: 'private' stamp: 'len 6/2/2020 16:00:04'!
adjustBounds
	| newBounds |
	newBounds _ (self externalize: self sourcePoint) rect: (self externalize: self targetPoint).
	self morphBounds: newBounds.
	labelMorph ifNotNil: [labelMorph morphAlign: labelMorph morphBounds center with: newBounds extent // 2]! !

!GraphEdgeMorph methodsFor: 'private' stamp: 'len 2/19/2016 19:01'!
hideLabel
	labelMorph notNil ifTrue: [labelMorph delete. labelMorph _ nil]! !

!GraphEdgeMorph methodsFor: 'private' stamp: 'len 4/30/2016 06:56'!
showLabel
	(labelMorph notNil or: [label isNil]) ifTrue: [^ self].
	labelMorph _ StringMorph contents: label asString font: Preferences standardMenuFont.
	labelMorph color: Color white.
	self addMorph: labelMorph.
	labelMorph morphAlign: labelMorph morphBounds leftCenter with: self morphBounds rightCenter + (1@0)! !

!SpringEdgeMorph methodsFor: 'initialization' stamp: 'Michael Piel 4/21/2004 11:12'!
initialize
	super initialize.
	desiredDistance _ 100! !

!SpringEdgeMorph methodsFor: 'moving' stamp: 'len 2/25/2016 17:08'!
approachNodes
	"Connected nodes are tied with a rubber band."
	| f currentDistance diff delta |
	(currentDistance _ (diff _ source x + source dx - target x - target dx) norm) > desiredDistance
		ifTrue:
			[f _ (desiredDistance - currentDistance) asFloat / (currentDistance * 4).
			delta _ diff * f / 2.
			target dx: target dx - delta.
			source dx: source dx + delta]! !

!SpringEdgeMorph methodsFor: 'moving' stamp: 'len 4/27/2016 07:57'!
desiredDistance
	^ desiredDistance! !

!SpringEdgeMorph methodsFor: 'moving' stamp: 'len 2/15/2003 18:07'!
desiredDistance: anInteger
	"Set the distance I want between my vertices."
	desiredDistance _ anInteger! !

!SpringEdgeMorph methodsFor: 'stepping' stamp: 'len 2/26/2016 03:04'!
step
	(source owner isNil or: [target owner isNil]) ifTrue: [^ self delete].
"	self approachNodes."
	self adjustBounds! !

!SpringEdgeMorph methodsFor: 'stepping' stamp: 'len 2/24/2016 21:12'!
wantsSteps
	^ false! !

!Graph methodsFor: '*Morphic-Graphs' stamp: 'len 7/30/2016 07:33'!
asMorph
	| answer components |
	(components _ self components) size = 1
		ifTrue: [^ SpringGraphMorph new graph: self].
	answer _ CompositeGraphMorph newRow.
	components do: [:each|
		answer addComponent: (SpringGraphMorph new graph: each)].
	^ answer! !
