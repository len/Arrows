'From Cuis6.3 [latest update: #6222] on 12 April 2024 at 6:14:29 am'!
'Description '!
!provides: 'Tests-Graphs' 1 3!
!requires: 'Graphs' 1 1 nil!
SystemOrganization addCategory: #'Tests-Graphs'!


!classDefinition: #GraphTest category: #'Tests-Graphs'!
TestCase subclass: #GraphTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Graphs'!
!classDefinition: 'GraphTest class' category: #'Tests-Graphs'!
GraphTest class
	instanceVariableNames: ''!


!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 12/30/2017 16:17:24'!
testCartesianProduct
	| G H |
	G _ Graph unordered addEdge: 1 -> 2; yourself.
	H _ G × G.
	self assert: H size = 4.
	self assert: H numberOfEdges = 4! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 8/12/2022 13:16:32'!
testComplement
	| G H |
	G _ Graph unordered addEdges: {#a -> #b. #b -> #c. #d -> #e}; yourself.
	H _ G complement.
	self assert: (G ∪ H) isComplete.
	self assert: (G ∩ H) isEdgeless.
	self assert: H complement = G! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2016 10:08'!
testConnectedComponents
	| G |
	G _ Graph unordered addEdges: {#a -> #b. #b -> #c. #d -> #e}; yourself.
	self assert: G components size = 2.
	self assert: (G components collect: [:each| each order]) = #(2 3) asSet.
	self assert: G components anyOne components size = 1! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2016 03:39'!
testDistance
	| G |
	G _ Digraph unordered addEdges: {#a -> #b. #b -> #c. #d -> #e}; yourself.
	self assert: (G distanceFrom: #a to: #c) = 2.
	self assert: (G distanceFrom: #d to: #e) = 1.
	self assert: (G distanceFrom: #b to: #b) = 0.
	self assert: (G distanceFrom: #a to: #e) isInfinite! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2016 03:39'!
testEquals
	| G H |
	G _ Digraph ordered addEdges: {#a -> #b. #b -> #c. #c -> #a. #X -> #Y}; yourself.
	H _ Digraph ordered addEdges: {#a -> #b. #b -> #c. #c -> #a}; yourself.
	self assert: G ~= H.
	H addEdge: #X -> #Y.
	self assert: G = H.
	self assert: G hash = H hash! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2016 03:15'!
testImplicit
	| G |
	G _ RootedDigraph unordered roots: {2^3} children: [:each| each divisors copyWithout: each].
	self assert: G size = 4.
	self assert: G values asSet = #(1 2 4 8) asSet.
	self assert: G edges size = 6! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 8/12/2022 13:16:40'!
testIntersection
	| G H I |
	G _ Digraph unordered addEdges: {#a -> #b. #b -> #c. #d -> #e. #x -> #b}; yourself.
	H _ Digraph unordered addEdges: {#A -> #B. #b -> #c. #d -> #e. #X -> #X. #b -> #a}; yourself.
	I _ G ∩ H.
	self assert: I numberOfEdges = 2.
	self assert: (I hasEdge: #b -> #c).
	self assert: (I hasEdge: #d -> #e)! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/11/2024 07:57:14'!
testIsComplete
	| G |
	G _ Digraph unordered.
	G addEdges: {#a -> #b. #b -> #a. #b -> #c. #a -> #c. #c -> #a}.
	G _ G reflexiveClosure.
	self assert: G isComplete not.
	G addEdge: #c -> #b.
	self assert: G isComplete! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2016 10:03'!
testIsConnected
	| G |
	G _ Graph unordered addEdges: {#a -> #b. #b -> #c. #d -> #e}; yourself.
	self assert: G isConnected not.
	G addEdge: #e -> #a.
	self assert: G isConnected! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2016 03:39'!
testIsCyclic
	| G |
	G _ Digraph unordered addEdges: {#a -> #b. #b -> #c. #d -> #e}; yourself.
	self assert: G isCyclic not.
	G addEdge: #e -> #a.
	self assert: G isCyclic not.
	G addEdge: #c -> #a.
	self assert: G isCyclic! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2016 03:40'!
testIsSimple
	| G |
	G _ Digraph ordered addEdges: {#a -> #b. #b -> #c. #d -> #e}; yourself.
	self assert: G isSimple.
	G addEdge: #c -> #b.
	self assert: G isSimple.
	G addEdge: #c -> #b.
	self assert: G isSimple not! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2016 03:41'!
testIsSimple2
	| G |
	G _ Digraph ordered addEdges: {#a -> #b. #b -> #c. #d -> #e}; yourself.
	self assert: G isSimple.
	G addEdge: #c -> #c.
	self assert: G isSimple not "a graph with a loop is not simple"! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2016 10:04'!
testPetersenGraph
	| G |
	G _ Graph petersen.
	self assert: G nodes size = 10.
	self assert: G edges size = 15.
	self assert: G isCubic.
	self assert: G radius = 2.
	self assert: G diameter = 2.
"	self assert: G girth = 5"! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 5/25/2016 03:41'!
testShortestPath
	| G |
	G _ Digraph unordered.
	G addEdges: {#a -> #b. #b -> #a. #b -> #c. #a -> #c. #c -> #a}.
	self assert: (G shortestPathFrom: #c to: #b) asArray = #(a b).
	G addEdge: #c -> #b.
	self assert: (G shortestPathFrom: #c to: #b) asArray = #(b)! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 8/9/2020 07:51:20'!
testTranspose
	| G T |
	G _ Digraph unordered addEdges: {#a -> #b. #b -> #c. #d -> #e}; yourself.
	T _ G transpose.
	self assert: (G hasEdge: #b -> #c).
	self assert: (G hasEdge: #c -> #b) not.
	self assert: (T hasEdge: #b -> #c) not.
	self assert: (T hasEdge: #c -> #b)! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 11/16/2016 10:08'!
testUndirectedEdgeContraction
	| G |
	G _ Graph ordered addEdges: {1->2. 2->3. 3->4. 4->1. 1->3}; yourself.
	self assert: G edges size = 5.
	self assert: (G / (1->2)) edges size = 4.
	self assert: (G / (1->2) / ((1->2)->3)) edges size = 3! !

!GraphTest methodsFor: 'as yet unclassified' stamp: 'len 4/11/2024 07:57:52'!
testUnion
	| G |
	G _ Digraph unordered addEdges: {#a -> #b. #b -> #c. #d -> #e. #x -> #b}; yourself.
	self assert: (G ∪ G transpose) = G symmetricClosure! !
