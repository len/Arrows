'From Cuis 6.0 [latest update: #5434] on 5 August 2022 at 9:28:07 am'!
'Description '!
!provides: 'Tests-Tableaux' 1 0!
!requires: 'Tableaux' 1 0 nil!
SystemOrganization addCategory: 'Tests-Tableaux'!


!classDefinition: #TableauxTest category: 'Tests-Tableaux'!
TestCase subclass: #TableauxTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Tableaux'!
!classDefinition: 'TableauxTest class' category: 'Tests-Tableaux'!
TableauxTest class
	instanceVariableNames: ''!


!TableauxTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 12:39:27'!
testYoungTableauxSemistandard
	| T |
	T _ YoungTableau rows: #((1 1 2 2 3) (2 3 4 4) (4) (5)).
	self assert: T isSemistandard.
	self assert: T shape = #(5 4 1 1)! !

!TableauxTest methodsFor: 'as yet unclassified' stamp: 'len 4/16/2022 12:39:20'!
testYoungTableauxStandard
	| T |
	T _ YoungTableau rows: #((1 4 6 8) (2 5 7) (3)).
	self assert: T isStandard.
	self assert: T weight = #(1 1 1 1 1 1 1 1).
	self assert: T shape = #(4 3 1)! !
