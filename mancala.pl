:- use_module(library(arrays),[
                               aref/3,
			       aset/4,
                               new_array/1
                              ]).

:- use_module(library(lists),[
			      append/3,
			      nth0/3
			     ]).

writefirstmoves([]).
writefirstmoves([strategy(MancalaA,MoveList,FieldsList)|MoreMoves]) :-
	format("Fields: ~q   Mancala A: ~d   Moves: ~q~n",[FieldsList,MancalaA,MoveList]),
	writefirstmoves(MoreMoves).

goodfirstmove :-
	start_state(StartFields),
	setof(strategy(MancalaA,MoveList,FieldsList),
	      EndFields^(
			 goodfirstmove_aux(StartFields,MoveList,EndFields),
			 fields_to_list(EndFields,FieldsList),
			 nth0(6,FieldsList,MancalaA)
			),
	      Strategies),
	writefirstmoves(Strategies).

goodfirstmove_aux(Fields,[SidePos|NextMoves],EndFields) :-
	findmove(Fields,0,InHand,NewFields,SidePos),
	move(state(InHand,NewFields,pos(0,SidePos),0),state(NewerFields,NextPlayer)),
	goodfirstmove_aux_aux(NextPlayer,NewerFields,NextMoves,EndFields).

goodfirstmove_aux_aux(0,Fields,Moves,EndFields) :-
	goodfirstmove_aux(Fields,Moves,EndFields).
goodfirstmove_aux_aux(1,Fields,[],Fields).

findmove(Fields,PlayerSide,InHand,NewFields,PlayerSidePos) :-
	aref(PlayerSide,Fields,PlayerSideFields),
	findmove_aux(0,PlayerSideFields,InHand,PlayerSidePos,NewPlayerSideFields),
	aset(PlayerSide,Fields,NewPlayerSideFields,NewFields).

findmove_aux(6,_,_,_,_) :-
	!,
	fail.
findmove_aux(SidePos,SideFields,InHand,SidePos,NewSideFields) :-
	aref(SidePos,SideFields,InHand),
	InHand > 0,
	aset(SidePos,SideFields,0,NewSideFields).
findmove_aux(SidePos,SideFields,InHand,ChosenSidePos,NewSideFields) :-
	NewSidePos is SidePos + 1,
	findmove_aux(NewSidePos,SideFields,InHand,ChosenSidePos,NewSideFields).

start_state(StartState) :-
	start_state_one_side(StartStateSide0),
	start_state_one_side(StartStateSide1),
	new_array(EmptyArray),
	aset(0,EmptyArray,StartStateSide0,HalfArray),
	aset(1,HalfArray,StartStateSide1,StartState).

start_state_one_side(SideArray) :-
	new_array(EmptyArray),
	start_state_one_side_aux(0,EmptyArray,SideArray).

start_state_one_side_aux(6,OldArray,SideArray) :-
	!,
	aset(6,OldArray,0,SideArray).
start_state_one_side_aux(Index,OldArray,SideArray) :-
	aset(Index,OldArray,4,NewArray),
	NewIndex is Index + 1,
	start_state_one_side_aux(NewIndex,NewArray,SideArray).

move(state(0,Fields,pos(PlayerSide,6),PlayerSide),state(Fields,PlayerSide)) :- !.
move(state(0,Fields,Pos,PlayerSide),EndState) :-
	getfieldvalue(Pos,Fields,CurrentSideFields,Value),
	Value > 1,
	!,
	erasefieldvalue_known(Pos,Fields,CurrentSideFields,NewFields),
	move(state(Value,NewFields,Pos,PlayerSide),EndState).
move(state(0,Fields,pos(PlayerSide,Pos),PlayerSide),state(NewestFields,OtherSide)) :-
	!,
	switchsides(PlayerSide,OtherSide),
	OtherSidePos is 5 - Pos,
	getfieldvalue(pos(OtherSide,OtherSidePos),Fields,OtherSideFields,Value),
	(Value > 0 ->
	    erasefieldvalue_known(pos(OtherSide,OtherSidePos),Fields,OtherSideFields,NewFields),
	    updatefieldvalue(pos(PlayerSide,6),NewFields,Value,NewestFields)
	;
	    NewestFields = Fields).
move(state(0,Fields,pos(OtherSide,_),_),state(Fields,OtherSide)) :- !.
move(state(InHand,Fields,Pos,PlayerSide),EndState) :-
	setnewpos(Pos,PlayerSide,NewPos),
	NewInHand is InHand - 1,
	updatefieldvalue(NewPos,Fields,1,NewFields),
	move(state(NewInHand,NewFields,NewPos,PlayerSide),EndState).

getfieldvalue(Pos,Fields,Value) :-
	getfieldvalue(Pos,Fields,_,Value).
getfieldvalue(pos(Side,Pos),Fields,CurrentSideFields,Value) :-
	aref(Side,Fields,CurrentSideFields),
	aref(Pos,CurrentSideFields,Value).

setfieldvalue(pos(Side,Pos),Fields,Value,NewFields) :-
	aref(Side,Fields,CurrentSideFields),
	setfieldvalue(pos(Side,Pos),Fields,CurrentSideFields,Value,NewFields).
setfieldvalue(pos(Side,Pos),Fields,CurrentSideFields,Value,NewFields) :-
	aset(Pos,CurrentSideFields,Value,NewCurrentSideFields),
	aset(Side,Fields,NewCurrentSideFields,NewFields).

updatefieldvalue(Pos,Fields,Difference,NewFields) :-
	updatefieldvalue(Pos,Fields,Difference,_,NewFields).
updatefieldvalue(Pos,Fields,Difference,OldValue,NewFields) :-
	getfieldvalue(Pos,Fields,CurrentSideFields,OldValue),
	NewValue is OldValue + Difference,
	setfieldvalue(Pos,Fields,CurrentSideFields,NewValue,NewFields).

erasefieldvalue(Pos,Fields,OldValue,NewFields) :-
	getfieldvalue(Pos,Fields,CurrentSideFields,OldValue),
	erasefieldvalue_known(Pos,Fields,CurrentSideFields,NewFields).
erasefieldvalue_known(Pos,Fields,CurrentSideFields,NewFields) :-
	setfieldvalue(Pos,Fields,CurrentSideFields,0,NewFields).

%% setnewpos(pos(Side,Pos),PlayerSide,pos(NewSide,NewPos))
setnewpos(pos(Side,5),PlayerSide,pos(PlayerSide,0)) :-
	Side \== PlayerSide,
	!.
setnewpos(pos(Side,6),_,pos(OtherSide,0)) :-
	!,
	switchsides(Side,OtherSide).
setnewpos(pos(Side,Pos),_,pos(Side,NewPos)) :-
	NewPos is Pos + 1.

switchsides(0,1).
switchsides(1,0).

fields_to_list(EndFields,FieldsList) :-
	aref(0,EndFields,SideA),
	fields_to_list_aux(0,SideA,ListA),
	aref(1,EndFields,SideB),
	fields_to_list_aux(0,SideB,ListB),
	append(ListA,ListB,FieldsList).

fields_to_list_aux(7,_,[]) :- !.
fields_to_list_aux(Index,SideArray,[Value|SideList]) :-
	aref(Index,SideArray,Value),
	NewIndex is Index + 1,
	fields_to_list_aux(NewIndex,SideArray,SideList).
