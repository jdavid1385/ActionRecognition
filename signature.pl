/*************************************************************************

         name: signature.pl
      version: July 2002
  description: Declaration of example vocabulary & predicates to work with it
      authors: Michael Kohlhase, Aljoscha Burchardt, Stephan Walter
 
*************************************************************************/

:- module(signature,[const/1,fovar/1,pred/2,newvar/1,newconst/1,
	             vars2atoms/1,vars2atomsList/1,resetVars/0]).

:- dynamic(varcount/1).
:- dynamic(constcount/1).
:- dynamic(const/1).
:- dynamic(fovar/1).


/*========================================================================
   Vocabulary
========================================================================*/

%% First-order individual constants
const(mary).
const(john).
const(tweety).
const(mutzi).
const(miles).
const(anna).
const(peter).

%% Predicates
pred(walk,1).
pred(woman,1).
pred(man,1). 
pred(therapist,1).
pred(siamesecat,1).
pred(bird,1).
pred(owner,1).
pred(moron,1).
pred(run,1).
pred(sleep,1).
 
pred(love,2).
pred(hate,2).
pred(eat,2).
pred(of,2).

%% Variables
fovar(x).
fovar(y).
fovar(z).
fovar(u).
fovar(v).
fovar(w).


/*========================================================================
   New Constants
========================================================================*/

%% new consts
constcount(0).

newconst(Sym) :-
	inc(constcount,N),
	concat_atom([c,N],Sym),
	assert(const(Sym)).


/*========================================================================
   New Variables
========================================================================*/

%% First-order variables, they are represented as PROLOG constants.
%% var would overwrite in-built Prolog var/1, so we assert them as  fovar(x),fovar(y)...

assertVars :- 
	assertVarsRec([x,y,z,u,v,w]).

assertVarsRec([]). 

assertVarsRec([V|Vs]) :-
	assert(fovar(V)),
	assertVarsRec(Vs).

resetVars :-
	retractall(fovar(_)),
	assertVars,
	retractall(varcount(_)),
	assert(varcount(0)).

:- resetVars.

int_to_atom(Int,Atom):-number_codes(Int,Codes),name(Atom,Codes).

%% new variables v1, v2...
newvar(Sym) :-
	inc(varcount,N),
	concat_atom([v,N],Sym),
	assert(fovar(Sym)).

/*========================================================================
   Make (var-)atoms out of all Prolog variables of a term.
========================================================================*/

vars2atoms(T) :-
	free_variables(T,Vars),
	vars2atomsRec(Vars).

vars2atomsRec([]).

vars2atomsRec([H|T]) :-
	newvar(H),
	vars2atomsRec(T).


vars2atomsList([]).

vars2atomsList([H|T]) :-
	vars2atoms(H),
	vars2atomsList(T).


/*========================================================================
   A Little Helper
========================================================================*/

inc(Counter,New) :-
	C =.. [Counter,Old],
	retract(C),
	New is Old + 1,
	NewC =.. [Counter,New],
	assert(NewC).
