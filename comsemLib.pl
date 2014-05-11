/*************************************************************************
based on:
         name: comsemLib.pl
      version: July 13, 2001
  description: Term Manipulation, Unification, Printing
      authors: Patrick Blackburn & Johan Bos

deletions and additions:

      version: July, 2002
  description: Helpers
      authors: Stephan Walter, Aljoscha Burchardt

*************************************************************************/

:- module(comsemLib,
	[compose/3,
	substitute/4,
	printRepresentations/1
	]).

:- ensure_loaded(signature).

/*========================================================================
   Compose predicate argument structure
========================================================================*/

compose(Term,Symbol,ArgList):-
    Term =.. [Symbol|ArgList].

/*========================================================================
   Substitution Predicates
========================================================================*/

substitute(Term,Var,Exp,Result):- 
   Exp==Var, !, Result=Term.
substitute(_Term,_Var,Exp,Result):- 
   \+ compound(Exp), !, Result=Exp.
substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,[Exp,F]),
   member(Functor,[lambda,forall,exists]), !, 
   (
    Exp==Var, !, 
    Result=Formula
   ; 
    substitute(Term,Var,F,R),
    compose(Result,Functor,[Exp,R])
   ).
substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,ArgList),
   substituteList(Term,Var,ArgList,ResultList),
   compose(Result,Functor,ResultList).

substituteList(_Term,_Var,[],[]).
substituteList(Term,Var,[Exp|Others],[Result|ResultOthers]):-
   substitute(Term,Var,Exp,Result),
   substituteList(Term,Var,Others,ResultOthers).

/*========================================================================
   Printing a set of representations
========================================================================*/

printRepresentations(Readings):-
   printRep(Readings,0).

printRep([],_):- nl.
printRep([Reading|OtherReadings],M):-
   N is M + 1, nl, write(N), tab(2), 
   \+ \+ (numbervars(Reading,0,_), write(Reading)),
   printRep(OtherReadings,N).



/*========================================================================
   Generate the Herbrand base out of a (signed) model
========================================================================*/



