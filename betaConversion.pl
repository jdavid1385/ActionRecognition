/*************************************************************************

         name: betaConversion.pl
      version: May 2002
  description: Predicates for beta-reduction
      authors: Johan Bos, Patrick Blackburn

*************************************************************************/
  
:- module(betaConversion,[betaConvertList/2,betaConvert/2]).
  
%:- ensure_loaded(comsemOperators).
  
:- use_module(comsemLib,[compose/3,substitute/4]).

:- op(950,yfx,@).       % application
  
betaConvert(Var,Result):-
	var(Var),
	!,
	Result=Var.
  
betaConvert(Functor@Arg,Result):-
        compound(Functor),
        betaConvert(Functor,lambda(X,Formula)),
        !,
        substitute(Arg,X,Formula,BetaConverted),
        betaConvert(BetaConverted,Result).
  
betaConvert(Formula,Result):-
	compose(Formula,Functor,Formulas),
	betaConvertList(Formulas,ResultFormulas),
	compose(Result,Functor,ResultFormulas).
  
betaConvertList([],[]).

betaConvertList([Formula|Others],[Result|ResultOthers]):-
	betaConvert(Formula,Result),
	betaConvertList(Others,ResultOthers).







