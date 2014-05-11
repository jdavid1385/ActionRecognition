/*************************************************************************

         name: firstLambda.pl
      version: May 2002
  description: A DCG for lambda-based semantic construction
      authors: Stephan Walter, Aljoscha Burchardt

*************************************************************************/

:- module(firstLambda, [s/3, np/3, vp/3]).

%:- [comsemOperators].

:- use_module(comsemLib, [compose/3, substitute/4]).
:- use_module(signature, [resetVars/0, vars2atoms/1]).
 
betaConvert(Var,Result) :-
	var(Var),
	!,
	Result = Var.

:- op(950, yfx,  @).    % application
:- op(900, yfx, =>).    % implication
:- op(800, yfx,  &).    % conjunction

betaConvert(Functor@Arg, Result) :-
        compound(Functor),
        betaConvert(Functor, lambda(X,Formula)),
        !,
        substitute(Arg, X, Formula, BetaConverted),
        betaConvert(BetaConverted, Result).
  
betaConvert(Formula, Result) :-
	compose(Formula, Functor,Formulas),
	betaConvertList(Formulas, ResultFormulas),
	compose(Result, Functor, ResultFormulas).
  
betaConvertList([], []).

betaConvertList([Formula|Others], [Result|ResultOthers]) :-
	betaConvert(Formula, Result),
	betaConvertList(Others, ResultOthers).

/*========================================================================
  Combine Rules
========================================================================
*/

s(NP@(PP@VP))--> np(NP), vp(VP), pp(PP).

s(NP@VP)--> np(NP), vp(VP).

np(PN)--> pn(PN).

np(Det@Noun)--> det(Det), noun(Noun).

vp(IV)--> iv(IV).

vp(TV@NP)--> tv(TV), np(NP).

pp(Prep@NP) --> prep(Prep), np(NP).

/*========================================================================
  Lexical Rules
========================================================================*/

det(lambda(P, lambda(Q, forall(X, (P@X) => (Q@X))))) --> [every].

det(lambda(P, lambda(Q, exists(X, (P@X) &  (Q@X))))) --> [a].

pn(lambda(P,P@john))--> [john].

pn(lambda(P,P@mary))--> [mary].

noun(lambda(X,siamesecat(X)))--> [siamese,cat].

noun(lambda(X,woman(X)))--> [woman].

noun(lambda(X,car(X)))--> [car].

noun(lambda(X,man(X)))--> [man].

iv(lambda(X,walk(X)))--> [walks].

iv(lambda(X,move(X)))--> [moves].


tv(lambda(X,lambda(Y,X@lambda(Z,love(Y,Z)))))--> [loves].

/*

 A preposition has an action (Q) and an object (Y).
 We know that there exists an entity that must perform the action but
 this entity is also missing, so we have three 
 missing pieces: 
  + entity performing the action,
  + the action itself,
  + the object of the prepositional phrase

*/
 
prep(lambda(P,lambda(Q,lambda(X, P@lambda(Y,towards(Q@X,Y) ))))) --> [towards].

snt([mary,loves,john]).
snt([john,walks]).
snt([a,woman,loves,a,siamese,cat]).
snt([a,woman,walks]).
snt([a,car,moves]).
snt([a,car,moves,towards,a,woman]).
snt([john,walks,towards,mary]).

test(Converted) :-
        snt(Sentence),
        s(Formula,Sentence,[]),
	resetVars,vars2atoms(Formula),
	betaConvert(Formula,Converted).

test_final(Salida) :- %resetVars,

    Agent = lambda(O, car(O)),
    Prep = lambda(L,lambda(M,lambda(J, L@lambda(G,towards(M@J,G) )))),
    Object = lambda(Z,man(Z)),
    Verb = lambda(X,move(X)),
    A = lambda(S, lambda(T, exists(Y, (S@Y)&(T@Y) ))),
    A2 = lambda(H, lambda(P, exists(U, (H@U)&(P@U) ))),
    %A2 =  lambda(P, exists(U, car(U) & (P@U) )),

    Phrase = ((A2@Agent)@((Prep@(A@Object))@Verb)),
    %Phrase = (A2@((Prep@(A@Object))@Verb)),
    s(Phrase, Salida, []).
%    vars2atoms(Phrase), betaConvert(Phrase, Salida).
    


% [What I want] --> a car moves towards a man
% exists(x, car(x) & exists(y, men(y) & towards(moves(x), y)))

% a ==> lambda(P, lambda(Q, exists(X, (P@X)&(Q@X) )))
% car ==> lambda(X, car(X))
% moves ==> lambda(X, moves(X))
% towards ==> lambda(X,lambda(Y,X@lambda(Z,towards(Y,Z))))--> [loves].
% man ==> lambda(Z, man(X))

