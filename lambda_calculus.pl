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

s(NP@VP)--> np(NP), vp(VP).

s(NP@VP)--> np(NP), vp(VP), v_mod(PP).

%v_mod(NP@VP)--> pp

%pp(PP@VP)-->  pa(PP).

np(PN)--> pn(PN).

np(Det@Noun)--> det(Det), noun(Noun).

vp(IV)--> iv(IV).

vp(TV@NP)--> tv(TV), np(NP).

/*========================================================================
  Lexical Rules
========================================================================*/

det(lambda(P, lambda(Q, forall(X, (P@X) => (Q@X))))) --> [every].

det(lambda(P, lambda(Q, exists(X, (P@X) &  (Q@X))))) --> [a].

%pa(lambda(P,lambda(Q,towards(P, Q))))--> [towards].

pn(lambda(P,P@john))--> [john].

pn(lambda(P,P@mary))--> [mary].

noun(lambda(X,siamesecat(X)))--> [siamese,cat].

noun(lambda(X,woman(X)))--> [woman].

noun(lambda(X,car(X)))--> [car].

ivp(lambda(X,P))--> iv(P).

ivp(lambda(X,X@P))--> iv(P).

iv(lambda(X,walk(X)))--> [walks].

tv(lambda(X,lambda(Y,X@lambda(Z,love(Y,Z)))))--> [loves].

pa(lambda(X,lambda(Q,X@lambda(P,towards(P, Q)))))--> [towards].

snt([mary,loves,john]).
snt([john,walks]).
snt([a,woman,loves,a,siamese,cat]).
snt([a,woman,walks]).
snt([a,car,moves]).
snt([john,walks,towards,mary]).




test(Converted) :-
        snt(Sentence),
	s(Formula,Sentence,[]),
	resetVars,vars2atoms(Formula),
        writeln(Formula),
	betaConvert(Formula,Converted).

test2(Salida) :- resetVars, JOPO = ((lambda(X,X@lambda(Y,moves(Y)))@lambda(R,R@john))@lambda(X,lambda(Q,X@lambda(P,towards(P, Q))))@lambda(T,T@mary)), 
    vars2atoms(JOPO), betaConvert(JOPO, Salida).

test3(Salida) :- resetVars, JOPO = (lambda(R,R@mary)@(lambda(R,R@john)@(lambda(P,lambda(Q,towards(P, Q)))@lambda(Y,moves(Y))))),
    vars2atoms(JOPO), betaConvert(JOPO, Salida).

testing(Salida) :- resetVars, JJ = 
    (((lambda(L,lambda(M, L@lambda(G,towards(M,G) ))) @ ((lambda(P, lambda(Q, exists(X, (P@X)&(Q@X) ))) @ lambda(H, car(H))) @ (lambda(S, lambda(T, exists(Y, (S@Y)&(T@Y) ))) @ lambda(Z,man(Z))))) @ lambda(U,moves(U)))) ,
    vars2atoms(JJ), betaConvert(JJ, Salida).

% [What I want] --> a car moves towards a man
% exists(x, car(x) & exists(y, men(y) & towards(moves(x), y)))

% a ==> lambda(P, lambda(Q, exists(X, (P@X)&(Q@X) )))
% car ==> lambda(X, car(X))
% moves ==> lambda(X, moves(X))
% towards ==> lambda(X,lambda(Y,X@lambda(Z,towards(Y,Z))))--> [loves].
% man ==> lambda(Z, man(X))





test4(Salida) :- resetVars, JOPO = (lambda(R,R@mary)@(lambda(R,R@john)@(lambda(P,lambda(Q,towards(P, Q)))@lambda(Y,moves(Y))))),
    vars2atoms(JOPO), betaConvert(JOPO, Salida).


test5(Salida) :- resetVars, JOPO = (((lambda(R,R@john)@lambda(X,X@lambda(Y,moves(Y))))@lambda(X,lambda(Q,X@lambda(P,towards(P, Q)))))@lambda(T,T@mary)), vars2atoms(JOPO), betaConvert(JOPO, Salida).


testing(Salida) :-
    resetVars,
    JJ = (
           ((lambda(P, lambda(Q, exists(Y, (P@Y) & (lambda(R, exists(X, (R@X) & (lambda(U,towards(Q@U,Y))@X) ))@Y)  ))) @ lambda(T,man(T)) ) @ lambda(V, moves(V)) ) @  lambda(U, car(U))
         ),
         vars2atoms(JJ), betaConvert(JJ, Salida).
