:- module(contour, [intro_test/5, detect_object/2]).

type(line(X1:Y1, X2:Y2), hor_pos)   :- Y1 == Y2, X1 < X2.
type(line(X1:Y1, X2:Y2), hor_neg)   :- Y1 == Y2, X1 > X2.
type(line(X1:Y1, X2:Y2), ver_pos)   :- X1 == X2, Y1 < Y2.
type(line(X1:Y1, X2:Y2), ver_neg)   :- X1 == X2, Y1 > Y2.

type(line(X1:Y1, X2:Y2), slope_I)   :- X2 > X1, Y2 > Y1.
type(line(X1:Y1, X2:Y2), slope_II)  :- X2 < X1, Y2 > Y1.
type(line(X1:Y1, X2:Y2), slope_III) :- X2 < X1, Y2 < Y1.
type(line(X1:Y1, X2:Y2), slope_IV)  :- X2 > X1, Y2 < Y1.

% Since the scan is left to right the only possible initial
% transitions are those with an x-component >= 0.
starting([slope_I, slope_II, ver_pos, ver_neg, hor_pos, circle]).

% Call __ypos__ the set of line types with y-component > 0. Respectively __yneg__ to
% the set of line types with y-component < 0.

plane(ypos, [slope_I,   slope_II, ver_pos]).
plane(yneg, [slope_III, slope_IV, ver_neg]).

% Once detected the figure, test that its soundness. The 3th Output
% argument can be fed directly into the centroid algorithm as the
% underlying geometry is already in its clock-wise format.

intro_test(Xi:Yi, Feats, [Xi:Yi|R], [Feat|Re], Rem_Feats) :- 
                               starting(Choices),
                               line_from_feature((Xi:Yi)/(Xj:Yj), Feat, Type-Mats, Feats),
                               member(Type, Choices),                            % Possible choices for a start line
                               select(Feat, Feats, LFeats),
                               test(Xj:Yj, LFeats, R, Type-Mats, Re, Rem_Feats). % start looking for a closed geometry

test(Xi:Yi, Feats, [Xi:Yi|R], TypeOrig, [Feat|Re], Rem_Feats) :-
                               line_from_feature((Xi:Yi)/(Xj:Yj), Feat, TypeDest, Feats),
                               inner_transition(TypeOrig, TypeDest),             % Is it a correct inner geometry transition?
                               select(Feat, Feats, LFeats),
                               test(Xj:Yj, LFeats, R, TypeDest, Re, Rem_Feats).  % Is it possible having more connected features starting from Xj:Yj?

test(X:Y, LFeats, [X:Y], _, [], LFeats).

% TODO: This is not backtracing, it might be a problem with the member
% outside. It reads as, there is a member of choices such that
% line_from_feature/4 is true.

line_from_feature((Xi:Yi)/(Xj:Yj), feature(GType,Points,ULO:DRI), Type-(ULO:DRI), Feats) :-
                                       member(feature(GType,Points,ULO:DRI), Feats),
                                       select(Xi:Yi, Points, RPoints),            % Is it possible to take Xi:Yi from the points in first feature?
                                       member(Xj:Yj, RPoints),                    % pick a second point Xj:Yj (It is generic for lines and circles)
                                       notrace(type(line(Xi:Yi, Xj:Yj), Type)).   % Regardless of its belonging type (line or circle)

% Transitions with the same line type and equivalent side material.
inner_transition(Type, Type). 

%=====================[ Transitions with material (Mat) to the left ]==========================%

% Vertical =negative (positive)= line with material to the =left= 
inner_transition(ver_neg-(Mat:bg), Type-(Mat:bg))   :- plane(yneg, Allowed), member(Type, Allowed).
inner_transition(ver_pos-(Mat:bg), Type-(Mat:bg))   :- plane(ypos, Allowed), member(Type, Allowed).

% Vertical =negative (positive)= line with material to the =right=
inner_transition(ver_neg-(bg:Mat), Type-(Mat:bg))   :- plane(ypos, Allowed), member(Type, Allowed).
inner_transition(ver_pos-(bg:Mat), Type-(Mat:bg))   :- plane(yneg, Allowed), member(Type, Allowed).

% slope_I (slope_III)= line with material to the =left=
inner_transition(slope_I-(Mat:bg),   Type-(Mat:bg)) :- plane(ypos, Allowed), member(Type, Allowed).
inner_transition(slope_III-(Mat:bg), Type-(Mat:bg)) :- plane(yneg, Allowed), member(Type, Allowed).

% slope_I (slope_III)= line with material to the =right=
inner_transition(slope_I-(bg:Mat),   Type-(Mat:bg)) :- plane(yneg, Allowed), member(Type, Allowed).
inner_transition(slope_III-(bg:Mat), Type-(Mat:bg)) :- plane(ypos, Allowed), member(Type, Allowed).

% slope_II (slope_IV)= line with material to the =left=
inner_transition(slope_II-(Mat:bg), Type-(Mat:bg))  :- plane(ypos, Allowed), member(Type, Allowed).
inner_transition(slope_IV-(Mat:bg), Type-(Mat:bg))  :- plane(yneg, Allowed), member(Type, Allowed).

% slope_II (slope_IV)= line with material to the =right=
inner_transition(slope_II-(bg:Mat), Type-(Mat:bg))  :- plane(yneg, Allowed), member(Type, Allowed).
inner_transition(slope_IV-(bg:Mat), Type-(Mat:bg))  :- plane(ypos, Allowed), member(Type, Allowed).

%===============================================================================================%

%=====================[ Transitions with material (Mat) to the right ]==========================%

% Source:

% Vertical =negative (positive)= line with material to the =left= 
inner_transition(ver_pos-(Mat:bg), Type-(bg:Mat))   :- plane(yneg, Allowed), member(Type, Allowed).
inner_transition(ver_neg-(Mat:bg), Type-(bg:Mat))   :- plane(ypos, Allowed), member(Type, Allowed).

% Vertical =negative (positive)= line with material to the =right=
inner_transition(ver_pos-(bg:Mat), Type-(bg:Mat))   :- plane(ypos, Allowed), member(Type, Allowed).
inner_transition(ver_neg-(bg:Mat), Type-(bg:Mat))   :- plane(yneg, Allowed), member(Type, Allowed).

% slope_I (slope_III)= line with material to the =left=
inner_transition(slope_I-(Mat:bg),   Type-(bg:Mat)) :- plane(yneg, Allowed), member(Type, Allowed).
inner_transition(slope_III-(Mat:bg), Type-(bg:Mat)) :- plane(ypos, Allowed), member(Type, Allowed).

% slope_I (slope_III)= line with material to the =right=
inner_transition(slope_I-(bg:Mat),   Type-(bg:Mat)) :- plane(ypos, Allowed), member(Type, Allowed).
inner_transition(slope_III-(bg:Mat), Type-(bg:Mat)) :- plane(yneg, Allowed), member(Type, Allowed).

% slope_II (slope_IV)= line with material to the =left=
inner_transition(slope_II-(Mat:bg), Type-(bg:Mat))  :- plane(yneg, Allowed), member(Type, Allowed).
inner_transition(slope_IV-(Mat:bg), Type-(bg:Mat))  :- plane(ypos, Allowed), member(Type, Allowed).

% slope_II (slope_IV)= line with material to the =right=
inner_transition(slope_II-(bg:Mat), Type-(bg:Mat))  :- plane(ypos, Allowed), member(Type, Allowed).
inner_transition(slope_IV-(bg:Mat), Type-(bg:Mat))  :- plane(yneg, Allowed), member(Type, Allowed).

%===============================================================================================%

%=====================[ Transitions with material (Mat) up or down ]==========================%

% Source:

% Vertical positive line with material to the =left= 
inner_transition(ver_pos-(Mat:bg), hor_neg-(bg:Mat)). 
inner_transition(ver_pos-(Mat:bg), hor_pos-(Mat:bg)). 

% Vertical positive line with material to the =right=
inner_transition(ver_pos-(bg:Mat), hor_pos-(bg:Mat)). 
inner_transition(ver_pos-(bg:Mat), hor_neg-(Mat:bg)). 

% Vertical negative line with material to the =left=
inner_transition(ver_neg-(Mat:bg), hor_neg-(Mat:bg)). 
inner_transition(ver_neg-(Mat:bg), hor_pos-(bg:Mat)). 

% Vertical negative line with material to the =right=
inner_transition(ver_neg-(bg:Mat), hor_pos-(Mat:bg)). 
inner_transition(ver_neg-(bg:Mat), hor_neg-(bg:Mat)). 

% slope_I line with material to the =left=
inner_transition(slope_I-(Mat:bg), hor_pos-(Mat:bg)). 
inner_transition(slope_I-(Mat:bg), hor_neg-(bg:Mat)). 

% slope_I line with material to the =right=
inner_transition(slope_I-(bg:Mat), hor_neg-(Mat:bg)). 
inner_transition(slope_I-(bg:Mat), hor_pos-(bg:Mat)). 



% slope_III line with material to the =left=
inner_transition(slope_III-(Mat:bg), hor_neg-(Mat:bg)). 
inner_transition(slope_III-(Mat:bg), hor_pos-(bg:Mat)). 

% slope_III line with material to the =right=
inner_transition(slope_III-(bg:Mat), hor_pos-(Mat:bg)).
inner_transition(slope_III-(bg:Mat), hor_neg-(bg:Mat)). 



% slope_II line with material to the =left=
inner_transition(slope_II-(Mat:bg), hor_pos-(Mat:bg)). 
inner_transition(slope_II-(Mat:bg), hor_neg-(bg:Mat)). 

% slope_II line with material to the =right=
inner_transition(slope_II-(bg:Mat), hor_neg-(Mat:bg)). 
inner_transition(slope_II-(bg:Mat), hor_pos-(bg:Mat)). 


% slope_IV line with material to the =left=
inner_transition(slope_IV-(Mat:bg), hor_neg-(Mat:bg)). 
inner_transition(slope_IV-(Mat:bg), hor_pos-(bg:Mat)). 

% slope_IV line with material to the =right=
inner_transition(slope_IV-(bg:Mat), hor_pos-(Mat:bg)).
inner_transition(slope_IV-(bg:Mat), hor_neg-(bg:Mat)). 

%===============================================================================================%

%=========================[ Sources with material (Mat) up or down ]============================%

%=====================[ Transitions with material (Mat) to the left ]==========================%

inner_transition(hor_pos-(Mat:bg),  Type-(Mat:bg)) :- plane(ypos, Allowed), member(Type, Allowed).
inner_transition(hor_pos-(bg:Mat),  Type-(Mat:bg)) :- plane(yneg, Allowed), member(Type, Allowed).

inner_transition(hor_neg-(Mat:bg),  Type-(Mat:bg)) :- plane(yneg, Allowed), member(Type, Allowed).
inner_transition(hor_neg-(bg:Mat),  Type-(Mat:bg)) :- plane(ypos, Allowed), member(Type, Allowed).

%=====================[ Transitions with material (Mat) to the rigth ]==========================%

inner_transition(hor_pos-(Mat:bg),  Type-(bg:Mat)) :- plane(yneg, Allowed), member(Type, Allowed).
inner_transition(hor_pos-(bg:Mat),  Type-(bg:Mat)) :- plane(ypos, Allowed), member(Type, Allowed).

inner_transition(hor_neg-(Mat:bg),  Type-(bg:Mat)) :- plane(ypos, Allowed), member(Type, Allowed).
inner_transition(hor_neg-(bg:Mat),  Type-(bg:Mat)) :- plane(yneg, Allowed), member(Type, Allowed).


%==========================[< Detection Algorithm >]===================================

/*
  Check the line below as an example of behavior that might be useful at some point.
 F = [[1,2,3], [4,5,6], [1,2,5]], R = [[1,3],[3,1]],  setof(G, H^(member(H, R), member(G, F), forall(member(X,H), member(X,G))), K).
 Check if there is at least one  holding, i.e., the expression below is equivalent to a
 satisfiability test,<< there is AT LEAST one feature metting the constraints >> The schemata can be:
                                 setof(ANY, CONSTRAINTS, SET). 
*/

% =================[< List-based approach >]=======================
/*
In the list based approach we feed the algorithm with the features
from a closed geometry directly. In the actual implementation the
values come from the movement detection algorithm, this way there is a
focused search, so no need to wandering around with all features in
the world. 

Both approaches do this in this approach, i.e., they do not check 
structural correctness; this way there is no need to re-calculate
the soundness of the countour with respect to its structural
parameters. However in a parallelized version where movement
detection is completely orthogonalized from shape deetction, the
structural correctness would be needed at shape detection. Then to
check which object is moving, the CoG would be used from the moving cloud. 

*/
get_line(A-B, Features) :- member(feature(line,[A,B],_), Features);
                           member(feature(line,[B,A],_), Features).

get_circle(Points, Features) :- member(feature(circle,P,_), Features), match_points(Points,P).

match_points([],_).
match_points([Point|Rest],P) :- member(Point,P), match_points(Rest,P).

detect_object(Object, FeaturesInWorld) :- 
  clause(object(Object),Features),
  detect(Features, FeaturesInWorld).

detect(Feat, Features) :-
     check_features(Feat, Features).

detect((Feat, R), Features) :- 
     check_features(Feat, Features),
     detect(R, Features).

object_module(Object,Matched) :- 
  clause(object(Object),Features),
  detect(Features, Matched,0).

% =================[< Module-based approach >]=======================

get_line_module(A,B) :- features:feature(line,[A, B],_);
                        features:feature(line,[B, A],_).

get_circle_module(Points) :- call(features:feature(circle,P,_)), match_points(Points,P).

detect_module(Feature, Matched, Start) :- 
    (
     Feature =.. [ type | [line(X1:Y1, X2:Y2), Type] ],
     get_line_module(X1:Y1, X2:Y2),
     type(line(X1:Y1, X2:Y2), Type);
     Feature =.. [ circle | [Points]],
     get_circle_module(Points)
    ),
    (Start > 0 -> Matched1 is Matched + 1;
                  Matched1 = 1).

detect_module((Feature, R), Matched, Start) :-
  (
   Feature =.. [ type | [line(X1:Y1, X2:Y2), Type] ],
   get_line_module(X1:Y1, X2:Y2),
   type(line(X1:Y1, X2:Y2), Type);
   Feature =.. [ circle | [Points]],
   get_circle_module(Points)
  ),
  (Start > 0 -> Matched1 is Matched + 1;
                Matched1 = 1),
  detect(R, Matched1, 1).


% ==============================[< Helpers >]=====================================%
assert_features([]).
assert_features([H|R]):-
  assert(features:H),
  assert_features(R).

check_features(Feature, Features) :- 
     Feature =.. [ type | [line(X1:Y1, X2:Y2), Type] ],
     get_line((X1:Y1)-(X2:Y2), Features),
     type(line(X1:Y1, X2:Y2), Type);
     Feature =.. [ circle | [Points]],
     get_circle(Points, Features).

:- assert_features([
                    feature(line,   [0:0,0:1], bg:metal),
                    feature(line,   [5:1,5:0], metal:bg),
                    feature(line,   [2:2,3:2], bg:metal),
                    feature(line,   [2:0,3:0], metal:bg),
                    feature(line,   [1:1,2:2], bg:metal),
                    feature(line,   [3:2,4:1], metal:bg),
                    feature(line,   [0:1,1:1], bg:metal),
                    feature(line,   [0:0,1:0], metal:bg),
                    feature(line,   [4:0,5:0], metal:bg),
                    feature(line,   [4:1,5:1], bg:metal),
                    feature(circle, [1:0,2:0], metal:bg), 
                    feature(circle, [3:0,4:0], metal:bg)
                   ]).

object(car) :-
    type(line(A,B),ver_pos),
    type(line(B,C),hor_pos),
    type(line(C,D),slope_I),
    type(line(D,E),hor_pos),
    type(line(E,F),slope_IV),
    type(line(F,G),hor_pos),
    type(line(G,H),ver_neg),
    type(line(H,I),hor_neg),
    circle([I,J]),
    type(line(J,K),hor_neg),
    circle([K,L]),
    type(line(L,A), hor_neg).


