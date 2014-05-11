% Look at the test at the end of this file

:- use_module(betaConversion, [betaConvert/2]).
:- use_module(comsemLib, [printRepresentations/1,compose/3]).
:- use_module(signature, [resetVars/0,vars2atoms/1]).
:- use_module(grammar, [s/3]).
:- use_module(movement_detection, [clustered_features/1, restart_detection/0, remove_all_points/0]).
:- use_module(contour, [detect_object/2]).
:- use_module(polygon_membership, [inside_poly/3]).
:- use_module(library(chr)).

:- dynamic count/1.
:- chr_constraint cog/1, point/1, entity/3, action/3, movin/2, actions/1, pp_link/4, frame/3.

:- op(950,yfx,@).       % application
:- op(800,xfy,&).       % application

% Features are asserted in the database in the format:
                                %
                                %                            |--> line  ==>
                                %                            | Surrounding
                                %                            | Texture:
                                %                            |     {
                                %                            |     above:below,
                                %                            |     left:right
                                %                            |     }
% features(+Type, +PointSet, +SurroundindTextures),  Types ->o
                                %                            |
                                %                            |
                                %                            |--> circle ==>
                                %                              Surrounding
                                %                              Texture:
                                %                                  {
                                %                                   inner:outer
                                %                                  }


% The clause that solves the moving features is as follows:

% cluster_regions/0 is a clustering predicate, it initializes the
% regions of similar features for which movement is detected.
% After clustering is done, detect the centroids to finally detect
% the moving entity --> action(lambda(E,move(E)), Centroid, T)

% The moving point's region is detected by the ray algorithm making use of the
% structural aspect of the features (texture)

% This rule works for rigid and chained. The information is handled by
% the acting_entity/3 rule which also returns the updated information
frame( Entity, info(type:entity & InfoEntity), T ) \
frame( Action, info(type:action & InfoAction), T ) <=>
                      acting_entity(InfoEntity, InfoAction, UpdatedInfo) |
                      frame(Entity@Action, info(type:action & UpdatedInfo), T).
  
% Leave only the last movement events, if it did not compose with
% anything before then do not continue.

%pp_link( Prep, move(X), E, T1) \ pp_link( Prep, move(X), E, T2) <=> T2 < T1 | true.

frame( ActingEntityA, info(type:action & InfoA), T2),
frame( ActingEntityB, info(type:action & InfoB), T1) <=>
                      1 is T2 - T1,
                      entity_direction(InfoA, InfoB, UpdatedInfo) |
                      ActingEntityA = ActingEntityB,
                      frame( ActingEntityA, info(type:action & UpdatedInfo), T2).

frame( lambda(X, X@E), info(type:entity & InfoA), T2) \
frame( lambda(Y, Y@E), info(type:entity & InfoB), T1) <=>
                                             T2 >= T1, removeable(InfoA, InfoB) | X = Y.

% If an entity didn't react in past ocassions it won't do anymore so
% it is safe to remove it.
frame( _@LambdaPredA, info(type:entity & InfoA), T2) \
frame( _@LambdaPredB, info(type:entity & InfoB), T1) <=>
                                             T2 >= T1, removeable(InfoA, InfoB) |  LambdaPredA=LambdaPredB.

frame( _@LambdaMoveA, info(type:action & InfoA), T2) \
frame( _@LambdaMoveB, info(type:action & InfoB), T1) <=>
                                             T2 >= T1, removeable(InfoA, InfoB) |  LambdaMoveA=LambdaMoveB.

frame( AgentA@_, info(type:prepositional), T2) \
frame( AgentB@_, info(type:prepositional), T1) <=>
                 T2 > T1, Delta is T2 - T1, Delta > 9 |  AgentA=AgentB.

removeable(InfoA,InfoB) :-
    InfoA = (parts:_ & struct:chained),
    InfoB = (parts:_ & struct:chained);

    InfoA = (points:_ & struct:rigid),
    InfoB = (points:_ & struct:rigid);

    InfoA = (centroid:_ & direction:_),
    InfoB = (centroid:_ & direction:_).

test-removal_lambda:- frame(lambda(R,R@man)@lambda(Y,move(Y)),info(type:action & centroid:(0:0)  & direction:_),0),
                 frame(lambda(P,P@man)@lambda(X,move(X)),info(type:action & centroid:(10:0) & direction:_),1),
                 frame( lambda(P, exists(X, car(X) & (P@X) )), info(type:entity & points:_ & struct:rigid),   1),
                 frame( lambda(Q, exists(Y, car(X) & (Q@Y) )), info(type:entity & points:_ & struct:rigid),   2),
                 frame(lambda(A,exists(B,man(B)& (A@B) )),     info(type:entity & parts:_  & struct:chained), 0),
                 frame(lambda(C,exists(D,man(D)& (C@D) )),     info(type:entity & parts:_  & struct:chained), 1).

frame( EntityA, info(type:entity & InfoA), T) \
frame( ActEntityB, info(type:action & InfoB), T) <=>
                            linkeable(EntityA-InfoA, ActEntityB-InfoB, Phrase) |                             
                            frame( Phrase, info(type:prepositional), T).

prep(towards, lambda(L,lambda(M,lambda(J, L@lambda(G,towards(M@J,G) ))))).
prep(away, lambda(L,lambda(M,lambda(J, L@lambda(G,away(M@J,G) ))))).



/* ===========================================[ ALL HELPERS HERE ]=================================================*/

acting_entity(InfoA, InfoB, UpdatedInfo) :-
    InfoA = (points:P & struct:rigid),
    InfoB = centroid:C,
    inside_poly(C, P, inside),
    UpdatedInfo = (centroid:C & direction:_);

    InfoA = (parts:Parts & struct:chained),
    InfoB = moving:MovPoints,
    is_part(MovPoints, Parts, MovingParts),
    UpdatedInfo = (moving:MovingParts & direction:_).

entity_direction(InfoA, InfoB, UpdatedInfo) :-
    InfoA = (centroid:C_2 & direction:_),
    InfoB = (centroid:C_1 & direction:_),
    direction(Dir, C_2, C_1),
    UpdatedInfo = (centroid:C_2 & direction:Dir);    
    
    InfoA = (moving:MovingPartsA & direction:_),
    InfoB = (moving:MovingPartsB & direction:_),
    member(Part-PointsA, MovingPartsA), member(A, PointsA),
    member(Part-PointsB, MovingPartsB), member(B, PointsB),
    direction(Dir, A, B),
    UpdatedInfo = (moving:MovingPartsA & direction:Dir).  

linkeable(
          EntityA-InfoA, 
          (EntityB@ActionB)-InfoB,
          Phrase
         ) :-
             info_schema(InfoA, InfoB, Schema),
             Agent = EntityB,
             Preposition = Schema,
             Object = EntityA,
             Verb = ActionB,
             Phrase = (Agent@((Preposition@Object)@Verb)).


is_part(MovingPoints, Parts, MovingParts):- 
    % Give me a bag of pairs Part-MP (moving points), such that for every
    % Part in a chained object, the points of this specific Part are
    % also Moving Points
    bafof(Part-MP,
          (
           member(Part:Points, Parts),
           bagof(M, ( member(M, MovingPoints), member(M, Points) ), MP)
          ), 
          MovingParts).

% Depending on the representation of InfoA and InfoB
info_schema( InfoA, InfoB, Schema) :-
         (
          InfoA = (points:Points & struct:rigid), 
          InfoB = (centroid:B & direction:DirMove),
          notrace(inside_poly(B, Points, outside));

          InfoA = (parts:Parts & struct:chained), 
          InfoB = (centroid:B & direction:DirMove),
          member(_:PointsA, Parts), member(A, PointsA);

          InfoA = (points:Points & struct:rigid),
          InfoB = (moving:MovingParts & direction:DirMove),
          \+in_moving_parts(Parts, MovingParts),
          member(A, Points),
          member(_-PointsB, MovingParts), member(B, PointsB);

          InfoA = (parts:Parts & struct:chained), 
          InfoB = (moving:MovingParts & direction:DirMove),
          \+in_moving_parts(Parts, MovingParts),
          member(_:PointsA, Parts), member(A, PointsA),
          member(_-PointsB, MovingParts), member(B, PointsB)
         ),

         rel_position(RelPos, B, A),
         rel_mov_direction(Prep, DirMove & RelPos),
         prep(Prep, Schema).

in_moving_parts(Parts, MovingParts) :-
            member(Part-Points, MovingParts),
            member(Part:PPoints, Parts),
            \+intersection(Points, PPoints, []).

% Let us keep for instance only 5 time points.
%action( move(E1), C1, T1 ), action( move(E2), C, T1 ),
%action( move(E1), C2, T2 ), action( move(E2), C, T2 ),
%action( move(E1), C3, T3 ), action( move(E2), C, T3 ),
%action( move(E1), C4, T3 ), action( move(E2), C, T4 ) <=>
                              
%action( move(E1), C1, T1 ), action( move(E2), C, T1 )

direction(right, Ax:Ay, Bx:By) :- Ax > Bx, Ay = By.
direction(left,  Ax:Ay, Bx:By) :- Ax < Bx, Ay = By.

direction(up, Ax:Ay, Bx:By) :- Ax = Bx, Ay > By.
direction(down,  Ax:Ay, Bx:By) :- Ax = Bx, Ay < By.

%Potentially used
direction(diagonal-up-right, Ax:Ay, Bx:By) :- Ax > Bx, Ay > By.
direction(diagonal-up-left,  Ax:Ay, Bx:By) :- Ax < Bx, Ay > By.

direction(diagonal-down-right, Ax:Ay, Bx:By) :- Ax > Bx, Ay < By.
direction(diagonal-down-left,  Ax:Ay, Bx:By) :- Ax < Bx, Ay < By.

rel_position( right, Ax:_, Bx:_) :- Ax > Bx.
rel_position( left,  Ax:_, Bx:_) :- Ax < Bx.

/*
 Relative movement of B to A.

     move right  |
     pos left    |
    *-->    *    | towards
    B          A |
=========================
     move right  |
     pos left    |
 <--*       *    | away
    B       A    | 
*/

% Opposite vector of movement and relative position then approaching.
% rel_move_direction(Preposition, MovDir & RelPos)

rel_mov_direction(towards, left & right).
rel_mov_direction(towards, right & left).

% Vector of movement and side equal then running away.
rel_mov_direction(away, left & left).
rel_mov_direction(away, right & right).


% Basically what we have is a set of clusters detected by the cluster
% algorithm. Such algorithm only clusters the closed geometries;  now
% it is neccesary to  re-check the left points for open chains as for
% instance a men.


% All the entities found here are of the type rigid.
expand_clusters([], _).
expand_clusters([Cluster:Points-Centroid | RClusters], T) :-
         frame( lambda(X, move(X)), info(type:action & centroid:Centroid), T),
         notrace(detect_object(I,Cluster)), Pred =.. [I|[Y]],
         frame( lambda(P, lambda(Q, exists(Z, (P@Z) & (Q@Z) )))@lambda(Y, Pred), info(type:entity & points:Points & struct:rigid), T),
         expand_clusters(RClusters, T).


% Sampler
update_feats :- features:retract(features(F1,t2)),
                features:retract(features(_,t1)), new_batch(F2), features:assert(features(F1,t1)),
                features:assert(features(F2,t2)).


/* ===========================================[ TEST HERE ]============================================================*/

% Test features.This should be imported from the test-generator output files directly, but let us keep it explicit now for in-place changes.

:- features:assert(features([
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
         ],t1)),  features:assert(features([
            feature(line,   [10:0,10:1], bg:metal),
            feature(line,   [15:1,15:0], metal:bg),
            feature(line,   [12:2,13:2], bg:metal),
            feature(line,   [12:0,13:0], metal:bg),
            feature(line,   [11:1,12:2], bg:metal),
            feature(line,   [13:2,14:1], metal:bg),
            feature(line,   [10:1,11:1], bg:metal),
            feature(line,   [10:0,11:0], metal:bg),
            feature(line,   [14:0,15:0], metal:bg),
            feature(line,   [14:1,15:1], bg:metal),
            feature(circle, [11:0,12:0], metal:bg), 
            feature(circle, [13:0,14:0], metal:bg)
         ],t2)).

new_batch([
            feature(line,   [20:0,20:1], bg:metal),
            feature(line,   [25:1,25:0], metal:bg),
            feature(line,   [22:2,23:2], bg:metal),
            feature(line,   [22:0,23:0], metal:bg),
            feature(line,   [21:1,22:2], bg:metal),
            feature(line,   [23:2,24:1], metal:bg),
            feature(line,   [20:1,21:1], bg:metal),
            feature(line,   [20:0,21:0], metal:bg),
            feature(line,   [24:0,25:0], metal:bg),
            feature(line,   [24:1,25:1], bg:metal),
            feature(circle, [21:0,22:0], metal:bg), 
            feature(circle, [23:0,24:0], metal:bg)
          ]).


object_old_repr(car) :-
  vertical(A,B),
  horizontal(B,C),
  pos_slope(C,D),
  horizontal(D,E),
  neg_slope(E,F),
  horizontal(F,G),
  vertical(G,H),
  horizontal(H,I),
  circle(I,J),
  horizontal(J,K),
  circle(K,L),
  horizontal(L,A).

% For object detection we use only the geometric aspect of the
% features.

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
                     
line_types([slope_I,
            slope_II,
            slope_III,
            slope_IV,
            hor_pos,
            hor_neg,
            ver_pos,
            ver_neg,
            chained]).

type(line(X1:Y1, X2:Y2), hor_pos) :- Y1 == Y2, X1 < X2.
type(line(X1:Y1, X2:Y2), hor_neg) :- Y1 == Y2, X1 > X2.
type(line(X1:Y1, X2:Y2), ver_pos) :- X1 == X2, Y1 < Y2.
type(line(X1:Y1, X2:Y2), ver_neg) :- X1 == X2, Y1 > Y2.

type(line(X1:Y1, X2:Y2), slope_I)   :- X2 > X1, Y2 > Y1.
type(line(X1:Y1, X2:Y2), slope_II)  :- X2 < X1, Y2 > Y1.
type(line(X1:Y1, X2:Y2), slope_III) :- X2 < X1, Y2 < Y1.
type(line(X1:Y1, X2:Y2), slope_IV)  :- X2 > X1, Y2 < Y1.

assert_features([]).
assert_features([H|R]):-
  assert(features:H),
  assert_features(R).

:- assert_features([
                feature(line,   [0:0,0:1], metal:wood),
                feature(line,   [5:1,5:0], metal:sky),
                feature(line,   [2:2,3:2], metal:concrete),
                feature(line,   [2:0,3:0], metal:concrete),
                feature(line,   [1:1,2:2], metal:sky),
                feature(line,   [3:2,4:1], metal:grass),
                feature(line,   [0:1,1:1], metal:grass),
                feature(line,   [0:0,1:0], metal:concrete),
                feature(line,   [4:0,5:0], metal:concrete),
                feature(line,   [4:1,5:1], metal:concrete),
                feature(circle, [1:0,2:0], rubber:concrete),
                feature(circle, [3:0,4:0], rubber:concrete)
                ]).



% Intro  to the test
intro :-
         notrace(clustered_features(Clusters)),
         expand_clusters(Clusters, 0),
         %frame(lambda(P,P@man), info(type:entity & points:[30:0]), 0),
         frame( lambda(P, lambda(Q, exists(X, (P@X) & (Q@X) )))@lambda(Y,man(Y)) , info(type:entity & 
                                                        parts:[
                                                               head-neck:[30:2, 30:1.8], 
                                                               left-leg: [29.5:0, 29.5:0.5, 30:1], 
                                                               right-leg:[31.5:0, 31.5:0.5, 30:1],                                         
                                                               left-arm: [27.5:1, 28.5:1.5, 30:1.8], 
                                                               right-arm:[30.5:0, 30.1:1.5, 30:1.8],
                                                               trunk:[30:1, 30:1.8]
                                                              ] & struct:chained), 0),
         test(1),
         find_chr_constraint(frame(Phrase, info(type:prepositional), T)),
         s(Phrase, Output, [at, T]), writeln(Output).

test(3). 
test(T) :-
          remove_all_points,
          restart_detection, 
          notrace(update_feats),
          notrace(clustered_features(Clusters)),
          notrace(expand_clusters(Clusters, T)),
          frame( lambda(P, lambda(Q, exists(X, (P@X) & (Q@X) )))@lambda(Y,man(Y)), info(type:entity & 
                                                        parts:[
                                                               head-neck:[30:2, 30:1.8], 
                                                               left-leg: [29.5:0, 29.5:0.5, 30:1], 
                                                               right-leg:[31.5:0, 31.5:0.5, 30:1],                                         
                                                               left-arm: [27.5:1, 28.5:1.5, 30:1.8], 
                                                               right-arm:[30.5:0, 30.1:1.5, 30:1.8],
                                                               trunk:[30:1, 30:1.8]
                                                              ] & struct:chained), T),

          T1 is T+1,
          test(T1).

