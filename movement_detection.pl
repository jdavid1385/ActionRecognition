:- module(testing, [clustered_features/1, moving_points/1, scc/1, intro_test/5, remove_all_points/0, restart_detection/0]).
:- use_module(contour, [intro_test/5]).
:- use_module(geom, [centroid/2]).
:- use_module(library(chr)).

:- chr_constraint point/2, points/1, moving/1, remove_all/0, restart/0.
:- dynamic point/1, clustering/1, cl/1, scc/1.

% We do not need to worry about duplicate points
% (joints); the dup_rem rule removes such duplicates.

% This part can be run in parallel, however since to detect moving
% points we need to check all points which remained in t2, this task
% must be executed until the end before performing the check.
 
unfold   @ points([Coord-T|Rest]) <=> point(Coord,T), points(Rest).

dup_rem  @ points([]), points([]) , point(CoordA, TA) \ 
                                    point(CoordB, TB)  <=> 
                                                   CoordA = CoordB, 
                                                   TA = TB | true.

still    @ points([]), points([]) \ 
           point(CoordA, t1), point(CoordB, t2) <=>  
                                            CoordA = CoordB | true.

fold_mov @ point(Coord, t2), moving(R) <=> moving([Coord|R]).

rem_all  @ remove_all \ point(_,_) <=> true.

restart  @ moving(_), remove_all, restart, points([]), points([]) <=> moving([]).

% The size of the region is the number of clustered elements
% centroid @ c(Centroids), region_point((Xi:Yi)/Region) <=> select(Region/(X:Y), Centroids, R),
%                                                          size(Region, N), Xp is X + Xi/N, Yp is Y + Yi/N, c([Region-(Xp:Yp)|R]).

clustered_features(CC) :- moving_points(Coords), cluster_regions(Clusters,Coords),
                          bagof(Cluster:PolyPoints-Centroid, (member(Cluster:PolyPoints, Clusters),
                                                              leftmost_X(X:Y, Cluster),
                                                              centroid(PolyPoints-(X:Y), Centroid)) , CC).

% This version is preferable for getting all chr_constraints with the
% point(C,t2) structure. All points at t2 which were not consumed
% are the moving points. NOTE: This is wrong, the side effect will
% only take into account a single member meeting the criteria, not all
% of the members having COORD in their points will be stored. It is
% left only for references.

% moving_points_WRONG :- forall(find_chr_constraint(point(Coord,t2)), store_moving(Coord)).

% store_moving_WRONG(Coord) :- features:features(Features,t2),
%                              (member(feature(L,[Coord,Coord2],Textures),Features),
%                              movement:assert(feature(L, [Coord,Coord2],Textures));
%                              member(feature(L,[Coord2,Coord],Textures),Features),
%                              movement:assert(feature(L,
%                              [Coord2,Coord],Textures))).

moving_points(Coords) :- expose_all_points -> % After CHR still
                                              % rule is applied ==>
                                                              remaining_points(Coords).

% Here we expose the whole set of points for the moving window [t1,t2]. 

expose_all_points :- aggregate(bag(Features/T), features:features(Features,T), Batch), expose_all_points(Batch).
expose_all_points([]).
expose_all_points([Features/T|Rest]) :- expose_points(Features,T), expose_all_points(Rest).

% Here is performed the actual exposure. The rule unfold, performs the unfolding of all points to be later passed through the rest of constraints.
expose_points(Features, T) :- bagof(Point-T, point_in_features(Point-T, Features), All_Points), points(All_Points).
% Here we use a trick to attach the time to each point for exposure
point_in_features(Point-_, Features) :- member(feature(_,Points,_),Features), member(Point, Points).

remaining_points(Coords) :- moving([]), % Here the control is given to
                                        % CHR. The CHR should have a flag
                                        % that shows the actual state of
                                        % the data store (active/
                                        % inactive) appart from the
                                        % "active constraints"; it
                                        % should become inactive only
                                        % when it has searched all
                                        % contraints and none gets
                                        % active. This more or less is
                                        % the case with the actual
                                        % implementation, however
                                        % the flag is not actually
                                        % implemented. If parallel
                                        % behavior is desired repeat,
                                        % \+find_chr_constraint(point(_,t2)), 
                                        % moving(Coords) is an option. 
                            repeat,
                                \+find_chr_constraint(point(_,t2)),
                                  find_chr_constraint(moving(Coords)).

remove_all_points :- remove_all,
                     repeat,
                      \+ find_chr_constraint(point(_,_)).

restart_detection :- restart.
% cluster_regions, moving_centroids.

cluster_regions(Clusters, Coordinates) :- % take all points left after the still rule is
                                          % applied and cluster them by structural features.
    notrace(moving_features(Coordinates, Features)),
    cluster(Features,Clusters).

cluster([], []).
cluster(Features, [Cluster:Points|RClusters]) :-
    leftmost_X(X, Features),
    notrace(intro_test(X, Features, Points, Cluster, RFeatures)), reverse(Points, [X|_]), 
    cluster(RFeatures, RClusters).

moving_features(Coords, Features) :- setof(Feat, feature_at_t2(Coords, Feat), Features).

feature_at_t2(Coords, Feat) :- member(Coord, Coords),
                               bagof(F, coord_in_feature(Coord,t2,F), Feats_with_Coord),
                               member(Feat, Feats_with_Coord).

coord_in_feature(Coord,  T, feature(L,Points,Textures)) :- features:features(Features, T),
                                                           member(feature(L,Points,Textures), Features),
                                                           member(Coord, Points).

% It must be discriminated by Ys for all to be examined, try removing
% Ys using aggregate(min(Xs), point_in_feature(Xs:_), X)
leftmost_X(X:Y, Features) :- notrace(aggregate(min(Xs), Ys, point_in_feature(Xs:Ys, Features), X)), point_in_feature(X:Y, Features).
% This can be used for searching with branching. For all features
% containing points all its point members will be searched.

point_in_feature(Point,Features) :- member(feature(_,Points,_), Features), member(Point, Points).

find_moving_points([]).

:- assert(clusterin([b:c, a:b, c:d])).
testoid(X:Y, Points, [X:Y|R]) :- member(X:Y, Points), member(Y:Z, Points), select(X:Y, Points, Other), testoid(Y:Z, Other, R).
testoid(X:Y, _, [X:Y]).

:- assert(cl([f(a,[a:b, c:d],m1:m2),
              f(b,[c:d, e:f, i:j],m1:m2),
              f(a,[e:f, w:y],m1:m2),
              f(a,[e:f, g:h],m1:m2),
              f(b,[i:j, k:l, m:n, o:p],m1:m2),
              f(a,[o:p, q:r],m1:m2),
              f(a,[q:r, s:t],m1:m2),
              f(a,[s:t, u:v],m1:m2),
              f(a,[u:v, m:n],m1:m2),
              f(a,[a:b, o:p],m1:m2)
             ])).

:- assert(scl([f(a,[0:0, 0:1],m1:m2), 
              f(b,[0:1, 0:2, 1:1.5],m1:m2),
              f(a,[0:2, 1:3],m1:m2),
              f(a,[0:2, 0:3],m1:m2),
              f(b,[1:1.5, 2:1.5, 2:2, 2:3],m1:m2),
              f(a,[2:3, 2:4],m1:m2),
              f(a,[2:4, 3:3],m1:m2),
              f(a,[3:3, 3:2],m1:m2),
              f(a,[3:2, 2:2],m1:m2),
              f(a,[0:0, 2:3],m1:m2)
             ])).

:- assert(scc([
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
         ])).


test(Xi:Yi, Feats, [Xi:Yi|R]) :-  select(f(_,Points, _), Feats,  LFeats),   % Take any feature
                                  select(f(_,Points2,_), LFeats, RFeats),   % Take a second feature

                                  select(Xi:Yi, Points, RPoints),           % Is it possible to take Xi:Yi from the points in first feature?
                                  member(Xj:Yj, RPoints),                   % pick a second point Xj:Yj (It is generic for lines and circles)
                              
                                  select(Xj:Yj, Points2, RPoints2),         % Is it possible to take Xj:Yj from the points in second feature?

                                                                            % Connected section up to Xj:Yj
                                  (
                                   test(Xj:Yj, LFeats, R);                  % Is it possible having more connected features starting from Xj:Yj?
                                   
                                   member(XFinal:YFinal, RPoints2),         % This is the final point, pick XFinal:YFinal from the second feature
                                   forall(member(f(_,DPoints,_), RFeats),   % Is it really true that there are no more possible connections?
                                          \+member(XFinal:YFinal, DPoints)),% It must be checked to stop backtracking from taking into account
                                                                            % subsets of already found solutions.
                                   R = [Xj:Yj, XFinal:YFinal]               % Percolate the value of this final feature
                                  ).


