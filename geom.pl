:- module(geom, [centroid/2]).

leftmost_X(X:Y, Tuples) :- notrace(aggregate(min(Xs), Ys, point_in_tuples(Xs:Ys, Tuples), X)), point_in_tuples(X:Y, Tuples).
point_in_tuples(Point,Tuples) :- member(Points, Tuples), member(Point, Points).

% Calculate polygon area and x/y - weighted areas.
% Formula :
%                 T_i = \frac{x_i y_{i+1} - x_{i+1} y_i}{2}
%                 C_x = \sum_{i=0}^{n-1}(x_i+x_{i+1})T_i 
%                 C_y = \sum_{i=0}^{n-1}(y_i+y_{i+1})T_i
%                 A = \sum_{i=0}^{n-1} T_i

% In the first term (T-(X:Y)), X:Y is the shift from 0 of the polygon 
area([_:_]-_, 0:0, 0).
area([X1:Y1,X2:Y2|XYs]-(X:Y), WArea_x:WArea_y, Area) :- 
     	area([X2:Y2|XYs]-(X:Y), WArea1_x:WArea1_y, Area1), 
     	PArea is (X1-X)*(Y2-Y)-(Y1-Y)*(X2-X),
        Area is PArea + Area1,
        WArea_x is PArea * (X1+X2-2*X) + WArea1_x,
        WArea_y is PArea * (Y1+Y2-2*Y) + WArea1_y.

% This argument must unify Poly with the 3-th Output argument from intro_test.

centroid(Poly-(Xs:Ys), X:Y) :- area(Poly-(Xs:Ys), WArea_x:WArea_y, PArea), Area is PArea/2, X is WArea_x/(6*Area)+Xs, Y is WArea_y/(6*Area)+Ys.

% This predicate sorts the features points to its clock-wise order.
% The area will have negative sign in this case. But the entire
% expression for centroid will hold its appropiate value.

polygon_points( _, [], []).
polygon_points( X0:Y0, Tuples, [X0:Y0|R] ) :- (
                                               select([X0:Y0, X1:Y1],  Tuples, RTuples);
                                               select([X1:Y1, X0:Y0],  Tuples, RTuples)
                                               ),
                                               polygon_points( X1:Y1, RTuples, R).

% The Tuples below correspond to feature's points of a car polygon
test_tuples( [ [0:0,0:1],  [5:1,5:0],  [2:2,3:2], [2:0,3:0],  [1:1,2:2],  [3:2,4:1], 
               [0:1,1:1],  [0:0,1:0],  [4:0,5:0], [4:1,5:1],  [1:0,2:0],  [3:0,4:0] ]).

test_tuples_2([[10:0,10:1],
               [15:1,15:0],
               [12:2,13:2],
               [12:0,13:0],
               [11:1,12:2],
               [13:2,14:1],
               [10:1,11:1],
               [10:0,11:0],
               [14:0,15:0],
               [14:1,15:1],
               [11:0,12:0],
               [13:0,14:0]]).

test-conversion(O-(X:Y)) :- test_tuples(T), leftmost_X(X:Y,T), point_in_tuples(Init,T),  polygon_points(Init, T, O).
test-centroid(C) :- test-conversion(Poly-Shift), centroid(Poly-Shift, C).

