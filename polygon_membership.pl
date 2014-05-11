:- module(polygon,[inside_poly/3]).
:- dynamic count/1, inside/1.
:- [library(aggregate)].

count(0).
inc :- retract(count(X)), X1 is X+1, assert(count(X1)). 
loop :- repeat, inc, test.
test :- count(X), X == 10, !.
test :- fail.
               
% Based on the simulation of simplicity algorithm in
% http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

exor(A,B):- \+A,B;A,\+B.
in_range(Coordinate,CA,CB) :- exor((CA>Coordinate),(CB>Coordinate)).


inside(false).
reset_ray :- retract(inside(_)), assert(inside(false)).
toggle_ray :- retract(inside(X)), call(X), assert(inside(false)); assert(inside(true)).

inside(_,[_|[]]).
inside(X:Y, [X1:Y1,X2:Y2|R]) :- in_range(Y,Y1,Y2), X > ( ((X2-X1)*(Y-Y1))/(Y2-Y1) + X1),toggle_ray, inside(X:Y, [X2:Y2|R]); inside(X:Y, [X2:Y2|R]).

get_line(_,_,[]).
get_line([XA:YA,XB:YB],[X1:Y1,X2:Y2|R]):- [XA:YA,XB:YB]=[X1:Y1,X2:Y2]; get_line([XA:YA,XB:YB],[X2:Y2|R]).

% The equation of a line given 2 points A and B (Line(A,B)) is: 
%
%                        (YB-YA)
%               Y - YA = ------- * (X - XA) 
%                        (XB-YB) 
% 
% It is important that the direction of rotation for the line is
% setted to clock-wise for boundaries and anti-clock-wise for holes.
% We are going to check whether the point (X,Y), i.e the tested point is at the left
% half-plane of our line (it is a matter of taste, it could also be
% the right side, but also the direction of lines has to be changed in
% that case), this is to project the ray from the point to the right (or left)
% and acknowledge the intersection with the line. We have chosen to project
% the ray in the horizontal direction (again it is a matter of taste,
% it could also be done in vertical with similar restrictions), so we have:
%  
%                   (XB-XA)
%               X < ------- * (Y - YA) + XA
%                   (YB-YA) 
%
% Now we need to know if the point is at the left (or right) side of
% the line segment only, not the entire plane, so we need to 
% restrict the search only to this segment, but this is easy since
% to be inside the segment only one point in the line can be higher
% than Y in the vertical axis. As this is a stronger restriction it
% needs to be the first to check, so we take first only those lines
% meeting this requirement and then check its possition. By the Jordan
% Curve theorem any ray projected to a polygon must intersect at an
% even number of lines. So we are done, we will throw the ray to the
% right and then everytime it intersects a line, toggle its state.
% However in our implementation we are goint to check the lenght of
% the bag of solutions meeting the given restrictions and decide the
% innership upon it.

% for each line in the polygon this have to be done so.
is_left_half_plane(_,[],[],_).
is_left_half_plane(X:Y,[XA:YA,XB:YB], [[X1:Y1,X2:Y2]|R], Test) :- [XA:YA, XB:YB] = [X1:Y1, X2:Y2], call(Test, X , (((XB - XA) * (Y - YA)) / (YB - YA) + XA)); 
                                                            is_left_half_plane(X:Y, [XA:YA, XB:YB], R, Test).

in_y_range_at_poly(Y,[XA:YA,XB:YB],Polygon) :- get_line([XA:YA,XB:YB],Polygon), in_range(Y,YA,YB).
all_in_range(Coordinate,Polygon,Lines) :- aggregate(bag(Line), in_y_range_at_poly(Coordinate,Line,Polygon), Lines).

traverses_ray(X:Y, Lines, Count) :- aggregate(bag(Line), is_left_half_plane(X:Y, Line, Lines, <), IntersectingLines), length(IntersectingLines, Count).

%We are ready to introduce it.
inside_poly(X:Y,Polygon,Answer) :- all_in_range(Y,Polygon,Lines), traverses_ray(X:Y, Lines, Count), (1 is mod(Count,2)->Answer=inside;Answer=outside).


