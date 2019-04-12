% Quang Dang
% qvdang
% $Id: functions.pl,v 1.2 2019-03-10 20:27:38-07 - - $

haversine_distance( Lat1, Lon1, Lat2, Lon2, Dist) :-
    Lon is Lon1 - Lon2,
    Lat is Lat1 - Lat2,
    Temp is sin( Lat / 2 ) ** 2
        + cos( Lat1 ) * cos( Lat2 ) * sin( Lon / 2 ) ** 2,
    Dis is 2 * atan2( sqrt( Temp ), sqrt( 1 - Temp )),
    Dist is Dis * 3961.

degmin_rads( degmin( Deg, Min ), Rad ) :-
    Degrees is Deg + Min / 60,
    Rad is Degrees * pi / 180.  

not( X ) :- X, !, fail.
not( _ ).

distance( PortA, PortB, Dist ) :-
    airport( PortA, _, LatA, LonA ),
    airport( PortB, _, LatB, LonB ),
    degmin_rads( LatA, LatA_in_rad ),
    degmin_rads( LatB, LatB_in_rad ),
    degmin_rads( LonA, LonA_in_rad ),
    degmin_rads( LonB, LonB_in_rad ),
    haversine_distance( LatA_in_rad, LonA_in_rad, 
        LatB_in_rad, LonB_in_rad, Dist ).

hours_float( time( H, M ), Hour_get ) :- 
    Hour_get is H * 60 + M.

flying_time( Dist, Time ) :-
    Time is Dist / 500 * 60.

find_path( Dep, Arr, List ) :-
    find_path( Dep, Arr, [Dep], List ).

find_path( Dep, Dep, _, [Dep] ).
find_path( Dep, Arr, Tried, [[Dep, Depart_time, Arrive_time]
         |List] ) :- 
    flight( Dep, Arr, Dep_time ),
    not( member( Arr, Tried ) ),
    distance( Dep, Arr, Dist ),
    hours_float( Dep_time, Depart_time ),
    flying_time( Dist, Time ),
    Fly_time is round(Time),
    Arrive_time is Depart_time + Fly_time,
    Arrive_time < 1439.0,
    find_path( Arr, Arr, [Arr|Tried], List ).
find_path( Dep, Arr, Tried, [[Dep, Depart_time, Arrive_time]
         |List] ) :-
    flight( Dep, Trans, Dep_time ),
    not( member( Trans, Tried )),
    distance( Dep, Trans, Dist ),
    hours_float( Dep_time, Depart_time ),
    flying_time( Dist, Time ),
    Fly_time is round( Time ),
    Arrive_time is Depart_time + Fly_time,
    Arrive_time < 1439.0,
    flight( Trans, _, Next_Dep_time),
    hours_float( Next_Dep_time, Next_Dep_time_float ),
    At_hub_time is Next_Dep_time_float - Arrive_time,
    At_hub_time1 is At_hub_time - 30,
    At_hub_time1 >= 0,
    find_path( Trans, Arr, [Trans|Tried], List ).
float_hour( Time_fl, H, M ) :-
    H is div( Time_fl, 60 ),
    M is mod( Time_fl, 60 ).
print_time( Time ) :- 
    float_hour( Time, H, M ),
    M >= 10,
    format('~w:~w~n',[H,M] ).
print_time( Time ) :-
    float_hour( Time, H, M ),
    M < 10,
   write( 'M: '),
    write( M ),
    nl,
    Zero is 0,
    format('~w:~w~w~n', [H,Zero,M]).
to_upper( Atom, Str ) :-
    atom_chars( Atom, List_atom ),
    List_atom = [A, B, C],
    lower_upper( A, CharA ),
    lower_upper( B, CharB ),
    lower_upper( C, CharC ),
    atom_concat( CharA, CharB, S ),
    atom_concat( S, CharC, Str ).
writepaths( [] ) :- !.    
writepaths( [[Dep, Depart_time, Arrive_time], Arr|[] ] ) :-
     airport( Dep, Dep_full_name, _, _ ),
     airport( Arr, Arr_full_name, _, _ ),
     to_upper( Dep, Depart ),
     format( 'depart ~w ~a ', [Depart, Dep_full_name] ), 
     flush_output,
     print_time(Depart_time),
     to_upper( Arr, Arrival ),
     format( 'arrive ~w ~a ', [Arrival, Arr_full_name] ),
     print_time( Arrive_time ).
writepaths( [[Dep1, Depart_time1, Arrive_time1],
              [Dep2, Depart_time2, Arrive_time2] |Tail ] ) :-
     airport( Dep1, Dep_full_name1, _, _ ),
     airport( Dep2, Dep_full_name2, _, _ ),
     to_upper( Dep1, Depart1 ),
     format( 'depart ~w ~a ', [Depart1, Dep_full_name1] ), 
     flush_output,
     print_time(Depart_time1),
     to_upper( Dep2, Depart2 ),
     format( 'arrive ~w ~a ', [Depart2, Dep_full_name2] ),
     print_time( Arrive_time1 ),
     writepaths( [[Dep2, Depart_time2, Arrive_time2] | Tail] ).
fly( Dep1, Dep1 ):- 
    write( 'Error: Departure and Arrival are the same' ),
    nl,
    !, fail.
fly( Dep, Arr ):-
    airport( Dep, _, _, _ ),
    airport( Arr, _, _, _ ),
    find_path( Dep, Arr, List ),
    writepaths( List ).

fly( Dep, Arr ) :-
    airport( Dep, Dep_name,_,_),
    airport( Arr, Arr_name,_,_),
    to_upper( Dep, Depart ),
    to_upper( Arr, Arrive ),
    format( 'There is no flight from ~w ~a to ~w ~a', 
            [Depart, Dep_name, Arrive, Arr_name] ),
    !, fail.
fly( _, _ ) :-
    write( 'There is no information about the flight.' ),
    nl, !, fail.
