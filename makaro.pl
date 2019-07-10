:- use_module(library(clpfd)).
?- set_prolog_flag(answer_write_options,[max_depth(0)]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Main 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
makaro(Rows) :- %writeln('ROWS'), 
				check_adj(Rows),
				writeln('Problem: '), maplist(portray_clause, Rows), writeln(''),
				transpose(Rows, Columns), !, 
				%write('Columns = '), writeln(Columns), writeln('COLS'), 
				check_adj(Columns),
				check_regions(Rows),
				append(Rows, Vs),
				%write('Vs = '), writeln(Vs),
				get_values(Vs, Values),
				maplist(label, [Values]),
				%write('Values = '), writeln(Values), writeln(''),
				check_arrows(Rows),
				writeln('Solution: '), maplist(portray_clause, Rows), writeln('').

get_values(Rows, Values) :- get_values_aux(Rows, [], Values).

get_values_aux([], Values, Values) :- !.
get_values_aux([(_, V)|T] ,Acc, Values) :- 	%write('V = '), writeln(V),
											(
												(integer(V); var(V)) ->
													N #= V;
													N #= 0
											),
											%write('N = '), writeln(N),
											append(Acc, [N], New_Acc),
											%write('New_Acc = '), writeln(New_Acc),
											get_values_aux(T, New_Acc, Values).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Verify adjacent numbers
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
check_adj([]) :- !.
check_adj([Row|T]) :- %write('Row = '), writeln(Row), 
						check_adj_row(Row),
						check_adj(T).

check_adj_row([]) :- !.
check_adj_row([_|[]]) :- !.
check_adj_row([(_, N1)|[(R, N2)|T]]) :- adj_ok(N1, N2),
										check_adj_row([(R, N2)|T]).

adj_ok(N, _) :- not(integer(N)), not(var(N)), !.
adj_ok(_, N) :- not(integer(N)), not(var(N)), !.
adj_ok(N1, N2) :- %write('#\\= '), writeln((N1, N2)),
					N1 #\= N2.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Verify regions
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
check_regions(Rows) :- append(Rows, Vs),
						msort(Vs, Ordered), !,
						%write('Ordered = '), writeln(Ordered),
						build_regions(Ordered, Regions), !,
						%write('Regions = '), writeln(Regions),
						maplist(region_ok, Regions).

build_regions(Ordered, Regions) :- build_regions_aux(Ordered, (#, []), [], Regions).

build_regions_aux([], (_, NList), Acc, Regions) :- append(Acc, [NList], Regions), !.
build_regions_aux([(#, _)|T], Current_Region, Acc, Regions) :- %write('T = '), writeln(T), 
																build_regions_aux(T, Current_Region, Acc, Regions).
build_regions_aux([(R, N)|T], (R, NList), Acc, Regions) :-  
															append(NList, [N], New_NList),
															%write('(R, N, NList, New_NList) = '), writeln((R, N, NList, New_NList)),
															build_regions_aux(T, (R, New_NList), Acc, Regions).
build_regions_aux([(R2, N)|T], (R1, NList), Acc, Regions) :- R1 \= R2,
															(	R1 \= # ->
																append(Acc, [NList], New_Acc);
																New_Acc = Acc
															),
															%write('(R1, R2, N, NList, New_Acc) = '), writeln((R1, R2, N, NList, New_Acc)), 
															build_regions_aux(T, (R2, [N]), New_Acc, Regions).
													
region_ok(Region) :- all_distinct(Region),
					length(Region, L), Region ins 1..L.
					
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Verify arrows
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
check_arrows(Rows) :-  findall(((Row, Col), A), at(Rows, Row, Col, (#,A)), Arrows),
							arrows_ok(Arrows, Rows).

arrows_ok([], _) :- !.					
arrows_ok([Arrow|T], Rows) :- arrow_ok(Arrow, Rows),
								arrows_ok(T, Rows).

% Empty square (no arrow)								
arrow_ok(((_, _), #), _).

% Up
arrow_ok(((R, C), u), Rows) :- Ru #= R - 1,
								at(Rows, Ru, C, (_, Up)),
								get_down(R, C, Rows, Down),
								get_left(R, C, Rows, Left),
								get_right(R, C, Rows, Right),
								%write('Up: '), writeln(((R, C), Up, Down, Left, Right)),
								Up > Down, Up > Left, Up > Right.
% Down
arrow_ok(((R, C), d), Rows) :- Rd #= R + 1,
								at(Rows, Rd, C, (_, Down)),
								get_up(R, C, Rows, Up),
								get_left(R, C, Rows, Left),
								get_right(R, C, Rows, Right),
								%write('Down: '), writeln(((R, C), Up, Down, Left, Right)),
								Down > Up, Down > Left, Down > Right.
								
% Left
arrow_ok(((R, C), l), Rows) :- Cl #= C - 1,
								at(Rows, R, Cl, (_, Left)),
								get_up(R, C, Rows, Up),
								get_down(R, C, Rows, Down),
								get_right(R, C, Rows, Right),
								%write('Left: '), writeln(((R, C), Up, Down, Left, Right)),
								Left > Up, Left > Down, Left > Right.
% Right
arrow_ok(((R, C), r), Rows) :- Cr #= C + 1,
								at(Rows, R, Cr, (_, Right)),
								get_up(R, C, Rows, Up),
								get_down(R, C, Rows, Down),
								get_left(R, C, Rows, Left),
								%write('Right: '), writeln(((R, C), Up, Down, Left, Right)),
								Right > Up, Right > Down, Right > Left.
get_up(1, _, _, 0) :- !.
get_up(R, C, Rows, Up) :- Ru #= R - 1,
							at(Rows, Ru, C, (_, Temp)),
							(
								integer(Temp) ->
									Up #= Temp;
									Up #= 0
							).

get_down(R, _, Rows, 0) :- length(Rows, R), !.
get_down(R, C, Rows, Down) :- Rd #= R + 1,
								at(Rows, Rd, C, (_, Temp)),
							(
								integer(Temp) ->
									Down #= Temp;
									Down #= 0
							).
							
get_left(_, 1, _, 0) :- !.
get_left(R, C, Rows, Left) :- Cl #= C - 1,
								at(Rows, R, Cl, (_, Temp)),
							(
								integer(Temp) ->
									Left #= Temp;
									Left #= 0
							).
							
get_right(_, C, Rows, 0) :- length(Rows, C), !.
get_right(R, C, Rows, Right) :- Cr #= C + 1,
								at(Rows, R, Cr, (_, Temp)),
								(
									integer(Temp) ->
										Right #= Temp;
										Right #= 0
								).

at(Mat, Row, Col, Val) :- nth1(Row, Mat, ARow), nth1(Col, ARow, Val).
indices(List, E, Is) :- findall(N, nth1(N, List, E), Is).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sample problems
	
	For every pair (L, N), we have:
	- L: letters a, b, c,... represent the region of the cell.
		 # is used for the arrows and empty squares.
	- N: integer with the value in the cell.
		 u, d, l, r are used for arrows pointing up, down, left, and 
		 right respectively, while # is used for empty squares.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
problem(1, P) :- 
        P = [[(a,1),(#,d),(c,_),(c,3)],
             [(a,2),(a,3),(c,1),(#,u)],
             [(d,_),(d,2),(#,d),(e,_)],
             [(d,3),(e,1),(e,4),(e,3)]].
			 
problem(2, P) :- 
        P = [[(a,1),(a,2),(d,1),(d,_)],
             [(#,r),(a,_),(d,2),(e,1)],
             [(c,_),(c,1),(#,r),(e,4)],
             [(c,3),(#,l),(e,2),(e,3)]].
			 
problem(3, P) :- 
        P = [[(a,1),(#,d),(c,2),(c,3)],
             [(a,2),(a,3),(c,1),(#,u)],
             [(d,1),(d,2),(#,d),(e,_)],
             [(d,3),(e,_),(e,4),(e,3)]].
			 
problem(4, P) :- 
        P = [[(a,1),(a,2),(b,4),(#,l),(c,3),(#,l),(d,1),(#,d)],
             [(#,#),(b,1),(b,2),(e,1),(c,2),(c,1),(d,4),(d,2)],
             [(f,1),(f,2),(b,3),(e,2),(e,3),(g,2),(d,3),(h,1)],
             [(#,#),(i,1),(j,2),(j,1),(#,u),(g,1),(k,2),(h,3)],
             [(l,1),(i,2),(#,#),(m,2),(m,1),(g,3),(k,1),(h,2)],
             [(l,2),(n,3),(n,2),(#,#),(o,2),(p,1),(q,4),(q,3)],
             [(s,3),(#,#),(n,1),(#,d),(o,1),(p,2),(#,u),(q,2)],
             [(s,2),(s,1),(#,r),(t,4),(t,3),(t,1),(t,2),(q,1)]].

problem(5, P) :- 
        P = [[(a,_),(a,2),(b,_),(#,l),(c,_),(#,l),(d,_),(#,d)],
             [(#,#),(b,_),(b,_),(e,_),(c,_),(c,_),(d,4),(d,_)],
             [(f,_),(f,_),(b,3),(e,_),(e,_),(g,2),(d,_),(h,1)],
             [(#,#),(i,1),(j,_),(j,_),(#,u),(g,_),(k,_),(h,_)],
             [(l,1),(i,_),(#,#),(m,_),(m,_),(g,_),(k,_),(h,_)],
             [(l,_),(n,3),(n,_),(#,#),(o,_),(p,1),(q,4),(q,_)],
             [(s,3),(#,#),(n,1),(#,d),(o,_),(p,_),(#,u),(q,_)],
             [(s,2),(s,_),(#,r),(t,_),(t,3),(t,1),(t,_),(q,1)]].
			 
problem(6, P) :- 
        P = [[(a,_),(#,d),(g,_),(i,2),(i,_),(o,_),(o,3),(o,4)],
             [(b,_),(b,5),(g,2),(j,_),(#,d),(#,u),(o,_),(#,u)],
             [(#,r),(b,_),(b,3),(j,_),(k,3),(k,_),(p,_),(p,_)],
             [(c,_),(b,_),(#,u),(f,1),(k,2),(l,_),(#,l),(q,3)],
             [(c,_),(f,3),(f,2),(f,4),(l,_),(l,5),(l,_),(q,_)],
             [(c,3),(#,#),(#,d),(h,_),(#,u),(l,_),(#,#),(q,1)],
             [(d,_),(d,2),(h,_),(h,_),(m,_),(m,4),(m,_),(#,d)],
             [(e,2),(e,3),(e,_),(#,#),(n,2),(n,_),(m,3),(m,_)]].