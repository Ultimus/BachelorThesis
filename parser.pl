:-use_module(library(lists)).

packed_visited_expression(root,root).
packed_visited_expression(0,'$bind_var'(active,[],'$bind_var'(ready,[],'$bind_var'(waiting,[],[])))).
packed_visited_expression(1,'$bind_var'(active,[],'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(2,'PID'),[])))).
packed_visited_expression(2,'$bind_var'(active,[],'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(4,'PID'),[])))).
packed_visited_expression(3,'$bind_var'(active,[],'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(8,'PID'),[])))).
packed_visited_expression(4,'$bind_var'(active,[],'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(6,'PID'),[])))).
packed_visited_expression(5,'$bind_var'(active,[],'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(10,'PID'),[])))).
packed_visited_expression(6,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,[],[])))).
packed_visited_expression(7,'$bind_var'(active,[],'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(12,'PID'),[])))).
packed_visited_expression(8,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,[],[])))).
packed_visited_expression(9,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,[],[])))).
packed_visited_expression(10,'$bind_var'(active,[],'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(14,'PID'),[])))).
packed_visited_expression(11,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(4,'PID'),[])))).
packed_visited_expression(12,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(2,'PID'),[])))).
packed_visited_expression(13,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(2,'PID'),[])))).
packed_visited_expression(14,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(4,'PID'),[])))).
packed_visited_expression(15,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(8,'PID'),[])))).
packed_visited_expression(16,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(6,'PID'),[])))).
packed_visited_expression(17,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,'$avl_bv'(4,'PID'),'$bind_var'(waiting,[],[])))).
packed_visited_expression(18,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,'$avl_bv'(4,'PID'),'$bind_var'(waiting,'$avl_bv'(2,'PID'),[])))).
packed_visited_expression(19,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(8,'PID'),[])))).
packed_visited_expression(20,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(10,'PID'),[])))).
packed_visited_expression(21,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,'$avl_bv'(8,'PID'),'$bind_var'(waiting,[],[])))).
packed_visited_expression(22,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,[],'$bind_var'(waiting,'$avl_bv'(12,'PID'),[])))).
packed_visited_expression(23,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,'$avl_bv'(8,'PID'),'$bind_var'(waiting,'$avl_bv'(2,'PID'),[])))).
packed_visited_expression(24,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,'$avl_bv'(4,'PID'),'$bind_var'(waiting,[],[])))).
packed_visited_expression(25,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,'$avl_bv'(2,'PID'),'$bind_var'(waiting,[],[])))).
packed_visited_expression(26,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,'$avl_bv'(2,'PID'),'$bind_var'(waiting,[],[])))).
packed_visited_expression(27,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,'$avl_bv'(8,'PID'),'$bind_var'(waiting,[],[])))).
packed_visited_expression(28,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,'$avl_bv'(2,'PID'),'$bind_var'(waiting,'$avl_bv'(8,'PID'),[])))).
packed_visited_expression(29,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,'$avl_bv'(2,'PID'),'$bind_var'(waiting,'$avl_bv'(4,'PID'),[])))).
packed_visited_expression(30,'$bind_var'(active,'$avl_bv'(8,'PID'),'$bind_var'(ready,'$avl_bv'(6,'PID'),'$bind_var'(waiting,[],[])))).
packed_visited_expression(31,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,'$avl_bv'(4,'PID'),'$bind_var'(waiting,'$avl_bv'(8,'PID'),[])))).
packed_visited_expression(32,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,'$avl_bv'(8,'PID'),'$bind_var'(waiting,'$avl_bv'(4,'PID'),[])))).
packed_visited_expression(33,'$bind_var'(active,'$avl_bv'(4,'PID'),'$bind_var'(ready,'$avl_bv'(10,'PID'),'$bind_var'(waiting,[],[])))).
packed_visited_expression(34,'$bind_var'(active,'$avl_bv'(2,'PID'),'$bind_var'(ready,'$avl_bv'(12,'PID'),'$bind_var'(waiting,[],[])))).



toL([],A,A).
toL('$bind_var'(Name,Value,C),A,R) :- A2= [b(Name,Value)|A], toL(C,A2,R).

lex:- write('module Variables where \nimport Token\n\nvars:: [[Token]]'), nl, write('vars = [') ,
  findall(state(Id,Bs), (packed_visited_expression(Id, ToL),toL(ToL,[],Bs)),L),
  map(L,dump), write('    []]'), nl.

lex:- write('module Variables where \nimport Token\n\nvars:: [[Token]]'), nl, write('vars = [') ,
  findall(state(Id,Bs), (packed_visited_expression(Id,const_and_vars(_,B)), toL(B,[],Bs)), L),
  map(L, dump), write('    []]'), nl.


pp(b(_Name,pred_true)) :- write('N 1, '),!.
pp(b(_Name,pred_false)) :- write('N 0, '),!.
pp(b(_Name,'$avl_exp'(L))) :- format('S ~w, ',[R]), R = pp(L),!.
pp(b(_Name,'$avl_bv'(N,_))):- format('Bin ~w, ',[N]),!.
pp(b(_Name,[])) :- write('S [], '),!.
pp(b(_Name,FD)):- name(FD, FDN), fd(FDN,N), format('N ~w, ',[N]),!.
pp(b(_Name,X)) :- number(X), format('N ~w, ',[X]),!.

dump(state(Id,Bs)) :-
  write('    ['), map(Bs,pp), format('N ~w],',[Id]),nl.

map([],_).
map([H|T], F):- C =.. [F,H], call(C), map(T,F).

fd([],[]).
fd([36, 102, 100, 95|T],N):- findNumber(T,N). 

digit(48,R):-R = 0.
digit(49,R):-R = 1.
digit(50,R):-R = 2.
digit(51,R):-R = 3.
digit(52,R):-R = 4.
digit(53,R):-R = 5.
digit(54,R):-R = 6.
digit(55,R):-R = 7.
digit(56,R):-R = 8.
digit(57,R):-R = 9.
digit(_,false).




findNumber([],[]).
findNumber([H|_],R):- digit(H,R), number(R).
findNumber([H|T],R):- digit(H,M), \+number(M), findNumber(T,R).
 


:-lex,halt.



