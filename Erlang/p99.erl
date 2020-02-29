-module(p99).
-compile(export_all). %% VERY BAD PRACTICE, private functions should keep private
%% In this excercise, I chose to do this, because is more confortable for testing
%% and because that way, people may look at private functions and see how they work
%% You should always export only the functions that are public in the module.

%% Solutions to 99 prolog problems in Erlang
%% All solutions assume that the domain is correct, there are no entry-validations
%% Anyway, in Erlang, is very easy to do so, with warnings or exceptions
%% Check specification on github.com/eddituss/99Problems

%% For a quick search of an specific solution you may look for %% pxx  where xx is  problem's number. If you find any mistakes or a more comfortable way to solve the problem, please contact me!

%% File made for academic purposes, for teaching syntax to university students or general public
%% For more information you may contact the author Eddy Ramírez at edramirez@tec.ac.cr


%% Domain:   An element and a list
%% Codomain: A boolean, true if the element is member of the list, false otherwise
%% General purpose functions

member(E,[E|_T])->true;
member(_E,[])->false;
member(E,[_H|T])->member(E,T).


%% Domain: A non-empty list
%% Codomain: The list with only the last element of the input
%% p01
last([X])->[X];
last([_H|T])->last(T).

%% Domain: A list with at least 2 elements
%% Codomain: A list with the last 2 elements
%% p02
butlast([X,Y])->[X,Y];
butlast([_H|T])->butlast(T).

%% Domain: A list and a position less than the length of the list
%% Codomain: The element at the n-th position (starting at zero)
%% p03
elementAt(0,[H|_T])->H;
elementAt(N,[_H|T])->elementAt(N-1,T).

%% Domain: A list
%% Codomain: A number with the length of the input
%% p04
listSize([])->0;
listSize([_H|T])->1+listSize(T).

%% Domain: A list
%% Codomain: The input reversed
%% p05  -  innefficient
invert([])->[];
invert([H|T])->invert(T)++[H].

%% Domain: A list
%% Codomain: The input reversed
%% p05  -  efficient
invertTail(L)->invertTail(L,[]).

invertTail([],A)->A;
invertTail([H|T],A)->invertTail(T,[H|A]).

%% Domain:  A list
%% Codomain: A boolean, true if it is palindrome, false if it is not
%% p06
palindrome(L)-> L =:= lists:reverse(L).


%% Domain: A list (may be nested)
%% Codomain: A list with all the input's internal parenthesis "erased"
%% p07
flatten([])->[];
flatten([[H|T]|TT])->flatten([H|T])++flatten(TT);
flatten([[]|T])->flatten(T);
flatten([H|T])->[H|flatten(T)].

%% Domain: A list
%% Codomain: The input with every consecutive equal elements placed once
%% p08
eliminateDuplicates([H,H|T])->eliminateDuplicates([H|T]);
eliminateDuplicates([H|T])->[H|eliminateDuplicates(T)];
eliminateDuplicates([])->[].


%% Domain: A list
%% Codomain: A list of lists each one with the consecutive equal elements in it
%% p09
pack([])->[];
pack([H|T])->pack(T,[H]).

pack([H|T],[H|P])->pack(T,[H,H|P]);
pack([H|T],Pack)->[Pack|pack(T,[H])];
pack([],Pack)->[Pack].

%% Domain: A list
%% Codomain: A list with tuples, each one with the cardinality of the consecutive equal elements in the list
%% p10
lengthEncoding(L)->[{X,length(X)}||X<-pack(L)].
  
%% Domain: A list
%% Codomain: A list with tuples, each one with the cardinality of the consecutive equal elements in the list
%% p11
lengthEncodingMinus1(L)->[tuple(X)||X<-pack(L)].

%% Domain: A pack
%% Codomain: A tuple if the pack is larger than 1, the element otherwise
tuple([X])->X;
tuple([H|T])->{H,1+length(T)}.


%% p12
%% Domain:  A natural number and an element
%% Codomain: A list of length cant, filled with cant elements
repeat(0,_E)->[];
repeat(N,E)->[E|repeat(N-1,E)].

%% Domain:   A list with tuples of the form {elem,cant} or just elem (equivalent to (1 elem))
%% Codomain: A list with all the elements unpacked
decode([{E,Cant}|L])->repeat(Cant,E)++decode(L);
decode([E|L])->[E|decode(L)];
decode([])->[].

%% Domain: A list
%% Codomain: A list with the cardinality of the consecutive equal elements 
%% p13
lengthEncodingDirect([])->[];
lengthEncodingDirect([H|T])->lengthEncodingDirect(T,H,1).

lengthEncodingDirect([H|T],H,N)->lengthEncodingDirect(T,H,N+1);
lengthEncodingDirect([H1|T],H2,1)->[H2|lengthEncodingDirect(T,H1,1)];
lengthEncodingDirect([H1|T],H2,N)->[{H2,N}|lengthEncodingDirect(T,H1,1)];
lengthEncodingDirect([],H,1)->[H];
lengthEncodingDirect([],H,N)->[{H,N}].



%% Domain:  A list
%% Codomain: The same list with the elements duplied
%% p14
dupli([H|T])->[H,H|dupli(T)];
dupli([])->[].


%% Domain: A list and a natural number cant
%% Codomain: A list with each element replied cant times
%% p15
repli([H|T],N)->repeat(N,H)++repli(T,N);
repli([],_N)->[].

%% Domain:  A list and a natural number n
%% Codomain: A list with every n-th element dropped off
%% p16
dropN(L,N)->dropN(L,N,1).

dropN([_H|T],N,N)->dropN(T,N,1);
dropN([H|T],N,Cont)->[H|dropN(T,N,Cont+1)];
dropN([],_N,_Cont)->[].

%% Domain:  A list and a natural number less or equal than the length of the list
%% Codomain: A tuple with two lists, the first with the first part elements, the second with the others
%% p17
split(L,N)->split(L,N,[]).

split(L,0,A)->{lists:reverse(A),L};
split([H|T],N,A)->split(T,N-1,[H|A]).

%% p18
%% Domain: A list and a natural number less or equal than the length of the list
%% Codomain: A list with the first k elements of the list
firstN(_L,0)->[];
firstN([H|T],N)->[H|firstN(T,N-1)].

%% Domain: A list and 2 natural numbers b and e, 0<=b<=e<=length of list
%% Codomain: A sublist from position b to position e
slice(L,0,E)->firstN(L,E);
slice([_H|T],B,E)->slice(T,B-1,E-1).



%% p19
%% Domain:  Two integer numbers
%% Codomain: The natural number r = a(b) that 0<=r<b
mod(A,B)-> (B+(A rem B)) rem B.

%% Domain: A list and an integer number rot
%% Codomain: A rot rotation of the list (negatives to left)
% using guard for saving the usage of another function
rotate(L,R)when R>length(L)->rotate(L,mod(R,length(L)));  
rotate(L,R)->{Front,End}=split(L,R), End++Front.

%% Domain:  A list and a natural number less than the length of the list
%% Codomain: The list without the k-th element
%% p20
removeAt([_H|T],0)->T;
removeAt([H|T],N)->[H|removeAt(T,N-1)].

%% Domain: A list and a natural number less than the length of the list and an element
%% Codomain: The list with the k-th element inserted
%% p21
%% insert-at
  %%(l elem k)


%% Domain: Two integer numbers a and b, with a <= b
%% Codomain: An ordered list with all the integers between a and b, inclusive
%% p22
%% range
  %%(begin end)


%% Domain: A list and a natural number less or equal the length of the list
%% Codomain: Cant different elementos randomly selected from the list
%% p23
%% rnd-select
  %%(l cant)


%% rnd-select-aux
  %%(l cant acum pos)



%% Domain: A two natural numbers, c and t, where c < t
%% Codomain: A list with c randomly selected numbers from 1 to t.
%% p24
%% lotto-select
  %%(cant tot)



%% Domain:  A list
%% Codomain: A random permutation of the list
%% p25
%% rnd-permu
  %%(l)


%% Domain: A number k and a list, the number must be less or equal the length of the list
%% Codomain: The list of all possible combinations of k elements of the list
%% p26
%% combination
  %%(k l)



%% p27
%% Domain: Two sets A and B
%% Codomain: The set resulting from A-B
%% difference
  %%(a b) 



%% Domain: An Element and a lists of lists
%% Codomain: A list with the element inserted in all the sublists (of each list) 
%% place-in
  %%(elem ll)



%% Domain: A list of elements and a list of natural numbers,
%%         the sum of this numbers must be equal to the length of the first list
%% Codomain: All the possible combinatios of multinomials coefficient
%% group
  %%(l distrib)



%% group-aux
  %%(l groups)



%% p28
%% Domain: A list of lists (length-i list-i), a number p, a sublist of length less than p, a sublist of the rest
%% Codomain: Two lists into a list, one with minors, the other with the rest
%% sort-split
  %%(len-list piv minor greater)



%% Domain:  A list of lists in the form (length-i list-i)
%% Codomain: A sorted lists of length-i based
%% Using quick-sort
%% lsort-aux
  %%(len-list)



%% Domain: A lists of lists
%% Codomain: The lists of lists sorted by the lists' length
%% lsort
  %%(ll)



%% p31
%% Domain: A natural number
%% Codomain: A boolean, true is it is prime, false otherwise
%% prime?
  %%(n)


%% prime?-aux
  %%(n i)



%% p32
%% Domain: Two natural numbers
%% Codomain: The number that is the GCD of the input
%% Using euclid's algorithm ¡Check for the mathemathical proof!
%% gcd
  %%(a b)



%% p33
%% Domain: Two natural numbers
%% Codomain: A boolean, true if the numbers are coprime, false otherwise
%% coprimes
  %%(a b)



%% p34
%% Domain: A natural number
%% Codomain: Euler totient of the number (innefficient)
%% phi
  %%(n)



%% p35
%% Domain: A natural number greater than 1
%% Codomain: The list of the prime factors of n
%% prime-factors
  %%(n)



%% prime-factors-aux
  %%(n i)


%% p36
%% Domain: A natural number greater than 1
%% Codomain: The list of ((a1 b1) (a2 b2) ...) that n = a1**b1* a2**b2 * ...
%% prime-factors-mult
  %%(n)


%% p37
%% Domain: A natural number
%% Codomain: Euler totient of the number (efficient)
%% euler-totient
  %%(n)



%% p38
%% Copy this code, it just measure cpu time of the algorithm
%% (time (phi 1200000)) (time (euler-totient 1200000))
%% (time (phi 10090)) (time (euler-totient 10090))
%% In general:
%%   the first has a cost of n*sqrt(n) in general
%%   the second one log(n) for composite numbers (more frequently found) and sqrt(n) for prime numbers

%% p39
%% Domain:  Two natural numbers a, b that a <= b
%% Codomain: The ordered list of prime numbers between a and b
%% prime-range
  %%(a b)


%% p40
%% Domain: A natural even number
%% Codomain: A list with two primes which sum is n
%% goldbach
  %%(n)


%% goldbach-aux
  %%(n p)



%% p41
%% Domain: Two natural number a,b with a<=b
%% Codomain: The goldbach pair for every even number between beg and end
%% goldbach-list
  %%(beg end)



%% p49
%% Domain: A natural number greater than 1
%% Codomain: The list of strings of n bits
%% gray-code
  %%(n)



%% Domain: A list (as a set) and a number
%% Codomain: Set to the nth power
%% pow-set
  %%(set n)



%% Domain: Multiplies two sets (only when called from pow-set)
%% Codomain: The set multiplied
%% times
  %%(s1 s2)



%% p50
%% Domain: A multiset of frequencies
%% Codomain: A set of huffman codes
%% Example of entrance '((a 15) (b 1) (c 45) (d 8) (e 16) (f 12) (g 10))
%% huffman-codes
  %%(lfre)



%% Domain: A list of frequencies in the Huffman way
%% Codomain: The list sorted from the least frequent to the most frequent
%% huff-sort
  %%(tree-list)


%% Domain: A list of trees sorted by frequency
%% Codomain: A single huffman tree
%% huffman-tree
  %%(hsort)


%% Domain: A sorted list of trees (by frequency) and a new tree
%% Codomain: The sorted list with the new tree in it
%% insert-sort
  %%(elem L)


%% Domain: A binary tree in the way '(Root LeftChild RightChild)
%% Codomain: A boolean, true if it is a leaf, false otherwise
%% leaf?
  %%(node)


%% Domain: A tree
%% Codomain: A list with the route to the leaves of the tree
%% routes-to-leaves
  %%(tree)



%% routes-to-leaves-aux  %% Routes inverted
  %%(tree route)



%% p54
%% Domain: A list
%% Codomain: A boolean true if the input is a binary tree (in scheme), false otherwise
%% binary-tree?
  %%(l)


%% p55 (alternative version for exactly (2**n)-1 nodes)
%% Domain: A natural number
%% Codomain: A perfect balanced tree with n height
%% perfect-balanced-tree
  %%(n)


%% p55 (All balanced trees of heigth n) To Achieve the problem specification, pow set must be filtered... the idea is the same)
%% Domain: A natural number n
%% Codomain: All balanced trees of height n in a list
%% balanced-trees
  %%(n)


%% balanced-trees-aux
  %%(combo)



%% p56
%% Domain: 2 subtrees: right and left
%% Codomain: A boolean, true if right is mirroring left by it's structure, false otherwise
%% mirror?
  %%(right left)



%% Domain: A binary tree
%% Codomain: A boolean, true if the right-son is mirroring left-son, false otherwise
%% symetric-bt?
  %%(tree)


%% p57  Binary search tree (bst)
%% Domain: A BST of numbers and an element (also a number)
%% Codomain: The BST with the element inserted
%% insert-bst
  %%(tree elem)



%% Domain: A list of numbers
%% Codomain: A BST with the elements of the list inserted in the list's order
%% construct
  %%(lis)


%% construct-aux
  %%(lis bst)


%%p58
%% TO-DO: fix this (estaba en reunion y no lo he ni probado)
%% Domain:   A real number
%% Codomain: The logarithm in base 2
%% log2
  %%(n)


%% Domain: A natural number n
%% Codomain: The balanced trees with n nodes
%% balanced-trees-with-n-nodes
  %%(n)

