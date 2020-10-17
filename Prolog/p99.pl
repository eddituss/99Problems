%% Solutions to 99 prolog problems
%% All solutions assume that the relationship is correct, there are no entry-validations

%% Check problem specification on github.com/eddituss/99Problems

%% For a quick search of an specific solution you may look for %% pxx  where xx is  problem's number. If you find any mistakes or a more comfortable way to solve the problem, please contact me!

%% File made for academic purposes, for teaching syntax to university students or general public
%% For more information you may contact the author Eddy Ramírez at edramirez@tec.ac.cr




%% A relationship between a non-empty list and it's last element
%% p01
mylast([X],X):-!.
mylast([_H|T],X):-mylast(T,X).


%% A relationship between a list with at least two elements and the but-last element
%% p02
butlast([X,_Y],X):-!.
butlast([_H|T],X):-butlast(T,X).

%% A relationship between a number n, a list with at least n+1 elements and 
%% the n-th element of the list
%% p03
elementAt(0,[H|_T],H):-!.
elementAt(N,[_H|T],X):- N1 is N-1, elementAt(N1,T,X).

%% A relationship between a list and it's size
%% p04  (sin usar length)
listSize([],0):-!.  
listSize([_H|T],X):- listSize(T,SizeT), X is SizeT+1.

%% A relationship between a list and it's reverse
%% p05  -  innefficient  (sin usar reverse)
invert([],[]):-!.
invert([H|T],X):-invert(T,TI),append(TI,[H],X).

%% A relationship between a list and it's reverse
%% p05  -  efficient
invertTail([],[]):-!.
invertTail(L,X):-invertTail(L,[],X).

invertTail([],X,X):-!.
invertTail([H|T],Acum,X):-invertTail(T,[H|Acum],X).

%% A relationship on palindrome lists
%% p06
palindrome(L):-invertTail(L,L).


%% A relationship between a list and it's flattened version
%% p07
aplanar([],[]):-!.
aplanar([[H|T]|TT],FF):-!,aplanar([H|T],F1), aplanar(TT,F2), append(F1,F2,FF).
aplanar([[]|T],F):-!,aplanar(T,F).
aplanar([H|T],[H|FT]):-aplanar(T,FT).

%% A relationship between a list and it's version without consecutive duplicates
%% p08
eliminateDuplicates([],[]):-!.
eliminateDuplicates([H,H|T],X):-!, eliminateDuplicates([H|T],X).
eliminateDuplicates([H|T],[H|X]):-eliminateDuplicates(T,X).


%% A relationship between a list and a list with all consecutive packed in sublists
%% p09
pack([],[]):-!.
pack([H|T],X):-pack(T,[H],X).

pack([],P,[P]):-!.
pack([H|T],[H|P],X):-!,pack(T,[H,H|P],X).
pack([H|T],L,[L|X]):-pack(T,[H],X).


%% A relationship between a list and the code of the list
%% Example: [a,a,a,a,b,b] and [[a,4],[b,2]]
%% p10
lengthEncoding(L,LE):- pack(L,PL), encode1(PL,LE).

encode1([],[]):-!.
encode1([[A|TA]|T],[[A,Len]|X]):-length(TA,Le), Len is Le+1, encode1(T,X).
  

%% A relationship between a list and the code of the list (without making packs)
%% Example: [a,a,a,a,b,b,c] and [[a,4],[b,2],c] 
%% p11
lengthEncoding1(L,LE):-pack(L,PL), encode2(PL,LE).

encode2([],[]):-!.
encode2([[A]|T], [A|LE1]):-!, encode2(T,LE1).
encode2([[A|TA]|T],[[A,Len]|X]):-length(TA,Le), Len is Le+1, encode2(T,X).


%% p12
%% A relationship between a number N, an element E and a list with N E's
% Example: (1, 3, [1,1,1])
repeatN(_E,0,[]):-!.
repeatN(E,Amount,[E|T]):- A is Amount-1, repeatN(E, A, T).

%% A relationship between a list of frequencies and the list with the total sequences
% Example: [[a,2],b,[c,3]] and [a,a,b,c,c,c]
decode([[A,NA]| T], LDecode):-!,repeatN(A,NA,ListA), decode(T,TDecode),
                            append(ListA,TDecode,LDecode).
decode([H|T], [H|TDecode]):-decode(T,TDecode).
decode([],[]):-!.

%% 
%% p13
encodeDirect([],[]):-!.
encodeDirect([H|T], EN):- encodeDirect(T,H,1,EN).

encodeDirect([],H,1,[H]):-!.
encodeDirect([],H,Cont,[[H,Cont]]):-!.
encodeDirect([H|T], H, Cont, EN):-!,ContNext is Cont+1, 
                                encodeDirect(T,H,ContNext,EN).
encodeDirect([H|T],E,1,[E|EN]):-!,encodeDirect(T,H,1,EN).
encodeDirect([H|T],E,C,[[E,C]|EN]):- encodeDirect(T,H,1,EN).



%% A relationship between a list and the same list with each element duplied
%% p14
dupli([H|T],[H,H|DupliT]):- dupli(T,DupliT).
dupli([],[]):-!.

%% A relationship between a list, a number N and the same list with each element replied N times
%% p15
repli([H|T],Amount,Repli):-!,repeatN(H, Amount, HA), repli(T, Amount, TA), 
                          append(HA, TA, Repli).
repli([],_A,[]).

%% Domain:  A list and a natural number n
%% Codomain: A list with every n-th element dropped off
%% p16
dropN([],_N,[]):-!.
dropN(L,N,LXN):-dropN(L,N,1,LXN).

dropN([],_N,_C,[]):-!.
dropN([_H|T], N,N,TXN):-!,dropN(T,N,1,TXN).
dropN([H|T],N,C,[H|TXN]):- C1 is C+1, dropN(T,N,C1,TXN).

%% A relation between a list L a length n, 
% P1 (the first n elements) and P2 the rest of L
%% p17
split([],_N,[],[]):-!.
split(L,0,[],L):-!.
split([H|T], N, [H|LS1],LS2):- N1 is N-1, split(T,N1,LS1,LS2).



%% A relation between a list, numbers n, m  
% and the slice between n a m of L
%% p18
slice([],_I,_F,[]):-!.
slice(L,1,F,X):- !,split(L,F,X,_L).
slice([_H|T],I,F,X):- I2 is I-1, F2 is F-1, slice(T,I2,F2,X).


%% p19
% A relation between a number n, a quotient b and a remainder X

rotate(L,K,LR):- length(L,Lon), KMod is K mod Lon, split(L,KMod,L2,L1),
                append(L1,L2,LR).



%% A relation between a list, a number n and the list without nth position
%% p20
removeAt([],_,[]):-!.
removeAt([_H|T],1,T):-!.
removeAt([H|T],N,[H|RN]):-N1 is N-1, removeAt(T,N1,RN).


%A relation between a list, a number n, an element e and the list
% with e at the nth position
%% p21
insertAt([],_P,E,[E]):-!.
insertAt(L,1,E,[E|L]):-!.
insertAt([H|T],P,E,[H|TE]):-P1 is P-1, insertAt(T,P1,E,TE).



%A relation between a number n, m and the list with the natural numbers
% between N and m
%% p22
range(I,F,[]):-I>F,!.
range(I,F,[I|R]):-I1 is I+1, range(I1, F,R).


% A relation between a list L a number n and a list with n random numbers
%% p23
rndSelect(_L,0,[]):-!.
rndSelect(L,K,[H|T]):- member(H,L), delete(L,H,LsH), K1 is K-1, 
                        rndSelect(LsH,K1,T).


  
%A relation between 2 numbers,n and k and the rndSelectk of the range(0,N)
%% p24
lottoSelect(C,T,X):-range(0,T,R), rndSelect(R,C,X).


%A relation of a list and a permutation of it
%% p25
perm([],[]).
perm(L,[H|T]):- member(H,L), delete(L,H,LsH), perm(LsH,T).

% A relation between a list L and a number n and a combination of n elements of L
%% p26
combination(_L,0,[]):-!.
combination(L,K,L):- length(L,K),!.
combination([H|T],K,[H|CT]):- K1 is K-1, combination(T,K1,CT).
combination([_H|T],K,CT):-combination(T,K,CT).


% A relation between a list L, a list of numbers G(that sums length(L)
% a List with the distribution of L in G[i] groups

resta([],_L,[]):-!.
resta([H|T],L,[H|Resta]):- \+ memberchk(H,L),!, resta(T,L,Resta).
resta([_H|T],L,Resta):- resta(T,L,Resta).

%% p27
group([],[],[]):-!.
group(L,[Size|Ss],[Comb|Groups]):- combination(L,Size,Comb), 
            resta(L,Comb,RL),  group(RL,Ss,Groups).
            
%% A list of lists LL and the List of lists ordered by length

partition([],_E,[],[]):-!.
partition([H|T],E,[H|Menores],Mayores):- length(H,LH), LH<E,!, 
                        partition(T,E,Menores,Mayores).
partition([H|T],E,Menores,[H|Mayores]):-partition(T,E,Menores,Mayores).

lsort([],[]):-!.
lsort([H|T],Lord):- length(H,LH), partition(T,LH,Men,May),
                    lsort(Men,MenOrd), lsort(May,MayOrd),
                    append(MenOrd,[H|MayOrd],Lord).



% A relation that admits only primes
%% p31
prime(2).
prime(3).
prime(X):- X>1, \+ 0 is X rem 2, prime(X,3).

prime(X,I):- I*I > X,!.
prime(X,I):- \+ 0 is X rem I, I2 is I+2, prime(X,I2).

%% p32
%% Using euclid's algorithm ¡Check for the mathemathical proof!
mygcd(A,0,A):-!.
mygcd(A,B,X):- ANext is A mod B, mygcd(B,ANext,X).


%% p33
% A relation that admits only coprimes
coprime(A,B):- 1 is gcd(A,B).


%% p34
%% (innefficient) Eulertotient
filterCoprime(_N,[],[]):-!.
filterCoprime(N,[H|T],[H|Cop]):-coprime(H,N),!,filterCoprime(N,T,Cop).
filterCoprime(N,[_H|T],Cop):-filterCoprime(N,T,Cop).

totientPhi(N,Phi):-range(0,N,LN), filterCoprime(N,LN,Coprimes),
                    length(Coprimes,Phi).

% A relation between a Number n and a list of prime factors of N
%% p35
primeFactors(N,[2|FN]):-0 is N rem 2,!, N1 is N div 2,primeFactors(N1,FN).
primeFactors(N,FN):- primeFactors(N,3,FN).

primeFactors(1,_I,[]):-!.
primeFactors(N,I,[I|FN]):- 0 is N rem I,!,N1 is N div I, 
                    primeFactors(N1,I,FN).
primeFactors(N,I,[N]):- I*I > N,!.
primeFactors(N,I,FN):- I2 is I+1, primeFactors(N,I2,FN).




%% p36
%% A relation between a number n and it's "factorization"
primeFactorsMult(N,X):- primeFactors(N,FN), pack(FN,PFN),
                    encode1(PFN,X).

% A relation between numbers a, n, and a^n
pot(_A,0,1):-!.
pot(A,E,X):- 0 is E rem 2,!, E2 is E div 2, pot(A,E2,RX), X is RX*RX.
pot(A,E,X):- E2 is E div 2, pot(A,E2,RX), X is RX*RX*A.


calcPhi([],1):-!.
calcPhi([[Prime,Exp]| FPU], Phi):- calcPhi(FPU,Phi2), 
                E1 is Exp-1, pot(Prime,E1,X), P1 is Prime-1, 
                Phi is Phi2 * P1 * X.

%% p37
%% A relation between a natural number n and phi(n) (efficient)
phi(N,Phi):- primeFactorsMult(N,FPU), calcPhi(FPU,Phi).




%% p38
%% DONE!


filterPrimos([],[]):-!.
filterPrimos([H|T],[H|Prim]):- prime(H),!, filterPrimos(T,Prim).
filterPrimos([_H|T],Prim):-filterPrimos(T,Prim).


%% p39
%% A Relation between two numbers n and m and a list with the primes between them
%% this can be done with the siege ... (much better)...

listPrimes(I,F,Primos):- range(I,F,I2F),filterPrimos(I2F,Primos).



%% p40
%% A relation between an even number n and a pair of primes that sums n
goldbach(N,P1,P2):- listPrimes(2,N,Primos), 
                    member(P1,Primos), member(P2,Primos), N is P1+P2.



%% p41
%% Domain: Two natural number a,b with a<=b
%% Codomain: The goldbach pair for every even number between beg and end


%% p49
%% Domain: A natural number greater than 1
%% Codomain: The list of strings of n bits

%% Domain: A list (as a set, where every element is in a list) and a number
%% Codomain: Set to the nth power


%% Domain: Multiplies two sets 
%% Codomain: The set multiplied

%% p50
%% Domain: A multiset (of frequencies)
%% Codomain: A set of huffman codes
%% Example of entrance [{a,15},{b,1},{c,45},{d,8},{e,16},{f,12},{g,10}]


%% Domain: A list of trees sorted by frequency
%% Codomain: A single huffman tree


%% Domain: A tree
%% Codomain: A list with the route to the leaves of the tree

%% p54
%% Domain: A tuple 
%% Codomain: A boolean true if the input is a binary tree (in erlang), false otherwise


%% p55 
%% Domain: A natural number
%% Codomain: A perfect balanced tree with n height


%% p55 (All balanced trees of heigth n) To Achieve the problem specification, pow set must be filtered... the idea is the same)
%% Domain: A natural number n
%% Codomain: All balanced trees of height n in a list





%% p56
%% Domain: 2 subtrees: right and left
%% Codomain: A boolean, true if right is mirroring left by it's structure, false otherwise




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

