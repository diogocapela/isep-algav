% Exercício 1 - a) Obter a média de uma lista de inteiros

somaTotalNumerosLista([], 0).
somaTotalNumerosLista([H|T], S):-
    somaTotalNumerosLista(T, S1),
    S is S1 + H.

tamanhoLista([], 0).
tamanhoLista([_|T], R):-
    tamanhoLista(T, R1),
    R is R1 + 1.

mediaLista(L, M):-
    somaTotalNumerosLista(L, T0),
    tamanhoLista(L, T1),
    M is T0 / T1.

% Exercício 1 - b) Obter o menor valor de uma lista de inteiros

menorValorDaLista([N], N):-!. % apenas vai para o 1 predicado caso a lista so tenha um elemento la dentro

menorValorDaLista([H|T], N):-
    menorValorDaLista(T, N),
    (H > N ; H = N),!. % aqui o ; significa OU

menorValorDaLista([H|T], H):-
    menorValorDaLista(T, N),
    H < N,!.

% Exercício 1 - c) Contar o número de elementos pares e ímpares numa lista de inteiros

contarNumeroDePares([], 0):-!.

contarNumeroDePares([H|T], P):-
    contarNumeroDePares(T, P1),
    0 is H mod 2,
    P is P1 + 1,!.

contarNumeroDePares([_|T], P):-
    contarNumeroDePares(T, P).

contarNumeroDeImpares([], 0):-!.

contarNumeroDeImpares([H|T], P):-
    contarNumeroDeImpares(T, P1),
    1 is H mod 2,
    P is P1 + 1,!.

contarNumeroDeImpares([_|T], P):-
    contarNumeroDeImpares(T, P).


contarParesEImpares(L, P, I):-
    contarNumeroDePares(L, P),
    contarNumeroDeImpares(L, I).

% Exercício 1 - d) Verificar se uma lista tem elementos repetidos

verifica([], _, 0). % verifica se uma lista é vazia
verifica([H, _], H, 1). 
verifica([_, T], N, R):- % vai iterativamente verificando se o Head de baixo está repetido num Tail de cima
    verifica(T, N, R).

verificaRepetidos([]):-
    false.

verificaRepetidos([H|T]):-
    verifica(T, H, 1),!.

verificaRepetidos([_|T]):-
    verificaRepetidos(T),!.




