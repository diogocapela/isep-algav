continente(africa).
continente(america).
continente(asia).
continente(europa).
continente(oceania).

pais(argentina, america, 44.3).
pais(bolivia, america, 11.1).
pais(brasil, america, 209.3).
pais(chile, america, 18.1).
pais(colombia, america, 49.1).
pais(equador, america, 17.4).
pais(guiana, america, 0.8).
pais(guianafrancesa, america, 0.3).
pais(paraguai, america, 6.8).
pais(peru, america, 32.2).
pais(suriname, america, 0.6).
pais(venezuela, america, 32).
pais(cuba, america, 11.5).
pais(uruguai, america, 3.5).
pais(mexico,america,129.2).
pais(estadosunidos,america,327.2).
pais(canada,america,37.1).
pais(albania, europa, 2.9).
pais(alemanha, europa, 82.8).
pais(armenia, europa, 2.9).
pais(austria, europa, 8.8).
pais(belgica, europa, 11.4).
pais(bielorussia, europa, 9.5).
pais(bosnia, europa, 3.5).
pais(bulgaria, europa, 7.1).
pais(chipre, europa, 0.9).
pais(croacia, europa, 4.2).
pais(dinamarca, europa, 5.7).
pais(eslovaquia, europa, 5.4).
pais(eslovenia, europa, 2.1).
pais(espanha, europa, 46.7).
pais(estonia, europa, 1.3).
pais(finlandia, europa, 5.5).
pais(franca, europa, 67).
pais(georgia, europa, 3.7).
pais(grecia, europa, 10.8).
pais(holanda, europa, 17.1).
pais(hungria, europa, 9.8).
pais(irlanda, europa, 4.8).
pais(islandia, europa, 0.3).
pais(italia, europa, 60.6).
pais(kosovo, europa, 1.8).
pais(letonia, europa, 2).
pais(liechtenstein, europa, 0.04).
pais(lituania, europa, 2.8).
pais(luxemburgo, europa, 0.6).
pais(macedonia, europa, 2.1).
pais(malta, europa, 0.5).
pais(moldavia, europa, 3.6).
pais(monaco, europa, 0.04).
pais(montenegro, europa, 0.6).
pais(noruega, europa, 5.3).
pais(polonia, europa, 38.4).
pais(portugal, europa, 10.2).
pais(reinounido, europa, 66).
pais(republicacheca, europa, 10.6).
pais(romenia, europa, 19.6).
pais(russia, europa, 144.5).
pais(servia, europa, 7).
pais(suecia, europa, 10).
pais(suica, europa, 8.4).
pais(turquia, europa, 79.8).
pais(ucrania, europa, 44.8).

fronteira(argentina, bolivia).
fronteira(argentina, brasil).
fronteira(argentina, chile).
fronteira(argentina, paraguai).
fronteira(argentina, uruguai).

fronteira(bolivia, brasil).
fronteira(bolivia, chile).
fronteira(bolivia, paraguai).
fronteira(bolivia, peru).

fronteira(brasil, colombia).
fronteira(brasil, guiana).
fronteira(brasil, guianafrancesa).
fronteira(brasil, paraguai).
fronteira(brasil, peru).
fronteira(brasil, suriname).
fronteira(brasil, uruguai).
fronteira(brasil, venezuela).

fronteira(chile, peru).

fronteira(colombia, equador).
fronteira(colombia, peru).
fronteira(colombia, venezuela).

fronteira(equador, peru).

fronteira(guiana, suriname).
fronteira(guiana, venezuela).

fronteira(guianafrancesa, suriname).

fronteira(albania, grecia).
fronteira(albania, macedonia).
fronteira(albania, montenegro).
fronteira(albania, kosovo).

fronteira(alemanha, austria).
fronteira(alemanha, belgica).
fronteira(alemanha, dinamarca).
fronteira(alemanha, franca).
fronteira(alemanha, holanda).
fronteira(alemanha, luxemburgo).
fronteira(alemanha, polonia).
fronteira(alemanha, republicacheca).
fronteira(alemanha, suica).

fronteira(armenia, georgia).
fronteira(armenia, turquia).

fronteira(austria, eslovaquia).
fronteira(austria, eslovenia).
fronteira(austria, hungria).
fronteira(austria, italia).
fronteira(austria, liechtenstein).
fronteira(austria, republicacheca).
fronteira(austria, suica).

fronteira(belgica, franca).
fronteira(belgica, holanda).
fronteira(belgica, luxemburgo).

fronteira(bielorussia, letonia).
fronteira(bielorussia, lituania).
fronteira(bielorussia, polonia).
fronteira(bielorussia, russia).
fronteira(bielorussia, ucrania).

fronteira(bosnia, croacia).
fronteira(bosnia, montenegro).
fronteira(bosnia, servia).

fronteira(bulgaria, grecia).
fronteira(bulgaria, macedonia).
fronteira(bulgaria, romenia).
fronteira(bulgaria, servia).
fronteira(bulgaria, turquia).

fronteira(croacia, eslovenia).
fronteira(croacia, hungria).
fronteira(croacia, montenegro).
fronteira(croacia, servia).

fronteira(eslovaquia, hungria).
fronteira(eslovaquia, polonia).
fronteira(eslovaquia, republicacheca).
fronteira(eslovaquia, ucrania).

fronteira(eslovenia, hungria).
fronteira(eslovenia, italia).

fronteira(espanha, franca).
fronteira(espanha, portugal).

fronteira(estonia, letonia).
fronteira(estonia, russia).

fronteira(finlandia, noruega).
fronteira(finlandia, russia).
fronteira(finlandia, suecia).

fronteira(franca, italia).
fronteira(franca, luxemburgo).
fronteira(franca, monaco).
fronteira(franca, suica).

fronteira(georgia, russia).
fronteira(georgia, turquia).

fronteira(grecia, macedonia).
fronteira(grecia, turquia).

fronteira(hungria, romenia).
fronteira(hungria, servia).
fronteira(hungria, ucrania).

fronteira(irlanda, reinounido).

fronteira(italia, suica).

fronteira(kosovo, macedonia).
fronteira(kosovo, montenegro).
fronteira(kosovo, servia).

fronteira(letonia, lituania).
fronteira(letonia, russia).

fronteira(liechtenstein, suica).

fronteira(lituania, polonia).
fronteira(lituania, russia).

fronteira(macedonia, servia).

fronteira(moldavia, romenia).
fronteira(moldavia, ucrania).

fronteira(noruega, suecia).
fronteira(noruega, russia).

fronteira(montenegro, servia).

fronteira(polonia, republicacheca).
fronteira(polonia, russia).
fronteira(polonia, ucrania).

fronteira(romenia, servia).
fronteira(romenia, ucrania).

fronteira(russia, ucrania).

% exercicio 3 a)

vizinho(A, B):-
    fronteira(A, B).

% exercicio 3 b)

% Exemplo 1

% o operador \+ da switch do valor do booleano

continentesSemPaises(C):-
    continente(C),
    \+pais(_,C,_).

% Exemplo 2
% o R neste caso é uma flag que é inicializada a 1
% o underscore significa que nao interessa o argumento

continentesSemPaises(C):-
    continente(C),
    verificarSeTemPaises(C, R),
    !,
    R=0.

verificarSeTemPaises(C, 1):-
    pais(_,C,_).
    verificarSeTemPaises(_,0).


% exercicio 3 c)

semVizinhos(L):-
    \+terVizinhos(L).

terVizinhos(L):-
    pais(L,_,_),
    fronteira(L,_);
    fronteira(_,L).

% exercicio 3 d)

chegoLaFacil(P1,P2):-
    fronteira(P1,P2);fronteira(P2,P1);
    fronteira(P1,X),fronteira(X,P2);
    fronteira(P2,X),fronteira(X,P1).

% exercicio 4 a)
% N (numero), P (potencia), R (resultado)

potencia(N, P, R):- N>0,!,potenciaPositiva(N, P, R).

potencia(N, P, R):- N<0,!,potenciaNegativa(N, P, R).
 


potenciaPositiva(N, 1, R):-
    R is N,!. % condicao de paragem

potenciaPositiva(N, P, R):-
    N1 is N - 1,
    potenciaPositiva(N, N1, R1),
    R is N * R1.




potenciaNegativa(N, 1, R):-
    R is 1 / N,!. % condicao de paragem

potenciaNegativa(N, P, R):-
    N1 is N - 1,
    potenciaNegativa(N, N1, R1),
    R is N / R1.



% exercicio 4 c)

