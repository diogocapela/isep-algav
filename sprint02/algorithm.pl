% Linhas

linhas([lA]).

% Maquinas

maquinas([ma,mb,mc,md,me]).

% Ferramentas

ferramentas([fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]).

% Maquinas que constituem as Linhas

tipos_maq_linha(lA,[ma,mb,mc,md,me]).
% ...

% Opera��es

tipo_operacoes([opt1,opt2,opt3,opt4,opt5,opt6,opt7,opt8,opt9,opt10]).

% operacoes/1 vai ser criado dinamicamente
%no exemplo dara' uma lista com 30 operacoes 6 lotes de produtos * 5 operacoes por produto

%operacoes_atrib_maq/2 vai ser criado dinamicamente
%no exemplo cada maquina tera' 6 operacoes atribuidas, uma por cada lote de produtos

% classif_operacoes/2 deve ser criado dinamicamente 
%no exemplo teremos 30 factos deste tipo, um para cada operacao


% Afeta��o de tipos de opera��es a tipos de m�quinas
% com ferramentas, tempos de setup e tempos de execucao)

operacao_maquina(opt1,ma,fa,1,1).
operacao_maquina(opt2,mb,fb,2.5,2).
operacao_maquina(opt3,mc,fc,1,3).
operacao_maquina(opt4,md,fd,1,1).
operacao_maquina(opt5,me,fe,2,3).
operacao_maquina(opt6,mb,ff,1,4).
operacao_maquina(opt7,md,fg,2,5).
operacao_maquina(opt8,ma,fh,1,6).
operacao_maquina(opt9,me,fi,1,7).
operacao_maquina(opt10,mc,fj,20,2).





%...


% PRODUTOS

produtos([pA,pB,pC,pD,pE,pF]).

operacoes_produto(pA,[opt1,opt2,opt3,opt4,opt5]).
operacoes_produto(pB,[opt1,opt6,opt3,opt4,opt5]).
operacoes_produto(pC,[opt1,opt2,opt3,opt7,opt5]).
operacoes_produto(pD,[opt8,opt2,opt3,opt4,opt5]).
operacoes_produto(pE,[opt1,opt2,opt3,opt4,opt9]).
operacoes_produto(pF,[opt1,opt2,opt10,opt4,opt5]).



% ENCOMENDAS

%Clientes

clientes([clA,clB,clC]).


% prioridades dos clientes

prioridade_cliente(clA,2).
prioridade_cliente(clB,1).
prioridade_cliente(clC,3).

% ...

% Encomendas do cliente, 
% termos e(<produto>,<n.unidades>,<tempo_conclusao>)
encomenda(clA,[e(pA,4,50),e(pB,4,70), e(pC,20,500),e(pE,1,450)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200),e(pA,3,100)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120), e(pB,4,100)]).

% ...




% cria_op_enc - fizeram-se correcoes face a versao anterior

:- dynamic operacoes_atrib_maq/2.
:- dynamic classif_operacoes/2.
:- dynamic op_prod_client/9.
:- dynamic operacoes/1.


cria_op_enc:-retractall(operacoes(_)),
retractall(operacoes_atrib_maq(_,_)),retractall(classif_operacoes(_,_)),
retractall(op_prod_client(_,_,_,_,_,_,_,_,_)),
		findall(t(Cliente,Prod,Qt,TConc),
		(encomenda(Cliente,LE),member(e(Prod,Qt,TConc),LE)),
		LT),cria_ops(LT,0),
findall(Op,classif_operacoes(Op,_),LOp),asserta(operacoes(LOp)),
maquinas(LM),
 findall(_,
		(member(M,LM),
		 findall(Opx,op_prod_client(Opx,M,_,_,_,_,_,_,_),LOpx),
		 assertz(operacoes_atrib_maq(M,LOpx))),_).

cria_ops([],_).
cria_ops([t(Cliente,Prod,Qt,TConc)|LT],N):-
			operacoes_produto(Prod,LOpt),
	cria_ops_prod_cliente(LOpt,Cliente,Prod,Qt,TConc,N,N1),
			cria_ops(LT,N1).


cria_ops_prod_cliente([],_,_,_,_,Nf,Nf).
cria_ops_prod_cliente([Opt|LOpt],Client,Prod,Qt,TConc,N,Nf):-
		cria_ops_prod_cliente2(Opt,Prod,Client,Qt,TConc,N,Ni),
	cria_ops_prod_cliente(LOpt,Client,Prod,Qt,TConc,Ni,Nf).


cria_ops_prod_cliente2(Opt,Prod,Client,Qt,TConc,N,Ni):-
			Ni is N+1,
			atomic_concat(op,Ni,Op),
			assertz(classif_operacoes(Op,Opt)),
			operacao_maquina(Opt,M,F,Tsetup,Texec),
	assertz(op_prod_client(Op,M,F,Prod,Client,Qt,TConc,Tsetup,Texec)).



:-cria_op_enc.


	
:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.




% tarefas(NTarefas).
tarefas(10).

% parameteriza��o
inicializa:-write('Numero de novas Geracoes: '),read(NG), 			(retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), 	asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).


gera:-
	inicializa,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	gera_geracao(0,NG,PopOrd).

gera_populacao(Pop):-
	populacao(TamPop),
	tarefas(NumT),
	findall(Tarefa,tarefa(Tarefa,_,_,_),ListaTarefas),
	gera_populacao(TamPop,ListaTarefas,NumT,Pop).

gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,ListaTarefas,NumT,[Ind|Resto]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,ListaTarefas,NumT,Resto),
	gera_individuo(ListaTarefas,NumT,Ind),
	not(member(Ind,Resto)).
gera_populacao(TamPop,ListaTarefas,NumT,L):-
	gera_populacao(TamPop,ListaTarefas,NumT,L).

gera_individuo([G],1,[G]):-!.

gera_individuo(ListaTarefas,NumT,[G|Resto]):-
	NumTemp is NumT + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,ListaTarefas,G,NovaLista),
	NumT1 is NumT-1,
	gera_individuo(NovaLista,NumT1,Resto).

retira(1,[G|Resto],G,Resto).
retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).

avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).

avalia(Seq,V):-
	avalia(Seq,0,V).

avalia([],_,0).
avalia([T|Resto],Inst,V):-
	tarefa(T,Dur,Prazo,Pen),
	InstFim is Inst+Dur,
	avalia(Resto,InstFim,VResto),
	(
		(InstFim =< Prazo,!, VT is 0)
  ;
		(VT is (InstFim-Prazo)*Pen)
	),
	V is VT+VResto.

ordena_populacao(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).


btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).


gera_geracao(G,G,Pop):-!,
    write('Geracaoo '), write(G), write(':'), nl, write(Pop), nl, 
    calcula_media_populacao(Pop, Media),
    write('media = '), write(Media), nl.

gera_geracao(N,G,Pop):-
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	cruzamento(Pop,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	N1 is N+1,
	gera_geracao(N1,G,NPopOrd).

gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	tarefas(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).


cruzamento([],[]).
cruzamento([Ind*_],[Ind]).
cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2|Resto1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
        cruzar(Ind1,Ind2,P1,P2,NInd1),
	  cruzar(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	cruzamento(Resto,Resto1).

preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).


sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	tarefas(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).


elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	tarefas(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).


insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	tarefas(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).


eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).

mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).












devolve_id_tarefas(ListaTarefas):-
    findall(
            Id,
            tarefa(Id,_,_,_),
                ListaTarefas
                ).
    devolve_tarefas(ListaTarefas):-
        findall(
        (Id, T1, T2, P),
            tarefa(Id,T1,T2,P),
            ListaTarefas
            ).
    
    extrai_tarefas():-
        findall(
            Cliente,
            encomenda(Cliente, _),
            ListaClientes
        ),
        cria_tarefas_clientes(ListaClientes, 1).
    
    cria_tarefas_clientes([], _):- !.
    
    cria_tarefas_clientes([Cliente|Resto], Id):-
        cria_tarefas_cliente(Cliente, Id, IdRetorno),
        cria_tarefas_clientes(Resto, IdRetorno).
    
    cria_tarefas_cliente(Cliente, Id, IdRetorno):-
        findall(
            Encomenda,
            encomenda(Cliente, Encomenda),
            ListaEncomendas
            ),
            cria_tarefas_cliente_aux(ListaEncomendas, Cliente, Id, IdRetorno).
    
    cria_tarefas_cliente_aux([[]], _, Id, IdRetorno):- IdRetorno is Id.
    
    cria_tarefas_cliente_aux([[e(Produto, NumeroUnidades, TempoConclusao)|Resto]], Cliente, Id, IdRetorno):-
        cria_tarefa(Cliente, e(Produto, NumeroUnidades, TempoConclusao), TarefaRetorno, Id),
        TarefaRetorno = e(IdTarefa, TempoProc, TempoConc, Peso),
        atomic_concat('t', IdTarefa, IdFinal),
        assertz(tarefa(IdFinal, TempoProc, TempoConc, Peso)),
        Id1 is Id +1,
        cria_tarefas_cliente_aux([Resto], Cliente, Id1, IdRetorno).
    
    %funcao que recebendo uma encomenda retorna uma tarefa
    cria_tarefa(Cliente, e(Produto, NumeroUnidades, TempoConclusao), TarefaRetorno, Id):-
        %encontrar lista de operacoes que constituem a encomenda
        findall(
            Operacao,
            op_prod_client(Operacao, _, _, Produto, _, _, _, _, _),
            ListaOperacoes
        ),
        %calcular makespan da encomenda
        calculaMakeSpanTarefa(ListaOperacoes,  NumeroUnidades, TempoNecessario),!,
        prioridade_cliente(Cliente, Prioridade),
        %criar tuplo correspondente a tarefa
        TarefaRetorno = e(Id, TempoNecessario, TempoConclusao, Prioridade).
    
    
    %funcao que calcula o makespan de uma lista de operacoes
    calculaMakeSpanTarefa(ListaOperacoes, NumeroUnidades, Tempo):-
        %encontrar maior operacao pois esta vai ser multiplicada pelo numero de unidades da encomenda
        encontraTarefaMaiorTempoExecucao(ListaOperacoes, Operacao),
        calculaMakeSpanTarefaAux(ListaOperacoes, Operacao, NumeroUnidades, 0, TempoTemp),
        %encontrar tempo de preparaçao que temos de deduzir ao makespan total
        calculaTempoPreparacao(ListaOperacoes, TempoPrep),
        Tempo is TempoTemp - TempoPrep.
    
    
    %condicao de paragem quando lista esta vazia
    calculaMakeSpanTarefaAux([], _, _, TempoAux, TempoReturn):-
        TempoReturn is TempoAux, !.
    
    %fluxo normal
    calculaMakeSpanTarefaAux([OperacaoAtual|Resto], Operacao, NumeroUnidades, TempoAux, TempoReturn):-
        %descobrir tempo de execucao da funcao
        op_prod_client(OperacaoAtual,_, _, _, _, _, _, _, TempoExecucao),
        %se operacao for igual a operacao com maoior tempo adicionar ao makespan o tempo de execucao dela * numero de unidades pedida
        %caso contrario apenas adicionar tempo de execucao
        (
            (Operacao=OperacaoAtual,
            TempoAux1 is TempoAux + NumeroUnidades*TempoExecucao);
            (TempoAux1 is TempoAux + TempoExecucao)
        ), !,  
        calculaMakeSpanTarefaAux(Resto, Operacao, NumeroUnidades, TempoAux1, TempoReturn).
    
    
    %funcao para encontrar tarefa com maior tempo de execucao numa lista de tarefas
    encontraTarefaMaiorTempoExecucao(ListaOperacoes, Operacao):-
        encontraMaiorTempoExecucaoAux(ListaOperacoes, 0, _, Operacao), !.
    
    %condicao de paragem
    encontraMaiorTempoExecucaoAux([], _, OperacaoAtual, Operacao):- 
        Operacao = OperacaoAtual.
    
    %condicao de paragem
    encontraMaiorTempoExecucaoAux([OperacaoAtual|Resto], TempoExecucaoOperacao, OperacaoMaior, OperacaoResult):-
        op_prod_client(OperacaoAtual, _, _, _, _, _, _, _, Tempo),
        %se tempo for maior que o maior tempo atual, atualizar maior tempo atual na nova chamada a funcao
        (
            (Tempo>TempoExecucaoOperacao,
             encontraMaiorTempoExecucaoAux(Resto, Tempo, OperacaoAtual, OperacaoResult));
             (encontraMaiorTempoExecucaoAux(Resto, TempoExecucaoOperacao, OperacaoMaior, OperacaoResult))
        ).
    
    %calcular tempo de preparacao duma lista de operacoes
    calculaTempoPreparacao(Operacoes, Tempo):-
        calculaTempoPreparacaoAux(Operacoes, 0, [], ListaTempos),
        %dar sort pois queremos o tempo mais pequeno e assim conseguimos acede-lo pois ele fica na head
        sort(ListaTempos, ListaTemposSorted),
        ListaTemposSorted = [Tempo|_].
    
    %condicao de paragem
    calculaTempoPreparacaoAux([], _, ListaTempos, ListaTemposResult):-
        ListaTemposResult = ListaTempos, !.
    
    
    %fluxo normal
    %vamos fazer uma lista com todos os tempos de preparação possiveis para depois podermos escolher o mais baixo
    calculaTempoPreparacaoAux([Operacao|Resto], TempoStack, ListaTempos, ListaTemposResult):-
        op_prod_client(Operacao,_, _, _, _, _, _, TempoSetup, TempoExec),
        TempoAux is TempoStack-TempoSetup,
        TempoStack1 is TempoStack+TempoExec,
        calculaTempoPreparacaoAux(Resto, TempoStack1, [TempoAux|ListaTempos], ListaTemposResult).



    calcula_media_populacao(Pop, Media):- calcula_media_populacao_aux(Pop, 0, 0, Media).

calcula_media_populacao_aux([], Soma, Numero, Resultado):- Resultado is Soma/Numero.

calcula_media_populacao_aux([_*Avaliacao|Resto], Media, Numero, Resultado):-
    Numero1 is Numero +1, 
    Soma is Media+Avaliacao,
    calcula_media_populacao_aux(Resto, Soma, Numero1, Resultado).
