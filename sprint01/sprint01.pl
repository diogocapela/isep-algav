:- dynamic operacoes_atrib_maq/2.
:- dynamic classif_operacoes/2.
:- dynamic op_prod_client/9.
:- dynamic operacoes/1.
% Linhas

linhas([lA]).

% Maquinas

maquinas([ma]).

% Ferramentas

ferramentas([fa,fa,fc]).    

% Maquinas que constituem as Linhas

tipos_maq_linha(lA,[ma]).

% Opera��es

tipo_operacoes([opt1,opt2,opt3]).

% operacoes/1 deve ser criado dinamicamente

operacoes([op1,op2,op3,op4,op5]).

%operacoes_atrib_maq depois deve ser criado dinamicamente
operacoes_atrib_maq(ma,[op1,op2,op3,op4,op5]).

% classif_operacoes/2 deve ser criado dinamicamente %%atomic_concat(op,NumOp,Resultado)
classif_operacoes(op1,opt1).
classif_operacoes(op2,opt2).
classif_operacoes(op3,opt1).
classif_operacoes(op4,opt2).
classif_operacoes(op5,opt3).

% Afeta��o de tipos de opera��es a tipos de m�quinas
% com ferramentas, tempos de setup e tempos de execucao)

operacao_maquina(opt1,ma,fa,5,60).
operacao_maquina(opt2,ma,fb,6,30).
operacao_maquina(opt3,ma,fc,8,40).
operacao_maquina(opt4,ma,fa,8,40).

% PRODUTOS

produtos([pA,pB,pC]).

operacoes_produto(pA,[opt1]).
operacoes_produto(pB,[opt2]).
operacoes_produto(pC,[opt3]).

% ENCOMENDAS

%Clientes

clientes([clA,clB]).

% prioridades dos clientes

prioridade_cliente(clA,1).
prioridade_cliente(clB,2).
% ...

% Encomendas do cliente, 
% termos e(<produto>,<n.unidades>,<tempo_conclusao>)

encomenda(clA,[e(pA,1,100),e(pB,1,100)]).
encomenda(clB,[e(pA,4,110),e(pB,2,150),e(pC,2,300)]).


%CRIACAO DINAMICA
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
	cria_ops_prod_cliente(LOpt,Prod,Cliente,Qt,TConc,N,N1),
			cria_ops(LT,N1).


cria_ops_prod_cliente([],_,_,_,_,Nf,Nf).
cria_ops_prod_cliente([Opt|LOpt],Client,Prod,Qt,TConc,N,Nf):-
		cria_ops_prod_cliente2(Opt,Prod,Client,Qt,TConc,N,Ni),
	cria_ops_prod_cliente(LOpt,Client,Prod,Qt,TConc,Ni,Nf).


cria_ops_prod_cliente2(_,_,_,0,_,Ni,Ni):-!.
cria_ops_prod_cliente2(Opt,Prod,Client,Qt,TConc,N,Ni2):-
			Ni is N+1,
			atomic_concat(op,Ni,Op),
			assertz(classif_operacoes(Op,Opt)),
			operacao_maquina(Opt,M,F,Tsetup,Texec),
	assertz(op_prod_client(Op,M,F,Prod,Client,Qt,TConc,Tsetup,Texec)),
	Qt2 is Qt -1,
	cria_ops_prod_cliente2(Opt,Prod,Client,Qt2,TConc,Ni,Ni2).

:-cria_op_enc.


%HEURISTICA PARA MINIMIZACAO TEMPO OCUPACAO
minimizaTempoOcupacao2(Lop, LR):- 
    minimizaTempoOcupacao(0, Lop, [], [], ListaR),
   reverse(ListaR, LR).

%caso de paragem
minimizaTempoOcupacao(_, [], _, ListaRetorno, ListaRetornoRes):- ListaRetornoRes = ListaRetorno.

%caso inicial
minimizaTempoOcupacao(0, [(Op, Ferr)|Restantes], _, ListaRetorno, ListaRetornoRes):-  
    minimizaTempoOcupacao(Ferr, Restantes, [], [Op|ListaRetorno], ListaRetornoRes), !.

%se ferramenta for a mesma
minimizaTempoOcupacao(Ferramenta, [(Op, Ferr)|Restantes], _, ListaRetorno,  ListaRetornoRes):- 
    Ferramenta = Ferr,  
    minimizaTempoOcupacao(Ferramenta, Restantes, [], [Op|ListaRetorno], ListaRetornoRes), !.

%se ferramenta for diferente e ja tiver visto a lista toda
minimizaTempoOcupacao(_, [(Op, Ferr)|Restantes], ListaVisitados, ListaRetorno, ListaRetornoRes):-
    length([(Op, Ferr)|Restantes], N), length(ListaVisitados, N),
    minimizaTempoOcupacao(Ferr, Restantes, [], [Op|ListaRetorno], ListaRetornoRes), !.

%se ferramenta for diferente e ainda não tiver visto a lista toda
minimizaTempoOcupacao(Ferramenta, [Prox|Restantes], ListaVisitados, ListaRetorno, ListaRetornoRes):-
    append(Restantes, [Prox], ListaRev),
    minimizaTempoOcupacao(Ferramenta, ListaRev, [Prox|ListaVisitados], ListaRetorno, ListaRetornoRes), !.


%HEURISTICA PARA MINIMIZACAO TEMPO DE ATRASO
eddTempoAtrasos(Lista, LR, TempoAtraso, TempoTotal):- 
    sort(Lista, LR),
    calculaTempoTotalEAtraso(LR, TempoAtraso, TempoTotal).

%funcao auxiliar
calculaTempoTotalEAtraso(Lista, TempoAtraso, TempoTotal):- calculaTempoTotalEAtrasoAux(0 ,Lista, 0, 0, TempoAtraso, TempoTotal).

%condicao de paragem
calculaTempoTotalEAtrasoAux(_, [], TempoAtraso, TempoTotal, TempoAtrasoRes, TempoTotalRes ):-
    TempoAtrasoRes is TempoAtraso,
    TempoTotalRes is TempoTotal.

%se ferramenta for igual e houver atraso nesta operacao
calculaTempoTotalEAtrasoAux(Ferramenta, [(Deadline, TempoOcu, _, _, Ferr) | Restantes], TempoAtraso, TempoTotal, TempoAtrasoRes, TempoTotalRes):- 
    Ferramenta = Ferr,
    TempoTotal1 is TempoTotal + TempoOcu,
    TempoTotal1 > Deadline,
    TempoAtrasoTemp is TempoTotal1 - Deadline, 
    TempoAtraso1 is TempoAtraso + TempoAtrasoTemp,
    calculaTempoTotalEAtrasoAux(Ferr, Restantes, TempoAtraso1, TempoTotal1, TempoAtrasoRes, TempoTotalRes), !.

%se ferramenta for igual e nao houver atraso nesta operacao
calculaTempoTotalEAtrasoAux(Ferramenta, [(Deadline, TempoOcu, _, _, Ferr) | Restantes], TempoAtraso, TempoTotal, TempoAtrasoRes, TempoTotalRes):- 
    Ferramenta = Ferr,
    TempoTotal1 is TempoTotal + TempoOcu,
    TempoTotal1 =< Deadline,
    calculaTempoTotalEAtrasoAux(Ferr, Restantes, TempoAtraso, TempoTotal1, TempoAtrasoRes, TempoTotalRes), !.

%se ferramenta não for igual e houver atraso nesta operacao
calculaTempoTotalEAtrasoAux(_, [(Deadline, TempoOcu, TempoPrep, _, Ferr) | Restantes], TempoAtraso, TempoTotal, TempoAtrasoRes, TempoTotalRes):- 
    TempoTotal1 is TempoTotal + TempoOcu+TempoPrep,
    TempoTotal1 > Deadline,
    TempoAtrasoTemp is TempoTotal1 - Deadline, 
    TempoAtraso1 is TempoAtraso + TempoAtrasoTemp,
    calculaTempoTotalEAtrasoAux(Ferr, Restantes, TempoAtraso1, TempoTotal1, TempoAtrasoRes, TempoTotalRes), !.

%se ferramenta não for igual e não houver atraso nesta operacao
calculaTempoTotalEAtrasoAux(_, [(Deadline, TempoOcu, TempoPrep, _, Ferr) | Restantes], TempoAtraso, TempoTotal, TempoAtrasoRes, TempoTotalRes):- 
    TempoTotal1 is TempoTotal + TempoOcu+TempoPrep,
    TempoTotal1 =< Deadline,
    calculaTempoTotalEAtrasoAux(Ferr, Restantes, TempoAtraso, TempoTotal1, TempoAtrasoRes, TempoTotalRes), !.


% permuta/2 gera permuta��es de listas
permuta([ ],[ ]).
permuta(L,[X|L1]):-apaga1(X,L,Li),permuta(Li,L1).

apaga1(X,[X|L],L).
apaga1(X,[Y|L],[Y|L1]):-apaga1(X,L,L1).

% permuta_tempo/3 faz uma permuta��o das opera��es atribu�das a uma maquina e calcula tempo de ocupa��o incluindo trocas de ferramentas




soma_tempos(_,_,[],0).
soma_tempos(Fer,M,[Op|LOp],Tempo):- classif_operacoes(Op,Opt),
	operacao_maquina(Opt,M,Fer1,Tsetup,Texec),
	soma_tempos(Fer1,M,LOp,Tempo1),
	((Fer1==Fer,!,Tempo is Texec+Tempo1);
			Tempo is Tsetup+Texec+Tempo1).

% melhor escalonamento com findall, gera todas as solucoes e escolhe melhor

melhor_escalonamento(M,Lm,Tm):-
				get_time(Ti),
				findall(p(LP,Tempo), 
				permuta_tempo(M,LP,Tempo), LL),
				melhor_permuta(LL,Lm,Tm),
				get_time(Tf),Tcomp is Tf-Ti,
				write('GERADO EM '),write(Tcomp),
				write(' SEGUNDOS'),nl.

melhor_permuta([p(LP,Tempo)],LP,Tempo):-!.
melhor_permuta([p(LP,Tempo)|LL],LPm,Tm):- 							melhor_permuta(LL,LP1,T1),
		((Tempo<T1,!,Tm is Tempo,LPm=LP);(Tm is T1,LPm=LP1)).

melhor_escalonamento1(M,Lm,Tm):-
    get_time(Ti),
    (melhor_escalonamento11(M);true),retract(melhor_sol_to(Lm,Tm)),
    get_time(Tf),Tcomp is Tf-Ti,
    write('GERADO EM '),write(Tcomp),
    write(' SEGUNDOS'),nl.

melhor_escalonamento11(M):- asserta(melhor_sol_to(_,10000)),!,
    permuta_tempo(M,LP,Tempo),
    atualiza(LP,Tempo),
    fail.

permuta_tempo(M,LP,Tempo):- operacoes_atrib_maq(M,L),
permuta(L,LP),soma_tempos(semfer,M,LP,Tempo).

atualiza(LP,T):-melhor_sol_to(_,Tm),
    T<Tm,
    retract(melhor_sol_to(_,_)),
    asserta(melhor_sol_to(LP,T)),!.


%FUNCAO AUXILIAR PARA SELECIONAR O MELHOR TEMPO NUMA LISTA
melhor_tempo(ListaOpTempos, ListaRes, MelhorOpTempo):-
    melhor_tempo_aux(ListaOpTempos, [], 1000000, ListaRes, MelhorOpTempo).

melhor_tempo_aux([], ListaTemp, MelhorTemp, ListaFinal, MelhorFinal):-
    reverse(ListaTemp, ListTemp1),
    ListaFinal = ListTemp1,
    MelhorFinal is MelhorTemp, !.

melhor_tempo_aux([p(Tempo, p(Operacoes))|Restantes], _, MelhorTemp, ListaFinal, MelhorFinal):-
    Tempo<MelhorTemp, !,
    melhor_tempo_aux(Restantes, Operacoes, Tempo, ListaFinal, MelhorFinal).

melhor_tempo_aux([_|Restantes], ListaTemp, MelhorTemp, ListaFinal, MelhorFinal):- !,
    melhor_tempo_aux(Restantes, ListaTemp, MelhorTemp, ListaFinal, MelhorFinal).

%MELJHOR ESCALONAMENTO FEITO POR NOS PARA TEMPO DE ATRASO
melhor_escalonamento_tempo_atraso(ListaOperacoes, ListaOperacoesRes, TempoAtraso):-
    findall( 
        p(Tempo, p(ListaOperacoesTemp)),
        func_intermedia(ListaOperacoes, ListaOperacoesTemp, Tempo),
        ListaOperacoesTemposAtraso),
    melhor_tempo(ListaOperacoesTemposAtraso, ListaOperacoesRes, TempoAtraso).

func_intermedia(Lista, ListaRetorno, Tempo):-
    permuta(Lista, NovaLista), 
    extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao(NovaLista, ListaTotal),
    calculaTempoTotalEAtraso(ListaTotal, Tempo, _),
    extraiOperacaoListaTotal(ListaTotal, ListaRetorno).


%FUNCAO AUXILIAR PARA EXTRAIR INFORMACAO BASEADO NUMA OPERACAO
extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao(ListaOperacoes, ListaRetorno):-
    extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_aux(ListaOperacoes, [], ListaRetorno2),
    reverse(ListaRetorno2, ListaRetorno).

extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_aux([], ListaRetorno, ListaRetornoFinal):- ListaRetornoFinal = ListaRetorno, !.


extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_aux([(Operacao, Deadline)|Resto], ListaRetorno, ListaRetornoFinal):-
    classif_operacoes(Operacao, TipoOperacao),
    operacao_maquina(TipoOperacao, _, Ferramenta, TempoPrep, TempoOcupacao),
    extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_aux(Resto, [(Deadline, TempoOcupacao, TempoPrep, Operacao, Ferramenta)|ListaRetorno], ListaRetornoFinal).

%EXTRAIR  OPETACVAO DUMA LISTA
extraiOperacaoListaTotal(ListaTotal, ListaResultado):-
    extraiOperacaoListaTotalAux(ListaTotal, [], ListaTemp),
    reverse(ListaTemp, ListaResultado).


extraiOperacaoListaTotalAux([], ListaResultadoAux, ListaResultado):- ListaResultado = ListaResultadoAux, !.
extraiOperacaoListaTotalAux([(_, _,_, Operacao,_)|Resto], ListaResultadoAux, ListaResultado):-
    extraiOperacaoListaTotalAux(Resto, [Operacao|ListaResultadoAux], ListaResultado).

%MELHOR ESCALONAMENTO FEITO POR NOS PARA TEMPO DE OCUPACAO
melhor_escalonamento_tempo_ocupacao(ListaOperacoes, ListaOperacoesRes, TempoAtraso):-
    findall( 
        p(Tempo, p(ListaOperacoesTemp)),
        func_intermedia_tempo_ocupacao(ListaOperacoes, ListaOperacoesTemp, Tempo),
        ListaOperacoesTemposAtraso),
    melhor_tempo(ListaOperacoesTemposAtraso, ListaOperacoesRes, TempoAtraso).

func_intermedia_tempo_ocupacao(Lista, ListaRetorno, Tempo):-
    permuta(Lista, NovaLista), 
    extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_tempo_ocupacao(NovaLista, ListaTotal),
    calculaTempoTotalEAtraso(ListaTotal, _, Tempo),
    extraiOperacaoListaTotal(ListaTotal, ListaRetorno).


%FUNCAO AUXILIAR DE EXTRACAO
extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_tempo_ocupacao(ListaOperacoes, ListaRetorno):-
    extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_aux_tempo_ocupacao(ListaOperacoes, [], ListaRetorno2),
    reverse(ListaRetorno2, ListaRetorno).

extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_aux_tempo_ocupacao([], ListaRetorno, ListaRetornoFinal):- ListaRetornoFinal = ListaRetorno, !.


extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_aux_tempo_ocupacao([Operacao|Resto], ListaRetorno, ListaRetornoFinal):-
    classif_operacoes(Operacao, TipoOperacao),
    operacao_maquina(TipoOperacao, _, Ferramenta, TempoPrep, TempoOcupacao),
    extraiFerramenta_TempoOcu_TempoPrep_EOp_De_Operacao_aux_tempo_ocupacao(Resto, [(100, TempoOcupacao, TempoPrep, Operacao, Ferramenta)|ListaRetorno], ListaRetornoFinal).


%A* PARA TEMPODE OCUPACAO
aStarTempoOcupacao(ListaOperacoes, ListaRetorno, Custo):-
	aStar2TempoOcupacao([(_,0,[],ListaOperacoes)], ListaRetorno, Custo).

%CONDICAO DE PARAGEM
aStar2TempoOcupacao([(_, CustoAtual, OperacoesRealizadas, [])|_], ListaRetorno, Custo):-
	Custo is CustoAtual,
	reverse(OperacoesRealizadas, ListaRetorno), !.

%PRIMEIRA EXECUCAO
aStar2TempoOcupacao([(_,0,[],OperacoesFaltam)], Caminho, Custo):-
	findall(
		(CustoEstimadoX, CustoAtualX, [Next], OperacoesFaltamX),
		(
			apaga1(Next, OperacoesFaltam, OperacoesFaltamX),
			op_prod_client(Next,_,Ferr,_,_,_,_,Tsetup,TempoOcu),
			CustoAtualX is TempoOcu+Tsetup,
			findall(
				(Operacao),
				(op_prod_client(Operacao,_,Ferr,_,_,_,_,_,_)),
				(ListaOperacoesMesmaFerramenta)
			),
			subtract(OperacoesFaltamX, ListaOperacoesMesmaFerramenta, ListaEstimativa),
			estimativa(ListaEstimativa, Estimativa),
			CustoEstimadoX is  Estimativa
		),
		Novos
	), 
	sort(Novos, TodosSorted),
	aStar2TempoOcupacao(TodosSorted, Caminho, Custo), !.

%FLOW NORMAL
aStar2TempoOcupacao([(CustoEstimado,CustoAtual,OperacoesRealizadas,OperacoesFaltam)|Outros], Caminho, Custo):-
	
	OperacoesRealizadas =[OperacaoAtual|_], op_prod_client(OperacaoAtual,_,Ferramenta,_,_,_,_,_,_),
	findall(
		(CustoEstimadoX, CustoAtualX, [Next|OperacoesRealizadasX], OperacoesFaltamX),
		(
			apaga1(Next, OperacoesFaltam, OperacoesFaltamX),
			op_prod_client(Next,_,Ferr,_,_,_,_,Tsetup,TempoOcu),
			OperacoesRealizadasX = OperacoesRealizadas,
			((Ferramenta = Ferr, !, 
				CustoAtualX is CustoAtual+TempoOcu);
			(CustoAtualX is CustoAtual+TempoOcu+Tsetup)),
			findall(
				(Operacao),
				(op_prod_client(Operacao,_,Ferr,_,_,_,_,_,_)),
				(ListaOperacoesMesmaFerramenta)
			),
			subtract(OperacoesFaltamX, ListaOperacoesMesmaFerramenta, ListaEstimativa),
			estimativa(ListaEstimativa, Estimativa),
			CustoEstimadoX is CustoEstimado + Estimativa
		),
		Novos
	), 
	append(Novos, Outros, Todos),
	sort(Todos, TodosSorted),
	aStar2TempoOcupacao(TodosSorted, Caminho, Custo).


%ESTIMATIVA PARA TEMPO DE OCUPACAO
estimativa(LOp,Estimativa):-
	findall(p(FOp,Tsetup),
	(member(Op,LOp),op_prod_client(Op,_,FOp,_,_,_,_,Tsetup,_)),
	LFTsetup),
	elimina_repetidos(LFTsetup,L),
	soma_setups(L,Estimativa).

%A* PARA TEMPO DE ATRASO
aStarTempoAtraso(ListaOperacoes, ListaRetorno, Atraso):-
	aStar2TempoAtraso([(_,0,0,[],ListaOperacoes)], ListaRetorno, Atraso).

%CONDICAO DE PARAGEM
aStar2TempoAtraso([_,_, AtrasoAtual, OperacoesRealizadas, []|_], ListaFinal, AtrasoFinal):-
	reverse(OperacoesRealizadas, ListaFinal),
	AtrasoFinal is AtrasoAtual.

%PRIMEIRA EXECUCAO
aStar2TempoAtraso([(_,0,0,[],OperacoesFaltam)], Caminho, Custo):-
	findall(
		(CustoEstimadoX, CustoAtualX, AtrasoAtualX, [Next], OperacoesFaltamX),
		(
			apaga1(Next, OperacoesFaltam, OperacoesFaltamX),
			op_prod_client(Next,_,Ferr,_,_,_,Deadline,Tsetup,TempoOcu),
			CustoAtualX is TempoOcu+Tsetup,
			AtrasoAtualX is Deadline-CustoAtualX,
			estimativaAtraso(OperacoesFaltamX, CustoAtualX, Ferr, Estimativa),
			CustoEstimadoX is  Estimativa
		),
		Novos
	), 
	sort(Novos, TodosSorted),
	aStar2TempoAtraso(TodosSorted, Caminho, Custo), !.

%EXECUCAO NORMAL
aStar2TempoAtraso([(CustoEstimado,CustoAtual, AtrasoAtual, OperacoesRealizadas,OperacoesFaltam)|Outros], Caminho, Custo):-
	OperacoesRealizadas =[OperacaoAtual|_], op_prod_client(OperacaoAtual,_,Ferramenta,_,_,_,_,_,_),
	findall(
		(CustoEstimadoX, CustoAtualX, AtrasoAtualX, [Next|OperacoesRealizadasX], OperacoesFaltamX),
		(
			apaga1(Next, OperacoesFaltam, OperacoesFaltamX),
			OperacoesRealizadasX = OperacoesRealizadas,
			op_prod_client(Next,_,Ferr,_,_,_,Deadline,Tsetup,TempoOcu),
			((Ferramenta = Ferr, !, 
				CustoAtualX is CustoAtual+TempoOcu);
			(CustoAtualX is CustoAtual+TempoOcu+Tsetup)),
			AtrasoAtualX is AtrasoAtual+Deadline-CustoAtualX,
			estimativaAtraso(OperacoesFaltamX, CustoAtualX, Ferr, Estimativa),
			CustoEstimadoX is CustoEstimado + Estimativa
		),
		Novos
	), 
	append(Novos, Outros, Todos),
	sort(Todos, TodosSorted),
	aStar2TempoAtraso(TodosSorted, Caminho, Custo).

%ESTIMATIVA PARA TEMPO DE ATRASO
estimativaAtraso(LOp, TempoAtual, FerramentaAtual, Estimativa):-
	findall(
		p(Atraso),
	 	(
			 member(Operacao,LOp),
			 op_prod_client(Operacao,_,_,_,_,_,Deadline,_,_),
			 calculaAtraso(Operacao, Deadline, FerramentaAtual, TempoAtual, Atraso)
		),
		ListaAtrasos),
		sort(ListaAtrasos, ListaFinal1),
		reverse(ListaFinal1, ListaFinal),
		ListaFinal = [p(Estimativa)|_].

%FUNCAO AUXILIAR PARA ESTIMATIVA DE TEMPO DE ATRASO
calculaAtraso(Operacao, Deadline, FerramentaAtual, TempoAtual, Atraso):-
	op_prod_client(Operacao,_,Ferr,_,_,_,_,Tsetup,TempoOcu),
	((FerramentaAtual=Ferr, !, 
	  Temp is TempoAtual+TempoOcu);
	(Temp  is TempoAtual+TempoOcu+Tsetup)),
	Atraso is Temp-Deadline.

%FUNCOES AUXILIARES
elimina_repetidos([],[]).
elimina_repetidos([X|L],L1):-member(X,L),!,elimina_repetidos(L,L1).
elimina_repetidos([X|L],[X|L1]):-elimina_repetidos(L,L1).

soma_setups([],0).
soma_setups([p(_,Tsetup)|L],Ttotal):- soma_setups(L,T1), Ttotal is Tsetup+T1.