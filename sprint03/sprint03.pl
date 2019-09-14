% Linhas

linhas([lA, lB]).

% Maquinas

maquinas([ma,mb,mc,md,me]).

% Ferramentas

ferramentas([fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]).

% Maquinas que constituem as Linhas

tipo_maquina(mA).
tipo_maquina(mB).
tipo_maquina(mC).
tipo_maquina(mD).
tipo_maquina(mE).


maq_linha(lA,[ma,mb,mc,md,me]).
maq_linha(lB,[mf,mg,mh,mi,mj]).

maq_tipo_maquina(ma,mA).
maq_tipo_maquina(mb,mB).
maq_tipo_maquina(mc,mC).
maq_tipo_maquina(md,mD).
maq_tipo_maquina(me,mE).
maq_tipo_maquina(mf,mA).
maq_tipo_maquina(mg,mB).
maq_tipo_maquina(mh,mC).
maq_tipo_maquina(mi,mD).
maq_tipo_maquina(mj,mE).

% Opera��es

tipo_operacoes([opt1,opt2,opt3,opt4,opt5,opt6,opt7,opt8,opt9,opt10]).

% operacoes/1 vai ser criado dinamicamente
%no exemplo dara' uma lista com 30 operacoes 6 lotes de produtos * 5 operacoes por produto

%operacoes_atrib_maq/2 vai ser criado dinamicamente
%no exemplo cada maquina tera' 6 operacoes atribuidas, uma por cada lote de produtos

% classif_operacoes/2 deve ser criado dinamicamente 
%no exemplo teremos 30 factos deste tipo, um para cada operacao

% Afetacao de tipos de operacoes a tipos de maquinas
% com ferramentas, tempos de setup e tempos de execucao)

operacao_maquina(opt1,mA,fa,1,1).
operacao_maquina(opt2,mB,fb,2.5,2).
operacao_maquina(opt3,mC,fc,1,3).
operacao_maquina(opt4,mD,fd,1,1).
operacao_maquina(opt5,mE,fe,2,3).
operacao_maquina(opt6,mB,ff,1,4).
operacao_maquina(opt7,mD,fg,2,5).
operacao_maquina(opt8,mA,fh,1,6).
operacao_maquina(opt9,mE,fi,1,7).
operacao_maquina(opt10,mC,fj,20,2).

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

% Encomendas do cliente, 
% termos e(<produto>,<n.unidades>,<tempo_conclusao>)

%encomenda(clA,[e(pA,4,50)]).
encomenda(clA,[e(pA,4,50),e(pB,4,70)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200)]).
%encomenda(clC,[e(pE,4,60),e(pF,6,120)]).

% cria_op_enc - fizeram-se correcoes face a versao anterior

:- dynamic operacoes_atrib_maq/2.
:- dynamic classif_operacoes/2.
:- dynamic op_prod_client/9.
:- dynamic operacoes/1.


% op_prod_client(Operacao,Maquina,Ferramenta,Produto,Cliente,Quantidade,TempoConclusao,Temposetup,TempoExec)
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

% ################## CODIGO ALGORITMO GENETICO ###########################
%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################	
geracoes(3).
populacao(1).
prob_cruzamento(0.8).
prob_mutacao(0.1).



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
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	populacao(TamPop),
	(
		(TamPop=1, 
		gera_geracao(NG,NG,PopOrd)
		);
		(gera_geracao(0,NG,PopOrd))
	).	
	

gera_populacao(Pop):-
	populacao(TamPop),
	tarefas(NumT),
	findall(Tarefa,tarefa(Tarefa,_,_,_),ListaTarefas),
	(
		(TamPop = 1,
		ListaTarefas= [Elemento],
		Pop = [Elemento]);
		(gera_populacao(TamPop,ListaTarefas,NumT,Pop))
	).

gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,ListaTarefas,NumT,[Ind|Resto]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,ListaTarefas,NumT,Resto),
	(
		(TamPop = 2,
		criaIndividuoEdd(ListaTarefas, Ind));
		(TamPop = 1,
		criaIndividuoMinSlack(ListaTarefas, Ind));
		(gera_individuo(ListaTarefas,NumT,Ind))
	),
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
		(VT is (InstFim-Prazo)*(6-Pen))
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


apaga_scores_tarefas(ListaTarefasAvaliadas, ListaTarefas):-
	apaga_scores_tarefas_aux(ListaTarefasAvaliadas, [], ListaTarefas).

apaga_scores_tarefas_aux([], ListaTemp, ListaFinal):-
	reverse(ListaTemp, ListaFinal).

apaga_scores_tarefas_aux([Tarefa*_|Resto], ListaTemp, ListaFinal):-
	apaga_scores_tarefas_aux(Resto, [Tarefa|ListaTemp], ListaFinal).


gera_geracao(G,G,Pop):-!,
    write('Geracaoo '), write(G), write(':'), nl, write(Pop), nl, 
    calcula_media_populacao(Pop, Media),
	write('media = '), write(Media), 
	(retract(tarefas_escalonadas(_));
	true),
	apaga_scores_tarefas(Pop, ListaTarefas),
	ListaTarefas = [MelhorLista|_],
	asserta(tarefas_escalonadas(MelhorLista)), !.
	


%###################################################################################
%###################################################################################
%A ESTA FUNCAO FOI ADICIONADO UM RAMDOM PERMUTATION A POPULACAO ORIGINAL DE MODO AOS
%CRUZAMENTOS SEREM ENTRE DOIS INDIVIDUOS ALEATORIOS E NAO SEMPRE ENTRE DOIS SEGUIDOS 
%###################################################################################
%###################################################################################

%aqui foi adicionado o random_permutation, o selecionaDoisMoelhoresIndividuos e o torneioSelecao para selecao da proxima geracao
gera_geracao(N,G,Pop):-
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	random_permutation(Pop, PopPermutada),
	cruzamento(PopPermutada,[], NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	append(PopPermutada, NPopAv, NPopTemporaria),
	list_to_set(NPopTemporaria, NPopCompleta),
	selecionaDoisMelhoresIndividuos(NPopCompleta, Ind1, Ind2),
	apaga1(Ind1, NPopCompleta, NPop11),
	apaga1(Ind2, NPop11, NPop12),
	length(Pop, Numero),
	Numero1 is Numero-2,
	torneioSelecao(Numero1, 0, NPop12, NPopSelecionada),
	append([Ind1, Ind2], NPopSelecionada, NPopFinal),
	ordena_populacao(NPopFinal,NPopOrd),
	N1 is N+1,
	gera_geracao(N1,G,NPopOrd).


calcula_media_populacao(Pop, Media):- calcula_media_populacao_aux(Pop, 0, 0, Media).

calcula_media_populacao([], _, 0, Resultado):- Resultado is 0, !.

calcula_media_populacao_aux([], Soma, Numero, Resultado):- Resultado is Soma/Numero.

calcula_media_populacao_aux([_*Avaliacao|Resto], Media, Numero, Resultado):-
    Numero1 is Numero +1, 
    Soma is Media+Avaliacao,
    calcula_media_populacao_aux(Resto, Soma, Numero1, Resultado).


%###################################################################################
%###################################################################################

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

%funcao modificada de modo a encaixar nas modificacoes do gera_geracao
cruzamento([], PopTemp, PopFinal):- PopFinal = PopTemp.
cruzamento([Ind*_],[], [Ind]).
cruzamento([Ind*_],[Ind], [Ind]).
cruzamento([Ind1*_,Ind2*_|Resto],PopTemp, PopFinal):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	(
	(Pc =< Pcruz,!,
    cruzar(Ind1,Ind2,P1,P2,NInd1),
	cruzar(Ind2,Ind1,P1,P2,NInd2),
	cruzamento(Resto,[NInd1, NInd2 |PopTemp], PopFinal));
	(cruzamento(Resto,PopTemp, PopFinal))
	).

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

%funcao que seleciona os dois melhores individuos de uma lista
selecionaDoisMelhoresIndividuos(Pop, Ind1, Ind2):-
	ordena_populacao(Pop, NPop),
	NPop =  [Ind1, Ind2|_].


%##########################################################################################
%########################## FIM CODIGO ALGORITMO GENETICO #################################
%##########################################################################################
%##########################################################################################
%##########################################################################################

%agenda_maq(ma,[t(TempoInicio,TempoFim,setup,[Ferramenta]), t(TempoInicio,TempoFim,exec,[Operacao,NumeroUnidades,Producto,Cliente,Tarefa]),
%t(20.5,21.5,setup,[fa]), t(21.5,25.5,exec,[op6,4,pB,clA,t2])])

:- dynamic agenda_maq/2.

cria_agenda_maquinas():-
	tarefas_escalonadas(Tarefas_escalonadas),
	cria_agenda_maquinas_aux(Tarefas_escalonadas, 0).

cria_agenda_maquinas_aux([], _):- !.

cria_agenda_maquinas_aux([TarefaAtual|Resto], TempoAtual):-
	selecionaLinhaParaTarefa(TarefaAtual, Linha),
	insereTarefaLinha(TarefaAtual, Linha, TempoAtual, TempoApos),
	write('Tempo Atual '), write(TempoAtual), write('Tempo Apos '), write(TempoApos), nl,
	cria_agenda_maquinas_aux(Resto, TempoApos), !.

insereTarefaLinha(Tarefa, Linha, TempoAtual, TempoApos):-
	devolve_encomenda_com_tarefa(Tarefa, Encomenda), !, 
	devolve_tuplo_operacoes_produto_cliente_com_encomenda(Encomenda, (Cliente, Produto, ListaOperacoes, NumeroUnidades, _)), !,
	calculaTempoPreparacao(ListaOperacoes, TempoPrep),
	(
		(extrai_todos_items_tarefas(Linha, ListaFinal),
		ListaFinal = [],
		TempoStart is TempoAtual-TempoPrep);
		(TempoStart is TempoAtual)
	),
	insereOperacaosProdutoEmAgenda(ListaOperacoes, Produto, Cliente, NumeroUnidades, Tarefa, Linha, TempoStart, TempoApos), !.

insereOperacaosProdutoEmAgenda([], _, _, _, _, _, TempoAtual, TempoApos):- TempoApos is TempoAtual.

insereOperacaosProdutoEmAgenda([Operacao|Resto], Produto, Cliente, NumeroUnidades, Tarefa, Linha, TempoAtual, TempoApos):-
	insereOperacaoProdutoEmAgenda(Operacao, Produto, Cliente, NumeroUnidades, Tarefa, Linha, TempoAtual, TempoAposTemp),
	insereOperacaosProdutoEmAgenda(Resto, Produto, Cliente, NumeroUnidades, Tarefa, Linha, TempoAposTemp, TempoApos), !.

insereOperacaoProdutoEmAgenda(Operacao, Produto, Cliente, NumeroUnidades, Tarefa, Linha, TempoAtual, TempoApos):-
	maq_linha(Linha,ListaMaquinasLinha),!,
	classif_operacoes(Operacao, TipoOperacao),!,
	operacao_maquina(TipoOperacao,TipoMaquina,Ferramenta,TempoSetup,TempoExecucao),!,
	find_maquina_linha(ListaMaquinasLinha, TipoMaquina, Maquina),
	member(Maquina, ListaMaquinasLinha),
	(
		(agenda_maq(Maquina, ListaAgenda));
		(asserta(agenda_maq(Maquina, [])),
			ListaAgenda = [])
	),
	extrai_ultima_ferramenta(ListaAgenda, UltimaFerramenta),
	extrai_ultimo_tempo(Tarefa, UltimoTempoFinish, Linha),
	(
		(UltimoTempoFinish=0,
		UltimoTempo is TempoAtual);
		(UltimoTempo is TempoAtual)
	),
	(
		(Ferramenta = UltimaFerramenta, !,
		TempoFinal is UltimoTempo + TempoExecucao*NumeroUnidades,
		TempoFim is UltimoTempo,
		ListaAgendaTemp = ListaAgenda);
		(TempoFim is UltimoTempo,
		TempoInicio is UltimoTempo -TempoSetup,
		append(ListaAgenda, [t(TempoInicio,UltimoTempo,setup,[Ferramenta])], ListaAgendaTemp), 
		TempoFinal is TempoFim + TempoExecucao*NumeroUnidades)
	),
	(
		(TempoFinal =< UltimoTempoFinish,
	TempoFinalExecucao is UltimoTempoFinish + TempoExecucao);
		(TempoFinalExecucao is TempoFinal)
	),
	write('OPERACAO '), write(Operacao), write('UltimoTempoFinish '), write(UltimoTempoFinish),
	write('UltimoTempo '), write(UltimoTempo),
	write('TEMPO ATUAL '), write(TempoAtual), nl,
	append(ListaAgendaTemp, [t(TempoAtual,TempoFinalExecucao,exec,[Operacao,NumeroUnidades,Produto,Cliente,Tarefa])], NovaListaAgenda),
	((retractall(agenda_maq(Maquina, _)), !);
	true),
	asserta(agenda_maq(Maquina, NovaListaAgenda)), 
	TempoApos is TempoFim +TempoExecucao, !.


find_maquina_linha([MaquinaAtual|Resto], TipoMaquina, Maquina):-
	(
		(maq_tipo_maquina(MaquinaAtual, TipoMaquina),
		Maquina = MaquinaAtual);
		(find_maquina_linha(Resto, TipoMaquina, Maquina))
	).



extrai_todos_items_tarefas(Linha, ListaFinal):-
	maq_linha(Linha,ListaMaquinasLinha),!,
	extrai_todos_items_tarefas_aux(ListaMaquinasLinha, [], ListaFinal),!.

extrai_todos_items_tarefas_aux([], ListaTemp, ListaFinal):-
	ListaFinal = ListaTemp,!.

extrai_todos_items_tarefas_aux([Maquina|Resto], ListaTemp, ListaFinal):-
	(
		(
		agenda_maq(Maquina, ListaItems), 
		append(ListaTemp, ListaItems, NovaLista),
		extrai_todos_items_tarefas_aux(Resto, NovaLista, ListaFinal)
		);
		(extrai_todos_items_tarefas_aux(Resto, ListaTemp, ListaFinal))
	), !.

extrai_ultimo_tempo(Tarefa, UltimoTempoFinish, Linha):-
	extrai_todos_items_tarefas(Linha, ListaOperacoesAgenda),
	extrai_ultimo_tempo_aux(ListaOperacoesAgenda, Tarefa, 0, UltimoTempoTemp, 0, Operacao),
	(
	(Operacao =0, 
	UltimoTempoFinish = 0);
	
	(UltimoTempoFinish is UltimoTempoTemp)
	).

extrai_ultimo_tempo_aux([[]], _, 0, UltimoTempo, 0, Operacao):-
	UltimoTempo is 0,
	Operacao is 0.

extrai_ultimo_tempo_aux([], _, UltimoTempoTemp, UltimoTempo, OperacaoTemp, Operacao):-
	UltimoTempo is UltimoTempoTemp,
	Operacao = OperacaoTemp.

extrai_ultimo_tempo_aux([ItemAgenda|Resto], Tarefa, UltimoTempoTemp, UltimoTempo, OperacaoTemp, Operacao):-
	(
		
		(ItemAgenda =t(_, TempoFinal, exec, [OperacaoNova,_,_,_, Tarefa]),
			(
				(TempoFinal>UltimoTempoTemp,
				extrai_ultimo_tempo_aux(Resto, Tarefa, TempoFinal, UltimoTempo, OperacaoNova, Operacao));
				(extrai_ultimo_tempo_aux(Resto, Tarefa, UltimoTempoTemp, UltimoTempo, OperacaoTemp, Operacao))
			)
		);
		(extrai_ultimo_tempo_aux(Resto, Tarefa, UltimoTempoTemp, UltimoTempo, OperacaoTemp, Operacao))
	).

	
extrai_ultima_ferramenta([], UltimaFerramenta):-
	(
		(\+not_inst(UltimaFerramenta), !);
		UltimaFerramenta is 0
	).

extrai_ultima_ferramenta([Atual|Resto], UltimaFerramenta):-
	(
		(Atual = t(_,_,setup,[UltimaFerramenta]), !);
		(Atual = t(_,_,exec,_))
	),
	extrai_ultima_ferramenta(Resto, UltimaFerramenta).

%predicado para ver se variavel esta instanciado
not_inst(Var):-
	\+(\+(Var=0)),
	\+(\+(Var=1)).


selecionaLinhaParaTarefa(Tarefa, Linha):-
	devolve_encomenda_com_tarefa(Tarefa, Encomenda), 
	devolve_tuplo_operacoes_produto_cliente_com_encomenda(Encomenda, (_, _, ListaOperacoes, _, _)),
	transforma_lista_operacoes_em_tipo_operacao(ListaOperacoes, ListaTipoOperacoes),
	calculaLinhasQuePodemFazerTarefa(ListaTipoOperacoes, ListaLinhas),
	selecionaLinhaDeLista(ListaLinhas, Linha).

selecionaLinhaDeLista(ListaLinhas, Linha):-
	selecionaLinhaDeLista_aux(ListaLinhas, lA, 100000000, Linha).

selecionaLinhaDeLista_aux([], MelhorLinhaAtual, _, Linha):-
	Linha = MelhorLinhaAtual.

selecionaLinhaDeLista_aux([LinhaAtual|Resto], MelhorLinhaAtual, MelhorTempo, Linha):-
	calculaMakespanLinha(LinhaAtual, Makespan),
	(
		(Makespan<MelhorTempo,
		selecionaLinhaDeLista_aux(Resto, LinhaAtual, Makespan, Linha));
		(selecionaLinhaDeLista_aux(Resto, MelhorLinhaAtual, MelhorTempo, Linha))
	).

calculaMakespanLinha(Linha, Makespan):-
	maq_linha(Linha, Maquinas),
	calculaMaiorMakespanMaquinas(Maquinas, 0, Makespan).

calculaMaiorMakespanMaquinas([], MaiorTempo, Makespan):-
	Makespan is MaiorTempo.

calculaMaiorMakespanMaquinas([Maquina|Resto], MaiorTempo, Makespan):-
	calculaMakespanMaquina(Maquina, Tempo),
	(
		(Tempo>MaiorTempo,
		calculaMaiorMakespanMaquinas(Resto, Tempo, Makespan));
		(calculaMaiorMakespanMaquinas(Resto, MaiorTempo, Makespan))
	).

calculaMakespanMaquina(Maquina, Tempo):-
	(
		(agenda_maq(Maquina, ListaAgenda),
		calculaMakespanMaquina_aux(ListaAgenda, Tempo));
		(Tempo is 0)
	).

calculaMakespanMaquina_aux([t(_,TempoFinal,exec,_)], Tempo):-
	Tempo is TempoFinal, !.

calculaMakespanMaquina_aux([_|Resto], Tempo):-
	calculaMakespanMaquina_aux(Resto, Tempo).





calculaLinhasQuePodemFazerTarefa(ListaTipoOperacoes, LinhasPossiveis):-
	linhas(LinhasExistentes),
	calculaLinhasQuePodemFazerTarefa_aux(ListaTipoOperacoes, ListaTipoOperacoes, LinhasExistentes, [], LinhasPossiveis).


calculaLinhasQuePodemFazerTarefa_aux(_, _, [], LinhasPossiveisTemp, LinhasPossiveis):-
	LinhasPossiveisTemp = LinhasPossiveis.

calculaLinhasQuePodemFazerTarefa_aux(ListaTipoOperacaoes, [], [LinhaAtual|RestoLinhas], LinhasPossiveisTemp, LinhasPossiveis):-
	calculaLinhasQuePodemFazerTarefa_aux(ListaTipoOperacaoes, ListaTipoOperacaoes, RestoLinhas, [LinhaAtual|LinhasPossiveisTemp], LinhasPossiveis).

calculaLinhasQuePodemFazerTarefa_aux(ListaTipoOperacoes, [TipoOperacao|RestoOperacoes], [LinhaAtual|RestoLinhas], LinhasPossiveisTemp, LinhasPossiveis):-
	maq_linha(LinhaAtual, Maquinas),
transforma_lista_maquina_em_tipo_maquina(Maquinas, TiposMaquina),
	(
		(possibilidadeFazerTipoOperacaoEmListaTipoMaquina(TiposMaquina, TipoOperacao),
		calculaLinhasQuePodemFazerTarefa_aux(ListaTipoOperacoes, RestoOperacoes, [LinhaAtual|RestoLinhas], LinhasPossiveisTemp, LinhasPossiveis));
		(calculaLinhasQuePodemFazerTarefa_aux(ListaTipoOperacoes, ListaTipoOperacoes, RestoLinhas, LinhasPossiveisTemp, LinhasPossiveis))
	).

possibilidadeFazerTipoOperacaoEmListaTipoMaquina([TipoMaquina|Resto], TipoOperacao):-
	(
		(operacao_maquina(TipoOperacao,TipoMaquina,_,_,_), !);
		(possibilidadeFazerTipoOperacaoEmListaTipoMaquina(Resto, TipoOperacao))
	).

possibilidadeFazerTipoOperacaoEmListaTipoMaquina([], _):-
	false.


transforma_lista_maquina_em_tipo_maquina(ListaMaquinas, ListaTipoMaquina):-
	transforma_lista_maquina_em_tipo_maquina_aux(ListaMaquinas, [], ListaTipoMaquina).

transforma_lista_maquina_em_tipo_maquina_aux([], ListaTemp, ListaTipoMaquina):-
	ListaTipoMaquina = ListaTemp.

transforma_lista_maquina_em_tipo_maquina_aux([Maquina|Resto], ListaTemp, ListaTipoMaquina):-
	maq_tipo_maquina(Maquina,TipoMaquina),
	transforma_lista_maquina_em_tipo_maquina_aux(Resto, [TipoMaquina|ListaTemp], ListaTipoMaquina).


transforma_lista_operacoes_em_tipo_operacao(ListaOperacoes, ListaTipoOperacao):-
	transforma_lista_operacoes_em_tipo_operacao_aux(ListaOperacoes, [], ListaTipoOperacao).

transforma_lista_operacoes_em_tipo_operacao_aux([], ListaTemp, ListaTipoOperacao):-
	ListaTipoOperacao = ListaTemp.

transforma_lista_operacoes_em_tipo_operacao_aux([Operacao|Resto], ListaTemp, ListaTipoOperacao):-
	classif_operacoes(Operacao,TipoOperacao),
	transforma_lista_operacoes_em_tipo_operacao_aux(Resto, [TipoOperacao|ListaTemp], ListaTipoOperacao).



%##########################################################################################
%########################## INICIO EXTRACAO TAREFA ########################################
%##########################################################################################
%##########################################################################################
%##########################################################################################


/*Extração das tarefas a usar no Algoritmo Genético (AG) a partir dos dados do sistema
produtivo (linha, máquinas, ferramentas, produtos, clientes, encomendas, prioridades, tempos
de setup/processamento/conclusão, etc). As tarefas corresponderão ao fabrico de lotes de N
produtos iguais associados aos pedidos de fabrico de produtos indicados nas encomendas. As
tarefas deverão ser caraterizadas por um identificador, o respetivo makespan (tempo de
processamento do lote de N produtos na linha), incluindo o tempo de setup necessário, o tempo
de conclusão em fábrica e o peso de penalização de atraso (wj) que deverá estar relacionado
com a prioridade do cliente. Deverá também ser identificado o nº de lotes associados às
encomendas, ou seja, o nº de tarefas.*/

% tarefa(Id,TempoProcessamento,TempConc,PesoPenalizacao).
% op_prod_client(Operacao,Maquina,Ferramenta,Produto,Cliente,Quantidade,TempoConclusao,Temposetup,TempoExec)
%EncomendaProduto é um tuplo  e(<produto>,<n.unidades>,<tempo_conclusao>)

%funcao que devolce uma lista com o id de todas as tarefas existentes
devolve_id_tarefas(ListaTarefas):-
findall(
		Id,
		tarefa(Id,_,_,_),
		ListaTarefas
		).

%funcao que devolve todas as tarefas existentes no sistema
devolve_tarefas(ListaTarefas):-
	findall(
	(Id, T1, T2, P),
	tarefa(Id,T1,T2,P),
	ListaTarefas
	).


%funcao usada para extrair tarefas e carrega las no sistema a partir das encomendas

devolve_lista_operacoes_produto_cliente_com_encomendas(ListaEncomendas, ListaOperacoesProdutoCliente):-
	devolve_lista_operacoes_produto_cliente_com_encomendas_aux(ListaEncomendas, [], ListaOperacoesProdutoCliente).

devolve_lista_operacoes_produto_cliente_com_encomendas_aux([], ListaTemp, ListaOperacoesProdutoCliente):-
	reverse(ListaTemp, ListaOperacoesProdutoCliente).

devolve_lista_operacoes_produto_cliente_com_encomendas_aux([Encomenda|Resto], ListaTemp, ListaOperacoesProdutoCliente):-
	devolve_tuplo_operacoes_produto_cliente_com_encomenda(Encomenda, OperacoesProdutoCliente),
	devolve_lista_operacoes_produto_cliente_com_encomendas_aux(Resto, [OperacoesProdutoCliente|ListaTemp], ListaOperacoesProdutoCliente).

devolve_tuplo_operacoes_produto_cliente_com_encomenda((Cliente, Produto, NumeroUnidades, Deadline), OperacoesProdutoCliente):-
	findall(
        Operacao,
        op_prod_client(Operacao, _, _, Produto, _, _, _, _, _),
        ListaOperacoes
	),
	OperacoesProdutoCliente = (Cliente, Produto, ListaOperacoes, NumeroUnidades, Deadline).


devolve_encomendas_com_lista_tarefas(ListaTarefas, ListaEncomendas):-
	devolve_encomendas_com_lista_tarefas_aux(ListaTarefas, [], ListaEncomendas).

devolve_encomendas_com_lista_tarefas_aux([], ListaTemp, ListaEncomendas):-
	reverse(ListaTemp, ListaEncomendas).

devolve_encomendas_com_lista_tarefas_aux([Tarefa|Resto], ListaTemp, ListaEncomendas):-
	devolve_encomenda_com_tarefa(Tarefa, Encomenda),
	devolve_encomendas_com_lista_tarefas_aux(Resto, [Encomenda|ListaTemp], ListaEncomendas).

devolve_encomenda_com_tarefa(IdTarefa, Encomenda):-
	tarefa_produto_cliente(IdTarefa, Cliente, NumeroUnidades, Produto, Deadline), 
	Encomenda = (Cliente, Produto, NumeroUnidades, Deadline).

extrai_tarefas():-
	findall(
		Cliente,
		encomenda(Cliente, _),
		ListaClientes
	),
	cria_tarefas_clientes(ListaClientes, 1), 
	devolve_id_tarefas(L),
	length(L, N), 
	asserta(tarefas(N)).

%conducao paragem
cria_tarefas_clientes([], _):- !.

%fluxo normal, vai criar as tarefas de um cliente todas, um de cada vez
cria_tarefas_clientes([Cliente|Resto], Id):-
	cria_tarefas_cliente(Cliente, Id, IdRetorno),
	cria_tarefas_clientes(Resto, IdRetorno).

%cria todas as tarefas de um cliente
cria_tarefas_cliente(Cliente, Id, IdRetorno):-
	findall(
		Encomenda,
		encomenda(Cliente, Encomenda),
		ListaEncomendas
		),
		cria_tarefas_cliente_aux(ListaEncomendas, Cliente, Id, IdRetorno).

%condicao paragem
cria_tarefas_cliente_aux([[]], _, Id, IdRetorno):- IdRetorno is Id.

%fluxoo normal
cria_tarefas_cliente_aux([[e(Produto, NumeroUnidades, TempoConclusao)|Resto]], Cliente, Id, IdRetorno):-
	cria_tarefa(Cliente, e(Produto, NumeroUnidades, TempoConclusao), TarefaRetorno, Id),
	TarefaRetorno = e(IdTarefa, TempoProc, TempoConc, Peso),
	atomic_concat('t', IdTarefa, IdFinal),
	assertz(tarefa(IdFinal, TempoProc, TempoConc, Peso)),
	assertz(tarefa_produto_cliente(IdFinal, Cliente, NumeroUnidades, Produto, TempoConclusao)),
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


%##########################################################################################
%########################## FIM EXTRACAO TAREFA ###########################################
%##########################################################################################
%##########################################################################################
%##########################################################################################




%##########################################################################################
%######## EDD PARA GERACAO DE SOLUCAO BASEADA NA SOMA PESADA DOS TEMPOS DE ATRASO #########
%##########################################################################################
%##########################################################################################
%##########################################################################################

%tarefa(Id,TempoProcessamento,TempConc,PesoPenalizacao)

%criar individuo edd
criaIndividuoEdd(ListaTarefas, Individuo):-
	criaListaTuplosTarefasComTarefas(ListaTarefas, [], ListaTuplos),
	edd_somaPesada(ListaTuplos, ListaTemp),
	criaListaTarefasComListaTuplosTarefas(ListaTemp, [], ListaOrdenada),
	reverse(ListaOrdenada, Individuo).
	
%funcoes auxiliares
criaListaTuplosTarefasComTarefas([], ListaTemp, ListaFinal):-
	ListaFinal = ListaTemp.

criaListaTuplosTarefasComTarefas([Tarefa|Resto], ListaTemp, ListaFinal):-
	tarefa(Tarefa, TempoProcessamento, TempoConc, PesoPenalizacao),
	criaListaTuplosTarefasComTarefas(Resto, [(Tarefa, TempoProcessamento, TempoConc, PesoPenalizacao)|ListaTemp], ListaFinal).

criaListaTarefasComListaTuplosTarefas([], ListaTemp, Result):-
	Result = ListaTemp.

criaListaTarefasComListaTuplosTarefas([(Tarefa, _, _, _)|Resto], ListaTemp, Result):-
	criaListaTarefasComListaTuplosTarefas(Resto, [Tarefa|ListaTemp], Result).
%fim funcoes auxiliares

%funcao edd
edd_somaPesada(ListaTarefas, Solucao):-
	edd_somaPesadaAux(ListaTarefas, [], Solucao).

%condicao paragem
edd_somaPesadaAux([], ListaIntermedia, Solucao):-
	sort(ListaIntermedia, ListaSorted),
	removeDealinePonderadaDeTarefas(ListaSorted, [], Solucao), !.

%fluxo normal
edd_somaPesadaAux([(Id, TempoProcessamento, TempoConclusao, Peso)|Resto], ListaIntermedia, Solucao):-
	PesoPonderado is (9+Peso)/10,
	DeadlinePonderada is TempoConclusao/PesoPonderado,
	edd_somaPesadaAux(Resto, [(DeadlinePonderada, Id, TempoProcessamento, TempoConclusao, Peso)|ListaIntermedia], Solucao).

%funcoes auxiliares
removeDealinePonderadaDeTarefas([], ListaTemp, ListaFinal):-
	reverse(ListaTemp, ListaFinal), !.


removeDealinePonderadaDeTarefas([(_, Id, TempoProcessamento, TempoConclusao, Peso)|Resto], ListaTemp, ListaFinal):-
	removeDealinePonderadaDeTarefas(Resto, [(Id, TempoProcessamento, TempoConclusao, Peso)|ListaTemp], ListaFinal).


%##########################################################################################
%###################################### FIM EDD ###########################################
%##########################################################################################
%##########################################################################################
%##########################################################################################



%##########################################################################################
%############################## MINIMUM SLACK TIME ########################################
%########## PARA GERACAO DE SOLUCAO BASEADA NA SOMA PESADA DOS TEMPOS DE ATRASO ###########
%##########################################################################################
%##########################################################################################
%##########################################################################################


%########################## FUNCIONAMENTO MINIMUM SLACK TIME ##############################
% Select the waiting operation associated with the job that has minimum slack time.

% Slack time is equal to the difference between the due date and the earlies possible
% finish time of the job

%criar individuo min slack
criaIndividuoMinSlack(ListaTarefas, Individuo):-
	criaListaTuplosTarefasComTarefas(ListaTarefas, [], ListaTuplos),
	min_slack_pesado(ListaTuplos, ListaTemp),
	criaListaTarefasComListaTuplosMinSlack(ListaTemp, [], Individuo), !.


%calcular minslack
min_slack_pesado(ListaTarefas, Solucao):-
	min_slack_aux(ListaTarefas, [], ListaTarefas, 0, [], Solucao).

%selecionar individuo, chamar funcao para calcular proximo
min_slack_aux([], ListaIntermedia, Restantes, TempoAtual, SolucaoTemp, Solucao):-
	sort(ListaIntermedia, ListaSorted),
	ListaSorted = [(Slack, Id, TempoProcessamento, TempoConclusao, Peso)|_],
	TempoAtual1 is TempoAtual + TempoProcessamento,
	delete(Restantes,(Id, TempoProcessamento, TempoConclusao, Peso), Restantes1),
	(
		(Restantes1 = [],
		Solucao = [(Slack, Id, TempoProcessamento, TempoConclusao, Peso)|SolucaoTemp] );
		(min_slack_aux(Restantes1, [], Restantes1, TempoAtual1, [(Slack, Id, TempoProcessamento, TempoConclusao, Peso)|SolucaoTemp], Solucao))
	).

%fluxo normal onde se calacular slacktimes ponderados
min_slack_aux([(Id, TempoProcessamento, TempoConclusao, Peso)|Resto], ListaIntermedia, Restantes, TempoAtual, SolucaoTemp, Solucao):-
	PesoPonderado is (9+Peso)/10,
	SlackTime is TempoConclusao - (TempoProcessamento  + TempoAtual),
	SlackTimePonderado is SlackTime + 2/PesoPonderado,
	min_slack_aux(Resto, [(SlackTimePonderado,  Id, TempoProcessamento, TempoConclusao, Peso)|ListaIntermedia], Restantes, TempoAtual, SolucaoTemp, Solucao).
	

%funcoes auxiliares
criaListaTarefasComListaTuplosMinSlack([], ListaTemp, Result):-
	Result = ListaTemp.

criaListaTarefasComListaTuplosMinSlack([(_, Tarefa, _, _, _)|Resto], ListaTemp, Result):-
	criaListaTarefasComListaTuplosMinSlack(Resto, [Tarefa|ListaTemp], Result).


%##########################################################################################
%############################## MINIMUM SLACK TIME ########################################
%########## PARA GERACAO DE SOLUCAO BASEADA NA SOMA PESADA DOS TEMPOS DE ATRASO ###########
%##########################################################################################
%##########################################################################################
%##########################################################################################

%funcao que faz torneio entre individuos
torneioSelecao(NumeroFinal, NumeroFinal, _, ListaRetorno):-
	ListaRetorno = [], !.

%selecionar qual vai se rfluxo, se dobro se menor que dobro, se mais que dobro
torneioSelecao(NumeroFinal, NumeroAtual, ListaIndividuos, ListaRetorno):-
	NumeroTemp is NumeroFinal * 2,
	NumeroAtual >= NumeroTemp,
	selecionaMelhoresIndividuos(NumeroTemp, ListaIndividuos, NovaLista),
	random_permutation(NovaLista, ListaPermutada),
	torneioSelecaoDobro(ListaPermutada, [], ListaRetorno), !.

torneioSelecao(NumeroFinal, NumeroAtual, ListaIndividuos, ListaRetorno):-
	NumeroTemp is NumeroFinal * 2,
	NumeroAtual < NumeroTemp,
	torneioSelecaoMenorQueDobro(NumeroFinal, 0, ListaIndividuos, [], ListaRetorno, []), !.


%funcao que faz torneio quando temos um numero de individuos menor do que o dobro do que queremos selecionar
%condicao de paregem
torneioSelecaoMenorQueDobro(NumeroFinal, NumeroFinal, _, ListaTemp, ListaFinal, _):-
	ListaFinal = ListaTemp, !.

%quando ja nao ha mais individuos pomos os excluidos ate agora como excluidos
torneioSelecaoMenorQueDobro(NumeroFinal, NumeroAtual, [], ListaTemp, ListaFinal, Excluidos):-
	torneioSelecaoMenorQueDobro(NumeroFinal, NumeroAtual, Excluidos, ListaTemp, ListaFinal, []), !.

%quando so resta um individuo, ele passa automaticamente
torneioSelecaoMenorQueDobro(NumeroFinal, NumeroAtual, [Individuo], ListaTemp, ListaFinal, Excluidos):-
	NumeroAtual1 is NumeroAtual+1,
	torneioSelecaoMenorQueDobro(NumeroFinal, NumeroAtual1, [], [Individuo|ListaTemp], ListaFinal, Excluidos), !.	

%fluxo normal de execucao
torneioSelecaoMenorQueDobro(NumeroFinal, NumeroAtual, ListaIndividuos, ListaTemp, ListaFinal, Excluidos):-
	ListaIndividuos = [Primeiro|Resto], 
	Resto = [Segundo|Restante],
	torneioEntreIndividuos(Primeiro, Segundo, Escolhido),
	NumeroAtual1 is NumeroAtual+1,
	(
		(Primeiro = Escolhido,
		Out = Segundo);
		(Segundo = Escolhido,
		Out = Primeiro)
	),
	torneioSelecaoMenorQueDobro(NumeroFinal, NumeroAtual1, Restante, [Escolhido|ListaTemp], ListaFinal, [Out|Excluidos]), !.

%funcao que faz torneio quando temos exatamento o dobro dos individuos que pretendemos selecionar
torneioSelecaoDobro([], ListaTemp, ListaFinal):-
	ListaFinal = ListaTemp.

torneioSelecaoDobro(ListaIndividuos, ListaTemp, ListaFinal):-
	ListaIndividuos = [Proximo|Resto],
	Resto = [Seguinte|Restante],
	torneioEntreIndividuos(Proximo, Seguinte, Escolhido),
	torneioSelecaoDobro(Restante, [Escolhido|ListaTemp], ListaFinal).



%funcao que seleciona N melhores individuos de uma lista atraves de torneios
selecionaMelhoresIndividuos(NumeroIndividuos, Lista, NovaLista):-
	%gerar probabilidades de passares
	geraProbabilidades(Lista, [], ListaProb),
	sort(ListaProb, ListaSorted),
	selecionaMelhoresIndividuosAux(0, NumeroIndividuos, ListaSorted, [], NovaLista).

%funcao que seleciona os melhores individuos ja com probabilidades
selecionaMelhoresIndividuosAux(NumeroAtual, NumeroAtual, _, ListaTemp, NovaLista):- NovaLista = ListaTemp.

selecionaMelhoresIndividuosAux(NumeroAtual, NumeroMaximo, [(_, Individuo*Avaliacao)|Resto], ListaTemp, NovaLista):-
	NumeroAtual1 is NumeroAtual+1,
	selecionaMelhoresIndividuosAux(NumeroAtual1, NumeroMaximo, Resto, [(Individuo*Avaliacao)|ListaTemp], NovaLista).

geraProbabilidades([], ListaTemp, ListaFinal):- ListaFinal is ListaTemp.

geraProbabilidades([Individuo*Avaliacao|Resto], ListaTemp, ListaProb):-
	random(0, Avaliacao, Prob),
	geraProbabilidades(Resto, [(Prob, Individuo*Avaliacao)|ListaTemp], ListaProb).


%funcao que faz um torneio entre dois individuos
torneioEntreIndividuos((Individuo1*Avaliacao1), (Individuo2*Avaliacao2), IndividuoFinal):-
	%gerar numero entre 0 e peso do individuo
	random(0, Avaliacao1, Prob1),
	random(0, Avaliacao2, Prob2),
	%selecionar individuo
	(
	(Prob1>=Prob2,
	IndividuoFinal = Individuo2*Avaliacao2, !);
	(IndividuoFinal = Individuo1*Avaliacao1), !
	).



%##########################################################################################
%##########################################################################################
%################################# MELHOR ESCALONAMENTO ###################################
%##########################################################################################
%##########################################################################################
%##########################################################################################


apaga1(X,[X|L],L).
apaga1(X,[Y|L],[Y|L1]):-apaga1(X,L,L1).


%Melhor escalonamento adaptado com pesos
	melhor_escalonamento11(ListaTarefas):- asserta(melhor_sol_to(_,10000)),!,
	permuta_tempo(ListaTarefas,NovaLista,Tempo),
	atualiza(NovaLista,Tempo),
	fail.

permuta_tempo(Lista, ListaNova, Avaliacao):- 
	permuta(Lista, ListaNova),
	avalia(ListaNova, Avaliacao).


melhor_escalonamento(ListaFinal, Avaliacao):-
	get_time(TempoAtual),
	devolve_id_tarefas(ListaTarefas),
	write(ListaTarefas),nl,
	(melhor_escalonamento11(ListaTarefas);true),retract(melhor_sol_to(ListaFinal,Avaliacao)),
	get_time(TempoFinal),Tcomp is TempoFinal-TempoAtual,
	write('GERADO EM '),write(Tcomp),
	write(' SEGUNDOS'),nl.


melhor_permuta([p(LP,Tempo)],LP,Tempo):-!.
melhor_permuta([p(LP,Tempo)|LL],LPm,Tm):- 							
melhor_permuta(LL,LP1,T1),
((Tempo<T1,!,Tm is Tempo,LPm=LP);(Tm is T1,LPm=LP1)).


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

soma_tempos(_,_,[],0).
soma_tempos(Fer,M,[Op|LOp],Tempo):- classif_operacoes(Op,Opt),
	operacao_maquina(Opt,M,Fer1,Tsetup,Texec),
	soma_tempos(Fer1,M,LOp,Tempo1),
	((Fer1==Fer,!,Tempo is Texec+Tempo1);
			Tempo is Tsetup+Texec+Tempo1).


permuta([ ],[ ]).
permuta(L,[X|L1]):-apaga1(X,L,Li),permuta(Li,L1).


extrai_agenda_maquina_individual(Maquina, Agenda):-
	agenda_maq(Maquina, Agenda).

extrai_agenda_maquinas(Agenda):-
	findall(Maquina,
		agenda_maq(Maquina, _),
	ListaMaquinas),
	extrai_agenda_maquinas_aux(ListaMaquinas, [], Agenda).

extrai_agenda_maquinas_aux([], ListaTemp, Agenda):-
	Agenda = ListaTemp.

extrai_agenda_maquinas_aux([Maquina|Resto], ListaTemp, Agenda):-
	agenda_maq(Maquina, ListaMaquina),
	Item = (Maquina, ListaMaquina),
	extrai_agenda_maquinas_aux(Resto, [Item|ListaTemp], Agenda).