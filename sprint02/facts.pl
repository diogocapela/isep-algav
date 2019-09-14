% Linhas

linhas([lA]).

% Maquinas

maquinas([ma,mb,mc,md,me]).

% Ferramentas

ferramentas([fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]).

% Maquinas que constituem as Linhas

tipos_maq_linha(lA,[ma,mb,mc,md,me]).

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

encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pC,3,30),e(pD,5,200)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200),e(pA,4,50),e(pB,4,70)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120),e(pE,4,60),e(pF,6,120)]).

% cria_op_enc - fizeram-se correcoes face a versao anterior
