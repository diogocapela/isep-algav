% Bibliotecas HTTP
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
% Bibliotecas JSON
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).
% MéTODO GET: Tratamento de 'http://localhost:5000/register_operation?id=1&name=male&timeTakes=100'		
% ou http_client:http_get('http://localhost:5000/register_operation?name=\'José\'&sex=male&birth_year=1975',X,[]).

% MéTODO POST
% http_client:http_post('http://localhost:5000/register_user', form_data([name='José', sex=male, birth_year=1975]), Reply, []).

% Relação entre pedidos HTTP e predicados que os processam		
:- http_handler('/register_operation', register_operation, []).
:- http_handler('/register_tool', register_tool, []).
:- http_handler('/register_machine', register_machine, []).
:- http_handler('/register_line', register_line, []).
:- http_handler('/register_machine_on_line', register_machine_on_line, []).
% :- http_handler('/register_operation_type', register_operation_type, []).

% Criação de servidor HTTP no porto 'Port'					
server(Port) :-						
        http_server(http_dispatch, [port(Port)]).
		
register_operation(Request) :-
    http_parameters(Request,
                    [name(Name, []),
                    timeTakes(TK, [between(0,99999)])
                    ]),
    format('Content-type: text/plain~n~n'),
    format('Operation registered!~n'),
    format('Name: ~w~nTime it Takes: ~w~n',[Name,TK]),
    asserta(operacao(Name,TK)).

register_tool(Request) :-
    http_parameters(Request,
                    [name(Name, [])
                    ]),
    format('Content-type: text/plain~n~n'),
    format('cliente registered!~n'),
    asserta(cliente(Name)).

register_machine(Request) :-
    http_parameters(Request,
                    [name(Name, [])
                    ]),
    format('Content-type: text/plain~n~n'),
    format('maquina registered!~n'),
    asserta(maquina(Name)).

register_line(Request) :-
    http_parameters(Request,
                    [name(Name, [])
                    ]),
    format('Content-type: text/plain~n~n'),
    format('linha registered!~n'),
    asserta(linha(Name)).

register_machine_on_line(Request) :-
  http_parameters(Request,
                  [
                    line(Line, []),
                    machine(Machine,[])
                  ]),
  format('Content-type: text/plain~n~n'),
  format('linha registered!~n'),
  asserta(linha(Name)).


% MéTODO POST enviando um ficheiro de texto
% http_client:http_post('http://localhost:5000/send_file_post', form_data([file=file('./teste.txt')]), Reply, []).

% send_file_post(Request) :-
%	http_parameters(Request,[ file(X,[])]),
%    format('Content-type: text/plain~n~n'),
%	format('Received: ~w~n',[X]).


% register_tool(Request) :-
%	http_parameters(Request,
%                    [ 
%                      name(Name, []),
%                      startupTime(ST, [between(0,99999)])
%                    ]),
%    format('Content-type: text/plain~n~n'),
%	format('Received: ~w~n',[X]).

:- json_object operation(name:string, timeTakes:integer).
:- json_object tool(name:string, startupTime:integer).
:- json_object machine(name:string).

load_from_api() :-
  % ler os valores iniciais do MDF
  http_open([host('arqsimocfactoryservice.azurewebsites.net'),scheme(https),path('/api/v1/operations'),header('Authorization','VH0bxo3PB4WJjUc4xxLzmZO9Wu7xowtc')], In, []),
  json_read_dict(In, Dict),
  close(In).



