#!/usr/bin/pl -t main -q -f

% unix deamon for deployment
:- use_module(library(http/http_unix_daemon)).

% init script
% :- initialization(main,main).

main(Argv) :-
    interactive(false),
    load_from_api(),
    server(8888).
    %http_daemon(Options).
