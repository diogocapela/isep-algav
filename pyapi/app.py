#!/bin/env python3
from __future__ import unicode_literals
import fcntl
import os
import sys
from pp import busca_machines_types, inicia_escalonamento, busca_machines, regista_encomenda, busca_ferramentas, busca_production_lines, busca_linhas_maquinas, busca_produtos, busca_operacoes, busca_operacao_maquina, busca_operacoes_produtos, regista_clientes
import json
from flask import Flask, request, jsonify, make_response
import subprocess
# from pyswip import Prolog
# prolog = Prolog()
# prolog.consult("main.pl")
from flask_cors import CORS
app = Flask(__name__)
CORS(app)


base_url = 'https://3na66-factory-prod.azurewebsites.net/api/v1/'
base_url_mdp = 'https://3na66-production-prod.azurewebsites.net/api/'
base_url_om = 'https://ge.diog.co'

ps = None


@app.route('/api/submete_encomendas', methods=['POST'])
def submete_encomendas():
    global ps
    if ps is None:
        ps = subprocess.Popen(('/usr/local/bin/swipl'), stdout=subprocess.PIPE,
                              stdin=subprocess.PIPE, stderr=subprocess.PIPE)
        fd = ps.stdout.fileno()
        fl = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)
    regista(ps)
    (ret, prioridades) = regista_encomenda(request, ps)
    return inicia_escalonamento(ps)


@app.route('/api/planning', methods=['GET'])
def moc_pp():
    moc = [
        {
            "machine": "m1",
            "tasks": [
                {
                    "fromTime": 0.5,
                    "toTime": 1.5,
                    "action": "setup",
                    "bodyAction": {
                        "tool": "fa"
                    }
                },
                {
                    "fromTime": 1.5,
                    "toTime": 4.5,
                    "action": "exec",
                    "bodyAction": {
                        "op": "op11",
                        "qty": 3,
                        "product": "p3",
                        "client": "cl2",
                        "task": "t3"
                    }
                },
                {
                    "fromTime": 27.0,
                    "toTime": 31.0,
                    "action": "exec",
                    "bodyAction": {
                        "op": "op6",
                        "qty": 4,
                        "product": "p2",
                        "client": "cl1",
                        "task": "t2"
                    }
                }
            ]
        },
        {
            "machine": "m2",
            "tasks": [
                {
                    "fromTime": 0.0,
                    "toTime": 2.5,
                    "action": "setup",
                    "bodyAction": {
                        "tool": "fb"
                    }
                },
                {
                    "fromTime": 2.5,
                    "toTime": 8.5,
                    "action": "exec",
                    "bodyAction": {
                        "op": "op1",
                        "qty": 3,
                        "product": "p3",
                        "client": "cl2",
                        "task": "t3"
                    }
                },
                {
                    "fromTime": 27.0,
                    "toTime": 28.0,
                    "action": "setup",
                    "bodyAction": {
                        "tool": "ff"
                    }
                },
                {
                    "fromTime": 28,
                    "toTime": 44,
                    "action": "exec",
                    "bodyAction": {
                        "op": "op7",
                        "qty": 4,
                        "product": "p2",
                        "client": "cl1",
                        "task": "t2"
                    }
                }
            ]
        }
    ]

    return make_response(jsonify(moc))


def regista(ps):
    if ps.poll() is not None:
        ps = subprocess.Popen(('/usr/local/bin/swipl'), stdout=subprocess.PIPE,
                              stdin=subprocess.PIPE, stderr=subprocess.PIPE)
        fd = ps.stdout.fileno()
        fl = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)

    # registar tipos
    busca_machines_types(base_url, ps)

    # print("Registar maquinas...")
    (lm, mfactos) = busca_machines(base_url, ps)

    # print("Registar ferramentas...")
    ffactos = busca_ferramentas(base_url, ps)

    # print("Registar linhas de producao...")
    pfactos = busca_production_lines(base_url, ps)

    # print("Registar maquinas nas linhas de producao")
    lfactos = busca_linhas_maquinas(base_url, ps)

    # print("Registar produtos")
    (produtosfactos, products_mps) = busca_produtos(base_url_mdp, ps)

    # print("Registar operacoes")
    opsfactos = busca_operacoes(base_url, ps)

    # print("Registar operacoes as maquinas")
    opmacaquina = busca_operacao_maquina(base_url, lm, ps)

    # print("Registar operacoes aos produtos")
    opprodutos = busca_operacoes_produtos(base_url_mdp, products_mps, ps)

    # print("Registar clientes")
    clientes = regista_clientes(base_url_om, "admin@sapo.pt", "123123", ps)
    # print(pipe_into_stdin)
    # return mfactos+"\n"+ffactos+"\n"+lfactos+"\n"+produtosfactos+"\n"+opsfactos+"\n"+opmacaquina+"\n"+opprodutos+"\n"+clientes
    return ""


if __name__ == '__main__':
    app.run(host='0.0.0.0', debug=False, port=8080)
