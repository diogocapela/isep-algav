from __future__ import unicode_literals
import requests
from prolog import query, pipe_into_stdin, pipe_into_stdin_block


def cria_tup_encomenda(produto, qtd, deadline):
    return "e("+produto+","+str(qtd)+","+str(deadline)+")"


def request_api(url):
    response = requests.get(url)
    jsondata = response.json()
    return jsondata


def request_api_withpost(url, postData):
    response = requests.post(url, data=postData)
    jsondata = response.json()
    return jsondata


def request_api_withauth(url, token):
    response = requests.get(url, headers={'Authorization': "Bearer "+token})
    jsondata = response.json()
    return jsondata

# encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pC,3,30),e(pD,5,200)]).
# encomenda(clB,[e(pC,3,30),e(pD,5,200),e(pA,4,50),e(pB,4,70)]).
# encomenda(clC,[e(pE,4,60),e(pF,6,120),e(pE,4,60),e(pF,6,120)]).


def regista_encomenda(Request, ps):
    content = Request.get_json()

    j = 0
    encomenda_cliente = ""
    prioridades = []
    for clienteOrders in content:
        encfacto = ""
        if j != 0:
            encomenda_cliente += "\n"
        j += 1
        encfacto += "encomenda("
        encfacto += clienteOrders['cliente'] + ",["
        i = 0
        for tup in clienteOrders['encomenda']:
            if i != 0:
                encfacto += ","
            i += 1
            encfacto += cria_tup_encomenda(
                tup['produto'], tup['qtd'], tup['deadline'])

        encfacto += "])"

        encomenda_cliente += encfacto
        (pipe_into_stdin("assertz("+encfacto+").", ps))
        prioridades.append(
            "assertz(prioridade_cliente("+clienteOrders['cliente']+","+str(clienteOrders["prioridade"])+")).")
    for prio in prioridades:
        pipe_into_stdin(prio, ps)
    return (encomenda_cliente, prioridades)

#
# maquinas([ma,mb,mc,md,me]).
# neste caso m1,m2,m3,...


def busca_machines(url, ps):
    maquinas = []
    jsondata = request_api(url+"machines")
    # processa maquinas
    machine_facto = "assertz(maquinas(["
    i = 0
    for machine in jsondata:
        if machine["active"] == True:
            if i != 0:
                machine_facto += ","
            machine_facto += "m"+str(machine["id"])
            maquinas.append((machine["id"], machine["machineType"]))
            (pipe_into_stdin("assertz(maq_tipo_maquina(m" +
                             str(machine["id"])+",mt"+str(machine["machineType"])+")).", ps))
            i += 1
    machine_facto += "]))."
    (pipe_into_stdin(machine_facto, ps))
    return (maquinas, machine_facto)

# tipo_maquina(mA).


def busca_machines_types(url, ps):
    maquinas = []
    jsondata = request_api(url+"machine-types")
    # processa maquinas
    machine_facto = ""
    i = 0
    for machine in jsondata:
        if i != 0:
            machine_facto += "\n"
        pipe_into_stdin(
            "assertz(tipo_maquina("+"mt"+str(machine["id"])+")).", ps)
        machine_facto += "mt"+str(machine["id"])
        maquinas.append((machine["id"]))
        i += 1
    return (maquinas, machine_facto)


# ferramentas([fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]).
def busca_ferramentas(url, ps):
    ferramentas = []
    response = requests.get(url+"tools")
    jsondata = response.json()
    # processa maquinas
    tool_fact = "assertz(ferramentas(["
    i = 0
    for tool in jsondata:
        if i != 0:
            tool_fact += ","
        tool_fact += tool["tool"]
        ferramentas.append(tool["tool"])
        i += 1
    tool_fact += "]))."
    (pipe_into_stdin(tool_fact, ps))
    return tool_fact


# Linhas
# linhas([lA]).

def busca_production_lines(url, ps):
    linhas_producao = []
    response = requests.get(url+"production-lines")
    jsondata = response.json()
    # processa maquinas
    line_fact = "assertz(linhas(["
    i = 0
    for line in jsondata:
        if i != 0:
            line_fact += ","
        line_fact += "l"+str(line["id"])
        linhas_producao.append(line["id"])
        i += 1
    line_fact += "]))."
    (pipe_into_stdin(line_fact, ps))
    return line_fact


# % Maquinas que constituem as Linhas
# maq_linha(lA,[ma,mb,mc,md,me]).

def busca_linhas_maquinas(url, ps):
    response = requests.get(url+"production-lines")
    jsondata = response.json()
    tool_fact = "assertz(maq_linha("
    j = 0
    for linha in jsondata:
        if j != 0:
            tool_fact += "\nassertz(maq_linha("
        j += 1
        tool_fact += "l"+str(linha["id"])+",["
        i = 0
        for maquina in linha["machines"]:
            if i != 0:
                tool_fact += ","
            tool_fact += "m"+str(maquina)
            i += 1
        tool_fact += "]))."
    (pipe_into_stdin(tool_fact, ps))
    return tool_fact


# produtos([pA,pB,pC,pD,pE,pF]).
def busca_produtos(url, ps):
    products = []
    jsondata = request_api(url+"products")
    # processa maquinas
    produto_facto = "assertz(produtos(["
    i = 0
    for produto in jsondata:
        if i != 0:
            produto_facto += ","
        produto_facto += "p"+str(produto["id"])
        products.append((produto["id"], produto["manufacturingPlan"]["id"]))
        i += 1
    produto_facto += "]))."
    (pipe_into_stdin(produto_facto, ps))
    return (produto_facto, products)


# tipo_operacoes([opt1,opt2,opt3,opt4,opt5,opt6,opt7,opt8,opt9,opt10]).
def busca_operacoes(url, ps):
    operacoes = []
    jsondata = request_api(url+"operations")
    # processa maquinas
    operation_fact = "assertz(tipo_operacoes(["
    i = 0
    for operation in jsondata:
        if i != 0:
            operation_fact += ","
        operation_fact += "opt"+str(operation["id"])
        operacoes.append(operation["id"])
        i += 1
    operation_fact += "]))."
    (pipe_into_stdin(operation_fact, ps))
    return operation_fact


# operacao_maquina(opt1,mt,fa,1,1).
def busca_operacao_maquina(url, maquinas, ps):
    jsondata = request_api(url+"machine-types")
    retData = ""
    i = 0
    for mt in jsondata:
        machineTypeId = mt["id"]
        for (machine, type) in maquinas:
            if type == machineTypeId:
                # adicionar operacao_maquinas
                for op in mt["operations"]:
                    if i != 0:
                        retData += "\n"
                    i += 1
                    retData += "assertz(operacao_maquina(opt"+str(op["id"])+","+"mt"+str(machineTypeId)+","+op["tool"]+","+str(
                        op["timeTakes"])+","+str(op["startupTime"])+"))."
    for opm in retData.splitlines():
        if len(opm) > 2:
            (pipe_into_stdin(opm, ps))
    return retData


# operacoes_produto(pA,[opt1,opt2,opt3,opt4,opt5]).
def busca_operacoes_produtos(url, prodmps, ps):
    opprodfact = ""
    # prodmps contem um pair de produto e o seu plano de fabrico
    j = 0
    for (produto, mp) in prodmps:
        jsondata = request_api(url+"manufacturingPlans/"+str(mp))
        if j != 0:
            opprodfact += "\n"
        j += 1
        opprodfact += "assertz(operacoes_produto(p"+str(produto)+",["
        i = 0
        for op in jsondata["operations"]:
            if i != 0:
                opprodfact += ","
            i += 1
            opprodfact += "opt"+str(op)
        opprodfact += "]))."
    for opp in opprodfact.splitlines():
        if len(opp) > 2:
            (pipe_into_stdin(opp, ps))
    return opprodfact


def regista_clientes(url, user, password, ps):
    # buscar token primeiro
    # POC
    retData = "assertz(clientes(["
    jsdata = {
        "email": user,
        "password": password
    }
    data = request_api_withpost(url+"/auth/login?role=client", jsdata)
    data = request_api_withauth(url+"/users", data["data"]["token"])
    i = 0
    for user in data:
        if i != 0:
            retData += ","
        i += 1
        retData += "cl"+user["_id"]
    retData += "]))."
    pipe_into_stdin(retData, ps)
    return retData


def inicia_escalonamento(ps):

    print(pipe_into_stdin_block("consult(\"ai.pl\").", ps))
    print(pipe_into_stdin("cria_op_enc.\n.", ps))
    print(pipe_into_stdin("extrai_tarefas().", ps))
    print(pipe_into_stdin("gera.\n.", ps))
    print(pipe_into_stdin("cria_agenda_maquinas().", ps))
    return pipe_into_stdin("extrai_agenda_maquinas(X).", ps)
