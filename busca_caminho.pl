:- module(busca_caminho, [mostrar_todos_os_caminhos/2]).

:- use_module(parser).
:- use_module(impressao).

%% pode_ir(+CidadeA, +CidadeB, -Distancia)
%
% Verifica se é possível ir diretamente entre duas cidades conectadas por uma ligação,
% independentemente da direção da conexão (ida ou volta).
%
% Utiliza o predicado parser:ligacao/3 para consultar se há uma ligação entre as cidades.
%
% @param CidadeA   Primeira cidade.
% @param CidadeB   Segunda cidade.
% @param Distancia Distância entre as cidades, caso exista uma ligação direta.
pode_ir(CidadeA, CidadeB, Distancia) :-
    parser:ligacao(CidadeA, CidadeB, Distancia).

pode_ir(CidadeA, CidadeB, Distancia) :-
    parser:ligacao(CidadeB, CidadeA, Distancia).

%% buscar_caminho(+Origem, +Destino, -Caminho, -Distancia)
%
% Encontra um caminho entre Origem e Destino.
% Retorna a lista de cidades no Caminho e a Distancia total percorrida.
%
% @param Origem    Cidade de partida.
% @param Destino   Cidade de destino.
% @param Caminho   Caminho encontrado (lista de cidades).
% @param Distancia Distância total do caminho.
buscar_caminho(Origem, Destino, Caminho, Distancia) :-
    busca(Origem, Destino, [Origem], Caminho, 0, Distancia).

%% busca(+Atual, +Destino, +Visitadas, -Caminho, +Acum, -Distancia)
%
% Predicado auxiliar para realizar a busca em profundidade evitando ciclos.
% Mantém uma lista de cidades Visitadas, acumula a distância e retorna
% o Caminho e a Distancia ao alcançar o destino.
%
% @param Atual     Cidade atual na busca.
% @param Destino   Cidade destino da busca.
% @param Visitadas Lista de cidades já visitadas.
% @param Caminho   Caminho resultante (inversão de Visitadas ao final).
% @param Acum      Distância acumulada até o ponto atual.
% @param Distancia Distância total do caminho final.
busca(Destino, Destino, Visitadas, Caminho, Distancia, Distancia) :-
    reverse(Visitadas, Caminho).

busca(Atual, Destino, Visitadas, Caminho, Acum, Distancia) :-
    pode_ir(Atual, Prox, D),
    \+ member(Prox, Visitadas),
    NovoAcum is Acum + D,
    busca(Prox, Destino, [Prox | Visitadas], Caminho, NovoAcum, Distancia).

%% ordenar_por_distancia(+Lista, -Ordenada)
%
% Ordena uma lista de pares [Caminho, Distancia] em ordem crescente de distância.
%
% @param Lista    Lista de pares não ordenada.
% @param Ordenada Lista ordenada pela distância.
ordenar_por_distancia(Lista, Ordenada) :-
    predsort(comparar, Lista, Ordenada).

%% comparar(?Ordem, +Elem1, +Elem2)
%
% Predicado de comparação usado por predsort/3 para ordenar pares [_, Distancia].
%
% @param Ordem Resultado da comparação (< ou >).
% @param Elem1 Primeiro elemento a ser comparado (par [_, D1]).
% @param Elem2 Segundo elemento a ser comparado (par [_, D2]).
comparar(<, [_, D1], [_, D2]) :-
    D1 < D2.

comparar(>, [_, D1], [_, D2]) :-
    D1 >= D2.

%% mostrar_todos_os_caminhos(+Origem, +Destino)
%
% Exibe todos os caminhos possíveis entre Origem e Destino, ordenados por distância.
% Se não houver caminhos possíveis, exibe uma mensagem apropriada.
%
% @param Origem  Cidade de origem.
% @param Destino Cidade de destino.
mostrar_todos_os_caminhos(Origem, Destino) :-
    (   setof([Caminho, Distancia], buscar_caminho(Origem, Destino, Caminho, Distancia), Lista)
    ->  ordenar_por_distancia(Lista, Ordenada),
        exibir_caminhos(Ordenada)
    ;   writeln('\nNao ha caminhos possiveis entre essas cidades.')
    ).
