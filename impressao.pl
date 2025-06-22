:- module(impressao, [exibir_caminhos/1]).

%% exibir_caminhos(+Lista)
%
%  Exibe uma lista de caminhos com suas respectivas distâncias.
%  O primeiro caminho é destacado como o menor caminho.
%
%  @param Lista Lista de pares [Caminho, Distancia], onde:
%               - Caminho é uma lista de cidades.
%               - Distancia é o valor total do percurso.
exibir_caminhos(Lista) :-
    exibir_caminhos(Lista, 1).

%% exibir_caminhos(+Lista, +Indice)
%
%  Predicado auxiliar recursivo que exibe cada caminho com numeração.
%  O primeiro caminho é destacado como o menor.
%
%  @param Lista  Lista de caminhos a serem exibidos.
%  @param Indice Índice atual da exibição.
exibir_caminhos([], _).

exibir_caminhos([[Caminho, Distancia]|Resto], 1) :-
    format('\nCaminho 1 (Menor caminho):~n'),
    exibir_rota(Caminho),
    format('Distancia total: ~w milhas~n~n', [Distancia]),
    exibir_caminhos(Resto, 2).
exibir_caminhos([[Caminho, Distancia]|Resto], N) :-
    format('Caminho ~w:~n', [N]),
    exibir_rota(Caminho),
    format('Distancia total: ~w milhas~n~n', [Distancia]),
    N1 is N + 1,
    exibir_caminhos(Resto, N1).

%% exibir_rota(+Caminho)
%
%  Exibe o caminho formatado com setas entre as cidades.
%
%  @param Caminho Lista de cidades representando a rota.
exibir_rota([Ultima]) :-
    format('~w~n', [Ultima]).
exibir_rota([Atual|Resto]) :-
    format('~w > ', [Atual]),
    exibir_rota(Resto).
