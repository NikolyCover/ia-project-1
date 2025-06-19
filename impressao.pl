:- module(impressao, [exibir_caminhos/1]).

exibir_caminhos(Lista) :- exibir_caminhos(Lista, 1).

exibir_caminhos([], _).
exibir_caminhos([[Caminho, Distancia]|Resto], 1) :-
    format('\nCaminho 1 (Menor caminho):~n'),
    exibir_rota(Caminho),
    format('Distância total: ~w milhas~n~n', [Distancia]),
    exibir_caminhos(Resto, 2).
exibir_caminhos([[Caminho, Distancia]|Resto], N) :-
    format('Caminho ~w:~n', [N]),
    exibir_rota(Caminho),
    format('Distância total: ~w milhas~n~n', [Distancia]),
    N1 is N + 1,
    exibir_caminhos(Resto, N1).

exibir_rota([Ultima]) :- format('~w~n', [Ultima]).
exibir_rota([Atual|Resto]) :- format('~w > ', [Atual]), exibir_rota(Resto).
