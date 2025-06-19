:- module(logica_caminho, [buscar_caminho/4, ordenar_por_distancia/2]).
:- use_module(ligacoes).

buscar_caminho(Origem, Destino, Caminho, Distancia) :-
    busca(Origem, Destino, [Origem], Caminho, 0, Distancia).

busca(Destino, Destino, Visitadas, Caminho, Distancia, Distancia) :- reverse(Visitadas, Caminho).
busca(Atual, Destino, Visitadas, Caminho, Acum, Distancia) :-
    pode_ir(Atual, Prox, D),
    \+ member(Prox, Visitadas),
    NovoAcum is Acum + D,
    busca(Prox, Destino, [Prox|Visitadas], Caminho, NovoAcum, Distancia).

ordenar_por_distancia(Lista, Ordenada) :-
    predsort(comparar, Lista, Ordenada).

comparar(<, [_, D1], [_, D2]) :- D1 < D2.
comparar(>, [_, D1], [_, D2]) :- D1 >= D2.
