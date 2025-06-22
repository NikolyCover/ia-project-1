:- module(busca_caminho, [mostrar_todos_os_caminhos/2]).

:- use_module(parser).
:- use_module(impressao).

buscar_caminho(Origem, Destino, Caminho, Distancia) :-
    busca(Origem, Destino, [Origem], Caminho, 0, Distancia).

busca(Destino, Destino, Visitadas, Caminho, Distancia, Distancia) :- reverse(Visitadas, Caminho).

busca(Atual, Destino, Visitadas, Caminho, Acum, Distancia) :-
    parser:pode_ir(Atual, Prox, D),
    \+ member(Prox, Visitadas),
    NovoAcum is Acum + D,
    busca(Prox, Destino, [Prox|Visitadas], Caminho, NovoAcum, Distancia).

ordenar_por_distancia(Lista, Ordenada) :-
    predsort(comparar, Lista, Ordenada).

comparar(<, [_, D1], [_, D2]) :- D1 < D2.
comparar(>, [_, D1], [_, D2]) :- D1 >= D2.


mostrar_todos_os_caminhos(Origem, Destino) :-
    (   setof([Caminho, Distancia], buscar_caminho(Origem, Destino, Caminho, Distancia), Lista)
    ->  ordenar_por_distancia(Lista, Ordenada),
        exibir_caminhos(Ordenada)
    ;   writeln("\nNao ha caminhos possiveis entre essas cidades.")
    ).

