:- use_module(ligacoes).
:- use_module(logica_caminho).
:- use_module(impressao).

mostrar_todos_os_caminhos(Origem, Destino) :-
    (   setof([Caminho, Distancia], buscar_caminho(Origem, Destino, Caminho, Distancia), Lista)
    ->  ordenar_por_distancia(Lista, Ordenada),
        exibir_caminhos(Ordenada)
    ;   writeln("\nNão há caminhos possíveis entre essas cidades.")
    ).
