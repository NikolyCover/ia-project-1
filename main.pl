:- use_module(busca_caminho).
:- use_module(parser).

executar(Arquivo) :-
    ler_termos_arquivo(Arquivo),
    parser:cidade_inicial(I),
    parser:cidade_final(F),
    mostrar_todos_os_caminhos(I, F).
