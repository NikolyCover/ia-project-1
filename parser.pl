:- module(parser, [ler_termos_arquivo/1]).

ler_termos_arquivo(Caminho) :-
    open(Caminho, read, Stream),
    ler_termos(Stream),
    close(Stream).

ler_termos(Stream) :-
    read(Stream, Termo),
    ( Termo == end_of_file ->
        true
    ;
        assertz(Termo),
        ler_termos(Stream)
    ).
