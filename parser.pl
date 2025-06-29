:- module(parser, [ler_termos_arquivo/1]).

%% ler_termos_arquivo(+Caminho)
%
% Lê todos os termos de um arquivo Prolog e os adiciona dinamicamente à base de conhecimento
% utilizando assertz/1.
%
% @param Caminho Caminho para o arquivo contendo os termos Prolog.
ler_termos_arquivo(Caminho) :-
    open(Caminho, read, Stream),
    ler_termos(Stream),
    close(Stream).

%% ler_termos(+Stream)
%
% Lê termos sequencialmente de um stream até encontrar end_of_file.
% Cada termo é adicionado à base de conhecimento com assertz/1.
%
% @param Stream Stream aberto para leitura dos termos.
ler_termos(Stream) :-
    read(Stream, Termo),
    (   Termo == end_of_file
    ->  true
    ;   assertz(Termo),
        ler_termos(Stream)
    ).
