:- use_module(busca_caminho).
:- use_module(parser).

%% executar(+Arquivo)
%
%  Executa o programa principal:
%  - Lê os fatos do arquivo fornecido (ex: pode_ir/3, cidade_inicial/1, cidade_final/1),
%  - Obtém a cidade inicial e final da base de conhecimento,
%  - Exibe todos os caminhos possíveis entre essas duas cidades.
%
%  @param Arquivo Caminho para o arquivo contendo os fatos Prolog.
executar(Arquivo) :-
    ler_termos_arquivo(Arquivo),
    parser:cidade_inicial(I),
    parser:cidade_final(F),
    mostrar_todos_os_caminhos(I, F).
