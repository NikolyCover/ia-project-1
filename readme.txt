============================
Sistema de Busca de Caminhos
============================

Descrição:
-----------
Este programa em Prolog encontra e exibe todos os caminhos possíveis entre duas cidades,
com base em dados fornecidos em um arquivo de fatos. Os caminhos são ordenados pela distância total,
do menor para o maior.

Arquivos principais:
--------------------
1. parser.pl           - Define os fatos: ligações entre cidades, cidade de origem e cidade de destino.
2. busca_caminho.pl    - Implementa a lógica de busca em profundidade e ordenação dos caminhos.
3. impressao.pl        - Responsável por exibir os caminhos encontrados de forma organizada.
4. main.pl             - Arquivo principal com o predicado `executar/1` para iniciar o programa.

Como executar:
--------------
1. Abra o terminal e **navegue até o diretório do projeto** onde os arquivos `.pl` estão localizados.

2. Inicie o interpretador SWI-Prolog com o comando `swipl` na pasta raiz do projeto.

3. Carregue o arquivo principal:

   ?- [main].

4. Execute o programa passando o nome do arquivo de dados com os fatos Prolog:

   ?- executar('_exemplo.txt').

O programa irá:
- Carregar os fatos do arquivo fornecido.
- Obter automaticamente a cidade de origem e destino.
- Calcular todos os caminhos possíveis entre essas cidades.
- Exibir os caminhos ordenados por distância, do menor ao maior.

Formato esperado do arquivo de fatos:
-------------------------------------
O arquivo passado para `executar/1` deve conter:

- Ligações entre cidades:
    ligacao(Cidade1, Cidade2, Distancia).

    Exemplo:
    ligacao(sao_paulo, campinas, 100).

- Cidade de origem:
    cidade_inicial(Cidade).

    Exemplo:
    cidade_inicial(sao_paulo).

- Cidade de destino:
    cidade_final(Cidade).

    Exemplo:
    cidade_final(rio_de_janeiro).

Exemplo completo:
-----------------
Arquivo _exemplo.txt:
    ligacao(atlanta, nashville, 250).
    ligacao(nashville, orlando, 600).
    ligacao(orlando, miami, 405).
    ligacao(atlanta, charlotte, 400).
    ligacao(charlotte, cincinnati, 500).
    ligacao(cincinnati, nashville, 350).
    cidade_inicial(atlanta).
    cidade_final(miami).

No terminal:

    ?- [main].
    ?- executar('_exemplo.txt').

Saída esperada (ordenada por distância crescente):

    Caminho 1 (Menor caminho):
    atlanta > nashville > orlando > miami
    Distancia total: 1255 milhas

    Caminho 2:
    atlanta > charlotte > cincinnati > nashville > orlando > miami
    Distancia total: 2105 milhas

Requisitos:
-----------
- SWI-Prolog instalado: https://www.swi-prolog.org/
- Estar no diretório do projeto antes de carregar os arquivos.
- Todos os arquivos `.pl` do projeto devem estar no mesmo diretório.
- O arquivo de entrada deve estar no mesmo diretório.

Autores:
-------
Nikoly Cover Pereira
Vinicius de Oliveira Jimenez