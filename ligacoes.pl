:- module(ligacoes, [pode_ir/3]).

ligacao(atlanta, charlotte, 390).
ligacao(charlotte, miami, 715).
ligacao(miami, la, 215).
ligacao(la, nashville, 455).
ligacao(charlotte, orlando, 715).
ligacao(orlando, nashville, 400).
ligacao(atlanta, nashville, 400).

pode_ir(A, B, D) :- ligacao(A, B, D).
pode_ir(A, B, D) :- ligacao(B, A, D).
