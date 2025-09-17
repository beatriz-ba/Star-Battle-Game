% Predicado, visualiza, que percorre e escreve cada  elemento da lista Lista.
visualiza([]). % Caso base
visualiza([H|T]):-
    writeln(H),
    visualiza(T).


% Predicado, visualizaLinha,que percorre e escreve cada elemento da lista Lista, aparecendo antes o número da linha em causa.
visualizaLinha(L):-visualizaLinha(L,1).
visualizaLinha([],_).  % Caso base
visualizaLinha([H|T],Linha):-
    write(Linha), write(': '), writeln(H),
    New_Linha is Linha + 1,
    visualizaLinha(T,New_Linha).


% Predicado, insereObjecto, que insere um objeto Obj na posição (L,C) no tabuleiro Tab, predicado não falha se as coordenadas
% não fizerem parte do tabuleiro ou se já existir um objeto nessas coordenadas.
insereObjecto((L,C),Tab,Obj):-
    nth1(L,Tab,Linha),  % Obtém a linha L do tabuleiro Tab.
    nth1(C,Linha,Elem),  % Obtém o elemento na coluna C da linha Linha.
    var(Elem), 
    Elem=Obj, 
    !. 

% Evita que o Prolog tente encontrar outras soluções depois de falhar a primeira tentativa.
insereObjecto((L,C),Tab,Obj):-
    !.


% Predicado,insereVariosObjectos, que insere os objetos de ListaObjs, nas coordenadas de ListaCoords.
% Predicado não falha se alguma coordenada não fizer parte do tabuleiro.
% Nâo falha se já existir um objeto nessa coordenada no entanto avança para a coordenada e objeto seguintes.
% Predicado falha se as listas ListaCoords e ListaObjs tiverem dimensões diferentes.
insereVariosObjectos([],_,[]):- % Caso base
    !.
insereVariosObjectos([(L,C)|T],Tab,[P|R]):-
    insereObjecto((L,C),Tab,P),  % Insere o objeto P na posição (L,C).
    insereVariosObjectos(T,Tab,R),
    !.

% Evita que o Prolog tente encontrar outras soluções depois da inserção falhar 
% ou quando existem parâmetros que não se ajustam às cláusulas anteriores.
insereVariosObjectos((_, _), _, _):-
    !.



% Predicado, inserePontosVolta, que insere pontos 'p' nas posições à volta da coordenada (L,C), no tabuleiro Tab.
inserePontosVolta(Tab,(L,C)):-
    Prox_Linha is L + 1,  % Próxima linha.
    Ant_Linha is L - 1,  % Linha anterior.
    Prox_Coluna is C + 1,  % Próxima coluna.
    Ant_Coluna is C - 1,  % Coluna anterior.
    % Insere 'p' nas posições acima e abaixo de (L,C).
    insereVariosObjectos([(Prox_Linha,C),(Ant_Linha,C)],Tab,[p,p]),
    % Insere 'p' nas posições à esquerda de (L,C).
    insereVariosObjectos([(Ant_Linha,Ant_Coluna),(L,Ant_Coluna),(Prox_Linha,Ant_Coluna)],Tab,[p,p,p]),
    % Insere 'p' nas posições à direita de (L,C).
    insereVariosObjectos([(Ant_Linha,Prox_Coluna),(L,Prox_Coluna),(Prox_Linha,Prox_Coluna)],Tab,[p,p,p]),
    !.

% Evita que o Prolog tente encontrar outras soluções depois de falhar a primeira tentativa.
inserePontosVolta(_,_):-
    !.



% Predicado, inserePontos, que insere pontos 'p' ,no tabuleiro Tab, em todas as coordenadas de ListaCoord.
inserePontos(_,[]).% Caso base
inserePontos(Tab,[(L,C)|T]):-
    insereObjecto((L,C),Tab,p), % Insere o ponto 'p' na posição (L,C).
    inserePontos(Tab,T),
    !.

% Se na posição de alguma das coordenadas de ListaCoord existir um objeto que não seja uma variável, continua.
inserePontos(Tab,[H|T]):-
    inserePontos(Tab,T),
    !.


% Predicado ,objectosEmCoordenadas, devolve uma lista com os objetos,do tabuleiro Tab, que se encontram nas coordenadas da lista ListaCoord.
% Predicado falha se alguma coordenada de ListaCoord não pertencer ao tabuleiro.
objectosEmCoordenadas([],_,[]).% Caso base
objectosEmCoordenadas([(L,C)|T],Tab,ListaObjs):-
    nth1(L,Tab,Linha),
    nth1(C,Linha,Elem),
    objectosEmCoordenadas(T,Tab,ListaObjs_temp),
    ListaObjs = [Elem|ListaObjs_temp].  % Adiciona o elemento atual à lista de objetos.




% Predicado ,coordObjectos, devolve uma lista com todas as coordenadas de um tabuleiro Tab onde o objeto Obj está localizado,
% e o número de objetos encontrados igual a Obj.
coordObjectos(_,_,[],[],0):- % Caso base
    !. 

coordObjectos(Obj,Tab,[(L,C)|T],ListaCoords,NumObj):-
    nth1(L,Tab,Linha),
    nth1(C,Linha,Elem),
    nonvar(Elem),
    nonvar(Obj),
    Obj == Elem, % Verifica se o elemento é igual ao objeto procurado.
    coordObjectos(Obj,Tab,T,ListaCoords_temp,NumObj_temp),
    ListaCoords = [(L,C)|ListaCoords_temp],  % Adiciona a coordenada atual à lista de coordenadas.
    NumObj is NumObj_temp + 1,  % Incrementa o número de objetos encontrados.
    !.

% Verifica se a posição tem uma variavel e o objeto procurado também é variável.
coordObjectos(Obj,Tab,[(L,C)|T],ListaCoords,NumObj):-
    nth1(L,Tab,Linha), 
    nth1(C,Linha,Elem),
    (var(Elem), var(Obj)),
    coordObjectos(Obj,Tab,T,ListaCoords_temp,NumObj_temp),
    ListaCoords = [(L,C)|ListaCoords_temp],
    NumObj is NumObj_temp + 1,
    !.

% Se a coordenada não corresponde às cláusulas anteriores, continua a busca.
coordObjectos(Obj,Tab,[(L,C)|T],ListaCoords,NumObj):-
    coordObjectos(Obj,Tab,T,ListaCoords,NumObj),
    !.


% Predicado coordLinhas_Novo devolve uma lista das coordenadas das linhas do tabuleiro Tab.
% Predicado, do codigo auxiliar, coordLinhas ligeiramente modificado.
coordLinhas_Novo(Num, CoordLinhas) :-  
    coordLinhas_Novo(Num, CoordLinhas, 1, []). 
    
coordLinhas_Novo(Num, CoordLinhas, N, CoordLinhas) :- N > Num, !.    
coordLinhas_Novo(Num, CoordLinhas, N, Aux) :-  
    findall((N, C), between(1, Num, C), CoordLinhaN),
    append(Aux, CoordLinhaN, NovoAux),
    NovoN is N + 1,
    coordLinhas_Novo(Num, CoordLinhas, NovoN, NovoAux).


% Predicado auxiliar que devolve uma lista com as coordenadas das variáveis do tabuleiro Tab.
coordenadasVars_aux([],_,[]).% Caso base

coordenadasVars_aux([(L,C)|T],Tab,Lista_vars2):-
    nth1(L,Tab,Linha),
    nth1(C,Linha,Elem),
    var(Elem),
    !,
    coordenadasVars_aux(T,Tab,Lista_vars2_temp), 
    Lista_vars2 = [(L,C)|Lista_vars2_temp].

% Caso a coordenada não ter uma variavel no tabuleiro, continua a busca.
coordenadasVars_aux([_|T], Tabuleiro, Lista_vars2) :- 
    coordenadasVars_aux(T, Tabuleiro, Lista_vars2).


% Predicado ,coordenadasVars, que verifica se ListaVars é a lista com as coordenadas das variáveis do tabuleiro Tab.
coordenadasVars(Tab, ListaVars) :-
    length(Tab, Num),  % Obtém o comprimento das linhas e colunas do tabuleiro Tab.
    coordLinhas_Novo(Num, CoordLinhas),  % Gera todas as coordenadas das linhas do tabuleiro
    coordenadasVars_aux(CoordLinhas, Tab, ListaVars).  % Verifica se ListaVars é a lista com todas as coordenadas das variáveis.



% Predicado auxiliar, quantasVariaveis, que devolve o número de variáveis das coordenadas de uma lista, do tabuleiro Tab.
quantasVariaveis([], _, 0).% Caso base

quantasVariaveis([(L,C)|T], Tab, Count_variaveis) :-
    nth1(L, Tab, Linha),
    nth1(C, Linha, Elem),
    (var(Elem) ->  % Verifica se o elemento é uma variável
        Count_variaveis_temp is 1  % Incrementa a contagem de variáveis temporária, se for uma variável.
    ;
        Count_variaveis_temp is 0  % Senão contagem de variáveis temporária é 0.
    ),
    quantasVariaveis(T, Tab, RestCount),
    Count_variaveis is RestCount + Count_variaveis_temp,  % Soma a contagem temporária à contagem total.
    !.


% Predicado fechaListaCoordenadas que após a sua aplicação, as coordenadas de ListaCoord sâo apenas estrelas e pontos, 
% considerando as hipóteses h1, h2, h3.
% Se nenhuma das hipóteses se verificar, o tabuleiro mantém-se inalterável.
fechaListaCoordenadas(Tab, ListaCoord) :-
    coordObjectos(e,Tab,ListaCoord,ListaEstrelas,Count_Estrelas),% número de estrelas nas coordenadas de ListaCoord.
    quantasVariaveis(ListaCoord, Tab, Count_variaveis), % número de variáveis nas coordenadas de ListaCoord.
    (
        % Hipótese h1: 2 estrelas, insere pontos.
        Count_Estrelas == 2 ->
        inserePontos(Tab, ListaCoord)  % Insere pontos nas restantes coordenadas
    ;
        % Hipótese h2: 1 estrela e 1 variável, insere uma estrela e pontos ao redor.
        Count_Estrelas == 1,
        Count_variaveis == 1 ->
        coordenadasVars_aux(ListaCoord, Tab, [Coord]),  % Obtém a coordenada da única variável.
        insereObjecto(Coord, Tab, e),  % Insere uma estrela na coordenada da variavel.
        inserePontosVolta(Tab, Coord)  % Insere pontos á volta da estrela introduzida anteriormente.
    ;
        % Hipótese h3: 0 estrelas e 2 variáveis, insere duas estrelas e pontos ao redor.
        Count_Estrelas == 0,
        Count_variaveis == 2 ->
        coordenadasVars_aux(ListaCoord, Tab, [Coord1, Coord2]),  % Obtém as coordenadas das duas variáveis.
        insereObjecto(Coord1, Tab, e),  % Insere uma estrela na primeira coordenada.
        insereObjecto(Coord2, Tab, e),  % Insere uma estrela na segunda coordenada.
        inserePontosVolta(Tab, Coord1),  % Insere pontos à volta da primeira estrela.
        inserePontosVolta(Tab, Coord2)  % Insere pontos à volta da segunda estrela.
    ;
        true  % Se nenhuma das hipóteses se verificar, o tabuleiro mantém-se inalterável.
    ),
    !.


% Predicado, fecha, que após a sua aplicação Tabuleiro será o resultado de aplicar o predicado fechaListaCoordenadas a cada lista.
fecha(_,[]) :- % Csao base
    !. 

fecha(Tab, [H|T]) :-
    fechaListaCoordenadas(Tab, H),  % aplica fechaListaCoordenadas a uma das listas de ListaListasCoord.
    fecha(Tab, T),
    !.



% Predicado auxiliar,verificaEstrelas, que verifica se não existe estrelas no tabuleiro Tab.
verificaEstrelas([], _) :- 
    !.

verificaEstrelas([(L,C)|T], Tab) :-
    nth1(L, Tab, Linha), 
    nth1(C, Linha, Elem), 
    (var(Elem); Elem == p),  % Verifica se o elemento é uma variável ou um ponto 'p'.
    verificaEstrelas(T, Tab),
    !.

% Predicado auxiliar, prefixo, que verifica se a primeira lista é um prefixo da segunda lista.
prefixo([], _) :-
    !.
prefixo([X|Xs], [X|Ys]) :- 
    prefixo(Xs, Ys), 
    !.

% Predicado, sublista, que verifica se uma lista é uma sublista de outra lista.
sublista(Xs, Ys) :- 
    prefixo(Xs, Ys),  % Verifica se Xs é um prefixo de Ys
    !.
% Continua a busca ignorando o primeiro elemento da segunda lista.
sublista(Xs, [_|Ys]) :- 
    sublista(Xs, Ys),
    !.


% Predicado, encontraSequencia, que devolve Seq, uma sublista de ListaCoords,
% se as suas coordenadas representam posições com variáveis,
% se as suas coordenadas aparecem seguidas (numa linha, coluna ou região),
% se não existir estrelas nas coordenadas,
% e se N for o tamanho de Seq.
encontraSequencia(Tab, N, ListaCoords, Seq):-
    coordenadasVars_aux(ListaCoords, Tab, Lista_vars),  % Encontra todas as coordenadas de variáveis.
    length(Lista_vars, N),  % Verifica se a lista de variaveis tem tamanho N.
    sublista(Lista_vars, ListaCoords),% Verifica se Lista_vars é uma sublista de ListaCoords, ou seja se as coordenadas aparecem seguidas.
    verificaEstrelas(ListaCoords, Tab),  % Verifica se não existem estrelas nas coordenadas de ListaCoords 
    Seq = Lista_vars,  % Atribui Lista_vars a Seq.
    !.


% Predicado ,aplicaPadraoI, que aplica uma estrela 'e' em (L1, C1) e (L3, C3) 
% e os pontos obrigatórios 'p' à volta de cada estrela no tabuleiro Tab.
aplicaPadraoI(Tab, ListaCoords):-
    length(ListaCoords, 3),  % Verifica se a lista de coordenadas tem exatamente 3 elementos.
    ListaCoords = [(L1, C1), _, (L3, C3)],  % Atribui valores às coordenadas iniciais e finais.
    insereObjecto((L1, C1), Tab, e),  % Insere uma estrela na primeira coordenada.
    inserePontosVolta(Tab, (L1, C1)),  % Insere pontos à volta da primeira estrela.
    insereObjecto((L3, C3), Tab, e),  % Insere uma estrela na terceira coordenada.
    inserePontosVolta(Tab, (L3, C3)),  % Insere pontos à volta da terceira estrela.
    !.

% Caso a lista não tenha exatamente 3 elementos, falha.
aplicaPadraoI(_, _):- 
    !, fail.


% Predicado, aplicaPadroes, aplica o aplicaPadraoI se ter-se-ão encontrado sequências de tamanho 3
% ou aplica o aplicaPadraoT se ter-se-ão encontrado sequências de tamanho 4.
aplicaPadroes(_, []) :- % Caso base
    !.

aplicaPadroes(Tab, [H|T]):-
    (
        encontraSequencia(Tab, 3, H, Seq) ->  % Verifica se há uma sequência de tamanho 3
        aplicaPadraoI(Tab, Seq)  % Aplica o padrão I.
    ;
        encontraSequencia(Tab, 4, H, Seq) ->  % Verifica se há uma sequência de tamanho 4
        aplicaPadraoT(Tab, Seq)  % Aplica o padrão T.
    ),
    aplicaPadroes(Tab, T). 

% Se não se verificar a cláusula anterior continua a processar a lista de listas de coordenadas.
aplicaPadroes(Tab, [H|T]):-
    aplicaPadroes(Tab, T),
    !.


% Predicado auxiliar, compara_Tab, que compara dois tabuleiros Tab1 e Tab2.
compara_Tab(_, _, []) :-
    !. 

compara_Tab(Tab1, Tab2, [H|T]) :-
    compara_lista(Tab1, Tab2, H),  % Compara os tabuleiros Tab1 e Tab2, nas coordenadas da lista H.
    compara_Tab(Tab1, Tab2, T).

% Predicado auxiliar, compara_lista, que compara elementos de dois tabuleiros Tab1 e Tab2 nas coordenadas de um a lista .
compara_lista(_, _, []) :-
    !.

compara_lista(Tab1, Tab2, [(L, C)|T]) :-
    nth1(L, Tab1, Linha1),  
    nth1(C, Linha1, Elem1),  % Obtém o elemento de Tab1.
    nth1(L, Tab2, Linha2), 
    nth1(C, Linha2, Elem2),  % Obtém o elemento de Tab2.
    (Elem1 == p, Elem2 == p ;  % Verifica se os elementos são ambos pontos 'p'.
     Elem1 == e, Elem2 == e ;  % Verifica se os elementos são ambos estrelas 'e'.
     var(Elem1), var(Elem2)),  % Verifica se os elementos são ambos variáveis.
    compara_lista(Tab1, Tab2, T).

% Predicado auxiliar, tabuleiro_diferentes, que verifica se dois tabuleiro são diferentes.
tabuleiros_diferentes(Tab1, Tab2, Coord) :-
    \+ compara_Tab(Tab1, Tab2, Coord).


% Predicado, resolve, que resulta de aplicar os predicados aplicaPadroes e fecha até já não
% haver mais alterações nas variáveis do tabuleiro ou seja resolve um puzzle no tabuleiro Tab com base na estrutura Estru.
resolve(Estru, Tab) :-
    length(Tab, Num),  % Obtém o número de linhas/colunas do tabuleiro Tab.
    coordLinhas(Num, CoordLinhas),  % Gera as coordenadas das linhas.
    coordColunas(Num, CoordColunas),  % Gera as coordenadas das colunas.
    coordRegioes(Estru, CoordRegioes),  % Gera as coordenadas das regiões com base na estrutura Estru.
    resolve_aux(Tab, CoordLinhas, CoordColunas, CoordRegioes, Estru, TabFinal).  % Chama a função auxiliar resolve_aux.

% Predicado auxiliar, resolve_aux, que aplica padrões e devolve o tabuleiro final até que não haja mais alterações nas variáveis do Tab.
resolve_aux(Tab, CoordLinhas, CoordColunas, CoordRegioes, Estru, TabFinal) :-
    copy_term(Tab, TabTemp),  % Faz uma cópia temporária do tabuleiro Tab.
    aplicaPadroes(Tab, CoordLinhas),  % Aplica padrões nas linhas.
    fecha(Tab, CoordLinhas),  % Fecha as linhas.
    aplicaPadroes(Tab, CoordColunas),  % Aplica padrões nas colunas.
    fecha(Tab, CoordColunas),  % Fecha as colunas.
    aplicaPadroes(Tab, CoordRegioes),  % Aplica padrões nas regiões.
    fecha(Tab, CoordRegioes),  % Fecha as regiões.
    coordTodas(Estru, CoordTodas),  % Obtém todas as coordenadas com base na estrutura Estru.
    (tabuleiros_diferentes(TabTemp, Tab, CoordTodas) ->  % Verifica se o tabuleiro cópia é diferente de Tab.
        resolve_aux(Tab, CoordLinhas, CoordColunas, CoordRegioes, Estru, TabFinal)  % Se for diferente contínua a resolver
    ; 
        TabFinal = Tab  % Senão atribui o tabuleiro Tab ao tabuleiro final TabFinal.
    ).

