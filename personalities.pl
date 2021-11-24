:- style_check(-singleton).

% Suma de elementos
sum_elements([], 0).
sum_elements([X|Xs], S):- sum_elements(Xs, S2), S is S2 + X.

% Numero mayor
mayor_que(X,Y,N) :- X > Y, N is X; X < Y, N is Y.

% Respuestas.
escoge :- writeln('    Totalmente de acuerdo (td), Un poco (up), Neutral (n), No realmente (nr), No estoy de acuerdo (nd)').
respuesta(X) :- member(X,[td,up,n,nr,nd]).

% Rango de las respuestas
rango(3,td).
rango(2,up).
rango(1,n).
rango(0,nr).
rango(0,nd).

% checa el rango de las respuestas
check(S) :- S =< 12 , S >= 8,!. 
check(S) :- S < 8.

% resultado de tipologia
% se agrega el predicado asserta(tipologia(N, i)) para saber cual es.

% Preguntas introversion y extroversion
intro_extro_questions:- 
    
    % introversion
    writeln('Evito las multitudes y busco la tranquilidad.'),
    escoge, 
    read(AI),respuesta(AI), rango(WI,AI), nl, 
    writeln('Prefiero pequeñas reuniones con amigos cercanos.'),
    escoge,
    read(BI),respuesta(BI), rango(XI,BI), nl, 
    writeln('Estar sólo me recarga las pilas.'),
    escoge, 
    read(CI),respuesta(CI), rango(YI,CI), nl,
    writeln('Ensaya las cosas antes de decirlas; a menudo contesta con "lo tendrá que pensar o le contesto más tarde"'),
    escoge, 
    read(DI),respuesta(DI), rango(ZI,DI), nl,
    
    L=[WI,XI,YI,ZI], sum_elements(L,SUMA), check(SUMA),
    asserta(tipologia(SUMA, i)),
   
    % extroversion
    writeln('Me distraigo con facilidad, sin mucha concentración en una única tarea.'),
    escoge, 
    read(AE), respuesta(AE), rango(WE,AE), nl, 
    writeln('Me gusta ser el centro de atención.'),
    escoge,
    read(BE), respuesta(BE), rango(XE,BE), nl,
    writeln('Me junto con gente con facilidad y participo en muchas actividades.'),
    escoge,
    read(CE), respuesta(CE), rango(YE,CE), nl,
    writeln('Le gusta ir a reuniones y tiende a manifestar su opinion.'),
    escoge,
    read(DE), respuesta(DE), rango(ZE,DE), nl,
    
    L2 = [WE,XE,YE,ZE], sum_elements(L2,SUMA2), check(SUMA2),
    asserta(tipologia(SUMA2, e)), mayor_que(SUMA,SUMA2, Res), 

    tipologia(Res, T), S = T, writeln(S).

% Dimensiones dicotomicas
fuente_energia(extroversion(e,pregunta(A)), introversion(i)). %retorna true
percibir_mundo(sensorial(s), intuitivo(n)).
forma_evaluacion(racionalista(t),sentimental(f)).
estilo_vida(perceptivos(p), jueces(j)).

% respuestas
imprimir(Lista) :- Lista = [H|C], write(H), nl, imprimir(C).

main :-
    writeln('Bienvenido al test de personalidad MBTI'),nl,
    writeln("Contesta las siguientes preguntas, se honesto con tus respuestas, evita responder a todo neutral"),
    nl,
    intro_extro_questions.