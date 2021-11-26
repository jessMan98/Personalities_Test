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
check(S) :- S =< 12. 

% resultado de tipologia: se agrega el predicado asserta(tipologia(N, i)) para saber cual es.


% Preguntas introversion y extroversion
intro_extro_questions:- 
    
    writeln('Fuente de Energia'),nl,
    
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
    
    LI = [WI,XI,YI,ZI], sum_elements(LI,SUMA), check(SUMA),
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
    
    LE = [WE,XE,YE,ZE], sum_elements(LE,SUMA2), check(SUMA2),
        asserta(tipologia(SUMA2, e)), 
        mayor_que(SUMA,SUMA2, Res), 

        tipologia(Res, T), assertz(letra(T)), writeln(T).

sen_int_questions:-
    
    writeln('Forma de percibir el mundo'), nl, 
    
    % sensorial
    writeln('Valoro el realismo y el sentido común.'),
        escoge,
        read(AS), respuesta(AS), rango(WS,AS), nl, 
    
    writeln('Tiendo a ser concreto y literal; A dar descripciones detalladas.'),
        escoge,
        read(BS), respuesta(BS), rango(XS,BS), nl, 
    
    writeln('Prefiere resultados con hechos y numeros que con ideas y teorias'),
        escoge,
        read(CS), respuesta(CS), rango(YS,CS), nl, 
    
    writeln('Confío en las experiencias pasadas.'),
        escoge,
        read(DS), respuesta(DS), rango(ZS,DS), nl, 

    LS = [WS,XS,YS,ZS], sum_elements(LS,SUMA3), check(SUMA3),
        asserta(tipologia(SUMA3, s)),

    % intuitivo
    writeln('Confío en las corazonadas.'),
        escoge,
        read(AN), respuesta(AN), rango(WN,AN), nl, 
    
    writeln('Valoro la imaginación y la innovación.'),
        escoge,
        read(BN), respuesta(BN), rango(XN,BN), nl, 
    
    writeln('Encuentra el futuro y sus posibilidades interesantes, más que atemorizantes; generalmente le atrae mas a donde va que donde esta.'),
        escoge,
        read(CN), respuesta(CN), rango(YN,CN), nl, 
    
    writeln('Le interesa saber como funcionan las cosas solo por placer.'),
        escoge,
        read(DN), respuesta(DN), rango(ZN,DN), nl, 

    LN = [WN,XN,YN,ZN], sum_elements(LN,SUMA4), check(SUMA4),
        asserta(tipologia(SUMA4,n)), mayor_que(SUMA3,SUMA4, Rsn),

        tipologia(Rsn, T), assertz(letra(T)), writeln(T).

rac_sen_questions :-
    
    writeln('Forma de Evaluacion'), nl,
    
    % racionalista
    writeln('Anoto los pros y contras de cada opción.'),
        escoge,
        read(AR), respuesta(AR), rango(WR,AR), nl, 
    
    writeln('Suelo poner en duda los razonamientos de otros, porque podrían estar equivocados.'),
        escoge,
        read(BR), respuesta(BR), rango(XR,BR), nl, 
    
    writeln('Tiende a dar mas crédito a cosas que son logicas y cientificas.'),
        escoge,
        read(CR), respuesta(CR), rango(YR,CR), nl, 
    
    writeln('Recuerda los numeros y cifras mas facilmente que las caras y los nombres.'),
        escoge,
        read(DR), respuesta(DR), rango(ZR,DR), nl, 

    LR = [WR,XR,YR,ZR], sum_elements(LR,SUMA5), check(SUMA5),
        asserta(tipologia(SUMA5,t)),

    % sentimental
    writeln('Trato con la gente con compasión, según se requiera.'),
        escoge,
        read(ASE), respuesta(ASE), rango(WSE,ASE), nl, 
    
    writeln('Aprecio que me pregunten a menudo por mi estado emocional.'),
        escoge,
        read(BSE), respuesta(BSE), rango(XSE,BSE), nl, 
    
    writeln('Se pone en el lugar de los demas; Ud. es quien en una reunión probablemente pregunta como afectara esto a la gente involucrada.'),
        escoge,
        read(CSE), respuesta(CSE), rango(YSE,CSE), nl, 
    
    writeln('Disfruta prestando servicios necesarios a la gente aunque algunos se aprovechen de Ud.'),
        escoge,
        read(DSE), respuesta(DSE), rango(ZSE,DSE), nl,

    LSE = [WSE,XSE,YSE,ZSE], sum_elements(LSE, SUMA6), check(SUMA6),
        asserta(tipologia(SUMA6,f)), mayor_que(SUMA5,SUMA6, Rrf),

        tipologia(Rrf, T), assertz(letra(T)), writeln(T).

pe_ju_questions :- 
    
    writeln('Estilo de vida'),nl,
    
    % perceptivos
    writeln('Disfruto al empezar las cosas.'),
        escoge,
        read(AP), respuesta(AP), rango(WP,AP), nl, 
    
    writeln('Me gustan las sorpresas y adaptarme a cambios de última hora.'),
        escoge,
        read(BP), respuesta(BP), rango(XP,BP), nl, 
    
    writeln('Ignoro las listas de cosas pendientes, si es que alguna vez he hecho alguna.'),
        escoge,
        read(CP), respuesta(CP), rango(YP,CP), nl, 
    
    writeln('Adora explorar lo desconocido, aun cuando sea algo tan simple como el camino del trabajo a casa.'),
        escoge,
        read(DP), respuesta(DP), rango(ZP,DP), nl, 
    
    LP = [WP,XP,YP,ZP], sum_elements(LP,SUMA7), check(SUMA7),
        asserta(tipologia(SUMA7,p)),

    % jueces
    writeln('Prefiero que mi vida esté determinada siguiendo un rumbo más o menos prefijado, imponiendo mi fuerza de voluntad en ella.'),
        escoge,
        read(AJ), respuesta(AJ), rango(WJ,AJ), nl, 
    
    writeln('No me gustan las sorpresas, prefiero tener advertencias anticipadas.'),
        escoge,
        read(BJ), respuesta(BJ), rango(XJ,BJ), nl, 
    
    writeln('Tiene un lugar para cada cosa y no se siente satisfecho hasta que cada cosa esta en su sitio.'),
        escoge,
        read(CJ), respuesta(CJ), rango(YJ,CJ), nl, 
    
    writeln('Siempre tiene que esperar a los otros, quienes nunca parecen ser puntuales'),
        escoge,
        read(DJ), respuesta(DJ), rango(ZJ,DJ), nl,

    LJ = [WJ,XJ,YJ,ZJ], sum_elements(LJ,SUMA8), check(SUMA8),
        asserta(tipologia(SUMA8,j)), mayor_que(SUMA7,SUMA8, Rpj),

        tipologia(Rpj, T), assertz(letra(T)), writeln(T).

% Grupos de personalidad
analistas :- ( letra(i),letra(n),letra(t),letra(j) ; letra(i),letra(n),letra(t),letra(p) ) ;
             ( letra(e),letra(n),letra(t),letra(j) ; letra(e),letra(n),letra(t),letra(p) ) ,
             
             (writeln('Los ANALISTAS')).


% 16 Personalidades
personalidad :- letra(i),letra(n),letra(t),letra(j) , analistas,
                writeln('Usted es un(a) INTJ').

personalidad :- letra(i),letra(n),letra(t),letra(p),
                writeln('Usted es un(a) INTP').

personalidad :- letra(e),letra(n),letra(t),letra(j),
                writeln('Usted es un(a) ENTJ'),!.

personalidad :- letra(e),letra(n),letra(t),letra(p),
                writeln('Usted es un(a) ENTP'),!.

personalidad :- letra(i),letra(n),letra(f),letra(j), 
                writeln('Usted es un(a) INFJ'),!.

personalidad :- letra(i),letra(n),letra(f),letra(p),
                writeln('Usted es un(a) INFP'),!.

personalidad :- letra(e),letra(n),letra(f),letra(j), 
                writeln('Usted es un(a) ENFJ'),!.

personalidad :- letra(e),letra(n),letra(f),letra(p), 
                writeln('Usted es un(a) ENFP'),!.

personalidad :- letra(i),letra(s),letra(t),letra(j), 
                writeln('Usted es un(a) ISTJ'),!.

personalidad :- letra(i),letra(s),letra(f),letra(j), 
                writeln('Usted es un(a) ISFJ'),!.

personalidad :- letra(e),letra(s),letra(t),letra(j), 
                writeln('Usted es un(a) ESTJ'),!.

personalidad :- letra(e),letra(s),letra(f),letra(j), 
                writeln('Usted es un(a) ESFJ'),!.

personalidad :- letra(i),letra(s),letra(t),letra(p), 
                writeln('Usted es un(a) ISTP'),!.

personalidad :- letra(i),letra(s),letra(f),letra(p), 
                writeln('Usted es un(a) ISFP'),!.

personalidad :- letra(e),letra(s),letra(t),letra(p), 

                writeln('Usted es un(a) ESTP'),!.

personalidad :- letra(e),letra(s),letra(f),letra(p), 
                writeln('Usted es un(a) ESFP'),!.


imprimir(Lista) :- Lista = [H|C], write(H), nl, imprimir(C).

limpiar :- nl,
           write("*~* Limpiando memoria de trabajo *~*"),
           nl,
           retractall(letra(_)),
           retractall(tipologia(_,_)),
           nl,
           write("*~* Memoria limpia *~*"),
           nl.

main :-
    writeln('Bienvenido al test de personalidad MBTI'),nl,
    writeln("Contesta las siguientes preguntas, se honesto con tus respuestas, evita responder a todo neutral."),
    nl,
    intro_extro_questions, sen_int_questions, rac_sen_questions, pe_ju_questions, personalidad, limpiar.
    
    
