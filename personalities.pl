:- style_check(-singleton).

% Suma de elementos
sum_elements([], 0).
sum_elements([X|Xs], S):- sum_elements(Xs, S2), S is S2 + X.

% Numero mayor
choose([], []). 
choose(List, Elt) :- 
     length(List, Length), 
     random(0, Length, Index), 
     nth0(Index, List, Elt).

mayor_que(X,Y,N) :- X > Y, N is X; X < Y, N is Y.
mayor_que(X,Y,N) :- X == Y, N is X. 
mayor_que(X,Y,N) :- Y == X, N is Y.

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
    
    writeln('*** Fuente de Energia ****'),nl,
    
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
    
    writeln('**** Forma de percibir el mundo ****'), nl, 
    
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
    
    writeln('**** Forma de Evaluacion ****'), nl,
    
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
    
    writeln('**** Estilo de vida ****'),nl,
    
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

analistas :- ( ( letra(i),letra(n),letra(t),letra(j) ; letra(i),letra(n),letra(t),letra(p) ) ; ( letra(e),letra(n),letra(t),letra(j) ; letra(e),letra(n),letra(t),letra(p) ) ),
             (writeln('Perteneces al grupo de los ** Analistas **')).

diplomaticos :- ( ( letra(i),letra(n),letra(f),letra(j) ; letra(i),letra(n),letra(f),letra(p) ) ; ( letra(e),letra(n),letra(f),letra(j) ; letra(e),letra(n),letra(f),letra(p) ) ),
             (writeln('Perteneces al grupo de los ** Diplomáticos **')).

centinelas :- ( ( letra(i),letra(s),letra(t),letra(j) ; letra(i),letra(s),letra(f),letra(j) ) ; ( letra(e),letra(s),letra(t),letra(j) ; letra(e),letra(s),letra(f),letra(f) ) ),
             (writeln('Perteneces al grupo de los ** Centinelas **')).

exploradores :- ( ( letra(i),letra(s),letra(t),letra(p) ; letra(i),letra(s),letra(f),letra(p) ) ; ( letra(e),letra(s),letra(t),letra(p) ; letra(e),letra(s),letra(f),letra(p) ) ),
             (writeln('Perteneces al grupo de los ** Exploradores **')).

% 16 Personalidades

personalidad :- letra(i),letra(n),letra(t),letra(j), nl, analistas, nl,
                writeln(' *** Usted es un(a) INTJ(Arquitecto) ***'), nl,
                writeln('Descripción: Pensadores, imaginativos y estratégicos, con un plan para todo...'), nl,
                writeln('Arquitectos Famosos: Friedrich Nietzsche, Michelle Obama, Elon Musk'), nl,
                writeln('Solo el 1.5% de la población tiene esta personalidad.'), nl,!.

personalidad :- letra(i),letra(n),letra(t),letra(p), nl, analistas, nl,
                writeln('Usted es un(a) INTP(Lógico)'), nl,
                writeln('Descripción: Inventores, innovadores con una sed insaciable de conocimiento...'), nl,
                writeln('Lógicos Famosos: Bill Gates, Albert Einstein, Isaac Newton'), nl,
                writeln('Solo el 2.5% de la población tiene esta personalidad.'),!.

personalidad :- letra(e),letra(n),letra(t),letra(j), nl, analistas, nl,
                writeln('Usted es un(a) ENTJ(Comandante)'), nl,
                writeln('Descripción: Líderes, audaces, imaginativos y de voluntad fuerte, siempre en busca de un camnino, o creando uno...'), nl,
                writeln('Comandantes Famosos: Steve Jobs, Gordon Ramsay, Jim Carrey'), nl,
                writeln('Solo el 4% de la población tiene esta personalidad.'),!.

personalidad :- letra(e),letra(n),letra(t),letra(p), nl, analistas, nl,
                writeln('Usted es un(a) ENTP(Innovador)'), nl, 
                writeln('Descripción: Pensadores inteligentes y curiosos que no pueden resistir un reto intelectual..'), nl,
                writeln('Innovadores Famosos: Mark Twain, Tom Hanks,Thomas Edison'), nl,
                writeln('Solo el 4.5% de la población tiene esta personalidad.'),!.

personalidad :- letra(i),letra(n),letra(f),letra(j), nl, diplomaticos, nl,
                writeln('Usted es un(a) INFJ(Abogado)'), nl,
                writeln('Descripción: Callados y místicos,, que sin embargo son inspiradores e idealistas incansables...'), nl,
                writeln('Abogados Famosos: Nelson Mandela, Madre Teresa, Martin Luther King'), nl,
                writeln('Solo el 1% de la población tiene esta personalidad.'),!.

personalidad :- letra(i),letra(n),letra(f),letra(p), nl, diplomaticos, nl,
                writeln('Usted es un(a) INFP(Mediador)'), nl,
                writeln('Descripción: Personas poéticas, amables y altruistas, siempre en busca de ayudar a una buena causa...'), nl,
                writeln('Mediadores Famosos: Willian Shakespeare, Alicia Keys, Jhonny Depp'), nl,
                writeln('Solo el 2% de la población tiene esta personalidad.'),!.

personalidad :- letra(e),letra(n),letra(f),letra(j), nl, diplomaticos, nl,
                writeln('Usted es un(a) ENFJ(Protagonista)'), nl,
                writeln('Descripción: Líderes carismáticos e inspiradores, capaces de cautivar a quienes los escuchan...'), nl,
                writeln('Protagonistas Famosos: Barack Obama, Ben Affleck, Morpheus'), nl,
                writeln('Solo el 4% de la población tiene esta personalidad.'),!.

personalidad :- letra(e),letra(n),letra(f),letra(p), nl, diplomaticos, nl,
                writeln('Usted es un(a) ENFP(Activista)'), nl,
                writeln('Descripción: Espíritus libres y entusiastas, creativos y sociales, que siempre pueden encontrar una razón para sonreir...'), nl,
                writeln('Activistas Famosos: Robert Downey, Quentin Tarantino, Robin Williams'), nl,
                writeln('Solo el 7% de la población tiene esta personalidad.'),!.

personalidad :- letra(i),letra(s),letra(t),letra(j), nl, centinelas, nl,
                writeln('Usted es un(a) ISTJ(Logistica)'), nl,
                writeln('Descripción: Individuos prácticos y enfocados a los hechos, de cuya confiabilidad no puede dudarse...'), nl,
                writeln('Logístas Famosos: Denzel Washington, George Bush, Anthony Hopkins'), nl,
                writeln('Solo el 8.5% de la población tiene esta personalidad.'),!.

personalidad :- letra(i),letra(s),letra(f),letra(j), nl, centinelas, nl,
                writeln('Usted es un(a) ISFJ(Defensor)'), nl,
                writeln('Descripción: Protectores muy dedicados y cálidos, siempre listos para defender a sus seres queridos...'), nl,
                writeln('Defensores Famosos: Beyonce, Vin Diesel, Reina Elizabeth'), nl,
                writeln('Solo el 7% de la población tiene esta personalidad.'),!.

personalidad :- letra(e),letra(s),letra(t),letra(j), nl, centinelas, nl,
                writeln('Usted es un(a) ESTJ(Ejecutivo)'), nl,
                writeln('Descripción: Administradores excelentes, inigualables al adiministrar cosas y personas...'), nl,
                writeln('Ejecutivos Famosos: Frank Sinatra, John Rockefeller, Sonia Sotomayor'), nl,
                writeln('Solo el 13% de la población tiene esta personalidad.'),!.

personalidad :- letra(e),letra(s),letra(f),letra(j), nl, centinelas, nl,
                writeln('Usted es un(a) ESFJ(Cónsul)'), nl,
                writeln('Descripción: Personas extraordinariamente consideradas, sociables y ppopulares, siempre en busca de ayudar...'), nl,
                writeln('Cónsules Famosos: Bill Clinton, Taylor Swift, Steve Harvey'), nl,
                writeln('Solo el 12% de la población tiene esta personalidad.'),!.

personalidad :- letra(i),letra(s),letra(t),letra(p), nl, exploradores, nl,
                writeln('Usted es un(a) ISTP(Virtuoso)'), nl,
                writeln('Descripción: Experimentadores audaces y prácticos, maestros en el uso de todo tipo de herramientas...'), nl,
                writeln('Virtuosos Famosos: Michael Jordan, Clint Eastwood, Tom Cruise'), nl,
                writeln('Solo el 6% de la población tiene esta personalidad.'),!.

personalidad :- letra(i),letra(s),letra(f),letra(p), nl, exploradores, nl,
                writeln('Usted es un(a) ISFP(Aventurero)'), nl,
                writeln('Descripción: Artistas flexibles y encantadores, siempre listos para explorar y experimentar algo nuevo...'), nl,
                writeln('Aventureros Famosos: Lana del Rey, Frida Kahlo, Michael Jackson'), nl,
                writeln('Solo el 6% de la población tiene esta personalidad.'),!.

personalidad :- letra(e),letra(s),letra(t),letra(p), nl, exploradores, nl,
                writeln('Usted es un(a) ESTP(Emprendedor)'), nl,
                writeln('Descripción: Personas inteligentes, enérgicas y muy perceptivas, que realmente disfrutan vivir al límite...'), nl,
                writeln('Emprendedores Famosos: Jack Nicholson, Ernest Hemingway, Bruce Willis'), nl,
                writeln('Solo el 10% de la población tiene esta personalidad.'),!.

personalidad :- letra(e),letra(s),letra(f),letra(p), nl, exploradores, nl,
                writeln('Usted es un(a) ESFP(Animador)'), nl,
                writeln('Descripción: Animadores espontáneos, enérgicos y entusiastas. La vida nunca es aburrid a su alrededor...'), nl,
                writeln('Animadores Famosos: Elton john, Marilin Monroe, Adele'), nl,
                writeln('Solo el 11% de la población tiene esta personalidad.'),!.


imprimir(Lista) :- Lista = [H|C], write(H), nl, imprimir(C).

limpiar :- nl,
           retractall(letra(_)),
           retractall(tipologia(_,_)),
           nl,
           write("*~* Memoria limpia *~*"),
           nl.

main :-
    writeln('*** Bienvenido al test 16 Personalities ***'),nl,
    writeln("Contesta las siguientes preguntas, se honesto con tus respuestas, evita responder a todo neutral."),
    nl,
    intro_extro_questions, sen_int_questions, rac_sen_questions, pe_ju_questions, personalidad, limpiar.
    
    
