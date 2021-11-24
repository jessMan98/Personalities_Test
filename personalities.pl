:- style_check(-singleton).

% Suma de elementos
sum_elements([], 0).
sum_elements([X|Xs], S):- sum_elements(Xs, S2), S is S2 + X.

% Respuestas.
respuesta(X) :- member(X,[td,up,n,nr,nd]).

% Rango de las respuestas
rango(3,td).
rango(2,up).
rango(1,n).
rango(0,nr).
rango(0,nd).

check(S) :- S is 16.
check(S) :- S is 8.
check(S) :- S > 8 , S < 16. 
check(S) :- S is 4.
 
% Preguntas introversion
intro_question:- 
    writeln('Evito las multitudes y busco la tranquilidad.'),
    read(A),respuesta(A), rango(X,A),
    writeln('Prefiero pequeñas reuniones con amigos cercanos.'),
    read(B),respuesta(B), rango(Y,B),
    write('Estar sólo me recarga las pilas.'),
    read(C),respuesta(C), rango(W,C),
    write('Ensaya las cosas antes de decirlas; a menudo contesta con "lo tendrá que pensar o le contesto más tarde"'),
    read(D),respuesta(D), rango(Z,D),
    L=[X,Y,W,Z], sum_elements(L,SUMA).


% Dimensiones dicotomicas
fuente_energia(extroversion(e,pregunta(A)), introversion(i)). %retorna true
percibir_mundo(sensorial(s), intuitivo(n)).
forma_evaluacion(racionalista(t),sentimental(f)).
estilo_vida(perceptivos(p), jueces(j)).

% respuestas
imprimir(Lista) :- Lista = [H|C], write(H), nl, imprimir(C).

extro_question(R):-
    writeln('Me distraigo con facilidad, sin mucha concentración en una única tarea.'),
    respuesta(R),
    writeln('Me gusta ser el centro de atención.'),
    respuesta(R).

i_e(R):- intro_question(R), extro_question(R).

main :-
    writeln('Bienvenido al test de personalidad MBTI'),nl,
    writeln("Contesta las siguientes preguntas, se honesto con tus respuestas, evita responder a todo neutral"),
    nl,
    i_e(R).