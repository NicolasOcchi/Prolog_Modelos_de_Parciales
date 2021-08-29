% Nicolas Daniel Occhi

%%% Parte 1 - Sombrero Seleccionador %%%
%%% hay dos formas de hacerlo: predicados indivuales o juntar todo en un solo predicado %%%

%%% PREDICADOS "UNIFICADOS" (justo en este parcial el tema de la lista de caracteristicas no causaba problemas) %%% 

% mago(NombreMago, StatusSangre, Caracteristicas, CasaQueOdiaríaIr).
% Caracteristicas es un functor el cual contiene una lista
mago(harry, mestizo, [corajudo, amistoso, orgulloso, inteligente], odiriaIrA(slytherin)).
mago(draco, pura, [inteligente, orgulloso], odiriaIrA(hufflepuff)).
% % (...) pero no es corajudo ni amistoso. -> NO SE MODELA, por principio de universo cerrado
% % lo que no se encuentra dentro de la base de conocimientos se presume como falso.
mago(hermione, impura, [inteligente, orgulloso, responsable], _).

caracteristicaCasa(gryffindor, coraje).
caracteristicaCasa(slytherin, orgullo).
caracteristicaCasa(slytherin, inteligencia).
caracteristicaCasa(ravenclaw, inteligencia).
caracteristicaCasa(ravenclaw, responsabilidad).
caracteristicaCasa(hufflepuff, amistoso).

%% 1)
permiteEntrar(Casa, _):-
	Casa \= slytherin.
permiteEntrar(slytherin, Mago):-
	mago(Mago, Sangre, _, _),
	Sangre \= impura.

%% 2)
magoApropiadoParaCasa(Casa, Mago):-
	mago(Mago, _, ListaCaracteristicas, _),
	forall(caracteristicaCasa(Casa, Caracteristca), member(Caracteristca, ListaCaracteristicas)).

%% 3)
podriaQuedarSeleccionado(Mago, Casa):-
	permiteEntrar(Casa, Mago),
	magoApropiadoParaCasa(Casa, Mago),
	mago(Mago, _,_, odiriaIrA(CasaQueOdia)),
	CasaQueOdia \= Casa.

%4) -> Sacado de la resolucion de los profes 
cadenaDeAmistades(ListaMagos):-
    todosAmistosos(ListaMagos),
    cadenaDeCasas(ListaMagos).

todosAmistosos(ListaMagos):-
    forall(member(Mago,ListaMagos), esAmistoso(Mago)).

esAmistoso(Mago):-
    mago(Mago, _,ListaCaracteristicas, _),
	member(amistoso, ListaCaracteristicas).

cadenaDeCasas([Mago1,Mago2 | MagosSiguientes]):- % -> Planteamos predicado con lista generica
	podriaQuedarSeleccionado(Mago1,Casa),
	podriaQuedarSeleccionado(Mago2,Casa),
    cadenaDeCasas([Mago2 | MagosSiguientes]).

cadenaDeCasas([_]). % -> Caso base en caso de que la lista sea un solo elem.
cadenaDeCasas([]). % -> Caso base en caso de que la lista esta vacia.


%%% PREDICADOS INDIVUALES (es mas "sencillo" de trabajar) %%% 
/*
mago(harry).
mago(draco).
mago(hermione).

tipoSangre(harry, mestizo).
tipoSangre(draco, pura).
tipoSangre(hermione, impura).

caracteristicaMago(harry, corajudo).
caracteristicaMago(harry, amistoso).
caracteristicaMago(harry, orgulloso).
caracteristicaMago(harry, inteligente).
caracteristicaMago(draco, inteligente).
caracteristicaMago(draco, orgulloso).
% Draco (...) pero no es corajudo ni amistoso. -> NO SE MODELA, por principio de universo cerrado
% lo que no se encuentra dentro de la base de conocimientos se presume como falso.
caracteristicaMago(hermione, inteligente).
caracteristicaMago(hermione, orgulloso).
caracteristicaMago(hermione, responsable).

odiriaIr(harry, slytherin).
odiriaIr(draco, hufflepuff).
% Hermione (...) No hay ninguna casa a la que odiaría ir -> NO SE MODELA, por principio de universo cerrado
% lo que no se encuentra dentro de la base de conocimientos se presume como falso.

% caracteristicaSombrero(Casa, Caracteristica).
caracteristicaSombrero(gryffindor, coraje).
caracteristicaSombrero(slytherin, orgullo).
caracteristicaSombrero(slytherin, inteligencia).
caracteristicaSombrero(ravenclaw, inteligencia).
caracteristicaSombrero(ravenclaw, responsabilidad).
caracteristicaSombrero(hufflepuff, amistoso).

%% 1) 
% permiteEntrar(Casa, Mago).
permiteEntrar(Casa, _):-
	Casa \= slytherin.

permiteEntrar(slytherin, Mago):-
	not(tipoSangre(Mago, impura)). 

%% 2)
caracterApropiadoParaLaCasa(Casa, Mago):-
	mago(Mago),
	forall(caracteristicaSombrero(Casa, Caracteristica), caracteristicaMago(Mago, Caracteristica)).

%% 3)
seleccionSombrero(Mago, CasaSeleccionada):-
	caracterApropiadoParaLaCasa(CasaSeleccionada, Mago),
	not(odiriaIr(Mago, CasaSeleccionada)).

seleccionSombrero(hermione, gryffindor). % caso donde hermione hackea el sombrero.

%% 4) 
cadenaDeAmistades(Mago):-
	findall(Mago, (seleccionSombrero(Mago, CasaSeleccionada), caracteristicaMago(Mago, amistoso)), MagosCompanieros).


*/

%%%%% Parte 2 - La copa de las casas %%%%%

% malaAccion(Accion, PuntosQueResta).
malaAccion(andar_De_Noche_Fuera_De_La_Cama, -50).
malaAccion(fueA(LugarProhibido), PuntosQueResta):- % ir a un lugar prohibido
	sancionLugarProhibido(LugarProhibido, PuntosQueResta).

% accion(Mago, AccionCometida).
accion(harry, andar_De_Noche_Fuera_De_La_Cama).
accion(hermione, fueA(tercer_Piso)).
accion(hermione, fueA(seccion_Restringida_Biblioteca)).
accion(harry, fueA(bosque)).
accion(harry, fueA(tercer_Piso)).
accion(draco, fueA(mazmorras)).

% BuenasAcciones
accionPremiada(ron, ganarAjedrezMagico, 50).
accionPremiada(hermione, salvarAmigosDeMuerteHorrible, 50).
accionPremiada(harry, ganarleAVoldemor, 60).

% BuenasAcciones
buenaAccion(Accion, Puntaje):-
	accionPremiada(_,Accion, Puntaje).

% lugarProhibido(Lugar, PuntosQueResta).
sancionLugarProhibido(bosque, -50).
sancionLugarProhibido(seccion_Restringida_Biblioteca, -10).
sancionLugarProhibido(tercer_Piso, -75).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).


%%%%% 1)
%hizoAlgunaAccion(Mago, Accion).
hizoAlgunaAccion(Mago, Accion):-
	accion(Mago, Accion).
hizoAlgunaAccion(Mago, AccionPremiada):-
	accionPremiada(Mago, AccionPremiada, _).

% a)
buenAlumno(Mago):-
	hizoAlgunaAccion(Mago,_), % si hizo alguna accion.
	not((hizoAlgunaAccion(Mago, Accion), malaAccion(Accion,_))).
	% no existe una accion cometida por el mago que sea una mala accion
	% ese not es equivalente a:
	% forall(accion(Mago, AccionCometida), not(malaAccion(AccionCometida,_))).

% b)
accionRecurrente(Accion):-
	accion(Mago, Accion),
	accion(OtroMago, Accion),
	Mago \= OtroMago.

%%%%% 2)
casa(Casa):- caracteristicaCasa(Casa,_).

puntajeTotalCasa(Casa, PuntajeTotal):-
	casa(Casa), % se unifica la variable Casa
	findall(Puntos, puntosDeMiembroDeCasa(Casa, Puntos), PuntosDeTodosLosMiembros),
	sum_list(PuntosDeTodosLosMiembros, PuntajeTotal).

% predicados auxiliares
puntosDeMiembroDeCasa(Casa, Puntos):-
    esDe(Mago, Casa), % miembro de la casa   
    puntosQueHizo(Mago, Puntos).

puntosQueHizo(Mago,PuntosNeg):-
        hizoAlgunaAccion(Mago, Accion),
        malaAccion(Accion, PuntosNeg).

puntosQueHizo(Mago,PuntosPos):-
        hizoAlgunaAccion(Mago, Accion),
        buenaAccion(Accion, PuntosPos).

%%%%% 3)
casaGanadora(Casa):-
	casa(Casa), % se unifica la variable Casa
	forall(puntajeTotalCasa(Casa, PuntajeTotal), PuntajeTotal > PuntajeTotal).

%%%%% 4)
accion(hermione, respondiopregunta(donde_se_encuentra_un_Bezoar, 20, snape)).
accion(hermione, respondiopregunta(como_hacer_para_levitar_una_pluma, 25, flitwick)).

buenaAccion(respondiopregunta(Pregunta, Dificultad, snape), Puntos):-
	accion(_, respondiopregunta(Pregunta, Dificultad, snape)).
	Puntos is Dificultad/2.

buenaAccion(respondiopregunta(Pregunta, Dificultad, Profesor), PuntosReales):-
	accion(_, respondiopregunta(Pregunta, Dificultad, Profesor)).
	Profesor \= snape.


	

:- begin_tests(casasDeHogwarts).

	test(fakeTest):-
		true.

:- end_tests(casasDeHogwarts).