% Nicolas Daniel Occhi

%%% Punto 1 (2 puntos) %%%

% cree(Persona, Personaje).
% entra en juego el concepto de predicados/hechos indivuales.
cree(gabriel, campanita).
cree(gabriel, mago_De_Oz).
cree(gabriel, cavenaghi).
cree(juan, conejo_De_Pascua).
cree(macarena, reyes_Magos).
cree(macarena, mago_Capria).
cree(macarena, campanita).
% diego no cree en nadie -> NO SE MODELA, por principio de universo cerrado, ya que lo
% que no se encuentra en la base de concomiento se presume como falso.

% suenio(Persona, Suenio). donde suenio es un functor.
% entra en juego el concepto de functores que NO son predicados 
% (por ende no tienen valor de verdad) y los utilizamos para agrupar informacion relacionada.

% ganarLoterias[lista de numeros con los que se quiere ganar]
% cantante(cantidad de disco que se quiere vender)
% futbolista(equipo a jugar)
suenio(gabriel, ganarLoteria([5,9])).
suenio(gabriel, futbolista(arsenal)).
suenio(juan, cantante(100000)).
suenio(macarena, cantante(10000)).
% Macarena no quiere ganar la lotería -> NO SE MODELA por principio de universo cerrado
% Macarena (...) sí ser cantante estilo “Eruca Sativa” -> NO MODELAMOS EL ESTILO, ya que
% por como nos aclararon que se modela el suenio de "ser cantante" no corresponde modelar
% el estilo



%%% Punto 2 (4 puntos) %%%
equipo(arsenal, chico).
equipo(aldosivi, chico).

persona(Persona):-suenio(Persona,_).

maplist(_, [], []).
maplist(PredicadoTransformador, [Orig|Origs], [Transf|Transfs]):-
     call(PredicadoTransformador, Orig, Transf),
     maplist(PredicadoTransformador, Origs, Transfs).

esAmbiciosa(Persona):-
	persona(Persona),
	findall(Suenio, suenio(Persona, Suenio), Suenios),
	maplist(dificultadSuenio, Suenios, DificultadesDeSuenios),
	sum_list(DificultadesDeSuenios, SumaDeDificultades),
	SumaDeDificultades > 20.

% ALTERNAIVA SIN MAPLIST 
% (se reemplaza el findall y el maplist por esto)
% findall(Dificultad,(suenio(Persona, Suenio), dificultadSuenio(Suenio,Dificultad)), Dificultades),



% caso cantante >= 500000 discos vendidos
dificultadSuenio(cantante(CantDiscosVendidos), DificultadSuenio):- 
	CantDiscosVendidos >= 500000,
	DificultadSuenio is 6.

% caso cantante < 500000 discos vendidos
dificultadSuenio(cantante(CantDiscosVendidos), DificultadSuenio):-
	CantDiscosVendidos < 500000,
	DificultadSuenio is 4.

% caso cantante ganarLoterias
dificultadSuenio(ganarLoteria(NumerosApostados), DificultadSuenio):-
	length(NumerosApostados, CantNumerosApostados),
	DificultadSuenio is 10*CantNumerosApostados.

% caso jugar en equipo grande
dificultadSuenio(futbolista(Equipo), DificultadSuenio):-
	esEquipoGrande(Equipo),
	DificultadSuenio is 16.

% caso jugar en equipo chico (no jugar en equipo grande)
dificultadSuenio(futbolista(Equipo), DificultadSuenio):-
	not(esEquipoGrande(Equipo)),
	DificultadSuenio is 3.

esEquipoGrande(Equipo):-
	equipo(Equipo, grande).



%%% Punto 3 (4 puntos) %%%
personaje(Personaje):- cree(_,Personaje).


tieneQuimica(Personaje, Persona):-
	cree(Persona, Personaje),
	quimica(Personaje, Persona).

quimica(campanita, Persona):-
	suenio(Persona, Suenio),
	dificultadSuenio(Suenio, DificultadSuenio),
	DificultadSuenio < 5.
quimica(Personaje, Persona):-
	persona(Persona),
	personaje(Personaje),
	Personaje \= campanita, % me parece q NO es necesaria esta linea ya que entra a este caso si se rebota el de arriba.
	forall(suenio(Persona, Suenio), suenioPuro(Suenio)),
	not(esAmbiciosa(Persona)).

suenioPuro(futbolista(_)).
suenioPuro(cantante(CantDiscosVendidos)):- CantDiscosVendidos < 200000.

%%% Punto 4 (2 puntos) %%%

% amigo(Personaje, AmigoDelPersonaje).
amigo(campanita, reyes_Magos).
amigo(campanita, conejo_De_Pascua).
amigo(conejo_De_Pascua, cavenaghi).

estaEnfermo(campanita).
estaEnfermo(reyes_Magos).
estaEnfermo(conejo_De_Pascua).
% mago_Capria NO está enfermo, entonces no se modela

puedeAlegrar(Personaje, Persona):-
	suenio(Persona,_), % la persona tiene algun suenio
	tieneQuimica(Personaje, Persona),
	apto(Personaje).

apto(Personaje):- %caso de que el personaje NO está enfermo
	not(estaEnfermo(Personaje)).
apto(Personaje):- % si esta enfermo, me voy a ver si tiene un amigo que no lo esté
	amigoDeBackup(Personaje, AmigoDelPersonaje),
	not(estaEnfermo(AmigoDelPersonaje)).

%caso base
amigoDeBackup(Personaje, AmigoDelPersonaje):-
	amigo(Personaje, AmigoDelPersonaje).
%caso recursivo
amigoDeBackup(Personaje, AmigoDelPersonaje):-
	amigo(Personaje, AmigoDelAmigoDelPersonaje),
	amigoDeBackup(AmigoDelAmigoDelPersonaje, AmigoDelPersonaje).


:- begin_tests(suenios).

	test(fakeTest):-
		true.

:- end_tests(suenios).