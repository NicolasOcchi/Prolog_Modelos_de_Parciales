% Nicolas Daniel Occhi

:- discontiguous ([tomo/2]). 
/* para que prolog no moleste con que los predicados no estan todos seguidos */

% jugadores conocidos
jugador(maradona).
jugador(chamot).
jugador(balbo).
jugador(caniggia).
jugador(passarella).
jugador(pedemonti).
jugador(basualdo).

% relaciona lo que toma cada jugador
tomo(maradona, sustancia(efedrina)).
tomo(maradona, compuesto(cafeVeloz)).
tomo(caniggia, producto(cocacola, 2)).
tomo(chamot, compuesto(cafeVeloz)).
tomo(balbo, producto(gatoreit, 2)).

% relaciona la máxima cantidad de un producto que 1 jugador puede ingerir
maximo(cocacola, 3). 
maximo(gatoreit, 1).
maximo(naranju, 5).

% relaciona las sustancias que tiene un compuesto
composicion(cafeVeloz, [efedrina, ajipupa, extasis, whisky, cafe]).

% sustancias prohibidas por la asociación
sustanciaProhibida(efedrina).
sustanciaProhibida(cocaina).

%%%% 1)

tomo(passarella, CosaQueToma):- 
	not(tomo(maradona, CosaQueToma)).

tomo(pedemonti, CosaQueToma):- 
	tomo(chamot, CosaQueToma).

tomo(pedemonti, CosaQueToma):- 
	tomo(maradona, CosaQueToma).

/* 
basualdo no toma coca cola --> NO SE MODELA, por principio de universo cerrado todo lo
que no se encuentra en la base de conocimiento se presume falso.
*/

%%%% 2)

puedeSerSuspendido(Jugador):-
	jugador(Jugador),
	tomo(Jugador, CosaIngerida),
	motivoSuspension(CosaIngerida).

motivoSuspension(sustancia(CosaIngerida)):- 
	sustanciaProhibida(CosaIngerida).

motivoSuspension(compuesto(NombreCompuesto)):-
	composicion(NombreCompuesto, IngredientesDelCompuesto),
	member(Ingrediente, IngredientesDelCompuesto), 
	sustanciaProhibida(Ingrediente).

motivoSuspension(producto(ProductoIngerido, CantidadIngerida)):-
	maximo(ProductoIngerido, MaxPermitido),
	CantidadIngerida > MaxPermitido.


%%%% 4)

amigo(maradona, caniggia).
amigo(caniggia, balbo).
amigo(balbo, chamot).
amigo(balbo, pedemonti).

malaInfluencia(Jugador, AmigoDelJugador):-
	jugador(Jugador),
	jugador(AmigoDelJugador),
	amigoDeAmigo(Jugador, AmigoDelJugador),
	puedeSerSuspendido(Jugador),
	puedeSerSuspendido(AmigoDelJugador).

amigoDeAmigo(Jugador, AmigoDelJugador):-
	amigo(Jugador, AmigoDelJugador).
amigoDeAmigo(AmigoDelAmigoDelJugador, AmigoDelJugador):-
	amigo(Jugador, AmigoDelJugador),
	amigoDeAmigo(AmigoDelAmigoDelJugador, Jugador).

%%%% 4)
atiende(cahe, maradona).
atiende(cahe, chamot).
atiende(cahe, balbo).
atiende(zin, caniggia).
atiende(cureta, pedemonti).
atiende(cureta, basualdo).

chanta(Medico):-
	distinct(Medico, atiende(Medico,_)), %unifico la variable medico y pongo el distinct para que aparezca un solo resultado de cada uno
	forall( atiende(Medico, Jugador), puedeSerSuspendido(Jugador) ).

%%%% 5) 
nivelFalopez(efedrina, 10).
nivelFalopez(cocaina, 100).
nivelFalopez(extasis, 120).
nivelFalopez(omeprazol, 5).

cuantaFalopaTiene(Jugador, AlteracionEnSangreTotal):-
	jugador(Jugador),
	findall(AlteracionEnSangre, (tomo(Jugador, CosaIngerida), nivelDeAlteracion(CosaIngerida, AlteracionEnSangre)), Alteraciones),
	sum_list(Alteraciones, AlteracionEnSangreTotal).

% predicados auxiliares
nivelDeAlteracion(producto(_, _), 0).

nivelDeAlteracion(sustancia(NombreSustancia), AlteracionEnSangre):- 
	nivelFalopez(NombreSustancia, AlteracionEnSangre).

nivelDeAlteracion(compuesto(NombreCompuesto), AlteracionEnSangre):-
	composicion(NombreCompuesto, IngredientesDelCompuesto),
	findall(Nivel, (member(Ingrediente, IngredientesDelCompuesto), nivelFalopez(Sustancia, Nivel)), Niveles),
	sum_list(Nivel, AlteracionEnSangre).

/* %% Alternativa con Maplist del "nivelDeAlteracion" para un compuesto
nivelDeAlteracion(compuesto(NombreCompuesto), AlteracionEnSangre):-
	composicion(NombreCompuesto, IngredientesDelCompuesto),
	maplist(nivelFalopezIngredientesDeCompuestos, IngredientesDelCompuesto, NivelesDeFalopezDeLosIngredientes),
	sum_list(NivelesDeFalopezDeLosIngredientes, AlteracionEnSangre).

% predicado sacado del apunte Modulo 7
maplist(_, [], []).
maplist(PredicadoTransformador, [Orig|Origs], [Transf|Transfs]):-
     call(PredicadoTransformador, Orig, Transf),
     maplist(PredicadoTransformador, Origs, Transfs).

predicado auxiliar || en el caso de que el compuesto no se encuentre 
en la base de conocimientos se define 0 su nivel de alteracion en sangre.

si no se agrega este predicado, al hacer el maplist, puede dar false cuando
aparece una sustancia que no esta en la base.

nivelFalopezIngredientesDeCompuestos(IngredientesDelCompuesto, AlteracionEnSangre):-
	not(nivelFalopez(IngredientesDelCompuesto, _)),
	AlteracionEnSangre is 0.
nivelFalopezIngredientesDeCompuestos(IngredientesDelCompuesto, AlteracionEnSangre):-
	nivelFalopez(IngredientesDelCompuesto, AlteracionEnSangre).

*/

%%%% 6) 
medicoConProblemas(Medico):-
	atiende(Medico,_),
	findall(Jugador, (atiende(Medico, Jugador), jugadorConflictivo(Jugador)), JugadoresConflictivos),
	length(JugadoresConflictivos, CantidadDeJugadoresConflictivos),
	CantidadDeJugadoresConflictivos > 3.

jugadorConflictivo(Jugador):-
	puedeSerSuspendido(Jugador).
jugadorConflictivo(Jugador):-
	amigoDeAmigo(maradona, Jugador).


%%%%% 7)
programaTVFantinesco(Lista):-
	findall(Jugador, distinct(Jugador, puedeSerSuspendido(Jugador)), JugadoresSuspendibles),
	combinatoriaDeJugadores(JugadoresSuspendibles, Lista).
	% el distinct es para que maradona no aparezca miles de veces

combinatoriaDeJugadores([],[]).
combinatoriaDeJugadores([Jugador|Jugadores], [Jugador|Posibles]):-
	puedeSerSuspendido(Jugador),
	combinatoriaDeJugadores(Jugadores, Posibles).
combinatoriaDeJugadores([_|Jugadores], Posibles):-
	combinatoriaDeJugadores(Jugadores, Posibles).



:- begin_tests(cafeVeloz).

	test(fakeTest):-
		true.

:- end_tests(cafeVeloz).