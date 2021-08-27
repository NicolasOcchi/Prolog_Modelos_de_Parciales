% Nicolas Daniel Occhi

:- discontiguous ([tomo/2]). 
% para que prolog no moleste con que 
% los predicados no estan todos seguidos

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

% Se pide:
% 1) Hacer lo que sea necesario para incorporar los siguientes conocimientos:
% a. passarella toma todo lo que no tome Maradona
% b. pedemonti toma todo lo que toma chamot y lo que toma Maradona
% c. basualdo no toma coca cola


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

% 2) Definir el predicado puedeSerSuspendido/1 que relaciona si un jugador puede ser
% suspendido en base a lo que tomó. El predicado debe ser inversible.
% a. un jugador puede ser suspendido si tomó una sustancia que está prohibida
% b. un jugador puede ser suspendido si tomó un compuesto que tiene una sustancia prohibida
% c. o un jugador puede ser suspendido si tomó una cantidad excesiva de un producto
% (más que el máximo permitido):
% ?- puedeSerSuspendido(X).
% X = maradona ;  tomó efedrina y cafeVeloz
% X = chamot ;  tomó cafeVeloz
% X = balbo ;  tomó 2 gatoreits! > 1

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


% 3) Si agregamos los siguientes hechos:

amigo(maradona, caniggia).
amigo(caniggia, balbo).
amigo(balbo, chamot).
amigo(balbo, pedemonti).

% Defina el predicado malaInfluencia/2 que relaciona dos jugadores, si ambos pueden ser
% suspendidos y además se conocen. Un jugador conoce a sus amigos y a los conocidos de sus
% amigos.
% ? malaInfluencia(maradona, Quien).
% Quien = chamot ;
% Quien = balbo ;
% Quien = pedemonti ; (con el agregado del punto 1)
% (Maradona no es mala influencia para Caniggia porque no lo podrían suspender)

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

% 4) Agregamos ahora la lista de médicos que atiende a cada jugador
atiende(cahe, maradona).
atiende(cahe, chamot).
atiende(cahe, balbo).
atiende(zin, caniggia).
atiende(cureta, pedemonti).
atiende(cureta, basualdo).

% Definir el predicado chanta/1, que se verifica para los médicos que sólo atienden a jugadores que
% podrían ser suspendidos. El predicado debe ser inversible.
% ? chanta(X).
% X = cahe
chanta(Medico):-
	distinct(Medico, atiende(Medico,_)), %unifico la variable medico y pongo el distinct para que aparezca un solo resultado de cada uno
	forall( atiende(Medico, Jugador), puedeSerSuspendido(Jugador) ).

% 5) Si conocemos el nivel de alteración en sangre de una sustancia con los siguientes hechos
nivelFalopez(efedrina, 10).
nivelFalopez(cocaina, 100).
nivelFalopez(extasis, 120).
nivelFalopez(omeprazol, 5).
% predicado agregado para que funcione el maplist
nivelFalopez(Sustancia, 0):-
	not((nivelFalopez(OtraSustancia,_), Sustancia \= OtraSustancia)).  % nos fijamos que la sustancia no se encuentre dentro de la base de conocimientos
	

% Definir el predicado cuantaFalopaTiene/2, que relaciona el nivel de alteración en sangre que tiene
% un jugador, considerando que:
% - todos los productos (como la coca cola y el gatoreit), no tienen nivel de alteración (asumir 0)
% - las sustancias tienen definidas el nivel de alteración en base al predicado nivelFalopez/2
% - los compuestos suman los niveles de falopez de cada sustancia que tienen.
% El predicado debe ser inversible en ambos argumentos. Ej: el cafeVeloz tiene nivel 130 (120 del
% éxtasis + 10 de la efedrina, las sustancias que no tienen nivel se asumen 0).
% ?- cuantaFalopaTiene(Jugador, Cantidad).
% Jugador = maradona, Cantidad = 140 ;  tomó efedrina (10) y cafeVeloz (130)
% Jugador = chamot, Cantidad = 130 ;  tomó cafeVeloz (130)

maplist(_, [], []).
maplist(PredicadoTransformador, [Orig|Origs], [Transf|Transfs]):-
     call(PredicadoTransformador, Orig, Transf),
     maplist(PredicadoTransformador, Origs, Transfs).

filter(Criterio, ListaOriginal, ListaNueva):-
   findall(Elem, 
           (member(Elem, ListaOriginal), call(Criterio, Elem)),
           ListaNueva).


% cuantaFalopaTiene(Jugador, AlteracionEnSangre):-

nivelDeAlteracion(producto(_, _), AlteracionEnSangre):- AlteracionEnSangre is 0.

nivelDeAlteracion(sustancia(NombreSustancia), AlteracionEnSangre):- 
	nivelFalopez(NombreSustancia, AlteracionEnSangre).

nivelDeAlteracion(compuesto(NombreCompuesto), AlteracionEnSangre):-
	composicion(NombreCompuesto, IngredientesDelCompuesto),
	maplist(nivelFalopez, IngredientesDelCompuesto, NivelesDeFalopezDeIngredientes),
	sum_list(NivelesDeFalopezDeIngredientes, AlteracionEnSangre).
% no funciona bien, si no encuentra la sustancia tira false y no puede encontrar la alteracion en sangre

% 6) Definir el predicado medicoConProblemas/1, que se satisface si un médico atiende a más de 3
% jugadores conflictivos, esto es
% - que pueden ser suspendidos o
% - que conocen a Maradona (según el punto 3, donde son amigos directos o conocen a
% alguien que es amigo de él). El predicado debe ser inversible.
% ? medicoConProblemas(X).
% X = cahe

medicoConProblemas(Medico):-
	atiende(Medico,_),
	findall(Jugador, (atiende(Medico, Jugador), jugadorConflictivo(Jugador)), JugadoresConflictivos),
	length(JugadoresConflictivos, CantidadDeJugadoresConflictivos),
	CantidadDeJugadoresConflictivos > 3.

jugadorConflictivo(Jugador):-
	puedeSerSuspendido(Jugador).
jugadorConflictivo(Jugador):-
	amigoDeAmigo(maradona, Jugador).


% 7- Definir el predicado programaTVFantinesco/1, que permite armar una combinatoria de
% jugadores que pueden ser suspendidos. Ej:
% ? programaTVFantinesco(Lista)
% Lista = []
% Lista = [maradona]
% Lista = [maradona, chamot]
% Lista = [maradona, chamot, balbo]
% etc. No importa si aparece más de una vez Maradona en su solución.

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