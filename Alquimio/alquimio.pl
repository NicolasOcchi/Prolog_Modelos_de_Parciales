% Nicolas Daniel Occhi

%% 1)

persona(Persona):- inventario(Persona,_).

% inventario(Persona, Material).
inventario(ana, agua).
inventario(ana, tierra).
inventario(ana, vapor).
inventario(ana, hierro).

inventario(beto, Material):-
	inventario(ana, Material).

inventario(cata, fuego).
inventario(cata, tierra).
inventario(cata, agua).
inventario(cata, aire).
/* cata (...) pero no tiene vapor -> NO SE MODELA, por principio de universo cerrado
ya que todo lo que no se encuentra en la base de datos se presume falso. */

/*
construir(pasto, Persona):-
	inventario(Persona, agua),
	inventario(Persona, tierra).

construir(hierro, Persona):-
	inventario(Persona, fuego),
	inventario(Persona, agua),
	inventario(Persona, tierra).

construir(huesos, Persona):-
	inventario(Persona, pasto),
	inventario(Persona, agua).

construir(presion, Persona):-
	inventario(Persona, hierro),
	inventario(Persona, vapor).

construir(vapor, Persona):-
	inventario(Persona, agua),
	inventario(Persona, fuego).

construir(playStation, Persona):-
	inventario(Persona, silicio),
	inventario(Persona, hierro),
	inventario(Persona, plastico).

construir(silicio, Persona):-
	inventario(Persona, tierra).

construir(plastico, Persona):-
	inventario(Persona, huesos),
	inventario(Persona, presion).

*/

% elemento(NombreElemento, MaterialesNecesariosParaConstruirlo).
requiere(pasto, [agua, tierra]).
requiere(hierro, [fuego, agua, tierra]).
requiere(huesos, [pasto, agua]).
requiere(presion, [hierro, vapor]).
requiere(vapor, [agua, fuego]).
requiere(playStation, [silicio, hierro, plastico]).
requiere(silicio, [tierra]).
requiere(plastico, [huesos, presion]).


% Los círculos alquímicos tienen diámetro en cms y cantidad de niveles.
% Las cucharas tienen una longitud en cms.
% Hay distintos tipos de libro.
herramienta(ana, circulo(50,3)).
herramienta(ana, cuchara(40)).
herramienta(beto, circulo(20,1)).
herramienta(beto, libro(inerte)).
herramienta(cata, libro(vida)).
herramienta(cata, circulo(100,5)).


%% 2)
tieneIngredientesPara(Elemento, Persona):-
	requiere(Elemento, Ingredientes),
	forall(member(Ingrediente, Ingredientes), inventario(Persona, Ingrediente)).
	% si ponia al reves los predicados del forall no funcionaba, buscaba que todos las cosas que tiene en el inventario sean parte de la lista de ingredientes

%% 3)

/* esta vivo si el elemento esta construido con agua o fuego, o en todo caso, si alguno 
de los ingredientes del elemento está hecho de agua o fuego  */

% lo q saque de la resolucion, esta forma de hacer recursividad nunca se me hubiese ocurrido.

%casos base
estaVivo(fuego).
estaVivo(agua).
%caso recursivo
estaVivo(Elemento):-
	requiereElemento(Elemento, Ingredientes),
	estaVivo(Elemento).

requiereElemento(ElementoCompuesto, Material) :-
	requiere(ElementoCompuesto, Materiales),
	member(Material, Materiales).

puedeConstruir(Elemento, Persona):-
	tieneIngredientesPara(Elemento, Persona),
	tieneHerramientaParaConstruir(Elemento, Persona).

tieneHerramientaParaConstruir(Elemento, Persona):-
	herramienta(Persona, Herramienta),
	sirveParaConstruir(Herramienta, Elemento).

sirveParaConstruir(libro(vida), Elemento):-
	estaVivo(Elemento).
sirveParaConstruir(libro(inerte), Elemento):-
	not(estaVivo(Elemento)).

sirveParaConstruir(Herramienta, Elemento) :- 
	cantidadSoportada(Herramienta, CantidadSoportada),
	cantidadDeIngredientes(Elemento, CantidadIngredientes),
	CantidadSoportada >= CantidadIngredientes.

cantidadDeIngredientes(ElementoCompuesto, Cantidad) :-
	requiere(ElementoCompuesto, Materiales),
	length(Materiales, Cantidad).

% cantidadSoportada(Herramienta, Cantidad)
cantidadSoportada(cuchara(Longitud), Cantidad) :-
	Cantidad is Longitud / 10.
cantidadSoportada(circulo(Diametro, Niveles), Cantidad) :-
	Cantidad is Diametro / 100 * Niveles.

%% 5)
esTodoPoderoso(Persona):-
	persona(Persona),
	tieneTodosLosPrimitivos(Persona),
	tieneHerramientasParaFaltantes(Persona).

tieneTodosLosPrimitivos(Persona):-
	forall(inventario(Persona, Elemento), not(requiereElemento(Elemento,_))).

tieneHerramientasParaFaltantes(Persona):-
	forall(not(inventario(Persona, Elmento)), tieneHerramientaParaConstruir(Elemento, Persona)).

%% 6)
% quienGana(Persona)
quienGana(Persona) :-
	persona(Persona),
	forall(contrincante(Persona, Contrincante), contruyeMasCosas(Persona, Contrincante)).
  
  contrincante(Persona, Contrincante) :-
	persona(Persona),
	persona(Contrincante),
	Persona \= Contrincante.
  
  contruyeMasCosas(Ganador, Perdedor) :-
	cantidadDeElementosQuePuedeConstruir(Ganador, Mayor),
	cantidadDeElementosQuePuedeConstruir(Perdedor, Menor),
	Mayor > Menor.
  
  cantidadDeElementosQuePuedeConstruir(Persona, Cantidad) :-
	findall(Elemento, puedeConstruir(Persona, Elemento), Elementos),
	list_to_set(Elementos, ElementosSinRepetidos),
	length(ElementosSinRepetidos, Cantidad).


%% 7)
/* Mencionar un lugar de la solución donde se haya hecho uso del concepto de universo cerrado.
en el punto 1, donde dince "cata (...) pero no tiene vapor" -> NO SE MODELA, por 
principio de universo cerrado ya que todo lo que no se encuentra en la 
base de datos se presume falso. */

%% 8)
puedeLlegarATener(Persona, Elemento) :-
	inventario(Persona, Elemento).
  
puedeLlegarATener(Persona, Elemento) :-
	tieneHerramientaParaConstruir(Persona, Elemento),
	forall(requiereElemento(Elemento, Ingrediente), puedeLlegarATener(Persona, Ingrediente)).


:- begin_tests(alquimio).

	test(fakeTest):-
		true.

:- end_tests(alquimio).