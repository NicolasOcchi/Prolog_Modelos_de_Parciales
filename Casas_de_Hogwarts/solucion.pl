% Nicolas Daniel Occhi

%%% Parte 1 - Sombrero Seleccionador %%%

% mago(NombreMago, StatusSangre, Caracteristicas, CasaQueOdiaríaIr).
% Caracteristicas es un functor el cual contiene una lista
% mago(harry, mestizo, caracteristicas([corajudo, amistoso, orgulloso, inteligente]), slytherin).
% mago(draco, pura, caracteristicas([inteligente, orgulloso]), hufflepuff).
% % (...) pero no es corajudo ni amistoso. -> NO SE MODELA, por principio de universo cerrado
% % lo que no se encuentra dentro de la base de conocimientos se presume como falso.
% mago(hermione, impura, caracteristicas([inteligente, orgulloso, responsable]), _).


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
odiriaIr(harry, hufflepuff).
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
permiteEntrar(gryffindor, _).
permiteEntrar(hufflepuff, _).
permiteEntrar(ravenclaw, _).
permiteEntrar(slytherin, Mago):- not(tipoSangre(Mago, impura)). 

%% 2)
caracterApropiadoParaLaCasa(Casa, Mago):-
	mago(Mago),
	forall(caracteristicaSombrero(Casa, Caracteristica), caracteristicaMago(Mago, Caracteristica)).

%% 3)
seleccionSombrero(Mago, CasaSeleccionada):-
	caracterApropiadoParaLaCasa(CasaSeleccionada, Mago),
	not(odiriaIr(Mago, CasaSeleccionada)).

seleccionSombrero(hermione, gryffindor). % caso donde hermione hackea el sombrero.

%% 4) ????
% Definir un predicado cadenaDeAmistades/1 que se cumple para una lista de magos si todos ellos se
% caracterizan por ser amistosos y cada uno podría estar en la misma casa que el siguiente. No hace
% falta que sea inversible, se consultará de forma individual.
cadenaDeAmistades(Mago):-
	findall(Mago, (seleccionSombrero(Mago, CasaSeleccionada), caracteristicaMago(Mago, amistoso)), MagosCompanieros).


%%% Parte 2 - La copa de las casas %%%



:- begin_tests(casasDeHogwarts).

	test(fakeTest):-
		true.

:- end_tests(casasDeHogwarts).