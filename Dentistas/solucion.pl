% Nicolas Daniel Occhi

:- discontiguous puedeAtenderA/2.

% Se tiene una base de conocimientos donde se modelan los pacientes que se
% atienden en una clínica odontológica. Cada paciente puede ser:
% • De obra social, representado por el functor pacienteObraSocial(nombre del
% paciente, número de legajo de la obra social, nombre de la obra social)
% • Particular, representado por el functor pacienteParticular(nombre del
% paciente, edad)
% • Pacientes que vienen de otras clínicas odontológicas con las que tienen
% convenio. El paciente puede tener una obra social pero no importa
% registrarla en el modelo. Este paciente se representa con el functor
% pacienteClinica(nombre del paciente, nombre de la clínica)

% Algunos ejemplos de lo que la base contiene:
dentista(pereyra).
dentista(deLeon).
puedeAtenderA(pereyra, pacienteObraSocial(karlsson, 1231, osde)).
puedeAtenderA(pereyra, pacienteParticular(rocchio, 24)).
puedeAtenderA(deLeon, pacienteClinica(dodino, odontoklin)).

% costo de servicios para cada obra social
costo(osde, tratamientoConducto, 200).
costo(omint, tratamientoConducto, 250).

% costo de servicios por atención particular
costo(tratamientoConducto, 1200).

% porcentaje que se cobra a clínicas asociadas
clinica(odontoklin, 80).

% Se pide:
% 1) Agregar el conocimiento sobre los pacientes que puede atender un dentista
% (predicado puedeAtenderA/2):
% - El doctor Cureta puede atender a todos los pacientes particulares de más de
% 60 años y a los pacientes de la clínica Sarlanga.
% - El doctor Patolinger puede atender todos los pacientes que puede atender
% Pereyra y los que no puedan atenderse con De León.
% - El doctor Saieg puede atender a todos los pacientes.
% No es necesario que el predicado sea inversible.

puedeAtenderA(cureta, pacienteParticular(_,Edad)):- Edad > 60.
puedeAtenderA(cureta, pacienteClinica(_,sarlanga)).

puedeAtenderA(patolinger, TipoPaciente):- puedeAtenderA(pereyra, TipoPaciente).
puedeAtenderA(patolinger, TipoPaciente):- not(puedeAtenderA(deLeon, TipoPaciente)).

puedeAtenderA(saieg,_).

/*
2) Relacionar el precio de un servicio para un paciente
- Para los pacientes de obra social, se determina en base al predicado
costo/3 (no es que lo pague el cliente, lo paga la obra social pero no
importa a los fines prácticos del enunciado)
- Para los pacientes particulares, se determina en base al predicado costo/2
(no interviene ninguna obra social). Si el paciente tiene más de 45 años, se
le cobra un adicional de 50 $.
- Para los pacientes que vienen de una clínica con la que tienen convenio, se
le aplica el porcentaje de lo que costaría a un particular (según el predicado
clinica/2, ej: si el servicio particular sale 1000 y la clínica tiene un
porcentaje de 70, el precio de ese servicio es de 700 $).
El predicado debe ser inversible para los últimos dos argumentos.
?- precio(pacienteObraSocial(karlsson, 1231, osde), Servicio, Precio).
Servicio = tratamientoConducto
Precio = 200  es lo que le cobran a los pacientes de Osde
...
*/

% precio(Paciente, Servicio, Precio):-
precio(pacienteObraSocial(_,_,NombreObraSocial), Servicio, PrecioFinal):-
	costo(NombreObraSocial, Servicio, PrecioFinal).

precio(pacienteParticular(_,Edad), Servicio, PrecioFinal):-
	Edad > 45,
	costo(Servicio, Precio),
	PrecioFinal is Precio + 50. % adiconal de 50 pesos por ser mayor a 45 años

precio(pacienteParticular(_,Edad), Servicio, PrecioFinal):-
	Edad < 45,
	costo(Servicio, PrecioFinal).

precio(pacienteClinica(_,NombreClinica), Servicio, PrecioFinal):-
	clinica(NombreClinica, PorcentajeDescuento),
	costo(Servicio, Precio), % unificamos el valor del tratamiento de los particulares
	PrecioFinal is (Precio * (PorcentajeDescuento/100)). % se aplica el descuento de la clinica con conevio

% 3) Se agrega la información de los servicios que realizó cada dentista, con formato
% servicioRealizado(fecha, dentista, servicio(servicio, functor paciente))
servicioRealizado(fecha(10, 11, 2010), pereyra, servicio(tratamientoConducto, pacienteObraSocial(karlsson, 1231, osde))).
servicioRealizado(fecha(16, 11, 2010), pereyra, servicio(tratamientoConducto, pacienteClinica(dodino, odontoklin))).
servicioRealizado(fecha(21, 12, 2010), deLeon, servicio(tratamientoConducto, pacienteObraSocial(karlsson, 1231, osde))).
% Relacionar cuánto facturó un dentista en un mes determinado (considerar sólo el mes, no el año):
% ? montoFacturacion(pereyra, 11, Cuanto).
% Cuanto = 1160 (200 de karlsson + 960 de Dodino, 80% de 1200)
% El predicado debe ser inversible para el primer y el tercer argumento.

cobroDeServicio(Medico, Fecha, Servicio, Cobro):-
	dentista(Medico),
	servicioRealizado(Fecha, Medico, servicio(Servicio, Paciente)),
	precio(Paciente, Servicio, Cobro).

montoFacturacion(Medico, NumeroMes, MontoFacturado):-
	dentista(Medico),
	findall(MontoCobrado, cobroDeServicio(Medico, fecha(_,NumeroMes,_), _, MontoCobrado), Montos),
	sum_list(Montos, MontoFacturado).
	
/*
4) Resolver el predicado dentistaCool/1, que relaciona al dentista que sólo
atendió a pacientes interesantes. El predicado debe ser inversible.
• Un paciente de obra social es interesante cuando el precio del
tratamiento de conducto es de más de 1000 pesos.
• Los pacientes particulares son todos interesantes.
*/

pacienteInteresante(pacienteObraSocial(_,_,NombreObraSocial)):-
	costo(NombreObraSocial, tratamientoConducto, Precio),
	Precio > 1000.
pacienteInteresante(pacienteParticular(_,_)).



% 5) Agregamos en la base la siguiente información:
confia(pereyra, deLeon).
confia(cureta, pereyra).
% Definir el predicado atiendeDeUrgenciaA/2, donde un dentista atiende de
% urgencia a un paciente:
% • Si lo puede atender (definido por el predicado puedeAtenderA/2)
% • Si alguno de los dentistas en los que confía atendería de urgencia a ese
% paciente. El predicado debe funcionar a n niveles posibles (la solución
% debe ser general).
% En el ejemplo, Cureta atendería de urgencia a Dodino, porque confía en Pereyra
% (que a su vez confía en De León que sí lo atendería a Dodino)


% es recursividad como el punto 4 del parcial de sueños

atiendeDeUrgencia(Medico, Paciente):-
	% dentista(Medico),
	apto(Medico, Paciente).

apto(Medico, Paciente):- % caso de que el medico lo pueda atender
	puedeAtenderA(Medico, Paciente).
apto(Medico, Paciente):- % si el medico no lo puede atender, se deriva a un medico de confianza
	amigoDeBackup(Medico, AmigoDelMedico),
	puedeAtenderA(AmigoDelMedico, Paciente).

%caso base
amigoDeBackup(Medico, AmigoDelMedico):-
	confia(Medico, AmigoDelMedico).
%caso recursivo
amigoDeBackup(Medico, AmigoDelMedico):-
	confia(Medico, AmigoDelAmigoDelMedico),
	amigoDeBackup(AmigoDelAmigoDelMedico, AmigoDelMedico).


% 6) Definir el predicado pacienteAlQueLeVieronLaCara/1, que relaciona al
% paciente al cual le realizaron todos servicios caros.
% • Para los pacientes de obra social, se define el predicado
% serviciosCaros /2 de la siguiente manera:
% serviciosCaros(osde, [tratamientoConducto, implanteOseo]).
% • Los servicios a pacientes particulares que cuestan más de 500 pesos se
% consideran caros.
% El predicado debe ser inversible para el único argumento.

/*
pacienteAlQueLeVieronLaCara(Paciente):-
	forall(servicioRealizado(_,_, servicio(ServicioRealizado, Paciente)), )
servicioRealizado(fecha(10, 11, 2010), pereyra, servicio(tratamientoConducto, pacienteObraSocial(karlsson, 1231, osde))).

servicioC
serviciosCaros(osde, [tratamientoConducto, implanteOseo]).
serviciosCaros(Servicio, Precio):- Precio > 500.
*/

% 7) Definir el predicado serviciosMalHechos/2, que relaciona un dentista con
% servicios que están “mal realizados”, esto se determina si un mismo servicio se
% hizo un mes para un dentista y luego al mes siguiente se hizo el mismo servicio
% (no importa si el dentista fue el mismo o no, considerar que el que hizo mal el
% servicio fue el dentista del mes anterior, no el del siguiente).
% ? serviciosMalHechos(Dentista, Servicios).
% Dentista = pereyra
% Servicios = [tratamientoConducto]
% ...
% El predicado debe ser inversible para todos los argumentos.

serviciosMalHechos(Medico, Servicios):-
	dentista(Medico),
	% servicioRealizado(fecha(_,))
	
	servicioRealizado(fecha(_,Mes,_), Medico, servicio(ServicioRealizado, Paciente)),
	servicioRealizado(fecha(_,MesSiguiente,_), OtroMedico, servicio(ServicioRealizado, Paciente)).

% seRepiteServicio(Medico, Servicio):-




:- begin_tests(dentistas).

	test(fakeTest):-
		true.

:- end_tests(dentistas).