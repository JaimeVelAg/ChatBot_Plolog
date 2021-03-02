import Data.Char;
import Data.List;

-- Lista de Frases --
frases = [  "¿Quien es Joxim?","¿Quien te programó?","Sobre que tema podemos hablar o charlar?","¿Te gustan los Videojuegos?",
						"Hola","Quienes son tus creadores?","Eres una Inteligencia Artificial?","Ya instalaste Linux?","Cual es tu nombre?",
						"¿Cuál es la ubicación del tec?","¿Quién es el mejor maestro del tec?","¿Quién es el peor maestro del tec?",
            "¿Cuál es la mejor carrera?", "¿Cuál es la materia más difícil?","¿Cuál es la materia más fácil",
            "¿Cuál es la carrera más fáci?", "¿Qué carrera me recomiendas estudiar?", "¿Está rica la comida de la cafetería?",
            "¿Dónde se encuentran los laboratorios de sistemas?", "¿En que edificio toman clase los alumnos de sistemas?", "¿Quién es el mejor alumno del tec?",
            "¿Qué mes es el pony fest?", "¿Qué mes es el aniversario del tec?",
            "¿Cuál es la materia favorita de los alumnos?", "¿Quién es el peor alumno del tec?", "¿Cuál es el deporte preferido por los alumnos del tec?",
            "¿Quiénes son los alumnos más inteligentes?", "Tengo hambre", "La mayor cantidad de alumnos de sistemas son hombres o mujeres",
            "¿Donde se juntan los frikis de la carrera?", "¿Los alumnos de sistemas hacen deporte?", "A los alumnos de sistemas del gusta el futbol",
            "¿Los alumnos de sistemas son inteligentes?", "¿Cuál es el número telefónico del tec?", "Quién enseña matematicas en el tec?",
            "¿Cuáles son los salones más bonitos?", "Qué computadora usas?", "Para qué usas tu celular?",
            "¿La mayoria de los alumnos son locales o foraneos?","¿Son más inteligentes los alumnos locales o foraneos?", "Cual es el correo electronico del tec?",
            "Qué me recomiendas comer en el tec?", "¿Cuándo es el ponyfest?", "Que hacen los alumnos de sistemas?", "Cual es el mejor sistema operativo?",
            "Tengo mucha tarea"]

-- Lista de Respuestas --
respuestas = [  "El mejor profe de Sistemas","Diego Zamora y Jaime Isai son mis Creadores",
								"Me puedes hacer preguntas sobre el TEC, la Cafeteria y sobre los alumnos de Sistemas","Si!!, Me gusta mucho Fortnite y FIFA",
								"Hola!!","Diego Zamora y Jaime Isai son mis Creadores","Para los expertos en Sistemas no soy una Inteligencia Artifical",
								"Soy multiplataforma, tengo un Dual Boot","Me dicen Carlos Cervantes","Morelia, Mich. Av tecnologico","Joxim Gallegos","El que te reprobo",
                "La carrera de sistemas", "Programacion logica y funcional", "Etica",
                "Industrial", "Depende de lo que te guste, pero sistemas es buena opcion", "Nunca comas nada de ahi",
                "En el edificio AG", "En el edificio k", "Jaime Velazquez, es el CR7 de la escuela",
                "Es en el mes de abril, si no lo cancelan :(", "Es a finales de marzo",
                "Cualquier materia que de Joxim", "Un tal Jesus Ivan de Puruandiro", "Futbol, yo creo",
                "Los alumnos de administracion", "Te recomiendo que pidas algo por internet", "La mayoria son hombres",
                "En la pony plaza", "La mayoria, no", "No saben ni que es un balon, la mayoria",
                "A veces son inteligentes", "4431008978", "Gerardo Medina es muy bueno",
                "Los del tec, pero de Monterrey", "No tengo, solo una calculadora", "Para jugar among us y estar en whatsapp",
                "La mayoria son alumnos de Morelia", "Los foraneos, sin duda", "nomelose@itmorelia.edu.mx",
                "Te recomiendo comer hasta llegar a tu casa", "Es en el mes de abril, si no lo cancelan :(", "Hacer tarea todo el dia", "Mac OS X",
                "Animo, tu puedes"]

-- stop words
stopWords = ["a", "aca", "ahi","ajena","el","algo","algun","alguna","alguno","algunos","algunas","alla","alli","ambos","ante","antes","aquel","aquella","aquello","aqui"
            ,"arriba","asi","atras","aun","aunque","bajo","bastante","bien","cabe","cada","casi","cierto","cierta","ciertos","como","con","conmigo","conseguimos","conseguir","consigo"
            ,"consigue","consiguen","contigo","contra","cual","cuales","cualquier","cualquiera","cuan","cuando","cuanto","cuantos","cuanta","cuantas","de","dejar","del","demas","demasiado"
            ,"demasiada","dentro","desde","dos","al","la","los","ella","en","entonces","entre","era","eras","es","eres","esta","estado","estan","estamos","estar","este","esto","estos"
            ,"estoy","fue","fui","fuimos","hace","hacen","hacemos","hacen","hacer","hacia","hago","hasta","intenta","intento","jamas","junto","la","lo","las","los","mas","mi","mis","me"
            ,"mio","misma","mismo","mucho","muchos","mucha","muchas","muy","nada","ni","ningun","no","nos","nosotros","nunca","otro","otra","para","pero","poco","poca","pocos","poder"
            ,"podria","por","porque","puede","puedo","pueden","pues","que","querer","quiero","quiere","quiza","saber","se","si","siempre","siendo","sin", "sobre","solo","somos","soy","su"
            ,"suya","suyo","tal","tan","tener","tengo","ti","tiempo","tiene","tienes","todo","toda","todos","tomar","tras","tu","tus","tuya","un","una","uno","unos","va","van","vamos"
            ,"voy","y","ya","yo","sera", "ser"]

-- Me da un ordinario diferente en un interprete de mac a uno de windows
-- puse para ambos
quitaAcento c
    | (ord c)==225 = 'a'
    | (ord c)==169 = 'e'
    | (ord c)==233 = 'e'
    | (ord c)==173 = 'i'
    | (ord c)==237 = 'i'
    | (ord c)==243 = 'o'
    | (ord c)==179 = 'o'
    | (ord c)==195 = '*' -- para que se elimine el caracter
    | (ord c)==186 = 'u'
    | (ord c)==250 = 'u'
    | otherwise = c

quitaSimbolo c
    | (ord c)==194 = '*' -- para que se elimine el caracter
    | (ord c)==161 = '*' -- para que se elimine el caracter
    | (ord c)==191 = '*' -- para que se elimine el caracter
    | otherwise = c

quitaPunto frase = [c | c <- frase, (isPunctuation c)==False]

aMinusculas frase = toLower frase

aListasPalabras frase = words frase

quitaStopWords frase = [c | c <- frase, not (c `elem` stopWords) ]

concatenarFrases frase = concat frase

crearDiccionario palabrasQuery palabrasFrases = union palabrasQuery palabrasFrases

-- Funcion para generar un vector --
getVector diccionario frase = [ countPalabraFrase frase palabra | palabra <- diccionario]

-- Funcion para contar cuantas veces aparece una palabra del diccionario en una frase --
countPalabraFrase [] palabra = 0
countPalabraFrase (x:xs) palabra = if(x == palabra)
									then
                                        1 + (countPalabraFrase xs palabra)
                                    else
                                        countPalabraFrase xs palabra

-- Formula para la similitud de cosenos --
simuitudCoseno vectorA vectorB = let
                                    divisor = sumatoriaMultiplicacion vectorA vectorB
                                    raizVA = sqrt (fromIntegral(sumatoriaCuadrados vectorA))
                                    raizVB = sqrt (fromIntegral(sumatoriaCuadrados vectorB))
                                    dividendo = raizVA * raizVB
                                  in
                                    fromIntegral divisor / dividendo

-- Funcion para la sumatoria de la multiclicacion de A*B
sumatoriaMultiplicacion [] [] = 0
sumatoriaMultiplicacion (x:xs) (y:ys) =  (x*y) + (sumatoriaMultiplicacion xs ys)

-- Funcion para la sumatoria de los cuadrados en un Vector --
sumatoriaCuadrados [] = 0
sumatoriaCuadrados (x:xs) = (x*x) + (sumatoriaCuadrados xs)

-- Para sacar el indice de un elemento --
getIndice [] elemento indice = (-1)
getIndice (x:xs) elemento indice = if (x == elemento)
									then indice
									else getIndice xs elemento (indice+1)

bot query = let
                      -- quitar acentos
                      cleanQueryAce   = map quitaAcento query
                      cleanFsAce      = [ (map quitaAcento f) | f<-frases ]

                      -- quitar simbolos
                      cleanQuerySym   = map quitaSimbolo cleanQueryAce
                      cleanFsAceSym   = [ (map quitaSimbolo f) | f<-cleanFsAce ]

                      -- quitar signos de puntuación
                      cleanPunto      = quitaPunto cleanQuerySym
                      cleanFs         = [quitaPunto f | f<-cleanFsAceSym]

                      -- cambiar mayúsculas a minúsculas
                      queryMinuscula  = map aMinusculas cleanPunto
                      minuscula       = [map aMinusculas f | f<-cleanFs]

                      -- pasar a listas de palabras
                      listasP         = aListasPalabras queryMinuscula
                      listasPalabras  = [aListasPalabras f | f<-minuscula]

                      -- quitar las stop words
                      quitaSWQuery         = quitaStopWords listasP
                      sinStopWordsFrases    = [quitaStopWords f | f<-listasPalabras]

                      -- crear el diccionario de palabras
                      concatFrases    = concatenarFrases sinStopWordsFrases --para tener lista de palabras y no lista con listas
                      diccionario     = crearDiccionario quitaSWQuery concatFrases

					            -- vectores --
                      miVectorQuery = getVector diccionario quitaSWQuery
                      misVectoresFrases = [ getVector diccionario frase | frase <- sinStopWordsFrases]

					            -- Calculamos similitud de cosenos --
                      getSimilitud = [ simuitudCoseno miVectorQuery vectorFrase  | vectorFrase <- misVectoresFrases ]

                      -- mayor de la similitudes de coseno --
                      maxSimilitud = maximum getSimilitud
                      getIndiceSimilitud = getIndice getSimilitud maxSimilitud 0

                     in
                      if(maxSimilitud == 0) then
                        "Necesito mas informacion..."
						else
							if (getIndiceSimilitud == -1) then
								"frase null"
							else
								respuestas !! getIndiceSimilitud
