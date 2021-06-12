module Lib where
import Data.List
import Text.Show.Functions

data Ladron = UnLadron {
    profesion :: String,
    nombreLadron :: String,
    habilidadLadron :: [Habilidad],
    armas :: [Arma]
} deriving (Show)

type Habilidad = String

data Rehen = UnRehen {
    nombreRehen :: String,
    nivelComplot :: Int,
    nivelMiedo :: Int,
    plan :: [Plan]
} deriving (Show)

data Arma = UnArma {
    nombreArma :: String,
    calibre :: Int,
    cantidadDeBalas :: Int,
    efectoArma :: Arma -> Rehen -> Rehen
} deriving (Show)

type Plan = Ladron -> Ladron

efectoAmetralladora :: Arma -> Rehen -> Rehen
efectoAmetralladora arma rehen = rehen {nivelComplot = div (nivelComplot rehen) 2, nivelMiedo = nivelMiedo rehen + cantidadDeBalas arma}

efectoPistola :: Arma -> Rehen -> Rehen
efectoPistola arma rehen = rehen {nivelComplot = (nivelComplot rehen) - (5* calibre arma),nivelMiedo = nivelMiedo rehen + (3* length (nombreArma arma))}

atacarAlLadron :: Rehen -> Ladron -> Ladron
atacarAlLadron compañero ladron = cantidadDeArmasASacar ladron nombreRehen 10 compañero

esconderse :: Ladron -> Ladron
esconderse ladron = cantidadDeArmasASacar ladron habilidadLadron 3 ladron

cantidadDeArmasASacar ladron funcion cantidad persona= ladron {armas= drop (div (length (funcion persona)) cantidad ) (armas ladron)}

{-1) Modelar a los siguientes personajes-}
tokio = UnLadron "" "Tokio" ["Trabajo psicologico","Entrar en moto","Bokita"] [UnArma "Pistola" 9 8 efectoPistola,UnArma "Pistola" 9 8 efectoPistola, UnArma "Ametralladora" 1 30 efectoAmetralladora]
profesor = UnLadron "" "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso","estar siempre un paso adelante"] []
pablo = UnRehen "Pablo12312" 40 30 [esconderse]
arturito = UnRehen "Arturo" 70 50 [esconderse, atacarAlLadron pablo]

{-2 Saber si un ladrón es inteligente. Ocurre cuando tiene más de dos habilidades, 
además el Profesor es la mente maestra, por lo que indudablemente es inteligente. -}

unLadronEsInteligente ladron = nombreLadron ladron == "Profesor" || length (habilidadLadron ladron) > 2

{-3) Que un ladrón consiga un arma nueva, y se la agregue a las que ya tiene.-}
pistola = UnArma "Pistola" 9 8 efectoPistola
consigueArmaNueva arma ladron  = ladron {armas = arma : armas ladron}

{-4) Que un ladrón intimide a un rehén, usando alguno de los métodos planeados.-}

hacerseElMalo ladron rehen = consecuenciaDeHacerseElMalo ladron rehen

consecuenciaDeHacerseElMalo ladron rehen | nombreLadron ladron == "Berlin" = rehen {nivelMiedo = nivelMiedo rehen + sumarCantidadLetrasHabilidadLadron ladron}
                                         | nombreLadron ladron == "Rio" = rehen {nivelComplot = nivelComplot rehen + 20}
                                         | otherwise = rehen {nivelMiedo = nivelMiedo rehen +10}
sumarCantidadLetrasHabilidadLadron ladron = length.concat.habilidadLadron $ ladron

--disparos ladron rehen =   (sort (map (length . nombreArma) (armas ladron)))
--compararArmas arma | nombreArma arma 

intimidar metodo ladron rehen = metodo ladron rehen

{-5 Que un ladrón calme las aguas, disparando al techo frente a un grupo de rehenes, de los cuales se calman los que tengan más de 60 de complot.-}
{-
calmarLasAguas ladron rehen = (disparos ladron) verificarComplot rehen

verificarComplot rehen = map ((60>). nivelComplot) rehen
-}

{-6) Saber si un ladrón puede escaparse de la policía. Esto se cumple cuando alguna de las habilidades del ladrón empieza con “disfrazarse de”. -}

puedeEscaparse ladron habilidad = verificarHabilidad ladron habilidad
verificarHabilidad ladron habilidad = any ((==habilidad) . take (length habilidad)) (habilidadLadron ladron)


{-7) Saber si la cosa pinta mal, que es cuando dados unos ladrones y unos rehenes,
 el nivel de complot promedio de los rehenes es mayor al nivel de miedo promedio multiplicado por la cantidad de armas de los ladrones.-}

laCosaPintaMal ladrones rehenes = (promedioNivelComplot rehenes) > ((promedioNivelMiedo rehenes) * (cantidadDeArmas ladrones))

promedioNivelComplot rehenes = promedio (map (nivelComplot) rehenes)
promedioNivelMiedo rehenes = promedio (map (nivelMiedo) rehenes)
cantidadDeArmas ladrones =  sum (map (length.armas) ladrones)

promedio xs = div (sum xs) (length xs)
{-
{-8) Que los rehenes se rebelen contra un ladrón, usando el plan que tengan en mente. Saben que es mala idea,
por lo que todos pierden 10 de complot antes de comenzar la rebelión. -}

seRebelan rehen ladron = rehen {nivelComplot = nivelComplot rehen -10, plan = (plan rehen) ladron}

probado rehen = plan rehen

{-9) Ejecutar el Plan Valencia, que consiste en escapar con la mayor cantidad de dinero posible.
 El dinero conseguido, es igual a $1000000, multiplicado por la cantidad de armas que tengan todos los ladrones en total, luego de que:
se armen todos con una ametralladora de 45 balas
todos los rehenes se rebelen contra todos los ladrones -}
planValencia ladrones rehenes = map (consigueArmaNueva ametralladora) ladrones
ametralladora = UnArma "Ametralladora" 1 30 efectoAmetralladora

-}

{-10) ¿Se puede ejecutar el plan valencia si uno de los ladrones tiene una cantidad infinita de armas? Justifique.
No se puede ejecutar, ya que al final de la funcion se debe multiplicar 1000000 por la cantidad de armas y si esta es infinita, nunca se llega a ejecutar
-}
{-11) ¿Se puede ejecutar el plan valencia si uno de los ladrones tiene una cantidad infinita de habilidades? Justifique.
No
-}