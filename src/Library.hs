module Library where
import PdePreludat

type RecursoNatural = String

-- Punto 1
data Pais = Pais {
    nombre :: String,
    ingresoPerCapita :: Number,
    empleadosPublicos :: Number,
    empleadosPrivados :: Number,
    recursosNaturales :: [RecursoNatural],
    millonesDeDeuda :: Number
} deriving Show

namibia = Pais {
    nombre = "Namibia",
    ingresoPerCapita = 4140,
    empleadosPublicos = 400000,
    empleadosPrivados = 650000,
    recursosNaturales = ["Minería", "Ecoturismo"],
    millonesDeDeuda = 50
}


-- Punto 2
type Receta = [Estrategia]
type Estrategia = Pais -> Pais

hacerPrestamo :: Number -> Estrategia
hacerPrestamo prestamo pais = pais {
    millonesDeDeuda = millonesDeDeuda pais + (1.5 * prestamo)
}

reducirSectorPublico :: Number -> Estrategia
reducirSectorPublico cantidadEmpleos pais = pais {
    empleadosPublicos = (empleadosPublicos pais) - cantidadEmpleos,
    ingresoPerCapita = ingresoPerCapita pais - calcularIngresoADisminuir cantidadEmpleos pais
}

calcularIngresoADisminuir :: Number -> Pais -> Number
calcularIngresoADisminuir cantidadEmpleos pais | cantidadEmpleos > 100 = (ingresoPerCapita pais) * 0.8
                                               | otherwise =  (ingresoPerCapita pais) * 0.85

cederEmpresa :: RecursoNatural -> Estrategia
cederEmpresa recurso pais = pais {
    recursosNaturales = filter (/= recurso) (recursosNaturales pais)
}

blindaje :: Estrategia
blindaje pais = hacerPrestamo ((calcularPBI pais) / 2) (reducirSectorPublico 500 pais)

calcularPBI :: Pais -> Number
calcularPBI pais = ingresoPerCapita pais * (empleadosPublicos pais + empleadosPrivados pais)

-- Punto 3

prestar200yCederMineria :: Receta
prestar200yCederMineria = [hacerPrestamo 200, cederEmpresa "Minería"]

aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta = foldr ($) 

namibia' = aplicarReceta namibia prestar200yCederMineria

-- El efecto colateral no existe en haskell, por eso se dice que se genera un nuevo mundo
-- cada vez que se le hace una modificacion al mismo. El paradigma funcional, al ser atemporal
-- permite que la entrada de una funcion, sea el resultante de la modificacion de un mundo'
-- y generando un mundo'' como salida, con las modificaciones del mundo' y las modificaciones generadas por la funcion.

-- Punto 4

-- Orden Superior en el filter con toda la funcion que le sigue
-- Aplicacion parcial en lo que deberia ser el segundo parametro del filter
-- donde deberia ser pasada la lista de paises pero se uso el point free en conjunto con la aplicacion parcial
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter (\pais -> elem "Petróleo" (recursosNaturales pais))

-- Composicion en el sum.map
totalDeudaAFavor :: [Pais] -> Number
totalDeudaAFavor = sum.map millonesDeDeuda

-- Punto 5

recetasOrdenadas :: Pais -> [Receta] -> Bool
recetasOrdenadas _ [unaReceta] = True
recetasOrdenadas pais (receta1:receta2:recetas) = 
    calcularPBI (aplicarReceta pais receta1) < calcularPBI (aplicarReceta pais receta2) && 
    recetasOrdenadas pais (receta2:recetas)

-- Punto 6

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

-- En el punto 4a, no podrian ser procesadas por el Show ya que no puede mostrar
-- infinitos elementos, pues no puede converger en un valor debido a que la lista es infinita. 
-- En palabras simples no "sabe" cuantos elementos filtrar

-- En cambio, en el 4b, gracias a la evaluacion perezosa de haskell y el hecho de que la infinidad
-- esta dada en los recursos naturales, la funcion podria operar sin problemas ya que no tiene en cuenta
-- esta lista para operar.
