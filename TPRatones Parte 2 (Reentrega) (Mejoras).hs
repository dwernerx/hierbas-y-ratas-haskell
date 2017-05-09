import Test.Hspec
import Data.List
import Data.Ord
import Text.Show.Functions

data Raton = CRaton {
 edad :: Float,
 peso :: Float,
 altura :: Float,
 enfermedades :: Enfermedades
} deriving (Show, Eq)

type Hierba = Raton -> Raton
type Hierbas = [Hierba]
type Indice = Float
type Estudio = Raton -> Indice
type Diagnostico = Raton -> Bool
type Analisis = Estudio -> Diagnostico
type Enfermedad = String
type Enfermedades = [Enfermedad]
type Colonia = [Raton]
type Medicamento = Raton -> Raton
type Tratamiento = Diagnostico -> Hierbas -> Raton -> Raton
type Observacion = Colonia -> Indice

--------------------------------PARTE 1-----------------------------

-----------------------------RATONES----------------------------
mickeyMouse = CRaton {edad = 88, peso = 20, altura = 0.8, enfermedades = ["disneymania","hipotermia"]}
jerry = CRaton {edad = 76, peso = 2, altura = 0.3, enfermedades = ["tuberculosis","varicela","endemia"]}

----------------------------ESTUDIOS----------------------------
estudioDeMasaCorporal :: Estudio
estudioDeMasaCorporal raton = peso raton / ((altura raton)^2)

estudioDeAntiguedad :: Estudio
estudioDeAntiguedad = calcularAntiguedad.edad

calcularAntiguedad :: Float -> Indice
calcularAntiguedad = (/85).(+5)

----------------------------ANALISIS----------------------------

analisisDeExceso :: Indice -> Analisis
analisisDeExceso valorCritico estudio = (>valorCritico).estudio

analisisDeRangoMedio :: (Indice, Indice) -> Analisis
analisisDeRangoMedio rango estudio = not.(enRango rango).estudio
enRango rango valor = (fst rango)<valor && (snd rango)>valor

analisisBerreta :: Analisis
analisisBerreta _ = const False

------------------------------HIERBAS---------------------------

hierbaBuena :: Hierba
hierbaBuena =  cambiarEdad (/2)

hierbaMala :: Hierba
hierbaMala =  cambiarEdad (*2)

cambiarEdad :: (Float -> Float) -> Raton -> Raton
cambiarEdad f raton = raton {edad = (f.edad) raton}


hierbaZort :: Hierba
hierbaZort _ = CRaton 0 0 0 []


alcachofa :: Float -> Hierba
alcachofa porcentaje = cambiarPeso (calcularPorcentaje porcentaje)

cambiarPeso :: (Float -> Float) -> Raton -> Raton
cambiarPeso porcentaje raton = raton {peso = (porcentaje.peso) raton}

calcularPorcentaje :: Float -> Float -> Float
calcularPorcentaje porcentaje peso = peso - (peso * (porcentaje/100))


------------------------------MEDICINAS-------------------------
mezclarDosHierbas :: Hierba -> Hierba -> Hierba
mezclarDosHierbas = (.)

hacerMedicamento :: Hierbas -> Medicamento
hacerMedicamento = foldl1 mezclarDosHierbas

-----------------------------TRATAMIENTO------------------------

tratamiento :: Tratamiento
tratamiento diagnostico hierbas raton = foldl (curarRaton diagnostico) raton hierbas

curarRaton :: Diagnostico -> Raton -> Hierba -> Raton
curarRaton diagnostico raton hierba
 |diagnostico raton = hierba raton
 |otherwise = raton

-------------------------------PARTE 2-----------------------------
-- 1) a)
cantidadDeEnfermedades :: Estudio
cantidadDeEnfermedades = genericLength.enfermedades

-- b)
diagnosticarEnfermedad :: Enfermedad -> Diagnostico
diagnosticarEnfermedad enfermedad = (elem enfermedad).enfermedades

--c) -- noTieneEnfermedades
esPinky :: Diagnostico
esPinky = (== 0).cantidadDeEnfermedades
-- esPinky = null.enfermedades

-- 2) a)
hierbaVerde :: String -> Hierba
hierbaVerde terminacion = curarEnfermedad (eliminarEnfermedad terminacion)

curarEnfermedad :: (Enfermedades -> Enfermedades) -> Raton -> Raton
curarEnfermedad enfermedadesSinCurar raton = raton {enfermedades = (enfermedadesSinCurar.enfermedades) raton}

eliminarEnfermedad :: String -> Enfermedades -> Enfermedades
eliminarEnfermedad terminacion = filter (not .(esSufijo terminacion))

esSufijo :: String -> String -> Bool
esSufijo terminacion = (elem terminacion).tails

-- b)
pdpCilina :: Medicamento
pdpCilina = hacerMedicamento $ map hierbaVerde enfermedadesInfecciosas

enfermedadesInfecciosas = ["sis", "itis", "emia", "cocos"]

-- 3) Observaciones
-- a)
promedioDeEstudio :: Estudio -> Observacion
promedioDeEstudio estudio = promedio.(estudiarColonia estudio)

promedio lista = (sum lista) / (genericLength lista)

-- devuelve los indices de aplicar el estudio a los ratones
estudiarColonia :: Estudio -> Colonia -> [Indice]
estudiarColonia estudio = map estudio

-- b)
cantidadDeEnfermos :: Diagnostico -> Observacion
cantidadDeEnfermos diagnostico = genericLength . (ratonesEnPeligro diagnostico)

ratonesEnPeligro :: Diagnostico -> Colonia -> Colonia
ratonesEnPeligro diagnostico = filter diagnostico

-- c)
deLimite ::  Estudio -> Diagnostico -> Observacion
deLimite estudio diagnostico = maximum . (estudiarColonia estudio) . (ratonesEnPeligro diagnostico)


-- 4)
{-
esPeligrosa :: Enfermedad -> Colonia -> Bool
esPeligrosa enfermedad = all (diagnosticarEnfermedad enfermedad)

enfermedadesPeligrosas :: Colonia -> Enfermedades
enfermedadesPeligrosas colonia = filter (flip esPeligrosa colonia) (enfermedadesDeColonia colonia)

enfermedadesDeColonia :: Colonia -> Enfermedades
enfermedadesDeColonia = nub . concatMap enfermedades
-- nub: eliminar repetidos
-}

-- Versión Nahuel
esPeligrosa :: Enfermedad -> Colonia -> Bool
esPeligrosa enfermedad = all (diagnosticarEnfermedad enfermedad)

enfermedadesPeligrosas :: Colonia -> Enfermedades
enfermedadesPeligrosas colonia = filtrarEnfermedadesPeligrosas colonia $ enfermedadesDeColonia colonia

filtrarEnfermedadesPeligrosas colonia = filter (flip esPeligrosa colonia)

enfermedadesDeColonia :: Colonia -> Enfermedades
enfermedadesDeColonia = nub . concatMap enfermedades

-- 5)
medicinaFuncional :: Diagnostico -> Medicamento -> Colonia -> Bool
medicinaFuncional diagnostico medicina = any (not . diagnostico) . curarRatonesEnPeligro diagnostico medicina

-- medico aquellos ratones en el que el diagnostico dio positivo (en peligro)
curarRatonesEnPeligro :: Diagnostico -> Medicamento -> Colonia -> Colonia
curarRatonesEnPeligro diagnostico medicina = (medicarColonia medicina) . (ratonesEnPeligro diagnostico)

-- mapea ratones luego de aplicarle el medicamento
medicarColonia :: Medicamento -> Colonia -> Colonia
medicarColonia = map

-- 6)
{-
-- ordeno la lista de tuplas de menor a mayor, tomo el primero y devuelvo la medicina
laMejorMedicina :: [Medicamento] -> Observacion -> Colonia -> Medicamento
laMejorMedicina medicinas observacion = fst . head . tuplesort . observacionesSobreMedicinas medicinas observacion

-- devuelve una lista de tuplas (medicina,indice)
observacionesSobreMedicinas :: [Medicamento] -> Observacion -> Colonia -> [(Medicamento,Indice)]
observacionesSobreMedicinas medicinas observacion colonia = map (\medicina -> (medicina , (observarAplicacionMedicina medicina observacion colonia))) medicinas

observarAplicacionMedicina :: Medicamento -> Observacion -> Observacion
observarAplicacionMedicina medicina observacion = observacion.medicarColonia medicina

-- ordena la lista de tuplas de menor a mayor segun el segundo elemento de la misma
tuplesort xs = sortBy (comparing snd) xs
-}


-- Version Nahuel
laMejorMedicina :: [Medicamento] -> Observacion -> Colonia -> Medicamento
laMejorMedicina medicinas observacion colonia = minimumBy (comparing (\medicina -> observarAplicacionMedicina medicina observacion colonia)) medicinas

observarAplicacionMedicina :: Medicamento -> Observacion -> Observacion
observarAplicacionMedicina medicina observacion = observacion.medicarColonia medicina

----------------------CASOS DE PRUEBA---------------------------------
diagnosticoEsViejo :: Diagnostico
diagnosticoEsViejo = analisisDeExceso 1 estudioDeAntiguedad

diagnosticoSobrepeso :: Diagnostico
diagnosticoSobrepeso = analisisDeRangoMedio (18.5, 25) estudioDeMasaCorporal

hierbaSinEfecto ::  Hierba
hierbaSinEfecto = mezclarDosHierbas hierbaBuena hierbaMala

ratisalil :: Medicamento
ratisalil = hacerMedicamento [hierbaZort,hierbaMala]
-- ratisalil = mezclarDosHierbas hierbaZort hierbaMala
-- ratisalil = hierbaZort.hierbaMala

pondsAntiAge :: Medicamento
pondsAntiAge = hacerMedicamento [(alcachofa 10),hierbaBuena,hierbaBuena,hierbaBuena]

tratamientoContraAntiguedad :: Analisis -> Raton -> Raton
tratamientoContraAntiguedad analisis = tratamiento diagnosticoEsViejo [hierbaBuena, pondsAntiAge]


-- 8) a) diagnóstico para saber si un ratón posee disneymania.
poseeDisneymania :: Diagnostico
poseeDisneymania = diagnosticarEnfermedad "disneymania"

-- 9)
coloniaPDP :: Colonia
coloniaPDP = [jerry, mickeyMouse]

-- a) observación que indique el promedio de enfermedades de una colonia
promedioDeEnfermedades :: Observacion
promedioDeEnfermedades = promedioDeEstudio cantidadDeEnfermedades

-- c) cantidad de enfermos que poseen la enfermedad disneymania para la colonia de PDP es 1
cantidadEnfermosConDisneymania :: Observacion
cantidadEnfermosConDisneymania = cantidadDeEnfermos poseeDisneymania

---------------------------------------------------
--              TESTS
-------------------------------------------------------

runTests = hspec $ do
 describe "Mickey Mouse: " $ do
  it "tiene edad = 88, peso = 20, altura = 0.8 y padece de disneymania e hipotermia" $ do
    mickeyMouse `shouldBe` (CRaton 88.0 20.0 0.8 ["disneymania","hipotermia"])
  it "su indice de antigüedad es mayor a 1" $ do
    mickeyMouse `shouldSatisfy` diagnosticoEsViejo
  it "su índice de masa corporal no se encuentra entre 18.5 y 25" $ do
    mickeyMouse `shouldSatisfy` diagnosticoSobrepeso
  it "padece de disneymania" $ do   -- 8)b)
    mickeyMouse `shouldSatisfy` poseeDisneymania


 describe "Jerry: " $ do
  it "tiene edad = 76, peso = 2, altura = 0.3 y padece de tuberculosis, varicela y endemia" $ do
   jerry `shouldBe` (CRaton 76.0 2.0 0.3 ["tuberculosis","varicela","endemia"])
  it "su indice de antigüedad es menor a 1" $ do
   jerry `shouldNotSatisfy` diagnosticoEsViejo
  it "su índice de masa corporal se encuentra entre 18.5 y 25" $ do
   jerry `shouldNotSatisfy` diagnosticoSobrepeso
  it "no padece de disneymania" $ do  -- 8)b)
   jerry `shouldNotSatisfy` poseeDisneymania
  it "al darle pdpCilina, solo le queda varicela como enfermedad" $ do   --8) c)
   pdpCilina jerry `shouldBe` (CRaton 76.0 2.0 0.3 ["varicela"])


 describe "Colonia PDP: " $ do
  it "tiene un promedio de enfermedades de 2.5" $ do  -- 9)b)
   promedioDeEnfermedades coloniaPDP `shouldBe` 2.5
  it "solo un raton posee la enfermedad disneymania" $ do  -- 9)c)
   cantidadEnfermosConDisneymania coloniaPDP `shouldBe` 1
  it "la pdpCilina no funciona para tratar la enfermedad disneymania" $ do  -- 10)a)
   pdpCilina mickeyMouse `shouldBe` mickeyMouse
  it "una hierba verde de 'ania' funciona para tratar la enfermedad disneymania" $ do  -- 10)b)
   hierbaVerde "ania" mickeyMouse `shouldBe` (CRaton 88.0 20.0 0.8 ["hipotermia"])


 describe "Medicamentos: " $ do
  it "mezclar una Hierba Buena con una Hierba Mala crea una hierba que no produce efecto" $ do
   hierbaSinEfecto jerry `shouldBe` jerry
  it "Ratisalil produce los mismos resultados que una Hierba Zort" $ do
   ratisalil mickeyMouse `shouldBe` hierbaZort mickeyMouse
  it "si se aplica Ponds Anti Age a Jerry queda con edad = 9.5, peso = 1.8 y altura = 0.3" $ do
   pondsAntiAge jerry `shouldBe` (CRaton 9.5 1.8 0.3 ["tuberculosis","varicela","endemia"])


 describe "Tratamiento: " $ do
  it "aplicar tratamiento contra la antigüedad a Mickey Mouse tiene el mismo efecto que una hierba buena" $ do
   tratamientoContraAntiguedad (analisisDeExceso 1) mickeyMouse `shouldBe` hierbaBuena mickeyMouse
  it "aplicar tratamiento contra la antigüedad a Jerry tiene el mismo efecto que una alcachofa de 0" $ do
   tratamientoContraAntiguedad (analisisDeExceso 1) jerry `shouldBe` alcachofa 0 jerry
