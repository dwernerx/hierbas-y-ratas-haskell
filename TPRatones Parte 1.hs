data Raton = CRaton {
 edad :: Float,
 peso :: Float,
 altura :: Float
} deriving (Show, Eq)

-----------------------------RATONES----------------------------
mickeyMouse = CRaton {edad = 88, peso = 20, altura = 0.8}
jerry = CRaton {edad = 76, peso = 2, altura = 0.3}


----------------------------ESTUDIOS----------------------------
estudioDeMasaCorporal :: Raton -> Float
estudioDeMasaCorporal (CRaton _ peso altura) = peso / (altura^2)

estudioDeAntiguedad :: Raton -> Float
estudioDeAntiguedad (CRaton edad _ _) = (edad + 5) / 85


----------------------------ANALISIS----------------------------
analisisDeExceso :: (Ord a) => a -> (Raton -> a) -> Raton -> Bool
analisisDeExceso valorCritico estudio raton = estudio raton > valorCritico

analisisDeRangoMedio :: (Ord a) => a -> a -> (Raton -> a) -> Raton -> Bool
analisisDeRangoMedio valor1 valor2 estudio raton = (estudio raton)<valor1 || (estudio raton)>valor2
-- analisisDeRangoMedio valor1 valor2 estudio raton =  (estudio raton)<(min valor1 valor2) || (estudio raton)>(max valor1 valor2)
-- en el caso de que no se ingresen los valores de menor a mayor

analisisBerreta :: (Ord a) => (Raton -> a) -> Raton -> Bool
analisisBerreta estudio raton = False


------------------------------HIERBAS---------------------------
-- type Hierba = Raton -> Raton
hierbaBuena :: Raton -> Raton
hierbaBuena (CRaton edad peso altura) = CRaton (edad / 2) peso altura

hierbaMala :: Raton -> Raton
hierbaMala (CRaton edad peso altura) = CRaton (edad * 2) peso altura

alcachofa :: Float -> Raton -> Raton
alcachofa porcentaje (CRaton edad peso altura) = CRaton edad (perderPeso porcentaje peso) altura

perderPeso :: Float -> Float -> Float
perderPeso porcentaje peso = peso - (peso * (porcentaje/100))

hierbaZort :: Raton -> Raton
hierbaZort (CRaton _ _ _) = CRaton 0 0 0


------------------------------MEDICINAS-------------------------
-- mezclarDosHierbas :: Raton -> (Raton -> Raton) -> (Raton -> Raton) -> Raton
-- mezclarDosHierbas raton hierba1 hierba2  = hierba1 (hierba2 raton) -- Esto se puede hacer con la otra funcion recursiva

hacerMedicamento :: Raton -> [Raton -> Raton] -> Raton
hacerMedicamento raton [] = raton
hacerMedicamento raton [hierba] = hierba raton
hacerMedicamento raton (x:xs) = hacerMedicamento (x raton) xs


ratisalil :: Raton -> Raton
ratisalil raton = hacerMedicamento raton [hierbaZort,hierbaMala]
-- ratisalil raton = mezclarDosHierbas raton hierbaZort hierbaMala
-- ratisalil = hierbaZort.hierbaMala

pondsAntiAge :: Raton -> Raton
pondsAntiAge raton = hacerMedicamento raton [(alcachofa 10),hierbaBuena,hierbaBuena,hierbaBuena]


-----------------------------TRATAMIENTO------------------------
tratamiento :: Raton -> (Raton -> Bool) -> [Raton -> Raton] -> Raton
tratamiento raton analisis [] = raton
tratamiento raton analisis [hierba]
 |(analisis raton) = hierba raton
 |otherwise = raton

tratamiento raton analisis (x:xs)
 |(analisis raton) = tratamiento (x raton) analisis xs
 |otherwise = raton


tratamientoContraAntiguedad raton analisis hierbas = tratamiento raton (analisis estudioDeAntiguedad) hierbas
