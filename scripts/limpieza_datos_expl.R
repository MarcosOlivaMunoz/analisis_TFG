library(tidyverse)

# Cargamos los datos y eliminamos la primera columna (Marca Temporal)
pretest <- read_csv("data/Pretest.csv")
pretest <- pretest[-1]
test1 <- read_csv("data/Test1.csv")
test1 <- test1[-1]
test2 <- read_csv("data/Test2.csv")
test2 <- test2[-1]
test3 <- read_csv("data/Test3.csv")
test3 <- test3[-1]

# Cambiamos los nombres de las columnas
colnames(pretest) = c("P1", "P2", "P3", "P4", "P5", "P6", "P7")
colnames(test1) = c("P1", "P2", "P3", "P4", "P5")
colnames(test2) = c("Participación", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")
colnames(test3) = c("Participación", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")

# Aplicamos el formato necesario para analizar
pretest <- pretest %>% mutate(P1 = case_when(pretest$P1=="\"¿Cuántos hermanos tienes?\"" ~ "R1 (correcta)", 
    pretest$P1=="\"¿Tienes hermanos?\"" ~ "R2", 
    pretest$P1=="\"¿Cuál de las siguientes opciones corresponde al número de hermanos que tienes? Entre 0 y 1 hermanos, Entre 2 y 3 hermanos, 4 hermanos o más\"" ~ "R3", 
    pretest$P1=="\"¿Cuál es tu altura (en centímetros)?\"" ~ "R4")) %>% 
  mutate(P2 = case_when(pretest$P2=="\"¿Cuántos hermanos tienes?\"" ~ "R1", 
    pretest$P2=="\"¿Tienes hermanos?\"" ~ "R2", 
    pretest$P2=="\"¿Cuál de las siguientes opciones corresponde al número de hermanos que tienes? Entre 0 y 1 hermanos, Entre 2 y 3 hermanos, 4 hermanos o más\"" ~ "R3", 
    pretest$P2=="\"¿Cuál es tu altura (en centímetros)?\"" ~ "R4 (correcta)")) %>% 
  mutate(P3 = case_when(pretest$P3=="\"¿Cuántos hermanos tienes?\"" ~ "R1", 
    pretest$P3=="\"¿Tienes hermanos?\"" ~ "R2 (correcta)", 
    pretest$P3=="\"¿Cuál de las siguientes opciones corresponde al número de hermanos que tienes? Entre 0 y 1 hermanos, Entre 2 y 3 hermanos, 4 hermanos o más\"" ~ "R3", 
    pretest$P3=="\"¿Cuál es tu altura (en centímetros)?\"" ~ "R4")) %>% 
  mutate(P4 = case_when(pretest$P4=="La nota media de cada centro participante en el estudio" ~ "R1", 
    pretest$P4=="La nota media de los centros públicos y privados participantes en el estudio" ~ "R2", 
    pretest$P4=="La nota media de matemáticas de cada estudiante de cada centro participante en el estudio" ~ "R3 (correcta)", 
    pretest$P4=="La nota media de matemáticas de cada centro participante en el estudio" ~ "R4")) %>% 
  mutate(P5 = case_when(pretest$P5==11 ~ "R1", 
    pretest$P5==12 ~ "R2 (correcta)", 
    pretest$P5==15 ~ "R3", 
    pretest$P5==18 ~ "R4")) %>% 
  mutate(P6 = case_when(pretest$P6=="Comparar el número de alcohólicos en Palma y en Barcelona" ~ "R1", 
    pretest$P6=="Comparar el porcentaje de alcohólicos en Palma y en Barcelona" ~ "R2 (correcta)", 
    pretest$P6=="Comparar el número medio de alcohólicos en Palma y en Barcelona" ~ "R3", 
    pretest$P6=="Ninguna de las respuestas es correcta" ~ "R4")) %>% 
  mutate(P7 = case_when(pretest$P7=="Un gráfico circular" ~ "R1", 
    pretest$P7=="Un diagrama de barras" ~ "R2 (correcta)", 
    pretest$P7=="Una nube de puntos" ~ "R3", 
    pretest$P7=="Un gráfico de línea" ~ "R4"))

test1 <- test1 %>% mutate(P1 = case_when(test1$P1=="Sí" ~ "R1", 
    test1$P1=="No" ~ "R2 (correcta)")) %>% 
  mutate(P2 = case_when(test1$P2=="Sí" ~ "R1 (correcta)", 
    test1$P2=="No" ~ "R2")) %>% 
  mutate(P3 = case_when(test1$P3=="Cualitativa" ~ "R1",
    test1$P3=="Cuantitativa discreta" ~ "R2",
    test1$P3=="Cuantitativa contínua" ~ "R3 (correcta)")) %>% 
  mutate(P4 = case_when(test1$P4=="Cualitativa" ~ "R1 (correcta)",
    test1$P4=="Cuantitativa discreta" ~ "R2",
    test1$P4=="Cuantitativa contínua" ~ "R3")) %>% 
  mutate(P5 = case_when(test1$P5=="Cualitativa" ~ "R1",
    test1$P5=="Cuantitativa discreta" ~ "R2 (correcta)",
    test1$P5=="Cuantitativa contínua" ~ "R3"))

test2 <- test2 %>% mutate(P1 = case_when(test2$P1=="Sí" ~ "R1 (correcta)", 
    test2$P1=="No" ~ "R2")) %>%
  mutate(P2 = case_when(test2$P2=="Cualitativa" ~ "R1",
    test2$P2=="Cuantitativa discreta" ~ "R2",
    test2$P2=="Cuantitativa contínua" ~ "R3 (correcta)")) %>%
  mutate(P3 = case_when(test2$P3=="Cualitativa" ~ "R1",
    test2$P3=="Cuantitativa discreta" ~ "R2 (correcta)",
    test2$P3=="Cuantitativa contínua" ~ "R3")) %>%
  mutate(P4 = case_when(test2$P4=="Los mayores de 18 años en las Islas Baleares durante la temporada alta de turismo" ~ "R1",
    test2$P4=="Los residentes de las Islas Baleares responsables de hacer la compra" ~ "R2 (correcta)",
    test2$P4=="Los residentes de Mallorca que trabajan" ~ "R3",
    test2$P4=="Los residentes de Mallorca mayores de 18 años" ~ "R4")) %>%
  mutate(P5 = case_when(test2$P5=="Seleccionar gente al azar que camine por el Paseo Marítimo" ~ "R1",
    test2$P5=="Seleccionar a personas que están comprando en Mercadona" ~ "R2",
    test2$P5=="Seleccionar gente al azar en cada municipio de las Islas Baleares" ~ "R3",
    test2$P5=="Seleccionar gente proporcional a cada municipio de las Islas Baleares" ~ "R4 (correcta)")) %>%
  mutate(P6 = case_when(test2$P6=="Fórmula 1" ~ "R1 (correcta)",
    test2$P6=="Fórmula 2" ~ "R2",
    test2$P6=="Fórmula 3" ~ "R3",
    test2$P6=="No hay fórmula, se busca el valor en la tabla" ~ "R4")) %>%
  mutate(P7 = case_when(test2$P7=="Respuesta abierta" ~ "R1",
    test2$P7=="Escala lineal" ~ "R2 (correcta)",
    test2$P7=="Opción múltiple" ~ "R3",
    test2$P7=="Opción única" ~ "R4")) %>%
  mutate(P8 = case_when(test2$P8=="Sí, la pregunta es correcta" ~ "R1",
    test2$P8=="No, porque no ofrece suficientes opciones" ~ "R2",
    test2$P8=="No, porque está abierta a interpretación" ~ "R3 (correcta)",
    test2$P8=="Sí, siempre y cuando se especifique que los números indican la cantidad de veces que asistes a la semana" ~ "R4")) %>%
  mutate(P9 = case_when(test2$P9=="Sí, la pregunta es correcta" ~ "R1",
    test2$P9=="No, porque el vocabulario es inadecuado" ~ "R2",
    test2$P9=="No, porque es demasiado ambigua" ~ "R3",
    test2$P9=="Sí, siempre y cuando se reformule para que no esté cargada" ~ "R4 (correcta)"))

test3 <- test3 %>% mutate(P1 = case_when(test3$P1=="Cualitativa" ~ "R1",
    test3$P1=="Cuantitativa discreta" ~ "R2",
    test3$P1=="Cuantitativa contínua" ~ "R3 (correcta)")) %>%
  mutate(P2 = case_when(test3$P2=="Sus compañeros de clase" ~ "R1",
    test3$P2=="Los alumnos de ESO de todos los institutos de la ciudad" ~ "R2 (correcta)",
    test3$P2=="Las familias con al menos un hijo que cursen actualmente secundaria" ~ "R3",
    test3$P2=="Todos los adolescentes entre 12 y 18 años" ~ "R4")) %>%
  mutate(P3 = case_when(test3$P3=="Sí, la pregunta es correcta" ~ "R1 (correcta)",
    test3$P3=="No, porque está abierta a interpretación" ~ "R2",
    test3$P3=="No, porque debería dar unas opciones concretas" ~ "R3",
    test3$P3=="Sí, siempre y cuando se reformule para que no esté cargada" ~ "R4")) %>%
  mutate(P4 = case_when(test3$P4=="El primer cuartil" ~ "R1",
    test3$P4=="La media" ~ "R2",
    test3$P4=="La mediana" ~ "R3 (correcta)",
    test3$P4=="El tercer cuartil" ~ "R4")) %>%
  mutate(P5 = case_when(test3$P5=="El gráfico A" ~ "R1",
    test3$P5=="El gráfico B" ~ "R2 (correcta)",
    test3$P5=="Ambos gráficos son adecuados" ~ "R3",
    test3$P5=="Ningún gráfico es adecuado; sería mejor hacer otro tipo" ~ "R4")) %>%
  mutate(P6 = case_when(test3$P6=="Existen un par de valores atípicos y deberíamos eliminarlos sin dudar." ~ "R1",
    test3$P6=="La disparidad entre la media y la mediana indica que los datos se distribuyen de manera simétrica" ~ "R2",
    test3$P6=="El 50% central de los empleados cobran entre 1500€ y 2000€" ~ "R3 (correcta)",
    test3$P6=="El 25% superior de los empleados cobran más de 1500€" ~ "R4")) %>%
  mutate(P7 = case_when(test3$P7=="En frecuencia relativa, los hombres tiene más demencia que las mujeres" ~ "R1 (correcta)",
    test3$P7=="En frecuencia absoluta, las mujeres tienen menos alzhéimer que los hombres" ~ "R2",
    test3$P7=="En frecuencia absoluta, no se puede apreciar una diferencia significativa de Alzheimer por sexo" ~ "R3",
    test3$P7=="En frecuencia relativa, los hombres tienen menos alzhéimer que las mujeres" ~ "R4")) %>%
  mutate(P8 = case_when(test3$P8=="Ambas variables pueden estar relacionadas linealmente" ~ "R1",
    test3$P8=="No hay relación entre ambas variables" ~ "R2",
    test3$P8=="Existe un valor influyente que cambia la regresión" ~ "R3 (correcta)",
    test3$P8=="No tiene sentido que hayan valores negativos en el gráfico" ~ "R4")) %>%
  mutate(P9 = case_when(test3$P9=="Por los valores atípicos, ambas clases tienen la misma dispersión" ~ "R1",
    test3$P9=="Por la diferencia de las medianas, 4ºA presenta mayor dispersión que 4ºB" ~ "R2",
    test3$P9=="Por los bigotes, el 4ºB tiene mayor dispersión que el 4ºA" ~ "R3 (correcta)",
    test3$P9=="Por límites de las cajas, el 4ºB tiene mayor dispersión que el 4ºA" ~ "R4"))