library(tidyverse)
library(ggpubr)

#source("scripts/limpieza_datos_expl.R")
#source("scripts/limpieza_datos_bin.R")

# DF previo: todas las preguntas, categorizado por todas las respuestas,
# con frecuncias absolutas y relativas
testCount <- tibble(
  Pregunta = c(rep("P1.0",4), rep("P2.0",4), rep("P3.0",4), rep("P4.0",4),
               rep("P5.0",4), rep("P6.0",4), rep("P7.0",4), rep("P1.1",2),
               rep("P2.1",2), rep("P3.1",3), rep("P4.1",3), rep("P5.1",3),
               rep("P1.2",2), rep("P2.2",3), rep("P3.2",3), rep("P4.2",4),
               rep("P5.2",4), rep("P6.2",4), rep("P7.2",4), rep("P8.2",4),
               rep("P9.2",4), rep("P1.3",3), rep("P2.3",4), rep("P3.3",4),
               rep("P4.3",4), rep("P5.3",4), rep("P6.3",4), rep("P7.3",4),
               rep("P8.3",4), rep("P9.3",4)) %>%
    ordered(., levels= c("P1.0","P2.0","P3.0","P4.0","P5.0","P6.0","P7.0",
                         "P1.1","P2.1","P3.1","P4.1","P5.1", "P1.2","P2.2",
                         "P3.2","P4.2","P5.2","P6.2","P7.2","P8.2","P9.2",
                         "P1.3","P2.3","P3.3","P4.3","P5.3","P6.3","P7.3",
                         "P8.3","P9.3")),
  Test = c(rep("Pretest",28), rep("Test 1",13), rep("Test 2",32),
           rep("Test 3",35)) %>% factor(.),
  Respuesta = c(rep(c("R1","R2","R3","R4"),7), rep(c("R1","R2"),2),
                rep(c("R1","R2","R3"),3), "R1", "R2", rep(c("R1","R2","R3"),2),
                rep(c("R1","R2","R3","R4"),6), "R1", "R2", "R3",
                rep(c("R1","R2","R3","R4"),8)) %>% factor(.),
  F.abs = c(table(pretest$P1), table(pretest$P2), table(pretest$P3),
            table(pretest$P4), table(pretest$P5), table(pretest$P6),
            table(pretest$P7), table(test1$P1), table(test1$P2),
            table(test1$P3), table(test1$P4), table(test1$P5),
            table(test2$P1), table(test2$P2), table(test2$P3),
            table(test2$P4), 0, table(test2$P5), table(test2$P6),
            table(test2$P7), table(test2$P8), table(test2$P9),
            table(test3$P1), table(test3$P2), table(test3$P3),
            table(test3$P4), table(test3$P5), table(test3$P6),
            table(test3$P7), table(test3$P8), table(test3$P9)),
  count = c(rep(sum(table(pretest$P1)),28),
            rep(sum(table(test1$P3)),13),
            rep(sum(table(test2$P2)),32), 
            rep(sum(table(test3$P1)),35))
)
testCount <- testCount %>% mutate(F.rel = round(100*F.abs/count,1))

# DF previo: todas las preguntas, categorizado por respuesta correcta o incorrecta,
# con frecuncias absolutas y relativas
testCountBIN <- tibble(
  Pregunta = c(rep("P1.0",2), rep("P2.0",2), rep("P3.0",2), rep("P4.0",2),
               rep("P5.0",2), rep("P6.0",2), rep("P7.0",2), rep("P1.1",2),
               rep("P2.1",2), rep("P3.1",2), rep("P4.1",2), rep("P5.1",2),
               rep("P1.2",2), rep("P2.2",2), rep("P3.2",2), rep("P4.2",2),
               rep("P5.2",2), rep("P6.2",2), rep("P7.2",2), rep("P8.2",2),
               rep("P9.2",2), rep("P1.3",2), rep("P2.3",2), rep("P3.3",2),
               rep("P4.3",2), rep("P5.3",2), rep("P6.3",2), rep("P7.3",2),
               rep("P8.3",2), rep("P9.3",2)) %>%
    ordered(., levels= c("P1.0","P2.0","P3.0","P4.0","P5.0","P6.0","P7.0",
                         "P1.1","P2.1","P3.1","P4.1","P5.1", "P1.2","P2.2",
                         "P3.2","P4.2","P5.2","P6.2","P7.2","P8.2","P9.2",
                         "P1.3","P2.3","P3.3","P4.3","P5.3","P6.3","P7.3",
                         "P8.3","P9.3")),
  Test = c(rep("Pretest",14), rep("Test 1",10), rep("Test 2",18),
           rep("Test 3",18)) %>% factor(.),
  Respuesta = rep(c("Incorrecto","Correcto"),30) %>% factor(.),
  F.abs = c(table(pretestBIN$P1), table(pretestBIN$P2), table(pretestBIN$P3),
            table(pretestBIN$P4), table(pretestBIN$P5), table(pretestBIN$P6),
            table(pretestBIN$P7), table(test1BIN$P1), table(test1BIN$P2),
            table(test1BIN$P3), table(test1BIN$P4), table(test1BIN$P5),
            table(test2BIN$P1), table(test2BIN$P2), table(test2BIN$P3),
            table(test2BIN$P4), table(test2BIN$P5), table(test2BIN$P6),
            table(test2BIN$P7), table(test2BIN$P8), table(test2BIN$P9),
            table(test3BIN$P1), table(test3BIN$P2), table(test3BIN$P3),
            table(test3BIN$P4), table(test3BIN$P5), table(test3BIN$P6),
            table(test3BIN$P7), table(test3BIN$P8), table(test3BIN$P9)),
  count = c(rep(sum(table(pretestBIN$P1)),14),
            rep(sum(table(test1BIN$P3)),10),
            rep(sum(table(test2BIN$P2)),18), 
            rep(sum(table(test3BIN$P1)),18))
)
testCountBIN <- testCountBIN %>% mutate(F.rel = round(100*F.abs/count,1))