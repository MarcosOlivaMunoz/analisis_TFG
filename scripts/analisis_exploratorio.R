library(tidyverse)
library(ggpubr)

#source("limpieza_datos_expl.R")
#source("limpieza_datos_bin.R")

# Gráfico 1 | Análisis de las preguntas 1, 2 y 3 del pretest
pretest1 <- pretest[1:3]
pretest1 <- pretest1 %>% 
  mutate(P1 = case_when(pretest1$P1=="R1 (correcta)" ~ "Cuan. Disc.", 
    pretest1$P1=="R2" ~ "Cual.", 
    pretest1$P1=="R3" ~ "Distractor", 
    pretest1$P1=="R4" ~ "Cuan. Cont.")) %>% 
  mutate(P2 = case_when(pretest1$P2=="R1" ~ "Cuan. Disc.", 
    pretest1$P2=="R2" ~ "Cual.", 
    pretest1$P2=="R3" ~ "Distractor", 
    pretest1$P2=="R4 (correcta)" ~ "Cuan. Cont.")) %>% 
  mutate(P3 = case_when(pretest1$P3=="R1" ~ "Cuan. Disc.", 
    pretest1$P3=="R2 (correcta)" ~ "Cual.", 
    pretest1$P3=="R3" ~ "Distractor", 
    pretest1$P3=="R4" ~ "Cuan. Cont."))
pretest1 <- pretest1 %>% 
  mutate(P1=factor(P1,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont.")),
         P2=factor(P2,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont.")),
         P3=factor(P3,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont.")))

plot1a <- ggplot(data=pretest1,aes(x=P1)) + 
  geom_bar(position = "dodge", fill=c("#33CC33","#CC3333","#CC3333","#CC3333")) + 
  guides(fill = "none") + 
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P1: Cuantitativo Discreto", y="") + 
  theme_minimal()
plot1b <- ggplot(data=pretest1,aes(x=P2, fill=P2)) + 
  geom_bar(position = "dodge", fill=c("#CC3333","#CC3333","#CC3333","#33CC33")) + 
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P2: Cuantitativo Continuo", y="") + 
  theme_minimal()
plot1c <- ggplot(data=pretest1,aes(x=P3, fill=P3)) + 
  geom_bar(position = "dodge", fill=c("#CC3333","#33CC33","#CC3333","#CC3333")) + 
  guides(fill = "none") + 
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P3: Cualitativo", y="") + 
  theme_minimal()
plot1 <- ggarrange(plot1a, plot1b, plot1c, ncol=3, align = "v") %>% 
  annotate_figure(., top = text_grob("Cuantitativo o Cualitativo (pretest, frecuencia absoluta)",
                                     color = "black", face = "bold", size = 14))

# Gráfico 2 | Avance de las respuestas correctas a lo largo del estudio
# Distinción entre Cuantitativo y Cualitativo
data1BIN <- tibble(
  Pregunta = c(rep("P1.0",2), rep("P2.0",2), rep("P3.0",2),
               rep("P3.1",2), rep("P4.1",2), rep("P5.1",2),
               rep("P2.2",2), rep("P3.2",2),
               rep("P1.3",2)) %>%
    ordered(., levels= c("P1.0","P2.0","P3.0","P3.1","P4.1","P5.1","P2.2","P3.2","P1.3")),
  Test = c(rep("Pretest",6), rep("Test 1",6), rep("Test 2",4), rep("Test 3",2)) %>% factor(.),
  Respuesta = rep(c("Incorrecto", "Correcto"),9) %>% factor(.),
  F.abs = c(table(pretestBIN$P1), table(pretestBIN$P2), table(pretestBIN$P3),
            table(test1BIN$P3), table(test1BIN$P4), table(test1BIN$P5),
            table(test2BIN$P2), table(test2BIN$P3),
            table(test3BIN$P1)),
  count = c(rep(sum(table(pretestBIN$P1)),6),
            rep(sum(table(test1BIN$P3)),6),
            rep(sum(table(test2BIN$P2)),4), 
            rep(sum(table(test3BIN$P1)),2))
)
data1BIN <- data1BIN %>% mutate(F.rel = round(100*F.abs/count,1))

plot2 <- ggplot(data=filter(data1BIN, Respuesta=="Correcto"), aes(fill=Test,x=Pregunta,y=F.rel,group=Test)) +
  geom_bar(stat="identity", width=0.85, position=position_dodge2(width = 0.5, padding = 0.25)) +
  scale_fill_manual(values=c("coral3","goldenrod3","chartreuse","chartreuse4")) + 
  geom_text(aes(label=paste0(F.rel, "%")), vjust=2, color="white") + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  scale_x_discrete(labels=c("P1","P2","P3","P3","P4","P5","P2","P3","P1")) + 
  labs(x="", y="Porcentaje de respuestas correctas") + 
  theme_minimal()

# Gráfico 3 | Avance de las respuestas correctas a lo largo del estudio
# Preguntas estadísticas
data2BIN <- tibble(
  Pregunta = c(rep("P1.1",2), rep("P2.1",2),
               rep("P1.2",2)) %>%
    ordered(., levels= c("P1.1","P2.1","P1.2")),
  Test = c(rep("Test 1",4), rep("Test 2",2)) %>% factor(.),
  Respuesta = rep(c("Incorrecto", "Correcto"),3) %>% factor(.),
  F.abs = c(table(test1BIN$P1), table(test1BIN$P2),
            table(test2BIN$P1)),
  count = c(rep(sum(table(test1BIN$P1)),4),
            rep(sum(table(test2BIN$P1)),2))
)
data2BIN <- data2BIN %>% mutate(F.rel = round(100*F.abs/count,1))

plot3 <- ggplot(data=filter(data2BIN, Respuesta=="Correcto"), aes(fill=Test,x=Pregunta,y=F.rel,group=Test)) +
  geom_bar(stat="identity", width=0.85, position=position_dodge2(width = 0.5, padding = 0.25)) +
  scale_fill_manual(values=c("coral1","chartreuse4")) + 
  geom_text(aes(label=paste0(F.rel, "%")), vjust=2, color="white") + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  scale_x_discrete(labels=c("P1","P2","P1")) + 
  labs(x="", y="Porcentaje de respuestas correctas") + 
  theme_minimal()
