library(tidyverse)
library(ggpubr)

source("scripts/limpieza_datos_expl.R")
source("scripts/limpieza_datos_bin.R")
source("scripts/datos_expl.R")

# Gráfico 1 | Análisis de las preguntas 1, 2 y 3 del pretest
pretestCount <- testCount %>% filter(Test=="Pretest") %>% 
  mutate(Respuesta = c("Cuan. Disc.", "Cual.", "Distractor", "Cuan. Cont.",
                       "Cuan. Disc.", "Cual.", "Distractor", "Cuan. Cont.",
                       "Cuan. Disc.", "Cual.", "Distractor", "Cuan. Cont.",
                       "Distractor 1", "Distractor 2", "Resp. Correcta", "Distractor 3",
                       "Moda", "Mediana", "Media", "Distractor",
                       "Freq. Abs.", "Freq. Rel.", "Distractor 1", "Distractor 2",
                       "Circular", "Barras", "Puntos", "Línea"))

plot1a <- ggplot(data=filter(pretestCount, Pregunta=="P1.0") %>% 
                   mutate(Respuesta=factor(Respuesta,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont."))),
                 aes(x=Respuesta,y=F.abs)) +
  geom_bar(stat="identity", position = "dodge", fill=c("chartreuse4","coral3","coral3","coral3")) + 
  guides(fill = "none") +
  geom_text(aes(label=paste0(F.rel, "%")), vjust=c(2,2,2,-1), color=c("white","white","white","black"), size=2.75) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P1: Cuantitativo Discreto", y="Frecuencia absoluta") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot1b <- ggplot(data=filter(pretestCount, Pregunta=="P2.0") %>% 
                   mutate(Respuesta=factor(Respuesta,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont."))),
                 aes(x=Respuesta,y=F.abs)) +
  geom_bar(stat="identity", position = "dodge", fill=c("coral3","coral3","coral3","chartreuse4")) + 
  guides(fill = "none") +
  geom_text(aes(label=paste0(F.rel, "%")), vjust=2, color="white", size=2.75) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P2: Cuantitativo Continuo", y="") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot1c <- ggplot(data=filter(pretestCount, Pregunta=="P3.0") %>% 
                   mutate(Respuesta=factor(Respuesta,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont."))),
                 aes(x=Respuesta,y=F.abs)) +
  geom_bar(stat="identity", position = "dodge", fill=c("coral3","coral3","chartreuse4","coral3")) + 
  guides(fill = "none") +
  geom_text(aes(label=paste0(F.rel, "%")), vjust=2, color="white", size=2.75) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P3: Cualitativo", y="") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot1 <- ggarrange(plot1a, plot1b, plot1c, ncol=3, align = "v")

# Gráfico 2 | Avance de las respuestas correctas a lo largo del estudio
# Distinción entre Cuantitativo y Cualitativo

# P1.0, P2.0, P3.0, P3.1, P4.1, P5.1, P2.2, P3.2, P1.3
arquetipo1CountBIN <- testCountBIN[c(1:6,19:24,27:30,43:44),]

plot3 <- ggplot(data=filter(arquetipo1CountBIN, Respuesta=="Correcto"), aes(fill=Test,x=Pregunta,y=F.rel,group=Test)) +
  geom_bar(stat="identity", width=0.85, position=position_dodge2(width = 0.5, padding = 0.25)) +
  scale_fill_manual(values=c("coral3","goldenrod3","chartreuse","chartreuse4")) + 
  geom_text(aes(label=paste0(F.rel, "%")), vjust=2, color="white") + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  scale_x_discrete(labels=c("P1","P2","P3","P3","P4","P5","P2","P3","P1")) + 
  labs(x="", y="Porcentaje de respuestas correctas") + 
  theme_minimal()

# Gráfico 3 | Avance de las respuestas correctas a lo largo del estudio
# Preguntas estadísticas

# P1.1, P2.1, P1.2
arquetipo2CountBIN <- testCountBIN[c(15:18,25:26),]

plot4 <- ggplot(data=filter(arquetipo2CountBIN, Respuesta=="Correcto"), aes(fill=Test,x=Pregunta,y=F.rel,group=Test)) +
  geom_bar(stat="identity", width=0.85, position=position_dodge2(width = 0.5, padding = 0.25)) +
  scale_fill_manual(values=c("goldenrod3","chartreuse4")) + 
  geom_text(aes(label=paste0(F.rel, "%")), vjust=2, color="white") + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  scale_x_discrete(labels=c("P1","P2","P1")) + 
  labs(x="", y="Porcentaje de respuestas correctas") + 
  theme_minimal()
