# Testes com modelos hierárquicos
# MODUS
# Prof. Jorge Alexandre
# Script: Neylson Crepalde
#################################

library(foreign)
library(lme4)
library(merTools)
library(lmerTest)
library(ggplot2)

setwd("~/Documentos/MODUS_regressao")
list.files()

pnad96 = read.spss("PNAD96_25a60_Modus.sav", to.data.frame = TRUE)

# Montando um modelo hierárquico com intertepto aleatório e efeito aleatório

model = lme4::lmer(lnrenda~(1|isei88)+anosesco+raçabin+(raçabin|isei88), data = pnad96, REML = FALSE)
summary(model)

#Plotando o efeito aleatório
plotREsim(REsim(model, 1000))

######################
##### Avaliando o modelo
plot(model, pch=19, col=adjustcolor("red", .2))

#Teste estatístico dos efeitos aleatórios
rand(model)

#Análise de resíduos de nível 1
hist(residuals(model), col="yellow")
qqnorm(residuals(model))

#Análise de resíduos de nível 2
hist(model@u, col="lightblue")
qqnorm(model@u)

# Calculando a correlação intraclasse
# ICC = var(erro nível2)/(var(erro nível2)+var(erro nível1))
ICC = var(model@u) / ( var(model@u) + var(residuals(model)))
print(ICC)

############################
#Plotando os efeitos
ggplot(pnad96, aes(x=anosesco, y=lnrenda))+geom_point()+
  stat_smooth(aes(col=factor(isei88)), method="lm", se=F)+
  theme(legend.position = "left")
