####################################
############# MODUS ################
############ Aula 7 ################
##### Prof. Jorge Alexandre ########
### Monitoria: Neylson Crepalde ####
####################################

############################################
# Setando o diretório de trabalho
setwd('~/MODUS_regressao')
# Lista os arquivos existentes no diretório
list.files()


# Carregando os pacotes necessários
library(foreign)
library(ggplot2)
library(car)

# Lê o banco de dados
pnad96 = read.spss("PNAD96_25a60_Modus.sav", to.data.frame = T)

# Estimando de novo o modelo:
# Y = anosesco
# X = iseipai, raçabin

reg = lm(anosesco~iseipai+raçabin, data=pnad96)
summary(reg)

#Plotando as retas de regressão
ggplot(pnad96, aes(x=iseipai, y=anosesco))+geom_point()+
  stat_smooth(aes(col=raçabin), method="lm", se=F)

######################################################
# Modelos com variáveis categóricas com
# mais de uma categoria

#Y = anosesco
#X = iseipai e região

# Conferir a classe da variável
class(pnad96$região)
# Se a variável fosse do tipo character (texto), seria corrigido
# assim:
# pnad96$região = as.factor(pnad96$região)

# definir a categoria de referência

pnad96$região <- relevel(pnad96$região, ref="Nordeste")

# Montando o modelo de regressão

modelo = lm(anosesco~iseipai+região, data=pnad96)
summary(modelo)
coef(modelo)

# Plotando a reta de regressão com interceptos diferentes para
# as diferentes regiões
intercepto = coef(modelo)[1]
slope = coef(modelo)[2]
plot(pnad96$iseipai, pnad96$anosesco)
abline(a=intercepto, b=slope, lwd=2, col="red")
abline(a=intercepto+coef(modelo)[3], b=slope, lwd=1, col="blue")
abline(a=intercepto+coef(modelo)[4], b=slope, lwd=1, col="green")
abline(a=intercepto+coef(modelo)[5], b=slope, lwd=1, col="black")
abline(a=intercepto+coef(modelo)[6], b=slope, lwd=1, col="yellow")



# Plotando a curva de regressão
ggplot(pnad96, aes(x=iseipai, y=anosesco))+geom_point()+
  stat_smooth(aes(col=região), method="lm", se=FALSE)





