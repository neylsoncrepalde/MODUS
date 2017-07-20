# Modus
# Análise de Regressão
# Prof. Jorge Alexandre
# Monitoria: Neylson Crepalde
#############################

### Aula 2

########################################################
# Setando o diretório de trabalho
setwd('~/MODUS_regressao')
# Lista os arquivos existentes no diretório
list.files()


# Carregando os pacotes necessários
library(foreign)
library(ggplot2)

# Lê o banco de dados
pnad = read.spss("PNAD2014_30a50_novo4.sav", to.data.frame = T)

#Pedindo média de x e y

summary(pnad$anosesco)  #média de y
summary(pnad$escmãe)    #média de x

# Refazendo a regressão
reg <- lm(anosesco ~ escmãe, data=pnad)
summary(reg)

plot(pnad$escmãe, pnad$anosesco, pch=19)
abline(reg=reg, lwd=2, col="red")

pnad_pad = data.frame(Y = pnad$anosesco, X = pnad$escmãe)

pnad_pad$Y = (pnad_pad$Y - mean(pnad_pad$Y)) / sd(pnad_pad$Y)
pnad_pad$X = (pnad_pad$X - mean(pnad_pad$X)) / sd(pnad_pad$X)

reg_pad = lm(Y ~ X, data = pnad_pad)
summary(reg_pad)

plot(pnad_pad$X, pnad_pad$Y)
abline(reg=reg_pad, lwd=2, col="blue")

#Média e varância das variáveis padronizadas
summary(pnad_pad$Y)
summary(pnad_pad$X)

var(pnad_pad$Y)
var(pnad_pad$X)

# Análise de variância da regressão
anova(reg)

# Rodar a regressão:
# Y = anosesco
# X = isei88pai

reg2 <- lm(anosesco ~ isei88pai, data = pnad)
summary(reg2)
