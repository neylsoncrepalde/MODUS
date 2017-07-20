### MODUS
### Aula 3
### Prof. Jorge Alexandre
##########################

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

names(pnad)
qqnorm(pnad$renda)   #Gráfico de probabilidade normal de renda
qqnorm(pnad$lnrenda) #Gráfico de probabilidade normal de log de renda
hist(pnad$lnrenda)   # Histograma do log da renda
hist(pnad$renda)     # Histograma da renda

# Rodando a regressão
reg = lm(renda ~ anosesco, data=pnad)
summary(reg)

res <- residuals(reg) # calcula os resíduos
hist(res)             # plota resíduos no histograma
qqnorm(res)           # plota qqplot dos resíduos

# Rodando a regressão log-lin
reg2 = lm(lnrenda ~ anosesco, data=pnad)
summary(reg2)

res2 = residuals(reg2)
hist(res2)
qqnorm(res2)

# Criando
pnad$logrenda <- log(pnad$renda)
hist(pnad$logrenda)
names(pnad)


coef(reg2)[1]       #Extraindo o primeiro coeficiente da reg
exp(coef(reg2)[1])  #calcula o exponencial do 1 coef

coef(reg2)[2]       #Extraindo o segundo coeficiente (da regressao)

# taxa simple = b1 * 100
coef(reg2)[2] * 100
#taxa composta = ( exp(b1)-1 ) * 100
(exp(coef(reg2)[2]) - 1) * 100


# plotando a reta de regressão
# modelo linear
plot(pnad$anosesco, pnad$renda)
abline(reg=reg, lwd=2, col="red")

# modelo log-lin
plot(pnad$anosesco, pnad$lnrenda)
abline(reg=reg2, lwd=2, col="blue")

#############################
### EXERCÍCIO!!!
### Novo modelo de regressão
### Y = log da renda
### X = ISEI do pai

modelo = lm(lnrenda ~ isei88pai, data=pnad)
summary(modelo)  
  
r = residuals(modelo)
hist(r)
qqnorm(r)

plot(y=pnad$lnrenda, x=pnad$isei88pai)
abline(reg=modelo, lwd=2, col="orange")

summary(modelo)

intercepto = coef(modelo)[1]
b1 = coef(modelo)[2]

# intercepto
exp(intercepto)

# beta1
( exp(b1) - 1 ) * 100

##########################
### Exercício 2
# Rodar modelo de regressão
# Y = lnrenda
# X = escmãe

