############## MODUS ############
###### Análise de Regressão #####
## Prof. Jorge Alexandre Neves ##
## Monitoria: Neylson Crepalde ##
#################################

#### AULA 1

# Montando um modelinho de regressão hipotético...
# Vamos ver se dá pra confiar na estimação

X = rnorm(100)      #gera dados aleatórios
e = rnorm(100)      #gera dados aleatórios

Y = 2 + 1.75*X + e  # Montamos o modelo com os coeficientes reais
hist(Y)             #plota um histograma para verificar a distribuição de Y

reg = lm(Y~X)       #monta o modelo
summary(reg)        #exibe os resultados do modelo
coef(reg)           #exibe só os coeficientes
confint(reg)        #exibe o intervalo de confiança

#### Funcionou?

# plotando a curva de regressão
plot(X,Y, pch = 19)                      #plota o diagrama de dispersão
abline(reg = reg, lwd = 2, col = "red")  #plota a reta

########################################################
# Setando o diretório de trabalho
setwd('~/MODUS_regressao')
# Lista os arquivos existentes no diretório
list.files()


# Carregando os pacotes necessários
library(foreign)
library(ggplot2)
library(descr)

# Lê o banco de dados
pnad = read.spss("PNAD2014_30a50_novo4.sav", to.data.frame = T)

names(pnad)  #nomes das variáveis
head(pnad)   #primeiros casos
str(pnad)    #estrutura do objeto

# plotando a dispersão do log da renda
ggplot(pnad, aes(x=lnrenda))+geom_histogram()


##########
# Regressão

# analisando a variável dependente
summary(pnad$anosesco)
sd(pnad$anosesco)

# Montando a regressão
reg = lm(anosesco~escmãe, data=pnad)

#Anova e resultados
anova(reg)
summary(reg)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

