####################################
############# MODUS ################
############ Aula 8 ################
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


####################################
# Retomando o modelo de ontem

#Y = lnrenda
#X = anosesco em interação com raçabin

m1 = lm(lnrenda~anosesco*raçabin, pnad96)
summary(m1)

intercepto = coef(m1)[1]
exp(intercepto) # Valor esperado de Y quando x1 é 0 e x2 é 0
exp(intercepto)*1.2536 # Valor esperado de Y quando x1=0 e x2=1

###########################################
### Interação entre variáveis quantitativas

# Elabore o modelo:
# Y = lnrenda
# X = anosesco*idadecen e idadecen2

m2 = lm(lnrenda~anosesco*idadecen+idadecen2, pnad96)
summary(m2)

intercepto = coef(m2)[1]
exp(intercepto)

coef(m2)
# efeito de anosesco para idade média (40 anos)
coef(m2)[2]
# efeito de anosesco para 30 anos
coef(m2)[2] - coef(m2)[5]*10

#####################################
