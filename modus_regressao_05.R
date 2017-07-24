### MODUS
### Aula 5
### Prof. Jorge Alexandre
### Monitoria: Neylson Crepalde
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


#MONTANDO MODELOS

# Y = isei88
# X = anosesco

reg1 = lm(isei88 ~ anosesco, data=pnad)
summary(reg1)

## Y = isei88
## X = isei88pai

reg2 = lm(isei88 ~ isei88pai, data=pnad)
summary(reg2)

####
# Montando isei88 ~ anosesco + isei88pai

reg3 = lm(isei88 ~ anosesco + isei88pai, data=pnad)
summary(reg3)

summary(reg1)

#Novo Modelo
# Y = isei88
# X = anosesco + isei88pai + escmãe

reg4 = lm(isei88 ~ anosesco + isei88pai + escmãe, data=pnad)
summary(reg4)
stargazer(reg1, reg2, reg3, reg4, type="html", 
          out="tabela.html")

