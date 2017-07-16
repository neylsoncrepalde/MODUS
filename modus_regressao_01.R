############## MODUS ############
###### Análise de Regressão #####
## Prof. Jorge Alexandre Neves ##
## Monitoria: Neylson Crepalde ##
#################################

#### AULA 1

# Setando o diretório de trabalho
setwd('~/Documentos/Neylson Crepalde/Doutorado/MODUS')
# Lista os arquivos existentes no diretório
list.files()


# Carregando os pacotes necessários
library(foreign)
library(ggplot2)
library(descr)

# Lê o banco de dados
pnad = read.spss("PNAD96_30a50_novo3.sav", to.data.frame = T)

names(pnad)  #nomes das variáveis
head(pnad)   #primeiros casos
str(pnad)    #estrutura do objeto

# plotando a dispersão do log da renda
ggplot(pnad, aes(x=lnrenda))+geom_histogram()




