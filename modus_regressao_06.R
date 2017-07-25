####################################
############# MODUS ################
############ Aula 6 ################
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
pnad = read.spss("PNAD2014_30a50_novo4.sav", to.data.frame = T)


###############################

# Modelo de regressão
reg = lm(isei88 ~ anosesco + isei88pai + escmãe, 
         data=pnad)

summary(reg)

# Fazendo teste de colinearidade
#install.packages("car")
vif(reg)

# Se VIF < 10 = OK
# Se VIF >= 10 = colinearidade

#####################################
# Novo banco de dados
library(foreign)
pnad96 = read.spss("PNAD96_25a60_Modus.sav", 
                   to.data.frame = T)
View(pnad96)

#Calculando a média de renda para cada idade
pnad96$idade = pnad96$idadecen + 40.1481  #Recriando a idade
pnad96$idade[1:100]                       # Verifica os 100 primeiros casos
pnad96$idade = round(pnad96$idade)        # Arredonda
pnad96$idade[1:100]                       # Verifica os 100 primeiros casos

summary(pnad96$idade)      #Verificando a variável

#Calculando a média de renda para cada idade
media_renda = c()
for (i in 25:60){
  media_renda[i-24] = mean(pnad96$lnrenda[pnad96$idade==i])
}
media_renda   # Verificando a variável

#Plota idade contra média de renda
plot(25:60, media_renda, type = "b")


##########################################
### REGRESSÃO POLINOMIAL

### Criando a idade elevada ao quadrado
pnad96$idade2 = pnad96$idade^2
summary(pnad96$idade2)


### Criando um modelo de regressão polinomial
reg_poli = lm(lnrenda ~ idade + idade2, data=pnad96)
summary(reg_poli)
vif(reg_poli)  #verifica colinearidade

#########################################################
# É importante centralizar a idade na média para resolver
# o problema de colinearidade
#########################################################

#Criando a idade centralizada na média
pnad96$idadecentralizada = pnad96$idade - mean(pnad96$idade)
summary(pnad96$idadecentralizada)

#Criando idade centralizada ao quadrado
pnad96$idadecentralizada2 = pnad96$idadecentralizada^2
summary(pnad96$idadecentralizada2)

# Rodando regressão com idade centralizada
reg_poli_cent = lm(lnrenda~idadecentralizada+
                     idadecentralizada2, data=pnad96)
summary(reg_poli_cent)  #Exibe resultados
vif(reg_poli_cent)      #Verifica colinearidade (teste VIF)


###########################################################
#### Regressão com variáveis independentes categóricas ####
###########################################################

# Modelo de regressão
# Y = anosesco
# X = iseipai e raçabin

modelo = lm(anosesco ~ iseipai + raçabin, data=pnad96)
summary(modelo)

#Plotando as curvas de regressão para cada categoria de raça
plot(pnad96$iseipai, pnad96$anosesco)
abline(coef(modelo)[1],coef(modelo)[2], lwd=2, col="red")
abline(coef(modelo)[1]+coef(modelo)[3],coef(modelo)[2], lwd=2, col="blue")

# O mesmo com ggplot2!!!
library(ggplot2)
ggplot(pnad96, aes(x=iseipai, y=anosesco))+geom_point()+
  stat_smooth(aes(col=raçabin),method="lm",se=F)


###########################################
# EXERCÍCIO

# Monte o modelo
# Y = isei88
# X = anosesco, migração

regmig = lm(isei88~anosesco+migração, pnad96)
summary(regmig)
