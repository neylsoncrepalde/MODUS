#####################################
## Testes com Equações Estruturais ##
#########      MODUS      ###########
###### Prof.: Jorge Alexandre #######
##### Script: Neylson Crepalde ######
#####################################

#Usando o pacote lavaan
library(lavaan)
library(semPlot)

help(lavaan)

# The Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '

fit <- lavaan(HS.model, data=HolzingerSwineford1939,
              auto.var=TRUE, auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)
summary(fit, fit.measures=TRUE)


## The industrialization and Political Democracy Example 
## Bollen (1989), page 332
model <- ' 
# latent variable definitions
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + a*y2 + b*y3 + c*y4
dem65 =~ y5 + a*y6 + b*y7 + c*y8

# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60

# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'

fit <- sem(model, data=PoliticalDemocracy)
summary(fit, fit.measures=TRUE)

# Plotando o modelo
semPaths(fit, what = "std", layout = "tree")

###########################################################
### Agora com os dados da PNAD96

library(foreign)
setwd("~/MODUS_regressao")
list.files()
pnad96 = read.spss("PNAD96_25a60_Modus.sav", to.data.frame = T)

modelo = '
#variáveis latentes
DSE =~ iseipai + anosesco

#regressões
lnrenda ~ DSE
isei88 ~ DSE
anosesco ~ iseipai
'

fit = sem(modelo, data=pnad96)
summary(fit)
semPaths(fit, what = "est", layout = "tree", fade=F)
