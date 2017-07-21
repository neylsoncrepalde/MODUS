### MODUS
### Aula 4
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

################################
### Estimar um modelo log-lin
### Y = lnrenda
### X = anosesco

reg = lm(lnrenda ~ anosesco, data=pnad) #Roda a regressão
resultados = summary(reg)   # salva os resultados num objeto
resultados                  # Exibe os resultados
resultados$r.squared        # Exibe o R2

### Criando o log de anos de escolaridade
pnad$lnanosesco = log(pnad$anosesco + 1)
hist(pnad$anosesco)

# Rodando a regressão log-log
reg_log = lm(lnrenda ~ lnanosesco, data=pnad)
resultados_log = summary(reg_log)
print(resultados_log)
resultados_log$r.squared


resultados = summary(reg)
resultados_log = summary(reg_log)


# Colocando os R2 lado a lado para comparar
cbind(resultados$r.squared, resultados_log$r.squared)

#Exibindo os R2 de outro jeito
cat("R2 do modelo log-lin: "); resultados$r.squared
cat("R2 do modelo log-log: "); resultados_log$r.squared
