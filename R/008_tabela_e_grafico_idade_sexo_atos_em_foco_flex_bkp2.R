#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
banco_HOMICIDIO = banco_atos_em_foco
# ADOLESCENTE ENCAMINHADOS. Retirados do banco SEM MBA sem adolescentes duplicados
# ordenar nesta ordem para que, quando cortar nome repetidos, preservar data do último ato.
banco_HOMICIDIO <-banco_HOMICIDIO[order(banco_HOMICIDIO$DATA_ATO, decreasing=TRUE),]#ordenar, decrescente, data do ato
banco_HOMICIDIO <-banco_HOMICIDIO[order(banco_HOMICIDIO$NOME2, decreasing=FALSE),]#ordenar, crescente, nome2

#########################################################################################################
banco_HOMICIDIO = filter(banco_HOMICIDIO, ATO_INFRACIONAL == "HOMICÍDIO")
  #|ATO_INFRACIONAL_ATA_02 == "HOMICÍDIO" | ATO_INFRACIONAL_ATA_03 == "HOMICÍDIO")


#########################################################################################################

#retirar nomes duplicados:snr=sem nome repetido
df_snr_banco_HOMICIDIO <- banco_HOMICIDIO[!duplicated(data.frame(banco_HOMICIDIO$NOME2, banco_HOMICIDIO$NASCIMENTO)),]

#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=TRUE)

#write.csv(df_snr_banco_HOMICIDIO, file ="df_snr_banco_HOMICIDIO.csv",row.names=TRUE)
#write.xlsx(df_snr_banco_HOMICIDIO, file ="df_snr_banco_HOMICIDIO.xlsx")
#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos s/inf ou #valor!)
df_snr_banco_HOMICIDIO_bkp = df_snr_banco_HOMICIDIO

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_banco_HOMICIDIO = df_snr_banco_HOMICIDIO %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_banco_HOMICIDIO$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_HOMICIDIO$SEXO <- as.character(df_snr_sexo_idade_banco_HOMICIDIO$SEXO)

df_snr_sexo_idade_banco_HOMICIDIO$SEXO[df_snr_sexo_idade_banco_HOMICIDIO$SEXO == ""]<- "M"
table(df_snr_sexo_idade_banco_HOMICIDIO$SEXO)

df_snr_sexo_idade_banco_HOMICIDIO <- table(df_snr_sexo_idade_banco_HOMICIDIO$IDADE, df_snr_sexo_idade_banco_HOMICIDIO$SEXO, useNA ="always")
#write.csv(df_snr_sexo_idade_banco_HOMICIDIO, file ="df_snr_sexo_idade_banco_HOMICIDIO.csv",row.names=TRUE)
#write.csv(df_snr_sexo_idade_banco_HOMICIDIO, file ="df_snr_sexo_idade_banco_HOMICIDIO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_HOMICIDIO)

df_snr_sexo_idade_banco_HOMICIDIO = data.frame(df_snr_sexo_idade_banco_HOMICIDIO)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_HOMICIDIO_bkp = df_snr_sexo_idade_banco_HOMICIDIO


df_snr_sexo_idade_banco_HOMICIDIO

colnames(df_snr_sexo_idade_banco_HOMICIDIO) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_banco_HOMICIDIO

df_snr_sexo_idade_banco_HOMICIDIO$IDADE <- as.character(df_snr_sexo_idade_banco_HOMICIDIO$IDADE)
df_snr_sexo_idade_banco_HOMICIDIO$SEXO <- as.character(df_snr_sexo_idade_banco_HOMICIDIO$SEXO)
sum(df_snr_sexo_idade_banco_HOMICIDIO$QUANTIDADE)


df_snr_sexo_idade_banco_HOMICIDIO = filter(df_snr_sexo_idade_banco_HOMICIDIO, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_HOMICIDIO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_HOMICIDIO$IDADE[df_snr_sexo_idade_banco_HOMICIDIO$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_HOMICIDIO$IDADE[which(is.na(df_snr_sexo_idade_banco_HOMICIDIO$IDADE))] <- "s/inf"
df_snr_sexo_idade_banco_HOMICIDIO


df_snr_sexo_idade_banco_HOMICIDIO$IDADE <- paste(df_snr_sexo_idade_banco_HOMICIDIO$IDADE, "anos", sep=" ")
df_snr_sexo_idade_banco_HOMICIDIO$IDADE[df_snr_sexo_idade_banco_HOMICIDIO$IDADE == "s/inf anos"]<- "s/inf"

df_snr_sexo_idade_banco_HOMICIDIO <- reshape(data = df_snr_sexo_idade_banco_HOMICIDIO, idvar = "IDADE", timevar = "SEXO", direction = "wide")
df_snr_sexo_idade_banco_HOMICIDIO

#colnames(df_snr_sexo_idade_banco_HOMICIDIO) <- c("IDADE", "MAS")#retirei feminino. Só tem masculino. Estava assim: c("IDADE", "FEM", "MAS")
colnames(df_snr_sexo_idade_banco_HOMICIDIO) <- c("IDADE", "FEM", "MAS")#retirei feminino. Só tem masculino. Estava assim: c("IDADE", "FEM", "MAS")

df_snr_sexo_idade_banco_HOMICIDIO

#df_snr_sexo_idade_banco_HOMICIDIO$FEM[which(is.na(df_snr_sexo_idade_banco_HOMICIDIO$FEM))] <- 0 #linha comentada quando não tem feminino
df_snr_sexo_idade_banco_HOMICIDIO$FEM[which(is.na(df_snr_sexo_idade_banco_HOMICIDIO$FEM))] <- 0 #linha comentada
df_snr_sexo_idade_banco_HOMICIDIO$MAS[which(is.na(df_snr_sexo_idade_banco_HOMICIDIO$MAS))] <- 0

df_snr_sexo_idade_banco_HOMICIDIO
#ordenar idade
df_snr_sexo_idade_banco_HOMICIDIO = df_snr_sexo_idade_banco_HOMICIDIO %>% arrange(IDADE)

#df_snr_sexo_idade_banco_HOMICIDIO$FEM <- as.numeric(df_snr_sexo_idade_banco_HOMICIDIO$FEM) #comentada

df_snr_sexo_idade_banco_HOMICIDIO$FEM <- as.numeric(df_snr_sexo_idade_banco_HOMICIDIO$FEM) #comentada quando falta fem
df_snr_sexo_idade_banco_HOMICIDIO$MAS <- as.numeric(df_snr_sexo_idade_banco_HOMICIDIO$MAS)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
#df_snr_sexo_idade_banco_HOMICIDIO$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_HOMICIDIO$FEM)*100, 2) #comentada
df_snr_sexo_idade_banco_HOMICIDIO$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_HOMICIDIO$FEM)*100, 2) #comentada quando falta fem
df_snr_sexo_idade_banco_HOMICIDIO$M <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_HOMICIDIO$MAS)*100, 2)
df_snr_sexo_idade_banco_HOMICIDIO
#########################################################################################################
df_snr_sexo_idade_banco_HOMICIDIO <- df_snr_sexo_idade_banco_HOMICIDIO[c("IDADE", "FEM", "MAS", "F", "M")]#comentada quando falta fem
#df_snr_sexo_idade_banco_HOMICIDIO <- df_snr_sexo_idade_banco_HOMICIDIO[c("IDADE", "MAS", "M")]
df_snr_sexo_idade_banco_HOMICIDIO <- df_snr_sexo_idade_banco_HOMICIDIO[c("IDADE", "FEM", "MAS", "F", "M")]#comentada quando falta fem

df_snr_sexo_idade_banco_HOMICIDIO
#########################################################################################################

df_snr_sexo_idade_banco_HOMICIDIO<- rbind(df_snr_sexo_idade_banco_HOMICIDIO,
                                          data.frame(IDADE = "TOTAL",
                                                     FEM = sum(df_snr_sexo_idade_banco_HOMICIDIO$FEM),#comentada quando falta fem
                                                     F = sum(df_snr_sexo_idade_banco_HOMICIDIO$F),#comentada quando falta fem
                                                     MAS = sum(df_snr_sexo_idade_banco_HOMICIDIO$MAS),
                                                     M = sum(df_snr_sexo_idade_banco_HOMICIDIO$M),
                                                     stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_HOMICIDIO

colnames(df_snr_sexo_idade_banco_HOMICIDIO) <- c("IDADE", "FEM", "MAS", "%", "M")
df_snr_sexo_idade_banco_HOMICIDIO
#########################################################################################################
#########################################################################################################
#salvando tabela
#pdf(file="tabela_df_snr_sexo_idade_banco_HOMICIDIO_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_df_snr_sexo_idade_banco_HOMICIDIO_alternativa")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_HOMICIDIO = df_snr_banco_HOMICIDIO %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_HOMICIDIO$SEXO)

df_snr_sexo_idade_HOMICIDIO$SEXO <- as.character(df_snr_sexo_idade_HOMICIDIO$SEXO)

df_snr_sexo_idade_HOMICIDIO$SEXO[df_snr_sexo_idade_HOMICIDIO$SEXO == ""]<- "M"
table(df_snr_sexo_idade_HOMICIDIO$SEXO)

df_snr_sexo_idade_HOMICIDIO <- table(df_snr_sexo_idade_HOMICIDIO$IDADE, df_snr_sexo_idade_HOMICIDIO$SEXO, useNA ="always")
##write.csv(df_snr_sexo_idade_HOMICIDIO, file ="df_snr_sexo_idade_HOMICIDIO.csv",row.names=TRUE)
##write.csv(df_snr_sexo_idade_HOMICIDIO, file ="df_snr_sexo_idade_HOMICIDIO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_HOMICIDIO)

df_snr_sexo_idade_HOMICIDIO = data.frame(df_snr_sexo_idade_HOMICIDIO)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_HOMICIDIO_bkp = df_snr_sexo_idade_HOMICIDIO


df_snr_sexo_idade_HOMICIDIO

colnames(df_snr_sexo_idade_HOMICIDIO) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_HOMICIDIO

df_snr_sexo_idade_HOMICIDIO$IDADE <- as.character(df_snr_sexo_idade_HOMICIDIO$IDADE)
df_snr_sexo_idade_HOMICIDIO$SEXO <- as.character(df_snr_sexo_idade_HOMICIDIO$SEXO)
sum(df_snr_sexo_idade_HOMICIDIO$QUANTIDADE)


df_snr_sexo_idade_HOMICIDIO = filter(df_snr_sexo_idade_HOMICIDIO, !QUANTIDADE == 0)
df_snr_sexo_idade_HOMICIDIO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_HOMICIDIO$IDADE[df_snr_sexo_idade_HOMICIDIO$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_HOMICIDIO$IDADE[which(is.na(df_snr_sexo_idade_HOMICIDIO$IDADE))] <- "s/inf"
df_snr_sexo_idade_HOMICIDIO


#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_HOMICIDIO_rmark = df_snr_sexo_idade_HOMICIDIO

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_HOMICIDIO_rmark = df_snr_sexo_idade_HOMICIDIO_rmark %>%
  top_n(3, IDADE) %>% arrange(desc(IDADE))

#somando
sum(df_snr_sexo_idade_HOMICIDIO_rmark$QUANTIDADE)

#para escolher linhas e posicoes
df_snr_sexo_idade_HOMICIDIO_rmark[3,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_HOMICIDIO$IDADE <- paste(df_snr_sexo_idade_HOMICIDIO$IDADE, "anos", sep=" ")
df_snr_sexo_idade_HOMICIDIO$IDADE[df_snr_sexo_idade_HOMICIDIO$IDADE == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade_HOMICIDIO




setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#########################################################################################################
#########################################################################################################
# GRAFICO SEXO HOMICIDIO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_sexo_HOMICIDIO = df_snr_banco_HOMICIDIO_bkp %>%
  select(SEXO)

table(df_sexo_HOMICIDIO$SEXO)

#########################################################################################################
#########################################################################################################

df_sexo_HOMICIDIO$SEXO <- as.character(df_sexo_HOMICIDIO$SEXO)

df_sexo_HOMICIDIO$SEXO[df_sexo_HOMICIDIO$SEXO == ""]<- "M"
table(df_sexo_HOMICIDIO$SEXO)

df_sexo_HOMICIDIO = data.frame(table(df_sexo_HOMICIDIO$SEXO))

colnames(df_sexo_HOMICIDIO) <- c("SEXO", "QUANTIDADE")

sum(df_sexo_HOMICIDIO$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_sexo_HOMICIDIO$SEXO <- as.character(df_sexo_HOMICIDIO$SEXO)

df_sexo_HOMICIDIO$SEXO[df_sexo_HOMICIDIO$SEXO == "F"]<- "FEMININO"
df_sexo_HOMICIDIO$SEXO[df_sexo_HOMICIDIO$SEXO == "M"]<- "MASCULINO"

#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_sexo_HOMICIDIO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_sexo_HOMICIDIO <- df_sexo_HOMICIDIO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_sexo_HOMICIDIO$PERCENTUAL <- paste(df_sexo_HOMICIDIO$PERCENTUAL, "%", sep=" ")

df_sexo_HOMICIDIO


setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#ROUBO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
banco_ROUBO = banco_atos_em_foco
# ADOLESCENTE ENCAMINHADOS. Retirados do banco SEM MBA sem adolescentes duplicados
# ordenar nesta ordem para que, quando cortar nome repetidos, preservar data do último ato.
banco_ROUBO <-banco_ROUBO[order(banco_ROUBO$DATA_ATO, decreasing=TRUE),]#ordenar, decrescente, data do ato
banco_ROUBO <-banco_ROUBO[order(banco_ROUBO$NOME2, decreasing=FALSE),]#ordenar, crescente, nome2

#########################################################################################################
banco_ROUBO = filter(banco_ROUBO, ATO_INFRACIONAL == "ROUBO")
 #|ATO_INFRACIONAL_ATA_02 == "ROUBO" | ATO_INFRACIONAL_ATA_03 == "ROUBO")

#########################################################################################################

#retirar nomes duplicados:snr=sem nome repetido
df_snr_banco_ROUBO <- banco_ROUBO[!duplicated(data.frame(banco_ROUBO$NOME2, banco_ROUBO$NASCIMENTO)),]

#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=TRUE)

#write.csv(df_snr_banco_ROUBO, file ="df_snr_banco_ROUBO.csv",row.names=TRUE)
#write.xlsx(df_snr_banco_ROUBO, file ="df_snr_banco_ROUBO.xlsx")
#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos s/inf ou #valor!)
df_snr_banco_ROUBO_bkp = df_snr_banco_ROUBO

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_banco_ROUBO = df_snr_banco_ROUBO %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_banco_ROUBO$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_ROUBO$SEXO <- as.character(df_snr_sexo_idade_banco_ROUBO$SEXO)

df_snr_sexo_idade_banco_ROUBO$SEXO[df_snr_sexo_idade_banco_ROUBO$SEXO == ""]<- "M"
table(df_snr_sexo_idade_banco_ROUBO$SEXO)

df_snr_sexo_idade_banco_ROUBO <- table(df_snr_sexo_idade_banco_ROUBO$IDADE, df_snr_sexo_idade_banco_ROUBO$SEXO, useNA ="always")
#write.csv(df_snr_sexo_idade_banco_ROUBO, file ="df_snr_sexo_idade_banco_ROUBO.csv",row.names=TRUE)
#write.csv(df_snr_sexo_idade_banco_ROUBO, file ="df_snr_sexo_idade_banco_ROUBO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_ROUBO)

df_snr_sexo_idade_banco_ROUBO = data.frame(df_snr_sexo_idade_banco_ROUBO)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_ROUBO_bkp = df_snr_sexo_idade_banco_ROUBO


df_snr_sexo_idade_banco_ROUBO

colnames(df_snr_sexo_idade_banco_ROUBO) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_banco_ROUBO

df_snr_sexo_idade_banco_ROUBO$IDADE <- as.character(df_snr_sexo_idade_banco_ROUBO$IDADE)
df_snr_sexo_idade_banco_ROUBO$SEXO <- as.character(df_snr_sexo_idade_banco_ROUBO$SEXO)
sum(df_snr_sexo_idade_banco_ROUBO$QUANTIDADE)


df_snr_sexo_idade_banco_ROUBO = filter(df_snr_sexo_idade_banco_ROUBO, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_ROUBO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_ROUBO$IDADE[df_snr_sexo_idade_banco_ROUBO$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_ROUBO$IDADE[which(is.na(df_snr_sexo_idade_banco_ROUBO$IDADE))] <- "s/inf"
df_snr_sexo_idade_banco_ROUBO


df_snr_sexo_idade_banco_ROUBO$IDADE <- paste(df_snr_sexo_idade_banco_ROUBO$IDADE, "anos", sep=" ")
df_snr_sexo_idade_banco_ROUBO$IDADE[df_snr_sexo_idade_banco_ROUBO$IDADE == "s/inf anos"]<- "s/inf"

df_snr_sexo_idade_banco_ROUBO <- reshape(data = df_snr_sexo_idade_banco_ROUBO, idvar = "IDADE", timevar = "SEXO", direction = "wide")
df_snr_sexo_idade_banco_ROUBO

colnames(df_snr_sexo_idade_banco_ROUBO) <- c("IDADE", "FEM", "MAS")
df_snr_sexo_idade_banco_ROUBO

df_snr_sexo_idade_banco_ROUBO$FEM[which(is.na(df_snr_sexo_idade_banco_ROUBO$FEM))] <- 0
df_snr_sexo_idade_banco_ROUBO$MAS[which(is.na(df_snr_sexo_idade_banco_ROUBO$MAS))] <- 0

df_snr_sexo_idade_banco_ROUBO
#ordenar idade
df_snr_sexo_idade_banco_ROUBO = df_snr_sexo_idade_banco_ROUBO %>% arrange(IDADE)

df_snr_sexo_idade_banco_ROUBO$FEM <- as.numeric(df_snr_sexo_idade_banco_ROUBO$FEM)
df_snr_sexo_idade_banco_ROUBO$MAS <- as.numeric(df_snr_sexo_idade_banco_ROUBO$MAS)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_snr_sexo_idade_banco_ROUBO$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_ROUBO$FEM)*100, 2)
df_snr_sexo_idade_banco_ROUBO$M <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_ROUBO$MAS)*100, 2)
df_snr_sexo_idade_banco_ROUBO
#########################################################################################################
df_snr_sexo_idade_banco_ROUBO <- df_snr_sexo_idade_banco_ROUBO[c("IDADE", "FEM", "F", "MAS", "M")]
df_snr_sexo_idade_banco_ROUBO
#########################################################################################################

df_snr_sexo_idade_banco_ROUBO<- rbind(df_snr_sexo_idade_banco_ROUBO,
                                      data.frame(IDADE = "TOTAL",
                                                 FEM = sum(df_snr_sexo_idade_banco_ROUBO$FEM),
                                                 F = sum(df_snr_sexo_idade_banco_ROUBO$F),
                                                 MAS = sum(df_snr_sexo_idade_banco_ROUBO$MAS),
                                                 M = sum(df_snr_sexo_idade_banco_ROUBO$M),
                                                 stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_ROUBO

colnames(df_snr_sexo_idade_banco_ROUBO) <- c("IDADE", "FEM", "%", "MAS", "%")
df_snr_sexo_idade_banco_ROUBO
#########################################################################################################
#########################################################################################################
#salvando tabela
#pdf(file="tabela_df_snr_sexo_idade_banco_ROUBO_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_df_snr_sexo_idade_banco_ROUBO_alternativa")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_ROUBO = df_snr_banco_ROUBO %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_ROUBO$SEXO)

df_snr_sexo_idade_ROUBO$SEXO <- as.character(df_snr_sexo_idade_ROUBO$SEXO)

df_snr_sexo_idade_ROUBO$SEXO[df_snr_sexo_idade_ROUBO$SEXO == ""]<- "M"
table(df_snr_sexo_idade_ROUBO$SEXO)

df_snr_sexo_idade_ROUBO <- table(df_snr_sexo_idade_ROUBO$IDADE, df_snr_sexo_idade_ROUBO$SEXO, useNA ="always")
##write.csv(df_snr_sexo_idade_ROUBO, file ="df_snr_sexo_idade_ROUBO.csv",row.names=TRUE)
##write.csv(df_snr_sexo_idade_ROUBO, file ="df_snr_sexo_idade_ROUBO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_ROUBO)

df_snr_sexo_idade_ROUBO = data.frame(df_snr_sexo_idade_ROUBO)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_ROUBO_bkp = df_snr_sexo_idade_ROUBO


df_snr_sexo_idade_ROUBO

colnames(df_snr_sexo_idade_ROUBO) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_ROUBO

df_snr_sexo_idade_ROUBO$IDADE <- as.character(df_snr_sexo_idade_ROUBO$IDADE)
df_snr_sexo_idade_ROUBO$SEXO <- as.character(df_snr_sexo_idade_ROUBO$SEXO)
sum(df_snr_sexo_idade_ROUBO$QUANTIDADE)


df_snr_sexo_idade_ROUBO = filter(df_snr_sexo_idade_ROUBO, !QUANTIDADE == 0)
df_snr_sexo_idade_ROUBO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_ROUBO$IDADE[df_snr_sexo_idade_ROUBO$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_ROUBO$IDADE[which(is.na(df_snr_sexo_idade_ROUBO$IDADE))] <- "s/inf"
df_snr_sexo_idade_ROUBO


#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_ROUBO_rmark = df_snr_sexo_idade_ROUBO

#excluindo sem informacao
df_snr_sexo_idade_ROUBO_rmark1 = filter(df_snr_sexo_idade_ROUBO_rmark, !IDADE == "s/inf")
#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_ROUBO_rmark1 = df_snr_sexo_idade_ROUBO_rmark1 %>%
  top_n(5, QUANTIDADE) %>% arrange(desc(IDADE))

#somando
sum(df_snr_sexo_idade_ROUBO_rmark$QUANTIDADE)

#para escolher linhas e posicoes
df_snr_sexo_idade_ROUBO_rmark1[4,1]
df_snr_sexo_idade_ROUBO_rmark1[1,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



df_snr_sexo_idade_ROUBO$IDADE <- paste(df_snr_sexo_idade_ROUBO$IDADE, "anos", sep=" ")
df_snr_sexo_idade_ROUBO$IDADE[df_snr_sexo_idade_ROUBO$IDADE == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade_ROUBO


setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#########################################################################################################
# GRAFICO SEXO ROUBO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_sexo_ROUBO = df_snr_banco_ROUBO_bkp %>%
  select(SEXO)

table(df_sexo_ROUBO$SEXO)

#########################################################################################################
#########################################################################################################

df_sexo_ROUBO$SEXO <- as.character(df_sexo_ROUBO$SEXO)

df_sexo_ROUBO$SEXO[df_sexo_ROUBO$SEXO == ""]<- "M"
table(df_sexo_ROUBO$SEXO)

df_sexo_ROUBO = data.frame(table(df_sexo_ROUBO$SEXO))

colnames(df_sexo_ROUBO) <- c("SEXO", "QUANTIDADE")

sum(df_sexo_ROUBO$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_sexo_ROUBO$SEXO <- as.character(df_sexo_ROUBO$SEXO)

df_sexo_ROUBO$SEXO[df_sexo_ROUBO$SEXO == "F"]<- "FEMININO"
df_sexo_ROUBO$SEXO[df_sexo_ROUBO$SEXO == "M"]<- "MASCULINO"

#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_sexo_ROUBO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_sexo_ROUBO <- df_sexo_ROUBO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_sexo_ROUBO$PERCENTUAL <- paste(df_sexo_ROUBO$PERCENTUAL, "%", sep=" ")

df_sexo_ROUBO


setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################

#########################################################################################################
#FURTO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
banco_FURTO = banco_atos_em_foco
# ADOLESCENTE ENCAMINHADOS. Retirados do banco SEM MBA sem adolescentes duplicados
# ordenar nesta ordem para que, quando cortar nome repetidos, preservar data do último ato.
banco_FURTO <-banco_FURTO[order(banco_FURTO$DATA_ATO, decreasing=TRUE),]#ordenar, decrescente, data do ato
banco_FURTO <-banco_FURTO[order(banco_FURTO$NOME2, decreasing=FALSE),]#ordenar, crescente, nome2

#########################################################################################################
banco_FURTO = filter(banco_FURTO, ATO_INFRACIONAL == "FURTO")
#|ATO_INFRACIONAL_ATA_02 == "FURTO" |ATO_INFRACIONAL_ATA_03 == "FURTO")

#########################################################################################################

#retirar nomes duplicados:snr=sem nome repetido
df_snr_banco_FURTO <- banco_FURTO[!duplicated(data.frame(banco_FURTO$NOME2, banco_FURTO$NASCIMENTO)),]

#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=TRUE)

#write.csv(df_snr_banco_FURTO, file ="df_snr_banco_FURTO.csv",row.names=TRUE)
#write.xlsx(df_snr_banco_FURTO, file ="df_snr_banco_FURTO.xlsx")
#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos s/inf ou #valor!)
df_snr_banco_FURTO_bkp = df_snr_banco_FURTO

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_banco_FURTO = df_snr_banco_FURTO %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_banco_FURTO$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_FURTO$SEXO <- as.character(df_snr_sexo_idade_banco_FURTO$SEXO)

df_snr_sexo_idade_banco_FURTO$SEXO[df_snr_sexo_idade_banco_FURTO$SEXO == ""]<- "M"
table(df_snr_sexo_idade_banco_FURTO$SEXO)

df_snr_sexo_idade_banco_FURTO <- table(df_snr_sexo_idade_banco_FURTO$IDADE, df_snr_sexo_idade_banco_FURTO$SEXO, useNA ="always")
#write.csv(df_snr_sexo_idade_banco_FURTO, file ="df_snr_sexo_idade_banco_FURTO.csv",row.names=TRUE)
#write.csv(df_snr_sexo_idade_banco_FURTO, file ="df_snr_sexo_idade_banco_FURTO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_FURTO)

df_snr_sexo_idade_banco_FURTO = data.frame(df_snr_sexo_idade_banco_FURTO)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_FURTO_bkp = df_snr_sexo_idade_banco_FURTO


df_snr_sexo_idade_banco_FURTO

colnames(df_snr_sexo_idade_banco_FURTO) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_banco_FURTO

df_snr_sexo_idade_banco_FURTO$IDADE <- as.character(df_snr_sexo_idade_banco_FURTO$IDADE)
df_snr_sexo_idade_banco_FURTO$SEXO <- as.character(df_snr_sexo_idade_banco_FURTO$SEXO)
sum(df_snr_sexo_idade_banco_FURTO$QUANTIDADE)


df_snr_sexo_idade_banco_FURTO = filter(df_snr_sexo_idade_banco_FURTO, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_FURTO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_FURTO$IDADE[df_snr_sexo_idade_banco_FURTO$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_FURTO$IDADE[which(is.na(df_snr_sexo_idade_banco_FURTO$IDADE))] <- "s/inf"
df_snr_sexo_idade_banco_FURTO


df_snr_sexo_idade_banco_FURTO$IDADE <- paste(df_snr_sexo_idade_banco_FURTO$IDADE, "anos", sep=" ")
df_snr_sexo_idade_banco_FURTO$IDADE[df_snr_sexo_idade_banco_FURTO$IDADE == "s/inf anos"]<- "s/inf"

df_snr_sexo_idade_banco_FURTO <- reshape(data = df_snr_sexo_idade_banco_FURTO, idvar = "IDADE", timevar = "SEXO", direction = "wide")
df_snr_sexo_idade_banco_FURTO

colnames(df_snr_sexo_idade_banco_FURTO) <- c("IDADE", "FEM", "MAS")
df_snr_sexo_idade_banco_FURTO

df_snr_sexo_idade_banco_FURTO$FEM[which(is.na(df_snr_sexo_idade_banco_FURTO$FEM))] <- 0
df_snr_sexo_idade_banco_FURTO$MAS[which(is.na(df_snr_sexo_idade_banco_FURTO$MAS))] <- 0

df_snr_sexo_idade_banco_FURTO
#ordenar idade
df_snr_sexo_idade_banco_FURTO = df_snr_sexo_idade_banco_FURTO %>% arrange(IDADE)

df_snr_sexo_idade_banco_FURTO$FEM <- as.numeric(df_snr_sexo_idade_banco_FURTO$FEM)
df_snr_sexo_idade_banco_FURTO$MAS <- as.numeric(df_snr_sexo_idade_banco_FURTO$MAS)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_snr_sexo_idade_banco_FURTO$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_FURTO$FEM)*100, 2)
df_snr_sexo_idade_banco_FURTO$M <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_FURTO$MAS)*100, 2)
df_snr_sexo_idade_banco_FURTO
#########################################################################################################
df_snr_sexo_idade_banco_FURTO <- df_snr_sexo_idade_banco_FURTO[c("IDADE", "FEM", "F", "MAS", "M")]
df_snr_sexo_idade_banco_FURTO
#########################################################################################################

df_snr_sexo_idade_banco_FURTO<- rbind(df_snr_sexo_idade_banco_FURTO,
                                      data.frame(IDADE = "TOTAL",
                                                 FEM = sum(df_snr_sexo_idade_banco_FURTO$FEM),
                                                 F = sum(df_snr_sexo_idade_banco_FURTO$F),
                                                 MAS = sum(df_snr_sexo_idade_banco_FURTO$MAS),
                                                 M = sum(df_snr_sexo_idade_banco_FURTO$M),
                                                 stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_FURTO

colnames(df_snr_sexo_idade_banco_FURTO) <- c("IDADE", "FEM", "%", "MAS", "%")
df_snr_sexo_idade_banco_FURTO
#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_FURTO = df_snr_banco_FURTO %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_FURTO$SEXO)

df_snr_sexo_idade_FURTO$SEXO <- as.character(df_snr_sexo_idade_FURTO$SEXO)

df_snr_sexo_idade_FURTO$SEXO[df_snr_sexo_idade_FURTO$SEXO == "m"]<- "M"
table(df_snr_sexo_idade_FURTO$SEXO)

df_snr_sexo_idade_FURTO <- table(df_snr_sexo_idade_FURTO$IDADE, df_snr_sexo_idade_FURTO$SEXO, useNA ="always")
##write.csv(df_snr_sexo_idade_FURTO, file ="df_snr_sexo_idade_FURTO.csv",row.names=TRUE)
##write.csv(df_snr_sexo_idade_FURTO, file ="df_snr_sexo_idade_FURTO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_FURTO)

df_snr_sexo_idade_FURTO = data.frame(df_snr_sexo_idade_FURTO)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_FURTO_bkp = df_snr_sexo_idade_FURTO


df_snr_sexo_idade_FURTO

colnames(df_snr_sexo_idade_FURTO) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_FURTO

df_snr_sexo_idade_FURTO$IDADE <- as.character(df_snr_sexo_idade_FURTO$IDADE)
df_snr_sexo_idade_FURTO$SEXO <- as.character(df_snr_sexo_idade_FURTO$SEXO)
sum(df_snr_sexo_idade_FURTO$QUANTIDADE)


df_snr_sexo_idade_FURTO = filter(df_snr_sexo_idade_FURTO, !QUANTIDADE == 0)
df_snr_sexo_idade_FURTO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_FURTO$IDADE[df_snr_sexo_idade_FURTO$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_FURTO$IDADE[which(is.na(df_snr_sexo_idade_FURTO$IDADE))] <- "s/inf"
df_snr_sexo_idade_FURTO

#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_FURTO_rmark = df_snr_sexo_idade_FURTO

#excluindo sem informacao
df_snr_sexo_idade_FURTO_rmark1 = filter(df_snr_sexo_idade_FURTO_rmark, !IDADE == "s/inf")
#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_FURTO_rmark1 = df_snr_sexo_idade_FURTO_rmark1 %>%
  top_n(4, IDADE) %>% arrange(desc(IDADE))

#somando
sum(df_snr_sexo_idade_FURTO_rmark$QUANTIDADE)

#para escolher linhas e posicoes
df_snr_sexo_idade_FURTO_rmark1[4,1]
df_snr_sexo_idade_FURTO_rmark1[1,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################




df_snr_sexo_idade_FURTO$IDADE <- paste(df_snr_sexo_idade_FURTO$IDADE, "anos", sep=" ")
df_snr_sexo_idade_FURTO$IDADE[df_snr_sexo_idade_FURTO$IDADE == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade_FURTO


setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#########################################################################################################
# GRAFICO SEXO FURTO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_sexo_FURTO = df_snr_banco_FURTO_bkp %>%
  select(SEXO)

table(df_sexo_FURTO$SEXO)

#########################################################################################################
#########################################################################################################

df_sexo_FURTO$SEXO <- as.character(df_sexo_FURTO$SEXO)

df_sexo_FURTO$SEXO[df_sexo_FURTO$SEXO == ""]<- "M"
table(df_sexo_FURTO$SEXO)

df_sexo_FURTO = data.frame(table(df_sexo_FURTO$SEXO))

colnames(df_sexo_FURTO) <- c("SEXO", "QUANTIDADE")

sum(df_sexo_FURTO$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_sexo_FURTO$SEXO <- as.character(df_sexo_FURTO$SEXO)

df_sexo_FURTO$SEXO[df_sexo_FURTO$SEXO == "F"]<- "FEMININO"
df_sexo_FURTO$SEXO[df_sexo_FURTO$SEXO == "M"]<- "MASCULINO"

#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_sexo_FURTO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_sexo_FURTO <- df_sexo_FURTO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_sexo_FURTO$PERCENTUAL <- paste(df_sexo_FURTO$PERCENTUAL, "%", sep=" ")

df_sexo_FURTO


setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#USO_DE_DROGAS
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
banco_USO_DE_DROGAS = banco_atos_em_foco
# ADOLESCENTE ENCAMINHADOS. Retirados do banco SEM MBA sem adolescentes duplicados
# ordenar nesta ordem para que, quando cortar nome repetidos, preservar data do último ato.
banco_USO_DE_DROGAS <-banco_USO_DE_DROGAS[order(banco_USO_DE_DROGAS$DATA_ATO, decreasing=TRUE),]#ordenar, decrescente, data do ato
banco_USO_DE_DROGAS <-banco_USO_DE_DROGAS[order(banco_USO_DE_DROGAS$NOME2, decreasing=FALSE),]#ordenar, crescente, nome2

#########################################################################################################
banco_USO_DE_DROGAS = filter(banco_USO_DE_DROGAS, ATO_INFRACIONAL == "POSSE DE DROGAS PARA USO PESSOAL")
 #|ATO_INFRACIONAL_ATA_02 == "POSSE DE DROGAS PARA USO PESSOAL" | ATO_INFRACIONAL_ATA_03 == "POSSE DE DROGAS PARA USO PESSOAL")

#########################################################################################################

#retirar nomes duplicados:snr=sem nome repetido
df_snr_banco_USO_DE_DROGAS <- banco_USO_DE_DROGAS[!duplicated(data.frame(banco_USO_DE_DROGAS$NOME2, banco_USO_DE_DROGAS$NASCIMENTO)),]

#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=TRUE)

#write.csv(df_snr_banco_USO_DE_DROGAS, file ="df_snr_banco_USO_DE_DROGAS.csv",row.names=TRUE)
#write.xlsx(df_snr_banco_USO_DE_DROGAS, file ="df_snr_banco_USO_DE_DROGAS.xlsx")
#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos s/inf ou #valor!)
df_snr_banco_USO_DE_DROGAS_bkp = df_snr_banco_USO_DE_DROGAS

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_banco_USO_DE_DROGAS = df_snr_banco_USO_DE_DROGAS %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO <- as.character(df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO)

df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO[df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO == ""]<- "M"
table(df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO)

df_snr_sexo_idade_banco_USO_DE_DROGAS <- table(df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE, df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO, useNA ="always")
#write.csv(df_snr_sexo_idade_banco_USO_DE_DROGAS, file ="df_snr_sexo_idade_banco_USO_DE_DROGAS.csv",row.names=TRUE)
#write.csv(df_snr_sexo_idade_banco_USO_DE_DROGAS, file ="df_snr_sexo_idade_banco_USO_DE_DROGAS.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_USO_DE_DROGAS)

df_snr_sexo_idade_banco_USO_DE_DROGAS = data.frame(df_snr_sexo_idade_banco_USO_DE_DROGAS)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_USO_DE_DROGAS_bkp = df_snr_sexo_idade_banco_USO_DE_DROGAS


df_snr_sexo_idade_banco_USO_DE_DROGAS

colnames(df_snr_sexo_idade_banco_USO_DE_DROGAS) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_banco_USO_DE_DROGAS

df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE <- as.character(df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE)
df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO <- as.character(df_snr_sexo_idade_banco_USO_DE_DROGAS$SEXO)
sum(df_snr_sexo_idade_banco_USO_DE_DROGAS$QUANTIDADE)


df_snr_sexo_idade_banco_USO_DE_DROGAS = filter(df_snr_sexo_idade_banco_USO_DE_DROGAS, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_USO_DE_DROGAS

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE[df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE[which(is.na(df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE))] <- "s/inf"
df_snr_sexo_idade_banco_USO_DE_DROGAS


df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE <- paste(df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE, "anos", sep=" ")
df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE[df_snr_sexo_idade_banco_USO_DE_DROGAS$IDADE == "s/inf anos"]<- "s/inf"

df_snr_sexo_idade_banco_USO_DE_DROGAS <- reshape(data = df_snr_sexo_idade_banco_USO_DE_DROGAS, idvar = "IDADE", timevar = "SEXO", direction = "wide")
df_snr_sexo_idade_banco_USO_DE_DROGAS

colnames(df_snr_sexo_idade_banco_USO_DE_DROGAS) <- c("IDADE", "FEM", "MAS")
df_snr_sexo_idade_banco_USO_DE_DROGAS

df_snr_sexo_idade_banco_USO_DE_DROGAS$FEM[which(is.na(df_snr_sexo_idade_banco_USO_DE_DROGAS$FEM))] <- 0
df_snr_sexo_idade_banco_USO_DE_DROGAS$MAS[which(is.na(df_snr_sexo_idade_banco_USO_DE_DROGAS$MAS))] <- 0

df_snr_sexo_idade_banco_USO_DE_DROGAS
#ordenar idade
df_snr_sexo_idade_banco_USO_DE_DROGAS = df_snr_sexo_idade_banco_USO_DE_DROGAS %>% arrange(IDADE)

df_snr_sexo_idade_banco_USO_DE_DROGAS$FEM <- as.numeric(df_snr_sexo_idade_banco_USO_DE_DROGAS$FEM)
df_snr_sexo_idade_banco_USO_DE_DROGAS$MAS <- as.numeric(df_snr_sexo_idade_banco_USO_DE_DROGAS$MAS)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_snr_sexo_idade_banco_USO_DE_DROGAS$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_USO_DE_DROGAS$FEM)*100, 2)
df_snr_sexo_idade_banco_USO_DE_DROGAS$M <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_USO_DE_DROGAS$MAS)*100, 2)
df_snr_sexo_idade_banco_USO_DE_DROGAS
#########################################################################################################
df_snr_sexo_idade_banco_USO_DE_DROGAS <- df_snr_sexo_idade_banco_USO_DE_DROGAS[c("IDADE", "FEM", "F", "MAS", "M")]
df_snr_sexo_idade_banco_USO_DE_DROGAS
#########################################################################################################

df_snr_sexo_idade_banco_USO_DE_DROGAS<- rbind(df_snr_sexo_idade_banco_USO_DE_DROGAS,
                                              data.frame(IDADE = "TOTAL",
                                                         FEM = sum(df_snr_sexo_idade_banco_USO_DE_DROGAS$FEM),
                                                         F = sum(df_snr_sexo_idade_banco_USO_DE_DROGAS$F),
                                                         MAS = sum(df_snr_sexo_idade_banco_USO_DE_DROGAS$MAS),
                                                         M = sum(df_snr_sexo_idade_banco_USO_DE_DROGAS$M),
                                                         stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_USO_DE_DROGAS

colnames(df_snr_sexo_idade_banco_USO_DE_DROGAS) <- c("IDADE", "FEM", "%", "MAS", "%")
df_snr_sexo_idade_banco_USO_DE_DROGAS
#########################################################################################################
#########################################################################################################
#salvando tabela
#pdf(file="tabela_df_snr_sexo_idade_banco_USO_DE_DROGAS_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_df_snr_sexo_idade_banco_USO_DE_DROGAS_alternativa")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_USO_DE_DROGAS = df_snr_banco_USO_DE_DROGAS %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_USO_DE_DROGAS$SEXO)

df_snr_sexo_idade_USO_DE_DROGAS$SEXO <- as.character(df_snr_sexo_idade_USO_DE_DROGAS$SEXO)

df_snr_sexo_idade_USO_DE_DROGAS$SEXO[df_snr_sexo_idade_USO_DE_DROGAS$SEXO == ""]<- "M"
table(df_snr_sexo_idade_USO_DE_DROGAS$SEXO)

df_snr_sexo_idade_USO_DE_DROGAS <- table(df_snr_sexo_idade_USO_DE_DROGAS$IDADE, df_snr_sexo_idade_USO_DE_DROGAS$SEXO, useNA ="always")
##write.csv(df_snr_sexo_idade_USO_DE_DROGAS, file ="df_snr_sexo_idade_USO_DE_DROGAS.csv",row.names=TRUE)
##write.csv(df_snr_sexo_idade_USO_DE_DROGAS, file ="df_snr_sexo_idade_USO_DE_DROGAS.csv",row.names=TRUE)
sum(df_snr_sexo_idade_USO_DE_DROGAS)

df_snr_sexo_idade_USO_DE_DROGAS = data.frame(df_snr_sexo_idade_USO_DE_DROGAS)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_USO_DE_DROGAS_bkp = df_snr_sexo_idade_USO_DE_DROGAS


df_snr_sexo_idade_USO_DE_DROGAS

colnames(df_snr_sexo_idade_USO_DE_DROGAS) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_USO_DE_DROGAS

df_snr_sexo_idade_USO_DE_DROGAS$IDADE <- as.character(df_snr_sexo_idade_USO_DE_DROGAS$IDADE)
df_snr_sexo_idade_USO_DE_DROGAS$SEXO <- as.character(df_snr_sexo_idade_USO_DE_DROGAS$SEXO)
sum(df_snr_sexo_idade_USO_DE_DROGAS$QUANTIDADE)


df_snr_sexo_idade_USO_DE_DROGAS = filter(df_snr_sexo_idade_USO_DE_DROGAS, !QUANTIDADE == 0)
df_snr_sexo_idade_USO_DE_DROGAS

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_USO_DE_DROGAS$IDADE[df_snr_sexo_idade_USO_DE_DROGAS$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_USO_DE_DROGAS$IDADE[which(is.na(df_snr_sexo_idade_USO_DE_DROGAS$IDADE))] <- "s/inf"
df_snr_sexo_idade_USO_DE_DROGAS


#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_USO_DE_DROGAS_rmark = df_snr_sexo_idade_USO_DE_DROGAS

#excluindo sem informacao e outros
df_snr_sexo_idade_USO_DE_DROGAS_rmark1 = filter(df_snr_sexo_idade_USO_DE_DROGAS_rmark, !IDADE == "s/inf")
df_snr_sexo_idade_USO_DE_DROGAS_rmark1 = filter(df_snr_sexo_idade_USO_DE_DROGAS_rmark, !IDADE == 18)
#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_USO_DE_DROGAS_rmark1 = df_snr_sexo_idade_USO_DE_DROGAS_rmark1 %>%
  top_n(5, IDADE) %>% arrange(desc(IDADE))

#somando
sum(df_snr_sexo_idade_USO_DE_DROGAS_rmark$QUANTIDADE)

#para escolher linhas e posicoes
df_snr_sexo_idade_USO_DE_DROGAS_rmark1[5,1]
df_snr_sexo_idade_USO_DE_DROGAS_rmark1[1,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################




df_snr_sexo_idade_USO_DE_DROGAS$IDADE <- paste(df_snr_sexo_idade_USO_DE_DROGAS$IDADE, "anos", sep=" ")
df_snr_sexo_idade_USO_DE_DROGAS$IDADE[df_snr_sexo_idade_USO_DE_DROGAS$IDADE == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade_USO_DE_DROGAS


setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#########################################################################################################
# GRAFICO SEXO USO_DE_DROGAS PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_sexo_USO_DE_DROGAS = df_snr_banco_USO_DE_DROGAS_bkp %>%
  select(SEXO)

table(df_sexo_USO_DE_DROGAS$SEXO)

#########################################################################################################
#########################################################################################################

df_sexo_USO_DE_DROGAS$SEXO <- as.character(df_sexo_USO_DE_DROGAS$SEXO)

df_sexo_USO_DE_DROGAS$SEXO[df_sexo_USO_DE_DROGAS$SEXO == ""]<- "M"
table(df_sexo_USO_DE_DROGAS$SEXO)

df_sexo_USO_DE_DROGAS = data.frame(table(df_sexo_USO_DE_DROGAS$SEXO))

colnames(df_sexo_USO_DE_DROGAS) <- c("SEXO", "QUANTIDADE")

sum(df_sexo_USO_DE_DROGAS$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_sexo_USO_DE_DROGAS$SEXO <- as.character(df_sexo_USO_DE_DROGAS$SEXO)

df_sexo_USO_DE_DROGAS$SEXO[df_sexo_USO_DE_DROGAS$SEXO == "F"]<- "FEMININO"
df_sexo_USO_DE_DROGAS$SEXO[df_sexo_USO_DE_DROGAS$SEXO == "M"]<- "MASCULINO"

#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_sexo_USO_DE_DROGAS

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_sexo_USO_DE_DROGAS <- df_sexo_USO_DE_DROGAS %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_sexo_USO_DE_DROGAS$PERCENTUAL <- paste(df_sexo_USO_DE_DROGAS$PERCENTUAL, "%", sep=" ")

df_sexo_USO_DE_DROGAS


setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRAFICO_DE_DROGAS
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
banco_TRAFICO_DE_DROGAS = banco_atos_em_foco
# ADOLESCENTE ENCAMINHADOS. Retirados do banco SEM MBA sem adolescentes duplicados
# ordenar nesta ordem para que, quando cortar nome repetidos, preservar data do último ato.
banco_TRAFICO_DE_DROGAS <-banco_TRAFICO_DE_DROGAS[order(banco_TRAFICO_DE_DROGAS$DATA_ATO, decreasing=TRUE),]#ordenar, decrescente, data do ato
banco_TRAFICO_DE_DROGAS <-banco_TRAFICO_DE_DROGAS[order(banco_TRAFICO_DE_DROGAS$NOME2, decreasing=FALSE),]#ordenar, crescente, nome2

#########################################################################################################
banco_TRAFICO_DE_DROGAS = filter(banco_TRAFICO_DE_DROGAS, ATO_INFRACIONAL == "TRÁFICO DE DROGAS")
 #|ATO_INFRACIONAL_ATA_02 == "TRÁFICO DE DROGAS" |ATO_INFRACIONAL_ATA_03 == "TRÁFICO DE DROGAS")

#########################################################################################################

#retirar nomes duplicados:snr=sem nome repetido
df_snr_banco_TRAFICO_DE_DROGAS <- banco_TRAFICO_DE_DROGAS[!duplicated(data.frame(banco_TRAFICO_DE_DROGAS$NOME2, banco_TRAFICO_DE_DROGAS$NASCIMENTO)),]

#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=TRUE)

#write.csv(df_snr_banco_TRAFICO_DE_DROGAS, file ="df_snr_banco_TRAFICO_DE_DROGAS.csv",row.names=TRUE)
#write.xlsx(df_snr_banco_TRAFICO_DE_DROGAS, file ="df_snr_banco_TRAFICO_DE_DROGAS.xlsx")
#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos s/inf ou #valor!)
df_snr_banco_TRAFICO_DE_DROGAS_bkp = df_snr_banco_TRAFICO_DE_DROGAS

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS = df_snr_banco_TRAFICO_DE_DROGAS %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO <- as.character(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO)

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO[df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO == ""]<- "M"
table(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO)

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS <- table(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE, df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO, useNA ="always")
#write.csv(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS, file ="df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS.csv",row.names=TRUE)
#write.csv(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS, file ="df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS)

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS = data.frame(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS_bkp = df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS


df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS

colnames(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE <- as.character(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE)
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO <- as.character(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$SEXO)
sum(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$QUANTIDADE)


df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS = filter(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE[df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE[which(is.na(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE))] <- "s/inf"
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS


df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE <- paste(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE, "anos", sep=" ")
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE[df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$IDADE == "s/inf anos"]<- "s/inf"

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS <- reshape(data = df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS, idvar = "IDADE", timevar = "SEXO", direction = "wide")
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS

colnames(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS) <- c("IDADE", "FEM", "MAS")
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$FEM[which(is.na(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$FEM))] <- 0
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$MAS[which(is.na(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$MAS))] <- 0

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS
#ordenar idade
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS = df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS %>% arrange(IDADE)

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$FEM <- as.numeric(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$FEM)
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$MAS <- as.numeric(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$MAS)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$FEM)*100, 2)
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$M <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$MAS)*100, 2)
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS
#########################################################################################################
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS <- df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS[c("IDADE", "FEM", "F", "MAS", "M")]
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS
#########################################################################################################

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS<- rbind(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS,
                                                  data.frame(IDADE = "TOTAL",
                                                             FEM = sum(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$FEM),
                                                             F = sum(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$F),
                                                             MAS = sum(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$MAS),
                                                             M = sum(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS$M),
                                                             stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS

colnames(df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS) <- c("IDADE", "FEM", "%", "MAS", "%")
df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS
#########################################################################################################
#########################################################################################################
#salvando tabela
#pdf(file="tabela_df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_df_snr_sexo_idade_banco_TRAFICO_DE_DROGAS_alternativa")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_TRAFICO_DE_DROGAS = df_snr_banco_TRAFICO_DE_DROGAS %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO)

df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO <- as.character(df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO)

df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO[df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO == ""]<- "M"
table(df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO)

df_snr_sexo_idade_TRAFICO_DE_DROGAS <- table(df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE, df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO, useNA ="always")
##write.csv(df_snr_sexo_idade_TRAFICO_DE_DROGAS, file ="df_snr_sexo_idade_TRAFICO_DE_DROGAS.csv",row.names=TRUE)
##write.csv(df_snr_sexo_idade_TRAFICO_DE_DROGAS, file ="df_snr_sexo_idade_TRAFICO_DE_DROGAS.csv",row.names=TRUE)
sum(df_snr_sexo_idade_TRAFICO_DE_DROGAS)

df_snr_sexo_idade_TRAFICO_DE_DROGAS = data.frame(df_snr_sexo_idade_TRAFICO_DE_DROGAS)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_TRAFICO_DE_DROGAS_bkp = df_snr_sexo_idade_TRAFICO_DE_DROGAS


df_snr_sexo_idade_TRAFICO_DE_DROGAS

colnames(df_snr_sexo_idade_TRAFICO_DE_DROGAS) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_TRAFICO_DE_DROGAS

df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE <- as.character(df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE)
df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO <- as.character(df_snr_sexo_idade_TRAFICO_DE_DROGAS$SEXO)
sum(df_snr_sexo_idade_TRAFICO_DE_DROGAS$QUANTIDADE)


df_snr_sexo_idade_TRAFICO_DE_DROGAS = filter(df_snr_sexo_idade_TRAFICO_DE_DROGAS, !QUANTIDADE == 0)
df_snr_sexo_idade_TRAFICO_DE_DROGAS

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE[df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE[which(is.na(df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE))] <- "s/inf"
df_snr_sexo_idade_TRAFICO_DE_DROGAS


#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark = df_snr_sexo_idade_TRAFICO_DE_DROGAS

#excluindo sem informacao e outros
df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark1 = filter(df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark, !IDADE == "s/inf")
df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark1 = filter(df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark, !IDADE >= 18)
#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark1 = df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark1 %>%
  top_n(5, IDADE) %>% arrange(desc(IDADE))

#somando
sum(df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark$QUANTIDADE)

#para escolher linhas e posicoes
df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark1[5,1]
df_snr_sexo_idade_TRAFICO_DE_DROGAS_rmark1[1,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################




df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE <- paste(df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE, "anos", sep=" ")
df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE[df_snr_sexo_idade_TRAFICO_DE_DROGAS$IDADE == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade_TRAFICO_DE_DROGAS

#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)


setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#########################################################################################################
# GRAFICO SEXO TRAFICO_DE_DROGAS PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_sexo_TRAFICO_DE_DROGAS = df_snr_banco_TRAFICO_DE_DROGAS_bkp %>%
  select(SEXO)

table(df_sexo_TRAFICO_DE_DROGAS$SEXO)

#########################################################################################################
#########################################################################################################

df_sexo_TRAFICO_DE_DROGAS$SEXO <- as.character(df_sexo_TRAFICO_DE_DROGAS$SEXO)

df_sexo_TRAFICO_DE_DROGAS$SEXO[df_sexo_TRAFICO_DE_DROGAS$SEXO == ""]<- "M"
table(df_sexo_TRAFICO_DE_DROGAS$SEXO)

df_sexo_TRAFICO_DE_DROGAS = data.frame(table(df_sexo_TRAFICO_DE_DROGAS$SEXO))

colnames(df_sexo_TRAFICO_DE_DROGAS) <- c("SEXO", "QUANTIDADE")

sum(df_sexo_TRAFICO_DE_DROGAS$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_sexo_TRAFICO_DE_DROGAS$SEXO <- as.character(df_sexo_TRAFICO_DE_DROGAS$SEXO)

df_sexo_TRAFICO_DE_DROGAS$SEXO[df_sexo_TRAFICO_DE_DROGAS$SEXO == "F"]<- "FEMININO"
df_sexo_TRAFICO_DE_DROGAS$SEXO[df_sexo_TRAFICO_DE_DROGAS$SEXO == "M"]<- "MASCULINO"

#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_sexo_TRAFICO_DE_DROGAS

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_sexo_TRAFICO_DE_DROGAS <- df_sexo_TRAFICO_DE_DROGAS %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_sexo_TRAFICO_DE_DROGAS$PERCENTUAL <- paste(df_sexo_TRAFICO_DE_DROGAS$PERCENTUAL, "%", sep=" ")

df_sexo_TRAFICO_DE_DROGAS


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#FIM
#########################################################################################################
