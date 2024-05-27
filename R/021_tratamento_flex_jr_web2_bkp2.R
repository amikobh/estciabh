#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR JR:
dir.create(file.path("~/diretorio_r/estciabh", "justica_restaurativa"))
setwd(file.path("~/diretorio_r/estciabh"))
#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

#########################################################################################################
banco_jr <- read.csv("banco_jr.csv",header=TRUE, sep="|", encoding = "UTF-8", skip = 2) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

names(banco_jr)

banco_jr <- as_tibble(banco_jr)

#RENOMEANDO COLUNA
colnames(banco_jr)[41]<-'DATA_DO_ENCAMINHAMENTO_DO_JUIZ'
#banco_jr <-rename( banco_jr, c( "DATA_DO_ENCAMINHAMENTO_DO_JUIZ._PELA_ATA_DE._AUDIENCIA" = "DATA_DO_ENCAMINHAMENTO_DO_JUIZ"))

names(banco_jr)

banco_jr$DATA_DO_ENCAMINHAMENTO_DO_JUIZ[banco_jr$DATA_DO_ENCAMINHAMENTO_DO_JUIZ == ""]<- NA
banco_jr$DATA_DO_ENCAMINHAMENTO_DO_JUIZ[banco_jr$DATA_DO_ENCAMINHAMENTO_DO_JUIZ == "SEM INFORMACAO"]<- NA

banco_jr$DATA_DO_ENCAMINHAMENTO_DO_JUIZ <- dmy(banco_jr$DATA_DO_ENCAMINHAMENTO_DO_JUIZ)

banco_jr$DATA_DO_RECEBIMENTO_NO_SETOR[banco_jr$DATA_DO_RECEBIMENTO_NO_SETOR == ""]<- NA
banco_jr$DATA_DO_RECEBIMENTO_NO_SETOR <- dmy(banco_jr$DATA_DO_RECEBIMENTO_NO_SETOR)

banco_jr$NASCIMENTO[banco_jr$NASCIMENTO == ""]<- NA
banco_jr$NASCIMENTO <- dmy(banco_jr$NASCIMENTO)

banco_jr$DATA_DO_FATO[banco_jr$DATA_DO_FATO == ""]<- NA
banco_jr$DATA_DO_FATO <- dmy(banco_jr$DATA_DO_FATO)


#########################################################################################################
#filtrar data
#banco_jr = banco_jr %>%
  #filter(DATA_DO_ENCAMINHAMENTO_DO_JUIZ >= (str_c(format(Sys.Date()-365*1, "%Y"), "-01-01")) &
          # DATA_DO_ENCAMINHAMENTO_DO_JUIZ <= (str_c(format(Sys.Date()-365*1, "%Y"), "-12-31")))
  #filter(DATA_DO_ENCAMINHAMENTO_DO_JUIZ >= (str_c(format(Sys.Date()-365*2, "%Y"), "-01-01")) & DATA_DO_ENCAMINHAMENTO_DO_JUIZ <= (str_c(format(Sys.Date()-365*2, "%Y"), "-12-31")))

#banco_jr = banco_jr %>%
  #filter(DATA_DO_RECEBIMENTO_NO_SETOR >= (str_c(format(Sys.Date()-365*1, "%Y"), "-01-01")) &
          # DATA_DO_RECEBIMENTO_NO_SETOR <= (str_c(format(Sys.Date()-365*1, "%Y"), "-12-31")))
#########################################################################################################

#########################################################################################################
#tabela total_casos_jr
total_casos_jr = data.frame(nrow(banco_jr))

colnames(total_casos_jr) <- c("QUANTIDADE DE CASOS ENCAMINHADOS")

#para tabela gt abaixo:
total_casos_jr_gt = total_casos_jr

#########################################################################################################
#########################################################################################################


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
## calcula o intervalo em anos
banco_jr$IDADE = as.period(interval(banco_jr$NASCIMENTO, banco_jr$DATA_DO_FATO))
# SEPARAR SO O PRIMEIRO ITEM DE "17y 2m 28d 0H 0M 0S" GERADO PELO SCRIPT ANTERIOR.
banco_jr$IDADE = banco_jr$IDADE@year

banco_jr%>%
  select(NASCIMENTO, DATA_DO_FATO, IDADE, DATA_DO_ENCAMINHAMENTO_DO_JUIZ)

banco_jr$NOME2 <- gsub(" ","", banco_jr$NOME)
#########################################################################################################
#retirar nomes duplicados:snr=sem nome repetido
#snr_banco_ESCOLA <- escola_bkp[!duplicated(data.frame(escola_bkp$NOME2, escola_bkp$NASCIMENTO)),]



# Remove duplicate rows of the dataframe using variables
snr_banco_jr = distinct(banco_jr, NOME2,NASCIMENTO, .keep_all= TRUE)


#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=FALSE)

#########################################################################################################
#########################################################################################################


#########################################################################################################

banco_JR = snr_banco_jr

#########################################################################################################
#########################################################################################################
JR_bkp = banco_JR
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
#retirar nomes duplicados:snr=sem nome repetido
#snr_banco_JR <- JR_bkp[!duplicated(data.frame(JR_bkp$NOME2, JR_bkp$NASCIMENTO)),]

# Remove duplicate rows of the dataframe using variables
snr_banco_JR = distinct(JR_bkp, NOME2,NASCIMENTO, .keep_all= TRUE)


#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=FALSE)

#########################################################################################################
#########################################################################################################



#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos s/inf ou #valor!)
JR_snr = snr_banco_JR

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_JR_bkp = JR_snr %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_JR_bkp$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_JR_bkp$SEXO <- as.character(df_snr_sexo_idade_JR_bkp$SEXO)

df_snr_sexo_idade_JR_bkp$SEXO[df_snr_sexo_idade_JR_bkp$SEXO == ""]<- "M"
table(df_snr_sexo_idade_JR_bkp$SEXO)

df_snr_sexo_idade_JR_bkp <- table(df_snr_sexo_idade_JR_bkp$IDADE, df_snr_sexo_idade_JR_bkp$SEXO, useNA ="always")
#write.csv(df_snr_sexo_idade_JR_bkp, file ="df_snr_sexo_idade_JR_bkp.csv",row.names=FALSE)
#write.csv(df_snr_sexo_idade_JR_bkp, file ="df_snr_sexo_idade_JR_bkp.csv",row.names=FALSE)
sum(df_snr_sexo_idade_JR_bkp)

df_snr_sexo_idade_JR_bkp = data.frame(df_snr_sexo_idade_JR_bkp)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_JR_bkp_bkp = df_snr_sexo_idade_JR_bkp


df_snr_sexo_idade_JR_bkp

colnames(df_snr_sexo_idade_JR_bkp) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_JR_bkp

df_snr_sexo_idade_JR_bkp$IDADE <- as.character(df_snr_sexo_idade_JR_bkp$IDADE)
df_snr_sexo_idade_JR_bkp$SEXO <- as.character(df_snr_sexo_idade_JR_bkp$SEXO)
sum(df_snr_sexo_idade_JR_bkp$QUANTIDADE)


df_snr_sexo_idade_JR_bkp = filter(df_snr_sexo_idade_JR_bkp, !QUANTIDADE == 0)
df_snr_sexo_idade_JR_bkp

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_JR_bkp$IDADE[df_snr_sexo_idade_JR_bkp$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_JR_bkp$IDADE[which(is.na(df_snr_sexo_idade_JR_bkp$IDADE))] <- "s/inf"
df_snr_sexo_idade_JR_bkp




#df_snr_sexo_idade_JR_bkp$IDADE <- paste(df_snr_sexo_idade_JR_bkp$IDADE, "anos", sep=" ")
df_snr_sexo_idade_JR_bkp$IDADE[df_snr_sexo_idade_JR_bkp$IDADE == "s/inf anos"]<- "s/inf"

df_snr_sexo_idade_JR_bkp <- reshape(data = df_snr_sexo_idade_JR_bkp, idvar = "IDADE", timevar = "SEXO", direction = "wide")
df_snr_sexo_idade_JR_bkp

colnames(df_snr_sexo_idade_JR_bkp) <- c("IDADE", "FEMININO", "MASCULINO")
df_snr_sexo_idade_JR_bkp

df_snr_sexo_idade_JR_bkp$FEMININO[which(is.na(df_snr_sexo_idade_JR_bkp$FEMININO))] <- 0
df_snr_sexo_idade_JR_bkp$MASCULINO[which(is.na(df_snr_sexo_idade_JR_bkp$MASCULINO))] <- 0

df_snr_sexo_idade_JR_bkp
#ordenar idade
df_snr_sexo_idade_JR_bkp = df_snr_sexo_idade_JR_bkp %>% arrange(IDADE)

df_snr_sexo_idade_JR_bkp$FEMININO <- as.numeric(df_snr_sexo_idade_JR_bkp$FEMININO)
df_snr_sexo_idade_JR_bkp$MASCULINO <- as.numeric(df_snr_sexo_idade_JR_bkp$MASCULINO)

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
df_snr_sexo_idade_JR_bkp$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_JR_bkp$FEMININO)*100, 2)
df_snr_sexo_idade_JR_bkp$M <- round_preserve_sum(prop.table(df_snr_sexo_idade_JR_bkp$MASCULINO)*100, 2)
df_snr_sexo_idade_JR_bkp
#########################################################################################################
colnames(df_snr_sexo_idade_JR_bkp) <- c("IDADE", "FEM", "MAS", "F", "M")
df_snr_sexo_idade_JR_bkp

#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_JR_bkp_rmark = df_snr_sexo_idade_JR_bkp

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)
df_snr_sexo_idade_JR_bkp_rmark = filter(df_snr_sexo_idade_JR_bkp_rmark, !IDADE == "s/inf")
#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_JR_bkp_rmark = df_snr_sexo_idade_JR_bkp_rmark %>%
  top_n(6, MAS) %>% arrange(desc(MAS))

#somando
#sum(df_snr_sexo_idade_JR_bkp_rmark$M)

#para escolher linhas e posicoes
#df_snr_sexo_idade_JR_bkp_rmark[2,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

#########################################################################################################

df_snr_sexo_idade_JR_bkp<- rbind(df_snr_sexo_idade_JR_bkp,
                                     data.frame(IDADE = "TOTAL",
                                                FEM = sum(df_snr_sexo_idade_JR_bkp$FEMININO),
                                                F = sum(df_snr_sexo_idade_JR_bkp$F),
                                                MAS = sum(df_snr_sexo_idade_JR_bkp$MASCULINO),
                                                M = sum(df_snr_sexo_idade_JR_bkp$M),
                                                stringsAsFactors = FALSE))

df_snr_sexo_idade_JR_bkp

df_snr_sexo_idade_JR_bkp =
df_snr_sexo_idade_JR_bkp %>%
  select(IDADE, FEM, F, MAS, M)

colnames(df_snr_sexo_idade_JR_bkp) <- c("IDADE","FEM", "%", "MAS", "%")
df_snr_sexo_idade_JR_bkp
#########################################################################################################
#########################################################################################################
#salvando tabela
#pdf(file="tabela_df_snr_sexo_idade_JR_bkp_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_df_snr_sexo_idade_JR_bkp_alternativa")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_JR = JR_snr %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade_JR$SEXO)

df_snr_sexo_idade_JR$SEXO <- as.character(df_snr_sexo_idade_JR$SEXO)

df_snr_sexo_idade_JR$SEXO[df_snr_sexo_idade_JR$SEXO == ""]<- "M"
table(df_snr_sexo_idade_JR$SEXO)

df_snr_sexo_idade_JR <- table(df_snr_sexo_idade_JR$IDADE, df_snr_sexo_idade_JR$SEXO, useNA ="always")
##write.csv(df_snr_sexo_idade_JR, file ="df_snr_sexo_idade_JR.csv",row.names=FALSE)
##write.csv(df_snr_sexo_idade_JR, file ="df_snr_sexo_idade_JR.csv",row.names=FALSE)
sum(df_snr_sexo_idade_JR)

df_snr_sexo_idade_JR = data.frame(df_snr_sexo_idade_JR)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_JR_bkp = df_snr_sexo_idade_JR


df_snr_sexo_idade_JR

colnames(df_snr_sexo_idade_JR) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade_JR

df_snr_sexo_idade_JR$IDADE <- as.character(df_snr_sexo_idade_JR$IDADE)
df_snr_sexo_idade_JR$SEXO <- as.character(df_snr_sexo_idade_JR$SEXO)
sum(df_snr_sexo_idade_JR$QUANTIDADE)


df_snr_sexo_idade_JR = filter(df_snr_sexo_idade_JR, !QUANTIDADE == 0)
df_snr_sexo_idade_JR

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_JR$IDADE[df_snr_sexo_idade_JR$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_JR$IDADE[which(is.na(df_snr_sexo_idade_JR$IDADE))] <- "s/inf"
df_snr_sexo_idade_JR


df_snr_sexo_idade_JR$IDADE <- paste(df_snr_sexo_idade_JR$IDADE, "anos", sep=" ")
df_snr_sexo_idade_JR$IDADE[df_snr_sexo_idade_JR$IDADE == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade_JR



#########################################################################################################
# GRAFICO SEXO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_JR_PIZZA = snr_banco_JR %>%
  select(SEXO)

table(df_snr_sexo_JR_PIZZA$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_JR_PIZZA$SEXO <- as.character(df_snr_sexo_JR_PIZZA$SEXO)

df_snr_sexo_JR_PIZZA$SEXO[df_snr_sexo_JR_PIZZA$SEXO == ""]<- "M"
table(df_snr_sexo_JR_PIZZA$SEXO)

df_snr_sexo_JR_PIZZA = data.frame(table(df_snr_sexo_JR_PIZZA$SEXO))

colnames(df_snr_sexo_JR_PIZZA) <- c("SEXO", "QUANTIDADE")

sum(df_snr_sexo_JR_PIZZA$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_snr_sexo_JR_PIZZA$SEXO <- as.character(df_snr_sexo_JR_PIZZA$SEXO)

df_snr_sexo_JR_PIZZA$SEXO[df_snr_sexo_JR_PIZZA$SEXO == "F"]<- "FEMININO"
df_snr_sexo_JR_PIZZA$SEXO[df_snr_sexo_JR_PIZZA$SEXO == "M"]<- "MASCULINO"

df_snr_sexo_JR_PIZZA_original=df_snr_sexo_JR_PIZZA

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
df_snr_sexo_JR_PIZZA

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_snr_sexo_JR_PIZZA <- df_snr_sexo_JR_PIZZA %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_snr_sexo_JR_PIZZA$PERCENTUAL <- paste(df_snr_sexo_JR_PIZZA$PERCENTUAL, "%", sep=" ")

df_snr_sexo_JR_PIZZA



#salvar pdf
########################################################################################################
#########################################################################################################

# TABELA_001
JR_snr_bkp = JR_snr

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp = JR_snr %>%
  select(SEXO)

table(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO <- as.character(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO)

df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO[df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO == "F"]<- "FEMININO"
df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO[df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO == "M"]<- "MASCULINO"
table(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO)

df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp <- table(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO)

setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))

#write.csv(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp, file ="df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp.csv",row.names=FALSE)

#sum(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp)

df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp = data.frame(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp)
#########################################################################################################
#########################################################################################################


df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp_bkp = df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp


df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp

colnames(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp) <- c("SEXO", "QUANTIDADE")

df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp


df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO <- as.character(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$SEXO)
sum(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$QUANTIDADE)


df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp = filter(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp, !QUANTIDADE == 0)
df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$IDADE[which(is.na(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$IDADE))] <- "s/inf"
df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp


#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$IDADE <- paste(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$IDADE, "anos", sep=" ")
#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$IDADE[df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$IDADE == "s/inf anos"]<- "s/inf"

#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp <- reshape(data = df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp, idvar = "SEXO", timevar = "QUANTIDADE", direction = "wide")
df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp

#colnames(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp) <- c("IDADE", "FEM", "MAS")
df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp

#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$FEM[which(is.na(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$FEM))] <- 0
#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$MAS[which(is.na(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$MAS))] <- 0

df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp
#ordenar idade
#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp = df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp %>% arrange(IDADE)

#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$FEM <- as.numeric(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$FEM)
#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$MAS <- as.numeric(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$MAS)

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
#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$F <- round_preserve_sum(prop.table(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$FEM)*100, 2)
#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$M <- round_preserve_sum(prop.table(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$MAS)*100, 2)
df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp
#########################################################################################################
#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp <- df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp[c("IDADE", "FEM", "F", "MAS", "M")]
df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp
#########################################################################################################

#df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp<- rbind(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp,
#                                    data.frame(IDADE = "TOTAL",
#                                              FEM = sum(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$FEM),
#                                             F = sum(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$F),
#                                            MAS = sum(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$MAS),
#                                           M = sum(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp$M),
#                                          stringsAsFactors = FALSE))

df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp

#colnames(df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp) <- c("IDADE", "FEM", "%", "MAS", "%")
df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp
#########################################################################################################
#########################################################################################################
###########################################################
#salvando tabela
#pdf(file="tabela_df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_df_snr_sexo_JR_PIZZA_idade_JR_tab_bkp_alternativa")
##setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
########################################################################################################
# RACA_COR_jr INICIO
#########################################################################################################

RACA_COR_jr <- data.frame(table(banco_jr$RACA_COR))

colnames(RACA_COR_jr) <- c("RACA_COR_jr", "QUANTIDADE")
#acrescentando coluna com percentual
RACA_COR_jr$QUANTIDADE <- as.numeric(RACA_COR_jr$QUANTIDADE)
RACA_COR_jr
RACA_COR_jr = filter(RACA_COR_jr, !QUANTIDADE == 0)
RACA_COR_jr

RACA_COR_jr$RACA_COR_jr <- as.character(RACA_COR_jr$RACA_COR_jr)

RACA_COR_jr$RACA_COR_jr[RACA_COR_jr$RACA_COR_jr == "NSA"]<- "NAO RESPONDEU"
RACA_COR_jr$RACA_COR_jr[RACA_COR_jr$RACA_COR_jr == "SEM INFORMACAO"]<- "NAO RESPONDEU"
RACA_COR_jr$RACA_COR_jr[RACA_COR_jr$RACA_COR_jr == "NAO SABE"]<- "VNAO SABE"
RACA_COR_jr$RACA_COR_jr[RACA_COR_jr$RACA_COR_jr == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
RACA_COR_jr$RACA_COR_jr[RACA_COR_jr$RACA_COR_jr == ""]<- "VNAO RESPONDEU"
RACA_COR_jr[order(RACA_COR_jr$RACA_COR_jr),]#ordenar, crescente, nome2
RACA_COR_jr


RACA_COR_jr <- ddply(RACA_COR_jr,
                     c("RACA_COR_jr"),
                     summarise,
                     QUANTIDADE = sum(QUANTIDADE)
                     #,
                     #PERCENTUAL = sum(PERCENTUAL)
                     )


#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
RACA_COR_jr$PERCENTUAL <- round_preserve_sum(prop.table(RACA_COR_jr$QUANTIDADE)*100, 2)
#write.csv(RACA_COR_jr, file ="RACA_COR_jr.csv",row.names=TRUE)
###write.xlsx(RACA_COR_jr, file = "RACA_COR_jr.xlsx")

#RACA_COR_jr$QUANTIDADE <- NULL



RACA_COR_jr


#RACA_COR_jr$RACA_COR_jr[RACA_COR_jr$RACA_COR_jr == NA]<- "SEM INFORMACAO"







RACA_COR_jr$RACA_COR_jr[RACA_COR_jr$RACA_COR_jr == "VNAO SABE"]<- "NÃO SABE"
RACA_COR_jr$RACA_COR_jr[RACA_COR_jr$RACA_COR_jr == "VNAO RESPONDEU"]<- "NÃO RESPONDEU"


RACA_COR_jr




#salvando para utilizacao graficos
RACA_COR_jr_bkp = RACA_COR_jr

#########################################################################################################
#########################################################################################################

#script para o bookdown

RACA_COR_jr_rmark = RACA_COR_jr

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
RACA_COR_jr_rmark = RACA_COR_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(RACA_COR_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
RACA_COR_jr_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
RACA_COR_jr <- rbind(RACA_COR_jr,
                     data.frame(RACA_COR_jr = "TOTAL",
                                QUANTIDADE = sum(RACA_COR_jr$QUANTIDADE),
                                PERCENTUAL = sum(RACA_COR_jr$PERCENTUAL),
                                stringsAsFactors = FALSE))

RACA_COR_jr
colnames(RACA_COR_jr) <- c("RAÇA/COR", "QUANTIDADE", "%")
RACA_COR_jr

#para tabela gt abaixo:
RACA_COR_jr_gt = RACA_COR_jr

#colnames(RACA_COR_jr) <- c("RACA_COR_jr", "%")
#RACA_COR_jr <- RACA_COR_jr[c("RACA_COR_jr", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO RACA_COR_jr
#RACA_COR_jr_original=RACA_COR_jr #salvando RACA_COR_jrs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
RACA_COR_jr=RACA_COR_jr_bkp
#########################################################################################################
#########################################################################################################
#GRAFICO RACA_COR_jr
#RACA_COR_jr_original=RACA_COR_jr #salvando RACA_COR_jrs atendimento original
RACA_COR_jr=RACA_COR_jr_bkp


RACA_COR_jr<-RACA_COR_jr%>%
  arrange(PERCENTUAL)

RACA_COR_jr




setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
# RACA_COR_jr FIM
#########################################################################################################

#############################################################################################################
#ESCOLARIDADE_jr
#########################################################################################################

ESCOLARIDADE_jr =
  banco_jr |>
  select(SERIE_ATUAL_OU_ULTIMA_CURSADA)

#adaptando para o restante dos scripts
colnames(ESCOLARIDADE_jr)[1]<-'ESCOLARIDADE_jr'

ESCOLARIDADE_jr$ESCOLARIDADE_jr = ajustar_nomes(ESCOLARIDADE_jr$ESCOLARIDADE_jr)
#########################################################################################################
#AJUSTA OS FORA DE PADRÃO AQUI:
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "EJA"]<- "EJAENSFUND" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == ""]<- "SEMINFORMACAO" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "NSA"]<- "NAORESPONDEU" #FIZ OPÇÃO PELO FUND
#ORDENANDO

ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "1ªSERIE-ENSFUND"]<- "A1ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "2ªSERIE-ENSFUND"]<- "B2ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "3ªSERIE-ENSFUND"]<- "C3ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "4ªSERIE-ENSFUND"]<- "D4ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "5ªSERIE-ENSFUND"]<- "E5ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "6ªSERIE-ENSFUND"]<- "F6ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "7ªSERIE-ENSFUND"]<- "G7ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "8ªSERIE-ENSFUND"]<- "H8ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "9ªSERIE-ENSFUND"]<- "I9ªSERIE-ENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "1ºANO-ENSMEDIO"]<- "J1ºANO-ENSMEDIO"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "2ºANO-ENSMEDIO"]<- "K2ºANO-ENSMEDIO"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "3ºANO-ENSMEDIO"]<- "L3ºANO-ENSMEDIO"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "FACULDADE1ºPERIODO"]<- "LAFACULDADE1ºPERIODO"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "EJAENSFUND"]<- "MEJAENSFUND"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "EJAENSMEDIO"]<- "NEJAENSMEDIO"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "NAOSABE"]<- "ONAOSABE"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "NAORESPONDEU"]<- "PNAORESPONDEU"
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "SEMINFORMACAO"]<- "QSEMINFORMACAO"

#########################################################################################################
# salvando para gráfico
ESCOLARIDADE_jr_bkp = ESCOLARIDADE_jr

ESCOLARIDADE_jr_bkp =
  ESCOLARIDADE_jr_bkp %>%
  janitor::tabyl(ESCOLARIDADE_jr) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr[ESCOLARIDADE_jr_bkp$ESCOLARIDADE_jr == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"


#########################################################################################################
#replace "%" with "" in the percentual column
ESCOLARIDADE_jr_bkp$PERCENTUAL2 <- str_replace (ESCOLARIDADE_jr_bkp$percent, "%", "")
ESCOLARIDADE_jr_bkp$PERCENTUAL2 = as.numeric(ESCOLARIDADE_jr_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(ESCOLARIDADE_jr_bkp)[1]<-'ESCOLARIDADE_jr_bkp'
colnames(ESCOLARIDADE_jr_bkp)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_jr_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESCOLARIDADE_jr_bkp_rmark = ESCOLARIDADE_jr_bkp

ESCOLARIDADE_jr_bkp_rmark = ESCOLARIDADE_jr_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
ESCOLARIDADE_jr_bkp_rmark =
  ESCOLARIDADE_jr_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "VNÃO SABE"]<- "UNÃO SABE"
#ESCOLARIDADE_jr$ESCOLARIDADE_jr[ESCOLARIDADE_jr$ESCOLARIDADE_jr == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
ESCOLARIDADE_jr_TABELA =
  ESCOLARIDADE_jr %>%
  janitor::tabyl(ESCOLARIDADE_jr) %>%
  arrange(ESCOLARIDADE_jr) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#ordenando:

ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr[ESCOLARIDADE_jr_TABELA$ESCOLARIDADE_jr == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(ESCOLARIDADE_jr_TABELA)[1]<-'ESCOLARIDADE'
colnames(ESCOLARIDADE_jr_TABELA)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_jr_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#ESCOLARIDADE_jr FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# NATUREZA_ESCOLA_jr INICIO
# OBSERVAR SCRIPT LINHA 7
#########################################################################################################
#É PRECISO TRABALHAR SÓ COM O SIM DA VARIÁVEL ANTERIOR:
#NATUREZA_ESCOLA_jr = filter(banco_jr, TRABALHA_ATUALMENTE == "SIM")

NATUREZA_ESCOLA_jr <- data.frame(table(banco_jr$NATUREZA_ESCOLA))

colnames(NATUREZA_ESCOLA_jr) <- c("NATUREZA_ESCOLA_jr", "QUANTIDADE")
#acrescentando coluna com percentual
NATUREZA_ESCOLA_jr$QUANTIDADE <- as.numeric(NATUREZA_ESCOLA_jr$QUANTIDADE)
NATUREZA_ESCOLA_jr
NATUREZA_ESCOLA_jr = filter(NATUREZA_ESCOLA_jr, !QUANTIDADE == 0)



NATUREZA_ESCOLA_jr

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
NATUREZA_ESCOLA_jr$PERCENTUAL <- round_preserve_sum(prop.table(NATUREZA_ESCOLA_jr$QUANTIDADE)*100, 2)
#write.csv(NATUREZA_ESCOLA_jr, file ="NATUREZA_ESCOLA_jr.csv",row.names=TRUE)
###write.xlsx(NATUREZA_ESCOLA_jr, file = "NATUREZA_ESCOLA_jr.xlsx")

#NATUREZA_ESCOLA_jr$QUANTIDADE <- NULL
NATUREZA_ESCOLA_jr
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr <- as.character(NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr)

NATUREZA_ESCOLA_jr
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == "NSA"]<- "NAO RESPONDEU"
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == "SEM INFORMACAO"]<- "NAO RESPONDEU"
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == "NAO SABE"]<- "VNAO SABE"
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == ""]<- "VNAO RESPONDEU"
NATUREZA_ESCOLA_jr[order(NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr),]#ordenar, crescente, nome2
NATUREZA_ESCOLA_jr

#NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == NA]<- "SEM INFORMACAO"


NATUREZA_ESCOLA_jr <- ddply(NATUREZA_ESCOLA_jr,
                            c("NATUREZA_ESCOLA_jr"),
                            summarise,
                            QUANTIDADE = sum(QUANTIDADE),
                            PERCENTUAL = sum(PERCENTUAL))




NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == "VNAO SABE"]<- "NAO SABE"
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == "VNAO RESPONDEU"]<- "NAO RESPONDEU"
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == "PUBLICA"]<- "PÚBLICA"
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr[NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr == "NAO RESPONDEU"]<- "NÃO RESPONDEU"

NATUREZA_ESCOLA_jr




#salvando para utilizacao graficos
NATUREZA_ESCOLA_jr_bkp = NATUREZA_ESCOLA_jr

#########################################################################################################
#########################################################################################################

#script para o bookdown

NATUREZA_ESCOLA_jr_rmark = NATUREZA_ESCOLA_jr

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
NATUREZA_ESCOLA_jr_rmark = NATUREZA_ESCOLA_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(NATUREZA_ESCOLA_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
NATUREZA_ESCOLA_jr_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
NATUREZA_ESCOLA_jr <- rbind(NATUREZA_ESCOLA_jr,
                            data.frame(NATUREZA_ESCOLA_jr = "TOTAL",
                                       QUANTIDADE = sum(NATUREZA_ESCOLA_jr$QUANTIDADE),
                                       PERCENTUAL = sum(NATUREZA_ESCOLA_jr$PERCENTUAL),
                                       stringsAsFactors = FALSE))

NATUREZA_ESCOLA_jr
colnames(NATUREZA_ESCOLA_jr) <- c("NATUREZA", "QUANTIDADE", "%")
NATUREZA_ESCOLA_jr

#para tabela gt abaixo:
NATUREZA_ESCOLA_jr_gt = NATUREZA_ESCOLA_jr

#colnames(NATUREZA_ESCOLA_jr) <- c("NATUREZA_ESCOLA_jr", "%")
#NATUREZA_ESCOLA_jr <- NATUREZA_ESCOLA_jr[c("NATUREZA_ESCOLA_jr", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO NATUREZA_ESCOLA_jr
#NATUREZA_ESCOLA_jr_original=NATUREZA_ESCOLA_jr #salvando NATUREZA_ESCOLA_jrs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
NATUREZA_ESCOLA_jr=NATUREZA_ESCOLA_jr_bkp
#########################################################################################################
#########################################################################################################
#GRAFICO NATUREZA_ESCOLA_jr
#NATUREZA_ESCOLA_jr_original=NATUREZA_ESCOLA_jr #salvando NATUREZA_ESCOLA_jrs atendimento original
NATUREZA_ESCOLA_jr=NATUREZA_ESCOLA_jr_bkp


NATUREZA_ESCOLA_jr<-NATUREZA_ESCOLA_jr%>%
  arrange(PERCENTUAL)

NATUREZA_ESCOLA_jr




setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
# NATUREZA_ESCOLA_jr FIM
#########################################################################################################
#########################################################################################################
########################################################################################################
# TRABALHA_ATUALMENTE_jr INICIO
#########################################################################################################

TRABALHA_ATUALMENTE_jr <- data.frame(table(banco_jr$TRABALHA_ATUALMENTE))

colnames(TRABALHA_ATUALMENTE_jr) <- c("TRABALHA_ATUALMENTE_jr", "QUANTIDADE")
#acrescentando coluna com percentual
TRABALHA_ATUALMENTE_jr$QUANTIDADE <- as.numeric(TRABALHA_ATUALMENTE_jr$QUANTIDADE)
TRABALHA_ATUALMENTE_jr
TRABALHA_ATUALMENTE_jr = filter(TRABALHA_ATUALMENTE_jr, !QUANTIDADE == 0)
TRABALHA_ATUALMENTE_jr

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
TRABALHA_ATUALMENTE_jr$PERCENTUAL <- round_preserve_sum(prop.table(TRABALHA_ATUALMENTE_jr$QUANTIDADE)*100, 2)
#write.csv(TRABALHA_ATUALMENTE_jr, file ="TRABALHA_ATUALMENTE_jr.csv",row.names=TRUE)
###write.xlsx(TRABALHA_ATUALMENTE_jr, file = "TRABALHA_ATUALMENTE_jr.xlsx")

#TRABALHA_ATUALMENTE_jr$QUANTIDADE <- NULL
TRABALHA_ATUALMENTE_jr
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr <- as.character(TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr)

TRABALHA_ATUALMENTE_jr
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == "NSA"]<- "NAO RESPONDEU"
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == "SEM INFORMACAO"]<- "NAO RESPONDEU"
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == "NAO SABE"]<- "VNAO SABE"
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == ""]<- "VNAO RESPONDEU"
TRABALHA_ATUALMENTE_jr[order(TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr),]#ordenar, crescente, nome2
TRABALHA_ATUALMENTE_jr

#TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == NA]<- "SEM INFORMACAO"


TRABALHA_ATUALMENTE_jr <- ddply(TRABALHA_ATUALMENTE_jr,
                                c("TRABALHA_ATUALMENTE_jr"),
                                summarise,
                                QUANTIDADE = sum(QUANTIDADE),
                                PERCENTUAL = sum(PERCENTUAL))




TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == "VNAO SABE"]<- "NAO SABE"
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == "VNAO RESPONDEU"]<- "NAO RESPONDEU"
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == "NAO"]<- "NÃO"
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr[TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr == "NAO RESPONDEU"]<- "NÃO RESPONDEU"

TRABALHA_ATUALMENTE_jr




#salvando para utilizacao graficos
TRABALHA_ATUALMENTE_jr_bkp = TRABALHA_ATUALMENTE_jr

#########################################################################################################
#########################################################################################################

#script para o bookdown

TRABALHA_ATUALMENTE_jr_rmark = TRABALHA_ATUALMENTE_jr

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
TRABALHA_ATUALMENTE_jr_rmark = TRABALHA_ATUALMENTE_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(TRABALHA_ATUALMENTE_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
TRABALHA_ATUALMENTE_jr_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
TRABALHA_ATUALMENTE_jr <- rbind(TRABALHA_ATUALMENTE_jr,
                                data.frame(TRABALHA_ATUALMENTE_jr = "TOTAL",
                                           QUANTIDADE = sum(TRABALHA_ATUALMENTE_jr$QUANTIDADE),
                                           PERCENTUAL = sum(TRABALHA_ATUALMENTE_jr$PERCENTUAL),
                                           stringsAsFactors = FALSE))

TRABALHA_ATUALMENTE_jr
colnames(TRABALHA_ATUALMENTE_jr) <- c("TRABALHO", "QUANTIDADE", "%")
TRABALHA_ATUALMENTE_jr

#para tabela gt abaixo:
TRABALHA_ATUALMENTE_jr_gt = TRABALHA_ATUALMENTE_jr

#colnames(TRABALHA_ATUALMENTE_jr) <- c("TRABALHA_ATUALMENTE_jr", "%")
#TRABALHA_ATUALMENTE_jr <- TRABALHA_ATUALMENTE_jr[c("TRABALHA_ATUALMENTE_jr", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO TRABALHA_ATUALMENTE_jr
#TRABALHA_ATUALMENTE_jr_original=TRABALHA_ATUALMENTE_jr #salvando TRABALHA_ATUALMENTE_jrs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
TRABALHA_ATUALMENTE_jr=TRABALHA_ATUALMENTE_jr_bkp
#########################################################################################################
#########################################################################################################
#GRAFICO TRABALHA_ATUALMENTE_jr
#TRABALHA_ATUALMENTE_jr_original=TRABALHA_ATUALMENTE_jr #salvando TRABALHA_ATUALMENTE_jrs atendimento original
TRABALHA_ATUALMENTE_jr=TRABALHA_ATUALMENTE_jr_bkp


TRABALHA_ATUALMENTE_jr<-TRABALHA_ATUALMENTE_jr%>%
  arrange(PERCENTUAL)

TRABALHA_ATUALMENTE_jr




setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
# TRABALHA_ATUALMENTE_jr FIM
#########################################################################################################
#########################################################################################################
########################################################################################################
# NATUREZA_DO_TRABALHO_jr INICIO
# OBSERVAR SCRIPT LINHA 7
#########################################################################################################
#É PRECISO TRABALHAR SÓ COM O SIM DA VARIÁVEL ANTERIOR:
NATUREZA_DO_TRABALHO_jr = filter(banco_jr, TRABALHA_ATUALMENTE == "SIM")

NATUREZA_DO_TRABALHO_jr <- data.frame(table(NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO))

colnames(NATUREZA_DO_TRABALHO_jr) <- c("NATUREZA_DO_TRABALHO_jr", "QUANTIDADE")
#acrescentando coluna com percentual
NATUREZA_DO_TRABALHO_jr$QUANTIDADE <- as.numeric(NATUREZA_DO_TRABALHO_jr$QUANTIDADE)
NATUREZA_DO_TRABALHO_jr
NATUREZA_DO_TRABALHO_jr = filter(NATUREZA_DO_TRABALHO_jr, !QUANTIDADE == 0)



NATUREZA_DO_TRABALHO_jr

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
NATUREZA_DO_TRABALHO_jr$PERCENTUAL <- round_preserve_sum(prop.table(NATUREZA_DO_TRABALHO_jr$QUANTIDADE)*100, 2)
#write.csv(NATUREZA_DO_TRABALHO_jr, file ="NATUREZA_DO_TRABALHO_jr.csv",row.names=TRUE)
###write.xlsx(NATUREZA_DO_TRABALHO_jr, file = "NATUREZA_DO_TRABALHO_jr.xlsx")

#NATUREZA_DO_TRABALHO_jr$QUANTIDADE <- NULL
NATUREZA_DO_TRABALHO_jr
NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr <- as.character(NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr)

NATUREZA_DO_TRABALHO_jr
NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr[NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr == "SEM INFORMACAO"]<- "NAO RESPONDEU"
NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr[NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr == "NAO SABE"]<- "VNAO SABE"
NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr[NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr[NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr == ""]<- "VNAO RESPONDEU"
NATUREZA_DO_TRABALHO_jr[order(NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr),]#ordenar, crescente, nome2
NATUREZA_DO_TRABALHO_jr

#NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr[NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr == NA]<- "SEM INFORMACAO"


NATUREZA_DO_TRABALHO_jr <- ddply(NATUREZA_DO_TRABALHO_jr,
                                 c("NATUREZA_DO_TRABALHO_jr"),
                                 summarise,
                                 QUANTIDADE = sum(QUANTIDADE),
                                 PERCENTUAL = sum(PERCENTUAL))




NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr[NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr == "VNAO SABE"]<- "NAO SABE"
NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr[NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr == "VNAO RESPONDEU"]<- "NAO RESPONDEU"
NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr[NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr == "NAO RESPONDEU"]<- "NÃO RESPONDEU"

NATUREZA_DO_TRABALHO_jr




#salvando para utilizacao graficos
NATUREZA_DO_TRABALHO_jr_bkp = NATUREZA_DO_TRABALHO_jr

#########################################################################################################
#########################################################################################################

#script para o bookdown

NATUREZA_DO_TRABALHO_jr_rmark = NATUREZA_DO_TRABALHO_jr

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
NATUREZA_DO_TRABALHO_jr_rmark = NATUREZA_DO_TRABALHO_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
#sum(NATUREZA_DO_TRABALHO_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
NATUREZA_DO_TRABALHO_jr_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
NATUREZA_DO_TRABALHO_jr <- rbind(NATUREZA_DO_TRABALHO_jr,
                                 data.frame(NATUREZA_DO_TRABALHO_jr = "TOTAL",
                                            QUANTIDADE = sum(NATUREZA_DO_TRABALHO_jr$QUANTIDADE),
                                            PERCENTUAL = sum(NATUREZA_DO_TRABALHO_jr$PERCENTUAL),
                                            stringsAsFactors = FALSE))

NATUREZA_DO_TRABALHO_jr
colnames(NATUREZA_DO_TRABALHO_jr) <- c("NATUREZA", "QUANTIDADE", "%")
NATUREZA_DO_TRABALHO_jr

#para tabela gt abaixo:
NATUREZA_DO_TRABALHO_jr_gt = NATUREZA_DO_TRABALHO_jr

#colnames(NATUREZA_DO_TRABALHO_jr) <- c("NATUREZA_DO_TRABALHO_jr", "%")
#NATUREZA_DO_TRABALHO_jr <- NATUREZA_DO_TRABALHO_jr[c("NATUREZA_DO_TRABALHO_jr", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO NATUREZA_DO_TRABALHO_jr
#NATUREZA_DO_TRABALHO_jr_original=NATUREZA_DO_TRABALHO_jr #salvando NATUREZA_DO_TRABALHO_jrs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
NATUREZA_DO_TRABALHO_jr=NATUREZA_DO_TRABALHO_jr_bkp
#########################################################################################################
#########################################################################################################
#GRAFICO NATUREZA_DO_TRABALHO_jr
#NATUREZA_DO_TRABALHO_jr_original=NATUREZA_DO_TRABALHO_jr #salvando NATUREZA_DO_TRABALHO_jrs atendimento original
NATUREZA_DO_TRABALHO_jr=NATUREZA_DO_TRABALHO_jr_bkp


NATUREZA_DO_TRABALHO_jr<-NATUREZA_DO_TRABALHO_jr%>%
  arrange(PERCENTUAL)

NATUREZA_DO_TRABALHO_jr




setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
# NATUREZA_DO_TRABALHO_jr FIM
#########################################################################################################
#########################################################################################################
########################################################################################################
# RENDA_MENSAL_jr INICIO
# OBSERVAR SCRIPT LINHA 7
#########################################################################################################
#É PRECISO TRABALHAR SÓ COM O SIM DA VARIÁVEL ANTERIOR:
RENDA_MENSAL_jr = filter(banco_jr, TRABALHA_ATUALMENTE == "SIM")

RENDA_MENSAL_jr <- data.frame(table(RENDA_MENSAL_jr$RENDA_MENSAL))

colnames(RENDA_MENSAL_jr) <- c("RENDA_MENSAL_jr", "QUANTIDADE")
#acrescentando coluna com percentual
RENDA_MENSAL_jr$QUANTIDADE <- as.numeric(RENDA_MENSAL_jr$QUANTIDADE)
RENDA_MENSAL_jr
RENDA_MENSAL_jr = filter(RENDA_MENSAL_jr, !QUANTIDADE == 0)



RENDA_MENSAL_jr

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
RENDA_MENSAL_jr$PERCENTUAL <- round_preserve_sum(prop.table(RENDA_MENSAL_jr$QUANTIDADE)*100, 2)
#write.csv(RENDA_MENSAL_jr, file ="RENDA_MENSAL_jr.csv",row.names=TRUE)
###write.xlsx(RENDA_MENSAL_jr, file = "RENDA_MENSAL_jr.xlsx")

#RENDA_MENSAL_jr$QUANTIDADE <- NULL
RENDA_MENSAL_jr
RENDA_MENSAL_jr$RENDA_MENSAL_jr <- as.character(RENDA_MENSAL_jr$RENDA_MENSAL_jr)

RENDA_MENSAL_jr
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "NSA"]<- "NAO RESPONDEU"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "SEM INFORMACAO"]<- "NAO RESPONDEU"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "NAO SABE"]<- "VNAO SABE"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == ""]<- "VNAO RESPONDEU"
RENDA_MENSAL_jr[order(RENDA_MENSAL_jr$RENDA_MENSAL_jr),]#ordenar, crescente, nome2
RENDA_MENSAL_jr

#RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == NA]<- "SEM INFORMACAO"


RENDA_MENSAL_jr <- ddply(RENDA_MENSAL_jr,
                         c("RENDA_MENSAL_jr"),
                         summarise,
                         QUANTIDADE = sum(QUANTIDADE),
                         PERCENTUAL = sum(PERCENTUAL))




RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "VNAO SABE"]<- "NAO SABE"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "VNAO RESPONDEU"]<- "NAO RESPONDEU"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "VNÃO SABE"]<- "NÃO SABE"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "ATE 1 SALARIO MININO "]<- "ATÉ 1 SALÁRIO MÍNIMO"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "DE 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "DE 2 A 3 SALARIOS MININOS"]<- "DE 2 A 3 SALÁRIOS MÍNIMOS"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "EACIMA DE 4 SALARIOS MINIMOS"]<- "MAIS DE 4 SALÁRIOS MÍNIMOS"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "NAO SABE"]<- "NÃO SABE"
RENDA_MENSAL_jr$RENDA_MENSAL_jr[RENDA_MENSAL_jr$RENDA_MENSAL_jr == "NAO RESPONDEU"]<- "NÃO RESPONDEU"

RENDA_MENSAL_jr




#salvando para utilizacao graficos
RENDA_MENSAL_jr_bkp = RENDA_MENSAL_jr

#########################################################################################################
#########################################################################################################

#script para o bookdown

RENDA_MENSAL_jr_rmark = RENDA_MENSAL_jr

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
RENDA_MENSAL_jr_rmark = RENDA_MENSAL_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(RENDA_MENSAL_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
RENDA_MENSAL_jr_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
RENDA_MENSAL_jr <- rbind(RENDA_MENSAL_jr,
                         data.frame(RENDA_MENSAL_jr = "TOTAL",
                                    QUANTIDADE = sum(RENDA_MENSAL_jr$QUANTIDADE),
                                    PERCENTUAL = sum(RENDA_MENSAL_jr$PERCENTUAL),
                                    stringsAsFactors = FALSE))

RENDA_MENSAL_jr
colnames(RENDA_MENSAL_jr) <- c("RENDA", "QUANTIDADE", "%")
RENDA_MENSAL_jr

#para tabela gt abaixo:
RENDA_MENSAL_jr_gt = RENDA_MENSAL_jr

#colnames(RENDA_MENSAL_jr) <- c("RENDA_MENSAL_jr", "%")
#RENDA_MENSAL_jr <- RENDA_MENSAL_jr[c("RENDA_MENSAL_jr", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO RENDA_MENSAL_jr
#RENDA_MENSAL_jr_original=RENDA_MENSAL_jr #salvando RENDA_MENSAL_jrs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
RENDA_MENSAL_jr=RENDA_MENSAL_jr_bkp
#########################################################################################################
#########################################################################################################
#GRAFICO RENDA_MENSAL_jr
#RENDA_MENSAL_jr_original=RENDA_MENSAL_jr #salvando RENDA_MENSAL_jrs atendimento original
RENDA_MENSAL_jr=RENDA_MENSAL_jr_bkp


RENDA_MENSAL_jr<-RENDA_MENSAL_jr%>%
  arrange(PERCENTUAL)

RENDA_MENSAL_jr



setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
# RENDA_MENSAL_jr FIM
#########################################################################################################
########################################################################################################
# ESTADO_CIVIL_jr INICIO
#########################################################################################################

ESTADO_CIVIL_jr <- data.frame(table(banco_jr$ESTADO_CIVIL))

colnames(ESTADO_CIVIL_jr) <- c("ESTADO_CIVIL_jr", "QUANTIDADE")
#acrescentando coluna com percentual
ESTADO_CIVIL_jr$QUANTIDADE <- as.numeric(ESTADO_CIVIL_jr$QUANTIDADE)
ESTADO_CIVIL_jr
ESTADO_CIVIL_jr = filter(ESTADO_CIVIL_jr, !QUANTIDADE == 0)
ESTADO_CIVIL_jr

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
ESTADO_CIVIL_jr$PERCENTUAL <- round_preserve_sum(prop.table(ESTADO_CIVIL_jr$QUANTIDADE)*100, 2)
#write.csv(ESTADO_CIVIL_jr, file ="ESTADO_CIVIL_jr.csv",row.names=TRUE)
###write.xlsx(ESTADO_CIVIL_jr, file = "ESTADO_CIVIL_jr.xlsx")

#ESTADO_CIVIL_jr$QUANTIDADE <- NULL
ESTADO_CIVIL_jr
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr <- as.character(ESTADO_CIVIL_jr$ESTADO_CIVIL_jr)

ESTADO_CIVIL_jr
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == "NSA"]<- "NAO RESPONDEU"
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == "SEM INFORMACAO"]<- "NAO RESPONDEU"
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == "NAO SABE"]<- "VNAO SABE"
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == ""]<- "VNAO RESPONDEU"
ESTADO_CIVIL_jr[order(ESTADO_CIVIL_jr$ESTADO_CIVIL_jr),]#ordenar, crescente, nome2
ESTADO_CIVIL_jr

#ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == NA]<- "SEM INFORMACAO"


ESTADO_CIVIL_jr <- ddply(ESTADO_CIVIL_jr,
                         c("ESTADO_CIVIL_jr"),
                         summarise,
                         QUANTIDADE = sum(QUANTIDADE),
                         PERCENTUAL = sum(PERCENTUAL))




ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == "VNAO SABE"]<- "NAO SABE"
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == "VNAO RESPONDEU"]<- "NAO RESPONDEU"
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr[ESTADO_CIVIL_jr$ESTADO_CIVIL_jr == "NAO RESPONDEU"]<- "NÃO RESPONDEU"

ESTADO_CIVIL_jr




#salvando para utilizacao graficos
ESTADO_CIVIL_jr_bkp = ESTADO_CIVIL_jr

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESTADO_CIVIL_jr_rmark = ESTADO_CIVIL_jr

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
ESTADO_CIVIL_jr_rmark = ESTADO_CIVIL_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(ESTADO_CIVIL_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
ESTADO_CIVIL_jr_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
ESTADO_CIVIL_jr <- rbind(ESTADO_CIVIL_jr,
                         data.frame(ESTADO_CIVIL_jr = "TOTAL",
                                    QUANTIDADE = sum(ESTADO_CIVIL_jr$QUANTIDADE),
                                    PERCENTUAL = sum(ESTADO_CIVIL_jr$PERCENTUAL),
                                    stringsAsFactors = FALSE))

ESTADO_CIVIL_jr
colnames(ESTADO_CIVIL_jr) <- c("ESTADO CIVIL", "QUANTIDADE", "%")
ESTADO_CIVIL_jr

#para tabela gt abaixo:
ESTADO_CIVIL_jr_gt = ESTADO_CIVIL_jr

#colnames(ESTADO_CIVIL_jr) <- c("ESTADO_CIVIL_jr", "%")
#ESTADO_CIVIL_jr <- ESTADO_CIVIL_jr[c("ESTADO_CIVIL_jr", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO ESTADO_CIVIL_jr
#ESTADO_CIVIL_jr_original=ESTADO_CIVIL_jr #salvando ESTADO_CIVIL_jrs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
ESTADO_CIVIL_jr=ESTADO_CIVIL_jr_bkp
#########################################################################################################
#########################################################################################################
#GRAFICO ESTADO_CIVIL_jr
#ESTADO_CIVIL_jr_original=ESTADO_CIVIL_jr #salvando ESTADO_CIVIL_jrs atendimento original
ESTADO_CIVIL_jr=ESTADO_CIVIL_jr_bkp


ESTADO_CIVIL_jr<-ESTADO_CIVIL_jr%>%
  arrange(PERCENTUAL)

ESTADO_CIVIL_jr




setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
# ESTADO_CIVIL_jr FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# ESTADO_CIVIL_PAIS INICIO
# OBSERVAR SCRIPT LINHA 7
#########################################################################################################
#É PRECISO TRABALHAR SÓ COM O SIM DA VARIÁVEL ANTERIOR:
#ESTADO_CIVIL_PAIS = filter(banco_jr, TRABALHA_ATUALMENTE == "SIM")

ESTADO_CIVIL_PAIS <- data.frame(table(banco_jr$ESTADO_CIVIL_PAIS))

colnames(ESTADO_CIVIL_PAIS) <- c("ESTADO_CIVIL_PAIS", "QUANTIDADE")
#acrescentando coluna com percentual
ESTADO_CIVIL_PAIS$QUANTIDADE <- as.numeric(ESTADO_CIVIL_PAIS$QUANTIDADE)
ESTADO_CIVIL_PAIS
ESTADO_CIVIL_PAIS = filter(ESTADO_CIVIL_PAIS, !QUANTIDADE == 0)



ESTADO_CIVIL_PAIS

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
ESTADO_CIVIL_PAIS$PERCENTUAL <- round_preserve_sum(prop.table(ESTADO_CIVIL_PAIS$QUANTIDADE)*100, 2)
#write.csv(ESTADO_CIVIL_PAIS, file ="ESTADO_CIVIL_PAIS.csv",row.names=TRUE)
###write.xlsx(ESTADO_CIVIL_PAIS, file = "ESTADO_CIVIL_PAIS.xlsx")

#ESTADO_CIVIL_PAIS$QUANTIDADE <- NULL
ESTADO_CIVIL_PAIS
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS <- as.character(ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS)

ESTADO_CIVIL_PAIS
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "PAI FALECIDO"]<- "PAIS FALECIDOS"
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "NSA"]<- "NAO RESPONDEU"
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "SEM INFORMACAO"]<- "NAO RESPONDEU"
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "NAO SABE"]<- "VNAO SABE"
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == ""]<- "VNAO RESPONDEU"
ESTADO_CIVIL_PAIS[order(ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS),]#ordenar, crescente, nome2
ESTADO_CIVIL_PAIS

#ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == NA]<- "SEM INFORMACAO"


ESTADO_CIVIL_PAIS <- ddply(ESTADO_CIVIL_PAIS,
                           c("ESTADO_CIVIL_PAIS"),
                           summarise,
                           QUANTIDADE = sum(QUANTIDADE),
                           PERCENTUAL = sum(PERCENTUAL))




ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "VNAO SABE"]<- "NAO SABE"
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "VNAO RESPONDEU"]<- "NAO RESPONDEU"
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "MAE FALECIDO"]<- "MÃE FALECIDA"
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS[ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS == "NAO RESPONDEU"]<- "NÃO RESPONDEU"

ESTADO_CIVIL_PAIS




#salvando para utilizacao graficos
ESTADO_CIVIL_PAIS_bkp = ESTADO_CIVIL_PAIS

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESTADO_CIVIL_PAIS_rmark = ESTADO_CIVIL_PAIS

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
ESTADO_CIVIL_PAIS_rmark = ESTADO_CIVIL_PAIS_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(ESTADO_CIVIL_PAIS_rmark$QUANTIDADE)

#para escolher linhas e posicoes
ESTADO_CIVIL_PAIS_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
ESTADO_CIVIL_PAIS <- rbind(ESTADO_CIVIL_PAIS,
                           data.frame(ESTADO_CIVIL_PAIS = "TOTAL",
                                      QUANTIDADE = sum(ESTADO_CIVIL_PAIS$QUANTIDADE),
                                      PERCENTUAL = sum(ESTADO_CIVIL_PAIS$PERCENTUAL),
                                      stringsAsFactors = FALSE))

ESTADO_CIVIL_PAIS
colnames(ESTADO_CIVIL_PAIS) <- c("ESTADO CIVIL", "QUANTIDADE", "%")
ESTADO_CIVIL_PAIS

#para tabela gt abaixo:
ESTADO_CIVIL_PAIS_gt = ESTADO_CIVIL_PAIS

#colnames(ESTADO_CIVIL_PAIS) <- c("ESTADO_CIVIL_PAIS", "%")
#ESTADO_CIVIL_PAIS <- ESTADO_CIVIL_PAIS[c("ESTADO_CIVIL_PAIS", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO ESTADO_CIVIL_PAIS
#ESTADO_CIVIL_PAIS_original=ESTADO_CIVIL_PAIS #salvando ESTADO_CIVIL_PAISs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
ESTADO_CIVIL_PAIS=ESTADO_CIVIL_PAIS_bkp
#########################################################################################################
#########################################################################################################
#GRAFICO ESTADO_CIVIL_PAIS
#ESTADO_CIVIL_PAIS_original=ESTADO_CIVIL_PAIS #salvando ESTADO_CIVIL_PAISs atendimento original
ESTADO_CIVIL_PAIS=ESTADO_CIVIL_PAIS_bkp


ESTADO_CIVIL_PAIS<-ESTADO_CIVIL_PAIS%>%
  arrange(PERCENTUAL)

ESTADO_CIVIL_PAIS




setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
# ESTADO_CIVIL_PAIS FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
# DROGAS_USO_jr INICIO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#separando colunas para empilhar em uma:
#apaga colunas com os dados pessoais

#############################################################################################################
#FUNCAO TIRAR ACENTOS E ETC
#########################################################################################################
ajustar_nomes=function(x){
  x%>%
    stringr::str_trim() %>%                        #Remove espaços em branco sobrando
    stringr::str_to_upper() %>%                    #Converte todas as strings para minusculo (lower para minusculo)
    rm_accent() %>%                                #Remove os acentos com a funcao criada acima
    stringr::str_replace_all("[/' '.()]", "") %>% #Substitui os caracteres especiais por "_"
    stringr::str_replace_all("_+", "") %>%        #Substitui os caracteres especiais por ""
    stringr::str_replace("_$", "")                 #Substitui o caracter especiais por ""
}
#############################################################################################################

banco_jr$USA_DROGAS_ATUALMENTE = ajustar_nomes(banco_jr$USA_DROGAS_ATUALMENTE)

BANCO_USO_DROGAS = filter(banco_jr, USA_DROGAS_ATUALMENTE == "SIM")

#salvando para scrip do rmarkdown
BANCO_USO_DROGAS_bkp = BANCO_USO_DROGAS

BANCO_USO_DROGAS =

  BANCO_USO_DROGAS %>%
  pivot_longer(cols = starts_with("DROGAS_USO"), values_to = "DROGA_USADA") %>%
  #select(-name) %>%
  filter(DROGA_USADA != "NSA")

DROGAS_USO_jr = data.frame(table(BANCO_USO_DROGAS$DROGA_USADA))

colnames(DROGAS_USO_jr) <- c("DROGAS_USO_jr", "QUANTIDADE")
#acrescentando coluna com percentual
DROGAS_USO_jr$QUANTIDADE <- as.numeric(DROGAS_USO_jr$QUANTIDADE)
DROGAS_USO_jr
DROGAS_USO_jr = filter(DROGAS_USO_jr, !QUANTIDADE == 0)
DROGAS_USO_jr = filter(DROGAS_USO_jr, !DROGAS_USO_jr == "NSA")
DROGAS_USO_jr

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
DROGAS_USO_jr$PERCENTUAL <- round_preserve_sum(prop.table(DROGAS_USO_jr$QUANTIDADE)*100, 2)
#write.csv(DROGAS_USO_jr, file ="DROGAS_USO_jr.csv",row.names=TRUE)
###write.xlsx(DROGAS_USO_jr, file = "DROGAS_USO_jr.xlsx")

#DROGAS_USO_jr$QUANTIDADE <- NULL
DROGAS_USO_jr
DROGAS_USO_jr$DROGAS_USO_jr <- as.character(DROGAS_USO_jr$DROGAS_USO_jr)

DROGAS_USO_jr


#substituicoes
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == ""]<- "NAO RESPONDEU"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "NSA"]<- "NAO RESPONDEU"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "SOLVENTES/INALANTES(THINNER,COLA,LOLO,LANCA PERFUME)"]<- "SOLVENTES/INALANTES"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "PSICOFARMACOS( EXTASE,REMEDIOS DE TARJA PRETA,ANSIOLITICOS,ANTIDEPRESSIVOS)"]<- "PSICOFARMACOS"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "PSICOFARMACOS( REMEDIOS DE TARJA PRETA,ANSIOLITICOS,ANTIDEPRESSIVOS)"]<- "PSICOFARMACOS"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "SEM INFORMACAO"]<- "NAO RESPONDEU"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "NAO SABE"]<- "VNAO SABE"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
DROGAS_USO_jr[order(DROGAS_USO_jr$DROGAS_USO_jr),]#ordenar, crescente, nome2
DROGAS_USO_jr

#DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == NA]<- "SEM INFORMACAO"


DROGAS_USO_jr <- ddply(DROGAS_USO_jr,
                       c("DROGAS_USO_jr"),
                       summarise,
                       QUANTIDADE = sum(QUANTIDADE),
                       PERCENTUAL = sum(PERCENTUAL))



#para ordenar ao final
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "VNÃO SABE"]<- "NÃO SABE"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "ALCOOL"]<- "ÁLCOOL"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "COCAINA"]<- "COCAÍNA"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "EXTASE"]<- "ÊXTASE"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "PSICOFARMACOS"]<- "PSICOFÁRMACOS"
DROGAS_USO_jr$DROGAS_USO_jr[DROGAS_USO_jr$DROGAS_USO_jr == "NAO RESPONDEU"]<- "NÃO RESPONDEU"
DROGAS_USO_jr

#salvando para utilizacao graficos
DROGAS_USO_jr_bkp = DROGAS_USO_jr

#########################################################################################################
#########################################################################################################

#script para o bookdown

DROGAS_USO_jr_rmark = DROGAS_USO_jr

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
DROGAS_USO_jr_rmark = DROGAS_USO_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(DROGAS_USO_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
DROGAS_USO_jr_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

DROGAS_USO_jr


#salvando para utilizacao graficos
DROGAS_USO_jr_bkp = DROGAS_USO_jr

#acrescentando linha com total
DROGAS_USO_jr <- rbind(DROGAS_USO_jr,
                       data.frame(DROGAS_USO_jr = "TOTAL",
                                  QUANTIDADE = sum(DROGAS_USO_jr$QUANTIDADE),
                                  PERCENTUAL = sum(DROGAS_USO_jr$PERCENTUAL),
                                  stringsAsFactors = FALSE))

DROGAS_USO_jr
colnames(DROGAS_USO_jr) <- c("USO DE DROGAS", "QUANTIDADE", "%")
DROGAS_USO_jr

#para tabela gt abaixo:
DROGAS_USO_jr_gt = DROGAS_USO_jr


#colnames(DROGAS_USO_jr) <- c("DROGAS_USO_jr", "QUANTIDADE", "%")
#DROGAS_USO_jr <- DROGAS_USO_jr[c("DROGAS_USO_jr", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO DROGAS_USO_jr
#DROGAS_USO_jr_original=DROGAS_USO_jr #salvando DROGAS_USO_jrs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
DROGAS_USO_jr=DROGAS_USO_jr_bkp
#########################################################################################################
#########################################################################################################
#GRAFICO DROGAS_USO_jr
#DROGAS_USO_jr_original=DROGAS_USO_jr #salvando DROGAS_USO_jrs atendimento original
DROGAS_USO_jr=DROGAS_USO_jr_bkp


DROGAS_USO_jr<-DROGAS_USO_jr%>%
  arrange(PERCENTUAL)

DROGAS_USO_jr



setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# DROGAS_USO_jr FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
###MEDIDAS PROTETIVAS
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

BANCO_MEDIDAS_jr = banco_jr %>%
  select(SEXO, DATA_DO_FATO, TEM_MEDIDA_PROTETIVA_NESSE_PROCESSO,
         QUAL_MEDIDA_PROTETIVA_01, QUAL_MEDIDA_PROTETIVA_02, QUAL_MEDIDA_PROTETIVA_03,
         QUAL_MEDIDA_PROTETIVA_04, QUAL_MEDIDA_PROTETIVA_05, QUAL_MEDIDA_PROTETIVA_06)
#########################################################################################################
#write.csv(BANCO_MEDIDAS_jr, file = "BANCO_MEDIDAS_jr.csv", row.names = TRUE)
###write.xlsx(BANCO_MEDIDAS_jr, file = "BANCO_MEDIDAS_jr.xlsx")
#########################################################################################################

BANCO_MEDIDAS_jr$TEM_MEDIDA_PROTETIVA_NESSE_PROCESSO <- gsub(" ","", BANCO_MEDIDAS_jr$TEM_MEDIDA_PROTETIVA_NESSE_PROCESSO)

table(BANCO_MEDIDAS_jr$TEM_MEDIDA_PROTETIVA_NESSE_PROCESSO)

BANCO_MEDIDAS_jr_total = BANCO_MEDIDAS_jr

BANCO_MEDIDAS_jr = filter(BANCO_MEDIDAS_jr, TEM_MEDIDA_PROTETIVA_NESSE_PROCESSO == "SIM")

BANCO_MEDIDAS_jr_sim = BANCO_MEDIDAS_jr

table(BANCO_MEDIDAS_jr$TEM_MEDIDA_PROTETIVA_NESSE_PROCESSO)
#BANCO_MEDIDAS_jr<-BANCO_MEDIDAS_jr[(BANCO_MEDIDAS_jr$MEDIDA_PROTETIVA == "SIM"),]


#########################################################################################################
#convertendo de factor para character:
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01 <- as.character(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02 <- as.character(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03 <- as.character(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04 <- as.character(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05 <- as.character(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06 <- as.character(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06)
#########################################################################################################
# substituir NSA em DESCONSIDERAR AO SOMAR
table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01 == "NSA",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01)


BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01 == "",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01)

table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_01)


table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02 == "NSA",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02)

BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02 == "",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02)

table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_02)


table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03 == "NSA",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03)


BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03 == "",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03)

table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_03)


table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04 == "NSA",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04)


BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04 == "",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04)

table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_04)


table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05 == "NSA",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05)


BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05 == "",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05)

table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_05)

table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06)
BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06 == "NSA",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06)


BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06 = ifelse(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06 == "",
                                                   "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06)

table(BANCO_MEDIDAS_jr$QUAL_MEDIDA_PROTETIVA_06)






#########################################################################################################
#########################################################################################################
#########################################################################################################

#separando colunas para empilhar em uma:

BANCO_MEDIDAS_jr_01 = BANCO_MEDIDAS_jr %>%
  select(QUAL_MEDIDA_PROTETIVA_01)

BANCO_MEDIDAS_jr_02 = BANCO_MEDIDAS_jr %>%
  select(QUAL_MEDIDA_PROTETIVA_02)

BANCO_MEDIDAS_jr_03 = BANCO_MEDIDAS_jr %>%
  select(QUAL_MEDIDA_PROTETIVA_03)


BANCO_MEDIDAS_jr_04 = BANCO_MEDIDAS_jr %>%
  select(QUAL_MEDIDA_PROTETIVA_04)

BANCO_MEDIDAS_jr_05 = BANCO_MEDIDAS_jr %>%
  select(QUAL_MEDIDA_PROTETIVA_05)

BANCO_MEDIDAS_jr_06 = BANCO_MEDIDAS_jr %>%
  select(QUAL_MEDIDA_PROTETIVA_06)



#########################################################################################################
#renomeando variavel para empilhar:

colnames(BANCO_MEDIDAS_jr_01) = c("MEDIDAS")
colnames(BANCO_MEDIDAS_jr_02) = c("MEDIDAS")
colnames(BANCO_MEDIDAS_jr_03) = c("MEDIDAS")
colnames(BANCO_MEDIDAS_jr_04) = c("MEDIDAS")
colnames(BANCO_MEDIDAS_jr_05) = c("MEDIDAS")
colnames(BANCO_MEDIDAS_jr_06) = c("MEDIDAS")
#########################################################################################################
#empilhando, em uma coluna:

BANCO_MEDIDAS_jr = rbind(BANCO_MEDIDAS_jr_01, BANCO_MEDIDAS_jr_02, BANCO_MEDIDAS_jr_03,
                         BANCO_MEDIDAS_jr_04, BANCO_MEDIDAS_jr_05, BANCO_MEDIDAS_jr_06)

BANCO_MEDIDAS_jr$MEDIDAS <- gsub(" ","", BANCO_MEDIDAS_jr$MEDIDAS)
sum(table(BANCO_MEDIDAS_jr))
table(BANCO_MEDIDAS_jr$MEDIDAS)
#########################################################################################################
# substituindo

BANCO_MEDIDAS_jr = data.frame (table(BANCO_MEDIDAS_jr$MEDIDAS))

#########################################################################################################
#########################################################################################################
#renomeando variavel :

colnames(BANCO_MEDIDAS_jr) = c("MEDIDA", "QUANTIDADE")
#########################################################################################################
sum(table(BANCO_MEDIDAS_jr))
table(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr$MEDIDA <- as.character(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr = filter(BANCO_MEDIDAS_jr, !MEDIDA == "DESCONSIDERARAOSOMAR")
BANCO_MEDIDAS_jr
#########################################################################################################
#########################################################################################################
table(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr$MEDIDA = ifelse(BANCO_MEDIDAS_jr$MEDIDA == "1",
                                 "ART. 101, I", BANCO_MEDIDAS_jr$MEDIDA)

table(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr$MEDIDA = ifelse(BANCO_MEDIDAS_jr$MEDIDA == "2",
                                 "ART. 101, II", BANCO_MEDIDAS_jr$MEDIDA)

table(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr$MEDIDA = ifelse(BANCO_MEDIDAS_jr$MEDIDA == "3",
                                 "ART. 101, III", BANCO_MEDIDAS_jr$MEDIDA)

table(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr$MEDIDA = ifelse(BANCO_MEDIDAS_jr$MEDIDA == "4",
                                 "ART. 101, IV", BANCO_MEDIDAS_jr$MEDIDA)

table(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr$MEDIDA = ifelse(BANCO_MEDIDAS_jr$MEDIDA == "5",
                                 "ART. 101, V", BANCO_MEDIDAS_jr$MEDIDA)

table(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr$MEDIDA = ifelse(BANCO_MEDIDAS_jr$MEDIDA == "6",
                                 "ART. 101, VI", BANCO_MEDIDAS_jr$MEDIDA)

table(BANCO_MEDIDAS_jr$MEDIDA)
BANCO_MEDIDAS_jr$MEDIDA = ifelse(BANCO_MEDIDAS_jr$MEDIDA == "7",
                                 "ART. 101, VII", BANCO_MEDIDAS_jr$MEDIDA)

#########################################################################################################
#########################################################################################################
#BANCO_MEDIDAS_jr = data.frame(table(BANCO_MEDIDAS_jr))

#colnames(BANCO_MEDIDAS_jr) <- c("MEDIDA", "QUANTIDADE")

BANCO_MEDIDAS_jr  <- BANCO_MEDIDAS_jr[order(BANCO_MEDIDAS_jr[,1],decreasing=FALSE),]

BANCO_MEDIDAS_jr_bkp = BANCO_MEDIDAS_jr #salvando atos atendimento original

#library(grid)
#library(gridExtra)

#acrescentando coluna com percentual
BANCO_MEDIDAS_jr$QUANTIDADE <- as.numeric(BANCO_MEDIDAS_jr$QUANTIDADE)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
#BANCO_MEDIDAS_jr$PERCENTUAL <- round(prop.table(BANCO_MEDIDAS_jr$QUANTIDADE)*100, 2)
BANCO_MEDIDAS_jr$PERCENTUAL <- round_preserve_sum(prop.table(BANCO_MEDIDAS_jr$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#BANCO_MEDIDAS_jr = mutate(BANCO_MEDIDAS_jr,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)


BANCO_MEDIDAS_jr_bkp=BANCO_MEDIDAS_jr


#########################################################################################################
#########################################################################################################

#script para o bookdown

BANCO_MEDIDAS_jr_rmark = BANCO_MEDIDAS_jr

#SEPARAR CASOS DE ARQUIVAMENTO E REMISSÕES PARA SCRIPTS EM 014_DECISOES_Rmd

#filter(BANCO_MEDIDAS_jr_rmark, !grepl("REMISSAO", DECISAO))
#BANCO_MEDIDAS_jr_rmark = filter(BANCO_MEDIDAS_jr_rmark, grepl("ARQUIVAMENTO|REMISSAO", DECISAO))


BANCO_MEDIDAS_jr_rmark <- BANCO_MEDIDAS_jr_rmark %>%
  arrange(desc(PERCENTUAL))

#BANCO_MEDIDAS_jr_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
BANCO_MEDIDAS_jr_rmark = BANCO_MEDIDAS_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
#sum(BANCO_MEDIDAS_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
BANCO_MEDIDAS_jr_rmark[1,2]
#outra forma de calcular percentual
#BANCO_MEDIDAS_jr = mutate(BANCO_MEDIDAS_jr,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

#acrescentando linha com total
BANCO_MEDIDAS_jr <- rbind(BANCO_MEDIDAS_jr,
                          data.frame(MEDIDA = "TOTAL", QUANTIDADE = sum(BANCO_MEDIDAS_jr$QUANTIDADE), PERCENTUAL = sum(BANCO_MEDIDAS_jr$PERCENTUAL),
                                     stringsAsFactors = FALSE))

colnames(BANCO_MEDIDAS_jr) <- c("MEDIDA", "QUANTIDADE", "%")

#para tabela gt abaixo:
BANCO_MEDIDAS_jr_gt = BANCO_MEDIDAS_jr


###write.xlsx(BANCO_MEDIDAS_jr, file = "BANCO_MEDIDAS_jr_total.xlsx") #salvando para usar na comparada
#write.csv(BANCO_MEDIDAS_jr, file = "BANCO_MEDIDAS_jr_total.csv", row.names=FALSE) #salvando com modificações anteriores


#########################################################################################################
#########################################################################################################
#tabela alternativa
#require(ggpubr)
#########################################################################################################
#salvando tabela
#pdf(file="tabela_BANCO_MEDIDAS_jr_geral_alternativa.pdf",  width = 6, height = 4.8, title = "BANCO_MEDIDAS_jr")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# BANCO_MEDIDAS_jr FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
### BANCO_MEDIDAS_SOCIOEDU_jr
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

BANCO_MEDIDAS_SOCIOEDU_jr = banco_jr %>%
  select(SEXO, DATA_DO_FATO, TEM_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO,
         QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01,
         QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02,
         QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03,
         QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04)
#########################################################################################################
#write.csv(BANCO_MEDIDAS_SOCIOEDU_jr, file = "BANCO_MEDIDAS_SOCIOEDU_jr.csv", row.names = TRUE)
###write.xlsx(BANCO_MEDIDAS_SOCIOEDU_jr, file = "BANCO_MEDIDAS_SOCIOEDU_jr.xlsx")
#########################################################################################################

BANCO_MEDIDAS_SOCIOEDU_jr$TEM_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO <- gsub(" ","", BANCO_MEDIDAS_SOCIOEDU_jr$TEM_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO)

table(BANCO_MEDIDAS_SOCIOEDU_jr$TEM_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO)

BANCO_MEDIDAS_SOCIOEDU_jr_total = BANCO_MEDIDAS_SOCIOEDU_jr

BANCO_MEDIDAS_SOCIOEDU_jr = filter(BANCO_MEDIDAS_SOCIOEDU_jr, TEM_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO == "SIM")

BANCO_MEDIDAS_SOCIOEDU_jr_sim = BANCO_MEDIDAS_SOCIOEDU_jr

table(BANCO_MEDIDAS_SOCIOEDU_jr$TEM_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO)
#BANCO_MEDIDAS_SOCIOEDU_jr<-BANCO_MEDIDAS_SOCIOEDU_jr[(BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA_PROTETIVA == "SIM"),]


#########################################################################################################
#convertendo de factor para character:
BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01 <- as.character(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01)
BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02 <- as.character(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02)
BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03 <- as.character(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03)
BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04 <- as.character(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04)
#BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_05 <- as.character(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_05)
#BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_06 <- as.character(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_06)
#########################################################################################################
# substituir NSA em DESCONSIDERAR AO SOMAR
table(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01)
BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01 = ifelse(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01 == "NSA",
                                                                                 "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01)


BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01 = ifelse(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01 == "",
                                                                                 "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01)

table(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01)


table(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02)
BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02 = ifelse(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02 == "NSA",
                                                                                 "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02)

BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02 = ifelse(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02 == "",
                                                                                 "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02)

table(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02)


table(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03)
BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03 = ifelse(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03 == "NSA",
                                                                                 "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03)


BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03 = ifelse(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03 == "",
                                                                                 "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03)

table(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03)


table(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04)
BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04 = ifelse(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04 == "NSA",
                                                                                 "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04)


BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04 = ifelse(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04 == "",
                                                                                 "DESCONSIDERARAOSOMAR", BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04)

table(BANCO_MEDIDAS_SOCIOEDU_jr$QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04)


#########################################################################################################
#########################################################################################################
#########################################################################################################

#separando colunas para empilhar em uma:

BANCO_MEDIDAS_SOCIOEDU_jr_01 = BANCO_MEDIDAS_SOCIOEDU_jr %>%
  select(QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_01)

BANCO_MEDIDAS_SOCIOEDU_jr_02 = BANCO_MEDIDAS_SOCIOEDU_jr %>%
  select(QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_02)

BANCO_MEDIDAS_SOCIOEDU_jr_03 = BANCO_MEDIDAS_SOCIOEDU_jr %>%
  select(QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_03)


BANCO_MEDIDAS_SOCIOEDU_jr_04 = BANCO_MEDIDAS_SOCIOEDU_jr %>%
  select(QUAL_MEDIDA._SOCIOEDUCATIVA_NESSE_PROCESSO_04)

#########################################################################################################
#renomeando variavel para empilhar:

colnames(BANCO_MEDIDAS_SOCIOEDU_jr_01) = c("MEDIDAS")
colnames(BANCO_MEDIDAS_SOCIOEDU_jr_02) = c("MEDIDAS")
colnames(BANCO_MEDIDAS_SOCIOEDU_jr_03) = c("MEDIDAS")
colnames(BANCO_MEDIDAS_SOCIOEDU_jr_04) = c("MEDIDAS")
#########################################################################################################
#empilhando, em uma coluna:

BANCO_MEDIDAS_SOCIOEDU_jr = rbind(BANCO_MEDIDAS_SOCIOEDU_jr_01, BANCO_MEDIDAS_SOCIOEDU_jr_02, BANCO_MEDIDAS_SOCIOEDU_jr_03,
                                  BANCO_MEDIDAS_SOCIOEDU_jr_04)

BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDAS <- gsub(" ","", BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDAS)
sum(table(BANCO_MEDIDAS_SOCIOEDU_jr))
table(BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDAS)
#########################################################################################################
# substituindo

BANCO_MEDIDAS_SOCIOEDU_jr = data.frame (table(BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDAS))

#########################################################################################################
#########################################################################################################
#renomeando variavel :

colnames(BANCO_MEDIDAS_SOCIOEDU_jr) = c("MEDIDA", "QUANTIDADE")
#########################################################################################################
sum(table(BANCO_MEDIDAS_SOCIOEDU_jr))
table(BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA)
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA <- as.character(BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA)
BANCO_MEDIDAS_SOCIOEDU_jr = filter(BANCO_MEDIDAS_SOCIOEDU_jr, !MEDIDA == "DESCONSIDERARAOSOMAR")
BANCO_MEDIDAS_SOCIOEDU_jr
#########################################################################################################
#########################################################################################################
#preenchimento de celulas:
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "ARQUIVAMENTO/ATIPICIDADE"]<-	"ARQUIVAMENTO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "SENTENCAINCONCLUSIVA"]<-	"VOUTROS"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "INTERNACAO"]<-	"INTERNAÇÃO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "LA"]<-	"REMISSÃO c/c LA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "OUTRAS(OS)"]<-	"VOUTROS"
#BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "PSC"]<-	"REMISSÃO c/c PSC"
#BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "PSC/LA"]<-	"REMISSÃO c/c LA/PSC"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
#BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "SEMINFORMACAO"]<-	"AGUARDANDO SENTENCA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "SEMINFORMACAO"]<-	"ENCAMINHADOS PARA SENTENÇA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO DOS AUTOS A DELEGACIA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "L.A"]<-	"LIBERDADE ASSISTIDA"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == "PSC"]<-	"PRESTAÇÃO DE SERVIÇO À COMUNIDADE"
BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA[BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA == ""]<-	"VAZIO"

sum(BANCO_MEDIDAS_SOCIOEDU_jr$QUANTIDADE)
#write.csv(BANCO_MEDIDAS_SOCIOEDU_jr, file ="BANCO_MEDIDAS_SOCIOEDU_jr.csv", row.names=FALSE)

BANCO_MEDIDAS_SOCIOEDU_jr


#########################################################################################################
#########################################################################################################
#BANCO_MEDIDAS_SOCIOEDU_jr = data.frame(table(BANCO_MEDIDAS_SOCIOEDU_jr))

#colnames(BANCO_MEDIDAS_SOCIOEDU_jr) <- c("MEDIDA", "QUANTIDADE")

BANCO_MEDIDAS_SOCIOEDU_jr  <- BANCO_MEDIDAS_SOCIOEDU_jr[order(BANCO_MEDIDAS_SOCIOEDU_jr[,1],decreasing=FALSE),]

BANCO_MEDIDAS_SOCIOEDU_jr_bkp = BANCO_MEDIDAS_SOCIOEDU_jr #salvando atos atendimento original

#library(grid)
#library(gridExtra)

#acrescentando coluna com percentual
BANCO_MEDIDAS_SOCIOEDU_jr$QUANTIDADE <- as.numeric(BANCO_MEDIDAS_SOCIOEDU_jr$QUANTIDADE)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
#BANCO_MEDIDAS_SOCIOEDU_jr$PERCENTUAL <- round(prop.table(BANCO_MEDIDAS_SOCIOEDU_jr$QUANTIDADE)*100, 2)
BANCO_MEDIDAS_SOCIOEDU_jr$PERCENTUAL <- round_preserve_sum(prop.table(BANCO_MEDIDAS_SOCIOEDU_jr$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#BANCO_MEDIDAS_SOCIOEDU_jr = mutate(BANCO_MEDIDAS_SOCIOEDU_jr,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)


BANCO_MEDIDAS_SOCIOEDU_jr_bkp=BANCO_MEDIDAS_SOCIOEDU_jr


#########################################################################################################
#########################################################################################################

#script para o bookdown

BANCO_MEDIDAS_SOCIOEDU_jr_rmark = BANCO_MEDIDAS_SOCIOEDU_jr

#SEPARAR CASOS DE ARQUIVAMENTO E REMISSÕES PARA SCRIPTS EM 014_DECISOES_Rmd

#filter(BANCO_MEDIDAS_SOCIOEDU_jr_rmark, !grepl("REMISSAO", DECISAO))
#BANCO_MEDIDAS_SOCIOEDU_jr_rmark = filter(BANCO_MEDIDAS_SOCIOEDU_jr_rmark, grepl("ARQUIVAMENTO|REMISSAO", DECISAO))


BANCO_MEDIDAS_SOCIOEDU_jr_rmark <- BANCO_MEDIDAS_SOCIOEDU_jr_rmark %>%
  arrange(desc(PERCENTUAL))

#BANCO_MEDIDAS_SOCIOEDU_jr_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
BANCO_MEDIDAS_SOCIOEDU_jr_rmark = BANCO_MEDIDAS_SOCIOEDU_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
#sum(BANCO_MEDIDAS_SOCIOEDU_jr_rmark$QUANTIDADE)

#para escolher linhas e posicoes
BANCO_MEDIDAS_SOCIOEDU_jr_rmark[1,2]
#outra forma de calcular percentual
#BANCO_MEDIDAS_SOCIOEDU_jr = mutate(BANCO_MEDIDAS_SOCIOEDU_jr,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

#acrescentando linha com total
BANCO_MEDIDAS_SOCIOEDU_jr <- rbind(BANCO_MEDIDAS_SOCIOEDU_jr,
                                   data.frame(MEDIDA = "TOTAL", QUANTIDADE = sum(BANCO_MEDIDAS_SOCIOEDU_jr$QUANTIDADE), PERCENTUAL = sum(BANCO_MEDIDAS_SOCIOEDU_jr$PERCENTUAL),
                                              stringsAsFactors = FALSE))

colnames(BANCO_MEDIDAS_SOCIOEDU_jr) <- c("MEDIDA", "QUANTIDADE", "%")

#para tabela gt abaixo:
BANCO_MEDIDAS_SOCIOEDU_jr_gt = BANCO_MEDIDAS_SOCIOEDU_jr


###write.xlsx(BANCO_MEDIDAS_SOCIOEDU_jr, file = "BANCO_MEDIDAS_SOCIOEDU_jr_total.xlsx") #salvando para usar na comparada
#write.csv(BANCO_MEDIDAS_SOCIOEDU_jr, file = "BANCO_MEDIDAS_SOCIOEDU_jr_total.csv", row.names=FALSE) #salvando com modificações anteriores


#########################################################################################################
#########################################################################################################
#tabela alternativa
#require(ggpubr)
#########################################################################################################
#salvando tabela
#pdf(file="tabela_BANCO_MEDIDAS_SOCIOEDU_jr_geral_alternativa.pdf",  width = 6, height = 4.8, title = "BANCO_MEDIDAS_SOCIOEDU_jr")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# BANCO_MEDIDAS_SOCIOEDU_jr FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO INCIDENCIA:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_atos_em_foco_jr =

  banco_jr %>%
  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") %>%
  #select(-name) %>%
  filter(ATO_INFRACIONAL != "DESCONSIDERARAOSOMAR")

banco_atos_em_foco_jr$ATO_INFRACIONAL <- as.character(banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL <- gsub(" ","",banco_atos_em_foco_jr$ATO_INFRACIONAL)


head (banco_atos_em_foco_jr %>%
        select(NOME2, PROCESSO, name, ATO_INFRACIONAL), 20)
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
#banco =

#  banco %>%
#  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") %>%
#select(-name) %>%
#  filter(ATO_INFRACIONAL != "DESCONSIDERARAOSOMAR")

#head(banco, n=10)[,117:120]
#########################################################################################################

#########################################################################################################
#AMEACA

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "147.ARTCPB",
                                               "AMEAÇA", banco_atos_em_foco_jr$ATO_INFRACIONAL)


########################################################################################################
#########################################################################################################
#CRIME DE TRÂNSITO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "309.ARTCTB",
                                               "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÂO/HABILITAÇÃO)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "310.ARTCTB",
                                               "CRIME DE TRÂNSITO (ENTREGAR DIRECAO A NAO HABILITADO)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#para discrinar: é so anular com #

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÂO/HABILITAÇÃO)",
                                               "CRIME DE TRÂNSITO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (ENTREGAR DIRECAO A NAO HABILITADO)",
                                               "CRIME DE TRÂNSITO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#DANO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "163.ARTCPB",
                                               "DANO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "213.ARTCPB",
                                               "ESTUPRO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO DE VULNERÁVEL

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "217-A.ARTCPB",
                                               "ESTUPRO DE VULNERÁVEL", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#FURTO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "155.ARTCPB",
                                               "FURTO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#FURTO (TENTATIVA)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "155C/C14.ARTCPB",
                                               "FURTO (TENTATIVA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "121.ARTCPB",
                                               "HOMICÍDIO", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO (TENTATIVA)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "121C/C14,II.ARTCPB",
                                               "HOMICÍDIO (TENTATIVA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "129.ARTCPB",
                                               "LESÃO CORPORAL", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "129§3º.ARTCPB",
                                               "LESÃO CORPORAL", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "129§9º.ARTCPB",
                                               "LESÃO CORPORAL", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL (TENTATIVA). Ordem para trocar LESÃO CORPORAL (TENTATIVA) por VIAS DE FATO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "129C/C14,II.ARTCPB",
                                               "VIAS DE FATO", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PICHAÇÃO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "65.ART9.605",
                                               "PICHAÇÃO", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PORTE/POSSE DE ARMA

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "12.ART10.826",
                                               "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "14.ART10.826",
                                               "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "16.ART10.826",
                                               "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)", banco_atos_em_foco_jr$ATO_INFRACIONAL)



#para discrinar: é so anular com #

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)",
                                               "PORTE/POSSE DE ARMA", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)",
                                               "PORTE/POSSE DE ARMA", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)",
                                               "PORTE/POSSE DE ARMA", banco_atos_em_foco_jr$ATO_INFRACIONAL)



#########################################################################################################
#########################################################################################################
#RECEPTAÇÃO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "180.ARTCPB",
                                               "RECEPTAÇÃO", banco_atos_em_foco_jr$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157.ARTCPB",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2º,I,IIeVARTCPB",
                                               "ROUBO (EM CONCURSO DE PESSOAS)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2º,I.ARTCPB",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2º,IeII.ARTCPB",
                                               "ROUBO (EM CONCURSO DE PESSOAS)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºA,IARTCPB",
                                               "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºIIARTCPB",
                                               "ROUBO (EM CONCURSO DE PESSOAS)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºIIeVARTCPB",
                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºA,I",
                                               "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºAIARTCPB",
                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºAIeIIARTCPB",
                                               "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºII,VeVIIARTCPB",
                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºIIeVIARTCPB",
                                               "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)



banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºIIeVIIARTCPB",
                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºIIEVIIARTCPB",
                                               "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºVARTCPB",
                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2ºVIIARTCPB",
                                               "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#para discrinar: é so anular com #

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ROUBO",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ROUBO",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                               "ROUBO", banco_atos_em_foco_jr$ATO_INFRACIONAL)



#########################################################################################################
#ROUBO (TENTATIVA)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§2º,IeIIC/C14,II.ARTCPB",
                                               "ROUBO (TENTATIVA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157C/C14,II.ARTCPB",
                                               "ROUBO (TENTATIVA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ROUBO (§3º)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§3ºARTCPB",
                                               "LATROCÍNIO", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ROUBO (§3º) (TENTATIVA)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "157§3ºARTCPBC/C14,II,CPB",
                                               "ROUBO (§3º) (TENTATIVA)", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#SEQUESTRO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "148.ARTCPB",
                                               "SEQUESTRO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#TRÁFICO DE DROGAS


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "33.ART11.343",
                                               "TRÁFICO DE DROGAS", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "37.ART11.343",
                                               "TRÁFICO DE DROGAS (INFORMANTE)", banco_atos_em_foco_jr$ATO_INFRACIONAL)



#para discrinar: é so anular com #

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "TRÁFICO DE DROGAS",
                                               "TRÁFICO DE DROGAS", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (INFORMANTE)",
                                               "TRÁFICO DE DROGAS", banco_atos_em_foco_jr$ATO_INFRACIONAL)





#########################################################################################################
#########################################################################################################
#########################################################################################################
#ASSOCIAÇÃO TRÁFICO DE DROGAS


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "35.ART11.343",
                                               "TRÁFICO DE DROGAS (ASSOCIACAO)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "34.ART11.343",
                                               "TRÁFICO DE DROGAS (ASSOCIACAO)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#para discrinar: é so anular com #

#banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (ASSOCIACAO)",
#                                           "TRÁFICO DE DROGAS", banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#POSSE DE DROGAS PARA USO PESSOAL

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "28.ART11.343",
                                               "POSSE DE DROGAS PARA USO PESSOAL", banco_atos_em_foco_jr$ATO_INFRACIONAL)



#########################################################################################################
#########################################################################################################
#VIAS DE FATO

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "21.ARTLCP",
                                               "VIAS DE FATO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


#########################################################################################################
#OUTROS

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "140.ARTCPB",
                                               "INJÚRIA", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "140§3º.ARTCPB",
                                               "INJÚRIA", banco_atos_em_foco_jr$ATO_INFRACIONAL)



banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "330.ARTCPB",
                                               "DESOBEDIÊNCIA", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "331.ARTCPB",
                                               "DESACATO", banco_atos_em_foco_jr$ATO_INFRACIONAL)


banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "139.ARTCPB",
                                               "DESOBEDIÊNCIA", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "2.ART13.185",
                                               "INTIMIDAÇÃO SISTEMÁTICA (BULLYING)", banco_atos_em_foco_jr$ATO_INFRACIONAL)

banco_atos_em_foco_jr$ATO_INFRACIONAL = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL == "329.ARTCPB",
                                               "RESISTÊNCIA", banco_atos_em_foco_jr$ATO_INFRACIONAL)



table(banco_atos_em_foco_jr$ATO_INFRACIONAL)

#########################################################################################################
#preenchendo os vazios
banco_atos_em_foco_jr$ATO_INFRACIONAL[banco_atos_em_foco_jr$ATO_INFRACIONAL == ""]<- "VOUTROS"
#########################################################################################################

head (banco_atos_em_foco_jr %>%
        select(NOME2, PROCESSO, name, ATO_INFRACIONAL), 20)

#########################################################################################################
# substituir ato duplicado, mesma linha, em DESCONSIDERAR AO SOMAR Somente em ATA_02 e ATA_03

#banco_atos_em_foco_jr$ATO_INFRACIONAL_ATA_02 = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL_ATA_01 == banco_atos_em_foco_jr$ATO_INFRACIONAL_ATA_02,
#                                                  "REPETIDO", banco_atos_em_foco_jr$ATO_INFRACIONAL_ATA_02)

#banco_atos_em_foco_jr$ATO_INFRACIONAL_ATA_03 = ifelse(banco_atos_em_foco_jr$ATO_INFRACIONAL_ATA_01 == banco_atos_em_foco_jr$ATO_INFRACIONAL_ATA_03,
#                                                  "REPETIDO", banco_atos_em_foco_jr$ATO_INFRACIONAL_ATA_03)


#banco_sem_concurso_jr <- banco_atos_em_foco_jr[!duplicated(data.frame(banco_atos_em_foco_jr$PROCESSO, banco_atos_em_foco_jr$name, banco_atos_em_foco_jr$ATO_INFRACIONAL)),]

library(dplyr)

# Remove duplicate rows of the dataframe using variables
banco_sem_concurso_jr = distinct(banco_atos_em_foco_jr, PROCESSO,name,ATO_INFRACIONAL, .keep_all= TRUE)


head (banco_sem_concurso_jr %>%
        select(NOME2, PROCESSO, name, ATO_INFRACIONAL), 20)

#PARA COMPARAR
head (banco_atos_em_foco_jr %>%
        select(NOME2, PROCESSO, ATO_INFRACIONAL), 20)

head (banco_sem_concurso_jr %>%
        select(NOME2, PROCESSO, name, ATO_INFRACIONAL), 20)

banco_sem_concurso_jr_bkp = banco_sem_concurso_jr

#write.csv(banco_sem_concurso_jr, file = "banco_sem_concurso_jr.csv", row.names = FALSE)



banco_incidencia_jr = banco_sem_concurso_jr

head (banco_incidencia_jr %>%
        select(NOME2, PROCESSO, ATO_INFRACIONAL), 20)
#########################################################################################################

library(dplyr)

banco_incidencia_jr = banco_incidencia_jr %>%
  select(ATO_INFRACIONAL)

#########################################################################################################
banco_incidencia_jr$ATO_INFRACIONAL[banco_incidencia_jr$ATO_INFRACIONAL == "FATO ATÍPICO"]<- "OUTROS"

#OUTROS (PARA QUE ELE SEJA CONSIDERADO "VOUTROS" E APARECA EM ORDEM ALFABETICA NA HORA DE SOMAR COM TABELA DINAMICA)

banco_incidencia_jr$ATO_INFRACIONAL[banco_incidencia_jr$ATO_INFRACIONAL == "OUTROS"]<- "VOUTROS"
banco_incidencia_jr$ATO_INFRACIONAL[banco_incidencia_jr$ATO_INFRACIONAL == "TERMOSEMINF"]<- "VS/INF"

#########################################################################################################

#banco_incidencia_jr$ATO_INFRACIONAL <- gsub(" ","", banco_incidencia_jr$ATO_INFRACIONAL)

table(banco_incidencia_jr)
###EXCLUIR LINHA DESCONSIDERAR AO SOMAR
banco_incidencia_jr$ATO_INFRACIONAL[banco_incidencia_jr$ATO_INFRACIONAL == "NSA"]<- "DESCONSIDERARAOSOMAR"
table(banco_incidencia_jr)
sum(table(banco_incidencia_jr))
#banco_incidencia_jr <- banco_incidencia_jr[!(banco_incidencia_jr$ATO_INFRACIONAL == "0"),]
#banco_incidencia_jr <- banco_incidencia_jr[!(banco_incidencia_jr$ATO_INFRACIONAL == "DESCONSIDERAR AO SOMAR"),]
banco_incidencia_jr = filter(banco_incidencia_jr, !ATO_INFRACIONAL == "DESCONSIDERARAOSOMAR")
sum(table(banco_incidencia_jr))
banco_incidencia_jr = filter(banco_incidencia_jr, !ATO_INFRACIONAL == "REPETIDO")
table(banco_incidencia_jr)
sum(table(banco_incidencia_jr))

banco_incidencia_jr = filter(banco_incidencia_jr, !ATO_INFRACIONAL == "MBA")
table(banco_incidencia_jr)
sum(table(banco_incidencia_jr))
#########################################################################################################
#Encontrando OS VARIADOS ARTIGOS QUE SOBRARAM e os que já estao como VOUTROS e os colocando na NOVA COLUNA ATO_INFRACIONAL2

banco_incidencia_jr$ATO = grepl(pattern = "ART", x = banco_incidencia_jr$ATO_INFRACIONAL) | grepl(pattern = "VOUTROS", x = banco_incidencia_jr$ATO_INFRACIONAL)

table(banco_incidencia_jr$ATO)

#substituindo
banco_incidencia_jr$ATO = ifelse(banco_incidencia_jr$ATO == TRUE,
                                 "VOUTROS", banco_incidencia_jr$ATO_INFRACIONAL)

sum(table(banco_incidencia_jr))
head(banco_incidencia_jr, 25)
table(banco_incidencia_jr$ATO_INFRACIONAL)
table(banco_incidencia_jr$ATO)
#########################################################################################################
#excluir coluna
#banco_incidencia_jr  <- banco_incidencia_jr[order(banco_incidencia_jr[,2],decreasing=FALSE),]
banco_incidencia_jr = arrange(banco_incidencia_jr, ATO)

banco_incidencia_jr$ATO_INFRACIONAL <- NULL

###renomeando:
#banco_incidencia_jr <- banco_incidencia_jr[!(banco_incidencia_jr$QUANTIDADE == "0"),]
#banco_incidencia_jr <- banco_incidencia_jr[!(banco_incidencia_jr$ATO == "DESCONSIDERAR AO SOMAR"),]
#banco_incidencia_jr = data.frame(table(banco_incidencia_jr$ATO_INFRACIONAL))
banco_incidencia_jr = data.frame(table(banco_incidencia_jr$ATO))
#banco_incidencia_jr = as_tibble(table(banco_incidencia_jr$ATO))
#ordenar coluna para facilitar visualizacao na soma com tabela dinamica
banco_incidencia_jr$Var1 <- as.character(banco_incidencia_jr$Var1)

banco_incidencia_jr$Var1[banco_incidencia_jr$Var1 == "VS/INF"]<- "SEM INFORMAÇÃO"
banco_incidencia_jr$Var1[banco_incidencia_jr$Var1 == "VOUTROS"]<- "OUTROS"


#write.csv(banco_incidencia_jr, file ="banco_incidencia_jr_atual.csv",row.names=FALSE)

#SALVAR CSV NO DIRETORIO RAIZ
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#write.csv(banco_incidencia_jr, file ="banco_incidencia_jr_atual.csv",row.names=FALSE)

#VOLTAR PAAR O DIRETORIO PADRAO
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

##write.xlsx(banco_incidencia_jr, file ="banco_incidencia_jr_atual.xlsx")

#########################################################################################################
#########################################################################################################

#########################################################################################################
##TABELA ENVOLVIMENTOS Atos Infracionais

colnames(banco_incidencia_jr) <- c("ATO", "QUANTIDADE")

banco_incidencia_jr_bkp = banco_incidencia_jr #salvando atos atendimento original

#library(grid)
#library(gridExtra)

#acrescentando coluna com percentual
banco_incidencia_jr$QUANTIDADE <- as.numeric(banco_incidencia_jr$QUANTIDADE)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
#banco_incidencia_jr$PERCENTUAL <- round(prop.table(banco_incidencia_jr$QUANTIDADE)*100, 2)
banco_incidencia_jr$PERCENTUAL <- round(prop.table(banco_incidencia_jr$QUANTIDADE)*100, 2)

#script para o bookdown

banco_incidencia_jr_rmark = banco_incidencia_jr

#banco_incidencia_jr_rmark <- banco_incidencia_jr_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_jr_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
banco_incidencia_jr_rmark = banco_incidencia_jr_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(banco_incidencia_jr_rmark$PERCENTUAL)

#para escolher linhas e posicoes
banco_incidencia_jr_rmark[1,2]
#outra forma de calcular percentual
#banco_incidencia_jr = mutate(banco_incidencia_jr,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)




#acrescentando linha com total
banco_incidencia_jr <- rbind(banco_incidencia_jr,
                             data.frame(ATO = "TOTAL", QUANTIDADE = sum(banco_incidencia_jr$QUANTIDADE), PERCENTUAL = sum(round_preserve_sum(banco_incidencia_jr$PERCENTUAL, 0)),
                                        stringsAsFactors = FALSE))


colnames(banco_incidencia_jr) <- c("ATO", "QUANTIDADE", "%")

##write.xlsx(banco_incidencia_jr, file = "banco_incidencia_jr_atual_total.xlsx") #salvando para usar na comparada
#write.csv(banco_incidencia_jr, file = "banco_incidencia_jr_atual_total.csv", row.names=FALSE) #salvando com modificações anteriores


#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
banco_incidencia_jr=banco_incidencia_jr_bkp

#library(grid)
#library(gridExtra)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#INCIDENCIA COMPARADA. obs: trazer arquivo ano anterior para a pasta.
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
banco_incidencia_jr_atual = banco_incidencia_jr
colnames(banco_incidencia_jr_atual) <- c("ATO", "QUANTIDADE")

#Garantir ordem alfabetica
banco_incidencia_jr_atual$ATO <- as.character(banco_incidencia_jr_atual$ATO)
banco_incidencia_jr_atual$ATO[banco_incidencia_jr_atual$ATO == "SEM INFORMAÇÃO"]<- "VSEM INFORMAÇÃO"
banco_incidencia_jr_atual$ATO[banco_incidencia_jr_atual$ATO == "OUTROS"]<- "VOUTROS"
#banco_incidencia_jr_atual  <- banco_incidencia_jr_atual[order(banco_incidencia_jr_atual[,1],decreasing=FALSE),]
banco_incidencia_jr_atual<-banco_incidencia_jr_atual %>%
  arrange(ATO)
#renomeando
banco_incidencia_jr_atual$ATO[banco_incidencia_jr_atual$ATO == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_incidencia_jr_atual$ATO[banco_incidencia_jr_atual$ATO == "VOUTROS"]<- "OUTROS"

#RETORNANDO PARA O DIRETÓRIO PADRÃO
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

banco_incidencia_jr_atual

#Garantir ordem alfabetica
banco_incidencia_jr_atual$ATO <- as.character(banco_incidencia_jr_atual$ATO)
banco_incidencia_jr_atual$ATO[banco_incidencia_jr_atual$ATO == "SEM INFORMAÇÃO"]<- "VSEM INFORMAÇÃO"
banco_incidencia_jr_atual$ATO[banco_incidencia_jr_atual$ATO == "OUTROS"]<- "VOUTROS"
banco_incidencia_jr_atual  <- banco_incidencia_jr_atual[order(banco_incidencia_jr_atual[,1],decreasing=FALSE),]

#renomeando
banco_incidencia_jr_atual$ATO[banco_incidencia_jr_atual$ATO == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_incidencia_jr_atual$ATO[banco_incidencia_jr_atual$ATO == "VOUTROS"]<- "OUTROS"
banco_incidencia_jr_atual

#renomeando colunas
#colnames(banco_incidencia_jr_atual) <- c("ATO", "ANOANTERIOR", "ANOATUAL")

#acrescentando coluna com percentual
banco_incidencia_jr_atual$QUANTIDADE <- as.numeric(banco_incidencia_jr_atual$QUANTIDADE)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
#banco_incidencia_jr_atual$PERCENTUAL <- round(prop.table(banco_incidencia_jr_atual$QUANTIDADE)*100, 2)
banco_incidencia_jr_atual$PERCENTUAL <- round_preserve_sum(prop.table(banco_incidencia_jr_atual$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#banco_incidencia_jr_atual = mutate(banco_incidencia_jr_atual,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)






banco_incidencia_jr_atual$ATO <- as.character(banco_incidencia_jr_atual$ATO)

#NA POR ZERO

banco_incidencia_jr_atual = replace(x = banco_incidencia_jr_atual, list = is.na(banco_incidencia_jr_atual), values = 0)

banco_incidencia_jr_atual_bkp = banco_incidencia_jr_atual
#acrescentando linha com total
banco_incidencia_jr_atual <- rbind(banco_incidencia_jr_atual,
                                   data.frame(ATO = "TOTAL",
                                              QUANTIDADE = sum(banco_incidencia_jr_atual$QUANTIDADE),
                                              PERCENTUAL = sum(banco_incidencia_jr_atual$PERCENTUAL),
                                              stringsAsFactors = FALSE))

#banco_incidencia_jr_atual$VAR <- round(((banco_incidencia_jr_atual$ANOATUAL*100)/banco_incidencia_jr_atual$ANOANTERIOR)-100, 2)



#colnames(banco_incidencia_jr_atual) <- c("ATO", format(Sys.Date()-365*2, "%Y"), format(Sys.Date()-365*1, "%Y"), "VAR%")

#TESTES TABELA GT:
colnames(banco_incidencia_jr_atual) <- c("ATO", "QUANTIDADE", "%")

banco_incidencia_jr_atual_gt = banco_incidencia_jr_atual



#########################################################################################################
#pdf(file="TABELA_002_banco_incidencia_jr_atual_alternativa.pdf", width = 5, height = 7, title = "INCIDENCIA COMPARADA")



#########################################################################################################
#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#FIM
#########################################################################################################

