#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR JR:
dir.create(file.path("~/diretorio_r/estciabh", "mba"))
setwd(file.path("~/diretorio_r/estciabh"))
#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

#########################################################################################################
#banco_so_com_mba <- read.csv("banco_so_com_mba.csv",header=TRUE, sep="|", encoding = "UTF-8", skip = 2) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

banco_so_com_mba <- as_tibble(banco_so_com_mba)



banco_so_com_mba_bkp = banco_so_com_mba
#########################################################################################################
#filtrar data
#banco_so_com_mba = banco_so_com_mba %>%
  #filter(DATA_DO_ENCAMINHAMENTO_DO_JUIZ >= (str_c(format(Sys.Date()-365*1, "%Y"), "-01-01")) &
          # DATA_DO_ENCAMINHAMENTO_DO_JUIZ <= (str_c(format(Sys.Date()-365*1, "%Y"), "-12-31")))
  #filter(DATA_DO_ENCAMINHAMENTO_DO_JUIZ >= (str_c(format(Sys.Date()-365*2, "%Y"), "-01-01")) & DATA_DO_ENCAMINHAMENTO_DO_JUIZ <= (str_c(format(Sys.Date()-365*2, "%Y"), "-12-31")))

#banco_so_com_mba = banco_so_com_mba %>%
  #filter(DATA_DO_RECEBIMENTO_NO_SETOR >= (str_c(format(Sys.Date()-365*1, "%Y"), "-01-01")) &
          # DATA_DO_RECEBIMENTO_NO_SETOR <= (str_c(format(Sys.Date()-365*1, "%Y"), "-12-31")))
#########################################################################################################

#########################################################################################################
#tabela total_MBA
total_MBA = data.frame(nrow(banco_so_com_mba))

colnames(total_MBA) <- c("QUANTIDADE DE MBAs CUMPRIDOS")

#para tabela gt abaixo:
total_MBA_gt = total_MBA

#########################################################################################################
#########################################################################################################


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/mba"))

#########################################################################################################
#retirar nomes duplicados:snr=sem nome repetido
#snr_banco_ESCOLA <- escola_bkp[!duplicated(data.frame(escola_bkp$NOME2, escola_bkp$NASCIMENTO)),]



# Remove duplicate rows of the dataframe using variables
snr_banco_so_com_mba = distinct(banco_so_com_mba, NOME2,NASCIMENTO,FILIACAO2, .keep_all= TRUE)


#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=FALSE)

#########################################################################################################
#########################################################################################################


#########################################################################################################

#banco_so_com_mba = snr_banco_so_com_mba

#########################################################################################################
#########################################################################################################
MBA_bkp = banco_so_com_mba
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/mba"))
#########################################################################################################
#retirar nomes duplicados:snr=sem nome repetido
#snr_banco_so_com_mba <- MBA_bkp[!duplicated(data.frame(MBA_bkp$NOME2, MBA_bkp$NASCIMENTO)),]

# Remove duplicate rows of the dataframe using variables
#snr_banco_so_com_mba = distinct(MBA_bkp, NOME2,NASCIMENTO, .keep_all= TRUE)


#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=FALSE)

#########################################################################################################
#########################################################################################################



#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos s/inf ou #valor!)
MBA_snr = snr_banco_so_com_mba

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_MBA_idade_MBA_bkp = MBA_snr %>%
  select(SEXO, IDADE)

#table(df_snr_sexo_MBA_idade_MBA_bkp$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_MBA_idade_MBA_bkp$SEXO <- as.character(df_snr_sexo_MBA_idade_MBA_bkp$SEXO)

df_snr_sexo_MBA_idade_MBA_bkp$SEXO[df_snr_sexo_MBA_idade_MBA_bkp$SEXO == ""]<- "M"
#table(df_snr_sexo_MBA_idade_MBA_bkp$SEXO)

df_snr_sexo_MBA_idade_MBA_bkp <- table(df_snr_sexo_MBA_idade_MBA_bkp$IDADE, df_snr_sexo_MBA_idade_MBA_bkp$SEXO, useNA ="always")
#write.csv(df_snr_sexo_MBA_idade_MBA_bkp, file ="df_snr_sexo_MBA_idade_MBA_bkp.csv",row.names=FALSE)
#write.csv(df_snr_sexo_MBA_idade_MBA_bkp, file ="df_snr_sexo_MBA_idade_MBA_bkp.csv",row.names=FALSE)
#sum(df_snr_sexo_MBA_idade_MBA_bkp)

df_snr_sexo_MBA_idade_MBA_bkp = data.frame(df_snr_sexo_MBA_idade_MBA_bkp)
#########################################################################################################
#########################################################################################################


df_snr_sexo_MBA_idade_MBA_bkp_bkp = df_snr_sexo_MBA_idade_MBA_bkp


#df_snr_sexo_MBA_idade_MBA_bkp

colnames(df_snr_sexo_MBA_idade_MBA_bkp) <- c("IDADE", "SEXO", "QUANTIDADE")

#df_snr_sexo_MBA_idade_MBA_bkp

df_snr_sexo_MBA_idade_MBA_bkp$IDADE <- as.character(df_snr_sexo_MBA_idade_MBA_bkp$IDADE)
df_snr_sexo_MBA_idade_MBA_bkp$SEXO <- as.character(df_snr_sexo_MBA_idade_MBA_bkp$SEXO)



df_snr_sexo_MBA_idade_MBA_bkp = filter(df_snr_sexo_MBA_idade_MBA_bkp, !QUANTIDADE == 0)
#df_snr_sexo_MBA_idade_MBA_bkp

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_MBA_idade_MBA_bkp$IDADE[df_snr_sexo_MBA_idade_MBA_bkp$IDADE == "SEM INFORMAÇÃO"]<- "<NA>"
df_snr_sexo_MBA_idade_MBA_bkp$IDADE[which(is.na(df_snr_sexo_MBA_idade_MBA_bkp$IDADE))] <- "s/inf"
#df_snr_sexo_MBA_idade_MBA_bkp




#df_snr_sexo_MBA_idade_MBA_bkp$IDADE <- paste(df_snr_sexo_MBA_idade_MBA_bkp$IDADE, "anos", sep=" ")
df_snr_sexo_MBA_idade_MBA_bkp$IDADE[df_snr_sexo_MBA_idade_MBA_bkp$IDADE == "s/inf anos"]<- "s/inf"

df_snr_sexo_MBA_idade_MBA_bkp <- reshape(data = df_snr_sexo_MBA_idade_MBA_bkp, idvar = "IDADE", timevar = "SEXO", direction = "wide")
df_snr_sexo_MBA_idade_MBA_bkp

colnames(df_snr_sexo_MBA_idade_MBA_bkp) <- c("IDADE", "FEMININO", "MASCULINO")
#df_snr_sexo_MBA_idade_MBA_bkp

df_snr_sexo_MBA_idade_MBA_bkp$FEMININO[which(is.na(df_snr_sexo_MBA_idade_MBA_bkp$FEMININO))] <- 0
df_snr_sexo_MBA_idade_MBA_bkp$MASCULINO[which(is.na(df_snr_sexo_MBA_idade_MBA_bkp$MASCULINO))] <- 0

#df_snr_sexo_MBA_idade_MBA_bkp
#ordenar idade
df_snr_sexo_MBA_idade_MBA_bkp = df_snr_sexo_MBA_idade_MBA_bkp %>% arrange(IDADE)

df_snr_sexo_MBA_idade_MBA_bkp$FEMININO <- as.numeric(df_snr_sexo_MBA_idade_MBA_bkp$FEMININO)
df_snr_sexo_MBA_idade_MBA_bkp$MASCULINO <- as.numeric(df_snr_sexo_MBA_idade_MBA_bkp$MASCULINO)

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
df_snr_sexo_MBA_idade_MBA_bkp$F <- round_preserve_sum(prop.table(df_snr_sexo_MBA_idade_MBA_bkp$FEMININO)*100, 2)
df_snr_sexo_MBA_idade_MBA_bkp$M <- round_preserve_sum(prop.table(df_snr_sexo_MBA_idade_MBA_bkp$MASCULINO)*100, 2)
#df_snr_sexo_MBA_idade_MBA_bkp
#########################################################################################################
colnames(df_snr_sexo_MBA_idade_MBA_bkp) <- c("IDADE", "FEM", "MAS", "F", "M")
#df_snr_sexo_MBA_idade_MBA_bkp

#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_MBA_idade_MBA_bkp_rmark = df_snr_sexo_MBA_idade_MBA_bkp

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)
df_snr_sexo_MBA_idade_MBA_bkp_rmark = filter(df_snr_sexo_MBA_idade_MBA_bkp_rmark, !IDADE == "s/inf")
#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_MBA_idade_MBA_bkp_rmark = df_snr_sexo_MBA_idade_MBA_bkp_rmark %>%
  top_n(6, MAS) %>% arrange(desc(MAS))

#somando
#sum(df_snr_sexo_MBA_idade_MBA_bkp_rmark$M)

#para escolher linhas e posicoes
#df_snr_sexo_MBA_idade_MBA_bkp_rmark[2,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

#########################################################################################################

df_snr_sexo_MBA_idade_MBA_bkp<- rbind(df_snr_sexo_MBA_idade_MBA_bkp,
                                     data.frame(IDADE = "TOTAL",
                                                FEM = sum(df_snr_sexo_MBA_idade_MBA_bkp$FEMININO),
                                                F = sum(df_snr_sexo_MBA_idade_MBA_bkp$F),
                                                MAS = sum(df_snr_sexo_MBA_idade_MBA_bkp$MASCULINO),
                                                M = sum(df_snr_sexo_MBA_idade_MBA_bkp$M),
                                                stringsAsFactors = FALSE))

#df_snr_sexo_MBA_idade_MBA_bkp

df_snr_sexo_MBA_idade_MBA_bkp =
df_snr_sexo_MBA_idade_MBA_bkp %>%
  select(IDADE, FEM, F, MAS, M)

colnames(df_snr_sexo_MBA_idade_MBA_bkp) <- c("IDADE","FEM", "%", "MAS", "%")
#df_snr_sexo_MBA_idade_MBA_bkp
#########################################################################################################
#########################################################################################################
#salvando tabela
#pdf(file="tabela_df_snr_sexo_MBA_idade_MBA_bkp_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_df_snr_sexo_MBA_idade_MBA_bkp_alternativa")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_MBA_idade_MBA = MBA_snr %>%
  select(SEXO, IDADE)

#table(df_snr_sexo_MBA_idade_MBA$SEXO)

df_snr_sexo_MBA_idade_MBA$SEXO <- as.character(df_snr_sexo_MBA_idade_MBA$SEXO)

df_snr_sexo_MBA_idade_MBA$SEXO[df_snr_sexo_MBA_idade_MBA$SEXO == ""]<- "M"
#table(df_snr_sexo_MBA_idade_MBA$SEXO)

df_snr_sexo_MBA_idade_MBA <- table(df_snr_sexo_MBA_idade_MBA$IDADE, df_snr_sexo_MBA_idade_MBA$SEXO, useNA ="always")
##write.csv(df_snr_sexo_MBA_idade_MBA, file ="df_snr_sexo_MBA_idade_MBA.csv",row.names=FALSE)
##write.csv(df_snr_sexo_MBA_idade_MBA, file ="df_snr_sexo_MBA_idade_MBA.csv",row.names=FALSE)
#sum(df_snr_sexo_MBA_idade_MBA)

df_snr_sexo_MBA_idade_MBA = data.frame(df_snr_sexo_MBA_idade_MBA)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_MBA_idade_MBA_bkp = df_snr_sexo_MBA_idade_MBA


#df_snr_sexo_MBA_idade_MBA

colnames(df_snr_sexo_MBA_idade_MBA) <- c("IDADE", "SEXO", "QUANTIDADE")

#df_snr_sexo_MBA_idade_MBA

df_snr_sexo_MBA_idade_MBA$IDADE <- as.character(df_snr_sexo_MBA_idade_MBA$IDADE)
df_snr_sexo_MBA_idade_MBA$SEXO <- as.character(df_snr_sexo_MBA_idade_MBA$SEXO)
#sum(df_snr_sexo_MBA_idade_MBA$QUANTIDADE)


df_snr_sexo_MBA_idade_MBA = filter(df_snr_sexo_MBA_idade_MBA, !QUANTIDADE == 0)
#df_snr_sexo_MBA_idade_MBA

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_MBA_idade_MBA$IDADE[df_snr_sexo_MBA_idade_MBA$IDADE == "SEM INFORMAÇÃO"]<- "<NA>"
df_snr_sexo_MBA_idade_MBA$IDADE[which(is.na(df_snr_sexo_MBA_idade_MBA$IDADE))] <- "s/inf"
#df_snr_sexo_MBA_idade_MBA


df_snr_sexo_MBA_idade_MBA$IDADE <- paste(df_snr_sexo_MBA_idade_MBA$IDADE, "anos", sep=" ")
df_snr_sexo_MBA_idade_MBA$IDADE[df_snr_sexo_MBA_idade_MBA$IDADE == "s/inf anos"]<- "s/inf"
#df_snr_sexo_MBA_idade_MBA



#########################################################################################################
# GRAFICO SEXO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_MBA = snr_banco_so_com_mba %>%
  select(SEXO)

table(df_snr_sexo_MBA$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_MBA$SEXO <- as.character(df_snr_sexo_MBA$SEXO)

df_snr_sexo_MBA$SEXO[df_snr_sexo_MBA$SEXO == ""]<- "M"
#table(df_snr_sexo_MBA$SEXO)

df_snr_sexo_MBA = data.frame(table(df_snr_sexo_MBA$SEXO))

colnames(df_snr_sexo_MBA) <- c("SEXO", "QUANTIDADE")

#sum(df_snr_sexo_MBA$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_snr_sexo_MBA$SEXO <- as.character(df_snr_sexo_MBA$SEXO)

df_snr_sexo_MBA$SEXO[df_snr_sexo_MBA$SEXO == "F"]<- "FEMININO"
df_snr_sexo_MBA$SEXO[df_snr_sexo_MBA$SEXO == "M"]<- "MASCULINO"

df_snr_sexo_MBA_original=df_snr_sexo_MBA

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
#df_snr_sexo_MBA

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_snr_sexo_MBA <- df_snr_sexo_MBA %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_snr_sexo_MBA$PERCENTUAL <- paste(df_snr_sexo_MBA$PERCENTUAL, "%", sep=" ")

#df_snr_sexo_MBA



#salvar pdf
########################################################################################################

########################################################################################################
# df_snr_regional_residencia_MBA INICIO
#########################################################################################################
##TABELA REGIONAL
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_regional_residencia_MBA = snr_banco_so_com_mba %>%
  select(REGIONAL_RESIDENCIAL)

table(df_snr_regional_residencia_MBA$REGIONAL_RESIDENCIAL, useNA ="always")
#sum(table(df_snr_regional_residencia_MBA$REGIONAL_RESIDENCIAL, useNA ="always"))
#########################################################################################################
df_snr_regional_residencia_MBA <- data.frame(table(df_snr_regional_residencia_MBA$REGIONAL_RESIDENCIAL, useNA ="always"))
df_snr_regional_residencia_MBA_original=df_snr_regional_residencia_MBA #salvando atos atendimento original
#df_snr_regional_residencia_MBA=df_snr_regional_residencia_MBA_original
#df_snr_regional_residencia_MBA$Var1 <- NULL
colnames(df_snr_regional_residencia_MBA) <- c("REGIONAL", "QUANTIDADE")

#write.csv(df_snr_regional_residencia_MBA, file = "df_snr_regional_residencia_MBA_bruto.csv", row.names = TRUE)
###write.xlsx(df_snr_regional_residencia_MBA, file = "df_snr_regional_residencia_MBA_bruto.xlsx") #salvando com modificações anteriores

#df_snr_regional_residencia_MBA$SEXO <- NULL

df_snr_regional_residencia_MBA$REGIONAL <- as.character(df_snr_regional_residencia_MBA$REGIONAL)

df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "OESTE"]<- "ALTERNATIVA"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == ""]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "#N/DISP"]<- "SEM INFORMAÇÃO"
#df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == NA]<- "SEM INFORMAÇÃO"
#df_snr_regional_residencia_MBA

df_snr_regional_residencia_MBA = filter(df_snr_regional_residencia_MBA, !QUANTIDADE == 0)
#df_snr_regional_residencia_MBA

df_snr_regional_residencia_MBA$REGIONAL2 <- ifelse(grepl("BARREIRO|CENTRO-SUL|LESTE|NORDESTE|NOROESTE|NORTE|ALTERNATIVA|PAMPULHA|VENDA NOVA|RMBH|MG|SEM INFORMAÇÃO", df_snr_regional_residencia_MBA$REGIONAL, ignore.case = TRUE),
                                                   gsub(".*(BARREIRO|CENTRO-SUL|LESTE|NORDESTE|NOROESTE|NORTE|ALTERNATIVA|PAMPULHA|VENDA NOVA|RMBH|MG|SEM INFORMAÇÃO).*", "\\1",df_snr_regional_residencia_MBA$REGIONAL), "OUTRO ESTADO")

#write.csv(df_snr_regional_residencia_MBA, file = "df_snr_regional_residencia_MBA.csv", row.names = TRUE)
###write.xlsx(df_snr_regional_residencia_MBA, file = "df_snr_regional_residencia_MBA.xlsx")


df_snr_regional_residencia_MBA$REGIONAL <- NULL

colnames(df_snr_regional_residencia_MBA) <- c("QUANTIDADE", "REGIONAL")

df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "ALTERNATIVA"]<- "OESTE"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "RMBH"]<- "REGIÃO METROPOLITANA"

df_snr_regional_residencia_MBA <- df_snr_regional_residencia_MBA[c("REGIONAL", "QUANTIDADE")]

#library(grid)
#library(gridExtra)

##JUNTANDO AS LINHAS
library(plyr)
#sum(df_snr_regional_residencia_MBA$QUANTIDADE)

df_snr_regional_residencia_MBA <- ddply(df_snr_regional_residencia_MBA,
                                        c("REGIONAL"),
                                        summarise,
                                        QUANTIDADE = sum(QUANTIDADE))

###alterando
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "OUTRA CIDADE MG"]<- "ROUTRA CIDADE MG"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "OUTRO ESTADO"]<- "ROUTRO ESTADO"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "VENDA NOVA"]<- "PVENDA NOVA"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"

df_snr_regional_residencia_MBA <-df_snr_regional_residencia_MBA[order(df_snr_regional_residencia_MBA$REGIONAL),]

df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "ROUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "ROUTRO ESTADO"]<- "OUTRO ESTADO"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "PVENDA NOVA"]<- "VENDA NOVA"
df_snr_regional_residencia_MBA$REGIONAL[df_snr_regional_residencia_MBA$REGIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#caso queira separar so regionais
#df_snr_regional_residencia_MBA1 <- df_snr_regional_residencia_MBA[!(df_snr_regional_residencia_MBA$REGIONAL == 'REGIÃO METROPOLITANA'|
#                                                      df_snr_regional_residencia_MBA$REGIONAL == 'OUTRA CIDADE MG'|
#                                                     df_snr_regional_residencia_MBA$REGIONAL == 'OUTRO ESTADO'|
#                                                    df_snr_regional_residencia_MBA$REGIONAL == 'SEM INFORMAÇÃO'),]

#acrescentando coluna com percentual
df_snr_regional_residencia_MBA$QUANTIDADE <- as.numeric(df_snr_regional_residencia_MBA$QUANTIDADE)


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


df_snr_regional_residencia_MBA$PERCENTUAL <- round_preserve_sum(prop.table(df_snr_regional_residencia_MBA$QUANTIDADE)*100, 2)

#df_snr_regional_residencia_MBA <- df_snr_regional_residencia_MBA[order(df_snr_regional_residencia_MBA[,3],decreasing=TRUE),]
#write.csv(df_snr_regional_residencia_MBA, file ="df_snr_regional_residencia_MBA.csv",row.names=TRUE)
###write.xlsx(df_snr_regional_residencia_MBA, file ="df_snr_regional_residencia_MBA.xlsx")
#df_snr_regional_residencia_MBA$QUANTIDADE <- NULL

#salvando para utilizacao graficos
df_snr_regional_residencia_MBA_bkp = df_snr_regional_residencia_MBA

#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_regional_residencia_MBA_rmark = df_snr_regional_residencia_MBA

#SEPARANDO REGIAO METROPOLITANA POR CONTA DO TEXTO NO RELATORIO
df_snr_regional_residencia_MBA_rmark1 = df_snr_regional_residencia_MBA_rmark
df_snr_regional_residencia_MBA_rmark1 = filter(df_snr_regional_residencia_MBA_rmark1, !REGIONAL == "REGIÃO METROPOLITANA")

df_snr_regional_residencia_MBA_rmark1 = df_snr_regional_residencia_MBA_rmark1 %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#SO REGIAO METROPOLITANA e outros
df_snr_regional_residencia_MBA_rmark2 = filter(df_snr_regional_residencia_MBA_rmark, REGIONAL == "REGIÃO METROPOLITANA")
df_snr_regional_residencia_MBA_rmark3 = filter(df_snr_regional_residencia_MBA_rmark, REGIONAL == "OUTRA CIDADE MG")
df_snr_regional_residencia_MBA_rmark4 = filter(df_snr_regional_residencia_MBA_rmark, REGIONAL == "OUTRO ESTADO")
df_snr_regional_residencia_MBA_rmark5 = filter(df_snr_regional_residencia_MBA_rmark, REGIONAL == "SEM INFORMAÇÃO")
#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_regional_residencia_MBA_rmark = df_snr_regional_residencia_MBA_rmark %>%
  top_n(20, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
#sum(df_snr_regional_residencia_MBA_rmark$PERCENTUAL)

#para escolher linhas e posicoes
#df_snr_regional_residencia_MBA_rmark[1,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#acrescentando linha com total
df_snr_regional_residencia_MBA <- rbind(df_snr_regional_residencia_MBA,
                                        data.frame(REGIONAL = "TOTAL", QUANTIDADE = sum(df_snr_regional_residencia_MBA$QUANTIDADE), PERCENTUAL = sum(df_snr_regional_residencia_MBA$PERCENTUAL),
                                                   stringsAsFactors = FALSE))




colnames(df_snr_regional_residencia_MBA) <- c("REGIONAL", "QUANTIDADE", "%")


#para tabela gt abaixo:

df_snr_regional_residencia_MBA_gt = df_snr_regional_residencia_MBA

########################################################################################################
#########################################################################################################

#########################################################################################################

#########################################################################################################
#FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO df_snr_regional_residencia_MBA
#df_snr_regional_residencia_MBA_original=df_snr_regional_residencia_MBA #salvando REGIONALs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#GRAFICO REGIONAL
#df_snr_regional_residencia_MBA_original=df_snr_regional_residencia_MBA #salvando REGIONALs atendimento original
df_snr_regional_residencia_MBA=df_snr_regional_residencia_MBA_bkp

df_snr_regional_residencia_MBA<-df_snr_regional_residencia_MBA%>%
  arrange(QUANTIDADE)



setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#########################################################################################################
# df_snr_regional_residencia_MBA FIM
#########################################################################################################
#########################################################################################################



#########################################################################################################

# TABELA_001
MBA_snr_bkp = MBA_snr

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_MBA_idade_MBA_tab_bkp = MBA_snr %>%
  select(SEXO)

#table(df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO <- as.character(df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO)

df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO[df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO == "F"]<- "FEMININO"
df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO[df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO == "M"]<- "MASCULINO"
#table(df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO)

df_snr_sexo_MBA_idade_MBA_tab_bkp <- table(df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO)

setwd(file.path("~/diretorio_r/estciabh/mba"))

#write.csv(df_snr_sexo_MBA_idade_MBA_tab_bkp, file ="df_snr_sexo_MBA_idade_MBA_tab_bkp.csv",row.names=FALSE)

#sum(df_snr_sexo_MBA_idade_MBA_tab_bkp)

df_snr_sexo_MBA_idade_MBA_tab_bkp = data.frame(df_snr_sexo_MBA_idade_MBA_tab_bkp)
#########################################################################################################
#########################################################################################################


df_snr_sexo_MBA_idade_MBA_tab_bkp_bkp = df_snr_sexo_MBA_idade_MBA_tab_bkp


#df_snr_sexo_MBA_idade_MBA_tab_bkp

colnames(df_snr_sexo_MBA_idade_MBA_tab_bkp) <- c("SEXO", "QUANTIDADE")

#df_snr_sexo_MBA_idade_MBA_tab_bkp


df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO <- as.character(df_snr_sexo_MBA_idade_MBA_tab_bkp$SEXO)
#sum(df_snr_sexo_MBA_idade_MBA_tab_bkp$QUANTIDADE)


df_snr_sexo_MBA_idade_MBA_tab_bkp = filter(df_snr_sexo_MBA_idade_MBA_tab_bkp, !QUANTIDADE == 0)
#df_snr_sexo_MBA_idade_MBA_tab_bkp

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_MBA_idade_MBA_tab_bkp$IDADE[which(is.na(df_snr_sexo_MBA_idade_MBA_tab_bkp$IDADE))] <- "s/inf"
df_snr_sexo_MBA_idade_MBA_tab_bkp


#df_snr_sexo_MBA_idade_MBA_tab_bkp$IDADE <- paste(df_snr_sexo_MBA_idade_MBA_tab_bkp$IDADE, "anos", sep=" ")
#df_snr_sexo_MBA_idade_MBA_tab_bkp$IDADE[df_snr_sexo_MBA_idade_MBA_tab_bkp$IDADE == "s/inf anos"]<- "s/inf"

#df_snr_sexo_MBA_idade_MBA_tab_bkp <- reshape(data = df_snr_sexo_MBA_idade_MBA_tab_bkp, idvar = "SEXO", timevar = "QUANTIDADE", direction = "wide")
#df_snr_sexo_MBA_idade_MBA_tab_bkp

#colnames(df_snr_sexo_MBA_idade_MBA_tab_bkp) <- c("IDADE", "FEM", "MAS")
#df_snr_sexo_MBA_idade_MBA_tab_bkp

#df_snr_sexo_MBA_idade_MBA_tab_bkp$FEM[which(is.na(df_snr_sexo_MBA_idade_MBA_tab_bkp$FEM))] <- 0
#df_snr_sexo_MBA_idade_MBA_tab_bkp$MAS[which(is.na(df_snr_sexo_MBA_idade_MBA_tab_bkp$MAS))] <- 0

#df_snr_sexo_MBA_idade_MBA_tab_bkp
#ordenar idade
#df_snr_sexo_MBA_idade_MBA_tab_bkp = df_snr_sexo_MBA_idade_MBA_tab_bkp %>% arrange(IDADE)

#df_snr_sexo_MBA_idade_MBA_tab_bkp$FEM <- as.numeric(df_snr_sexo_MBA_idade_MBA_tab_bkp$FEM)
#df_snr_sexo_MBA_idade_MBA_tab_bkp$MAS <- as.numeric(df_snr_sexo_MBA_idade_MBA_tab_bkp$MAS)

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
#df_snr_sexo_MBA_idade_MBA_tab_bkp$F <- round_preserve_sum(prop.table(df_snr_sexo_MBA_idade_MBA_tab_bkp$FEM)*100, 2)
#df_snr_sexo_MBA_idade_MBA_tab_bkp$M <- round_preserve_sum(prop.table(df_snr_sexo_MBA_idade_MBA_tab_bkp$MAS)*100, 2)
#df_snr_sexo_MBA_idade_MBA_tab_bkp
#########################################################################################################
#df_snr_sexo_MBA_idade_MBA_tab_bkp <- df_snr_sexo_MBA_idade_MBA_tab_bkp[c("IDADE", "FEM", "F", "MAS", "M")]
#df_snr_sexo_MBA_idade_MBA_tab_bkp
#########################################################################################################

#df_snr_sexo_MBA_idade_MBA_tab_bkp<- rbind(df_snr_sexo_MBA_idade_MBA_tab_bkp,
#                                    data.frame(IDADE = "TOTAL",
#                                              FEM = sum(df_snr_sexo_MBA_idade_MBA_tab_bkp$FEM),
#                                             F = sum(df_snr_sexo_MBA_idade_MBA_tab_bkp$F),
#                                            MAS = sum(df_snr_sexo_MBA_idade_MBA_tab_bkp$MAS),
#                                           M = sum(df_snr_sexo_MBA_idade_MBA_tab_bkp$M),
#                                          stringsAsFactors = FALSE))

#df_snr_sexo_MBA_idade_MBA_tab_bkp

#colnames(df_snr_sexo_MBA_idade_MBA_tab_bkp) <- c("IDADE", "FEM", "%", "MAS", "%")
#df_snr_sexo_MBA_idade_MBA_tab_bkp
#########################################################################################################
#########################################################################################################
###########################################################
#salvando tabela
#pdf(file="tabela_df_snr_sexo_MBA_idade_MBA_tab_bkp_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_df_snr_sexo_MBA_idade_MBA_tab_bkp_alternativa")
##setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

########################################################################################################
# MOTIVO_MBA INICIO
#########################################################################################################

MOTIVO_MBA <- data.frame(table(banco_so_com_mba$MOTIVO_MBA))

colnames(MOTIVO_MBA) <- c("MOTIVO_MBA", "QUANTIDADE")
#acrescentando coluna com percentual
MOTIVO_MBA$QUANTIDADE <- as.numeric(MOTIVO_MBA$QUANTIDADE)
#MOTIVO_MBA
MOTIVO_MBA = filter(MOTIVO_MBA, !QUANTIDADE == 0)
#MOTIVO_MBA

MOTIVO_MBA$MOTIVO_MBA <- as.character(MOTIVO_MBA$MOTIVO_MBA)

MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "NSA"]<- "NAO RESPONDEU"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "SEM INFORMAÇÃO"]<- "NAO RESPONDEU"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "NAO SABE"]<- "VNAO SABE"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "NAO RESPONDEU"]<- "VNAO RESPONDEU"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == ""]<- "VNAO RESPONDEU"

MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "DESCUMPRIMENTO DE MEIO ABERTO"]<- "DESCUMPRIMENTO DE MEDIDA EM MEIO ABERTO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "EVASAO DA SEMILIBERDADE"]<- "EVASÃO DA UNIDADE DE SEMILIBERDADE"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "EVASAO DE CENTRO DE INTERNACAO"]<- "EVASÃO DA UNIDADE DE INTERNAÇÃO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "INICIAR CUMPRIMENTO DE MEDIDA DE INTERNACAO"]<- "INICIAR CUMPRIMENTO DE MEDIDA DE INTERNAÇÃO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "INICIAR CUMPRIMENTO DE MEDIDA DE SEMILIBERDADE"]<- "INICIAR CUMPRIMENTO DE MEDIDA DE SEMILIBERDADE"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "NAO COMPARECIMENTO A AUDIENCIA"]<- "NÃO COMPARECIMENTO A AUDIÊNCIA"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "RESPONDER PELO ATO INFRACIONAL"]<- "RESPONDER PELO ATO INFRACIONAL"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "RETORNAR UNIDADE "]<- "RETORNO A UNIDADE PARA CUMPRIMENTO DA MEDIDA"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "SEM INFORAMCAO"]<- "SEM INFORMAÇÃO"

MOTIVO_MBA[order(MOTIVO_MBA$MOTIVO_MBA),]#ordenar, crescente, nome2
#MOTIVO_MBA


MOTIVO_MBA <- ddply(MOTIVO_MBA,
                    c("MOTIVO_MBA"),
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
MOTIVO_MBA$PERCENTUAL <- round_preserve_sum(prop.table(MOTIVO_MBA$QUANTIDADE)*100, 2)
#write.csv(MOTIVO_MBA, file ="MOTIVO_MBA.csv",row.names=TRUE)
###write.xlsx(MOTIVO_MBA, file = "MOTIVO_MBA.xlsx")

#MOTIVO_MBA$QUANTIDADE <- NULL



#MOTIVO_MBA


#MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == NA]<- "SEM INFORMAÇÃO"







MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "VNAO SABE"]<- "NAO SABE"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "VNAO RESPONDEU"]<- "NAO RESPONDEU"


#MOTIVO_MBA




#salvando para utilizacao graficos
MOTIVO_MBA_bkp = MOTIVO_MBA

#########################################################################################################
#########################################################################################################

#script para o bookdown

MOTIVO_MBA_rmark = MOTIVO_MBA

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
MOTIVO_MBA_rmark = MOTIVO_MBA_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(MOTIVO_MBA_rmark$QUANTIDADE)

#para escolher linhas e posicoes
MOTIVO_MBA_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
MOTIVO_MBA <- rbind(MOTIVO_MBA,
                    data.frame(MOTIVO_MBA = "TOTAL",
                               QUANTIDADE = sum(MOTIVO_MBA$QUANTIDADE),
                               PERCENTUAL = sum(MOTIVO_MBA$PERCENTUAL),
                               stringsAsFactors = FALSE))

#MOTIVO_MBA
colnames(MOTIVO_MBA) <- c("MOTIVO", "QUANTIDADE", "%")
#MOTIVO_MBA

#para tabela gt abaixo:
MOTIVO_MBA_gt = MOTIVO_MBA

#colnames(MOTIVO_MBA) <- c("MOTIVO_MBA", "%")
#MOTIVO_MBA <- MOTIVO_MBA[c("MOTIVO_MBA", "QUANTIDADE")]
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO MOTIVO_MBA
#MOTIVO_MBA_original=MOTIVO_MBA #salvando MOTIVO_MBAs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/mba"))
#########################################################################################################
MOTIVO_MBA=MOTIVO_MBA
#########################################################################################################
#########################################################################################################
#GRAFICO MOTIVO_MBA
#MOTIVO_MBA_original=MOTIVO_MBA #salvando MOTIVO_MBAs atendimento original
MOTIVO_MBA=MOTIVO_MBA_bkp


MOTIVO_MBA<-MOTIVO_MBA%>%
  arrange(PERCENTUAL)



setwd(file.path("~/diretorio_r/estciabh/mba"))
#########################################################################################################
# MOTIVO_MBA FIM
#########################################################################################################

#########################################################################################################
# banco_ato_MBA INICIO
#########################################################################################################
#########################################################################################################
banco_ato_MBA = banco_so_com_mba
#########################################################################################################

#########################################################################################################
banco_ato_MBA$ATO_INFRACIONAL_MBA <- gsub(" ","", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#AMEAÇA

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "147.ARTCPB",
                                           "AMEAÇA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


########################################################################################################
#########################################################################################################
#CRIME DE TRÂNSITO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "309.ARTCTB",
                                           "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "310.ARTCTB",
                                           "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "311.ARTCTB",
                                           "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#para discrinar: é so anular com #

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)",
                                           "CRIME DE TRÂNSITO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)",
                                           "CRIME DE TRÂNSITO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)",
                                           "CRIME DE TRÂNSITO", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#########################################################################################################
#DANO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "163.ARTCPB",
                                           "DANO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "213.ARTCPB",
                                           "ESTUPRO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "215.ARTCPB",
                                           "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "215-A.ARTCPB",
                                           "IMPORTUNAÇÃO SEXUAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ESTUPRO DE VULNERÁVEL

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "217-A.ARTCPB",
                                           "ESTUPRO DE VULNERÁVEL", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#FURTO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "155.ARTCPB",
                                           "FURTO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#FURTO (TENTATIVA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "155C/C14.ARTCPB",
                                           "FURTO (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "121.ARTCPB",
                                           "HOMICÍDIO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO (TENTATIVA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "121C/C14,II.ARTCPB",
                                           "HOMICÍDIO (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "129.ARTCPB",
                                           "LESÃO CORPORAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "129§3º.ARTCPB",
                                           "LESÃO CORPORAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "129§9º.ARTCPB",
                                           "LESÃO CORPORAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL (TENTATIVA). Ordem para trocar LESÃO CORPORAL (TENTATIVA) por VIAS DE FATO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "129C/C14,II.ARTCPB",
                                           "VIAS DE FATO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#PICHAÇÃO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "65.ART9.605",
                                           "PICHAÇÃO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#PORTE/POSSE DE ARMA

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "12.ART10.826",
                                           "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "14.ART10.826",
                                           "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "16.ART10.826",
                                           "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "15.ART10.826",
                                           "ARMA DE FOGO - PORTE ILEGAL (DISPARO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "17.ART10.826",
                                           "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)", banco_ato_MBA$ATO_INFRACIONAL_MBA)



banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - PORTE ILEGAL (DISPARO)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#RECEPTAÇÃO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "180.ARTCPB",
                                           "RECEPTAÇÃO", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#########################################################################################################
#ROUBO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157.ARTCPB",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAICPB",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAIIARTCPB",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2º,I,IIeVARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2º,I.ARTCPB",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2º,IeII.ARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºA,IARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIeVARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºA,I",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAIARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAIeIIARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºII,VeVIIARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIeVIARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)



banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIeVIIARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIEVIIARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºVARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºVIIARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAIICPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#para discrinar: é so anular com #

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EM CONCURSO DE PESSOAS)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EM CONCURSO DE PESSOAS)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EMPREGO DE ARMA)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EM CONCURSO DE PESSOAS)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EMPREGO DE ARMA)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)



#########################################################################################################
#ROUBO (TENTATIVA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2º,IeIIC/C14,II.ARTCPB",
                                           "ROUBO (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157C/C14,II.ARTCPB",
                                           "ROUBO (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#ROUBO (§3º)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§3ºARTCPB",
                                           "ROUBO (§3º)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (§3º)",
                                           "LATROCÍNIO", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#########################################################################################################
#ROUBO (§3º) (TENTATIVA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§3ºARTCPBC/C14,II,CPB",
                                           "ROUBO (§3º) (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#SEQUESTRO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "148.ARTCPB",
                                           "SEQUESTRO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#TRÁFICO DE DROGAS


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "33.ART11.343",
                                           "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "37.ART11.343",
                                           "TRÁFICO DE DROGAS (INFORMANTE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)



#para discrinar: é so anular com #

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "TRÁFICO DE DROGAS",
                                           "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "TRÁFICO DE DROGAS (INFORMANTE)",
                                           "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)





#########################################################################################################
#########################################################################################################
#########################################################################################################
#ASSOCIAÇÃO TRÁFICO DE DROGAS


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "35.ART11.343",
                                           "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "34.ART11.343",
                                           "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#para discrinar: é so anular com #

#banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "TRÁFICO DE DROGAS (ASSOCIAÇÃO)",
#                                           "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#USO DE DROGAS

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "28.ART11.343",
                                           "POSSE DE DROGAS PARA USO PESSOAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)



#########################################################################################################
#########################################################################################################
#VIAS DE FATO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "21.ARTLCP",
                                           "VIAS DE FATO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#OUTROS

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "140.ARTCPB",
                                           "INJÚRIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "140§3º.ARTCPB",
                                           "INJÚRIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)



banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "330.ARTCPB",
                                           "DESOBEDIÊNCIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "331.ARTCPB",
                                           "DESACATO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "139.ARTCPB",
                                           "DESOBEDIÊNCIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "2.ART13.185",
                                           "INTIMIDAÇÃO SISTEMÁTICA (BULLYING)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "329.ARTCPB",
                                           "RESISTÊNCIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)



#table(banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "NSA",
                                           "SEM INFORMAÇÃO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "TERMOSEMINF.",
                                           "SEM INFORMAÇÃO", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#table(banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#########################################################################################################

###EXCLUIR LINHA DESCONSIDERAR AO SOMAR
#########################################################################################################
#Encontrando OS VARIADOS ARTIGOS QUE SOBRARAM e os que já estao como VOUTROS e os colocando na NOVA COLUNA ATO_INFRACIONAL_MBA_INFRACIONAL_MBA2

#########################################################################################################
#excluir coluna
#banco_ato_MBA  <- banco_ato_MBA[order(banco_ato_MBA[,2],decreasing=FALSE),]
banco_ato_MBA = arrange(banco_ato_MBA, ATO_INFRACIONAL_MBA)

###renomeando:
#banco_ato_MBA <- banco_ato_MBA[!(banco_ato_MBA$QUANTIDADE == "0"),]
#banco_ato_MBA <- banco_ato_MBA[!(banco_ato_MBA$ATO_INFRACIONAL_MBA == "DESCONSIDERAR AO SOMAR"),]
#banco_ato_MBA = data.frame(table(banco_ato_MBA$ATO_INFRACIONAL_MBA_INFRACIONAL_MBA))
banco_ato_MBA = data.frame(table(banco_ato_MBA$ATO_INFRACIONAL_MBA))
#banco_ato_MBA = as_tibble(table(banco_ato_MBA$ATO_INFRACIONAL_MBA))
#ordenar coluna para facilitar visualizacao na soma com tabela dinamica
banco_ato_MBA$Var1 <- as.character(banco_ato_MBA$Var1)

banco_ato_MBA$Var1[banco_ato_MBA$Var1 == "SEM INFORMAÇÃO"]<- "VSEM INFORMAÇÃO"
banco_ato_MBA$Var1[banco_ato_MBA$Var1 == "OUTROS"]<- "VOUTROS"

banco_ato_MBA<-banco_ato_MBA%>%
  arrange(Var1)

banco_ato_MBA$Var1[banco_ato_MBA$Var1 == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ato_MBA$Var1[banco_ato_MBA$Var1 == "VOUTROS"]<- "OUTROS"

#write.csv(banco_ato_MBA, file ="banco_ato_MBA.csv",row.names=FALSE)

#SALVAR CSV NO DIRETORIO RAIZ
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#write.csv(banco_ato_MBA, file ="banco_ato_MBA.csv",row.names=FALSE)

#VOLTAR PAAR O DIRETORIO PADRAO
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

##write.xlsx(banco_ato_MBA, file ="banco_ato_MBA.xlsx")

#########################################################################################################
#########################################################################################################

#########################################################################################################
##TABELA ENVOLVIMENTOS ATO_INFRACIONAL_MBAs Infracionais

colnames(banco_ato_MBA) <- c("ATO_INFRACIONAL_MBA", "QUANTIDADE")

banco_ato_MBA_bkp = banco_ato_MBA #salvando ATO_INFRACIONAL_MBAs atendimento original

#library(grid)
#library(gridExtra)

#acrescentando coluna com percentual
banco_ato_MBA$QUANTIDADE <- as.numeric(banco_ato_MBA$QUANTIDADE)

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
#banco_ato_MBA$PERCENTUAL <- round(prop.table(banco_ato_MBA$QUANTIDADE)*100, 2)
banco_ato_MBA$PERCENTUAL <- round(prop.table(banco_ato_MBA$QUANTIDADE)*100, 2)

#script para o bookdown

banco_ato_MBA_rmark = banco_ato_MBA

#banco_ato_MBA_rmark <- banco_ato_MBA_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_ato_MBA_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
banco_ato_MBA_rmark = banco_ato_MBA_rmark %>%
  top_n(5, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
#sum(banco_ato_MBA_rmark$PERCENTUAL)

#para escolher linhas e posicoes
#banco_ato_MBA_rmark[1,2]
#outra forma de calcular percentual
#banco_ato_MBA = mutate(banco_ato_MBA,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)




#acrescentando linha com total
banco_ato_MBA <- rbind(banco_ato_MBA,
                       data.frame(ATO_INFRACIONAL_MBA = "TOTAL", QUANTIDADE = sum(banco_ato_MBA$QUANTIDADE), PERCENTUAL = sum(round_preserve_sum(banco_ato_MBA$PERCENTUAL, 0)),
                                  stringsAsFactors = FALSE))


colnames(banco_ato_MBA) <- c("ATO_INFRACIONAL_MBA", "QUANTIDADE", "%")

##write.xlsx(banco_ato_MBA, file = "banco_ato_MBA_total.xlsx") #salvando para usar na comparada
#write.csv(banco_ato_MBA, file = "banco_ato_MBA_total.csv", row.names=FALSE) #salvando com modificações anteriores


#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
banco_ato_MBA=banco_ato_MBA_bkp

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
#banco_ato_MBA$PERCENTUAL <- round(prop.table(banco_ato_MBA$QUANTIDADE)*100, 2)
banco_ato_MBA$PERCENTUAL <- round_preserve_sum(prop.table(banco_ato_MBA$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#banco_ato_MBA = mutate(banco_ato_MBA,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)






banco_ato_MBA$ATO_INFRACIONAL_MBA <- as.character(banco_ato_MBA$ATO_INFRACIONAL_MBA)

#NA POR ZERO

banco_ato_MBA = replace(x = banco_ato_MBA, list = is.na(banco_ato_MBA), values = 0)

banco_ato_MBA_bkp = banco_ato_MBA
#acrescentando linha com total
banco_ato_MBA <- rbind(banco_ato_MBA,
                       data.frame(ATO_INFRACIONAL_MBA = "TOTAL",
                                  QUANTIDADE = sum(banco_ato_MBA$QUANTIDADE),
                                  PERCENTUAL = sum(banco_ato_MBA$PERCENTUAL),
                                  stringsAsFactors = FALSE))

#banco_ato_MBA$VAR <- round(((banco_ato_MBA$ANOATUAL*100)/banco_ato_MBA$ANOANTERIOR)-100, 2)



#colnames(banco_ato_MBA) <- c("ATO_INFRACIONAL_MBA", format(Sys.Date()-365*2, "%Y"), format(Sys.Date()-365*1, "%Y"), "VAR%")

#TESTES TABELA GT:
colnames(banco_ato_MBA) <- c("ATO INFRACIONAL", "QUANTIDADE", "%")

banco_ato_MBA_gt = banco_ato_MBA



#########################################################################################################


#########################################################################################################
#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
##########################################################################################################
# banco_ato_MBA FIM
#########################################################################################################
###########################################################################################################
#df_regional_banco_MBA INICIO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)


df_regional_banco_MBA <- banco_so_com_mba %>%
  select(DIA_SEMANA_ATO, REGIONAL_ATO)

#names(df_regional_banco_MBA)


#table(df_regional_banco_MBA$REGIONAL_ATO, useNA ="always")
#sum(table(df_regional_banco_MBA$REGIONAL_ATO, useNA ="always"))
#########################################################################################################
df_regional_banco_MBA <- data.frame(table(df_regional_banco_MBA$REGIONAL_ATO, useNA ="always"))
df_regional_banco_MBA_original=df_regional_banco_MBA #salvando atos atendimento original
#df_regional_banco_MBA=df_regional_banco_MBA_original
#df_regional_banco_MBA$Var1 <- NULL
colnames(df_regional_banco_MBA) <- c("REGIONAL", "QUANTIDADE")

##write.xlsx(df_regional_banco_MBA, file = "df_regional_banco_MBA_bruto.xlsx") #salvando com modificações anteriores

#df_regional_banco_MBA$SEXO <- NULL

df_regional_banco_MBA$REGIONAL <- as.character(df_regional_banco_MBA$REGIONAL)

#df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "OESTE"]<- "ALTERNATIVA"
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == ""]<- "SEM INFORMAÇÃO"
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "#N/DISP"]<- "SEM INFORMAÇÃO"
#df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == NA]<- "SEM INFORMAÇÃO"
#df_regional_banco_MBA

df_regional_banco_MBA = filter(df_regional_banco_MBA, !QUANTIDADE == 0)
#df_regional_banco_MBA

#df_regional_banco_MBA$REGIONAL2 <- ifelse(grepl("BARREIRO|CENTRO-SUL|LESTE|NORDESTE|NOROESTE|NORTE|ALTERNATIVA|PAMPULHA|VENDA NOVA|RMBH|MG|SEM INFORMAÇÃO", df_regional_banco_MBA$REGIONAL, ignore.case = TRUE),
#gsub(".*(BARREIRO|CENTRO-SUL|LESTE|NORDESTE|NOROESTE|NORTE|ALTERNATIVA|PAMPULHA|VENDA NOVA|RMBH|MG|SEM INFORMAÇÃO).*", "\\1",df_regional_banco_MBA$REGIONAL), "OUTRO ESTADO")

df_regional_banco_MBA$REGIONAL2 <- ifelse(grepl("MG", df_regional_banco_MBA$REGIONAL, ignore.case = T),
                                          gsub(".*(BARREIRO|CENTRO-SUL|LESTE|NORDESTE|NOROESTE|NORTE|ALTERNATIVA|PAMPULHA|VENDA NOVA|RMBH|MG|SEM INFORMAÇÃO).*", "\\1",df_regional_banco_MBA$REGIONAL), df_regional_banco_MBA$REGIONAL)




df_regional_banco_MBA$REGIONAL2 <- str_replace(df_regional_banco_MBA$REGIONAL2, "RMBH.*", "REGIÃO METROPOLITANA")
df_regional_banco_MBA$REGIONAL2 <- str_replace(df_regional_banco_MBA$REGIONAL2, "SEM.*", "SEM INFORMAÇÃO")
df_regional_banco_MBA$REGIONAL2 <- str_replace(df_regional_banco_MBA$REGIONAL2, "MG.*", "OUTRA CIDADE MG")

df_regional_banco_MBA$REGIONAL <- NULL

colnames(df_regional_banco_MBA) <- c("QUANTIDADE", "REGIONAL")

df_regional_banco_MBA =
  df_regional_banco_MBA %>%
  select(REGIONAL, QUANTIDADE)

#library(grid)
#library(gridExtra)

##JUNTANDO AS LINHAS
library(plyr)
#sum(df_regional_banco_MBA$QUANTIDADE)

df_regional_banco_MBA <- ddply(df_regional_banco_MBA,
                               c("REGIONAL"),
                               summarise,
                               QUANTIDADE = sum(QUANTIDADE))

###alterando
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "OUTRA CIDADE MG"]<- "ROUTRA CIDADE MG"
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "OUTRO ESTADO"]<- "ROUTRO ESTADO"
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "VENDA NOVA"]<- "PVENDA NOVA"
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"

df_regional_banco_MBA <-df_regional_banco_MBA[order(df_regional_banco_MBA$REGIONAL),]

df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "ROUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "ROUTRO ESTADO"]<- "OUTRO ESTADO"
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_MBA$REGIONAL[df_regional_banco_MBA$REGIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#caso queira separar so regionais
#df_regional_banco_MBA1 <- df_regional_banco_MBA[!(df_regional_banco_MBA$REGIONAL == 'REGIÃO METROPOLITANA'|
#                                                      df_regional_banco_MBA$REGIONAL == 'OUTRA CIDADE MG'|
#                                                     df_regional_banco_MBA$REGIONAL == 'OUTRO ESTADO'|
#                                                    df_regional_banco_MBA$REGIONAL == 'SEM INFORMAÇÃO'),]

#acrescentando coluna com percentual
df_regional_banco_MBA$QUANTIDADE <- as.numeric(df_regional_banco_MBA$QUANTIDADE)


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


df_regional_banco_MBA$PERCENTUAL <- round_preserve_sum(prop.table(df_regional_banco_MBA$QUANTIDADE)*100, 2)
#df_regional_banco_MBA
#df_regional_banco_MBA <- df_regional_banco_MBA[order(df_regional_banco_MBA[,3],decreasing=TRUE),]
#write.csv(df_regional_banco_MBA, file ="df_regional_banco_MBA.csv",row.names=TRUE)
##write.xlsx(df_regional_banco_MBA, file ="df_regional_banco_MBA.xlsx")
#df_regional_banco_MBA$QUANTIDADE <- NULL

#salvando para utilizacao graficos
df_regional_banco_MBA<-df_regional_banco_MBA %>%
  arrange(desc(QUANTIDADE))
df_regional_banco_MBA_bkp = df_regional_banco_MBA


#########################################################################################################
#########################################################################################################

#script para o bookdown

df_regional_banco_MBA_rmark = df_regional_banco_MBA

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_regional_banco_MBA_rmark = df_regional_banco_MBA_rmark %>%
  top_n(6, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(df_regional_banco_MBA_rmark$PERCENTUAL)

#para escolher linhas e posicoes
df_regional_banco_MBA_rmark[1,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

#acrescentando linha com total
df_regional_banco_MBA <- rbind(df_regional_banco_MBA,
                               data.frame(REGIONAL = "TOTAL", QUANTIDADE = sum(df_regional_banco_MBA$QUANTIDADE), PERCENTUAL = sum(df_regional_banco_MBA$PERCENTUAL),
                                          stringsAsFactors = FALSE))


colnames(df_regional_banco_MBA) <- c("REGIONAL", "QUANTIDADE", "%")

#para tabela gt abaixo:
df_regional_banco_MBA_gt = df_regional_banco_MBA

#colnames(df_regional_banco_MBA) <- c("REGIONAL", "%")
#df_regional_banco_MBA <- df_regional_banco_MBA[c("REGIONAL", "QUANTIDADE")]
#salvando tabela
#########################################################################################################
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################
#df_regional_banco_MBA FIM
#################################################################################################################################################################
##########################################################################################################################################################
#df_dia_semana_banco_MBA INICIO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

#banco_HOMICIDIO_sem_concurso <- banco_HOMICIDIO_puro[!duplicated(data.frame(banco_HOMICIDIO_puro$PROCESSO, banco_HOMICIDIO_puro$ATO_INFRACIONAL)),]
df_dia_semana_banco_MBA <- banco_so_com_mba %>%
  select(DIA_SEMANA_ATO, REGIONAL_ATO)


#table(df_dia_semana_banco_MBA$DIA_SEMANA_ATO, useNA ="always")
#sum(table(df_dia_semana_banco_MBA$DIA_SEMANA_ATO, useNA ="always"))
#########################################################################################################
df_dia_semana_banco_MBA <- data.frame(table(df_dia_semana_banco_MBA$DIA_SEMANA_ATO, useNA ="always"))
df_dia_semana_banco_MBA_original=df_dia_semana_banco_MBA #salvando atos atendimento original
#df_dia_semana_banco_MBA=df_dia_semana_banco_MBA_original
#df_dia_semana_banco_MBA$Var1 <- NULL
colnames(df_dia_semana_banco_MBA) <- c("DIA", "QUANTIDADE")

##write.xlsx(df_dia_semana_banco_MBA, file = "df_dia_semana_banco_MBA_bruto.xlsx") #salvando com modificações anteriores

#df_dia_semana_banco_MBA$SEXO <- NULL

#df_dia_semana_banco_MBA

df_dia_semana_banco_MBA = filter(df_dia_semana_banco_MBA, !QUANTIDADE == 0)


##write.xlsx(df_dia_semana_banco_MBA, file = "df_dia_semana_banco_MBA.xlsx")


#df_dia_semana_banco_MBA$DIA <- NULL

#colnames(df_dia_semana_banco_MBA) <- c("QUANTIDADE", "DIA")

df_dia_semana_banco_MBA <- df_dia_semana_banco_MBA[c("DIA", "QUANTIDADE")]


#library(grid)
#library(gridExtra)

##JUNTANDO AS LINHAS
library(plyr)
#sum(df_dia_semana_banco_MBA$QUANTIDADE)

df_dia_semana_banco_MBA <- ddply(df_dia_semana_banco_MBA,
                                 c("DIA"),
                                 summarise,
                                 QUANTIDADE = sum(QUANTIDADE))

#df_dia_semana_banco_MBA


#caso queira separar so regionais
#df_dia_semana_banco_MBA1 <- df_dia_semana_banco_MBA[!(df_dia_semana_banco_MBA$DIA == 'REGIÃO METROPOLITANA'|
#                                                      df_dia_semana_banco_MBA$DIA == 'OUTRA CIDADE MG'|
#                                                     df_dia_semana_banco_MBA$DIA == 'OUTRO ESTADO'|
#                                                    df_dia_semana_banco_MBA$DIA == 'SEM INFORMAÇÃO'),]

#acrescentando coluna com percentual
df_dia_semana_banco_MBA$QUANTIDADE <- as.numeric(df_dia_semana_banco_MBA$QUANTIDADE)


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


df_dia_semana_banco_MBA$PERCENTUAL <- round(prop.table(df_dia_semana_banco_MBA$QUANTIDADE)*100, 2)




#df_dia_semana_banco_MBA
#df_dia_semana_banco_MBA <- df_dia_semana_banco_MBA[order(df_dia_semana_banco_MBA[,3],decreasing=TRUE),]
#write.csv(df_dia_semana_banco_MBA, file ="df_dia_semana_banco_MBA.csv",row.names=TRUE)
##write.xlsx(df_dia_semana_banco_MBA, file ="df_dia_semana_banco_MBA.xlsx")
#df_dia_semana_banco_MBA$QUANTIDADE <- NULL

#salvando para utilizacao graficos
df_dia_semana_banco_MBA<-df_dia_semana_banco_MBA %>%
  arrange(desc(QUANTIDADE))
df_dia_semana_banco_MBA_bkp = df_dia_semana_banco_MBA



df_dia_semana_banco_MBA$DIA <- as.character(df_dia_semana_banco_MBA$DIA)

#df_dia_semana_banco_MBA

df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "segunda"]<- "ASEGUNDA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "terça"]<- "BTERÇA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "quarta"]<- "CQUARTA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "quinta"]<- "DQUINTA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "sexta"]<- "ESEXTA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "sábado"]<- "FSABADO"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "domingo"]<- "GDOMINGO"

#df_dia_semana_banco_MBA

df_dia_semana_banco_MBA <-df_dia_semana_banco_MBA[order(df_dia_semana_banco_MBA$DIA),]

#df_dia_semana_banco_MBA

df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "ASEGUNDA"]<- "SEGUNDA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "BTERÇA"]<- "TERÇA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "CQUARTA"]<- "QUARTA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "DQUINTA"]<- "QUINTA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "ESEXTA"]<- "SEXTA"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "FSABADO"]<- "SABADO"
df_dia_semana_banco_MBA$DIA[df_dia_semana_banco_MBA$DIA == "GDOMINGO"]<- "DOMINGO"


#df_dia_semana_banco_MBA


#########################################################################################################
#########################################################################################################

#script para o bookdown

df_dia_semana_banco_MBA_rmark = df_dia_semana_banco_MBA

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_dia_semana_banco_MBA_rmark = df_dia_semana_banco_MBA_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
#sum(df_dia_semana_banco_MBA_rmark$PERCENTUAL)

#para escolher linhas e posicoes
#df_dia_semana_banco_MBA_rmark[1,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################



#acrescentando linha com total
df_dia_semana_banco_MBA <- rbind(df_dia_semana_banco_MBA,
                                 data.frame(DIA = "TOTAL", QUANTIDADE = sum(df_dia_semana_banco_MBA$QUANTIDADE), PERCENTUAL = sum(round_preserve_sum(df_dia_semana_banco_MBA$PERCENTUAL, 0)),
                                            stringsAsFactors = FALSE))

#df_dia_semana_banco_MBA

colnames(df_dia_semana_banco_MBA) <- c("DIA", "QUANTIDADE", "%")

#para a tabele gt:
df_dia_semana_banco_MBA_gt = df_dia_semana_banco_MBA

#colnames(df_dia_semana_banco_MBA) <- c("DIA", "%")
#df_dia_semana_banco_MBA <- df_dia_semana_banco_MBA[c("DIA", "QUANTIDADE")]
#salvando tabela
#########################################################################################################
#pdf(file=TABELA[3,],"_004_dia_semana_HOMICIDIO_alternativa.pdf",  width = 6, height = 4.8, title = "df_dia_semana_banco_MBA")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################
#########################################################################################################
#df_dia_semana_banco_MBA FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
