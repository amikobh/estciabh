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


#########################################################################################################
#df_snr_regional_residencia_MBA
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_regional_residencia_MBA =
  snr_banco_so_com_mba %>%
  select(REGIONAL_RESIDENCIAL)

colnames(df_snr_regional_residencia_MBA)[1]<-'regional_residencial'

#########################################################################################################
#encontrando parte do texto e substituindo
df_snr_regional_residencia_MBA$regional_residencial[agrep("/MG", df_snr_regional_residencia_MBA$regional_residencial)] <- "UOUTRA CIDADE MG"
df_snr_regional_residencia_MBA$regional_residencial[agrep("RMBH", df_snr_regional_residencia_MBA$regional_residencial)] <- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA$regional_residencial[agrep("RIBEIRAO DAS NEVES", df_snr_regional_residencia_MBA$regional_residencial)] <- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA$regional_residencial[agrep("CATAGUASES", df_snr_regional_residencia_MBA$regional_residencial)] <- "UOUTRA CIDADE MG"
df_snr_regional_residencia_MBA$regional_residencial[agrep("CIDADE DE BRASILIA/DF", df_snr_regional_residencia_MBA$regional_residencial)] <- "VOUTRO ESTADO"
df_snr_regional_residencia_MBA$regional_residencial[agrep("N/DISP", df_snr_regional_residencia_MBA$regional_residencial)] <- "ZSEM INFORMAÇÃO"
df_snr_regional_residencia_MBA$regional_residencial[agrep("INFORMACAO", df_snr_regional_residencia_MBA$regional_residencial)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == ""]<- "ZSEM INFORMAÇÃO"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "PAMPULHA"]<- "OESTEPAMPULHA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "VENDA NOVA"]<- "PVENDA NOVA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "REGIÃO METROPOLITANA"]<- "QREGIÃO METROPOLITANA"

#########################################################################################################

# salvando para gráfico
df_snr_regional_residencia_MBA_bkp = df_snr_regional_residencia_MBA

df_snr_regional_residencia_MBA_bkp =
  df_snr_regional_residencia_MBA_bkp %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "OESTEPAMPULHA"]<- "PAMPULHA"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "PVENDA NOVA"]<- "VENDA NOVA"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_snr_regional_residencia_MBA_bkp)[1]<-'df_snr_regional_residencia_MBA_bkp'
colnames(df_snr_regional_residencia_MBA_bkp)[2]<-'QUANTIDADE'
colnames(df_snr_regional_residencia_MBA_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_snr_regional_residencia_MBA =
  df_snr_regional_residencia_MBA %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(regional_residencial) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "OESTEPAMPULHA"]<- "PAMPULHA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "PVENDA NOVA"]<- "VENDA NOVA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_snr_regional_residencia_MBA)[1]<-'REGIONAL'
colnames(df_snr_regional_residencia_MBA)[2]<-'QUANTIDADE'
colnames(df_snr_regional_residencia_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#df_snr_regional_residencia_MBA =
#  df_snr_regional_residencia_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_snr_regional_residencia_MBA FIM
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

#########################################################################################################
#MOTIVO_MBA
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

MOTIVO_MBA =
  banco_so_com_mba %>%
  select(MOTIVO_MBA)

#colnames(MOTIVO_MBA)[1]<-'regional_residencial'
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "DESCUMPRIMENTO DE MEIO ABERTO"]<- "DESCUMPRIMENTO DE MEDIDA EM MEIO ABERTO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "EVASAO DA SEMILIBERDADE"]<- "EVASÃO DA UNIDADE DE SEMILIBERDADE"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "EVASAO DE CENTRO DE INTERNACAO"]<- "EVASÃO DA UNIDADE DE INTERNAÇÃO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "INICIAR CUMPRIMENTO DE MEDIDA DE INTERNACAO"]<- "INICIAR CUMPRIMENTO DE MEDIDA DE INTERNAÇÃO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "NAO COMPARECIMENTO A AUDIENCIA"]<- "NÃO COMPARECIMENTO A AUDIÊNCIA"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "RETORNAR UNIDADE "]<- "RETORNO A UNIDADE PARA CUMPRIMENTO DA MEDIDA"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "SEM INFORAMCAO"]<- "SEM INFORMAÇÃO"
#MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#encontrando parte do texto e substituindo
#########################################################################################################

# salvando para gráfico
MOTIVO_MBA_bkp = MOTIVO_MBA

MOTIVO_MBA_bkp =
  MOTIVO_MBA_bkp %>%
  janitor::tabyl(MOTIVO_MBA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
MOTIVO_MBA_bkp$MOTIVO_MBA[MOTIVO_MBA_bkp$MOTIVO_MBA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
MOTIVO_MBA_bkp$MOTIVO_MBA[MOTIVO_MBA_bkp$MOTIVO_MBA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#MOTIVO_MBA_bkp$regional_residencial[MOTIVO_MBA_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

colnames(MOTIVO_MBA_bkp)[1]<-'MOTIVO_MBA_bkp'
colnames(MOTIVO_MBA_bkp)[2]<-'QUANTIDADE'
colnames(MOTIVO_MBA_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

MOTIVO_MBA =
  MOTIVO_MBA %>%
  janitor::tabyl(MOTIVO_MBA) %>%
  arrange(MOTIVO_MBA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR

colnames(MOTIVO_MBA)[1]<-'MOTIVO'
colnames(MOTIVO_MBA)[2]<-'QUANTIDADE'
colnames(MOTIVO_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#MOTIVO_MBA =
#  MOTIVO_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# MOTIVO_MBA FIM
#########################################################################################################
#########################################################################################################
# banco_ato_MBA
#########################################################################################################
#########################################################################################################

library(dplyr)

banco_ato_MBA =
  banco_so_com_mba |>
  select(ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = gsub(" ","", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
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

#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_ato_MBA_bkp = banco_ato_MBA

banco_ato_MBA_bkp =
  banco_ato_MBA_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL_MBA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
banco_ato_MBA_bkp$ATO_INFRACIONAL_MBA[banco_ato_MBA_bkp$ATO_INFRACIONAL_MBA == "VS/INF"]<- "SEM INFORMAÇÃO"
banco_ato_MBA_bkp$ATO_INFRACIONAL_MBA[banco_ato_MBA_bkp$ATO_INFRACIONAL_MBA == "VOUTROS"]<- "OUTROS"


colnames(banco_ato_MBA_bkp)[1]<-'banco_ato_MBA_bkp'
colnames(banco_ato_MBA_bkp)[2]<-'QUANTIDADE'
colnames(banco_ato_MBA_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#para script rmd:
banco_ato_MBA_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ato_MBA_bkp$PERCENTUAL))
banco_ato_MBA_bkp_rmd = tail(banco_ato_MBA_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_ato_MBA =
  banco_ato_MBA %>%
  janitor::tabyl(ATO_INFRACIONAL_MBA) %>%
  arrange(ATO_INFRACIONAL_MBA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
banco_ato_MBA$ATO_INFRACIONAL_MBA[banco_ato_MBA$ATO_INFRACIONAL_MBA == "VS/INF"]<- "SEM INFORMAÇÃO"
banco_ato_MBA$ATO_INFRACIONAL_MBA[banco_ato_MBA$ATO_INFRACIONAL_MBA == "VOUTROS"]<- "OUTROS"



colnames(banco_ato_MBA)[1]<-'ATO INFRACIONAL'
colnames(banco_ato_MBA)[2]<-'QUANTIDADE'
colnames(banco_ato_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#banco_ato_MBA =
#  banco_ato_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
write.csv(banco_ato_MBA, file ="banco_ato_MBA_atual.csv",row.names=FALSE)
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ato_MBA FIM
#########################################################################################################
#########################################################################################################
# df_regional_ATO_banco_MBA
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_regional_ATO_banco_MBA =
  banco_so_com_mba %>%
  select(REGIONAL_ATO)

colnames(df_regional_ATO_banco_MBA)[1]<-'REGIONAL_ATO'

#########################################################################################################
#encontrando parte do texto e substituindo
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("/MG", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("RMBH", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("RIBEIRAO DAS NEVES", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("CATAGUASES", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("CIDADE DE BRASILIA/DF", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "VOUTRO ESTADO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("N/DISP", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("INFORMACAO", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == ""]<- "SEM INFORMAÇÃO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "PAMPULHA"]<- "PAMPULHA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "VENDA NOVA"]<- "VENDA NOVA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "REGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#########################################################################################################

# salvando para gráfico
df_regional_ATO_banco_MBA_bkp = df_regional_ATO_banco_MBA

df_regional_ATO_banco_MBA_bkp =
  df_regional_ATO_banco_MBA_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)


colnames(df_regional_ATO_banco_MBA_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_ATO_banco_MBA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_regional_ATO_banco_MBA_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_regional_ATO_banco_MBA_bkp$PERCENTUAL))
df_regional_ATO_banco_MBA_bkp_rmd = tail(df_regional_ATO_banco_MBA_bkp,5)
#########################################################################################################
# Adaptando para scrip grafico:
#SUBSTITUIR
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_regional_ATO_banco_MBA_bkp)[1]<-'df_regional_ATO_banco_MBA_bkp'
colnames(df_regional_ATO_banco_MBA_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_ATO_banco_MBA_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_regional_ATO_banco_MBA =
  df_regional_ATO_banco_MBA %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_regional_ATO_banco_MBA)[1]<-'REGIONAL'
colnames(df_regional_ATO_banco_MBA)[2]<-'QUANTIDADE'
colnames(df_regional_ATO_banco_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#df_regional_ATO_banco_MBA =
#  df_regional_ATO_banco_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_regional_ATO_banco_MBA FIM
#########################################################################################################
#########################################################################################################
# df_DIA_SEMANA_banco_MBA
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_DIA_SEMANA_banco_MBA =
  banco_so_com_mba %>%
  select(DIA_SEMANA_ATO)

colnames(df_DIA_SEMANA_banco_MBA)[1]<-'DIA_SEMANA'

#########################################################################################################

df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "segunda"]<- "SEGUNDA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "terça"]<- "TERÇA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "quarta"]<- "QUARTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "quinta"]<- "QUINTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "sexta"]<- "SEXTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "sábado"]<- "SÁBADO"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "domingo"]<- "DOMINGO"
#########################################################################################################

# salvando para gráfico
df_DIA_SEMANA_banco_MBA_bkp = df_DIA_SEMANA_banco_MBA

df_DIA_SEMANA_banco_MBA_bkp =
  df_DIA_SEMANA_banco_MBA_bkp %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

colnames(df_DIA_SEMANA_banco_MBA_bkp)[1]<-'df_DIA_SEMANA_banco_MBA_bkp'
colnames(df_DIA_SEMANA_banco_MBA_bkp)[2]<-'QUANTIDADE'
colnames(df_DIA_SEMANA_banco_MBA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_DIA_SEMANA_banco_MBA_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_DIA_SEMANA_banco_MBA_bkp$PERCENTUAL))
df_DIA_SEMANA_banco_MBA_bkp_rmd = tail(df_DIA_SEMANA_banco_MBA_bkp,5)
#########################################################################################################
#########################################################################################################
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "DOMINGO"]<- "ADOMINGO"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "SEGUNDA"]<- "BSEGUNDA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "TERÇA"]<- "CTERÇA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "QUARTA"]<- "DQUARTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "QUINTA"]<- "EQUINTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "SEXTA"]<- "FSEXTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "SÁBADO"]<- "GSÁBADO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_DIA_SEMANA_banco_MBA =
  df_DIA_SEMANA_banco_MBA %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(DIA_SEMANA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "ADOMINGO"]<- "DOMINGO"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "BSEGUNDA"]<- "SEGUNDA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "CTERÇA"]<- "TERÇA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "DQUARTA"]<- "QUARTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "EQUINTA"]<- "QUINTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "FSEXTA"]<- "SEXTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "GSÁBADO"]<- "SÁBADO"
#########################################################################################################

colnames(df_DIA_SEMANA_banco_MBA)[1]<-'DIA'
colnames(df_DIA_SEMANA_banco_MBA)[2]<-'QUANTIDADE'
colnames(df_DIA_SEMANA_banco_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#df_DIA_SEMANA_banco_MBA =
#  df_DIA_SEMANA_banco_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_DIA_SEMANA_banco_MBA FIM
#########################################################################################################

#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
