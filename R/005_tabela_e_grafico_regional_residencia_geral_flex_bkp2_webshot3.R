##TABELA REGIONAL
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_regional_residencia = df_snr_banco_sem_mba %>%
  select(REGIONAL_RESIDENCIAL)

table(df_snr_regional_residencia$REGIONAL_RESIDENCIAL, useNA ="always")
sum(table(df_snr_regional_residencia$REGIONAL_RESIDENCIAL, useNA ="always"))
#########################################################################################################
df_snr_regional_residencia <- data.frame(table(df_snr_regional_residencia$REGIONAL_RESIDENCIAL, useNA ="always"))
df_snr_regional_residencia_original=df_snr_regional_residencia #salvando atos atendimento original
#df_snr_regional_residencia=df_snr_regional_residencia_original
#df_snr_regional_residencia$Var1 <- NULL
colnames(df_snr_regional_residencia) <- c("REGIONAL", "QUANTIDADE")

#write.csv(df_snr_regional_residencia, file = "df_snr_regional_residencia_bruto.csv", row.names = TRUE)
###write.xlsx(df_snr_regional_residencia, file = "df_snr_regional_residencia_bruto.xlsx") #salvando com modificações anteriores

#df_snr_regional_residencia$SEXO <- NULL

df_snr_regional_residencia$REGIONAL <- as.character(df_snr_regional_residencia$REGIONAL)

df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "OESTE"]<- "ALTERNATIVA"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == ""]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "#N/DISP"]<- "SEM INFORMAÇÃO"
#df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == NA]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia

df_snr_regional_residencia = filter(df_snr_regional_residencia, !QUANTIDADE == 0)
df_snr_regional_residencia

df_snr_regional_residencia$REGIONAL2 <- ifelse(grepl("BARREIRO|CENTRO-SUL|LESTE|NORDESTE|NOROESTE|NORTE|ALTERNATIVA|PAMPULHA|VENDA NOVA|RMBH|MG|SEM INFORMAÇÃO", df_snr_regional_residencia$REGIONAL, ignore.case = TRUE),
                                            gsub(".*(BARREIRO|CENTRO-SUL|LESTE|NORDESTE|NOROESTE|NORTE|ALTERNATIVA|PAMPULHA|VENDA NOVA|RMBH|MG|SEM INFORMAÇÃO).*", "\\1",df_snr_regional_residencia$REGIONAL), "OUTRO ESTADO")

#write.csv(df_snr_regional_residencia, file = "df_snr_regional_residencia.csv", row.names = TRUE)
###write.xlsx(df_snr_regional_residencia, file = "df_snr_regional_residencia.xlsx")


df_snr_regional_residencia$REGIONAL <- NULL

colnames(df_snr_regional_residencia) <- c("QUANTIDADE", "REGIONAL")

df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "ALTERNATIVA"]<- "OESTE"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "RMBH"]<- "REGIÃO METROPOLITANA"

df_snr_regional_residencia <- df_snr_regional_residencia[c("REGIONAL", "QUANTIDADE")]

#library(grid)
#library(gridExtra)

##JUNTANDO AS LINHAS
library(plyr)
sum(df_snr_regional_residencia$QUANTIDADE)

df_snr_regional_residencia <- ddply(df_snr_regional_residencia,
                                 c("REGIONAL"),
                                 summarise,
                                 QUANTIDADE = sum(QUANTIDADE))

###alterando
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "OUTRA CIDADE MG"]<- "ROUTRA CIDADE MG"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "OUTRO ESTADO"]<- "ROUTRO ESTADO"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "VENDA NOVA"]<- "PVENDA NOVA"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"

df_snr_regional_residencia <-df_snr_regional_residencia[order(df_snr_regional_residencia$REGIONAL),]

df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "ROUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "ROUTRO ESTADO"]<- "OUTRO ESTADO"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "PVENDA NOVA"]<- "VENDA NOVA"
df_snr_regional_residencia$REGIONAL[df_snr_regional_residencia$REGIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#caso queira separar so regionais
#df_snr_regional_residencia1 <- df_snr_regional_residencia[!(df_snr_regional_residencia$REGIONAL == 'REGIÃO METROPOLITANA'|
 #                                                      df_snr_regional_residencia$REGIONAL == 'OUTRA CIDADE MG'|
  #                                                     df_snr_regional_residencia$REGIONAL == 'OUTRO ESTADO'|
   #                                                    df_snr_regional_residencia$REGIONAL == 'SEM INFORMAÇÃO'),]

#acrescentando coluna com percentual
df_snr_regional_residencia$QUANTIDADE <- as.numeric(df_snr_regional_residencia$QUANTIDADE)


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


df_snr_regional_residencia$PERCENTUAL <- round_preserve_sum(prop.table(df_snr_regional_residencia$QUANTIDADE)*100, 2)

#df_snr_regional_residencia <- df_snr_regional_residencia[order(df_snr_regional_residencia[,3],decreasing=TRUE),]
#write.csv(df_snr_regional_residencia, file ="df_snr_regional_residencia.csv",row.names=TRUE)
###write.xlsx(df_snr_regional_residencia, file ="df_snr_regional_residencia.xlsx")
#df_snr_regional_residencia$QUANTIDADE <- NULL

#salvando para utilizacao graficos
df_snr_regional_residencia_bkp = df_snr_regional_residencia

#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_regional_residencia_rmark = df_snr_regional_residencia

#SEPARANDO REGIAO METROPOLITANA POR CONTA DO TEXTO NO RELATORIO
df_snr_regional_residencia_rmark1 = df_snr_regional_residencia_rmark
df_snr_regional_residencia_rmark1 = filter(df_snr_regional_residencia_rmark1, !REGIONAL == "REGIÃO METROPOLITANA")

df_snr_regional_residencia_rmark1 = df_snr_regional_residencia_rmark1 %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#SO REGIAO METROPOLITANA e outros
df_snr_regional_residencia_rmark2 = filter(df_snr_regional_residencia_rmark, REGIONAL == "REGIÃO METROPOLITANA")
df_snr_regional_residencia_rmark3 = filter(df_snr_regional_residencia_rmark, REGIONAL == "OUTRA CIDADE MG")
df_snr_regional_residencia_rmark4 = filter(df_snr_regional_residencia_rmark, REGIONAL == "OUTRO ESTADO")
df_snr_regional_residencia_rmark5 = filter(df_snr_regional_residencia_rmark, REGIONAL == "SEM INFORMAÇÃO")
#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_regional_residencia_rmark = df_snr_regional_residencia_rmark %>%
  top_n(20, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(df_snr_regional_residencia_rmark$PERCENTUAL)

#para escolher linhas e posicoes
df_snr_regional_residencia_rmark[1,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#acrescentando linha com total
df_snr_regional_residencia <- rbind(df_snr_regional_residencia,
                                    data.frame(REGIONAL = "TOTAL", QUANTIDADE = sum(df_snr_regional_residencia$QUANTIDADE), PERCENTUAL = sum(df_snr_regional_residencia$PERCENTUAL),
                                               stringsAsFactors = FALSE))




colnames(df_snr_regional_residencia) <- c("REGIONAL", "QUANTIDADE", "%")


#para tabela gt abaixo:

df_snr_regional_residencia_gt = df_snr_regional_residencia

########################################################################################################
#########################################################################################################

#########################################################################################################

#########################################################################################################
#FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO df_snr_regional_residencia
#df_snr_regional_residencia_original=df_snr_regional_residencia #salvando REGIONALs atendimento original
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#GRAFICO REGIONAL
#df_snr_regional_residencia_original=df_snr_regional_residencia #salvando REGIONALs atendimento original
df_snr_regional_residencia=df_snr_regional_residencia_bkp

df_snr_regional_residencia<-df_snr_regional_residencia%>%
  arrange(QUANTIDADE)

df_snr_regional_residencia$REGIONAL = factor(df_snr_regional_residencia$REGIONAL)

#pdf(file="GRAFICO_006_df_snr_regional_residencia_alternativo.pdf", title = "grafico_df_snr_regional_residencia", width = 10, height = 8)


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#FIM
#########################################################################################################
