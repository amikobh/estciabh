
#########################################################################################################
#TRATAMENTO banco_GERAL_snr_SEXO_IDADE
#########################################################################################################
#banco_SNR = distinct(banco_sem_mba, NOME2, NASCIMENTO, .keep_all= TRUE)
#########################################################################################################

banco_GERAL_snr_SEXO_IDADE =

  banco_SNR %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_GERAL_snr_SEXO_IDADE$SEXO[banco_GERAL_snr_SEXO_IDADE$SEXO == ""]<- "M"
#########################################################################################################
banco_GERAL_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_GERAL_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_GERAL_snr_SEXO_IDADE$SEXO[banco_GERAL_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_GERAL_snr_SEXO_IDADE$SEXO[banco_GERAL_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_GERAL_snr_SEXO_IDADE_pizza = banco_GERAL_snr_SEXO_IDADE
#########################################################################################################
banco_GERAL_snr_SEXO_IDADE_pizza =
  banco_GERAL_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_GERAL_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_GERAL_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_GERAL_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_GERAL_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_GERAL_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_GERAL_snr_SEXO_IDADE_pizza$IDADE[banco_GERAL_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_GERAL_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_GERAL_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_GERAL_snr_SEXO_IDADE_graf_pizza <- ddply(banco_GERAL_snr_SEXO_IDADE_pizza,
                                               c("SEXO"),
                                               summarise,
                                               QUANTIDADE = sum(QUANTIDADE))

banco_GERAL_snr_SEXO_IDADE_graf_pizza =
  banco_GERAL_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_GERAL_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_GERAL_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_GERAL_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_GERAL_snr_SEXO_IDADE_pizza,
                                              c("IDADE"),
                                              summarise,
                                              QUANTIDADE = sum(QUANTIDADE))

banco_GERAL_snr_SEXO_IDADE_pizza_bkp = banco_GERAL_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_GERAL_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_GERAL_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_GERAL_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_GERAL_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_GERAL_snr_SEXO_IDADE =

  banco_SNR %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_GERAL_snr_SEXO_IDADE$SEXO[banco_GERAL_snr_SEXO_IDADE$SEXO == ""]<- "M"

#########################################################################################################
banco_GERAL_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_GERAL_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_GERAL_snr_SEXO_IDADE$SEXO[banco_GERAL_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_GERAL_snr_SEXO_IDADE$SEXO[banco_GERAL_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_GERAL_snr_SEXO_IDADE_pizza = banco_GERAL_snr_SEXO_IDADE
#########################################################################################################
banco_GERAL_snr_SEXO_IDADE_pizza =
  banco_GERAL_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_GERAL_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_GERAL_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_GERAL_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_GERAL_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_GERAL_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_GERAL_snr_SEXO_IDADE_pizza$IDADE[banco_GERAL_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_GERAL_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_GERAL_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_GERAL_snr_SEXO_IDADE_graf_pizza <- ddply(banco_GERAL_snr_SEXO_IDADE_pizza,
                                               c("SEXO"),
                                               summarise,
                                               QUANTIDADE = sum(QUANTIDADE))

banco_GERAL_snr_SEXO_IDADE_graf_pizza =
  banco_GERAL_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_GERAL_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_GERAL_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_GERAL_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_GERAL_snr_SEXO_IDADE_pizza,
                                              c("IDADE"),
                                              summarise,
                                              QUANTIDADE = sum(QUANTIDADE))

banco_GERAL_snr_SEXO_IDADE_pizza_bkp = banco_GERAL_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_GERAL_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_GERAL_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_GERAL_snr_SEXO_IDADE FIM
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
########################################################################################################
#FIM
##################################################################################################################################################################################################

#########################################################################################################
#########################################################################################################
#grafico linha comparando atendimentos anuais. Obs: acrescentar ano e número de atendimentos(df numero_de_casos_geral
######. Corrigir titulos.

#criando data frame com os dados:
#criando data frame com os dados:
df_atendimento <- data.frame(ANO = c(2015,2016,2017, 2018, 2019, 2020, 2021, 2022, format(Sys.Date()-365*1, "%Y")),
                             ATENDIMENTO = c(8518,8176,8244,7786, 6838, 4672, 3649, 3054, nrow(numero_de_casos_geral)))

df_atendimento1 <- data.frame(ANO = c(2015,2016,2017, 2018, 2019, 2020, 2021, 2022, format(Sys.Date()-365*1, "%Y")),
                              ADOLESCENTES = c(5718, 5456, 4854, 4520, 4022, 2680, 2102, 1848, nrow(banco_SNR)))


df_atendimento = full_join(df_atendimento,df_atendimento1,by="ANO")
df_atendimento

df_atendimento = data.table(df_atendimento)


df_atendimento <- melt(df_atendimento, id = c("ANO"))


colnames(df_atendimento) <- c("ANO", "TIPO", "QUANTIDADE")


df_atendimento$TIPO <- as.character(df_atendimento$TIPO)

df_atendimento$TIPO[df_atendimento$TIPO == "ATENDIMENTO"]<- "TOTAL DE CASOS ATENDIDOS"
df_atendimento$TIPO[df_atendimento$TIPO == "ADOLESCENTES"]<- "ADOLESCENTES ATENDIDOS"

#########################################################################################################


#tabela alternativa
#########################################################################################################
#salvando tabela
#pdf(file="TABELA_001_df_atendimento.pdf", width = 6, height = 3, title = "tabela_df_atendimento_alternativa")
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################

