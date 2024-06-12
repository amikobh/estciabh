#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_HOMICIDIO_snr_SEXO_IDADE
#########################################################################################################
banco_HOMICIDIO_snr =
  banco_geral_sem_concurso |>
  filter(ATO_INFRACIONAL %in% "HOMICÍDIO")
#########################################################################################################
incidencia_HOMICIDIO_gt = data.table(nrow(banco_HOMICIDIO))
colnames(incidencia_HOMICIDIO_gt)[1]<-'Incidência homicídio'
#########################################################################################################
########################################################################################################

banco_HOMICIDIO_snr_SEXO_IDADE =

  banco_HOMICIDIO_snr %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_HOMICIDIO_snr_SEXO_IDADE$SEXO[banco_HOMICIDIO_snr_SEXO_IDADE$SEXO == ""]<- "M"
#########################################################################################################
banco_HOMICIDIO_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_HOMICIDIO_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_HOMICIDIO_snr_SEXO_IDADE$SEXO[banco_HOMICIDIO_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_HOMICIDIO_snr_SEXO_IDADE$SEXO[banco_HOMICIDIO_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_HOMICIDIO_snr_SEXO_IDADE_pizza = banco_HOMICIDIO_snr_SEXO_IDADE
#########################################################################################################
banco_HOMICIDIO_snr_SEXO_IDADE_pizza =
  banco_HOMICIDIO_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_HOMICIDIO_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE[banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_HOMICIDIO_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_HOMICIDIO_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza <- ddply(banco_HOMICIDIO_snr_SEXO_IDADE_pizza,
                                                   c("SEXO"),
                                                   summarise,
                                                   QUANTIDADE = sum(QUANTIDADE))

banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza =
  banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_HOMICIDIO_snr_SEXO_IDADE_pizza,
                                                  c("IDADE"),
                                                  summarise,
                                                  QUANTIDADE = sum(QUANTIDADE))

banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp = banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_HOMICIDIO_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_HOMICIDIO_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_HOMICIDIO_snr_SEXO_IDADE =

  banco_HOMICIDIO_snr %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_HOMICIDIO_snr_SEXO_IDADE$SEXO[banco_HOMICIDIO_snr_SEXO_IDADE$SEXO == ""]<- "M"

#########################################################################################################
banco_HOMICIDIO_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_HOMICIDIO_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_HOMICIDIO_snr_SEXO_IDADE$SEXO[banco_HOMICIDIO_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_HOMICIDIO_snr_SEXO_IDADE$SEXO[banco_HOMICIDIO_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_HOMICIDIO_snr_SEXO_IDADE_pizza = banco_HOMICIDIO_snr_SEXO_IDADE
#########################################################################################################
banco_HOMICIDIO_snr_SEXO_IDADE_pizza =
  banco_HOMICIDIO_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_HOMICIDIO_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE[banco_HOMICIDIO_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_HOMICIDIO_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_HOMICIDIO_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza <- ddply(banco_HOMICIDIO_snr_SEXO_IDADE_pizza,
                                                   c("SEXO"),
                                                   summarise,
                                                   QUANTIDADE = sum(QUANTIDADE))

banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza =
  banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_HOMICIDIO_snr_SEXO_IDADE_pizza,
                                                  c("IDADE"),
                                                  summarise,
                                                  QUANTIDADE = sum(QUANTIDADE))

banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp = banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_HOMICIDIO_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_HOMICIDIO_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_ROUBO_snr_SEXO_IDADE
#########################################################################################################


banco_ROUBO_snr =
  banco_geral_SNR |>
  filter(ATO_INFRACIONAL %in% "ROUBO")
#########################################################################################################
#########################################################################################################
incidencia_ROUBO_gt = data.table(nrow(banco_ROUBO))
colnames(incidencia_ROUBO_gt)[1]<-'Incidência roubo'
#########################################################################################################
#########################################################################################################
########################################################################################################

banco_ROUBO_snr_SEXO_IDADE =

  banco_ROUBO_snr %>%
  #filter(CAUSA_JURIDICA %in% "ROUBO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_ROUBO_snr_SEXO_IDADE$SEXO[banco_ROUBO_snr_SEXO_IDADE$SEXO == ""]<- "M"
#########################################################################################################
banco_ROUBO_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_ROUBO_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_ROUBO_snr_SEXO_IDADE$SEXO[banco_ROUBO_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_ROUBO_snr_SEXO_IDADE$SEXO[banco_ROUBO_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_ROUBO_snr_SEXO_IDADE_pizza = banco_ROUBO_snr_SEXO_IDADE
#########################################################################################################
banco_ROUBO_snr_SEXO_IDADE_pizza =
  banco_ROUBO_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "ROUBO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_ROUBO_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE[banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_ROUBO_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_ROUBO_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_ROUBO_snr_SEXO_IDADE_graf_pizza <- ddply(banco_ROUBO_snr_SEXO_IDADE_pizza,
                                               c("SEXO"),
                                               summarise,
                                               QUANTIDADE = sum(QUANTIDADE))

banco_ROUBO_snr_SEXO_IDADE_graf_pizza =
  banco_ROUBO_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_ROUBO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_ROUBO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_ROUBO_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_ROUBO_snr_SEXO_IDADE_pizza,
                                              c("IDADE"),
                                              summarise,
                                              QUANTIDADE = sum(QUANTIDADE))

banco_ROUBO_snr_SEXO_IDADE_pizza_bkp = banco_ROUBO_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_ROUBO_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_ROUBO_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_ROUBO_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_ROUBO_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_ROUBO_snr_SEXO_IDADE =

  banco_ROUBO_snr %>%
  #filter(CAUSA_JURIDICA %in% "ROUBO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_ROUBO_snr_SEXO_IDADE$SEXO[banco_ROUBO_snr_SEXO_IDADE$SEXO == ""]<- "M"

#########################################################################################################
banco_ROUBO_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_ROUBO_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_ROUBO_snr_SEXO_IDADE$SEXO[banco_ROUBO_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_ROUBO_snr_SEXO_IDADE$SEXO[banco_ROUBO_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_ROUBO_snr_SEXO_IDADE_pizza = banco_ROUBO_snr_SEXO_IDADE
#########################################################################################################
banco_ROUBO_snr_SEXO_IDADE_pizza =
  banco_ROUBO_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "ROUBO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_ROUBO_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE[banco_ROUBO_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_ROUBO_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_ROUBO_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_ROUBO_snr_SEXO_IDADE_graf_pizza <- ddply(banco_ROUBO_snr_SEXO_IDADE_pizza,
                                               c("SEXO"),
                                               summarise,
                                               QUANTIDADE = sum(QUANTIDADE))

banco_ROUBO_snr_SEXO_IDADE_graf_pizza =
  banco_ROUBO_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_ROUBO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_ROUBO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_ROUBO_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_ROUBO_snr_SEXO_IDADE_pizza,
                                              c("IDADE"),
                                              summarise,
                                              QUANTIDADE = sum(QUANTIDADE))

banco_ROUBO_snr_SEXO_IDADE_pizza_bkp = banco_ROUBO_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_ROUBO_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_ROUBO_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_ROUBO_snr_SEXO_IDADE FIM
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_FURTO_snr_SEXO_IDADE
#########################################################################################################


banco_FURTO_snr =
  banco_geral_SNR |>
  filter(ATO_INFRACIONAL %in% "FURTO")
#########################################################################################################
#########################################################################################################
incidencia_FURTO_gt = data.table(nrow(banco_FURTO))
colnames(incidencia_FURTO_gt)[1]<-'Incidência furto'
#########################################################################################################
#########################################################################################################
########################################################################################################

banco_FURTO_snr_SEXO_IDADE =

  banco_FURTO_snr %>%
  #filter(CAUSA_JURIDICA %in% "FURTO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_FURTO_snr_SEXO_IDADE$SEXO[banco_FURTO_snr_SEXO_IDADE$SEXO == ""]<- "M"
#########################################################################################################
banco_FURTO_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_FURTO_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_FURTO_snr_SEXO_IDADE$SEXO[banco_FURTO_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_FURTO_snr_SEXO_IDADE$SEXO[banco_FURTO_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_FURTO_snr_SEXO_IDADE_pizza = banco_FURTO_snr_SEXO_IDADE
#########################################################################################################
banco_FURTO_snr_SEXO_IDADE_pizza =
  banco_FURTO_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "FURTO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_FURTO_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_FURTO_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_FURTO_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_FURTO_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_FURTO_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_FURTO_snr_SEXO_IDADE_pizza$IDADE[banco_FURTO_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_FURTO_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_FURTO_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_FURTO_snr_SEXO_IDADE_graf_pizza <- ddply(banco_FURTO_snr_SEXO_IDADE_pizza,
                                               c("SEXO"),
                                               summarise,
                                               QUANTIDADE = sum(QUANTIDADE))

banco_FURTO_snr_SEXO_IDADE_graf_pizza =
  banco_FURTO_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_FURTO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_FURTO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_FURTO_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_FURTO_snr_SEXO_IDADE_pizza,
                                              c("IDADE"),
                                              summarise,
                                              QUANTIDADE = sum(QUANTIDADE))

banco_FURTO_snr_SEXO_IDADE_pizza_bkp = banco_FURTO_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_FURTO_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_FURTO_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_FURTO_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_FURTO_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_FURTO_snr_SEXO_IDADE =

  banco_FURTO_snr %>%
  #filter(CAUSA_JURIDICA %in% "FURTO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_FURTO_snr_SEXO_IDADE$SEXO[banco_FURTO_snr_SEXO_IDADE$SEXO == ""]<- "M"

#########################################################################################################
banco_FURTO_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_FURTO_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_FURTO_snr_SEXO_IDADE$SEXO[banco_FURTO_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_FURTO_snr_SEXO_IDADE$SEXO[banco_FURTO_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_FURTO_snr_SEXO_IDADE_pizza = banco_FURTO_snr_SEXO_IDADE
#########################################################################################################
banco_FURTO_snr_SEXO_IDADE_pizza =
  banco_FURTO_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "FURTO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_FURTO_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_FURTO_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_FURTO_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_FURTO_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_FURTO_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_FURTO_snr_SEXO_IDADE_pizza$IDADE[banco_FURTO_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_FURTO_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_FURTO_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_FURTO_snr_SEXO_IDADE_graf_pizza <- ddply(banco_FURTO_snr_SEXO_IDADE_pizza,
                                               c("SEXO"),
                                               summarise,
                                               QUANTIDADE = sum(QUANTIDADE))

banco_FURTO_snr_SEXO_IDADE_graf_pizza =
  banco_FURTO_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_FURTO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_FURTO_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_FURTO_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_FURTO_snr_SEXO_IDADE_pizza,
                                              c("IDADE"),
                                              summarise,
                                              QUANTIDADE = sum(QUANTIDADE))

banco_FURTO_snr_SEXO_IDADE_pizza_bkp = banco_FURTO_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_FURTO_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_FURTO_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_FURTO_snr_SEXO_IDADE FIM
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_USO_DE_DROGAS_snr_SEXO_IDADE
#########################################################################################################


banco_USO_DE_DROGAS_snr =
  banco_geral_SNR |>
  filter(ATO_INFRACIONAL %in% "POSSE DE DROGAS PARA USO PESSOAL")
#########################################################################################################
#########################################################################################################
#########################################################################################################
incidencia_USO_DE_DROGAS_gt = data.table(nrow(banco_USO_DE_DROGAS))
colnames(incidencia_USO_DE_DROGAS_gt)[1]<-'Incidência posse de drogas para uso pessoal'
#########################################################################################################
########################################################################################################

banco_USO_DE_DROGAS_snr_SEXO_IDADE =

  banco_USO_DE_DROGAS_snr %>%
  #filter(CAUSA_JURIDICA %in% "USO_DE_DROGAS" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO == ""]<- "M"
#########################################################################################################
banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza = banco_USO_DE_DROGAS_snr_SEXO_IDADE
#########################################################################################################
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza =
  banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "USO_DE_DROGAS") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE[banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza <- ddply(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza,
                                                       c("SEXO"),
                                                       summarise,
                                                       QUANTIDADE = sum(QUANTIDADE))

banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza =
  banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza,
                                                      c("IDADE"),
                                                      summarise,
                                                      QUANTIDADE = sum(QUANTIDADE))

banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp = banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_USO_DE_DROGAS_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_USO_DE_DROGAS_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_USO_DE_DROGAS_snr_SEXO_IDADE =

  banco_USO_DE_DROGAS_snr %>%
  #filter(CAUSA_JURIDICA %in% "USO_DE_DROGAS" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO == ""]<- "M"

#########################################################################################################
banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_USO_DE_DROGAS_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza = banco_USO_DE_DROGAS_snr_SEXO_IDADE
#########################################################################################################
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza =
  banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "USO_DE_DROGAS") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE[banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza <- ddply(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza,
                                                       c("SEXO"),
                                                       summarise,
                                                       QUANTIDADE = sum(QUANTIDADE))

banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza =
  banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza,
                                                      c("IDADE"),
                                                      summarise,
                                                      QUANTIDADE = sum(QUANTIDADE))

banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp = banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_USO_DE_DROGAS_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE
#########################################################################################################


banco_TRAFICO_DE_DROGAS_snr =
  banco_geral_SNR |>
  filter(ATO_INFRACIONAL %in% "TRÁFICO DE DROGAS")
#########################################################################################################
#########################################################################################################
#########################################################################################################
incidencia_TRAFICO_DE_DROGAS_gt = data.table(nrow(banco_TRAFICO_DE_DROGAS))
colnames(incidencia_TRAFICO_DE_DROGAS_gt)[1]<-'Incidência tráfico de drogas'
#########################################################################################################
########################################################################################################

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE =

  banco_TRAFICO_DE_DROGAS_snr %>%
  #filter(CAUSA_JURIDICA %in% "TRAFICO_DE_DROGAS" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO == ""]<- "M"
#########################################################################################################
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza = banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE
#########################################################################################################
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza =
  banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "TRAFICO_DE_DROGAS") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE[banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza <- ddply(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza,
                                                           c("SEXO"),
                                                           summarise,
                                                           QUANTIDADE = sum(QUANTIDADE))

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza =
  banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza,
                                                          c("IDADE"),
                                                          summarise,
                                                          QUANTIDADE = sum(QUANTIDADE))

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp = banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE =

  banco_TRAFICO_DE_DROGAS_snr %>%
  #filter(CAUSA_JURIDICA %in% "TRAFICO_DE_DROGAS" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO == ""]<- "M"

#########################################################################################################
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO[banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza = banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE
#########################################################################################################
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza =
  banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "TRAFICO_DE_DROGAS") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE[banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza <- ddply(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza,
                                                           c("SEXO"),
                                                           summarise,
                                                           QUANTIDADE = sum(QUANTIDADE))

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza =
  banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza,
                                                          c("IDADE"),
                                                          summarise,
                                                          QUANTIDADE = sum(QUANTIDADE))

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp = banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE FIM
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#FIM
#########################################################################################################
