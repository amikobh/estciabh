#############################################################################################################
#intervalo_decisao
#########################################################################################################
intervalo_decisao =
  banco_sem_mba %>%
  filter(COMPARECIMENTO_AUD_PRELIMINAR %in% "COMPARECEU") %>%
  select(DATA_ATO, DATA_AUDIENCIA_PRELIMINAR) |>
  na.omit()

#########################################################################################################
#########################################################################################################
intervalo_decisao$INTER_01 = as.period(interval(intervalo_decisao$DATA_ATO, intervalo_decisao$DATA_AUDIENCIA_PRELIMINAR))
intervalo_decisao$INTER_02 = (intervalo_decisao$DATA_ATO) %--% (intervalo_decisao$DATA_AUDIENCIA_PRELIMINAR) / ddays(1)
#########################################################################################################
#########################################################################################################
#tabela
tempo_medio_decisao =
  intervalo_decisao |>
  select(INTER_02) |>
  mutate("TEMPO MÉDIO" = round(mean(intervalo_decisao$INTER_02), 1)) |>
  select("TEMPO MÉDIO")

tempo_medio_decisao =  tail(tempo_medio_decisao, n=1)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#classificando

intervalo_decisao$INTERV_03 = ifelse(intervalo_decisao$INTER_02 < 1,
                                     "MESMO DIA", "DESCONSIDERAR")


intervalo_decisao$INTERV_04 = ifelse(intervalo_decisao$INTER_02 == 1,
                                     "01 DIA", "DESCONSIDERAR")

intervalo_decisao$INTERV_05 = ifelse((intervalo_decisao$INTER_02 > 1 & intervalo_decisao$INTER_02 <=7),
                                     "ENTRE 01 DIA E 01 SEMANA", "DESCONSIDERAR")


intervalo_decisao$INTERV_06 = ifelse((intervalo_decisao$INTER_02 > 7 & intervalo_decisao$INTER_02 <=30),
                                     "ENTRE 01 SEMANA E 01 MÊS", "DESCONSIDERAR")


intervalo_decisao$INTERV_07 = ifelse((intervalo_decisao$INTER_02 > 30 & intervalo_decisao$INTER_02 <=180),
                                     "ENTRE 01 E 06 MESES", "DESCONSIDERAR")


intervalo_decisao$INTERV_08 = ifelse((intervalo_decisao$INTER_02 > 180 & intervalo_decisao$INTER_02 <=360),
                                     "ENTRE 06 MESES E 01 ANO", "DESCONSIDERAR")


intervalo_decisao$INTERV_09 = ifelse(intervalo_decisao$INTER_02 > 360,
                                     "MAIS DE 01 ANO", "DESCONSIDERAR")

#########################################################################################################
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
intervalo_decisao =

  intervalo_decisao %>%
  pivot_longer(cols = starts_with("INTERV"), values_to = "TEMPO") %>%
  #select(-name) %>%
  filter(TEMPO != "DESCONSIDERAR")

#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
intervalo_decisao_bkp = intervalo_decisao

intervalo_decisao_bkp =
  intervalo_decisao_bkp %>%
  janitor::tabyl(TEMPO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
#intervalo_decisao_bkp$ATO[intervalo_decisao_bkp$ATO == "VS/INF"]<- "SEM INFORMAÇÃO"
#intervalo_decisao_bkp$ATO[intervalo_decisao_bkp$ATO == "VOUTROS"]<- "OUTROS"


colnames(intervalo_decisao_bkp)[1]<-'intervalo_decisao_bkp'
colnames(intervalo_decisao_bkp)[2]<-'QUANTIDADE'
colnames(intervalo_decisao_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#para script rmd:
intervalo_decisao_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", intervalo_decisao_bkp$PERCENTUAL))
intervalo_decisao_bkp_rmd = tail(intervalo_decisao_bkp,3)
#########################################################################################################

intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "MESMO DIA"]<- "AMESMO DIA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "01 DIA"]<- "B01 DIA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "ENTRE 01 DIA E 01 SEMANA"]<- "CENTRE 01 DIA E 01 SEMANA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "ENTRE 01 SEMANA E 01 MÊS"]<- "DENTRE 01 SEMANA E 01 MÊS"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "ENTRE 01 E 06 MESES"]<- "EENTRE 01 E 06 MESES"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "ENTRE 06 MESES E 01 ANO"]<- "FENTRE 06 MESES E 01 ANO"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "MAIS DE 01 ANO"]<- "GMAIS DE 01 ANO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

intervalo_decisao =
  intervalo_decisao %>%
  janitor::tabyl(TEMPO) %>%
  arrange(TEMPO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#colnames(intervalo_decisao)[1]<-'ATO'
# Adaptando:
#intervalo_decisao$ATO[intervalo_decisao$ATO == "VS/INF"]<- "SEM INFORMAÇÃO"
#intervalo_decisao$ATO[intervalo_decisao$ATO == "VOUTROS"]<- "OUTROS"
#############################################################################################################
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "AMESMO DIA"]<- "MESMO DIA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "B01 DIA"]<- "01 DIA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "CENTRE 01 DIA E 01 SEMANA"]<- "ENTRE 01 DIA E 01 SEMANA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "DENTRE 01 SEMANA E 01 MÊS"]<- "ENTRE 01 SEMANA E 01 MÊS"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "EENTRE 01 E 06 MESES"]<- "ENTRE 01 E 06 MESES"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "FENTRE 06 MESES E 01 ANO"]<- "ENTRE 06 MESES E 01 ANO"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "GMAIS DE 01 ANO"]<- "MAIS DE 01 ANO"

#########################################################################################################

colnames(intervalo_decisao)[1]<-'TEMPO'
colnames(intervalo_decisao)[2]<-'QUANTIDADE'
colnames(intervalo_decisao)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# intervalo_decisao FIM
#########################################################################################################
#########################################################################################################
