#############################################################################################################
#intervalo_sentenca
#########################################################################################################
intervalo_sentenca =
  banco_tratado %>%
  #filter(COMPARECIMENTO_AUD_PRELIMINAR %in% "COMPARECEU") %>%
  select(DATA_ATO, DATA_SENTENCA, SENTENCA)

#########################################################################################################
#resolvendo data sentença sem informação
intervalo_sentenca$SENTENCA[intervalo_sentenca$SENTENCA == ""]<- "VAZIO"

intervalo_sentenca =
  intervalo_sentenca |>
  filter(!SENTENCA %in% "VAZIO" & !SENTENCA %in% "SEMINFORMACAO")

#########################################################################################################
#########################################################################################################
intervalo_sentenca$INTER_01 = as.period(interval(intervalo_sentenca$DATA_ATO, intervalo_sentenca$DATA_SENTENCA))
intervalo_sentenca$INTER_02 = (intervalo_sentenca$DATA_ATO) %--% (intervalo_sentenca$DATA_SENTENCA) / ddays(1)
#########################################################################################################
#########################################################################################################
#tabela
tempo_medio_sentenca =
  intervalo_sentenca |>
  select(INTER_02) |>
  mutate(TEMPO_MEDIO = ifelse(INTER_02 < 0, INTER_02*-1, INTER_02))

  tempo_medio_sentenca =
    tempo_medio_sentenca |>
    mutate("TEMPO MÉDIO" = round(mean(TEMPO_MEDIO, na.rm = TRUE), 0)) |>
    select("TEMPO MÉDIO")


tempo_medio_sentenca =  tail(tempo_medio_sentenca, n=1)
tempo_medio_sentenca$`TEMPO MÉDIO` = paste(tempo_medio_sentenca$`TEMPO MÉDIO`, "dias", sep = " ")
#########################################################################################################
#########################################################################################################
#########################################################################################################
#classificando

intervalo_sentenca$INTERV_03 = ifelse(intervalo_sentenca$INTER_02 < 1,
                                      "MESMO DIA", "DESCONSIDERAR")


intervalo_sentenca$INTERV_04 = ifelse(intervalo_sentenca$INTER_02 == 1,
                                      "01 DIA", "DESCONSIDERAR")

intervalo_sentenca$INTERV_05 = ifelse((intervalo_sentenca$INTER_02 > 1 & intervalo_sentenca$INTER_02 <=7),
                                      "ENTRE 01 DIA E 01 SEMANA", "DESCONSIDERAR")


intervalo_sentenca$INTERV_06 = ifelse((intervalo_sentenca$INTER_02 > 7 & intervalo_sentenca$INTER_02 <=30),
                                      "ENTRE 01 SEMANA E 01 MÊS", "DESCONSIDERAR")


intervalo_sentenca$INTERV_07 = ifelse((intervalo_sentenca$INTER_02 > 30 & intervalo_sentenca$INTER_02 <=180),
                                      "ENTRE 01 E 06 MESES", "DESCONSIDERAR")


intervalo_sentenca$INTERV_08 = ifelse((intervalo_sentenca$INTER_02 > 180 & intervalo_sentenca$INTER_02 <=360),
                                      "ENTRE 06 MESES E 01 ANO", "DESCONSIDERAR")


intervalo_sentenca$INTERV_09 = ifelse(intervalo_sentenca$INTER_02 > 360,
                                      "MAIS DE 01 ANO", "DESCONSIDERAR")

#########################################################################################################
#ainda resolvendo data sentença sem informação
intervalo_sentenca$SENTENCA[intervalo_sentenca$SENTENCA == ""]<- "VAZIO"

intervalo_sentenca$INTERV_010 = ifelse(is.na(intervalo_sentenca$INTER_02),
                                       "SEM INFORMAÇÃO", "DESCONSIDERAR")

#########################################################################################################
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
intervalo_sentenca =

  intervalo_sentenca %>%
  pivot_longer(cols = starts_with("INTERV"), values_to = "TEMPO") %>%
  #select(-name) %>%
  filter(TEMPO != "DESCONSIDERAR")

#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
intervalo_sentenca_bkp = intervalo_sentenca

intervalo_sentenca_bkp =
  intervalo_sentenca_bkp %>%
  janitor::tabyl(TEMPO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
#intervalo_sentenca_bkp$ATO[intervalo_sentenca_bkp$ATO == "VS/INF"]<- "SEM INFORMAÇÃO"
#intervalo_sentenca_bkp$ATO[intervalo_sentenca_bkp$ATO == "VOUTROS"]<- "OUTROS"


colnames(intervalo_sentenca_bkp)[1]<-'intervalo_sentenca_bkp'
colnames(intervalo_sentenca_bkp)[2]<-'QUANTIDADE'
colnames(intervalo_sentenca_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#para script rmd:
intervalo_sentenca_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", intervalo_sentenca_bkp$PERCENTUAL))
intervalo_sentenca_bkp_rmd = tail(intervalo_sentenca_bkp,3)
#########################################################################################################

intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "MESMO DIA"]<- "AMESMO DIA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "01 DIA"]<- "B01 DIA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "ENTRE 01 DIA E 01 SEMANA"]<- "CENTRE 01 DIA E 01 SEMANA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "ENTRE 01 SEMANA E 01 MÊS"]<- "DENTRE 01 SEMANA E 01 MÊS"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "ENTRE 01 E 06 MESES"]<- "EENTRE 01 E 06 MESES"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "ENTRE 06 MESES E 01 ANO"]<- "FENTRE 06 MESES E 01 ANO"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "MAIS DE 01 ANO"]<- "GMAIS DE 01 ANO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

intervalo_sentenca =
  intervalo_sentenca %>%
  janitor::tabyl(TEMPO) %>%
  arrange(TEMPO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#colnames(intervalo_sentenca)[1]<-'ATO'
# Adaptando:
#intervalo_sentenca$ATO[intervalo_sentenca$ATO == "VS/INF"]<- "SEM INFORMAÇÃO"
#intervalo_sentenca$ATO[intervalo_sentenca$ATO == "VOUTROS"]<- "OUTROS"
#############################################################################################################
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "AMESMO DIA"]<- "MESMO DIA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "B01 DIA"]<- "01 DIA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "CENTRE 01 DIA E 01 SEMANA"]<- "ENTRE 01 DIA E 01 SEMANA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "DENTRE 01 SEMANA E 01 MÊS"]<- "ENTRE 01 SEMANA E 01 MÊS"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "EENTRE 01 E 06 MESES"]<- "ENTRE 01 E 06 MESES"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "FENTRE 06 MESES E 01 ANO"]<- "ENTRE 06 MESES E 01 ANO"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "GMAIS DE 01 ANO"]<- "MAIS DE 01 ANO"

#########################################################################################################

colnames(intervalo_sentenca)[1]<-'TEMPO'
colnames(intervalo_sentenca)[2]<-'QUANTIDADE'
colnames(intervalo_sentenca)[3]<-'PERCENTUAL'

#############################################################################################################
# intervalo_sentenca FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
# TABELA SOMA DECISOES
#########################################################################################################

#soma_decisoes = data.frame(rbind(cbind("EM AUDIÊNCIA PRELIMINAR", sum(so_decisao_bkp$QUANTIDADE)), cbind("APÓS AUDIÊNCIA PRELIMINAR", sum(intervalo_sentenca_bkp$QUANTIDADE))))

soma_decisoes = data.frame(rbind(cbind("EM AUDIÊNCIA PRELIMINAR", sum(so_decisao_bkp$QUANTIDADE)),
                                 cbind("APÓS AUDIÊNCIA PRELIMINAR", sum(so_sentenca_bkp$QUANTIDADE)),
                                 #cbind("ENCAMINHADOS PARA DECISAO", sum(so_sentenca_bkp$QUANTIDADE)-sum(intervalo_sentenca_bkp$QUANTIDADE))))
                                 cbind("ENCAMINHADOS PARA DECISAO", so_sentenca1[1,2])))

soma_decisoes$X2=as.numeric(soma_decisoes$X2)
#acrescentando linha com total
#acrescentando linha com total
soma_decisoes <- rbind(soma_decisoes,
                       data.frame(X1 = "TOTAL", X2 = sum(soma_decisoes$X2),stringsAsFactors = FALSE))

colnames(soma_decisoes) <- c("DECISÕES", "QUANTIDADE")
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
