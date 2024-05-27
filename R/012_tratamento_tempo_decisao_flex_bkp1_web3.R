#########################################################################################################
#########################################################################################################
###AUDIENCIAS PRELIMINARES
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)
require(lubridate)

intervalo_decisao = so_decisao_com_compareceu %>%
 select(DATA_ATO, DATA_AUDIENCIA_PRELIMINAR)


head(intervalo_decisao, 10)


intervalo_decisao = intervalo_decisao %>%
  na.omit() %>%
  group_by(DATA_AUDIENCIA_PRELIMINAR)
head(intervalo_decisao, 10)

intervalo_decisao = data.frame(intervalo_decisao)

#CONVERTE PARA DATE (DATA_ATO JÁ CONVERTIDO LINHAS ACIMA)
#intervalo_decisao$INTERVALO_DECISAO <- as.character(intervalo_decisao$INTERVALO_DECISAO)
## calcula o intervalo em anos
intervalo_decisao$INTER_01 = as.period(interval(intervalo_decisao$DATA_ATO, intervalo_decisao$DATA_AUDIENCIA_PRELIMINAR))
head(intervalo_decisao, 10)

#intervalo_decisao$INTER_02 <- (ymd(intervalo_decisao$DATA_ATO)  %--%
                                   #   ymd(intervalo_decisao$DATA_AUDIENCIA_PRELIMINAR) / ddays(1))

intervalo_decisao$INTER_02 = (intervalo_decisao$DATA_ATO) %--% (intervalo_decisao$DATA_AUDIENCIA_PRELIMINAR) / ddays(1)

head(intervalo_decisao, 10)

#########################################################################################################
#tabela media tempo decisao
tempo_medio_decisao = data.frame(round(mean(intervalo_decisao$INTER_02), 1))

colnames(tempo_medio_decisao) <- c("TEMPO")

tempo_medio_decisao

tempo_medio_decisao$TEMPO = paste(tempo_medio_decisao$TEMPO, "DIAS", sep=" ")#para plotar o sinal de porcentagem

colnames(tempo_medio_decisao) <- c("TEMPO MÉDIO")

tempo_medio_decisao

#para tabela gt abaixo:
tempo_medio_decisao_gt = tempo_medio_decisao


#########################################################################################################
#tabela alternativa
#require(ggpubr)
#pdf(file="tabela_ato_em_foco_tempo_medio_decisao.pdf", width = 5.2, height = 1.5, title = "ato_em_foco_tempo_medio_decisao")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

   #########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################

#########################################################################################################
#classificando

intervalo_decisao$INTER_03 = ifelse(intervalo_decisao$INTER_02 < 1,
                                    "MESMO DIA", "DESCONSIDERAR")


intervalo_decisao$INTER_04 = ifelse(intervalo_decisao$INTER_02 == 1,
                                    "01 DIA", "DESCONSIDERAR")

intervalo_decisao$INTER_05 = ifelse((intervalo_decisao$INTER_02 > 1 & intervalo_decisao$INTER_02 <=7),
                                    "ENTRE 01 DIA E 01 SEMANA", "DESCONSIDERAR")


intervalo_decisao$INTER_06 = ifelse((intervalo_decisao$INTER_02 > 7 & intervalo_decisao$INTER_02 <=30),
                                    "ENTRE 01 SEMANA E 01 MÊS", "DESCONSIDERAR")


intervalo_decisao$INTER_07 = ifelse((intervalo_decisao$INTER_02 > 30 & intervalo_decisao$INTER_02 <=180),
                                    "ENTRE 01 E 06 MESES", "DESCONSIDERAR")


intervalo_decisao$INTER_08 = ifelse((intervalo_decisao$INTER_02 > 180 & intervalo_decisao$INTER_02 <=360),
                                    "ENTRE 06 MESES E 01 ANO", "DESCONSIDERAR")


intervalo_decisao$INTER_09 = ifelse(intervalo_decisao$INTER_02 > 360,
                                    "MAIS DE 01 ANO", "DESCONSIDERAR")

#########################################################################################################
#########################################################################################################

intervalo_decisao = intervalo_decisao %>%
                    select(5:11)




#separando colunas para empilhar em uma:

intervalo_decisao_03 = intervalo_decisao %>%
  select(INTER_03)

intervalo_decisao_04 = intervalo_decisao %>%
  select(INTER_04)

intervalo_decisao_05 = intervalo_decisao %>%
  select(INTER_05)

intervalo_decisao_06 = intervalo_decisao %>%
  select(INTER_06)

intervalo_decisao_07 = intervalo_decisao %>%
  select(INTER_07)

intervalo_decisao_08 = intervalo_decisao %>%
  select(INTER_08)

intervalo_decisao_09 = intervalo_decisao %>%
  select(INTER_09)

#########################################################################################################
#renomeando variavel para empilhar:

colnames(intervalo_decisao_03) = c("INTERVALO")
colnames(intervalo_decisao_04) = c("INTERVALO")
colnames(intervalo_decisao_05) = c("INTERVALO")
colnames(intervalo_decisao_06) = c("INTERVALO")
colnames(intervalo_decisao_07) = c("INTERVALO")
colnames(intervalo_decisao_08) = c("INTERVALO")
colnames(intervalo_decisao_09) = c("INTERVALO")
#########################################################################################################
#########################################################################################################
intervalo_decisao = as_tibble(rbind(intervalo_decisao_03, intervalo_decisao_04, intervalo_decisao_05,
                                    intervalo_decisao_06, intervalo_decisao_07, intervalo_decisao_08, intervalo_decisao_09))


sum(table(intervalo_decisao))
table(intervalo_decisao$INTERVALO)
intervalo_decisao = filter(intervalo_decisao, !INTERVALO == "DESCONSIDERAR")
table(intervalo_decisao$INTERVALO)
head(intervalo_decisao, 10)


#########################################################################################################
intervalo_decisao = data.frame(table(intervalo_decisao))

colnames(intervalo_decisao) <- c("TEMPO", "QUANTIDADE")

intervalo_decisao  <- intervalo_decisao[order(intervalo_decisao[,1],decreasing=FALSE),]

intervalo_decisao_bkp = intervalo_decisao #salvando atos atendimento original

#library(grid)
#library(gridExtra)

#acrescentando coluna com percentual
intervalo_decisao$QUANTIDADE <- as.numeric(intervalo_decisao$QUANTIDADE)

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
#intervalo_decisao$PERCENTUAL <- round(prop.table(intervalo_decisao$QUANTIDADE)*100, 2)
intervalo_decisao$PERCENTUAL <- round_preserve_sum(prop.table(intervalo_decisao$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#intervalo_decisao = mutate(intervalo_decisao,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)


intervalo_decisao_bkp=intervalo_decisao



intervalo_decisao$TEMPO <- as.character(intervalo_decisao$TEMPO)

intervalo_decisao

intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "MESMO DIA"]<- "AMESMO DIA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "01 DIA"]<- "B01 DIA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "ENTRE 01 DIA E 01 SEMANA"]<- "CENTRE 01 DIA E 01 SEMANA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "ENTRE 01 SEMANA E 01 MÊS"]<- "DENTRE 01 SEMANA E 01 MÊS"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "ENTRE 01 E 06 MESES"]<- "EENTRE 01 E 06 MESES"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "ENTRE 06 MESES E 01 ANO"]<- "FENTRE 06 MESES E 01 ANO"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "MAIS DE 01 ANO"]<- "GMAIS DE 01 ANO"

intervalo_decisao

intervalo_decisao <-intervalo_decisao[order(intervalo_decisao$TEMPO),]

intervalo_decisao

intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "AMESMO DIA"]<- "MESMO DIA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "B01 DIA"]<- "01 DIA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "CENTRE 01 DIA E 01 SEMANA"]<- "ENTRE 01 DIA E 01 SEMANA"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "DENTRE 01 SEMANA E 01 MÊS"]<- "ENTRE 01 SEMANA E 01 MÊS"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "EENTRE 01 E 06 MESES"]<- "ENTRE 01 E 06 MESES"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "FENTRE 06 MESES E 01 ANO"]<- "ENTRE 06 MESES E 01 ANO"
intervalo_decisao$TEMPO[intervalo_decisao$TEMPO == "GMAIS DE 01 ANO"]<- "MAIS DE 01 ANO"


intervalo_decisao

#########################################################################################################
#########################################################################################################

#script para o bookdown

intervalo_decisao_rmark = intervalo_decisao

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
intervalo_decisao_rmark = intervalo_decisao_rmark %>%
  top_n(3, PERCENTUAL) %>% arrange(desc(PERCENTUAL))

#somando
sum(intervalo_decisao_rmark$PERCENTUAL)

#para escolher linhas e posicoes
intervalo_decisao_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################




#acrescentando linha com total
intervalo_decisao <- rbind(intervalo_decisao,
                           data.frame(TEMPO = "TOTAL", QUANTIDADE = sum(intervalo_decisao$QUANTIDADE), PERCENTUAL = sum(intervalo_decisao$PERCENTUAL),
                                      stringsAsFactors = FALSE))

colnames(intervalo_decisao) <- c("TEMPO", "QUANTIDADE", "%")

#para tabela gt abaixo:
intervalo_decisao_gt = intervalo_decisao

#write.xlsx(intervalo_decisao, file = "intervalo_decisao_total.xlsx") #salvando para usar na comparada
#write.csv(intervalo_decisao, file = "intervalo_decisao_total.csv", row.names=FALSE) #salvando com modificações anteriores


#########################################################################################################
#########################################################################################################
#tabela alternativa
#require(ggpubr)
#########################################################################################################
#salvando tabela
#pdf(file="tabela_intervalo_decisao_geral_alternativa.pdf",  width = 6, height = 4.8, title = "intervalo_decisao")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# intervalo_decisao FIM
#########################################################################################################
