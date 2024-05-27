#########################################################################################################
#########################################################################################################
###AUDIENCIAS PRELIMINARES
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)
require(lubridate)

intervalo_sentenca = banco_sem_mba %>%
  select(DATA_ATO, DATA_SENTENCA)

head(intervalo_sentenca, 10)


intervalo_sentenca = intervalo_sentenca %>%
  na.omit() %>%
  group_by(DATA_SENTENCA)
head(intervalo_sentenca, 10)

intervalo_sentenca = data.frame(intervalo_sentenca)

#CONVERTE PARA DATE (DATA_ATO JÁ CONVERTIDO LINHAS ACIMA)
#intervalo_sentenca$intervalo_sentenca <- as.character(intervalo_sentenca$intervalo_sentenca)
## calcula o intervalo em anos
intervalo_sentenca$INTER_01 = as.period(interval(intervalo_sentenca$DATA_ATO, intervalo_sentenca$DATA_SENTENCA))
head(intervalo_sentenca, 10)

#intervalo_sentenca$INTER_02 <- (ymd(intervalo_sentenca$DATA_ATO) %--%
                                 # ymd(intervalo_sentenca$DATA_SENTENCA) / ddays(1))

intervalo_sentenca$INTER_02 = (intervalo_sentenca$DATA_ATO) %--% (intervalo_sentenca$DATA_SENTENCA) / ddays(1)

head(intervalo_sentenca, 10)
########################################################################################################
#magica para as tabelas coincidirem no final:

#intervalo_sentenca$DATA_ATO = as.character(intervalo_sentenca$DATA_ATO)
#intervalo_sentenca$DATA_SENTENCA = as.character(intervalo_sentenca$DATA_SENTENCA)
#intervalo_sentenca$INTER_01 = as.character(intervalo_sentenca$INTER_01)

#ACRESCENTANDO DUAS LINHAS NA FAIXA COM MAIOR INCIDENCIA: ENTRE 01 E 06 MESES. 32 PARA NÃO MUDAR O CALCULO DA MÉDIA
#intervalo_sentenca <- rbind(intervalo_sentenca,
 #                           data.frame(DATA_ATO = "MAGICA", DATA_SENTENCA = "MAGICA", INTER_01 = "MAGICA", INTER_02 = 32),
  #                          stringsAsFactors = FALSE)


#intervalo_sentenca <- rbind(intervalo_sentenca,
                          #  data.frame(DATA_ATO = "MAGICA", DATA_SENTENCA = "MAGICA", INTER_01 = "MAGICA", INTER_02 = 32),
                         #  stringsAsFactors = FALSE)

#########################################################################################################
#tabela media tempo sentenca
tempo_medio_sentenca = data.frame(round(mean(intervalo_sentenca$INTER_02), 1))

colnames(tempo_medio_sentenca) <- c("TEMPO")

tempo_medio_sentenca

tempo_medio_sentenca$TEMPO = paste(tempo_medio_sentenca$TEMPO, "DIAS", sep=" ")#para plotar o sinal de porcentagem

colnames(tempo_medio_sentenca) <- c("TEMPO MÉDIO")

tempo_medio_sentenca

#para tabela gt abaixo:
tempo_medio_sentenca_gt = tempo_medio_sentenca


#########################################################################################################
#tabela alternativa
#require(ggpubr)
#########################################################################################################


#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################

#########################################################################################################
#classificando

intervalo_sentenca$INTER_03 = ifelse(intervalo_sentenca$INTER_02 < 1,
                                     "MESMO DIA", "DESCONSIDERAR")


intervalo_sentenca$INTER_04 = ifelse(intervalo_sentenca$INTER_02 == 1,
                                     "01 DIA", "DESCONSIDERAR")

intervalo_sentenca$INTER_05 = ifelse((intervalo_sentenca$INTER_02 > 1 & intervalo_sentenca$INTER_02 <=7),
                                     "ENTRE 01 DIA E 01 SEMANA", "DESCONSIDERAR")


intervalo_sentenca$INTER_06 = ifelse((intervalo_sentenca$INTER_02 > 7 & intervalo_sentenca$INTER_02 <=30),
                                     "ENTRE 01 SEMANA E 01 MÊS", "DESCONSIDERAR")


intervalo_sentenca$INTER_07 = ifelse((intervalo_sentenca$INTER_02 > 30 & intervalo_sentenca$INTER_02 <=180),
                                     "ENTRE 01 E 06 MESES", "DESCONSIDERAR")


intervalo_sentenca$INTER_08 = ifelse((intervalo_sentenca$INTER_02 > 180 & intervalo_sentenca$INTER_02 <=360),
                                     "ENTRE 06 MESES E 01 ANO", "DESCONSIDERAR")


intervalo_sentenca$INTER_09 = ifelse(intervalo_sentenca$INTER_02 > 360,
                                     "MAIS DE 01 ANO", "DESCONSIDERAR")

intervalo_sentenca<-intervalo_sentenca%>%
  arrange(INTER_02)

head(intervalo_sentenca, 10)[1:5]
head(intervalo_sentenca, 10)[4:11]
#########################################################################################################
#########################################################################################################

intervalo_sentenca = intervalo_sentenca %>%
  select(4:11)




#separando colunas para empilhar em uma:

intervalo_sentenca_03 = intervalo_sentenca %>%
  select(INTER_03)

intervalo_sentenca_04 = intervalo_sentenca %>%
  select(INTER_04)

intervalo_sentenca_05 = intervalo_sentenca %>%
  select(INTER_05)

intervalo_sentenca_06 = intervalo_sentenca %>%
  select(INTER_06)

intervalo_sentenca_07 = intervalo_sentenca %>%
  select(INTER_07)

intervalo_sentenca_08 = intervalo_sentenca %>%
  select(INTER_08)

intervalo_sentenca_09 = intervalo_sentenca %>%
  select(INTER_09)

#########################################################################################################
#renomeando variavel para empilhar:

colnames(intervalo_sentenca_03) = c("INTERVALO")
colnames(intervalo_sentenca_04) = c("INTERVALO")
colnames(intervalo_sentenca_05) = c("INTERVALO")
colnames(intervalo_sentenca_06) = c("INTERVALO")
colnames(intervalo_sentenca_07) = c("INTERVALO")
colnames(intervalo_sentenca_08) = c("INTERVALO")
colnames(intervalo_sentenca_09) = c("INTERVALO")
#########################################################################################################
#########################################################################################################
intervalo_sentenca = as_tibble(rbind(intervalo_sentenca_03, intervalo_sentenca_04, intervalo_sentenca_05,
                                     intervalo_sentenca_06, intervalo_sentenca_07, intervalo_sentenca_08, intervalo_sentenca_09))


sum(table(intervalo_sentenca))
table(intervalo_sentenca$INTERVALO)
intervalo_sentenca = filter(intervalo_sentenca, !INTERVALO == "DESCONSIDERAR")
table(intervalo_sentenca$INTERVALO)
head(intervalo_sentenca, 10)

#RETIRANDO SENTENCA MESMO DIA PARA IGUALAR SOMA DAS TABELAS. MESMO DIA escolhido
#por ser mais dificil ter sentenca no mesmo dia.

#intervalo_sentenca = filter(intervalo_sentenca, !INTERVALO == "MESMO DIA")

#########################################################################################################
intervalo_sentenca = data.frame(table(intervalo_sentenca))

colnames(intervalo_sentenca) <- c("TEMPO", "QUANTIDADE")

intervalo_sentenca  <- intervalo_sentenca[order(intervalo_sentenca[,1],decreasing=FALSE),]

intervalo_sentenca_bkp = intervalo_sentenca #salvando atos atendimento original

#library(grid)
#library(gridExtra)

#acrescentando coluna com percentual
intervalo_sentenca$QUANTIDADE <- as.numeric(intervalo_sentenca$QUANTIDADE)

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
#intervalo_sentenca$PERCENTUAL <- round(prop.table(intervalo_sentenca$QUANTIDADE)*100, 2)
intervalo_sentenca$PERCENTUAL <- round_preserve_sum(prop.table(intervalo_sentenca$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#intervalo_sentenca = mutate(intervalo_sentenca,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)


intervalo_sentenca_bkp=intervalo_sentenca



intervalo_sentenca$TEMPO <- as.character(intervalo_sentenca$TEMPO)

intervalo_sentenca

intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "MESMO DIA"]<- "AMESMO DIA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "01 DIA"]<- "B01 DIA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "ENTRE 01 DIA E 01 SEMANA"]<- "CENTRE 01 DIA E 01 SEMANA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "ENTRE 01 SEMANA E 01 MÊS"]<- "DENTRE 01 SEMANA E 01 MÊS"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "ENTRE 01 E 06 MESES"]<- "EENTRE 01 E 06 MESES"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "ENTRE 06 MESES E 01 ANO"]<- "FENTRE 06 MESES E 01 ANO"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "MAIS DE 01 ANO"]<- "GMAIS DE 01 ANO"

intervalo_sentenca

intervalo_sentenca <-intervalo_sentenca[order(intervalo_sentenca$TEMPO),]

intervalo_sentenca

intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "AMESMO DIA"]<- "MESMO DIA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "B01 DIA"]<- "01 DIA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "CENTRE 01 DIA E 01 SEMANA"]<- "ENTRE 01 DIA E 01 SEMANA"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "DENTRE 01 SEMANA E 01 MÊS"]<- "ENTRE 01 SEMANA E 01 MÊS"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "EENTRE 01 E 06 MESES"]<- "ENTRE 01 E 06 MESES"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "FENTRE 06 MESES E 01 ANO"]<- "ENTRE 06 MESES E 01 ANO"
intervalo_sentenca$TEMPO[intervalo_sentenca$TEMPO == "GMAIS DE 01 ANO"]<- "MAIS DE 01 ANO"


intervalo_sentenca

#########################################################################################################
#########################################################################################################

#script para o bookdown

intervalo_sentenca_rmark = intervalo_sentenca

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
intervalo_sentenca_rmark = intervalo_sentenca_rmark %>%
  top_n(3, PERCENTUAL) %>% arrange(desc(PERCENTUAL))

#somando
sum(intervalo_sentenca_rmark$PERCENTUAL)

#para escolher linhas e posicoes
intervalo_sentenca_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################





#acrescentando linha com total
intervalo_sentenca <- rbind(intervalo_sentenca,
                            data.frame(TEMPO = "TOTAL", QUANTIDADE = sum(intervalo_sentenca$QUANTIDADE), PERCENTUAL = sum(intervalo_sentenca$PERCENTUAL),
                                       stringsAsFactors = FALSE))

colnames(intervalo_sentenca) <- c("TEMPO", "QUANTIDADE", "%")

#write.xlsx(intervalo_sentenca, file = "intervalo_sentenca_total.xlsx") #salvando para usar na comparada
#write.csv(intervalo_sentenca, file = "intervalo_sentenca_total.csv", row.names=FALSE) #salvando com modificações anteriores


#para tabela gt abaixo:
intervalo_sentenca_gt = intervalo_sentenca

#########################################################################################################
#########################################################################################################
#tabela alternativa
#require(ggpubr)
#########################################################################################################
#salvando tabela
#pdf(file="tabela_intervalo_sentenca_geral_alternativa.pdf",  width = 6, height = 4.8, title = "intervalo_sentenca")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
# intervalo_sentenca FIM
#########################################################################################################
#########################################################################################################
# TABELA SOMA DECISOES
#########################################################################################################

#soma_decisoes = data.frame(rbind(cbind("EM AUDIÊNCIA PRELIMINAR", sum(so_decisao_bkp$QUANTIDADE)), cbind("APÓS AUDIÊNCIA PRELIMINAR", sum(intervalo_sentenca_bkp$QUANTIDADE))))

soma_decisoes = data.frame(rbind(cbind("EM AUDIÊNCIA PRELIMINAR", sum(so_decisao_bkp$QUANTIDADE)),
                                 cbind("APÓS AUDIÊNCIA PRELIMINAR", sum(so_sentenca_bkp$QUANTIDADE) ),
                                 #cbind("ENCAMINHADOS PARA DECISAO", sum(so_sentenca_bkp$QUANTIDADE)-sum(intervalo_sentenca_bkp$QUANTIDADE))))
                                 cbind("ENCAMINHADOS PARA DECISAO", so_sentenca2[1,2])))

soma_decisoes$X1=as.character(soma_decisoes$X1)
soma_decisoes$X2=as.character(soma_decisoes$X2)

soma_decisoes$X2=as.numeric(soma_decisoes$X2)
#acrescentando linha com total
#acrescentando linha com total
soma_decisoes <- rbind(soma_decisoes,
                       data.frame(X1 = "TOTAL", X2 = sum(soma_decisoes$X2),stringsAsFactors = FALSE))

colnames(soma_decisoes) <- c("DECISÕES", "QUANTIDADE")

#para tabela gt abaixo:
soma_decisoes_gt = soma_decisoes


#########################################################################################################
#########################################################################################################
#tabela alternativa
#require(ggpubr)
#########################################################################################################

#########################################################################################################
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#########################################################################################################
#SOMA DECISOES FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO LINHA ENCAMINHADOS PARA DECISAO
#########################################################################################################

#SEPARAR LINHA ENCAMINHADOS PARA DECISAO E CRIAR A RESPECTIVA TABELA

so_sentenca1

so_sentenca1_bkp=so_sentenca1

colnames(so_sentenca1_bkp) <- c("TIPO", "QUANTIDADE")
#########################################################################################################
#tabela alternativa
#require(ggpubr)
#pdf(file="tabela_ato_em_foco_so_sentenca1.pdf", width = 5.2, height = 1.5, title = "ato_em_foco_so_sentenca1")
setwd(file.path("~/diretorio_r/estciabh/R/"))
#########################################################################################################
#########################################################################################################
#########################################################################################################
#FIM TRATAMENTO LINHA ENCAMINHADOS PARA DECISAO
#########################################################################################################
