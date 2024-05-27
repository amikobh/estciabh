#########################################################################################################
#########################################################################################################
###AUDIENCIAS PRELIMINARES
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

so_sentenca = banco_sem_mba %>%
  select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, COMPARECIMENTO_AUD_PRELIMINAR)

#head(so_sentenca, 10)[3:7]

#PREENCHER COM NA'S CELULAS VAZIAS

#so_sentenca_com_compareceu <- subset(banco_sem_mba, COMPARECIMENTO_AUD_PRELIMINAR %in% c('COMPARECEU'))
so_sentenca <- table(so_sentenca$SENTENCA)

#TRASFORMAR TABELA ANTERIOR EM DATA FRAME

so_sentenca <- data.frame(so_sentenca)

#ACERTAR TERMOS
colnames(so_sentenca) <- c("SENTENCA", "QUANTIDADE")


so_sentenca$SENTENCA <- as.character(so_sentenca$SENTENCA)

so_sentenca

#preenchimento de celulas:
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "ARQUIVAMENTO/ATIPICIDADE"]<-	"ARQUIVAMENTO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "SENTENCAINCONCLUSIVA"]<-	"VOUTROS"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "INTERNACAO"]<-	"INTERNAÇÃO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#so_sentenca$SENTENCA[so_sentenca$SENTENCA == "LA"]<-	"REMISSÃO c/c LA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "OUTRAS(OS)"]<-	"VOUTROS"
#so_sentenca$SENTENCA[so_sentenca$SENTENCA == "PSC"]<-	"REMISSÃO c/c PSC"
#so_sentenca$SENTENCA[so_sentenca$SENTENCA == "PSC/LA"]<-	"REMISSÃO c/c LA/PSC"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
#so_sentenca$SENTENCA[so_sentenca$SENTENCA == "SEMINFORMACAO"]<-	"AGUARDANDO SENTENCA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "SEMINFORMACAO"]<-	"ENCAMINHADOS PARA SENTENÇA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO DOS AUTOS A DELEGACIA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
so_sentenca$SENTENCA[so_sentenca$SENTENCA == ""]<-	"VAZIO"
so_sentenca

#write.csv(so_sentenca, file ="so_sentenca.csv", row.names=FALSE)

##JUNTANDO AS LINHAS
library(plyr)

so_sentenca <- ddply(so_sentenca,
                    c("SENTENCA"),
                    summarise,
                    QUANTIDADE = sum(QUANTIDADE))

so_sentenca

so_sentenca = filter(so_sentenca, !SENTENCA == "VAZIO")
sum(so_sentenca$QUANTIDADE)

#SEPARAR LINHA ENCAMINHADOS PARA SENTECA
so_sentenca1 = filter(so_sentenca, SENTENCA == "ENCAMINHADOS PARA SENTENÇA")
#MUDANDO ENCAMINHADOS PARA SENTENÇA PARA ENCAMINHADOS PARA DECISAO
so_sentenca1$SENTENCA[so_sentenca1$SENTENCA == "ENCAMINHADOS PARA SENTENÇA"]<-	"ENCAMINHADOS PARA DECISÃO"
so_sentenca2 = so_sentenca1
#APAGAR ENCAMINHADOS PARA SENTECA E SEMINFORMACAO
so_sentenca2[1,2]
#so_sentenca = filter(so_sentenca, !SENTENCA == "SEMINFORMACAO")
so_sentenca = filter(so_sentenca, !SENTENCA == "ENCAMINHADOS PARA SENTENÇA")
so_sentenca
sum(so_sentenca$QUANTIDADE)

#so_sentenca  <- so_sentenca[order(so_sentenca[,1],decreasing=FALSE),]
so_sentenca

so_sentenca$SENTENCA[so_sentenca$SENTENCA == "VOUTROS"]<-	"OUTROS"
so_sentenca
#########################################################################################################
#########################################################################################################
#########################################################################################################
so_sentenca_bkp = so_sentenca #salvando atos atendimento original

#library(grid)
#library(gridExtra)

#acrescentando coluna com percentual
so_sentenca$QUANTIDADE <- as.numeric(so_sentenca$QUANTIDADE)

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
#so_sentenca$PERCENTUAL <- round(prop.table(so_sentenca$QUANTIDADE)*100, 2)
so_sentenca$PERCENTUAL <- round_preserve_sum(prop.table(so_sentenca$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#so_sentenca = mutate(so_sentenca,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)


so_sentenca_bkp=so_sentenca

#########################################################################################################
#########################################################################################################

#script para o bookdown

so_sentenca_rmark = so_sentenca

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
so_sentenca_rmark = so_sentenca_rmark %>%
  top_n(3, PERCENTUAL) %>% arrange(desc(PERCENTUAL))

#somando
sum(so_sentenca_rmark$PERCENTUAL)

#para escolher linhas e posicoes
so_sentenca_rmark[1,2]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################


#acrescentando linha com total
so_sentenca <- rbind(so_sentenca,
                    data.frame(SENTENCA = "TOTAL", QUANTIDADE = sum(so_sentenca$QUANTIDADE), PERCENTUAL = sum(so_sentenca$PERCENTUAL),
                               stringsAsFactors = FALSE))

colnames(so_sentenca) <- c("SENTENÇA", "QUANTIDADE", "%")

#write.xlsx(so_sentenca, file = "so_sentenca_total.xlsx") #salvando para usar na comparada
#write.csv(so_sentenca, file = "so_sentenca_total.csv", row.names=FALSE) #salvando com modificações anteriores

#para tabela gt abaixo:
so_sentenca_gt = so_sentenca

#########################################################################################################
#########################################################################################################
#tabela alternativa
#require(ggpubr)
#########################################################################################################
#salvando tabela
#pdf(file="tabela_so_sentenca_geral_alternativa.pdf",  width = 6, height = 4.8, title = "so_sentenca")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# so_sentenca FIM
#########################################################################################################


