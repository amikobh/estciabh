#########################################################################################################
#########################################################################################################
###AUDIENCIAS PRELIMINARES
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

decisao_decisao = banco_sem_mba %>%
  select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, COMPARECIMENTO_AUD_PRELIMINAR)

head(decisao_decisao, 10)[3:7]

#PREENCHER COM NA'S CELULAS VAZIAS

#6.1 DECISÕES: banco com MBA?
so_decisao_com_compareceu = filter(decisao_decisao, COMPARECIMENTO_AUD_PRELIMINAR == "COMPARECEU")
head(so_decisao_com_compareceu, 10)[3:7]

#so_decisao_com_compareceu <- subset(banco_sem_mba, COMPARECIMENTO_AUD_PRELIMINAR %in% c('COMPARECEU'))
so_decisao <- table(so_decisao_com_compareceu$DECISAO)

#TRASFORMAR TABELA ANTERIOR EM DATA FRAME

so_decisao <- data.frame(so_decisao)

#ACERTAR TERMOS
colnames(so_decisao) <- c("DECISAO", "QUANTIDADE")


so_decisao$DECISAO <- as.character(so_decisao$DECISAO)

so_decisao

#preenchimento de celulas:
so_decisao$DECISAO[so_decisao$DECISAO == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
so_decisao$DECISAO[so_decisao$DECISAO == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
so_decisao$DECISAO[so_decisao$DECISAO == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
so_decisao$DECISAO[so_decisao$DECISAO == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
so_decisao$DECISAO[so_decisao$DECISAO == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
so_decisao$DECISAO[so_decisao$DECISAO == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
so_decisao$DECISAO[so_decisao$DECISAO == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
so_decisao$DECISAO[so_decisao$DECISAO == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
so_decisao$DECISAO[so_decisao$DECISAO == "INTERNACAO"]<-	"INTERNAÇÃO"
so_decisao$DECISAO[so_decisao$DECISAO == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
so_decisao$DECISAO[so_decisao$DECISAO == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
so_decisao$DECISAO[so_decisao$DECISAO == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#so_decisao$DECISAO[so_decisao$DECISAO == "LA"]<-	"REMISSAO c/c LA"
so_decisao$DECISAO[so_decisao$DECISAO == "OUTRAS(OS)"]<-	"VOUTROS"
#so_decisao$DECISAO[so_decisao$DECISAO == "PSC"]<-	"REMISSAO c/c PSC"
#so_decisao$DECISAO[so_decisao$DECISAO == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$DECISAO[so_decisao$DECISAO == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$DECISAO[so_decisao$DECISAO == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$DECISAO[so_decisao$DECISAO == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
so_decisao$DECISAO[so_decisao$DECISAO == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
so_decisao$DECISAO[so_decisao$DECISAO == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
so_decisao$DECISAO[so_decisao$DECISAO == "SEMINFORMACAO"]<-	"VAZIO"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOAPSC"]<-	"RETORNO A PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOALA"]<-	"RETORNO A LA"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
so_decisao$DECISAO[so_decisao$DECISAO == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
so_decisao$DECISAO[so_decisao$DECISAO == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
so_decisao$DECISAO[so_decisao$DECISAO == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
so_decisao$DECISAO[so_decisao$DECISAO == ""]<-	"VAZIO"



so_decisao
sum(so_decisao$QUANTIDADE)
#write.csv(so_decisao, file ="so_decisao.csv", row.names=FALSE)

so_decisao
##JUNTANDO AS LINHAS
library(plyr)

so_decisao <- ddply(so_decisao,
                    c("DECISAO"),
                    summarise,
                    QUANTIDADE = sum(QUANTIDADE))

so_decisao
sum(so_decisao$QUANTIDADE)
so_decisao = filter(so_decisao, !DECISAO == "VAZIO")
sum(table(so_decisao))
so_decisao = filter(so_decisao, !DECISAO == "SEMINFORMACAO")

so_decisao
sum(so_decisao$QUANTIDADE)

#so_decisao  <- so_decisao[order(so_decisao[,1],decreasing=FALSE),]
so_decisao

so_decisao$DECISAO[so_decisao$DECISAO == "VOUTROS"]<-	"OUTROS"
so_decisao
#########################################################################################################
#########################################################################################################
#########################################################################################################
so_decisao_bkp = so_decisao #salvando atos atendimento original
sum(so_decisao_bkp$QUANTIDADE)
#library(grid)
#library(gridExtra)

#acrescentando coluna com percentual
so_decisao$QUANTIDADE <- as.numeric(so_decisao$QUANTIDADE)

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
#so_decisao$PERCENTUAL <- round(prop.table(so_decisao$QUANTIDADE)*100, 2)
so_decisao$PERCENTUAL <- round_preserve_sum(prop.table(so_decisao$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#so_decisao = mutate(so_decisao,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)


so_decisao_bkp=so_decisao
#########################################################################################################
#########################################################################################################

#script para o bookdown

so_decisao_rmark = so_decisao
so_decisao_rmark1 = so_decisao
#SEPARAR CASOS DE ARQUIVAMENTO E REMISSÕES PARA SCRIPTS EM 014_DECISOES_Rmd

#filter(so_decisao_rmark, !grepl("REMISSAO", DECISAO))
so_decisao_rmark = filter(so_decisao_rmark, grepl("ARQUIVAMENTO|REMISSÃO", DECISAO))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
so_decisao_rmark1 = so_decisao_rmark1 %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
#sum(so_decisao_rmark$QUANTIDADE)

#para escolher linhas e posicoes
so_decisao_rmark1[1,2]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

#acrescentando linha com total
so_decisao <- rbind(so_decisao,
                    data.frame(DECISAO = "TOTAL", QUANTIDADE = sum(so_decisao$QUANTIDADE), PERCENTUAL = sum(so_decisao$PERCENTUAL),
                               stringsAsFactors = FALSE))

colnames(so_decisao) <- c("DECISAO", "QUANTIDADE", "%")

#para tabela gt abaixo:
so_decisao_gt = so_decisao



#write.xlsx(so_decisao, file = "so_decisao_total.xlsx") #salvando para usar na comparada
#write.csv(so_decisao, file = "so_decisao_total.csv", row.names=FALSE) #salvando com modificações anteriores




#########################################################################################################
#########################################################################################################
#tabela alternativa
#########################################################################################################
#salvando tabela
#pdf(file="tabela_so_decisao_geral_alternativa.pdf",  width = 6, height = 4.8, title = "so_decisao")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################




#salvar pdf

setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# so_decisao FIM
#########################################################################################################


