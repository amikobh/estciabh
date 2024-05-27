#########################################################################################################
#TRATAMENTO HOMICIDIO:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#banco_sem_concurso <- banco_linhas_necessarias_df_sem_mba[!duplicated(data.frame(banco_linhas_necessarias_df_sem_mba$PROCESSO, banco_linhas_necessarias_df_sem_mba$ATO_INFRACIONAL_ATA_01)),]
banco_HOMICIDIO = banco_sem_concurso
table(banco_HOMICIDIO$ATO_INFRACIONAL)
#banco_HOMICIDIO = filter(banco_HOMICIDIO, ATO_INFRACIONAL_ATA_01 == "HOMICÍDIO" |
 #                                      ATO_INFRACIONAL_ATA_02 == "HOMICÍDIO" |
  #                                     ATO_INFRACIONAL_ATA_03 == "HOMICÍDIO")



banco_HOMICIDIO = filter(banco_HOMICIDIO, ATO_INFRACIONAL == "HOMICÍDIO")

banco_HOMICIDIO_puro = banco_HOMICIDIO

#banco somente HOMICIDIO
#write.xlsx(banco_HOMICIDIO_puro, file = "banco_HOMICIDIO_puro.xlsx")
#write.csv(banco_HOMICIDIO_puro, file = "banco_HOMICIDIO_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
#########################################################################################################
library(dplyr)

banco_HOMICIDIO = banco_HOMICIDIO %>%
  select(NOME2, NASCIMENTO, IDADE, SEXO, DATA_ATO, PROCESSO, ATO_INFRACIONAL, DIA_SEMANA_ATO,
         DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, REGIONAL_RESIDENCIAL, TIPO_LOG.ATO,
         NOME_LOG_ATO, N_LOG.ATO, BAIRRO_REGIAO_ATO, REGIONAL_ATO, CIDADE_ATO, UF_ATO, PAIS_ATO)


banco_HOMICIDIO_bkp = banco_HOMICIDIO
#write.xlsx(banco_HOMICIDIO, file = "banco_HOMICIDIO23c.xlsx")
#write.csv(banco_HOMICIDIO, file = "banco_HOMICIDIO23c.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
table(banco_HOMICIDIO$ATO_INFRACIONAL)
#banco_HOMICIDIO$ATO_INFRACIONAL <- gsub(" ","", banco_HOMICIDIO$ATO_INFRACIONAL)

#########################################################################################################
#write.xlsx(banco_HOMICIDIO_puro, file = "banco_HOMICIDIO_puro.xlsx")
##write.csv(banco_HOMICIDIO_puro, file = "banco_HOMICIDIO_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
banco_HOMICIDIO = banco_HOMICIDIO %>%
  select(ATO_INFRACIONAL)

banco_HOMICIDIO = data.frame(table(banco_HOMICIDIO))
colnames(banco_HOMICIDIO) <- c("ATO", "INCIDÊNCIA")

banco_HOMICIDIO
#salvar para calculo taxa 100mil

banco_HOMICIDIO_TX = banco_HOMICIDIO
#########################################################################################################
#########################################################################################################
#tabela incidencia_HOMICIDIO
incidencia_HOMICIDIO = data.frame(banco_HOMICIDIO[1,2])

colnames(incidencia_HOMICIDIO) <- c("INCIDÊNCIA HOMICÍDIO")

#para tabela gt abaixo:
incidencia_HOMICIDIO_gt = incidencia_HOMICIDIO

#########################################################################################################
#########################################################################################################

#########################################################################################################

#dev.off()

#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO ROUBO:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#banco_sem_concurso <- banco_linhas_necessarias_df_sem_mba[!duplicated(data.frame(banco_linhas_necessarias_df_sem_mba$PROCESSO, banco_linhas_necessarias_df_sem_mba$ATO_INFRACIONAL_ATA_01)),]
banco_ROUBO = banco_sem_concurso

#banco_ROUBO = filter(banco_ROUBO, ATO_INFRACIONAL_ATA_01 == "ROUBO" |
#                                      ATO_INFRACIONAL_ATA_02 == "ROUBO" |
#                                     ATO_INFRACIONAL_ATA_03 == "ROUBO")

banco_ROUBO =
  banco_ROUBO %>%
    filter(ATO_INFRACIONAL %in% c("ROUBO", "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", "ROUBO (EM CONCURSO DE PESSOAS)"))



banco_ROUBO$ATO_INFRACIONAL = grepl(pattern = "ROUBO", x = banco_ROUBO$ATO_INFRACIONAL)

#substituindo
banco_ROUBO$ATO_INFRACIONAL = ifelse(banco_ROUBO$ATO_INFRACIONAL == TRUE,
                              "ROUBO", banco_ROUBO$ATO_INFRACIONAL)

table(banco_ROUBO$ATO_INFRACIONAL)


banco_ROUBO_puro = banco_ROUBO

#banco somente ROUBO
#write.xlsx(banco_ROUBO_puro, file = "banco_ROUBO_puro.xlsx")
#write.csv(banco_ROUBO_puro, file = "banco_ROUBO_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
#########################################################################################################
library(dplyr)

banco_ROUBO = banco_ROUBO %>%
  select(NOME2, NASCIMENTO, IDADE, SEXO, DATA_ATO, PROCESSO, ATO_INFRACIONAL, DIA_SEMANA_ATO,
         DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, REGIONAL_RESIDENCIAL, TIPO_LOG.ATO,
         NOME_LOG_ATO, N_LOG.ATO, BAIRRO_REGIAO_ATO, REGIONAL_ATO, CIDADE_ATO, UF_ATO, PAIS_ATO)


banco_ROUBO_bkp = banco_ROUBO
#write.xlsx(banco_ROUBO, file = "banco_ROUBO23c.xlsx")
#write.csv(banco_ROUBO, file = "banco_ROUBO23c.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
table(banco_ROUBO$ATO_INFRACIONAL)
#banco_ROUBO$ATO_INFRACIONAL <- gsub(" ","", banco_ROUBO$ATO_INFRACIONAL)

#########################################################################################################
#write.xlsx(banco_ROUBO_puro, file = "banco_ROUBO_puro.xlsx")
##write.csv(banco_ROUBO_puro, file = "banco_ROUBO_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
banco_ROUBO = banco_ROUBO %>%
  select(ATO_INFRACIONAL)

banco_ROUBO = data.frame(table(banco_ROUBO))
colnames(banco_ROUBO) <- c("ATO", "INCIDÊNCIA")

banco_ROUBO
#salvar para calculo taxa 100mil

banco_ROUBO_TX = banco_ROUBO

#########################################################################################################
#########################################################################################################
#tabela incidencia_ROUBO
incidencia_ROUBO = data.frame(banco_ROUBO[1,2])

colnames(incidencia_ROUBO) <- c("INCIDÊNCIA ROUBO")

#para tabela gt abaixo:
incidencia_ROUBO_gt = incidencia_ROUBO

#########################################################################################################
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO FURTO:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#banco_sem_concurso <- banco_linhas_necessarias_df_sem_mba[!duplicated(data.frame(banco_linhas_necessarias_df_sem_mba$PROCESSO, banco_linhas_necessarias_df_sem_mba$ATO_INFRACIONAL_ATA_01)),]
banco_FURTO = banco_sem_concurso

#banco_FURTO = filter(banco_FURTO, ATO_INFRACIONAL_ATA_01 == "FURTO" |
#                                      ATO_INFRACIONAL_ATA_02 == "FURTO" |
#                                     ATO_INFRACIONAL_ATA_03 == "FURTO")

banco_FURTO = filter(banco_FURTO, ATO_INFRACIONAL == "FURTO")

banco_FURTO_puro = banco_FURTO

#banco somente FURTO
#write.xlsx(banco_FURTO_puro, file = "banco_FURTO_puro.xlsx")
#write.csv(banco_FURTO_puro, file = "banco_FURTO_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
#########################################################################################################
library(dplyr)

banco_FURTO = banco_FURTO %>%
  select(NOME2, NASCIMENTO, IDADE, SEXO, DATA_ATO, PROCESSO, ATO_INFRACIONAL, DIA_SEMANA_ATO,
         DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, REGIONAL_RESIDENCIAL, TIPO_LOG.ATO,
         NOME_LOG_ATO, N_LOG.ATO, BAIRRO_REGIAO_ATO, REGIONAL_ATO, CIDADE_ATO, UF_ATO, PAIS_ATO)


banco_FURTO_bkp = banco_FURTO
#write.xlsx(banco_FURTO, file = "banco_FURTO23c.xlsx")
#write.csv(banco_FURTO, file = "banco_FURTO23c.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
table(banco_FURTO$ATO_INFRACIONAL)
#banco_FURTO$ATO_INFRACIONAL <- gsub(" ","", banco_FURTO$ATO_INFRACIONAL)

#########################################################################################################
#write.xlsx(banco_FURTO_puro, file = "banco_FURTO_puro.xlsx")
##write.csv(banco_FURTO_puro, file = "banco_FURTO_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
banco_FURTO = banco_FURTO %>%
  select(ATO_INFRACIONAL)

banco_FURTO = data.frame(table(banco_FURTO))
colnames(banco_FURTO) <- c("ATO", "INCIDÊNCIA")

banco_FURTO

#salvar para calculo taxa 100mil

banco_FURTO_TX = banco_FURTO
#########################################################################################################
#########################################################################################################
#tabela incidencia_FURTO
incidencia_FURTO = data.frame(banco_FURTO[1,2])

colnames(incidencia_FURTO) <- c("INCIDÊNCIA FURTO")

#para tabela gt abaixo:
incidencia_FURTO_gt = incidencia_FURTO

#########################################################################################################
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################
#TRATAMENTO USO_DE_DROGAS:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#banco_sem_concurso <- banco_linhas_necessarias_df_sem_mba[!duplicated(data.frame(banco_linhas_necessarias_df_sem_mba$PROCESSO, banco_linhas_necessarias_df_sem_mba$ATO_INFRACIONAL_ATA_01)),]
banco_USO_DE_DROGAS = banco_sem_concurso

#banco_USO_DE_DROGAS = filter(banco_USO_DE_DROGAS, ATO_INFRACIONAL_ATA_01 == "USO_DE_DROGAS" |
#                                      ATO_INFRACIONAL_ATA_02 == "USO_DE_DROGAS" |
#                                     ATO_INFRACIONAL_ATA_03 == "USO_DE_DROGAS")

banco_USO_DE_DROGAS = filter(banco_USO_DE_DROGAS, ATO_INFRACIONAL == "POSSE DE DROGAS PARA USO PESSOAL")

banco_USO_DE_DROGAS_puro = banco_USO_DE_DROGAS

#banco somente USO_DE_DROGAS
#write.xlsx(banco_USO_DE_DROGAS_puro, file = "banco_USO_DE_DROGAS_puro.xlsx")
#write.csv(banco_USO_DE_DROGAS_puro, file = "banco_USO_DE_DROGAS_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
#########################################################################################################
library(dplyr)

banco_USO_DE_DROGAS = banco_USO_DE_DROGAS %>%
  select(NOME2, NASCIMENTO, IDADE, SEXO, DATA_ATO, PROCESSO, ATO_INFRACIONAL, DIA_SEMANA_ATO,
         DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, REGIONAL_RESIDENCIAL, TIPO_LOG.ATO,
         NOME_LOG_ATO, N_LOG.ATO, BAIRRO_REGIAO_ATO, REGIONAL_ATO, CIDADE_ATO, UF_ATO, PAIS_ATO)


banco_USO_DE_DROGAS_bkp = banco_USO_DE_DROGAS
#write.xlsx(banco_USO_DE_DROGAS, file = "banco_USO_DE_DROGAS23c.xlsx")
#write.csv(banco_USO_DE_DROGAS, file = "banco_USO_DE_DROGAS23c.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
table(banco_USO_DE_DROGAS$ATO_INFRACIONAL)
#banco_USO_DE_DROGAS$ATO_INFRACIONAL <- gsub(" ","", banco_USO_DE_DROGAS$ATO_INFRACIONAL)

#########################################################################################################
#write.xlsx(banco_USO_DE_DROGAS_puro, file = "banco_USO_DE_DROGAS_puro.xlsx")
##write.csv(banco_USO_DE_DROGAS_puro, file = "banco_USO_DE_DROGAS_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
banco_USO_DE_DROGAS = banco_USO_DE_DROGAS %>%
  select(ATO_INFRACIONAL)

banco_USO_DE_DROGAS = data.frame(table(banco_USO_DE_DROGAS))
colnames(banco_USO_DE_DROGAS) <- c("ATO", "INCIDÊNCIA")

banco_USO_DE_DROGAS

#salvar para calculo taxa 100mil

banco_USO_DE_DROGAS_TX = banco_USO_DE_DROGAS
#########################################################################################################
#########################################################################################################
#tabela incidencia_USO_DE_DROGAS
incidencia_USO_DE_DROGAS = data.frame(banco_USO_DE_DROGAS[1,2])

colnames(incidencia_USO_DE_DROGAS) <- c("INCIDÊNCIA POSSE DE DROGAS PARA USO PESSOAL")

#para tabela gt abaixo:
incidencia_USO_DE_DROGAS_gt = incidencia_USO_DE_DROGAS

#########################################################################################################
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################
#TRATAMENTO TRAFICO_DE_DROGAS:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#banco_sem_concurso <- banco_linhas_necessarias_df_sem_mba[!duplicated(data.frame(banco_linhas_necessarias_df_sem_mba$PROCESSO, banco_linhas_necessarias_df_sem_mba$ATO_INFRACIONAL_ATA_01)),]
banco_TRAFICO_DE_DROGAS = banco_sem_concurso

#banco_TRAFICO_DE_DROGAS = filter(banco_TRAFICO_DE_DROGAS, ATO_INFRACIONAL_ATA_01 == "TRAFICO_DE_DROGAS" |
#                                      ATO_INFRACIONAL_ATA_02 == "TRAFICO_DE_DROGAS" |
#                                     ATO_INFRACIONAL_ATA_03 == "TRAFICO_DE_DROGAS")

banco_TRAFICO_DE_DROGAS = filter(banco_TRAFICO_DE_DROGAS, ATO_INFRACIONAL == "TRÁFICO DE DROGAS")

banco_TRAFICO_DE_DROGAS_puro = banco_TRAFICO_DE_DROGAS

#banco somente TRAFICO_DE_DROGAS
#write.xlsx(banco_TRAFICO_DE_DROGAS_puro, file = "banco_TRAFICO_DE_DROGAS_puro.xlsx")
#write.csv(banco_TRAFICO_DE_DROGAS_puro, file = "banco_TRAFICO_DE_DROGAS_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
#########################################################################################################
library(dplyr)

banco_TRAFICO_DE_DROGAS = banco_TRAFICO_DE_DROGAS %>%
  select(NOME2, NASCIMENTO, IDADE, SEXO, DATA_ATO, PROCESSO, ATO_INFRACIONAL, DIA_SEMANA_ATO,
         DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, REGIONAL_RESIDENCIAL, TIPO_LOG.ATO,
         NOME_LOG_ATO, N_LOG.ATO, BAIRRO_REGIAO_ATO, REGIONAL_ATO, CIDADE_ATO, UF_ATO, PAIS_ATO)


banco_TRAFICO_DE_DROGAS_bkp = banco_TRAFICO_DE_DROGAS
#write.xlsx(banco_TRAFICO_DE_DROGAS, file = "banco_TRAFICO_DE_DROGAS23c.xlsx")
#write.csv(banco_TRAFICO_DE_DROGAS, file = "banco_TRAFICO_DE_DROGAS23c.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
table(banco_TRAFICO_DE_DROGAS$ATO_INFRACIONAL)
#banco_TRAFICO_DE_DROGAS$ATO_INFRACIONAL <- gsub(" ","", banco_TRAFICO_DE_DROGAS$ATO_INFRACIONAL)

#########################################################################################################
#write.xlsx(banco_TRAFICO_DE_DROGAS_puro, file = "banco_TRAFICO_DE_DROGAS_puro.xlsx")
##write.csv(banco_TRAFICO_DE_DROGAS_puro, file = "banco_TRAFICO_DE_DROGAS_puro.csv", row.names=FALSE)
#########################################################################################################
#########################################################################################################
banco_TRAFICO_DE_DROGAS = banco_TRAFICO_DE_DROGAS %>%
  select(ATO_INFRACIONAL)

banco_TRAFICO_DE_DROGAS = data.frame(table(banco_TRAFICO_DE_DROGAS))
colnames(banco_TRAFICO_DE_DROGAS) <- c("ATO", "INCIDÊNCIA")

banco_TRAFICO_DE_DROGAS

#salvar para calculo taxa 100mil

banco_TRAFICO_DE_DROGAS_TX = banco_TRAFICO_DE_DROGAS
#########################################################################################################
#########################################################################################################
#tabela incidencia_TRAFICO_DE_DROGAS
incidencia_TRAFICO_DE_DROGAS = data.frame(banco_TRAFICO_DE_DROGAS[1,2])

colnames(incidencia_TRAFICO_DE_DROGAS) <- c("INCIDÊNCIA TRÁFICO DE DROGAS")

#para tabela gt abaixo:
incidencia_TRAFICO_DE_DROGAS_gt = incidencia_TRAFICO_DE_DROGAS

#########################################################################################################
#########################################################################################################

#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio


#########################################################################################################
#FIM
#########################################################################################################
#########################################################################################################


