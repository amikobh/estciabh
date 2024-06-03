#########################################################################################################
#TRATAMENTO INCIDENCIA:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################

banco_atos_em_foco$ATO_INFRACIONAL_ATA_01 <- gsub(" ","", banco_atos_em_foco$ATO_INFRACIONAL_ATA_01)
banco_atos_em_foco$ATO_INFRACIONAL_ATA_02 <- gsub(" ","", banco_atos_em_foco$ATO_INFRACIONAL_ATA_02)
banco_atos_em_foco$ATO_INFRACIONAL_ATA_03 <- gsub(" ","", banco_atos_em_foco$ATO_INFRACIONAL_ATA_03)
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_atos_em_foco =

  banco_atos_em_foco %>%
  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") %>%
  #select(-name) %>%
  filter(!ATO_INFRACIONAL %in% "NSA" & !ATO_INFRACIONAL %in% "TERMOSEMINF." & !ATO_INFRACIONAL %in% "VAZIO")

#########################################################################################################
#########################################################################################################
#substituição especifica de artigo deve anteceder a genérica: vide as primeiras linhas e latrocinio e roubo:

banco_atos_em_foco$ATO_INFRACIONAL = sub("28.ART11.*.*", "POSSE DE DROGAS PARA USO PESSOAL", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("34.ART11.*.*", "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("35.ART11.*.*", "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub(".*ART11.343.*.*", "TRÁFICO DE DROGAS", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub(".*ART10.826.*.*", "POSSE/PORTE ILEGAL", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub(".*ARTCTB.*.*", "CRIME DE TRÂNSITO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("121.ART.*.*", "HOMICÍDIO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("121C/C14.*.*", "HOMICÍDIO (TENTATIVA)", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("129.*.*", "LESÃO CORPORAL", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("129§.*.*", "LESÃO CORPORAL", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("137.*.*", "RIXA", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("140.ART.*.*", "RIXA", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("140§*.*", "RIXA", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("147.*.*", "AMEAÇA", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("148.ART.*.*", "SEQUESTRO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("155.ART.*.*", "FURTO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("155C/C.*.*", "FURTO (TENTATIVA)", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("157.ART.*.*", "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("157C/C.*.*", "ROUBO (TENTATIVA)", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("157§3.*.*", "LATROCÍNIO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("157§.*.*", "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("163.ART.*.*", "DANO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("171.ART.*.*", "ESTELIONATO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("180.ART.*.*", "RECEPTAÇÃO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("19.ART.*.*", "PORTE ARMA (LCP)", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("21.ART.*.*", "VIAS DE FATO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("213.ART.*.*", "ESTUPRO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("215.ART.*.*", "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("215-A.*.*", "IMPORTUNAÇÃO SEXUAL", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("217-A.*.*", "ESTUPRO DE VUNERÁVEL", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("311.ARTCPB.*.*", "ADULTERAÇÃO DE SINAL IDENTIFICADOR DE VEÍCULO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("329.ART.*.*", "RESISTÊNCIA", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("330.ART.*.*", "DESOBEDIÊNCIA", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("331.ART.*.*", "DESACATO", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub("65.ART9.*.*", "PICHAÇÃO", banco_atos_em_foco$ATO_INFRACIONAL)

#substituindo os restantes em outros
banco_atos_em_foco$ATO_INFRACIONAL = sub(".*OUTROS.*.*", "VOUTROS", banco_atos_em_foco$ATO_INFRACIONAL)
banco_atos_em_foco$ATO_INFRACIONAL = sub(".*ARTCPB.*.*", "VOUTROS", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
banco_atos_em_foco_ESCOLA = banco_atos_em_foco

banco_sem_concurso = distinct(banco_atos_em_foco, PROCESSO, name, ATO_INFRACIONAL, .keep_all= TRUE)

banco_incidencia = banco_sem_concurso

banco_SNR = distinct(banco_atos_em_foco, NOME2, NASCIMENTO, .keep_all= TRUE)

total_de_adolescentes_encaminhados = banco_SNR


#########################################################################################################

library(dplyr)

banco_incidencia = banco_incidencia %>%
  select(ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
###banco_incidencia
#########################################################################################################
#setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_incidencia_bkp = banco_incidencia

banco_incidencia_bkp =
  banco_incidencia_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(banco_incidencia_bkp)[1]<-'banco_incidencia_bkp'
colnames(banco_incidencia_bkp)[2]<-'QUANTIDADE'
colnames(banco_incidencia_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#para script rmd:
banco_incidencia_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_incidencia_bkp$PERCENTUAL))
banco_incidencia_bkp_rmd = tail(banco_incidencia_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_incidencia =
  banco_incidencia %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(ATO_INFRACIONAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

colnames(banco_incidencia)[1]<-'ATO'
colnames(banco_incidencia)[2]<-'QUANTIDADE'
colnames(banco_incidencia)[3]<-'PERCENTUAL'

#############################################################################################################

#banco_incidencia =
#  banco_incidencia %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
write.csv(banco_incidencia, file ="banco_incidencia_atual.csv",row.names=FALSE)
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_incidencia FIM
#########################################################################################################
#########################################################################################################
#INCIDENCIA COMPARADA. obs: trazer arquivo ano anterior para a pasta.
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
banco_incidencia_atual = banco_incidencia
#########################################################################################################
#INCIDENCIA COMPARADA. obs: trazer arquivo ano anterior para a pasta.
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#MUDANDO DIRETORIO PARA BUSCAR O ARQUIVO
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#Banco ano anterior
banco_incidencia_anterior <- read.csv("banco_incidencia_anterior.csv",header=TRUE, sep=",", dec=".", encoding = "UTF-8" )

#RETORNANDO PARA O DIRETÓRIO PADRÃO
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

colnames(banco_incidencia_anterior) <- c("ATO", "QUANTIDADE")

banco_incidencia_anterior = banco_incidencia_anterior %>%
  select(ATO, QUANTIDADE)



#juntando tudo

banco_incidencia_atual$PERCENTUAL <- NULL
banco_incidencia_atual$ATO[banco_incidencia_atual$ATO == "VOUTROS"]<- "OUTROS"

banco_incidencia_atual =
  banco_incidencia_atual |>
  filter(!ATO %in% "Total")

banco_incidencia_anterior =
  banco_incidencia_anterior |>
  filter(!ATO %in% "Total")

#mudando grafia de total

incidencia_comparada = full_join(banco_incidencia_anterior,banco_incidencia_atual, by="ATO")
incidencia_comparada

#NA POR ZERO

incidencia_comparada = replace(x = incidencia_comparada, list = is.na(incidencia_comparada), values = 0)


#renomeando colunas
colnames(incidencia_comparada) <- c("ATO", "ANOANTERIOR", "ANOATUAL")

incidencia_comparada =
  incidencia_comparada |>
  arrange(ATO)

#acrescentando linha com total
incidencia_comparada <- rbind(incidencia_comparada,
                              data.frame(ATO = "TOTAL", ANOANTERIOR = sum(incidencia_comparada$ANOANTERIOR), ANOATUAL = sum(incidencia_comparada$ANOATUAL),
                                         stringsAsFactors = FALSE))

incidencia_comparada$VAR <- round(((incidencia_comparada$ANOATUAL*100)/incidencia_comparada$ANOANTERIOR)-100, 2)


colnames(incidencia_comparada) <- c("ATO", format(Sys.Date()-365*2, "%Y"), format(Sys.Date()-365*1, "%Y"), "VAR%")
#colnames(incidencia_comparada) <- c("ATO", format(Sys.Date()-365*1, "%Y"), format(Sys.Date()-365*0, "%Y"), "VAR%")
#########################################################################################################

#########################################################################################################
# banco_incidencia FIM
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
