
#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR AMOSTRA:
dir.create(file.path("~/diretorio_r/estciabh", "amostra"))
setwd(file.path("~/diretorio_r/estciabh/amostra"))

#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)
banco_amostra = banco_tratado %>%
  select(NOME2, IDADE, SEXO, NASCIMENTO, DATA_ATO, N_QUEST,                         DATA_APLICACAO,
         ENTRVISTADOR,                    RACA_COR,                        ESTADO_CIVIL,                    POSSUI_FILHOS,
         QUANTOS,                         ESTA.GRAVIDA,                    POSSUI_DOC_01,                   POSSUI_DOC_02,
         POSSUI_DOC_03,                   POSSUI_DOC_04,                   POSSUI_DOC_05,                   POSSUI_DOC.06,
         ESTUDA_ATUALMENTE.1,             PQ_NAO_ESTUDA,                   NATUREZA_ESCOLA,                 SERIE_ATUAL_OU_ULTIMA_CURSADA,
         TRABALHA_ATUALMENTE,             NATUREZA_DO_TRABALHO,            RENDA_MENSAL,                    TEM.CURSO.PROFISSIONALIZANTE,
         SE_FEZ_QUAL._01,                 SE_FEZ_QUAL._02,                 TEM_INTERESSE_EM_FAZER_CURSO,    QUAL_AREA_DE_INTERESSE,
         BAIRRO,                          CIDADE,                          TIPO_MORADIA,                    NUMERO_MORADORES,
         NUMERO_COMODOS,                  NATUREZA_MORADIA,                ESTADO_CIVIL_PAIS,               COM_QUEM_RESIDE_01,
         COM_QUEM_RESIDE_02,              COM_QUEM_RESIDE_03,              HA_QUANTO_TEMPO,                 RENDA_FAMILIAR,
         NATUREZA_DO_TRABALHO_01,         QUEM_TRABALHA._01,               NATUREZA_DO_TRABALHO_02,         QUEM_TRABALHA._02,
         TEM_BENEFICIO_GOVERNAMENTAL_01,  TEM_BENEFICIO_GOVERNAMENTAL_02,  TEM_BENEFICIO_GOVERNAMENTAL_03,  TEM_BENEFICIO_GOVERNAMENTAL_04,
         ATIVIDADES_GRUPOS_SOCIAIS_01,    ATIVIDADES_GRUPOS_SOCIAIS_02,    ATIVIDADES_GRUPOS_SOCIAIS_03,    ATIVIDADES_GRUPOS_SOCIAIS_04,
         ATIVIDADES_GRUPOS_SOCIAIS_05,    ATIVIDADES_GRUPOS_SOCIAIS_06,    ATIVIDADES_GRUPOS_SOCIAIS_07,    ATIVIDADES_GRUPOS_SOCIAIS_08,
         USA_DROGAS_ATUALMENTE,           DROGAS_USO_01,                   DROGAS_USO_02,                   DROGAS_USO_03,
         DROGAS_USO_04,                   DROGAS_USO_05,                   DROGAS_USO_06,                   DROGAS_USO_07,
         DROGAS_USO_08,                   DROGA_MAIS_USADA,                TRATAMENTO_DE_DROGAS,            JA_FOI_APREENDIDO,
         JA_FOI_INTERNADO_NO_CEIP,        ESTA_CUMPRINDO_OU_CUMPRIU_MEDIDA_SOCIOEDUCATIVA,   QUAL_MEDIDA_01,                  QUAL_MEDIDA_02,
         QUAL_MEDIDA_03,                  QUAL_MEDIDA_04)

#write.csv(banco_amostra, file ="banco_amostra_inicial.csv",row.names=FALSE)
#write.xlsx(banco_amostra, file = "banco_amostra_inicial.xlsx")
head(banco_amostra, 8)[1:5]
#########################################################################################################

#########################################################################################################
#PREENCHER COM NA'S CELULAS VAZIAS
banco_amostra$N_QUEST[banco_amostra$N_QUEST == ""]<- NA
#VERIFICAR QUANTIDADE DE NA'S
#amostra = banco_amostra %>%
# na.omit() %>%
#group_by(N_QUEST)

##SEPARAR do banco_amostra a AMOSTRA
amostra<-subset(banco_amostra, !is.na(banco_amostra$N_QUEST))

head(amostra, 8)[1:5]
#APAGAR LINHA ESPECIFICA:
#amostra$N_QUEST <- as.character(amostra$N_QUEST)
#amostra[which(amostra$N_QUEST == 211), ] <-  NULL
#amostra <- subset(amostra, N_QUEST==211)
#amostra<-amostra[!(amostra$N_QUEST == "211"),]



#VERIFICAR QUANTIDADE DE NA'S

table(amostra$N_QUEST, useNA ="always")

#APAGAR NA'S EM amostra$N_QUEST
#amostra[which(amostra$N_QUEST == NA), ] <-  NULL

#write.csv(amostra, file ="amostra.csv",row.names=FALSE)
#write.xlsx(amostra, file = "amostra.xlsx")

names_amostra <- as_tibble(names(amostra))

names_amostra

#write.csv(names_amostra, file ="NAMES_AMOSTRA.csv",row.names=FALSE)

#amostra <- subset(amostra, DATA_ATO >= "2020-01-01" & DATA_ATO <= "2020-06-01")
amostra = amostra %>%
  #filter(DATA_ATO >= (str_c(format(Sys.Date()-365*0, "%Y"), "-01-01")) & DATA_ATO <= (str_c(format(Sys.Date()-365*0, "%Y"), "-12-31")))
  filter(DATA_ATO >= (str_c(format(Sys.Date()-365*1, "%Y"), "-01-01")) & DATA_ATO <= (str_c(format(Sys.Date()-365*1, "%Y"), "-12-31")))

#write.csv(amostra, file ="amostra.csv",row.names=FALSE)

entradas_periodo_sem_nomes_repetidos <- amostra[!duplicated(data.frame(amostra$NOME2, amostra$NASCIMENTO)),]

#export(amostra, "amostra1.csv")

##exemplo % com dois dígitos
#round(escolaridade01, 2)

####AMOSTRA SEM NOMES REPETIDOS, PARA ANALISE DOS DADOS SOCIOECONOMICOS#############

amostra_snr <- entradas_periodo_sem_nomes_repetidos
#write.csv(amostra_snr, file ="amostra_snr.csv",row.names=TRUE)
#write.xlsx(amostra_snr, file = "amostra_snr.xlsx")

amostra_snr_bkp = amostra_snr
#########################################################################################################
#GERAR ARQUIVO PARA ENTREGA A CEINFO: VÂNIA

arquivo_amostra_ceinfo = amostra_snr
arquivo_amostra_ceinfo$NOME2 <- NULL
arquivo_amostra_ceinfo$NASCIMENTO <- NULL
arquivo_amostra_ceinfo$DATA_ATO <- NULL

#acrescentando ID:
#arquivo_amostra_ceinfo$ID <- seq_along(1:nrow(arquivo_amostra_ceinfo))

arquivo_amostra_ceinfo =
arquivo_amostra_ceinfo %>% mutate(ID = seq_along(1:nrow(arquivo_amostra_ceinfo)), .before = IDADE)

#write.csv(arquivo_amostra_ceinfo, file ="arquivo_AMOSTRA_CEINFO.csv",row.names=TRUE)
write.csv(arquivo_amostra_ceinfo, file ="arquivo_AMOSTRA_CEINFO.csv",row.names=FALSE)
write_ods(arquivo_amostra_ceinfo, "arquivo_amostra_ceinfo.ods")

#FIM
#########################################################################################################

#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR AMOSTRA:

setwd(file.path("~/diretorio_r/estciabh/amostra"))
#########################################################################################################
#############################################################################################################
#RACA_COR
#########################################################################################################

RACA_COR =
  amostra_snr |>
  select(RACA_COR)

#########################################################################################################
RACA_COR$RACA_COR[RACA_COR$RACA_COR == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
RACA_COR$RACA_COR[RACA_COR$RACA_COR == "NAO SABE"]<- "VNÃO SABE"
RACA_COR$RACA_COR[RACA_COR$RACA_COR == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
RACA_COR$RACA_COR[RACA_COR$RACA_COR == "INDIGENA"]<- "INDÍGENA"
#########################################################################################################
# salvando para gráfico
RACA_COR_bkp = RACA_COR

RACA_COR_bkp =
  RACA_COR_bkp %>%
  janitor::tabyl(RACA_COR) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
RACA_COR_bkp$RACA_COR[RACA_COR_bkp$RACA_COR == "VNÃO SABE"]<- "NÃO SABE"
RACA_COR_bkp$RACA_COR[RACA_COR_bkp$RACA_COR == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
RACA_COR_bkp$PERCENTUAL2 <- str_replace (RACA_COR_bkp$percent, "%", "")
RACA_COR_bkp$PERCENTUAL2 = as.numeric(RACA_COR_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(RACA_COR_bkp)[1]<-'RACA_COR_bkp'
colnames(RACA_COR_bkp)[2]<-'QUANTIDADE'
colnames(RACA_COR_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
RACA_COR_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", RACA_COR_bkp$PERCENTUAL))
RACA_COR_bkp_rmd = tail(RACA_COR_bkp,3)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
RACA_COR$RACA_COR[RACA_COR$RACA_COR == "VNÃO SABE"]<- "UNÃO SABE"
#RACA_COR$RACA_COR[RACA_COR$RACA_COR == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
RACA_COR_TABELA =
  RACA_COR %>%
  janitor::tabyl(RACA_COR) %>%
  arrange(RACA_COR) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
RACA_COR_TABELA$RACA_COR[RACA_COR_TABELA$RACA_COR == "UNÃO SABE"]<- "NÃO SABE"
RACA_COR_TABELA$RACA_COR[RACA_COR_TABELA$RACA_COR == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(RACA_COR_TABELA)[1]<-'RAÇA/COR'
colnames(RACA_COR_TABELA)[2]<-'QUANTIDADE'
colnames(RACA_COR_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#RACA_COR FIM
#########################################################################################################
#############################################################################################################
#ESTADO_CIVIL
#########################################################################################################

ESTADO_CIVIL =
  amostra_snr |>
  select(ESTADO_CIVIL)

#########################################################################################################
ESTADO_CIVIL$ESTADO_CIVIL[ESTADO_CIVIL$ESTADO_CIVIL == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
ESTADO_CIVIL$ESTADO_CIVIL[ESTADO_CIVIL$ESTADO_CIVIL == "NAO SABE"]<- "VNÃO SABE"
ESTADO_CIVIL$ESTADO_CIVIL[ESTADO_CIVIL$ESTADO_CIVIL == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
ESTADO_CIVIL$ESTADO_CIVIL[ESTADO_CIVIL$ESTADO_CIVIL == "UNIAO ESTAVEL"]<- "UNIÃO ESTÁVEL"
#########################################################################################################
# salvando para gráfico
ESTADO_CIVIL_bkp = ESTADO_CIVIL

ESTADO_CIVIL_bkp =
  ESTADO_CIVIL_bkp %>%
  janitor::tabyl(ESTADO_CIVIL) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
ESTADO_CIVIL_bkp$ESTADO_CIVIL[ESTADO_CIVIL_bkp$ESTADO_CIVIL == "VNÃO SABE"]<- "NÃO SABE"
ESTADO_CIVIL_bkp$ESTADO_CIVIL[ESTADO_CIVIL_bkp$ESTADO_CIVIL == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
ESTADO_CIVIL_bkp$PERCENTUAL2 <- str_replace (ESTADO_CIVIL_bkp$percent, "%", "")
ESTADO_CIVIL_bkp$PERCENTUAL2 = as.numeric(ESTADO_CIVIL_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(ESTADO_CIVIL_bkp)[1]<-'ESTADO_CIVIL_bkp'
colnames(ESTADO_CIVIL_bkp)[2]<-'QUANTIDADE'
colnames(ESTADO_CIVIL_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESTADO_CIVIL_bkp_rmark = ESTADO_CIVIL_bkp

ESTADO_CIVIL_bkp_rmark = ESTADO_CIVIL_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
ESTADO_CIVIL_bkp_rmark =
  ESTADO_CIVIL_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
ESTADO_CIVIL$ESTADO_CIVIL[ESTADO_CIVIL$ESTADO_CIVIL == "VNÃO SABE"]<- "UNÃO SABE"
#ESTADO_CIVIL$ESTADO_CIVIL[ESTADO_CIVIL$ESTADO_CIVIL == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
ESTADO_CIVIL_TABELA =
  ESTADO_CIVIL %>%
  janitor::tabyl(ESTADO_CIVIL) %>%
  arrange(ESTADO_CIVIL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
ESTADO_CIVIL_TABELA$ESTADO_CIVIL[ESTADO_CIVIL_TABELA$ESTADO_CIVIL == "UNÃO SABE"]<- "NÃO SABE"
ESTADO_CIVIL_TABELA$ESTADO_CIVIL[ESTADO_CIVIL_TABELA$ESTADO_CIVIL == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(ESTADO_CIVIL_TABELA)[1]<-'ESTADO CIVIL'
colnames(ESTADO_CIVIL_TABELA)[2]<-'QUANTIDADE'
colnames(ESTADO_CIVIL_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#ESTADO_CIVIL FIM
#########################################################################################################

#############################################################################################################
#POSSUI_FILHOS
#########################################################################################################

POSSUI_FILHOS =
  amostra_snr |>
  select(POSSUI_FILHOS)

#########################################################################################################
POSSUI_FILHOS$POSSUI_FILHOS[POSSUI_FILHOS$POSSUI_FILHOS == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
POSSUI_FILHOS$POSSUI_FILHOS[POSSUI_FILHOS$POSSUI_FILHOS == "NAO SABE"]<- "VNÃO SABE"
#POSSUI_FILHOS$POSSUI_FILHOS[POSSUI_FILHOS$POSSUI_FILHOS == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
POSSUI_FILHOS$POSSUI_FILHOS[POSSUI_FILHOS$POSSUI_FILHOS == "NAO RESPONDEU"]<- "NÃO RESPONDEU"
POSSUI_FILHOS$POSSUI_FILHOS[POSSUI_FILHOS$POSSUI_FILHOS == "NAO"]<- "NÃO"
#########################################################################################################
# salvando para gráfico
POSSUI_FILHOS_bkp = POSSUI_FILHOS

POSSUI_FILHOS_bkp =
  POSSUI_FILHOS_bkp %>%
  janitor::tabyl(POSSUI_FILHOS) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
POSSUI_FILHOS_bkp$POSSUI_FILHOS[POSSUI_FILHOS_bkp$POSSUI_FILHOS == "VNÃO SABE"]<- "NÃO SABE"
POSSUI_FILHOS_bkp$POSSUI_FILHOS[POSSUI_FILHOS_bkp$POSSUI_FILHOS == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
POSSUI_FILHOS_bkp$PERCENTUAL2 <- str_replace (POSSUI_FILHOS_bkp$percent, "%", "")
POSSUI_FILHOS_bkp$PERCENTUAL2 = as.numeric(POSSUI_FILHOS_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(POSSUI_FILHOS_bkp)[1]<-'POSSUI_FILHOS_bkp'
colnames(POSSUI_FILHOS_bkp)[2]<-'QUANTIDADE'
colnames(POSSUI_FILHOS_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

POSSUI_FILHOS_bkp_rmark = POSSUI_FILHOS_bkp

POSSUI_FILHOS_bkp_rmark = POSSUI_FILHOS_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
POSSUI_FILHOS_bkp_rmark =
  POSSUI_FILHOS_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
POSSUI_FILHOS$POSSUI_FILHOS[POSSUI_FILHOS$POSSUI_FILHOS == "VNÃO SABE"]<- "UNÃO SABE"
#POSSUI_FILHOS$POSSUI_FILHOS[POSSUI_FILHOS$POSSUI_FILHOS == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
POSSUI_FILHOS_TABELA =
  POSSUI_FILHOS %>%
  janitor::tabyl(POSSUI_FILHOS) %>%
  arrange(POSSUI_FILHOS) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
POSSUI_FILHOS_TABELA$POSSUI_FILHOS[POSSUI_FILHOS_TABELA$POSSUI_FILHOS == "UNÃO SABE"]<- "NÃO SABE"
POSSUI_FILHOS_TABELA$POSSUI_FILHOS[POSSUI_FILHOS_TABELA$POSSUI_FILHOS == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(POSSUI_FILHOS_TABELA)[1]<-'FILHOS'
colnames(POSSUI_FILHOS_TABELA)[2]<-'QUANTIDADE'
colnames(POSSUI_FILHOS_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#POSSUI_FILHOS FIM
#########################################################################################################
#########################################################################################################
#############################################################################################################
#ESTA_GRAVIDA
#########################################################################################################

ESTA_GRAVIDA =
  amostra_snr |>
  filter(SEXO %in% "F") |> #SÓ SEXO FEMININO FICA GRÁVIDA
  select(ESTA.GRAVIDA)


colnames(ESTA_GRAVIDA)[1]<-'ESTA_GRAVIDA'
#########################################################################################################
ESTA_GRAVIDA$ESTA_GRAVIDA[ESTA_GRAVIDA$ESTA_GRAVIDA == "NSA"]<- "NÃO RESPONDEU"
ESTA_GRAVIDA$ESTA_GRAVIDA[ESTA_GRAVIDA$ESTA_GRAVIDA == "NAO SABE"]<- "VNÃO SABE"
ESTA_GRAVIDA$ESTA_GRAVIDA[ESTA_GRAVIDA$ESTA_GRAVIDA == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
ESTA_GRAVIDA$ESTA_GRAVIDA[ESTA_GRAVIDA$ESTA_GRAVIDA == "NAO"]<- "NÃO"
#########################################################################################################
# salvando para gráfico
ESTA_GRAVIDA_bkp = ESTA_GRAVIDA

ESTA_GRAVIDA_bkp =
  ESTA_GRAVIDA_bkp %>%
  janitor::tabyl(ESTA_GRAVIDA) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
ESTA_GRAVIDA_bkp$ESTA_GRAVIDA[ESTA_GRAVIDA_bkp$ESTA_GRAVIDA == "VNÃO SABE"]<- "NÃO SABE"
ESTA_GRAVIDA_bkp$ESTA_GRAVIDA[ESTA_GRAVIDA_bkp$ESTA_GRAVIDA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
ESTA_GRAVIDA_bkp$PERCENTUAL2 <- str_replace (ESTA_GRAVIDA_bkp$percent, "%", "")
ESTA_GRAVIDA_bkp$PERCENTUAL2 = as.numeric(ESTA_GRAVIDA_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(ESTA_GRAVIDA_bkp)[1]<-'ESTA_GRAVIDA_bkp'
colnames(ESTA_GRAVIDA_bkp)[2]<-'QUANTIDADE'
colnames(ESTA_GRAVIDA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESTA_GRAVIDA_bkp_rmark = ESTA_GRAVIDA_bkp

ESTA_GRAVIDA_bkp_rmark = ESTA_GRAVIDA_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
ESTA_GRAVIDA_bkp_rmark =
  ESTA_GRAVIDA_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
ESTA_GRAVIDA$ESTA_GRAVIDA[ESTA_GRAVIDA$ESTA_GRAVIDA == "VNÃO SABE"]<- "UNÃO SABE"
#ESTA_GRAVIDA$ESTA_GRAVIDA[ESTA_GRAVIDA$ESTA_GRAVIDA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
ESTA_GRAVIDA_TABELA =
  ESTA_GRAVIDA %>%
  janitor::tabyl(ESTA_GRAVIDA) %>%
  arrange(ESTA_GRAVIDA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
ESTA_GRAVIDA_TABELA$ESTA_GRAVIDA[ESTA_GRAVIDA_TABELA$ESTA_GRAVIDA == "UNÃO SABE"]<- "NÃO SABE"
ESTA_GRAVIDA_TABELA$ESTA_GRAVIDA[ESTA_GRAVIDA_TABELA$ESTA_GRAVIDA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(ESTA_GRAVIDA_TABELA)[1]<-'GRAVIDEZ'
colnames(ESTA_GRAVIDA_TABELA)[2]<-'QUANTIDADE'
colnames(ESTA_GRAVIDA_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#ESTA_GRAVIDA FIM
#########################################################################################################
#############################################################################################################
#POSSUI_DOC
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
POSSUI_DOC =

  amostra_snr %>%
  pivot_longer(cols = starts_with("POSSUI_DOC"), values_to = "POSSUI_DOC_GERAL") %>%
  #select(-name) %>%
  filter(POSSUI_DOC_GERAL != "NSA")
#########################################################################################################

POSSUI_DOC =
  POSSUI_DOC |>
  select(POSSUI_DOC_GERAL)

colnames(POSSUI_DOC)[1]<-'POSSUI_DOC'
#########################################################################################################
POSSUI_DOC$POSSUI_DOC[POSSUI_DOC$POSSUI_DOC == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
POSSUI_DOC$POSSUI_DOC[POSSUI_DOC$POSSUI_DOC == "NAO SABE"]<- "VNÃO SABE"
POSSUI_DOC$POSSUI_DOC[POSSUI_DOC$POSSUI_DOC == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
POSSUI_DOC$POSSUI_DOC[POSSUI_DOC$POSSUI_DOC == "NAO"]<- "NÃO"
#########################################################################################################
# salvando para gráfico
POSSUI_DOC_bkp = POSSUI_DOC

POSSUI_DOC_bkp =
  POSSUI_DOC_bkp %>%
  janitor::tabyl(POSSUI_DOC) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
POSSUI_DOC_bkp$POSSUI_DOC[POSSUI_DOC_bkp$POSSUI_DOC == "VNÃO SABE"]<- "NÃO SABE"
POSSUI_DOC_bkp$POSSUI_DOC[POSSUI_DOC_bkp$POSSUI_DOC == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
POSSUI_DOC_bkp$PERCENTUAL2 <- str_replace (POSSUI_DOC_bkp$percent, "%", "")
POSSUI_DOC_bkp$PERCENTUAL2 = as.numeric(POSSUI_DOC_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(POSSUI_DOC_bkp)[1]<-'POSSUI_DOC_bkp'
colnames(POSSUI_DOC_bkp)[2]<-'QUANTIDADE'
colnames(POSSUI_DOC_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

POSSUI_DOC_bkp_rmark = POSSUI_DOC_bkp

POSSUI_DOC_bkp_rmark = POSSUI_DOC_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
POSSUI_DOC_bkp_rmark =
  POSSUI_DOC_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
POSSUI_DOC$POSSUI_DOC[POSSUI_DOC$POSSUI_DOC == "VNÃO SABE"]<- "UNÃO SABE"
#POSSUI_DOC$POSSUI_DOC[POSSUI_DOC$POSSUI_DOC == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
POSSUI_DOC_TABELA =
  POSSUI_DOC %>%
  janitor::tabyl(POSSUI_DOC) %>%
  arrange(POSSUI_DOC) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
POSSUI_DOC_TABELA$POSSUI_DOC[POSSUI_DOC_TABELA$POSSUI_DOC == "UNÃO SABE"]<- "NÃO SABE"
POSSUI_DOC_TABELA$POSSUI_DOC[POSSUI_DOC_TABELA$POSSUI_DOC == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(POSSUI_DOC_TABELA)[1]<-'TIPO DOCUMENTO'
colnames(POSSUI_DOC_TABELA)[2]<-'QUANTIDADE'
colnames(POSSUI_DOC_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#POSSUI_DOC FIM
#########################################################################################################
#########################################################################################################
#############################################################################################################
#SERIE_ATUAL_OU_ULTIMA_CURSADA
#########################################################################################################

SERIE_ATUAL_OU_ULTIMA_CURSADA =
  amostra_snr |>
  select(SERIE_ATUAL_OU_ULTIMA_CURSADA)


SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA = ajustar_nomes(SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA)
#########################################################################################################
#AJUSTA OS FORA DE PADRÃO AQUI:
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "EJA"]<- "EJAENSFUND" #FIZ OPÇÃO PELO FUND

#ORDENANDO

SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "1ªSERIE-ENSFUND"]<- "A1ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "2ªSERIE-ENSFUND"]<- "B2ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "3ªSERIE-ENSFUND"]<- "C3ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "4ªSERIE-ENSFUND"]<- "D4ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "5ªSERIE-ENSFUND"]<- "E5ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "6ªSERIE-ENSFUND"]<- "F6ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "7ªSERIE-ENSFUND"]<- "G7ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "8ªSERIE-ENSFUND"]<- "H8ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "9ªSERIE-ENSFUND"]<- "I9ªSERIE-ENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "1ºANO-ENSMEDIO"]<- "J1ºANO-ENSMEDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "2ºANO-ENSMEDIO"]<- "K2ºANO-ENSMEDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "3ºANO-ENSMEDIO"]<- "L3ºANO-ENSMEDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "EJAENSFUND"]<- "MEJAENSFUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "EJAENSMEDIO"]<- "NEJAENSMEDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "NAOSABE"]<- "ONAOSABE"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "NAORESPONDEU"]<- "PNAORESPONDEU"
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "SEMINFORMACAO"]<- "QSEMINFORMACAO"

#########################################################################################################
# salvando para gráfico
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp = SERIE_ATUAL_OU_ULTIMA_CURSADA

SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp =
  SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp %>%
  janitor::tabyl(SERIE_ATUAL_OU_ULTIMA_CURSADA) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "MEJAENSFUND"]<- "EJA - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "ONAOSABE"]<- "NÃO SABE"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$SERIE_ATUAL_OU_ULTIMA_CURSADA == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"


#########################################################################################################
#replace "%" with "" in the percentual column
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$PERCENTUAL2 <- str_replace (SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$percent, "%", "")
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$PERCENTUAL2 = as.numeric(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp)[1]<-'SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp'
colnames(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp)[2]<-'QUANTIDADE'
colnames(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp_rmark = SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp

SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp_rmark = SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp_rmark =
  SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "VNÃO SABE"]<- "UNÃO SABE"
#SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA =
  SERIE_ATUAL_OU_ULTIMA_CURSADA %>%
  janitor::tabyl(SERIE_ATUAL_OU_ULTIMA_CURSADA) %>%
  arrange(SERIE_ATUAL_OU_ULTIMA_CURSADA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#ordenando:

SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "MEJAENSFUND"]<- "EJA - ENS FUND"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "ONAOSABE"]<- "NÃO SABE"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"
SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA[SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA$SERIE_ATUAL_OU_ULTIMA_CURSADA == "NSA"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA)[1]<-'ESCOLARIDADE'
colnames(SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA)[2]<-'QUANTIDADE'
colnames(SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#SERIE_ATUAL_OU_ULTIMA_CURSADA FIM
#########################################################################################################
#############################################################################################################
#NATUREZA_ESCOLA
#########################################################################################################

NATUREZA_ESCOLA =
  amostra_snr |>
  select(NATUREZA_ESCOLA)

#########################################################################################################
NATUREZA_ESCOLA$NATUREZA_ESCOLA[NATUREZA_ESCOLA$NATUREZA_ESCOLA == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
NATUREZA_ESCOLA$NATUREZA_ESCOLA[NATUREZA_ESCOLA$NATUREZA_ESCOLA == "NAO SABE"]<- "VNÃO SABE"
NATUREZA_ESCOLA$NATUREZA_ESCOLA[NATUREZA_ESCOLA$NATUREZA_ESCOLA == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
NATUREZA_ESCOLA$NATUREZA_ESCOLA[NATUREZA_ESCOLA$NATUREZA_ESCOLA == "PUBLICA"]<- "PÚBLICA"
#########################################################################################################
# salvando para gráfico
NATUREZA_ESCOLA_bkp = NATUREZA_ESCOLA

NATUREZA_ESCOLA_bkp =
  NATUREZA_ESCOLA_bkp %>%
  janitor::tabyl(NATUREZA_ESCOLA) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
NATUREZA_ESCOLA_bkp$NATUREZA_ESCOLA[NATUREZA_ESCOLA_bkp$NATUREZA_ESCOLA == "VNÃO SABE"]<- "NÃO SABE"
NATUREZA_ESCOLA_bkp$NATUREZA_ESCOLA[NATUREZA_ESCOLA_bkp$NATUREZA_ESCOLA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
NATUREZA_ESCOLA_bkp$PERCENTUAL2 <- str_replace (NATUREZA_ESCOLA_bkp$percent, "%", "")
NATUREZA_ESCOLA_bkp$PERCENTUAL2 = as.numeric(NATUREZA_ESCOLA_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(NATUREZA_ESCOLA_bkp)[1]<-'NATUREZA_ESCOLA_bkp'
colnames(NATUREZA_ESCOLA_bkp)[2]<-'QUANTIDADE'
colnames(NATUREZA_ESCOLA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

NATUREZA_ESCOLA_bkp_rmark = NATUREZA_ESCOLA_bkp

NATUREZA_ESCOLA_bkp_rmark = NATUREZA_ESCOLA_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
NATUREZA_ESCOLA_bkp_rmark =
  NATUREZA_ESCOLA_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
NATUREZA_ESCOLA$NATUREZA_ESCOLA[NATUREZA_ESCOLA$NATUREZA_ESCOLA == "VNÃO SABE"]<- "UNÃO SABE"
#NATUREZA_ESCOLA$NATUREZA_ESCOLA[NATUREZA_ESCOLA$NATUREZA_ESCOLA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
NATUREZA_ESCOLA_TABELA =
  NATUREZA_ESCOLA %>%
  janitor::tabyl(NATUREZA_ESCOLA) %>%
  arrange(NATUREZA_ESCOLA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
NATUREZA_ESCOLA_TABELA$NATUREZA_ESCOLA[NATUREZA_ESCOLA_TABELA$NATUREZA_ESCOLA == "UNÃO SABE"]<- "NÃO SABE"
NATUREZA_ESCOLA_TABELA$NATUREZA_ESCOLA[NATUREZA_ESCOLA_TABELA$NATUREZA_ESCOLA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(NATUREZA_ESCOLA_TABELA)[1]<-'NATUREZA DA ESCOLA'
colnames(NATUREZA_ESCOLA_TABELA)[2]<-'QUANTIDADE'
colnames(NATUREZA_ESCOLA_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#NATUREZA_ESCOLA FIM
#########################################################################################################
#########################################################################################################
#############################################################################################################
#TRABALHA_ATUALMENTE
#########################################################################################################

TRABALHA_ATUALMENTE =
  amostra_snr |>
  select(TRABALHA_ATUALMENTE)

#########################################################################################################
TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE == "NAO SABE"]<- "VNÃO SABE"
TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE == "NAO"]<- "NÃO"
#########################################################################################################
# salvando para gráfico
TRABALHA_ATUALMENTE_bkp = TRABALHA_ATUALMENTE

TRABALHA_ATUALMENTE_bkp =
  TRABALHA_ATUALMENTE_bkp %>%
  janitor::tabyl(TRABALHA_ATUALMENTE) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
TRABALHA_ATUALMENTE_bkp$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE_bkp$TRABALHA_ATUALMENTE == "VNÃO SABE"]<- "NÃO SABE"
TRABALHA_ATUALMENTE_bkp$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE_bkp$TRABALHA_ATUALMENTE == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
TRABALHA_ATUALMENTE_bkp$PERCENTUAL2 <- str_replace (TRABALHA_ATUALMENTE_bkp$percent, "%", "")
TRABALHA_ATUALMENTE_bkp$PERCENTUAL2 = as.numeric(TRABALHA_ATUALMENTE_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(TRABALHA_ATUALMENTE_bkp)[1]<-'TRABALHA_ATUALMENTE_bkp'
colnames(TRABALHA_ATUALMENTE_bkp)[2]<-'QUANTIDADE'
colnames(TRABALHA_ATUALMENTE_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

TRABALHA_ATUALMENTE_bkp_rmark = TRABALHA_ATUALMENTE_bkp

TRABALHA_ATUALMENTE_bkp_rmark = TRABALHA_ATUALMENTE_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
TRABALHA_ATUALMENTE_bkp_rmark =
  TRABALHA_ATUALMENTE_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE == "VNÃO SABE"]<- "UNÃO SABE"
#TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE$TRABALHA_ATUALMENTE == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
TRABALHA_ATUALMENTE_TABELA =
  TRABALHA_ATUALMENTE %>%
  janitor::tabyl(TRABALHA_ATUALMENTE) %>%
  arrange(TRABALHA_ATUALMENTE) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
TRABALHA_ATUALMENTE_TABELA$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE_TABELA$TRABALHA_ATUALMENTE == "UNÃO SABE"]<- "NÃO SABE"
TRABALHA_ATUALMENTE_TABELA$TRABALHA_ATUALMENTE[TRABALHA_ATUALMENTE_TABELA$TRABALHA_ATUALMENTE == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(TRABALHA_ATUALMENTE_TABELA)[1]<-'TRABALHO ATUAL'
colnames(TRABALHA_ATUALMENTE_TABELA)[2]<-'QUANTIDADE'
colnames(TRABALHA_ATUALMENTE_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#TRABALHA_ATUALMENTE FIM
#########################################################################################################
#########################################################################################################
#############################################################################################################
#RENDA_MENSAL
#########################################################################################################

RENDA_MENSAL =
  amostra_snr |>
  filter(TRABALHA_ATUALMENTE %in% "SIM") |>
  select(RENDA_MENSAL)

#########################################################################################################

RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "ATE 1 SALARIO MININO "]<- "ATE 1 SALÁRIO MÍNIMO"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "DE  2  A 3 SALARIOS MININOS ( DE R$1874,00 A R$2811,00)"]<- "DE  2  A 3 SALARIOS MININOS"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "ACIMA DE 4 SALARIOS MINIMOS ( ACIMA DE R$3748,00)"]<- "EACIMA DE 4 SALARIOS MINIMOS"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "DE 1 A 2 SALARIOS MINIMOS"]<- "ATEA 1 A 2 SALARIOS MINIMOS"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == ""]<- "NÃO RESPONDEU"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "NSA"]<- "NÃO RESPONDEU"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "NÃO SABE"]<- "VNÃO SABE"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
RENDA_MENSAL[order(RENDA_MENSAL$RENDA_MENSAL),]#ordenar, crescente, nome2
#########################################################################################################



RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "NAO SABE"]<- "VNÃO SABE"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "UNIAO ESTAVEL"]<- "UNIÃO ESTÁVEL"
#########################################################################################################
# salvando para gráfico
RENDA_MENSAL_bkp = RENDA_MENSAL

RENDA_MENSAL_bkp =
  RENDA_MENSAL_bkp %>%
  janitor::tabyl(RENDA_MENSAL) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
RENDA_MENSAL_bkp$RENDA_MENSAL[RENDA_MENSAL_bkp$RENDA_MENSAL == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
RENDA_MENSAL_bkp$RENDA_MENSAL[RENDA_MENSAL_bkp$RENDA_MENSAL == "VNÃO SABE"]<- "NÃO SABE"
RENDA_MENSAL_bkp$RENDA_MENSAL[RENDA_MENSAL_bkp$RENDA_MENSAL == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
RENDA_MENSAL_bkp$PERCENTUAL2 <- str_replace (RENDA_MENSAL_bkp$percent, "%", "")
RENDA_MENSAL_bkp$PERCENTUAL2 = as.numeric(RENDA_MENSAL_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(RENDA_MENSAL_bkp)[1]<-'RENDA_MENSAL_bkp'
colnames(RENDA_MENSAL_bkp)[2]<-'QUANTIDADE'
colnames(RENDA_MENSAL_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

RENDA_MENSAL_bkp_rmark = RENDA_MENSAL_bkp

RENDA_MENSAL_bkp_rmark = RENDA_MENSAL_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
RENDA_MENSAL_bkp_rmark =
  RENDA_MENSAL_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "VNÃO SABE"]<- "UNÃO SABE"
#RENDA_MENSAL$RENDA_MENSAL[RENDA_MENSAL$RENDA_MENSAL == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
RENDA_MENSAL_TABELA =
  RENDA_MENSAL %>%
  janitor::tabyl(RENDA_MENSAL) %>%
  arrange(RENDA_MENSAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
RENDA_MENSAL_TABELA$RENDA_MENSAL[RENDA_MENSAL_TABELA$RENDA_MENSAL == "VNÃO SABE"]<- "NÃO SABE"
RENDA_MENSAL_TABELA$RENDA_MENSAL[RENDA_MENSAL_TABELA$RENDA_MENSAL == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
RENDA_MENSAL_TABELA$RENDA_MENSAL[RENDA_MENSAL_TABELA$RENDA_MENSAL == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
RENDA_MENSAL_TABELA$RENDA_MENSAL[RENDA_MENSAL_TABELA$RENDA_MENSAL == "EACIMA DE 4 SALARIOS MINIMOS"]<- "MAIS DE 4 SALÁRIOS MÍNIMOS"
RENDA_MENSAL_TABELA$RENDA_MENSAL[RENDA_MENSAL_TABELA$RENDA_MENSAL == "UNÃO SABE"]<- "NÃO SABE"
RENDA_MENSAL_TABELA$RENDA_MENSAL[RENDA_MENSAL_TABELA$RENDA_MENSAL == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(RENDA_MENSAL_TABELA)[1]<-'RENDA MENSAL'
colnames(RENDA_MENSAL_TABELA)[2]<-'QUANTIDADE'
colnames(RENDA_MENSAL_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#RENDA_MENSAL FIM
#########################################################################################################
#############################################################################################################
#RENDA_FAMILIAR
#########################################################################################################

RENDA_FAMILIAR =
  amostra_snr |>
  #filter(TRABALHA_ATUALMENTE %in% "SIM") |>
  select(RENDA_FAMILIAR)

#########################################################################################################

RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "ATE 1 SALARIO MININO "]<- "ATE 1 SALÁRIO MÍNIMO"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "DE  2  A 3 SALARIOS MININOS ( DE R$1874,00 A R$2811,00)"]<- "DE  2  A 3 SALARIOS MININOS"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "ACIMA DE 4 SALARIOS MINIMOS ( ACIMA DE R$3748,00)"]<- "EACIMA DE 4 SALARIOS MINIMOS"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "DE 1 A 2 SALARIOS MINIMOS"]<- "ATEA 1 A 2 SALARIOS MINIMOS"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == ""]<- "NÃO RESPONDEU"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "NSA"]<- "NÃO RESPONDEU"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "NÃO SABE"]<- "VNÃO SABE"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
RENDA_FAMILIAR[order(RENDA_FAMILIAR$RENDA_FAMILIAR),]#ordenar, crescente, nome2
#########################################################################################################



RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "NAO SABE"]<- "VNÃO SABE"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "UNIAO ESTAVEL"]<- "UNIÃO ESTÁVEL"
#########################################################################################################
# salvando para gráfico
RENDA_FAMILIAR_bkp = RENDA_FAMILIAR

RENDA_FAMILIAR_bkp =
  RENDA_FAMILIAR_bkp %>%
  janitor::tabyl(RENDA_FAMILIAR) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
RENDA_FAMILIAR_bkp$RENDA_FAMILIAR[RENDA_FAMILIAR_bkp$RENDA_FAMILIAR == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
RENDA_FAMILIAR_bkp$RENDA_FAMILIAR[RENDA_FAMILIAR_bkp$RENDA_FAMILIAR == "VNÃO SABE"]<- "NÃO SABE"
RENDA_FAMILIAR_bkp$RENDA_FAMILIAR[RENDA_FAMILIAR_bkp$RENDA_FAMILIAR == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
RENDA_FAMILIAR_bkp$PERCENTUAL2 <- str_replace (RENDA_FAMILIAR_bkp$percent, "%", "")
RENDA_FAMILIAR_bkp$PERCENTUAL2 = as.numeric(RENDA_FAMILIAR_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(RENDA_FAMILIAR_bkp)[1]<-'RENDA_FAMILIAR_bkp'
colnames(RENDA_FAMILIAR_bkp)[2]<-'QUANTIDADE'
colnames(RENDA_FAMILIAR_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

RENDA_FAMILIAR_bkp_rmark = RENDA_FAMILIAR_bkp

RENDA_FAMILIAR_bkp_rmark = RENDA_FAMILIAR_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
RENDA_FAMILIAR_bkp_rmark =
  RENDA_FAMILIAR_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "ACIMA DE 5 SALARIOS MINIMOS "]<- "EACIMA DE 5 SALARIOS MINIMOS"
RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "VNÃO SABE"]<- "UNÃO SABE"
#RENDA_FAMILIAR$RENDA_FAMILIAR[RENDA_FAMILIAR$RENDA_FAMILIAR == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
RENDA_FAMILIAR_TABELA =
  RENDA_FAMILIAR %>%
  janitor::tabyl(RENDA_FAMILIAR) %>%
  arrange(RENDA_FAMILIAR) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR[RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR == "VNÃO SABE"]<- "NÃO SABE"
RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR[RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR[RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR[RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR == "EACIMA DE 4 SALARIOS MINIMOS"]<- "MAIS DE 4 SALÁRIOS MÍNIMOS"
RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR[RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR == "UNÃO SABE"]<- "NÃO SABE"
RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR[RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR[RENDA_FAMILIAR_TABELA$RENDA_FAMILIAR == "EACIMA DE 5 SALARIOS MINIMOS"]<- "ACIMA DE 5 SALARIOS MINIMOS"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(RENDA_FAMILIAR_TABELA)[1]<-'RENDA MENSAL'
colnames(RENDA_FAMILIAR_TABELA)[2]<-'QUANTIDADE'
colnames(RENDA_FAMILIAR_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#RENDA_FAMILIAR FIM
#########################################################################################################

#############################################################################################################
#TIPO_MORADIA
#########################################################################################################

TIPO_MORADIA =
  amostra_snr |>
  #filter(TRABALHA_ATUALMENTE %in% "SIM") |>
  select(TIPO_MORADIA)

#########################################################################################################

TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == ""]<- "NÃO RESPONDEU"
TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "NSA"]<- "NÃO RESPONDEU"
TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "NÃO SABE"]<- "VNÃO SABE"
TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
TIPO_MORADIA[order(TIPO_MORADIA$TIPO_MORADIA),]#ordenar, crescente, nome2
#########################################################################################################



TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "NAO SABE"]<- "VNÃO SABE"
TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "UNIAO ESTAVEL"]<- "UNIÃO ESTÁVEL"
#########################################################################################################
# salvando para gráfico
TIPO_MORADIA_bkp = TIPO_MORADIA

TIPO_MORADIA_bkp =
  TIPO_MORADIA_bkp %>%
  janitor::tabyl(TIPO_MORADIA) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
TIPO_MORADIA_bkp$TIPO_MORADIA[TIPO_MORADIA_bkp$TIPO_MORADIA == "VNÃO SABE"]<- "NÃO SABE"
TIPO_MORADIA_bkp$TIPO_MORADIA[TIPO_MORADIA_bkp$TIPO_MORADIA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
TIPO_MORADIA_bkp$PERCENTUAL2 <- str_replace (TIPO_MORADIA_bkp$percent, "%", "")
TIPO_MORADIA_bkp$PERCENTUAL2 = as.numeric(TIPO_MORADIA_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(TIPO_MORADIA_bkp)[1]<-'TIPO_MORADIA_bkp'
colnames(TIPO_MORADIA_bkp)[2]<-'QUANTIDADE'
colnames(TIPO_MORADIA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

TIPO_MORADIA_bkp_rmark = TIPO_MORADIA_bkp

TIPO_MORADIA_bkp_rmark = TIPO_MORADIA_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
TIPO_MORADIA_bkp_rmark =
  TIPO_MORADIA_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "VNÃO SABE"]<- "UNÃO SABE"
#TIPO_MORADIA$TIPO_MORADIA[TIPO_MORADIA$TIPO_MORADIA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
TIPO_MORADIA_TABELA =
  TIPO_MORADIA %>%
  janitor::tabyl(TIPO_MORADIA) %>%
  arrange(TIPO_MORADIA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
TIPO_MORADIA_TABELA$TIPO_MORADIA[TIPO_MORADIA_TABELA$TIPO_MORADIA == "UNÃO SABE"]<- "NÃO SABE"
TIPO_MORADIA_TABELA$TIPO_MORADIA[TIPO_MORADIA_TABELA$TIPO_MORADIA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(TIPO_MORADIA_TABELA)[1]<-'TIPO DE MORADIA'
colnames(TIPO_MORADIA_TABELA)[2]<-'QUANTIDADE'
colnames(TIPO_MORADIA_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#TIPO_MORADIA FIM
#########################################################################################################
#########################################################################################################
#############################################################################################################
#NATUREZA_MORADIA
#########################################################################################################

NATUREZA_MORADIA =
  amostra_snr |>
  #filter(TRABALHA_ATUALMENTE %in% "SIM") |>
  select(NATUREZA_MORADIA)

#########################################################################################################

NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == ""]<- "NÃO RESPONDEU"
NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "NSA"]<- "NÃO RESPONDEU"
NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "NÃO SABE"]<- "VNÃO SABE"
NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
NATUREZA_MORADIA[order(NATUREZA_MORADIA$NATUREZA_MORADIA),]#ordenar, crescente, nome2
#########################################################################################################



NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "NAO SABE"]<- "VNÃO SABE"
NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "UNIAO ESTAVEL"]<- "UNIÃO ESTÁVEL"
#########################################################################################################
# salvando para gráfico
NATUREZA_MORADIA_bkp = NATUREZA_MORADIA

NATUREZA_MORADIA_bkp =
  NATUREZA_MORADIA_bkp %>%
  janitor::tabyl(NATUREZA_MORADIA) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
NATUREZA_MORADIA_bkp$NATUREZA_MORADIA[NATUREZA_MORADIA_bkp$NATUREZA_MORADIA == "VNÃO SABE"]<- "NÃO SABE"
NATUREZA_MORADIA_bkp$NATUREZA_MORADIA[NATUREZA_MORADIA_bkp$NATUREZA_MORADIA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
NATUREZA_MORADIA_bkp$PERCENTUAL2 <- str_replace (NATUREZA_MORADIA_bkp$percent, "%", "")
NATUREZA_MORADIA_bkp$PERCENTUAL2 = as.numeric(NATUREZA_MORADIA_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(NATUREZA_MORADIA_bkp)[1]<-'NATUREZA_MORADIA_bkp'
colnames(NATUREZA_MORADIA_bkp)[2]<-'QUANTIDADE'
colnames(NATUREZA_MORADIA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

NATUREZA_MORADIA_bkp_rmark = NATUREZA_MORADIA_bkp

NATUREZA_MORADIA_bkp_rmark = NATUREZA_MORADIA_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
NATUREZA_MORADIA_bkp_rmark =
  NATUREZA_MORADIA_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "VNÃO SABE"]<- "UNÃO SABE"
#NATUREZA_MORADIA$NATUREZA_MORADIA[NATUREZA_MORADIA$NATUREZA_MORADIA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
NATUREZA_MORADIA_TABELA =
  NATUREZA_MORADIA %>%
  janitor::tabyl(NATUREZA_MORADIA) %>%
  arrange(NATUREZA_MORADIA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
NATUREZA_MORADIA_TABELA$NATUREZA_MORADIA[NATUREZA_MORADIA_TABELA$NATUREZA_MORADIA == "UNÃO SABE"]<- "NÃO SABE"
NATUREZA_MORADIA_TABELA$NATUREZA_MORADIA[NATUREZA_MORADIA_TABELA$NATUREZA_MORADIA == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(NATUREZA_MORADIA_TABELA)[1]<-'NATUREZA DA PROPRIEDADE'
colnames(NATUREZA_MORADIA_TABELA)[2]<-'QUANTIDADE'
colnames(NATUREZA_MORADIA_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#NATUREZA_MORADIA FIM
#########################################################################################################
#############################################################################################################
#DROGAS_USO
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
DROGAS_USO =

  amostra_snr %>%
  filter(USA_DROGAS_ATUALMENTE %in% "SIM") |>
  pivot_longer(cols = starts_with("DROGAS_USO"), values_to = "DROGAS_USO_GERAL") %>%
  #select(-name) %>%
  filter(DROGAS_USO_GERAL != "NSA")
#########################################################################################################
#Para script informando o quantidade de adolescentes que responderam SIM ao USA_DROGAS_ATUALMENTE
DROGAS_USO_ATUAL =

  amostra_snr %>%
  filter(USA_DROGAS_ATUALMENTE %in% "SIM")

#########################################################################################################

DROGAS_USO =
  DROGAS_USO |>
  select(DROGAS_USO_GERAL)

colnames(DROGAS_USO)[1]<-'DROGAS_USO'
#########################################################################################################
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "NAO RESPONDEU"]<- "NÃO RESPONDEU"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == ""]<- "NÃO RESPONDEU"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "NSA"]<- "NÃO RESPONDEU"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "SOLVENTES/INALANTES(THINNER,COLA,LOLO,LANCA PERFUME)"]<- "SOLVENTES/INALANTES"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "PSICOFARMACOS( EXTASE,REMEDIOS DE TARJA PRETA,ANSIOLITICOS,ANTIDEPRESSIVOS)"]<- "PSICOFÁRMACOS"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "PSICOFARMACOS( REMEDIOS DE TARJA PRETA,ANSIOLITICOS,ANTIDEPRESSIVOS)"]<- "PSICOFÁRMACOS"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "NÃO SABE"]<- "VNÃO SABE"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "NAO SABE"]<- "VNÃO SABE"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "COCAINA"]<- "COCAÍNA"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "EXTASE"]<- "ÊXTASE"
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "ALCOOL"]<- "ÁLCOOL"
#########################################################################################################
# salvando para gráfico
DROGAS_USO_bkp = DROGAS_USO

DROGAS_USO_bkp =
  DROGAS_USO_bkp %>%
  janitor::tabyl(DROGAS_USO) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
DROGAS_USO_bkp$DROGAS_USO[DROGAS_USO_bkp$DROGAS_USO == "VNÃO SABE"]<- "NÃO SABE"
DROGAS_USO_bkp$DROGAS_USO[DROGAS_USO_bkp$DROGAS_USO == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
DROGAS_USO_bkp$PERCENTUAL2 <- str_replace (DROGAS_USO_bkp$percent, "%", "")
DROGAS_USO_bkp$PERCENTUAL2 = as.numeric(DROGAS_USO_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(DROGAS_USO_bkp)[1]<-'DROGAS_USO_bkp'
colnames(DROGAS_USO_bkp)[2]<-'QUANTIDADE'
colnames(DROGAS_USO_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

DROGAS_USO_bkp_rmark = DROGAS_USO_bkp

DROGAS_USO_bkp_rmark = DROGAS_USO_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
DROGAS_USO_bkp_rmark =
  DROGAS_USO_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "VNÃO SABE"]<- "UNÃO SABE"
#DROGAS_USO$DROGAS_USO[DROGAS_USO$DROGAS_USO == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
DROGAS_USO_TABELA =
  DROGAS_USO %>%
  janitor::tabyl(DROGAS_USO) %>%
  arrange(DROGAS_USO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
DROGAS_USO_TABELA$DROGAS_USO[DROGAS_USO_TABELA$DROGAS_USO == "UNÃO SABE"]<- "NÃO SABE"
DROGAS_USO_TABELA$DROGAS_USO[DROGAS_USO_TABELA$DROGAS_USO == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(DROGAS_USO_TABELA)[1]<-'DROGA'
colnames(DROGAS_USO_TABELA)[2]<-'QUANTIDADE'
colnames(DROGAS_USO_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#############################################################################################################
#DROGAS_USO FIM
#########################################################################################################


#########################################################################################################
# DROGAS_USO FIM
#########################################################################################################

