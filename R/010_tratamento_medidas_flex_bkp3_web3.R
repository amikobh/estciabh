#############################################################################################################
#BANCO_MEDIDAS
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
BANCO_MEDIDAS =

  banco_tratado |>
  filter(MEDIDA_PROTETIVA %in% "SIM") |>
  pivot_longer(cols = starts_with("QUAL_MEDIDA_PROTETIVA_0"), values_to = "BANCO_MEDIDAS_GERAL") %>%
  #select(-name) %>%
  filter(BANCO_MEDIDAS_GERAL != "NSA")
#########################################################################################################

BANCO_MEDIDAS =
  BANCO_MEDIDAS |>
  select(BANCO_MEDIDAS_GERAL)

colnames(BANCO_MEDIDAS)[1]<-'BANCO_MEDIDAS'
#########################################################################################################
#########################################################################################################
#########################################################################################################
BANCO_MEDIDAS$BANCO_MEDIDAS = ifelse(BANCO_MEDIDAS$BANCO_MEDIDAS == "1",
                                     "ART. 101, I", BANCO_MEDIDAS$BANCO_MEDIDAS)

table(BANCO_MEDIDAS$BANCO_MEDIDAS)
BANCO_MEDIDAS$BANCO_MEDIDAS = ifelse(BANCO_MEDIDAS$BANCO_MEDIDAS == "2",
                                     "ART. 101, II", BANCO_MEDIDAS$BANCO_MEDIDAS)

table(BANCO_MEDIDAS$BANCO_MEDIDAS)
BANCO_MEDIDAS$BANCO_MEDIDAS = ifelse(BANCO_MEDIDAS$BANCO_MEDIDAS == "3",
                                     "ART. 101, III", BANCO_MEDIDAS$BANCO_MEDIDAS)

table(BANCO_MEDIDAS$BANCO_MEDIDAS)
BANCO_MEDIDAS$BANCO_MEDIDAS = ifelse(BANCO_MEDIDAS$BANCO_MEDIDAS == "4",
                                     "ART. 101, IV", BANCO_MEDIDAS$BANCO_MEDIDAS)

table(BANCO_MEDIDAS$BANCO_MEDIDAS)
BANCO_MEDIDAS$BANCO_MEDIDAS = ifelse(BANCO_MEDIDAS$BANCO_MEDIDAS == "5",
                                     "ART. 101, V", BANCO_MEDIDAS$BANCO_MEDIDAS)

table(BANCO_MEDIDAS$BANCO_MEDIDAS)
BANCO_MEDIDAS$BANCO_MEDIDAS = ifelse(BANCO_MEDIDAS$BANCO_MEDIDAS == "6",
                                     "ART. 101, VI", BANCO_MEDIDAS$BANCO_MEDIDAS)

table(BANCO_MEDIDAS$BANCO_MEDIDAS)
BANCO_MEDIDAS$BANCO_MEDIDAS = ifelse(BANCO_MEDIDAS$BANCO_MEDIDAS == "7",
                                     "ART. 101, VII", BANCO_MEDIDAS$BANCO_MEDIDAS)

#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
BANCO_MEDIDAS_bkp = BANCO_MEDIDAS

BANCO_MEDIDAS_bkp =
  BANCO_MEDIDAS_bkp %>%
  janitor::tabyl(BANCO_MEDIDAS) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
BANCO_MEDIDAS_bkp$BANCO_MEDIDAS[BANCO_MEDIDAS_bkp$BANCO_MEDIDAS == "VNÃO SABE"]<- "NÃO SABE"
BANCO_MEDIDAS_bkp$BANCO_MEDIDAS[BANCO_MEDIDAS_bkp$BANCO_MEDIDAS == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
BANCO_MEDIDAS_bkp$PERCENTUAL2 <- str_replace (BANCO_MEDIDAS_bkp$percent, "%", "")
BANCO_MEDIDAS_bkp$PERCENTUAL2 = as.numeric(BANCO_MEDIDAS_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(BANCO_MEDIDAS_bkp)[1]<-'BANCO_MEDIDAS_bkp'
colnames(BANCO_MEDIDAS_bkp)[2]<-'QUANTIDADE'
colnames(BANCO_MEDIDAS_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

BANCO_MEDIDAS_bkp_rmark = BANCO_MEDIDAS_bkp

BANCO_MEDIDAS_bkp_rmark = BANCO_MEDIDAS_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
BANCO_MEDIDAS_bkp_rmark =
  BANCO_MEDIDAS_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
BANCO_MEDIDAS$BANCO_MEDIDAS[BANCO_MEDIDAS$BANCO_MEDIDAS == "VNÃO SABE"]<- "UNÃO SABE"
#BANCO_MEDIDAS$BANCO_MEDIDAS[BANCO_MEDIDAS$BANCO_MEDIDAS == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
BANCO_MEDIDAS_TABELA =
  BANCO_MEDIDAS %>%
  janitor::tabyl(BANCO_MEDIDAS) %>%
  arrange(BANCO_MEDIDAS) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
BANCO_MEDIDAS_TABELA$BANCO_MEDIDAS[BANCO_MEDIDAS_TABELA$BANCO_MEDIDAS == "UNÃO SABE"]<- "NÃO SABE"
BANCO_MEDIDAS_TABELA$BANCO_MEDIDAS[BANCO_MEDIDAS_TABELA$BANCO_MEDIDAS == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(BANCO_MEDIDAS_TABELA)[1]<-'MEDIDA'
colnames(BANCO_MEDIDAS_TABELA)[2]<-'QUANTIDADE'
colnames(BANCO_MEDIDAS_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#BANCO_MEDIDAS FIM
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
