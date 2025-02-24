#########################################################################################################
# df_dia_semana_banco_HOMICIDIO_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_HOMICIDIO =
  banco_geral_sem_concurso |>
  filter(ATO_INFRACIONAL %in% "HOMICÍDIO")

banco_HOMICIDIO_sem_concurso <- banco_HOMICIDIO

df_dia_semana_banco_HOMICIDIO_gt =
  banco_HOMICIDIO_sem_concurso %>%
  select(DIA_SEMANA_ATO)

colnames(df_dia_semana_banco_HOMICIDIO_gt)[1]<-'DIA_SEMANA'

#########################################################################################################

df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "segunda"]<- "SEGUNDA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "terça"]<- "TERÇA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "quarta"]<- "QUARTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "quinta"]<- "QUINTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "sexta"]<- "SEXTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "sábado"]<- "SÁBADO"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "domingo"]<- "DOMINGO"
#########################################################################################################

# salvando para gráfico
df_dia_semana_banco_HOMICIDIO_gt_bkp = df_dia_semana_banco_HOMICIDIO_gt

df_dia_semana_banco_HOMICIDIO_gt_bkp =
  df_dia_semana_banco_HOMICIDIO_gt_bkp %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

colnames(df_dia_semana_banco_HOMICIDIO_gt_bkp)[1]<-'df_dia_semana_banco_HOMICIDIO_gt_bkp'
colnames(df_dia_semana_banco_HOMICIDIO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_HOMICIDIO_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_dia_semana_banco_HOMICIDIO_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_dia_semana_banco_HOMICIDIO_gt_bkp$PERCENTUAL))
df_dia_semana_banco_HOMICIDIO_gt_bkp_rmd = tail(df_dia_semana_banco_HOMICIDIO_gt_bkp,5)
#########################################################################################################
#########################################################################################################
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "DOMINGO"]<- "ADOMINGO"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "SEGUNDA"]<- "BSEGUNDA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "TERÇA"]<- "CTERÇA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "QUARTA"]<- "DQUARTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "QUINTA"]<- "EQUINTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "SEXTA"]<- "FSEXTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "SÁBADO"]<- "GSÁBADO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_dia_semana_banco_HOMICIDIO_gt =
  df_dia_semana_banco_HOMICIDIO_gt %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(DIA_SEMANA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "ADOMINGO"]<- "DOMINGO"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "BSEGUNDA"]<- "SEGUNDA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "CTERÇA"]<- "TERÇA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "DQUARTA"]<- "QUARTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "EQUINTA"]<- "QUINTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "FSEXTA"]<- "SEXTA"
df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA[df_dia_semana_banco_HOMICIDIO_gt$DIA_SEMANA == "GSÁBADO"]<- "SÁBADO"
#########################################################################################################

colnames(df_dia_semana_banco_HOMICIDIO_gt)[1]<-'DIA'
colnames(df_dia_semana_banco_HOMICIDIO_gt)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_HOMICIDIO_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_dia_semana_banco_HOMICIDIO_gt =
#  df_dia_semana_banco_HOMICIDIO_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_dia_semana_banco_HOMICIDIO_gt FIM
#########################################################################################################
#########################################################################################################
# df_regional_banco_HOMICIDIO_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_HOMICIDIO_sem_concurso <- banco_HOMICIDIO

df_regional_banco_HOMICIDIO_gt =
  banco_HOMICIDIO_sem_concurso %>%
  select(REGIONAL_ATO)

colnames(df_regional_banco_HOMICIDIO_gt)[1]<-'REGIONAL_ATO'

#########################################################################################################
#encontrando parte do texto e substituindo
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[agrep("/MG", df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[agrep("RMBH", df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[agrep("RIBEIRAO DAS NEVES", df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[agrep("CATAGUASES", df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[agrep("CIDADE DE BRASILIA/DF", df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO)] <- "VOUTRO ESTADO"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[agrep("N/DISP", df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[agrep("INFORMACAO", df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == ""]<- "SEM INFORMAÇÃO"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "PAMPULHA"]<- "PAMPULHA"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "VENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "REGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#########################################################################################################

# salvando para gráfico
df_regional_banco_HOMICIDIO_gt_bkp = df_regional_banco_HOMICIDIO_gt

df_regional_banco_HOMICIDIO_gt_bkp =
  df_regional_banco_HOMICIDIO_gt_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)


colnames(df_regional_banco_HOMICIDIO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_HOMICIDIO_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_regional_banco_HOMICIDIO_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_regional_banco_HOMICIDIO_gt_bkp$PERCENTUAL))
df_regional_banco_HOMICIDIO_gt_bkp_rmd = tail(df_regional_banco_HOMICIDIO_gt_bkp,5)
#########################################################################################################
# Adaptando para scrip grafico:
#SUBSTITUIR
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt_bkp$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_regional_banco_HOMICIDIO_gt_bkp)[1]<-'df_regional_banco_HOMICIDIO_gt_bkp'
colnames(df_regional_banco_HOMICIDIO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_HOMICIDIO_gt_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_regional_banco_HOMICIDIO_gt =
  df_regional_banco_HOMICIDIO_gt %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO[df_regional_banco_HOMICIDIO_gt$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_regional_banco_HOMICIDIO_gt)[1]<-'REGIONAL'
colnames(df_regional_banco_HOMICIDIO_gt)[2]<-'QUANTIDADE'
colnames(df_regional_banco_HOMICIDIO_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_regional_banco_HOMICIDIO_gt =
#  df_regional_banco_HOMICIDIO_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_regional_banco_HOMICIDIO_gt FIM
#########################################################################################################
#########################################################################################################
# df_dia_semana_banco_ROUBO_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_ROUBO =
  banco_geral_sem_concurso |>
  filter(ATO_INFRACIONAL %in% "ROUBO")


banco_ROUBO_sem_concurso <- banco_ROUBO

df_dia_semana_banco_ROUBO_gt =
  banco_ROUBO_sem_concurso %>%
  select(DIA_SEMANA_ATO)

colnames(df_dia_semana_banco_ROUBO_gt)[1]<-'DIA_SEMANA'

#########################################################################################################

df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "segunda"]<- "SEGUNDA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "terça"]<- "TERÇA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "quarta"]<- "QUARTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "quinta"]<- "QUINTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "sexta"]<- "SEXTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "sábado"]<- "SÁBADO"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "domingo"]<- "DOMINGO"
#########################################################################################################

# salvando para gráfico
df_dia_semana_banco_ROUBO_gt_bkp = df_dia_semana_banco_ROUBO_gt

df_dia_semana_banco_ROUBO_gt_bkp =
  df_dia_semana_banco_ROUBO_gt_bkp %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

colnames(df_dia_semana_banco_ROUBO_gt_bkp)[1]<-'df_dia_semana_banco_ROUBO_gt_bkp'
colnames(df_dia_semana_banco_ROUBO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_ROUBO_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_dia_semana_banco_ROUBO_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_dia_semana_banco_ROUBO_gt_bkp$PERCENTUAL))
df_dia_semana_banco_ROUBO_gt_bkp_rmd = tail(df_dia_semana_banco_ROUBO_gt_bkp,5)
#########################################################################################################
#########################################################################################################
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "DOMINGO"]<- "ADOMINGO"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "SEGUNDA"]<- "BSEGUNDA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "TERÇA"]<- "CTERÇA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "QUARTA"]<- "DQUARTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "QUINTA"]<- "EQUINTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "SEXTA"]<- "FSEXTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "SÁBADO"]<- "GSÁBADO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_dia_semana_banco_ROUBO_gt =
  df_dia_semana_banco_ROUBO_gt %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(DIA_SEMANA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "ADOMINGO"]<- "DOMINGO"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "BSEGUNDA"]<- "SEGUNDA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "CTERÇA"]<- "TERÇA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "DQUARTA"]<- "QUARTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "EQUINTA"]<- "QUINTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "FSEXTA"]<- "SEXTA"
df_dia_semana_banco_ROUBO_gt$DIA_SEMANA[df_dia_semana_banco_ROUBO_gt$DIA_SEMANA == "GSÁBADO"]<- "SÁBADO"
#########################################################################################################

colnames(df_dia_semana_banco_ROUBO_gt)[1]<-'DIA'
colnames(df_dia_semana_banco_ROUBO_gt)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_ROUBO_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_dia_semana_banco_ROUBO_gt =
#  df_dia_semana_banco_ROUBO_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_dia_semana_banco_ROUBO_gt FIM
#########################################################################################################
#########################################################################################################
# df_regional_banco_ROUBO_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_ROUBO_sem_concurso <- banco_ROUBO

df_regional_banco_ROUBO_gt =
  banco_ROUBO_sem_concurso %>%
  select(REGIONAL_ATO)

colnames(df_regional_banco_ROUBO_gt)[1]<-'REGIONAL_ATO'

#########################################################################################################
#encontrando parte do texto e substituindo
df_regional_banco_ROUBO_gt$REGIONAL_ATO[agrep("/MG", df_regional_banco_ROUBO_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[agrep("RMBH", df_regional_banco_ROUBO_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[agrep("RIBEIRAO DAS NEVES", df_regional_banco_ROUBO_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[agrep("CATAGUASES", df_regional_banco_ROUBO_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[agrep("CIDADE DE BRASILIA/DF", df_regional_banco_ROUBO_gt$REGIONAL_ATO)] <- "VOUTRO ESTADO"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[agrep("N/DISP", df_regional_banco_ROUBO_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[agrep("INFORMACAO", df_regional_banco_ROUBO_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == ""]<- "SEM INFORMAÇÃO"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "PAMPULHA"]<- "PAMPULHA"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "VENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "REGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#########################################################################################################

# salvando para gráfico
df_regional_banco_ROUBO_gt_bkp = df_regional_banco_ROUBO_gt

df_regional_banco_ROUBO_gt_bkp =
  df_regional_banco_ROUBO_gt_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)


colnames(df_regional_banco_ROUBO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_ROUBO_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_regional_banco_ROUBO_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_regional_banco_ROUBO_gt_bkp$PERCENTUAL))
df_regional_banco_ROUBO_gt_bkp_rmd = tail(df_regional_banco_ROUBO_gt_bkp,5)
#########################################################################################################
# Adaptando para scrip grafico:
#SUBSTITUIR
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO[df_regional_banco_ROUBO_gt_bkp$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_regional_banco_ROUBO_gt_bkp)[1]<-'df_regional_banco_ROUBO_gt_bkp'
colnames(df_regional_banco_ROUBO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_ROUBO_gt_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_regional_banco_ROUBO_gt =
  df_regional_banco_ROUBO_gt %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_ROUBO_gt$REGIONAL_ATO[df_regional_banco_ROUBO_gt$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_regional_banco_ROUBO_gt)[1]<-'REGIONAL'
colnames(df_regional_banco_ROUBO_gt)[2]<-'QUANTIDADE'
colnames(df_regional_banco_ROUBO_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_regional_banco_ROUBO_gt =
#  df_regional_banco_ROUBO_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_regional_banco_ROUBO_gt FIM
#########################################################################################################

#########################################################################################################
# df_dia_semana_banco_FURTO_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_FURTO =
  banco_geral_sem_concurso |>
  filter(ATO_INFRACIONAL %in% "FURTO")

banco_FURTO_sem_concurso <- banco_FURTO

df_dia_semana_banco_FURTO_gt =
  banco_FURTO_sem_concurso %>%
  select(DIA_SEMANA_ATO)

colnames(df_dia_semana_banco_FURTO_gt)[1]<-'DIA_SEMANA'

#########################################################################################################

df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "segunda"]<- "SEGUNDA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "terça"]<- "TERÇA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "quarta"]<- "QUARTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "quinta"]<- "QUINTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "sexta"]<- "SEXTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "sábado"]<- "SÁBADO"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "domingo"]<- "DOMINGO"
#########################################################################################################

# salvando para gráfico
df_dia_semana_banco_FURTO_gt_bkp = df_dia_semana_banco_FURTO_gt

df_dia_semana_banco_FURTO_gt_bkp =
  df_dia_semana_banco_FURTO_gt_bkp %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

colnames(df_dia_semana_banco_FURTO_gt_bkp)[1]<-'df_dia_semana_banco_FURTO_gt_bkp'
colnames(df_dia_semana_banco_FURTO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_FURTO_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_dia_semana_banco_FURTO_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_dia_semana_banco_FURTO_gt_bkp$PERCENTUAL))
df_dia_semana_banco_FURTO_gt_bkp_rmd = tail(df_dia_semana_banco_FURTO_gt_bkp,5)
#########################################################################################################
#########################################################################################################
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "DOMINGO"]<- "ADOMINGO"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "SEGUNDA"]<- "BSEGUNDA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "TERÇA"]<- "CTERÇA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "QUARTA"]<- "DQUARTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "QUINTA"]<- "EQUINTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "SEXTA"]<- "FSEXTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "SÁBADO"]<- "GSÁBADO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_dia_semana_banco_FURTO_gt =
  df_dia_semana_banco_FURTO_gt %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(DIA_SEMANA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "ADOMINGO"]<- "DOMINGO"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "BSEGUNDA"]<- "SEGUNDA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "CTERÇA"]<- "TERÇA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "DQUARTA"]<- "QUARTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "EQUINTA"]<- "QUINTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "FSEXTA"]<- "SEXTA"
df_dia_semana_banco_FURTO_gt$DIA_SEMANA[df_dia_semana_banco_FURTO_gt$DIA_SEMANA == "GSÁBADO"]<- "SÁBADO"
#########################################################################################################

colnames(df_dia_semana_banco_FURTO_gt)[1]<-'DIA'
colnames(df_dia_semana_banco_FURTO_gt)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_FURTO_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_dia_semana_banco_FURTO_gt =
#  df_dia_semana_banco_FURTO_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_dia_semana_banco_FURTO_gt FIM
#########################################################################################################
#########################################################################################################
# df_regional_banco_FURTO_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_FURTO_sem_concurso <- banco_FURTO

df_regional_banco_FURTO_gt =
  banco_FURTO_sem_concurso %>%
  select(REGIONAL_ATO)

colnames(df_regional_banco_FURTO_gt)[1]<-'REGIONAL_ATO'

#########################################################################################################
#encontrando parte do texto e substituindo
df_regional_banco_FURTO_gt$REGIONAL_ATO[agrep("/MG", df_regional_banco_FURTO_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_FURTO_gt$REGIONAL_ATO[agrep("RMBH", df_regional_banco_FURTO_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_FURTO_gt$REGIONAL_ATO[agrep("RIBEIRAO DAS NEVES", df_regional_banco_FURTO_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_FURTO_gt$REGIONAL_ATO[agrep("CATAGUASES", df_regional_banco_FURTO_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_FURTO_gt$REGIONAL_ATO[agrep("CIDADE DE BRASILIA/DF", df_regional_banco_FURTO_gt$REGIONAL_ATO)] <- "VOUTRO ESTADO"
df_regional_banco_FURTO_gt$REGIONAL_ATO[agrep("N/DISP", df_regional_banco_FURTO_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
df_regional_banco_FURTO_gt$REGIONAL_ATO[agrep("INFORMACAO", df_regional_banco_FURTO_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == ""]<- "SEM INFORMAÇÃO"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "PAMPULHA"]<- "PAMPULHA"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "VENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "REGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#########################################################################################################

# salvando para gráfico
df_regional_banco_FURTO_gt_bkp = df_regional_banco_FURTO_gt

df_regional_banco_FURTO_gt_bkp =
  df_regional_banco_FURTO_gt_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)


colnames(df_regional_banco_FURTO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_FURTO_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_regional_banco_FURTO_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_regional_banco_FURTO_gt_bkp$PERCENTUAL))
df_regional_banco_FURTO_gt_bkp_rmd = tail(df_regional_banco_FURTO_gt_bkp,5)
#########################################################################################################
# Adaptando para scrip grafico:
#SUBSTITUIR
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO[df_regional_banco_FURTO_gt_bkp$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_regional_banco_FURTO_gt_bkp)[1]<-'df_regional_banco_FURTO_gt_bkp'
colnames(df_regional_banco_FURTO_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_FURTO_gt_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_regional_banco_FURTO_gt =
  df_regional_banco_FURTO_gt %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_FURTO_gt$REGIONAL_ATO[df_regional_banco_FURTO_gt$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_regional_banco_FURTO_gt)[1]<-'REGIONAL'
colnames(df_regional_banco_FURTO_gt)[2]<-'QUANTIDADE'
colnames(df_regional_banco_FURTO_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_regional_banco_FURTO_gt =
#  df_regional_banco_FURTO_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_regional_banco_FURTO_gt FIM
#########################################################################################################

#########################################################################################################
# df_dia_semana_banco_USO_DE_DROGAS_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_USO_DE_DROGAS =
  banco_geral_sem_concurso |>
  filter(ATO_INFRACIONAL %in% "POSSE DE DROGAS PARA USO PESSOAL")

banco_USO_DE_DROGAS_sem_concurso <- banco_USO_DE_DROGAS

df_dia_semana_banco_USO_DE_DROGAS_gt =
  banco_USO_DE_DROGAS_sem_concurso %>%
  select(DIA_SEMANA_ATO)

colnames(df_dia_semana_banco_USO_DE_DROGAS_gt)[1]<-'DIA_SEMANA'

#########################################################################################################

df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "segunda"]<- "SEGUNDA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "terça"]<- "TERÇA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "quarta"]<- "QUARTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "quinta"]<- "QUINTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "sexta"]<- "SEXTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "sábado"]<- "SÁBADO"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "domingo"]<- "DOMINGO"
#########################################################################################################

# salvando para gráfico
df_dia_semana_banco_USO_DE_DROGAS_gt_bkp = df_dia_semana_banco_USO_DE_DROGAS_gt

df_dia_semana_banco_USO_DE_DROGAS_gt_bkp =
  df_dia_semana_banco_USO_DE_DROGAS_gt_bkp %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

colnames(df_dia_semana_banco_USO_DE_DROGAS_gt_bkp)[1]<-'df_dia_semana_banco_USO_DE_DROGAS_gt_bkp'
colnames(df_dia_semana_banco_USO_DE_DROGAS_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_USO_DE_DROGAS_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_dia_semana_banco_USO_DE_DROGAS_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_dia_semana_banco_USO_DE_DROGAS_gt_bkp$PERCENTUAL))
df_dia_semana_banco_USO_DE_DROGAS_gt_bkp_rmd = tail(df_dia_semana_banco_USO_DE_DROGAS_gt_bkp,5)
#########################################################################################################
#########################################################################################################
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "DOMINGO"]<- "ADOMINGO"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "SEGUNDA"]<- "BSEGUNDA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "TERÇA"]<- "CTERÇA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "QUARTA"]<- "DQUARTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "QUINTA"]<- "EQUINTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "SEXTA"]<- "FSEXTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "SÁBADO"]<- "GSÁBADO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_dia_semana_banco_USO_DE_DROGAS_gt =
  df_dia_semana_banco_USO_DE_DROGAS_gt %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(DIA_SEMANA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "ADOMINGO"]<- "DOMINGO"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "BSEGUNDA"]<- "SEGUNDA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "CTERÇA"]<- "TERÇA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "DQUARTA"]<- "QUARTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "EQUINTA"]<- "QUINTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "FSEXTA"]<- "SEXTA"
df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_USO_DE_DROGAS_gt$DIA_SEMANA == "GSÁBADO"]<- "SÁBADO"
#########################################################################################################

colnames(df_dia_semana_banco_USO_DE_DROGAS_gt)[1]<-'DIA'
colnames(df_dia_semana_banco_USO_DE_DROGAS_gt)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_USO_DE_DROGAS_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_dia_semana_banco_USO_DE_DROGAS_gt =
#  df_dia_semana_banco_USO_DE_DROGAS_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_dia_semana_banco_USO_DE_DROGAS_gt FIM
#########################################################################################################
#########################################################################################################
# df_regional_banco_USO_DE_DROGAS_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_USO_DE_DROGAS_sem_concurso <- banco_USO_DE_DROGAS

df_regional_banco_USO_DE_DROGAS_gt =
  banco_USO_DE_DROGAS_sem_concurso %>%
  select(REGIONAL_ATO)

colnames(df_regional_banco_USO_DE_DROGAS_gt)[1]<-'REGIONAL_ATO'

#########################################################################################################
#encontrando parte do texto e substituindo
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[agrep("/MG", df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[agrep("RMBH", df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[agrep("RIBEIRAO DAS NEVES", df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[agrep("CATAGUASES", df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[agrep("CIDADE DE BRASILIA/DF", df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO)] <- "VOUTRO ESTADO"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[agrep("N/DISP", df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[agrep("INFORMACAO", df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == ""]<- "SEM INFORMAÇÃO"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "PAMPULHA"]<- "PAMPULHA"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "VENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "REGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#########################################################################################################

# salvando para gráfico
df_regional_banco_USO_DE_DROGAS_gt_bkp = df_regional_banco_USO_DE_DROGAS_gt

df_regional_banco_USO_DE_DROGAS_gt_bkp =
  df_regional_banco_USO_DE_DROGAS_gt_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)


colnames(df_regional_banco_USO_DE_DROGAS_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_USO_DE_DROGAS_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_regional_banco_USO_DE_DROGAS_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_regional_banco_USO_DE_DROGAS_gt_bkp$PERCENTUAL))
df_regional_banco_USO_DE_DROGAS_gt_bkp_rmd = tail(df_regional_banco_USO_DE_DROGAS_gt_bkp,5)
#########################################################################################################
# Adaptando para scrip grafico:
#SUBSTITUIR
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_regional_banco_USO_DE_DROGAS_gt_bkp)[1]<-'df_regional_banco_USO_DE_DROGAS_gt_bkp'
colnames(df_regional_banco_USO_DE_DROGAS_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_USO_DE_DROGAS_gt_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_regional_banco_USO_DE_DROGAS_gt =
  df_regional_banco_USO_DE_DROGAS_gt %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_USO_DE_DROGAS_gt$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_regional_banco_USO_DE_DROGAS_gt)[1]<-'REGIONAL'
colnames(df_regional_banco_USO_DE_DROGAS_gt)[2]<-'QUANTIDADE'
colnames(df_regional_banco_USO_DE_DROGAS_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_regional_banco_USO_DE_DROGAS_gt =
#  df_regional_banco_USO_DE_DROGAS_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_regional_banco_USO_DE_DROGAS_gt FIM
#########################################################################################################

#########################################################################################################
# df_dia_semana_banco_TRAFICO_DE_DROGAS_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_TRAFICO_DE_DROGAS =
  banco_geral_sem_concurso |>
  filter(ATO_INFRACIONAL %in% "TRÁFICO DE DROGAS")

banco_TRAFICO_DE_DROGAS_sem_concurso <- banco_TRAFICO_DE_DROGAS

df_dia_semana_banco_TRAFICO_DE_DROGAS_gt =
  banco_TRAFICO_DE_DROGAS_sem_concurso %>%
  select(DIA_SEMANA_ATO)

colnames(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt)[1]<-'DIA_SEMANA'

#########################################################################################################

df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "segunda"]<- "SEGUNDA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "terça"]<- "TERÇA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "quarta"]<- "QUARTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "quinta"]<- "QUINTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "sexta"]<- "SEXTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "sábado"]<- "SÁBADO"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "domingo"]<- "DOMINGO"
#########################################################################################################

# salvando para gráfico
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp = df_dia_semana_banco_TRAFICO_DE_DROGAS_gt

df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp =
  df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

colnames(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp)[1]<-'df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp'
colnames(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp$PERCENTUAL))
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp_rmd = tail(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt_bkp,5)
#########################################################################################################
#########################################################################################################
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "DOMINGO"]<- "ADOMINGO"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "SEGUNDA"]<- "BSEGUNDA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "TERÇA"]<- "CTERÇA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "QUARTA"]<- "DQUARTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "QUINTA"]<- "EQUINTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "SEXTA"]<- "FSEXTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "SÁBADO"]<- "GSÁBADO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_dia_semana_banco_TRAFICO_DE_DROGAS_gt =
  df_dia_semana_banco_TRAFICO_DE_DROGAS_gt %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(DIA_SEMANA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "ADOMINGO"]<- "DOMINGO"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "BSEGUNDA"]<- "SEGUNDA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "CTERÇA"]<- "TERÇA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "DQUARTA"]<- "QUARTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "EQUINTA"]<- "QUINTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "FSEXTA"]<- "SEXTA"
df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA[df_dia_semana_banco_TRAFICO_DE_DROGAS_gt$DIA_SEMANA == "GSÁBADO"]<- "SÁBADO"
#########################################################################################################

colnames(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt)[1]<-'DIA'
colnames(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt)[2]<-'QUANTIDADE'
colnames(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_dia_semana_banco_TRAFICO_DE_DROGAS_gt =
#  df_dia_semana_banco_TRAFICO_DE_DROGAS_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_dia_semana_banco_TRAFICO_DE_DROGAS_gt FIM
#########################################################################################################
#########################################################################################################
# df_regional_banco_TRAFICO_DE_DROGAS_gt
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_TRAFICO_DE_DROGAS_sem_concurso <- banco_TRAFICO_DE_DROGAS

df_regional_banco_TRAFICO_DE_DROGAS_gt =
  banco_TRAFICO_DE_DROGAS_sem_concurso %>%
  select(REGIONAL_ATO)

colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt)[1]<-'REGIONAL_ATO'

#########################################################################################################
#encontrando parte do texto e substituindo
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[agrep("/MG", df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[agrep("RMBH", df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[agrep("RIBEIRAO DAS NEVES", df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[agrep("CATAGUASES", df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO)] <- "UOUTRA CIDADE MG"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[agrep("CIDADE DE BRASILIA/DF", df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO)] <- "VOUTRO ESTADO"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[agrep("N/DISP", df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[agrep("INFORMACAO", df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == ""]<- "SEM INFORMAÇÃO"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "PAMPULHA"]<- "PAMPULHA"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "VENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "REGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#########################################################################################################

# salvando para gráfico
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp = df_regional_banco_TRAFICO_DE_DROGAS_gt

df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp =
  df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)


colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$PERCENTUAL))
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp_rmd = tail(df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp,5)
#########################################################################################################
# Adaptando para scrip grafico:
#SUBSTITUIR
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp)[1]<-'df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp'
colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_regional_banco_TRAFICO_DE_DROGAS_gt =
  df_regional_banco_TRAFICO_DE_DROGAS_gt %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO[df_regional_banco_TRAFICO_DE_DROGAS_gt$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt)[1]<-'REGIONAL'
colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt)[2]<-'QUANTIDADE'
colnames(df_regional_banco_TRAFICO_DE_DROGAS_gt)[3]<-'PERCENTUAL'

#############################################################################################################

#df_regional_banco_TRAFICO_DE_DROGAS_gt =
#  df_regional_banco_TRAFICO_DE_DROGAS_gt %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_regional_banco_TRAFICO_DE_DROGAS_gt FIM
#########################################################################################################





#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#FIM
#########################################################################################################
