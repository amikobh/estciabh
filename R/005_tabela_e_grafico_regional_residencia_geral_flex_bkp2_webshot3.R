#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_regional_residencia =
  banco_SNR %>%
  select(REGIONAL_RESIDENCIAL)

colnames(df_snr_regional_residencia)[1]<-'regional_residencial'

#########################################################################################################
#encontrando parte do texto e substituindo
df_snr_regional_residencia$regional_residencial[agrep("/MG", df_snr_regional_residencia$regional_residencial)] <- "UOUTRA CIDADE MG"
df_snr_regional_residencia$regional_residencial[agrep("RMBH", df_snr_regional_residencia$regional_residencial)] <- "REGIÃO METROPOLITANA"
df_snr_regional_residencia$regional_residencial[agrep("RIBEIRAO DAS NEVES", df_snr_regional_residencia$regional_residencial)] <- "REGIÃO METROPOLITANA"
df_snr_regional_residencia$regional_residencial[agrep("CATAGUASES", df_snr_regional_residencia$regional_residencial)] <- "UOUTRA CIDADE MG"
df_snr_regional_residencia$regional_residencial[agrep("CIDADE DE BRASILIA/DF", df_snr_regional_residencia$regional_residencial)] <- "VOUTRO ESTADO"
df_snr_regional_residencia$regional_residencial[agrep("N/DISP", df_snr_regional_residencia$regional_residencial)] <- "ZSEM INFORMAÇÃO"
df_snr_regional_residencia$regional_residencial[agrep("INFORMACAO", df_snr_regional_residencia$regional_residencial)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == ""]<- "ZSEM INFORMAÇÃO"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "PAMPULHA"]<- "OESTEPAMPULHA"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "VENDA NOVA"]<- "PVENDA NOVA"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "REGIÃO METROPOLITANA"]<- "QREGIÃO METROPOLITANA"

#########################################################################################################

# salvando para gráfico
df_snr_regional_residencia_bkp = df_snr_regional_residencia

df_snr_regional_residencia_bkp =
  df_snr_regional_residencia_bkp %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "OESTEPAMPULHA"]<- "PAMPULHA"
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "PVENDA NOVA"]<- "VENDA NOVA"
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia_bkp$regional_residencial[df_snr_regional_residencia_bkp$regional_residencial == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_snr_regional_residencia_bkp)[1]<-'df_snr_regional_residencia_bkp'
colnames(df_snr_regional_residencia_bkp)[2]<-'QUANTIDADE'
colnames(df_snr_regional_residencia_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
df_snr_regional_residencia_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_snr_regional_residencia_bkp$PERCENTUAL))
df_snr_regional_residencia_bkp_rmd = tail(df_snr_regional_residencia_bkp,5)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_snr_regional_residencia =
  df_snr_regional_residencia %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(regional_residencial) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "OESTEPAMPULHA"]<- "PAMPULHA"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "PVENDA NOVA"]<- "VENDA NOVA"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia$regional_residencial[df_snr_regional_residencia$regional_residencial == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_snr_regional_residencia)[1]<-'REGIONAL'
colnames(df_snr_regional_residencia)[2]<-'QUANTIDADE'
colnames(df_snr_regional_residencia)[3]<-'PERCENTUAL'

#############################################################################################################

#df_snr_regional_residencia =
#  df_snr_regional_residencia %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_snr_regional_residencia FIM
#########################################################################################################
