#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR JR:
dir.create(file.path("~/diretorio_r/estciabh", "mba"))
setwd(file.path("~/diretorio_r/estciabh"))
#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

#########################################################################################################
#banco_mba_com_ato <- read.csv("banco_mba_com_ato.csv",header=TRUE, sep="|", encoding = "UTF-8", skip = 2) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

banco_mba_com_ato <- as_tibble(banco_mba_com_ato)



banco_mba_com_ato_bkp = banco_mba_com_ato
#########################################################################################################
#filtrar data
#banco_mba_com_ato = banco_mba_com_ato %>%
  #filter(DATA_DO_ENCAMINHAMENTO_DO_JUIZ >= (str_c(format(Sys.Date()-365*1, "%Y"), "-01-01")) &
          # DATA_DO_ENCAMINHAMENTO_DO_JUIZ <= (str_c(format(Sys.Date()-365*1, "%Y"), "-12-31")))
  #filter(DATA_DO_ENCAMINHAMENTO_DO_JUIZ >= (str_c(format(Sys.Date()-365*2, "%Y"), "-01-01")) & DATA_DO_ENCAMINHAMENTO_DO_JUIZ <= (str_c(format(Sys.Date()-365*2, "%Y"), "-12-31")))

#banco_mba_com_ato = banco_mba_com_ato %>%
  #filter(DATA_DO_RECEBIMENTO_NO_SETOR >= (str_c(format(Sys.Date()-365*1, "%Y"), "-01-01")) &
          # DATA_DO_RECEBIMENTO_NO_SETOR <= (str_c(format(Sys.Date()-365*1, "%Y"), "-12-31")))
#########################################################################################################

#########################################################################################################
#tabela total_MBA
total_MBA = data.frame(nrow(banco_mba_com_ato))

colnames(total_MBA) <- c("QUANTIDADE DE MBAs CUMPRIDOS")

#para tabela gt abaixo:
total_MBA_gt = total_MBA

#########################################################################################################
#########################################################################################################


#########################################################################################################
#TRATAMENTO banco_MBA_snr_SEXO_IDADE
#########################################################################################################
banco_MBA_snr = distinct(banco_mba_com_ato, NOME2, NASCIMENTO, .keep_all= TRUE)
#########################################################################################################

banco_MBA_snr_SEXO_IDADE =

  banco_MBA_snr %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_MBA_snr_SEXO_IDADE$SEXO[banco_MBA_snr_SEXO_IDADE$SEXO == ""]<- "M"
#########################################################################################################
banco_MBA_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_MBA_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_MBA_snr_SEXO_IDADE$SEXO[banco_MBA_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_MBA_snr_SEXO_IDADE$SEXO[banco_MBA_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_MBA_snr_SEXO_IDADE_pizza = banco_MBA_snr_SEXO_IDADE
#########################################################################################################
banco_MBA_snr_SEXO_IDADE_pizza =
  banco_MBA_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_MBA_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_MBA_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_MBA_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_MBA_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_MBA_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_MBA_snr_SEXO_IDADE_pizza$IDADE[banco_MBA_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_MBA_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_MBA_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_MBA_snr_SEXO_IDADE_graf_pizza <- ddply(banco_MBA_snr_SEXO_IDADE_pizza,
                                             c("SEXO"),
                                             summarise,
                                             QUANTIDADE = sum(QUANTIDADE))

banco_MBA_snr_SEXO_IDADE_graf_pizza =
  banco_MBA_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_MBA_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_MBA_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_MBA_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_MBA_snr_SEXO_IDADE_pizza,
                                            c("IDADE"),
                                            summarise,
                                            QUANTIDADE = sum(QUANTIDADE))

banco_MBA_snr_SEXO_IDADE_pizza_bkp = banco_MBA_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_MBA_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_MBA_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_MBA_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_MBA_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_MBA_snr_SEXO_IDADE =

  banco_MBA_snr %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

banco_MBA_snr_SEXO_IDADE$SEXO[banco_MBA_snr_SEXO_IDADE$SEXO == ""]<- "M"

#########################################################################################################
banco_MBA_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_MBA_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_MBA_snr_SEXO_IDADE$SEXO[banco_MBA_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_MBA_snr_SEXO_IDADE$SEXO[banco_MBA_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_MBA_snr_SEXO_IDADE_pizza = banco_MBA_snr_SEXO_IDADE
#########################################################################################################
banco_MBA_snr_SEXO_IDADE_pizza =
  banco_MBA_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_MBA_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_MBA_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_MBA_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_MBA_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_MBA_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_MBA_snr_SEXO_IDADE_pizza$IDADE[banco_MBA_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_MBA_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_MBA_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_MBA_snr_SEXO_IDADE_graf_pizza <- ddply(banco_MBA_snr_SEXO_IDADE_pizza,
                                             c("SEXO"),
                                             summarise,
                                             QUANTIDADE = sum(QUANTIDADE))

banco_MBA_snr_SEXO_IDADE_graf_pizza =
  banco_MBA_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_MBA_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_MBA_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_MBA_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_MBA_snr_SEXO_IDADE_pizza,
                                            c("IDADE"),
                                            summarise,
                                            QUANTIDADE = sum(QUANTIDADE))

banco_MBA_snr_SEXO_IDADE_pizza_bkp = banco_MBA_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_MBA_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_MBA_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_MBA_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#df_snr_regional_residencia_MBA
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_regional_residencia_MBA =
  banco_MBA_snr %>%
  select(REGIONAL_RESIDENCIAL)

colnames(df_snr_regional_residencia_MBA)[1]<-'regional_residencial'

#########################################################################################################
#encontrando parte do texto e substituindo
df_snr_regional_residencia_MBA$regional_residencial[agrep("/MG", df_snr_regional_residencia_MBA$regional_residencial)] <- "UOUTRA CIDADE MG"
df_snr_regional_residencia_MBA$regional_residencial[agrep("RMBH", df_snr_regional_residencia_MBA$regional_residencial)] <- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA$regional_residencial[agrep("RIBEIRAO DAS NEVES", df_snr_regional_residencia_MBA$regional_residencial)] <- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA$regional_residencial[agrep("CATAGUASES", df_snr_regional_residencia_MBA$regional_residencial)] <- "UOUTRA CIDADE MG"
df_snr_regional_residencia_MBA$regional_residencial[agrep("CIDADE DE BRASILIA/DF", df_snr_regional_residencia_MBA$regional_residencial)] <- "VOUTRO ESTADO"
df_snr_regional_residencia_MBA$regional_residencial[agrep("N/DISP", df_snr_regional_residencia_MBA$regional_residencial)] <- "ZSEM INFORMAÇÃO"
df_snr_regional_residencia_MBA$regional_residencial[agrep("INFORMACAO", df_snr_regional_residencia_MBA$regional_residencial)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == ""]<- "ZSEM INFORMAÇÃO"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "PAMPULHA"]<- "OESTEPAMPULHA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "VENDA NOVA"]<- "PVENDA NOVA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "REGIÃO METROPOLITANA"]<- "QREGIÃO METROPOLITANA"

#########################################################################################################

# salvando para gráfico
df_snr_regional_residencia_MBA_bkp = df_snr_regional_residencia_MBA

df_snr_regional_residencia_MBA_bkp =
  df_snr_regional_residencia_MBA_bkp %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "OESTEPAMPULHA"]<- "PAMPULHA"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "PVENDA NOVA"]<- "VENDA NOVA"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia_MBA_bkp$regional_residencial[df_snr_regional_residencia_MBA_bkp$regional_residencial == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_snr_regional_residencia_MBA_bkp)[1]<-'df_snr_regional_residencia_MBA_bkp'
colnames(df_snr_regional_residencia_MBA_bkp)[2]<-'QUANTIDADE'
colnames(df_snr_regional_residencia_MBA_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
df_snr_regional_residencia_MBA_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_snr_regional_residencia_MBA_bkp$PERCENTUAL))

# 1. Carregar bibliotecas necessárias (se já não estiverem carregadas)
library(dplyr)

# 2. Definir as regionais oficiais de Belo Horizonte
regionais_bh <- c("PAMPULHA", "BARREIRO", "CENTRO-SUL", "LESTE", "NORDESTE",
                  "NOROESTE", "NORTE", "OESTE", "VENDA NOVA", "HIPERCENTRO")

# 3. Criar dataframe apenas com regionais de BH, ordenado por quantidade decrescente
df_snr_regional_residencia_MBA_bkp_bh <- df_snr_regional_residencia_MBA_bkp %>%
  filter(df_snr_regional_residencia_MBA_bkp %in% regionais_bh) %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(df_snr_regional_residencia_MBA_bkp = as.character(df_snr_regional_residencia_MBA_bkp))

# 5. Obter o valor da Região Metropolitana
qtd_regiao_metropolitana <- df_snr_regional_residencia_MBA_bkp %>%
  filter(df_snr_regional_residencia_MBA_bkp == "REGIÃO METROPOLITANA") %>%
  pull(QUANTIDADE)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_snr_regional_residencia_MBA =
  df_snr_regional_residencia_MBA %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(regional_residencial) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "OESTEPAMPULHA"]<- "PAMPULHA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "PVENDA NOVA"]<- "VENDA NOVA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_snr_regional_residencia_MBA$regional_residencial[df_snr_regional_residencia_MBA$regional_residencial == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(df_snr_regional_residencia_MBA)[1]<-'REGIONAL'
colnames(df_snr_regional_residencia_MBA)[2]<-'QUANTIDADE'
colnames(df_snr_regional_residencia_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#df_snr_regional_residencia_MBA =
#  df_snr_regional_residencia_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_snr_regional_residencia_MBA FIM
#########################################################################################################
#########################################################################################################
#MOTIVO_MBA
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

MOTIVO_MBA =
  banco_mba_com_ato %>%
  select(MOTIVO_MBA)

#colnames(MOTIVO_MBA)[1]<-'regional_residencial'
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "DESCUMPRIMENTO DE MEIO ABERTO"]<- "DESCUMPRIMENTO DE MEDIDA EM MEIO ABERTO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "EVASAO DA SEMILIBERDADE"]<- "EVASÃO DA UNIDADE DE SEMILIBERDADE"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "EVASAO DE CENTRO DE INTERNACAO"]<- "EVASÃO DA UNIDADE DE INTERNAÇÃO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "INICIAR CUMPRIMENTO DE MEDIDA DE INTERNACAO"]<- "INICIAR CUMPRIMENTO DE MEDIDA DE INTERNAÇÃO"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "NAO COMPARECIMENTO A AUDIENCIA"]<- "NÃO COMPARECIMENTO A AUDIÊNCIA"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "RETORNAR UNIDADE "]<- "RETORNO A UNIDADE PARA CUMPRIMENTO DA MEDIDA"
MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "SEM INFORAMCAO"]<- "SEM INFORMAÇÃO"
#MOTIVO_MBA$MOTIVO_MBA[MOTIVO_MBA$MOTIVO_MBA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#encontrando parte do texto e substituindo
#########################################################################################################

# salvando para gráfico
MOTIVO_MBA_bkp = MOTIVO_MBA

MOTIVO_MBA_bkp =
  MOTIVO_MBA_bkp %>%
  janitor::tabyl(MOTIVO_MBA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
MOTIVO_MBA_bkp$MOTIVO_MBA[MOTIVO_MBA_bkp$MOTIVO_MBA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
MOTIVO_MBA_bkp$MOTIVO_MBA[MOTIVO_MBA_bkp$MOTIVO_MBA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#MOTIVO_MBA_bkp$regional_residencial[MOTIVO_MBA_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

colnames(MOTIVO_MBA_bkp)[1]<-'MOTIVO_MBA_bkp'
colnames(MOTIVO_MBA_bkp)[2]<-'QUANTIDADE'
colnames(MOTIVO_MBA_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

MOTIVO_MBA =
  MOTIVO_MBA %>%
  janitor::tabyl(MOTIVO_MBA) %>%
  arrange(MOTIVO_MBA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR

colnames(MOTIVO_MBA)[1]<-'MOTIVO'
colnames(MOTIVO_MBA)[2]<-'QUANTIDADE'
colnames(MOTIVO_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#MOTIVO_MBA =
#  MOTIVO_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# MOTIVO_MBA FIM
#########################################################################################################
#########################################################################################################
# banco_ato_MBA
#########################################################################################################
#########################################################################################################

library(dplyr)

banco_ato_MBA =
  banco_mba_com_ato |>
  select(ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = gsub(" ","", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#AMEAÇA

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "147.ARTCPB",
                                           "AMEAÇA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


########################################################################################################
#########################################################################################################
#CRIME DE TRÂNSITO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "309.ARTCTB",
                                           "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "310.ARTCTB",
                                           "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "311.ARTCTB",
                                           "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#para discrinar: é so anular com #

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)",
                                           "CRIME DE TRÂNSITO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)",
                                           "CRIME DE TRÂNSITO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)",
                                           "CRIME DE TRÂNSITO", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#########################################################################################################
#DANO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "163.ARTCPB",
                                           "DANO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "213.ARTCPB",
                                           "ESTUPRO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "215.ARTCPB",
                                           "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "215-A.ARTCPB",
                                           "IMPORTUNAÇÃO SEXUAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ESTUPRO DE VULNERÁVEL

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "217-A.ARTCPB",
                                           "ESTUPRO DE VULNERÁVEL", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#FURTO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "155.ARTCPB",
                                           "FURTO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#FURTO (TENTATIVA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "155C/C14.ARTCPB",
                                           "FURTO (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "121.ARTCPB",
                                           "HOMICÍDIO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO (TENTATIVA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "121C/C14,II.ARTCPB",
                                           "HOMICÍDIO (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "129.ARTCPB",
                                           "LESÃO CORPORAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "129§3º.ARTCPB",
                                           "LESÃO CORPORAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "129§9º.ARTCPB",
                                           "LESÃO CORPORAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL (TENTATIVA). Ordem para trocar LESÃO CORPORAL (TENTATIVA) por VIAS DE FATO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "129C/C14,II.ARTCPB",
                                           "VIAS DE FATO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#PICHAÇÃO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "65.ART9.605",
                                           "PICHAÇÃO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#PORTE/POSSE DE ARMA

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "12.ART10.826",
                                           "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "14.ART10.826",
                                           "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "16.ART10.826",
                                           "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "15.ART10.826",
                                           "ARMA DE FOGO - PORTE ILEGAL (DISPARO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "17.ART10.826",
                                           "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)", banco_ato_MBA$ATO_INFRACIONAL_MBA)



banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - PORTE ILEGAL (DISPARO)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)",
                                           "PORTE/POSSE DE ARMA", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#RECEPTAÇÃO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "180.ARTCPB",
                                           "RECEPTAÇÃO", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#########################################################################################################
#ROUBO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157.ARTCPB",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAICPB",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAIIARTCPB",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2º,I,IIeVARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2º,I.ARTCPB",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2º,IeII.ARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºA,IARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIeVARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºA,I",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAIARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAIeIIARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºII,VeVIIARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIeVIARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)



banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIeVIIARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºIIEVIIARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºVARTCPB",
                                           "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºVIIARTCPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2ºAIICPB",
                                           "ROUBO (EMPREGO DE ARMA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#para discrinar: é so anular com #

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EM CONCURSO DE PESSOAS)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EM CONCURSO DE PESSOAS)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EMPREGO DE ARMA)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EM CONCURSO DE PESSOAS)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (EMPREGO DE ARMA)",
                                           "ROUBO", banco_ato_MBA$ATO_INFRACIONAL_MBA)



#########################################################################################################
#ROUBO (TENTATIVA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§2º,IeIIC/C14,II.ARTCPB",
                                           "ROUBO (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157C/C14,II.ARTCPB",
                                           "ROUBO (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#ROUBO (§3º)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§3ºARTCPB",
                                           "ROUBO (§3º)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "ROUBO (§3º)",
                                           "LATROCÍNIO", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#########################################################################################################
#ROUBO (§3º) (TENTATIVA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "157§3ºARTCPBC/C14,II,CPB",
                                           "ROUBO (§3º) (TENTATIVA)", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#SEQUESTRO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "148.ARTCPB",
                                           "SEQUESTRO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#########################################################################################################
#TRÁFICO DE DROGAS


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "33.ART11.343",
                                           "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "37.ART11.343",
                                           "TRÁFICO DE DROGAS (INFORMANTE)", banco_ato_MBA$ATO_INFRACIONAL_MBA)



#para discrinar: é so anular com #

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "TRÁFICO DE DROGAS",
                                           "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "TRÁFICO DE DROGAS (INFORMANTE)",
                                           "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)





#########################################################################################################
#########################################################################################################
#########################################################################################################
#ASSOCIAÇÃO TRÁFICO DE DROGAS


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "35.ART11.343",
                                           "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "34.ART11.343",
                                           "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#para discrinar: é so anular com #

#banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "TRÁFICO DE DROGAS (ASSOCIAÇÃO)",
#                                           "TRÁFICO DE DROGAS", banco_ato_MBA$ATO_INFRACIONAL_MBA)

#########################################################################################################
#########################################################################################################
#USO DE DROGAS

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "28.ART11.343",
                                           "POSSE DE DROGAS PARA USO PESSOAL", banco_ato_MBA$ATO_INFRACIONAL_MBA)



#########################################################################################################
#########################################################################################################
#VIAS DE FATO

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "21.ARTLCP",
                                           "VIAS DE FATO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


#########################################################################################################
#OUTROS

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "140.ARTCPB",
                                           "INJÚRIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "140§3º.ARTCPB",
                                           "INJÚRIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)



banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "330.ARTCPB",
                                           "DESOBEDIÊNCIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "331.ARTCPB",
                                           "DESACATO", banco_ato_MBA$ATO_INFRACIONAL_MBA)


banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "139.ARTCPB",
                                           "DESOBEDIÊNCIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "2.ART13.185",
                                           "INTIMIDAÇÃO SISTEMÁTICA (BULLYING)", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "329.ARTCPB",
                                           "RESISTÊNCIA", banco_ato_MBA$ATO_INFRACIONAL_MBA)



#table(banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "NSA",
                                           "SEM INFORMAÇÃO", banco_ato_MBA$ATO_INFRACIONAL_MBA)

banco_ato_MBA$ATO_INFRACIONAL_MBA = ifelse(banco_ato_MBA$ATO_INFRACIONAL_MBA == "TERMOSEMINF.",
                                           "SEM INFORMAÇÃO", banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#table(banco_ato_MBA$ATO_INFRACIONAL_MBA)
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_ato_MBA_bkp = banco_ato_MBA

banco_ato_MBA_bkp =
  banco_ato_MBA_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL_MBA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
banco_ato_MBA_bkp$ATO_INFRACIONAL_MBA[banco_ato_MBA_bkp$ATO_INFRACIONAL_MBA == "VS/INF"]<- "SEM INFORMAÇÃO"
banco_ato_MBA_bkp$ATO_INFRACIONAL_MBA[banco_ato_MBA_bkp$ATO_INFRACIONAL_MBA == "VOUTROS"]<- "OUTROS"


colnames(banco_ato_MBA_bkp)[1]<-'banco_ato_MBA_bkp'
colnames(banco_ato_MBA_bkp)[2]<-'QUANTIDADE'
colnames(banco_ato_MBA_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#para script rmd:
banco_ato_MBA_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ato_MBA_bkp$PERCENTUAL))
banco_ato_MBA_bkp_rmd = tail(banco_ato_MBA_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_ato_MBA =
  banco_ato_MBA %>%
  janitor::tabyl(ATO_INFRACIONAL_MBA) %>%
  arrange(ATO_INFRACIONAL_MBA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
banco_ato_MBA$ATO_INFRACIONAL_MBA[banco_ato_MBA$ATO_INFRACIONAL_MBA == "VS/INF"]<- "SEM INFORMAÇÃO"
banco_ato_MBA$ATO_INFRACIONAL_MBA[banco_ato_MBA$ATO_INFRACIONAL_MBA == "VOUTROS"]<- "OUTROS"



colnames(banco_ato_MBA)[1]<-'ATO INFRACIONAL'
colnames(banco_ato_MBA)[2]<-'QUANTIDADE'
colnames(banco_ato_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#banco_ato_MBA =
#  banco_ato_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
write.csv(banco_ato_MBA, file ="banco_ato_MBA_atual.csv",row.names=FALSE)
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ato_MBA FIM
#########################################################################################################
#########################################################################################################
# df_regional_ATO_banco_MBA
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_regional_ATO_banco_MBA =
  banco_mba_com_ato %>%
  select(REGIONAL_ATO)

colnames(df_regional_ATO_banco_MBA)[1]<-'REGIONAL_ATO'

#########################################################################################################
#encontrando parte do texto e substituindo
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("/MG", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "OUTRA CIDADE MG"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("RMBH", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("RIBEIRAO DAS NEVES", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("CATAGUASES", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "OUTRA CIDADE MG"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("CIDADE DE BRASILIA/DF", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "VOUTRO ESTADO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("N/DISP", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[agrep("INFORMACAO", df_regional_ATO_banco_MBA$REGIONAL_ATO)] <- "ZSEM INFORMAÇÃO"
#substituindo
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == ""]<- "SEM INFORMAÇÃO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "PAMPULHA"]<- "PAMPULHA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "VENDA NOVA"]<- "VENDA NOVA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "REGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#########################################################################################################

# salvando para gráfico
df_regional_ATO_banco_MBA_bkp = df_regional_ATO_banco_MBA

df_regional_ATO_banco_MBA_bkp =
  df_regional_ATO_banco_MBA_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)


colnames(df_regional_ATO_banco_MBA_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_ATO_banco_MBA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_regional_ATO_banco_MBA_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_regional_ATO_banco_MBA_bkp$PERCENTUAL))

# 1. Carregar bibliotecas necessárias (se já não estiverem carregadas)
library(dplyr)

# 2. Definir as regionais oficiais de Belo Horizonte
regionais_bh <- c("PAMPULHA", "BARREIRO", "CENTRO-SUL", "LESTE", "NORDESTE",
                  "NOROESTE", "NORTE", "OESTE", "VENDA NOVA", "HIPERCENTRO")

# 3. Criar dataframe apenas com regionais de BH, ordenado por quantidade decrescente
df_regional_ATO_banco_MBA_bkp_bh <- df_regional_ATO_banco_MBA_bkp %>%
  filter(REGIONAL_ATO %in% regionais_bh) %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(REGIONAL_ATO = as.character(REGIONAL_ATO))

# 5. Obter o valor da Região Metropolitana
qtd_regiao_metropolitana_ATO_banco_MBA <- df_regional_ATO_banco_MBA_bkp %>%
  filter(REGIONAL_ATO == "REGIÃO METROPOLITANA") %>%
  pull(QUANTIDADE)

#########################################################################################################
# Adaptando para scrip grafico:
#SUBSTITUIR
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "OESTEPAMPULHA"]<- "PAMPULHA"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "PVENDA NOVA"]<- "VENDA NOVA"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO[df_regional_ATO_banco_MBA_bkp$REGIONAL_ATO == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(df_regional_ATO_banco_MBA_bkp)[1]<-'df_regional_ATO_banco_MBA_bkp'
colnames(df_regional_ATO_banco_MBA_bkp)[2]<-'QUANTIDADE'
colnames(df_regional_ATO_banco_MBA_bkp)[3]<-'PERCENTUAL'
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "PAMPULHA"]<- "UPAMPULHA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "VENDA NOVA"]<- "VVENDA NOVA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "REGIÃO METROPOLITANA"]<- "WREGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "OUTRA CIDADE MG"]<- "XOUTRA CIDADE MG"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "OUTRO ESTADO"]<- "YOUTRO ESTADO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"

#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_regional_ATO_banco_MBA =
  df_regional_ATO_banco_MBA %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#########################################################################################################

# Adaptando:
#SUBSTITUIR
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "UPAMPULHA"]<- "PAMPULHA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "VVENDA NOVA"]<- "VENDA NOVA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "WREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "XOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "YOUTRO ESTADO"]<- "OUTRO ESTADO"
df_regional_ATO_banco_MBA$REGIONAL_ATO[df_regional_ATO_banco_MBA$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

#########################################################################################################
colnames(df_regional_ATO_banco_MBA)[1]<-'REGIONAL'
colnames(df_regional_ATO_banco_MBA)[2]<-'QUANTIDADE'
colnames(df_regional_ATO_banco_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#df_regional_ATO_banco_MBA =
#  df_regional_ATO_banco_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_regional_ATO_banco_MBA FIM
#########################################################################################################
#########################################################################################################
# df_DIA_SEMANA_banco_MBA
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_DIA_SEMANA_banco_MBA =
  banco_mba_com_ato %>%
  select(DIA_SEMANA_ATO)

colnames(df_DIA_SEMANA_banco_MBA)[1]<-'DIA_SEMANA'

#########################################################################################################

df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "segunda"]<- "SEGUNDA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "terça"]<- "TERÇA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "quarta"]<- "QUARTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "quinta"]<- "QUINTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "sexta"]<- "SEXTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "sábado"]<- "SÁBADO"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "domingo"]<- "DOMINGO"
#########################################################################################################

# salvando para gráfico
df_DIA_SEMANA_banco_MBA_bkp = df_DIA_SEMANA_banco_MBA

df_DIA_SEMANA_banco_MBA_bkp =
  df_DIA_SEMANA_banco_MBA_bkp %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

colnames(df_DIA_SEMANA_banco_MBA_bkp)[1]<-'df_DIA_SEMANA_banco_MBA_bkp'
colnames(df_DIA_SEMANA_banco_MBA_bkp)[2]<-'QUANTIDADE'
colnames(df_DIA_SEMANA_banco_MBA_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#para script rmd:
df_DIA_SEMANA_banco_MBA_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", df_DIA_SEMANA_banco_MBA_bkp$PERCENTUAL))
df_DIA_SEMANA_banco_MBA_bkp_rmd = tail(df_DIA_SEMANA_banco_MBA_bkp,5)
#########################################################################################################
#########################################################################################################
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "DOMINGO"]<- "ADOMINGO"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "SEGUNDA"]<- "BSEGUNDA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "TERÇA"]<- "CTERÇA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "QUARTA"]<- "DQUARTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "QUINTA"]<- "EQUINTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "SEXTA"]<- "FSEXTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "SÁBADO"]<- "GSÁBADO"
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

df_DIA_SEMANA_banco_MBA =
  df_DIA_SEMANA_banco_MBA %>%
  janitor::tabyl(DIA_SEMANA) %>%
  arrange(DIA_SEMANA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "ADOMINGO"]<- "DOMINGO"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "BSEGUNDA"]<- "SEGUNDA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "CTERÇA"]<- "TERÇA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "DQUARTA"]<- "QUARTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "EQUINTA"]<- "QUINTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "FSEXTA"]<- "SEXTA"
df_DIA_SEMANA_banco_MBA$DIA_SEMANA[df_DIA_SEMANA_banco_MBA$DIA_SEMANA == "GSÁBADO"]<- "SÁBADO"
#########################################################################################################

colnames(df_DIA_SEMANA_banco_MBA)[1]<-'DIA'
colnames(df_DIA_SEMANA_banco_MBA)[2]<-'QUANTIDADE'
colnames(df_DIA_SEMANA_banco_MBA)[3]<-'PERCENTUAL'

#############################################################################################################

#df_DIA_SEMANA_banco_MBA =
#  df_DIA_SEMANA_banco_MBA %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# df_DIA_SEMANA_banco_MBA FIM
#########################################################################################################

#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
