#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR ESCOLA:
dir.create(file.path("~/diretorio_r/estciabh", "justica_restaurativa"))
setwd(file.path("~/diretorio_r/estciabh"))
#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
banco_JR <- read.csv("banco_jr.csv",header=TRUE, sep="|", encoding = "UTF-8", skip = 2) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
write.csv(banco_JR, file = "banco_JR_inicial.csv", row.names = TRUE)
#########################################################################################################
#Acertando nome colunas
#rename column by name
banco_JR =
  banco_JR |>
  rename_at('DATA_DO_ENCAMINHAMENTO_DO_JUIZ._PELA_ATA_DE._AUDIENCIA', ~'DATA_DO_ENCAMINHAMENTO_DO_JUIZ')

#setnames(banco_JR, "NOME_DA_.ESCOLA", "NOME_ESCOLA")
#########################################################################################################

banco_JR$DATA_DO_ENCAMINHAMENTO_DO_JUIZ[banco_JR$DATA_DO_ENCAMINHAMENTO_DO_JUIZ == ""]<- NA
banco_JR$DATA_DO_ENCAMINHAMENTO_DO_JUIZ[banco_JR$DATA_DO_ENCAMINHAMENTO_DO_JUIZ == "SEM INFORMACAO"]<- NA

banco_JR$DATA_DO_ENCAMINHAMENTO_DO_JUIZ <- dmy(banco_JR$DATA_DO_ENCAMINHAMENTO_DO_JUIZ)

banco_JR$DATA_DO_RECEBIMENTO_NO_SETOR[banco_JR$DATA_DO_RECEBIMENTO_NO_SETOR == ""]<- NA
banco_JR$DATA_DO_RECEBIMENTO_NO_SETOR <- dmy(banco_JR$DATA_DO_RECEBIMENTO_NO_SETOR)

banco_JR$NASCIMENTO[banco_JR$NASCIMENTO == ""]<- NA
banco_JR$NASCIMENTO <- dmy(banco_JR$NASCIMENTO)

banco_JR$DATA_DO_FATO[banco_JR$DATA_DO_FATO == ""]<- NA
banco_JR$DATA_DO_FATO <- dmy(banco_JR$DATA_DO_FATO)
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
## calcula o intervalo em anos
banco_JR$IDADE = as.period(interval(banco_JR$NASCIMENTO, banco_JR$DATA_DO_FATO))
# SEPARAR SO O PRIMEIRO ITEM DE "17y 2m 28d 0H 0M 0S" GERADO PELO SCRIPT ANTERIOR.
banco_JR$IDADE = banco_JR$IDADE@year

banco_JR$NOME2 <- gsub(" ","", banco_JR$NOME)
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/justica_restaurativa"))
#########################################################################################################
#########################################################################################################
#tabela total_casos_ESCOLA
banco_JR_total_casos = data.frame(nrow(banco_JR))

colnames(banco_JR_total_casos) <- c("QUANTIDADE DE CASOS ENCAMINHADOS")
#########################################################################################################
# Remove duplicate rows of the dataframe using variables
banco_JR_snr = distinct(banco_JR, NOME2, NASCIMENTO, .keep_all= TRUE)

#banco sem concurso de pessoas
banco_JR$PROCESSO2 = ajustar_nomes(banco_JR$PROCESSO)
banco_JR$PROCESSO2 = gsub("-", "", banco_JR$PROCESSO2)
#########################################################################################################
#########################################################################################################
banco_JR_atos_em_foco =

  banco_JR %>%
  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") %>%
  #select(-name) %>%
  filter(ATO_INFRACIONAL != "NSA")
#########################################################################################################
banco_JR_sem_concurso <- distinct(banco_JR_atos_em_foco, PROCESSO2, ATO_INFRACIONAL, .keep_all= TRUE)
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_JR_snr_SEXO_IDADE
#########################################################################################################

#########################################################################################################

banco_JR_snr_SEXO_IDADE =

  banco_JR_snr %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

#########################################################################################################
banco_JR_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_JR_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_JR_snr_SEXO_IDADE$SEXO[banco_JR_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_JR_snr_SEXO_IDADE$SEXO[banco_JR_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_JR_snr_SEXO_IDADE_pizza = banco_JR_snr_SEXO_IDADE
#########################################################################################################
banco_JR_snr_SEXO_IDADE_pizza =
  banco_JR_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_JR_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_JR_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_JR_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_JR_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_JR_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_JR_snr_SEXO_IDADE_pizza$IDADE[banco_JR_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_JR_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_JR_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_JR_snr_SEXO_IDADE_graf_pizza <- ddply(banco_JR_snr_SEXO_IDADE_pizza,
                                            c("SEXO"),
                                            summarise,
                                            QUANTIDADE = sum(QUANTIDADE))

banco_JR_snr_SEXO_IDADE_graf_pizza =
  banco_JR_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2))


banco_JR_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_JR_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_JR_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_JR_snr_SEXO_IDADE_pizza,
                                           c("IDADE"),
                                           summarise,
                                           QUANTIDADE = sum(QUANTIDADE))

banco_JR_snr_SEXO_IDADE_pizza_bkp = banco_JR_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_JR_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_JR_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_JR_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_JR_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_JR_snr_SEXO_IDADE =

  banco_JR_snr %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

#########################################################################################################
banco_JR_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_JR_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_JR_snr_SEXO_IDADE$SEXO[banco_JR_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_JR_snr_SEXO_IDADE$SEXO[banco_JR_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_JR_snr_SEXO_IDADE_pizza = banco_JR_snr_SEXO_IDADE
#########################################################################################################
banco_JR_snr_SEXO_IDADE_pizza =
  banco_JR_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_JR_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_JR_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_JR_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_JR_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_JR_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_JR_snr_SEXO_IDADE_pizza$IDADE[banco_JR_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_JR_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_JR_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_JR_snr_SEXO_IDADE_graf_pizza <- ddply(banco_JR_snr_SEXO_IDADE_pizza,
                                            c("SEXO"),
                                            summarise,
                                            QUANTIDADE = sum(QUANTIDADE))

banco_JR_snr_SEXO_IDADE_graf_pizza =
  banco_JR_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2))


banco_JR_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_JR_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_JR_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_JR_snr_SEXO_IDADE_pizza,
                                           c("IDADE"),
                                           summarise,
                                           QUANTIDADE = sum(QUANTIDADE))

banco_JR_snr_SEXO_IDADE_pizza_bkp = banco_JR_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_JR_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_JR_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_JR_snr_SEXO_IDADE FIM
#########################################################################################################
#############################################################################################################
#banco_JR_raca
#########################################################################################################

banco_JR_raca =
  banco_JR_snr |>
  select(RACA_COR)

colnames(banco_JR_raca)[1]<-'cor'

#########################################################################################################
banco_JR_raca$cor[banco_JR_raca$cor == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
banco_JR_raca$cor[banco_JR_raca$cor == "NAO SABE"]<- "VNÃO SABE"
banco_JR_raca$cor[banco_JR_raca$cor == "NSA"]<- "NÃO RESPONDEU"
banco_JR_raca$cor[banco_JR_raca$cor == "INDIGENA"]<- "INDÍGENA"
#########################################################################################################
# salvando para gráfico
banco_JR_raca_bkp = banco_JR_raca

banco_JR_raca_bkp =
  banco_JR_raca_bkp %>%
  janitor::tabyl(cor) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_JR_raca_bkp$cor[banco_JR_raca_bkp$cor == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_raca_bkp$cor[banco_JR_raca_bkp$cor == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
banco_JR_raca_bkp$PERCENTUAL2 <- str_replace (banco_JR_raca_bkp$percent, "%", "")
banco_JR_raca_bkp$PERCENTUAL2 = as.numeric(banco_JR_raca_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_JR_raca_bkp)[1]<-'banco_JR_raca_bkp'
colnames(banco_JR_raca_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_raca_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_raca_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_raca_bkp$PERCENTUAL))
banco_JR_raca_bkp_rmd = tail(banco_JR_raca_bkp,3)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_JR_raca$cor[banco_JR_raca$cor == "VNÃO SABE"]<- "UNÃO SABE"
#banco_JR_raca$cor[banco_JR_raca$cor == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_JR_raca_TABELA =
  banco_JR_raca %>%
  janitor::tabyl(cor) %>%
  arrange(cor) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_JR_raca_TABELA$cor[banco_JR_raca_TABELA$cor == "UNÃO SABE"]<- "NÃO SABE"
banco_JR_raca_TABELA$cor[banco_JR_raca_TABELA$cor == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_JR_raca_TABELA)[1]<-'RAÇA/COR'
colnames(banco_JR_raca_TABELA)[2]<-'QUANTIDADE'
colnames(banco_JR_raca_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_JR_raca FIM
#########################################################################################################
#############################################################################################################
#banco_JR_escolaridade
#########################################################################################################

banco_JR_escolaridade =
  banco_JR_snr |>
  select(SERIE_ATUAL_OU_ULTIMA_CURSADA)

#adaptando para o restante dos scripts
colnames(banco_JR_escolaridade)[1]<-'banco_JR_escolaridade'

banco_JR_escolaridade$banco_JR_escolaridade = ajustar_nomes(banco_JR_escolaridade$banco_JR_escolaridade)
#########################################################################################################
#AJUSTA OS FORA DE PADRÃO AQUI:
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "EJA"]<- "EJAENSFUND" #FIZ OPÇÃO PELO FUND
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == ""]<- "SEMINFORMACAO" #FIZ OPÇÃO PELO FUND
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "NSA"]<- "NAORESPONDEU" #FIZ OPÇÃO PELO FUND
#ORDENANDO

banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "1ªSERIE-ENSFUND"]<- "A1ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "2ªSERIE-ENSFUND"]<- "B2ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "3ªSERIE-ENSFUND"]<- "C3ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "4ªSERIE-ENSFUND"]<- "D4ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "5ªSERIE-ENSFUND"]<- "E5ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "6ªSERIE-ENSFUND"]<- "F6ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "7ªSERIE-ENSFUND"]<- "G7ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "8ªSERIE-ENSFUND"]<- "H8ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "9ªSERIE-ENSFUND"]<- "I9ªSERIE-ENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "1ºANO-ENSMEDIO"]<- "J1ºANO-ENSMEDIO"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "2ºANO-ENSMEDIO"]<- "K2ºANO-ENSMEDIO"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "3ºANO-ENSMEDIO"]<- "L3ºANO-ENSMEDIO"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "FACULDADE1ºPERIODO"]<- "LAFACULDADE1ºPERIODO"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "EJAENSFUND"]<- "MEJAENSFUND"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "EJAENSMEDIO"]<- "NEJAENSMEDIO"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "NAOSABE"]<- "ONAOSABE"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "NAORESPONDEU"]<- "PNAORESPONDEU"
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "SEMINFORMACAO"]<- "QSEMINFORMACAO"

#########################################################################################################
# salvando para gráfico
banco_JR_escolaridade_bkp = banco_JR_escolaridade

banco_JR_escolaridade_bkp =
  banco_JR_escolaridade_bkp %>%
  janitor::tabyl(banco_JR_escolaridade) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "MEJAENSFUND"]<- "EJA - ENS FUND"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "ONAOSABE"]<- "NÃO SABE"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
banco_JR_escolaridade_bkp$banco_JR_escolaridade[banco_JR_escolaridade_bkp$banco_JR_escolaridade == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"


#########################################################################################################
#replace "%" with "" in the percentual column
banco_JR_escolaridade_bkp$PERCENTUAL2 <- str_replace (banco_JR_escolaridade_bkp$percent, "%", "")
banco_JR_escolaridade_bkp$PERCENTUAL2 = as.numeric(banco_JR_escolaridade_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_JR_escolaridade_bkp)[1]<-'banco_JR_escolaridade_bkp'
colnames(banco_JR_escolaridade_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_escolaridade_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_escolaridade_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_escolaridade_bkp$PERCENTUAL))
banco_JR_escolaridade_bkp_rmd = tail(banco_JR_escolaridade_bkp,3)
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "VNÃO SABE"]<- "UNÃO SABE"
#banco_JR_escolaridade$banco_JR_escolaridade[banco_JR_escolaridade$banco_JR_escolaridade == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_JR_escolaridade_TABELA =
  banco_JR_escolaridade %>%
  janitor::tabyl(banco_JR_escolaridade) %>%
  arrange(banco_JR_escolaridade) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#ordenando:

banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "MEJAENSFUND"]<- "EJA - ENS FUND"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "ONAOSABE"]<- "NÃO SABE"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
banco_JR_escolaridade_TABELA$banco_JR_escolaridade[banco_JR_escolaridade_TABELA$banco_JR_escolaridade == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_JR_escolaridade_TABELA)[1]<-'ESCOLARIDADE'
colnames(banco_JR_escolaridade_TABELA)[2]<-'QUANTIDADE'
colnames(banco_JR_escolaridade_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_JR_escolaridade FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
# banco_JR_natureza_escola
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_JR_natureza_escola =
  banco_JR_snr %>%
  select(NATUREZA_ESCOLA)

colnames(banco_JR_natureza_escola)[1]<-'natureza_escola'

#########################################################################################################
#encontrando parte do texto e substituindo
banco_JR_natureza_escola$natureza_escola[agrep("NSA", banco_JR_natureza_escola$natureza_escola)] <- "NÃO RESPONDEU"
banco_JR_natureza_escola$natureza_escola[banco_JR_natureza_escola$natureza_escola == "PUBLICA"]<- "PÚBLICA"

#banco_JR_natureza_escola$natureza_escola[agrep("MUNICIPAL", banco_JR_natureza_escola$natureza_escola)] <- "MUNICIPAL"

#########################################################################################################

# salvando para gráfico
banco_JR_natureza_escola_bkp = banco_JR_natureza_escola

banco_JR_natureza_escola_bkp =
  banco_JR_natureza_escola_bkp %>%
  janitor::tabyl(natureza_escola) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "PVENDA NOVA"]<- "VENDA NOVA"
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_JR_natureza_escola_bkp$natureza_escola[banco_JR_natureza_escola_bkp$natureza_escola == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(banco_JR_natureza_escola_bkp)[1]<-'banco_JR_natureza_escola_bkp'
colnames(banco_JR_natureza_escola_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_natureza_escola_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_natureza_escola_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_natureza_escola_bkp$PERCENTUAL))
banco_JR_natureza_escola_bkp_rmd = tail(banco_JR_natureza_escola_bkp,5)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_JR_natureza_escola =
  banco_JR_natureza_escola %>%
  janitor::tabyl(natureza_escola) %>%
  arrange(natureza_escola) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_JR_natureza_escola$natureza_escola[banco_JR_natureza_escola$natureza_escola == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_natureza_escola$natureza_escola[banco_JR_natureza_escola$natureza_escola == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_natureza_escola$natureza_escola[banco_JR_natureza_escola$natureza_escola == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_JR_natureza_escola$natureza_escola[banco_JR_natureza_escola$natureza_escola == "PVENDA NOVA"]<- "VENDA NOVA"
banco_JR_natureza_escola$natureza_escola[banco_JR_natureza_escola$natureza_escola == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_natureza_escola$natureza_escola[banco_JR_natureza_escola$natureza_escola == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_JR_natureza_escola$natureza_escola[banco_JR_natureza_escola$natureza_escola == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(banco_JR_natureza_escola)[1]<-'NATUREZA'
colnames(banco_JR_natureza_escola)[2]<-'QUANTIDADE'
colnames(banco_JR_natureza_escola)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_JR_natureza_escola FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
# banco_JR_trabalho_atual
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_JR_trabalho_atual =
  banco_JR_snr %>%
  select(TRABALHA_ATUALMENTE)

banco_JR_trabalho_atual$TRABALHA_ATUALMENTE = ajustar_nomes(banco_JR_trabalho_atual$TRABALHA_ATUALMENTE)

colnames(banco_JR_trabalho_atual)[1]<-'trabalho_atual'

#########################################################################################################
#encontrando parte do texto e substituindo
banco_JR_trabalho_atual$trabalho_atual[agrep("NAO", banco_JR_trabalho_atual$trabalho_atual)] <- "NÃO"
banco_JR_trabalho_atual$trabalho_atual[agrep("NSA", banco_JR_trabalho_atual$trabalho_atual)] <- "NÃO RESPONDEU"

banco_JR_trabalho_atual$trabalho_atual[banco_JR_trabalho_atual$trabalho_atual == ""]<- "NÃO RESPONDEU"

#########################################################################################################

# salvando para gráfico
banco_JR_trabalho_atual_bkp = banco_JR_trabalho_atual

banco_JR_trabalho_atual_bkp =
  banco_JR_trabalho_atual_bkp %>%
  janitor::tabyl(trabalho_atual) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "PVENDA NOVA"]<- "VENDA NOVA"
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_JR_trabalho_atual_bkp$trabalho_atual[banco_JR_trabalho_atual_bkp$trabalho_atual == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(banco_JR_trabalho_atual_bkp)[1]<-'banco_JR_trabalho_atual_bkp'
colnames(banco_JR_trabalho_atual_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_trabalho_atual_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_trabalho_atual_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_trabalho_atual_bkp$PERCENTUAL))
banco_JR_trabalho_atual_bkp_rmd = tail(banco_JR_trabalho_atual_bkp,5)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_JR_trabalho_atual =
  banco_JR_trabalho_atual %>%
  janitor::tabyl(trabalho_atual) %>%
  arrange(trabalho_atual) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_JR_trabalho_atual$trabalho_atual[banco_JR_trabalho_atual$trabalho_atual == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_trabalho_atual$trabalho_atual[banco_JR_trabalho_atual$trabalho_atual == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_trabalho_atual$trabalho_atual[banco_JR_trabalho_atual$trabalho_atual == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_JR_trabalho_atual$trabalho_atual[banco_JR_trabalho_atual$trabalho_atual == "PVENDA NOVA"]<- "VENDA NOVA"
banco_JR_trabalho_atual$trabalho_atual[banco_JR_trabalho_atual$trabalho_atual == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_trabalho_atual$trabalho_atual[banco_JR_trabalho_atual$trabalho_atual == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_JR_trabalho_atual$trabalho_atual[banco_JR_trabalho_atual$trabalho_atual == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(banco_JR_trabalho_atual)[1]<-'TRABALHO ATUAL'
colnames(banco_JR_trabalho_atual)[2]<-'QUANTIDADE'
colnames(banco_JR_trabalho_atual)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_JR_trabalho_atual FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
# banco_JR_natureza_trabalho
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_JR_natureza_trabalho =
  banco_JR_snr %>%
  filter(TRABALHA_ATUALMENTE %in% "SIM") |>
  select(NATUREZA_DO_TRABALHO)

banco_JR_natureza_trabalho$NATUREZA_DO_TRABALHO = ajustar_nomes(banco_JR_natureza_trabalho$NATUREZA_DO_TRABALHO)

colnames(banco_JR_natureza_trabalho)[1]<-'natureza_trabalho'

#########################################################################################################
#encontrando parte do texto e substituindo
#banco_JR_natureza_trabalho$natureza_trabalho[agrep("NAO", banco_JR_natureza_trabalho$natureza_trabalho)] <- "NÃO"
#banco_JR_natureza_trabalho$natureza_trabalho[agrep("NSA", banco_JR_natureza_trabalho$natureza_trabalho)] <- "NÃO RESPONDEU"

banco_JR_natureza_trabalho$natureza_trabalho[banco_JR_natureza_trabalho$natureza_trabalho == "SEMINFORMACAO"]<- "NÃO RESPONDEU"

#########################################################################################################

# salvando para gráfico
banco_JR_natureza_trabalho_bkp = banco_JR_natureza_trabalho

banco_JR_natureza_trabalho_bkp =
  banco_JR_natureza_trabalho_bkp %>%
  janitor::tabyl(natureza_trabalho) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "PVENDA NOVA"]<- "VENDA NOVA"
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_JR_natureza_trabalho_bkp$natureza_trabalho[banco_JR_natureza_trabalho_bkp$natureza_trabalho == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(banco_JR_natureza_trabalho_bkp)[1]<-'banco_JR_natureza_trabalho_bkp'
colnames(banco_JR_natureza_trabalho_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_natureza_trabalho_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_natureza_trabalho_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_natureza_trabalho_bkp$PERCENTUAL))
banco_JR_natureza_trabalho_bkp_rmd = tail(banco_JR_natureza_trabalho_bkp,5)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_JR_natureza_trabalho =
  banco_JR_natureza_trabalho %>%
  janitor::tabyl(natureza_trabalho) %>%
  arrange(natureza_trabalho) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_JR_natureza_trabalho$natureza_trabalho[banco_JR_natureza_trabalho$natureza_trabalho == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_natureza_trabalho$natureza_trabalho[banco_JR_natureza_trabalho$natureza_trabalho == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_natureza_trabalho$natureza_trabalho[banco_JR_natureza_trabalho$natureza_trabalho == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_JR_natureza_trabalho$natureza_trabalho[banco_JR_natureza_trabalho$natureza_trabalho == "PVENDA NOVA"]<- "VENDA NOVA"
banco_JR_natureza_trabalho$natureza_trabalho[banco_JR_natureza_trabalho$natureza_trabalho == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_JR_natureza_trabalho$natureza_trabalho[banco_JR_natureza_trabalho$natureza_trabalho == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_JR_natureza_trabalho$natureza_trabalho[banco_JR_natureza_trabalho$natureza_trabalho == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(banco_JR_natureza_trabalho)[1]<-'NATUREZA DO TRABALHO'
colnames(banco_JR_natureza_trabalho)[2]<-'QUANTIDADE'
colnames(banco_JR_natureza_trabalho)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_JR_natureza_trabalho FIM
#########################################################################################################
#############################################################################################################
#banco_JR_renda_mensal
#########################################################################################################

banco_JR_renda_mensal =
  banco_JR_snr |>
  filter(TRABALHA_ATUALMENTE %in% "SIM") |>
  select(RENDA_MENSAL)

colnames(banco_JR_renda_mensal)[1]<-'renda_mensal'

#########################################################################################################

banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "ATE 1 SALARIO MININO "]<- "ATE 1 SALÁRIO MÍNIMO"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "DE  2  A 3 SALARIOS MININOS ( DE R$1874,00 A R$2811,00)"]<- "DE  2  A 3 SALARIOS MININOS"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "ACIMA DE 4 SALARIOS MINIMOS ( ACIMA DE R$3748,00)"]<- "EACIMA DE 4 SALARIOS MINIMOS"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "DE 1 A 2 SALARIOS MINIMOS"]<- "ATEA 1 A 2 SALARIOS MINIMOS"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == ""]<- "NÃO RESPONDEU"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "NSA"]<- "NÃO RESPONDEU"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "NÃO SABE"]<- "VNÃO SABE"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"

#banco_JR_renda_mensal$renda_mensal[order(banco_JR_renda_mensal$renda_mensal),]#ordenar, crescente, nome2
#########################################################################################################



banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "NAO SABE"]<- "VNÃO SABE"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "UNIAO ESTAVEL"]<- "UNIÃO ESTÁVEL"
#########################################################################################################
# salvando para gráfico
banco_JR_renda_mensal_bkp = banco_JR_renda_mensal

banco_JR_renda_mensal_bkp =
  banco_JR_renda_mensal_bkp %>%
  janitor::tabyl(renda_mensal) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_JR_renda_mensal_bkp$renda_mensal[banco_JR_renda_mensal_bkp$renda_mensal == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
banco_JR_renda_mensal_bkp$renda_mensal[banco_JR_renda_mensal_bkp$renda_mensal == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_renda_mensal_bkp$renda_mensal[banco_JR_renda_mensal_bkp$renda_mensal == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
banco_JR_renda_mensal_bkp$PERCENTUAL2 <- str_replace (banco_JR_renda_mensal_bkp$percent, "%", "")
banco_JR_renda_mensal_bkp$PERCENTUAL2 = as.numeric(banco_JR_renda_mensal_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_JR_renda_mensal_bkp)[1]<-'banco_JR_renda_mensal_bkp'
colnames(banco_JR_renda_mensal_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_renda_mensal_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_renda_mensal_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_renda_mensal_bkp$PERCENTUAL))
banco_JR_renda_mensal_bkp_bkp_rmd = tail(banco_JR_renda_mensal_bkp,5)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "VNÃO SABE"]<- "UNÃO SABE"
#banco_JR_renda_mensal$renda_mensal[banco_JR_renda_mensal$renda_mensal == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_JR_renda_mensal_TABELA =
  banco_JR_renda_mensal %>%
  janitor::tabyl(renda_mensal) %>%
  arrange(renda_mensal) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_JR_renda_mensal_TABELA$renda_mensal[banco_JR_renda_mensal_TABELA$renda_mensal == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_renda_mensal_TABELA$renda_mensal[banco_JR_renda_mensal_TABELA$renda_mensal == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
banco_JR_renda_mensal_TABELA$renda_mensal[banco_JR_renda_mensal_TABELA$renda_mensal == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
banco_JR_renda_mensal_TABELA$renda_mensal[banco_JR_renda_mensal_TABELA$renda_mensal == "EACIMA DE 4 SALARIOS MINIMOS"]<- "MAIS DE 4 SALÁRIOS MÍNIMOS"
banco_JR_renda_mensal_TABELA$renda_mensal[banco_JR_renda_mensal_TABELA$renda_mensal == "UNÃO SABE"]<- "NÃO SABE"
banco_JR_renda_mensal_TABELA$renda_mensal[banco_JR_renda_mensal_TABELA$renda_mensal == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_JR_renda_mensal_TABELA)[1]<-'RENDA MENSAL'
colnames(banco_JR_renda_mensal_TABELA)[2]<-'QUANTIDADE'
colnames(banco_JR_renda_mensal_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#RENDA_MENSAL FIM
#########################################################################################################
#############################################################################################################
#banco_JR_estado_civil
#########################################################################################################

banco_JR_estado_civil =
  banco_JR_snr |>
  select(ESTADO_CIVIL)

colnames(banco_JR_estado_civil)[1]<-'estado_civil'

#########################################################################################################

banco_JR_estado_civil$estado_civil[banco_JR_estado_civil$estado_civil == "NSA"]<- "NÃO RESPONDEU"
banco_JR_estado_civil$estado_civil[banco_JR_estado_civil$estado_civil == "DE  2  A 3 SALARIOS MININOS ( DE R$1874,00 A R$2811,00)"]<- "DE  2  A 3 SALARIOS MININOS"
#banco_JR_estado_civil$estado_civil[order(banco_JR_estado_civil$estado_civil),]#ordenar, crescente, nome2
#########################################################################################################
banco_JR_estado_civil$estado_civil[banco_JR_estado_civil$estado_civil == "UNIAO ESTAVEL"]<- "UNIÃO ESTÁVEL"
#########################################################################################################
# salvando para gráfico
banco_JR_estado_civil_bkp = banco_JR_estado_civil

banco_JR_estado_civil_bkp =
  banco_JR_estado_civil_bkp %>%
  janitor::tabyl(estado_civil) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_JR_estado_civil_bkp$estado_civil[banco_JR_estado_civil_bkp$estado_civil == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
banco_JR_estado_civil_bkp$estado_civil[banco_JR_estado_civil_bkp$estado_civil == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_estado_civil_bkp$estado_civil[banco_JR_estado_civil_bkp$estado_civil == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
banco_JR_estado_civil_bkp$PERCENTUAL2 <- str_replace (banco_JR_estado_civil_bkp$percent, "%", "")
banco_JR_estado_civil_bkp$PERCENTUAL2 = as.numeric(banco_JR_estado_civil_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_JR_estado_civil_bkp)[1]<-'banco_JR_estado_civil_bkp'
colnames(banco_JR_estado_civil_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_estado_civil_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_estado_civil_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_estado_civil_bkp$PERCENTUAL))
banco_JR_estado_civil_bkp_bkp_rmd = tail(banco_JR_estado_civil_bkp,5)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_JR_estado_civil$estado_civil[banco_JR_estado_civil$estado_civil == "VNÃO SABE"]<- "UNÃO SABE"
#banco_JR_estado_civil$estado_civil[banco_JR_estado_civil$estado_civil == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_JR_estado_civil_TABELA =
  banco_JR_estado_civil %>%
  janitor::tabyl(estado_civil) %>%
  arrange(estado_civil) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_JR_estado_civil_TABELA$estado_civil[banco_JR_estado_civil_TABELA$estado_civil == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_estado_civil_TABELA$estado_civil[banco_JR_estado_civil_TABELA$estado_civil == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
banco_JR_estado_civil_TABELA$estado_civil[banco_JR_estado_civil_TABELA$estado_civil == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
banco_JR_estado_civil_TABELA$estado_civil[banco_JR_estado_civil_TABELA$estado_civil == "EACIMA DE 4 SALARIOS MINIMOS"]<- "MAIS DE 4 SALÁRIOS MÍNIMOS"
banco_JR_estado_civil_TABELA$estado_civil[banco_JR_estado_civil_TABELA$estado_civil == "UNÃO SABE"]<- "NÃO SABE"
banco_JR_estado_civil_TABELA$estado_civil[banco_JR_estado_civil_TABELA$estado_civil == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_JR_estado_civil_TABELA)[1]<-'ESTADO CIVIL'
colnames(banco_JR_estado_civil_TABELA)[2]<-'QUANTIDADE'
colnames(banco_JR_estado_civil_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#estado_civil FIM
#########################################################################################################
#############################################################################################################
#banco_JR_estado_civil_pais
#########################################################################################################

banco_JR_estado_civil_pais =
  banco_JR_snr |>
  select(ESTADO_CIVIL_PAIS)

colnames(banco_JR_estado_civil_pais)[1]<-'estado_civil_pais'

#########################################################################################################

banco_JR_estado_civil_pais$estado_civil_pais[banco_JR_estado_civil_pais$estado_civil_pais == "NSA"]<- "NÃO RESPONDEU"
banco_JR_estado_civil_pais$estado_civil_pais[banco_JR_estado_civil_pais$estado_civil_pais == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
#banco_JR_estado_civil_pais$estado_civil_pais[order(banco_JR_estado_civil_pais$estado_civil_pais),]#ordenar, crescente, nome2
#########################################################################################################
banco_JR_estado_civil_pais$estado_civil_pais[banco_JR_estado_civil_pais$estado_civil_pais == "UNIAO ESTAVEL"]<- "UNIÃO ESTÁVEL"
#########################################################################################################
# salvando para gráfico
banco_JR_estado_civil_pais_bkp = banco_JR_estado_civil_pais

banco_JR_estado_civil_pais_bkp =
  banco_JR_estado_civil_pais_bkp %>%
  janitor::tabyl(estado_civil_pais) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_JR_estado_civil_pais_bkp$estado_civil_pais[banco_JR_estado_civil_pais_bkp$estado_civil_pais == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
banco_JR_estado_civil_pais_bkp$estado_civil_pais[banco_JR_estado_civil_pais_bkp$estado_civil_pais == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_estado_civil_pais_bkp$estado_civil_pais[banco_JR_estado_civil_pais_bkp$estado_civil_pais == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
banco_JR_estado_civil_pais_bkp$PERCENTUAL2 <- str_replace (banco_JR_estado_civil_pais_bkp$percent, "%", "")
banco_JR_estado_civil_pais_bkp$PERCENTUAL2 = as.numeric(banco_JR_estado_civil_pais_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_JR_estado_civil_pais_bkp)[1]<-'banco_JR_estado_civil_pais_bkp'
colnames(banco_JR_estado_civil_pais_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_estado_civil_pais_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_estado_civil_pais_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_estado_civil_pais_bkp$PERCENTUAL))
banco_JR_estado_civil_pais_bkp_bkp_rmd = tail(banco_JR_estado_civil_pais_bkp,5)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_JR_estado_civil_pais$estado_civil_pais[banco_JR_estado_civil_pais$estado_civil_pais == "VNÃO SABE"]<- "UNÃO SABE"
#banco_JR_estado_civil_pais$estado_civil_pais[banco_JR_estado_civil_pais$estado_civil_pais == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_JR_estado_civil_pais_TABELA =
  banco_JR_estado_civil_pais %>%
  janitor::tabyl(estado_civil_pais) %>%
  arrange(estado_civil_pais) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_JR_estado_civil_pais_TABELA$estado_civil_pais[banco_JR_estado_civil_pais_TABELA$estado_civil_pais == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_estado_civil_pais_TABELA$estado_civil_pais[banco_JR_estado_civil_pais_TABELA$estado_civil_pais == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
banco_JR_estado_civil_pais_TABELA$estado_civil_pais[banco_JR_estado_civil_pais_TABELA$estado_civil_pais == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
banco_JR_estado_civil_pais_TABELA$estado_civil_pais[banco_JR_estado_civil_pais_TABELA$estado_civil_pais == "EACIMA DE 4 SALARIOS MINIMOS"]<- "MAIS DE 4 SALÁRIOS MÍNIMOS"
banco_JR_estado_civil_pais_TABELA$estado_civil_pais[banco_JR_estado_civil_pais_TABELA$estado_civil_pais == "UNÃO SABE"]<- "NÃO SABE"
banco_JR_estado_civil_pais_TABELA$estado_civil_pais[banco_JR_estado_civil_pais_TABELA$estado_civil_pais == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_JR_estado_civil_pais_TABELA)[1]<-'ESTADO CIVIL'
colnames(banco_JR_estado_civil_pais_TABELA)[2]<-'QUANTIDADE'
colnames(banco_JR_estado_civil_pais_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#estado_civil_pais FIM
#########################################################################################################
#############################################################################################################
#banco_JR_uso_drogas
#########################################################################################################

banco_JR_uso_drogas =
  banco_JR_snr |>
  pivot_longer(cols = starts_with("DROGAS_USO"), values_to = "DROGA_USADA") %>%
  #select(-name) %>%
  filter(DROGA_USADA != "NSA") |>
  filter(USA_DROGAS_ATUALMENTE %in% "SIM") |>
  select(DROGA_USADA)

colnames(banco_JR_uso_drogas)[1]<-'uso_drogas'

#########################################################################################################
#########################################################################################################
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "NAO RESPONDEU"]<- "NÃO RESPONDEU"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == ""]<- "NÃO RESPONDEU"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "NSA"]<- "NÃO RESPONDEU"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "SOLVENTES/INALANTES(THINNER,COLA,LOLO,LANCA PERFUME)"]<- "SOLVENTES/INALANTES"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "PSICOFARMACOS( EXTASE,REMEDIOS DE TARJA PRETA,ANSIOLITICOS,ANTIDEPRESSIVOS)"]<- "PSICOFÁRMACOS"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "PSICOFARMACOS( REMEDIOS DE TARJA PRETA,ANSIOLITICOS,ANTIDEPRESSIVOS)"]<- "PSICOFÁRMACOS"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "NÃO SABE"]<- "VNÃO SABE"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "NAO SABE"]<- "VNÃO SABE"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "COCAINA"]<- "COCAÍNA"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "EXTASE"]<- "ÊXTASE"
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "ALCOOL"]<- "ÁLCOOL"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_JR_uso_drogas_bkp = banco_JR_uso_drogas

banco_JR_uso_drogas_bkp =
  banco_JR_uso_drogas_bkp %>%
  janitor::tabyl(uso_drogas) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
#########################################################################################################
#replace "%" with "" in the percentual column
banco_JR_uso_drogas_bkp$PERCENTUAL2 <- str_replace (banco_JR_uso_drogas_bkp$percent, "%", "")
banco_JR_uso_drogas_bkp$PERCENTUAL2 = as.numeric(banco_JR_uso_drogas_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_JR_uso_drogas_bkp)[1]<-'banco_JR_uso_drogas_bkp'
colnames(banco_JR_uso_drogas_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_uso_drogas_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_uso_drogas_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_uso_drogas_bkp$PERCENTUAL))
banco_JR_uso_drogas_bkp_rmd = tail(banco_JR_uso_drogas_bkp,5)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "VNÃO SABE"]<- "UNÃO SABE"
#banco_JR_uso_drogas$uso_drogas[banco_JR_uso_drogas$uso_drogas == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_JR_uso_drogas_TABELA =
  banco_JR_uso_drogas %>%
  janitor::tabyl(uso_drogas) %>%
  arrange(uso_drogas) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_JR_uso_drogas_TABELA$uso_drogas[banco_JR_uso_drogas_TABELA$uso_drogas == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_uso_drogas_TABELA$uso_drogas[banco_JR_uso_drogas_TABELA$uso_drogas == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
banco_JR_uso_drogas_TABELA$uso_drogas[banco_JR_uso_drogas_TABELA$uso_drogas == "ATEA 1 A 2 SALARIOS MINIMOS"]<- "DE 1 A 2 SALÁRIOS MÍNIMOS"
banco_JR_uso_drogas_TABELA$uso_drogas[banco_JR_uso_drogas_TABELA$uso_drogas == "EACIMA DE 4 SALARIOS MINIMOS"]<- "MAIS DE 4 SALÁRIOS MÍNIMOS"
banco_JR_uso_drogas_TABELA$uso_drogas[banco_JR_uso_drogas_TABELA$uso_drogas == "UNÃO SABE"]<- "NÃO SABE"
banco_JR_uso_drogas_TABELA$uso_drogas[banco_JR_uso_drogas_TABELA$uso_drogas == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_JR_uso_drogas_TABELA)[1]<-'USO DE DROGAS'
colnames(banco_JR_uso_drogas_TABELA)[2]<-'QUANTIDADE'
colnames(banco_JR_uso_drogas_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_JR_uso_drogas FIM
#########################################################################################################
#############################################################################################################
#banco_JR_medidaspro
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_JR_medidaspro =

  banco_JR |>
  filter(TEM_MEDIDA_PROTETIVA_NESSE_PROCESSO %in% "SIM" | TEM_MEDIDA_PROTETIVA_OUTROS_.PROCESSOS %in% "SIM") |>
  pivot_longer(cols = starts_with("QUAL_MEDIDA_PROTETIVA_0"), values_to = "MEDIDA_PROTETIVA") %>%
  #select(-name) %>%
  filter(MEDIDA_PROTETIVA != "NSA") |>
  select(MEDIDA_PROTETIVA)

banco_JR_medidaspro1 =

  banco_JR |>
  filter(TEM_MEDIDA_PROTETIVA_NESSE_PROCESSO %in% "SIM" | TEM_MEDIDA_PROTETIVA_OUTROS_.PROCESSOS %in% "SIM")
#########################################################################################################
colnames(banco_JR_medidaspro)[1]<-'banco_JR_medidaspro'
#########################################################################################################
#########################################################################################################
#########################################################################################################
banco_JR_medidaspro$banco_JR_medidaspro = ifelse(banco_JR_medidaspro$banco_JR_medidaspro == "1",
                                                 "ART. 101, I", banco_JR_medidaspro$banco_JR_medidaspro)

table(banco_JR_medidaspro$banco_JR_medidaspro)
banco_JR_medidaspro$banco_JR_medidaspro = ifelse(banco_JR_medidaspro$banco_JR_medidaspro == "2",
                                                 "ART. 101, II", banco_JR_medidaspro$banco_JR_medidaspro)

table(banco_JR_medidaspro$banco_JR_medidaspro)
banco_JR_medidaspro$banco_JR_medidaspro = ifelse(banco_JR_medidaspro$banco_JR_medidaspro == "3",
                                                 "ART. 101, III", banco_JR_medidaspro$banco_JR_medidaspro)

table(banco_JR_medidaspro$banco_JR_medidaspro)
banco_JR_medidaspro$banco_JR_medidaspro = ifelse(banco_JR_medidaspro$banco_JR_medidaspro == "4",
                                                 "ART. 101, IV", banco_JR_medidaspro$banco_JR_medidaspro)

table(banco_JR_medidaspro$banco_JR_medidaspro)
banco_JR_medidaspro$banco_JR_medidaspro = ifelse(banco_JR_medidaspro$banco_JR_medidaspro == "5",
                                                 "ART. 101, V", banco_JR_medidaspro$banco_JR_medidaspro)

table(banco_JR_medidaspro$banco_JR_medidaspro)
banco_JR_medidaspro$banco_JR_medidaspro = ifelse(banco_JR_medidaspro$banco_JR_medidaspro == "6",
                                                 "ART. 101, VI", banco_JR_medidaspro$banco_JR_medidaspro)

table(banco_JR_medidaspro$banco_JR_medidaspro)
banco_JR_medidaspro$banco_JR_medidaspro = ifelse(banco_JR_medidaspro$banco_JR_medidaspro == "7",
                                                 "ART. 101, VII", banco_JR_medidaspro$banco_JR_medidaspro)

#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_JR_medidaspro_bkp = banco_JR_medidaspro

banco_JR_medidaspro_bkp =
  banco_JR_medidaspro_bkp %>%
  janitor::tabyl(banco_JR_medidaspro) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_JR_medidaspro_bkp$banco_JR_medidaspro[banco_JR_medidaspro_bkp$banco_JR_medidaspro == "VNÃO SABE"]<- "NÃO SABE"
banco_JR_medidaspro_bkp$banco_JR_medidaspro[banco_JR_medidaspro_bkp$banco_JR_medidaspro == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
banco_JR_medidaspro_bkp$PERCENTUAL2 <- str_replace (banco_JR_medidaspro_bkp$percent, "%", "")
banco_JR_medidaspro_bkp$PERCENTUAL2 = as.numeric(banco_JR_medidaspro_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_JR_medidaspro_bkp)[1]<-'banco_JR_medidaspro_bkp'
colnames(banco_JR_medidaspro_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_medidaspro_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_medidaspro_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_medidaspro_bkp$PERCENTUAL))
banco_JR_medidaspro_bkp_rmd = tail(banco_JR_medidaspro_bkp,5)
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_JR_medidaspro$banco_JR_medidaspro[banco_JR_medidaspro$banco_JR_medidaspro == "VNÃO SABE"]<- "UNÃO SABE"
#banco_JR_medidaspro$banco_JR_medidaspro[banco_JR_medidaspro$banco_JR_medidaspro == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_JR_medidaspro_TABELA =
  banco_JR_medidaspro %>%
  janitor::tabyl(banco_JR_medidaspro) %>%
  arrange(banco_JR_medidaspro) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_JR_medidaspro_TABELA$banco_JR_medidaspro[banco_JR_medidaspro_TABELA$banco_JR_medidaspro == "UNÃO SABE"]<- "NÃO SABE"
banco_JR_medidaspro_TABELA$banco_JR_medidaspro[banco_JR_medidaspro_TABELA$banco_JR_medidaspro == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_JR_medidaspro_TABELA)[1]<-'MEDIDA'
colnames(banco_JR_medidaspro_TABELA)[2]<-'QUANTIDADE'
colnames(banco_JR_medidaspro_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_JR_medidaspro FIM
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#############################################################################################################
#banco_JR_decisao
#########################################################################################################
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_JR_decisao = banco_JR_snr

banco_JR_decisao =
  banco_JR_decisao |>
  clean_names()

#########################################################################################################
banco_JR_decisao =
  banco_JR_decisao |>
  filter(tem_medida_socioeducativa_nesse_processo %in% "SIM" | tem_medida_socioeducativa_em_outros_processos %in% "SIM") |>
  pivot_longer(cols = starts_with("qual_medida_socioeducativa_"), values_to = "MEDIDA_SOCIOEDUCATIVA") %>%
  #select(-name) %>%
  filter(MEDIDA_SOCIOEDUCATIVA != "NSA") |>
  select(MEDIDA_SOCIOEDUCATIVA)
#########################################################################################################

#########################################################################################################
#########################################################################################################
colnames(banco_JR_decisao)[1]<-'banco_JR_decisao'
#########################################################################################################
#########################################################################################################

#preenchimento de celulas:
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "INTERNACAO"]<-	"INTERNAÇÃO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "L.A"]<-	"LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "OUTRAS(OS)"]<-	"VOUTROS"
#banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "PSC"]<-	"REMISSAO c/c PSC"
#banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "SEMINFORMACAO"]<-	"VAZIO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOAPSC"]<-	"RETORNO A PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOALA"]<-	"RETORNO A LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAO_CC_PSC"]<-	"REMISSÃO c/c PSC"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAO_CC_LA"]<-	"REMISSÃO c/c LA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == ""]<-	"VAZIO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "EXTINÇAO_PUNIBILIDADE_MORTE"]<-	"EXTINÇÃO PUNIBILIDADE POR MORTE"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "INTERNAÇAO"]<-	"INTERNAÇÃO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "NI"]<-	"VSEM INFORMAÇÃO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSÃO_C_EXCLUSÃO PROCESSUAL"]<-	"REMISSÃO c/c EXCLUSÃO PROCESSUAL"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAO"]<-	"REMISSÃO"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "REMISSAO_CC_ADV"]<-	"REMISSÃO c/c ADVERTÊNCIA"
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "EXTINÇÃO POR PRESCRIÇAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"


#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
# salvando para gráfico
banco_JR_decisao_bkp = banco_JR_decisao

banco_JR_decisao_bkp =
  banco_JR_decisao_bkp %>%
  janitor::tabyl(banco_JR_decisao) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_JR_decisao_bkp$banco_JR_decisao[banco_JR_decisao_bkp$banco_JR_decisao == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_decisao_bkp$banco_JR_decisao[banco_JR_decisao_bkp$banco_JR_decisao == "VOUTROS"]<- "OUTROS"
#########################################################################################################
#replace "%" with "" in the percentual column
banco_JR_decisao_bkp$PERCENTUAL2 <- str_replace (banco_JR_decisao_bkp$percent, "%", "")
banco_JR_decisao_bkp$PERCENTUAL2 = as.numeric(banco_JR_decisao_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_JR_decisao_bkp)[1]<-'banco_JR_decisao_bkp'
colnames(banco_JR_decisao_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_decisao_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_decisao_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_decisao_bkp$PERCENTUAL))
banco_JR_decisao_bkp_rmd = tail(banco_JR_decisao_bkp,5)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "VNÃO SABE"]<- "UNÃO SABE"
#banco_JR_decisao$banco_JR_decisao[banco_JR_decisao$banco_JR_decisao == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_JR_decisao_TABELA =
  banco_JR_decisao %>%
  janitor::tabyl(banco_JR_decisao) %>%
  arrange(banco_JR_decisao) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_JR_decisao_TABELA$banco_JR_decisao[banco_JR_decisao_TABELA$banco_JR_decisao == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_JR_decisao_TABELA$banco_JR_decisao[banco_JR_decisao_TABELA$banco_JR_decisao == "VOUTROS"]<- "OUTROS"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_JR_decisao_TABELA)[1]<-'DECISÃO'
colnames(banco_JR_decisao_TABELA)[2]<-'QUANTIDADE'
colnames(banco_JR_decisao_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_JR_decisao FIM
#########################################################################################################

#########################################################################################################
#banco_JR_incidencia
#########################################################################################################
#########################################################################################################

banco_JR_incidencia =
  banco_JR_sem_concurso %>%
  select(ATO_INFRACIONAL)
#########################################################################################################
banco_JR_incidencia$ATO_INFRACIONAL <- gsub(" ","",banco_JR_incidencia$ATO_INFRACIONAL)
#########################################################################################################

#########################################################################################################
#AMEAÇA

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "147.ARTCPB",
                                             "AMEAÇA", banco_JR_incidencia$ATO_INFRACIONAL)


########################################################################################################
#########################################################################################################
#CRIME DE TRÂNSITO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "309.ARTCTB",
                                             "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "310.ARTCTB",
                                             "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "311.ARTCTB",
                                             "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)", banco_JR_incidencia$ATO_INFRACIONAL)

#para discrinar: é so anular com #

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)",
                                             "CRIME DE TRÂNSITO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)",
                                             "CRIME DE TRÂNSITO", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)",
                                             "CRIME DE TRÂNSITO", banco_JR_incidencia$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#DANO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "163.ARTCPB",
                                             "DANO", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "213.ARTCPB",
                                             "ESTUPRO", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "215.ARTCPB",
                                             "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "215-A.ARTCPB",
                                             "IMPORTUNAÇÃO SEXUAL", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ESTUPRO DE VULNERÁVEL

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "217-A.ARTCPB",
                                             "ESTUPRO DE VULNERÁVEL", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#FURTO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "155.ARTCPB",
                                             "FURTO", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#FURTO (TENTATIVA)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "155C/C14.ARTCPB",
                                             "FURTO (TENTATIVA)", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "121.ARTCPB",
                                             "HOMICÍDIO", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO (TENTATIVA)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "121C/C14,II.ARTCPB",
                                             "HOMICÍDIO (TENTATIVA)", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "129.ARTCPB",
                                             "LESÃO CORPORAL", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "129§3º.ARTCPB",
                                             "LESÃO CORPORAL", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "129§9º.ARTCPB",
                                             "LESÃO CORPORAL", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL (TENTATIVA). Ordem para trocar LESÃO CORPORAL (TENTATIVA) por VIAS DE FATO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "129C/C14,II.ARTCPB",
                                             "VIAS DE FATO", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PICHAÇÃO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "65.ART9.605",
                                             "PICHAÇÃO", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PORTE/POSSE DE ARMA

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "12.ART10.826",
                                             "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "14.ART10.826",
                                             "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "16.ART10.826",
                                             "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "15.ART10.826",
                                             "ARMA DE FOGO - PORTE ILEGAL (DISPARO)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "17.ART10.826",
                                             "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)", banco_JR_incidencia$ATO_INFRACIONAL)



banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)",
                                             "PORTE/POSSE DE ARMA", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)",
                                             "PORTE/POSSE DE ARMA", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)",
                                             "PORTE/POSSE DE ARMA", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (DISPARO)",
                                             "PORTE/POSSE DE ARMA", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)",
                                             "PORTE/POSSE DE ARMA", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#RECEPTAÇÃO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "180.ARTCPB",
                                             "RECEPTAÇÃO", banco_JR_incidencia$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157.ARTCPB",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºAICPB",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºAIIARTCPB",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2º,I,IIeVARTCPB",
                                             "ROUBO (EM CONCURSO DE PESSOAS)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2º,I.ARTCPB",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2º,IeII.ARTCPB",
                                             "ROUBO (EM CONCURSO DE PESSOAS)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºA,IARTCPB",
                                             "ROUBO (EMPREGO DE ARMA)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºIIARTCPB",
                                             "ROUBO (EM CONCURSO DE PESSOAS)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºIIeVARTCPB",
                                             "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºA,I",
                                             "ROUBO (EMPREGO DE ARMA)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºAIARTCPB",
                                             "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºAIeIIARTCPB",
                                             "ROUBO (EMPREGO DE ARMA)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºII,VeVIIARTCPB",
                                             "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºIIeVIARTCPB",
                                             "ROUBO (EMPREGO DE ARMA)", banco_JR_incidencia$ATO_INFRACIONAL)



banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºIIeVIIARTCPB",
                                             "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºIIEVIIARTCPB",
                                             "ROUBO (EMPREGO DE ARMA)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºVARTCPB",
                                             "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºVIIARTCPB",
                                             "ROUBO (EMPREGO DE ARMA)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2ºAIICPB",
                                             "ROUBO (EMPREGO DE ARMA)", banco_JR_incidencia$ATO_INFRACIONAL)


#para discrinar: é so anular com #

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                             "ROUBO", banco_JR_incidencia$ATO_INFRACIONAL)



#########################################################################################################
#ROUBO (TENTATIVA)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§2º,IeIIC/C14,II.ARTCPB",
                                             "ROUBO (TENTATIVA)", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157C/C14,II.ARTCPB",
                                             "ROUBO (TENTATIVA)", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ROUBO (§3º)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§3ºARTCPB",
                                             "ROUBO (§3º)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "ROUBO (§3º)",
                                             "LATROCÍNIO", banco_JR_incidencia$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO (§3º) (TENTATIVA)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "157§3ºARTCPBC/C14,II,CPB",
                                             "ROUBO (§3º) (TENTATIVA)", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#SEQUESTRO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "148.ARTCPB",
                                             "SEQUESTRO", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#TRÁFICO DE DROGAS


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "33.ART11.343",
                                             "TRÁFICO DE DROGAS", banco_JR_incidencia$ATO_INFRACIONAL)


#banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "37.ART11.343",
                                             "TRÁFICO DE DROGAS (INFORMANTE)", banco_JR_incidencia$ATO_INFRACIONAL)



#para discrinar: é so anular com #

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "TRÁFICO DE DROGAS",
                                             "TRÁFICO DE DROGAS", banco_JR_incidencia$ATO_INFRACIONAL)


#banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (INFORMANTE)",
                                             "TRÁFICO DE DROGAS", banco_JR_incidencia$ATO_INFRACIONAL)





#########################################################################################################
#########################################################################################################
#########################################################################################################
#ASSOCIAÇÃO TRÁFICO DE DROGAS


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "35.ART11.343",
                                             "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "34.ART11.343",
                                             "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_JR_incidencia$ATO_INFRACIONAL)

#para discrinar: é so anular com #

#banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (ASSOCIAÇÃO)",
#                                           "TRÁFICO DE DROGAS", banco_JR_incidencia$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#USO DE DROGAS

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "28.ART11.343",
                                             "POSSE DE DROGAS PARA USO PESSOAL", banco_JR_incidencia$ATO_INFRACIONAL)



#########################################################################################################
#########################################################################################################
#VIAS DE FATO

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "21.ARTLCP",
                                             "VIAS DE FATO", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################
#OUTROS

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "140.ARTCPB",
                                             "INJÚRIA", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "140§3º.ARTCPB",
                                             "INJÚRIA", banco_JR_incidencia$ATO_INFRACIONAL)



banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "330.ARTCPB",
                                             "DESOBEDIÊNCIA", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "331.ARTCPB",
                                             "DESACATO", banco_JR_incidencia$ATO_INFRACIONAL)


banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "139.ARTCPB",
                                             "DESOBEDIÊNCIA", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "2.ART13.185",
                                             "INTIMIDAÇÃO SISTEMÁTICA (BULLYING)", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "329.ARTCPB",
                                             "RESISTÊNCIA", banco_JR_incidencia$ATO_INFRACIONAL)

banco_JR_incidencia$ATO_INFRACIONAL = ifelse(banco_JR_incidencia$ATO_INFRACIONAL == "137.ARTCPB",
                                             "RIXA", banco_JR_incidencia$ATO_INFRACIONAL)


#########################################################################################################

#SUBSTITUIR
banco_JR_incidencia$ATO_INFRACIONAL[banco_JR_incidencia$ATO_INFRACIONAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#banco_JR_incidencia$ATO_INFRACIONAL[agrep(".ART", banco_JR_incidencia$ATO_INFRACIONAL)] <- "OUTROS"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_JR_incidencia_bkp = banco_JR_incidencia

banco_JR_incidencia_bkp =
  banco_JR_incidencia_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_JR_incidencia_bkp$ATO_INFRACIONAL[banco_JR_incidencia_bkp$ATO_INFRACIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(banco_JR_incidencia_bkp)[1]<-'banco_JR_incidencia_bkp'
colnames(banco_JR_incidencia_bkp)[2]<-'QUANTIDADE'
colnames(banco_JR_incidencia_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_JR_incidencia_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_JR_incidencia_bkp$PERCENTUAL))
banco_JR_incidencia_bkp_rmd = tail(banco_JR_incidencia_bkp,5)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_JR_incidencia =
  banco_JR_incidencia %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(ATO_INFRACIONAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_JR_incidencia$ATO_INFRACIONAL[banco_JR_incidencia$ATO_INFRACIONAL == "SEMINFORMACAO"]<- "SEM INFORMAÇÃO"


colnames(banco_JR_incidencia)[1]<-'ATO INFRACIONAL'
colnames(banco_JR_incidencia)[2]<-'QUANTIDADE'
colnames(banco_JR_incidencia)[3]<-'PERCENTUAL'

#############################################################################################################
# banco_JR_incidencia =
#   banco_JR_incidencia %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))
#########################################################################################################
# banco_JR_incidencia FIM
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#FIM
#########################################################################################################

