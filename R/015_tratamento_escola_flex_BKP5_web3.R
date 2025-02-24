#########################################################################################################
# 1 PROCEDIMENTOS INICIAIS

#rm(list=ls(all=TRUE)): SEM USAR SCRIPT.E sim, este:

##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR ESCOLA:
dir.create(file.path("~/diretorio_r/estciabh", "ESCOLA"))
setwd(file.path("~/diretorio_r/estciabh/ESCOLA"))
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
banco_ESCOLA = banco_incidencia_geral_ESCOLA
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
write.csv(banco_ESCOLA, file = "banco_ESCOLA_inicial.csv", row.names = TRUE)
#########################################################################################################
#Acertando nome colunas
#rename column by name
banco_ESCOLA =
  banco_ESCOLA |>
  rename_at('NOME_DA_.ESCOLA', ~'NOME_ESCOLA')


#setnames(banco_ESCOLA, "NOME_DA_.ESCOLA", "NOME_ESCOLA")
#########################################################################################################
#PREENCHER COM NA'S CELULAS VAZIAS e excluir
banco_ESCOLA$NOME_ESCOLA[banco_ESCOLA$NOME_ESCOLA == ""]<- "VAZIO"

banco_ESCOLA =
  banco_ESCOLA |>
  filter(!NOME_ESCOLA %in% "VAZIO")

#########################################################################################################
banco_ESCOLA_bkp = banco_ESCOLA
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/ESCOLA"))
#########################################################################################################
write_ods(banco_ESCOLA, "banco_ESCOLA.ods")
#########################################################################################################
#tabela total_casos_ESCOLA
banco_ESCOLA_total_casos = data.frame(nrow(banco_ESCOLA))

colnames(banco_ESCOLA_total_casos) <- c("QUANTIDADE DE CASOS ENCAMINHADOS")
#########################################################################################################
# Remove duplicate rows of the dataframe using variables
banco_ESCOLA_snr = distinct(banco_ESCOLA, NOME2, NASCIMENTO, .keep_all= TRUE)
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_ESCOLA =

  banco_ESCOLA %>%
  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") %>%
  #select(-name) %>%
  filter(!ATO_INFRACIONAL %in% "NSA" & !ATO_INFRACIONAL %in% "TERMOSEMINF." & !ATO_INFRACIONAL %in% "VAZIO")

#########################################################################################################
#########################################################################################################
#substituição especifica de artigo deve anteceder a genérica: vide as primeiras linhas e latrocinio e roubo:

banco_ESCOLA$ATO_INFRACIONAL = sub("28.ART11.*.*", "POSSE DE DROGAS PARA USO PESSOAL", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("34.ART11.*.*", "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("35.ART11.*.*", "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub(".*ART11.343.*.*", "TRÁFICO DE DROGAS", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub(".*ART10.826.*.*", "PORTE/POSSE DE ARMA", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub(".*ARTCTB.*.*", "CRIME DE TRÂNSITO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("121.ART.*.*", "HOMICÍDIO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("121C/C14.*.*", "HOMICÍDIO (TENTATIVA)", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("129.*.*", "LESÃO CORPORAL", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("129§.*.*", "LESÃO CORPORAL", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("137.*.*", "RIXA", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("140.ART.*.*", "RIXA", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("140§*.*", "RIXA", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("147.*.*", "AMEAÇA", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("148.ART.*.*", "SEQUESTRO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("155.ART.*.*", "FURTO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("155C/C.*.*", "FURTO (TENTATIVA)", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("157.ART.*.*", "ROUBO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("157C/C.*.*", "ROUBO (TENTATIVA)", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("157§3.*.*", "LATROCÍNIO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("157§.*.*", "ROUBO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("163.ART.*.*", "DANO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("171.ART.*.*", "ESTELIONATO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("180.ART.*.*", "RECEPTAÇÃO", banco_ESCOLA$ATO_INFRACIONAL)
#banco_ESCOLA$ATO_INFRACIONAL = sub("19.ART.*.*", "PORTE ARMA (LCP)", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("21.ART.*.*", "VIAS DE FATO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("213.ART.*.*", "ESTUPRO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("215.ART.*.*", "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("215-A.*.*", "IMPORTUNAÇÃO SEXUAL", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("217-A.*.*", "ESTUPRO DE VULNERÁVEL", banco_ESCOLA$ATO_INFRACIONAL)
#banco_ESCOLA$ATO_INFRACIONAL = sub("311.ARTCPB.*.*", "ADULTERAÇÃO DE SINAL IDENTIFICADOR DE VEÍCULO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("329.ART.*.*", "RESISTÊNCIA", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("330.ART.*.*", "DESOBEDIÊNCIA", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("331.ART.*.*", "DESACATO", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub("65.ART9.*.*", "PICHAÇÃO", banco_ESCOLA$ATO_INFRACIONAL)

#substituindo os restantes em outros
banco_ESCOLA$ATO_INFRACIONAL = sub(".*OUTROS.*.*", "VOUTROS", banco_ESCOLA$ATO_INFRACIONAL)
banco_ESCOLA$ATO_INFRACIONAL = sub(".*ART.*.*", "VOUTROS", banco_ESCOLA$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#banco sem concurso de pessoas
banco_ESCOLA_sem_concurso <- banco_ESCOLA[!duplicated(data.frame(banco_ESCOLA$PROCESSO, banco_ESCOLA$ATO_INFRACIONAL)),]
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_ESCOLA_snr_SEXO_IDADE
#########################################################################################################

#########################################################################################################

banco_ESCOLA_snr_SEXO_IDADE =

  banco_ESCOLA_snr %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

#########################################################################################################
banco_ESCOLA_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_ESCOLA_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_ESCOLA_snr_SEXO_IDADE$SEXO[banco_ESCOLA_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_ESCOLA_snr_SEXO_IDADE$SEXO[banco_ESCOLA_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_ESCOLA_snr_SEXO_IDADE_pizza = banco_ESCOLA_snr_SEXO_IDADE
#########################################################################################################
banco_ESCOLA_snr_SEXO_IDADE_pizza =
  banco_ESCOLA_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_ESCOLA_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE[banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_ESCOLA_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_ESCOLA_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_ESCOLA_snr_SEXO_IDADE_graf_pizza <- ddply(banco_ESCOLA_snr_SEXO_IDADE_pizza,
                                                c("SEXO"),
                                                summarise,
                                                QUANTIDADE = sum(QUANTIDADE))

banco_ESCOLA_snr_SEXO_IDADE_graf_pizza =
  banco_ESCOLA_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_ESCOLA_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_ESCOLA_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_ESCOLA_snr_SEXO_IDADE_pizza,
                                               c("IDADE"),
                                               summarise,
                                               QUANTIDADE = sum(QUANTIDADE))

banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp = banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_ESCOLA_snr_SEXO_IDADE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_ESCOLA_snr_SEXO_IDADE
#########################################################################################################
#########################################################################################################

banco_ESCOLA_snr_SEXO_IDADE =

  banco_ESCOLA_snr %>%
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, IDADE)

#########################################################################################################
banco_ESCOLA_snr_SEXO_IDADE$SEXO = ajustar_nomes(banco_ESCOLA_snr_SEXO_IDADE$SEXO)
#########################################################################################################
#encontrando parte do texto e substituindo
banco_ESCOLA_snr_SEXO_IDADE$SEXO[banco_ESCOLA_snr_SEXO_IDADE$SEXO == "F"]<- "FEMININO"
banco_ESCOLA_snr_SEXO_IDADE$SEXO[banco_ESCOLA_snr_SEXO_IDADE$SEXO == "M"]<- "MASCULINO"
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_ESCOLA_snr_SEXO_IDADE_pizza = banco_ESCOLA_snr_SEXO_IDADE
#########################################################################################################
banco_ESCOLA_snr_SEXO_IDADE_pizza =
  banco_ESCOLA_snr_SEXO_IDADE_pizza |>
  #filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, IDADE, sort = TRUE)

colnames(banco_ESCOLA_snr_SEXO_IDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
########################################################################################################
#SUBSTITUIR
banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE[which(is.na(banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE))] <- "s/inf"
banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE <- paste(banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE, "anos", sep=" ")
banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE[banco_ESCOLA_snr_SEXO_IDADE_pizza$IDADE == "s/inf anos"]<- "s/inf"
#########################################################################################################
banco_ESCOLA_snr_SEXO_IDADE_pizza$PERCENTUAL <- round_preserve_sum(prop.table(banco_ESCOLA_snr_SEXO_IDADE_pizza$QUANTIDADE)*100, 2)
#########################################################################################################
# GRAFICO PIZZA
banco_ESCOLA_snr_SEXO_IDADE_graf_pizza <- ddply(banco_ESCOLA_snr_SEXO_IDADE_pizza,
                                                c("SEXO"),
                                                summarise,
                                                QUANTIDADE = sum(QUANTIDADE))

banco_ESCOLA_snr_SEXO_IDADE_graf_pizza =
  banco_ESCOLA_snr_SEXO_IDADE_graf_pizza |>
  mutate(PERCENTUAL = round_preserve_sum(proportions(QUANTIDADE), 2)*100)


banco_ESCOLA_snr_SEXO_IDADE_graf_pizza$PERCENTUAL2 <- paste(banco_ESCOLA_snr_SEXO_IDADE_graf_pizza$PERCENTUAL, "%", sep="")
#########################################################################################################
#para script rmd:

banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp <- ddply(banco_ESCOLA_snr_SEXO_IDADE_pizza,
                                               c("IDADE"),
                                               summarise,
                                               QUANTIDADE = sum(QUANTIDADE))

banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp = banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp |> arrange(QUANTIDADE)

banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp_rmd = tail(banco_ESCOLA_snr_SEXO_IDADE_pizza_bkp,5)
#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_ESCOLA_snr_SEXO_IDADE FIM
#########################################################################################################

#############################################################################################################
#ESCOLARIDADE_banco_escola
#########################################################################################################

ESCOLARIDADE_banco_escola =
  banco_ESCOLA_snr |>
  select(ESCOLARIDADE)

#adaptando para o restante dos scripts
colnames(ESCOLARIDADE_banco_escola)[1]<-'ESCOLARIDADE_banco_escola'

ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola = ajustar_nomes(ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola)
#########################################################################################################
#AJUSTA OS FORA DE PADRÃO AQUI:
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "EJA"]<- "EJAENSFUND" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == ""]<- "SEMINFORMACAO" #FIZ OPÇÃO PELO FUND
#ORDENANDO

ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "1ªSERIE-ENSFUND"]<- "A1ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "2ªSERIE-ENSFUND"]<- "B2ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "3ªSERIE-ENSFUND"]<- "C3ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "4ªSERIE-ENSFUND"]<- "D4ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "5ªSERIE-ENSFUND"]<- "E5ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "6ªSERIE-ENSFUND"]<- "F6ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "7ªSERIE-ENSFUND"]<- "G7ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "8ªSERIE-ENSFUND"]<- "H8ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "9ªSERIE-ENSFUND"]<- "I9ªSERIE-ENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "1ºANO-ENSMEDIO"]<- "J1ºANO-ENSMEDIO"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "2ºANO-ENSMEDIO"]<- "K2ºANO-ENSMEDIO"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "3ºANO-ENSMEDIO"]<- "L3ºANO-ENSMEDIO"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "EJAENSFUND"]<- "MEJAENSFUND"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "EJAENSMEDIO"]<- "NEJAENSMEDIO"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "NAOSABE"]<- "ONAOSABE"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "NAORESPONDEU"]<- "PNAORESPONDEU"
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "SEMINFORMACAO"]<- "QSEMINFORMACAO"

#########################################################################################################
# salvando para gráfico
ESCOLARIDADE_banco_escola_bkp = ESCOLARIDADE_banco_escola

ESCOLARIDADE_banco_escola_bkp =
  ESCOLARIDADE_banco_escola_bkp %>%
  janitor::tabyl(ESCOLARIDADE_banco_escola) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_bkp$ESCOLARIDADE_banco_escola == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"


#########################################################################################################
#replace "%" with "" in the percentual column
ESCOLARIDADE_banco_escola_bkp$PERCENTUAL2 <- str_replace (ESCOLARIDADE_banco_escola_bkp$percent, "%", "")
ESCOLARIDADE_banco_escola_bkp$PERCENTUAL2 = as.numeric(ESCOLARIDADE_banco_escola_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(ESCOLARIDADE_banco_escola_bkp)[1]<-'ESCOLARIDADE_banco_escola_bkp'
colnames(ESCOLARIDADE_banco_escola_bkp)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_banco_escola_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESCOLARIDADE_banco_escola_bkp_rmark = ESCOLARIDADE_banco_escola_bkp

ESCOLARIDADE_banco_escola_bkp_rmark = ESCOLARIDADE_banco_escola_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
ESCOLARIDADE_banco_escola_bkp_rmark =
  ESCOLARIDADE_banco_escola_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "VNÃO SABE"]<- "UNÃO SABE"
#ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola$ESCOLARIDADE_banco_escola == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
ESCOLARIDADE_banco_escola_TABELA =
  ESCOLARIDADE_banco_escola %>%
  janitor::tabyl(ESCOLARIDADE_banco_escola) %>%
  arrange(ESCOLARIDADE_banco_escola) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#ordenando:

ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola[ESCOLARIDADE_banco_escola_TABELA$ESCOLARIDADE_banco_escola == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(ESCOLARIDADE_banco_escola_TABELA)[1]<-'ESCOLARIDADE'
colnames(ESCOLARIDADE_banco_escola_TABELA)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_banco_escola_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#ESCOLARIDADE_banco_escola FIM
#########################################################################################################
#########################################################################################################
#banco_ESCOLA_incidencia
#########################################################################################################

#########################################################################################################

banco_ESCOLA_incidencia =
  banco_ESCOLA_sem_concurso %>%
  select(ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
banco_ESCOLA_incidencia$ATO_INFRACIONAL[banco_ESCOLA_incidencia$ATO_INFRACIONAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
banco_ESCOLA_incidencia$ATO_INFRACIONAL[agrep(".ART", banco_ESCOLA_incidencia$ATO_INFRACIONAL)] <- "OUTROS"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_ESCOLA_incidencia_bkp = banco_ESCOLA_incidencia

banco_ESCOLA_incidencia_bkp =
  banco_ESCOLA_incidencia_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_ESCOLA_incidencia_bkp$ATO_INFRACIONAL[banco_ESCOLA_incidencia_bkp$ATO_INFRACIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(banco_ESCOLA_incidencia_bkp)[1]<-'banco_ESCOLA_incidencia_bkp'
colnames(banco_ESCOLA_incidencia_bkp)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_incidencia_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_ESCOLA_incidencia_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ESCOLA_incidencia_bkp$PERCENTUAL))
banco_ESCOLA_incidencia_bkp_rmd = tail(banco_ESCOLA_incidencia_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_ESCOLA_incidencia =
  banco_ESCOLA_incidencia %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(ATO_INFRACIONAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_incidencia$ATO_INFRACIONAL[banco_ESCOLA_incidencia$ATO_INFRACIONAL == "VS/INF"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_incidencia$ATO_INFRACIONAL[banco_ESCOLA_incidencia$ATO_INFRACIONAL == "VOUTROS"]<- "OUTROS"

colnames(banco_ESCOLA_incidencia)[1]<-'ATO INFRACIONAL'
colnames(banco_ESCOLA_incidencia)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_incidencia)[3]<-'PERCENTUAL'

#############################################################################################################
# banco_ESCOLA_incidencia =
#   banco_ESCOLA_incidencia %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ESCOLA_incidencia FIM
#########################################################################################################
#banco_ESCOLA_primariedade
#########################################################################################################
#########################################################################################################

banco_ESCOLA_primariedade =
  banco_ESCOLA_snr %>%
  select(PRIMARIO)
#########################################################################################################
banco_ESCOLA_primariedade$PRIMARIO = ajustar_nomes(banco_ESCOLA_primariedade$PRIMARIO)
#########################################################################################################
#SUBSTITUIR
banco_ESCOLA_primariedade$PRIMARIO[banco_ESCOLA_primariedade$PRIMARIO == "NAO"]<- "NÃO"
banco_ESCOLA_primariedade$PRIMARIO[banco_ESCOLA_primariedade$PRIMARIO == ""]<- "NÃO"
banco_ESCOLA_primariedade$PRIMARIO[agrep(".ART", banco_ESCOLA_primariedade$PRIMARIO)] <- "OUTROS"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_ESCOLA_primariedade_bkp = banco_ESCOLA_primariedade

banco_ESCOLA_primariedade_bkp =
  banco_ESCOLA_primariedade_bkp %>%
  janitor::tabyl(PRIMARIO) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_ESCOLA_primariedade_bkp$PRIMARIO[banco_ESCOLA_primariedade_bkp$PRIMARIO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(banco_ESCOLA_primariedade_bkp)[1]<-'banco_ESCOLA_primariedade_bkp'
colnames(banco_ESCOLA_primariedade_bkp)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_primariedade_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_ESCOLA_primariedade_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ESCOLA_primariedade_bkp$PERCENTUAL))
banco_ESCOLA_primariedade_bkp_rmd = tail(banco_ESCOLA_primariedade_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_ESCOLA_primariedade =
  banco_ESCOLA_primariedade %>%
  janitor::tabyl(PRIMARIO) %>%
  arrange(PRIMARIO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_primariedade$PRIMARIO[banco_ESCOLA_primariedade$PRIMARIO == "VS/INF"]<- "SEM INFORMAÇÃO"


colnames(banco_ESCOLA_primariedade)[1]<-'PRIMÁRIO'
colnames(banco_ESCOLA_primariedade)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_primariedade)[3]<-'PERCENTUAL'

#############################################################################################################
# banco_ESCOLA_primariedade =
#   banco_ESCOLA_primariedade %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ESCOLA_primariedade FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
###banco_ESCOLA_decisao
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/ESCOLA"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_ESCOLA_decisao =
  banco_ESCOLA_bkp |>
  select(DECISAO)
#########################################################################################################
colnames(banco_ESCOLA_decisao)[1]<-'decisao'
#########################################################################################################
#########################################################################################################
banco_ESCOLA_decisao$decisao <- gsub(" ","", banco_ESCOLA_decisao$decisao)
#########################################################################################################

#preenchimento de celulas:
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "INTERNACAO"]<-	"INTERNAÇÃO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "LA"]<-	"REMISSAO c/c LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "OUTRAS(OS)"]<-	"VOUTROS"
#banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "PSC"]<-	"REMISSAO c/c PSC"
#banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "SEMINFORMACAO"]<-	"VAZIO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOAPSC"]<-	"RETORNO A PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOALA"]<-	"RETORNO A LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == ""]<-	"VAZIO"

#########################################################################################################
#########################################################################################################
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "VAZIO"]<-	"SEM INFORMAÇÃO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "VOUTROS"]<-	"OUTROS"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_ESCOLA_decisao_bkp = banco_ESCOLA_decisao

banco_ESCOLA_decisao_bkp =
  banco_ESCOLA_decisao_bkp %>%
  janitor::tabyl(decisao) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

colnames(banco_ESCOLA_decisao_bkp)[1]<-'banco_ESCOLA_decisao_bkp'
colnames(banco_ESCOLA_decisao_bkp)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_decisao_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_ESCOLA_decisao_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ESCOLA_decisao_bkp$PERCENTUAL))
banco_ESCOLA_decisao_bkp_rmd = tail(banco_ESCOLA_decisao_bkp,5)

banco_ESCOLA_decisao_bkp_rmd1 =
  banco_ESCOLA_decisao_bkp |>
  filter(grepl("^REMISSÃO|ARQUIVAMENTO$", banco_ESCOLA_decisao_bkp))
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#ordenando outros para final da tabela
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "OUTROS"]<-	"ROUTROS"
#########################################################################################################
banco_ESCOLA_decisao =
  banco_ESCOLA_decisao %>%
  janitor::tabyl(decisao) %>%
  arrange(decisao) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_decisao$decisao[banco_ESCOLA_decisao$decisao == "ROUTROS"]<- "OUTROS"

colnames(banco_ESCOLA_decisao)[1]<-'DECISÃO'
colnames(banco_ESCOLA_decisao)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_decisao)[3]<-'PERCENTUAL'

#############################################################################################################
# banco_ESCOLA_decisao =
#   banco_ESCOLA_decisao %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ESCOLA_decisao FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
#banco_ESCOLA_vitima
#########################################################################################################
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_ESCOLA_vitima = banco_ESCOLA_sem_concurso |>
  select(-name)

banco_ESCOLA_vitima =
  banco_ESCOLA_vitima %>%
  pivot_longer(cols = starts_with("TIPO_DE_VITIMA"), values_to = "VITIMA") %>%
  #select(-name) %>%
  filter(VITIMA != "") |>
  select(VITIMA) |>
  filter(!VITIMA %in% "NSA")

#########################################################################################################
#########################################################################################################
#########################################################################################################
banco_ESCOLA_vitima$VITIMA = ajustar_nomes(banco_ESCOLA_vitima$VITIMA)
#########################################################################################################
banco_ESCOLA_vitima$VITIMA[banco_ESCOLA_vitima$VITIMA == "COMUNIDADEESCOLAR"]<- "COMUNIDADE ESCOLAR"
banco_ESCOLA_vitima$VITIMA[banco_ESCOLA_vitima$VITIMA == "DIRETORA"]<- "DIRETOR(A)"
banco_ESCOLA_vitima$VITIMA[banco_ESCOLA_vitima$VITIMA == "FUNCIONARIODAESCOLA"]<- "FUNCIONÁRIO DA ESCOLA"
#banco_ESCOLA_vitima$VITIMA[agrep(".ART", banco_ESCOLA_vitima$VITIMA)] <- "OUTROS"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_ESCOLA_vitima_bkp = banco_ESCOLA_vitima

banco_ESCOLA_vitima_bkp =
  banco_ESCOLA_vitima_bkp %>%
  janitor::tabyl(VITIMA) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_ESCOLA_vitima_bkp$VITIMA[banco_ESCOLA_vitima_bkp$VITIMA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(banco_ESCOLA_vitima_bkp)[1]<-'banco_ESCOLA_vitima_bkp'
colnames(banco_ESCOLA_vitima_bkp)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_vitima_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_ESCOLA_vitima_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ESCOLA_vitima_bkp$PERCENTUAL))
banco_ESCOLA_vitima_bkp_rmd = tail(banco_ESCOLA_vitima_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_ESCOLA_vitima =
  banco_ESCOLA_vitima %>%
  janitor::tabyl(VITIMA) %>%
  arrange(VITIMA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_vitima$VITIMA[banco_ESCOLA_vitima$VITIMA == "VS/INF"]<- "SEM INFORMAÇÃO"


colnames(banco_ESCOLA_vitima)[1]<-'VÍTIMA'
colnames(banco_ESCOLA_vitima)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_vitima)[3]<-'PERCENTUAL'

#############################################################################################################
# banco_ESCOLA_vitima =
#   banco_ESCOLA_vitima %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ESCOLA_vitima FIM
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_ESCOLA_regional_residencia =
  banco_ESCOLA_snr %>%
  select(REGIONAL_RESIDENCIAL)

colnames(banco_ESCOLA_regional_residencia)[1]<-'regional_residencial'

#########################################################################################################
#encontrando parte do texto e substituindo
banco_ESCOLA_regional_residencia$regional_residencial[agrep("/MG", banco_ESCOLA_regional_residencia$regional_residencial)] <- "UOUTRA CIDADE MG"
banco_ESCOLA_regional_residencia$regional_residencial[agrep("RMBH", banco_ESCOLA_regional_residencia$regional_residencial)] <- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_residencia$regional_residencial[agrep("RIBEIRAO DAS NEVES", banco_ESCOLA_regional_residencia$regional_residencial)] <- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_residencia$regional_residencial[agrep("CATAGUASES", banco_ESCOLA_regional_residencia$regional_residencial)] <- "UOUTRA CIDADE MG"
banco_ESCOLA_regional_residencia$regional_residencial[agrep("CIDADE DE BRASILIA/DF", banco_ESCOLA_regional_residencia$regional_residencial)] <- "VOUTRO ESTADO"
banco_ESCOLA_regional_residencia$regional_residencial[agrep("N/DISP", banco_ESCOLA_regional_residencia$regional_residencial)] <- "ZSEM INFORMAÇÃO"
banco_ESCOLA_regional_residencia$regional_residencial[agrep("INFORMACAO", banco_ESCOLA_regional_residencia$regional_residencial)] <- "ZSEM INFORMAÇÃO"
#substituindo
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == ""]<- "ZSEM INFORMAÇÃO"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "PAMPULHA"]<- "OESTEPAMPULHA"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "VENDA NOVA"]<- "PVENDA NOVA"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "REGIÃO METROPOLITANA"]<- "QREGIÃO METROPOLITANA"

#########################################################################################################

# salvando para gráfico
banco_ESCOLA_regional_residencia_bkp = banco_ESCOLA_regional_residencia

banco_ESCOLA_regional_residencia_bkp =
  banco_ESCOLA_regional_residencia_bkp %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "PVENDA NOVA"]<- "VENDA NOVA"
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_ESCOLA_regional_residencia_bkp$regional_residencial[banco_ESCOLA_regional_residencia_bkp$regional_residencial == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(banco_ESCOLA_regional_residencia_bkp)[1]<-'banco_ESCOLA_regional_residencia_bkp'
colnames(banco_ESCOLA_regional_residencia_bkp)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_regional_residencia_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_ESCOLA_regional_residencia_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ESCOLA_regional_residencia_bkp$PERCENTUAL))
banco_ESCOLA_regional_residencia_bkp_rmd = tail(banco_ESCOLA_regional_residencia_bkp,5)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_ESCOLA_regional_residencia =
  banco_ESCOLA_regional_residencia %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(regional_residencial) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "PVENDA NOVA"]<- "VENDA NOVA"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_ESCOLA_regional_residencia$regional_residencial[banco_ESCOLA_regional_residencia$regional_residencial == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(banco_ESCOLA_regional_residencia)[1]<-'REGIONAL'
colnames(banco_ESCOLA_regional_residencia)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_regional_residencia)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ESCOLA_regional_residencia FIM
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_ESCOLA_tipo_escola =
  banco_ESCOLA_sem_concurso %>%
  select(NOME_ESCOLA)

colnames(banco_ESCOLA_tipo_escola)[1]<-'tipo_escola'

#########################################################################################################
#encontrando parte do texto e substituindo
banco_ESCOLA_tipo_escola$tipo_escola[agrep("ESTADUAL", banco_ESCOLA_tipo_escola$tipo_escola)] <- "ESTADUAL"
banco_ESCOLA_tipo_escola$tipo_escola[agrep("MUNICIPAL", banco_ESCOLA_tipo_escola$tipo_escola)] <- "MUNICIPAL"

banco_ESCOLA_tipo_escola$tipo_escola =
  ifelse(banco_ESCOLA_tipo_escola$tipo_escola == "ESTADUAL" | banco_ESCOLA_tipo_escola$tipo_escola == "MUNICIPAL",
         banco_ESCOLA_tipo_escola$tipo_escola, "PARTICULAR")

#########################################################################################################

# salvando para gráfico
banco_ESCOLA_tipo_escola_bkp = banco_ESCOLA_tipo_escola

banco_ESCOLA_tipo_escola_bkp =
  banco_ESCOLA_tipo_escola_bkp %>%
  janitor::tabyl(tipo_escola) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "PVENDA NOVA"]<- "VENDA NOVA"
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_ESCOLA_tipo_escola_bkp$tipo_escola[banco_ESCOLA_tipo_escola_bkp$tipo_escola == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(banco_ESCOLA_tipo_escola_bkp)[1]<-'banco_ESCOLA_tipo_escola_bkp'
colnames(banco_ESCOLA_tipo_escola_bkp)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_tipo_escola_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_ESCOLA_tipo_escola_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ESCOLA_tipo_escola_bkp$PERCENTUAL))
banco_ESCOLA_tipo_escola_bkp_rmd = tail(banco_ESCOLA_tipo_escola_bkp,5)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_ESCOLA_tipo_escola =
  banco_ESCOLA_tipo_escola %>%
  janitor::tabyl(tipo_escola) %>%
  arrange(tipo_escola) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_tipo_escola$tipo_escola[banco_ESCOLA_tipo_escola$tipo_escola == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_tipo_escola$tipo_escola[banco_ESCOLA_tipo_escola$tipo_escola == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_tipo_escola$tipo_escola[banco_ESCOLA_tipo_escola$tipo_escola == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_ESCOLA_tipo_escola$tipo_escola[banco_ESCOLA_tipo_escola$tipo_escola == "PVENDA NOVA"]<- "VENDA NOVA"
banco_ESCOLA_tipo_escola$tipo_escola[banco_ESCOLA_tipo_escola$tipo_escola == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_tipo_escola$tipo_escola[banco_ESCOLA_tipo_escola$tipo_escola == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_ESCOLA_tipo_escola$tipo_escola[banco_ESCOLA_tipo_escola$tipo_escola == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(banco_ESCOLA_tipo_escola)[1]<-'ESCOLA'
colnames(banco_ESCOLA_tipo_escola)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_tipo_escola)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ESCOLA_tipo_escola FIM
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_ESCOLA_regional_ato =
  banco_ESCOLA_sem_concurso %>%
  select(REGIONAL_ATO)

colnames(banco_ESCOLA_regional_ato)[1]<-'regional_ato'

#########################################################################################################
#encontrando parte do texto e substituindo
banco_ESCOLA_regional_ato$regional_ato[agrep("/MG", banco_ESCOLA_regional_ato$regional_ato)] <- "UOUTRA CIDADE MG"
banco_ESCOLA_regional_ato$regional_ato[agrep("RMBH", banco_ESCOLA_regional_ato$regional_ato)] <- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_ato$regional_ato[agrep("RIBEIRAO DAS NEVES", banco_ESCOLA_regional_ato$regional_ato)] <- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_ato$regional_ato[agrep("CATAGUASES", banco_ESCOLA_regional_ato$regional_ato)] <- "UOUTRA CIDADE MG"
banco_ESCOLA_regional_ato$regional_ato[agrep("CIDADE DE BRASILIA/DF", banco_ESCOLA_regional_ato$regional_ato)] <- "VOUTRO ESTADO"
banco_ESCOLA_regional_ato$regional_ato[agrep("N/DISP", banco_ESCOLA_regional_ato$regional_ato)] <- "ZSEM INFORMAÇÃO"
banco_ESCOLA_regional_ato$regional_ato[agrep("INFORMACAO", banco_ESCOLA_regional_ato$regional_ato)] <- "ZSEM INFORMAÇÃO"
#substituindo
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == ""]<- "ZSEM INFORMAÇÃO"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "PAMPULHA"]<- "OESTEPAMPULHA"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "VENDA NOVA"]<- "PVENDA NOVA"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "REGIÃO METROPOLITANA"]<- "QREGIÃO METROPOLITANA"

#########################################################################################################

# salvando para gráfico
banco_ESCOLA_regional_ato_bkp = banco_ESCOLA_regional_ato

banco_ESCOLA_regional_ato_bkp =
  banco_ESCOLA_regional_ato_bkp %>%
  janitor::tabyl(regional_ato) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "PVENDA NOVA"]<- "VENDA NOVA"
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_ESCOLA_regional_ato_bkp$regional_ato[banco_ESCOLA_regional_ato_bkp$regional_ato == "VOUTRO ESTADO"]<- "OUTRO ESTADO"

colnames(banco_ESCOLA_regional_ato_bkp)[1]<-'banco_ESCOLA_regional_ato_bkp'
colnames(banco_ESCOLA_regional_ato_bkp)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_regional_ato_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_ESCOLA_regional_ato_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_ESCOLA_regional_ato_bkp$PERCENTUAL))
banco_ESCOLA_regional_ato_bkp_rmd = tail(banco_ESCOLA_regional_ato_bkp,5)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_ESCOLA_regional_ato =
  banco_ESCOLA_regional_ato %>%
  janitor::tabyl(regional_ato) %>%
  arrange(regional_ato) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "OESTEPAMPULHA"]<- "PAMPULHA"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "PVENDA NOVA"]<- "VENDA NOVA"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "QREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "UOUTRA CIDADE MG"]<- "OUTRA CIDADE MG"
banco_ESCOLA_regional_ato$regional_ato[banco_ESCOLA_regional_ato$regional_ato == "VOUTRO ESTADO"]<- "OUTRO ESTADO"



colnames(banco_ESCOLA_regional_ato)[1]<-'REGIONAL'
colnames(banco_ESCOLA_regional_ato)[2]<-'QUANTIDADE'
colnames(banco_ESCOLA_regional_ato)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_ESCOLA_regional_ato FIM
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#FIM
#########################################################################################################

