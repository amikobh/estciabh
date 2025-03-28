#########################################################################################################
dir.create(file.path("~/diretorio_r/estciabh/corre", "planilhas"))
dir.create(file.path("~/diretorio_r/estciabh/corre", "bancos"))
dir.create(file.path("~/diretorio_r/estciabh/corre/planilhas", "comissarias"))
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/corre/bancos"))#configurar diretorio
#########################################################################################################
# importando os bancos

painel <- read.csv("painel.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
banco_semiliberdade <- read.csv("semiliberdade.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
banco_internacao <- read.csv("internacao.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
#banco <- read.csv("banco_atual.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 3) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

#########################################################################################################

# acertando os nomes das variáveis de todos o bancos

painel =
  painel %>%
  clean_names()


banco_semiliberdade =
  banco_semiliberdade %>%
  clean_names()

banco_internacao =
  banco_internacao %>%
  clean_names()

#################################################################################################################
#ESTE BANCO servirá para o tratamento internação e semi
#################################################################################################################

#selecionando colunas do banco painel

painel_tratado = painel |> select(1,3)

#tratando banco painel
painel_tratado <- painel_tratado %>%
  rename(IDS = id_do_adolescente)

#tratando coluna id do banco painel
painel_tratado$IDS = as.factor(painel_tratado$IDS)

#excluindo repetidos
painel_tratado  <- painel_tratado %>% distinct(IDS, nome )

#################################################################################################################
#TRATAMENTO banco_semiliberdade
#################################################################################################################


# formatando campos data_da_atividade e hora_da_atividade:

banco_semiliberdade$data_da_atividade = as.Date(banco_semiliberdade$data_da_atividade)

banco_semiliberdade$hora_da_atividade = strptime(banco_semiliberdade$hora_da_atividade, format = "%H:%M")

banco_semiliberdade$hora_da_atividade <- format(banco_semiliberdade$hora_da_atividade, "%H:%M")

names(banco_semiliberdade)



banco_semiliberdade <- banco_semiliberdade %>%
  rename(NOMES_IDS = nomes_ids)

banco_semiliberdade$carimbo_de_data_hora = NULL


# Criando as novas colunas e separando os IDs em linhas separadas
df_new_semiliberdade <- banco_semiliberdade %>%
  mutate(
    INICIAIS = str_extract_all(NOMES_IDS, "\\D+") %>% map_chr(toString, collapse = ", "),
    IDS = str_extract_all(NOMES_IDS, "\\d+")
  ) %>%
  unnest(IDS)


#################################################################################################################
# Normalizar a coluna 'atividade2' (remover espaços extras).
#str_squish(atividade2): A função str_squish() remove os espaços extras e os múltiplos


df_new_semiliberdade <- df_new_semiliberdade %>%
  mutate(atividade2 = str_squish(atividade),    # Remove espaços extras
         atividade2 = toupper(atividade2))      # Converte para maiúsculas
#################################################################################################################

#################################################################################################################
#Salvando para a equipe verificar:

setwd(file.path("~/diretorio_r/estciabh/corre/planilhas/comissarias/"))#configurar diretorio
#################################################################################################################
# Remover a coluna
df_new_semiliberdade <- df_new_semiliberdade %>% select(-nome)

# Fazendo a junção com dplyr
semiliberdade_final <- df_new_semiliberdade %>%
  left_join(painel_tratado, by = "IDS") %>%
  mutate(NOME = ifelse(is.na(nome), "SEM CORRESPONDÊNCIA", nome)) %>%
  select(IDS, NOME, everything(), -nome)

#para Sandra e Vanessa verificarem os sem correspondência
# Exportando para um arquivo CSV


write.csv(semiliberdade_final, "semiliberdade.csv", row.names = FALSE)


sem_correspondencia_semi = semiliberdade_final |>
  filter(NOME %in% "SEM CORRESPONDÊNCIA")

# Exportando para um arquivo CSV
write.csv(sem_correspondencia_semi, "sem_correspondencia_semi.csv", row.names = FALSE)

#########################################################################################################

library(dplyr)

semiliberdade_sem_duplicadas <- semiliberdade_final %>%
  distinct(IDS, NOME, unidade, atividade2, data_da_atividade, hora_da_atividade, .keep_all = TRUE)



#arrumando coluna atividade2 semiliberdade_sem_duplicadas

# Carregar o pacote dplyr
library(dplyr)


# Usando base R para procurar varios termos ou palavra
semiliberdade_sem_aulas <- semiliberdade_sem_duplicadas[grepl("AUDIÊNCIA CONCENTRADA|SEM TREINO|OBS|NÃO TEREMOS", semiliberdade_sem_duplicadas$atividade2), ]

semiliberdade_sem_duplicadas <- semiliberdade_sem_duplicadas[!grepl("AUDIÊNCIA CONCENTRADA|SEM TREINO|OBS|NÃO TEREMOS", semiliberdade_sem_duplicadas$atividade2), ]




library(fuzzyjoin)
library(dplyr)

# Criando um dataframe com o valor correto
correcao_semiliberdade <- data.frame(
  atividade_correta = "CORRIDA NA RUA"
)

# Juntando os dataframes com base em similaridade
semiliberdade_sem_duplicadas <- stringdist_left_join(semiliberdade_sem_duplicadas, correcao_semiliberdade, by = c("atividade2" = "atividade_correta"), method = "jw", max_dist = 0.2) %>%
  mutate(atividade2 = ifelse(is.na(atividade_correta), atividade2, atividade_correta)) %>%
  select(-atividade_correta)


# Substituindo as células que contêm "CTE" por "TREINO CTE"

# Substituir todas as ocorrências de 'CTE' por 'TREINO CTE' na coluna 'atividade2'
# Carregar os pacotes necessários
library(dplyr)
library(stringr)


semiliberdade_sem_duplicadas <- semiliberdade_sem_duplicadas %>%
  mutate(atividade2 = if_else(str_detect(atividade2, "CTE"), "TREINO CTE", atividade2))

#################################################################################################################
#TRATAMENTO banco_semiliberdade
#################################################################################################################
#atv = quantas vezes um mesmo adolecente participou das atividades?

# Contar as ocorrências de cada valor único
semiliberdade_atv <- semiliberdade_sem_duplicadas %>%
  count(IDS)

# Criar faixas com base nas repetições
semiliberdade_atv <- semiliberdade_atv %>%
  mutate(faixa = case_when(
    n < 10 ~ "Menos de 10 vezes",
    n >= 10 & n <= 20 ~ "Entre 10 e 20 vezes",
    n >= 21 & n <= 30 ~ "Entre 21 e 30 vezes",
    n > 30 ~ "Acima de 30 vezes"
  ))

# Contar quantas vezes cada faixa ocorre
faixa_semiliberdade_atv <- semiliberdade_atv %>%
  count(faixa) %>%
  arrange(factor(faixa, levels = c("Menos de 10 vezes", "Entre 10 e 20 vezes", "Entre 21 e 30 vezes", "Acima de 30 vezes")))


# Adicionar a linha de total
faixa_semiliberdade_atv <- faixa_semiliberdade_atv %>%
  bind_rows(
    data.frame(faixa = "Total", n = sum(faixa_semiliberdade_atv$n))
  )


#print(faixa_semiliberdade_atv)

# Contagem das atividades
tabela_atividades_semiliberdade <- table(semiliberdade_sem_duplicadas$atividade2)

# Converter para um data frame
tabela_atividades_semiliberdade <- as.data.frame(tabela_atividades_semiliberdade)
colnames(tabela_atividades_semiliberdade) <- c("Atividade", "Frequencia")

# Calcular porcentagem (arredondada para 2 casas decimais)
tabela_atividades_semiliberdade$Porcentagem <- round((tabela_atividades_semiliberdade$Frequencia / sum(tabela_atividades_semiliberdade$Frequencia)) * 100, 2)

# Adicionar linha de total
total_atividades <- data.frame(
  Atividade = "Total",
  Frequencia = sum(tabela_atividades_semiliberdade$Frequencia),
  Porcentagem = 100.00  # Total sempre será 100%
)

# Combinar tabelas
tabela_atividades_semiliberdade <- rbind(tabela_atividades_semiliberdade, total_atividades)

# Exibir tabela
#print(tabela_atividades_semiliberdade)


# Criar coluna com os dias da semana
semiliberdade_sem_duplicadas$dias_da_semana <- weekdays(as.Date(semiliberdade_sem_duplicadas$data_da_atividade))

# Contagem dos dias da semana
tabela_dias_semiliberdade <- table(semiliberdade_sem_duplicadas$dias_da_semana)

# Converter para um data frame
tabela_dias_semiliberdade <- as.data.frame(tabela_dias_semiliberdade)
colnames(tabela_dias_semiliberdade) <- c("Dia_da_Semana", "Frequencia")

# Calcular porcentagem (arredondada para 2 casas decimais)
tabela_dias_semiliberdade$Porcentagem <- round((tabela_dias_semiliberdade$Frequencia / sum(tabela_dias_semiliberdade$Frequencia)) * 100, 2)

# Adicionar linha de total
total_dias <- data.frame(
  Dia_da_Semana = "Total",
  Frequencia = sum(tabela_dias_semiliberdade$Frequencia),
  Porcentagem = 100.00  # Total sempre será 100%
)

# Combinar tabelas
tabela_dias_semiliberdade <- rbind(tabela_dias_semiliberdade, total_dias)

# Exibir tabela
#print(tabela_dias_semiliberdade)



# Criar coluna com o período do dia, tratando NAs
semiliberdade_sem_duplicadas$periodo_do_dia <- ifelse(
  is.na(semiliberdade_sem_duplicadas$hora_da_atividade),
  "S/INF",
  ifelse(as.numeric(substr(semiliberdade_sem_duplicadas$hora_da_atividade, 1, 2)) < 12, "MANHÃ", "TARDE")
)

# Contagem dos períodos
tabela_periodo_semiliberdade <- table(semiliberdade_sem_duplicadas$periodo_do_dia)

# Converter para um data frame
tabela_periodo_semiliberdade <- as.data.frame(tabela_periodo_semiliberdade)
colnames(tabela_periodo_semiliberdade) <- c("Periodo", "Frequencia")

# Calcular porcentagem (arredondada para 2 casas decimais)
tabela_periodo_semiliberdade$Porcentagem <- round((tabela_periodo_semiliberdade$Frequencia / sum(tabela_periodo_semiliberdade$Frequencia)) * 100, 2)

# Reordenar as linhas: colocar "S/ INFORMAÇÃO" logo antes do total
#if ("S/INF" %in% tabela_periodo_semiliberdade$Periodo) {
# tabela_periodo_semiliberdade <- tabela_periodo_semiliberdade[order(tabela_periodo_semiliberdade$Periodo != "S/INF"), ]
#}

# Reordenar as linhas: colocar "MANHÃ" e "TARDE" primeiro, depois "S/INF"
tabela_periodo_semiliberdade <- tabela_periodo_semiliberdade[order(factor(tabela_periodo_semiliberdade$Periodo, levels = c("MANHÃ", "TARDE", "S/INF"))), ]

# Adicionar linha de total
total_periodo_semiliberdade <- data.frame(
  Periodo = "Total",
  Frequencia = sum(tabela_periodo_semiliberdade$Frequencia),
  Porcentagem = 100.00  # Total sempre será 100%
)

# Combinar tabelas
tabela_periodo_semiliberdade <- rbind(tabela_periodo_semiliberdade, total_periodo_semiliberdade)

# Exibir tabela
#print(tabela_periodo_semiliberdade)

# Contagem das unidades
tabela_unidade_semiliberdade <- table(semiliberdade_sem_duplicadas$unidade)

# Converter para um data frame
tabela_unidade_semiliberdade <- as.data.frame(tabela_unidade_semiliberdade)
colnames(tabela_unidade_semiliberdade) <- c("Unidade", "Frequencia")

# Calcular porcentagem (arredondada para 2 casas decimais)
tabela_unidade_semiliberdade$Porcentagem <- round((tabela_unidade_semiliberdade$Frequencia / sum(tabela_unidade_semiliberdade$Frequencia)) * 100, 2)

# Adicionar linha de total
total_unidade <- data.frame(
  Unidade = "Total",
  Frequencia = sum(tabela_unidade_semiliberdade$Frequencia),
  Porcentagem = 100.00  # Total sempre será 100%
)

# Combinar tabelas
tabela_unidade_semiliberdade <- rbind(tabela_unidade_semiliberdade, total_unidade)

# Exibir tabela
#print(tabela_unidade_semiliberdade)


semiliberdade_SNR = semiliberdade_sem_duplicadas %>%
  distinct(IDS, .keep_all = TRUE)

#########################################################################################################
#########################################################################################################
#################################################################################################################
#TRATAMENTO banco_internacao
#################################################################################################################
banco_internacao <- banco_internacao %>%
  rename(NOMES_IDS = nomes_ids)

banco_internacao$carimbo_de_data_hora = NULL


# Criando as novas colunas e separando os IDs em linhas separadas
df_new_internacao <- banco_internacao %>%
  mutate(
    INICIAIS = str_extract_all(NOMES_IDS, "\\D+") %>% map_chr(toString, collapse = ", "),
    IDS = str_extract_all(NOMES_IDS, "\\d+")
  ) %>%
  unnest(IDS)

#########################################################################################################
#################################################################################################################
#Salvando para a equipe verificar:

setwd(file.path("~/diretorio_r/estciabh/corre/planilhas/comissarias/"))#configurar diretorio
#################################################################################################################


df_new_internacao <- df_new_internacao %>%
  mutate(atividade2 = str_squish(atividade),    # Remove espaços extras
         atividade2 = toupper(atividade2))      # Converte para maiúsculas


# Fazendo a junção com dplyr
# Remover a coluna
df_new_internacao <- df_new_internacao %>% select(-nome)

internacao_final <- df_new_internacao %>%
  left_join(painel_tratado, by = "IDS") %>%
  mutate(NOME = ifelse(is.na(nome), "SEM CORRESPONDÊNCIA", nome)) %>%
  select(IDS, NOME, everything(), -nome)



# Exportando para um arquivo CSV
write.csv(internacao_final, "internacao.csv", row.names = FALSE)


sem_correspondencia_int = internacao_final |>
  filter(NOME %in% "SEM CORRESPONDÊNCIA")

# Exportando para um arquivo CSV
write.csv(sem_correspondencia_int, "sem_correspondencia_int.csv", row.names = FALSE)


internacao_sem_duplicadas <- internacao_final %>%
  distinct(IDS, NOME, unidade, atividade2, data_da_atividade, hora_da_atividade, .keep_all = TRUE)

#arrumando coluna atividade2 internacao_sem_duplicadas

# Carregar o pacote dplyr
library(dplyr)

# Usando base R para procurar varios termos ou palavra
internacao_sem_aulas <- internacao_sem_duplicadas[grepl("AUDIÊNCIA CONCENTRADA|SEM TREINO|OBS|NÃO TEREMOS", internacao_sem_duplicadas$atividade2), ]

internacao_sem_duplicadas <- internacao_sem_duplicadas[!grepl("AUDIÊNCIA CONCENTRADA|SEM TREINO|OBS|NÃO TEREMOS", internacao_sem_duplicadas$atividade2), ]

library(fuzzyjoin)
library(dplyr)

# Criando um dataframe com o valor correto
correcao_internacao <- data.frame(
  atividade_correta = "CORRIDA NA RUA"
)

# Juntando os dataframes com base em similaridade
internacao_sem_duplicadas <- stringdist_left_join(internacao_sem_duplicadas, correcao_internacao, by = c("atividade2" = "atividade_correta"), method = "jw", max_dist = 0.2) %>%
  mutate(atividade2 = ifelse(is.na(atividade_correta), atividade2, atividade_correta)) %>%
  select(-atividade_correta)


# Substituindo as células que contêm "CTE" por "TREINO CTE"

# Substituir todas as ocorrências de 'CTE' por 'TREINO CTE' na coluna 'atividade2'
# Carregar os pacotes necessários
library(dplyr)
library(stringr)


internacao_sem_duplicadas <- internacao_sem_duplicadas %>%
  mutate(atividade2 = if_else(str_detect(atividade2, "CTE"), "TREINO CTE", atividade2))

#########################################################################################################
#########################################################################################################

#TRABALHANDO O BANCO internacao:

#atv = quantas vezes um mesmo adolecente participou das atividades?

# Contar as ocorrências de cada valor único
internacao_atv <- internacao_sem_duplicadas %>%
  count(IDS)

# Criar faixas com base nas repetições
internacao_atv <- internacao_atv %>%
  mutate(faixa = case_when(
    n < 10 ~ "Menos de 10 vezes",
    n >= 10 & n <= 20 ~ "Entre 11 e 20 vezes",
    n >= 21 & n <= 30 ~ "Entre 21 e 30 vezes",
    n > 30 ~ "Acima de 30 vezes"
  ))

# Contar quantas vezes cada faixa ocorre
faixa_internacao_atv <- internacao_atv %>%
  count(faixa) %>%
  arrange(factor(faixa, levels = c("Menos de 10 vezes", "Entre 10 e 20 vezes", "Entre 21 e 30 vezes", "Acima de 30 vezes")))


# Adicionar a linha de total
faixa_internacao_atv <- faixa_internacao_atv %>%
  bind_rows(
    data.frame(faixa = "Total", n = sum(faixa_internacao_atv$n))
  )


#print(faixa_internacao_atv)

# Contagem das atividades
tabela_atividades_internacao <- table(internacao_sem_duplicadas$atividade2)

# Converter para um data frame
tabela_atividades_internacao <- as.data.frame(tabela_atividades_internacao)
colnames(tabela_atividades_internacao) <- c("Atividade", "Frequencia")

# Calcular porcentagem (arredondada para 2 casas decimais)
tabela_atividades_internacao$Porcentagem <- round((tabela_atividades_internacao$Frequencia / sum(tabela_atividades_internacao$Frequencia)) * 100, 2)

# Adicionar linha de total
total_atividades <- data.frame(
  Atividade = "Total",
  Frequencia = sum(tabela_atividades_internacao$Frequencia),
  Porcentagem = 100.00  # Total sempre será 100%
)

# Combinar tabelas
tabela_atividades_internacao <- rbind(tabela_atividades_internacao, total_atividades)

# Exibir tabela
#print(tabela_atividades_internacao)


# Criar coluna com os dias da semana
internacao_sem_duplicadas$dias_da_semana <- weekdays(as.Date(internacao_sem_duplicadas$data_da_atividade))

# Contagem dos dias da semana
tabela_dias_internacao <- table(internacao_sem_duplicadas$dias_da_semana)

# Converter para um data frame
tabela_dias_internacao <- as.data.frame(tabela_dias_internacao)
colnames(tabela_dias_internacao) <- c("Dia_da_Semana", "Frequencia")

# Calcular porcentagem (arredondada para 2 casas decimais)
tabela_dias_internacao$Porcentagem <- round((tabela_dias_internacao$Frequencia / sum(tabela_dias_internacao$Frequencia)) * 100, 2)

# Adicionar linha de total
total_dias <- data.frame(
  Dia_da_Semana = "Total",
  Frequencia = sum(tabela_dias_internacao$Frequencia),
  Porcentagem = 100.00  # Total sempre será 100%
)

# Combinar tabelas
tabela_dias_internacao <- rbind(tabela_dias_internacao, total_dias)

# Exibir tabela
#print(tabela_dias_internacao)



# Criar coluna com o período do dia, tratando NAs
internacao_sem_duplicadas$periodo_do_dia <- ifelse(
  is.na(internacao_sem_duplicadas$hora_da_atividade),
  "S/INF",
  ifelse(as.numeric(substr(internacao_sem_duplicadas$hora_da_atividade, 1, 2)) < 12, "MANHÃ", "TARDE")
)

# Contagem dos períodos
tabela_periodo_internacao <- table(internacao_sem_duplicadas$periodo_do_dia)

# Converter para um data frame
tabela_periodo_internacao <- as.data.frame(tabela_periodo_internacao)
colnames(tabela_periodo_internacao) <- c("Periodo", "Frequencia")

# Calcular porcentagem (arredondada para 2 casas decimais)
tabela_periodo_internacao$Porcentagem <- round((tabela_periodo_internacao$Frequencia / sum(tabela_periodo_internacao$Frequencia)) * 100, 2)

# Reordenar as linhas: colocar "S/ INFORMAÇÃO" logo antes do total
#if ("S/INF" %in% tabela_periodo_internacao$Periodo) {
# tabela_periodo_internacao <- tabela_periodo_internacao[order(tabela_periodo_internacao$Periodo != "S/INF"), ]
#}

# Reordenar as linhas: colocar "MANHÃ" e "TARDE" primeiro, depois "S/INF"
tabela_periodo_internacao <- tabela_periodo_internacao[order(factor(tabela_periodo_internacao$Periodo, levels = c("MANHÃ", "TARDE", "S/INF"))), ]

# Adicionar linha de total
total_periodo_internacao <- data.frame(
  Periodo = "Total",
  Frequencia = sum(tabela_periodo_internacao$Frequencia),
  Porcentagem = 100.00  # Total sempre será 100%
)

# Combinar tabelas
tabela_periodo_internacao <- rbind(tabela_periodo_internacao, total_periodo_internacao)

# Exibir tabela
#print(tabela_periodo_internacao)



# Contagem das unidades
tabela_unidade_internacao <- table(internacao_sem_duplicadas$unidade)

# Converter para um data frame
tabela_unidade_internacao <- as.data.frame(tabela_unidade_internacao)
colnames(tabela_unidade_internacao) <- c("Unidade", "Frequencia")

# Calcular porcentagem (arredondada para 2 casas decimais)
tabela_unidade_internacao$Porcentagem <- round((tabela_unidade_internacao$Frequencia / sum(tabela_unidade_internacao$Frequencia)) * 100, 2)

# Adicionar linha de total
total_unidade <- data.frame(
  Unidade = "Total",
  Frequencia = sum(tabela_unidade_internacao$Frequencia),
  Porcentagem = 100.00  # Total sempre será 100%
)

# Combinar tabelas
tabela_unidade_internacao <- rbind(tabela_unidade_internacao, total_unidade)


# Exibir tabela
#print(tabela_unidade_internacao)


internacao_SNR =  internacao_sem_duplicadas %>%
  distinct(IDS, .keep_all = TRUE)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#juntando semiliberdade
#########################################################################################################
# Carregar o pacote data.table
library(data.table)
banco_atos_em_foco_corre = banco_atos_em_foco

banco_atos_em_foco_corre <- banco_atos_em_foco_corre %>%
  rename(IDS = ID)

# Converter os dataframes para data.table
setDT(semiliberdade_SNR)
setDT(banco_atos_em_foco_corre)

# Realizar a junção
semiliberdade_GERAL <- merge(
  semiliberdade_SNR[, .(IDS, NOME)],  # Seleciona as colunas IDS e NOME do primeiro dataframe
  banco_atos_em_foco_corre[, .(IDS, SEXO, ESCOLARIDADE_TERMO, DATA_ATO, IDADE, PROCESSO,
                               ATO_INFRACIONAL_ATA_01, ATO_INFRACIONAL_ATA_02, ATO_INFRACIONAL_ATA_03,
                               MEDIDA_PROTETIVA, QUAL_MEDIDA_PROTETIVA_01, QUAL_MEDIDA_PROTETIVA_02,
                               QUAL_MEDIDA_PROTETIVA_03, QUAL_MEDIDA_PROTETIVA_04, QUAL_MEDIDA_PROTETIVA_05,
                               QUAL_MEDIDA_PROTETIVA_06, REGIONAL_RESIDENCIAL, REGIONAL_ATO)],  # Seleciona as colunas relevantes do segundo dataframe
  by = "IDS",  # Junção pela coluna IDS
  all.x = TRUE  # Junção à esquerda, ou seja, mantém todas as linhas do semiliberdade_SNR
)


# Substituir todos os NAs no dataframe por "SEM INFORMACAO2"
semiliberdade_GERAL <- semiliberdade_GERAL %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "SEM INFORMACAO2", .)))


# Carregar o pacote dplyr
library(dplyr)

semiliberdade_GERAL_SNR = semiliberdade_GERAL %>%
  distinct(IDS, .keep_all = TRUE)


# Excluir linhas onde SEXO == "SEM INFORMACAO2"
semiliberdade_GERAL <- semiliberdade_GERAL %>%
  filter(SEXO != "SEM INFORMACAO2")


#########################################################################################################
#TRATAMENTO INCIDENCIA:
#########################################################################################################
#########################################################################################################
#########################################################################################################

semiliberdade_GERAL$ATO_INFRACIONAL_ATA_01 <- gsub(" ","", semiliberdade_GERAL$ATO_INFRACIONAL_ATA_01)
semiliberdade_GERAL$ATO_INFRACIONAL_ATA_02 <- gsub(" ","", semiliberdade_GERAL$ATO_INFRACIONAL_ATA_02)
semiliberdade_GERAL$ATO_INFRACIONAL_ATA_03 <- gsub(" ","", semiliberdade_GERAL$ATO_INFRACIONAL_ATA_03)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_incidencia_semiliberdade_GERAL =

  semiliberdade_GERAL %>%
  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") |>
  #select(-name) %>%
  filter(!ATO_INFRACIONAL %in% "NSA" & !ATO_INFRACIONAL %in% "TERMOSEMINF." & !ATO_INFRACIONAL %in% "VAZIO")

#########################################################################################################
#########################################################################################################
#substituição especifica de artigo deve anteceder a genérica: vide as primeiras linhas e latrocinio e roubo:

banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("28.ART11.*.*", "POSSE DE DROGAS PARA USO PESSOAL", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("34.ART11.*.*", "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("35.ART11.*.*", "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub(".*ART11.343.*.*", "TRÁFICO DE DROGAS", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub(".*ART10.826.*.*", "PORTE/POSSE DE ARMA", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub(".*ARTCTB.*.*", "CRIME DE TRÂNSITO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("121.ART.*.*", "HOMICÍDIO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("121C/C14.*.*", "HOMICÍDIO (TENTATIVA)", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("129.*.*", "LESÃO CORPORAL", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("129§.*.*", "LESÃO CORPORAL", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("137.*.*", "RIXA", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("140.ART.*.*", "INJÚRIA", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("140§*.*", "INJÚRIA", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("147.*.*", "AMEAÇA", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("148.ART.*.*", "SEQUESTRO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("155.ART.*.*", "FURTO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("155C/C.*.*", "FURTO (TENTATIVA)", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("157.ART.*.*", "ROUBO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("157C/C.*.*", "ROUBO (TENTATIVA)", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("157§3.*.*", "LATROCÍNIO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("157§.*.*", "ROUBO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("163.ART.*.*", "DANO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("171.ART.*.*", "ESTELIONATO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("180.ART.*.*", "RECEPTAÇÃO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
#banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("19.ART.*.*", "PORTE ARMA (LCP)", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("21.ART.*.*", "VIAS DE FATO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("213.ART.*.*", "ESTUPRO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("215.ART.*.*", "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("215-A.*.*", "IMPORTUNAÇÃO SEXUAL", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("217-A.*.*", "ESTUPRO DE VULNERÁVEL", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
#banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("311.ARTCPB.*.*", "ADULTERAÇÃO DE SINAL IDENTIFICADOR DE VEÍCULO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("329.ART.*.*", "RESISTÊNCIA", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("330.ART.*.*", "DESOBEDIÊNCIA", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("331.ART.*.*", "DESACATO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub("65.ART9.*.*", "PICHAÇÃO", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)

#substituindo os restantes em outros
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub(".*OUTROS.*.*", "VOUTROS", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)
banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL = sub(".*ART.*.*", "VOUTROS", banco_incidencia_semiliberdade_GERAL$ATO_INFRACIONAL)

#########################################################################################################
banco_geral_sem_concurso_semiliberdade = distinct(banco_incidencia_semiliberdade_GERAL, DATA_ATO, PROCESSO, ATO_INFRACIONAL, .keep_all= TRUE)
#########################################################################################################
banco_geral_semiliberdade_SNR = distinct(banco_incidencia_semiliberdade_GERAL, IDS, .keep_all= TRUE)
#########################################################################################################
#########################################################################################################
#INCIDENCIA semiliberdade
#########################################################################################################
library(dplyr)

banco_geral_sem_concurso_semiliberdade = banco_geral_sem_concurso_semiliberdade %>%
  select(ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
###banco_geral_sem_concurso_semiliberdade
#########################################################################################################
#setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_geral_sem_concurso_semiliberdade_bkp = banco_geral_sem_concurso_semiliberdade

banco_geral_sem_concurso_semiliberdade_bkp =
  banco_geral_sem_concurso_semiliberdade_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(banco_geral_sem_concurso_semiliberdade_bkp)[1]<-'banco_geral_sem_concurso_semiliberdade_bkp'
colnames(banco_geral_sem_concurso_semiliberdade_bkp)[2]<-'QUANTIDADE'
colnames(banco_geral_sem_concurso_semiliberdade_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#para script rmd:
banco_geral_sem_concurso_semiliberdade_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_geral_sem_concurso_semiliberdade_bkp$PERCENTUAL))
banco_geral_sem_concurso_semiliberdade_bkp_rmd = tail(banco_geral_sem_concurso_semiliberdade_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_geral_sem_concurso_semiliberdade =
  banco_geral_sem_concurso_semiliberdade %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(ATO_INFRACIONAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

colnames(banco_geral_sem_concurso_semiliberdade)[1]<-'ATO'
colnames(banco_geral_sem_concurso_semiliberdade)[2]<-'QUANTIDADE'
colnames(banco_geral_sem_concurso_semiliberdade)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
# Criar tabela de frequências cruzadas entre IDADE e SEXO
tabela_IDADE_SEXO_semiliberdade <- semiliberdade_GERAL_SNR %>%
  tabyl(IDADE, SEXO) %>%
  adorn_totals(c("row", "col")) %>% # Adicionar totais nas linhas e colunas
  adorn_percentages("col") %>% # Calcular porcentagens por coluna (SEXO)
  adorn_pct_formatting(digits = 2) %>% # Formatar porcentagens com 2 casas decimais
  adorn_ns() %>% # Adicionar contagens absolutas entre parênteses
  mutate(IDADE = ifelse(IDADE == "Total", "Total", paste0(IDADE, " anos"))) # Adicionar "anos" à coluna IDADE


# Substituir "VS/INFORMAÇÃO" por "S/INFORMAÇÃO" na coluna 'ato'
tabela_IDADE_SEXO_semiliberdade <- tabela_IDADE_SEXO_semiliberdade %>%
  mutate(IDADE = gsub("Total", "TOTAL", IDADE))


# Modificar os nomes das colunas
colnames(tabela_IDADE_SEXO_semiliberdade) <- c("IDADE", "FEMININO", "MASCULINO", "TOTAL")

# Exibir a tabela
print(tabela_IDADE_SEXO_semiliberdade)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#juntando internacao
#########################################################################################################
# Carregar o pacote data.table
library(data.table)

# Converter os dataframes para data.table
setDT(internacao_SNR)
setDT(banco_atos_em_foco_corre)

# Realizar a junção
internacao_GERAL <- merge(
  internacao_SNR[, .(IDS, NOME)],  # Seleciona as colunas IDS e NOME do primeiro dataframe
  banco_atos_em_foco_corre[, .(IDS, SEXO, ESCOLARIDADE_TERMO, DATA_ATO, IDADE, PROCESSO,
                               ATO_INFRACIONAL_ATA_01, ATO_INFRACIONAL_ATA_02, ATO_INFRACIONAL_ATA_03,
                               MEDIDA_PROTETIVA, QUAL_MEDIDA_PROTETIVA_01, QUAL_MEDIDA_PROTETIVA_02,
                               QUAL_MEDIDA_PROTETIVA_03, QUAL_MEDIDA_PROTETIVA_04, QUAL_MEDIDA_PROTETIVA_05,
                               QUAL_MEDIDA_PROTETIVA_06, REGIONAL_RESIDENCIAL, REGIONAL_ATO)],  # Seleciona as colunas relevantes do segundo dataframe
  by = "IDS",  # Junção pela coluna IDS
  all.x = TRUE  # Junção à esquerda, ou seja, mantém todas as linhas do internacao_SNR
)


# Substituir todos os NAs no dataframe por "SEM INFORMACAO2"
internacao_GERAL <- internacao_GERAL %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "SEM INFORMACAO2", .)))


# Carregar o pacote dplyr
library(dplyr)


internacao_GERAL_SNR = internacao_GERAL %>%
  distinct(IDS, .keep_all = TRUE)

# Excluir linhas onde SEXO == "SEM INFORMACAO2"
internacao_GERAL <- internacao_GERAL %>%
  filter(SEXO != "SEM INFORMACAO2")


#########################################################################################################
#TRATAMENTO INCIDENCIA:
#########################################################################################################
#########################################################################################################
#########################################################################################################

internacao_GERAL$ATO_INFRACIONAL_ATA_01 <- gsub(" ","", internacao_GERAL$ATO_INFRACIONAL_ATA_01)
internacao_GERAL$ATO_INFRACIONAL_ATA_02 <- gsub(" ","", internacao_GERAL$ATO_INFRACIONAL_ATA_02)
internacao_GERAL$ATO_INFRACIONAL_ATA_03 <- gsub(" ","", internacao_GERAL$ATO_INFRACIONAL_ATA_03)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_incidencia_internacao_GERAL =

  internacao_GERAL %>%
  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") |>
  #select(-name) %>%
  filter(!ATO_INFRACIONAL %in% "NSA" & !ATO_INFRACIONAL %in% "TERMOSEMINF." & !ATO_INFRACIONAL %in% "VAZIO")

#########################################################################################################
#########################################################################################################
#substituição especifica de artigo deve anteceder a genérica: vide as primeiras linhas e latrocinio e roubo:

banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("28.ART11.*.*", "POSSE DE DROGAS PARA USO PESSOAL", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("34.ART11.*.*", "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("35.ART11.*.*", "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub(".*ART11.343.*.*", "TRÁFICO DE DROGAS", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub(".*ART10.826.*.*", "PORTE/POSSE DE ARMA", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub(".*ARTCTB.*.*", "CRIME DE TRÂNSITO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("121.ART.*.*", "HOMICÍDIO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("121C/C14.*.*", "HOMICÍDIO (TENTATIVA)", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("129.*.*", "LESÃO CORPORAL", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("129§.*.*", "LESÃO CORPORAL", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("137.*.*", "RIXA", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("140.ART.*.*", "INJÚRIA", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("140§*.*", "INJÚRIA", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("147.*.*", "AMEAÇA", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("148.ART.*.*", "SEQUESTRO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("155.ART.*.*", "FURTO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("155C/C.*.*", "FURTO (TENTATIVA)", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("157.ART.*.*", "ROUBO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("157C/C.*.*", "ROUBO (TENTATIVA)", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("157§3.*.*", "LATROCÍNIO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("157§.*.*", "ROUBO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("163.ART.*.*", "DANO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("171.ART.*.*", "ESTELIONATO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("180.ART.*.*", "RECEPTAÇÃO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
#banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("19.ART.*.*", "PORTE ARMA (LCP)", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("21.ART.*.*", "VIAS DE FATO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("213.ART.*.*", "ESTUPRO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("215.ART.*.*", "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("215-A.*.*", "IMPORTUNAÇÃO SEXUAL", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("217-A.*.*", "ESTUPRO DE VULNERÁVEL", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
#banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("311.ARTCPB.*.*", "ADULTERAÇÃO DE SINAL IDENTIFICADOR DE VEÍCULO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("329.ART.*.*", "RESISTÊNCIA", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("330.ART.*.*", "DESOBEDIÊNCIA", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("331.ART.*.*", "DESACATO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub("65.ART9.*.*", "PICHAÇÃO", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)

#substituindo os restantes em outros
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub(".*OUTROS.*.*", "VOUTROS", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)
banco_incidencia_internacao_GERAL$ATO_INFRACIONAL = sub(".*ART.*.*", "VOUTROS", banco_incidencia_internacao_GERAL$ATO_INFRACIONAL)

#########################################################################################################
banco_geral_sem_concurso_internacao = distinct(banco_incidencia_internacao_GERAL, DATA_ATO, PROCESSO, ATO_INFRACIONAL, .keep_all= TRUE)
#########################################################################################################
banco_geral_internacao_SNR = distinct(banco_incidencia_internacao_GERAL, IDS, .keep_all= TRUE)
#########################################################################################################
#########################################################################################################
#INCIDENCIA internacao
#########################################################################################################
library(dplyr)

banco_geral_sem_concurso_internacao = banco_geral_sem_concurso_internacao %>%
  select(ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
###banco_geral_sem_concurso_internacao
#########################################################################################################
#setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_geral_sem_concurso_internacao_bkp = banco_geral_sem_concurso_internacao

banco_geral_sem_concurso_internacao_bkp =
  banco_geral_sem_concurso_internacao_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(banco_geral_sem_concurso_internacao_bkp)[1]<-'banco_geral_sem_concurso_internacao_bkp'
colnames(banco_geral_sem_concurso_internacao_bkp)[2]<-'QUANTIDADE'
colnames(banco_geral_sem_concurso_internacao_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#para script rmd:
banco_geral_sem_concurso_internacao_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_geral_sem_concurso_internacao_bkp$PERCENTUAL))
banco_geral_sem_concurso_internacao_bkp_rmd = tail(banco_geral_sem_concurso_internacao_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_geral_sem_concurso_internacao =
  banco_geral_sem_concurso_internacao %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(ATO_INFRACIONAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

colnames(banco_geral_sem_concurso_internacao)[1]<-'ATO'
colnames(banco_geral_sem_concurso_internacao)[2]<-'QUANTIDADE'
colnames(banco_geral_sem_concurso_internacao)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################
# Criar tabela de frequências cruzadas entre IDADE e SEXO
tabela_IDADE_SEXO_internacao <- internacao_GERAL_SNR %>%
  tabyl(IDADE, SEXO) %>%
  adorn_totals(c("row", "col")) %>% # Adicionar totais nas linhas e colunas
  adorn_percentages("col") %>% # Calcular porcentagens por coluna (SEXO)
  adorn_pct_formatting(digits = 2) %>% # Formatar porcentagens com 2 casas decimais
  adorn_ns() %>% # Adicionar contagens absolutas entre parênteses
  mutate(IDADE = ifelse(IDADE == "Total", "Total", paste0(IDADE, " anos"))) # Adicionar "anos" à coluna IDADE


# Substituir "VS/INFORMAÇÃO" por "S/INFORMAÇÃO" na coluna 'ato'
tabela_IDADE_SEXO_internacao <- tabela_IDADE_SEXO_internacao %>%
  mutate(IDADE = gsub("Total", "TOTAL", IDADE))


# Modificar os nomes das colunas
colnames(tabela_IDADE_SEXO_internacao) <- c("IDADE", "FEMININO", "MASCULINO", "TOTAL")

# Exibir a tabela
print(tabela_IDADE_SEXO_internacao)

#########################################################################################################
#########################################################################################################


# Criar uma tabela com as contagens de linhas
tabela_total_adls_corre <- tribble(
  ~GRUPO, ~QUANTIDADE,
  "SEMILIBERDADE", nrow(semiliberdade_SNR),
  "INTERNAÇÃO", nrow(internacao_SNR)
) %>%
  bind_rows(  # Adicionar linha de total
    tibble(GRUPO = "Total", QUANTIDADE = sum(.$QUANTIDADE))
  )

print(tabela_total_adls_corre)

#############################################################################################################
#ESCOLARIDADE_semiliberdade_CORRE
#########################################################################################################

ESCOLARIDADE_semiliberdade_CORRE =
  semiliberdade_GERAL_SNR |>
  select(ESCOLARIDADE_TERMO)

#adaptando para o restante dos scripts
colnames(ESCOLARIDADE_semiliberdade_CORRE)[1]<-'ESCOLARIDADE_semiliberdade_CORRE'

ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE = ajustar_nomes(ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE)
#########################################################################################################
#AJUSTA OS FORA DE PADRÃO AQUI:
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "EJA"]<- "EJAENSFUND" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == ""]<- "SEMINFORMACAO" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "NSA"]<- "NAORESPONDEU" #FIZ OPÇÃO PELO FUND
#ORDENANDO

ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "1ªSERIE-ENSFUND"]<- "A1ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "2ªSERIE-ENSFUND"]<- "B2ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "3ªSERIE-ENSFUND"]<- "C3ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "4ªSERIE-ENSFUND"]<- "D4ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "5ªSERIE-ENSFUND"]<- "E5ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "6ªSERIE-ENSFUND"]<- "F6ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "7ªSERIE-ENSFUND"]<- "G7ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "8ªSERIE-ENSFUND"]<- "H8ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "9ªSERIE-ENSFUND"]<- "I9ªSERIE-ENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "1ºANO-ENSMEDIO"]<- "J1ºANO-ENSMEDIO"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "2ºANO-ENSMEDIO"]<- "K2ºANO-ENSMEDIO"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "3ºANO-ENSMEDIO"]<- "L3ºANO-ENSMEDIO"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "FACULDADE1ºPERIODO"]<- "LAFACULDADE1ºPERIODO"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "EJAENSFUND"]<- "MEJAENSFUND"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "EJAENSMEDIO"]<- "NEJAENSMEDIO"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "NAOSABE"]<- "ONAOSABE"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "NAORESPONDEU"]<- "PNAORESPONDEU"
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "SEMINFORMACAO"]<- "QSEMINFORMACAO"

#########################################################################################################
# salvando para gráfico
ESCOLARIDADE_semiliberdade_CORRE_bkp = ESCOLARIDADE_semiliberdade_CORRE

ESCOLARIDADE_semiliberdade_CORRE_bkp =
  ESCOLARIDADE_semiliberdade_CORRE_bkp %>%
  janitor::tabyl(ESCOLARIDADE_semiliberdade_CORRE) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_bkp$ESCOLARIDADE_semiliberdade_CORRE == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"


#########################################################################################################
#replace "%" with "" in the percentual column
ESCOLARIDADE_semiliberdade_CORRE_bkp$PERCENTUAL2 <- str_replace (ESCOLARIDADE_semiliberdade_CORRE_bkp$percent, "%", "")
ESCOLARIDADE_semiliberdade_CORRE_bkp$PERCENTUAL2 = as.numeric(ESCOLARIDADE_semiliberdade_CORRE_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(ESCOLARIDADE_semiliberdade_CORRE_bkp)[1]<-'ESCOLARIDADE_semiliberdade_CORRE_bkp'
colnames(ESCOLARIDADE_semiliberdade_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_semiliberdade_CORRE_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESCOLARIDADE_semiliberdade_CORRE_bkp_rmark = ESCOLARIDADE_semiliberdade_CORRE_bkp

ESCOLARIDADE_semiliberdade_CORRE_bkp_rmark = ESCOLARIDADE_semiliberdade_CORRE_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
ESCOLARIDADE_semiliberdade_CORRE_bkp_rmark =
  ESCOLARIDADE_semiliberdade_CORRE_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "VNÃO SABE"]<- "UNÃO SABE"
#ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE$ESCOLARIDADE_semiliberdade_CORRE == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
ESCOLARIDADE_semiliberdade_CORRE_TABELA =
  ESCOLARIDADE_semiliberdade_CORRE %>%
  janitor::tabyl(ESCOLARIDADE_semiliberdade_CORRE) %>%
  arrange(ESCOLARIDADE_semiliberdade_CORRE) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#ordenando:

ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE[ESCOLARIDADE_semiliberdade_CORRE_TABELA$ESCOLARIDADE_semiliberdade_CORRE == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(ESCOLARIDADE_semiliberdade_CORRE_TABELA)[1]<-'ESCOLARIDADE'
colnames(ESCOLARIDADE_semiliberdade_CORRE_TABELA)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_semiliberdade_CORRE_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#ESCOLARIDADE_semiliberdade_CORRE FIM
#########################################################################################################

