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

#########################################################################################################

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
#ESTE BANCO servirá para o tratamento do projeto na internação
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
# Normalizar a coluna 'atividade2' (remover espaços extras).
#str_squish(atividade2): A função str_squish() remove os espaços extras e os múltiplos


df_new_semiliberdade <- df_new_semiliberdade %>%
  mutate(atividade2 = str_squish(atividade),    # Remove espaços extras
         atividade2 = toupper(atividade2))      # Converte para maiúsculas


df_new_internacao <- df_new_internacao %>%
  mutate(atividade2 = str_squish(atividade),    # Remove espaços extras
         atividade2 = toupper(atividade2))      # Converte para maiúsculas
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













library(dplyr)

semiliberdade_sem_duplicadas <- semiliberdade_final %>%
  distinct(IDS, NOME, unidade, atividade2, data_da_atividade, hora_da_atividade, .keep_all = TRUE)


internacao_sem_duplicadas <- internacao_final %>%
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




#TRABALHANDO O BANCO SEMILIBERDADE:

#atv = quantas vezes um mesmo adolecente participou das atividades?

# Contar as ocorrências de cada valor único
semiliberdade_atv <- semiliberdade_sem_duplicadas %>%
  count(IDS)

# Criar faixas com base nas repetições
semiliberdade_atv <- semiliberdade_atv %>%
  mutate(faixa = case_when(
    n < 10 ~ "Menos de 10 vezes",
    n >= 11 & n <= 20 ~ "Entre 11 e 20 vezes",
    n >= 21 & n <= 30 ~ "Entre 21 e 30 vezes",
    n > 30 ~ "Acima de 30 vezes"
  ))

# Contar quantas vezes cada faixa ocorre
faixa_semiliberdade_atv <- semiliberdade_atv %>%
  count(faixa) %>%
  arrange(factor(faixa, levels = c("Menos de 10 vezes", "Entre 11 e 20 vezes", "Entre 21 e 30 vezes", "Acima de 30 vezes")))


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


#TRABALHANDO O BANCO internacao:

#atv = quantas vezes um mesmo adolecente participou das atividades?

# Contar as ocorrências de cada valor único
internacao_atv <- internacao_sem_duplicadas %>%
  count(IDS)

# Criar faixas com base nas repetições
internacao_atv <- internacao_atv %>%
  mutate(faixa = case_when(
    n < 10 ~ "Menos de 10 vezes",
    n >= 11 & n <= 20 ~ "Entre 11 e 20 vezes",
    n >= 21 & n <= 30 ~ "Entre 21 e 30 vezes",
    n > 30 ~ "Acima de 30 vezes"
  ))

# Contar quantas vezes cada faixa ocorre
faixa_internacao_atv <- internacao_atv %>%
  count(faixa) %>%
  arrange(factor(faixa, levels = c("Menos de 10 vezes", "Entre 11 e 20 vezes", "Entre 21 e 30 vezes", "Acima de 30 vezes")))


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







# Fazendo a junção com dplyr
# Convertendo ambas as colunas para o mesmo tipo
banco <- banco %>%
  mutate(IDS = as.character(IDS))

semiliberdade_sem_duplicadas <- semiliberdade_sem_duplicadas %>%
  mutate(IDS = as.character(IDS))



total_final <- banco%>%
  left_join(semiliberdade_sem_duplicadas, by = "IDS") %>%
  mutate(IDS = ifelse(is.na(IDS), "SEM CORRESPONDÊNCIA", IDS)) %>%
  select(IDS, everything())


resultado <- total_final %>%
  group_by(IDS, NOME.x) %>%
  filter(data_da_atividade == max(data_da_atividade)) %>%
  ungroup()

