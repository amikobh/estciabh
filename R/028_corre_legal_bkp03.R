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
banco_natacao <- read.csv("natacao.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
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


banco_natacao =
  banco_natacao %>%
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
#TRATAMENTO banco_natacao
#################################################################################################################
# Carregar pacotes necessários
library(dplyr)
library(lubridate)
library(stringr)

# 1) Mudar o nome da coluna 'data_do_treino' para 'data_da_atividade'
banco_natacao <- banco_natacao %>%
  rename(data_da_atividade = data_do_treino,
         ids = id)

banco_natacao = banco_natacao |>
  select(-carimbo_de_data_hora)


# 2) Corrigir anos errados nas datas (se o ano for '0024' ou '1204', substituímos por '2024')
banco_natacao <- banco_natacao %>%
  mutate(data_da_atividade = as.character(data_da_atividade)) %>%
  mutate(data_da_atividade = if_else(
    str_sub(data_da_atividade, 1, 4) == "0024",
    paste0("2024", str_sub(data_da_atividade, 5, 10)),
    if_else(str_sub(data_da_atividade, 1, 4) == "1204",
            paste0("2024", str_sub(data_da_atividade, 5, 10)),
            data_da_atividade
    )
  ))

# 3) Transformar a coluna 'data_da_atividade' para a classe Date usando lubridate
banco_natacao <- banco_natacao %>%
  mutate(data_da_atividade = ymd(data_da_atividade))  # Converte para formato de data

# 4) Criar a coluna 'atividade2' e preencher com 'BANCONATACAO'
banco_natacao <- banco_natacao %>%
  mutate(atividade2 = "BANCONATACAO")

# 5) Separar os dataframes com base na coluna 'unidade'
# a) Criar o dataframe natacao_semiliberdade
natacao_semiliberdade <- banco_natacao %>%
  filter(str_detect(unidade, "SEMILIBERDADE"))

# b) Criar o dataframe natacao_internacao
natacao_internacao <- banco_natacao %>%
  filter(!str_detect(unidade, "SEMILIBERDADE"))

# Verificar se a separação foi feita corretamente
head(natacao_semiliberdade)
head(natacao_internacao)

# 6) Verificar se há mais erros similares nas datas
# Exibir as primeiras linhas para verificação
head(banco_natacao$data_da_atividade)

# 7) Verificar todas as datas que possuem problemas de ano
datas_com_erro <- banco_natacao %>%
  filter(str_sub(data_da_atividade, 1, 4) == "0024" | str_sub(data_da_atividade, 1, 4) == "1204")

# Exibir as datas com erros
print(datas_com_erro)

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

df_new_semiliberdade = df_new_semiliberdade |>
  select(-ids)

# Converter a coluna IDS para inteiro
df_new_semiliberdade$IDS <- as.integer(df_new_semiliberdade$IDS)

# Verificar a classe da coluna após a conversão
class(df_new_semiliberdade$IDS)  # Deve retornar "integer"
#################################################################################################################
# Carregar o pacote dplyr
library(dplyr)

# Renomear a coluna 'ids' de natacao_semiliberdade para 'IDS' para alinhar com df_new_semiliberdade
natacao_semiliberdade <- natacao_semiliberdade %>%
  rename(IDS = ids)


# Carregar o pacote dplyr
library(dplyr)

# Identificar as colunas comuns entre os dois dataframes
colunas_comum <- intersect(names(df_new_semiliberdade), names(natacao_semiliberdade))

# Remover 'data_da_atividade' e 'IDS' da lista de colunas a serem modificadas
colunas_comum <- colunas_comum[!(colunas_comum %in% c("data_da_atividade", "IDS"))]

# Para cada coluna comum, garantir que as classes (tipos) em natacao_semiliberdade sejam as mesmas de df_new_semiliberdade
for (coluna in colunas_comum) {
  # Verificar o tipo da coluna no df_new_semiliberdade
  tipo_coluna_df_new <- typeof(df_new_semiliberdade[[coluna]])

  # Ajustar a classe da coluna correspondente em natacao_semiliberdade para o tipo do df_new_semiliberdade
  natacao_semiliberdade[[coluna]] <- as(natacao_semiliberdade[[coluna]], tipo_coluna_df_new)
}

# Verificar os tipos das colunas após a conversão
sapply(natacao_semiliberdade, typeof)


# Garantir que ambas as colunas 'IDS' (em ambos os dataframes) sejam do mesmo tipo, por exemplo, 'character'
df_new_semiliberdade <- df_new_semiliberdade %>%
  mutate(IDS = as.character(IDS))  # Converter para 'character' em df_new_semiliberdade

natacao_semiliberdade <- natacao_semiliberdade %>%
  mutate(IDS = as.character(IDS))  # Converter para 'character' em natacao_semiliberdade

# Identificar as colunas que estão em df_new_semiliberdade, mas não em natacao_semiliberdade
colunas_nao_comuns <- setdiff(names(df_new_semiliberdade), names(natacao_semiliberdade))

# Adicionar as colunas ausentes em natacao_semiliberdade com "BANCONATACAO"
for (coluna in colunas_nao_comuns) {
  natacao_semiliberdade[[coluna]] <- "NATACAOBANCO"
}

# Converter a coluna total_de_jovens_na_unidade_nesta_data para inteiro
natacao_semiliberdade$total_de_jovens_na_unidade_nesta_data <- as.integer(natacao_semiliberdade$total_de_jovens_na_unidade_nesta_data)


# Realizar o empilhamento das linhas entre os dois dataframes
df_unido_semiliberdade <- bind_rows(df_new_semiliberdade, natacao_semiliberdade)
#################################################################################################################
#Salvando para a equipe verificar:

setwd(file.path("~/diretorio_r/estciabh/corre/planilhas/comissarias/"))#configurar diretorio
#################################################################################################################
# Remover a coluna
df_new_semiliberdade <- df_unido_semiliberdade %>% select(-nome)

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
#################################################################################################################
#TRATAMENTO banco_semiliberdade FIM
#################################################################################################################
#########################################################################################################
#################################################################################################################
#TRATAMENTO banco_internacao
#################################################################################################################


# formatando campos data_da_atividade e hora_da_atividade:

banco_internacao$data_da_atividade = as.Date(banco_internacao$data_da_atividade)

banco_internacao$hora_da_atividade = strptime(banco_internacao$hora_da_atividade, format = "%H:%M")

banco_internacao$hora_da_atividade <- format(banco_internacao$hora_da_atividade, "%H:%M")

names(banco_internacao)



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


#################################################################################################################
# Normalizar a coluna 'atividade2' (remover espaços extras).
#str_squish(atividade2): A função str_squish() remove os espaços extras e os múltiplos


df_new_internacao <- df_new_internacao %>%
  mutate(atividade2 = str_squish(atividade),    # Remove espaços extras
         atividade2 = toupper(atividade2))      # Converte para maiúsculas

df_new_internacao = df_new_internacao |>
  select(-ids)

# Converter a coluna IDS para inteiro
df_new_internacao$IDS <- as.integer(df_new_internacao$IDS)

# Verificar a classe da coluna após a conversão
class(df_new_internacao$IDS)  # Deve retornar "integer"
#################################################################################################################
# Carregar o pacote dplyr
library(dplyr)

# Renomear a coluna 'ids' de natacao_internacao para 'IDS' para alinhar com df_new_internacao
natacao_internacao <- natacao_internacao %>%
  rename(IDS = ids)


# Carregar o pacote dplyr
library(dplyr)

# Identificar as colunas comuns entre os dois dataframes
colunas_comum <- intersect(names(df_new_internacao), names(natacao_internacao))

# Remover 'data_da_atividade' e 'IDS' da lista de colunas a serem modificadas
colunas_comum <- colunas_comum[!(colunas_comum %in% c("data_da_atividade", "IDS"))]

# Para cada coluna comum, garantir que as classes (tipos) em natacao_internacao sejam as mesmas de df_new_internacao
for (coluna in colunas_comum) {
  # Verificar o tipo da coluna no df_new_internacao
  tipo_coluna_df_new <- typeof(df_new_internacao[[coluna]])

  # Ajustar a classe da coluna correspondente em natacao_internacao para o tipo do df_new_internacao
  natacao_internacao[[coluna]] <- as(natacao_internacao[[coluna]], tipo_coluna_df_new)
}

# Verificar os tipos das colunas após a conversão
sapply(natacao_internacao, typeof)


# Garantir que ambas as colunas 'IDS' (em ambos os dataframes) sejam do mesmo tipo, por exemplo, 'character'
df_new_internacao <- df_new_internacao %>%
  mutate(IDS = as.character(IDS))  # Converter para 'character' em df_new_internacao

natacao_internacao <- natacao_internacao %>%
  mutate(IDS = as.character(IDS))  # Converter para 'character' em natacao_internacao

# Identificar as colunas que estão em df_new_internacao, mas não em natacao_internacao
colunas_nao_comuns <- setdiff(names(df_new_internacao), names(natacao_internacao))

# Adicionar as colunas ausentes em natacao_internacao com "BANCONATACAO"
for (coluna in colunas_nao_comuns) {
  natacao_internacao[[coluna]] <- "NATACAOBANCO"
}

# Converter a coluna total_de_jovens_na_unidade_nesta_data para inteiro
natacao_internacao$total_de_jovens_na_unidade_nesta_data <- as.integer(natacao_internacao$total_de_jovens_na_unidade_nesta_data)
natacao_internacao$matricula <- as.integer(natacao_internacao$matricula)

# Realizar o empilhamento das linhas entre os dois dataframes
df_unido_internacao <- bind_rows(df_new_internacao, natacao_internacao)
#################################################################################################################
#Salvando para a equipe verificar:

setwd(file.path("~/diretorio_r/estciabh/corre/planilhas/comissarias/"))#configurar diretorio
#################################################################################################################
# Remover a coluna
df_new_internacao <- df_unido_internacao %>% select(-nome)

# Fazendo a junção com dplyr
internacao_final <- df_new_internacao %>%
  left_join(painel_tratado, by = "IDS") %>%
  mutate(NOME = ifelse(is.na(nome), "SEM CORRESPONDÊNCIA", nome)) %>%
  select(IDS, NOME, everything(), -nome)

#para Sandra e Vanessa verificarem os sem correspondência
# Exportando para um arquivo CSV


write.csv(internacao_final, "internacao.csv", row.names = FALSE)


sem_correspondencia_semi = internacao_final |>
  filter(NOME %in% "SEM CORRESPONDÊNCIA")

# Exportando para um arquivo CSV
write.csv(sem_correspondencia_semi, "sem_correspondencia_semi.csv", row.names = FALSE)

#########################################################################################################

library(dplyr)

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

#################################################################################################################
#TRATAMENTO banco_internacao
#################################################################################################################
#atv = quantas vezes um mesmo adolecente participou das atividades?

# Contar as ocorrências de cada valor único
internacao_atv <- internacao_sem_duplicadas %>%
  count(IDS)

# Criar faixas com base nas repetições
internacao_atv <- internacao_atv %>%
  mutate(faixa = case_when(
    n < 10 ~ "Menos de 10 vezes",
    n >= 10 & n <= 20 ~ "Entre 10 e 20 vezes",
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


internacao_SNR = internacao_sem_duplicadas %>%
  distinct(IDS, .keep_all = TRUE)

#########################################################################################################
#################################################################################################################
#TRATAMENTO banco_internacao FIM
#################################################################################################################
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
                               DECISAO, MEDIDA_PROTETIVA, QUAL_MEDIDA_PROTETIVA_01, QUAL_MEDIDA_PROTETIVA_02,
                               QUAL_MEDIDA_PROTETIVA_03, QUAL_MEDIDA_PROTETIVA_04, QUAL_MEDIDA_PROTETIVA_05,
                               QUAL_MEDIDA_PROTETIVA_06, SENTENCA, REGIONAL_RESIDENCIAL, REGIONAL_ATO)],  # Seleciona as colunas relevantes do segundo dataframe
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



# Criar tabela de frequências cruzadas entre IDADE e SEXO
tabela_IDADE_SEXO_semiliberdade <- semiliberdade_GERAL %>%
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
                               DECISAO, MEDIDA_PROTETIVA, QUAL_MEDIDA_PROTETIVA_01, QUAL_MEDIDA_PROTETIVA_02,
                               QUAL_MEDIDA_PROTETIVA_03, QUAL_MEDIDA_PROTETIVA_04, QUAL_MEDIDA_PROTETIVA_05,
                               QUAL_MEDIDA_PROTETIVA_06, SENTENCA, REGIONAL_RESIDENCIAL, REGIONAL_ATO)],  # Seleciona as colunas relevantes do segundo dataframe
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

#############################################################################################################
#########################################################################################################
# Criar tabela de frequências cruzadas entre IDADE e SEXO
tabela_IDADE_SEXO_internacao <- internacao_GERAL %>%
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


# Calculando a coluna PERCENTUAL
tabela_total_adls_corre$PERCENTUAL <- round((tabela_total_adls_corre$QUANTIDADE / tabela_total_adls_corre$QUANTIDADE[tabela_total_adls_corre$GRUPO == "Total"]) * 100, 2)

# Exibindo o dataframe final
print(tabela_total_adls_corre)



print(tabela_total_adls_corre)


#########################################################################################################
#TRATAMENTO banco_semiliberdade_CORRE DEMAIS VARIÁVEIS
#########################################################################################################

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
#########################################################################################################
#TRATAMENTO incidencia_semiliberdade_CORRE:
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
#########################################################################################################
#TRATAMENTO incidencia_semiliberdade_CORRE FIM:
#########################################################################################################
#########################################################################################################
###DECISAO_banco_semiliberdade_CORRE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/cedipro"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

DECISAO_banco_semiliberdade_CORRE = semiliberdade_GERAL
#DECISAO_geral_bkp = DECISAO_geral
#DECISAO02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, DECISAO_PROTETIVA, QUAL_DECISAO_PROTETIVA_01,
#       QUAL_DECISAO_PROTETIVA_02, QUAL_DECISAO_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#########################################################################################################
DECISAO_banco_semiliberdade_CORRE$DECISAO <- gsub(" ","", DECISAO_banco_semiliberdade_CORRE$DECISAO)
#########################################################################################################

#preenchimento de celulas:
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "INTERNACAO"]<-	"INTERNAÇÃO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "LA"]<-	"REMISSAO c/c LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "OUTRAS(OS)"]<-	"VOUTROS"
#DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "PSC"]<-	"REMISSAO c/c PSC"
#DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMESSAAOJUIZOCOMPETENTE-MAIORidade2"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "SEMINFORMACAO"]<-	"VAZIO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOAPSC"]<-	"RETORNO A PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOALA"]<-	"RETORNO A LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMESSAAOJUIZOCOMPETENTE-MAIORidade2CONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == ""]<-	"VAZIO"

#########################################################################################################
#########################################################################################################
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "VAZIO"]<-	"SEM INFORMAÇÃO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "VOUTROS"]<-	"OUTROS"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
DECISAO_banco_semiliberdade_CORRE_bkp = DECISAO_banco_semiliberdade_CORRE

DECISAO_banco_semiliberdade_CORRE_bkp =
  DECISAO_banco_semiliberdade_CORRE_bkp %>%
  janitor::tabyl(DECISAO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

colnames(DECISAO_banco_semiliberdade_CORRE_bkp)[1]<-'DECISAO_banco_semiliberdade_CORRE_bkp'
colnames(DECISAO_banco_semiliberdade_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(DECISAO_banco_semiliberdade_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#ordenando outros para final da tabela
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "OUTROS"]<-	"ROUTROS"
#########################################################################################################
DECISAO_banco_semiliberdade_CORRE =
  DECISAO_banco_semiliberdade_CORRE %>%
  janitor::tabyl(DECISAO) %>%
  arrange(DECISAO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
DECISAO_banco_semiliberdade_CORRE$DECISAO[DECISAO_banco_semiliberdade_CORRE$DECISAO == "ROUTROS"]<- "OUTROS"

colnames(DECISAO_banco_semiliberdade_CORRE)[1]<-'DECISÃO'
colnames(DECISAO_banco_semiliberdade_CORRE)[2]<-'QUANTIDADE'
colnames(DECISAO_banco_semiliberdade_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################
# DECISAO_banco_semiliberdade_CORRE =
#   DECISAO_banco_semiliberdade_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# DECISAO_banco_semiliberdade_CORRE FIM
#########################################################################################################
#########################################################################################################
###SENTENCA_banco_semiliberdade_CORRE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/cedipro"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

SENTENCA_banco_semiliberdade_CORRE = semiliberdade_GERAL
#SENTENCA_geral_bkp = SENTENCA_geral
#SENTENCA02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, SENTENCA, SENTENCA_PROTETIVA, QUAL_SENTENCA_PROTETIVA_01,
#       QUAL_SENTENCA_PROTETIVA_02, QUAL_SENTENCA_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#########################################################################################################
SENTENCA_banco_semiliberdade_CORRE$SENTENCA <- gsub(" ","", SENTENCA_banco_semiliberdade_CORRE$SENTENCA)
#########################################################################################################

#preenchimento de celulas:
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "SENTENCAINCONCLUSIVA"]<-	"VOUTROS"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "semiliberdade"]<-	"INTERNAÇÃO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "semiliberdadePROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "semiliberdadePROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "LA"]<-	"REMISSAO c/c LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "OUTRAS(OS)"]<-	"VOUTROS"
#SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "PSC"]<-	"REMISSAO c/c PSC"
#SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMESSAAOJUIZOCOMPETENTE-MAIORidade2"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOAsemiliberdade"]<-	"RETORNO A INTERNAÇÃO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "SEMINFORMACAO"]<-	"VAZIO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOAPSC"]<-	"RETORNO A PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOALA"]<-	"RETORNO A LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMESSAAOJUIZOCOMPETENTE-MAIORidade2CONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == ""]<-	"VAZIO"

#########################################################################################################
#########################################################################################################
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "VAZIO"]<-	"SEM INFORMAÇÃO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "VOUTROS"]<-	"OUTROS"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
SENTENCA_banco_semiliberdade_CORRE_bkp = SENTENCA_banco_semiliberdade_CORRE

SENTENCA_banco_semiliberdade_CORRE_bkp =
  SENTENCA_banco_semiliberdade_CORRE_bkp %>%
  janitor::tabyl(SENTENCA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

colnames(SENTENCA_banco_semiliberdade_CORRE_bkp)[1]<-'SENTENCA_banco_semiliberdade_CORRE_bkp'
colnames(SENTENCA_banco_semiliberdade_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(SENTENCA_banco_semiliberdade_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#ordenando outros para final da tabela
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "OUTROS"]<-	"ROUTROS"
#########################################################################################################
SENTENCA_banco_semiliberdade_CORRE =
  SENTENCA_banco_semiliberdade_CORRE %>%
  janitor::tabyl(SENTENCA) %>%
  arrange(SENTENCA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
SENTENCA_banco_semiliberdade_CORRE$SENTENCA[SENTENCA_banco_semiliberdade_CORRE$SENTENCA == "ROUTROS"]<- "OUTROS"

colnames(SENTENCA_banco_semiliberdade_CORRE)[1]<-'SENTENÇA'
colnames(SENTENCA_banco_semiliberdade_CORRE)[2]<-'QUANTIDADE'
colnames(SENTENCA_banco_semiliberdade_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################
# SENTENCA_banco_semiliberdade_CORRE =
#   SENTENCA_banco_semiliberdade_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# SENTENCA_banco_semiliberdade_CORRE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO PROTETIVAS_banco_semiliberdade_CORRE
#########################################################################################################

PROTETIVAS_banco_semiliberdade_CORRE =

  semiliberdade_GERAL %>%
  filter(MEDIDA_PROTETIVA == "SIM")

PROTETIVAS_banco_semiliberdade_CORRE =
  PROTETIVAS_banco_semiliberdade_CORRE %>%
  pivot_longer(cols = starts_with("QUAL_MEDIDA_PROTETIVA_0"), values_to = "MEDIDA_PROTETIVA_GERAL") %>%
  #select(-name) %>%
  filter(MEDIDA_PROTETIVA_GERAL != "" & MEDIDA_PROTETIVA_GERAL != "NSA")

#########################################################################################################
PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL == "1",
                                                                     "ART. 101, I", PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL == "2",
                                                                     "ART. 101, II", PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL == "3",
                                                                     "ART. 101, III", PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL == "4",
                                                                     "ART. 101, IV", PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL == "5",
                                                                     "ART. 101, V", PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL == "6",
                                                                     "ART. 101, VI", PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL == "7",
                                                                     "ART. 101, VII", PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
#PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL[PROTETIVAS_banco_semiliberdade_CORRE$MEDIDA_PROTETIVA_GERAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
PROTETIVAS_banco_semiliberdade_CORRE_bkp = PROTETIVAS_banco_semiliberdade_CORRE

PROTETIVAS_banco_semiliberdade_CORRE_bkp =
  PROTETIVAS_banco_semiliberdade_CORRE_bkp %>%
  janitor::tabyl(MEDIDA_PROTETIVA_GERAL) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(PROTETIVAS_banco_semiliberdade_CORRE_bkp)[1]<-'PROTETIVAS_banco_semiliberdade_CORRE_bkp'
colnames(PROTETIVAS_banco_semiliberdade_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(PROTETIVAS_banco_semiliberdade_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

PROTETIVAS_banco_semiliberdade_CORRE =
  PROTETIVAS_banco_semiliberdade_CORRE %>%
  janitor::tabyl(MEDIDA_PROTETIVA_GERAL) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:

colnames(PROTETIVAS_banco_semiliberdade_CORRE)[1]<-'MEDIDA PROTETIVA'
colnames(PROTETIVAS_banco_semiliberdade_CORRE)[2]<-'QUANTIDADE'
colnames(PROTETIVAS_banco_semiliberdade_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################
# PROTETIVAS_banco_semiliberdade_CORRE =
#   PROTETIVAS_banco_semiliberdade_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
#########################################################################################################
#TRATAMENTO PROTETIVAS_banco_semiliberdade_CORRE FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
###REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/cedipro"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE = semiliberdade_GERAL
#regional_ato_geral_bkp = regional_ato_geral
#regional_ato02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, regional_ato, regional_ato_PROTETIVA, QUAL_regional_ato_PROTETIVA_01,
#       QUAL_regional_ato_PROTETIVA_02, QUAL_regional_ato_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#AGREGAR RMBH
REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE$REGIONAL_RESIDENCIAL <- str_replace(REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE$REGIONAL_RESIDENCIAL, "RMBH.*", "ZREGIÃO METROPOLITANA")

#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE$REGIONAL_RESIDENCIAL == ""]<- "ZSEM INFORMAÇÃO"


#########################################################################################################
#########################################################################################################
# salvando para gráfico
REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp = REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE

REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp =
  REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp %>%
  janitor::tabyl(REGIONAL_RESIDENCIAL) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp$REGIONAL_RESIDENCIAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp$REGIONAL_RESIDENCIAL == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"

colnames(REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp)[1]<-'REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp'
colnames(REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE =
  REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE %>%
  janitor::tabyl(REGIONAL_RESIDENCIAL) %>%
  arrange(REGIONAL_RESIDENCIAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE$REGIONAL_RESIDENCIAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE$REGIONAL_RESIDENCIAL == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"



colnames(REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE)[1]<-'REGIONAL'
colnames(REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE)[2]<-'QUANTIDADE'
colnames(REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################

#REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE =
#  REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE FIM
#########################################################################################################
#########################################################################################################
###REGIONAL_ATO_banco_semiliberdade_CORRE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/cedipro"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

REGIONAL_ATO_banco_semiliberdade_CORRE = semiliberdade_GERAL
#REGIONAL_ATO_geral_bkp = REGIONAL_ATO_geral
#REGIONAL_ATO02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, REGIONAL_ATO, REGIONAL_ATO_PROTETIVA, QUAL_REGIONAL_ATO_PROTETIVA_01,
#       QUAL_REGIONAL_ATO_PROTETIVA_02, QUAL_REGIONAL_ATO_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#AGREGAR RMBH
REGIONAL_ATO_banco_semiliberdade_CORRE$REGIONAL_ATO <- str_replace(REGIONAL_ATO_banco_semiliberdade_CORRE$REGIONAL_ATO, "RMBH.*", "ZREGIÃO METROPOLITANA")

#SUBSTITUIR
REGIONAL_ATO_banco_semiliberdade_CORRE$REGIONAL_ATO[REGIONAL_ATO_banco_semiliberdade_CORRE$REGIONAL_ATO == ""]<- "ZSEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# salvando para gráfico
REGIONAL_ATO_banco_semiliberdade_CORRE_bkp = REGIONAL_ATO_banco_semiliberdade_CORRE

REGIONAL_ATO_banco_semiliberdade_CORRE_bkp =
  REGIONAL_ATO_banco_semiliberdade_CORRE_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
REGIONAL_ATO_banco_semiliberdade_CORRE_bkp$REGIONAL_ATO[REGIONAL_ATO_banco_semiliberdade_CORRE_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_ATO_banco_semiliberdade_CORRE_bkp$REGIONAL_ATO[REGIONAL_ATO_banco_semiliberdade_CORRE_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"


colnames(REGIONAL_ATO_banco_semiliberdade_CORRE_bkp)[1]<-'REGIONAL_ATO_banco_semiliberdade_CORRE_bkp'
colnames(REGIONAL_ATO_banco_semiliberdade_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(REGIONAL_ATO_banco_semiliberdade_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

REGIONAL_ATO_banco_semiliberdade_CORRE =
  REGIONAL_ATO_banco_semiliberdade_CORRE %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
REGIONAL_ATO_banco_semiliberdade_CORRE$REGIONAL_ATO[REGIONAL_ATO_banco_semiliberdade_CORRE$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_ATO_banco_semiliberdade_CORRE$REGIONAL_ATO[REGIONAL_ATO_banco_semiliberdade_CORRE$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"



colnames(REGIONAL_ATO_banco_semiliberdade_CORRE)[1]<-'REGIONAL'
colnames(REGIONAL_ATO_banco_semiliberdade_CORRE)[2]<-'QUANTIDADE'
colnames(REGIONAL_ATO_banco_semiliberdade_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################

#REGIONAL_ATO_banco_semiliberdade_CORRE =
#  REGIONAL_ATO_banco_semiliberdade_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# REGIONAL_ATO_banco_semiliberdade_CORRE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO TRATAMENTO banco_semiliberdade_CORRE DEMAIS VARIÁVEIS FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_internacao_CORRE DEMAIS VARIÁVEIS
#########################################################################################################

#############################################################################################################
#ESCOLARIDADE_internacao_CORRE
#########################################################################################################

ESCOLARIDADE_internacao_CORRE =
  internacao_GERAL_SNR |>
  select(ESCOLARIDADE_TERMO)

#adaptando para o restante dos scripts
colnames(ESCOLARIDADE_internacao_CORRE)[1]<-'ESCOLARIDADE_internacao_CORRE'

ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE = ajustar_nomes(ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE)
#########################################################################################################
#AJUSTA OS FORA DE PADRÃO AQUI:
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "EJA"]<- "EJAENSFUND" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == ""]<- "SEMINFORMACAO" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "NSA"]<- "NAORESPONDEU" #FIZ OPÇÃO PELO FUND
#ORDENANDO

ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "1ªSERIE-ENSFUND"]<- "A1ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "2ªSERIE-ENSFUND"]<- "B2ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "3ªSERIE-ENSFUND"]<- "C3ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "4ªSERIE-ENSFUND"]<- "D4ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "5ªSERIE-ENSFUND"]<- "E5ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "6ªSERIE-ENSFUND"]<- "F6ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "7ªSERIE-ENSFUND"]<- "G7ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "8ªSERIE-ENSFUND"]<- "H8ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "9ªSERIE-ENSFUND"]<- "I9ªSERIE-ENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "1ºANO-ENSMEDIO"]<- "J1ºANO-ENSMEDIO"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "2ºANO-ENSMEDIO"]<- "K2ºANO-ENSMEDIO"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "3ºANO-ENSMEDIO"]<- "L3ºANO-ENSMEDIO"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "FACULDADE1ºPERIODO"]<- "LAFACULDADE1ºPERIODO"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "EJAENSFUND"]<- "MEJAENSFUND"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "EJAENSMEDIO"]<- "NEJAENSMEDIO"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "NAOSABE"]<- "ONAOSABE"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "NAORESPONDEU"]<- "PNAORESPONDEU"
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "SEMINFORMACAO"]<- "QSEMINFORMACAO"

#########################################################################################################
# salvando para gráfico
ESCOLARIDADE_internacao_CORRE_bkp = ESCOLARIDADE_internacao_CORRE

ESCOLARIDADE_internacao_CORRE_bkp =
  ESCOLARIDADE_internacao_CORRE_bkp %>%
  janitor::tabyl(ESCOLARIDADE_internacao_CORRE) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_bkp$ESCOLARIDADE_internacao_CORRE == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"


#########################################################################################################
#replace "%" with "" in the percentual column
ESCOLARIDADE_internacao_CORRE_bkp$PERCENTUAL2 <- str_replace (ESCOLARIDADE_internacao_CORRE_bkp$percent, "%", "")
ESCOLARIDADE_internacao_CORRE_bkp$PERCENTUAL2 = as.numeric(ESCOLARIDADE_internacao_CORRE_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(ESCOLARIDADE_internacao_CORRE_bkp)[1]<-'ESCOLARIDADE_internacao_CORRE_bkp'
colnames(ESCOLARIDADE_internacao_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_internacao_CORRE_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESCOLARIDADE_internacao_CORRE_bkp_rmark = ESCOLARIDADE_internacao_CORRE_bkp

ESCOLARIDADE_internacao_CORRE_bkp_rmark = ESCOLARIDADE_internacao_CORRE_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
ESCOLARIDADE_internacao_CORRE_bkp_rmark =
  ESCOLARIDADE_internacao_CORRE_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "VNÃO SABE"]<- "UNÃO SABE"
#ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE$ESCOLARIDADE_internacao_CORRE == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
ESCOLARIDADE_internacao_CORRE_TABELA =
  ESCOLARIDADE_internacao_CORRE %>%
  janitor::tabyl(ESCOLARIDADE_internacao_CORRE) %>%
  arrange(ESCOLARIDADE_internacao_CORRE) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#ordenando:

ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE[ESCOLARIDADE_internacao_CORRE_TABELA$ESCOLARIDADE_internacao_CORRE == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(ESCOLARIDADE_internacao_CORRE_TABELA)[1]<-'ESCOLARIDADE'
colnames(ESCOLARIDADE_internacao_CORRE_TABELA)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_internacao_CORRE_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#ESCOLARIDADE_internacao_CORRE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO incidencia_internacao_CORRE:
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
#########################################################################################################
#TRATAMENTO incidencia_internacao_CORRE FIM:
#########################################################################################################
#########################################################################################################
###DECISAO_banco_internacao_CORRE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/cedipro"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

DECISAO_banco_internacao_CORRE = internacao_GERAL
#DECISAO_geral_bkp = DECISAO_geral
#DECISAO02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, DECISAO_PROTETIVA, QUAL_DECISAO_PROTETIVA_01,
#       QUAL_DECISAO_PROTETIVA_02, QUAL_DECISAO_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#########################################################################################################
DECISAO_banco_internacao_CORRE$DECISAO <- gsub(" ","", DECISAO_banco_internacao_CORRE$DECISAO)
#########################################################################################################

#preenchimento de celulas:
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "INTERNACAO"]<-	"INTERNAÇÃO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "LA"]<-	"REMISSAO c/c LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "OUTRAS(OS)"]<-	"VOUTROS"
#DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "PSC"]<-	"REMISSAO c/c PSC"
#DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMESSAAOJUIZOCOMPETENTE-MAIORidade2"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOAinternacao"]<-	"RETORNO A internacao"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "internacao"]<-	"internacao"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "SEMINFORMACAO"]<-	"VAZIO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOAPSC"]<-	"RETORNO A PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOALA"]<-	"RETORNO A LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMESSAAOJUIZOCOMPETENTE-MAIORidade2CONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == ""]<-	"VAZIO"

#########################################################################################################
#########################################################################################################
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "VAZIO"]<-	"SEM INFORMAÇÃO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "VOUTROS"]<-	"OUTROS"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
DECISAO_banco_internacao_CORRE_bkp = DECISAO_banco_internacao_CORRE

DECISAO_banco_internacao_CORRE_bkp =
  DECISAO_banco_internacao_CORRE_bkp %>%
  janitor::tabyl(DECISAO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

colnames(DECISAO_banco_internacao_CORRE_bkp)[1]<-'DECISAO_banco_internacao_CORRE_bkp'
colnames(DECISAO_banco_internacao_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(DECISAO_banco_internacao_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#ordenando outros para final da tabela
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "OUTROS"]<-	"ROUTROS"
#########################################################################################################
DECISAO_banco_internacao_CORRE =
  DECISAO_banco_internacao_CORRE %>%
  janitor::tabyl(DECISAO) %>%
  arrange(DECISAO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
DECISAO_banco_internacao_CORRE$DECISAO[DECISAO_banco_internacao_CORRE$DECISAO == "ROUTROS"]<- "OUTROS"

colnames(DECISAO_banco_internacao_CORRE)[1]<-'DECISÃO'
colnames(DECISAO_banco_internacao_CORRE)[2]<-'QUANTIDADE'
colnames(DECISAO_banco_internacao_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################
# DECISAO_banco_internacao_CORRE =
#   DECISAO_banco_internacao_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# DECISAO_banco_internacao_CORRE FIM
#########################################################################################################
#########################################################################################################
###SENTENCA_banco_internacao_CORRE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/cedipro"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

SENTENCA_banco_internacao_CORRE = internacao_GERAL
#SENTENCA_geral_bkp = SENTENCA_geral
#SENTENCA02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, SENTENCA, SENTENCA_PROTETIVA, QUAL_SENTENCA_PROTETIVA_01,
#       QUAL_SENTENCA_PROTETIVA_02, QUAL_SENTENCA_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#########################################################################################################
SENTENCA_banco_internacao_CORRE$SENTENCA <- gsub(" ","", SENTENCA_banco_internacao_CORRE$SENTENCA)
#########################################################################################################

#preenchimento de celulas:
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "SENTENCAINCONCLUSIVA"]<-	"VOUTROS"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "internacao"]<-	"INTERNAÇÃO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "internacaoPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "internacaoPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "LA"]<-	"REMISSAO c/c LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "OUTRAS(OS)"]<-	"VOUTROS"
#SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "PSC"]<-	"REMISSAO c/c PSC"
#SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMESSAAOJUIZOCOMPETENTE-MAIORidade2"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOAinternacao"]<-	"RETORNO A INTERNAÇÃO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOAinternacao"]<-	"RETORNO A internacao"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "internacao"]<-	"internacao"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "SEMINFORMACAO"]<-	"VAZIO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOAPSC"]<-	"RETORNO A PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOALA"]<-	"RETORNO A LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMESSAAOJUIZOCOMPETENTE-MAIORidade2CONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == ""]<-	"VAZIO"

#########################################################################################################
#########################################################################################################
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "VAZIO"]<-	"SEM INFORMAÇÃO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "VOUTROS"]<-	"OUTROS"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
SENTENCA_banco_internacao_CORRE_bkp = SENTENCA_banco_internacao_CORRE

SENTENCA_banco_internacao_CORRE_bkp =
  SENTENCA_banco_internacao_CORRE_bkp %>%
  janitor::tabyl(SENTENCA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

colnames(SENTENCA_banco_internacao_CORRE_bkp)[1]<-'SENTENCA_banco_internacao_CORRE_bkp'
colnames(SENTENCA_banco_internacao_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(SENTENCA_banco_internacao_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#ordenando outros para final da tabela
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "OUTROS"]<-	"ROUTROS"
#########################################################################################################
SENTENCA_banco_internacao_CORRE =
  SENTENCA_banco_internacao_CORRE %>%
  janitor::tabyl(SENTENCA) %>%
  arrange(SENTENCA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
SENTENCA_banco_internacao_CORRE$SENTENCA[SENTENCA_banco_internacao_CORRE$SENTENCA == "ROUTROS"]<- "OUTROS"

colnames(SENTENCA_banco_internacao_CORRE)[1]<-'SENTENÇA'
colnames(SENTENCA_banco_internacao_CORRE)[2]<-'QUANTIDADE'
colnames(SENTENCA_banco_internacao_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################
# SENTENCA_banco_internacao_CORRE =
#   SENTENCA_banco_internacao_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# SENTENCA_banco_internacao_CORRE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO PROTETIVAS_banco_internacao_CORRE
#########################################################################################################

PROTETIVAS_banco_internacao_CORRE =

  internacao_GERAL %>%
  filter(MEDIDA_PROTETIVA == "SIM")

PROTETIVAS_banco_internacao_CORRE =
  PROTETIVAS_banco_internacao_CORRE %>%
  pivot_longer(cols = starts_with("QUAL_MEDIDA_PROTETIVA_0"), values_to = "MEDIDA_PROTETIVA_GERAL") %>%
  #select(-name) %>%
  filter(MEDIDA_PROTETIVA_GERAL != "" & MEDIDA_PROTETIVA_GERAL != "NSA")

#########################################################################################################
PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL == "1",
                                                                  "ART. 101, I", PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL == "2",
                                                                  "ART. 101, II", PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL == "3",
                                                                  "ART. 101, III", PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL == "4",
                                                                  "ART. 101, IV", PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL == "5",
                                                                  "ART. 101, V", PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL == "6",
                                                                  "ART. 101, VI", PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)

table(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)
PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL = ifelse(PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL == "7",
                                                                  "ART. 101, VII", PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
#PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL[PROTETIVAS_banco_internacao_CORRE$MEDIDA_PROTETIVA_GERAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
PROTETIVAS_banco_internacao_CORRE_bkp = PROTETIVAS_banco_internacao_CORRE

PROTETIVAS_banco_internacao_CORRE_bkp =
  PROTETIVAS_banco_internacao_CORRE_bkp %>%
  janitor::tabyl(MEDIDA_PROTETIVA_GERAL) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(PROTETIVAS_banco_internacao_CORRE_bkp)[1]<-'PROTETIVAS_banco_internacao_CORRE_bkp'
colnames(PROTETIVAS_banco_internacao_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(PROTETIVAS_banco_internacao_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

PROTETIVAS_banco_internacao_CORRE =
  PROTETIVAS_banco_internacao_CORRE %>%
  janitor::tabyl(MEDIDA_PROTETIVA_GERAL) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:

colnames(PROTETIVAS_banco_internacao_CORRE)[1]<-'MEDIDA PROTETIVA'
colnames(PROTETIVAS_banco_internacao_CORRE)[2]<-'QUANTIDADE'
colnames(PROTETIVAS_banco_internacao_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################
# PROTETIVAS_banco_internacao_CORRE =
#   PROTETIVAS_banco_internacao_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
#########################################################################################################
#TRATAMENTO PROTETIVAS_banco_internacao_CORRE FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
###REGIONAL_RESIDENCIAL_banco_internacao_CORRE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/cedipro"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

REGIONAL_RESIDENCIAL_banco_internacao_CORRE = internacao_GERAL
#regional_ato_geral_bkp = regional_ato_geral
#regional_ato02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, regional_ato, regional_ato_PROTETIVA, QUAL_regional_ato_PROTETIVA_01,
#       QUAL_regional_ato_PROTETIVA_02, QUAL_regional_ato_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#AGREGAR RMBH
REGIONAL_RESIDENCIAL_banco_internacao_CORRE$REGIONAL_RESIDENCIAL <- str_replace(REGIONAL_RESIDENCIAL_banco_internacao_CORRE$REGIONAL_RESIDENCIAL, "RMBH.*", "ZREGIÃO METROPOLITANA")

#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_internacao_CORRE$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_internacao_CORRE$REGIONAL_RESIDENCIAL == ""]<- "ZSEM INFORMAÇÃO"


#########################################################################################################
#########################################################################################################
# salvando para gráfico
REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp = REGIONAL_RESIDENCIAL_banco_internacao_CORRE

REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp =
  REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp %>%
  janitor::tabyl(REGIONAL_RESIDENCIAL) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp$REGIONAL_RESIDENCIAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp$REGIONAL_RESIDENCIAL == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"

colnames(REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp)[1]<-'REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp'
colnames(REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

REGIONAL_RESIDENCIAL_banco_internacao_CORRE =
  REGIONAL_RESIDENCIAL_banco_internacao_CORRE %>%
  janitor::tabyl(REGIONAL_RESIDENCIAL) %>%
  arrange(REGIONAL_RESIDENCIAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_internacao_CORRE$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_internacao_CORRE$REGIONAL_RESIDENCIAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_RESIDENCIAL_banco_internacao_CORRE$REGIONAL_RESIDENCIAL[REGIONAL_RESIDENCIAL_banco_internacao_CORRE$REGIONAL_RESIDENCIAL == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"



colnames(REGIONAL_RESIDENCIAL_banco_internacao_CORRE)[1]<-'REGIONAL'
colnames(REGIONAL_RESIDENCIAL_banco_internacao_CORRE)[2]<-'QUANTIDADE'
colnames(REGIONAL_RESIDENCIAL_banco_internacao_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################

#REGIONAL_RESIDENCIAL_banco_internacao_CORRE =
#  REGIONAL_RESIDENCIAL_banco_internacao_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# REGIONAL_RESIDENCIAL_banco_internacao_CORRE FIM
#########################################################################################################
#########################################################################################################
###REGIONAL_ATO_banco_internacao_CORRE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/cedipro"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

REGIONAL_ATO_banco_internacao_CORRE = internacao_GERAL
#REGIONAL_ATO_geral_bkp = REGIONAL_ATO_geral
#REGIONAL_ATO02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, REGIONAL_ATO, REGIONAL_ATO_PROTETIVA, QUAL_REGIONAL_ATO_PROTETIVA_01,
#       QUAL_REGIONAL_ATO_PROTETIVA_02, QUAL_REGIONAL_ATO_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#AGREGAR RMBH
REGIONAL_ATO_banco_internacao_CORRE$REGIONAL_ATO <- str_replace(REGIONAL_ATO_banco_internacao_CORRE$REGIONAL_ATO, "RMBH.*", "ZREGIÃO METROPOLITANA")

#SUBSTITUIR
REGIONAL_ATO_banco_internacao_CORRE$REGIONAL_ATO[REGIONAL_ATO_banco_internacao_CORRE$REGIONAL_ATO == ""]<- "ZSEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# salvando para gráfico
REGIONAL_ATO_banco_internacao_CORRE_bkp = REGIONAL_ATO_banco_internacao_CORRE

REGIONAL_ATO_banco_internacao_CORRE_bkp =
  REGIONAL_ATO_banco_internacao_CORRE_bkp %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
REGIONAL_ATO_banco_internacao_CORRE_bkp$REGIONAL_ATO[REGIONAL_ATO_banco_internacao_CORRE_bkp$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_ATO_banco_internacao_CORRE_bkp$REGIONAL_ATO[REGIONAL_ATO_banco_internacao_CORRE_bkp$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"


colnames(REGIONAL_ATO_banco_internacao_CORRE_bkp)[1]<-'REGIONAL_ATO_banco_internacao_CORRE_bkp'
colnames(REGIONAL_ATO_banco_internacao_CORRE_bkp)[2]<-'QUANTIDADE'
colnames(REGIONAL_ATO_banco_internacao_CORRE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

REGIONAL_ATO_banco_internacao_CORRE =
  REGIONAL_ATO_banco_internacao_CORRE %>%
  janitor::tabyl(REGIONAL_ATO) %>%
  arrange(REGIONAL_ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
REGIONAL_ATO_banco_internacao_CORRE$REGIONAL_ATO[REGIONAL_ATO_banco_internacao_CORRE$REGIONAL_ATO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_ATO_banco_internacao_CORRE$REGIONAL_ATO[REGIONAL_ATO_banco_internacao_CORRE$REGIONAL_ATO == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"



colnames(REGIONAL_ATO_banco_internacao_CORRE)[1]<-'REGIONAL'
colnames(REGIONAL_ATO_banco_internacao_CORRE)[2]<-'QUANTIDADE'
colnames(REGIONAL_ATO_banco_internacao_CORRE)[3]<-'PERCENTUAL'

#############################################################################################################

#REGIONAL_ATO_banco_internacao_CORRE =
#  REGIONAL_ATO_banco_internacao_CORRE %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# REGIONAL_ATO_banco_internacao_CORRE FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO TRATAMENTO banco_internacao_CORRE DEMAIS VARIÁVEIS FIM
#########################################################################################################
