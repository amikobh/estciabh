#########################################################################################################
dir.create(file.path("~/diretorio_r/estciabh/corre", "planilhas"))
dir.create(file.path("~/diretorio_r/estciabh/corre", "bancos"))

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
# Converte tudo para maiúsculo
df_new_semiliberdade$atividade2 <- toupper(df_new_semiliberdade$atividade)

# Remove espaços e caracteres especiais
df_new_semiliberdade$atividade2 <- gsub("[^A-Z0-9]", "", df_new_semiliberdade$atividade2)



# Converte tudo para maiúsculo
df_new_internacao$atividade2 <- toupper(df_new_internacao$atividade)

# Remove espaços e caracteres especiais
df_new_internacao$atividade2 <- gsub("[^A-Z0-9]", "", df_new_internacao$atividade2)


#################################################################################################################
#Salvando para a equipe verificar:
setwd(file.path("~/corre/planilhas/"))#configurar diretorio
#################################################################################################################


# Fazendo a junção com dplyr
semiliberdade_final <- df_new_semiliberdade %>%
  left_join(painel_tratado, by = "IDS") %>%
  mutate(NOME = ifelse(is.na(nome), "SEM CORRESPONDÊNCIA", nome)) %>%
  select(IDS, NOME, everything(), -nome)

# Exportando para um arquivo CSV
write.csv(semiliberdade_final, "semiliberdade.csv", row.names = FALSE)


sem_correspondencia_semi = semiliberdade_final |>
  filter(NOME %in% "SEM CORRESPONDÊNCIA")

# Exportando para um arquivo CSV
write.csv(sem_correspondencia_semi, "sem_correspondencia_semi.csv", row.names = FALSE)

# Fazendo a junção com dplyr
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
  distinct(IDS, NOME, unidade, atividade, data_da_atividade, hora_da_atividade, .keep_all = TRUE)


internacao_sem_duplicadas <- internacao_final %>%
  distinct(IDS, NOME, unidade, atividade, data_da_atividade, hora_da_atividade, .keep_all = TRUE)



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

