banco_atos_em_foco %>%
  summarise(na_count = sum(is.na(ID))) %>%
  pull(na_count)


df_new_semiliberdade %>%
  select(NOMES_IDS, INICIAIS, IDS) %>%
  slice_head(n = 50) %>%
  print()


# Normalizar a coluna 'atividade2' (remover espaços extras).
#str_squish(atividade2): A função str_squish() remove os espaços extras e os múltiplos

df_new_semiliberdade <- df_new_semiliberdade %>%
  mutate(atividade2 = str_squish(atividade),    # Remove espaços extras
         atividade2 = toupper(atividade2))      # Converte para maiúsculas


# Carregar o pacote dplyr
library(dplyr)

# Procurar por "NÃO TEREMOS" e excluir as linhas que contêm essa palavra
internacao_sem_duplicadas <- internacao_sem_duplicadas %>%
  filter(!grepl("NÃO TEREMOS", atividade2))  # Excluir linhas que contêm 'apple'


# Usando base R para procurar varios termos ou palavra
internacao_sem_aulas <- internacao_sem_duplicadas[grepl("AUDIÊNCIA CONCENTRADA|SEM TREINO|OBS|NÃO TEREMOS", internacao_sem_duplicadas$atividade2), ]

internacao_sem_duplicadas <- internacao_sem_duplicadas[!grepl("AUDIÊNCIA CONCENTRADA|SEM TREINO|OBS|NÃO TEREMOS", internacao_sem_duplicadas$atividade2), ]






library(fuzzyjoin)
library(dplyr)

# Criando um dataframe com o valor correto
correcao <- data.frame(
  atividade_correta = "CORRIDA NA RUA"
)

# Juntando os dataframes com base em similaridade
internacao_sem_duplicadas <- stringdist_left_join(internacao_sem_duplicadas, correcao, by = c("atividade2" = "atividade_correta"), method = "jw", max_dist = 0.2) %>%
  mutate(atividade2 = ifelse(is.na(atividade_correta), atividade2, atividade_correta)) %>%
  select(-atividade_correta)


internacao_sem_duplicadas |> tabyl(atividade2)


# Substituindo as células que contêm "CTE" por "TREINO CTE"

# Substituir todas as ocorrências de 'CTE' por 'TREINO CTE' na coluna 'atividade2'
# Carregar os pacotes necessários
library(dplyr)
library(stringr)


internacao_sem_duplicadas <- internacao_sem_duplicadas %>%
  mutate(atividade2 = if_else(str_detect(atividade2, "CTE"), "TREINO CTE", atividade2))


internacao_sem_duplicadas |> tabyl(atividade2)




internacao_sem_duplicadas$atividade2[grepl("CTE", internacao_sem_duplicadas$atividade2)] <- "TREINO CTE"

internacao_sem_duplicadas |> tabyl(atividade2)

# Substituir todas as ocorrências de 'CTE' por 'TREINO CTE' na coluna 'atividade2'
# Carregar os pacotes necessários
library(dplyr)
library(stringr)


internacao_sem_duplicadas <- internacao_sem_duplicadas %>%
  mutate(atividade2 = if_else(str_detect(atividade2, "CTE"), "TREINO CTE", atividade2))

# Visualizar o resultado


# Contar quantas vezes cada dado se repete
df_count <- df %>%
  count(IDS)

