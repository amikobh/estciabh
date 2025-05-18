library(pdftools)
library(tidyverse)

# Definir o diretório de trabalho
setwd("/media/backup/config/pessoal/elerson/diretorio_r/estciabh/letalidade/arq_fontes/cai_2023")

# Função para extrair o texto de um PDF
extract_text <- function(pdf_file) {
  text <- pdf_text(pdf_file)
  return(paste(text, collapse = "\n"))  # Colapsar páginas em um único texto
}

# Função para processar o texto e criar o dataframe
process_text <- function(text) {
  # Extrair nome
  nome <- str_extract(text, "Nome: [^\n]+") %>% str_remove("Nome: ") %>% replace_na("")

  # Extrair CPF
  cpf <- str_extract(text, "CPF: [^\n]+") %>% str_remove("CPF: ") %>% replace_na("")

  # Extrair RG
  rg <- str_extract(text, "RG: [^\n]+") %>% str_remove("RG: ") %>% replace_na("")

  # Extrair nome do pai
  nome_pai <- str_extract(text, "Nome pai: [^\n]+") %>% str_remove("Nome pai: ") %>% replace_na("")

  # Extrair nome da mãe
  nome_mae <- str_extract(text, "Nome mãe: [^\n]+") %>% str_remove("Nome mãe: ") %>% replace_na("")

  # Verificar se há a expressão "NADA HAVER E/OU TER HAVIDO CONTRA"
  if (str_detect(text, "NADA HAVER E/OU TER HAVIDO CONTRA")) {
    # Caso TEXTO B: Somente preencher o nome e a coluna CAI
    df <- data.frame(
      nome = nome,
      cpf = "",
      rg = "",
      nome_pai = "",
      nome_mae = "",
      cai = "NADA HAVER E/OU TER HAVIDO CONTRA",
      processo = "",
      sentenca = "",
      Enquadramento_001 = "",
      Enquadramento_002 = "",
      Enquadramento_003 = "",
      medida_social_001 = "",
      medida_social_002 = "",
      medida_social_003 = "",
      stringsAsFactors = FALSE
    )
    return(df)
  } else {
    # Caso TEXTO A: Extrair todos os processos e informações associadas
    blocos_processos <- str_extract_all(text, "Processo\\s+Distribuição\\s+Situação\\s+([\\s\\S]+?)(?=\\d+-\\d+\\.\\d+|$)")[[1]]

    # Criar um dataframe temporário para armazenar os dados
    df <- data.frame(
      nome = character(),
      cpf = character(),
      rg = character(),
      nome_pai = character(),
      nome_mae = character(),
      cai = character(),
      processo = character(),
      sentenca = character(),
      Enquadramento_001 = character(),
      Enquadramento_002 = character(),
      Enquadramento_003 = character(),
      medida_social_001 = character(),
      medida_social_002 = character(),
      medida_social_003 = character(),
      stringsAsFactors = FALSE
    )

    # Processar cada bloco de texto correspondente a um processo
    for (bloco in blocos_processos) {
      # Extrair número do processo
      processo <- str_extract(bloco, "\\d+-\\d+\\.\\d+") %>% replace_na("")

      # Extrair sentença
      sentenca <- str_extract(bloco, "SENTENÇA: [^\n]+") %>% str_remove("SENTENÇA: ") %>% replace_na("")

      # Extrair enquadramentos
      enquadramentos <- str_extract_all(bloco, "ART\\. [^\\n]+")[[1]]
      enquadramentos <- if (length(enquadramentos) == 0) rep("", 3) else c(enquadramentos, rep("", 3 - length(enquadramentos)))

      # Extrair medidas sociais
      medidas_sociais <- str_extract_all(bloco, "MEDIDA\\(S\\) SOCIAL\\(IS\\):\\s+([^\\n]+\\n)+")[[1]] %>%
        str_split("\n") %>%
        unlist() %>%
        str_trim() %>%
        discard(~ . == "")
      medidas_sociais <- if (length(medidas_sociais) == 0) rep("", 3) else c(medidas_sociais, rep("", 3 - length(medidas_sociais)))

      # Criar uma linha para o dataframe
      linha <- data.frame(
        nome = nome,
        cpf = cpf,
        rg = rg,
        nome_pai = nome_pai,
        nome_mae = nome_mae,
        cai = "HAVER E/OU TER HAVIDO CONTRA",
        processo = processo,
        sentenca = sentenca,
        Enquadramento_001 = enquadramentos[1],
        Enquadramento_002 = enquadramentos[2],
        Enquadramento_003 = enquadramentos[3],
        medida_social_001 = medidas_sociais[1],
        medida_social_002 = medidas_sociais[2],
        medida_social_003 = medidas_sociais[3],
        stringsAsFactors = FALSE
      )

      # Adicionar a linha ao dataframe
      df <- rbind(df, linha)
    }

    return(df)
  }
}

# Listar todos os arquivos PDF no diretório
pdf_files <- list.files(pattern = "*.pdf", full.names = TRUE)

# Verificar se há arquivos PDF no diretório
if (length(pdf_files) == 0) {
  stop("Nenhum arquivo PDF encontrado no diretório especificado.")
} else {
  message("Foram encontrados ", length(pdf_files), " arquivos PDF para processamento.")
}

# Extrair o texto de todos os PDFs
all_texts <- map(pdf_files, extract_text)

# Verificar se todos os textos foram extraídos com sucesso
if (length(all_texts) == 0) {
  stop("Falha ao extrair o texto dos arquivos PDF.")
} else {
  message("Texto extraído de ", length(all_texts), " arquivos PDF com sucesso.")
}

# Processar todos os textos e combinar em um único dataframe
dadoslet <- map_dfr(all_texts, process_text)

# Exibir o dataframe resultante
print(dadoslet)

# Salvar o dataframe em um arquivo CSV (opcional)
write.csv(dadoslet, "dadoslet.csv", row.names = FALSE)
