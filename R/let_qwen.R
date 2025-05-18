# Carregar bibliotecas necessárias
library(pdftools)
library(tesseract) # Para OCR, caso necessário
library(dplyr)
library(tidyr)

# Função para extrair informações de um arquivo PDF
extrair_informacoes <- function(caminho_pdf) {
  # Tentar extrair texto diretamente do PDF
  texto <- pdf_text(caminho_pdf)

  # Verificar se o texto está vazio (PDF pode ser baseado em imagem)
  if (all(trimws(texto) == "")) {
    cat("Texto vazio detectado. Tentando OCR...\n")
    texto <- ocr_data(caminho_pdf) # Usar OCR para extrair texto
  }

  # Dividir o texto em linhas
  linhas <- unlist(strsplit(texto, "\n"))

  # Exibir o texto extraído para depuração
  cat("Texto extraído do PDF:\n", texto, "\n")

  # Inicializar variáveis para armazenar os dados
  nome <- cpf <- rg <- nome_pai <- nome_mae <- cai <- NULL
  processos <- list() # Lista para armazenar informações de cada processo

  # Variáveis temporárias para processos
  processo_atual <- NULL
  sentenca_atual <- NULL
  enquadramento_atual <- vector("list", 0)
  medida_social_atual <- vector("list", 0)

  # Processar as linhas para extrair informações
  for (linha in linhas) {
    linha <- trimws(linha) # Remover espaços extras

    # Debug: Exibir cada linha processada
    cat("Processando linha:", linha, "\n")

    if (grepl("Nome\\s*:", linha)) {
      nome <- gsub("Nome\\s*:", "", linha)
      cat("Encontrado Nome:", nome, "\n")
    } else if (grepl("CPF\\s*:", linha)) {
      cpf <- gsub("CPF\\s*:", "", linha)
      cat("Encontrado CPF:", cpf, "\n")
    } else if (grepl("RG\\s*:", linha)) {
      rg <- gsub("RG\\s*:", "", linha)
      cat("Encontrado RG:", rg, "\n")
    } else if (grepl("Nome pai\\s*:", linha)) {
      nome_pai <- gsub("Nome pai\\s*:", "", linha)
      cat("Encontrado Nome Pai:", nome_pai, "\n")
    } else if (grepl("Nome mãe\\s*:", linha)) {
      nome_mae <- gsub("Nome mãe\\s*:", "", linha)
      cat("Encontrado Nome Mãe:", nome_mae, "\n")
    } else if (grepl("HAVER E/OU TER HAVIDO CONTRA|NADA HAVER E/OU TER HAVIDO CONTRA", linha)) {
      cai <- linha
      cat("Encontrado CAI:", cai, "\n")
    } else if (grepl("^\\d{5}-\\d{2}\\.\\d{4}", linha)) {
      # Quando encontrar um novo processo, salve o anterior (se houver)
      if (!is.null(processo_atual)) {
        processos <- append(processos, list(
          processo = processo_atual,
          sentenca = sentenca_atual,
          enquadramento = enquadramento_atual,
          medida_social = medida_social_atual
        ))
        cat("Salvo processo anterior.\n")
      }
      # Iniciar um novo processo
      processo_atual <- linha
      sentenca_atual <- NULL
      enquadramento_atual <- vector("list", 0)
      medida_social_atual <- vector("list", 0)
      cat("Encontrado Processo:", processo_atual, "\n")
    } else if (grepl("SENTENÇA\\s*:", linha)) {
      sentenca_atual <- gsub("SENTENÇA\\s*:", "", linha)
      cat("Encontrada Sentença:", sentenca_atual, "\n")
    } else if (grepl("ART\\.", linha)) {
      enquadramento_atual <- append(enquadramento_atual, linha)
      cat("Encontrado Enquadramento:", linha, "\n")
    } else if (grepl("PRESTAÇÃO SERVIÇO|LIBERDADE ASSISTIDA|ADVERTÊNCIA", linha)) {
      medida_social_atual <- append(medida_social_atual, linha)
      cat("Encontrada Medida Social:", linha, "\n")
    }
  }

  # Salvar o último processo (se houver)
  if (!is.null(processo_atual)) {
    processos <- append(processos, list(
      processo = processo_atual,
      sentenca = sentenca_atual,
      enquadramento = enquadramento_atual,
      medida_social = medida_social_atual
    ))
    cat("Salvo último processo.\n")
  }

  # Caso específico: "NADA HAVER E/OU TER HAVIDO CONTRA"
  if (!is.null(cai) && grepl("NADA HAVER E/OU TER HAVIDO CONTRA", cai)) {
    dados <- data.frame(
      nome = nome,
      cpf = NA,
      rg = NA,
      nome_pai = NA,
      nome_mae = NA,
      cai = cai,
      processo = NA,
      sentenca = NA,
      stringsAsFactors = FALSE
    )
    cat("Tratado caso 'NADA HAVER E/OU TER HAVIDO CONTRA'.\n")
    return(dados)
  }

  # Criar um dataframe com os dados extraídos
  dados <- data.frame(
    nome = character(),
    cpf = character(),
    rg = character(),
    nome_pai = character(),
    nome_mae = character(),
    cai = character(),
    processo = character(),
    sentenca = character(),
    stringsAsFactors = FALSE
  )

  # Adicionar uma linha para cada processo
  for (proc in processos) {
    # Determinar o número máximo de colunas para enquadramento e medida social
    max_cols <- max(length(proc$enquadramento), length(proc$medida_social))

    # Criar nomes de colunas dinâmicos
    enquadramento_cols <- paste0("Enquadramento_", sprintf("%03d", seq_len(max_cols)))
    medida_social_cols <- paste0("medida_social_", sprintf("%03d", seq_len(max_cols)))

    # Criar um dataframe temporário para o processo atual
    temp_df <- data.frame(
      nome = nome,
      cpf = cpf,
      rg = rg,
      nome_pai = nome_pai,
      nome_mae = nome_mae,
      cai = cai,
      processo = proc$processo,
      sentenca = proc$sentenca,
      stringsAsFactors = FALSE
    )

    # Adicionar colunas de enquadramento e medida social
    for (i in seq_along(enquadramento_cols)) {
      temp_df[[enquadramento_cols[i]]] <- if (i <= length(proc$enquadramento)) proc$enquadramento[i] else NA
    }
    for (i in seq_along(medida_social_cols)) {
      temp_df[[medida_social_cols[i]]] <- if (i <= length(proc$medida_social)) proc$medida_social[i] else NA
    }

    # Combinar o dataframe temporário ao dataframe final
    dados <- bind_rows(dados, temp_df)
  }

  return(dados)
}

# Diretório contendo os arquivos PDF (pode ser alterado)
diretorio <- "/media/backup/config/pessoal/elerson/diretorio_r/estciabh/letalidade/arq_fontes/cai_2023"

# Listar todos os arquivos PDF no diretório
arquivos_pdf <- list.files(diretorio, pattern = "*.pdf", full.names = TRUE)

# Verificar se há arquivos PDF no diretório
if (length(arquivos_pdf) == 0) {
  stop("Nenhum arquivo PDF encontrado no diretório especificado.")
}

cat("Arquivos PDF encontrados:\n")
print(arquivos_pdf)

# Testar a função extrair_informacoes em um único arquivo
caminho_teste <- arquivos_pdf[1] # Primeiro arquivo PDF
print(paste("Testando arquivo:", caminho_teste))

# Extrair informações do primeiro arquivo
dados_teste <- extrair_informacoes(caminho_teste)
print("Dados extraídos do primeiro arquivo:")
print(dados_teste)

# Processar todos os arquivos PDF e combinar os resultados em um único data frame
dadoslet <- lapply(arquivos_pdf, extrair_informacoes) %>%
  bind_rows()

# Exibir o dataframe final
print("Dataframe final:")
print(dadoslet)

# Salvar o dataframe em um arquivo CSV (opcional)
write.csv(dadoslet, file = "dadoslet.csv", row.names = FALSE)
