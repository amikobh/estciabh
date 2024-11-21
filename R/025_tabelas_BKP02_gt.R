# Criar uma variável global para contar o número da tabela
numero_tabela_global <- 1

# Função para gerar tabela com base em qualquer dataframe
gerar_tabela <- function(df, titulo_base, subtitulo = NULL,
                         aplicar_estilo_condicional = FALSE,
                         aplicar_estilo_coluna_1 = FALSE,
                         aplicar_estilo_coluna_2 = FALSE,
                         aplicar_estilo_coluna_4 = FALSE,
                         aplicar_estilo_coluna_6 = FALSE,
                         texto_fonte = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") {

  # Usar o número da tabela global
  numero_tabela <- numero_tabela_global

  # Incrementar o número da tabela global para a próxima chamada
  assign("numero_tabela_global", numero_tabela_global + 1, envir = .GlobalEnv)

  # Verificar a quantidade de colunas no dataframe
  n_colunas <- ncol(df)

  # Gerar o título no formato "TABELA 7: Regional de Residência"
  titulo <- str_c("TABELA ", numero_tabela, ": ", titulo_base)

  # Iniciar a tabela base
  tabela <- df %>%
    gt() %>%

    # Definir título e subtítulo (se fornecido)
    tab_header(
      title = md(titulo),
      subtitle = if (!is.null(subtitulo)) subtitulo else ""
    ) %>%

    # Fonte da tabela
    tab_source_note(texto_fonte) %>%

    # Estilo global para toda a tabela
    tab_options(
      data_row.padding = px(1),
      table.font.size = 12,
      heading.align = "center",
      heading.subtitle.font.size = "small"
    ) %>%

    # Estilo para os cabeçalhos (todas as colunas em negrito)
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = 1:n_colunas)
    )

  # Estilo para a última linha (em negrito)
  tabela <- tabela %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = nrow(df))
    )

  # Estilo adicional para o cabeçalho da primeira coluna, se o parâmetro estiver ativo
  if (aplicar_estilo_coluna_1 && n_colunas >= 1) {
    tabela <- tabela %>%
      tab_style(
        style = cell_text(weight = "bold", align = "center"),
        locations = cells_column_labels(columns = c(1))
      ) %>%
      tab_style(
        style = cell_text(weight = "normal", align = "center"),
        locations = cells_body(columns = c(1))
      )
  }

  # Estilo adicional para o cabeçalho das colunas 1 e 2, com alinhamento distinto
  if (aplicar_estilo_coluna_2 && n_colunas >= 2) {
    tabela <- tabela %>%
      tab_style(
        style = cell_text(weight = "bold", align = "left"),
        locations = cells_column_labels(columns = c(1))
      ) %>%
      tab_style(
        style = cell_text(weight = "bold", align = "right"),
        locations = cells_column_labels(columns = c(2))
      )

    # Garantir que apenas as linhas anteriores à última sejam normalizadas
    tabela <- tabela %>%
      tab_style(
        style = cell_text(weight = "normal"),
        locations = cells_body(rows = 1:(nrow(df) - 1))
      )
  }

  # Estilo adicional para tabelas com exatamente 4 colunas
  if (aplicar_estilo_coluna_4 && n_colunas == 4) {
    tabela <- tabela %>%
      tab_style(
        style = cell_text(weight = "bold", color = "black"),  # Texto preto em negrito
        locations = cells_column_labels(columns = everything())
      )
  }

  # Estilo adicional para tabelas com exatamente 6 colunas
  if (aplicar_estilo_coluna_6 && n_colunas == 6) {
    tabela <- tabela %>%
      tab_style(
        style = cell_text(weight = "bold", color = "black"),  # Texto preto em negrito
        locations = cells_column_labels(columns = everything())
      )
  }

  # Estilo condicional, se o parâmetro estiver ativo
  if (aplicar_estilo_condicional) {
    if ("VAR%" %in% names(df)) {
      tabela <- tabela %>%
        tab_style(
          style = cell_text(color = "red", align = "right"),
          locations = cells_body(
            columns = c("VAR%"),
            rows = `VAR%` < 0
          )
        )
    }
  }


  # Gerar o caminho de saída com o formato "TABELA[6,].png"
  nome_arquivo <- str_c("TABELA[", numero_tabela, ",].png")

  # Salvar a tabela gerada
  gtsave(tabela, nome_arquivo, expand = 10)

  # Retornar o nome do arquivo salvo
  return(nome_arquivo)
}

##################################################################################################################
##################################################################################################################

#criar diretorio
dir.create(file.path("~/diretorio_r/estciabh", "imagens"))
setwd(file.path("~/diretorio_r/estciabh/imagens"))
# Gerar a tabela com título numerado e salvar, com o estilo condicional e a primeira coluna com estilo (negrito, centralizado)


##################################################################################################################
##################################################################################################################


gerar_tabela(
  df = incidencia_comparada,
  titulo_base = "Incidência Atos Infracionais, 2022 e 2023",
  subtitulo = NULL,  # Sem subtítulo
  aplicar_estilo_condicional = TRUE,  # Ativar o estilo condicional
  aplicar_estilo_coluna_1 = FALSE  # Aplicar estilo à primeira coluna
)

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_snr_regional_residencia,
  titulo_base = "Regional de Residência, 2023",
  subtitulo = NULL,  # Sem subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna

gerar_tabela(
  df = total_MBA_gt,
  titulo_base = "Quantidade de MBAs Cumpridos, 2023",
  subtitulo = NULL,  # Sem subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_snr_regional_residencia_MBA,
  titulo_base = "Regional de residência, 2023",
  subtitulo = "MBAs",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = MOTIVO_MBA,
  titulo_base = "Motivo de expedição do MBA, 2023",
  subtitulo = "MBAs",  # com subtítulo
  #texto_fonte = "Fonte: Relatório Anual",
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_regional_ATO_banco_MBA,
  titulo_base = "Regional do local de cumprimento, 2023",
  subtitulo = "MBAs",  # Sem subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_DIA_SEMANA_banco_MBA,
  titulo_base = "Dia da semana do cumprimento, 2023",
  subtitulo = "MBAs",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


##################################################################################################################
##################################################################################################################
####ATOS EM FOCO:

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = atos_em_foco,
  titulo_base = "Incidência atos em foco, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = TRUE  # Não aplicar estilo na primeira coluna
)

####ATOS EM FOCO: HOMICIDIO

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = incidencia_HOMICIDIO_gt,
  titulo_base = "Incidência homicídio, 2023",
  subtitulo = NULL,  # Sem subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_dia_semana_banco_HOMICIDIO_gt,
  titulo_base = "Dia da semana, 2023",
  subtitulo = "HOMICÍDIO",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_regional_banco_HOMICIDIO_gt,
  titulo_base = "Regional, 2023",
  subtitulo = "HOMICÍDIO",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)
##################################################################################################################
####ATOS EM FOCO: ROUBO

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = incidencia_ROUBO_gt,
  titulo_base = "Incidência ROUBO, 2023",
  subtitulo = NULL,  # Sem subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_dia_semana_banco_ROUBO_gt,
  titulo_base = "Dia da semana, 2023",
  subtitulo = "ROUBO",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_regional_banco_ROUBO_gt,
  titulo_base = "Regional, 2023",
  subtitulo = "ROUBO",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
####ATOS EM FOCO: FURTO

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = incidencia_FURTO_gt,
  titulo_base = "Incidência FURTO, 2023",
  subtitulo = NULL,  # Sem subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_dia_semana_banco_FURTO_gt,
  titulo_base = "Dia da semana, 2023",
  subtitulo = "FURTO",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_regional_banco_FURTO_gt,
  titulo_base = "Regional, 2023",
  subtitulo = "FURTO",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
####ATOS EM FOCO: USO_DE_DROGAS

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = incidencia_USO_DE_DROGAS_gt,
  titulo_base = "Incidência posse de drogas para uso pessoal, 2023",
  subtitulo = NULL,  # Sem subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_dia_semana_banco_USO_DE_DROGAS_gt,
  titulo_base = "Dia da semana, 2023",
  subtitulo = "POSSE DE DROGAS PARA USO PESSOAL",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_regional_banco_USO_DE_DROGAS_gt,
  titulo_base = "Regional, 2023",
  subtitulo = "POSSE DE DROGAS PARA USO PESSOAL",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


####ATOS EM FOCO: TRAFICO_DE_DROGAS

# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = incidencia_TRAFICO_DE_DROGAS_gt,
  titulo_base = "Incidência tráfico de drogas, 2023",
  subtitulo = NULL,  # Sem subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_dia_semana_banco_TRAFICO_DE_DROGAS_gt,
  titulo_base = "Dia da semana, 2023",
  subtitulo = "TRÁFICO DE DROGAS",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)


# Gerar outra tabela com título numerado 8 e salvar, sem o estilo condicional nem o estilo na primeira coluna
gerar_tabela(
  df = df_regional_banco_TRAFICO_DE_DROGAS_gt,
  titulo_base = "Regional, 2023",
  subtitulo = "TRÁFICO DE DROGAS",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Dados amostrais socioeconomicos:


gerar_tabela(
  df = RACA_COR_TABELA,
  titulo_base = "Raça/Cor, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = ESTADO_CIVIL_TABELA,
  titulo_base = "Estado Civil, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = POSSUI_FILHOS_TABELA,
  titulo_base = "Adolescente Pai ou Mãe, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = ESTA_GRAVIDA_TABELA,
  titulo_base = "Adolescente Grávida, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = POSSUI_DOC_TABELA,
  titulo_base = "Documentação, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA,
  titulo_base = "Escolaridade, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = NATUREZA_ESCOLA_TABELA,
  titulo_base = "Natureza da Escola, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = TRABALHA_ATUALMENTE_TABELA,
  titulo_base = "Trabalho Atual, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = RENDA_MENSAL_TABELA,
  titulo_base = "Renda Mensal, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = RENDA_FAMILIAR_TABELA,
  titulo_base = "Renda Familiar, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = TIPO_MORADIA_TABELA,
  titulo_base = "Tipo de Moradia, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = NATUREZA_MORADIA_TABELA,
  titulo_base = "Natureza da Propriedade, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = DROGAS_USO_TABELA,
  titulo_base = "Uso de Drogas, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Medidasd Protetivas



gerar_tabela(
  df = BANCO_MEDIDAS_TABELA,
  titulo_base = "Medidas Protetivas, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Decisões

gerar_tabela(
  df = soma_decisoes,
  titulo_base = "Medidas Protetivas, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = so_decisao_TABELA,
  titulo_base = "Decisões em audiências preliminares, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = tempo_medio_decisao,
  titulo_base = "Tempo médio das decisões em audiências preliminares, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = intervalo_decisao,
  titulo_base = "Decisões em audiências preliminares, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Decisões após audiências preliminares

gerar_tabela(
  df = so_sentenca_TABELA,
  titulo_base = "Decisões após as audiências preliminares, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = tempo_medio_sentenca,
  titulo_base = "Tempo médio das decisões após as audiências preliminares, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = intervalo_sentenca,
  titulo_base = "Tempo das decisões após audiências preliminares, 2023",
  subtitulo = NULL,  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Atos infracionais nas escolas de Belo Horizonte

gerar_tabela(
  df = banco_ESCOLA_total_casos,
  titulo_base = "Quantidade de casos encaminhados, 2023",
  subtitulo = "Ato infracional nas escolas",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = ESCOLARIDADE_banco_escola_TABELA,
  titulo_base = "Escolaridade, 2023",
  subtitulo = "Ato infracional nas escolas",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_ESCOLA_incidencia,
  titulo_base = "Incidência atos infracionais, 2023",
  subtitulo = "Ato infracional nas escolas",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_ESCOLA_decisao,
  titulo_base = "Decisão, 2023",
  subtitulo = "Ato infracional nas escolas",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_ESCOLA_vitima,
  titulo_base = "Vítima, 2023",
  subtitulo = "Ato infracional nas escolas",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_ESCOLA_regional_residencia,
  titulo_base = "Regional de Residência, 2023",
  subtitulo = "Ato infracional nas escolas",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_ESCOLA_regional_ato,
  titulo_base = "Regional da escola, 2023",
  subtitulo = "Ato infracional nas escolas",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Justiça restaurativa

gerar_tabela(
  df = banco_JR_total_casos,
  titulo_base = "Quantidade de casos encaminhados, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_raca_TABELA,
  titulo_base = "Raça/Cor, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_escolaridade_TABELA,
  titulo_base = "Escolaridade, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_natureza_escola,
  titulo_base = "Natureza da Escola, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_trabalho_atual,
  titulo_base = "Trabalho atual, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_natureza_trabalho,
  titulo_base = "Natureza do Trabalho, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_renda_mensal_TABELA,
  titulo_base = "Renda Mensal, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_estado_civil_TABELA,
  titulo_base = "Estado Civil, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_estado_civil_pais_TABELA,
  titulo_base = "Estado civil dos pais, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_JR_uso_drogas_TABELA,
  titulo_base = "Uso de Drogas, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = banco_JR_medidaspro_TABELA,
  titulo_base = "Medidas protetivas, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = banco_JR_decisao_TABELA,
  titulo_base = "Medidas socioeducativas, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = banco_JR_incidencia,
  titulo_base = "Incidência atos infracionais, 2023",
  subtitulo = "Justiça Restaurativa",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Projetos socioeducativos
# Projeto CEDIPRO


gerar_tabela(
  df = banco_encaminhamento_CEDIPRO,
  titulo_base = "Total de casos encaminhados, 2023",
  subtitulo = "Projeto CEDIPRO",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Projeto CEDIPRO: alunos matriculados
gerar_tabela(
  df = banco_curso_adolescente_CEDIPRO,
  titulo_base = "Cursos, 2023",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA,
  titulo_base = "Escolaridade, 2023",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO,
  titulo_base = "Regional Residencial, 2023",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = INCIDENCIA_banco_matriculados_CEDIPRO,
  titulo_base = "Incidência, 2023",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = REGIONAL_ATO_banco_matriculados_CEDIPRO,
  titulo_base = "Regional Ato, 2023",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = PROTETIVAS_banco_matriculados_CEDIPRO,
  titulo_base = "Medidas Protetivas, 2023",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = DECISAO_banco_matriculados_CEDIPRO,
  titulo_base = "Decisão, 2023",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Projeto CEDIPRO: alunos desistentes

gerar_tabela(
  df = ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA,
  titulo_base = "Escolaridade, 2023",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO,
  titulo_base = "Regional Residencial, 2023",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = INCIDENCIA_banco_desistencia_CEDIPRO,
  titulo_base = "Incidência, 2023",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = REGIONAL_ATO_banco_desistencia_CEDIPRO,
  titulo_base = "Regional Ato, 2023",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = PROTETIVAS_banco_desistencia_CEDIPRO,
  titulo_base = "Medidas Protetivas, 2023",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = DECISAO_banco_desistencia_CEDIPRO,
  titulo_base = "Decisão, 2023",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Projeto CEDIPRO: alunos desistentes


gerar_tabela(
  df = banco_curso_parente_CEDIPRO,
  titulo_base = "Cursos, 2023",
  subtitulo = "Projeto CEDIPRO: parentes encaminhados",  # com subtítulo
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_2 = FALSE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################
# Letalidade

#Letalidade de adolescentes e jovens por causas externas em Belo Horizonte e municípios limítrofes

gerar_tabela(
  df = CAUSA_JURIDICA_let_TOTAL,
  titulo_base = "Letalidade por causas externas, Belo Horizonte e municípios limítrofes, 2021 e 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_4 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = soma_intervalo_idade_HOMICIDIO_let_TOTAL,
  titulo_base = "Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, 2021 e 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_4 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = soma_intervalo_idade_IGNORADA_let_TOTAL,
  titulo_base = "Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, 2021 e 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_4 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = tab_letalidade_geral_12_20_HOMICIDIO,
  titulo_base = "Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, 2018 a 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_6 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = tab_letalidade_geral_12_20_IGNORADA,
  titulo_base = "Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, 2018 a 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_6 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = banco_SEXO_hom_ign_LETALIDADE,
  titulo_base = "Sexo dos adolescentes e jovens vitimados por homicídio e causa ignorada, Belo Horizonte e municípios limítrofes, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_6 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_COR_hom_ign_LETALIDADE,
  titulo_base = "Raça/cor dos adolescentes e jovens vitimados por homicídio e causa ignorada, Belo Horizonte e municípios limítrofes, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_6 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_RACA_COR_HOMICIDIO_let,
  titulo_base = "Raça/cor, Belo Horizonte e municípios limítrofes, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_6 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_PROCEDENCIA_LET_TABELA,
  titulo_base = "Localidade do óbito, Belo Horizonte e municípios limítrofes, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_6 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = tempo_medio_geral_OBITO_TAB_01,
  titulo_base = "Tempo médio do óbito, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################
##################################################################################################################


gerar_tabela(
  df = tempo_medio_geral_OBITO_TAB_02,
  titulo_base = "Tempo entre última entrada CIABH e óbito, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = tempo_medio_geral_CIABH_TAB_01,
  titulo_base = "Tempo Médio entre a primeira e a última entrada, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = tempo_medio_geral_CIABH_TAB_02,
  titulo_base = "Tempo entre a primeira e a última entrada, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_MEDIA_ENTRADAS_Let,
  titulo_base = "Média de entradas, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = banco_NUMERO_ENTRADAS_let,
  titulo_base = "Entradas, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################


gerar_tabela(
  df = BANCO_MEDIDAS_LET_TABELA,
  titulo_base = "Medidas Protetivas, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################

gerar_tabela(
  df = BANCO_DECISAO_LET_TABELA,
  titulo_base = "Decisões aplicadas, 2022",
  subtitulo = "Letalidade",  # com subtítulo
  texto_fonte = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  #aplicar_estilo_condicional = FALSE,  # Estilo condicional desativado
  #aplicar_estilo_coluna_1 = TRUE  # Não aplicar estilo na primeira coluna
)

##################################################################################################################





