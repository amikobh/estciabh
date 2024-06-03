#########################################################################################################
#pdf(file="TABELA_002_incidencia_comparada_alternativa.pdf", width = 5, height = 7, title = "INCIDENCIA COMPARADA")
setwd(file.path("~/diretorio_r/estciabh/img_nao_apagar/"))

TABELA <- read.csv("tabela.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

#TABELA GT
#TABELA GT

#########################################################################################################
dir.create(file.path("~/diretorio_r/estciabh", "imagens"))
#########################################################################################################
#Atos infracionais
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))

incidencia_comparada %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[1,],": Incidência Atos Infracionais, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"), " e ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

   tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

    tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(incidencia_comparada)))%>%

   tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1,4)))%>%

  tab_style(
    style = cell_text(color = "red", align = "right"),
    locations = cells_body(
      columns = c(4),
      rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[1,].png", expand = 10)

#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

incidencia_comparada %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[1,],": Incidência Atos Infracionais, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"), " e ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(incidencia_comparada)))%>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1,4)))%>%

  tab_style(
    style = cell_text(color = "red", align = "right"),
    locations = cells_body(
      columns = c(4),
      rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave("tab_1.docx")
#########################################################################################################
#Perfil dos adolescentes atendidos
#########################################################################################################
df_snr_regional_residencia %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[2,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(df_snr_regional_residencia))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[2,].png", expand = 10)
#########################################################################################################
#Mandados de busca e apreensão cumpridos e encaminhados
#########################################################################################################
#########################################################################################################
#TRATAMENTO MBA
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


total_MBA_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[3,],": Quantidade de MBAs Cumpridos, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(total_MBA_gt))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[3,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_snr_regional_residencia_MBA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[4,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "MBAs") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(df_snr_regional_residencia_MBA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[4,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


MOTIVO_MBA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[5,],": Motivo de expedição do MBA, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "MBAs") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(MOTIVO_MBA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[5,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_ato_MBA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[6,],": Atos infracionais atribuídos aos adolescentes encaminhados por MBA, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_ato_MBA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[6,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT

#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_regional_ATO_banco_MBA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[7,],": Regional cometimento ato infracional, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "MBAs") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(df_regional_ATO_banco_MBA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[7,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_DIA_SEMANA_banco_MBA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[8,],": Dia da semana, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "MBAs") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(df_DIA_SEMANA_banco_MBA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[8,].png", expand = 10)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO_HOMICIDIO
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


incidencia_HOMICIDIO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[9,],": Incidência homicídio, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(incidencia_HOMICIDIO_gt))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[9,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_dia_semana_banco_HOMICIDIO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[10,],": Dia da semana, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "HOMICÍDIO") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_dia_semana_banco_HOMICIDIO_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[10,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_regional_banco_HOMICIDIO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[11,],": Regional, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "HOMICÍDIO") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_regional_banco_HOMICIDIO_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[11,].png", expand = 10)

#########################################################################################################
#TRATAMENTO_HOMICIDIO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO_ROUBO
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


incidencia_ROUBO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[12,],": Incidência roubo, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(incidencia_ROUBO_gt))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[12,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_dia_semana_banco_ROUBO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[13,],": Dia da semana, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "ROUBO") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_dia_semana_banco_ROUBO_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[13,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_regional_banco_ROUBO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[14,],": Regional, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "ROUBO") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_regional_banco_ROUBO_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[14,].png", expand = 10)

#########################################################################################################
#TRATAMENTO_ROUBO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO_FURTO
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


incidencia_FURTO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[15,],": Incidência furto, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(incidencia_FURTO_gt))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[15,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_dia_semana_banco_FURTO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[16,],": Dia da semana, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "FURTO") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_dia_semana_banco_FURTO_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[16,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_regional_banco_FURTO_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[17,],": Regional, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "FURTO") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_regional_banco_FURTO_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[17,].png", expand = 10)

#########################################################################################################
#TRATAMENTO_FURTO FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO_USO_DE_DROGAS
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


incidencia_USO_DE_DROGAS_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[18,],": Incidência posse de drogas para uso pessoal, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(incidencia_USO_DE_DROGAS_gt))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[18,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_dia_semana_banco_USO_DE_DROGAS_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[19,],": Dia da semana, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "POSSE DE DROGAS PARA USO PESSOAL") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_dia_semana_banco_USO_DE_DROGAS_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[19,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_regional_banco_USO_DE_DROGAS_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[20,],": Regional, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "POSSE DE DROGAS PARA USO PESSOAL") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_regional_banco_USO_DE_DROGAS_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[20,].png", expand = 10)

#########################################################################################################
#TRATAMENTO_USO_DE_DROGAS FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO_TRAFICO_DE_DROGAS
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


incidencia_TRAFICO_DE_DROGAS_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[21,],": Incidência tráfico de drogas, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(incidencia_TRAFICO_DE_DROGAS_gt))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[21,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_dia_semana_banco_TRAFICO_DE_DROGAS_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[22,],": Dia da semana, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "TRÁFICO DE DROGAS") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_dia_semana_banco_TRAFICO_DE_DROGAS_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[22,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


df_regional_banco_TRAFICO_DE_DROGAS_gt %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[23,],": Regional, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "TRÁFICO DE DROGAS") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center", size = "small")),
    locations = cells_body(rows = nrow(df_regional_banco_TRAFICO_DE_DROGAS_gt)))%>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(color = "red", align = "right"),
  # locations = cells_body(
  #  columns = c(4),
  #  rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  cols_align(align = "center",
             columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))


  gtsave(
    "TABELA[23,].png", expand = 10)

#########################################################################################################
#TRATAMENTO_TRAFICO_DE_DROGAS FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


RACA_COR_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[24,],": Raça/Cor, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(RACA_COR_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  # tab_style(
  #  style = cell_text(align = "center"),
  #  locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  #  style = cell_text(align = "center"),
  #  locations = cells_body(
  #   columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[24,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


ESTADO_CIVIL_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[25,],": Estado Civil, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(ESTADO_CIVIL_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[25,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


POSSUI_FILHOS_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[26,],": Adolescente Pai ou Mãe, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(POSSUI_FILHOS_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[26,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


ESTA_GRAVIDA_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[27,],": Adolescente Grávida, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(ESTA_GRAVIDA_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[27,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


POSSUI_DOC_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[28,],": Documentação, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(POSSUI_DOC_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[28,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[29,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(SERIE_ATUAL_OU_ULTIMA_CURSADA_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[29,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


NATUREZA_ESCOLA_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[30,],": Natureza da Escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(NATUREZA_ESCOLA_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[30,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


TRABALHA_ATUALMENTE_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[31,],": Trabalho Atual, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(TRABALHA_ATUALMENTE_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[31,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


RENDA_MENSAL_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[32,],": Renda Mensal, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(RENDA_MENSAL_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[32,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


RENDA_FAMILIAR_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[33,],": Renda Familiar, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(RENDA_FAMILIAR_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[33,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


TIPO_MORADIA_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[34,],": Tipo de Moradia, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(TIPO_MORADIA_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[34,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


NATUREZA_MORADIA_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[35,],": Natureza da Propriedade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(NATUREZA_MORADIA_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[35,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


DROGAS_USO_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[36,],": Uso de Drogas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(DROGAS_USO_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[36,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


BANCO_MEDIDAS_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[37,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(BANCO_MEDIDAS_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[37,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))

soma_decisoes %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[38,],": Total de Decisões, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(soma_decisoes))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[38,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


so_decisao_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[39,],": Decisão em Audiência Preliminar, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(so_decisao_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[39,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


tempo_medio_decisao %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[40,],": Tempo médio das decisões em audiências preliminares, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(tempo_medio_decisao))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1))) %>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

tab_options(
  data_row.padding = px(1),
  table.font.size = 12,
  #heading.title.font.size = 20,
  heading.align = "center",
  #heading.title.font.size = "small",
  heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[40,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


intervalo_decisao %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[41,],": Tempo das decisões em audiências preliminares, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(intervalo_decisao))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[41,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


so_sentenca_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[42,],": Sentenças, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(so_sentenca_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[42,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


tempo_medio_sentenca %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[43,],": Tempo médio das decisões após as audiências preliminares, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(tempo_medio_sentenca)))%>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

tab_options(
  data_row.padding = px(1),
  table.font.size = 12,
  #heading.title.font.size = 20,
  heading.align = "center",
  #heading.title.font.size = "small",
  heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[43,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


intervalo_sentenca %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[44,],": Tempo das decisões após audiências preliminares, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(intervalo_sentenca))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[44,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO_TRAFICO_DE_DROGAS
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_ESCOLA_total_casos %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[45,],": Quantidade de casos encaminhados, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Ato infracional nas escolas") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(banco_ESCOLA_total_casos))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[45,].png", expand = 10)
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


ESCOLARIDADE_banco_escola_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[46,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Ato infracional nas escolas") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(ESCOLARIDADE_banco_escola_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[46,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_ESCOLA_incidencia %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[47,],": Incidência atos infracionais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Ato infracional nas escolas") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_ESCOLA_incidencia))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[47,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_ESCOLA_decisao %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[48,],": Decisão, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Ato infracional nas escolas") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_ESCOLA_decisao))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[48,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_ESCOLA_vitima %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[49,],": Vítima, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Ato infracional nas escolas") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_ESCOLA_vitima))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[49,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_ESCOLA_regional_residencia %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[50,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Ato infracional nas escolas") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_ESCOLA_regional_residencia))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[50,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_ESCOLA_regional_ato %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[51,],": Regional da escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Ato infracional nas escolas") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_ESCOLA_regional_ato))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[51,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO_TRAFICO_DE_DROGAS
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_total_casos %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[52,],": Quantidade de casos encaminhados, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_total_casos))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[52,].png", expand = 10)
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_raca_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[53,],": Raça/Cor, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_raca_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[53,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))

banco_JR_escolaridade_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[54,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_escolaridade_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[54,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_natureza_escola %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[55,],": Natureza da Escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_natureza_escola))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[55,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_trabalho_atual %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[56,],": Trabalho atual, Belo Horizonte, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_trabalho_atual))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[56,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_natureza_trabalho %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[57,],": Natureza do Trabalho, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_natureza_trabalho))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[57,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_renda_mensal_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[58,],": Renda Mensal, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_renda_mensal_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[58,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_estado_civil_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[59,],": Estado Civil, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_estado_civil_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[59,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_estado_civil_pais_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[60,],": Estado civil dos pais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_estado_civil_pais_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[60,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_uso_drogas_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[61,],": Uso de Drogas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_uso_drogas_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[61,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_medidaspro_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[62,],": Medidas protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_medidaspro_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[62,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_decisao_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[63,],": Medidas socioeducativas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_decisao_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[63,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_JR_incidencia %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[64,],": Incidência atos infracionais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Justiça Restaurativa") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_JR_incidencia))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[64,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO CEDIPRO
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_encaminhamento_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[65,],": Total de casos encaminhados, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_encaminhamento_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[65,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_curso_adolescente_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[66,],": Cursos, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos matriculados") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_curso_adolescente_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[66,].png", expand = 10)



#########################################################################################################
#tratamento banco_matriculados_CEDIPRO
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[67,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos matriculados") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[67,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[68,],": Regional Residencial, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos matriculados") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[68,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


INCIDENCIA_banco_matriculados_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[69,],": Incidência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos matriculados") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(INCIDENCIA_banco_matriculados_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[69,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


REGIONAL_ATO_banco_matriculados_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[70,],": Regional Ato, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos matriculados") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(REGIONAL_ATO_banco_matriculados_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[70,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


PROTETIVAS_banco_matriculados_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[71,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos matriculados") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(PROTETIVAS_banco_matriculados_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[71,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


DECISAO_banco_matriculados_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[72,],": Decisão, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos matriculados") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(DECISAO_banco_matriculados_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[72,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#tratamento banco_matriculados_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#tratamento banco_desistencia_CEDIPRO
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[73,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos desistentes") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[73,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[74,],": Regional Residencial, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos desistentes") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[74,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


INCIDENCIA_banco_desistencia_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[75,],": Incidência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos desistentes") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(INCIDENCIA_banco_desistencia_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[75,].png", expand = 10)

#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


REGIONAL_ATO_banco_desistencia_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[76,],": Regional Ato, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos desistentes") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(REGIONAL_ATO_banco_desistencia_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[76,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


PROTETIVAS_banco_desistencia_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[77,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos desistentes") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(PROTETIVAS_banco_desistencia_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[77,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


DECISAO_banco_desistencia_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[78,],": Decisão, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: alunos desistentes") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(DECISAO_banco_desistencia_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[78,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#tratamento banco_desistencia_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#tratamento banco_curso_parente_CEDIPRO
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_curso_parente_CEDIPRO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[79,],": Cursos, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Projeto CEDIPRO: parentes encaminhados") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_curso_parente_CEDIPRO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:2)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[79,].png", expand = 10)

#########################################################################################################
#########################################################################################################
#tratamento banco_curso_parente_CEDIPRO FIM
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


CAUSA_JURIDICA_let_TOTAL %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[80,],": Letalidade por causas externas, Belo Horizonte e municípios limítrofes, 2021 e ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(CAUSA_JURIDICA_let_TOTAL))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:5)))%>%

  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body(columns = c(1)))%>%

  tab_style(
    style = cell_text(align = "left"),
    locations = cells_column_labels(columns = c(1)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:5)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[80,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


soma_intervalo_idade_HOMICIDIO_let_TOTAL %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[81,],": Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")))) ,
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(soma_intervalo_idade_HOMICIDIO_let_TOTAL))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:5)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[81,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


soma_intervalo_idade_IGNORADA_let_TOTAL %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[82,],": Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(soma_intervalo_idade_IGNORADA_let_TOTAL))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:5)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[82,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


tab_letalidade_geral_12_20_HOMICIDIO %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[83,],": Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(tab_letalidade_geral_12_20_HOMICIDIO))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:5)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:5)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[83,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


tab_letalidade_geral_12_20_IGNORADA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[84,],": Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(tab_letalidade_geral_12_20_IGNORADA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:6)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:6)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[84,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_SEXO_hom_ign_LETALIDADE %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[85,],": Sexo dos adolescentes e jovens vitimados por homicídio e causa ignorada, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_SEXO_hom_ign_LETALIDADE))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[85,].png", expand = 10)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_COR_hom_ign_LETALIDADE %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[86,],": Raça/cor dos adolescentes e jovens vitimados por homicídio e causa ignorada, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_COR_hom_ign_LETALIDADE))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[86,].png", expand = 10)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_RACA_COR_HOMICIDIO_let %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[87,],": Raça/cor, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_RACA_COR_HOMICIDIO_let))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[87,].png", expand = 10)

#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_PROCEDENCIA_LET_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[88,],": Localidade do óbito, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_PROCEDENCIA_LET_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[88,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


tempo_medio_geral_OBITO_TAB_01 %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[89,],": Tempo médio do óbito, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(tempo_medio_geral_OBITO_TAB_01))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1))) %>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

tab_options(
  data_row.padding = px(1),
  table.font.size = 12,
  #heading.title.font.size = 20,
  heading.align = "center",
  #heading.title.font.size = "small",
  heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[89,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


tempo_medio_geral_OBITO_TAB_02 %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[90,],": Tempo entre última entrada CIABH e óbito, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(tempo_medio_geral_OBITO_TAB_02))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[90,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


tempo_medio_geral_CIABH_TAB_01 %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[91,],": Tempo Médio entre a primeira e a última entrada, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(tempo_medio_geral_CIABH_TAB_01))) %>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

tab_options(
  data_row.padding = px(1),
  table.font.size = 12,
  #heading.title.font.size = 20,
  heading.align = "center",
  #heading.title.font.size = "small",
  heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[91,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


tempo_medio_geral_CIABH_TAB_02 %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[92,],": Tempo entre a primeira e a última entrada, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(tempo_medio_geral_CIABH_TAB_02))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[92,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_MEDIA_ENTRADAS_Let %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[93,],": Média de entradas, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "normal", align = "center", size = "small")),
    locations = cells_body(rows = nrow(banco_MEDIA_ENTRADAS_Let)))%>%

  tab_style(
    style = cell_text(weight = "bold", align = "center" ),
    locations = cells_column_labels(columns = c(1)))%>%

  # tab_style(
  #   style = cell_text(weight = "bold"),
  #  locations = cells_column_labels(columns = c(1,4)))%>%

  #  tab_style(
  #  style = cell_text(color = "red", align = "right"),
  #  locations = cells_body(
  #   columns = c(4),
  #   rows = `VAR%` < 0 )) %>%

tab_options(
  data_row.padding = px(1),
  table.font.size = 12,
  #heading.title.font.size = 20,
  heading.align = "center",
  #heading.title.font.size = "small",
  heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #  cols_align(align = "center",
  #   columns = 2:3) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[93,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


banco_NUMERO_ENTRADAS_let %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[94,],": Entradas, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_NUMERO_ENTRADAS_let))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[94,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


BANCO_MEDIDAS_LET_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[95,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(BANCO_MEDIDAS_LET_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[95,].png", expand = 10)
#########################################################################################################
#########################################################################################################
#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/estciabh/imagens"))


BANCO_DECISAO_LET_TABELA %>%
  gt() %>%

  tab_header(
    title = md((str_c(TABELA[96,],": Decisões aplicadas, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y")))),
    subtitle = "Letalidade") %>%

  tab_source_note("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(BANCO_DECISAO_LET_TABELA))) %>%

  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%

  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%

  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%

  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%

  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%

  # fmt_number(
  #columns = 3) %>%

  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))

  gtsave(
    "TABELA[96,].png", expand = 10)

#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
