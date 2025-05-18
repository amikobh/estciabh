# Carregar pacotes necessários
library(ggplot2)
library(cowplot)
library(scales)
library(forcats)

# Variável global para o contador de gráficos
contador_graficos <- 1

# Função para criar e salvar gráficos automaticamente
criar_e_salvar_grafico <- function(tipo, df = NULL, titulo, subtitulo = "", caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
                                   legenda = NULL, x_col = NULL, y_col = NULL,
                                   group_col = NULL, fill_col = NULL, label_col = NULL,
                                   paleta = "Set1", diretorio = "~/diretorio_r/estciabh/imagens", graficos = NULL, ...) {

  # Criar o diretório se não existir
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
  }

  # Usar o contador global para gerar o nome do arquivo
  nome_arquivo <- sprintf("GRAFICO[%d,].png", contador_graficos)
  salvar <- file.path(diretorio, nome_arquivo)

  # Adicionar o contador ao título, no formato "GRÁFICO 1:"
  titulo_completo <- sprintf("GRÁFICO %d: %s", contador_graficos, titulo)

  grafico <- NULL

  # Criar gráficos compostos
  if (tipo == "composto") {
    if (is.null(graficos) || length(graficos) < 2) {
      stop("Para gráficos compostos, forneça uma lista com pelo menos dois gráficos.")
    }

    # Criar gráficos compostos usando cowplot::plot_grid
    grafico_composto <- plot_grid(plotlist = graficos, ncol = 2)

    # Criar os títulos e subtítulos
    titulo_plot <- ggdraw() + draw_label(titulo_completo, fontface = "bold", hjust = 0.5, size = 14)
    subtitulo_plot <- ggdraw() + draw_label(subtitulo, hjust = 0.5, size = 12)

    # Ajustar a combinação do título, subtítulo e gráfico composto
    grafico <- plot_grid(titulo_plot, subtitulo_plot, grafico_composto, ncol = 1, rel_heights = c(0.0, 0.1, 1))

    # Adicionar a legenda de rodapé (caption)
    grafico <- ggdraw(add_sub(grafico, caption, size = 10, vjust = 1))  # Ajuste no vjust para posição correta do caption

    # Garantir que o título e subtítulo não se sobreponham e o gráfico não fique muito comprimido
    grafico <- grafico + theme(plot.margin = margin(8, 10, 8, 10))  # Ajustar as margens para evitar cortes top, right, bottom, left

  } else {
    # Outros tipos de gráfico
    if (is.null(x_col)) {
      if (tipo == "barra_horizontal") {
        x_col <- deparse(substitute(df))
      }
    }

    # Gráfico de Linha
    if (tipo == "linha") {
      grafico <- ggplot(df, aes_string(x = x_col, y = y_col, group = group_col)) +
        geom_line(aes_string(color = group_col), linewidth = 1.2) +
        geom_point(aes_string(color = group_col), size = 1.5) +
        scale_color_brewer("", palette = paleta) +
        geom_label_repel(aes_string(label = y_col), box.padding = unit(0.5, "lines"),
                         max.overlaps = Inf, label.size = 0.1, size = 3) +
        labs(title = titulo_completo, subtitle = subtitulo, caption = caption, x = "", y = NULL) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
              plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 12),  # Subtítulo sem negrito
              plot.caption = element_text(hjust = 0.5, size = 12),
              legend.position = "bottom")

    } else if (tipo == "barra_horizontal") {
      # Garantir que x_col seja um fator ou caractere
      if (!is.factor(df[[x_col]]) && !is.character(df[[x_col]])) {
        df[[x_col]] <- as.factor(df[[x_col]])
      }

      # Reorganizar os valores do eixo x com fct_reorder
      df[[x_col]] <- fct_reorder(df[[x_col]], df[[y_col]])

      # Ajustar a largura e o texto das barras
      grafico <- ggplot(df, aes_string(x = x_col, y = y_col)) +
        geom_bar(stat = "identity", fill = "#bb1e23") +
        coord_flip() +
        labs(title = titulo_completo, subtitle = subtitulo, caption = caption, x = "", y = NULL) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
              plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 12),  # Subtítulo sem negrito
              plot.caption = element_text(hjust = 0.5, size = 12)) +
        geom_text(aes_string(label = label_col), hjust = 0, nudge_x = 0.05, colour = "#bb1e23", size = 3) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.y = element_text(size = 10),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_blank(),
              plot.margin = margin(10, 10, 10, 30))  # Ajuste para evitar cortar as labels

    } else if (tipo == "barra_vertical") {
      # Argumentos adicionais para controle do título da legenda
      legend_title <- list(...)$legend_title %||% "PRIMÁRIO"  # Título da legenda

      grafico <- ggplot(df, aes_string(fill = fill_col, y = y_col, x = x_col)) +
        geom_bar(position = "dodge", stat = "identity") +
        labs(title = titulo_completo, subtitle = subtitulo, caption = caption, x = "", y = NULL, fill = legend_title) +  # Título da legenda personalizado
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 12),
          plot.caption = element_text(hjust = 0.5, size = 12),
          legend.position = "right"
        ) +
        geom_text(aes_string(label = y_col), vjust = 0, color = "red", fontface = "plain",
                  position = position_dodge(0.9), size = 3.5)
    } else if (tipo == "pizza") {
      # Argumentos adicionais para controle das labels
      lab_size <- list(...)$lab_size %||% 4  # Tamanho da fonte das labels
      lab_color <- list(...)$lab_color %||% "white"  # Cor da fonte das labels
      lab_face <- list(...)$lab_face %||% "plain"  # Estilo da fonte: "bold", "italic", "plain", etc.
      lab_pos <- list(...)$lab_pos %||% "in"  # Posição das labels: "in" ou "out"
      legend_title <- list(...)$legend_title %||% "Categorias"  # Título da legenda

      grafico <- ggpie(
        df,
        x = y_col,
        label = label_col,
        lab.pos = lab_pos,
        lab.font = list(color = lab_color, size = lab_size, face = lab_face),  # Personalização das labels
        lab.adjust = 0,
        fill = fill_col,
        color = "white",
        palette = paleta
      ) +
        theme(
          legend.position = "right",
          legend.text = element_text(size = 8, face = "bold"),
          plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 12),
          plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face = "plain", size = 12),
          plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)
        ) +
        labs(
          caption = caption,
          title = titulo_completo,
          subtitle = subtitulo,
          fill = legend_title  # Modificando o título da legenda
        )
    }
  }

  # Ajuste para garantir que o gráfico não corte nada na hora de salvar a imagem
  ggsave(salvar, plot = grafico, width = 12, height = 6, pointsize = 12, dpi = 512)

  # Incrementar o contador global
  assign("contador_graficos", contador_graficos + 1, envir = .GlobalEnv)

  return(grafico)
}

#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

# Criar gráficos sem rótulo no eixo Y, incluindo gráfico de barras verticais
#Atendimento ao adolescente
#########################################################################################################
# Gráfico de linha
criar_e_salvar_grafico(
  tipo = "linha",
  df = df_atendimento,
  titulo = "Atendimento de adolescentes, 2015 a 2023",
  subtitulo = NULL,  # Definindo o subtítulo
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "ANO",
  y_col = "QUANTIDADE",
  group_col = "TIPO"
)

# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_incidencia_geral_bkp,
  titulo = "Incidência Atos Infracionais, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_GERAL_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO" # Título da legenda,

)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_GERAL_snr_SEXO_IDADE_graf_pizza,
  titulo = "Sexo, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "SEXO",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = df_snr_regional_residencia_bkp,
  titulo = "Regional de Residência, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_MBA_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "MBAs cumpridos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)

#########################################################################################################
# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_MBA_snr_SEXO_IDADE_graf_pizza,
  titulo = "Sexo, 2024",
  subtitulo = "MBAs cumpridos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "SEXO",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = df_snr_regional_residencia_MBA_bkp,
  titulo = "Regional de Residência, 2024",
  subtitulo = "MBAs cumpridos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = MOTIVO_MBA_bkp,
  titulo = "Motivo do MBA, 2024",
  subtitulo = "MBAs cumpridos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = atos_em_foco_rmd,
  titulo = "Incidência atos em foco, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "ATO",
  y_col = "INCIDÊNCIA",
  label_col = "INCIDÊNCIA",
)


# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_HOMICIDIO_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Homicídio",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_HOMICIDIO_snr_SEXO_IDADE_graf_pizza,
  titulo = "Sexo, 2024",
  subtitulo = "Homicídio",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "SEXO",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_ROUBO_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Roubo",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_ROUBO_snr_SEXO_IDADE_graf_pizza,
  titulo = "Sexo, 2024",
  subtitulo = "Roubo",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "SEXO",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_FURTO_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Furto",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_FURTO_snr_SEXO_IDADE_graf_pizza,
  titulo = "Sexo, 2024",
  subtitulo = "Furto",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "SEXO",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_USO_DE_DROGAS_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Posse de drogas para uso pessoal",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_USO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza,
  titulo = "Sexo, 2024",
  subtitulo = "Posse de drogas para uso pessoal",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "SEXO",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Tráfico de drogas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_TRAFICO_DE_DROGAS_snr_SEXO_IDADE_graf_pizza,
  titulo = "Sexo, 2024",
  subtitulo = "Tráfico de drogas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "SEXO",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = RACA_COR_bkp,
  titulo = "Raça/Cor, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = ESTADO_CIVIL_bkp,
  titulo = "Estado Civil, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = POSSUI_FILHOS_bkp,
  titulo = "Adolescente Pai ou Mãe, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = ESTA_GRAVIDA_bkp,
  titulo = "Adolescente Grávida, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = POSSUI_DOC_bkp,
  titulo = "Documentação, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp,
  titulo = "Escolaridade, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = NATUREZA_ESCOLA_bkp,
  titulo = "Natureza da Escola, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = TRABALHA_ATUALMENTE_bkp,
  titulo = "Trabalho atual, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = RENDA_MENSAL_bkp,
  titulo = "Renda Mensal, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = RENDA_FAMILIAR_bkp,
  titulo = "Renda Familiar, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = TIPO_MORADIA_bkp,
  titulo = "Tipo de moradia, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = NATUREZA_MORADIA_bkp,
  titulo = "Natureza da Propriedade, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = DROGAS_USO_bkp,
  titulo = "Uso de drogas, 2024",
  subtitulo = "Dados socioeconômicos",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Medidas protetivas

# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = BANCO_MEDIDAS_bkp,
  titulo = "Medidas Protetivas, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Decisões

# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = so_decisao_bkp,
  titulo = "Decisões em audiências preliminares, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = intervalo_decisao_bkp,
  titulo = "Tempo das decisões em audiências preliminares, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Sentenças

# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = so_sentenca_bkp,
  titulo = "Decisões após as audiências preliminares, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = intervalo_sentenca_bkp,
  titulo = "Tempo das decisões após audiências preliminares, 2024",
  subtitulo = NULL,  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Atos infracionais nas escolas


# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_ESCOLA_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Atos infracionais nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_ESCOLA_snr_SEXO_IDADE_graf_pizza,
  titulo = "Sexo, 2024",
  subtitulo = "Atos infracionais nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "SEXO", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = ESCOLARIDADE_banco_escola_bkp,
  titulo = "Escolaridade, 2024",
  subtitulo = "Ato infracional nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_ESCOLA_incidencia_bkp,
  titulo = "Incidência atos infracionais, 2024",
  subtitulo = "Ato infracional nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)

# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_ESCOLA_primariedade_bkp,
  titulo = "Primariedade, 2024",
  subtitulo = "Atos infracionais nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "banco_ESCOLA_primariedade_bkp",
  label_col = "PERCENTUAL",
  legend_title = "PRIMÁRIO", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "black",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "out"      # Posição das labels
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_ESCOLA_decisao_bkp,
  titulo = "Decisão, 2024",
  subtitulo = "Ato infracional nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_ESCOLA_vitima_bkp,
  titulo = "Vítima, 2024",
  subtitulo = "Atos infracionais nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "banco_ESCOLA_vitima_bkp",
  label_col = "PERCENTUAL",
  legend_title = "VÍTIMA", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "black",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "out"      # Posição das labels
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_ESCOLA_regional_residencia_bkp,
  titulo = "Regional de Residência, 2024",
  subtitulo = "Ato infracional nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_ESCOLA_tipo_escola_bkp,
  titulo = "Escola, 2024",
  subtitulo = "Atos infracionais nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "banco_ESCOLA_tipo_escola_bkp",
  label_col = "PERCENTUAL",
  legend_title = "ESCOLA", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "black",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "out"      # Posição das labels,
)






# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_ESCOLA_regional_ato_bkp,
  titulo = "Regional da escola, 2024",
  subtitulo = "Ato infracional nas escolas",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)





# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_JR_snr_SEXO_IDADE_pizza,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)



# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_JR_snr_SEXO_IDADE_graf_pizza,
  titulo = "Escola, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL2",
  legend_title = "ESCOLA", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_raca_bkp,
  titulo = "Raça/Cor, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_escolaridade_bkp,
  titulo = "Escolaridade, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_natureza_escola_bkp,
  titulo = "Natureza da Escola, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_trabalho_atual_bkp,
  titulo = "Trabalho atual, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_natureza_trabalho_bkp,
  titulo = "Natureza do Trabalho, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_renda_mensal_bkp,
  titulo = "Renda Mensal, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)





# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_estado_civil_bkp,
  titulo = "Estado Civil, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)





# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_estado_civil_pais_bkp,
  titulo = "Estado civil dos pais, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_uso_drogas_bkp,
  titulo = "Uso de drogas, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_medidaspro_bkp,
  titulo = "Medidas Protetivas, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_decisao_bkp,
  titulo = "Medidas Socioeducativas, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_JR_incidencia_bkp,
  titulo = "Incidência atos infracionais, 2024",
  subtitulo = "Justiça Restaurativa",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)

# Projetos socioeducativos
# CEDIPRO

# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = banco_encaminhamento_CEDIPRO_bkp,
  titulo = "Casos encaminhados, 2024",
  subtitulo = "Projeto CEDIPRO",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "GRUPO",
  label_col = "PERCENTUAL",
  legend_title = "GRUPO", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "black",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "out"      # Posição das labels,
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_curso_adolescente_CEDIPRO_bkp,
  titulo = "Cursos, 2024",
  subtitulo = "Projeto CEDIPRO",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Alunos matriculados



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = df_snr_sexo_idade_banco_matriculados_CEDIPRO,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "idade",
  y_col = "QUANTIDADE",
  fill_col = "sexo",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = df_snr_sexo_pizza_banco_matriculados_CEDIPRO,
  titulo = "Sexo, 2024",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "sexo",
  label_col = "PERCENTUAL",
  legend_title = "SEXO", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp,
  titulo = "Escolaridade, 2024",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp,
  titulo = "Regional Residencial, 2024",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = INCIDENCIA_banco_matriculados_CEDIPRO_bkp,
  titulo = "Incidência, 2024",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp,
  titulo = "Regional Ato, 2024",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)






# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = PROTETIVAS_banco_matriculados_CEDIPRO_bkp,
  titulo = "Medidas Protetivas, 2024",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)






# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = DECISAO_banco_matriculados_CEDIPRO_bkp,
  titulo = "Decisão, 2024",
  subtitulo = "Projeto CEDIPRO: alunos matriculados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)

# Alunos desistentes



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = df_snr_sexo_idade_banco_desistencia_CEDIPRO,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "idade",
  y_col = "QUANTIDADE",
  fill_col = "sexo",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = df_snr_sexo_pizza_banco_desistencia_CEDIPRO,
  titulo = "Sexo, 2024",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "sexo",
  label_col = "PERCENTUAL",
  legend_title = "SEXO", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp,
  titulo = "Escolaridade, 2024",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp,
  titulo = "Regional Residencial, 2024",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = INCIDENCIA_banco_desistencia_CEDIPRO_bkp,
  titulo = "Incidência, 2024",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp,
  titulo = "Regional Ato, 2024",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = PROTETIVAS_banco_desistencia_CEDIPRO_bkp,
  titulo = "Medidas Protetivas, 2024",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = DECISAO_banco_desistencia_CEDIPRO_bkp,
  titulo = "Decisão, 2024",
  subtitulo = "Projeto CEDIPRO: alunos desistentes",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)

# Parentes encaminhados

# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_curso_parente_CEDIPRO_bkp,
  titulo = "Cursos, 2024",
  subtitulo = "Projeto CEDIPRO: Parentes encaminhados",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Projetos socioeducativos
# CORRE

# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = tabela_total_adls_CORRE_bkp,
  titulo = "Total de adolescentes incluídos, 2024",
  subtitulo = "Projeto CORRE LEGAL",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "MEDIDA",
  label_col = "PERCENTUAL2",
  legend_title = "MEDIDA", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)

# semiliberdade

# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tabela_atividades_semiliberdade_CORRE_bkp,
  titulo = "Atividades realizadas, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "ATIVIDADE",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = faixa_semiliberdade_atv_CORRE_bkp,
  titulo = "Frequência dos adolescentes às atividades, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "FREQUÊNCIA",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "QUANTIDADE"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tabela_dias_semiliberdade_CORRE_bkp,
  titulo = "Dias das atividades, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "DIA",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)





# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tabela_periodo_semiliberdade_CORRE_bkp,
  titulo = "Turno das atividades, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "PERÍODO",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL2"
)





# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tabela_unidade_semiliberdade_CORRE_bkp,
  titulo = "Unidade socioeducativa, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "UNIDADE",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = tabela_IDADE_SEXO_semiliberdade_CORRE_bkp,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = tabela_IDADE_SEXO_semiliberdade_CORRE_bkp1,
  titulo = "Sexo, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL",
  legend_title = "SEXO", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "black",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "out"      # Posição das labels,
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = ESCOLARIDADE_semiliberdade_CORRE_bkp,
  titulo = "Escolaridade, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = REGIONAL_RESIDENCIAL_banco_semiliberdade_CORRE_bkp,
  titulo = "Regional Residencial, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_geral_sem_concurso_semiliberdade_CORRE_bkp,
  titulo = "Incidência, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = REGIONAL_ATO_banco_semiliberdade_CORRE_bkp,
  titulo = "Regional Ato, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)






# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = SENTENCA_banco_semiliberdade_CORRE_bkp,
  titulo = "Sentença, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = PROTETIVAS_banco_semiliberdade_CORRE_bkp,
  titulo = "Medidas Protetivas, 2024",
  subtitulo = "Projeto CORRE LEGAL: semiliberdade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)

# CORRE semiliberdade FIM


# Projetos socioeducativos
# CORRE internacao

# internacao

# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tabela_atividades_internacao_CORRE_bkp,
  titulo = "Atividades realizadas, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "ATIVIDADE",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = faixa_internacao_atv_CORRE_bkp,
  titulo = "Frequência dos adolescentes às atividades, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "FREQUÊNCIA",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "QUANTIDADE"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tabela_dias_internacao_CORRE_bkp,
  titulo = "Dias das atividades, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "DIA",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)





# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tabela_periodo_internacao_CORRE_bkp,
  titulo = "Turno das atividades, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "PERÍODO",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL2"
)





# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tabela_unidade_internacao_CORRE_bkp,
  titulo = "Unidade socioeducativa, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "UNIDADE",  # Adicionando a coluna para o eixo x
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = tabela_IDADE_SEXO_internacao_CORRE_bkp,
  titulo = "Idade e Sexo, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  legend_title = "SEXO"  # Título da legenda
)


# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = tabela_IDADE_SEXO_internacao_CORRE_bkp1,
  titulo = "Sexo, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "PERCENTUAL",
  legend_title = "SEXO", # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "black",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "out"      # Posição das labels,
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = ESCOLARIDADE_internacao_CORRE_bkp,
  titulo = "Escolaridade, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = REGIONAL_RESIDENCIAL_banco_internacao_CORRE_bkp,
  titulo = "Regional Residencial, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_geral_sem_concurso_internacao_CORRE_bkp,
  titulo = "Incidência, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = REGIONAL_ATO_banco_internacao_CORRE_bkp,
  titulo = "Regional Ato, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)






# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = SENTENCA_banco_internacao_CORRE_bkp,
  titulo = "Sentença, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = PROTETIVAS_banco_internacao_CORRE_bkp,
  titulo = "Medidas Protetivas, 2024",
  subtitulo = "Projeto CORRE LEGAL: internação",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)

# CORRE internacao FIM






# Letalidade


# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = CAUSA_JURIDICA_let_bkp,
  titulo = "Letalidade por causas externas, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = soma_intervalo_idade_HOMICIDIO_let_bkp,
  titulo = "Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = soma_intervalo_idade_IGNORADA_let_bkp,
  titulo = "Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = tab_letalidade_geral_12_20_HOMICIDIO_bkp,
  titulo = "Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, 2018 a 2022",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "ANO",
  legend_title = "ANO"  # Título da legenda
)



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = tab_letalidade_geral_12_20_IGNORADA_bkp,
  titulo = "Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, 2018 a 2022",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "ANO",
  legend_title = "ANO"  # Título da legenda
)




#montando o próximo gráfico:

p1_banco_SEXO_HOMICIDIO_LETALIDADE_pizza =
  ggpie(banco_SEXO_HOMICIDIO_LETALIDADE_pizza,
        x= "QUANTIDADE", label = "QUANTIDADE",
        lab.pos = "in", lab.font = list(color = "white", face = "bold"),
        lab.adjust = 0,
        fill = "SEXO", color = "white", face="bold",
        palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", subtitle = "Belo Horizonte e municípios limítrofes") +
  labs(subtitle = "Belo Horizonte e municípios limítrofes") +
  ggtitle("HOMICÍDIO")
#ggsave("GRAFICO_068_sexo_idade_LET_pizza.png", width=9, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
p2_banco_SEXO_IGNORADA_LETALIDADE_pizza =
  ggpie(banco_SEXO_IGNORADA_LETALIDADE_pizza,
        x= "QUANTIDADE", label = "QUANTIDADE",
        lab.pos = "in", lab.font = list(color = "white", face = "bold"),
        lab.adjust = 0,
        fill = "SEXO", color = "white", face="bold",
        palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", subtitle = "Belo Horizonte e municípios limítrofes") +
  labs(subtitle = "Belo Horizonte e municípios limítrofes") +
  ggtitle("CAUSA IGNORADA")
#ggsave("GRAFICO_068_sexo_idade_LET_pizza.png", width=9, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
# Agora, chamamos a função para criar o gráfico composto
graficos_compostos <- list(p1_banco_SEXO_HOMICIDIO_LETALIDADE_pizza, p2_banco_SEXO_IGNORADA_LETALIDADE_pizza)

criar_e_salvar_grafico(tipo = "composto",
                       graficos = graficos_compostos,
                       titulo = "Sexo dos adolescente e jovens vitimados por homicídio e causa ignorada, 2023",
                       subtitulo = "Letalidade",
                       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
                       diretorio = "~/diretorio_r/estciabh/imagens")





# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_COR_hom_ign_LETALIDADE_pizza_bkp,
  titulo = "Raça/cor dos adolescentes e jovens vitimados por homicídio e causa ignorada, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  x_col = "COR",
  y_col = "QUANTIDADE",
  fill_col = "CAUSA_JURIDICA",
  legend_title = "CAUSA JURÍDICA"  # Título da legenda
)





# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = banco_HOM_IGN_GERAL,
  titulo = "Óbitos por homicídio e causa ignorada, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  x_col = "ANO",
  y_col = "QUANTIDADE",
  fill_col = "CAUSA_JURIDICA",
  legend_title = "CAUSA JURÍDICA"  # Título da legenda
)






# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = passagem_cia_GERAL_HOM_IGN_LET,
  titulo = "Óbitos por causas violentas e passagem pelo CIABH, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  x_col = "CAUSA_JURIDICA",
  y_col = "QUANTIDADE",
  fill_col = "PASSAGEM_CIABH",
  legend_title = "PASSAGEM CIABH"  # Título da legenda
)



# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = passagem_cia_HOMICIDIO_LET,
  titulo = "Óbitos por homicídio e passagem pelo CIABH, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "PASSAGEM_CIABH",
  legend_title = "PASSAGEM CIABH"  # Título da legenda
)




# Gráfico de barras verticais com diferentes categorias (por exemplo, sexo)
criar_e_salvar_grafico(
  tipo = "barra_vertical",
  df = passagem_cia_IGNORADA_LET,
  titulo = "Óbitos por causa ignorada e passagem pelo CIABH, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  x_col = "IDADE",
  y_col = "QUANTIDADE",
  fill_col = "PASSAGEM_CIABH",
  legend_title = "PASSAGEM CIABH"  # Título da legenda
)



# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = sexo_LET_HOMICIDIO,
  titulo = "Óbitos por homicídio de adolescentes e jovens, por sexo, Belo Horizonte, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  fill_col = "SEXO",
  label_col = "QUANTIDADE",
  legend_title = "SEXO",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_RACA_COR_HOMICIDIO_let_bkp,
  titulo = "Raça/cor, Belo Horizonte e municípios limítrofes, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_PROCEDENCIA_LET_bkp,
  titulo = "Localidade do óbito, Belo Horizonte e municípios limítrofes, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)



# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tempo_medio_geral_OBITO_TAB_02_bkp,
  titulo = "Tempo entre última entrada CIABH e óbito, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)





# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = tempo_medio_geral_CIABH_TAB_02_bkp,
  titulo = "Tempo entre a primeira e a última entrada, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)






# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = banco_NUMERO_ENTRADAS_let_bkp,
  titulo = "Entradas, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de pizza
criar_e_salvar_grafico(
  tipo = "pizza",
  df = graf_pizza_MEDIDAS_LET,
  titulo = "Aplicação de medida protetiva, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  fill_col = "POSSUI_MEDIDA_PROTETIVA",
  label_col = "PERCENTUAL",
  legend_title = "MEDIDA PROTETIVA",  # Título da legenda
  #lab_size = 3.5,        # Tamanho das labels
  lab_color = "white",  # Cor das labels
  lab_face = "bold", # Estilo das labels
  lab_pos = "in"      # Posição das labels,
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = BANCO_MEDIDAS_LET_bkp,
  titulo = "Medidas Protetivas, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)




# Gráfico de barras horizontais
criar_e_salvar_grafico(
  tipo = "barra_horizontal",
  df = BANCO_DECISAO_LET_bkp,
  titulo = "Decisões aplicadas, 2023",
  subtitulo = "Letalidade",  # Subtítulo personalizado
  caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
  y_col = "QUANTIDADE",
  label_col = "PERCENTUAL"
)

#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################

