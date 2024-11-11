# estciabh
Para criar uma função em R que possa ser aplicada a diferentes dataframes, você pode estruturar o código de forma flexível, substituindo as variáveis do `ggplot` para torná-lo reutilizável. Abaixo está uma sugestão de como você pode organizar essa função:


### Função `gerar_grafico`

```r
library(ggplot2)
library(ggrepel)
library(scales)
library(stringr)

gerar_grafico <- function(data, x, y, group, color, label, legend_title = "Legenda") {
  ggplot(data = data, aes(x = .data[[x]], y = .data[[y]], group = .data[[group]])) +
    geom_line(aes(color = .data[[color]]), linewidth = 1.2) +
    geom_point(aes(color = .data[[color]]), size = 1.5) +
    scale_color_brewer(legend_title = legend_title, palette = "Set1") +
    geom_label_repel(
      aes(label = .data[[label]]),
      box.padding = unit(0.5, "lines"),
      max.overlaps = Inf,
      label.size = 0.1,
      size = 3
    ) +
    labs(
      title = str_c("GRAFICO: Atendimento de adolescentes, Belo Horizonte, 2015 a ", format(Sys.Date() - 365 * 1, "%Y")),
      caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
      x = "",
      y = " "
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.caption = element_text(hjust = 0.5, size = 12)
    ) +
    theme(legend.position = "bottom")
}
```

### Parâmetros da Função:
- `data`: O dataframe a ser utilizado.
- `x`: O nome da variável a ser usada no eixo X.
- `y`: O nome da variável a ser usada no eixo Y.
- `group`: O nome da variável que define o agrupamento para as linhas.
- `color`: O nome da variável que define a cor das linhas e pontos.
- `label`: O nome da variável a ser usada como rótulo no gráfico.
- `legend_title`: Título da legenda (opcional).

### Como Usar a Função:
Você pode usar essa função com diferentes dataframes, fornecendo os parâmetros adequados.

#### Exemplo de uso:

```r
# Suponha que você tenha um dataframe chamado df1 com as colunas: 'ano', 'valor', 'grupo', 'categoria' e 'id'
gerar_grafico(
  data = df1,
  x = "ano",
  y = "valor",
  group = "grupo",
  color = "categoria",
  label = "id",
  legend_title = "Categoria"
)
```

### Explicação dos detalhes:
- A função usa `aes(x = .data[[x]], y = .data[[y]], group = .data[[group]])` para garantir que as colunas passadas como parâmetros sejam usadas corretamente no gráfico.
- A função `str_c()` é usada para concatenar a string do título do gráfico.
- `geom_label_repel()` é usado para adicionar rótulos aos pontos, e o comportamento da sobreposição é controlado com `max.overlaps = Inf`.
- A cor da linha e dos pontos é definida com `scale_color_brewer()`, usando a paleta "Set1".
- O tema é personalizado para centralizar o título e legenda, e ajustar o estilo da legenda.

### Adaptabilidade:
Esse código é flexível, e você pode utilizá-lo com qualquer dataframe desde que você forneça os nomes corretos das colunas para `x`, `y`, `group`, `color` e `label`. Isso torna a função útil para gráficos semelhantes em diferentes datasets, basta modificar os argumentos conforme necessário.




Para transformar esse código em uma função reutilizável, o objetivo é tornar as partes do código que dependem de entradas específicas parametrizáveis. Vou criar uma função em R com parâmetros que permitem ajustar o título da tabela, as colunas e outros detalhes sem a necessidade de modificar o código cada vez.

Aqui está como a função pode ser estruturada:

```r
library(gt)
library(dplyr)

# Função para gerar uma tabela formatada com base no dataframe fornecido
gerar_tabela_gt <- function(df, titulo, arquivo_saida, colunas_bold = c(1:3), formato_data = TRUE) {
  
  # Verifica se a data deve ser formatada
  if (formato_data) {
    ano_atual <- format(Sys.Date() - 365 * 1, "%Y")
  } else {
    ano_atual <- ""
  }

  # Cria a tabela com o estilo e formatação definidos
  tabela <- df %>%
    gt() %>%
    
    tab_header(
      title = md(str_c(titulo, ": Regional de Residência, Belo Horizonte, ", ano_atual)),
      subtitle = ""
    ) %>%
    
    tab_source_note("FONTE: VARA INFRACIONAL/SUASE/DOPCAD") %>%
    
    # Aplica o estilo em negrito na última linha
    tab_style(
      style = list(cell_text(weight = "bold", size = "small")),
      locations = cells_body(rows = nrow(df))
    ) %>%
    
    # Aplica negrito nos rótulos das colunas
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = colunas_bold)
    ) %>%
    
    # Alinha as colunas 2 e 3 à direita
    tab_style(
      style = cell_text(align = "right"),
      locations = cells_column_labels(columns = c(2, 3))
    ) %>%
    
    # Define opções gerais da tabela
    tab_options(
      data_row.padding = px(1),
      table.font.size = 12,
      heading.align = "center",
      heading.subtitle.font.size = "small"
    )
  
  # Salva a tabela gerada como uma imagem PNG
  gtsave(tabela, arquivo_saida, expand = 10)
  
  # Retorna a tabela para visualização, se desejado
  return(tabela)
}

# Exemplo de uso da função:
# Supondo que `df_snr_regional_residencia` seja seu dataframe

# Chama a função para gerar a tabela e salvar como PNG
gerar_tabela_gt(df_snr_regional_residencia, "Tabela Exemplo", "tabela_exemplo.png")
```

### Explicação dos parâmetros da função:

- **df**: O dataframe de entrada.
- **titulo**: O título da tabela que será exibido na parte superior. Esse título será complementado com a data do último ano (opcional).
- **arquivo_saida**: O nome do arquivo para salvar a tabela gerada em formato PNG.
- **colunas_bold**: Especifica as colunas que devem aparecer em negrito nos cabeçalhos. Por padrão, está configurado para as colunas 1 a 3.
- **formato_data**: Define se a data deve ser incluída no título (por padrão, é `TRUE`).

### Como usar:
- Você pode ajustar os parâmetros conforme necessário.
- Se o dataframe já está carregado como `df_snr_regional_residencia`, basta passar para a função com o título e o nome do arquivo de saída desejado.

Isso torna o processo mais flexível, e você pode reutilizar a função para gerar tabelas similares com diferentes datasets e parâmetros de formatação.
