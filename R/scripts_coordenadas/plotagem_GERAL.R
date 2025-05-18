library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

# --- 1. Configuração inicial ---
dir_base <- "/media/backup/config/pessoal/elerson/diretorio_r/estciabh/coordenadas"

# Carregar shapes base (uma vez só)
regional_shp <- st_read(file.path(dir_base, "REGIONAL.shp")) %>% st_transform(4326)
bairro_shp <- st_read(file.path(dir_base, "BAIRRO_OFICIAL.shp")) %>% st_transform(4326)

# --- 2. Função para criar mapas ---
criar_mapa_criminal <- function(dados, titulo, nome_arquivo, range_size, breaks) {
  # Processar os dados
  dados_sf <- dados %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    mutate(
      lat = st_coordinates(geometry)[,2],
      lon = st_coordinates(geometry)[,1]
    ) %>%
    filter(
      lat >= -20.00 & lat <= -19.70,
      lon >= -44.20 & lon <= -43.80
    ) %>%
    group_by(lon, lat) %>%
    summarise(total = n(), .groups = "drop")

  # Criar o mapa
  mapa <- ggplot() +
    geom_sf(data = regional_shp, fill = "white", color = "gray40", linewidth = 0.5, alpha = 0.8) +
    geom_sf(data = bairro_shp, fill = NA, color = "gray80", linewidth = 0.2) +

    geom_sf(
      data = dados_sf,
      aes(size = total, fill = total),
      shape = 21, color = "black", alpha = 0.7, stroke = 0.5
    ) +

    scale_size_continuous(
      range = range_size,
      breaks = breaks,
      guide = "none"
    ) +

    scale_fill_gradientn(
      name = "Nº de Ocorrências",
      colors = c("#ff9999", "#ff4444", "#cc0000", "#800000"),
      values = scales::rescale(breaks),
      breaks = breaks,
      limits = c(min(breaks), max(breaks)),
      guide = guide_colorbar(
        barwidth = 1.5,
        barheight = 15,
        title.position = "top"
      )
    ) +

    labs(title = titulo) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "right"
    )

  # Salvar o mapa
  ggsave(file.path(dir_base, "atual", nome_arquivo),
         plot = mapa, width = 12, height = 8, dpi = 300)

  return(mapa)
}

# --- 3. Configurações por crime ---
crimes <- list(
  list(
    nome = "HOMICIDIO",
    titulo = "Distribuição de Homicídios em Belo Horizonte",
    range = c(2, 6),
    breaks = c(1, 2, 3)  # Removido o parêntese extra aqui
  ),
  list(
    nome = "ROUBO",
    titulo = "Distribuição de Roubos em Belo Horizonte",
    range = c(3, 12),
    breaks = c(1, 10, 30, 60, 90)  # Removido o parêntese extra aqui
  ),
  list(
    nome = "FURTO",
    titulo = "Distribuição de Furtos em Belo Horizonte",
    range = c(3, 15),
    breaks = c(1, 50, 100, 150, 200, 240)  # Removido o parêntese extra aqui
  ),
  list(
    nome = "USO_DE_DROGAS",
    titulo = "Uso de Drogas em Belo Horizonte",
    range = c(3, 10),
    breaks = c(1, 10, 30, 50, 62)  # Removido o parêntese extra aqui
  ),
  list(
    nome = "TRAFICO_DE_DROGAS",
    titulo = "Tráfico de Drogas em Belo Horizonte",
    range = c(3, 20),
    breaks = c(1, 100, 300, 600, 825)  # Removido o parêntese extra aqui
  )
)
# --- 4. Processar todos os crimes ---
for (crime in crimes) {
  # Carregar os dados
  arquivo_csv <- file.path(dir_base, paste0("COORDENADAS_", crime$nome, ".csv"))

  if (file.exists(arquivo_csv)) {
    dados <- read.csv(arquivo_csv)

    # Criar e salvar o mapa
    mapa <- criar_mapa_criminal(
      dados = dados,
      titulo = crime$titulo,
      nome_arquivo = paste0("mapa_", tolower(crime$nome), ".png"),
      range_size = crime$range,
      breaks = crime$breaks
    )

    print(paste("Mapa gerado para:", crime$nome))
  } else {
    warning(paste("Arquivo não encontrado:", arquivo_csv))
  }
}
