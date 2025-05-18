library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

# --- 1. Configuração inicial ---
dir_base <- "/media/backup/config/pessoal/elerson/diretorio_r/estciabh/coordenadas"

# --- 2. Carregar shapes base ---
regional_shp <- st_read(file.path(dir_base, "REGIONAL.shp")) %>%
  st_transform(4326)  # WGS84

bairro_shp <- st_read(file.path(dir_base, "BAIRRO_OFICIAL.shp")) %>%
  st_transform(4326)

# --- 3. Carregar e processar dados de uso de drogas ---
coord_uso_droga <- read.csv(file.path(dir_base, "COORDENADAS_USO_DE_DROGAS.csv")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(
    lat = st_coordinates(geometry)[,2],
    lon = st_coordinates(geometry)[,1]
  ) %>%
  # Limites ampliados para incluir Barreiro
  filter(
    lat >= -20.00 & lat <= -19.70,
    lon >= -44.20 & lon <= -43.80
  ) %>%
  # Agrupar pontos nas mesmas coordenadas
  group_by(lon, lat) %>%
  summarise(
    total = n(),
    .groups = "drop"
  )

# --- 4. Verificação específica para Barreiro ---
barreiro_shp <- regional_shp %>% filter(NOME == "BARREIRO")
pontos_barreiro <- st_intersection(coord_uso_droga, st_buffer(barreiro_shp, 0.01))
print(paste("Pontos de uso de droga na região do Barreiro:", nrow(pontos_barreiro)))

# --- 5. Plotar o mapa ---
ggplot() +
  # Camadas de fundo
  geom_sf(data = regional_shp, fill = "white", color = "gray40", linewidth = 0.5, alpha = 0.8) +
  geom_sf(data = bairro_shp, fill = NA, color = "gray80", linewidth = 0.2) +

  # Pontos de uso de droga com tamanho e cor proporcional ao total
  geom_sf(
    data = coord_uso_droga,
    aes(size = total, fill = total),
    shape = 21,           # Círculo preenchido
    color = "black",
    stroke = 0.5,
    alpha = 0.9
  ) +

  # Escala de tamanho
  scale_size_continuous(
    name = "Nº de Ocorrências",
    range = c(3, 12),     # Ajuste conforme agrupamento
    breaks = c(1, 2, 5, 10, 20)
  ) +

  # Escala de cor (vermelho forte)
  scale_fill_gradient(
    name = "Nº de Ocorrências",
    low = "#ff6b6b",      # Vermelho claro
    high = "#8b0000",     # Vermelho escuro
    breaks = c(1, 2, 5, 10, 20),
    guide = guide_colorbar(
      barwidth = 1.5,
      barheight = 15,
      title.position = "top"
    )
  ) +

  # Rótulos das regionais
  geom_sf_text(
    data = regional_shp,
    aes(label = NOME),
    size = 3.3,
    color = "black",
    fontface = "bold",
    check_overlap = TRUE
  ) +

  # Elementos gráficos
  labs(
    title = "Distribuição espacial: Uso de Drogas em Belo Horizonte",
    subtitle = "Tamanho e cor dos círculos representam a quantidade de ocorrências",
    caption = "FONTE: Vara Infracional/Comissariado"
  ) +

  # Tema
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10)
  ) +

  # Elementos de navegação
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  )

# --- 6. Salvar imagem em alta resolução ---
output_path <- file.path(dir_base, "atual/mapa_uso.png")
ggsave(
  filename = output_path,
  width = 12,
  height = 8,
  dpi = 300,
  units = "in"
)
