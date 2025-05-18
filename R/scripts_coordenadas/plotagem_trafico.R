library(sf)
library(ggplot2)
library(dplyr)

# Definir diretório base
dir_base <- "/media/backup/config/pessoal/elerson/diretorio_r/estciabh/coordenadas"

# Carregar o shapefile das regionais
regional_shp <- st_read(file.path(dir_base, "REGIONAL.shp"))

# Transformar CRS para WGS 84 (caso necessário)
regional_shp <- st_transform(regional_shp, crs = 4326)

# Carregar o shapefile dos bairros
bairro_shp <- st_read(file.path(dir_base, "BAIRRO_OFICIAL.shp"))

# Transformar CRS para WGS 84 (caso necessário)
bairro_shp <- st_transform(bairro_shp, crs = 4326)

# Carregar CSV com coordenadas de homicídios e converter para formato espacial
coord_trafico <- read.csv(file.path(dir_base, "COORDENADAS_TRAFICO_DE_DROGAS.csv"))

# Converter o dataframe em um objeto espacial
coord_trafico_sf <- st_as_sf(coord_trafico, coords = c("longitude", "latitude"), crs = 4326)

# Extrair coordenadas e criar colunas separadas
coord_trafico_sf <- coord_trafico_sf %>%
  mutate(lat = st_coordinates(geometry)[,2], lon = st_coordinates(geometry)[,1])

# Remover pontos fora dos limites geográficos de Belo Horizonte
limite_lat <- c(-19.95, -19.75)  # Faixa de latitude
limite_lon <- c(-44.10, -43.85)  # Faixa de longitude

coord_trafico_sf <- coord_trafico_sf %>%
  filter(lat >= limite_lat[1], lat <= limite_lat[2],
         lon >= limite_lon[1], lon <= limite_lon[2])

# Criar o mapa
mapa <- ggplot() +
  geom_sf(data = regional_shp, fill = "white", color = "black", linewidth = 0.4) +  # Camada das regionais
  geom_sf(data = bairro_shp, fill = NA, color = "black", linewidth = 0.1, alpha = 10) +  # Camada dos bairros (transparente)
  geom_sf(data = coord_trafico_sf, color = "red", size = 2.5, alpha = 0.7) +  # Pontos de homicídios
  geom_sf_text(data = regional_shp, aes(label = NOME), size = 3.3, color = "black", fontface = "bold") +  # Nome das regionais
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Fundo branco
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  ggplot2::annotate("text", x = Inf, y = -Inf, label = "FONTE: Vara Infracional/Comissariado",
                    hjust = 1, vjust = -1, size = 4, color = "black")

# Definir caminho e nome do arquivo
output_path <- file.path(dir_base, "atual/mapa_trafico.png")

# Salvar o mapa gerado
ggsave(filename = output_path, plot = mapa,
       width = 12, height = 8, dpi = 300, units = "in")
