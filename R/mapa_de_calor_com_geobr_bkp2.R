# Instalar pacotes necessários (se ainda não instalados)
#install.packages(c("geobr", "sf", "dplyr", "ggplot2"))

# Carregar pacotes
library(geobr)
library(sf)
library(dplyr)
library(ggplot2)

setwd("/media/backup/config/pessoal/elerson/diretorio_r/estciabh/coordenadas")

# Passo 1: Ler os dados de coordenadas geográficas
#dados <- read.csv("COORDENADAS_TRAFICO_DE_DROGAS.csv") # Substituir pelo caminho do arquivo
dados <- read.csv("COORDENADAS_FURTO.csv") # Substituir pelo caminho do arquivo
#dados <- read.csv("COORDENADAS_ROUBO.csv") # Substituir pelo caminho do arquivo
#dados <- read.csv("COORDENADAS_HOMICIDIO.csv") # Substituir pelo caminho do arquivo
# Converte os dados para um objeto sf (Spatial Feature)
dados_sf <- st_as_sf(dados, coords = c("longitude", "latitude"), crs = 4326)

# Passo 2: Baixar os limites do município de Belo Horizonte
bh <- read_municipality(code_muni = 3106200, year = 2020)

# Passo 3: Criar uma grade regular sobre Belo Horizonte
# Transformar para a mesma projeção para facilitar cálculos
bh_proj <- st_transform(bh, crs = 3857) # Projeção métrica para criar a grade
dados_proj <- st_transform(dados_sf, crs = 3857)

# Definir o tamanho das células (em metros)
resolucao <- 1000 # Exemplo: células de 1 km x 1 km

# Criar a grade
grade <- st_make_grid(bh_proj, cellsize = resolucao, square = TRUE) %>%
  st_as_sf() %>%
  #rename(geometry = x)
dplyr::rename(geometry = x)
# Passo 4: Contar pontos por célula
# Atribuir cada ponto a uma célula
#dados_agrupados <- st_join(dados_proj, grade, join = st_within)
dados_agrupados <- st_join(grade, dados_proj, join = st_contains) #com correção

# Contar o número de pontos por célula
#densidade <- dados_agrupados %>%
# st_drop_geometry() %>%
#group_by(geometry) %>%
#summarise(contagem = n()) %>%
# st_as_sf()

densidade <- dados_agrupados %>%
  group_by(geometry) %>% # Agora `geometry` está associado à grade
  summarise(contagem = n(), .groups = "drop") %>%
  st_as_sf()

# Substitua "caminho/para/seu/shapefile.shp" pelo caminho correto do seu shapefile
shapefile <- st_read("/home/amikobh/diretorio_r/estciabh/coordenadas/mapas_BH/regional/REGIONAL.shp")
shapefile1 <- st_read("/home/amikobh/diretorio_r/estciabh/coordenadas/mapas_BH/vila_favela/VILA_FAVELA.shp")
# Passo 5: Plotar o mapa de calor



ggplot() +
  geom_sf(data = bh, fill = NA, color = "black", linewidth = 0.5) + # Limites de BH
  geom_sf(data = densidade, aes(fill = contagem), color = NA) + # Grade com densidade
  scale_fill_gradient(low = "white", high = "red", name = "Densidade")  +
  geom_sf(data = shapefile, fill = NA, color = "black", size = 0.5) +  # Camada do shapefile
  geom_sf_text(data = shapefile, aes(label = NOME), size = 3, color = "black", fontface = "bold") +  # Rótulos em negrito
  geom_sf(data = shapefile1, fill = NA, color = "black", size = 0.5) +  # Camada do shapefile
  #geom_sf_text(data = shapefile1, aes(label = APELIDO_LO), size = 1, color = "black", fontface = "bold") +  # Rótulos em negrito
  theme_void() + # Remove os eixos e o fundo
  theme(
    legend.position = "right", # Posiciona a legenda à direita
    legend.title = element_text(size = 12, face = "bold"), # Ajusta estilo da legenda
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centraliza e estiliza o título
    plot.caption = element_text(size = 10, hjust = 0.5, face = "italic") # Centraliza o caption
  ) +
  labs(
    title = "Mapa de Calor - Belo Horizonte",
    caption = "Fonte: Dados fornecidos e geobr"
  )


