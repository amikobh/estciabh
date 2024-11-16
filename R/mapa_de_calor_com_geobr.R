# Instalar pacotes necessários (caso não tenha instalado)
install.packages(c("ggplot2", "sf", "geobr", "viridis", "dplyr"))
library(ggplot2)
library(sf)
library(geobr)
library(viridis)
library(dplyr)

# Coordenadas fictícias para ilustrar
coordenadas <- coordenadas_HOMICIDIO

coordenadas = coordenadas |>
  select(-ENDERECO)

# Transformar as coordenadas em um objeto espacial (sf)
dados_sf <- st_as_sf(coordenadas, coords = c("longitude", "latitude"), crs = 4326)

# Separar as coordenadas do objeto sf para poder usar no gráfico
dados_sf <- st_transform(dados_sf, crs = 4326) # Garante que as coordenadas estão no formato correto
dados_sf <- cbind(dados_sf, st_coordinates(dados_sf))  # Adiciona as coordenadas separadas

# Baixar o mapa de Belo Horizonte
bh <- read_municipality(code_muni = 3106200)


# Carregar o seu shapefile local
# Substitua "caminho/para/seu/shapefile.shp" pelo caminho correto do seu shapefile
shapefile <- st_read("/home/amikobh/diretorio_r/estciabh/coordenadas/mapas_BH/regional/REGIONAL.shp")


# Criar o mapa de calor de Belo Horizonte
ggplot() +
  geom_sf(data = bh, fill = "white", color = "black") +  # Mapa de Belo Horizonte
  stat_density2d(data = dados_sf, aes(x = X, y = Y, fill = after_stat(level)),
                 geom = "polygon", bins = 3.5, adjust = 1) +  # Ajuste de bins e suavização de densidade
  scale_fill_viridis(option = "C") +  # Melhor paleta de cores
  theme_minimal() +  # Limpeza do gráfico
  labs(title = "Mapa de Calor - Belo Horizonte", subtitle = "Com as coordenadas fornecidas") +
  theme(
    legend.position = "right",  # Posiciona a legenda
    axis.title.x = element_blank(),  # Remove título do eixo x
    axis.title.y = element_blank(),  # Remove título do eixo y
    axis.text.x = element_blank(),  # Remove rótulos do eixo x
    axis.text.y = element_blank(),  # Remove rótulos do eixo y
    axis.ticks = element_blank(),   # Remove os ticks dos eixos
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centraliza o título
    plot.subtitle = element_text(hjust = 0.5, size = 12)  # Centraliza o subtítulo
  )


# Resultado esperado:
#   O mapa de calor será exibido com uma suavização controlada pela variável adjust e granularidade ajustada
# pelos bins.
# A paleta de cores será visível, destacando as áreas de maior concentração de pontos (quentes) e as áreas
# com menor concentração (frias).
# Teste e ajustes:
#  Ajuste de bins: Se você achar que o mapa de calor está muito "agitado" ou "liso", tente ajustar o número de bins.
# Um valor menor de bins resulta em menos detalhes, enquanto um valor maior aumenta a precisão da densidade.

# Ajuste de adjust: Se você desejar um mapa de calor mais detalhado ou mais suave, ajuste o valor de adjust.
# Valores menores (como 1.0) tendem a produzir mapas mais nítidos,
# enquanto valores maiores (como 2.0 ou mais) suavizam as áreas densas.

# Criar o mapa de calor de Belo Horizonte com a paleta vermelha
ggplot() +
  geom_sf(data = bh, fill = "white", color = "black") +  # Mapa de Belo Horizonte
  stat_density2d(data = dados_sf, aes(x = X, y = Y, fill = after_stat(level)),
                 geom = "polygon", bins = 5, adjust = .5) +  # Ajuste de bins e suavização de densidade
  scale_fill_gradient(low = "orange", high = "red") +  # Paleta de cores vermelha (do claro ao escuro)
  theme_minimal() +  # Limpeza do gráfico
  labs(title = "Mapa de Calor - Belo Horizonte", subtitle = "Com as coordenadas fornecidas") +
  theme(
    legend.position = "right",  # Posiciona a legenda
    axis.title.x = element_blank(),  # Remove título do eixo x
    axis.title.y = element_blank(),  # Remove título do eixo y
    axis.text.x = element_blank(),  # Remove rótulos do eixo x
    axis.text.y = element_blank(),  # Remove rótulos do eixo y
    axis.ticks = element_blank(),   # Remove os ticks dos eixos
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centraliza o título
    plot.subtitle = element_text(hjust = 0.5, size = 12)  # Centraliza o subtítulo
  )


# Criar o mapa de calor de Belo Horizonte com o shapefile adicionado
ggplot() +
  geom_sf(data = bh, fill = "white", color = "white") +  # Mapa de Belo Horizonte
  stat_density2d(data = dados_sf, aes(x = X, y = Y, fill = after_stat(level)),
                 geom = "polygon", bins = 5, adjust = .5) +  # Ajuste de bins e suavização de densidade
  scale_fill_gradient(low = "orange", high = "red") +  # Paleta de cores vermelha (do claro ao escuro)
  geom_sf(data = shapefile, fill = NA, color = "black", size = 0.5) +  # Camada do shapefile
  theme_minimal() +  # Limpeza do gráfico
  labs(title = "Mapa de Calor - Belo Horizonte", subtitle = "Com as coordenadas fornecidas") +
  theme(
    legend.position = "right",  # Posiciona a legenda
    axis.title.x = element_blank(),  # Remove título do eixo x
    axis.title.y = element_blank(),  # Remove título do eixo y
    axis.text.x = element_blank(),  # Remove rótulos do eixo x
    axis.text.y = element_blank(),  # Remove rótulos do eixo y
    axis.ticks = element_blank(),   # Remove os ticks dos eixos
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centraliza o título
    plot.subtitle = element_text(hjust = 0.5, size = 12)  # Centraliza o subtítulo
  )





# Criar o mapa de calor de Belo Horizonte com a camada do shapefile
ggplot() +
  geom_sf(data = bh, fill = "white", color = "white") +  # Mapa de Belo Horizonte
  stat_density2d(data = dados_sf, aes(x = X, y = Y, fill = after_stat(level)),
                 geom = "polygon", bins = 5, adjust = 1) +  # Ajuste de bins e suavização de densidade
  scale_fill_gradient(
    low = "white",
    high = "red",
    #limits = c(0, 50),   # Limites do gradiente
    #breaks = c(0, 15, 30, 60),  # Quebras da legenda
    na.value = "white"  # Define a cor para áreas sem dados (NA)
  ) +
  # Aqui definimos a escala de cor do branco (0) até o vermelho (50), e os intervalos de cor (breaks)
  geom_sf(data = shapefile, fill = NA, color = "black", size = 0.5) +  # Camada do shapefile
  geom_sf_text(data = shapefile, aes(label = NOME), size = 3, color = "black") +  # Adicionar rótulos
  theme_minimal() +  # Limpeza do gráfico
  labs(title = "Mapa de Calor - Belo Horizonte", subtitle = "Com as coordenadas fornecidas") +
  theme(
    legend.position = "right",  # Posiciona a legenda
    axis.title.x = element_blank(),  # Remove título do eixo x
    axis.title.y = element_blank(),  # Remove título do eixo y
    axis.text.x = element_blank(),  # Remove rótulos do eixo x
    axis.text.y = element_blank(),  # Remove rótulos do eixo y
    axis.ticks = element_blank(),   # Remove os ticks dos eixos
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centraliza o título
    plot.subtitle = element_text(hjust = 0.5, size = 12)  # Centraliza o subtítulo
  )


