#########################################################################################################
#TRATAMENTO COORDENADAS TRAFICO_DE_DROGAS:
#########################################################################################################
##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR coordenadas:
dir.create(file.path("~/diretorio_r/estciabh", "coordenadas"))
setwd(file.path("~/diretorio_r/estciabh/coordenadas"))
#########################################################################################################


ender_TRAFICO_DE_DROGAS<- banco_TRAFICO_DE_DROGAS_puro[,c("TIPO_LOG.ATO","NOME_LOG_ATO", "N_LOG.ATO", "BAIRRO_REGIAO_ATO", "CIDADE_ATO", "UF_ATO")]

ender_TRAFICO_DE_DROGAS$CIDADE_ATO <- c("BELO HORIZONTE")
ender_TRAFICO_DE_DROGAS$UF_ATO <- c("MINAS GERAIS")
ender_TRAFICO_DE_DROGAS$PAIS_ATO <- c("BRASIL")


ender_TRAFICO_DE_DROGAS$TIPO_LOG.ATO[ender_TRAFICO_DE_DROGAS$TIPO_LOG.ATO == "SEMINFO"]<- ""
ender_TRAFICO_DE_DROGAS$TIPO_LOG.ATO[ender_TRAFICO_DE_DROGAS$TIPO_LOG.ATO == "SEM INFORMACAO"]<- ""

ender_TRAFICO_DE_DROGAS$NOME_LOG_ATO[ender_TRAFICO_DE_DROGAS$NOME_LOG_ATO == "SEMINFO"]<- ""
ender_TRAFICO_DE_DROGAS$NOME_LOG_ATO[ender_TRAFICO_DE_DROGAS$NOME_LOG_ATO == "SEM INFORMACAO"]<- ""

ender_TRAFICO_DE_DROGAS$BAIRRO_REGIAO_ATO[ender_TRAFICO_DE_DROGAS$BAIRRO_REGIAO_ATO == "SEMINFO"]<- ""
ender_TRAFICO_DE_DROGAS$BAIRRO_REGIAO_ATO[ender_TRAFICO_DE_DROGAS$BAIRRO_REGIAO_ATO == "SEM INFORMACAO"]<- ""

ender_TRAFICO_DE_DROGAS$BAIRRO_REGIAO_ATO[ender_TRAFICO_DE_DROGAS$BAIRRO_REGIAO_ATO == "SEM INFORMACAO"]<- ""

ender_TRAFICO_DE_DROGAS$N_LOG.ATO <- gsub(" ","", ender_TRAFICO_DE_DROGAS$N_LOG.ATO)
ender_TRAFICO_DE_DROGAS$N_LOG.ATO[ender_TRAFICO_DE_DROGAS$N_LOG.ATO == "SN"]<- ""



ender_TRAFICO_DE_DROGAS <- ender_TRAFICO_DE_DROGAS[!(ender_TRAFICO_DE_DROGAS$BAIRRO_REGIAO_ATO == "VAZIO"),]

ender_TRAFICO_DE_DROGAS$ENDERECO = apply(ender_TRAFICO_DE_DROGAS, 1, function(x) paste(x[!is.na(x)], collapse=' '))

#ender_TRAFICO_DE_DROGAS = ender_TRAFICO_DE_DROGAS[!(ender_TRAFICO_DE_DROGAS$ENDER == "BELO HORIZONTE MINAS GERAIS"),]

library(dplyr)

dt_ender_TRAFICO_DE_DROGAS = ender_TRAFICO_DE_DROGAS %>%
  select(ENDERECO)

#TIRANDO RUA E NUMERO PARA ENTRAR NA GEOREFERENCIA:
#dt_ender_TRAFICO_DE_DROGAS$ENDERECO[dt_ender_TRAFICO_DE_DROGAS$ENDERECO == "RUA NOVE 231 MANTIQUEIRA BELO HORIZONTE MINAS GERAIS BRASIL"]<- "MANTIQUEIRA BELO HORIZONTE MINAS GERAIS BRASIL"

library(XML)
library(ggmap)
library(dplyr)
library(RCurl)
library(googleway)

key <- 'AIzaSyDKYIchtAe_gwjE1R2L7sIKns2mTxDf0NI'

register_google(key = "AIzaSyDKYIchtAe_gwjE1R2L7sIKns2mTxDf0NI")



#latlon_TRAFICO_DE_DROGAS = geocode(dt_ender_TRAFICO_DE_DROGAS$ENDERECO, key=key)

latlon_TRAFICO_DE_DROGAS =

dt_ender_TRAFICO_DE_DROGAS %>%
  geocode(ENDERECO,
          method = "arcgis",
          lat = latitude, long = longitude,
          full_results = FALSE)

coordenadas_TRAFICO_DE_DROGAS <- data.frame(dt_ender_TRAFICO_DE_DROGAS, latlon_TRAFICO_DE_DROGAS)

coordenadas_TRAFICO_DE_DROGAS

coordenadas_TRAFICO_DE_DROGAS = coordenadas_TRAFICO_DE_DROGAS %>%
  select(ENDERECO, latitude, longitude)

coordenadas_TRAFICO_DE_DROGAS

write.csv(coordenadas_TRAFICO_DE_DROGAS, file ="COORDENADAS_TRAFICO_DE_DROGAS.csv",row.names=FALSE)

setwd(file.path("~/diretorio_r/estciabh/R/scripts_coordenadas/"))#configurar diretorio
#########################################################################################################
#FIM TRATAMENTO COORDENADAS TRAFICO_DE_DROGAS
#########################################################################################################

