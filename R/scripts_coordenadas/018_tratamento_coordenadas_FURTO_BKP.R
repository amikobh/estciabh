#########################################################################################################
#TRATAMENTO COORDENADAS FURTO:
#########################################################################################################
##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR coordenadas:
dir.create(file.path("~/diretorio_r/estciabh", "coordenadas"))
setwd(file.path("~/diretorio_r/estciabh/coordenadas"))
#########################################################################################################


ender_FURTO<- banco_FURTO_puro[,c("TIPO_LOG.ATO","NOME_LOG_ATO", "N_LOG.ATO", "BAIRRO_REGIAO_ATO", "CIDADE_ATO", "UF_ATO")]

ender_FURTO$CIDADE_ATO <- c("BELO HORIZONTE")
ender_FURTO$UF_ATO <- c("MINAS GERAIS")
ender_FURTO$PAIS_ATO <- c("BRASIL")


ender_FURTO$TIPO_LOG.ATO[ender_FURTO$TIPO_LOG.ATO == "SEMINFO"]<- ""
ender_FURTO$TIPO_LOG.ATO[ender_FURTO$TIPO_LOG.ATO == "SEM INFORMACAO"]<- ""

ender_FURTO$NOME_LOG_ATO[ender_FURTO$NOME_LOG_ATO == "SEMINFO"]<- ""
ender_FURTO$NOME_LOG_ATO[ender_FURTO$NOME_LOG_ATO == "SEM INFORMACAO"]<- ""

ender_FURTO$BAIRRO_REGIAO_ATO[ender_FURTO$BAIRRO_REGIAO_ATO == "SEMINFO"]<- ""
ender_FURTO$BAIRRO_REGIAO_ATO[ender_FURTO$BAIRRO_REGIAO_ATO == "SEM INFORMACAO"]<- ""

ender_FURTO$BAIRRO_REGIAO_ATO[ender_FURTO$BAIRRO_REGIAO_ATO == "SEM INFORMACAO"]<- ""

ender_FURTO$N_LOG.ATO <- gsub(" ","", ender_FURTO$N_LOG.ATO)
ender_FURTO$N_LOG.ATO[ender_FURTO$N_LOG.ATO == "SN"]<- ""



ender_FURTO <- ender_FURTO[!(ender_FURTO$BAIRRO_REGIAO_ATO == "VAZIO"),]

ender_FURTO$ENDERECO = apply(ender_FURTO, 1, function(x) paste(x[!is.na(x)], collapse=' '))

#ender_FURTO = ender_FURTO[!(ender_FURTO$ENDER == "BELO HORIZONTE MINAS GERAIS"),]

library(dplyr)

dt_ender_FURTO = ender_FURTO %>%
  select(ENDERECO)

#TIRANDO RUA E NUMERO PARA ENTRAR NA GEOREFERENCIA:
#dt_ender_FURTO$ENDERECO[dt_ender_FURTO$ENDERECO == "RUA NOVE 231 MANTIQUEIRA BELO HORIZONTE MINAS GERAIS BRASIL"]<- "MANTIQUEIRA BELO HORIZONTE MINAS GERAIS BRASIL"

library(XML)
library(ggmap)
library(dplyr)
library(RCurl)
library(googleway)

key <- 'AIzaSyDKYIchtAe_gwjE1R2L7sIKns2mTxDf0NI'

register_google(key = "AIzaSyDKYIchtAe_gwjE1R2L7sIKns2mTxDf0NI")



#latlon_FURTO = geocode(dt_ender_FURTO$ENDERECO, key=key)

latlon_FURTO =

dt_ender_FURTO %>%
  geocode(ENDERECO,
          method = "arcgis",
          lat = latitude, long = longitude,
          full_results = FALSE)

coordenadas_FURTO <- data.frame(dt_ender_FURTO, latlon_FURTO)

coordenadas_FURTO

coordenadas_FURTO = coordenadas_FURTO %>%
  select(ENDERECO, latitude, longitude)

coordenadas_FURTO

write.csv(coordenadas_FURTO, file ="COORDENADAS_FURTO.csv",row.names=FALSE)

setwd(file.path("~/diretorio_r/estciabh/R/scripts_coordenadas/"))#configurar diretorio
#########################################################################################################
#FIM TRATAMENTO COORDENADAS FURTO
#########################################################################################################

