#########################################################################################################
#TRATAMENTO COORDENADAS HOMICIDIO:
#########################################################################################################
##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR coordenadas:
dir.create(file.path("~/tester", "coordenadas"))
setwd(file.path("~/tester/coordenadas"))
#########################################################################################################


ender_HOMICIDIO<- banco_HOMICIDIO_puro[,c("TIPO_LOG.ATO","NOME_LOG_ATO", "N_LOG.ATO", "BAIRRO_REGIAO_ATO", "CIDADE_ATO", "UF_ATO")]

ender_HOMICIDIO$CIDADE_ATO <- c("BELO HORIZONTE")
ender_HOMICIDIO$UF_ATO <- c("MINAS GERAIS")
ender_HOMICIDIO$PAIS_ATO <- c("BRASIL")


ender_HOMICIDIO$TIPO_LOG.ATO[ender_HOMICIDIO$TIPO_LOG.ATO == "SEMINFO"]<- ""
ender_HOMICIDIO$TIPO_LOG.ATO[ender_HOMICIDIO$TIPO_LOG.ATO == "SEM INFORMACAO"]<- ""

ender_HOMICIDIO$NOME_LOG_ATO[ender_HOMICIDIO$NOME_LOG_ATO == "SEMINFO"]<- ""
ender_HOMICIDIO$NOME_LOG_ATO[ender_HOMICIDIO$NOME_LOG_ATO == "SEM INFORMACAO"]<- ""

ender_HOMICIDIO$BAIRRO_REGIAO_ATO[ender_HOMICIDIO$BAIRRO_REGIAO_ATO == "SEMINFO"]<- ""
ender_HOMICIDIO$BAIRRO_REGIAO_ATO[ender_HOMICIDIO$BAIRRO_REGIAO_ATO == "SEM INFORMACAO"]<- ""

ender_HOMICIDIO$BAIRRO_REGIAO_ATO[ender_HOMICIDIO$BAIRRO_REGIAO_ATO == "SEM INFORMACAO"]<- ""

ender_HOMICIDIO$N_LOG.ATO <- gsub(" ","", ender_HOMICIDIO$N_LOG.ATO)
ender_HOMICIDIO$N_LOG.ATO[ender_HOMICIDIO$N_LOG.ATO == "SN"]<- ""



ender_HOMICIDIO <- ender_HOMICIDIO[!(ender_HOMICIDIO$BAIRRO_REGIAO_ATO == "VAZIO"),]

ender_HOMICIDIO$ENDERECO = apply(ender_HOMICIDIO, 1, function(x) paste(x[!is.na(x)], collapse=' '))

#ender_HOMICIDIO = ender_HOMICIDIO[!(ender_HOMICIDIO$ENDER == "BELO HORIZONTE MINAS GERAIS"),]

library(dplyr)

dt_ender_HOMICIDIO = ender_HOMICIDIO %>%
  select(ENDERECO)

#TIRANDO RUA E NUMERO PARA ENTRAR NA GEOREFERENCIA:
dt_ender_HOMICIDIO$ENDERECO[dt_ender_HOMICIDIO$ENDERECO == "RUA NOVE 231 MANTIQUEIRA BELO HORIZONTE MINAS GERAIS BRASIL"]<- "MANTIQUEIRA BELO HORIZONTE MINAS GERAIS BRASIL"

library(XML)
library(ggmap)
library(dplyr)
library(RCurl)
library(googleway)

key <- 'AIzaSyDKYIchtAe_gwjE1R2L7sIKns2mTxDf0NI'

register_google(key = "AIzaSyDKYIchtAe_gwjE1R2L7sIKns2mTxDf0NI")



#latlon_HOMICIDIO = geocode(dt_ender_HOMICIDIO$ENDERECO, key=key)

latlon_HOMICIDIO =
  
dt_ender_HOMICIDIO %>%
  geocode(ENDERECO,
          method = "arcgis",
          lat = latitude, long = longitude,
          full_results = FALSE)

coordenadas_HOMICIDIO <- data.frame(dt_ender_HOMICIDIO, latlon_HOMICIDIO)

coordenadas_HOMICIDIO

coordenadas_HOMICIDIO = coordenadas_HOMICIDIO %>%
  select(ENDERECO, latitude, longitude)

coordenadas_HOMICIDIO

write.csv(coordenadas_HOMICIDIO, file ="COORDENADAS_HOMICIDIO.csv",row.names=FALSE)

setwd(file.path("~/tester/scripts_v_001/"))#configurar diretorio
#########################################################################################################
#FIM TRATAMENTO COORDENADAS HOMICIDIO
#########################################################################################################

