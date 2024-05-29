#########################################################################################################
#TRATAMENTO COORDENADAS ROUBO:
#########################################################################################################
##CRIANDO E MUDANDO O DIRETORIO PARA TRABALHAR coordenadas:
dir.create(file.path("~/tester", "coordenadas"))
setwd(file.path("~/tester/coordenadas"))
#########################################################################################################


ender_ROUBO<- banco_ROUBO_puro[,c("TIPO_LOG.ATO","NOME_LOG_ATO", "N_LOG.ATO", "BAIRRO_REGIAO_ATO", "CIDADE_ATO", "UF_ATO")]

ender_ROUBO$CIDADE_ATO <- c("BELO HORIZONTE")
ender_ROUBO$UF_ATO <- c("MINAS GERAIS")
ender_ROUBO$PAIS_ATO <- c("BRASIL")


ender_ROUBO$TIPO_LOG.ATO[ender_ROUBO$TIPO_LOG.ATO == "SEMINFO"]<- ""
ender_ROUBO$TIPO_LOG.ATO[ender_ROUBO$TIPO_LOG.ATO == "SEM INFORMACAO"]<- ""

ender_ROUBO$NOME_LOG_ATO[ender_ROUBO$NOME_LOG_ATO == "SEMINFO"]<- ""
ender_ROUBO$NOME_LOG_ATO[ender_ROUBO$NOME_LOG_ATO == "SEM INFORMACAO"]<- ""

ender_ROUBO$BAIRRO_REGIAO_ATO[ender_ROUBO$BAIRRO_REGIAO_ATO == "SEMINFO"]<- ""
ender_ROUBO$BAIRRO_REGIAO_ATO[ender_ROUBO$BAIRRO_REGIAO_ATO == "SEM INFORMACAO"]<- ""

ender_ROUBO$BAIRRO_REGIAO_ATO[ender_ROUBO$BAIRRO_REGIAO_ATO == "SEM INFORMACAO"]<- ""

ender_ROUBO$N_LOG.ATO <- gsub(" ","", ender_ROUBO$N_LOG.ATO)
ender_ROUBO$N_LOG.ATO[ender_ROUBO$N_LOG.ATO == "SN"]<- ""



ender_ROUBO <- ender_ROUBO[!(ender_ROUBO$BAIRRO_REGIAO_ATO == "VAZIO"),]

ender_ROUBO$ENDERECO = apply(ender_ROUBO, 1, function(x) paste(x[!is.na(x)], collapse=' '))

#ender_ROUBO = ender_ROUBO[!(ender_ROUBO$ENDER == "BELO HORIZONTE MINAS GERAIS"),]

library(dplyr)

dt_ender_ROUBO = ender_ROUBO %>%
  select(ENDERECO)

#TIRANDO RUA E NUMERO PARA ENTRAR NA GEOREFERENCIA:
dt_ender_ROUBO$ENDERECO[dt_ender_ROUBO$ENDERECO == "RUA NOVE 231 MANTIQUEIRA BELO HORIZONTE MINAS GERAIS BRASIL"]<- "MANTIQUEIRA BELO HORIZONTE MINAS GERAIS BRASIL"

library(XML)
library(ggmap)
library(dplyr)
library(RCurl)
library(googleway)

key <- 'AIzaSyDKYIchtAe_gwjE1R2L7sIKns2mTxDf0NI'

register_google(key = "AIzaSyDKYIchtAe_gwjE1R2L7sIKns2mTxDf0NI")



#latlon_ROUBO = geocode(dt_ender_ROUBO$ENDERECO, key=key)

latlon_ROUBO =
  
dt_ender_ROUBO %>%
  geocode(ENDERECO,
          method = "arcgis",
          lat = latitude, long = longitude,
          full_results = FALSE)

coordenadas_ROUBO <- data.frame(dt_ender_ROUBO, latlon_ROUBO)

coordenadas_ROUBO

coordenadas_ROUBO = coordenadas_ROUBO %>%
  select(ENDERECO, latitude, longitude)

coordenadas_ROUBO

write.csv(coordenadas_ROUBO, file ="COORDENADAS_ROUBO.csv",row.names=FALSE)

setwd(file.path("~/tester/scripts_v_001/"))#configurar diretorio
#########################################################################################################
#FIM TRATAMENTO COORDENADAS ROUBO
#########################################################################################################

