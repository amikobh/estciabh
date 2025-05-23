tempo_script <- system.time({
# Import all data
setwd("/home/amikobh/diretorio_r/estciabh/R")

source('001_instalar_pacotes_bkp01.R')

#Sys.sleep(1)
source('002_tratamentos_iniciais_bkp4_flex_idade.R')
#Sys.sleep(1)
source('003_tratamento_incidencia_flex_bkp8_webshot3.R')
#Sys.sleep(1)
source('004_tabela_e_grafico_idade_sexo_geral_flex_bkp1.R')
#Sys.sleep(1)
source('005_tabela_e_grafico_regional_residencia_geral_flex_bkp2_webshot3.R')
#Sys.sleep(1)
source('006_tabelas_atos_em_foco_flex_bkp3_webshot3.R')
#Sys.sleep(1)
source('007_tabelas_dia_semana_e_regionais_atos_em_foco_flex_bkp2_web3.R')
#Sys.sleep(3)
source('008_tabela_e_grafico_idade_sexo_atos_em_foco_flex_bkp2.R')
#Sys.sleep(3)
source('009_tratamento_amostra_flex_bkp4_web3.R')
#Sys.sleep(5)
source('010_tratamento_medidas_flex_bkp3_web3.R')
#Sys.sleep(1)
source('011_tratamento_so_decisao_flex_bkp1_web3.R')
#Sys.sleep(1)
source('012_tratamento_tempo_decisao_flex_bkp1_web3.R')
#Sys.sleep(1)
source('013_tratamento_so_sentenca_flex_bkp3_web3.R')
#Sys.sleep(1)
source('014_tratamento_tempo_sentenca_flex_bkp5_web3.R')
#Sys.sleep(1)
source('015_tratamento_escola_flex_BKP5_web3.R')
#Sys.sleep(5)
source('021_tratamento_flex_jr_web2_bkp2.R')
#Sys.sleep(5)
source('023_tratamento_flex_mba_web2_bkp1.R')
#Sys.sleep(1)
source('024_tratamento_CEDIPRO_bkp04.R')
#Sys.sleep(5)
source('028_corre_legal_bkp03.R')
#Sys.sleep(8)
#Sys.sleep(5)
source('022_letalidade_flex_BKP08_web4.R')

})

# Exibindo o tempo de execução
print(tempo_script)

tempo_graficos <- system.time({
source('025_tabelas_BKP06_gt.R')
#Sys.sleep(5)
source('026_graficos_BKP06.R')
#Sys.sleep(5)
})

# Exibindo o tempo de execução
print(tempo_graficos)


tempo_render <- system.time({

setwd("/home/amikobh/diretorio_r/estciabh")
bookdown::render_book("index.Rmd", "bookdown::pdf_book")

})

# Exibindo o tempo de execução
print(tempo_render)
