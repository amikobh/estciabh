#########################################################################################################
#NOMES BANCOS CRIADOS
#001 banco_inicial_bkp <- banco # salva banco assim que carregado.
#002 banco_novas_colunas = banco # salva banco com novas colunas: nome2 idade e etc
#003 banco_bkp = banco # salva banco com preenchimento de celulas vazias em ato_infracional_atas, desconsiderar ao somar e
#substitui artigos dos crimes por nomes.
#004 banco_codificado <- banco_novas_colunas # para trabalhar banco a ser entregue
#005 banco_linhas_desnecessarias # exclui linhas DUPLICIDADE, NAOLOCALIZADOSISCOM, VITIMA
#006 banco_sem_mba = filter(banco, !SE_TEM_MBA == "SIM" | !ATO_INFRACIONAL_TERMO_01 == "MBA")
#007 numero_de_casos_geral = banco com mba sem linhas desnecessarias (005)
#008 numero_de_casos_sem_mba <- banco_sem_mba (006)
#009 banco_so_com_mba <-banco[(banco$SE_TEM_MBA == "SIM" & banco$ATO_INFRACIONAL_TERMO_01 == "MBA"),]
#010 numero_de_cumprimento_mba <- banco_so_com_mba
#011 banco_linhas_necessarias_df_sem_mba <- banco_sem_mba sem ARQUIVAMENTO, ABSOLVICAO, ETC EM SENTENCA E DECISAO
#012 banco_atos_em_foco = banco_linhas_necessarias_df_sem_mba
#013 banco_com_linhas_desnecessarias <- banco_sem_mba so com ARQUIVAMENTO, ABSOLVICAO, ETC EM SENTENCA E DECISAO
#014 df_atendimento <- para grafico atendimento anual
#########################################################################################################
#########################################################################################################
#########################################################################################################
#############################################################################################################
#FUNCAO TIRAR ACENTOS E ETC
#########################################################################################################
ajustar_nomes=function(x){
  x%>%
    stringr::str_trim() %>%                        #Remove espaços em branco sobrando
    stringr::str_to_upper() %>%                    #Converte todas as strings para minusculo (lower para minusculo)
    rm_accent() %>%                                #Remove os acentos com a funcao criada acima
    stringr::str_replace_all("[/' '.()]", "") %>% #Substitui os caracteres especiais por "_"
    stringr::str_replace_all("_+", "") %>%        #Substitui os caracteres especiais por ""
    stringr::str_replace("_$", "")                 #Substitui o caracter especiais por ""
}
#############################################################################################################
#############################################################################################################
#ACERTAR SOMA COLUNA PERCENTUAL
#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#########################################################################################################
#########################################################################################################
# 3 CARREGANDO O BANCO PARA TRATAMENTO NO R: observar se variaveis são iguais
#Ao salvar o banco como .csv escolher separador ":"

setwd(file.path("~/diretorio_r/estciabh"))

banco <- read.csv("banco_atual.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 3) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
banco_inicial_bkp <- banco

dir.create(file.path("~/diretorio_r/estciabh", "planilhas"))

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
##SALVANDO BANCO ORIGINAL
#write.csv(banco_inicial_bkp, file ="banco_inicial_bkp.csv",row.names=TRUE)
#write.xlsx(banco_inicial_bkp, file ="banco_inicial_bkp.xlsx")
#  verificando o nome das variaveis pertencentes ao objeto dados:

#############################################################################################################
#########################################################################################################
# Para converter um dataframe em tibble:

banco <- as_tibble(banco)


#head(banco, n=10)[,77:84]
#########################################################################################################

# 4 TRATAMENTOS INICIAIS:

banco$NOME <- as.character(banco$NOME)

#preencher celulas sem nomes com a palavra VAZIO

banco$NOME[banco$NOME == ""]<- "VAZIO"

#excluir celulas com nomes VAZIO

banco <- banco[!(banco$NOME == "VAZIO"),]

#PREENCHER COM NA'S CELULAS VAZIAS
require(lubridate)

banco$DATA_ATO[banco$DATA_ATO == ""]<- NA
banco$DATA_ATO <- dmy(banco$DATA_ATO)


banco$DATA_AUDIENCIA_PRELIMINAR[banco$DATA_AUDIENCIA_PRELIMINAR == ""]<- NA
banco$DATA_AUDIENCIA_PRELIMINAR <- dmy(banco$DATA_AUDIENCIA_PRELIMINAR)

banco$DATA_SENTENCA[banco$DATA_SENTENCA == ""]<- NA
banco$DATA_SENTENCA <- dmy(banco$DATA_SENTENCA)

#INSERIR COLUNAS DIA_SEMANA_ATO E DIA_SEMANA_AUDIENCIA_PRELIMINAR JÁ FORMATADAS PARA CAMPO DATAS
banco$DIA_SEMANA_ATO <- format.Date((banco$DATA_ATO), "%A")
banco$DIA_SEMANA_AUDIENCIA_PRELIMINAR <- format.Date((banco$DATA_AUDIENCIA_PRELIMINAR), "%A")

#head (banco %>%
   #     select(NASCIMENTO, IDADE, DATA_ATO, DIA_SEMANA_ATO, DATA_AUDIENCIA_PRELIMINAR, DIA_SEMANA_AUDIENCIA_PRELIMINAR,
    #           DATA_SENTENCA), 15)



#INSERIR COLUNAS CIDADE_ATO, UF_ATO e PAIS PREENCHIDAS
#banco$CIDADE_ATO <- c("BELO HORIZONTE")
#banco$UF_ATO <- c("MINAS GERAIS")
banco$PAIS_ATO <- c("BRASIL")
#INSERIR COLUNA IDADE

#CONVERTE PARA DATE (DATA_ATO JÁ CONVERTIDO LINHAS ACIMA)
#banco$NASCIMENTO = dmy(banco$NASCIMENTO)
banco$NASCIMENTO = dmy(banco$NASCIMENTO)

## calcula o intervalo em anos
banco$IDADE = as.period(interval(banco$NASCIMENTO, banco$DATA_ATO))


# SEPARAR SO O PRIMEIRO ITEM DE "17y 2m 28d 0H 0M 0S" GERADO PELO SCRIPT ANTERIOR.
banco$IDADE = banco$IDADE@year
#table(banco$IDADE, useNA ="always")


banco$NOME2 <- gsub(" ","", banco$NOME)
banco$FILIACAO2 <- gsub(" ","", banco$FILIACAO)

banco <- banco[!(banco$NOME2 == "AAPURAR"),]

#EXEMPLO DE REMOVER A COLUNA
#banco$CIDADE_ATO <- NULL
banco_novas_colunas = banco
#write.csv(banco_novas_colunas, file = "banco_com_novas_colunas.csv")

##############################################################################################################################################################################################################
#########################################################################################################
banco$ATO_INFRACIONAL_TERMO_01 <- as.character(banco$ATO_INFRACIONAL_TERMO_01)
banco$ATO_INFRACIONAL_TERMO_02 <- as.character(banco$ATO_INFRACIONAL_TERMO_02)

banco$ATO_INFRACIONAL_ATA_01 <- as.character(banco$ATO_INFRACIONAL_ATA_01)
banco$ATO_INFRACIONAL_ATA_02 <- as.character(banco$ATO_INFRACIONAL_ATA_02)
banco$ATO_INFRACIONAL_ATA_03 <- as.character(banco$ATO_INFRACIONAL_ATA_03)
# preencher celula vazia de ATO_INFRACIONAL_ATA_01 e 02 por ATO_INFRACIONAL_TERMO_01 ou 02


#preenchendo vazios

banco$ATO_INFRACIONAL_ATA_01 = ifelse(banco$ATO_INFRACIONAL_ATA_01 == "",
                                      banco$ATO_INFRACIONAL_TERMO_01, banco$ATO_INFRACIONAL_ATA_01)


banco$ATO_INFRACIONAL_ATA_02 = ifelse(banco$ATO_INFRACIONAL_ATA_02 == "",
                                      banco$ATO_INFRACIONAL_TERMO_02, banco$ATO_INFRACIONAL_ATA_02)

banco$ATO_INFRACIONAL_ATA_03 = ifelse(banco$ATO_INFRACIONAL_ATA_03 == "",
                                      "VAZIO", banco$ATO_INFRACIONAL_ATA_03)

#write.csv(banco, file ="banco_varialvel_ato_vazio_preenchida.csv")

#########################################################################################################
#########################################################################################################
# 5 CODIFICAR BANCO PARA ENTREGA A INTERESSADOS, SEM OS DADOS PESSOAIS E CODIFICANDO OS NOMES COM ID:

banco_codificado <- banco_novas_colunas

banco_codificado <-banco_codificado[order(banco_codificado$NOME2),]#ordenar, crescente, nome2

#banco_codificado <- banco_codificado %>%
# mutate(ID = 1:n()) %>%
# group_by(NOME2) %>%
#mutate(ID = first(ID))

#Basta lembrar que, internamente, os níveis de um fator são armazenados como numeros inteiros, por padrão coincidentes
#com a ordem alfabética. Portanto basta converter o vetor de nomes para fator e converter para numérico:


banco_codificado$ID <- as.factor(banco_codificado$NOME2)#acrescenta variável ID com base em nome2 transformado em factor
banco_codificado$ID <- as.numeric(banco_codificado$ID) #converte para numérico codificando os nomes


#write.csv(banco_codificado, file = "banco_codificado.csv")# salva com variável ID

#apaga colunas com os dados pessoais
banco_codificado$NOME <- NULL
banco_codificado$FILIACAO <- NULL
banco_codificado$NOME2 <- NULL
banco_codificado$FILIACAO2 <- NULL

#muda ordem de apresentação dos variáveis
banco_codificado <- banco_codificado[c("ID", "NASCIMENTO", "IDADE", "SEXO", "ESTUDA_ATUALMENTE", "ESCOLARIDADE_TERMO",
                                       "SE_TEM_MBA", "MOTIVO_MBA", "ATO_INFRACIONAL_MBA", "ATO_INFRACIONAL_TERMO_01", "ATO_INFRACIONAL_TERMO_02",
                                       "DATA_ATO", "DIA_SEMANA_ATO", "HORA_ENTRADA", "ENCAMINHAMENTO", "DATA_SAIDA", "HORA_SAIDA", "N_QUEST", "DATA_APLICACAO",
                                       "ENTRVISTADOR", "RACA_COR", "ESTADO_CIVIL", "POSSUI_FILHOS", "QUANTOS", "ESTA.GRAVIDA", "POSSUI_DOC_01", "POSSUI_DOC_02",
                                       "POSSUI_DOC_03", "POSSUI_DOC_04", "POSSUI_DOC_05", "POSSUI_DOC.06", "ESTUDA_ATUALMENTE.1", "PQ_NAO_ESTUDA",
                                       "NATUREZA_ESCOLA", "SERIE_ATUAL_OU_ULTIMA_CURSADA", "TRABALHA_ATUALMENTE", "NATUREZA_DO_TRABALHO", "RENDA_MENSAL",
                                       "TEM.CURSO.PROFISSIONALIZANTE", "SE_FEZ_QUAL._01", "SE_FEZ_QUAL._02", "TEM_INTERESSE_EM_FAZER_CURSO","QUAL_AREA_DE_INTERESSE", "BAIRRO", "CIDADE",
                                       "TIPO_MORADIA", "NUMERO_MORADORES", "NUMERO_COMODOS", "NATUREZA_MORADIA", "ESTADO_CIVIL_PAIS", "COM_QUEM_RESIDE_01",
                                       "COM_QUEM_RESIDE_02", "COM_QUEM_RESIDE_03", "HA_QUANTO_TEMPO", "RENDA_FAMILIAR", "NATUREZA_DO_TRABALHO_01",
                                       "QUEM_TRABALHA._01", "NATUREZA_DO_TRABALHO_02", "QUEM_TRABALHA._02", "TEM_BENEFICIO_GOVERNAMENTAL_01",
                                       "TEM_BENEFICIO_GOVERNAMENTAL_02", "TEM_BENEFICIO_GOVERNAMENTAL_03", "TEM_BENEFICIO_GOVERNAMENTAL_04",
                                       "ATIVIDADES_GRUPOS_SOCIAIS_01", "ATIVIDADES_GRUPOS_SOCIAIS_02", "ATIVIDADES_GRUPOS_SOCIAIS_03","ATIVIDADES_GRUPOS_SOCIAIS_04",
                                       "ATIVIDADES_GRUPOS_SOCIAIS_05", "ATIVIDADES_GRUPOS_SOCIAIS_06", "ATIVIDADES_GRUPOS_SOCIAIS_07","ATIVIDADES_GRUPOS_SOCIAIS_08",

                                       "USA_DROGAS_ATUALMENTE","DROGAS_USO_01", "DROGAS_USO_02", "DROGAS_USO_03", "DROGAS_USO_04", "DROGAS_USO_05",
                                       "DROGAS_USO_06","DROGAS_USO_07", "DROGAS_USO_08","DROGA_MAIS_USADA", "TRATAMENTO_DE_DROGAS","JA_FOI_APREENDIDO","JA_FOI_INTERNADO_NO_CEIP","ESTA_CUMPRINDO_OU_CUMPRIU_MEDIDA_SOCIOEDUCATIVA",
                                       "QUAL_MEDIDA_01", "QUAL_MEDIDA_02", "QUAL_MEDIDA_03", "QUAL_MEDIDA_04",

                                       "PROCESSO", "JUIZ", "ATO_INFRACIONAL_ATA_01", "ATO_INFRACIONAL_ATA_02",
                                       "ATO_INFRACIONAL_ATA_03", "DATA_AUDIENCIA_PRELIMINAR", "DECISAO", "MEDIDA_PROTETIVA", "QUAL_MEDIDA_PROTETIVA_01",
                                       "QUAL_MEDIDA_PROTETIVA_02", "QUAL_MEDIDA_PROTETIVA_03", "COMPARECIMENTO_AUD_PRELIMINAR", "DATA_NAO_COMPARECIMENTO",
                                       "SENTENCA", "DATA_SENTENCA", "TIPO_LOG.RESIDENCIAL", "BAIRRO_REGIAO_RESIDENCIAL",
                                       "REGIONAL_RESIDENCIAL", "CIDADE_RESIDENCIAL", "UF_RESIDENCIAL", "TIPO_LOG.ATO", "NOME_LOG_ATO", "N_LOG.ATO",
                                       "BAIRRO_REGIAO_ATO", "REGIONAL_ATO", "CIDADE_ATO", "UF_ATO", "TIPO_DE_VITIMA", "ESTUDA.",
                                       "ESCOLARIDADE", "PRIMARIO", "DIA_SEMANA_AUDIENCIA_PRELIMINAR", "PAIS_ATO")]


banco_codificado <-banco_codificado[order(banco_codificado$ID),]

#salva banco codificado para entrega:
write.csv(banco_codificado, file = "banco_codificado_ENTREGA.csv", row.names = FALSE)
#write_ods(banco_codificado, "banco_codificado_ENTREGA.ods")
#write.xlsx(banco_codificado, file = "banco_codificado_ENTREGA.xlsx", row.names = FALSE)
#########################################################################################################
#########################################################################################################

#########################################################################################################
library(dplyr)
#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################

banco$ATO_INFRACIONAL_ATA_01 <- gsub(" ","", banco$ATO_INFRACIONAL_ATA_01)
banco$ATO_INFRACIONAL_ATA_02 <- gsub(" ","", banco$ATO_INFRACIONAL_ATA_02)
banco$ATO_INFRACIONAL_ATA_03 <- gsub(" ","", banco$ATO_INFRACIONAL_ATA_03)



##scripts para corrigir contagem de atos em duplicidade:
#banco$ATO_INFRACIONAL_ATA_02 = ifelse(banco$ATO_INFRACIONAL_ATA_02 == banco$ATO_INFRACIONAL_ATA_01,
                                   #   "DESCONSIDERARAOSOMAR", banco$ATO_INFRACIONAL_ATA_02)


#banco$ATO_INFRACIONAL_ATA_03 = ifelse(banco$ATO_INFRACIONAL_ATA_03 == banco$ATO_INFRACIONAL_ATA_01,
                                    #  "DESCONSIDERARAOSOMAR", banco$ATO_INFRACIONAL_ATA_03)

#########################################################################################################
#########################################################################################################
banco_bkp = banco
########################################################################################################


# 6 SEPARANDO BANCO SEM MBA PUROS (NÚMERO DE CASOS DE ENVOLVIMENTO COM ATOS INFRACIONAIS)

##BANCO COM LINHAS QUE PRECISAM SER ELIMINADAS
banco$SENTENCA <- gsub(" ","", banco$SENTENCA)

banco_linhas_descartadas <- subset (banco, SENTENCA %in% c('DUPLICIDADE', 'NAOLOCALIZADOSISCOM', 'VITIMA'))
#write.csv(banco_linhas_desnecessarias, file = "banco_linhas_desnecessarias.csv")
##BANCO SEM AS LINHAS QUE PRECISAM SER ELIMINADAS
banco <- banco[!(banco$SENTENCA == 'DUPLICIDADE' | banco$SENTENCA == 'NAOLOCALIZADOSISCOM'| banco$SENTENCA == 'VITIMA'),]

#write.csv(banco, file ="banco_sem_linhas_desnecessarias.csv",row.names=TRUE)
#write.csv(banco, file ="banco_sem_linhas_desnecessarias.csv")
#banco$NOME2 <- gsub(" ","", banco$NOME)
#banco$FILIACAO2 <- gsub(" ","", banco$FILIACAO)
#write.csv(banco, file = "banco_conferencia_mba.csv")
#table(banco$ATO_INFRACIONAL_TERMO_01)

banco$SE_TEM_MBA <- gsub(" ","", banco$SE_TEM_MBA)

#########################################################
#########################################################
# ESSA SECÃO: preencher vazios com NÃO. E arrumar o SIM e o que precisar.

banco$SE_TEM_MBA[banco$SE_TEM_MBA == ""]<- "NAO"
banco$SE_TEM_MBA[banco$SE_TEM_MBA == "SM"]<- "SIM"
banco$SE_TEM_MBA[banco$SE_TEM_MBA == "MBA"]<- "SIM"
banco$SE_TEM_MBA[banco$SE_TEM_MBA == "SE_TEM_MBA"]<- "SIM"

#table(banco$SE_TEM_MBA)
#########################################################
#########################################################
#########################################################
#########################################################

##BANCO SEM MBA
banco$ATO_INFRACIONAL_TERMO_01 <- gsub(" ","", banco$ATO_INFRACIONAL_TERMO_01)

#banco_sem_mba<-banco[!(banco$SE_TEM_MBA == "SIM" & banco$ATO_INFRACIONAL_TERMO_01 == "MBA"),]

banco_sem_mba = filter(banco, !SE_TEM_MBA == "SIM" | !ATO_INFRACIONAL_TERMO_01 == "MBA")


#write.csv(banco_sem_mba, file ="banco_sem_mba.csv",row.names=FALSE)
##write.csv(banco_sem_mba, file ="banco_sem_mba.csv")

numero_de_casos_geral = banco

numero_de_casos_sem_mba <- banco_sem_mba

# 2.1 SEPARANDO BANCO COM MBA PUROS (CUMPRIMENTO DE MANDADO DE BUSCA E APREENS?O). Observar a falta do "!".

#mba puro
banco_so_com_mba <-banco[(banco$SE_TEM_MBA == "SIM" & banco$ATO_INFRACIONAL_TERMO_01 == "MBA"),]
#mba com novo ato
banco_mba_com_ato <- banco |> filter(SE_TEM_MBA %in% "SIM")
numero_de_cumprimento_mba <- banco_so_com_mba
write.csv(banco_so_com_mba, file ="banco_so_com_mba.csv",row.names=FALSE)
#########################################################################################################
#head (banco_so_com_mba %>%
#     select(SE_TEM_MBA, ATO_INFRACIONAL_TERMO_01), 15)

#head (banco_sem_mba %>%
# select(SE_TEM_MBA, ATO_INFRACIONAL_TERMO_01), 15)
#excluindo linhas. Usar este banco para calcular numero adolescentes atendidos X atos praticados (incidencia)?
#para saber reentradas. ver com o Marcelo
banco_sem_mba$SENTENCA <- gsub(" ","", banco_sem_mba$SENTENCA)
banco_sem_mba$DECISAO <- gsub(" ","", banco_sem_mba$DECISAO)

banco_tratado <- banco_sem_mba[!(banco_sem_mba$SENTENCA == 'ARQUIVAMENTO'| banco_sem_mba$SENTENCA == 'REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA'|
                                              banco_sem_mba$SENTENCA == 'REMESSAAOJUIZOCOMPETENTE-MAIORIDADE'| banco_sem_mba$SENTENCA == 'REMESSAAOJUIZOCOMPETENTE'|
                                              banco_sem_mba$SENTENCA == 'REMESSACOMARCACOMPETENTE'| banco_sem_mba$SENTENCA == 'REMETIDOSAUTOSJ.COMPETENTE' |
                                              banco_sem_mba$SENTENCA == 'ABSOLVICAO'| banco_sem_mba$SENTENCA == 'EXTINCAODOPROCESSO'| banco_sem_mba$SENTENCA == 'EXTINCAOPORMORTE' |
                                              banco_sem_mba$SENTENCA == 'EXTINCAOPORMORTE' | banco_sem_mba$SENTENCA == 'REMESSAAOJUIZOCOMPETENTE'  |
                                              banco_sem_mba$SENTENCA == 'EXTINCAOPORPRESCRICAO' | banco_sem_mba$SENTENCA == 'EXTINCAOPORPROCESSO' |
                                              banco_sem_mba$DECISAO == 'ARQUIVAMENTO'|
                                              banco_sem_mba$DECISAO == 'REMESSAAOJUIZOCOMPETENTE-MAIORIDADE'| banco_sem_mba$DECISAO == 'REMESSAAOJUIZOCOMPETENTE'|
                                              banco_sem_mba$DECISAO == 'REMESSACOMARCACOMPETENTE'| banco_sem_mba$DECISAO == 'REMETIDOSAUTOSJ.COMPETENTE' |
                                              banco_sem_mba$DECISAO == 'ABSOLVICAO'| banco_sem_mba$DECISAO == 'EXTINCAODOPROCESSO'| banco_sem_mba$DECISAO == 'EXTINCAOPORMORTE' |
                                              banco_sem_mba$DECISAO == 'EXTINCAOPORMORTE' | banco_sem_mba$DECISAO == 'REMESSAAOJUIZOCOMPETENTE'  | banco_sem_mba$DECISAO == 'EXTINCAOPORPROCESSO'),]



banco_sem_tratamento <- banco_sem_mba[(banco_sem_mba$SENTENCA == 'ARQUIVAMENTO'| banco_sem_mba$SENTENCA == 'REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA'|
                                                         banco_sem_mba$SENTENCA == 'REMESSAAOJUIZOCOMPETENTE-MAIORIDADE'| banco_sem_mba$SENTENCA == 'REMESSAAOJUIZOCOMPETENTE'|
                                                         banco_sem_mba$SENTENCA == 'REMESSACOMARCACOMPETENTE'| banco_sem_mba$SENTENCA == 'REMETIDOSAUTOSJ.COMPETENTE' |
                                                         banco_sem_mba$SENTENCA == 'ABSOLVICAO'| banco_sem_mba$SENTENCA == 'EXTINCAODOPROCESSO'| banco_sem_mba$SENTENCA == 'EXTINCAOPORMORTE' |
                                                         banco_sem_mba$SENTENCA == 'EXTINCAOPORMORTE' | banco_sem_mba$SENTENCA == 'REMESSAAOJUIZOCOMPETENTE'  |
                                                         banco_sem_mba$SENTENCA == 'EXTINCAOPORPRESCRICAO' | banco_sem_mba$SENTENCA == 'EXTINCAOPORPROCESSO' |
                                                         banco_sem_mba$DECISAO == 'ARQUIVAMENTO'|
                                                         banco_sem_mba$DECISAO == 'REMESSAAOJUIZOCOMPETENTE-MAIORIDADE'| banco_sem_mba$DECISAO == 'REMESSAAOJUIZOCOMPETENTE'|
                                                         banco_sem_mba$DECISAO == 'REMESSACOMARCACOMPETENTE'| banco_sem_mba$DECISAO == 'REMETIDOSAUTOSJ.COMPETENTE' |
                                                         banco_sem_mba$DECISAO == 'ABSOLVICAO'| banco_sem_mba$DECISAO == 'EXTINCAODOPROCESSO'| banco_sem_mba$DECISAO == 'EXTINCAOPORMORTE' |
                                                         banco_sem_mba$DECISAO == 'EXTINCAOPORMORTE' | banco_sem_mba$DECISAO == 'REMESSAAOJUIZOCOMPETENTE'  | banco_sem_mba$DECISAO == 'EXTINCAOPORPROCESSO'),]


setwd(file.path("~/diretorio_r/estciabh/planilhas/"))

write.csv(banco_tratado, file ="banco_tratado.csv",row.names=FALSE)
write.csv(banco_sem_tratamento, file ="banco_sem_tratamento.csv")

#salvar para tratamento atos em foco
banco_atos_em_foco = banco_tratado
#########################################################################################################
#tratamento atos em foco:
#########################################################################################################
banco_atos_em_foco_bkp = banco_atos_em_foco
#########################################################################################################
#########################################################################################################
#OBSERVAÇÃO: A GERAÇÃO DO GRAFICO SE ENCONTRA NO FINAL DO SCRIPT 004_tabela_e_grafico_idade_sexo_geral_flex_bkp1 EM
#RAZÃO DA VARIAVEL ADOLESCENTES.
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
#########################################################################################################
#FIM
#########################################################################################################
