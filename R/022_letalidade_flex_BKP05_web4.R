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

# 1 PROCEDIMENTOS INICIAIS : LIMPAR OBJETOS

#rm(list=ls(all=TRUE))

# 2) DEFINIR DIRETÓRIO DE TRABALHO: usar Ctrl+Shift+H e escolher diretório
dir.create(file.path("~/diretorio_r/estciabh", "letalidade"))
dir.create(file.path("~/diretorio_r/estciabh/letalidade", "planilhas"))
#########################################################################################################
#########################################################################################################
#tratamentos banco recebido IML

setwd(file.path("~/diretorio_r/estciabh/letalidade"))


banco_iml_inicial <- read.csv("iml_inicial_2022.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8") ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
#banco_letalidade02 <- read.csv("letalidade02.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8") ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
#banco_letalidade <- read.csv("letalidade_2021.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8") ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

library(dplyr)
#banco_iml_inicial <-  plyr::rename( banco_iml_inicial, c( "Requisicao" = "REQUISICAO",
#                                                      "Data.entrada" = "ENTRADA",
#                                                         "Data.necropsia" = "DATA_NECROPSIA",
#                                                         "data.DO" = "DATA_DO",
#                                                         "Data.Saída" = "DATA_SAIDA",
#                                                         "Tipo.de.Pericia" = "TIPO_PERICIA",
#                                                         "Nome" = "NOME",
#                                                         "Permanece.Desconhecido." = "PERMANECE_DESCONHECIDO",
#                                                         "Entrou.como.Desconhecido." = "ENTROU_DESCONHECIDO",
##                                                         "Mae" = "MAE",
#                                                        "Nascimento" = "NASCIMENTO",
#                                                         "Idade" = "IDADE",
#                                                         "Sexo" = "SEXO",
#                                                         "Cor" = "COR",
#                                                         "Procedencia" = "PROCEDENCIA",
#                                                         "EspecificacaoProcedencia" = "ESPECIFICACAO_PROCEDENCIA",
#                                                         "Endereco" = "ENDERECO",
#                                                         "Municipio.Ocorrência" = "MUNICIPIO_OCORRENCIA",
#                                                         "LatitudeSt" = "LATITUDE_OCORRENCIA",
#                                                         "LongitudeSt" = "LONGITUDE_OCORRENCIA",
#                                                         "NumeroDo" = "NUM_DO",
#                                                         "Causa.da.Morte.na.DO" = "CAUSA_MORTE_DO",
#                                                         "AcidenteTrabalho" = "ACIDENTE_TRABALHO",
#                                                         "Dna" = "DNA",
#                                                         "Papiloscopia" = "PAPILOSCOPIA",
#                                                         "Odontologia" = "ODONTOLOGIA",
#                                                         "CausaJuridicaMorte" = "CAUSA_JURIDICA",
#                                                         "Reds" = "REDS",
#                                                         "NaturezaReds" = "NATUREZA_REDS"))


#########################################################################################################
banco_iml_inicial =
  banco_iml_inicial %>%
  clean_names() # Limpar os alunos das variáveis

#########################################################################################################

colnames(banco_iml_inicial)[1]<-'ID'
colnames(banco_iml_inicial)[2]<-'REQUISICAO'
colnames(banco_iml_inicial)[3]<-'ENTRADA'
colnames(banco_iml_inicial)[4]<-'NECROPSIA'

colnames(banco_iml_inicial)[5]<-'DATA_DO'
colnames(banco_iml_inicial)[6]<-'DATA_SAIDA_IML'
colnames(banco_iml_inicial)[7]<-'CODIGO_PERICIA'
colnames(banco_iml_inicial)[8]<-'NOME'

colnames(banco_iml_inicial)[9]<-'DESCONHECIDO'
colnames(banco_iml_inicial)[10]<-'FOI_DESCONHECIDO'
colnames(banco_iml_inicial)[11]<-'ENDERECO'
colnames(banco_iml_inicial)[12]<-'MUNICIPIO'

colnames(banco_iml_inicial)[13]<-'MAE'
colnames(banco_iml_inicial)[14]<-'NASCIMENTO'
colnames(banco_iml_inicial)[15]<-'LATITUDE'
colnames(banco_iml_inicial)[16]<-'LONGITUDE'

colnames(banco_iml_inicial)[17]<-'NUMERO_DO'
colnames(banco_iml_inicial)[18]<-'CAUSA_MORTE'
colnames(banco_iml_inicial)[19]<-'EVOLUCAO_FINAL_CASO'
colnames(banco_iml_inicial)[20]<-'SEXO'

colnames(banco_iml_inicial)[21]<-'COR'
colnames(banco_iml_inicial)[22]<-'ACIDENTE_TRABALHO'
colnames(banco_iml_inicial)[23]<-'EVOLUCAO_FINAL_CASO_1'
colnames(banco_iml_inicial)[24]<-'DNA'

colnames(banco_iml_inicial)[25]<-'PASPILOSCOPIA'
colnames(banco_iml_inicial)[26]<-'ODONTOLOGIA'
colnames(banco_iml_inicial)[27]<-'CAUSA_JURIDICA'
colnames(banco_iml_inicial)[28]<-'SCDL_INT'

colnames(banco_iml_inicial)[29]<-'REDS'
colnames(banco_iml_inicial)[30]<-'NATUREZA_REDS'
colnames(banco_iml_inicial)[31]<-'PROCEDENCIA'
colnames(banco_iml_inicial)[32]<-'ESPECIFICACAO_PROCEDENCIA'


#acrescentando ID:
banco_iml_inicial$ID_1 <- seq_along(banco_iml_inicial[,1])

banco_iml_inicial_bkp <- banco_iml_inicial

#########################################################################################################
#########################################################################################################
##EVITAR ERRO DE PREENCHIMENTO DE DATA USADA

banco_iml_inicial = banco_iml_inicial %>%
  mutate(ENTRADA2 = substr(banco_iml_inicial$ENTRADA, start = 1, stop = 10))

banco_iml_inicial$ENTRADA2 = dmy(banco_iml_inicial$ENTRADA2)


#banco_iml_inicial$NASCIMENTO = as.Date(banco_iml_inicial$NASCIMENTO)
banco_iml_inicial$NASCIMENTO2 = dmy(banco_iml_inicial$NASCIMENTO)

banco_iml_inicial =

  banco_iml_inicial %>%
  #mutate(NASCIMENTO3 = NASCIMENTO2) %>%
  mutate(NASCIMENTO3 = case_when(NASCIMENTO2 > base::Sys.Date() ~ format(NASCIMENTO2, "19%y-%m-%d"),
                                 TRUE ~ format(NASCIMENTO2)))

#banco_iml_inicial$NASCIMENTO2 = as.Date(banco_iml_inicial$NASCIMENTO2)

banco_iml_inicial$NASCIMENTO4 = as.Date(banco_iml_inicial$NASCIMENTO3)

#banco_PARA_CAI$NASCIMENTO3 = str_replace_all( banco_PARA_CAI$NASCIMENTO,"/", "-")
#banco_PARA_CAI$NASCIMENTO4 = as.Date(banco_PARA_CAI$NASCIMENTO, "%m-%d-%Y")

## calcula o intervalo em anos
banco_iml_inicial$IDADE2 = as.period(interval(banco_iml_inicial$NASCIMENTO4, banco_iml_inicial$ENTRADA2))

# SEPARAR SO O PRIMEIRO ITEM DE "17y 2m 28d 0H 0M 0S" GERADO PELO SCRIPT ANTERIOR.
banco_iml_inicial$IDADE2 = banco_iml_inicial$IDADE2@year
#########################################################################################################
#########################################################################################################

library(abjutils)
banco_iml_inicial$MUNICIPIO2 = ajustar_nomes(banco_iml_inicial$MUNICIPIO)
library(abjutils)
banco_iml_inicial$PERMANECE_DESCONHECIDO2 = banco_iml_inicial$DESCONHECIDO

#########################################################################################################
#FILTROS INICIAIS
#FILTRO SE FOI RECONHECIDO = 0

banco_iml_inicial_permanece_desconhecido =
  banco_iml_inicial %>%
  filter(PERMANECE_DESCONHECIDO2 %in% c(1))

banco_iml_inicial =
  banco_iml_inicial %>%
  filter(PERMANECE_DESCONHECIDO2 %in% c(0))
#########################################################################################################
banco_iml_inicial$CAUSA_JURIDICA = ajustar_nomes(banco_iml_inicial$CAUSA_JURIDICA)
#banco_iml_inicial$CAUSA_JURIDICA[banco_iml_inicial$CAUSA_JURIDICA == "MISSING"]<- "IGNORADA"
#banco_iml_inicial$CAUSA_JURIDICA[banco_iml_inicial$CAUSA_JURIDICA == ""]<- "MISSING"

#table(banco_iml_inicial$CAUSA_JURIDICA)

#BANCO para ser analisado pelo iml os casos células vazias em CAUSA_JURIDICA
#banco_missing = banco_iml_inicial %>% filter(CAUSA_JURIDICA == "MISSING")
#banco_missing$CAUSA_JURIDICA[banco_missing$CAUSA_JURIDICA == "MISSING"]<- ""

#write.xlsx(banco_missing, 'banco_sem_dados_causa_juridica.xlsx')
#write.csv(banco_missing, file = "banco_sem_dados_causa_juridica.csv", row.names = FALSE)
#########################################################################################################
#########################################################################################################
#SCRIPT PARA TRATAMENTO DO BANCO QUE RETORNOU COM NOVO PREENCHIMENTO DA VARIAVEL CAUSA_JURIDICA
#tratamentos banco recebido IML

setwd(file.path("~/diretorio_r/estciabh/letalidade"))


banco_iml_inicial_novo <- read.csv("iml_inicial_2022_novo.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8") ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
#banco_letalidade02 <- read.csv("letalidade02.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8") ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
#banco_letalidade <- read.csv("letalidade_2021.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8") ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

#########################################################################################################
#PREENCHENDO OS VAZIOS COM MISSING
banco_iml_inicial_novo$CAUSA_JURIDICA[banco_iml_inicial_novo$CAUSA_JURIDICA == ""]<- "MISSING"

#########################################################################################################
#SEPARANDO A VARIAVEL PARA JUNTAR DEPOIS

banco_iml_inicial_novo =
  banco_iml_inicial_novo %>%
  select(ID, CAUSA_JURIDICA)

#########################################################################################################
#JUNTANDO
banco_iml_inicial_atualizado =
  banco_iml_inicial |>
  full_join(banco_iml_inicial_novo, by = "ID")
#########################################################################################################
#ARRUMANDO VARIÁVEL CAUSA JURÍDICA
banco_iml_inicial_atualizado =
  banco_iml_inicial_atualizado |>
  mutate(CAUSA_JURIDICA = ifelse( (CAUSA_JURIDICA.y %in% NA), CAUSA_JURIDICA.x, CAUSA_JURIDICA.y)) |>
  #excluindo as variáveis desnecessárias
  select (-c(CAUSA_JURIDICA.x, CAUSA_JURIDICA.y))
#########################################################################################################
banco_iml_inicial_atualizado$CAUSA_JURIDICA = ajustar_nomes(banco_iml_inicial_atualizado$CAUSA_JURIDICA)
#table(banco_iml_inicial_atualizado$CAUSA_JURIDICA)

banco_iml_inicial_atualizado$MUNICIPIO_OCORRENCIA2 = ajustar_nomes(banco_iml_inicial_atualizado$MUNICIPIO)


banco_iml_inicial_atualizado$CAUSA_JURIDICA[banco_iml_inicial_atualizado$CAUSA_JURIDICA == "ACIDENTE-NAOESPECIFICADO"]<- "ACIDENTE-NÃO ESPECIFICADO"
banco_iml_inicial_atualizado$CAUSA_JURIDICA[banco_iml_inicial_atualizado$CAUSA_JURIDICA == "HOMICIDIO-"]<- "HOMICÍDIO"
banco_iml_inicial_atualizado$CAUSA_JURIDICA[banco_iml_inicial_atualizado$CAUSA_JURIDICA == "HOMICIDIO"]<- "HOMICÍDIO"
banco_iml_inicial_atualizado$CAUSA_JURIDICA[banco_iml_inicial_atualizado$CAUSA_JURIDICA == "IGNORADA-"]<- "IGNORADA"
banco_iml_inicial_atualizado$CAUSA_JURIDICA[banco_iml_inicial_atualizado$CAUSA_JURIDICA == "NATURAL-"]<- "NATURAL"
banco_iml_inicial_atualizado$CAUSA_JURIDICA[banco_iml_inicial_atualizado$CAUSA_JURIDICA == "SUICIDIO-"]<- "SUICÍDIO"
banco_iml_inicial_atualizado$CAUSA_JURIDICA[banco_iml_inicial_atualizado$CAUSA_JURIDICA == "SUICIDIO"]<- "SUICÍDIO"

#AGREGAR ACIDENTE
banco_iml_inicial_atualizado$CAUSA_JURIDICA <- str_replace(banco_iml_inicial_atualizado$CAUSA_JURIDICA, "ACIDENT.*", "ACIDENTE")
banco_iml_inicial_atualizado$CAUSA_JURIDICA <- str_replace(banco_iml_inicial_atualizado$CAUSA_JURIDICA, "HOMIC.*", "HOMICÍDIO")
banco_iml_inicial_atualizado$CAUSA_JURIDICA <- str_replace(banco_iml_inicial_atualizado$CAUSA_JURIDICA, "SUIC.*", "SUICÍDIO")
banco_iml_inicial_atualizado$CAUSA_JURIDICA <- str_replace(banco_iml_inicial_atualizado$CAUSA_JURIDICA, "NATU.*", "NATURAL")
#bkp para filtros e utilizar depois

#########################################################################################################
##ATENÇÃO: PRIMEIRA ETAPA. OBJETIVO: entregar banco tratado para consulta das cais.
#APLICAÇÃO FILTROS PARA RETIRADA DAS CAIS:

banco_PARA_CAI = banco_iml_inicial_atualizado

#banco_PARA_CAI$NASCIMENTO3 = str_replace_all( banco_PARA_CAI$NASCIMENTO,"/", "-")
#banco_PARA_CAI$NASCIMENTO4 = as.Date(banco_PARA_CAI$NASCIMENTO, "%m-%d-%Y")

## calcula o intervalo em anos

#########################################################################################################
#########################################################################################################
#filtrar data (considerando ano em curso, *1= 1 ano atrás. *2= 2 anos atrás e assim sucessivamente)
banco_PARA_CAI = banco_PARA_CAI %>%
  filter(ENTRADA2 >= (str_c(format(Sys.Date()-365*2, "%Y"), "-01-01")) &
           ENTRADA2 <= (str_c(format(Sys.Date()-365*2, "%Y"), "-12-31")))
#########################################################################################################
#########################################################################################################
banco_PARA_CAI_SEM_municipios_limitrofes =
  banco_PARA_CAI %>%
  filter(!MUNICIPIO2 %in% c("BELOHORIZONTE", "CONTAGEM","NOVALIMA",
                            "RIBEIRAODASNEVES", "SABARA", "SANTALUZIA",
                            "VESPASIANO"))

banco_PARA_CAI =
  banco_PARA_CAI %>%
  filter(MUNICIPIO2 %in% c("BELOHORIZONTE", "CONTAGEM","NOVALIMA",
                           "RIBEIRAODASNEVES", "SABARA", "SANTALUZIA",
                           "VESPASIANO"))

banco_PARA_CAI_COM_municipios_limitrofes = banco_PARA_CAI

banco_PARA_CAI_12_20= banco_PARA_CAI %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20)

banco_PARA_CAI_12_20_INICIAL = banco_PARA_CAI_12_20

#library(readODS)
##write_ods(banco_PARA_CAI_12_20_INICIAL, "banco_PARA_CAI_12_20_INICIAL.ods")
##write_ods(banco_PARA_CAI_INICIAL, "banco_PARA_CAI_INICIAL.ods")

setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))

write.csv(banco_PARA_CAI_12_20_INICIAL, file = "banco_PARA_CAI_12_20_INICIAL.csv", row.names = FALSE)
##write_ods(banco_PARA_CAI_12_20_INICIAL, "banco_PARA_CAI_12_20_INICIAL.ods")

#########################################################################################################
#FIM APLICAÇÃO FILTROS PARA RETIRADA DAS CAIS
#########################################################################################################
#########################################################################################################
##### NOVA SECAO:
##ATENÇÃO: SEGUNDA ETAPA. OBJETIVO: tratar o banco preenchido com as cais e juntá-lo ao banco inicial.

#tratamento banco recebido Selmara:

setwd(file.path("~/diretorio_r/estciabh/letalidade"))

banco_COM_CAI_inicial <- read.csv("banco_com_cai_inicial.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8") ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

banco_COM_CAI_inicial_bkp = banco_COM_CAI_inicial

#########################################################################################################
#Preparando o banco para juntar ao banco iml inicial. SEPARANDO AS VARIÁVEIS NAO COINCIDENTES:
#mudando possição das colunas para o final do banco

banco_COM_CAI_inicial =
  banco_COM_CAI_inicial |>
  relocate(ID, ID_1, .after = last_col())

#selecionando somentes as colunas do banco que retornou com a CAI
banco_COM_CAI_inicial =
  banco_COM_CAI_inicial |>
  select(PASSAGEM_CIA:last_col())

banco_COM_CAI_inicial =
  banco_COM_CAI_inicial |>
  relocate(ID, ID_1)

#########################################################################################################
#JUNTANDO. VERIFICAR AQUI SE O BANCO A SE JUNTAR ESTÁ CORRETO

banco_iml_GERAL =
  banco_iml_inicial_atualizado |>
  full_join(banco_COM_CAI_inicial, by = c("ID", "ID_1"))

#########################################################################################################
#acertando variaveis datas:
banco_iml_GERAL$DATA_ULTIMA_ENTRADA2 = dmy(banco_iml_GERAL$DATA_ULTIMA_ENTRADA)
banco_iml_GERAL$DATA_PRIMEIRA_ENTRADA2 = dmy(banco_iml_GERAL$DATA_PRIMEIRA_ENTRADA)
#########################################################################################################
colnames(banco_iml_GERAL)[42]<-'PASSAGEM_CIABH'
banco_iml_GERAL_bkp = banco_iml_GERAL

#########################################################################################################
#########################################################################################################
#retirando linha em duplicidade:

#banco_iml_GERAL |> tabyl(CAUSA_JURIDICA)
#verificar se linhas a serem excluídas
banco_iml_GERAL =
  banco_iml_GERAL |>
  filter(!CAUSA_JURIDICA %in% "DESCONSIDERARPORDUPLICIDADE")

#INICIO SCRIPTS RELATORIO
#########################################################################################################
#########################################################################################################
banco_letalidade = banco_iml_GERAL
#########################################################################################################
#CARREGANDO O BANCO PARA TRATAMENTO NO R: observar se variaveis são iguais
#Ao salvar o banco como .csv escolher separador ":"
setwd(file.path("~/diretorio_r/estciabh/letalidade"))

library(dplyr)
#############################################################################################################
#FUNCAO TIRAR ACENTOS E ETC
#########################################################################################################
#filtrando quando não funciona o script filter acima

banco_letalidade$PERMANECE_DESCONHECIDO[banco_letalidade$PERMANECE_DESCONHECIDO2 == 0] <- "NAO"


banco_letalidade =
  banco_letalidade %>%
  filter(PERMANECE_DESCONHECIDO == "NAO")


#########################################################################################################
#filtros banco_letalidade
#filtrar data (considerando ano em curso, *1= 1 ano atrás. *2= 2 anos atrás e assim sucessivamente)
#banco_letalidade = banco_letalidade %>%
#filter(ENTRADA2 >= (str_c(format(Sys.Date()-365*2, "%Y"), "-01-01")) &
#     ENTRADA2 <= (str_c(format(Sys.Date()-365*2, "%Y"), "-12-31")))

#filtrando quando não funciona o script filter acima
banco_letalidade<-subset(banco_letalidade,ENTRADA2 >= (str_c(format(Sys.Date()-365*2, "%Y"), "-01-01")) &
                           ENTRADA2 <= (str_c(format(Sys.Date()-365*2, "%Y"), "-12-31")))

#########################################################################################################
library(abjutils)

banco_letalidade$CAUSA_JURIDICA <- as.character(banco_letalidade$CAUSA_JURIDICA)

#table(banco_letalidade$CAUSA_JURIDICA)

banco_letalidade$MUNICIPIO_OCORRENCIA2 = ajustar_nomes(banco_letalidade$MUNICIPIO)

#table(banco_letalidade$MUNICIPIO_OCORRENCIA2)

banco_letalidade$CAUSA_JURIDICA = ajustar_nomes(banco_letalidade$CAUSA_JURIDICA)


banco_letalidade$CAUSA_JURIDICA[banco_letalidade$CAUSA_JURIDICA == "ACIDENTE-NAOESPECIFICADO"]<- "ACIDENTE-NÃO ESPECIFICADO"
banco_letalidade$CAUSA_JURIDICA[banco_letalidade$CAUSA_JURIDICA == "HOMICIDIO-"]<- "HOMICÍDIO"
banco_letalidade$CAUSA_JURIDICA[banco_letalidade$CAUSA_JURIDICA == "HOMICIDIO"]<- "HOMICÍDIO"
banco_letalidade$CAUSA_JURIDICA[banco_letalidade$CAUSA_JURIDICA == "IGNORADA-"]<- "IGNORADA"
banco_letalidade$CAUSA_JURIDICA[banco_letalidade$CAUSA_JURIDICA == "NATURAL-"]<- "NATURAL"
banco_letalidade$CAUSA_JURIDICA[banco_letalidade$CAUSA_JURIDICA == "SUICIDIO-"]<- "SUICÍDIO"
banco_letalidade$CAUSA_JURIDICA[banco_letalidade$CAUSA_JURIDICA == "SUICIDIO"]<- "SUICÍDIO"

#AGREGAR ACIDENTE
banco_letalidade$CAUSA_JURIDICA <- str_replace(banco_letalidade$CAUSA_JURIDICA, "ACIDENT.*", "ACIDENTE")
banco_letalidade$CAUSA_JURIDICA <- str_replace(banco_letalidade$CAUSA_JURIDICA, "HOMIC.*", "HOMICÍDIO")
banco_letalidade$CAUSA_JURIDICA <- str_replace(banco_letalidade$CAUSA_JURIDICA, "SUIC.*", "SUICÍDIO")
banco_letalidade$CAUSA_JURIDICA <- str_replace(banco_letalidade$CAUSA_JURIDICA, "NATU.*", "NATURAL")
#bkp para filtros e utilizar depois

banco_letalidade |> tabyl(CAUSA_JURIDICA)
#########################################################################################################
banco_letalidade_inicial_SEM_FILTRO_IDADE_bkp <- banco_letalidade
#########################################################################################################
#########################################################################################################
banco_letalidade$MUNICIPIO_OCORRENCIA2 = ajustar_nomes(banco_letalidade$MUNICIPIO_OCORRENCIA)

banco_letalidade =
  banco_letalidade %>%
  filter(MUNICIPIO_OCORRENCIA2 %in% c("BELOHORIZONTE", "CONTAGEM","NOVALIMA",
                                      "RIBEIRAODASNEVES", "SABARA", "SANTALUZIA",
                                      "VESPASIANO"))
#########################################################################################################
banco_letalidade_12_20 =
  banco_letalidade %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20)
#########################################################################################################


#########################################################################################################
###CAUSA_JURIDICA_let
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/letalidade"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

CAUSA_JURIDICA_let =
  banco_letalidade_12_20 %>%
  select(CAUSA_JURIDICA)
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
# salvando para gráfico
CAUSA_JURIDICA_let_bkp = CAUSA_JURIDICA_let

CAUSA_JURIDICA_let_bkp =
  CAUSA_JURIDICA_let_bkp %>%
  janitor::tabyl(CAUSA_JURIDICA) %>%
  arrange(desc(n)) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR

colnames(CAUSA_JURIDICA_let_bkp)[1]<-'CAUSA_JURIDICA_let_bkp'
colnames(CAUSA_JURIDICA_let_bkp)[2]<-'QUANTIDADE'
colnames(CAUSA_JURIDICA_let_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

CAUSA_JURIDICA_let =
  CAUSA_JURIDICA_let %>%
  janitor::tabyl(CAUSA_JURIDICA) %>%
  arrange(CAUSA_JURIDICA) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR

colnames(CAUSA_JURIDICA_let)[1]<-'CAUSA JURÍDICA'
colnames(CAUSA_JURIDICA_let)[2]<-'QUANTIDADE'
colnames(CAUSA_JURIDICA_let)[3]<-'PERCENTUAL'

#############################################################################################################
#SALVANDO PARA USAR PRÓXIMO ANO
setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))
write.csv(CAUSA_JURIDICA_let, file ="CAUSA_JURIDICA_let_ATUAL.csv",row.names=F)
#########################################################################################################
#juntando tudo: OBSERVAR O SEPARADOR LINHA ABAIXO se sep="," ou se sep="|" para dar certo
setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))

CAUSA_JURIDICA_let_ANTERIOR <- read.csv("CAUSA_JURIDICA_let_ANTERIOR.csv", header=TRUE, sep=",", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

colnames(CAUSA_JURIDICA_let_ANTERIOR) <- c("CAUSA_JURÍDICA", "QUANTIDADE", "PERCENTUAL")
colnames(CAUSA_JURIDICA_let) <- c("CAUSA_JURÍDICA", "QUANTIDADE", "PERCENTUAL")
# Adaptando:
#SUBSTITUIR

CAUSA_JURIDICA_let$CAUSA_JURÍDICA[CAUSA_JURIDICA_let$CAUSA_JURÍDICA == "Total"]<- "TOTAL"

CAUSA_JURIDICA_let_TOTAL = full_join(CAUSA_JURIDICA_let_ANTERIOR, CAUSA_JURIDICA_let ,by="CAUSA_JURÍDICA")

colnames(CAUSA_JURIDICA_let_TOTAL) <- c("CAUSA JURÍDICA", "2021", "%21", "2022", "%22")
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# CAUSA_JURIDICA_let FIM
#########################################################################################################
#########################################################################################################
###soma_intervalo_idade_HOMICIDIO_let
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/letalidade"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

soma_intervalo_idade_HOMICIDIO_let =
  banco_letalidade_inicial_SEM_FILTRO_IDADE_bkp %>%
  filter (CAUSA_JURIDICA == "HOMICÍDIO")
#########################################################################################################
#########################################################################################################
#########################################################################################################
#classificando

soma_intervalo_idade_HOMICIDIO_let$INTER_03 = ifelse(soma_intervalo_idade_HOMICIDIO_let$IDADE2 < 12,
                                                     "ATÉ 11 ANOS", "DESCONSIDERAR")

soma_intervalo_idade_HOMICIDIO_let$INTER_04 = ifelse(soma_intervalo_idade_HOMICIDIO_let$IDADE2 >= 12 & soma_intervalo_idade_HOMICIDIO_let$IDADE2 <= 20,
                                                     "ENTRE 12 e 20 ANOS", "DESCONSIDERAR")

soma_intervalo_idade_HOMICIDIO_let$INTER_05 = ifelse(soma_intervalo_idade_HOMICIDIO_let$IDADE2 >= 21 & soma_intervalo_idade_HOMICIDIO_let$IDADE2 <= 29,
                                                     "ENTRE 21 e 29 ANOS", "DESCONSIDERAR")

soma_intervalo_idade_HOMICIDIO_let$INTER_06 = ifelse((soma_intervalo_idade_HOMICIDIO_let$IDADE2 > 29),
                                                     "MAIOR QUE 29 ANOS", "DESCONSIDERAR")

soma_intervalo_idade_HOMICIDIO_let$INTER_07= ifelse((soma_intervalo_idade_HOMICIDIO_let$IDADE2 == "S/INF"),
                                                    "SEM INFORMAÇÃO", "DESCONSIDERAR")

#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.

soma_intervalo_idade_HOMICIDIO_let =

  soma_intervalo_idade_HOMICIDIO_let %>%
  pivot_longer(cols = starts_with("INTER_0"), values_to = "INTERVALO_IDADE") %>%
  #select(-name) %>%
  filter(INTERVALO_IDADE != "DESCONSIDERAR")
#########################################################################################################
#########################################################################################################
# salvando para gráfico
soma_intervalo_idade_HOMICIDIO_let_bkp = soma_intervalo_idade_HOMICIDIO_let

soma_intervalo_idade_HOMICIDIO_let_bkp =
  soma_intervalo_idade_HOMICIDIO_let_bkp %>%
  janitor::tabyl(INTERVALO_IDADE) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR

colnames(soma_intervalo_idade_HOMICIDIO_let_bkp)[1]<-'soma_intervalo_idade_HOMICIDIO_let_bkp'
colnames(soma_intervalo_idade_HOMICIDIO_let_bkp)[2]<-'QUANTIDADE'
colnames(soma_intervalo_idade_HOMICIDIO_let_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

soma_intervalo_idade_HOMICIDIO_let =
  soma_intervalo_idade_HOMICIDIO_let %>%
  janitor::tabyl(INTERVALO_IDADE) %>%
  arrange(INTERVALO_IDADE) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR

colnames(soma_intervalo_idade_HOMICIDIO_let)[1]<-'FAIXA ETÁRIA'
colnames(soma_intervalo_idade_HOMICIDIO_let)[2]<-'QUANTIDADE'
colnames(soma_intervalo_idade_HOMICIDIO_let)[3]<-'PERCENTUAL'

#############################################################################################################
#SALVANDO PARA USAR PRÓXIMO ANO
setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))
write.csv(soma_intervalo_idade_HOMICIDIO_let, file ="soma_intervalo_idade_HOMICIDIO_let_ATUAL.csv",row.names=F)
#########################################################################################################
#juntando tudo: OBSERVAR O SEPARADOR LINHA ABAIXO se sep="," ou se sep="|" para dar certo
setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))

soma_intervalo_idade_HOMICIDIO_let_ANTERIOR <- read.csv("soma_intervalo_idade_HOMICIDIO_let_ANTERIOR.csv", header=TRUE, sep=",", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

colnames(soma_intervalo_idade_HOMICIDIO_let_ANTERIOR) <- c("FAIXA_ETÁRIA", "QUANTIDADE", "PERCENTUAL")
colnames(soma_intervalo_idade_HOMICIDIO_let) <- c("FAIXA_ETÁRIA", "QUANTIDADE", "PERCENTUAL")
# Adaptando:
#SUBSTITUIR

soma_intervalo_idade_HOMICIDIO_let$FAIXA_ETÁRIA[soma_intervalo_idade_HOMICIDIO_let$FAIXA_ETÁRIA == "Total"]<- "TOTAL"

soma_intervalo_idade_HOMICIDIO_let_TOTAL = full_join(soma_intervalo_idade_HOMICIDIO_let_ANTERIOR, soma_intervalo_idade_HOMICIDIO_let ,by="FAIXA_ETÁRIA")

colnames(soma_intervalo_idade_HOMICIDIO_let_TOTAL) <- c("FAIXA ETÁRIA", "2021", "%21", "2022", "%22")
#########################################################################################################


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# soma_intervalo_idade_HOMICIDIO_let FIM
#########################################################################################################setwd(file.path("~/diretorio_r/estciabh/letalidade"))
#########################################################################################################
###soma_intervalo_idade_IGNORADA_let
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/letalidade"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

soma_intervalo_idade_IGNORADA_let =
  banco_letalidade_inicial_SEM_FILTRO_IDADE_bkp %>%
  filter (CAUSA_JURIDICA == "IGNORADA")
#########################################################################################################
#########################################################################################################
#########################################################################################################
#classificando

soma_intervalo_idade_IGNORADA_let$INTER_03 = ifelse(soma_intervalo_idade_IGNORADA_let$IDADE2 < 12,
                                                    "ATÉ 11 ANOS", "DESCONSIDERAR")

soma_intervalo_idade_IGNORADA_let$INTER_04 = ifelse(soma_intervalo_idade_IGNORADA_let$IDADE2 >= 12 & soma_intervalo_idade_IGNORADA_let$IDADE2 <= 20,
                                                    "ENTRE 12 e 20 ANOS", "DESCONSIDERAR")

soma_intervalo_idade_IGNORADA_let$INTER_05 = ifelse(soma_intervalo_idade_IGNORADA_let$IDADE2 >= 21 & soma_intervalo_idade_IGNORADA_let$IDADE2 <= 29,
                                                    "ENTRE 21 e 29 ANOS", "DESCONSIDERAR")

soma_intervalo_idade_IGNORADA_let$INTER_06 = ifelse((soma_intervalo_idade_IGNORADA_let$IDADE2 > 29),
                                                    "MAIOR QUE 29 ANOS", "DESCONSIDERAR")

soma_intervalo_idade_IGNORADA_let$INTER_07= ifelse((soma_intervalo_idade_IGNORADA_let$IDADE2 == "S/INF"),
                                                   "SEM INFORMAÇÃO", "DESCONSIDERAR")

#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.

soma_intervalo_idade_IGNORADA_let =

  soma_intervalo_idade_IGNORADA_let %>%
  pivot_longer(cols = starts_with("INTER_0"), values_to = "INTERVALO_IDADE") %>%
  #select(-name) %>%
  filter(INTERVALO_IDADE != "DESCONSIDERAR")
#########################################################################################################
#########################################################################################################
# salvando para gráfico
soma_intervalo_idade_IGNORADA_let_bkp = soma_intervalo_idade_IGNORADA_let

soma_intervalo_idade_IGNORADA_let_bkp =
  soma_intervalo_idade_IGNORADA_let_bkp %>%
  janitor::tabyl(INTERVALO_IDADE) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR

colnames(soma_intervalo_idade_IGNORADA_let_bkp)[1]<-'soma_intervalo_idade_IGNORADA_let_bkp'
colnames(soma_intervalo_idade_IGNORADA_let_bkp)[2]<-'QUANTIDADE'
colnames(soma_intervalo_idade_IGNORADA_let_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

soma_intervalo_idade_IGNORADA_let =
  soma_intervalo_idade_IGNORADA_let %>%
  janitor::tabyl(INTERVALO_IDADE) %>%
  arrange(INTERVALO_IDADE) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR

colnames(soma_intervalo_idade_IGNORADA_let)[1]<-'FAIXA ETÁRIA'
colnames(soma_intervalo_idade_IGNORADA_let)[2]<-'QUANTIDADE'
colnames(soma_intervalo_idade_IGNORADA_let)[3]<-'PERCENTUAL'

#############################################################################################################
#SALVANDO PARA USAR PRÓXIMO ANO
setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))
write.csv(soma_intervalo_idade_IGNORADA_let, file ="soma_intervalo_idade_IGNORADA_let_ATUAL.csv",row.names=F)
#########################################################################################################
#juntando tudo: OBSERVAR O SEPARADOR LINHA ABAIXO se sep="," ou se sep="|" para dar certo
setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))

soma_intervalo_idade_IGNORADA_let_ANTERIOR <- read.csv("soma_intervalo_idade_IGNORADA_let_ANTERIOR.csv", header=TRUE, sep=",", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

colnames(soma_intervalo_idade_IGNORADA_let_ANTERIOR) <- c("FAIXA_ETÁRIA", "QUANTIDADE", "PERCENTUAL")
colnames(soma_intervalo_idade_IGNORADA_let) <- c("FAIXA_ETÁRIA", "QUANTIDADE", "PERCENTUAL")
# Adaptando:
#SUBSTITUIR

soma_intervalo_idade_IGNORADA_let$FAIXA_ETÁRIA[soma_intervalo_idade_IGNORADA_let$FAIXA_ETÁRIA == "Total"]<- "TOTAL"

soma_intervalo_idade_IGNORADA_let_TOTAL = full_join(soma_intervalo_idade_IGNORADA_let_ANTERIOR, soma_intervalo_idade_IGNORADA_let ,by="FAIXA_ETÁRIA")

colnames(soma_intervalo_idade_IGNORADA_let_TOTAL) <- c("FAIXA ETÁRIA", "2021", "%21", "2022", "%22")
#########################################################################################################


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# soma_intervalo_idade_IGNORADA_let FIM
##################################################################################################################################################################################################################
# TRATAMENTO OBITOS POR HOMICIDIO FAIXA ETARIA
#########################################################################################################

banco_HOMICIDIO_geral_faixa_etaria_ATUAL =
  banco_letalidade_12_20 %>%
  filter(MUNICIPIO_OCORRENCIA2 %in% c("BELOHORIZONTE", "CONTAGEM","NOVALIMA",
                                      "RIBEIRAODASNEVES", "SABARA", "SANTALUZIA",
                                      "VESPASIANO"))


banco_HOMICIDIO_geral_faixa_etaria_ATUAL = banco_letalidade_12_20 %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20 & CAUSA_JURIDICA == "HOMICÍDIO")


#table(banco_HOMICIDIO_geral_faixa_etaria_ATUAL$IDADE2)


banco_HOMICIDIO_geral_faixa_etaria_ATUAL = data.table(table(banco_HOMICIDIO_geral_faixa_etaria_ATUAL$IDADE2))

colnames(banco_HOMICIDIO_geral_faixa_etaria_ATUAL) = c("IDADE", "QUANTIDADE")

banco_HOMICIDIO_geral_faixa_etaria_ATUAL$IDADE <- paste(banco_HOMICIDIO_geral_faixa_etaria_ATUAL$IDADE, "anos", sep=" ")

########################################################################################################
#########################################################################################################
banco_HOMICIDIO_geral_faixa_etaria_ATUAL_bkp = banco_HOMICIDIO_geral_faixa_etaria_ATUAL

#banco_HOMICIDIO_geral_faixa_etaria_ATUAL

setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))

write.csv(banco_HOMICIDIO_geral_faixa_etaria_ATUAL, file = "banco_HOMICIDIO_geral_faixa_etaria_ATUAL.csv")
########################################################################################################

#########################################################################################################
#criando dataS frames 2018 a 2020:

banco_HOMICIDIO_geral_faixa_etaria_2018 <- data.table(IDADE = c(14,15,16, 17, 18, 19, 20),
                                                      QUANTIDADE = c(2,9,23,20, 28, 41, 29))

banco_HOMICIDIO_geral_faixa_etaria_2019 <- data.table(IDADE = c(14,15,16, 17, 18, 19, 20),
                                                      QUANTIDADE = c(3, 10, 6, 18, 22, 21, 24))

banco_HOMICIDIO_geral_faixa_etaria_2020 <- data.table(IDADE = c(14,15,16, 17, 18, 19, 20),
                                                      QUANTIDADE = c(1, 3, 5, 16, 11, 22, 21))

banco_HOMICIDIO_geral_faixa_etaria_2021 <- data.table(IDADE = c(13,14,15,16, 17, 18, 19, 20),
                                                      QUANTIDADE = c(1, 3, 4, 13, 8, 12, 23, 19))

bancos_A_B_HOMICIDIO = full_join(banco_HOMICIDIO_geral_faixa_etaria_2018,
                                 banco_HOMICIDIO_geral_faixa_etaria_2019, by = "IDADE")

bancos_A_B_C_HOMICIDIO = full_join(bancos_A_B_HOMICIDIO,
                                   banco_HOMICIDIO_geral_faixa_etaria_2020, by = "IDADE")


bancos_UNIDOS_HOMICIDIO = full_join(bancos_A_B_C_HOMICIDIO, banco_HOMICIDIO_geral_faixa_etaria_2021, by = "IDADE")

colnames(bancos_UNIDOS_HOMICIDIO) = c("IDADE", "2018", "2019", "2020", "2021")
bancos_UNIDOS_HOMICIDIO$IDADE <- paste(bancos_UNIDOS_HOMICIDIO$IDADE, "anos", sep=" ")
########################################################################################################
#UNINDO COM ANO EM TRATAMENTO
#########################################################################################################
banco_ATUAL_FAIXA_ETARIA_HOMICIDIO = full_join(bancos_UNIDOS_HOMICIDIO, banco_HOMICIDIO_geral_faixa_etaria_ATUAL, by = "IDADE")
colnames(banco_ATUAL_FAIXA_ETARIA_HOMICIDIO) = c("IDADE", "2018", "2019", "2020", "2021", format(Sys.Date()-365*2, "%Y"))

#PREENCHENDO NA POR 0
banco_ATUAL_FAIXA_ETARIA_HOMICIDIO[is.na(banco_ATUAL_FAIXA_ETARIA_HOMICIDIO)] <- 0

#ORDENANDO
banco_ATUAL_FAIXA_ETARIA_HOMICIDIO <- banco_ATUAL_FAIXA_ETARIA_HOMICIDIO%>%
  arrange(IDADE)

########################################################################################################
#BANCO A SER USADO NO ANO VINDOURO:
#########################################################################################################
banco_ANO_VINDOURO_HOMICIDIO = banco_ATUAL_FAIXA_ETARIA_HOMICIDIO

setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))

write.csv(banco_ANO_VINDOURO_HOMICIDIO, file = "banco_ANO_VINDOURO_HOMICIDIO.csv")
#write_ods(banco_ANO_VINDOURO_HOMICIDIO, "banco_ANO_VINDOURO_HOMICIDIO.ods")

setwd(file.path("~/diretorio_r/estciabh/letalidade"))
# INSERIR LINHA TOTAL
#PARA O GRAFICO DE BARRAS:
tab_letalidade_geral_12_20_HOMICIDIO_bkp <- reshape(data = banco_ATUAL_FAIXA_ETARIA_HOMICIDIO, idvar = "IDADE",
                                                    varying = c('2018', '2019', '2020', '2021', '2022'),
                                                    v.name=c("QUANTIDADE"),
                                                    times = c('2018', '2019', '2020', '2021', '2022'),
                                                    direction = "long")



colnames(tab_letalidade_geral_12_20_HOMICIDIO_bkp)[2]<-'ANO'

tab_letalidade_geral_12_20_HOMICIDIO_bkp1 = tab_letalidade_geral_12_20_HOMICIDIO_bkp

tab_letalidade_geral_12_20_HOMICIDIO_bkp1$CAUSA_JURIDICA <- c("HOMICÍDIO")

#########################################################################################################
tab_letalidade_geral_12_20_HOMICIDIO <- rbind(banco_ATUAL_FAIXA_ETARIA_HOMICIDIO,
                                              data.table(IDADE = "TOTAL",
                                                         '2018' = sum(banco_ATUAL_FAIXA_ETARIA_HOMICIDIO$'2018'),
                                                         '2019' = sum(banco_ATUAL_FAIXA_ETARIA_HOMICIDIO$'2019'),
                                                         '2020' = sum(banco_ATUAL_FAIXA_ETARIA_HOMICIDIO$'2020'),
                                                         '2021' = sum(banco_ATUAL_FAIXA_ETARIA_HOMICIDIO$'2021'),
                                                         '2022' = sum(banco_ATUAL_FAIXA_ETARIA_HOMICIDIO$'2022'),
                                                         stringsAsFactors = FALSE))


########################################################################################################
#########################################################################################################


########################################################################################################
#########################################################################################################



setwd(file.path("~/diretorio_r/estciabh/letalidade"))

#########################################################################################################
#########################################################################################################
########################################################################################################
#########################################################################################################

banco_IGNORADA_geral_faixa_etaria_ATUAL =
  banco_letalidade_12_20 %>%
  filter(MUNICIPIO_OCORRENCIA2 %in% c("BELOHORIZONTE", "CONTAGEM","NOVALIMA",
                                      "RIBEIRAODASNEVES", "SABARA", "SANTALUZIA",
                                      "VESPASIANO")) %>%

  filter(IDADE2 >= 12 & IDADE2 <= 20 & CAUSA_JURIDICA == "IGNORADA")


#table(banco_IGNORADA_geral_faixa_etaria_ATUAL$IDADE2)


banco_IGNORADA_geral_faixa_etaria_ATUAL = data.table(table(banco_IGNORADA_geral_faixa_etaria_ATUAL$IDADE2))

colnames(banco_IGNORADA_geral_faixa_etaria_ATUAL) = c("IDADE", "QUANTIDADE")

banco_IGNORADA_geral_faixa_etaria_ATUAL$IDADE <- paste(banco_IGNORADA_geral_faixa_etaria_ATUAL$IDADE, "anos", sep=" ")

########################################################################################################
#########################################################################################################
banco_IGNORADA_geral_faixa_etaria_ATUAL_bkp = banco_IGNORADA_geral_faixa_etaria_ATUAL

#banco_IGNORADA_geral_faixa_etaria_ATUAL

write.csv(banco_IGNORADA_geral_faixa_etaria_ATUAL, file = "banco_IGNORADA_geral_faixa_etaria_ATUAL.csv")
########################################################################################################

#########################################################################################################
#criando dataS frames 2018 a 2020:

banco_IGNORADA_geral_faixa_etaria_2018 <- data.table(IDADE = c(12, 13, 14, 15, 16, 17, 18, 19, 20),
                                                     QUANTIDADE = c(4, 2, 0, 8, 12, 16, 27, 15, 24))

banco_IGNORADA_geral_faixa_etaria_2019 <- data.table(IDADE = c(12, 13, 14, 15, 16, 17, 18, 19, 20),
                                                     QUANTIDADE = c(1, 1, 2, 6, 15, 18, 34, 20, 35))

banco_IGNORADA_geral_faixa_etaria_2020 <- data.table(IDADE = c(12, 13, 14, 15, 16, 17, 18, 19, 20),
                                                     QUANTIDADE = c(1, 5, 1, 7, 10, 16, 29, 32, 34))

banco_IGNORADA_geral_faixa_etaria_2021 <- data.table(IDADE = c(12, 13, 14, 15, 16, 17, 18, 19, 20),
                                                     QUANTIDADE = c(0, 1, 3, 1, 5, 11, 10, 17, 21))


bancos_A_B_IGNORADA = full_join(banco_IGNORADA_geral_faixa_etaria_2018, banco_IGNORADA_geral_faixa_etaria_2019, by = "IDADE")

bancos_A_B_C_IGNORADA = full_join(bancos_A_B_IGNORADA, banco_IGNORADA_geral_faixa_etaria_2020, by = "IDADE")

bancos_UNIDOS_IGNORADA = full_join(bancos_A_B_C_IGNORADA, banco_IGNORADA_geral_faixa_etaria_2021, by = "IDADE")

colnames(bancos_UNIDOS_IGNORADA) = c("IDADE", "2018", "2019", "2020", "2021")
bancos_UNIDOS_IGNORADA$IDADE <- paste(bancos_UNIDOS_IGNORADA$IDADE, "anos", sep=" ")
########################################################################################################
#UNINDO COM ANO EM TRATAMENTO
#########################################################################################################
banco_ATUAL_FAIXA_ETARIA_IGNORADA = full_join(bancos_UNIDOS_IGNORADA, banco_IGNORADA_geral_faixa_etaria_ATUAL, by = "IDADE")
colnames(banco_ATUAL_FAIXA_ETARIA_IGNORADA) = c("IDADE", "2018", "2019", "2020", "2021", format(Sys.Date()-365*2, "%Y"))

#PREENCHENDO NA POR 0
banco_ATUAL_FAIXA_ETARIA_IGNORADA[is.na(banco_ATUAL_FAIXA_ETARIA_IGNORADA)] <- 0

#ORDENANDO
banco_ATUAL_FAIXA_ETARIA_IGNORADA <- banco_ATUAL_FAIXA_ETARIA_IGNORADA%>%
  arrange(IDADE)

########################################################################################################
#BANCO A SER USADO NO ANO VINDOURO:
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/letalidade/planilhas"))
banco_ANO_VINDOURO_IGNORADA = banco_ATUAL_FAIXA_ETARIA_IGNORADA
write.csv(banco_ANO_VINDOURO_IGNORADA, file = "banco_ANO_VINDOURO_IGNORADA.csv")
#write_ods(banco_ANO_VINDOURO_IGNORADA, "banco_ANO_VINDOURO_IGNORADA.ods")
# INSERIR LINHA TOTAL
#PARA O GRAFICO DE BARRAS:

setwd(file.path("~/diretorio_r/estciabh/letalidade"))

tab_letalidade_geral_12_20_IGNORADA_bkp <- reshape(data = banco_ATUAL_FAIXA_ETARIA_IGNORADA, idvar = "IDADE",
                                                   varying = c('2018', '2019', '2020', '2021', '2022'),
                                                   v.name=c("QUANTIDADE"),
                                                   times = c('2018', '2019', '2020', '2021', '2022'),
                                                   direction = "long")


colnames(tab_letalidade_geral_12_20_IGNORADA_bkp)[2]<-'ANO'

tab_letalidade_geral_12_20_IGNORADA_bkp1 = tab_letalidade_geral_12_20_IGNORADA_bkp

tab_letalidade_geral_12_20_IGNORADA_bkp1$CAUSA_JURIDICA <- c("IGNORADA")
#########################################################################################################
tab_letalidade_geral_12_20_IGNORADA <- rbind(banco_ATUAL_FAIXA_ETARIA_IGNORADA,
                                             data.table(IDADE = "TOTAL",
                                                        '2018' = sum(banco_ATUAL_FAIXA_ETARIA_IGNORADA$'2018'),
                                                        '2019' = sum(banco_ATUAL_FAIXA_ETARIA_IGNORADA$'2019'),
                                                        '2020' = sum(banco_ATUAL_FAIXA_ETARIA_IGNORADA$'2020'),
                                                        '2021' = sum(banco_ATUAL_FAIXA_ETARIA_IGNORADA$'2021'),
                                                        '2022' = sum(banco_ATUAL_FAIXA_ETARIA_IGNORADA$'2022'),
                                                        stringsAsFactors = FALSE))
########################################################################################################
#########################################################################################################
banco_HOM_IGN_GERAL = bind_rows(tab_letalidade_geral_12_20_HOMICIDIO_bkp1, tab_letalidade_geral_12_20_IGNORADA_bkp1)

########################################################################################################
#########################################################################################################
banco_HOM_IGN_GERAL <- ddply(banco_HOM_IGN_GERAL,
                             c("ANO", "CAUSA_JURIDICA"),
                             summarise,
                             QUANTIDADE = sum(QUANTIDADE))

setwd(file.path("~/diretorio_r/estciabh/letalidade"))

#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_SEXO_hom_ign_LETALIDADE
#########################################################################################################

banco_SEXO_hom_ign_LETALIDADE =

  banco_letalidade_12_20 %>%
  filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(SEXO, CAUSA_JURIDICA)

#########################################################################################################
banco_SEXO_hom_ign_LETALIDADE$SEXO = ajustar_nomes(banco_SEXO_hom_ign_LETALIDADE$SEXO)
#########################################################################################################
#########################################################################################################
# PARA OS GRÁFICOS DE PIZZA
banco_SEXO_HOMICIDIO_LETALIDADE_pizza = banco_SEXO_hom_ign_LETALIDADE
banco_SEXO_IGNORADA_LETALIDADE_pizza = banco_SEXO_hom_ign_LETALIDADE
#########################################################################################################
banco_SEXO_HOMICIDIO_LETALIDADE_pizza =
  banco_SEXO_HOMICIDIO_LETALIDADE_pizza |>
  filter(CAUSA_JURIDICA %in% "HOMICÍDIO") |>
  count(SEXO, CAUSA_JURIDICA, sort = TRUE)

colnames(banco_SEXO_HOMICIDIO_LETALIDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################

banco_SEXO_IGNORADA_LETALIDADE_pizza =
  banco_SEXO_IGNORADA_LETALIDADE_pizza |>
  filter(CAUSA_JURIDICA %in% "IGNORADA") |>
  count(SEXO, CAUSA_JURIDICA, sort = TRUE)

colnames(banco_SEXO_IGNORADA_LETALIDADE_pizza)[3]<-'QUANTIDADE'
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
#banco_SEXO_hom_ign_LETALIDADE$MEDIDA_PROTETIVA[banco_SEXO_hom_ign_LETALIDADE$MEDIDA_PROTETIVA == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_SEXO_hom_ign_LETALIDADE_bkp = banco_SEXO_hom_ign_LETALIDADE

banco_SEXO_hom_ign_LETALIDADE_bkp =
  banco_SEXO_hom_ign_LETALIDADE_bkp %>%
  janitor::tabyl(SEXO, CAUSA_JURIDICA) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# Adaptando para scrip grafico:

colnames(banco_SEXO_hom_ign_LETALIDADE_bkp)[1]<-'banco_SEXO_hom_ign_LETALIDADE_bkp'
colnames(banco_SEXO_hom_ign_LETALIDADE_bkp)[2]<-'QUANTIDADE'
colnames(banco_SEXO_hom_ign_LETALIDADE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_SEXO_hom_ign_LETALIDADE =
  banco_SEXO_hom_ign_LETALIDADE %>%
  janitor::tabyl(SEXO, CAUSA_JURIDICA) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


#########################################################################################################
#########################################################################################################
#SUBSTITUIR
banco_SEXO_hom_ign_LETALIDADE$SEXO[banco_SEXO_hom_ign_LETALIDADE$SEXO == "Total"]<- "TOTAL"
#########################################################################################################
#############################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_SEXO_hom_ign_LETALIDADE FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_COR_hom_ign_LETALIDADE
#########################################################################################################

banco_COR_hom_ign_LETALIDADE =

  banco_letalidade_12_20 %>%
  filter(CAUSA_JURIDICA %in% "HOMICÍDIO" | CAUSA_JURIDICA %in% "IGNORADA") |>
  select(COR, CAUSA_JURIDICA)

#########################################################################################################
banco_COR_hom_ign_LETALIDADE$COR = ajustar_nomes(banco_COR_hom_ign_LETALIDADE$COR)
#########################################################################################################
#########################################################################################################
banco_COR_hom_ign_LETALIDADE_pizza = banco_COR_hom_ign_LETALIDADE

#########################################################################################################
banco_COR_hom_ign_LETALIDADE_pizza_bkp =
  banco_COR_hom_ign_LETALIDADE_pizza |>
  count(COR, CAUSA_JURIDICA, sort = TRUE)
colnames(banco_COR_hom_ign_LETALIDADE_pizza_bkp)[3]<-'QUANTIDADE'
#########################################################################################################
#SUBSTITUIR
banco_COR_hom_ign_LETALIDADE_pizza_bkp$COR[banco_COR_hom_ign_LETALIDADE_pizza_bkp$COR == "IGNORADO\\PREJUDICADO"]<- "IGNORADO/PREJUDICADO"
########################################################################################################
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
#banco_COR_hom_ign_LETALIDADE$MEDIDA_PROTETIVA[banco_COR_hom_ign_LETALIDADE$MEDIDA_PROTETIVA == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_COR_hom_ign_LETALIDADE_bkp = banco_COR_hom_ign_LETALIDADE

banco_COR_hom_ign_LETALIDADE_bkp =
  banco_COR_hom_ign_LETALIDADE_bkp %>%
  janitor::tabyl(COR, CAUSA_JURIDICA) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# Adaptando para scrip grafico:

colnames(banco_COR_hom_ign_LETALIDADE_bkp)[1]<-'banco_COR_hom_ign_LETALIDADE_bkp'
colnames(banco_COR_hom_ign_LETALIDADE_bkp)[2]<-'QUANTIDADE'
colnames(banco_COR_hom_ign_LETALIDADE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_COR_hom_ign_LETALIDADE =
  banco_COR_hom_ign_LETALIDADE %>%
  janitor::tabyl(COR, CAUSA_JURIDICA) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


#########################################################################################################
#########################################################################################################
#SUBSTITUIR
banco_COR_hom_ign_LETALIDADE$COR[banco_COR_hom_ign_LETALIDADE$COR == "Total"]<- "TOTAL"
banco_COR_hom_ign_LETALIDADE$COR[banco_COR_hom_ign_LETALIDADE$COR == "IGNORADO\\PREJUDICADO"]<- "IGNORADO/PREJUDICADO"
#########################################################################################################
#############################################################################################################
# Adaptando para scrip grafico:

colnames(banco_COR_hom_ign_LETALIDADE)[1]<-'RAÇA/COR'
#colnames(banco_COR_hom_ign_LETALIDADE_bkp)[2]<-'QUANTIDADE'
#colnames(banco_COR_hom_ign_LETALIDADE_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_COR_hom_ign_LETALIDADE FIM
#########################################################################################################

#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#########################################################################################################
##### NOVA SECAO:

#colnames(banco_iml_GERAL)[3]<-'ENTRADA'
#colnames(banco_iml_GERAL)[8]<-'NOME'
#colnames(banco_iml_GERAL)[14]<-'NASCIMENTO'
#colnames(banco_iml_GERAL)[13]<-'IDADE'
#colnames(banco_iml_GERAL)[9]<-'PERMANECE_DESCONHECIDO'
#colnames(banco_iml_GERAL)[19]<-'MUNICIPIO_OCORRENCIA'
#colnames(banco_iml_GERAL)[28]<-'CAUSA_JURIDICA'
#colnames(banco_iml_GERAL)[36]<-'PASSAGEM_CIABH'
#colnames(banco_iml_GERAL)[14]<-'SEXO'
#colnames(banco_iml_GERAL)[15]<-'COR'

#############################################################################################################
#banco_RACA_COR_HOMICIDIO_let encaminhado
#########################################################################################################
#

banco_RACA_COR_HOMICIDIO_let =
  banco_letalidade_12_20 %>%
  filter(PASSAGEM_CIABH %in% "SIM" & CAUSA_JURIDICA %in% "HOMICÍDIO") %>%
  select(COR)

#########################################################################################################
banco_RACA_COR_HOMICIDIO_let$COR = ajustar_nomes(banco_RACA_COR_HOMICIDIO_let$COR)
#########################################################################################################
#SUBSTITUIR
#banco_RACA_COR_HOMICIDIO_let$COR[banco_RACA_COR_HOMICIDIO_let$COR == "Total"]<- "TOTAL"
banco_RACA_COR_HOMICIDIO_let$COR[banco_RACA_COR_HOMICIDIO_let$COR == "IGNORADO\\PREJUDICADO"]<- "IGNORADO/PREJUDICADO"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_RACA_COR_HOMICIDIO_let_bkp = banco_RACA_COR_HOMICIDIO_let

banco_RACA_COR_HOMICIDIO_let_bkp =
  banco_RACA_COR_HOMICIDIO_let_bkp %>%
  janitor::tabyl(COR) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(banco_RACA_COR_HOMICIDIO_let_bkp)[1]<-'banco_RACA_COR_HOMICIDIO_let_bkp'
colnames(banco_RACA_COR_HOMICIDIO_let_bkp)[2]<-'QUANTIDADE'
colnames(banco_RACA_COR_HOMICIDIO_let_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_RACA_COR_HOMICIDIO_let =
  banco_RACA_COR_HOMICIDIO_let %>%
  janitor::tabyl(COR) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:

colnames(banco_RACA_COR_HOMICIDIO_let)[1]<-'RAÇA/COR'
colnames(banco_RACA_COR_HOMICIDIO_let)[2]<-'QUANTIDADE'
colnames(banco_RACA_COR_HOMICIDIO_let)[3]<-'PERCENTUAL'

#############################################################################################################
#banco_RACA_COR_HOMICIDIO_let FIM
#########################################################################################################

banco_iml_GERAL$PASSAGEM_CIABH = ajustar_nomes(banco_iml_GERAL$PASSAGEM_CIABH)

banco_iml_GERAL$PROCEDENCIA = ajustar_nomes(banco_iml_GERAL$PROCEDENCIA)


#RENOMEAR
banco_iml_GERAL$PROCEDENCIA <- str_replace(banco_iml_GERAL$PROCEDENCIA, "HOSPITAL.*", "HOSPITAL/UNIDADE DE SAÚDE")
banco_iml_GERAL$PROCEDENCIA <- str_replace(banco_iml_GERAL$PROCEDENCIA, "RESID.*", "RESIDÊNCIA")
banco_iml_GERAL$PROCEDENCIA <- str_replace(banco_iml_GERAL$PROCEDENCIA, "VIAP.*", "VIA PÚBLICA")
banco_iml_GERAL$PROCEDENCIA <- str_replace(banco_iml_GERAL$PROCEDENCIA, "NAOINF.*", "NÃO INFORMADO")


banco_iml_GERAL$CAUSA_JURIDICA = ajustar_nomes(banco_iml_GERAL$CAUSA_JURIDICA)
#RENOMEAR
banco_iml_GERAL$CAUSA_JURIDICA[banco_iml_GERAL$CAUSA_JURIDICA == "ACIDENTE-NAOESPECIFICADO"]<- "ACIDENTE-NÃO ESPECIFICADO"
banco_iml_GERAL$CAUSA_JURIDICA[banco_iml_GERAL$CAUSA_JURIDICA == "HOMICIDIO-"]<- "HOMICÍDIO"
banco_iml_GERAL$CAUSA_JURIDICA[banco_iml_GERAL$CAUSA_JURIDICA == "HOMICIDIO"]<- "HOMICÍDIO"
banco_iml_GERAL$CAUSA_JURIDICA[banco_iml_GERAL$CAUSA_JURIDICA == "IGNORADA-"]<- "IGNORADA"
banco_iml_GERAL$CAUSA_JURIDICA[banco_iml_GERAL$CAUSA_JURIDICA == "NATURAL-"]<- "NATURAL"
banco_iml_GERAL$CAUSA_JURIDICA[banco_iml_GERAL$CAUSA_JURIDICA == "SUICIDIO-"]<- "SUICÍDIO"
banco_iml_GERAL$CAUSA_JURIDICA[banco_iml_GERAL$CAUSA_JURIDICA == "SUICIDIO"]<- "SUICÍDIO"
banco_iml_GERAL$CAUSA_JURIDICA[banco_iml_GERAL$CAUSA_JURIDICA == ""]<- "IGNORADA"
#AGREGAR ACIDENTE
banco_iml_GERAL$CAUSA_JURIDICA <- str_replace(banco_iml_GERAL$CAUSA_JURIDICA, "ACIDENTE.*", "ACIDENTE")
banco_iml_GERAL$CAUSA_JURIDICA <- str_replace(banco_iml_GERAL$CAUSA_JURIDICA, "HOMIC.*", "HOMICÍDIO")
banco_iml_GERAL$CAUSA_JURIDICA <- str_replace(banco_iml_GERAL$CAUSA_JURIDICA, "SUIC.*", "SUICÍDIO")
#bkp para filtros e utilizar depois

#########################################################################################################

#banco_juntos1 = full_join(banco_iml_inicial_bkp, banco_iml_GERAL, by= "ID_1")
#banco_juntos1 = left_join(banco_iml_inicial_bkp, banco_iml_GERAL, by= "NOME")
#banco_juntos2 = full_join(banco_iml_inicial_bkp, banco_iml_GERAL, by= names(banco_iml_inicial_bkp))

#banco_juntos1 = left_join(banco_PARA_CAI_12_20, banco_iml_GERAL, by= names(banco_PARA_CAI_12_20))
#banco_juntos1 = left_join(banco_PARA_CAI_12_20, banco_iml_GERAL, by= "ID_1")
#banco_juntos = left_join(banco_iml_inicial_bkp, banco_juntos1, by= names(banco_iml_inicial_bkp))
#banco_juntos = left_join(banco_iml_inicial_bkp, banco_juntos1, by= "ID_1")
#########################################################################################################
#########################################################################################################
#passagem_cia_GERAL_HOM_IGN_LET
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

passagem_cia_GERAL_HOM_IGN_LET = banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2<= 20) %>%
  filter (CAUSA_JURIDICA == "HOMICÍDIO" | CAUSA_JURIDICA == "IGNORADA")  %>%
  select(CAUSA_JURIDICA, PASSAGEM_CIABH)

#table(passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH)
#names(banco_letalidade_inicial_SEM_FILTRO_IDADE_bkp)
#########################################################################################################
#########################################################################################################

passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH <- as.character(passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH)

#passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH[passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH == "m"]<- "M"
#table(passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH)

passagem_cia_GERAL_HOM_IGN_LET <- table(passagem_cia_GERAL_HOM_IGN_LET$CAUSA_JURIDICA, passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH, useNA ="always")
write.csv(passagem_cia_GERAL_HOM_IGN_LET, file ="passagem_cia_GERAL_HOM_IGN_LET.csv",row.names=TRUE)
#write.csv(passagem_cia_GERAL_HOM_IGN_LET, file ="passagem_cia_GERAL_HOM_IGN_LET.csv",row.names=TRUE)
#sum(passagem_cia_GERAL_HOM_IGN_LET)

passagem_cia_GERAL_HOM_IGN_LET = data.frame(passagem_cia_GERAL_HOM_IGN_LET)
#########################################################################################################
#########################################################################################################


passagem_cia_GERAL_HOM_IGN_LET_bkp = passagem_cia_GERAL_HOM_IGN_LET


#passagem_cia_GERAL_HOM_IGN_LET

colnames(passagem_cia_GERAL_HOM_IGN_LET) <- c("CAUSA_JURIDICA", "PASSAGEM_CIABH", "QUANTIDADE")

#passagem_cia_GERAL_HOM_IGN_LET

passagem_cia_GERAL_HOM_IGN_LET$CAUSA_JURIDICA <- as.character(passagem_cia_GERAL_HOM_IGN_LET$CAUSA_JURIDICA)
passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH <- as.character(passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH)

#passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH <- as.character(passagem_cia_GERAL_HOM_IGN_LET$PASSAGEM_CIABH)
#sum(passagem_cia_GERAL_HOM_IGN_LET$QUANTIDADE)


passagem_cia_GERAL_HOM_IGN_LET[is.na(passagem_cia_GERAL_HOM_IGN_LET)] <- 0


passagem_cia_GERAL_HOM_IGN_LET = filter(passagem_cia_GERAL_HOM_IGN_LET, !QUANTIDADE == 0)
passagem_cia_GERAL_HOM_IGN_LET = filter(passagem_cia_GERAL_HOM_IGN_LET, !PASSAGEM_CIABH == 0)
#passagem_cia_GERAL_HOM_IGN_LET = filter(passagem_cia_GERAL_HOM_IGN_LET, !PASSAGEM_CIABH == NA)


#passagem_cia_GERAL_HOM_IGN_LET
#passagem_cia_GERAL_HOM_IGN_LET$CAUSA_JURIDICA <- paste(passagem_cia_GERAL_HOM_IGN_LET$CAUSA_JURIDICA, "anos", sep=" ")


########################################################################################################
#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio

########################################################################################################
# passagem_cia_GERAL_HOM_IGN_LET FIM
########################################################################################################
#########################################################################################################
#passagem_cia_HOMICIDIO
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

passagem_cia_HOMICIDIO_LET = banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter(CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(CAUSA_JURIDICA, IDADE2, PASSAGEM_CIABH)


#table(passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH)

#########################################################################################################
#########################################################################################################

passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH <- as.character(passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH)

#passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH[passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH == "m"]<- "M"
#table(passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH)

passagem_cia_HOMICIDIO_LET <- table(passagem_cia_HOMICIDIO_LET$IDADE, passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH, useNA ="always")
##write.csv(passagem_cia_HOMICIDIO_LET, file ="passagem_cia_HOMICIDIO_LET.csv",row.names=TRUE)
##write.csv(passagem_cia_HOMICIDIO_LET, file ="passagem_cia_HOMICIDIO_LET.csv",row.names=TRUE)
#sum(passagem_cia_HOMICIDIO_LET)

passagem_cia_HOMICIDIO_LET = data.frame(passagem_cia_HOMICIDIO_LET)
#########################################################################################################
#########################################################################################################


passagem_cia_HOMICIDIO_LET_bkp = passagem_cia_HOMICIDIO_LET


#passagem_cia_HOMICIDIO_LET

colnames(passagem_cia_HOMICIDIO_LET) <- c("IDADE", "PASSAGEM_CIABH", "QUANTIDADE")

#passagem_cia_HOMICIDIO_LET

passagem_cia_HOMICIDIO_LET$IDADE <- as.character(passagem_cia_HOMICIDIO_LET$IDADE)
passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH <- as.character(passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH)

#passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH <- as.character(passagem_cia_HOMICIDIO_LET$PASSAGEM_CIABH)
#sum(passagem_cia_HOMICIDIO_LET$QUANTIDADE)


passagem_cia_HOMICIDIO_LET[is.na(passagem_cia_HOMICIDIO_LET)] <- 0


passagem_cia_HOMICIDIO_LET = filter(passagem_cia_HOMICIDIO_LET, !QUANTIDADE == 0)
passagem_cia_HOMICIDIO_LET = filter(passagem_cia_HOMICIDIO_LET, !PASSAGEM_CIABH == 0)
#passagem_cia_HOMICIDIO_LET = filter(passagem_cia_HOMICIDIO_LET, !PASSAGEM_CIABH == NA)


#passagem_cia_HOMICIDIO_LET
passagem_cia_HOMICIDIO_LET$IDADE <- paste(passagem_cia_HOMICIDIO_LET$IDADE, "anos", sep=" ")


########################################################################################################
#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio

########################################################################################################
# passagem_cia_HOMICIDIO FIM
########################################################################################################

#########################################################################################################
#passagem_cia_IGNORADA
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

passagem_cia_IGNORADA_LET = banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter(CAUSA_JURIDICA == "IGNORADA") %>%
  select(CAUSA_JURIDICA, IDADE2, PASSAGEM_CIABH)

#table(passagem_cia_IGNORADA_LET$PASSAGEM_CIABH)

#########################################################################################################
#########################################################################################################

passagem_cia_IGNORADA_LET$PASSAGEM_CIABH <- as.character(passagem_cia_IGNORADA_LET$PASSAGEM_CIABH)

#passagem_cia_IGNORADA_LET$PASSAGEM_CIABH[passagem_cia_IGNORADA_LET$PASSAGEM_CIABH == "m"]<- "M"
#table(passagem_cia_IGNORADA_LET$PASSAGEM_CIABH)

passagem_cia_IGNORADA_LET <- table(passagem_cia_IGNORADA_LET$IDADE, passagem_cia_IGNORADA_LET$PASSAGEM_CIABH, useNA ="always")
##write.csv(passagem_cia_IGNORADA_LET, file ="passagem_cia_IGNORADA_LET.csv",row.names=TRUE)
##write.csv(passagem_cia_IGNORADA_LET, file ="passagem_cia_IGNORADA_LET.csv",row.names=TRUE)
#sum(passagem_cia_IGNORADA_LET)

passagem_cia_IGNORADA_LET = data.frame(passagem_cia_IGNORADA_LET)
#########################################################################################################
#########################################################################################################


passagem_cia_IGNORADA_LET_bkp = passagem_cia_IGNORADA_LET


#passagem_cia_IGNORADA_LET

colnames(passagem_cia_IGNORADA_LET) <- c("IDADE", "PASSAGEM_CIABH", "QUANTIDADE")

#passagem_cia_IGNORADA_LET

passagem_cia_IGNORADA_LET$IDADE <- as.character(passagem_cia_IGNORADA_LET$IDADE)
passagem_cia_IGNORADA_LET$PASSAGEM_CIABH <- as.character(passagem_cia_IGNORADA_LET$PASSAGEM_CIABH)

#passagem_cia_IGNORADA_LET$PASSAGEM_CIABH <- as.character(passagem_cia_IGNORADA_LET$PASSAGEM_CIABH)
#sum(passagem_cia_IGNORADA_LET$QUANTIDADE)


passagem_cia_IGNORADA_LET[is.na(passagem_cia_IGNORADA_LET)] <- 0


passagem_cia_IGNORADA_LET = filter(passagem_cia_IGNORADA_LET, !QUANTIDADE == 0)
passagem_cia_IGNORADA_LET = filter(passagem_cia_IGNORADA_LET, !PASSAGEM_CIABH == 0)
#passagem_cia_IGNORADA_LET = filter(passagem_cia_IGNORADA_LET, !PASSAGEM_CIABH == NA)


#passagem_cia_IGNORADA_LET
passagem_cia_IGNORADA_LET$IDADE <- paste(passagem_cia_IGNORADA_LET$IDADE, "anos", sep=" ")
########################################################################################################
#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio

########################################################################################################
# passagem_cia_IGNORADA FIM
########################################################################################################

#########################################################################################################
#SEXO IDADE
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

sexo_idade_LET_bkp = banco_letalidade_inicial_SEM_FILTRO_IDADE_bkp %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter (CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(SEXO, IDADE2)

#########################################################################################################
#########################################################################################################

sexo_idade_LET_bkp$SEXO <- as.character(sexo_idade_LET_bkp$SEXO)

sexo_idade_LET_bkp$SEXO[sexo_idade_LET_bkp$SEXO == ""]<- "M"
#table(sexo_idade_LET_bkp$SEXO)

sexo_idade_LET_bkp <- table(sexo_idade_LET_bkp$IDADE, sexo_idade_LET_bkp$SEXO, useNA ="always")
##write.csv(sexo_idade_LET_bkp, file ="sexo_idade_LET_bkp.csv",row.names=FALSE)
##write.csv(sexo_idade_LET_bkp, file ="sexo_idade_LET_bkp.csv",row.names=FALSE)
#sum(sexo_idade_LET_bkp)

sexo_idade_LET_bkp = data.frame(sexo_idade_LET_bkp)
#########################################################################################################
#########################################################################################################


sexo_idade_LET_bkp1 = sexo_idade_LET_bkp


#sexo_idade_LET_bkp

colnames(sexo_idade_LET_bkp) <- c("IDADE", "SEXO", "QUANTIDADE")

#sexo_idade_LET_bkp

sexo_idade_LET_bkp$IDADE <- as.character(sexo_idade_LET_bkp$IDADE)
sexo_idade_LET_bkp$SEXO <- as.character(sexo_idade_LET_bkp$SEXO)
#sum(sexo_idade_LET_bkp$QUANTIDADE)


sexo_idade_LET_bkp = filter(sexo_idade_LET_bkp, !QUANTIDADE == 0)
#sexo_idade_LET_bkp

#PREENCHER COM NA'S CELULAS VAZIAS
#sexo_idade_LET_bkp$IDADE[sexo_idade_LET_bkp$IDADE == "SEM INFORMACAO"]<- "<NA>"
sexo_idade_LET_bkp$IDADE[which(is.na(sexo_idade_LET_bkp$IDADE))] <- "s/inf"
#sexo_idade_LET_bkp




#sexo_idade_LET_bkp$IDADE <- paste(sexo_idade_LET_bkp$IDADE, "anos", sep=" ")
sexo_idade_LET_bkp$IDADE[sexo_idade_LET_bkp$IDADE == "s/inf anos"]<- "s/inf"

sexo_idade_LET_bkp <- reshape(data = sexo_idade_LET_bkp, idvar = "IDADE", timevar = "SEXO", direction = "wide")
#sexo_idade_LET_bkp

#colnames(sexo_idade_LET_bkp) <- c("IDADE", "FEMININO", "IGNORADOPREJUDICADO", "MASCULINO")
colnames(sexo_idade_LET_bkp) <- c("IDADE", "FEMININO", "MASCULINO")
#sexo_idade_LET_bkp

sexo_idade_LET_bkp$FEMININO[which(is.na(sexo_idade_LET_bkp$FEMININO))] <- 0
#sexo_idade_LET_bkp$IGNORADOPREJUDICADO[which(is.na(sexo_idade_LET_bkp$IGNORADOPREJUDICADO))] <- 0
sexo_idade_LET_bkp$MASCULINO[which(is.na(sexo_idade_LET_bkp$MASCULINO))] <- 0

#sexo_idade_LET_bkp
#ordenar idade
sexo_idade_LET_bkp = sexo_idade_LET_bkp %>% arrange(IDADE)

sexo_idade_LET_bkp$FEMININO <- as.numeric(sexo_idade_LET_bkp$FEMININO)
#sexo_idade_LET_bkp$IGNORADOPREJUDICADO <- as.numeric(sexo_idade_LET_bkp$IGNORADOPREJUDICADO)
sexo_idade_LET_bkp$MASCULINO <- as.numeric(sexo_idade_LET_bkp$MASCULINO)

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
#usando a funcao criada:
sexo_idade_LET_bkp$F <- round_preserve_sum(prop.table(sexo_idade_LET_bkp$FEMININO)*100, 2)
#sexo_idade_LET_bkp$IGN <- round_preserve_sum(prop.table(sexo_idade_LET_bkp$IGNORADOPREJUDICADO)*100, 2)
sexo_idade_LET_bkp$M <- round_preserve_sum(prop.table(sexo_idade_LET_bkp$MASCULINO)*100, 2)
#sexo_idade_LET_bkp
#########################################################################################################
#colnames(sexo_idade_LET_bkp) <- c("IDADE", "FEM", "IGN","MAS", "F", "IG", "M")
colnames(sexo_idade_LET_bkp) <- c("IDADE", "FEM", "MAS", "F", "M")
#sexo_idade_LET_bkp

#########################################################################################################
#########################################################################################################

#script para o bookdown

sexo_idade_LET_bkp_rmark = sexo_idade_LET_bkp

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)
sexo_idade_LET_bkp_rmark = filter(sexo_idade_LET_bkp_rmark, !IDADE == "s/inf")
#selecionando os 3 principais e ordenando descrescente por quantidade
sexo_idade_LET_bkp_rmark = sexo_idade_LET_bkp_rmark %>%
  top_n(3, MAS) %>% arrange(desc(MAS))

#somando
#sum(sexo_idade_LET_bkp_rmark$M)

#para escolher linhas e posicoes
#sexo_idade_LET_bkp_rmark[2,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

#########################################################################################################

sexo_idade_LET_bkp<- rbind(sexo_idade_LET_bkp,
                           data.frame(IDADE = "TOTAL",
                                      FEM = sum(sexo_idade_LET_bkp$FEMININO),
                                      F = sum(sexo_idade_LET_bkp$F),
                                      #IGN = sum(sexo_idade_LET_bkp$IGN),
                                      #IG = sum(sexo_idade_LET_bkp$IG),
                                      MAS = sum(sexo_idade_LET_bkp$MASCULINO),
                                      M = sum(sexo_idade_LET_bkp$M),
                                      stringsAsFactors = FALSE))

#sexo_idade_LET_bkp

sexo_idade_LET_bkp =
  sexo_idade_LET_bkp %>%
  #select(IDADE, FEM, F, IGN, IG, MAS, M)
  select(IDADE, FEM, F, MAS, M)


#colnames(sexo_idade_LET_bkp) <- c("IDADE","FEM", "%", "IGN", "%", "MAS", "%")
colnames(sexo_idade_LET_bkp) <- c("IDADE","FEM", "%", "MAS", "%")
#sexo_idade_LET_bkp
#########################################################################################################
#########################################################################################################
#salvando tabela
#pdf(file="tabela_sexo_idade_LET_bkp_alternativa2.pdf", width = 5, height = 3.8, title = "tabela_sexo_idade_LET_bkp_alternativa")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#GRAFICO SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

sexo_LET_HOMICIDIO =

  banco_letalidade_12_20 %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  #filter (PASSAGEM_CIABH == "SIM") %>%
  filter(CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(SEXO)

sexo_LET_IGNORADA =

  banco_letalidade_12_20 %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  #filter (PASSAGEM_CIABH == "SIM") %>%
  filter(CAUSA_JURIDICA == "IGNORADA") %>%
  select(SEXO)
#table(sexo_LET$SEXO)

#########################################################################################################
#########################################################################################################

sexo_LET_HOMICIDIO$SEXO <- as.character(sexo_LET_HOMICIDIO$SEXO)

sexo_LET_HOMICIDIO$SEXO[sexo_LET_HOMICIDIO$SEXO == ""]<- "M"
#table(sexo_LET_HOMICIDIO$SEXO)

sexo_LET_HOMICIDIO = data.table(table(sexo_LET_HOMICIDIO$SEXO))

colnames(sexo_LET_HOMICIDIO) <- c("SEXO", "QUANTIDADE")

#sum(sexo_LET_HOMICIDIO$QUANTIDADE)

#########################################################################################################
#########################################################################################################


sexo_LET_HOMICIDIO$SEXO <- as.character(sexo_LET_HOMICIDIO$SEXO)

sexo_LET_HOMICIDIO$SEXO[sexo_LET_HOMICIDIO$SEXO == "F"]<- "FEMININO"
sexo_LET_HOMICIDIO$SEXO[sexo_LET_HOMICIDIO$SEXO == "M"]<- "MASCULINO"

sexo_LET_HOMICIDIO_original=sexo_LET_HOMICIDIO

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
#usando a funcao criada:
#sexo_LET_HOMICIDIO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

sexo_LET_HOMICIDIO <- sexo_LET_HOMICIDIO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

sexo_LET_HOMICIDIO$PERCENTUAL <- paste(sexo_LET_HOMICIDIO$PERCENTUAL, "%", sep=" ")

#sexo_LET_HOMICIDIO

#setwd(file.path("~/diretorio_r/estciabh/imagens"))


#salvar png
p1 =
  ggpie(sexo_LET_HOMICIDIO,
        x= "QUANTIDADE", label = "QUANTIDADE",
        lab.pos = "in", lab.font = list(color = "white", face = "bold"),
        lab.adjust = 0,
        fill = "SEXO", color = "white", face="bold",
        palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  # labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", subtitle = "Letalidade") +
  ggtitle("HOMICÍDIO")
#ggsave("GRAFICO_068_sexo_idade_LET_pizza.png", width=9, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
###ignorada
#########################################################################################################
sexo_LET_IGNORADA$SEXO <- as.character(sexo_LET_IGNORADA$SEXO)

sexo_LET_IGNORADA$SEXO[sexo_LET_IGNORADA$SEXO == ""]<- "M"
#table(sexo_LET_IGNORADA$SEXO)

sexo_LET_IGNORADA = data.table(table(sexo_LET_IGNORADA$SEXO))

colnames(sexo_LET_IGNORADA) <- c("SEXO", "QUANTIDADE")

#sum(sexo_LET_IGNORADA$QUANTIDADE)

#########################################################################################################
#########################################################################################################


sexo_LET_IGNORADA$SEXO <- as.character(sexo_LET_IGNORADA$SEXO)

sexo_LET_IGNORADA$SEXO[sexo_LET_IGNORADA$SEXO == "F"]<- "FEMININO"
sexo_LET_IGNORADA$SEXO[sexo_LET_IGNORADA$SEXO == "M"]<- "MASCULINO"

sexo_LET_IGNORADA_original=sexo_LET_IGNORADA

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
#usando a funcao criada:
#sexo_LET_IGNORADA

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

sexo_LET_IGNORADA <- sexo_LET_IGNORADA %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

sexo_LET_IGNORADA$PERCENTUAL <- paste(sexo_LET_IGNORADA$PERCENTUAL, "%", sep=" ")

#sexo_LET_IGNORADA

#setwd(file.path("~/diretorio_r/estciabh/imagens"))


#salvar png
p2 =
  ggpie(sexo_LET_IGNORADA,
        x= "QUANTIDADE", label = "QUANTIDADE",
        lab.pos = "in", lab.font = list(color = "white", face = "bold"),
        lab.adjust = 0,
        fill = "SEXO", color = "white", face="bold",
        palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") +
  ggtitle("CAUSA IGNORADA")



#########################################################################################################
# GRAFICO SEXO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

sexo_LET_HOMICIDIO_PASSAGEM_CIA =

  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter (PASSAGEM_CIABH == "SIM") %>%
  filter(CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(SEXO)

sexo_LET_IGNORADA_PASSAGEM_CIA =

  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter (PASSAGEM_CIABH == "SIM") %>%
  filter(CAUSA_JURIDICA == "IGNORADA") %>%
  select(SEXO)
#table(sexo_LET$SEXO)

#########################################################################################################
#########################################################################################################

sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO <- as.character(sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO)

sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO[sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO == ""]<- "M"
#table(sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO)

sexo_LET_HOMICIDIO_PASSAGEM_CIA = data.table(table(sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO))

colnames(sexo_LET_HOMICIDIO_PASSAGEM_CIA) <- c("SEXO", "QUANTIDADE")

#sum(sexo_LET_HOMICIDIO_PASSAGEM_CIA$QUANTIDADE)

#########################################################################################################
#########################################################################################################


sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO <- as.character(sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO)

sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO[sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO == "F"]<- "FEMININO"
sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO[sexo_LET_HOMICIDIO_PASSAGEM_CIA$SEXO == "M"]<- "MASCULINO"

sexo_LET_HOMICIDIO_PASSAGEM_CIA_original=sexo_LET_HOMICIDIO_PASSAGEM_CIA

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
#usando a funcao criada:
#sexo_LET_HOMICIDIO_PASSAGEM_CIA

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

sexo_LET_HOMICIDIO_PASSAGEM_CIA <- sexo_LET_HOMICIDIO_PASSAGEM_CIA %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

sexo_LET_HOMICIDIO_PASSAGEM_CIA$PERCENTUAL <- paste(sexo_LET_HOMICIDIO_PASSAGEM_CIA$PERCENTUAL, "%", sep=" ")

#sexo_LET_HOMICIDIO_PASSAGEM_CIA

#setwd(file.path("~/diretorio_r/estciabh/imagens"))


#salvar png
p1_PASSAGEM_CIA =
  ggpie(sexo_LET_HOMICIDIO_PASSAGEM_CIA,
        x= "QUANTIDADE", label = "QUANTIDADE",
        lab.pos = "in", lab.font = list(color = "white", face = "bold"),
        lab.adjust = 0,
        fill = "SEXO", color = "white", face="bold",
        palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  # labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", subtitle = "Letalidade") +
  ggtitle("HOMICÍDIO")
#ggsave("GRAFICO_068_sexo_idade_LET_pizza.png", width=9, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
###ignorada
#########################################################################################################
sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO <- as.character(sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO)

sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO[sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO == ""]<- "M"
#table(sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO)

sexo_LET_IGNORADA_PASSAGEM_CIA = data.table(table(sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO))

colnames(sexo_LET_IGNORADA_PASSAGEM_CIA) <- c("SEXO", "QUANTIDADE")

#sum(sexo_LET_IGNORADA_PASSAGEM_CIA$QUANTIDADE)

#########################################################################################################
#########################################################################################################


sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO <- as.character(sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO)

sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO[sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO == "F"]<- "FEMININO"
sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO[sexo_LET_IGNORADA_PASSAGEM_CIA$SEXO == "M"]<- "MASCULINO"

sexo_LET_IGNORADA_PASSAGEM_CIA_original=sexo_LET_IGNORADA_PASSAGEM_CIA

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
#usando a funcao criada:
#sexo_LET_IGNORADA_PASSAGEM_CIA

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

sexo_LET_IGNORADA_PASSAGEM_CIA <- sexo_LET_IGNORADA_PASSAGEM_CIA %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

sexo_LET_IGNORADA_PASSAGEM_CIA$PERCENTUAL <- paste(sexo_LET_IGNORADA_PASSAGEM_CIA$PERCENTUAL, "%", sep=" ")

#sexo_LET_IGNORADA_PASSAGEM_CIA

#setwd(file.path("~/diretorio_r/estciabh/imagens"))


#salvar png
p2_PASSAGEM_CIA =
  ggpie(sexo_LET_IGNORADA_PASSAGEM_CIA,
        x= "QUANTIDADE", label = "QUANTIDADE",
        lab.pos = "in", lab.font = list(color = "white", face = "bold"),
        lab.adjust = 0,
        fill = "SEXO", color = "white", face="bold",
        palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") +
  ggtitle("CAUSA IGNORADA")




#########################################################################################################
#########################################################################################################
#############################################################################################################
#banco_PROCEDENCIA_LET
#########################################################################################################

banco_PROCEDENCIA_LET =
  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter(PASSAGEM_CIABH == "SIM"& CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(PROCEDENCIA)

colnames(banco_PROCEDENCIA_LET)[1]<-'banco_PROCEDENCIA_LET'
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_PROCEDENCIA_LET_bkp = banco_PROCEDENCIA_LET

banco_PROCEDENCIA_LET_bkp =
  banco_PROCEDENCIA_LET_bkp %>%
  janitor::tabyl(banco_PROCEDENCIA_LET) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
#########################################################################################################
#replace "%" with "" in the percentual column
banco_PROCEDENCIA_LET_bkp$PERCENTUAL2 <- str_replace (banco_PROCEDENCIA_LET_bkp$percent, "%", "")
banco_PROCEDENCIA_LET_bkp$PERCENTUAL2 = as.numeric(banco_PROCEDENCIA_LET_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_PROCEDENCIA_LET_bkp)[1]<-'banco_PROCEDENCIA_LET_bkp'
colnames(banco_PROCEDENCIA_LET_bkp)[2]<-'QUANTIDADE'
colnames(banco_PROCEDENCIA_LET_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

banco_PROCEDENCIA_LET_bkp_rmark = banco_PROCEDENCIA_LET_bkp

banco_PROCEDENCIA_LET_bkp_rmark = banco_PROCEDENCIA_LET_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
banco_PROCEDENCIA_LET_bkp_rmark =
  banco_PROCEDENCIA_LET_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
#banco_PROCEDENCIA_LET$banco_PROCEDENCIA_LET[banco_PROCEDENCIA_LET$banco_PROCEDENCIA_LET == "VNÃO SABE"]<- "UNÃO SABE"
#banco_PROCEDENCIA_LET$banco_PROCEDENCIA_LET[banco_PROCEDENCIA_LET$banco_PROCEDENCIA_LET == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_PROCEDENCIA_LET_TABELA =
  banco_PROCEDENCIA_LET %>%
  janitor::tabyl(banco_PROCEDENCIA_LET) %>%
  arrange(banco_PROCEDENCIA_LET) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_PROCEDENCIA_LET_TABELA)[1]<-'LOCALIDADE'
colnames(banco_PROCEDENCIA_LET_TABELA)[2]<-'QUANTIDADE'
colnames(banco_PROCEDENCIA_LET_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_PROCEDENCIA_LET FIM
#########################################################################################################
#########################################################################################################
###RAÇA/COR
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################

#salvar pdf
################################################################################################################################################################################

#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)
require(lubridate)

#banco_letalidade_12_20$COR <- gsub(" ","", banco_letalidade_12_20$COR)

#banco_COR_LET =

# banco_letalidade_12_20 %>%
# filter(PASSAGEM_CIABH == "SIM" & CAUSA_JURIDICA == "HOMICÍDIO") %>%
#select(COR)


################################################################################################################################################################################

#########################################################################################################
#########################################################################################################

banco_COR_LET = banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter(PASSAGEM_CIABH == "SIM" & CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(COR)

#########################################################################################################
# substituindo

banco_COR_LET = data.table (table(banco_COR_LET$COR))

#########################################################################################################
#########################################################################################################
#renomeando variavel :

colnames(banco_COR_LET) = c("RACA_COR", "QUANTIDADE")
#########################################################################################################
#sum(table(banco_COR_LET))
banco_COR_LET$RACA_COR = ajustar_nomes(banco_COR_LET$RACA_COR)

table(banco_COR_LET$RACA_COR)
banco_COR_LET$RACA_COR <- as.character(banco_COR_LET$RACA_COR)
banco_COR_LET = filter(banco_COR_LET, !RACA_COR == "DESCONSIDERARAOSOMAR")
#banco_COR_LET
#########################################################################################################
#########################################################################################################
#table(banco_COR_LET$RACA_COR)
#banco_COR_LET$RACA_COR = ifelse(banco_COR_LET$RACA_COR == "1",
#                            "ART. 101, I", banco_COR_LET$RACA_COR)

#table(banco_COR_LET$RACA_COR)
banco_COR_LET$RACA_COR = ifelse(banco_COR_LET$RACA_COR == "IGNORADO\\PREJUDICADO",
                                "IGNORADO/PREJUDICADO", banco_COR_LET$RACA_COR)

table(banco_COR_LET$RACA_COR)
banco_COR_LET$RACA_COR = ifelse(banco_COR_LET$RACA_COR == "Parda",
                                "PARDA", banco_COR_LET$RACA_COR)

table(banco_COR_LET$RACA_COR)
banco_COR_LET$RACA_COR = ifelse(banco_COR_LET$RACA_COR == "Preta",
                                "PRETA", banco_COR_LET$RACA_COR)


banco_COR_LET <- ddply(banco_COR_LET,
                       c("RACA_COR"),
                       summarise,
                       QUANTIDADE = sum(QUANTIDADE))

#########################################################################################################
#########################################################################################################
#banco_COR_LET = data.frame(table(banco_COR_LET))

#colnames(banco_COR_LET) <- c("RACA_COR", "QUANTIDADE")

banco_COR_LET  <- banco_COR_LET[order(banco_COR_LET[,1],decreasing=FALSE),]

banco_COR_LET_bkp = banco_COR_LET #salvando atos atendimento original

library(grid)
library(gridExtra)

#acrescentando coluna com percentual
banco_COR_LET$QUANTIDADE <- as.numeric(banco_COR_LET$QUANTIDADE)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}

#usando a funcao criada:
#banco_COR_LET$PERCENTUAL <- round(prop.table(banco_COR_LET$QUANTIDADE)*100, 2)
banco_COR_LET$PERCENTUAL <- round_preserve_sum(prop.table(banco_COR_LET$QUANTIDADE)*100, 2)

#outra forma de calcular percentual
#banco_COR_LET = mutate(banco_COR_LET,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)


banco_COR_LET_bkp=banco_COR_LET


#########################################################################################################
#########################################################################################################

#script para o bookdown

banco_COR_LET_rmark = banco_COR_LET

#SEPARAR CASOS DE ARQUIVAMENTO E REMISSÕES PARA SCRIPTS EM 014_DECISOES_Rmd

#filter(banco_COR_LET_rmark, !grepl("REMISSAO", DECISAO))
#banco_COR_LET_rmark = filter(banco_COR_LET_rmark, grepl("ARQUIVAMENTO|REMISSAO", DECISAO))


banco_COR_LET_rmark <- banco_COR_LET_rmark %>%
  arrange(desc(PERCENTUAL))

#banco_COR_LET_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
banco_COR_LET_rmark = banco_COR_LET_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
#sum(banco_COR_LET_rmark$QUANTIDADE)

#para escolher linhas e posicoes
#banco_COR_LET_rmark[1,2]
#outra forma de calcular percentual
#banco_COR_LET = mutate(banco_COR_LET,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

#acrescentando linha com total
banco_COR_LET <- rbind(banco_COR_LET,
                       data.frame(RACA_COR = "TOTAL", QUANTIDADE = sum(banco_COR_LET$QUANTIDADE), PERCENTUAL = sum(banco_COR_LET$PERCENTUAL),
                                  stringsAsFactors = FALSE))

colnames(banco_COR_LET) <- c("RAÇA/COR", "QUANTIDADE", "%")

#para tabela gt abaixo:
banco_COR_LET_gt = banco_COR_LET


###write.xlsx(banco_COR_LET, file = "banco_COR_LET_total.xlsx") #salvando para usar na comparada
##write.csv(banco_COR_LET, file = "banco_COR_LET_total.csv", row.names=FALSE) #salvando com modificações anteriores


#########################################################################################################
#########################################################################################################
#tabela alternativa
require(ggpubr)
#########################################################################################################
#salvando tabela
#pdf(file="tabela_banco_COR_LET_geral_alternativa.pdf",  width = 6, height = 4.8, title = "banco_COR_LET")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################

#########################################################################################################

#########################################################################################################
#########################################################################################################


setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_COR_LET FIM
#########################################################################################################
#salvar pdf
########################################################################################################
####
#########################################################################################################
########################################################################################################
#SEXO IDADE FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
###tempo_medio_geral_OBITO_TAB_01
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)
require(lubridate)


tempo_medio_geral_OBITO_TAB_01 =

  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter(PASSAGEM_CIABH == "SIM") %>%
  filter (CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(DATA_ULTIMA_ENTRADA2, DATA_PRIMEIRA_ENTRADA2, ENTRADA2)


tempo_medio_geral_OBITO_TAB_01 = tempo_medio_geral_OBITO_TAB_01 %>%
  na.omit() %>%
  group_by(DATA_PRIMEIRA_ENTRADA2)



#########################################################################################################
#CONVERTE PARA DATE (DATA_ULTIMA_ENTRADA2 JÁ CONVERTIDO LINHAS ACIMA)
#tempo_medio_geral_OBITO_TAB_01$INTER_TEMPO_MEDIO_medio_geral <- as.character(tempo_medio_geral_OBITO_TAB_01$INTER_TEMPO_MEDIO_medio_geral)
## calcula o intervalo em anos
tempo_medio_geral_OBITO_TAB_01$INTER_01 = as.period(interval(tempo_medio_geral_OBITO_TAB_01$DATA_PRIMEIRA_ENTRADA2, tempo_medio_geral_OBITO_TAB_01$ENTRADA2))



#tempo_medio_geral_OBITO_TAB_01$INTER_02 <- (ymd(tempo_medio_geral_OBITO_TAB_01$DATA_ULTIMA_ENTRADA2)  %--%
#   ymd(tempo_medio_geral_OBITO_TAB_01$DATA_PRIMEIRA_ENTRADA2) / ddays(1))

#tempo_medio_geral_OBITO_TAB_01$INTER_02 = (tempo_medio_geral_OBITO_TAB_01$DATA_ULTIMA_ENTRADA2) %--% (tempo_medio_geral_OBITO_TAB_01$DATA_PRIMEIRA_ENTRADA2) / ddays(1)
tempo_medio_geral_OBITO_TAB_01$INTER_02 = (tempo_medio_geral_OBITO_TAB_01$DATA_PRIMEIRA_ENTRADA2) %--% (tempo_medio_geral_OBITO_TAB_01$ENTRADA2) / ddays(1)

#head(tempo_medio_geral_OBITO_TAB_01, 10)

#########################################################################################################
tempo_medio_geral_OBITO_TAB_02 = tempo_medio_geral_OBITO_TAB_01
#########################################################################################################
#tabela media tempo decisao
tempo_medio_geral_OBITO_TAB_01 = data.frame(round(mean(tempo_medio_geral_OBITO_TAB_01$INTER_02), 1))

colnames(tempo_medio_geral_OBITO_TAB_01) <- c("TEMPO")

#tempo_medio_geral_OBITO_TAB_01

tempo_medio_geral_OBITO_TAB_01$TEMPO = paste(tempo_medio_geral_OBITO_TAB_01$TEMPO, "DIAS", sep=" ")#para plotar o sinal de porcentagem

colnames(tempo_medio_geral_OBITO_TAB_01) <- c("TEMPO MÉDIO")

#tempo_medio_geral_OBITO_TAB_01

#para tabela gt abaixo:
#tempo_medio_geral_OBITO_TAB_01_gt = tempo_medio_geral_OBITO_TAB_01


#########################################################################################################
###tempo_medio_geral_OBITO_TAB_01 FIM
#########################################################################################################
#########################################################################################################
#tempo_medio_geral_OBITO_TAB_02
#########################################################################################################
#classificando

tempo_medio_geral_OBITO_TAB_02$INTER_03 = ifelse(tempo_medio_geral_OBITO_TAB_02$INTER_02 < 32,
                                                 "ATÉ 01 MÊS", "DESCONSIDERAR")


tempo_medio_geral_OBITO_TAB_02$INTER_04 = ifelse((tempo_medio_geral_OBITO_TAB_02$INTER_02 > 31 & tempo_medio_geral_OBITO_TAB_02$INTER_02 <= 180),
                                                 "ENTRE 01 E 06 MESES", "DESCONSIDERAR")

tempo_medio_geral_OBITO_TAB_02$INTER_05 = ifelse((tempo_medio_geral_OBITO_TAB_02$INTER_02 > 180 & tempo_medio_geral_OBITO_TAB_02$INTER_02 <= 365),
                                                 "ENTRE 06 MESES E 01 ANO", "DESCONSIDERAR")


tempo_medio_geral_OBITO_TAB_02$INTER_06 = ifelse((tempo_medio_geral_OBITO_TAB_02$INTER_02 > 365 & tempo_medio_geral_OBITO_TAB_02$INTER_02 <= 730),
                                                 "ENTRE 01 E 02 ANOS", "DESCONSIDERAR")


tempo_medio_geral_OBITO_TAB_02$INTER_07 = ifelse((tempo_medio_geral_OBITO_TAB_02$INTER_02 > 730 & tempo_medio_geral_OBITO_TAB_02$INTER_02 <= 1095),
                                                 "ENTRE 02 E 03 ANOS", "DESCONSIDERAR")


tempo_medio_geral_OBITO_TAB_02$INTER_08 = ifelse((tempo_medio_geral_OBITO_TAB_02$INTER_02 > 1095 & tempo_medio_geral_OBITO_TAB_02$INTER_02 <= 1460),
                                                 "ENTRE 03 E 04 ANOS", "DESCONSIDERAR")


tempo_medio_geral_OBITO_TAB_02$INTER_09 = ifelse(tempo_medio_geral_OBITO_TAB_02$INTER_02 > 1460,
                                                 "MAIS DE 04 ANOS", "DESCONSIDERAR")

#########################################################################################################

#########################################################################################################

tempo_medio_geral_OBITO_TAB_02 = tempo_medio_geral_OBITO_TAB_02 %>%
  select(6:12)




tempo_medio_geral_OBITO_TAB_02 =

  tempo_medio_geral_OBITO_TAB_02 %>%
  pivot_longer(cols = starts_with("INTER_0"), values_to = "INTER_TEMPO_MEDIO") %>%
  #select(-name) %>%
  filter(INTER_TEMPO_MEDIO != "DESCONSIDERAR")

#########################################################################################################


#tempo_medio_geral_OBITO_TAB_02

tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "ATÉ 01 MÊS"]<- "AATÉ 01 MÊS"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 01 E 06 MESES"]<- "BENTRE 01 E 06 MESES"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 06 MESES E 01 ANO"]<- "CENTRE 06 MESES E 01 ANO"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 01 E 02 ANOS"]<- "DENTRE 01 E 02 ANOS"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 02 E 03 ANOS"]<- "EENTRE 02 E 03 ANOS"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 03 E 04 ANOS"]<- "FENTRE 03 E 04 ANOS"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "MAIS DE 04 ANOS"]<- "GMAIS DE 04 ANOS"


#########################################################################################################
# salvando para gráfico
tempo_medio_geral_OBITO_TAB_02_bkp = tempo_medio_geral_OBITO_TAB_02

tempo_medio_geral_OBITO_TAB_02_bkp =
  tempo_medio_geral_OBITO_TAB_02_bkp %>%
  janitor::tabyl(INTER_TEMPO_MEDIO) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"




#########################################################################################################
#tempo_medio_geral_OBITO_TAB_02_bkp

#tempo_medio_geral_OBITO_TAB_02
tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO == "AATÉ 01 MÊS"]<- "ATÉ 01 MÊS"
tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO == "BENTRE 01 E 06 MESES"]<- "ENTRE 01 E 06 MESES"
tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO == "CENTRE 06 MESES E 01 ANO"]<- "ENTRE 06 MESES E 01 ANO"
tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO == "DENTRE 01 E 02 ANOS"]<- "ENTRE 01 E 02 ANOS"
tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO == "EENTRE 02 E 03 ANOS"]<- "ENTRE 02 E 03 ANOS"
tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO == "FENTRE 03 E 04 ANOS"]<- "ENTRE 03 E 04 ANOS"
tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02_bkp$INTER_TEMPO_MEDIO == "GMAIS DE 04 ANOS"]<- "MAIS DE 04 ANOS"
#########################################################################################################





colnames(tempo_medio_geral_OBITO_TAB_02_bkp)[1]<-'tempo_medio_geral_OBITO_TAB_02_bkp'
colnames(tempo_medio_geral_OBITO_TAB_02_bkp)[2]<-'QUANTIDADE'
colnames(tempo_medio_geral_OBITO_TAB_02_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

tempo_medio_geral_OBITO_TAB_02 =
  tempo_medio_geral_OBITO_TAB_02 %>%
  janitor::tabyl(INTER_TEMPO_MEDIO) %>%
  arrange(INTER_TEMPO_MEDIO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(tempo_medio_geral_OBITO_TAB_02)[1]<-'INTER_TEMPO_MEDIO'
colnames(tempo_medio_geral_OBITO_TAB_02)[2]<-'QUANTIDADE'
colnames(tempo_medio_geral_OBITO_TAB_02)[3]<-'PERCENTUAL'

#############################################################################################################
#########################################################################################################


#tempo_medio_geral_OBITO_TAB_02_bkp

#tempo_medio_geral_OBITO_TAB_02
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "AATÉ 01 MÊS"]<- "ATÉ 01 MÊS"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "BENTRE 01 E 06 MESES"]<- "ENTRE 01 E 06 MESES"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "CENTRE 06 MESES E 01 ANO"]<- "ENTRE 06 MESES E 01 ANO"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "DENTRE 01 E 02 ANOS"]<- "ENTRE 01 E 02 ANOS"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "EENTRE 02 E 03 ANOS"]<- "ENTRE 02 E 03 ANOS"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "FENTRE 03 E 04 ANOS"]<- "ENTRE 03 E 04 ANOS"
tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_OBITO_TAB_02$INTER_TEMPO_MEDIO == "GMAIS DE 04 ANOS"]<- "MAIS DE 04 ANOS"
#########################################################################################################

colnames(tempo_medio_geral_OBITO_TAB_02)[1]<-'TEMPO'


#############################################################################################################
###tempo_medio_geral_OBITO_TAB_02 FIM
#############################################################################################################
#########################################################################################################
###tempo_medio_geral_CIABH_TAB_01
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)
require(lubridate)


tempo_medio_geral_CIABH_TAB_01 =

  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter(PASSAGEM_CIABH == "SIM") %>%
  filter (CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(DATA_ULTIMA_ENTRADA2, DATA_PRIMEIRA_ENTRADA2, ENTRADA2)


tempo_medio_geral_CIABH_TAB_01 = tempo_medio_geral_CIABH_TAB_01 %>%
  na.omit() %>%
  group_by(DATA_PRIMEIRA_ENTRADA2)



#########################################################################################################
#CONVERTE PARA DATE (DATA_ULTIMA_ENTRADA2 JÁ CONVERTIDO LINHAS ACIMA)
#tempo_medio_geral_CIABH_TAB_01$INTER_TEMPO_MEDIO_medio_geral <- as.character(tempo_medio_geral_CIABH_TAB_01$INTER_TEMPO_MEDIO_medio_geral)
## calcula o intervalo em anos
tempo_medio_geral_CIABH_TAB_01$INTER_01 = as.period(interval(tempo_medio_geral_CIABH_TAB_01$DATA_PRIMEIRA_ENTRADA2, tempo_medio_geral_CIABH_TAB_01$DATA_ULTIMA_ENTRADA2))



#tempo_medio_geral_CIABH_TAB_01$INTER_02 <- (ymd(tempo_medio_geral_CIABH_TAB_01$DATA_ULTIMA_ENTRADA2)  %--%
#   ymd(tempo_medio_geral_CIABH_TAB_01$DATA_PRIMEIRA_ENTRADA2) / ddays(1))

#tempo_medio_geral_CIABH_TAB_01$INTER_02 = (tempo_medio_geral_CIABH_TAB_01$DATA_ULTIMA_ENTRADA2) %--% (tempo_medio_geral_CIABH_TAB_01$DATA_PRIMEIRA_ENTRADA2) / ddays(1)
tempo_medio_geral_CIABH_TAB_01$INTER_02 = (tempo_medio_geral_CIABH_TAB_01$DATA_PRIMEIRA_ENTRADA2) %--% (tempo_medio_geral_CIABH_TAB_01$DATA_ULTIMA_ENTRADA2) / ddays(1)

#head(tempo_medio_geral_CIABH_TAB_01, 10)

#########################################################################################################
tempo_medio_geral_CIABH_TAB_02 = tempo_medio_geral_CIABH_TAB_01
#########################################################################################################
#tabela media tempo decisao
tempo_medio_geral_CIABH_TAB_01 = data.frame(round(mean(tempo_medio_geral_CIABH_TAB_01$INTER_02), 1))

colnames(tempo_medio_geral_CIABH_TAB_01) <- c("TEMPO")

#tempo_medio_geral_CIABH_TAB_01

tempo_medio_geral_CIABH_TAB_01$TEMPO = paste(tempo_medio_geral_CIABH_TAB_01$TEMPO, "DIAS", sep=" ")#para plotar o sinal de porcentagem

colnames(tempo_medio_geral_CIABH_TAB_01) <- c("TEMPO MÉDIO")

#tempo_medio_geral_CIABH_TAB_01

#para tabela gt abaixo:
#tempo_medio_geral_CIABH_TAB_01_gt = tempo_medio_geral_CIABH_TAB_01


#########################################################################################################
###tempo_medio_geral_CIABH_TAB_01 FIM
#########################################################################################################
#########################################################################################################
#tempo_medio_geral_CIABH_TAB_02
#########################################################################################################
#classificando

tempo_medio_geral_CIABH_TAB_02$INTER_03 = ifelse(tempo_medio_geral_CIABH_TAB_02$INTER_02 < 32,
                                                 "ATÉ 01 MÊS", "DESCONSIDERAR")


tempo_medio_geral_CIABH_TAB_02$INTER_04 = ifelse((tempo_medio_geral_CIABH_TAB_02$INTER_02 > 31 & tempo_medio_geral_CIABH_TAB_02$INTER_02 <= 180),
                                                 "ENTRE 01 E 06 MESES", "DESCONSIDERAR")

tempo_medio_geral_CIABH_TAB_02$INTER_05 = ifelse((tempo_medio_geral_CIABH_TAB_02$INTER_02 > 180 & tempo_medio_geral_CIABH_TAB_02$INTER_02 <= 365),
                                                 "ENTRE 06 MESES E 01 ANO", "DESCONSIDERAR")


tempo_medio_geral_CIABH_TAB_02$INTER_06 = ifelse((tempo_medio_geral_CIABH_TAB_02$INTER_02 > 365 & tempo_medio_geral_CIABH_TAB_02$INTER_02 <= 730),
                                                 "ENTRE 01 E 02 ANOS", "DESCONSIDERAR")


tempo_medio_geral_CIABH_TAB_02$INTER_07 = ifelse((tempo_medio_geral_CIABH_TAB_02$INTER_02 > 730 & tempo_medio_geral_CIABH_TAB_02$INTER_02 <= 1095),
                                                 "ENTRE 02 E 03 ANOS", "DESCONSIDERAR")


tempo_medio_geral_CIABH_TAB_02$INTER_08 = ifelse((tempo_medio_geral_CIABH_TAB_02$INTER_02 > 1095 & tempo_medio_geral_CIABH_TAB_02$INTER_02 <= 1460),
                                                 "ENTRE 03 E 04 ANOS", "DESCONSIDERAR")


tempo_medio_geral_CIABH_TAB_02$INTER_09 = ifelse(tempo_medio_geral_CIABH_TAB_02$INTER_02 > 1460,
                                                 "MAIS DE 04 ANOS", "DESCONSIDERAR")

#########################################################################################################

#########################################################################################################

tempo_medio_geral_CIABH_TAB_02 = tempo_medio_geral_CIABH_TAB_02 %>%
  select(6:12)




tempo_medio_geral_CIABH_TAB_02 =

  tempo_medio_geral_CIABH_TAB_02 %>%
  pivot_longer(cols = starts_with("INTER_0"), values_to = "INTER_TEMPO_MEDIO") %>%
  #select(-name) %>%
  filter(INTER_TEMPO_MEDIO != "DESCONSIDERAR")

#########################################################################################################


#tempo_medio_geral_CIABH_TAB_02

tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "ATÉ 01 MÊS"]<- "AATÉ 01 MÊS"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 01 E 06 MESES"]<- "BENTRE 01 E 06 MESES"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 06 MESES E 01 ANO"]<- "CENTRE 06 MESES E 01 ANO"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 01 E 02 ANOS"]<- "DENTRE 01 E 02 ANOS"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 02 E 03 ANOS"]<- "EENTRE 02 E 03 ANOS"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "ENTRE 03 E 04 ANOS"]<- "FENTRE 03 E 04 ANOS"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "MAIS DE 04 ANOS"]<- "GMAIS DE 04 ANOS"


#########################################################################################################
# salvando para gráfico
tempo_medio_geral_CIABH_TAB_02_bkp = tempo_medio_geral_CIABH_TAB_02

tempo_medio_geral_CIABH_TAB_02_bkp =
  tempo_medio_geral_CIABH_TAB_02_bkp %>%
  janitor::tabyl(INTER_TEMPO_MEDIO) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"



#########################################################################################################

tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO == "AATÉ 01 MÊS"]<- "ATÉ 01 MÊS"
tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO == "BENTRE 01 E 06 MESES"]<- "ENTRE 01 E 06 MESES"
tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO == "CENTRE 06 MESES E 01 ANO"]<- "ENTRE 06 MESES E 01 ANO"
tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO == "DENTRE 01 E 02 ANOS"]<- "ENTRE 01 E 02 ANOS"
tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO == "EENTRE 02 E 03 ANOS"]<- "ENTRE 02 E 03 ANOS"
tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO == "FENTRE 03 E 04 ANOS"]<- "ENTRE 03 E 04 ANOS"
tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02_bkp$INTER_TEMPO_MEDIO == "GMAIS DE 04 ANOS"]<- "MAIS DE 04 ANOS"

#########################################################################################################


colnames(tempo_medio_geral_CIABH_TAB_02_bkp)[1]<-'tempo_medio_geral_CIABH_TAB_02_bkp'
colnames(tempo_medio_geral_CIABH_TAB_02_bkp)[2]<-'QUANTIDADE'
colnames(tempo_medio_geral_CIABH_TAB_02_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

tempo_medio_geral_CIABH_TAB_02 =
  tempo_medio_geral_CIABH_TAB_02 %>%
  janitor::tabyl(INTER_TEMPO_MEDIO) %>%
  arrange(INTER_TEMPO_MEDIO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(tempo_medio_geral_CIABH_TAB_02)[1]<-'INTER_TEMPO_MEDIO'
colnames(tempo_medio_geral_CIABH_TAB_02)[2]<-'QUANTIDADE'
colnames(tempo_medio_geral_CIABH_TAB_02)[3]<-'PERCENTUAL'

#############################################################################################################

#tempo_medio_geral_CIABH_TAB_02

tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "AATÉ 01 MÊS"]<- "ATÉ 01 MÊS"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "BENTRE 01 E 06 MESES"]<- "ENTRE 01 E 06 MESES"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "CENTRE 06 MESES E 01 ANO"]<- "ENTRE 06 MESES E 01 ANO"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "DENTRE 01 E 02 ANOS"]<- "ENTRE 01 E 02 ANOS"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "EENTRE 02 E 03 ANOS"]<- "ENTRE 02 E 03 ANOS"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "FENTRE 03 E 04 ANOS"]<- "ENTRE 03 E 04 ANOS"
tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO[tempo_medio_geral_CIABH_TAB_02$INTER_TEMPO_MEDIO == "GMAIS DE 04 ANOS"]<- "MAIS DE 04 ANOS"

colnames(tempo_medio_geral_CIABH_TAB_02)[1]<-'TEMPO'


#############################################################################################################
###tempo_medio_geral_CIABH_TAB_02 FIM
#############################################################################################################

#########################################################################################################
#############################################################################################################
#banco_NUMERO_ENTRADAS_let encaminhado
#########################################################################################################
#

banco_NUMERO_ENTRADAS_let =
  banco_letalidade_12_20 %>%
  filter(PASSAGEM_CIABH %in% "SIM" & CAUSA_JURIDICA %in% "HOMICÍDIO") %>%
  select(NUMERO_ENTRADAS)

#########################################################################################################

#########################################################################################################
#classificando

banco_NUMERO_ENTRADAS_let$INTER_03 = ifelse((banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS >= 1 & banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS <= 5),
                                            "ENTRE 01 E 05 ENTRADAS", "DESCONSIDERAR")


banco_NUMERO_ENTRADAS_let$INTER_04 = ifelse((banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS >= 6 & banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS <= 10),
                                            "ENTRE 06 E 10 ENTRADAS", "DESCONSIDERAR")

banco_NUMERO_ENTRADAS_let$INTER_05 = ifelse((banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS >= 11 & banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS <= 15),
                                            "ENTRE 11 E 15 ENTRADAS", "DESCONSIDERAR")


banco_NUMERO_ENTRADAS_let$INTER_06 = ifelse((banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS >= 16 & banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS <= 20),
                                            "ENTRE 16 E 20 ENTRADAS", "DESCONSIDERAR")


banco_NUMERO_ENTRADAS_let$INTER_07 = ifelse(banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS > 20,
                                            "MAIS DE 20 ENTRADAS", "DESCONSIDERAR")

#########################################################################################################

banco_NUMERO_ENTRADAS_let =

  banco_NUMERO_ENTRADAS_let %>%
  pivot_longer(cols = starts_with("INTER_0"), values_to = "INTER_TEMPO_MEDIO") %>%
  select(-name) %>%
  filter(INTER_TEMPO_MEDIO != "DESCONSIDERAR")
#########################################################################################################
#########################################################################################################
#banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS = ajustar_nomes(banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS)
#########################################################################################################
#SUBSTITUIR
#banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS[banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS == "Total"]<- "TOTAL"
#banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS[banco_NUMERO_ENTRADAS_let$NUMERO_ENTRADAS == "IGNORADO\\PREJUDICADO"]<- "IGNORADO/PREJUDICADO"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_NUMERO_ENTRADAS_let_bkp = banco_NUMERO_ENTRADAS_let

banco_NUMERO_ENTRADAS_let_bkp =
  banco_NUMERO_ENTRADAS_let_bkp %>%
  janitor::tabyl(INTER_TEMPO_MEDIO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(banco_NUMERO_ENTRADAS_let_bkp)[1]<-'banco_NUMERO_ENTRADAS_let_bkp'
colnames(banco_NUMERO_ENTRADAS_let_bkp)[2]<-'QUANTIDADE'
colnames(banco_NUMERO_ENTRADAS_let_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_NUMERO_ENTRADAS_let =
  banco_NUMERO_ENTRADAS_let %>%
  janitor::tabyl(INTER_TEMPO_MEDIO) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:

colnames(banco_NUMERO_ENTRADAS_let)[1]<-'ENTRADAS'
colnames(banco_NUMERO_ENTRADAS_let)[2]<-'QUANTIDADE'
colnames(banco_NUMERO_ENTRADAS_let)[3]<-'PERCENTUAL'
#########################################################################################################
#para tabela media de entradas:
banco_MEDIA_ENTRADAS_Let =
  banco_letalidade_12_20 %>%
  filter(PASSAGEM_CIABH %in% "SIM" & CAUSA_JURIDICA %in% "HOMICÍDIO") %>%
  select(NUMERO_ENTRADAS)


#########################################################################################################
#tabela media tempo decisao
banco_MEDIA_ENTRADAS_Let = data.frame(round(mean(banco_MEDIA_ENTRADAS_Let$NUMERO_ENTRADAS), 1))

colnames(banco_MEDIA_ENTRADAS_Let) <- c("MÉDIA ENTRADAS")

#banco_MEDIA_ENTRADAS_Let

#########################################################################################################
###banco_MEDIA_ENTRADAS_Let FIM
#########################################################################################################
#############################################################################################################
#banco_NUMERO_ENTRADAS_let FIM
#########################################################################################################
#############################################################################################################
#BANCO_MEDIDAS_LET
#########################################################################################################
BANCO_MEDIDAS_LET =
  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter(PASSAGEM_CIABH == "SIM") %>%
  filter(CAUSA_JURIDICA == "HOMICÍDIO") %>%
  filter(POSSUI_MEDIDA_PROTETIVA == "SIM") |>
  pivot_longer(cols = starts_with("MEDIDA_PROTETIVA_0"), values_to = "BANCO_MEDIDAS_LET_GERAL") %>%
  #select(-name) %>%
  filter(BANCO_MEDIDAS_LET_GERAL != "")

#########################################################################################################
#########################################################################################################

BANCO_MEDIDAS_LET =
  BANCO_MEDIDAS_LET |>
  select(BANCO_MEDIDAS_LET_GERAL)

colnames(BANCO_MEDIDAS_LET)[1]<-'BANCO_MEDIDAS_LET'
#########################################################################################################
#########################################################################################################
#########################################################################################################
BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET = ifelse(BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "I",
                                             "ART. 101, I", BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET)


BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET = ifelse(BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "II",
                                             "ART. 101, II", BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET)

table(BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET)
BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET = ifelse(BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "III",
                                             "ART. 101, III", BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET)


BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET = ifelse(BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "IV",
                                             "ART. 101, IV", BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET)


BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET = ifelse(BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "V",
                                             "ART. 101, V", BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET)


BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET = ifelse(BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "VI",
                                             "ART. 101, VI", BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET)


BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET = ifelse(BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "VII",
                                             "ART. 101, VII", BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET)

#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
BANCO_MEDIDAS_LET_bkp = BANCO_MEDIDAS_LET

BANCO_MEDIDAS_LET_bkp =
  BANCO_MEDIDAS_LET_bkp %>%
  janitor::tabyl(BANCO_MEDIDAS_LET) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
BANCO_MEDIDAS_LET_bkp$BANCO_MEDIDAS_LET[BANCO_MEDIDAS_LET_bkp$BANCO_MEDIDAS_LET == "VNÃO SABE"]<- "NÃO SABE"
BANCO_MEDIDAS_LET_bkp$BANCO_MEDIDAS_LET[BANCO_MEDIDAS_LET_bkp$BANCO_MEDIDAS_LET == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
BANCO_MEDIDAS_LET_bkp$PERCENTUAL2 <- str_replace (BANCO_MEDIDAS_LET_bkp$percent, "%", "")
BANCO_MEDIDAS_LET_bkp$PERCENTUAL2 = as.numeric(BANCO_MEDIDAS_LET_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(BANCO_MEDIDAS_LET_bkp)[1]<-'BANCO_MEDIDAS_LET_bkp'
colnames(BANCO_MEDIDAS_LET_bkp)[2]<-'QUANTIDADE'
colnames(BANCO_MEDIDAS_LET_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

BANCO_MEDIDAS_LET_bkp_rmark = BANCO_MEDIDAS_LET_bkp

BANCO_MEDIDAS_LET_bkp_rmark = BANCO_MEDIDAS_LET_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
BANCO_MEDIDAS_LET_bkp_rmark =
  BANCO_MEDIDAS_LET_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET[BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "VNÃO SABE"]<- "UNÃO SABE"
#BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET[BANCO_MEDIDAS_LET$BANCO_MEDIDAS_LET == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
BANCO_MEDIDAS_LET_TABELA =
  BANCO_MEDIDAS_LET %>%
  janitor::tabyl(BANCO_MEDIDAS_LET) %>%
  arrange(BANCO_MEDIDAS_LET) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
BANCO_MEDIDAS_LET_TABELA$BANCO_MEDIDAS_LET[BANCO_MEDIDAS_LET_TABELA$BANCO_MEDIDAS_LET == "UNÃO SABE"]<- "NÃO SABE"
BANCO_MEDIDAS_LET_TABELA$BANCO_MEDIDAS_LET[BANCO_MEDIDAS_LET_TABELA$BANCO_MEDIDAS_LET == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(BANCO_MEDIDAS_LET_TABELA)[1]<-'MEDIDA'
colnames(BANCO_MEDIDAS_LET_TABELA)[2]<-'QUANTIDADE'
colnames(BANCO_MEDIDAS_LET_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#graf_pizza_MEDIDAS_LET
#########################################################################################################

graf_pizza_MEDIDAS_LET =
  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  filter(PASSAGEM_CIABH %in% "SIM") %>%
  select(POSSUI_MEDIDA_PROTETIVA)
#########################################################################################################

graf_pizza_MEDIDAS_LET$POSSUI_MEDIDA_PROTETIVA = ajustar_nomes(graf_pizza_MEDIDAS_LET$POSSUI_MEDIDA_PROTETIVA)

graf_pizza_MEDIDAS_LET$POSSUI_MEDIDA_PROTETIVA <- str_replace (graf_pizza_MEDIDAS_LET$POSSUI_MEDIDA_PROTETIVA, "NAO", "NÃO")

colnames(graf_pizza_MEDIDAS_LET)[1]<-'graf_pizza_MEDIDAS_LET'
#########################################################################################################

graf_pizza_MEDIDAS_LET =
  graf_pizza_MEDIDAS_LET |>
  count(graf_pizza_MEDIDAS_LET, sort = TRUE)
#########################################################################################################
#colnames(graf_pizza_MEDIDAS_LET)[1]<-'QUANTIDADE'
colnames(graf_pizza_MEDIDAS_LET)[2]<-'QUANTIDADE'
#########################################################################################################
#########################################################################################################
#############################################################################################################
#BANCO_MEDIDAS_LET FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
#############################################################################################################
#BANCO_DECISAO_LET
#########################################################################################################
BANCO_DECISAO_LET =
  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  #filter(PASSAGEM_CIABH == "SIM") %>%
  #filter(CAUSA_JURIDICA == "HOMICÍDIO") %>%
  #filter(POSSUI_MEDIDA_PROTETIVA == "SIM") |>
  pivot_longer(cols = starts_with("DECISAO_APLICADA"), values_to = "BANCO_DECISAO_LET_GERAL") %>%
  #select(-name) %>%
  filter(BANCO_DECISAO_LET_GERAL != "")

#########################################################################################################
#########################################################################################################

BANCO_DECISAO_LET =
  BANCO_DECISAO_LET |>
  select(BANCO_DECISAO_LET_GERAL)

colnames(BANCO_DECISAO_LET)[1]<-'BANCO_DECISAO_LET'
#########################################################################################################
#########################################################################################################

#preenchimento de celulas:
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "INTERNACAO"]<-	"INTERNAÇÃO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "LA"]<-	"REMISSAO c/c LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "OUTRAS(OS)"]<-	"VOUTROS"
#BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "PSC"]<-	"REMISSAO c/c PSC"
#BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "SEMINFORMACAO"]<-	"VAZIO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOAPSC"]<-	"RETORNO A PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOALA"]<-	"RETORNO A LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAO_CC_PSC"]<-	"REMISSÃO c/c PSC"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAO_CC_LA"]<-	"REMISSÃO c/c LA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == ""]<-	"VAZIO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "EXTINÇAO_PUNIBILIDADE_MORTE"]<-	"EXTINÇÃO PUNIBILIDADE POR MORTE"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "INTERNAÇAO"]<-	"INTERNAÇÃO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "NI"]<-	"VSEM INFORMAÇÃO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSÃO_C_EXCLUSÃO PROCESSUAL"]<-	"REMISSÃO c/c EXCLUSÃO PROCESSUAL"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAO"]<-	"REMISSÃO"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "REMISSAO_CC_ADV"]<-	"REMISSÃO c/c ADVERTÊNCIA"
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "EXTINÇÃO POR PRESCRIÇAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"

#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
# salvando para gráfico
BANCO_DECISAO_LET_bkp = BANCO_DECISAO_LET

BANCO_DECISAO_LET_bkp =
  BANCO_DECISAO_LET_bkp %>%
  janitor::tabyl(BANCO_DECISAO_LET) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
BANCO_DECISAO_LET_bkp$BANCO_DECISAO_LET[BANCO_DECISAO_LET_bkp$BANCO_DECISAO_LET == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
BANCO_DECISAO_LET_bkp$BANCO_DECISAO_LET[BANCO_DECISAO_LET_bkp$BANCO_DECISAO_LET == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
BANCO_DECISAO_LET_bkp$PERCENTUAL2 <- str_replace (BANCO_DECISAO_LET_bkp$percent, "%", "")
BANCO_DECISAO_LET_bkp$PERCENTUAL2 = as.numeric(BANCO_DECISAO_LET_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(BANCO_DECISAO_LET_bkp)[1]<-'BANCO_DECISAO_LET_bkp'
colnames(BANCO_DECISAO_LET_bkp)[2]<-'QUANTIDADE'
colnames(BANCO_DECISAO_LET_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

BANCO_DECISAO_LET_bkp_rmark = BANCO_DECISAO_LET_bkp

BANCO_DECISAO_LET_bkp_rmark = BANCO_DECISAO_LET_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
BANCO_DECISAO_LET_bkp_rmark =
  BANCO_DECISAO_LET_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "VNÃO SABE"]<- "UNÃO SABE"
#BANCO_DECISAO_LET$BANCO_DECISAO_LET[BANCO_DECISAO_LET$BANCO_DECISAO_LET == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
BANCO_DECISAO_LET_TABELA =
  BANCO_DECISAO_LET %>%
  janitor::tabyl(BANCO_DECISAO_LET) %>%
  arrange(BANCO_DECISAO_LET) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
BANCO_DECISAO_LET_TABELA$BANCO_DECISAO_LET[BANCO_DECISAO_LET_TABELA$BANCO_DECISAO_LET == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
BANCO_DECISAO_LET_TABELA$BANCO_DECISAO_LET[BANCO_DECISAO_LET_TABELA$BANCO_DECISAO_LET == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(BANCO_DECISAO_LET_TABELA)[1]<-'DECISÃO'
colnames(BANCO_DECISAO_LET_TABELA)[2]<-'QUANTIDADE'
colnames(BANCO_DECISAO_LET_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#BANCO_DECISAO_LET FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
###EXTINÇÃO DE PUNIBILIDADE
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
banco_EXTPUNIB_LET =

  banco_iml_GERAL %>%
  filter(IDADE2 >= 12 & IDADE2 <= 20) %>%
  pivot_longer(cols = starts_with("DECISAO_APLICADA_0"), values_to = "DECISAO_APLICADA2") %>%
  #select(-name) %>%
  filter(DECISAO_APLICADA2 != "")

##########################################################################################################################################
#banco_EXTPUNIB_LET =
# banco_EXTPUNIB_LET %>%
# filter(CAUSA_JURIDICA == "HOMICÍDIO")
################################################################################################################################################################################

#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)
require(lubridate)

banco_EXTPUNIB_LET$DECISAO_APLICADA2 <- gsub(" ","", banco_EXTPUNIB_LET$DECISAO_APLICADA2)

#table(banco_EXTPUNIB_LET$DECISAO_APLICADA2)

banco_EXTPUNIB_LET$DECISAO_APLICADA2 = ajustar_nomes(banco_EXTPUNIB_LET$DECISAO_APLICADA2)

#table(banco_EXTPUNIB_LET$DECISAO_APLICADA2)

banco_EXTPUNIB_LET =
  banco_EXTPUNIB_LET %>%
  filter(DECISAO_APLICADA2 == "EXTINCAOPUNIBILIDADEMORTE")

#table(banco_EXTPUNIB_LET$DECISAO_APLICADA2)

banco_EXTPUNIB_LET$NOME2 = ajustar_nomes(banco_EXTPUNIB_LET$NOME)


#excluir duplicados
banco_EXTPUNIB_LET = distinct(banco_EXTPUNIB_LET, NOME2, NASCIMENTO, .keep_all= TRUE)



#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_EXTPUNIB_sexo_idade_LET_bkp =
  banco_EXTPUNIB_LET %>%
  #filter (CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(SEXO, IDADE2)

#table(banco_EXTPUNIB_sexo_idade_LET_bkp$SEXO)

banco_EXTPUNIB_sexo_idade_LET = banco_EXTPUNIB_sexo_idade_LET_bkp
#########################################################################################################

#table(banco_EXTPUNIB_sexo_idade_LET$SEXO)

banco_EXTPUNIB_sexo_idade_LET$SEXO <- as.character(banco_EXTPUNIB_sexo_idade_LET$SEXO)

banco_EXTPUNIB_sexo_idade_LET$SEXO[banco_EXTPUNIB_sexo_idade_LET$SEXO == ""]<- "M"
#table(banco_EXTPUNIB_sexo_idade_LET$SEXO)

banco_EXTPUNIB_sexo_idade_LET <- table(banco_EXTPUNIB_sexo_idade_LET$IDADE2, banco_EXTPUNIB_sexo_idade_LET$SEXO, useNA ="always")
###write.csv(banco_EXTPUNIB_sexo_idade_LET, file ="banco_EXTPUNIB_sexo_idade_LET.csv",row.names=FALSE)
###write.csv(banco_EXTPUNIB_sexo_idade_LET, file ="banco_EXTPUNIB_sexo_idade_LET.csv",row.names=FALSE)
#sum(banco_EXTPUNIB_sexo_idade_LET)

banco_EXTPUNIB_sexo_idade_LET = data.frame(banco_EXTPUNIB_sexo_idade_LET)
#########################################################################################################
#########################################################################################################
#########################################################################################################


banco_EXTPUNIB_sexo_idade_LET_bkp = banco_EXTPUNIB_sexo_idade_LET


#banco_EXTPUNIB_sexo_idade_LET

colnames(banco_EXTPUNIB_sexo_idade_LET) <- c("IDADE", "SEXO", "QUANTIDADE")

#banco_EXTPUNIB_sexo_idade_LET

banco_EXTPUNIB_sexo_idade_LET$IDADE <- as.character(banco_EXTPUNIB_sexo_idade_LET$IDADE)
banco_EXTPUNIB_sexo_idade_LET$SEXO <- as.character(banco_EXTPUNIB_sexo_idade_LET$SEXO)
#sum(banco_EXTPUNIB_sexo_idade_LET$QUANTIDADE)


banco_EXTPUNIB_sexo_idade_LET = filter(banco_EXTPUNIB_sexo_idade_LET, !QUANTIDADE == 0)
#banco_EXTPUNIB_sexo_idade_LET

#PREENCHER COM NA'S CELULAS VAZIAS
#banco_EXTPUNIB_sexo_idade_LET$IDADE[banco_EXTPUNIB_sexo_idade_LET$IDADE == "SEM INFORMACAO"]<- "<NA>"
banco_EXTPUNIB_sexo_idade_LET$IDADE[which(is.na(banco_EXTPUNIB_sexo_idade_LET$IDADE))] <- "s/inf"
#banco_EXTPUNIB_sexo_idade_LET


banco_EXTPUNIB_sexo_idade_LET$IDADE <- paste(banco_EXTPUNIB_sexo_idade_LET$IDADE, "anos", sep=" ")
banco_EXTPUNIB_sexo_idade_LET$IDADE[banco_EXTPUNIB_sexo_idade_LET$IDADE == "s/inf anos"]<- "s/inf"
#banco_EXTPUNIB_sexo_idade_LET


#########################################################################################################
# GRAFICO SEXO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

banco_EXTPUNIB_sexo_LET =


  banco_EXTPUNIB_LET %>%
  #filter (CAUSA_JURIDICA == "HOMICÍDIO") %>%
  select(SEXO)

#table(banco_EXTPUNIB_sexo_LET$SEXO)

#########################################################################################################
#########################################################################################################

banco_EXTPUNIB_sexo_LET$SEXO <- as.character(banco_EXTPUNIB_sexo_LET$SEXO)

banco_EXTPUNIB_sexo_LET$SEXO[banco_EXTPUNIB_sexo_LET$SEXO == ""]<- "M"
#table(banco_EXTPUNIB_sexo_LET$SEXO)

banco_EXTPUNIB_sexo_LET = data.frame(table(banco_EXTPUNIB_sexo_LET$SEXO))

colnames(banco_EXTPUNIB_sexo_LET) <- c("SEXO", "QUANTIDADE")

#sum(banco_EXTPUNIB_sexo_LET$QUANTIDADE)

#########################################################################################################
#########################################################################################################


banco_EXTPUNIB_sexo_LET$SEXO <- as.character(banco_EXTPUNIB_sexo_LET$SEXO)

banco_EXTPUNIB_sexo_LET$SEXO[banco_EXTPUNIB_sexo_LET$SEXO == "F"]<- "FEMININO"
banco_EXTPUNIB_sexo_LET$SEXO[banco_EXTPUNIB_sexo_LET$SEXO == "M"]<- "MASCULINO"

banco_EXTPUNIB_sexo_LET_original=banco_EXTPUNIB_sexo_LET

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
#usando a funcao criada:
#banco_EXTPUNIB_sexo_LET

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

banco_EXTPUNIB_sexo_LET <- banco_EXTPUNIB_sexo_LET %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

banco_EXTPUNIB_sexo_LET$PERCENTUAL <- paste(banco_EXTPUNIB_sexo_LET$PERCENTUAL, "%", sep=" ")

#banco_EXTPUNIB_sexo_LET

banco_EXTPUNIB_sexo_LET$SEXO = ajustar_nomes(banco_EXTPUNIB_sexo_LET$SEXO)



#########################################################################################################
#########################################################################################################
###EXTINÇÃO DE PUNIBILIDADE FIM
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
########################################################################################################
#FIM
##################################################################################################################################################################################################
