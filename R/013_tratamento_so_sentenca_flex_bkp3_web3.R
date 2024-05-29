#############################################################################################################
#so_sentenca
#########################################################################################################
so_sentenca =
  banco_sem_mba %>%
  #filter(COMPARECIMENTO_AUD_PRELIMINAR %in% "COMPARECEU") %>%
  select(SENTENCA)

#########################################################################################################
#########################################################################################################
colnames(so_sentenca)[1]<-'so_sentenca'
#########################################################################################################
#########################################################################################################

#preenchimento de celulas:
#########################################################################################################
#SEPARAR OS ENCAMINHADOS PARA DECISÃO PARA CONSTAR NA TABELA TOTAL DE DECISÃO.
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "SEMINFORMACAO"]<-	"ENCAMINHADOS PARA DECISÃO"
#########################################################################################################

so_sentenca$so_sentenca[so_sentenca$so_sentenca == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "INTERNACAO"]<-	"INTERNAÇÃO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#so_sentenca$so_sentenca[so_sentenca$so_sentenca == "LA"]<-	"REMISSAO c/c LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "OUTRAS(OS)"]<-	"VOUTROS"
#so_sentenca$so_sentenca[so_sentenca$so_sentenca == "PSC"]<-	"REMISSAO c/c PSC"
#so_sentenca$so_sentenca[so_sentenca$so_sentenca == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "SEMINFORMACAO"]<-	"VAZIO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOAPSC"]<-	"RETORNO A PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOALA"]<-	"RETORNO A LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAO_CC_PSC"]<-	"REMISSÃO c/c PSC"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAO_CC_LA"]<-	"REMISSÃO c/c LA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == ""]<-	"VAZIO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "EXTINÇAO_PUNIBILIDADE_MORTE"]<-	"EXTINÇÃO PUNIBILIDADE POR MORTE"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "INTERNAÇAO"]<-	"INTERNAÇÃO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "NI"]<-	"VSEM INFORMAÇÃO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSÃO_C_EXCLUSÃO PROCESSUAL"]<-	"REMISSÃO c/c EXCLUSÃO PROCESSUAL"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAO"]<-	"REMISSÃO"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "REMISSAO_CC_ADV"]<-	"REMISSÃO c/c ADVERTÊNCIA"
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "EXTINÇÃO POR PRESCRIÇAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"

#########################################################################################################
#########################################################################################################
#########################################################################################################
#separando para script soma das decisões

so_sentenca1 =
  so_sentenca |>
  filter(so_sentenca %in% "ENCAMINHADOS PARA DECISÃO") |>
  tabyl(so_sentenca)

#excluindo para tabela sentenças

so_sentenca =
  so_sentenca |>
  filter(!so_sentenca %in% "VAZIO" & !so_sentenca %in% "ENCAMINHADOS PARA DECISÃO")
#########################################################################################################
# salvando para gráfico
so_sentenca_bkp = so_sentenca

so_sentenca_bkp =
  so_sentenca_bkp %>%
  janitor::tabyl(so_sentenca) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
so_sentenca_bkp$so_sentenca[so_sentenca_bkp$so_sentenca == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
so_sentenca_bkp$so_sentenca[so_sentenca_bkp$so_sentenca == "VOUTROS"]<- "OUTROS"
#########################################################################################################
#replace "%" with "" in the percentual column
so_sentenca_bkp$PERCENTUAL2 <- str_replace (so_sentenca_bkp$percent, "%", "")
so_sentenca_bkp$PERCENTUAL2 = as.numeric(so_sentenca_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(so_sentenca_bkp)[1]<-'so_sentenca_bkp'
colnames(so_sentenca_bkp)[2]<-'QUANTIDADE'
colnames(so_sentenca_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

so_sentenca_bkp_rmark = so_sentenca_bkp

so_sentenca_bkp_rmark = so_sentenca_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
so_sentenca_bkp_rmark =
  so_sentenca_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
so_sentenca$so_sentenca[so_sentenca$so_sentenca == "VNÃO SABE"]<- "UNÃO SABE"
#so_sentenca$so_sentenca[so_sentenca$so_sentenca == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
so_sentenca_TABELA =
  so_sentenca %>%
  janitor::tabyl(so_sentenca) %>%
  arrange(so_sentenca) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
so_sentenca_TABELA$so_sentenca[so_sentenca_TABELA$so_sentenca == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
so_sentenca_TABELA$so_sentenca[so_sentenca_TABELA$so_sentenca == "VOUTROS"]<- "OUTROS"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(so_sentenca_TABELA)[1]<-'SENTENÇA'
colnames(so_sentenca_TABELA)[2]<-'QUANTIDADE'
colnames(so_sentenca_TABELA)[3]<-'PERCENTUAL'
#############################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))
#############################################################################################################
#so_sentenca FIM
#########################################################################################################
