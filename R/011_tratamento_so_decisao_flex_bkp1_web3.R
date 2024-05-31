#############################################################################################################
#so_decisao
#########################################################################################################
so_decisao =
  banco_sem_mba %>%
  filter(COMPARECIMENTO_AUD_PRELIMINAR %in% "COMPARECEU") %>%
  select(DECISAO)

#########################################################################################################
#########################################################################################################
colnames(so_decisao)[1]<-'so_decisao'
#########################################################################################################
#########################################################################################################

#preenchimento de celulas:
so_decisao$so_decisao[so_decisao$so_decisao == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
so_decisao$so_decisao[so_decisao$so_decisao == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
so_decisao$so_decisao[so_decisao$so_decisao == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
so_decisao$so_decisao[so_decisao$so_decisao == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
so_decisao$so_decisao[so_decisao$so_decisao == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
so_decisao$so_decisao[so_decisao$so_decisao == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
so_decisao$so_decisao[so_decisao$so_decisao == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
so_decisao$so_decisao[so_decisao$so_decisao == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
so_decisao$so_decisao[so_decisao$so_decisao == "INTERNACAO"]<-	"INTERNAÇÃO"
so_decisao$so_decisao[so_decisao$so_decisao == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
so_decisao$so_decisao[so_decisao$so_decisao == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
so_decisao$so_decisao[so_decisao$so_decisao == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#so_decisao$so_decisao[so_decisao$so_decisao == "LA"]<-	"REMISSAO c/c LA"
so_decisao$so_decisao[so_decisao$so_decisao == "OUTRAS(OS)"]<-	"VOUTROS"
#so_decisao$so_decisao[so_decisao$so_decisao == "PSC"]<-	"REMISSAO c/c PSC"
#so_decisao$so_decisao[so_decisao$so_decisao == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$so_decisao[so_decisao$so_decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$so_decisao[so_decisao$so_decisao == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$so_decisao[so_decisao$so_decisao == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
so_decisao$so_decisao[so_decisao$so_decisao == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
so_decisao$so_decisao[so_decisao$so_decisao == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
so_decisao$so_decisao[so_decisao$so_decisao == "SEMINFORMACAO"]<-	"VAZIO"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOAPSC"]<-	"RETORNO A PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOALA"]<-	"RETORNO A LA"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
so_decisao$so_decisao[so_decisao$so_decisao == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
so_decisao$so_decisao[so_decisao$so_decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAO_CC_PSC"]<-	"REMISSÃO c/c PSC"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAO_CC_LA"]<-	"REMISSÃO c/c LA"
so_decisao$so_decisao[so_decisao$so_decisao == ""]<-	"VAZIO"
so_decisao$so_decisao[so_decisao$so_decisao == "EXTINÇAO_PUNIBILIDADE_MORTE"]<-	"EXTINÇÃO PUNIBILIDADE POR MORTE"
so_decisao$so_decisao[so_decisao$so_decisao == "INTERNAÇAO"]<-	"INTERNAÇÃO"
so_decisao$so_decisao[so_decisao$so_decisao == "NI"]<-	"VSEM INFORMAÇÃO"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSÃO_C_EXCLUSÃO PROCESSUAL"]<-	"REMISSÃO c/c EXCLUSÃO PROCESSUAL"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAO"]<-	"REMISSÃO"
so_decisao$so_decisao[so_decisao$so_decisao == "REMISSAO_CC_ADV"]<-	"REMISSÃO c/c ADVERTÊNCIA"
so_decisao$so_decisao[so_decisao$so_decisao == "EXTINÇÃO POR PRESCRIÇAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"

#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
# salvando para gráfico
so_decisao_bkp = so_decisao

so_decisao_bkp =
  so_decisao_bkp %>%
  janitor::tabyl(so_decisao) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
so_decisao_bkp$so_decisao[so_decisao_bkp$so_decisao == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
so_decisao_bkp$so_decisao[so_decisao_bkp$so_decisao == "VOUTROS"]<- "OUTROS"
#########################################################################################################
#replace "%" with "" in the percentual column
so_decisao_bkp$PERCENTUAL2 <- str_replace (so_decisao_bkp$percent, "%", "")
so_decisao_bkp$PERCENTUAL2 = as.numeric(so_decisao_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(so_decisao_bkp)[1]<-'so_decisao_bkp'
colnames(so_decisao_bkp)[2]<-'QUANTIDADE'
colnames(so_decisao_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#########################################################################################################
#para script rmd:
so_decisao_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", so_decisao_bkp$PERCENTUAL))
so_decisao_bkp_rmd = tail(so_decisao_bkp,5)

so_decisao_bkp_rmd1 =
  so_decisao_bkp |>
  filter(grepl("^REMISSÃO|ARQUIVAMENTO$", so_decisao_bkp))
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
so_decisao$so_decisao[so_decisao$so_decisao == "VNÃO SABE"]<- "UNÃO SABE"
#so_decisao$so_decisao[so_decisao$so_decisao == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
so_decisao_TABELA =
  so_decisao %>%
  janitor::tabyl(so_decisao) %>%
  arrange(so_decisao) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
so_decisao_TABELA$so_decisao[so_decisao_TABELA$so_decisao == "VSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
so_decisao_TABELA$so_decisao[so_decisao_TABELA$so_decisao == "VOUTROS"]<- "OUTROS"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(so_decisao_TABELA)[1]<-'DECISÃO'
colnames(so_decisao_TABELA)[2]<-'QUANTIDADE'
colnames(so_decisao_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#so_decisao FIM
#########################################################################################################
