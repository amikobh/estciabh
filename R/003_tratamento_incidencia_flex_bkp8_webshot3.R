#########################################################################################################
#TRATAMENTO INCIDENCIA:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_atos_em_foco =

  banco_atos_em_foco %>%
  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") %>%
  #select(-name) %>%
  filter(ATO_INFRACIONAL != "DESCONSIDERARAOSOMAR")

#########################################################################################################
#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
#banco =

#  banco %>%
#  pivot_longer(cols = starts_with("ATO_INFRACIONAL_ATA"), values_to = "ATO_INFRACIONAL") %>%
#select(-name) %>%
#  filter(ATO_INFRACIONAL != "DESCONSIDERARAOSOMAR")

#head(banco, n=10)[,117:120]
#########################################################################################################

#########################################################################################################
#AMEAÇA

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "147.ARTCPB",
                                            "AMEAÇA", banco_atos_em_foco$ATO_INFRACIONAL)


########################################################################################################
#########################################################################################################
#CRIME DE TRÂNSITO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "309.ARTCTB",
                                            "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "310.ARTCTB",
                                            "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "311.ARTCTB",
                                            "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)", banco_atos_em_foco$ATO_INFRACIONAL)

#para discrinar: é so anular com #

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)",
                                            "CRIME DE TRÂNSITO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)",
                                            "CRIME DE TRÂNSITO", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)",
                                            "CRIME DE TRÂNSITO", banco_atos_em_foco$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#DANO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "163.ARTCPB",
                                            "DANO", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "213.ARTCPB",
                                            "ESTUPRO", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "215.ARTCPB",
                                            "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "215-A.ARTCPB",
                                            "IMPORTUNAÇÃO SEXUAL", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ESTUPRO DE VULNERÁVEL

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "217-A.ARTCPB",
                                            "ESTUPRO DE VULNERÁVEL", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#FURTO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "155.ARTCPB",
                                            "FURTO", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#FURTO (TENTATIVA)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "155C/C14.ARTCPB",
                                            "FURTO (TENTATIVA)", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "121.ARTCPB",
                                            "HOMICÍDIO", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO (TENTATIVA)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "121C/C14,II.ARTCPB",
                                            "HOMICÍDIO (TENTATIVA)", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "129.ARTCPB",
                                            "LESÃO CORPORAL", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "129§3º.ARTCPB",
                                            "LESÃO CORPORAL", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "129§9º.ARTCPB",
                                            "LESÃO CORPORAL", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL (TENTATIVA). Ordem para trocar LESÃO CORPORAL (TENTATIVA) por VIAS DE FATO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "129C/C14,II.ARTCPB",
                                            "VIAS DE FATO", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PICHAÇÃO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "65.ART9.605",
                                            "PICHAÇÃO", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PORTE/POSSE DE ARMA

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "12.ART10.826",
                                            "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "14.ART10.826",
                                            "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "16.ART10.826",
                                            "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "15.ART10.826",
                                            "ARMA DE FOGO - PORTE ILEGAL (DISPARO)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "17.ART10.826",
                                            "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)", banco_atos_em_foco$ATO_INFRACIONAL)



banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)",
                                            "PORTE/POSSE DE ARMA", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)",
                                            "PORTE/POSSE DE ARMA", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)",
                                            "PORTE/POSSE DE ARMA", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (DISPARO)",
                                            "PORTE/POSSE DE ARMA", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)",
                                            "PORTE/POSSE DE ARMA", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#RECEPTAÇÃO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "180.ARTCPB",
                                            "RECEPTAÇÃO", banco_atos_em_foco$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157.ARTCPB",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºAICPB",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºAIIARTCPB",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2º,I,IIeVARTCPB",
                                            "ROUBO (EM CONCURSO DE PESSOAS)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2º,I.ARTCPB",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2º,IeII.ARTCPB",
                                            "ROUBO (EM CONCURSO DE PESSOAS)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºA,IARTCPB",
                                            "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºIIARTCPB",
                                            "ROUBO (EM CONCURSO DE PESSOAS)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºIIeVARTCPB",
                                            "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºA,I",
                                            "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºAIARTCPB",
                                            "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºAIeIIARTCPB",
                                            "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºII,VeVIIARTCPB",
                                            "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºIIeVIARTCPB",
                                            "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco$ATO_INFRACIONAL)



banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºIIeVIIARTCPB",
                                            "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºIIEVIIARTCPB",
                                            "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºVARTCPB",
                                            "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºVIIARTCPB",
                                            "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2ºAIICPB",
                                            "ROUBO (EMPREGO DE ARMA)", banco_atos_em_foco$ATO_INFRACIONAL)


#para discrinar: é so anular com #

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                            "ROUBO", banco_atos_em_foco$ATO_INFRACIONAL)



#########################################################################################################
#ROUBO (TENTATIVA)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§2º,IeIIC/C14,II.ARTCPB",
                                            "ROUBO (TENTATIVA)", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157C/C14,II.ARTCPB",
                                            "ROUBO (TENTATIVA)", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ROUBO (§3º)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§3ºARTCPB",
                                            "ROUBO (§3º)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "ROUBO (§3º)",
                                            "LATROCÍNIO", banco_atos_em_foco$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO (§3º) (TENTATIVA)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "157§3ºARTCPBC/C14,II,CPB",
                                            "ROUBO (§3º) (TENTATIVA)", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#SEQUESTRO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "148.ARTCPB",
                                            "SEQUESTRO", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#TRÁFICO DE DROGAS


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "33.ART11.343",
                                            "TRÁFICO DE DROGAS", banco_atos_em_foco$ATO_INFRACIONAL)


#banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "37.ART11.343",
                                            "TRÁFICO DE DROGAS (INFORMANTE)", banco_atos_em_foco$ATO_INFRACIONAL)



#para discrinar: é so anular com #

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "TRÁFICO DE DROGAS",
                                            "TRÁFICO DE DROGAS", banco_atos_em_foco$ATO_INFRACIONAL)


#banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (INFORMANTE)",
                                            "TRÁFICO DE DROGAS", banco_atos_em_foco$ATO_INFRACIONAL)





#########################################################################################################
#########################################################################################################
#########################################################################################################
#ASSOCIAÇÃO TRÁFICO DE DROGAS


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "35.ART11.343",
                                            "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "34.ART11.343",
                                            "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", banco_atos_em_foco$ATO_INFRACIONAL)

#para discrinar: é so anular com #

#banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (ASSOCIAÇÃO)",
#                                           "TRÁFICO DE DROGAS", banco_atos_em_foco$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#USO DE DROGAS

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "28.ART11.343",
                                            "POSSE DE DROGAS PARA USO PESSOAL", banco_atos_em_foco$ATO_INFRACIONAL)



#########################################################################################################
#########################################################################################################
#VIAS DE FATO

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "21.ARTLCP",
                                            "VIAS DE FATO", banco_atos_em_foco$ATO_INFRACIONAL)


#########################################################################################################
#OUTROS

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "140.ARTCPB",
                                            "INJÚRIA", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "140§3º.ARTCPB",
                                            "INJÚRIA", banco_atos_em_foco$ATO_INFRACIONAL)



banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "330.ARTCPB",
                                            "DESOBEDIÊNCIA", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "331.ARTCPB",
                                            "DESACATO", banco_atos_em_foco$ATO_INFRACIONAL)


banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "139.ARTCPB",
                                            "DESOBEDIÊNCIA", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "2.ART13.185",
                                            "INTIMIDAÇÃO SISTEMÁTICA (BULLYING)", banco_atos_em_foco$ATO_INFRACIONAL)

banco_atos_em_foco$ATO_INFRACIONAL = ifelse(banco_atos_em_foco$ATO_INFRACIONAL == "329.ARTCPB",
                                            "RESISTÊNCIA", banco_atos_em_foco$ATO_INFRACIONAL)



#########################################################################################################
banco_atos_em_foco_ESCOLA = banco_atos_em_foco
#########################################################################################################
#########################################################################################################
# substituir ato duplicado, mesma linha, em DESCONSIDERAR AO SOMAR Somente em ATA_02 e ATA_03

#banco_atos_em_foco$ATO_INFRACIONAL_ATA_02 = ifelse(banco_atos_em_foco$ATO_INFRACIONAL_ATA_01 == banco_atos_em_foco$ATO_INFRACIONAL_ATA_02,
#                                                  "REPETIDO", banco_atos_em_foco$ATO_INFRACIONAL_ATA_02)

#banco_atos_em_foco$ATO_INFRACIONAL_ATA_03 = ifelse(banco_atos_em_foco$ATO_INFRACIONAL_ATA_01 == banco_atos_em_foco$ATO_INFRACIONAL_ATA_03,
#                                                  "REPETIDO", banco_atos_em_foco$ATO_INFRACIONAL_ATA_03)


#banco_sem_concurso <- banco_atos_em_foco[!duplicated(data.frame(banco_atos_em_foco$PROCESSO, banco_atos_em_foco$name, banco_atos_em_foco$ATO_INFRACIONAL)),]

library(dplyr)

# Remove duplicate rows of the dataframe using variables
banco_sem_concurso = distinct(banco_atos_em_foco, PROCESSO,name,ATO_INFRACIONAL, .keep_all= TRUE)


#PARA COMPARAR

banco_sem_concurso_bkp = banco_sem_concurso

#write.csv(banco_sem_concurso, file = "banco_sem_concurso.csv", row.names = FALSE)



banco_incidencia = banco_sem_concurso

#########################################################################################################

library(dplyr)

banco_incidencia = banco_incidencia %>%
  select(ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
###banco_incidencia
#########################################################################################################
#setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

#########################################################################################################
#Encontrando OS VARIADOS ARTIGOS QUE SOBRARAM e os que já estao como VOUTROS e os colocando na NOVA COLUNA ATO_INFRACIONAL2

banco_incidencia$ATO = grepl(pattern = "ART", x = banco_incidencia$ATO_INFRACIONAL) | grepl(pattern = "VOUTROS", x = banco_incidencia$ATO_INFRACIONAL)

#substituindo
banco_incidencia$ATO = ifelse(banco_incidencia$ATO == TRUE,
                              "VOUTROS", banco_incidencia$ATO_INFRACIONAL)

#substituindo
banco_incidencia$ATO = ifelse(banco_incidencia$ATO == "OUTROS",
                              "VOUTROS", banco_incidencia$ATO)
#########################################################################################################
banco_incidencia = arrange(banco_incidencia, ATO)

banco_incidencia$ATO_INFRACIONAL <- NULL
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
banco_incidencia_bkp = banco_incidencia

banco_incidencia_bkp =
  banco_incidencia_bkp %>%
  janitor::tabyl(ATO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
banco_incidencia_bkp$ATO[banco_incidencia_bkp$ATO == "VS/INF"]<- "SEM INFORMAÇÃO"
banco_incidencia_bkp$ATO[banco_incidencia_bkp$ATO == "VOUTROS"]<- "OUTROS"


colnames(banco_incidencia_bkp)[1]<-'banco_incidencia_bkp'
colnames(banco_incidencia_bkp)[2]<-'QUANTIDADE'
colnames(banco_incidencia_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#para script rmd:
banco_incidencia_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_incidencia_bkp$PERCENTUAL))
banco_incidencia_bkp_rmd = tail(banco_incidencia_bkp,3)
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_incidencia =
  banco_incidencia %>%
  janitor::tabyl(ATO) %>%
  arrange(ATO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
colnames(banco_incidencia)[1]<-'ATO'
# Adaptando:
banco_incidencia$ATO[banco_incidencia$ATO == "VS/INF"]<- "SEM INFORMAÇÃO"
banco_incidencia$ATO[banco_incidencia$ATO == "VOUTROS"]<- "OUTROS"



colnames(banco_incidencia)[1]<-'ATO'
colnames(banco_incidencia)[2]<-'QUANTIDADE'
colnames(banco_incidencia)[3]<-'PERCENTUAL'

#############################################################################################################

#banco_incidencia =
#  banco_incidencia %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))

setwd(file.path("~/diretorio_r/estciabh/planilhas"))
write.csv(banco_incidencia, file ="banco_incidencia_atual.csv",row.names=FALSE)
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# banco_incidencia FIM
#########################################################################################################
#########################################################################################################
#INCIDENCIA COMPARADA. obs: trazer arquivo ano anterior para a pasta.
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
banco_incidencia_atual = banco_incidencia
#########################################################################################################
#INCIDENCIA COMPARADA. obs: trazer arquivo ano anterior para a pasta.
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#MUDANDO DIRETORIO PARA BUSCAR O ARQUIVO
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

#Banco ano anterior
banco_incidencia_anterior <- read.csv("banco_incidencia_anterior.csv",header=TRUE, sep=",", dec=".", encoding = "UTF-8" )

#RETORNANDO PARA O DIRETÓRIO PADRÃO
setwd(file.path("~/diretorio_r/estciabh/planilhas"))

colnames(banco_incidencia_anterior) <- c("ATO", "QUANTIDADE")

banco_incidencia_anterior = banco_incidencia_anterior %>%
  select(ATO, QUANTIDADE)

#juntando tudo

incidencia_comparada = full_join(banco_incidencia_anterior,banco_incidencia_atual, by="ATO")
incidencia_comparada

#NA POR ZERO

incidencia_comparada = replace(x = incidencia_comparada, list = is.na(incidencia_comparada), values = 0)

incidencia_comparada$PERCENTUAL <- NULL


#renomeando colunas
colnames(incidencia_comparada) <- c("ATO", "ANOANTERIOR", "ANOATUAL")

incidencia_comparada$ATO[incidencia_comparada$ATO == "Total"]<- "TOTAL"

incidencia_comparada$VAR <- round(((incidencia_comparada$ANOATUAL*100)/incidencia_comparada$ANOANTERIOR)-100, 2)


colnames(incidencia_comparada) <- c("ATO", format(Sys.Date()-365*2, "%Y"), format(Sys.Date()-365*1, "%Y"), "VAR%")
#colnames(incidencia_comparada) <- c("ATO", format(Sys.Date()-365*1, "%Y"), format(Sys.Date()-365*0, "%Y"), "VAR%")
#########################################################################################################
# banco_incidencia FIM
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################