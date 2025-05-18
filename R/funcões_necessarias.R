#########################################################################################################
funcao_tabela = function(x, y) {

  x =
   tabyl(y) |>
   arrange(y) |>
   janitor::adorn_totals() |>
    adorn_pct_formatting(digits = 2)

  #colnames(x) = c("y", "QUANTIDADE", "PERCENTUAL")
  return(x)
}


funcao_tabela(banco_incidencia, banco_incidencia$ATO_INFRACIONAL)
#########################################################################################################

funcao_grafico = function(x,y) {

   x =
    tabyl(y) |>
    arrange(n) |>
    adorn_pct_formatting(digits = 2)

  return(x)

}

funcao_grafico(banco_incidencia, banco_incidencia$ATO_INFRACIONAL)
#########################################################################################################

funcao_rmd = function(x){

  x$PERCENTUAL2 = as.numeric(as.numeric(gsub("%", "", x$percent)))

  x= tail(x,3)

  return(x)
}

#########################################################################################################

TESTE = funcao_grafico(banco_incidencia, banco_incidencia$ATO_INFRACIONAL)

funcao_rmd(TESTE)


banco_incidencia_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_incidencia_bkp$PERCENTUAL))
banco_incidencia_bkp_rmd = tail(banco_incidencia_bkp,3)







