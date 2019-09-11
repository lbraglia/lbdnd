#' Effettua le divisioni di monete di varia tipologia
#' 
#' @param MO monete oro
#' @param MA monete argento
#' @param MR monete rame
#' @param MP monete platino
#' @param ME monete elettro
#' @param n numero di PG per i quale fare la ripartizione
#' @export
dividi_loot <- function(MO = 0, MA = 0, MR = 0, MP = 0, ME = 0, n = 6) {
    tot_in_MO <- (MP * 10) + MO + (ME / 2) + (MA / 10) + (MR / 100)
    c('pg' = n,
      "MO a testa" = floor(tot_in_MO / n),
      "resto in MO" = tot_in_MO %% n)
}
