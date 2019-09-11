#' Funzione che trasforma un mese numerico nel nome in CV
#'
#' Funzione che trasforma un mese numerico nel nome in CV
#' 
#' @param x number of month
#' @export
month_to_cv <- function(x){ 
    mesi   <- c("Hammer",                   #  1
                "Alturiak",                 #  2
                "Ches",                     #  3
                "Tarsakh",                  #  4
                "Mirtul",                   #  5
                "Kythorn",                  #  6
                "Flamerule",                #  7
                "Eleasis",                  #  8
                "Eleint",                   #  9
                "Marpenoth",                # 10
                "Uktar",                    # 11
                "Nightal")                  # 12
    mesi[x]
}
