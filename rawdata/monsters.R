library(jsonlite)
monsters <- fromJSON("open5e-api/data/WOTC_5e_SRD_v5.1/monsters.json",
                     simplifyVector = FALSE)
names(monsters) <-  unlist(lapply(monster, function(x) x[['name']]))
monsters_df <- fromJSON("open5e-api/data/WOTC_5e_SRD_v5.1/monsters.json",
                        simplifyVector = FALSE)

## Monster by name extractor: returns a list
monster <- function(x){
    monsters[x]
}





## list of class 

## print ASCII
monster('Aboleth')

## print Latex
toLatex(monster('Aboleth'))
