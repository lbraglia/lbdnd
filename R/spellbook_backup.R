#' Calcola il costo di un backup di spellbook in base alle magie trascritte
#' 
#' @param copied_spells vector with the number of trascribed
#' @examples
#' spellbook_backup(c(6,4,2)) # 6 lev1 spells, 4 lev2 spells, 2 lev3 spells
#' 
#' @export
spellbook_backup <- function(copied_spells = NULL, add_blank_spellbook = TRUE)
{
    if (!is.null(copied_spells)) {

        ## blank spellbook
        blank_spellbook_cost <- if (add_blank_spellbook) 50L else 0L

        ## spells copying
        levels <- seq_along(copied_spells)
        unit_cost <- levels * 10L
        
        transcription_costs <- data.frame(
            level = levels,
            n_of_spells = copied_spells,
            unit_cost = unit_cost,
            tot_cost = unit_cost * copied_spells)

        total_cost <- sum(transcription_costs$tot_cost) + blank_spellbook_cost
        
        list('Blank spellbook (MO)' = blank_spellbook_cost,
             'Transcription costs (MO)' = transcription_costs,
             'Total costs (MO)' = total_cost)
    }
    
}
