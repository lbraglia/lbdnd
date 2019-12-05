#' Calcola il costo di un backup di spellbook in base alle magie trascritte
#' 
#' @param n_full_cost vector with the number of trascribed spells
#'     which are payed at full cost (eg if evocator, all but evocation
#'     spells)
#' @param n_half_cost vector with the number of trascribed spells
#'     which are payed at half cost (eg if evocator, only evocation
#'     spells)
#' @examples
#' # Aelar 5th level
#' spellbook_backup(c(9,4,2), c(3,0,2))
#' 
#' @export
spellbook_backup <- function(n_full_cost = NULL,
                             n_half_cost = NULL,
                             add_blank_spellbook = TRUE)
{
    if (! (is.null(n_full_cost) && is.null(n_half_cost)) ) {
        if(length(n_full_cost) != length(n_half_cost))
            stop("n_full_cost and n_half_cost must be of the same length")
           
        ## blank spellbook
        blank_spellbook_cost <- if (add_blank_spellbook) 50L else 0L

        ## spells copying
        levels <- seq_len(max(length(n_full_cost), length(n_half_cost)))
        ## cost for a single spell backup
        full_cost <- levels * 10L
        half_cost <- full_cost * 0.5

        ## full-cost-spells transcription
        if (!is.null(n_full_cost)) {
            transcr_full_cost_spells <- data.frame(
                level = levels,
                n_of_full_cost_spells = n_full_cost,
                unit_cost = full_cost,
                tot_cost = full_cost * n_full_cost)
            tot_transcr_full_cost_spells <-
                sum(transcr_full_cost_spells$tot_cost)
        } else {
            tot_transcr_full_cost_spells <- 0L
        }

        ## half-cost-spells transcription
        if (!is.null(n_half_cost)) {
            transcr_half_cost_spells <- data.frame(
                level = levels,
                n_of_half_cost_spells = n_half_cost,
                unit_cost = half_cost,
                tot_cost = half_cost * n_half_cost)
            tot_transcr_half_cost_spells <-
                sum(transcr_half_cost_spells$tot_cost)
        } else {
            tot_transcr_half_cost_spells <- 0L
        }

        ## total cost of backup
        total_cost <-
            blank_spellbook_cost +
            tot_transcr_full_cost_spells +
            tot_transcr_half_cost_spells
        
        list('Blank spellbook (MO)' = blank_spellbook_cost,
             'Full cost spells transcription (MO)' = transcr_full_cost_spells,
             'Half cost spells transcription  (MO)' = transcr_half_cost_spells,
             'Total costs (MO)' = total_cost)
    }
    
}
