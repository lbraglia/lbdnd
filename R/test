library(lbstat)

d4 <- 1:4
d6 <- 1:6
d8 <- 1:8
d10 <- 1:10
d12 <- 1:12
d20 <- 1:20
d100 <- 1:100

dice_df <- function(...) {
    dices <- list(...)
    expand.grid(dices)
}

## distribuzione di 3d6
l3d6 <- dice_df(d6, d6, d6)
l3d6$sum <- apply(l3d6, 1, sum)

plot_distr <- function(x, rel_freq = TRUE, ...) {
    if (rel_freq) barplot( table(x) / sum(table(x)), ...)
    else barplot(table(x), ...)
}
plot_distr(l3d6$sum, las = 1)

## -------------------------------------------------
## tiri con svantaggio, normali, vantaggio
## -------------------------------------------------
l2d20 <- dice_df(d20, d20)

advantage <- function(x, y){
    ifelse(x > y, x, y)
}

disadvantage <- function(x, y){
    ifelse(x < y, x, y)
}

l2d20$advantage <- advantage(l2d20[, 1], l2d20[, 2])
l2d20$disadvantage <- disadvantage(l2d20[, 1], l2d20[, 2])
sd(d20)
sd(l2d20$advantage)
sd(l2d20$disadvantage)
## quindi con vantaggio si guadagnano mediamente 3.325 punti e si
## ha meno variabilità
mean(l2d20$advantage) - mean(d20)

ylab <- 'Prob'
ylim <- c(0, 0.1)
las <- 1
par(mfrow = c(3,1))
plot_distr(d20, las = las, main = 'd20', ylab = ylab, ylim = ylim)
## abline(v = mean(d20), col = 'red')
plot_distr(l2d20$advantage, las = las,
           main = 'd20 con vantaggio', ylab = ylab, ylim = ylim)
## abline(v = mean(l2d20$advantage), col = 'red')
plot_distr(l2d20$disadvantage, las = las,
           main = 'd20 con svantaggio', ylab = ylab, ylim = ylim)
## abline(v = mean(l2d20$disadvantage), col = 'red')

sprintf("%.3f", mean(l2d20$disadvantage))
sprintf("%.3f", mean(l2d20$advantage))

t(t(Table(l2d20$disadvantage)))


## -------------------------------------------------
## 1d12 vs 2d6
## -------------------------------------------------

l2d6 <- dice_df(d6, d6)
d6sum <- apply(l2d6, 1, sum)
d122 <- factor(d12)

ylim = c(0,0.20)
par(mfrow = c(2,1))
plot_distr(factor(d6sum, levels = 1:12), ylab = ylab, ylim = ylim, las = las, main = '2d6')
plot_distr(d122, las = las, ylim = ylim, ylab = ylab, main = '1d12')

## differenza nei valori attesi non è chissa cosa in termini di gioco,
## quindi 2d6 per i giocatori avversi al rischio se no il d12 da il
## brivido della maggior variabilita
mean(d6sum)
sd(d6sum)
mean(d12)
sd(d12)
