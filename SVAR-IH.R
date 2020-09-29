install.packages("vars")
install.packages("svars")

library("vars")
library("svars")
library("ggplot2")

?USA

ggplot2::autoplot(USA, facet = TRUE) + ggplot2::theme_bw()
reduced.form <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
structural.form <- id.ngml(reduced.form)
summary(structural.form)

structural.form$B <- structural.form$B[,c(3,2,1)]
structural.form$B[,3] <- structural.form$B[,3]*(-1)

impulse.response <- irf(structural.form, n.ahead = 30)
plot(impulse.response, scales = 'free_y')

cores <- parallel::detectCores() - 1
boot.svar <- wild.boot(structural.form, n.ahead = 30, nboot = 500, nc = cores)
plot(boot.svar)

fevd <- fevd(structural.form, n.ahead = 30)
plot(fevd)

hist.decomp <- hd(structural.form, series = 2)
plot(hist.decomp)
