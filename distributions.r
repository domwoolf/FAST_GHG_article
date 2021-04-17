
par(mar = c(5,5,5,5))

gasm = rriskDistributions::get.lnorm.par(q = c(0.02, 0.11, 0.33))
ef = rriskDistributions::get.lnorm.par(q = c(0.011, 0.014, 0.017))
monte = data.frame(gasm = rlnorm(10000, gasm[1], gasm[2]), ef   = rlnorm (10000, ef[1],   ef[2]))
monte$f = monte$ef*monte$gasm*44/28
p = seq(0.025, 0.975, 0.01)
rriskDistributions::get.lnorm.par(p = p, q = quantile(monte$f, probs=p))
#    meanlog      sdlog 
# -6.0312556  0.5856656 



ef = rriskDistributions::get.norm.par(q = c(0, 0.005, 0.011))
monte = data.frame(gasm = rlnorm(10000, gasm[1], gasm[2]), ef   = rnorm (10000, ef[1],   ef[2]))
monte$f = monte$ef*monte$gasm*44/28
monte = monte[monte$f > 0, ]
rriskDistributions::get.lnorm.par(p = p, q = quantile(monte$f, probs=p))



fracleach = rriskDistributions::get.lnorm.par(q = c(0.01, 0.24, 0.73))
ef = rriskDistributions::get.norm.par(q = c(0.0, 0.011, 0.02))
monte = data.frame(fracleach = rlnorm(10000, gasm[1], gasm[2]), ef   = rnorm (10000, ef[1],   ef[2]))
monte$f = monte$ef*monte$fracleach*44/28
p = seq(0.025, 0.975, 0.01)
rriskDistributions::get.lnorm.par(p = p, q = quantile(monte$f, probs=p))

ef = rriskDistributions::get.norm.par(q = 44/28*c(0.00001, 0.005, 0.011))

ef = rriskDistributions::get.norm.par(q = c(0.031, 0.054, 0.085))
