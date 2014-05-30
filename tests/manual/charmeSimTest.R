require(CHARME)

spec = charmeSpec()

sim = charmeSim(n = 2000)
plot(sim, type = "l")

x = as.numeric(sim)

par(mfrow = c(2,1), mar = c(2,2,2,2))

est = charmeSimpleEst(x,2)

plot(sim)
plot(est, rcol = 2:3)

sim@spec@trans
est@spec@trans
