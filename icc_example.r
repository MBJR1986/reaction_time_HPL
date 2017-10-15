#icc example in r

library(irr)
data("anxiety")

# Just one of the many possible ICC coefficients
icc(anxiety, model="twoway", type="consistency", unit = "average")

r1 <- round(rnorm(20, 10, 4))
r2 <- round(r1 + 10 + rnorm(20, 0, 2))
r3 <- round(r1 + 20 + rnorm(20, 0, 2))
icc(cbind(r1, r2, r3), "twoway") # High consistency, ICC = .853, p-value = 0.00001 (F-test)
icc(cbind(r1, r2, r3), "twoway", "agreement") # Low agreement ICC = .144, p-value = 0.03?
