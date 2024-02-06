dat = read.csv("../_data/_sim-results/RMSEs.csv")
r1 = tapply(dat$RMSE, list(dat$setups, dat$methods), mean)
r2 = apply(r1, 2, mean)
out = rbind(round(r1, 4), round(r2, 4))
out = out[,c(6,7,5,8,1,2,3,4)]
rownames(out) = c("A1", "A2", "A3",
                  "B1", "B2", "B3",
                  "C1", "C2", "C3",
                  "D1", "D2", "D3",
                  "E", "F", "Average")
write.csv(out, "../_data/_sim-results/performance.csv")
