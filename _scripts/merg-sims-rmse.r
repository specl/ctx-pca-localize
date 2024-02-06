getRDS = function(s){paste0("../_data/_sim-runs/_run",s,"/results.RDS")}
setups = as.list(1:14)
runs = as.list(1:108)
extractRMSEs = function(x){x$RMSEs}
retriveRMSES = function(s){
  path = getRDS(s)
  result = readRDS(path)
  res = mapply(extractRMSEs, result)
  return(t(res))
}
RMSEs = lapply(setups, retriveRMSES)
array_RMSEs = abind::abind(RMSEs, along = 3)
path = getRDS(1)
result = readRDS(path)
res = mapply(extractRMSEs, result)
met_names = rownames(res)
dat = data.frame("methods" = rep(rep(met_names, each = 108), 14),
                 "setups" = rep(1:14, each = 108*8),
                 "RMSE" = as.vector(array_RMSEs),
                 "run" = rep(rep(1:108,  8),  14))
dat$methods = factor(dat$methods, levels = met_names)

write.csv(dat, "../_data/_sim-results/RMSEs.csv")