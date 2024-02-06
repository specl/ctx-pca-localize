

extractPostSig2F = function(x){
  Del = x$twoF_output$del2.2F
  Lamb = x$twoF_output$lambda.2F
  d = dim(Del)
  inds = upper.tri(matrix(NA, d[2], d[2]))
  num_cors = sum(inds)
  cor_val = matrix(NA, d[1], num_cors)
  for (m in 1:d[1]){
    cor_mat = cov2cor(crossprod(t(Lamb[m,,]))+diag(Del[m,]))
    cor_val[m,] = cor_mat[inds]
  }
  return(cor_val)
}

retrivePostSig2F = function(s){
  path = getRDS(s)
  result = readRDS(path)
  Cors.2F = lapply(result, extractPostSig2F)
  Cors.2F_array = abind::abind(Cors.2F, along = 3)
  return(Cors.2F_array)
}

dat = readRDS("../_data/_coverage-runs/_setup11/results.RDS")
Cors.2F = lapply(dat, extractPostSig2F)
Cors.2F_array = abind::abind(Cors.2F, along = 3)
dat.2F.11 = data.frame("methods" = "2F",
                    "setups" = rep(11, each = 1008*15*4000),
                    "Cors" = as.vector(Cors.2F_array),
                    "run" = rep(rep(1:1008, each = 4000*15), 1),
                    "iter" = rep(rep(rep(1:4000, 15), 1008), 1),
                    "element" = rep(rep(rep(1:15, each = 4000), 1008), 1))

dat = readRDS("../_data/_coverage-runs/_setup14/results.RDS")
Cors.2F = lapply(dat, extractPostSig2F)
Cors.2F_array = abind::abind(Cors.2F, along = 3)
dat.2F.14 = data.frame("methods" = "2F",
                    "setups" = rep(14, each = 1008*15*4000),
                    "Cors" = as.vector(Cors.2F_array),
                    "run" = rep(rep(1:1008, each = 4000*15), 1),
                    "iter" = rep(rep(rep(1:4000, 15), 1008), 1),
                    "element" = rep(rep(rep(1:15, each = 4000), 1008), 1))


dat.2F = rbind(dat.2F.11, dat.2F.14)

lower_CI = tapply(dat.2F$Cors, 
                  list(dat.2F$run, 
                       dat.2F$element, 
                       dat.2F$setups), 
                  function(x) quantile(x, probs = .025))
upper_CI = tapply(dat.2F$Cors, 
                  list(dat.2F$run, 
                       dat.2F$element, 
                       dat.2F$setups), 
                  function(x) quantile(x, probs = .975))
post_mean = tapply(dat.2F$Cors, 
                   list(dat.2F$run, 
                        dat.2F$element, 
                        dat.2F$setups), 
                   mean)

t.cor = readRDS("../_data/_ground-truths/correlations.RDS")
extractTCors = function(x){
  ind = upper.tri(x)
  x[ind]
}
t.cor_mat = mapply(extractTCors, t.cor)[,c(11,14)]
t.cor_array.11 = array(rep(as.vector(t.cor_mat[,1]), each = 1008), 
                       dim = c(1008, 15, 1))
t.cor_array.14 = array(rep(as.vector(t.cor_mat[,2]), each = 1008), 
                       dim = c(1008, 15, 1))
t.cor_array = abind::abind(t.cor_array.11, t.cor_array.14, along = 3)


result = t.cor_array >= lower_CI & t.cor_array <= upper_CI
res = apply(result, 2:3, function(x) sum(x)/length(x))

post_mean_mean = apply(post_mean, 2:3, mean)
lower_CI_mean = apply(lower_CI, 2:3, mean)
upper_CI_mean = apply(upper_CI, 2:3, mean)


out = data.frame(
  "coverage" = as.vector(res),
  "setup" = rep(c(11,14), each = 15),
  "elements" = rep(1:15, 2),
  "ground_truth" = as.vector(t.cor_mat),
  "lower_CI_mean" = as.vector(lower_CI_mean),
  "upper_CI_mean" = as.vector(upper_CI_mean),
  "post_mean_mean" = as.vector(post_mean_mean)
)

write.csv(out, "../_data/_coverage-results/coverage.csv")
