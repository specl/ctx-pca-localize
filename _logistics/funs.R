library(mvtnorm)
library(R2jags)
library(parallel)



makeEmptyDat = function(I, J, L){
  sub = rep(1:I, each = J*L*2)
  task = rep(rep(1:J, each = 2*L),I)
  cond = rep(rep(1:2, each = L), I*J) 
  trials = rep(rep(rep(1:L, J), 2), I)
  dat = data.frame("sub" = sub,
                   "task" = task,
                   "cond" = cond,
                   "trial" = trials)
  return(dat)
}

makeAlpha = function(I, J, t.cor = .5, sig = .2, nu = .8){
  Nu = rep(nu, J)
  P = matrix(t.cor, J, J) + diag(rep(1-t.cor),J)
  Sig = diag(rep(sig,J))
  Cov =  Sig%*%P%*%Sig
  alpha = rmvnorm(I, Nu, Cov)
  return(list(Nu = Nu, P = P, Sig = Sig, Cov = Cov, val = alpha))
}

makeTheta = function(I, J, t.b, t.c = 0.025, t.nu = 0.08){
  Nu = rep(t.nu, J)
  t.lamb = t.b
  t.b = apply(t.b, 1, sum)
  t.a = sqrt(t.c^2-t.b^2)
  t.delta = diag(t.a^2)
  Cov = crossprod(t(t.lamb))+t.delta
  t.cor = cov2cor(Cov)
  theta = rmvnorm(I, Nu, Cov)
  return(list(Nu = Nu, rho = t.cor, delta = t.delta, Cov = Cov, val = theta))
}


addRT = function(dat, t.Alpha, t.Theta, t.sig = .2){
  subtask = cbind(dat$sub, dat$task)
  mu = t.Alpha[subtask] + (dat$cond-3/2) * t.Theta[subtask]
  dat$rt = rnorm(length(mu), mu, t.sig)
  return(dat)
}

surfaceCor = function(dat){
  tdat = tapply(dat$rt, list(dat$sub, dat$task, dat$cond), mean)
  d = tdat[,,2] - tdat[,,1]
  ave = (tdat[,,2] + tdat[o0=
                            ,,1])/2
  return(list(ave.cor = cor(ave), effect.cor = cor(d)))  
}


spearman = function(dat){
  tdat = tapply(dat$rt, list(dat$sub, dat$task, dat$cond), mean)
  d = tdat[,,2] - tdat[,,1]
  se = tapply(dat$rt, 
              list(dat$sub, dat$task, dat$cond), 
              function(x) sd(x)/sqrt(length(x))
  )
  se2.diff = colMeans(se[,,2]^2 + se[,,1]^2)
  cov.diff = cov(d) - diag(se2.diff)
  s.cor = cov2cor(cov.diff)
  s.cor[s.cor>1] = 1
  s.cor[s.cor<(-1)] = -1
  return(s.cor)
}

runJags = function(mod, dat, pars, nchains = 2, niter = 3000, nburnin = 1000){
  result = jags(model.file = textConnection(mod),
                data = dat,
                n.chains = nchains,
                n.iter = niter,
                n.burnin = nburnin,
                n.thin = 1,
                parameters.to.save = pars) 
  return(result)
}

myMean = function(x){
  mean(x, na.rm = T)
}

popThetaCors = function(theta){
  M = dim(theta)[1]
  J = dim(theta)[3]
  theta_cors = array(NA, dim = c(nrow(theta), J, J))
  for (m in 1:M){
    theta_cors[m,,] = cor(theta[m,,])
  }
  mean_theta_cors = apply(theta_cors, 2:3, myMean)
  return(list("avg_cor" = mean_theta_cors, "post_cor" = theta_cors))
}

popSigCors = function(Sig, p = F){
  M = dim(Sig)[1]
  J = dim(Sig)[3]
  corVal = array(dim=c(M,J,J))
  for (m in 1:M){
    Cov = Sig[m,,]
    if (p == T){
      Cov = solve(Cov)
    }
    corVal[m,,]=cov2cor(Cov)
  }
  return(list("avg_cor" = apply(corVal,c(2,3),mean), "post_cor" = corVal))
}

RMSE = function(true, comp){
  sqrt(sum((comp[upper.tri(comp)] - true[upper.tri(true)])^2)/15)
}
