# Setting Up Environment and Loading Scripts
folder_path = "../_logistics"
script_files = list.files(path = folder_path, 
                          pattern = "\\.R$", 
                          full.names = TRUE)
lapply(script_files, source)

# Loading Data
factors = readRDS("../_data/_ground-truths/factors.RDS")
t.covs = readRDS("../_data/_ground-truths/covariances.RDS")


# Setting Simulation Parameters
measurs = c("True Effect", "Usual", "Spearman", "Wishart",
            "1F", "1F+", "2F", "2F+")

I = 200
J = 6
L = 100

# Simulation Function
setupSim = function(X, setup, current_run){
  
  # Initial Setup
  print(current_run)
  set.seed(X)
  t.b = factors[[setup]]
  
  # Data Preparation
  tdat = makeEmptyDat(I, J, L)
  alphas = makeAlpha(I, J)
  thetas = makeTheta(I, J, t.b)
  dat = addRT(tdat, alphas$val, thetas$val)
  t.cor = thetas$rho
  cors_to_save = upper.tri(t.cor)
  
  # Initializing RMS Errors and Correlation Matrices
  rmses = 1:length(measurs)
  cors = matrix(NA, nrow = length(t.cor[cors_to_save]), ncol = length(measurs)+1)
  names(rmses) = measurs
  colnames(cors) = c("True Cor", measurs)
  
  # Calculating Correlations and RMSEs for Different Measures
  
  # True Cor
  cors[,1] = t.cor[cors_to_save]
  
  # True Effect
  effect.cor = cor(thetas$val)
  cors[,2] = effect.cor[cors_to_save]
  rmses[1] = RMSE(t.cor, effect.cor)
  
  # Usual
  usual = surfaceCor(dat)$effect.cor
  cors[,3] = usual[cors_to_save]
  rmses[2] = RMSE(t.cor, usual)
  
  # Spearman-disattenuated 
  spearman.dis = spearman(dat)
  cors[,4] = spearman.dis[cors_to_save]
  rmses[3] = RMSE(t.cor, spearman.dis)
  
  
  # Bayesian Models with JAGS
  data_list = list(
    "y" = dat$rt,
    "task" = dat$task,
    "sub" = dat$sub,
    "cond" = dat$cond,
    "I" = I,
    "J" = J,
    "diagJ" = diag(J),
    "N" = nrow(dat)
  )

  # Wishart 
  pars_wishart = c("pSig2", "theta")
  results.wishart = runJags(modJ.wishart, data_list, pars_wishart)
  thetas.wishart = results.wishart$BUGSoutput$sims.list$theta
  pSig2.wishart = results.wishart$BUGSoutput$sims.list$pSig2
  theta.cor.wishart = popThetaCors(thetas.wishart)$avg_cor
  cors[,5] = theta.cor.wishart[cors_to_save]
  rmses[4] = RMSE(t.cor, theta.cor.wishart)
  
  
  # 1F
  pars_F = c("Sig2", "lambda", "del2", "theta")
  results.1F = runJags(modJ.1F, data_list, pars_F)
  thetas.1F = results.1F$BUGSoutput$sims.list$theta
  Sig2.1F = results.1F$BUGSoutput$sims.list$Sig2
  lambda.1F = results.1F$BUGSoutput$sims.list$lambda
  del2.1F = results.1F$BUGSoutput$sims.list$del2
  theta.cor.1F = popThetaCors(thetas.1F)$avg_cor
  cors[,6] = theta.cor.1F[cors_to_save]
  rmses[5] = RMSE(t.cor, theta.cor.1F)
  
  # 1F+
  results.1Fplus = runJags(modJ.1Fplus, data_list, pars_F)
  thetas.1Fplus  = results.1Fplus$BUGSoutput$sims.list$theta
  Sig2.1Fplus  = results.1Fplus$BUGSoutput$sims.list$Sig2
  lambda.1Fplus  = results.1Fplus$BUGSoutput$sims.list$lambda
  del2.1Fplus  = results.1Fplus$BUGSoutput$sims.list$del2
  theta.cor.1Fplus  = popThetaCors(thetas.1Fplus)$avg_cor
  cors[,7] = theta.cor.1Fplus[cors_to_save]
  rmses[6] = RMSE(t.cor, theta.cor.1Fplus)
  
  # 2F 
  results.2F = runJags(modJ.2F, data_list, pars_F)
  thetas.2F = results.2F$BUGSoutput$sims.list$theta
  Sig2.2F = results.2F$BUGSoutput$sims.list$Sig2
  lambda.2F = results.2F$BUGSoutput$sims.list$lambda
  del2.2F = results.2F$BUGSoutput$sims.list$del2
  theta.cor.2F = popThetaCors(thetas.2F)$avg_cor
  cors[,8] = theta.cor.2F[cors_to_save]
  rmses[7] = RMSE(t.cor, theta.cor.2F)
  
  # 2F+
  results.2Fplus = runJags(modJ.2Fplus, data_list, pars_F)
  thetas.2Fplus  = results.2Fplus$BUGSoutput$sims.list$theta
  Sig2.2Fplus  = results.2Fplus$BUGSoutput$sims.list$Sig2
  lambda.2Fplus  = results.2Fplus$BUGSoutput$sims.list$lambda
  del2.2Fplus  = results.2Fplus$BUGSoutput$sims.list$del2
  theta.cor.2Fplus  = popThetaCors(thetas.2Fplus)$avg_cor
  cors[,9] = theta.cor.2Fplus[cors_to_save]
  rmses[8] = RMSE(t.cor, theta.cor.2Fplus)
  
  
  # Compiling Model Outputs
  factor_models_output = list(
    "pSig2.wishart" = pSig2.wishart,
    "lambda.1F" = lambda.1F,
    "lambda.1Fplus" = lambda.1Fplus,
    "lambda.2" = lambda.2F,
    "lambda.2Fplus" = lambda.2Fplus,
    "del2.1F" = del2.1F,
    "del2.1Fplus" = del2.1Fplus,
    "del2.2F" = del2.2F,
    "del2.2Fplus" = del2.2Fplus
  )
  
  out = list(
    "cors" = cors,
    "RMSEs" = rmses,
    "factor_models_output" = factor_models_output
  )
  
  return(out)
}

num_cores = 18
each_run = 108
runs = 1:num_cores
for (s in 1:length(factors)){
  # Recording Simulation Setup
  notes = paste0(
    "Setup ", s, 
    ":\nVariance-Covariance matrix for the simulation:\n", 
    paste(capture.output(print(t.covs[[s]])), collapse = "\n"), 
    "\nCorrelation matrix for the simulation:\n", 
    paste(capture.output(print(round(cov2cor(t.covs[[s]]), 2))), collapse = "\n")
  )
  dir = paste0("../_data/_sim-runs/_run",s,"/")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  dir2 = paste0("../_data/_sim-runs/_run",s,"/des.txt")
  writeLines(notes, dir2)
  
  # Random Seed Generation and Saving
  seed = sample.int(1e9, each_run)
  dir3 = paste0("../_data/_sim-runs/_run",s,"/seeds.RDS")
  saveRDS(seed, dir3)
  
  # Parallel Simulation Execution
  results = mclapply(1:each_run,
                     function(i) setupSim(seed[i],
                                          s,
                                          runs[i]),
                     mc.cores = num_cores)
  
  # Saving Results
  dir4 = paste0("../_data/_sim-runs/_run",s,"/results.RDS")
  saveRDS(results, dir4)
}

