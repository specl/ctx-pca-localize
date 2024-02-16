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
I = 200
J = 6
L = 100
pars_F = c("Sig2", "lambda", "del2", "theta")

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
  t.Cov = thetas$Cov
  
  
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
  
  
  # 2F 
  results.2F = runJags(modJ.2F, data_list, pars_F)
  lambda.2F = results.2F$BUGSoutput$sims.list$lambda
  del2.2F = results.2F$BUGSoutput$sims.list$del2
  
  # Compiling Outputs
  
  twoF_output = list(
    "lambda.2F" = lambda.2F,
    "del2.2F" = del2.2F
  )
  
  out = list(
    "twoF_output" = twoF_output,
    "setup" = setup
  )
  
  return(out)
}



num_cores = 18
each_run = 1008
runs = 1:num_cores
setups_sim = c(11, 14) 


for (s in setups_sim){
  # Recording Simulation Setup
  notes = paste0(
    "Setup ", s, 
    ":\nVariance-Covariance matrix for the simulation:\n", 
    paste(capture.output(print(t.covs[[s]])), collapse = "\n"), 
    "\nCorrelation matrix for the simulation:\n", 
    paste(capture.output(print(round(cov2cor(t.covs[[s]]), 2))), collapse = "\n")
  )
  dir = paste0("../_data/_coverage-runs/_setup",s,"/")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  dir2 = paste0("../_data/_coverage-runs/_setup",s,"/des.txt")
  writeLines(notes, dir2)
  
  # Random Seed Generation and Saving
  seed = sample.int(1e9, each_run)
  dir3 = paste0("../_data/_coverage-runs/_setup",s,"/seeds.RDS")
  saveRDS(seed, dir3)
  
  # Parallel Simulation Execution
  results = mclapply(1:each_run,
                     function(i) setupSim(seed[i],
                                          s,
                                          runs[i]),
                     mc.cores = num_cores)
  
  # Saving Results
  dir4 = paste0("../_data/_coverage-runs/_setup",s,"/results.RDS")
  saveRDS(results, dir4)
}

