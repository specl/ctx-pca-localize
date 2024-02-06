folder_path = "../_logistics"
script_files = list.files(path = folder_path, 
                          pattern = "\\.R$", 
                          full.names = TRUE)
lapply(script_files, source)
set.seed(42)

JAGS = F


I = 200
J = 6
L = 100
t.b = matrix(c(rep(.0176778,6),rep(0,6)),6,2)
thetas = makeTheta(t.b = t.b, I = I, J = J)
t.cor = round(thetas$rho,4)
c1 = t.cor[upper.tri(t.cor)]
alphas = makeAlpha(I, J)
tdat = makeEmptyDat(I, J, L)
dat = addRT(tdat, alphas$val, thetas$val)
pearson = surfaceCor(dat)
pearson.effect = pearson$effect.cor
c2 = pearson.effect[upper.tri(pearson.effect)]
c2.RMSE = RMSE(t.cor, pearson.effect)
spearman.effect = spearman(dat)
c3 = spearman.effect[upper.tri(spearman.effect)]
c3.RMSE = RMSE(t.cor, spearman.effect)




if (JAGS == F){
  if (!file.exists("_data/jags_setupProb_mod.RDS")) {
    data_list = list(
      "y" = dat$rt,
      "N" = nrow(dat),
      "I" = max(dat$sub),
      "J" = max(dat$task),
      "cond" = dat$cond,
      "task" = dat$task,
      "sub" = dat$sub,
      "tuneA" = 1,
      "tuneT" = .025,
      "diagJ" = diag(max(dat$task))
    )
    pars = c("pDeltaT2", "theta")
    iw_jags_res = runJags(modIWJags, data_list, pars)
    saveRDS(iw_jags_res, "_data/jags_setupProb_mod.RDS")
  } else {
    iw_jags_res = readRDS("_data/jags_setupProb_mod.RDS")
  }
  theta = iw_jags_res$BUGSoutput$sims.list$theta
  mod.jags.cor = popThetaCors(theta)$avg_cor
  c4 = mod.jags.cor[upper.tri(mod.jags.cor)]
  c4.RMSE = RMSE(t.cor, mod.jags.cor)
} else {
  if (!file.exists("_data/ROO_setupProb_mod.RDS")) {
    ROO_iw_res = genModWish2(dat)
    saveRDS(ROO_iw_res, "_data/ROO_setupProb_mod.RDS")
  } else {
    ROO_iw_res = readRDS("_data/ROO_setupProb_mod.RDS")
  }
  theta = ROO_iw_res$theta
  mod.ROO.cor = popThetaCors(theta)$avg_cor
  c4 = mod.ROO.cor[upper.tri(mod.ROO.cor)]
  c4.RMSE = RMSE(t.cor, mod.ROO.cor)
}

if (!file.exists("_data/twoFplus_setupProb.RDS")) {
  data_list = list(
    "y" = dat$rt,
    "N" = nrow(dat),
    "I" = max(dat$sub),
    "J" = max(dat$task),
    "K" = 2,
    "cond" = dat$cond,
    "task" = dat$task,
    "sub" = dat$sub,
    "tuneA" = 1,
    "tuneT" = .025,
    "diagJ" = diag(max(dat$task))
  )
  pars = c("pDelTheta2", "theta")
  twoFplus = runJags(mod2Fplus, data_list, pars)
  saveRDS(twoFplus, "_data/twoFplus_setupProb.RDS")
} else {
  twoFplus = readRDS("_data/twoFplus_setupProb.RDS")
}

theta = twoFplus$BUGSoutput$sims.list$theta
mod.jags.cor = popThetaCors(theta)$avg_cor
c5 = mod.jags.cor[upper.tri(mod.jags.cor)]
c5.RMSE = RMSE(t.cor, mod.jags.cor)


setupPlot1 = function(){
  set.seed(123)
  plot_cols = readRDS("_data/plot-cols.RDS")
  shades = unlist(plot_cols$mods_cols)
  plot(NA,NA,
       xlim = c(.9,5.5),
       ylim = c(0,1),
       axes = F,
       xlab = "",
       ylab = "Correlation Coefficents",
       cex.lab = 1.5)
  jit = rnorm(15, sd = .02)
  points(rep(1,15)+jit,c1,col = "black", pch = 19)
  points(rep(2,15)+jit,c2,col = shades[2], pch = 19)
  points(rep(3,15)+jit,c3,col = shades[3], pch = 19)
  points(rep(4,15)+jit,c4,col = shades[4], pch = 19)
  points(rep(5,15)+jit,c5,col = shades[8], pch = 19)
  lines(x = c(.8,5.3), y = rep(.5,2),lty=2)
  axis(2, cex.axis = 1.5)
  axis(1, at = 1:5, labels = F)
  text(x = c(1:5)+.25, 
       y = -.1, 
       labels = c("Truth", "Usual", "Spearman", "Unconstrained", "Constrained"), 
       srt = 45, 
       adj = 1, 
       xpd = TRUE, 
       cex = 1.3)
  text(2.4, .25, round(c2.RMSE,2), cex = 1.3)
  text(3.4, .55, round(c3.RMSE,2), cex = 1.3)
  text(4.4, .55, round(c4.RMSE,2), cex = 1.3)
  text(5.4, .55, round(c5.RMSE,2), cex = 1.3)
}



setupPlot2 = function(){
  plot_cols = readRDS("_data/plot-cols.RDS")
  post_cors = popThetaCors(theta)$post_cor
  shades = unlist(plot_cols$mods_cols)
  original_color = shades[4]
  red_component = strtoi(substr(original_color, 2, 3), base = 16) / 255
  green_component = strtoi(substr(original_color, 4, 5), base = 16) / 255
  blue_component = strtoi(substr(original_color, 6, 7), base = 16) / 255
  
  
  translucent_color <- rgb(red_component, green_component, blue_component, .8)
  hist(post_cors[,2,4], 
       border  = shades[4], 
       col = translucent_color,
       lwd = 4, 
       prob = TRUE, 
       axes = F, 
       breaks = 40,
       ylim = c(0,10),
       xlim = c(0,1.1),
       ylab = "",
       main = "",
       xlab = "Correlation Coefficient",
       cex.lab = 1.5)
  axis(1, at = seq(0,1,length.out=5), labels = seq(0,1,length.out=5), cex.axis = 1.3)
  scores = tapply(dat$rt, list(dat$sub,dat$task,dat$cond),mean)
  samp_effect = scores[,c(2,4),2] - scores[,c(2,4),1]
  sample.cor = cor.test(samp_effect[,1], samp_effect[,2])
  arrows(x0 = sample.cor$conf.int[1], y0 = 8,
         x1 = sample.cor$conf.int[2], y1 = 8, 
         angle = 90, code = 3, col = shades[2],
         length = .08, lwd = 2)
  points(sample.cor$estimate,8,cex = 2, pch = 19, col = shades[2])
  lines(rep(.5,2), c(0,10), lty = 1, col = "black", lwd = 3)
  lower = quantile(x = post_cors[,2,4], p=.025, na.rm = T)
  upper = quantile(x = post_cors[,2,4], p=.975, na.rm = T)
  lines(rep(lower,2), c(0,10), col = shades[4], lty = 3, lwd = 3)
  lines(rep(upper,2), c(0,10), col = shades[4], lty = 3, lwd = 3)
  legend("topright", 
         fill = c(shades[2], shades[4], "black"), 
         legend = c("Usual", "Unconstrained", "Truth"),
         cex = 1.2,
         bg = plot_cols$leg_col)
}

png("../_manuscript/_figs/prob_setup1.png", 
    width = 800, 
    height = 800,
    res = 150)  
setupPlot1()
dev.off()  
png("../_manuscript/_figs/prob_setup2.png", 
    width = 800, 
    height = 800, 
    res = 150)  
setupPlot2()
dev.off() 

png("../_manuscript/_figs/prob_setupAll.png",
    width = 1600,
    height = 800,
    res = 150)
par(mfrow=c(1,2))
par(mar = c(7, 5, 1, 2))
setupPlot1()
par(mar = c(7, 5, 4, 2))
setupPlot2()
title("A", outer = TRUE, line = -3,
      adj = 0.1, cex.main = 2)
title("B", outer = TRUE, line = -3,
      adj = 0.6, cex.main = 2)
dev.off()



