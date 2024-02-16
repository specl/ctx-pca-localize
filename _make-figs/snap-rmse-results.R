makePlot = function(results, my_colours){
  # RMSEs = results$rmse
  # runs = c(1,5,2,
  #          14,10,15,
  #          13,6,4,
  #          16, 7, 17,
  #          11,12
  # )
  # mods = c(2,3,4,8)
  # RMSEs = abind::abind(RMSEs, along = 3)
  ind = which(results$methods %in% c("Usual", "Spearman", "Wishart", "2F+"))
  results = results[ind,]
  results$methods = factor(results$methods, 
                           levels = c("Usual", "Spearman", "Wishart", "2F+"))
  RMSEs = tapply(results$RMSE, 
                 list(results$run, results$methods, results$setups),
                 mean)
  
  
  cols = my_colours$mods_cols[c(2,3,4,8)]
  
  nmods = ncol(RMSEs)
  RMSEs.mean = apply(RMSEs,c(3,2),median)
  RMSEs.sd = apply(RMSEs,c(3,2),sd)
  
  setup = list(
    "setup5" = 13:14,
    "setup4" = 10:12,
    "setup3" = 7:9,
    "setup2" = 4:6,
    "setup1" = 1:3
  )
  
  mods = list(
    "setup5" = 1:4,
    "setup4" = 1:4,
    "setup3" = 1:4,
    "setup2" = 1:4,
    "setup1" = 1:4
  )
  
  
  jits = seq(-.15, .15, length.out = 4)
  plot(NA, NA,
       xlim = c(1,14),
       ylim = c(0,.45),
       axes = F,
       xlab = "Ground Truths",
       ylab = "RMSE",
       cex.lab = 2)
  axis(2, 
       at = seq(0,.4, length.out = 5), 
       labels = seq(0,.4, length.out = 5), 
       cex.axis = 1.5, las = 2)
  
  
  myIntervals = function(rmse.mean,rmse.sd,x_ind,jits,col){
    arrows(x0 = x_ind+jits,
           x1 = x_ind+jits,
           y0 = rmse.mean - rmse.sd,
           y1 = rmse.mean + rmse.sd,
           code = 3, angle = 90, length = 0.05, xpd = TRUE,
           col = col)
  }
  pchs = c(16, 15, 17, 18)
  ltys = rev(1:4)
  myLines = function(which_mod,which_setup,jit_ind){
    which_setups = setup[[which_setup]]
    lines(x = which_setups+jit_ind, 
          y = RMSEs.mean[which_setups,which_mod],
          type = "b",
          col = cols[which_mod],
          lty = ltys[which_mod],
          lwd = 4,
          pch = pchs[which_mod],
          cex = 1.6)
    mapply(myIntervals, 
           RMSEs.mean[which_setups,which_mod],
           RMSEs.sd[which_setups,which_mod],
           which_setups,
           jit_ind,
           cols[which_mod])
  }
  
  
  
  for (i in 1:5){
    mapply(myLines,
           1:4,
           i,
           jits)
  }
  labs = c(expression(A[1]), expression(A[2]), expression(A[3]),
           expression(B[1]), expression(B[2]), expression(B[3]),
           expression(C[1]), expression(C[2]), expression(C[3]),
           expression(D[1]), expression(D[2]), expression(D[3]),
           expression(E[ ]), expression("F"[ ]))
  my_axis = function(x, schema){
    axis(1, at = x[1]:x[length(x)], labels = labs[x], cex.axis = 1.5)
    }
  mapply(my_axis,
         setup,
         rev(1:5))
  mods_names = c("Usual", "Spearman", "Wishart", "2F+")
  legend(x = 6, 
         y = .45, 
         legend = mods_names,  
         lty = ltys,
         pch = pchs,
         cex = 1.6,
         col = cols,
         bg = my_colours$leg_col,
         trace = FALSE)
}



results = read.csv("../_data/_sim-results/RMSEs.csv")
cols = readRDS("_data/plot-cols.RDS")
pdf("../_manuscript/_figs/rmse-snap.pdf",
    width = 10,
    height = 7)
par(mar = c(5, 5, 2, 2))
makePlot(results, cols)
dev.off()