# returnCoverage = function(dat, setup){
#   extractPostSig2F = function(x){
#     Del = x$twoF_output$del2.2F
#     Lamb = x$twoF_output$lambda.2F
#     d = dim(Del)
#     inds = upper.tri(matrix(NA, d[2], d[2]))
#     num_cors = sum(inds)
#     cor_val = matrix(NA, d[1], num_cors)
#     for (m in 1:d[1]){
#       cor_mat = cov2cor(crossprod(t(Lamb[m,,]))+diag(Del[m,]))
#       cor_val[m,] = cor_mat[inds]
#     }
#     return(cor_val)
#   }
# 
#   retrivePostSig2F = function(s){
#     path = getRDS(s)
#     result = readRDS(path)
#     Cors.2F = lapply(result, extractPostSig2F)
#     Cors.2F_array = abind::abind(Cors.2F, along = 3)
#     return(Cors.2F_array)
#   }
# 
#   Cors.2F = lapply(dat, extractPostSig2F)
#   Cors.2F_array = abind::abind(Cors.2F, along = 3)
#   dat.2F = data.frame("methods" = "2F",
#                       "setups" = rep(setup, each = 1008*15*4000),
#                       "Cors" = as.vector(Cors.2F_array),
#                       "run" = rep(rep(1:1008, each = 4000*15), 1),
#                       "iter" = rep(rep(rep(1:4000, 15), 1008), 1),
#                       "element" = rep(rep(rep(1:15, each = 4000), 1008), 1))
# 
#   return(dat.2F)
# }
# 
# dat1 = readRDS("../_data/_coverage-runs/_setup11/results.RDS")
# dat2 = readRDS("../_data/_coverage-runs/_setup14/results.RDS")
# dat1.2f = returnCoverage(dat1,11)
# dat2.2f = returnCoverage(dat2,14)
# dat1 = dat2 = NULL
# dat.2F = rbind(dat1.2f, dat2.2f)
# dat1.2f = dat2.2f = NULL
# 
# lower_CI = tapply(dat.2F$Cors,
#                   list(dat.2F$run,
#                        dat.2F$element,
#                        dat.2F$setups),
#                   function(x) quantile(x, probs = .025))
# upper_CI = tapply(dat.2F$Cors,
#                   list(dat.2F$run,
#                        dat.2F$element,
#                        dat.2F$setups),
#                   function(x) quantile(x, probs = .975))
# means = tapply(dat.2F$Cors,
#                list(dat.2F$run,
#                     dat.2F$element,
#                     dat.2F$setups),
#                mean)
# 
# dat.2F = NULL
# 
# 
# t.cor = readRDS("../_data/_ground-truths/correlations.RDS")
# extractTCors = function(x){
#   ind = upper.tri(x)
#   x[ind]
# }
# t.cor_array = mapply(extractTCors, t.cor)
# t.cor_array.1 = array(rep(as.vector(t.cor_array[,11]), each = 1008),
#                       dim = c(1008, 15, 1))
# t.cor_array.2 = array(rep(as.vector(t.cor_array[,14]), each = 1008),
#                       dim = c(1008, 15, 1))
# t.cor_array = abind::abind(t.cor_array.1, t.cor_array.2, along = 3)
# 
# result = t.cor_array >= lower_CI & t.cor_array <= upper_CI
# res = apply(result, 2:3, function(x) sum(x)/length(x))

plot2 = function(){
  par(mar = c(5, 6, 0, 1))
  plot(NA,
       NA,
       xlim = c(0,4.5),
       ylim = c(0,1010),
       axes = F,
       ylab = "",
       xlab = "")
  uu = upper_CI[,1,1]
  ll = lower_CI[,1,1]
  o = order(means[,1,1]) 
  ll = ll[o]
  uu = uu[o] 
  cols = c("indianred", "seagreen3")
  my_cols = cols[result[,1,1][o]+1]
  
  my_line = function(x0,x1,y0,y1,col){
    lines(x = c(x0,x1), y = c(y0,y1),col = col)
  }
  mapply(my_line, as.list(ll+1), as.list(uu+1), as.list(1:1008), as.list(1:1008), my_cols)
  abline(v = t.cor_array[1,1,1]+1, col = "black", lwd = 2, lty = 3)
  axis(1, at = seq(2.5, 4.5, length.out = 5), labels = c(-1,-.5, 0, .5, 1), lwd = 1.5, cex.axis = 1.5)
  axis(1, at = seq(0, 2, length.out = 5), labels = c(-1, -.5, 0, .5, 1), lwd = 1.5, cex.axis = 1.5)
  axis(2, at = c(1,1000), labels = c(1,1000), lwd = 1.5, las = 2, cex.axis = 1.5)
  # mtext(expression("Two-Factor Graded (" * D[2] *")"), side = 3, cex = 1.5, line = 1, adj = .05)
  # mtext(expression("Bi-Factor (F)"), side = 3, cex = 1.5, line = 1, adj = .825)
  mtext("Simulation Run", side = 2, cex = 1.5, line = 3.6, adj = .5)
  mtext("95% CIs for Selected Correlations", side = 1, cex = 1.5, line = 3, adj = .5)
  
  uu = upper_CI[,15,2]
  ll = lower_CI[,15,2]
  o = order(means[,15,2]) 
  ll = ll[o]
  uu = uu[o] 
  # cols = c("indianred", "lightseagreen")
  my_cols = cols[result[,15,2][o]+1]
  
  my_line = function(x0,x1,y0,y1,col){
    lines(x = c(x0,x1), y = c(y0,y1),col = col)
  }
  mapply(my_line, as.list(ll+3.5), as.list(uu+3.5), as.list(1:1008), as.list(1:1008), my_cols)
  abline(v = t.cor_array[1,15,2]+3.5, col = "black", lwd = 2, lty = 3)
}

results = read.csv("../_data/_coverage-results/coverage.csv")
tres1 = results[results$setup == 11,]
o = order(tres1$ground_truth)
tres1 = tres1[o,]
tres2 = results[results$setup == 14,]
o = order(tres2$ground_truth)
tres2 = tres2[o,]
cols = readRDS("_data/plot-cols.RDS")
col = cols$mods_cols[7]

plot1 = function(){
  par(mar = c(3, 6, 3, 1))
  plot(NA,
       NA,
       ylim = c(0,1),
       xlim = c(1,31),
       axes = F,
       ylab = "",
       xlab = "",
       cex.lab  = 1.2)
  axis(2, at = c(0,.2,.4,.6), labels =  c(0,.2,.4,.6), lwd = 1.5, las = 2, cex.axis = 1.5)
  axis(2, at = seq(.8, 1, .1), labels =  seq(.8, 1, .1), lwd = 1.5, las = 2, cex.axis = 1.5)
  
  points(1:15, tres1$ground_truth, col = "black", pch = 18, cex = 2)
  points(1:15, tres1$coverage, type = "l", lwd = 4, col = col)
  points(1:15, tres1$coverage, type = "p", lwd = 2, pch = 19, col = col)
  points(17:31, tres2$ground_truth, col = "black", pch = 18, cex = 2)
  points(17:31, tres2$coverage, type = "l", lwd = 3, col = col)
  points(17:31, tres2$coverage, type = "p", lwd = 2, pch = 19, col = col)
  # points(c(1, 31), c(tres1$coverage[1], tres2$coverage[15]), cex = 2, col = "gold", lwd = 3)
  # arrows(14, tres1$ground_truth[14], )
  shape::Arrows(x0 = 14, y0 = tres1$coverage[14]-.1,
                x1 = 14, y1 = tres1$ground_truth[14]+.125,
                lwd = 2, code = 3)
  shape::Arrows(x0 = 31, y0 = tres2$coverage[15]-.1,
                x1 = 31, y1 = tres2$ground_truth[15]+.125,
                lwd = 2, code = 3)
  
  
  axis(1, at = c(1,15), labels =  c(1,15), lwd = 1.5, cex.axis = 1.5)
  axis(1, at = c(17,31), labels =  c(1,15), lwd = 1.5, cex.axis = 1.5)
  mtext(expression("Two-Factor Graded (" * D[2] *")"), side = 3, cex = 1.5, line = .5, adj = .15)
  mtext(expression("Bi-Factor (F)"), side = 3, cex = 1.5, line = .5, adj = .8)
  mtext(expression("Pairwise Correlation Elements"), side = 1, cex = 1.2, line = .5, adj = .145)
  mtext(expression("Pairwise Correlation Elements"), side = 1, cex = 1.2, line = .5, adj = .86)
  mtext("Ground\n Truths", side = 2, cex = 1.5, line = 3.2, adj = .2)
  mtext("Coverage", side = 2, cex = 1.5, line = 3.6, adj = 1.2)
  lines(y=c(.95,.95), x=c(.5,15.5), lty = 3, lwd = 2)
  lines(y=c(.95,.95), x=c(16.5,31.5), lty = 3, lwd = 2)
}

pdf("../_manuscript/_figs/coverage.pdf",
    width = 10,
    height = 7)
par(mfrow = c(2,1))
plot1()
plot2()
dev.off()