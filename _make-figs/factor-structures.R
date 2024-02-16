library(lavaan)
library(semPlot)
plotSEMS = function(method = "tree2", szM = 13, szL = 10){
  
  shades = colorRampPalette(c("gray90", "gray1"))(20)
  
  plotSEM = function(setup, edge_cols, node_labels){
    dummy_data = matrix(rnorm(6*6), 100, 6)
    colnames(dummy_data) = c("t1", "t2", "t3", "t4", "t5", "t6")
    fit = lavaan(setup, 
                 data = dummy_data, 
                 auto.var = TRUE, 
                 auto.fix.first = FALSE, 
                 auto.cov.lv.x = FALSE)
    semPaths(fit, 
             whatLabels="no", 
             layout=method,
             as.expression = c("nodes"), 
             edge.label.cex = 0.8,
             sizeMan=szM, 
             sizeLat=szL,
             edge.color	= edge_cols,
             nodeLabels = node_labels)
  }
  setup1 = '
    # Measurement part
    G =~ t1 + t2 + t3 + t4 + t5 + t6
  
  
    # Residual variances for observed variables
    t1 ~~ e1*t1
    t2 ~~ e2*t2
    t3 ~~ e3*t3
    t4 ~~ e4*t4
    t5 ~~ e5*t5
    t6 ~~ e6*t6
  '
  setup2 = '
    # Measurement part
    S1 =~ t1 + t2 + t3 
    S2 =~ t4 + t5 + t6
  
  
    # Residual variances for observed variables
    t1 ~~ e1*t1
    t2 ~~ e2*t2
    t3 ~~ e3*t3
    t4 ~~ e4*t4
    t5 ~~ e5*t5
    t6 ~~ e6*t6
  '
  
  setup3 = '
    # Measurement part
    G1 =~ t1 + t2 + t3 + t4 + t5 + t6
    G2 =~ t1 + t2 + t3 + t4 + t5 + t6
  
  
    # Residual variances for observed variables
    t1 ~~ e1*t1
    t2 ~~ e2*t2
    t3 ~~ e3*t3
    t4 ~~ e4*t4
    t5 ~~ e5*t5
    t6 ~~ e6*t6
  '
  
  setup4 = '
    # Measurement part
    G1 =~ t1 + t2 + t3 + t4 + t5 + t6
    S1 =~ t1 + t2 + t3 
    S2 =~ t4 + t5 + t6
  
  
    # Residual variances for observed variables
    t1 ~~ e1*t1
    t2 ~~ e2*t2
    t3 ~~ e3*t3
    t4 ~~ e4*t4
    t5 ~~ e5*t5
    t6 ~~ e6*t6
  '
  
  setup5 = '
    # Measurement part
    G1 =~ t1 + t2 + t3 + t4 + t5 + t6
    S1 =~ t1 + t2 
    S2 =~ t3 + t4 
    S3 =~ t5 + t6
  
  
  
    # Residual variances for observed variables
    t1 ~~ e1*t1
    t2 ~~ e2*t2
    t3 ~~ e3*t3
    t4 ~~ e4*t4
    t5 ~~ e5*t5
    t6 ~~ e6*t6
  '
  par(mfrow=c(3,2))
  
  node_lab1 = c(expression(t[1]), expression(t[2]), expression(t[3]), 
                expression(t[4]), expression(t[5]), expression(t[6]), 
                expression(G))
  edge_cols1 = c(shades[rep(10,6)], rep("black",6), "white")
  edge_cols1.2 = c(shades[c(4,4,8,8,18,18)], rep("black",6), "white")
  plotSEM(setup1, edge_cols1, node_lab1)
  plotSEM(setup1, edge_cols1.2, node_lab1)
  
  node_lab2 = c(expression(t[1]), expression(t[2]), expression(t[3]), 
                expression(t[4]), expression(t[5]), expression(t[6]), 
                expression(S[1]), expression(S[2]))
  edge_cols2 = c(shades[rep(10,6)], rep("black",6), rep("white",2))
  plotSEM(setup2, edge_cols2, node_lab2)
  
  node_lab3 = c(expression(t[1]), expression(t[2]), expression(t[3]), 
                expression(t[4]), expression(t[5]), expression(t[6]), 
                expression(G[1]), expression(G[2]))
  edge_cols3 = c(shades[c(4,4,8,8,18,18)],
                 rev(shades[c(4,4,8,8,18,18)]),
                 rep("black",6), rep("white",2))
  plotSEM(setup3, edge_cols3, node_lab3)
  
  
  node_lab4 = c(expression(t[1]), expression(t[2]), expression(t[3]), 
                expression(t[4]), expression(t[5]), expression(t[6]), 
                expression(G), expression(S[1]), expression(S[2]))
  edge_cols4 = c(shades[rep(4,6)],
                 rev(shades[rep(10,6)]),
                 rep("black",6), rep("white",3))
  plotSEM(setup4, edge_cols4, node_lab4)
  
  node_lab5 = c(expression(t[1]), expression(t[2]), expression(t[3]), 
                expression(t[4]), expression(t[5]), expression(t[6]), 
                expression(G), expression(S[1]), expression(S[2]),
                expression(S[3]))
  edge_cols5 = c(shades[rep(4,6)],
                 rev(shades[rep(10,6)]),
                 rep("black",6), rep("white",4))
  plotSEM(setup5, edge_cols5, node_lab5)
  title(expression(A), outer = TRUE, line = -2,
        adj = 0.05, cex.main = 3)
  title(expression(B), outer = TRUE, line = -2,
        adj = 0.55, cex.main = 3)
  title(expression(C), outer = TRUE, line = -27,
        adj = 0.05, cex.main = 3)
  title(expression(D), outer = TRUE, line = -27,
        adj = 0.55, cex.main = 3)
  title(expression(E), outer = TRUE, line = -53,
        adj = 0.05, cex.main = 3)
  title(expression(F), outer = TRUE, line = -53,
        adj = 0.55, cex.main = 3)
}

pdf("../_manuscript/_figs/factor_tree.pdf",
    width = 8,
    height = 10)
plotSEMS(
  szM = 15,
  szL = 18)
dev.off()