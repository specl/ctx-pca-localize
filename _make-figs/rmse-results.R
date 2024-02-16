plotRMSE = function(temp, x_labs = T, y_labs = T, y_upper_lim = .3, y_title = -.05, adj_cex = 2){
  if (x_labs == T){
    par(mar = c(9, 5, 1, 2))
    if (adj_cex != 2){
      par(mar = c(6, 5, 1, 2))
    }
  } else{
    par(mar = c(2, 5, 2, 2))
  } 
  if (y_labs == T){
    ylab = "RMSE"
  } else{
    ylab = ""
  }
  my_colours = readRDS("_data/plot-cols.RDS")
  my_colours = my_colours$mods_cols
  boxplot(temp$value~temp$model,
          axes = F,
          xlab = "",
          ylab = ylab,
          col = my_colours,
          ylim = c(0, y_upper_lim),
          cex.lab = adj_cex)
  axis(1, at = 1:8, labels = F)
  if (x_labs == T){
    text(x = (1:8)+.3, y = y_title, labels = unique(temp$model), srt = 45, adj = 1, xpd = TRUE, cex = adj_cex)
  }
  axis(2, at = seq(0,y_upper_lim,.1), labels = seq(0,y_upper_lim,.1), cex.axis = adj_cex)
}
myTitle = function(lab, l, a, s){
  title(lab, outer = TRUE, line = l,
        adj = a, cex.main = s)
}

# results = readRDS("../../../../presentations/SIDIC2023/result.RDS")
# RMSEs = results$rmse
# runs = c(1,5,2,
#          14,10,15,
#          13,6,4,
#          16, 7, 17,
#          11,12
# )
# mods = c(1,2,3,4,6,8,10,12)
# RMSEs = abind::abind(RMSEs, along = 3)
# RMSEs = RMSEs[,mods,runs]

results = read.csv("../_data/_sim-results/RMSEs.csv")
RMSESs = tapply(results$RMSE, 
                list(results$run, results$methods, results$setups), 
                function (x) x)


d = dim(RMSEs)
mods_names = c("True Effect", "Usual", "Spearman", "Wishart",
               "1F", "1F+", "2F", "2F+")
# plot_data = data.frame(
#   "value" = as.vector(unlist(RMSEs)),
#   "setup" = rep(1:d[3], each = d[1]*d[2]),
#   "model" = rep(rep(mods_names, each = d[1]), d[3]))
# 

plot_data = data.frame(
  "value" = results$RMSE,
  "model" = results$methods,
  "setup" = results$setups
)
plot_data$model = factor(plot_data$model, levels = mods_names)


plot_data_list = list(
  "temp1" = plot_data[plot_data$setup==1,],
  "temp4" = plot_data[plot_data$setup==4,],
  "temp2" = plot_data[plot_data$setup==2,],
  "temp5" = plot_data[plot_data$setup==5,],
  "temp3" = plot_data[plot_data$setup==3,],
  "temp6" = plot_data[plot_data$setup==6,]
)

pdf("../_manuscript/_figs/rmse-res1.pdf",
    width = 8,
    height = 10)


layout_matrix = matrix(1:6, nrow = 3, byrow = TRUE)
ratio = .24/.4
layout(layout_matrix, heights = c(rep(1*ratio, 2), 1)) 

labs = list("A[1]", "A[2]", "A[3]",
            "B[1]", "B[2]", "B[3]",
            "C[1]", "C[2]", "C[3]",
            "D[1]", "D[2]", "D[3]",
            "E", "F")
expr_labs = lapply(labs, function(x) as.expression(parse(text=x)))

mapply(plotRMSE, 
       plot_data_list, 
       c(rep(F, 4), T, T),
       rep(c(T,F),3),
       c(rep(.3,4), .4,.4),
       SIMPLIFY = FALSE)
mapply(myTitle,
       expr_labs[1:6],
       c(rep(c(-2,-23,-42.5),2)),
       c(rep(.1,3), rep(.62,3)),
       3
)
dev.off()



plot_data_list = list(
  "temp1" = plot_data[plot_data$setup==7,],
  "temp4" = plot_data[plot_data$setup==10,],
  "temp2" = plot_data[plot_data$setup==8,],
  "temp5" = plot_data[plot_data$setup==11,],
  "temp3" = plot_data[plot_data$setup==9,],
  "temp6" = plot_data[plot_data$setup==12,]
)



pdf("../_manuscript/_figs/rmse-res2.pdf",
    width = 8,
    height = 10)

layout_matrix = matrix(1:6, nrow = 3, byrow = TRUE)
ratio = .22/.4
layout(layout_matrix, heights = c(rep(1*ratio, 2), 1)) 

mapply(plotRMSE,
       plot_data_list,
       c(rep(F, 4), T, T),
       rep(c(T,F),3),
       c(rep(.3,4), .45,.45),
       SIMPLIFY = FALSE)
mapply(myTitle,
       expr_labs[7:12],
       c(rep(c(-2,-22,-43.5),2)),
       c(rep(.1,3), rep(.62,3)),
       3
)
dev.off()



plot_data_list = list(
  "temp1" = plot_data[plot_data$setup==13,],
  "temp4" = plot_data[plot_data$setup==14,]
)
pdf("../_manuscript/_figs/rmse-res3.pdf",
    width = 8,
    height = 4)

layout_matrix = matrix(1:2, nrow = 1, byrow = TRUE)
layout(layout_matrix) 


mapply(plotRMSE,
       plot_data_list,
       c(rep(T, 2)),
       c(T, F),
       c(rep(.4,2)),
       rep(-.05,2),
       rep(1.3,2),
       SIMPLIFY = FALSE)
mapply(myTitle,
       expr_labs[13:14],
       -1,
       c(rep(.14,1), rep(.65,1)),
       2
)
dev.off()

