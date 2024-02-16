library(ggcorrplot)
library(ggplot2)
library(ggpubr)
library(cowplot)


t.cor = readRDS("../_data/_ground-truths//correlations.RDS")

plotCorr = function(corr, title){
  ggcorrplot(corr, type = "upper",
             outline.col = "black",
             colors = c("firebrick1", "gray95", "deepskyblue2"),
             lab = T, lab_size = 2.5) +
    theme(panel.background = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(), 
          axis.ticks = element_blank(),
          legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) + scale_x_reverse()
}

plot_list = lapply(1:length(t.cor), function(i) plotCorr(t.cor[[i]], paste(i)))

legend_data = data.frame(cor_value = seq(0, 1, length.out = 100))
legend_data$y = 1

legend_plot = ggplot(legend_data, aes(x = cor_value, y = y, fill = cor_value)) +
  geom_tile() +
  scale_fill_gradient2(low = "gray95", 
                       high = "deepskyblue4", 
                       mid = "deepskyblue1", 
                       midpoint = 0.5) +
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(title = "Correlation", 
                                title.position = "top", 
                                barwidth = 8, 
                                barheight = 1))

legend_only = get_legend(legend_plot)
labs = c("A^1", "B[2]", letters[1:12], "legend")  

labs = list("A[1]", "A[2]", "A[3]",
            "B[1]", "B[2]", "B[3]",
            "C[1]", "C[2]", "C[3]",
            "D[1]", "D[2]", "D[3]",
            "E", "F", "")


plot_list_with_legend = c(plot_list, list(legend_only))

x.pos = rep(c(-.01, .32, .655),5)
x.pos[13:14] = c(0, 0.34)



plot_to_save = plot_grid(plotlist = plot_list_with_legend, ncol = 3, nrow = 5) +
  draw_plot_label(labs, x = x.pos, 
                  y = rep(rev(seq(.2,1,length.out=5)), each = 3), parse = TRUE,
                  fontface = "bold", size = 20, family = "italic")



plot_to_save = plot_to_save + 
  theme(plot.background = element_rect(fill = "white", colour = "white"))
ggsave("../_manuscript/_figs/true_cors.png", 
       plot_to_save, 
       width = 6.6, 
       height = 8, 
       dpi = 300)
