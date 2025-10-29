# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries
library(dplyr)
library(circlize)
source('funcs.R')

# read data
dat = read.csv('significant_bivariate_relationships.csv') %>%
  filter(DV!='LIFESATISFACTION') %>%
  mutate(IV = recode(IV, !!!driving_var_recode_list),
         DV = recode(DV, !!! health_var_recode_list)) %>%
  mutate(weight = abs(beta))


# Node sets
# can reorder this
IV_nodes = unique(dat$IV)
DV_nodes = unique(dat$DV)

# Colors for nodes
IV_colors = rep("grey", length(IV_nodes))
names(IV_colors) = IV_nodes
grid_colors = c(IV_colors, DV_colors[-1]) # excluding life satisfaction

# Band colors (group by DV here)
link_colors = DV_colors[dat$DV]

# clear any prior circos state
circos.clear()

# Plot
png("../figs/chord_diagram.png", width = 10, height = 10, res=900, units = 'in')
circos.clear()
#circos.par(start.degree = 180) # rotate the diagram
circos.par(canvas.xlim = c(-1.9,1.9), canvas.ylim = c(0,0))
chordDiagram(
  x = dat[, c("IV", "DV", "weight")],
  grid.col = grid_colors,
  col = link_colors,
  link.largest.ontop = T,
  link.sort = T, 
  link.decreasing = F,
  transparency = 0.4,
  annotationTrack = "grid",
  link.lwd = 0.5,
  link.border = T,
  directional = 1, 
  diffHeight = mm_h(5),
  direction.type = c("diffHeight", "arrows"),
  link.arr.type = "big.arrow"
)

circos.trackPlotRegion( track.index = 1, 
                        panel.fun = function(x, y) 
                          { sector.name = get.cell.meta.data("sector.index") 
                          circos.text( CELL_META$xcenter+0.01, 
                                       CELL_META$ycenter+1, 
                                       labels = sector.name, 
                                       col = "black", 
                                       font = 1, 
                                       cex = 1.2, 
                                       adj=c(0,0.5), 
                                       facing = "clockwise", 
                                       niceFacing = TRUE ) }, 
                        bg.border = T )

circos.trackPlotRegion(
  factors = dat$IV,
  ylim = c(0, 1),
  bg.border = NA,
  panel.fun = function(x, y) {
    sector = CELL_META$sector.index
    this_dat = dat %>% filter(IV == sector)
    
    for (i in seq_len(nrow(this_dat))) {
      beta_sign = this_dat$beta[i]
      col = ifelse(beta_sign > 0, "#2ca25f", "#de2d26")  # green/red
      col = adjustcolor(col, alpha.f = 1)  # 60% opacity
      # Compute segment range
      x_range = CELL_META$xlim
      num_links = nrow(this_dat)
      x_start = x_range[1] + (i - 1) * diff(x_range) / num_links
      x_end = x_range[1] + i * diff(x_range) / num_links
      
      circos.rect(xleft = x_start, xright = x_end, ybottom = 1, ytop = 1.1,
                  col = col, border = NA)
    }
  }
)

legend("bottomright",
       legend = c("Positive Relationship", "Negative Relationship"),
       fill = c("#2ca25f", "#de2d26"),
       border = NA,
       bty = "n",
       cex = 1.2)

dev.off()

