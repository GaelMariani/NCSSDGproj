SDG_network[["score_pos"]][["data_pourc"]]$pos_neg <- "+"
SDG_network[["score_neg"]][["data_pourc"]]$pos_neg <- "-"

data_plot <- rbind(SDG_network[["score_pos"]][["data_pourc"]], SDG_network[["score_neg"]][["data_pourc"]]) %>%
  dplyr::mutate(test = ifelse(test = pos_neg == "-",
                              yes  = -1*relative_pourcent, 
                              no   = relative_pourcent),
                group_test = ifelse(test = pos_neg == "-",
                                    yes  = paste0(group, "_neg"), 
                                    no   = group),
                perc_goal_labs = ifelse(test = pos_neg == "-",
                                        yes  = (-1*perc_goal), 
                                        no   = perc_goal),
                perc_global = ifelse(test = pos_neg == "-",
                                     yes  = (-1*perc_global) , 
                                     no   = perc_global),
                x = paste0(SDG_number, pos_neg),
                x_test = paste(rep("SDG", length(.)), x))

text_plot <-  data_plot[seq(1,96,3),] %>%
  dplyr::mutate(color = dplyr::case_when(pos_neg == "+" ~ "darkgreen",
                                         pos_neg == "-" ~ "red"))
x_labels <- dplyr::filter(text_plot, pos_neg == "+") 

color_text <- c("#FDB713", "#00AED9", "#3EB049", "#F99D26", "#EF402B", "#279B48",
                "#48773E", "#F36D25", "#EB1C2D", "#C31F33", "#8F1838", "#02558B",
                "#CF8D2A", "#E11484", "#D3A029", "#007DBC")

order <- c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14)
order_group <- rev(c("Terrestrial", "Coastal", "Marine"))

color = c("#1134A6", "#5EA9A2", "#228B22", scales::alpha("#1134A6", 0.5), scales::alpha("#1134A6", 0.5), scales::alpha("#5EA9A2", 0.5))


test <- ggplot2::ggplot() +
  
  ## Plot bars
  ggplot2::geom_col(data        = data_plot, 
                    mapping     = ggplot2::aes(x     = goal, 
                                               y     = test,
                                               fill  = as.factor(group_test),
                                               #color = pos_neg,
                                               # group = pos_neg
                                               ),  
                    position    = "stack",
                    stat        = "identity",
                    width       = 0.65, 
                    alpha       = 0.8,
                    show.legend = FALSE) +
  
  ## Add text (number of targets achieved in each SDG)
  ggplot2::geom_text(mapping     = ggplot2::aes(x     = goal, 
                                                y     = perc_goal_labs, 
                                                group = pos_neg,
                                                color = pos_neg,
                                                label = text),
                     stat        = "identity",
                     data        = text_plot, 
                     size        = 3.5,
                     show.legend = FALSE) +


  ## scale modif
  # ggplot2::scale_fill_manual(values = color,
  #                            name   = NULL) + 
  
  ggplot2::scale_y_continuous(position = "right", 
                              breaks   = seq(plyr::round_any(min(data_plot$perc_goal_labs)+2, 10), 
                                             max(data_plot$perc_goal_labs) -2, 10), 
                              expand   = c(0.01,0,0.02,0)) +
  
  ggplot2::scale_x_discrete(labels = paste(rep("SDG", 11), rev(c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14))), 
                            expand = c(0.03,0.03))  +
  
  
  ggplot2::coord_flip() +
  
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text   = ggplot2::element_text(size = 12),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 # axis.text.y = ggplot2::element_text(color = rev(color_text), face = "bold"),
                 # axis.title  = ggplot2::element_text(size = 18),
                 
                 # Legend modifications
                 legend.position   = c(0.90, 0.90),
                 legend.text       = ggplot2::element_text(size = 16),
                 legend.background = ggplot2::element_rect(fill  = "transparent", 
                                                           color = "transparent"),
                 
                 # Widen the left margin
                 plot.margin = ggplot2::unit(c(2, 2, 4, 2), "lines"),
                 
                 # Remove grid on the background
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 plot.background  = ggplot2::element_rect(fill   = "transparent", 
                                                          colour = NA)) +
  
  
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) 

test
