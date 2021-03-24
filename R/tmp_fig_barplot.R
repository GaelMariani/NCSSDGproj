SDG_network[["score_pos"]][["data_pourc"]]$pos_neg <- "+"
SDG_network[["score_neg"]][["data_pourc"]]$pos_neg <- "-"

data_plot <- rbind(SDG_network[["score_pos"]][["data_pourc"]], SDG_network[["score_neg"]][["data_pourc"]])


data_plot <- data_plot %>%
  dplyr::mutate(x = paste0(SDG_number, pos_neg),
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

color = c("#1134A6", "#5EA9A2", "#228B22")


test_barplot_pourc <- ggplot2::ggplot() +
  
  ## Plot bars
  ggplot2::geom_col(data        = data_plot, 
                    mapping     = ggplot2::aes(x     = x_test, 
                                               y     = relative_pourcent,
                                               fill  = factor(group, levels = unique(order_group)),
                                               #color = pos_neg,
                                               group = pos_neg),  
                    #position    = ggplot2::position_dodge(width = 0.9),
                    stat        = "identity",
                    width       = 0.65, 
                    alpha       = 0.8,
                    show.legend = FALSE) +
  
  ## Add text (number of targets achieved in each SDG)
  ggplot2::geom_text(mapping     = ggplot2::aes(x     = x_test, 
                                                y     = perc_goal + 5, 
                                                group = pos_neg,
                                                color = pos_neg,
                                                label = text),
                     # nudge_y = 2, 
                     #position    = ggplot2::position_dodge(width = 0.9),
                     stat        = "identity",
                     data        = text_plot, 
                     size        = 3.5,
                     show.legend = FALSE) +
  
  ## Add text (number of targets achieved in each SDG)
  # ggplot2::geom_text(mapping     = ggplot2::aes(x     = x_test, 
  #                                               y     = -1.5, 
  #                                               group = pos_neg,
  #                                               color = pos_neg,
  #                                               label = pos_neg),
  #                    # nudge_y = 2, 
  #                    #position    = ggplot2::position_dodge(width = 0.9),
  #                    stat        = "identity",
  #                    data        = text_plot, 
  #                    size        = 4,
  #                    show.legend = FALSE) +

  ggplot2::annotate(geom = "text", 
                    x = seq(1, 32, 1)+0.1, 
                    y = -1.5, 
                    label = rep(c("-","+"), 16), 
                    color = rep(c("red","darkgreen"), 16), 
                    size = 4,
                    angle = 0) +
  
  ## scale modif
  ggplot2::scale_fill_manual(values = color,
                             name   = NULL) +
  
  ggplot2::scale_color_manual(values = c("red", "darkgreen"),
                              name   = NULL) +
  
  ggplot2::scale_y_continuous(position = "right", 
                              breaks   = seq(0, max(data_plot$perc_global), 10), 
                              expand   = c(0.03,0,0.1,0)) +
  
  ggplot2::annotate(geom = "text", 
                    x = seq(1.5, 31.5, 2), 
                    y = -5, 
                    label = x_labels$goal, 
                    color = color_text, 
                    size = 4,
                    angle = 0, 
                    fontface = "bold", 
                    hjust = 1) +
  
  ggplot2::geom_segment(mapping     = ggplot2::aes(x    = seq(0.7, 30.7, 2),
                                                   xend = seq(2.3, 32.3, 2),
                                                   y    = -3,
                                                   yend = -3),
                        color       = color_text,
                        size        = 1,
                        inherit.aes = FALSE) +

  # ggplot2::geom_text(data    = x_labels,
  #                    mapping = ggplot2::aes(x      = seq(1.5, 31.5, 2),
  #                                           y      = -8,
  #                                           label = goal)) +
  
  # ggplot2::scale_x_discrete(breaks = seq(1, 31, 2),
  #                           labels = paste(rep("SDG", 16), unique(text_plot$SDG_number)),
  #                           expand = c(0.03,0.03))  +
  
  ggplot2::coord_flip(ylim = c(-1,100) ,
                      clip = "off") +
  
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

test_barplot_pourc

save(test_barplot_pourc, file = here::here("results", "test_barplot_pourc.RData"))
ggplot2::ggsave(here::here("figures", "tmp.png"), width = 5, height = 6.8, device = "png")


  
  # Load panels
  fig1a <- NCSSDGproj::load_Fig1A()
  fig1b <- NCSSDGproj::load_Fig1B_test()
  legend <- NCSSDGproj::load_legend()
  
  # Assemble panels
  fig1 <- cowplot::ggdraw() +
    
    cowplot::draw_plot(fig1a, x = 0, y = 0.02, width = 0.61, height = 0.98) +
    cowplot::draw_plot(fig1b, x = 0.53, y = -0.044, width = 0.51, height = 1.105) +
    cowplot::draw_plot(legend, x = 0.3, y = 0, width = 0.5, height = 0.05) +
    cowplot::draw_plot_label(label = c("a", "b"),
                             size = 15,
                             x = c(0, 0.55),
                             y = c(1, 1)) 
  
    
    ggplot2::ggsave(here::here("figures", "Figure2_test.png"), width=10, height=9, device="png")   
  

################################################################################################

SDG_network[["score_pos"]][["data_pourc"]]$pos_neg <- "+"
SDG_network[["score_neg"]][["data_pourc"]]$pos_neg <- "-"

data_plot <- rbind(SDG_network[["score_pos"]][["data_pourc"]], SDG_network[["score_neg"]][["data_pourc"]])


data_pos <- SDG_network[["score_neg"]][["data_pourc"]] %>%
  dplyr::mutate(pos = (cumsum(relative_pourcent) - relative_pourcent)/2) # calculate the position of each label

data_neg <- SDG_network[["score_neg"]][["data_pourc"]] %>%
  dplyr::mutate(pos = (cumsum(relative_pourcent) - relative_pourcent)/2) # calculate the position of each label

text_plot <-  data_plot[seq(1,96,3),]
text_pos <- text_plot[text_plot$pos_neg == "+",]
text_neg <- text_plot[text_plot$pos_neg == "-",]


ggplot2::ggplot() +
  
  ggplot2::geom_bar(data     = data_pos,
                    mapping  = ggplot2::aes(x    = as.numeric(SDG_number), 
                                            y    = relative_pourcent,
                                            fill = factor(group, levels = unique(order_group)),
                                            group = as.factor()),
                    stat     = "identity",
                    position = "stack",
                    width    = 0.35,
                    show.legend = FALSE) +
  
  ## Add text (number of targets achieved in each SDG)
  ggplot2::geom_text(data    = text_pos,
                     mapping = ggplot2::aes(x     = as.numeric(SDG_number), 
                                            y     = perc_goal + 5, 
                                            label = text),
                     nudge_y = 2, 
                     size    = 5) +
  
  ggplot2::geom_bar(data      = data_neg,
                    mapping   = ggplot2::aes(x    = as.numeric(SDG_number) + 0.36, 
                                             y    = relative_pourcent,
                                             fill = factor(group, levels = unique(order_group))),
                    stat      = "identity",
                    position  = "stack",
                    width     = 0.35,
                    show.legend = FALSE) +
  
  ## Add text (number of targets achieved in each SDG)
  ggplot2::geom_text(data    = text_neg,
                     mapping = ggplot2::aes(x     = as.numeric(SDG_number) + 0.36, 
                                            y     = perc_goal + 5, 
                                            label = text),
                     nudge_y = 2,
                     size    = 5) +
  
  ggplot2::scale_x_discrete(labels = factor(data_pos$SDG_number, levels = rev(unique(order))))
  



data_plot <- data_plot %>% 
  dplyr::mutate(fill = dplyr::case_when(group == "Coastal" ~ "#5EA9A2", 
                                        group == "Marine" ~ "#1134A6",
                                        group == "Terrestrial" ~ "#228B22"))

