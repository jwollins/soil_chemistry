## Soil nutrient plots 
## Joe Collins 
## 2024-06-24

## 03 PLOTS ####



## 03.1 PACKAGES ####
setwd(dir = "~/Documents/GitHub/soil_chemistry/")

# source(file = "01_packages.R")
# 
# 
# ## 03.2 DATA ####
# 
# source(file = "02_data.R")


## 03.3 PLOTS ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/Soil/soil_chemistry/")
    
    
### P ####
    
    
    # this is the legend title with correct notation
    title_exp <- expression(Phosphorus~(Mg~l^{-1}))
    y_title <- expression(Available~P~(Mg~l^{-1}))
    
 p <-   ggplot(data = p_sum, 
           aes(x = Treatment, 
               y = mean, 
               fill = Treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = element_blank(),
        y = y_title,
        subtitle = title_exp, 
        caption = "") +
      theme_bw() +
      scale_fill_manual(values=c("white","tomato2", "turquoise3"), 
                        name = "Treatment") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom", 
            axis.text.x = element_blank()) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      facet_wrap(~ Year, 
                 ncol = 4, 
                 scales = 'free_x') 
 
 p

      # geom_signif(
      #   data = subset(p_sum, Year == 2022), # Subset data for Crop1
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.01, 
      #   annotations = "NS.", 
      #   fontface = 'italic', 
      #   y_position = c(20) # Adjust y-position if necessary
      # ) +
      # geom_signif(
      #   data = subset(p_sum, Year == "2023"), # Subset data for Crop2
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.01,
      #   annotations = "NS.",
      #   fontface = 'italic', 
      #   y_position = c(20) # Adjust y-position if necessary
      # ) 
    
    
    ggsave(filename = "p_plot.png", 
           path = "plots/", width = 8, height = 5)
    
    
    
    
    ### K ####
    
    
    # this is the legend title with correct notation
    title_exp <- expression(Potassium~(Mg~l^{-1}))
    y_title <- expression(Available~K~(Mg~l^{-1}))
    
k <-    ggplot(data = k_sum, 
           aes(x = Treatment, 
               y = mean, 
               fill = Treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = element_blank(),
        y = y_title,
        subtitle = title_exp, 
        caption = "") +
      theme_bw() +
      scale_fill_manual(values=c("white","tomato2", "turquoise3"), 
                        name = "Treatment") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom", 
            axis.text.x = element_blank()) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      facet_wrap(~ Year, 
                 ncol = 4, 
                 scales = 'free_x') 

k

      # geom_signif(
      #   data = subset(p_sum, Year == 2022), # Subset data for Crop1
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.01, 
      #   annotations = "NS.", 
      #   fontface = 'italic', 
      #   y_position = c(200) # Adjust y-position if necessary
      # ) +
      # geom_signif(
      #   data = subset(p_sum, Year == "2023"), # Subset data for Crop2
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.01,
      #   annotations = "NS.",
      #   fontface = 'italic', 
      #   y_position = c(200) # Adjust y-position if necessary
      # ) 
    
    
    ggsave(filename = "k_plot.png", 
           path = "plots/", width = 8, height = 5)
    
    
    
    
    ### Mg ####
    
    
    # this is the legend title with correct notation
    title_exp <- expression(Magnesium~(Mg~l^{-1}))
    y_title <- expression(Available~Mg~(Mg~l^{-1}))
    
 mg <-   ggplot(data = mg_sum, 
           aes(x = Treatment, 
               y = mean, 
               fill = Treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = element_blank(),
        y = y_title,
        subtitle = title_exp, 
        caption = "") +
      theme_bw() +
      scale_fill_manual(values=c("white","tomato2", "turquoise3"), 
                        name = "Treatment") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom", 
            axis.text.x = element_blank()) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      facet_wrap(~ Year, 
                 ncol = 4, 
                 scales = 'free_x') 
    
    
      # geom_signif(
      #   data = subset(p_sum, Year == 2022), # Subset data for Crop1
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.01, 
      #   annotations = "NS.", 
      #   fontface = 'italic', 
      #   y_position = c(100) # Adjust y-position if necessary
      # ) +
      # geom_signif(
      #   data = subset(p_sum, Year == "2023"), # Subset data for Crop2
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.01,
      #   annotations = "NS.",
      #   fontface = 'italic', 
      #   y_position = c(100) # Adjust y-position if necessary
      # ) 
      # 
    
    ggsave(filename = "mg_plot.png", 
           path = "plots/", width = 8, height = 5)

        
    
    
    
    ### pH ####
  
    
  ph <-  ggplot(data = ph_sum, 
           aes(x = Treatment, 
               y = mean, 
               fill = Treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = element_blank(),
        y = "pH",
        subtitle = "pH", 
        caption = "") +
      theme_bw() +
      scale_fill_manual(values=c("white","tomato2", "turquoise3"), 
                        name = "Treatment") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom",
            axis.text.x = element_blank()) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      facet_wrap(~ Year, 
                 ncol = 4, 
                 scales = 'free_x') 
    
    
      # geom_signif(
      #   data = subset(p_sum, Year == 2022), # Subset data for Crop1
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.01, 
      #   annotations = "NS.", 
      #   fontface = 'italic', 
      #   y_position = c(7.5) # Adjust y-position if necessary
      # ) +
      # geom_signif(
      #   data = subset(p_sum, Year == "2023"), # Subset data for Crop2
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.01,
      #   annotations = "NS.",
      #   fontface = 'italic', 
      #   y_position = c(7.5) # Adjust y-position if necessary
      # ) 
    
    
    ggsave(filename = "ph_plot.png", 
           path = "plots/", width = 8, height = 5)
    
    
    
    
    
    ### soc ####
    
    
  soc <-  ggplot(data = soc_sum, 
           aes(x = Treatment, 
               y = mean, 
               fill = Treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = element_blank(),
        y = "Soil Carbon (%)",
        subtitle = "Soil Carbon (%)", 
        caption = "") +
      theme_bw() +
      scale_fill_manual(values=c("white","tomato2", "turquoise3"), 
                        name = "Treatment") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom", 
            axis.text.x = element_blank()) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      facet_wrap(~ Year, 
                 ncol = 4, 
                 scales = 'free_x') 
    
    
    ggsave(filename = "soc_plot.png", 
           path = "plots/", width = 8, height = 5)
    
    
    
    
    
    ### N ####
    
    
  n <-  ggplot(data = n_sum, 
           aes(x = Treatment, 
               y = mean, 
               fill = Treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = element_blank(),
        y = "Soil Nitrogen (%)",
        subtitle = "Soil Nitrogen (%)", 
        caption = "") +
      theme_bw() +
      scale_fill_manual(values=c("white","tomato2", "turquoise3"), 
                        name = "Treatment") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom", 
            axis.text.x = element_blank()) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      facet_wrap(~ Year, 
                 ncol = 4, 
                 scales = 'free_x') 
      # geom_signif(
      #   data = subset(p_sum, Year == 2022), # Subset data for Crop1
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.001, 
      #   annotations = "NS.", 
      #   fontface = 'italic', 
      #   y_position = 0.2 # Adjust y-position if necessary
      # ) +
      # geom_signif(
      #   data = subset(p_sum, Year == "2023"), # Subset data for Crop2
      #   comparisons = list(c("Conventional", "Conservation")),
      #   map_signif_level = TRUE,
      #   textsize = 4,
      #   tip_length = 0.001,
      #   annotations = "NS.",
      #   fontface = 'italic', 
      #   y_position = 0.2 # Adjust y-position if necessary
      # ) 
    
    
    ggsave(filename = "n_plot.png", 
           path = "plots/", width = 8, height = 5)
    
    
    
    ### Bulk density ####
    
    
  bd <-  ggplot(data = bd_sum, 
           aes(x = Treatment, 
               y = mean, 
               fill = Treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = element_blank(),
        y = "Bulk Density (g / cm3)",
         subtitle = "Bulk Density (g / cm3)", 
        # caption = ""
        ) +
      theme_bw() +
      scale_fill_manual(values=c("white","tomato2", "turquoise3"), 
                        name = "Treatment") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom",
            axis.text.x = element_blank()) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      facet_wrap(~ Year, 
                 ncol = 4, 
                 scales = 'free_x') 
    
    # geom_signif(
    #   data = subset(p_sum, Year == 2022), # Subset data for Crop1
    #   comparisons = list(c("Conventional", "Conservation")),
    #   map_signif_level = TRUE,
    #   textsize = 4,
    #   tip_length = 0.001,
    #   annotations = "p = 0.02",
    #   fontface = 'italic',
    #   y_position = 0.9 # Adjust y-position if necessary
    # ) +
    # geom_signif(
    #   data = subset(p_sum, Year == "2023"), # Subset data for Crop2
    #   comparisons = list(c("Conventional", "Conservation")),
    #   map_signif_level = TRUE,
    #   textsize = 4,
    #   tip_length = 0.001,
    #   annotations = "NS.",
    #   fontface = 'italic',
    #   y_position = 0.9 # Adjust y-position if necessary
    # ) +
    #   geom_signif(
    #     data = subset(p_sum, Year == "2024"), # Subset data for Crop2
    #     comparisons = list(c("Conventional", "Conservation")),
    #     map_signif_level = TRUE,
    #     textsize = 4,
    #     tip_length = 0.001,
    #     annotations = "NS.",
    #     fontface = 'italic',
    #     y_position = 0.9 # Adjust y-position if necessary
    #   )
    
    
    ggsave(filename = "bd_plot.png", 
           path = "plots/", width = 8, height = 5)
    
    
    
    
    ## FIG 1 ####
    

ggarrange(p,k,mg,ph,soc,n,bd, 
          ncol = 3, 
          nrow = 3, 
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("A", "B", "C", "D", "E", "F", "G"))
    
    ggsave(filename = "fig_soil_lab.png", 
           path = "plots/", 
           width = 10, 
           height = 10)
    
    
    ggarrange(p,k,mg,ph,soc,n,bd, 
              ncol = 4, 
              nrow = 2, 
              common.legend = TRUE, 
              legend = "bottom", 
              labels = c("A", "B", "C", "D", "E", "F", "G"))
    
    ggsave(filename = "fig_soil_lab_landscape.png", 
           path = "plots/", 
           width = 15, 
           height = 6)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    