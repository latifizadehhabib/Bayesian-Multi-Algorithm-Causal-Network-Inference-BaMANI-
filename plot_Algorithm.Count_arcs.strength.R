

plot_Algorithm.Count_arcs.strength <- function(Alg.Count_arcs.strength.data) {
  
  data <- Alg.Count_arcs.strength.data
  
  # Assign categories based on condition
  data$category <- ifelse(data$Min.BIC_clear.direction == 1, "Min.BIC_clear.direction",
                        ifelse(data$Min.BIC.unclear.direction == 1, "Min.BIC.unclear.direction",
                               ifelse(data$Unclear.direction == 1, "Unclear.direction",
                                      ifelse(data$Excluded == 1, "Excluded", NA_character_))))
  
   arc_name <- as.numeric(data$Edge_No)
  algorithm_count <- as.numeric(data$Hit.Count)
  # ensure that the logarithmic transformation is applied only to positive values by adding a small positive value
  min_strength <- as.numeric(data$Min_strength)#+0.00000001
  max_strength <- as.numeric(data$Max_strength)#+0.00000001
  
  
  # Create plot with algorithm count as bars
  p1 <- ggplot(data, aes(x = arc_name, y = algorithm_count, fill = category)) +
    geom_col(position = "stack") +
    labs(y = "Algorithm Count") +
    scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
    theme_bw()+
    theme(panel.background = element_blank())+
    scale_x_continuous(breaks = seq(min(arc_name), max(arc_name), 1))# Add sequence of integers to x-axis labels
   
  # ---------------------------------------new
  # Create plot with Min strength and Max strength as points
  # p2 <- ggplot(data, aes(x = arc_name, y = min_strength, group = 1)) +
  #   geom_point(aes(color = "Min strength"), size = 2) +
  #   geom_point(aes(y = max_strength, color = "Max strength"), size = 2) +
  #   geom_line(aes(y = min_strength, group = 1), color = "black") +
  #   geom_line(aes(y = max_strength, group = 1), color = "black") +
  #   labs(x = NULL, y = "Strength", color = NULL) +
  #   # Add a check for zero or negative values before applying the logarithmic transformation
  #   scale_y_continuous(name = "Min / Max Strength",
  #                      trans = ifelse(any(min_strength <= 0 | max_strength <= 0), "log1p", "log10"),
  #                      breaks = seq(0, max(max_strength), (signif(max(max_strength)/20, digits = 1)))) +
  #   scale_color_manual(values = c("blue", "red")) +
  #   theme_bw()+
  #   theme(panel.background = element_blank())+
  #   scale_x_continuous(breaks = seq(min(arc_name), max(arc_name), 1))# Add sequence of integers to x-axis labels
  
  
  # ----------------------------original
  p2 <- ggplot(data, aes(x = arc_name, y = min_strength, group = 1)) +
    geom_point(aes(color = "Min strength"), size = 2) +
    geom_point(aes(y = max_strength, color = "Max strength"), size = 2) +
    geom_line(aes(y = min_strength, group = 1), color = "red") +
    geom_line(aes(y = max_strength, group = 1), color = "blue") +
    labs(x = NULL, y = "Strength", color = NULL) +
    # scale_y_continuous(name = "Min / Max Strength")+
    scale_y_log10(name = "Min / Max Strength") +
    scale_color_manual(values = c("blue", "red")) +
    theme_bw()+
    theme(panel.background = element_blank())+
    scale_x_continuous(breaks = seq(min(arc_name), max(arc_name), 1))# Add sequence of integers to x-axis labels
  
  # -------------------------------------old
  # Create plot with Min strength and Max strength as points
  # p2 <- ggplot(data, aes(x = arc_name, y = min_strength, group = 1)) +
  #   geom_point(aes(color = "Min strength"), size = 2) +
  #   geom_point(aes(y = max_strength, color = "Max strength"), size = 2) +
  #   geom_line(aes(y = min_strength, group = 1), color = "black") +
  #   geom_line(aes(y = max_strength, group = 1), color = "black") +
  #   labs(x = NULL, y = "Strength", color = NULL) +
  #   # scale_y_sqrt(name = "Min / Max Strength")+
  #   scale_y_log10(name = "Min / Max Strength") +
  #   # scale_y_continuous(name = "Min / Max Strength")+
  #   scale_color_manual(values = c("blue", "red")) +
  #   theme_bw()+
  #   theme(panel.background = element_blank())+
  #   scale_x_continuous(breaks = seq(min(arc_name), max(arc_name), 1))# Add sequence of integers to x-axis labels
  
  

  # Combine plots using cowplot
  plot_grid(p1, p2, nrow = 2, align = "v", axis = "tb", rel_widths = c(1, 0.7))
}





# # Final final
# library(ggplot2)
# 
# # Example data
# category <- c("Clear Direction+BIC Minimum", "Clear Direction+BIC Minimum", "Unclear Direction+BIC Minimum", "Unclear Direction", "Unclear Direction", "Excluded", "Excluded", "Excluded")
# arc_name <- c("Arc 1", "Arc 2", "Arc 3", "Arc 4", "Arc 5", "Arc 6", "Arc 7", "Arc 8")
# algorithm_count <- c(2, 5, 3, 1, 4, 2, 3, 1)
# min_strength <- c(0.0001, 0.0002, 0.0003, 0.0004, 0.0005, 0.0006, 0.0007, 0.0008)
# max_strength <- c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008)
# 
# df <- data.frame(category, arc_name, algorithm_count, min_strength, max_strength)
# 
# # Create plot with algorithm count as bars
# p1 <- ggplot(df, aes(x = arc_name, y = algorithm_count, fill = category)) +
#   geom_col(position = "stack") +
#   labs(title = "Arcs Divided into Four Categories by Algorithm Count", x = "Arc", y = "Algorithm Count") +
#   scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
#   theme_bw()
# 
# # Create plot with Min strength and Max strength as points
# p2 <- ggplot(df, aes(x = arc_name, y = min_strength, group = 1)) +
#   geom_point(aes(color = "Min strength"), size = 2) +
#   geom_point(aes(y = max_strength, color = "Max strength"), size = 2) +
#   geom_line(aes(y = min_strength, group = 1), color = "black") +
#   geom_line(aes(y = max_strength, group = 1), color = "black") +
#   labs(x = NULL, y = "Strength", color = NULL) +
#   scale_y_log10(name = "Min/Max Strength") +
#   # scale_y_log10(name = "Min/Max Strength", sec.axis = sec_axis(~ 10^., name = "Algorithm Count")) +
#   scale_color_manual(values = c("blue", "red")) +
#   
#   theme_bw()
# 
# # Combine plots using cowplot
# library(cowplot)
# plot_grid(p1, p2, nrow = 2, align = "v", axis = "tb", rel_widths = c(1, 0.7))
