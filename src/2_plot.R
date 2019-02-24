

# weekly standing plot ----------------------------------------------------

my_color <- c("#E70033", "#EF8200", "#00A94F",
              "#0063BE", "#6C6F70", "#651F76",
              "#A5A4DF")

rank_plot <- ggplot(data = dat_rank, aes(x = week, y = 13 - rank, 
                                         color = team, group = team, linetype = team,
                                         text = paste("rank:", rank))) + 
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_linetype_manual(name = "Team",
                        #                     labels = team_name,
                        values = rep(c("solid", "dotted"), times = 6)) +
  scale_color_manual(name = "Team",
                     #                     labels = team_name,
                     values = rep(my_color, each = 2)) +
  scale_y_discrete(breaks = c(1:12), 
                   labels = c("12th", "11th", 
                              "10th", "9th", "8th", "7th",
                              "6th", "5th", "4th", "3rd", "2nd", "1st"),
                   name = "Rank") +
  scale_x_discrete(labels = 1:(dim(dat_rank)[1]/12), 
                   name = "Week")



# Team's weekly performance, by number of winning categories --------------

viz_result <- ggplot(dat_result) +
  geom_bar(aes(week, margin, fill = final, width = 0.7), 
           stat = "identity") +
  scale_fill_manual(values = c("#E70033", "#00A94F"), guide = F) +
  theme(panel.margin = unit(3, "in")) +
  facet_wrap(~ team, ncol = 3, scales = "fixed") +
  theme_grey() +
  labs(x = "Week", y = "Number of Winning Categories") +
  scale_x_continuous(breaks = c(1:max(dat_result$week)),
                     labels = c(1:max(dat_result$week))) +
  scale_y_continuous(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8),
                     labels = c(-8, -6, -4, -2, 0, 2, 4, 6, 8))



# Create comparison plot for each category --------------------------------

cat_compr_plot <- function(var, var_name, team_list){
  team_list <- unlist(team_list)
  dat_tmp <- dat_detail %>% dplyr::filter(team %in% team_list)
  my_plot <- ggplot(data = dat_tmp, 
                    aes_string(x = "week", y = var,
                               color = "team", group = "team", linetype = "team",
                               text = var
  )) +
    geom_line() +
    geom_point() +
    theme_classic() +
    labs(x = "Week", y = var_name) +
    scale_linetype_manual(name = "Team",
                          values = rep(c("solid", "dotted"), each = 6)[1:length(team_list)]) +
    scale_color_manual(name = "Team",
                       values = rep(my_color, times = 2)[1:length(team_list)]) +
    scale_x_continuous(breaks = c(1:max(dat_tmp$week)),
                       labels = c(1:max(dat_tmp$week))) +
    theme(legend.position="bottom")
  
  return(my_plot)
}





