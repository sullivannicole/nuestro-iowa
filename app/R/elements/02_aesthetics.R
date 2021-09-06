#---------------------
# Aesthetic settings
#---------------------

# Hex codes
hex_purple <- "#5E72E4" #primary
hex_blue_lt <- "#5DCEF0" #info
hex_green <- "#63CF89" #success
hex_pink <- "#EA445B" #danger
hex_orange <- "#EC603E" #warning
hex_blue_dk <- "#172B4D" #default
hex_grey <- "#51535e"

project_ggtheme <- theme(panel.background = element_blank(),
                         axis.ticks.x = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         axis.line.y = element_line(color = "#cacee6", size = 0.8),
                         axis.ticks.y = element_line(color = "#cacee6", size = 0.8),
                         axis.text = element_text(family = "Karla", color = "#51535e", size = 14),
                         axis.title = element_text(family = "Karla", color = "#51535e", size = 15),
                         legend.position = "bottom",
                         legend.text = element_text(family = "Karla", color = "#51535e", size = 13),
                         legend.key = element_rect(fill = NA, color = NA),
                         legend.box.background = element_blank())

project_ggtheme_y <- theme(plot.title = element_text(hjust = 0.5, color = "#51535e", family = "Karla", size = 16),
                        panel.background = element_blank(),
                         axis.ticks.y = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         axis.line.x = element_line(color = "#cacee6", size = 0.8),
                         axis.ticks.x = element_line(color = "#cacee6", size = 0.8),
                         axis.text = element_text(family = "Karla", color = "#51535e", size = 14),
                         axis.title = element_text(family = "Karla", color = "#51535e", size = 15),
                         legend.position = "bottom",
                         legend.text = element_text(family = "Karla", color = "#51535e", size = 13),
                         legend.key = element_rect(fill = NA, color = NA),
                         legend.box.background = element_blank())

time_ggtheme <- theme(plot.title = element_text(hjust = 0.5, color = "#51535e", family = "Karla", size = 16),
                      panel.background = element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      axis.line.x = element_line(color = "#cacee6", size = 1.2),
                      axis.ticks.x = element_line(color = "#cacee6", size = 1.2),
                      panel.grid.major.y = element_line(color = "#ebeeff", size = 1.2),
                      axis.text = element_text(family = "Karla", color = "#51535e", size = 14),
                      axis.title = element_text(family = "Karla", color = "#51535e", size = 15),
                      legend.position = "bottom",
                      legend.text = element_text(family = "Karla", color = "#51535e", size = 13),
                      legend.key = element_rect(fill = NA),
                      strip.background = element_rect(fill = "#cacee6", color = "#cacee6"),
                      strip.text = element_text(family = "Karla", color = hex_purple, size = 14, face = "bold"))

dk_ggtheme <- theme(plot.title = element_text(hjust = 0.5, color = "white", family = "Karla", size = 16, face = "bold"),
                    plot.background = element_rect(fill = hex_blue_dk),
                    panel.background = element_rect(fill = hex_blue_dk),
                    plot.margin = margin(2, 2, 2, 2, "cm"),
                      axis.ticks.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      axis.line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_text(family = "Karla", color = "white", size = 14),
                      axis.title = element_text(family = "Karla", color = "white", size = 15),
                      legend.position = "bottom",
                      legend.text = element_text(family = "Karla", color = "white", size = 13),
                      legend.key = element_rect(fill = NA),
                      strip.background = element_rect(fill = "#cacee6", color = "#cacee6"),
                      strip.text = element_text(family = "Karla", color = hex_purple, size = 14, face = "bold"))

arc_ggtheme <- theme(panel.background = element_rect(fill = NA),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     axis.title = element_blank(),
                     legend.position = "bottom",
                     legend.text = element_text(family = "Karla"))