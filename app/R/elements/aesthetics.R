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
                         legend.key = element_rect(fill = NA))

time_ggtheme <- theme(panel.background = element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.y = element_blank(),
                      axis.line.x = element_line(color = "#cacee6", size = 0.8),
                      axis.ticks.x = element_line(color = "#cacee6", size = 0.8),
                      panel.grid.major.x = element_line(color = "#ebeeff", size = 0.6),
                      axis.text = element_text(family = "Karla", color = "#51535e", size = 14),
                      axis.title = element_text(family = "Karla", color = "#51535e", size = 15),
                      legend.position = "bottom",
                      legend.text = element_text(family = "Karla", color = "#51535e", size = 13),
                      legend.key = element_rect(fill = NA),
                      strip.background = element_rect(fill = "#303657", color = "#303657"),
                      strip.text = element_text(family = "Karla", color = "white", size = 14))

arc_ggtheme <- theme(panel.background = element_rect(fill = NA),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     axis.title = element_blank(),
                     legend.position = "bottom",
                     legend.text = element_text(family = "Karla"))