nu_comm_server <- function(input, output, session) {
  
  output$time_income <- renderPlotly({
    
    income <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & substr(variable, 1, 6) == "B19013") %>%
      mutate(concept = str_replace(concept,"\\(IN 2019 INFLATION-ADJUSTED DOLLARS\\)", "")) %>%
      separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
      mutate(race_ethnicity = str_trim(str_to_title(str_remove_all(race_ethnicity, "HOUSEHOLDER|\\)"))),
             text = str_wrap(paste0("$", formatC(estimate, format="d", big.mark=",")), 20),
             race_ethnicity = ifelse(is.na(race_ethnicity), "All races", 
                                     ifelse(race_ethnicity == "Hispanic Or Latino", "Latinx", race_ethnicity)),
             year = lubridate::ymd(year, truncated = 2L)) %>%
      filter(race_ethnicity %in% c("All races", "White Alone", "Latinx"))
    
    income_plot <- income %>%
      ggplot(aes(year, estimate/1000, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      # geom_point(size = 1.4, shape = 21, stroke = 1.4, fill = "white") +
      scale_color_manual(values = c(hex_grey, hex_pink, hex_purple, hex_green, hex_blue_lt, hex_orange, hex_blue_dk, "black", "green", "orange")) +
      labs(color = "",
           y = str_wrap("Median household income (in thousands)", width = 25)) +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      time_ggtheme
    
    ggplotly(income_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
  })
  
  # output$time_homeownership <- renderPlot({
  #   
  #   homeownership <- ia_counties_temporal_tidy %>%
  #     filter(county_name %in% input$region_choice & substr(variable, 1, 6) == "B25003") %>%
  #     separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
  #     mutate(race_ethnicity = str_trim(str_to_title(str_remove_all(race_ethnicity, "HOUSEHOLDER|\\)"))),
  #            race_ethnicity = recode(race_ethnicity,
  #                                   `Hispanic or Latino` = "Latinx"),
  #            text = str_wrap(paste0(label, " in ", county_name, ": ", percent, "%"))) %>%
  #     filter(race_ethnicity %in% c("All Races", "White Alone", "Latinx"))
  #   
  #   homeownership_plot <- homeownership %>%
  #     filter(label == "Owner occupied" & !(race_ethnicity %in% c("Some Other Race Alone", "Two Or More Races", "White Alone", "Native Hawaiian And Other Pacific Islander Alone", "American Indian And Alaska Native Alone"))) %>%
  #     mutate(race_ethnicity = ifelse(is.na(race_ethnicity), "All races", race_ethnicity),
  #            year = lubridate::ymd(year, truncated = 2L)) %>%
  #     ggplot(aes(year, percent, color= race_ethnicity, group = race_ethnicity, text = text)) +
  #     geom_line(size = 0.8) +
  #     geom_point(size = 1.4, shape = 21, stroke = 1.4, fill = "white") +
  #     scale_color_manual(values = c(hex_grey, hex_pink, hex_purple, hex_green, hex_blue_lt, hex_orange, hex_blue_dk)) +
  #     labs(color = "",
  #          y = str_wrap("% of households in race/ethnicity that are home-owning", width = 35)) +
  #     theme(legend.position = "bottom") +
  #     facet_wrap(~county_name, ncol = 2) +
  #     guides(color = guide_legend(ncol = 3,
  #                                 bycol = TRUE)) +
  #     time_ggtheme
  #   
  #   homeownership_plot
  #   
  #   # ggplotly(homeownership_plot, tooltip = "text") %>%
  #   #   layout(font = list(family = "Karla"),
  #   #          legend = list(orientation = "h", x = 0.4, y = -0.2)) %>%
  #   #   style(hoverlabel = list(bgcolor = "#172B4D",
  #   #                           bordercolor = "#172B4D",
  #   #                           font = list(family = "Karla", color = "white")))
  #   
  # })
  
  output$time_homeownership <- renderPlotly({

    homeownership <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & substr(variable, 1, 6) == "B25003") %>%
      separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
      mutate(race_ethnicity = str_trim(str_to_title(str_remove_all(race_ethnicity, "HOUSEHOLDER|\\)"))),
             text = str_wrap(paste0(round(percent, 1), "%")),
             race_ethnicity = ifelse(is.na(race_ethnicity), "All races",
                                     ifelse(race_ethnicity == "Hispanic Or Latino", "Latinx", race_ethnicity)),
             year = lubridate::ymd(year, truncated = 2L)) %>%
      filter(race_ethnicity %in% c("All races", "White Alone", "Latinx") & label == "Owner occupied")

    homeownership_plot <- homeownership %>%
      ggplot(aes(year, percent, color= race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      # geom_point(size = 1.4, shape = 21, stroke = 1.4, fill = "white") +
      scale_color_manual(values = c(hex_grey, hex_pink, hex_purple, hex_green, hex_blue_lt, hex_orange, hex_blue_dk)) +
      labs(color = "",
           y = "") + #str_wrap("% of households in race/ethnicity that are home-owning", width = 35)) +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      time_ggtheme


    ggplotly(homeownership_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)

  })
  
  employment <- reactive({
    
    employment <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & substr(variable, 1, 6) == "B20005" & variable_index %in% c("002", "003", "049", "050")) %>%
      mutate(label = str_replace(label, ":", " "),
             gender = word(label, start = 1, end = 1),
             denom = ifelse((gender == "Male" & variable_index == "002") | (gender == "Female" & variable_index == "049"), estimate, NA)) %>%
      group_by(variable_group, gender, county_name, year) %>%
      fill(denom, .direction = "down") %>%
      ungroup() %>%
      mutate(prop = estimate/denom,
             percent = prop*100) %>%
      filter(!(variable_index %in% c("002", "049"))) %>%
      mutate(concept = str_replace(concept,"\\(IN 2019 INFLATION-ADJUSTED DOLLARS\\)", "")) %>%
      separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
      mutate(race_ethnicity = str_trim(str_to_title(str_remove_all(race_ethnicity, "\\)"))),
             text = str_wrap(paste0(round(percent), "%")),
             race_ethnicity = ifelse(is.na(race_ethnicity), "All races", 
                                     ifelse(race_ethnicity == "Hispanic Or Latino", "Latinx", race_ethnicity)),
             year = lubridate::ymd(year, truncated = 2L)) %>%
      filter(race_ethnicity %in% c("All races", "White Alone", "Latinx"))
    
  })
  
  output$time_employment_female <- renderPlotly({
    
    employ_f_plot <- employment() %>%
      filter(gender == "Female") %>%
      ggplot(aes(year, percent, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      # geom_point(size = 1.4, shape = 21, stroke = 1.4, fill = "white") +
      scale_color_manual(values = c(hex_grey, hex_pink, hex_purple, hex_green, hex_blue_lt, hex_orange, hex_blue_dk)) +
      labs(color = "",
           # y = str_wrap("% of women that were employed full-time in the last 12 months", width = 35)) +
           y = "") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      time_ggtheme
    
    ggplotly(employ_f_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  output$time_employment_male <- renderPlotly({
    
    employ_m_plot <- employment() %>%
      filter(gender == "Male") %>%
      ggplot(aes(year, percent, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      # geom_point(size = 1.4, shape = 21, stroke = 1.4, fill = "white") +
      scale_color_manual(values = c(hex_grey, hex_pink, hex_purple, hex_green, hex_blue_lt, hex_orange, hex_blue_dk)) +
      labs(color = "",
           # y = str_wrap("% of men that were employed full-time in the last 12 months", width = 35)) +
           y = "") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      time_ggtheme
    
    ggplotly(employ_m_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
    
  })
  
  
  # output$bar_median_age <- renderPlotly({
  #   
  #   median_age <- state_county %>%
  #     filter(variable == "B01002I_001" & NAME %in% input$region_choice) %>%
  #     mutate(text = paste(NAME, ": ", estimate, " years"))
  #   
  #   age_plot <- ggplot(median_age, aes(NAME, estimate, text = text)) +
  #     geom_bar(stat = "identity", width = 0.1, fill = "#5E72E4") +
  #     # geom_text(aes(x = NAME, y = estimate), size = 3.5, family = "Karla") +
  #     labs(x = "", y = "Median age of Latinx pop. (in years)") +
  #     scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  #     theme_minimal() +
  #     theme(panel.grid = element_blank(),
  #           axis.line.x = element_line(color = "#cacee6", size = 0.8))
  #   
  #   ggplotly(age_plot, tooltip = "text") %>%
  #     layout(font = list(family = "Karla")) %>%
  #     style(hoverlabel = list(bgcolor = "#172B4D",
  #                             bordercolor = "#172B4D",
  #                             font = list(family = "Karla", color = "white")))
  # })
  
  # output$lollipop_income <- renderPlotly({
  #   
  #   income <- state_county %>%
  #     filter(variable == "B19013I_001" & NAME %in% input$region_choice) %>%
  #     mutate(text = paste0(NAME, ": $", format(estimate, big.mark = ",")))
  #   
  #   income_plot <- ggplot(income, aes(text = text)) +
  #     geom_linerange(aes(x = NAME, ymin = 0, ymax = estimate), color = hex_blue_dk, size = 1)+
  #     geom_point(aes(x = NAME, y = estimate), color = hex_blue_dk, size = 2.5) +
  #     labs(x = "",
  #          y = "Median household income\n\n",
  #          color = "") +
  #     coord_flip() +
  #     theme_minimal() +
  #     theme(panel.grid = element_blank(),
  #           legend.position = "bottom",
  #           axis.title = element_text(color = hex_grey))
  #   
  #   ggplotly(income_plot, tooltip = "text") %>%
  #     layout(font = list(family = "Karla")) %>%
  #     style(hoverlabel = list(bgcolor = "#172B4D",
  #                             bordercolor = "#172B4D",
  #                             font = list(family = "Karla", color = "white")))
  # })
  
}