nu_comm_server <- function(input, output, session) {
  
  output$time_income <- renderPlotly({
    
    income <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & substr(variable, 1, 6) == "B19013") %>%
      mutate(concept = str_replace(concept,"\\(IN 2019 INFLATION-ADJUSTED DOLLARS\\)", "")) %>%
      separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
      mutate(race_ethnicity = str_trim(str_to_title(str_remove_all(race_ethnicity, "HOUSEHOLDER|\\)"))),
             race_ethnicity = ifelse(is.na(race_ethnicity), "All races", 
                                     ifelse(race_ethnicity == "Hispanic Or Latino", "Latinx", race_ethnicity)),
             text = str_wrap(paste0(race_ethnicity, ": $", formatC(estimate, format="d", big.mark=",")), 20),
             year = lubridate::ymd(year, truncated = 2L)) %>%
      filter(race_ethnicity %in% c("All races", "White Alone", "Latinx"))
    
    income_plot <- income %>%
      ggplot(aes(year, estimate/1000, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      # geom_point(size = 1.4, shape = 21, stroke = 1.4, fill = "white") +
      scale_color_manual(values = c(hex_grey, hex_green, hex_purple),
                         breaks = c("All races", "Latinx", "White Alone")) +
      labs(color = "",
           x = "",
           y = "Median household income (in thousands)") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      expand_limits(y = 0) +
      time_ggtheme
    
    ggplotly(income_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
  })
  
  output$time_homeownership <- renderPlotly({
    
    homeownership <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & substr(variable, 1, 6) == "B25003") %>%
      separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
      mutate(race_ethnicity = str_trim(str_to_title(str_remove_all(race_ethnicity, "HOUSEHOLDER|\\)"))),
             race_ethnicity = ifelse(is.na(race_ethnicity), "All races",
                                     ifelse(race_ethnicity == "Hispanic Or Latino", "Latinx", race_ethnicity)),
             text = str_wrap(paste0(race_ethnicity, ": ", round(percent, 1), "%")),
             year = lubridate::ymd(year, truncated = 2L)) %>%
      filter(race_ethnicity %in% c("All races", "White Alone", "Latinx") & label == "Owner occupied")
    
    homeownership_plot <- homeownership %>%
      ggplot(aes(year, percent, color= race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      # geom_point(size = 1.4, shape = 21, stroke = 1.4, fill = "white") +
      scale_color_manual(values = c(hex_grey, hex_green, hex_purple),
                         breaks = c("All races", "Latinx", "White Alone")) +
      labs(color = "",
           y = "",
           x = "") + #str_wrap("% of households in race/ethnicity that are home-owning", width = 35)) +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      expand_limits(y = 0) +
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
      tidyr::fill(denom, .direction = "down") %>%
      ungroup() %>%
      mutate(prop = estimate/denom,
             percent = prop*100) %>%
      filter(!(variable_index %in% c("002", "049"))) %>%
      mutate(concept = str_replace(concept,"\\(IN 2019 INFLATION-ADJUSTED DOLLARS\\)", "")) %>%
      separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
      mutate(race_ethnicity = str_trim(str_to_title(str_remove_all(race_ethnicity, "\\)"))),
             race_ethnicity = ifelse(is.na(race_ethnicity), "All races",
                                     ifelse(race_ethnicity == "Hispanic Or Latino", "Latinx", race_ethnicity)),
             text = str_wrap(paste0(race_ethnicity, ": ", round(percent), "%")),
             year = lubridate::ymd(year, truncated = 2L)) %>%
      filter(race_ethnicity %in% c("All races", "White Alone", "Latinx"))
    
  })
  
  output$time_employment_female <- renderPlotly({
    
    employ_f_plot <- employment() %>%
      filter(gender == "Female") %>%
      ggplot(aes(year, percent, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      # geom_point(size = 1.4, shape = 21, stroke = 1.4, fill = "white") +
      scale_color_manual(values = c(hex_grey, hex_green, hex_purple),
                         breaks = c("All races", "Latinx", "White Alone")) +
      labs(color = "",
           # y = str_wrap("% of women that were employed full-time in the last 12 months", width = 35)) +
           y = "",
           x = "") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      expand_limits(y = 0) +
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
      scale_color_manual(values = c(hex_grey, hex_green, hex_purple),
                         breaks = c("All races", "Latinx", "White Alone")) +
      labs(color = "",
           y = "",
           x = "") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      expand_limits(y = 0) +
      time_ggtheme
    
    ggplotly(employ_m_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
    
  })
  
  output$time_poverty <- renderPlotly({
    
    poverty_disp <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & substr(variable, 1, 6) == "B17020" & variable_index == "002") %>%
      separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
      mutate(race_ethnicity = str_remove(str_to_title(race_ethnicity), "\\)"),
             race_ethnicity = ifelse(is.na(race_ethnicity), "All races",
                                     ifelse(race_ethnicity == "Hispanic Or Latino", "Latinx", race_ethnicity)),
             text = str_wrap(paste0(race_ethnicity, ": ", round(percent, 1), "%"), 20),
             year = lubridate::ymd(year, truncated = 2L))
    
    poverty_disp_plot <- poverty_disp %>%
      ggplot(aes(year, percent, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = c(hex_green, hex_purple),
                         breaks = c("Latinx", "White Alone")) +
      labs(color = "",
           y = "",
           x = "") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      expand_limits(y = 0) +
      time_ggtheme
    
    ggplotly(poverty_disp_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
    
  })
  
  bachelors <- reactive({
    
    ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & substr(variable, 1, 6) == "C15002" & variable_index %in% c("006", "011")) %>%
      separate(concept, into = c("concept", "race_ethnicity"), sep = "\\(", remove = F) %>%
      mutate(label = str_replace(label, ":", ", "),
             race_ethnicity = str_remove(str_to_title(race_ethnicity), "\\)"),
             race_ethnicity = ifelse(is.na(race_ethnicity), "All races",
                                     ifelse(race_ethnicity == "Hispanic Or Latino", "Latinx", race_ethnicity)),
             text = str_wrap(paste0(race_ethnicity, ": ", round(percent, 1), "%"), 20),
             year = lubridate::ymd(year, truncated = 2L))
    
  })
  
  output$bachelors_male_disparities <- renderPlotly({
    
    bachelors_m_plot <- bachelors() %>%
      filter(variable_index == "006") %>%
      ggplot(aes(year, percent, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = c(hex_green, hex_purple),
                         breaks = c("Latinx", "White Alone")) +
      labs(color = "",
           y = "",
           x = "") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      expand_limits(y = 0) +
      time_ggtheme
    
    ggplotly(bachelors_m_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
    
  })
  
  output$bachelors_female_disparities <- renderPlotly({
    
    bachelors_f_plot <- bachelors() %>%
      filter(variable_index == "011") %>%
      ggplot(aes(year, percent, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = c(hex_green, hex_purple),
                         breaks = c("Latinx", "White Alone")) +
      labs(color = "",
           y = "",
           x = "") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      expand_limits(y = 0) +
      time_ggtheme
    
    ggplotly(bachelors_f_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
    
  })
  
  output$hs_rates_disparities <- renderPlotly({
    
    grad_rates_plot <- hs_grad_rates %>%
      filter(NAME %in% input$region_choice) %>%
      mutate(year = as_date(ymd(year)),
             text = paste(year, ", ", text)) %>%
      ggplot(aes(year, average_rate, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = c(hex_grey, hex_green, hex_purple),
                         breaks = c("All races", "Latinx", "White")) +
      labs(color = "",
           y = "",
           x = "") +
      theme(legend.position = "bottom") +
      facet_wrap(~NAME, ncol = 2) +
      expand_limits(y = 0) +
      time_ggtheme
    
    ggplotly(grad_rates_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(#bgcolor = hex_blue_dk,
        font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
    
  })
  
  
  
}