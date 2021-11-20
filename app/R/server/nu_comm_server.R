nu_comm_server <- function(input, output, session) {
  
  output$time_income <- renderPlotly({
    
    p <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & category == "income") %>%
      ggplot(aes(year, estimate/1000, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = c(hex_grey, hex_green, hex_purple),
                         breaks = c("All races", "Latinx", "White Alone")) +
      labs(color = "",
           x = "",
           y = "Median household income (in $K)") +
      theme(legend.position = "bottom") +
      facet_wrap(~county_name, ncol = 2) +
      guides(color = guide_legend(ncol = 3,
                                  bycol = TRUE)) +
      expand_limits(y = 0) +
      time_ggtheme
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             margin = list(b = 80, 
                           l = 80), # otherwise the y axis label gets cut off
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
  })
  
  output$time_homeownership <- renderPlotly({
    
    p <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & category == "homeownership" & disaggregation == "Owners") %>%
      ggplot(aes(year, estimate, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
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
    
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
  })
 
  output$time_employment_female <- renderPlotly({
    
    p <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & category == "employment" & disaggregation == "Female") %>%
      ggplot(aes(year, estimate, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
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
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
  })
  
  output$time_employment_male <- renderPlotly({
    
    p <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & category == "employment" & disaggregation == "Male") %>%
      ggplot(aes(year, estimate, color = race_ethnicity, group = race_ethnicity, text = text)) +
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
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
    
  })
  
  output$time_poverty <- renderPlotly({
    
    p <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & category == "poverty") %>%
      ggplot(aes(year, estimate, color = race_ethnicity, group = race_ethnicity, text = text)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = c(hex_grey, hex_green, hex_purple),
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
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
    
  })
  
  output$bachelors_male_disparities <- renderPlotly({
    
    p <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & category == "bachelors" & disaggregation == "Male") %>%
      ggplot(aes(year, estimate, color = race_ethnicity, group = race_ethnicity, text = text)) +
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
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             hovermode = "x") %>%
      style(hoverlabel = list(bgcolor = hex_blue_dk,
                              font = list(family = "Karla", color = "white"))) %>%
      config(displayModeBar = F)
    
    
  })
  
  output$bachelors_female_disparities <- renderPlotly({
    
    p <- ia_counties_temporal_tidy %>%
      filter(county_name %in% input$region_choice & category == "bachelors" & disaggregation == "Female") %>%
      ggplot(aes(year, estimate, color = race_ethnicity, group = race_ethnicity, text = text)) +
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
    
    ggplotly(p, tooltip = "text") %>%
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
      filter(year(year) < 2020) %>%
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