nu_future_server <- function(input, output, session) {
  
  output$income_disparities <- renderPlotly({
    
    
    income_df <- disparities %>%
      filter(year >= 2021) %>%
      select(year, per_capita_income, per_capita_income_eliminating_disparities) %>%
      mutate(per_capita_income_eliminating_disparities = per_capita_income + (per_capita_income_eliminating_disparities-per_capita_income)*input$income_knob/100,
             text1 = paste0("With disparities: $", format(per_capita_income, big.mark = ",")),
             text2 = paste0("Year: ", year, "\nWithout disparities: $", format(per_capita_income_eliminating_disparities, big.mark = ",")))
    
    income_plot <- ggplot(income_df, aes(year, per_capita_income_eliminating_disparities)) +
      geom_line(aes(y = per_capita_income, text = text1, color = "With disparities \n(current trajectory)", group = 1), size = 1.2) +
      geom_line(aes(y = per_capita_income_eliminating_disparities, text = text2, color = "Without disparities", group = 1), size = 1.2) +
      scale_color_manual(values = c(hex_pink, hex_green)) +
      geom_ribbon(aes(ymin = per_capita_income, ymax = per_capita_income_eliminating_disparities), alpha = 0.2, fill = hex_green) +
      theme(legend.position = "bottom") +
      labs(y = "Per capita income (USD)\n",
           color = "") +
      ylim(10000, 75000) +
      # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
      project_ggtheme
    
    ggplotly(income_plot, tooltip = 'text') %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.25),
             font = list(family = "Karla"),
             hovermode = "x",
             dragmode = "select",
             height = 500) %>%
      style(hoverlabel = list(bordercolor = "white",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  output$earnings_disparities <- renderPlotly({
    
    
    dollars_df <- disparities %>%
      filter(year >= 2021) %>%
      select(year, latinx_dollars_earned, dollars_earned_eliminate_disparities) %>%
      mutate(latinx_dollars_earned = latinx_dollars_earned/1000000000,
             dollars_earned_eliminate_disparities = dollars_earned_eliminate_disparities/1000000000,
             dollars_earned_eliminate_disparities = latinx_dollars_earned + (dollars_earned_eliminate_disparities-latinx_dollars_earned)*input$earnings_knob/100,
             text1 = paste0("With disparities: $", format(round(latinx_dollars_earned, 2), big.mark = ",")),
             text2 = paste0("Year: ", year, "\nWithout disparities: $", format(round(dollars_earned_eliminate_disparities, 2), big.mark = ",")))
    
    dollars_plot <- ggplot(dollars_df, aes(year, latinx_dollars_earned)) +
      geom_line(aes(y = latinx_dollars_earned, text = text1, color = "With disparities \n(current trajectory)", group = 1), size = 1.2) +
      geom_line(aes(y = dollars_earned_eliminate_disparities, text = text2, color = "Without disparities", group = 1), size = 1.2) +
      scale_color_manual(values = c(hex_pink, hex_green)) +
      geom_ribbon(aes(ymin = latinx_dollars_earned, ymax = dollars_earned_eliminate_disparities), alpha = 0.2, fill = hex_green) +
      theme(legend.position = "bottom") +
      labs(y = "Latinx dollars earned (in $B)\n",
           color = "") +
      ylim(0.9, 9) +
      # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
      project_ggtheme
    
    ggplotly(dollars_plot, tooltip = 'text') %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.25),
             font = list(family = "Karla"),
             hovermode = "x",
             dragmode = "select",
             height = 500) %>%
      style(hoverlabel = list(bordercolor = "white",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  output$poverty_disparities <- renderPlotly({
    
    
    poverty_df <- disparities %>%
      filter(year >= 2021) %>%
      select(year, latinx_people_in_poverty, poverty_with_disparities_eliminated) %>%
      mutate(poverty_with_disparities_eliminated = latinx_people_in_poverty + (poverty_with_disparities_eliminated-latinx_people_in_poverty)*input$poverty_knob/100,
             text1 = paste0("With disparities: ", format(latinx_people_in_poverty, big.mark = ","), " Latinx individuals"),
             text2 = paste0("Year: ", year, "\nWithout disparities: ", format(poverty_with_disparities_eliminated, big.mark = ","), " Latinx individuals"))
    
    poverty_plot <- ggplot(poverty_df, aes(year, latinx_people_in_poverty)) +
      geom_line(aes(y = latinx_people_in_poverty, color = "With disparities \n(current trajectory)", text = text1, group = 1), size = 1.2) +
      geom_line(aes(y = poverty_with_disparities_eliminated, color = "Without disparities", text = text2, group = 1), size = 1.2) +
      scale_color_manual(values = c(hex_pink, hex_green)) +
      geom_ribbon(aes(ymin = latinx_people_in_poverty, ymax = poverty_with_disparities_eliminated), alpha = 0.2, fill = hex_green) +
      theme(legend.position = "bottom") +
      labs(y = "Latinx individuals in poverty\n",
           color = "") +
      ylim(3500, 24000) +
      project_ggtheme
    
    ggplotly(poverty_plot, tooltip = 'text') %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.25),
             font = list(family = "Karla"),
             hovermode = "x",
             dragmode = "select",
             height = 500) %>%
      style(hoverlabel = list(bordercolor = "white",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  output$homeowners_disparities <- renderPlotly({
    
    
    homeowners_df <- disparities %>%
      filter(year >= 2021) %>%
      select(year, latinx_homeowners, homeowners_eliminate_disparities) %>%
      mutate(homeowners_eliminate_disparities = latinx_homeowners + (homeowners_eliminate_disparities-latinx_homeowners)*input$homeowners_knob/100,
             text1 = paste0("With disparities: ", format(latinx_homeowners, big.mark = ","), " Latinx homeowners"),
             text2 = paste0("Year: ", year, "\nWithout disparities: ", format(homeowners_eliminate_disparities, big.mark = ","), " Latinx homeowners"))
    
    homeowners_plot <- ggplot(homeowners_df, aes(year, latinx_homeowners)) +
      geom_line(aes(y = latinx_homeowners, color = "With disparities \n(current trajectory)", text = text1, group = 1), size = 1.2) +
      geom_line(aes(y = homeowners_eliminate_disparities, color = "Without disparities", text = text2, group = 1), size = 1.2) +
      scale_color_manual(values = c(hex_pink, hex_green)) +
      geom_ribbon(aes(ymin = latinx_homeowners, ymax = homeowners_eliminate_disparities), alpha = 0.2, fill = hex_green) +
      theme(legend.position = "bottom") +
      labs(y = "Latinx individuals in poverty\n",
           color = "") +
      ylim(6200, 22000) +
      project_ggtheme
    
    ggplotly(homeowners_plot, tooltip = 'text') %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.25),
             font = list(family = "Karla"),
             hovermode = "x",
             dragmode = "select",
             height = 500) %>%
      style(hoverlabel = list(#bgcolor = hex_purple,
        bordercolor = "white",
        font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  output$homeowners_disparities2 <- renderPlot({
    
    
    homeowners_df <- disparities %>%
      filter(year >= 2021) %>%
      select(year, latinx_homeowners, homeowners_eliminate_disparities) %>%
      mutate(homeowners_eliminate_disparities = latinx_homeowners + 
               (homeowners_eliminate_disparities-latinx_homeowners)*input$homeowners_knob/100)
    
    ggplot(homeowners_df, aes(year, latinx_homeowners)) +
      geom_line(aes(y = latinx_homeowners, color = "With disparities \n(current trajectory)"), size = 1.2) +
      geom_line(aes(y = homeowners_eliminate_disparities, color = "Without disparities"), size = 1.2) +
      scale_color_manual(values = c(hex_pink, hex_green)) +
      geom_ribbon(aes(ymin = latinx_homeowners, ymax = homeowners_eliminate_disparities), alpha = 0.2, fill = hex_green) +
      theme(legend.position = "bottom") +
      labs(y = "Latinx individuals in poverty\n",
           color = "") +
      ylim(6200, 22000) +
      # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
      project_ggtheme
    
  })
}