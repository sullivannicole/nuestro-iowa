nu_future_server <- function(input, output, session) {
  
  #-------------Reactive df and vector outputs--------------
  
  # Attainment
  edu_df <- reactive({
    
    current_edu %>%
      mutate(edu_after_invest = case_when(level == "lt_hs" ~ n_latinx - input$hs_grads,
                                          level == "hs" ~ n_latinx + input$hs_grads - input$hs_to_assoc - input$hs_to_bach,
                                          level == "assoc" ~ n_latinx + input$hs_to_assoc - input$assoc_to_bach,
                                          level == "bach" ~ n_latinx + input$assoc_to_bach + input$hs_to_bach),
             change_in_n = n_latinx - edu_after_invest,
             pc_attain = edu_after_invest/sum(edu_after_invest)*100)
    
  })
  
  # Projected income
  
  total_increase <- reactive({ 
    
    total_undoc <- 8252
    
    hs_increase <- input$hs_grads * 2644
    associates_increase <- input$hs_to_assoc * 6093
    bachelors_increase <- (input$assoc_to_bach * 14807) + (input$hs_to_bach * 20900)
    undoc_increase <- if (input$undoc_to_citizens) total_undoc * 5659 else 0
    
    hs_increase + associates_increase + bachelors_increase + undoc_increase 
    
  })
  
  per_capita_income_df <- reactive({
    
    total_dollars_earned <- 996866128 + total_increase()
    per_capita_income <- total_dollars_earned/28455
    
    data.frame(race_ethnicity = c(race_ethnicity_vctr, "Latino"),
               category = c("Your choice", rep("Current state", 2)),
               income = c(per_capita_income, 53071, 35033)) %>%
      mutate(text = paste0(category, "\n", race_ethnicity, ": $", formatC(round(income, 1), format="d", big.mark=",")))
    
    
  })
  
  # Per capita
  # total_investment_value <- ((total_increase * (1.02)^30 - total_increase)/1.9662316753) * 30 + (total_increase * 30)
  # total_residents_affected <- if (undoc_to_citizen == "Yes") total_undoc + hs_grads + hs_to_assoc + assoc_to_bach + hs_to_bach else hs_grads + hs_to_assoc + assoc_to_bach + hs_to_bach
  # per_person_value_30_yrs <- total_investment_value/total_residents_affected
  
  #---------- Plots ------------------
  
  # Per capita income
  
  output$per_capita_income_number <- renderText({
    
    per_capita_income_future <- per_capita_income_df() %>%
      filter(race_ethnicity == "Latino" & category == "Your choice")
    
    # Population with commas
    paste0("+$", formatC(per_capita_income_future$income - 35033, format="d", big.mark=","))
    
    
  })
  
  output$plot_per_cap_inc <- renderPlotly({
    
    p <- ggplot(per_capita_income_df(), aes(race_ethnicity, income/1000, text = text, fill = category)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.2) +
      scale_fill_manual(values = c(hex_green, hex_purple)) +
      labs(#title = "Per Capita Income (in $1K)",
        x = "",
        y = "") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      project_ggtheme_y +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             height = 200#,
             # legend = list(orientation = "h", x = 0.4, y = -0.25)
      ) %>%
      style(hoverlabel = list(bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  attainment_df <- reactive({
    
    expand_grid(race_ethnicity = race_ethnicity_vctr,
                edu_level = factor(c("< HS", "HS", "Associate's", "Bachelor's"), levels = c("< HS", "HS", "Associate's", "Bachelor's"))) %>%
      mutate(attainment = c(edu_df()$pc_attain, 4.8, 46.5, 11, 37.7),
             text = paste0(edu_level, ": ", round(attainment, 1), "%"))
    
  })
  
  output$attainment_number <- renderText({
    
    attainment_future <- attainment_df() %>%
      filter(race_ethnicity == "Latino" & edu_level != "< HS") %>%
      summarize(at_or_above_hs = sum(attainment))
    
    paste0("+", round(attainment_future$at_or_above_hs - 64.6, 1), "%")
    
  })
  
  # Attainment
  output$plot_attainment <- renderPlotly({
    
    # Educational attainment bar
    p <- ggplot(attainment_df(), aes(race_ethnicity, attainment, fill = edu_level, text = text)) +
      geom_bar(stat = "identity", width = 0.15) +
      scale_fill_manual(values = c(hex_pink, hex_purple, hex_blue_lt, hex_green)) +
      # scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(#title = "Educational Attainment",
        x = "",
        y = "",
        fill = "") +
      project_ggtheme_y +
      theme(legend.position = "none") +
      coord_flip()
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             height = 200
      ) %>%
      style(hoverlabel = list(bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
    
  })
  
  
  total_addtl_dollars <- reactive({
    
    # Total addtl dollars per year line graph
    yrs_out <- 30
    total_per_year <- c(total_increase(), total_increase() * cumprod(rep(1.02, yrs_out)))
    
    data.frame(year = seq.Date(ymd("2021-01-01"), ymd("2021-01-01") + years(yrs_out), by = "years"),
               total_addtl = total_per_year) %>%
      mutate(text = paste0(year(year), ": $", round(total_addtl/1000000, 2), "M"))
    
  })
  
  output$total_addtl_dollars_number <- renderText({
    
    total_dollars_future <- total_addtl_dollars() %>%
      summarize(total_over_30 = sum(total_addtl))
    
    # Population with commas
    paste0("$", format(round(total_dollars_future$total_over_30/1000000, 2), format = "d", big.mark = ","), "M")
    
    
  })
  
  output$plot_addtl_dollars <- renderPlotly({
    
    
    p <- ggplot(total_addtl_dollars(), aes(year, total_addtl/1000000, text = text, group = 1)) +
      geom_area(alpha = 0.2, fill = hex_purple) +
      geom_line(color =  hex_purple, size = 1.2) +
      labs(#title = str_to_upper("Additional Latinx Earnings (in $M)"),
        y = "",
        x = "") +
      project_ggtheme_y +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank())
      # theme(axis.text = element_blank(),
      #       axis.ticks = element_blank(),
      #       panel.background =  element_rect(fill = "white"))
      
    #scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    #dk_ggtheme
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             
             height = 200
      ) %>%
      style(hoverlabel = list(bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  homeownership <- reactive({
    
    homeowner_df <- current_hhs %>%
      mutate(edu_after_invest = case_when(level == "lt_hs" ~ n_latinx - input$hs_grads,
                                          level == "hs" ~ n_latinx + input$hs_grads - input$hs_to_assoc - input$hs_to_bach,
                                          level == "assoc" ~ n_latinx + input$hs_to_assoc - input$assoc_to_bach,
                                          level == "bach" ~ n_latinx + input$assoc_to_bach + input$hs_to_bach),
             change_in_edu = n_latinx - edu_after_invest,
             prob_of_home_bach_lt_hs =  prob_of_home_bach - prob_of_home_lt_hs,
             change_in_home_given_edu = case_when(level == "lt_hs" ~ 0,
                                                  level == "hs" ~ (prob_of_home_hs - prob_of_home_lt_hs) * input$hs_grads,
                                                  level == "assoc" ~ (prob_of_home_assoc - prob_of_home_lt_hs) * input$hs_to_assoc,
                                                  level == "bach" ~ (prob_of_home_bach_lt_hs * input$assoc_to_bach) + (prob_of_home_bach_lt_hs * input$hs_to_bach) ),
             current_homeowners = sum(n_homeowners),
             current_renters = sum(n_latinx - n_homeowners),
             addtl_homeowners = sum(change_in_home_given_edu),
             homeowners_with_investments = current_homeowners + addtl_homeowners,
             renters_with_investments = sum(n_latinx) - homeowners_with_investments,
             homeowners_base = homeowners_with_investments/(homeowners_with_investments + renters_with_investments),
             homeownership_rate = ifelse(input$undoc_to_citizens, (homeowners_base + 0.087) * 100, homeowners_base * 100)) # 0.087 = undoc boost
    
    
    homeowner_rate <- unique(homeowner_df$homeownership_rate)
    
    data.frame(race_ethnicity = c(race_ethnicity_vctr, "Latino"),
               category = c(rep("Your choice", 2), rep("Current state", 4)),
               tenure = c(rep("Homeowners", 3), rep("Renters", 3)),
               pc = c(homeowner_rate, 73.7, 48.2, 100 - homeowner_rate, 26.3, 100 - 48.2)) %>%
      mutate(text = paste0(category, "\n", race_ethnicity, " ", tenure, ": ", round(pc, 1), "%")) %>%
      filter(tenure == "Homeowners")
    
  })
  
  output$homeownership_number <- renderText({
    
    homeownership_future <- homeownership() %>%
      filter(race_ethnicity == "Latino" & category == "Your choice")
    
    paste0("+", round(homeownership_future$pc - 48.2, 1), "%")
    
  })
  
  output$plot_homeownership <- renderPlotly({
    
    
    p <- ggplot(homeownership(), aes(race_ethnicity, pc, fill = category, text = text)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.2) +
      scale_fill_manual(values = c(hex_blue_lt, hex_blue_dk)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(#title = "Homeownership rates",
        x = "",
        y = "",
        fill = "") +
      project_ggtheme_y +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Karla"),
             legend = list(orientation = "h", x = 0.4, y = -0.25),
             height = 200
      ) %>%
      style(hoverlabel = list(bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  
  #------------------ Archive ------------------------
  # output$income_disparities <- renderPlotly({
  #   
  #   
  #   income_df <- disparities %>%
  #     filter(year >= 2021) %>%
  #     select(year, per_capita_income, per_capita_income_eliminating_disparities) %>%
  #     mutate(per_capita_income_eliminating_disparities = per_capita_income + (per_capita_income_eliminating_disparities-per_capita_income)*input$income_knob/100,
  #            text1 = paste0("With disparities: $", format(per_capita_income, big.mark = ",")),
  #            text2 = paste0("Year: ", year, "\nWithout disparities: $", format(per_capita_income_eliminating_disparities, big.mark = ",")))
  #   
  #   income_plot <- ggplot(income_df, aes(year, per_capita_income_eliminating_disparities)) +
  #     geom_line(aes(y = per_capita_income, text = text1, color = "With disparities \n(current trajectory)", group = 1), size = 1.2) +
  #     geom_line(aes(y = per_capita_income_eliminating_disparities, text = text2, color = "Without disparities", group = 1), size = 1.2) +
  #     scale_color_manual(values = c(hex_pink, hex_green)) +
  #     geom_ribbon(aes(ymin = per_capita_income, ymax = per_capita_income_eliminating_disparities), alpha = 0.2, fill = hex_green) +
  #     theme(legend.position = "bottom") +
  #     labs(y = "Per capita income (USD)\n",
  #          color = "") +
  #     ylim(10000, 75000) +
  #     # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
  #     project_ggtheme
  #   
  #   ggplotly(income_plot, tooltip = 'text') %>%
  #     config(displayModeBar = F) %>%
  #     layout(legend = list(orientation = "h", x = 0.4, y = -0.25),
  #            font = list(family = "Karla"),
  #            hovermode = "x",
  #            dragmode = "select",
  #            height = 500) %>%
  #     style(hoverlabel = list(bordercolor = "white",
  #                             font = list(family = "Karla", color = "white"))) %>% 
  #     config(displayModeBar = F)
  #   
  # })
  # 
  # output$earnings_disparities <- renderPlotly({
  #   
  #   
  #   dollars_df <- disparities %>%
  #     filter(year >= 2021) %>%
  #     select(year, latinx_dollars_earned, dollars_earned_eliminate_disparities) %>%
  #     mutate(latinx_dollars_earned = latinx_dollars_earned/1000000000,
  #            dollars_earned_eliminate_disparities = dollars_earned_eliminate_disparities/1000000000,
  #            dollars_earned_eliminate_disparities = latinx_dollars_earned + (dollars_earned_eliminate_disparities-latinx_dollars_earned)*input$earnings_knob/100,
  #            text1 = paste0("With disparities: $", format(round(latinx_dollars_earned, 2), big.mark = ",")),
  #            text2 = paste0("Year: ", year, "\nWithout disparities: $", format(round(dollars_earned_eliminate_disparities, 2), big.mark = ",")))
  #   
  #   dollars_plot <- ggplot(dollars_df, aes(year, latinx_dollars_earned)) +
  #     geom_line(aes(y = latinx_dollars_earned, text = text1, color = "With disparities \n(current trajectory)", group = 1), size = 1.2) +
  #     geom_line(aes(y = dollars_earned_eliminate_disparities, text = text2, color = "Without disparities", group = 1), size = 1.2) +
  #     scale_color_manual(values = c(hex_pink, hex_green)) +
  #     geom_ribbon(aes(ymin = latinx_dollars_earned, ymax = dollars_earned_eliminate_disparities), alpha = 0.2, fill = hex_green) +
  #     theme(legend.position = "bottom") +
  #     labs(y = "Latinx dollars earned (in $B)\n",
  #          color = "") +
  #     ylim(0.9, 9) +
  #     # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
  #     project_ggtheme
  #   
  #   ggplotly(dollars_plot, tooltip = 'text') %>%
  #     config(displayModeBar = F) %>%
  #     layout(legend = list(orientation = "h", x = 0.4, y = -0.25),
  #            font = list(family = "Karla"),
  #            hovermode = "x",
  #            dragmode = "select",
  #            height = 500) %>%
  #     style(hoverlabel = list(bordercolor = "white",
  #                             font = list(family = "Karla", color = "white"))) %>% 
  #     config(displayModeBar = F)
  #   
  # })
  # 
  # output$poverty_disparities <- renderPlotly({
  #   
  #   
  #   poverty_df <- disparities %>%
  #     filter(year >= 2021) %>%
  #     select(year, latinx_people_in_poverty, poverty_with_disparities_eliminated) %>%
  #     mutate(poverty_with_disparities_eliminated = latinx_people_in_poverty + (poverty_with_disparities_eliminated-latinx_people_in_poverty)*input$poverty_knob/100,
  #            text1 = paste0("With disparities: ", format(latinx_people_in_poverty, big.mark = ","), " Latinx individuals"),
  #            text2 = paste0("Year: ", year, "\nWithout disparities: ", format(poverty_with_disparities_eliminated, big.mark = ","), " Latinx individuals"))
  #   
  #   poverty_plot <- ggplot(poverty_df, aes(year, latinx_people_in_poverty)) +
  #     geom_line(aes(y = latinx_people_in_poverty, color = "With disparities \n(current trajectory)", text = text1, group = 1), size = 1.2) +
  #     geom_line(aes(y = poverty_with_disparities_eliminated, color = "Without disparities", text = text2, group = 1), size = 1.2) +
  #     scale_color_manual(values = c(hex_pink, hex_green)) +
  #     geom_ribbon(aes(ymin = latinx_people_in_poverty, ymax = poverty_with_disparities_eliminated), alpha = 0.2, fill = hex_green) +
  #     theme(legend.position = "bottom") +
  #     labs(y = "Latinx individuals in poverty\n",
  #          color = "") +
  #     ylim(3500, 24000) +
  #     project_ggtheme
  #   
  #   ggplotly(poverty_plot, tooltip = 'text') %>%
  #     config(displayModeBar = F) %>%
  #     layout(legend = list(orientation = "h", x = 0.4, y = -0.25),
  #            font = list(family = "Karla"),
  #            hovermode = "x",
  #            dragmode = "select",
  #            height = 500) %>%
  #     style(hoverlabel = list(bordercolor = "white",
  #                             font = list(family = "Karla", color = "white"))) %>% 
  #     config(displayModeBar = F)
  #   
  # })
  # 
  # output$homeowners_disparities <- renderPlotly({
  #   
  #   
  #   homeowners_df <- disparities %>%
  #     filter(year >= 2021) %>%
  #     select(year, latinx_homeowners, homeowners_eliminate_disparities) %>%
  #     mutate(homeowners_eliminate_disparities = latinx_homeowners + (homeowners_eliminate_disparities-latinx_homeowners)*input$homeowners_knob/100,
  #            text1 = paste0("With disparities: ", format(latinx_homeowners, big.mark = ","), " Latinx homeowners"),
  #            text2 = paste0("Year: ", year, "\nWithout disparities: ", format(homeowners_eliminate_disparities, big.mark = ","), " Latinx homeowners"))
  #   
  #   homeowners_plot <- ggplot(homeowners_df, aes(year, latinx_homeowners)) +
  #     geom_line(aes(y = latinx_homeowners, color = "With disparities \n(current trajectory)", text = text1, group = 1), size = 1.2) +
  #     geom_line(aes(y = homeowners_eliminate_disparities, color = "Without disparities", text = text2, group = 1), size = 1.2) +
  #     scale_color_manual(values = c(hex_pink, hex_green)) +
  #     geom_ribbon(aes(ymin = latinx_homeowners, ymax = homeowners_eliminate_disparities), alpha = 0.2, fill = hex_green) +
  #     theme(legend.position = "bottom") +
  #     labs(y = "Latinx individuals in poverty\n",
  #          color = "") +
  #     ylim(6200, 22000) +
  #     project_ggtheme
  #   
  #   ggplotly(homeowners_plot, tooltip = 'text') %>%
  #     config(displayModeBar = F) %>%
  #     layout(legend = list(orientation = "h", x = 0.4, y = -0.25),
  #            font = list(family = "Karla"),
  #            hovermode = "x",
  #            dragmode = "select",
  #            height = 500) %>%
  #     style(hoverlabel = list(#bgcolor = hex_purple,
  #       bordercolor = "white",
  #       font = list(family = "Karla", color = "white"))) %>% 
  #     config(displayModeBar = F)
  #   
  # })
  # 
  # output$homeowners_disparities2 <- renderPlot({
  #   
  #   
  #   homeowners_df <- disparities %>%
  #     filter(year >= 2021) %>%
  #     select(year, latinx_homeowners, homeowners_eliminate_disparities) %>%
  #     mutate(homeowners_eliminate_disparities = latinx_homeowners + 
  #              (homeowners_eliminate_disparities-latinx_homeowners)*input$homeowners_knob/100)
  #   
  #   ggplot(homeowners_df, aes(year, latinx_homeowners)) +
  #     geom_line(aes(y = latinx_homeowners, color = "With disparities \n(current trajectory)"), size = 1.2) +
  #     geom_line(aes(y = homeowners_eliminate_disparities, color = "Without disparities"), size = 1.2) +
  #     scale_color_manual(values = c(hex_pink, hex_green)) +
  #     geom_ribbon(aes(ymin = latinx_homeowners, ymax = homeowners_eliminate_disparities), alpha = 0.2, fill = hex_green) +
  #     theme(legend.position = "bottom") +
  #     labs(y = "Latinx individuals in poverty\n",
  #          color = "") +
  #     ylim(6200, 22000) +
  #     # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
  #     project_ggtheme
  #   
  # })
}