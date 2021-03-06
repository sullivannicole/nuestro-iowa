#---------------------Begin Tab 2 Alt A-----------------------------

nu_county_server <- function(input, output, session) {
  
  # Header
  output$highlighted_map <- renderPlot({
    
    ia_county_shp %>%
      mutate(input_status = ifelse(NAME == input$county_choice2, "1", "0")) %>%
      ggplot(aes(fill = input_status)) +
      geom_sf(color = hex_blue_contrast) +
      scale_fill_manual(values = c(hex_blue_contrast, hex_purple)) +
      theme_void() +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "transparent", color = "transparent"),
            plot.background = element_rect(fill = "transparent", color = "transparent"))
    
  }, bg ="transparent", height = 400, width = 500)
  
  output$region_pop <- renderText({
    
    county_selected <- ia_counties_tidy %>%
      filter(county_name == input$county_choice2 & variable_group == "B01001" & variable_index == "001")
    
    # Population with commas
    formatC(county_selected$estimate, format="d", big.mark=",")
    
    
  })
  
  
  output$region_latinx_pop <- renderText({
    
    county_selected <- ia_counties_tidy %>%
      filter(county_name == input$county_choice2 & variable_group == "B01001I" & variable_index == "001")
    
    # Population with commas
    formatC(county_selected$estimate, format="d", big.mark=",")
    
    
  })
  
  output$region_pc_latinx <- renderText({
    
    total_pop <- ia_counties_tidy %>%
      filter(county_name == input$county_choice2 & variable_group == "B01001" & variable_index == "001")
    
    latinx_pop <- ia_counties_tidy %>%
      filter(county_name == input$county_choice2 & variable_group == "B01001I" & variable_index == "001")
    
    # Population with commas
    round(latinx_pop$estimate/total_pop$estimate*100, 1)
    
    
  })
  
  # County overviews
  output$county_overview <- renderText({
    
    if (input$county_choice2 == "Polk") {
      
      "Polk County is home to the City of Des Moines and the largest population within the Metro, including the largest Latino population with over 40,000 residents. 
      On average, Latino household incomes and homeownership rates are lower in Polk County when compared with Marshall and Dallas County. 
      There are notable disparities in income, homeownership, poverty, and educational attainment between Latino residents and non-Latino white residents in Polk County."
      
    } else if(input$county_choice2 == "Dallas") {
      
      "Within Dallas County, a significant number of Latino residents live in the City of Perry and the Western Des Moines suburbs. 
      On average, Dallas County Latino residents have higher incomes and higher homeownership rates compared with the metro overall. There are more residents with origins in Central America as a proportion of overall Latino residents compared with the overall metro. 
      While Latino incomes in Dallas County are higher on average, there are disparities when comparing incomes to white, non-Latino residents."
      
    } else if(input$county_choice2 == "Marshall") {
      
      "Marshall County is home to a large population of Latino residents, mostly centered in and around the City of Marshalltown. 
      There are more Mexicans as a proportion of the total Latino population compared with the rest of the metro. Latino incomes and homeownership rates are higher on average than the overall metro. There are no disparities in income, homeownership rates, and high school graduation rates between Latino and non-Latino white residents in Marshall County; the gap has closed entirely in recent years. 
      However, there continue to be gaps in health insurance coverage, educational attainment, and poverty rates."
      
    } else if(input$county_choice2 == "Warren") {
      
      "Warren County is to the south of Polk County and has a relatively small Latino population, with some living just south of the airport, in the City of Norwalk, and in and near the City of Indianola. 
      In general, Latino residents living in Warren County have similar outcomes compared with the overall metro."
      
    } else if(input$county_choice2 == "Jasper") {
      
      "Jasper County lies just to the east of Polk County and has a relatively small Latino population, with most living in or south of the city of Newton. 
      In general, residents living in Jasper County have similar outcomes compared with the overall metro."
      
    } else {
      
      "Counties outside the Des Moines metro area enjoy the rich contributions of their own respective Latino communities.
      The demographic, economic, and health profiles of each county's Latino are each unique in their own rights.
      Scroll on to learn more."
    }
    
    
  })
  
  # Fact cards
  # County ages
  median_age_df <- reactive({
    
    ia_counties_tidy %>%
      filter(variable_group == "B01002" & county_name == input$county_choice2)
    
  })
  
  median_age_hisp_df <- reactive({
    
    ia_counties_tidy %>%
      filter(variable_group == "B01002I" & county_name == input$county_choice2)
    
  })
  
  hh_income_df <- reactive({
    
    ia_counties_tidy %>%
      filter(variable_group == "B19013" & county_name == input$county_choice2)
    
  })
  
  hh_income_hisp_df <- reactive({
    
    ia_counties_tidy %>%
      filter(variable_group == "B19013I" & county_name == input$county_choice2)
    
  })
  
  output$fast_facts <- renderText({
    
    median_age <- median_age_df()
    median_age_hisp <- median_age_hisp_df()
    hh_income <- hh_income_df()
    hh_income_hisp <- hh_income_hisp_df()
    
    glue("<b>Median age</b>
         <li>Overall: <b>{median_age$estimate[median_age$variable_index == '001']} (± {median_age$moe[median_age$variable_index == '001']})</b></li>
         <li>All Latinos: <b>{median_age_hisp$estimate[median_age_hisp$variable_index == '001']} (± {median_age_hisp$moe[median_age$variable_index == '001']})</b></li>
         <li>Male Latinos: <b>{median_age_hisp$estimate[median_age_hisp$variable_index == '002']} (± {median_age_hisp$moe[median_age$variable_index == '002']})</b></li>
         <li>Latinas: <b>{median_age_hisp$estimate[median_age_hisp$variable_index == '003']} (± {median_age_hisp$moe[median_age$variable_index == '003']})</b></li>
         <br>
         <b>Median income</b>
         <li>Overall: <b>${format(hh_income$estimate, big.mark = ',')} (± ${format(hh_income$moe, big.mark = ',')})</b></li>
         <li>Latinos: <b>${format(hh_income_hisp$estimate, big.mark = ',')} (± ${format(hh_income_hisp$moe, big.mark = ',')})</b></li>")
    
  })
  
  # output$overview <- renderText({
  #   
  #   median_age <- median_age_df()
  #   median_age_hisp <- median_age_hisp_df()
  #   hh_income <- hh_income_df()
  #   hh_income_hisp <- hh_income_hisp_df()
  #   
  #   glue("The median age of the county's population in 2019 was {median_age$estimate[median_age$variable_index == '001']} years (± {median_age$moe[median_age$variable_index == '001']}).
  #          For females, the median was {median_age$estimate[median_age$variable_index == '003']} years (± {median_age$moe[median_age$variable_index == '003']}), while for males, the median was 
  #          {median_age$estimate[median_age$variable_index == '002']} years (± {median_age$moe[median_age$variable_index == '002']}). For the Latinx population, the median
  #          age was {median_age_hisp$estimate[median_age_hisp$variable_index == '001']} years (± {median_age_hisp$moe[median_age$variable_index == '001']}). Latino males
  #          had a median age of {median_age_hisp$estimate[median_age_hisp$variable_index == '002']} years (± {median_age_hisp$moe[median_age$variable_index == '002']}), while the median age for Latinas
  #          alone was {median_age_hisp$estimate[median_age_hisp$variable_index == '003']} years (± {median_age_hisp$moe[median_age$variable_index == '003']}).
  #          <br></br>
  #          The median household income for the county in 2019 was ${format(hh_income$estimate, big.mark = ',')} (± ${format(hh_income$moe, big.mark = ',')}) in 2019-inflation-adjusted dollars.
  #          For the Latinx population, the median household income in the same year was ${format(hh_income_hisp$estimate, big.mark = ',')} (± ${format(hh_income_hisp$moe, big.mark = ',')}).")
  #   
  #   
  # })
  
  
  map_var2 <- reactive({if (input$unit2 == "as a %") "percent" else "estimat" })
  map_symbol2 <- reactive ({ if (input$unit2 == "as a %") "%" else ""})
  
  # Interactive leaflet map
  output$map2 <- renderLeaflet({
    
    argon_map <- "https://api.mapbox.com/styles/v1/sullivannicole/ckp33bliz5lh817o0g3xsrnyw/tiles/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic3VsbGl2YW5uaWNvbGUiLCJhIjoiY2prZTRzcnBvMDA1bTNwcGdkM2poamd6cyJ9.-Edd8XaXp1XUm6vUyReerw"
    map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © <a href='https://www.creative-tim.com/product/argon-design-system'>Argon-Style</a>"
    
    pal <- colorNumeric(c("#29066B", "#7D3AC1", "#AF4BCE", "#DB4CB2", "#EA7369", "#F0A58F", "#FCEAE6"), NULL)
    
    varname <- switch(map_var2(),
                      "percent" = "% of pop.",
                      "estimat" = "People")
    
    selected_county_zips <- ia_metro_zips %>%
      filter(county == paste0(input$county_choice2, " County"))

    selected_county_zips_shp <- ia_metro_zips_shp %>%
      filter(GEOID10 %in% selected_county_zips$zipcode)
    
    
    # Get ZCTAS that have centroids in the county
    # centroids <- gCentroid(df1, byid = TRUE)
    # 
    # mem_zcta2 <- df1[as.vector(gContains(memphis_ua, centroids, byid = TRUE)), ]
    
    ia_metro_tidy %>%
      filter(vrbl_gr == "B03001" & vrbl_nd == '003' & cnty_nm == input$county_choice2) %>%
      rename(var = !!map_var2()) %>%
      leaflet() %>%
      addTiles(urlTemplate = argon_map, attribution = map_attr) %>%
      # addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = FALSE,  
                  fillOpacity = 0.15, 
                  smoothFactor = 0.3,
                  fillColor = ~pal(var),
                  label = ~paste0("Latino pop.: ", formatC(var, big.mark = ","), map_symbol2())) %>% # use NAME variable for county
      addPolygons(data = selected_county_zips_shp, fill = F, weight = 2, color = hex_blue_lt, group = "Zip codes") %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Zip codes"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Zip codes") %>%
      addLegend(pal = pal, values = ~var, opacity = 0.3, layerId = "colorLegend", title = varname)
  })
  
  # Card 1: Demographics--------------------------------------------------------
  
  # Birthplace arc
  origin_df <- reactive({
    
    ia_counties_tidy %>%
      filter(county_name == input$county_choice2 & variable_group == "B06004I" & variable_index != "001") %>%
      mutate(ymax = cumsum(prop),
             ymin = lag(ymax),
             ymin = ifelse(is.na(ymin), 0, ymin),
             label = str_replace_all(label, "Estimate!!Total:!!", ""),
             label = case_when(label == "Native; born outside the United States" ~ "Born abroad to US citizens",
                               label == "Foreign born" ~ "Born abroad",
                               label == "Born in state of residence" ~ "Born in Iowa",
                               TRUE ~ label)) %>%
      mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
    
  })
  
  output$arcplot_origin2 <- renderPlot({
    
    origin_df() %>%
      ggplot() +
      ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
      coord_fixed() +
      scale_fill_manual(values = c(hex_purple, hex_blue_dk, hex_green, hex_blue_lt)) +
      scale_color_manual(values = c(hex_purple, hex_blue_dk, hex_green, hex_blue_lt)) +
      labs(color = "",
           fill = "") +
      arc_ggtheme +
      guides(fill = guide_legend(nrow = 2,byrow = TRUE))
    
  })
  
  
  output$arcplot_origin_text2 <- renderText({ 
    
    born_ia <- if(origin_df()$percent[origin_df()$label == "Born in Iowa"] > 0)  glue("{round(origin_df()$percent[origin_df()$label == 'Born in Iowa'], 1)}%  of the county's Latinos are Iowa-born. ") else NULL
    born_other_state <- if(origin_df()$percent[substr(origin_df()$label, 9, 13) == "other"] > 0)  glue("Another {round(origin_df()$percent[substr(origin_df()$label, 9, 13) == 'other'], 1)}% of the county's Latino Iowans were born in another state in the US. ") else NULL
    native_born <- if(origin_df()$percent[origin_df()$label == "Born abroad to US citizens"] > 0)  glue("Of the rest of the Latinos in {input$county_choice2}, {round(origin_df()$percent[origin_df()$label == 'Born abroad to US citizens'], 1)}% are native, born outside the US. ") else NULL
    foreign_born <- if(origin_df()$percent[origin_df()$label == "Born abroad"] > 0) glue("An estimated {round(origin_df()$percent[origin_df()$label == 'Born abroad'], 1)}% of the county's Latinos were foreign born. ") else NULL
    
    paste0(born_ia, born_other_state, native_born, foreign_born)
    
  })
  
  # Marital status
  status_df <- reactive({
    
    ia_counties_tidy %>%
      filter(variable_group == "B12002I" & county_name == input$county_choice2 & !variable_index %in% c("001", "002", "008")) %>%
      filter(percent != 0) %>%
      mutate(label = str_replace_all(label, ":", ", "),
             label = str_replace_all(label, "married \\(", "married \n\\("),
             gender = ifelse(substr(label, 1, 4) == "Male", "Male", "Female"),
             label = str_remove_all(label, "Male, |Female, "),
             text = paste0(gender, ", ", label, ": ", round(percent, 1), "%"))
  })
  
  output$bar_status <- renderPlotly({
    
    status_plot <- ggplot(status_df(), aes(percent, label, fill = gender, text = text)) +
      geom_bar(stat = "identity", width = 0.4, position = position_dodge()) +
      # geom_errorbar(aes(xmin = percent-moe_pc, xmax = percent+moe_pc), 
      #               width = 0.05, color = "#4f515c", position = position_dodge(0.4)) +
      scale_fill_manual(values = c(hex_green, hex_purple)) +
      labs(y = "", 
           x = "", #glue("% of Latinx pop. in {unique(status_df()$county_name)}"),
           fill = "") +
      theme_minimal() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      project_ggtheme
    
    ggplotly(status_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      plotly::config(displayModeBar = F)
    
  })
  
  output$bar_status_text <- renderText({ 
    
    # Max female status
    max_fem <- max(status_df()$percent[status_df()$gender == "Female"])
    max_fem_lab <- status_df()$label[status_df()$percent == max_fem]
    
    # Max male status
    max_male <- max(status_df()$percent[status_df()$gender == "Male"])
    max_male_lab <- status_df()$label[status_df()$percent == max_male]
    
    glue("The marital status with the largest share among Latinas ({round(max_fem, 1)}%) is '{str_to_lower(max_fem_lab)}'; amongst Latino males, the status with largest share ({round(max_male, 1)}%) is '{str_to_lower(max_male_lab)}'.")
    
  })
  
  # Language spoken at home
  
  output$lollipop_language <- renderPlotly({
    
    # Language at home
    language_plot <- ia_counties_tidy %>%
      filter(variable_group == "B16006" & county_name == input$county_choice2 & !(variable_index %in% c("001", "003")) & percent > 0) %>%
      mutate(label = str_replace(label, ":", "\n")) %>%
      ggplot(aes(text = glue("{round(percent, 1)}% \n {label}"))) +
      geom_segment(aes(x=label, xend=label, y=0, yend=percent), color=hex_purple, size = 0.8) +
      geom_point(aes(label, percent), color=hex_purple, size=3.5) +
      labs(y = "% of Latino pop.",
           x = "") +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.title = element_text(color = "#51535e"))
    
    ggplotly(language_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% config(displayModeBar = F)
    
  })
  
  
  # Ancestral origin arc
  heritage_df <- reactive({
    
    ia_counties_tidy %>%
      filter(county_name == input$county_choice2 & variable_group == "B03001" & variable_index %in% c("003", "004", "005", "006", "008", "016", "027")) %>%
      mutate(denom = ifelse(variable_index == "003", estimate, NA)) %>%
      tidyr::fill(denom, .direction = "updown") %>%
      mutate(prop = estimate/denom,
             percent = prop*100) %>%
      filter(percent != 100 & percent != 0) %>%
      mutate(ymax = cumsum(prop),
             ymin = lag(ymax),
             ymin = ifelse(is.na(ymin), 0, ymin),
             label = trimws(str_replace_all(label, "Hispanic or Latino:|:", ""))) %>%
      mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
    
    
  })
  
  output$arcplot_heritage2 <- renderPlot({

    ggplot(heritage_df()) +
      ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
      coord_fixed() +
      scale_fill_manual(values = c(hex_purple, hex_blue_dk, hex_orange, hex_pink, hex_green, hex_blue_lt)) +
      scale_color_manual(values = c(hex_purple, hex_blue_dk, hex_orange, hex_pink, hex_green, hex_blue_lt)) +
      labs(color = "",
           fill = "") +
      arc_ggtheme +
      guides(fill=guide_legend(nrow=2,byrow=TRUE))
    
  })
  
  output$heritage_text <- renderText({
    
    mexican <- if(heritage_df()$percent[heritage_df()$label == "Mexican"] > 0)  glue("{round(heritage_df()$percent[heritage_df()$label == 'Mexican'], 1)}%  identify as Mexican. ") else NULL
    puerto_rican <- if(heritage_df()$percent[substr(heritage_df()$label, 1, 3) == "Pue"] > 0)  glue("Another {round(heritage_df()$percent[substr(heritage_df()$label, 1, 3) == 'Pue'], 1)}% of the county's Latinos are Puerto Rican. ") else NULL
    cuban <- if(heritage_df()$percent[substr(heritage_df()$label, 1, 5) == "Cuban"] > 0) glue("In contrast, {round(heritage_df()$percent[substr(heritage_df()$label, 1, 5) == 'Cuban'], 1)}% of Latinos in the county identify as Cuban. ") else NULL
    central <- if(heritage_df()$percent[substr(heritage_df()$label, 1, 7) == "Central"] > 0) glue("Of the rest of the Latinos in {input$county_choice2}, {round(heritage_df()$percent[substr(heritage_df()$label, 1, 7) == 'Central'], 1)}% are Central American (from El Salvador, Guatemala, Honduras, Costa Rica and Panama). ") else NULL
    south <- if(heritage_df()$percent[substr(heritage_df()$label, 1, 5) == "South"] > 0) glue("An estimated {round(heritage_df()$percent[substr(heritage_df()$label, 1, 5) == 'South'], 1)}% of the county's Latinos are of South American descent, the largest proportion of which are Ecuadoran. ") else NULL
    
    
    paste0("The county's Latino community hails from diverse heritages. ", mexican, puerto_rican, cuban, central, south)
    
  })
  
  # output$arcplot2 <- renderPlot({
  #   
  #   pc_latin <- ia_counties_tidy %>%
  #     filter(county_name == input$county_choice2 & variable_group == "B03003" & variable_index != '001') %>%
  #     mutate(ymax = cumsum(prop),
  #            ymin = lag(ymax),
  #            ymin = ifelse(is.na(ymin), 0, ymin),
  #            label = as.factor(label)) %>%
  #     mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
  #   
  #   # Arc chart for % of county that's Latino
  #   pc_latin %>%
  #     ggplot() +
  #     ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
  #     coord_fixed() +
  #     scale_fill_manual(values = c("#5E72E4", "#172B4D")) +
  #     scale_color_manual(values = c("#5E72E4", "#172B4D")) +
  #     labs(color = "",
  #          fill = "") +
  #     arc_ggtheme
  #   
  # })
  
  # Card 2: Economics & Workforce--------------------------------------------------------
  
  gender_work_df <- reactive({
    
    # Employment rate by gender
    ia_counties_tidy %>%
      filter(variable_group == "B20005I" & county_name == input$county_choice2 & variable_index %in% c("003", "027", "028", "050", "074", "075") & percent > 0) %>%
      mutate(label = str_remove(label, ", year-round in the past 12 months"),
             label = str_replace_all(label, "s:", "s"),
             label = str_replace_all(label, ":", ", "),
             gender = ifelse(substr(label, 1, 4) == "Male", "Male", "Female"),
             label = str_remove_all(label, "Male, |Female, "),
             label = str_replace_all(label, ",", ", \n"),
             text = paste0(gender, ", ", label, ": ", round(percent, 1)))
    
  })
  
  output$bar_gender_work <- renderPlotly({
    
    gender_plot <- ggplot(gender_work_df(), aes(percent, label, fill = gender, text = text)) +
      geom_bar(stat = "identity", width = 0.4, position = position_dodge()) +
      
      # Don't include MOE's for now since we don't have them on everything
      # geom_errorbar(aes(xmin = percent-moe_pc, xmax = percent + moe_pc), 
      #               width = 0.1, color = "#4f515c", position = position_dodge(0.4)) +
      scale_fill_manual(values = c(hex_green, hex_purple)) +
      labs(y = "", 
           x = glue("% of Latino pop. in {unique(gender_work_df()$county_name)}")) +
      labs(fill = "") +
      theme_minimal() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      project_ggtheme
    
    ggplotly(gender_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  
  output$gender_work_text <- renderText({
    
    # Max female status
    max_fem <- max(gender_work_df()$percent[gender_work_df()$gender == "Female"])
    max_fem_lab <- gender_work_df()$label[gender_work_df()$percent == max_fem]
    
    # Max male status
    max_male <- max(gender_work_df()$percent[gender_work_df()$gender == "Male"])
    max_male_lab <- gender_work_df()$label[gender_work_df()$percent == max_male]
    
    glue("The employment status with the largest share among Latinas ({round(max_fem, 1)}%) is '{str_replace(str_to_lower(max_fem_lab), 'full-time, ', 'full-time')}'; amongst Latino males, the status with largest share ({round(max_male, 1)}%) is '{str_replace(str_to_lower(max_male_lab), 'full-time, ', 'full-time')}'.")
    
    
  })
  
  # Homeownership
  tenure_df <- reactive({
    
    ia_counties_tidy %>%
      filter(variable_group == "B25003I" & county_name == input$county_choice2 & variable_index != "001") %>%
      mutate(ymax = cumsum(prop),
             ymin = lag(ymax),
             ymin = ifelse(is.na(ymin), 0, ymin),
             label = as.factor(label)) %>%
      mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
    
  })
  
  output$arc_homeownership <- renderPlot({
    
    tenure_df() %>%
      ggplot() +
      ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
      coord_fixed() +
      scale_fill_manual(values = c(hex_purple, hex_blue_dk)) +
      scale_color_manual(values = c(hex_purple, hex_blue_dk)) +
      labs(color = "",
           fill = "") +
      arc_ggtheme
    
  })
  
  
  output$homeownership_text <- renderText({
    
    owners <- round(tenure_df()$percent[substr(tenure_df()$label, 1, 5) == 'Owner'], 1)
    renters <- 100-owners
    
    glue("Tenure in the county is split between homeownership and renter-ship. {owners}% in the county are home-owning, while the other {renters}% rent a dwelling.")
    
    
  })
  
  # Means of transportation
  output$lollipop_transportation2 <- renderPlotly({
    
    # Means of transportation
    means_transportation <- ia_counties_tidy %>%
      filter(county_name == input$county_choice2 & variable_group == "B08105I" & variable_index != "001" & percent > 0) %>%
      ggplot(aes(text = glue("{round(percent)}% \n {label}"))) +
      geom_segment(aes(x=label, xend=label, y=0, yend=percent), color="#EC603E", size = 0.8) +
      geom_point(aes(label, percent), color="#EC603E", size=3.5) +
      labs(y = "% of Latino pop.",
           x = "") +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid = element_blank())
    
    ggplotly(means_transportation, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  
  # Poverty
  poverty_df <- reactive({
    
    ia_counties_tidy %>% 
      filter(county_name == input$county_choice2 & variable_group == "B17020I") %>%
      mutate(poverty_group = case_when(variable_index %in% c(glue("00{3:6}")) ~ "Below poverty, \naged 59 & under",
                                       variable_index %in% c(glue("00{7:9}")) ~ "Below poverty, aged 60+",
                                       variable_index == "010" ~ "At or above poverty",
                                       TRUE ~ "Other")) %>%
      group_by(poverty_group) %>%
      summarize(percent = sum(percent)) %>%
      ungroup() %>%
      filter(poverty_group != "Other" & percent > 0) %>%
      mutate(text = paste0(poverty_group, ": ", round(percent, 1)))
    
    
  })
  
  
  output$chicklet_poverty2 <- renderPlotly({
    
    poverty_plot <- poverty_df() %>%
      ggplot(aes(poverty_group, percent, text = text)) +
      geom_bar(stat = "identity", width = 0.1, fill = "#5E72E4", color = "#5E72E4") +
      coord_flip() +
      labs(x = "",
           y = "% of Latino pop. in county") +
      theme_minimal() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      project_ggtheme
    
    ggplotly(poverty_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  
  output$poverty_text <- renderText({
    
    
    above <- if("At or above poverty" %in% poverty_df()$poverty_group)  glue("{round(poverty_df()$percent[substr(poverty_df()$poverty_group, 1, 2) == 'At'], 1)}% of the county's Latinos live at or above the federal poverty level. ") else NULL
    below_59_under <- if("Below poverty, \naged 59 & under" %in% poverty_df()$poverty_group)  glue("In contrast, {round(poverty_df()$percent[substr(poverty_df()$poverty_group, 22, 23) == '59'], 1)}% live below federal poverty level and are under the age of 60. ") else NULL
    below_60_over <- if("Below poverty, aged 60+" %in% poverty_df()$poverty_group) glue("Those over 60 and living below poverty form the remaining {round(poverty_df()$percent[substr(poverty_df()$poverty_group, 21, 22) == '60'], 1)}%. ") else NULL
    
    paste0("Living at or below poverty level refers to the a household's income in comparison to the income the federal government considers 'poverty level'. ", above, below_59_under, below_60_over)
    
  })
  
  
  # Card 3: Education--------------------------------------------------------
  # Educational attainment
  output$lollipop_education <- renderPlotly({
    
    education_plot <- ia_counties_tidy %>%
      filter(variable_group == "C15002I" & county_name == input$county_choice2 & !(variable_index %in% c("001", "002", "007")))  %>%
      mutate(gender = ifelse(substr(label, 1, 4) == "Male", "Male", "Female"),
             label = str_remove_all(label, "Male:|Female:| degree|\\(includes equivalency\\)")) %>%
      ggplot(aes(text = glue("{round(percent, 1)}% \n {label}"))) +
      geom_linerange(aes(x = label, ymin = 0, ymax = percent, color = gender), 
                     position = position_dodge(width = 0.3))+
      geom_point(aes(x = label, y = percent, color = gender),
                 position = position_dodge(width = 0.3)) +
      scale_color_manual(values = c(hex_green, hex_purple)) +
      labs(y = "% of Latino pop.",
           x = "",
           color = "") +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            legend.position = "bottom")
    
    ggplotly(education_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  # Disciplines in school
  
  disciplines_df <- reactive({
    
    
    ia_counties_tidy %>%
      filter(variable_group == "C15010I" & county_name == input$county_choice2 & !(variable_index %in% c("001")))  %>%
      mutate(ymax = cumsum(prop),
             ymin = lag(ymax),
             ymin = ifelse(is.na(ymin), 0, ymin),
             label = as.factor(label)) %>%
      mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
    
  })
  
  output$arc_disciplines <- renderPlot({
    
    disciplines_df() %>%
      ggplot() +
      ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
      coord_fixed() +
      scale_fill_manual(values = c(hex_green, hex_purple, hex_pink, hex_blue_dk, hex_blue_lt)) +
      scale_color_manual(values = c(hex_green, hex_purple, hex_pink, hex_blue_dk, hex_blue_lt)) +
      labs(color = "",
           fill = "") +
      guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
      arc_ggtheme
    
  })
  
  output$disciplines_text <- renderText({
    
    stem <- if(disciplines_df()$percent[disciplines_df()$label == "Science and Engineering"] > 0)  glue("{round(disciplines_df()$percent[disciplines_df()$label == 'Science and Engineering'], 1)}%  of Latino college students are currently majoring in a STEM field. ") else NULL
    business <- if(disciplines_df()$percent[substr(disciplines_df()$label, 1, 3) == "Bus"] > 0)  glue("Another {round(disciplines_df()$percent[substr(disciplines_df()$label, 1, 3) == 'Bus'], 1)}% of those Latino students have selected Business as their major. ") else NULL
    education <- if(disciplines_df()$percent[substr(disciplines_df()$label, 1, 3) == "Edu"] > 0) glue("An estimated {round(disciplines_df()$percent[substr(disciplines_df()$label, 1, 3) == 'Edu'], 1)}% of the county's Latino students are expecting to graduate with an Education degree. ") else NULL
    arts <- if(disciplines_df()$percent[substr(disciplines_df()$label, 1, 4) == "Arts"] > 0) glue("{round(disciplines_df()$percent[substr(disciplines_df()$label, 1, 4) == 'Arts'], 1)}% are currently pursuing degrees in Arts, Humanities and related disciplines. ") else NULL
    # south <- if(disciplines_df()$percent[substr(disciplines_df()$label, 1, 5) == "South"] > 0) glue("An estimated {round(disciplines_df()$percent[substr(disciplines_df()$label, 1, 5) == 'South'], 1)}% of the county's Latinx are of South American descent. ") else NULL
    
    
    paste0(stem, business, education, arts)
    
  })
  
  # Presence of a computer/type of internet
  
  output$bar_computer <- renderPlotly({
    
    
    internet <- ia_counties_tidy %>%
      filter(variable_group == "B28009I" & county_name == input$county_choice2 & !(variable_index %in% c("001", "002"))) %>%
      mutate(label = str_remove_all(label, "Has a | subscription alone| subscription|With a |With "),
             label = str_replace(label, "Without an", "Without"),
             label = str_to_sentence(str_replace(label, ":", ",\n")),
             text = paste0(label, ": ", round(percent, 1), "%"))
    
    
    internet_bar <- ggplot(internet, aes(percent, label, text = text)) +
      geom_bar(stat = "identity", width = 0.4, fill = hex_orange) +
      # Don't include MOEs for now since we don't have them on everything
      # geom_errorbar(aes(xmin = percent-moe_pc, xmax = percent+moe_pc), 
      #               width = 0.1, color = "#4f515c") +
      labs(y = "", 
           x = glue("% of Latino pop. in {unique(internet$county_name)}")) +
      labs(fill = "") +
      theme_minimal() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      project_ggtheme
    
    ggplotly(internet_bar, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  # School enrollment
  
  enrollment_df <- reactive({
    
    
    ia_counties_tidy %>%
      filter(variable_group == "B14007I" & county_name == input$county_choice2 & !(variable_index %in% c("001", "002"))) %>%
      mutate(label = str_remove(label, "Enrolled in school:"),
             label = str_replace(label, "Enrolled in college", "College"),
             label = ifelse(substr(label, 1, 2) == "En", "Pre-k through 12th", label)) %>%
      group_by(label) %>%
      summarize(prop = sum(prop),
                percent = sum(percent)) %>%
      ungroup() %>%
      mutate(ymax = cumsum(prop),
             ymin = lag(ymax),
             ymin = ifelse(is.na(ymin), 0, ymin),
             label = as.factor(label)) %>%
      mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
    
  })
  
  output$arc_enrolled <- renderPlot({
    
    enrollment_df() %>%
      ggplot() +
      ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
      coord_fixed() +
      scale_fill_manual(values = c(hex_green, hex_purple, hex_pink, hex_blue_lt, hex_blue_dk)) +
      scale_color_manual(values = c(hex_green, hex_purple, hex_pink, hex_blue_lt, hex_blue_dk)) +
      labs(color = "",
           fill = "") +
      guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
      arc_ggtheme
    
  })
  
  output$enrollment_text <- renderText({
    
    college <- if(enrollment_df()$percent[substr(enrollment_df()$label, 1, 7) == "College"] > 0)  glue("{round(enrollment_df()$percent[substr(enrollment_df()$label, 1, 7) == 'College'], 1)}%  of the county's Latinos are in college degree program. ") else NULL
    grad <- if(enrollment_df()$percent[substr(enrollment_df()$label, 1,4) == "Grad"] > 0)  glue("Meanwhile, {round(enrollment_df()$percent[substr(enrollment_df()$label, 1, 4) == 'Grad'], 1)}% are in graduate or professional school. ") else NULL
    not_enrolled <- if(enrollment_df()$percent[substr(enrollment_df()$label, 1, 3) == "Not"] > 0) glue("An estimated {round(enrollment_df()$percent[substr(enrollment_df()$label, 1, 3) == 'Not'], 1)}% are not enrolled in school. ") else NULL
    prek_12 <- if(enrollment_df()$percent[substr(enrollment_df()$label, 1, 4) == "Pre-"] > 0) glue("{round(enrollment_df()$percent[substr(enrollment_df()$label, 1, 4) == 'Pre-'], 1)}% are in preschool through grade 12. ") else NULL
    
    paste0(college, grad, not_enrolled, prek_12)
    
  })
  
  #-------Card 4: Health
  
  insurance_df <- reactive({
    
    ia_counties_tidy %>%
      filter(county_name == input$county_choice2 & variable_group == "C27001I" & variable_index %in% c("003", "004", "006", "007", "009", "010")) %>%
      separate(label, into = c("age_group", "coverage_category"), sep = ":") %>%
      mutate(age_group = str_squish(str_replace_all(age_group, "years", "")),
             age_group = factor(ifelse(age_group == "65 and over", "65+", age_group), levels = c("65+", "19 to 64", "Under 19")),
             coverage_category = ifelse(coverage_category == "With health insurance coverage", "coverage", "no coverage"),
             label = paste0(age_group, ": ", coverage_category),
             text = paste0("Ages ", age_group, " , ", coverage_category, ": ", round(percent, 1), "%"))
    
  })
  
  output$chicklet_insurance2 <- renderPlotly({
    
    ins_plot <- ggplot(insurance_df(), aes(age_group, percent, fill = coverage_category, text = text)) +
      geom_bar(stat = "identity", width = 0.3) +
      scale_fill_manual(values = c(hex_green, hex_pink)) +
      coord_flip() +
      labs(y = "% of county's Latino pop.",
           x = "",
           fill = "") +
      theme(panel.background = element_rect(fill = "transparent")) +
      theme_minimal() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      project_ggtheme
    
    ggplotly(ins_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white"))) %>% 
      config(displayModeBar = F)
    
  })
  
  output$insurance_text <- renderText({
    
    max_cov <- max(insurance_df()$percent[insurance_df()$coverage_category == "coverage"])
    max_cov_group <- insurance_df()$age_group[insurance_df()$percent == max_cov]
    
    min_cov <- max(insurance_df()$percent[insurance_df()$coverage_category == "no coverage"])
    min_cov_group <- insurance_df()$age_group[insurance_df()$percent == min_cov]
    
    glue("Having health insurance provides support for necessary medical care in both planned and unplanned circumstances. 
         Amongst the Latino community in the county, {round(max_cov, 1)}% in the {max_cov_group} age group have coverage. Conversely, the {min_cov_group} age group represent the largest group with no coverage ({round(min_cov, 1)}%).")
    
    
  })
  
  
  dataset_download <- reactive({
    
    ia_counties_tidy %>%
      mutate(variable_category = case_when(variable_group %in% c("B06004I", "B12002I", "B16006", "B03001", "B03003") ~ "Demographics",
                                           variable_group %in% c("C27001I", "B08105I", "B17020I", "B25003I", "B20005I") ~ "Economics and Workforce",
                                           variable_group %in% c("B28009I", "C15010I", "C15002I") ~ "Education",
                                           variable_group %in% c("B14007I") ~ "Health",
                                           TRUE ~ "Other")) %>%
      filter(variable_category %in% input$dataset & county_name == input$county_choice2) %>%
      select(-c(denom, prop, percent, denom_moe, moe_pc, county_name))
  })
  
  
  #---------------Data download accordion
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(paste(input$dataset, collapse = ", "), " ", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(dataset_download(), file)
    }
  )
  
  #---------------------End Tab 2 Alt A-----------------------
}