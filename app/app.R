library(shiny)
library(argonR)
library(argonDash)
library(bs4Dash)
library(magrittr)
library(leaflet)
library(shinyWidgets)
library(gfonts)
library(plotly)
library(scales)
library(sf)
# install.packages("ggchicklet", repos = "https://cinc.rud.is")
library(ggchicklet)
library(glue)
library(tidyverse)

# Primary
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

arc_ggtheme <- theme(panel.background = element_rect(fill = NA),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     axis.title = element_blank(),
                     legend.position = "bottom",
                     legend.text = element_text(family = "Karla"))

# setup_font(
#   id = "karla",
#   output_dir = "www/"
# )

# data - Uncomment when dashboard goes into production
# If pulling a new year, change acs_yr in create_data_extracts.R and re-run script
# New extracts will be saved in data folder
# Then change acs_yr in pull_data.R
# source("pull_data.R")

# template
source("sidebar.R")
# source("header.R")
source("footer.R")

# elements
# source("tabs/tab_0.R")
# source("tabs/tab_1.R")
source("tabs/tab_1_alt_a.R")
# source("tabs/tab_2.R")
source("tabs/tab_2_alt_a.R")
source("tabs/tab_3.R")
source("tabs/tab_4.R")

# App
shiny::shinyApp(
  ui = argonDashPage(
    title = "Argon Dashboard Nuestro IA",
    author = "Al Exito Iowa",
    description = "Argon Dash Nuestro IA",
    sidebar = argonSidebar,
    gfonts::use_font("karla", "www/css/karla.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    body = argonDashBody(
      argonTabItems(
        # tab_1,
        tab_1_alt_a,
        # tab_2,
        tab_2_alt_a,
        tab_3,
        tab_4
      )
    ),
    footer = argonFooter
  ),
  server = function(input, output) {
    
    # Tab 1-----------------------------------------------------------
    
    # output$state_total_latinx <- renderText({ 
    #   
    #   # % of state that's Latinx
    #   # ia_state_pc_latinx <- ia_state %>%
    #     # filter(variable %in% c("B01001I_001", "B01001_001") & year == acs_yr) %>%
    #     # spread(variable, value = estimate) %>%
    #     # fill("B01001I_001", .direction = "updown") %>%
    #     # fill("B01001_001", .direction = "updown") %>%
    #     # filter(row_number() == 1) %>%
    #     # mutate(pc_of_state_latinx = B01001I_001/B01001_001*100)
    #   
    #   # glue("{round(ia_state_pc_latinx$B01001I_001/1000, 0)}K Latinx in the state of Iowa")})
    #   
    #   
    #   # % of metro that's Latinx
    #   ia_metro_pc_latinx <- ia_metro_tracts_tidy %>%
    #     filter(variable %in% c("B01001I_001", "B01001_001") & year == acs_yr) %>%
    #     tibble() %>%
    #     select(-geometry) %>%
    #     group_by(variable, variable_group, variable_index) %>%
    #     summarize(estimate = sum(estimate)) %>%
    #     ungroup() %>%
    #     spread(variable, value = estimate) %>%
    #     tidyr::fill("B01001I_001", .direction = "updown") %>%
    #     tidyr::fill("B01001_001", .direction = "updown") %>%
    #     filter(row_number() == 1) %>%
    #     mutate(pc_of_metro_latinx = B01001I_001/B01001_001*100)
    #   
    #   glue("{round(ia_metro_pc_latinx$B01001I_001/1000, 0)}K Latinx in Central Iowa")})
    # 
    # output$state_pc_latinx <- renderText({
    # 
    #   # % of state that's Latinx
    #   ia_state_pc_latinx <- ia_state %>%
    #     filter(variable %in% c("B01001I_001", "B01001_001") & year == acs_yr) %>%
    #     spread(variable, value = estimate) %>%
    #     fill("B01001I_001", .direction = "updown") %>%
    #     fill("B01001_001", .direction = "updown") %>%
    #     filter(row_number() == 1) %>%
    #     mutate(pc_of_state_latinx = B01001I_001/B01001_001*100)
    # 
    #   glue("{round(ia_state_pc_latinx$pc_of_state_latinx)}% of the state pop.")
    # 
    #   })
    # 
    # output$state_pc_latinx_trend <- renderPlot({
    #   
    #   # % of state that's Latinx over time
    #   # ia_state_pc_latinx_trend <- ia_state %>%
    #   #   filter(variable %in% c("B01001I_001", "B01001_001")) %>%
    #   #   group_by(year) %>%
    #   #   mutate(denom = ifelse(variable == "B01001_001", estimate, NA)) %>%	
    #   #   arrange(year, variable) %>%
    #   #   fill(denom, .direction = "down") %>%
    #   #   ungroup() %>%
    #   #   mutate(pc_latinx = estimate/denom*100) %>%
    #   #   filter(pc_latinx != 100)
    #   # 
    #   # ia_state_pc_latinx_trend %>%
    #   #   mutate(year = lubridate::ymd(year, truncated = 2L)) %>%
    #   # ggplot(aes(year, pc_latinx)) +
    #   #   geom_line(color = "#63CF89", size = 0.8) +
    #   #   geom_point(shape = 21, fill = "white", color = "#63CF89", size = 2, stroke = 1.5) +
    #   #   labs(x = "",
    #   #        y = "") +
    #   #   theme_minimal() +
    #   #   theme(panel.grid = element_blank(),
    #   #         axis.text.y = element_text(size = 13, family = "Karla"),
    #   #         axis.text.x = element_blank())
    #   
    #   # % of metro that's Latinx over time
    #   ia_metro_pc_latinx_trend <- ia_metro_tracts_tidy %>%
    #     tibble() %>%
    #     select(-geometry) %>%
    #     filter(variable %in% c("B01001I_001", "B01001_001")) %>%
    #     group_by(year, variable) %>%
    #     summarize(estimate = sum(estimate)) %>%
    #     ungroup() %>%
    #     group_by(year) %>%
    #     mutate(denom = ifelse(variable == "B01001_001", estimate, NA)) %>%
    #     arrange(year, variable) %>%
    #     fill(denom, .direction = "down") %>%
    #     ungroup() %>%
    #     mutate(pc_latinx = estimate/denom*100) %>%
    #     filter(pc_latinx != 100)
    #   
    #   ia_metro_pc_latinx_trend %>%
    #     mutate(year = lubridate::ymd(year, truncated = 2L)) %>%
    #   ggplot(aes(year, pc_latinx)) +
    #     geom_line(color = "#63CF89", size = 0.8) +
    #     geom_point(shape = 21, fill = "white", color = "#63CF89", size = 2, stroke = 1.5) +
    #     labs(x = "",
    #          y = "") +
    #     theme_minimal() +
    #     theme(panel.grid = element_blank(),
    #           axis.text.y = element_text(size = 13, family = "Karla"),
    #           axis.text.x = element_blank())
    #   
    # }, height = 120, width = 200)
    # 
    # output$metro_businesses <- renderPlot({
    #   
    #   ggplot(latinx_businesses, aes(Industry, Businesses)) +
    #     geom_chicklet(width = 0.8, fill = "#5E72E4")+
    #     coord_flip() +
    #     labs(x = "", y = "") +
    #     scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    #     theme_minimal() +
    #     theme(legend.position = "bottom",
    #           panel.grid = element_blank())
    #   
    # }, height = 120, width = 250)
    # 
    # map_var <- reactive({ if (input$unit == "as a %") "percent" else "estimate"})
    # map_symbol <- reactive ({ if (input$unit == "as a %") "%" else ""})
    # 
    # # Interactive leaflet map
    # output$map <- renderLeaflet({
    #   
    #   argon_map <- "https://api.mapbox.com/styles/v1/sullivannicole/ckp33bliz5lh817o0g3xsrnyw/tiles/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic3VsbGl2YW5uaWNvbGUiLCJhIjoiY2prZTRzcnBvMDA1bTNwcGdkM2poamd6cyJ9.-Edd8XaXp1XUm6vUyReerw"
    #   map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © <a href='https://www.creative-tim.com/product/argon-design-system'>Argon-Style</a>"
    #   
    #   pal <- colorNumeric(c("#29066B", "#7D3AC1", "#AF4BCE", "#DB4CB2", "#EA7369", "#F0A58F", "#FCEAE6"), NULL)
    # 
    # # ia_counties_tidy %>%
    # iowa_metro_labeled %>%
    #   filter(variable_group == "B03001" & variable_index == '003') %>%
    #   rename(var = !!map_var()) %>%
    #   leaflet() %>%
    #   addTiles(urlTemplate = argon_map, attribution = map_attr) %>%
    #   # addProviderTiles("CartoDB.Positron") %>%
    #   addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.9,
    #               fillColor = ~pal(var),
    #               label = ~paste0(county_name, ": ", formatC(var, big.mark = ","), map_symbol())) %>% # use NAME variable for county
    #   addLegend(pal = pal, values = ~var, opacity = 1.0)
    # })
    
    # End Tab 1-----------------------------------------------------------
    
    # Tab 1 Alt A---------------------------------------------------------
    
    # observeEvent(input$jumpToP2, {
    #   updateTabsetPanel(session, "app_sidebar",
    #                     selected = "tab_2")
    # })
    
    # Tab 2--------------------------------------------------------------
    # output$tinymap <- renderPlot({
    #   
    #   ia_shp_data %>%
    #   filter(GEOID == input$county_choice) %>%
    #   ggplot() +
    #   geom_sf(fill = "#5E72E4", color = "#5E72E4") +
    #     labs(title = glue("{input$county_choice}\n\n")) +
    #     theme_void() +
    #     theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#5E72E4"))
    #   
    # }, height = 50, width = 100)
    
    # output$arcplot_origin <- renderPlot({
    #   
    #   pc_latin_origin <- ia_counties_tidy %>%
    #     filter(county_name == input$county_choice & variable_group == "B06004I" & variable_index != "001") %>%
    #     mutate(ymax = cumsum(prop),
    #            ymin = lag(ymax),
    #            ymin = ifelse(is.na(ymin), 0, ymin),
    #            label = str_replace_all(label, "Estimate!!Total:!!", "")) %>%
    #     mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
    #   
    #   pc_latin_origin %>%
    #     ggplot() +
    #     ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
    #     coord_fixed() +
    #     scale_fill_manual(values = c("#5E72E4", "#172B4D", "#63CF89", "#5DCEF0")) +
    #     scale_color_manual(values = c("#5E72E4", "#172B4D", "#63CF89", "#5DCEF0")) +
    #     labs(color = "",
    #          fill = "") +
    #     theme(panel.background = element_rect(fill = NA),
    #           axis.text = element_blank(),
    #           axis.ticks = element_blank(),
    #           axis.title = element_blank(),
    #           legend.position = "bottom") +
    #     guides(fill=guide_legend(nrow=2,byrow=TRUE))
    #   
    # })
    # 
    # output$county_chosen_text <- renderText({
    #   
    #   trimws(input$county_choice)
    #   
    # })
    # 
    # output$county_chosen_total_latinx <- renderText({
    #   
    #   county_chosen_total <- ia_counties_tidy %>%
    #     filter(county_name == input$county_choice & concept == 'HISPANIC OR LATINO ORIGIN' & variable_index == '003')
    #   
    #   county_chosen_total$estimate
    #   
    #   
    # })
    # 
    # output$arcplot_origin_text <- renderText({ 
    #   
    #   pc_latin_origin <- ia_counties_tidy %>%
    #     filter(county_name == input$county_choice & variable_group == "B06004I" & variable_index != "001") %>%
    #     mutate(label = str_replace_all(label, "Estimate!!Total:!!", ""),
    #            percent = round(as.numeric(percent)))
    #   
    #   born_ia <- pc_latin_origin %>% filter(label == "Born in state of residence")
    #   born_other_state <- pc_latin_origin %>% filter(label == "Born in other state in the United States")
    #   native_born_outside <- pc_latin_origin %>% filter(label == "Native; born outside the United States")
    #   foreign_born <- pc_latin_origin %>% filter(label == "Foreign born")
    #   
    #   if (born_ia$percent > 0) born_ia_sentence <- glue("{born_ia$percent}%  were born in Iowa. ") else born_ia_sentence <- ""
    #   if (born_other_state$percent > 0) born_other_state_sentence <- glue("Another {born_other_state$percent}% of the county's Latinx Iowans were born in another state in the US. ") else born_other_state_sentence <- ""
    #   if (native_born_outside$percent > 0) native_born_outside_sentence <- glue("Of the rest of the Latinx in {input$county_choice}, {native_born_outside$percent}% are native, born outside the US. ") else native_born_outside_sentence <- ""
    #   if(foreign_born$percent > 0) foreign_born_sentence <- glue("An estimated {foreign_born$percent}% of the county's Latinx were foreign born. ") else foreign_born_sentence <- ""
    #   
    #   glue("{born_ia_sentence}{born_other_state_sentence}{native_born_outside_sentence}{foreign_born_sentence}")
    #   
    # })
    # 
    # output$arcplot_heritage <- renderPlot({
    #   
    #   ia_counties_tidy %>%
    #     filter(county_name == input$county_choice & variable_group == "B03001" & variable_index %in% c("003", "004", "005", "006", "008", "016", "027")) %>%
    #     mutate(denom = ifelse(variable_index == "003", estimate, NA)) %>%
    #     fill(denom, .direction = "updown") %>%
    #     mutate(prop = estimate/denom,
    #            percent = prop*100) %>%
    #     filter(percent != 100 & percent != 0) %>%
    #     mutate(ymax = cumsum(prop),
    #            ymin = lag(ymax),
    #            ymin = ifelse(is.na(ymin), 0, ymin),
    #            label = trimws(str_replace_all(label, "Hispanic or Latino:|:", ""))) %>%
    #     mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1) %>%
    #     ggplot() +
    #     ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
    #     coord_fixed() +
    #     scale_fill_manual(values = c("#5E72E4",  "#172B4D","#EC603E", "#EA445B", "#63CF89", "#5DCEF0")) +
    #     scale_color_manual(values = c("#5E72E4",  "#172B4D","#EC603E", "#EA445B", "#63CF89", "#5DCEF0")) +
    #     labs(color = "",
    #          fill = "") +
    #     theme(panel.background = element_rect(fill = NA),
    #           axis.text = element_blank(),
    #           axis.ticks = element_blank(),
    #           axis.title = element_blank(),
    #           legend.position = "bottom",
    #           legend.text = element_text(family = "Karla")) +
    #     guides(fill=guide_legend(nrow=2,byrow=TRUE))
    #   
    # })
    # 
    # output$arcplot <- renderPlot({
    #   
    #   pc_latin <- ia_counties_tidy %>%
    #     filter(county_name == input$county_choice & variable_group == "B03003" & variable_index != '001') %>%
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
    #     theme(panel.background = element_rect(fill = NA),
    #           axis.text = element_blank(),
    #           axis.ticks = element_blank(),
    #           axis.title = element_blank(),
    #           legend.position = "bottom",
    #           legend.text = element_text(family = "Karla"))
    #   
    # })
    # 
    # # Lollipop chart of means of transportation to work
    # 
    # output$chicklet_poverty <- renderPlot({
    #   
    #   ia_counties_tidy %>% 
    #     filter(county_name == input$county_choice & variable_group == "B17020I") %>%
    #     mutate(poverty_group = case_when(variable_index %in% c(glue("00{3:6}")) ~ "Below poverty, \naged 59 & under",
    #                                      variable_index %in% c(glue("00{7:9}")) ~ "Below poverty, aged 60+",
    #                                      variable_index == "010" ~ "At or above poverty",
    #                                      TRUE ~ "Other")) %>%
    #     group_by(poverty_group) %>%
    #     summarize(percent = sum(percent)) %>%
    #     ungroup() %>%
    #     filter(poverty_group != "Other" & percent > 0) %>%
    #     ggplot(aes(poverty_group, percent)) +
    #     geom_bar(stat = "identity", width = 0.1, fill = "#5E72E4", color = "#5E72E4") +
    #     coord_flip() +
    #     labs(x = "",
    #          y = "% of Latinx pop. in county") +
    #     theme_minimal() +
    #     scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    #     theme(panel.grid.minor =element_blank(),
    #           panel.grid.major = element_blank(),
    #           axis.line.y = element_line(color = "#cacee6", size = 0.8),
    #           axis.ticks.y = element_line(color = "#cacee6", size = 0.8),
    #           axis.text = element_text(family = "Karla", color = "#51535e", size = 14),
    #           axis.title = element_text(family = "Karla", color = "#51535e", size = 15))
    #   
    # })
    # 
    # output$lollipop_transportation <- renderPlotly({
    #   
    #   # Means of transportation
    #   means_transportation <- ia_counties_tidy %>%
    #     filter(county_name == input$county_choice & variable_group == "B08105I" & variable_index != "001" & percent > 0) %>%
    #     ggplot(aes(text = glue("{round(percent)}% \n {label}"))) +
    #     geom_segment(aes(x=label, xend=label, y=0, yend=percent), color="#EC603E", size = 0.8) +
    #     geom_point(aes(label, percent), color="#EC603E", size=3.5) +
    #     labs(y = "% of Latinx pop.",
    #          x = "") +
    #     coord_flip() +
    #     theme_minimal() +
    #     theme(panel.grid = element_blank())
    #   
    #   ggplotly(means_transportation, tooltip = "text") %>%
    #     layout(font = list(family = "Karla")) %>%
    #     style(hoverlabel = list(bgcolor = "#172B4D",
    #                              bordercolor = "#172B4D",
    #                              font = list(family = "Karla", color = "white")))
    #   
    # })
    # 
    # output$chicklet_insurance <- renderPlot({
    #   
    #   insurance <- ia_counties_tidy %>%
    #     filter(county_name == input$county_choice & variable_group == "C27001I" & variable_index %in% c("003", "004", "006", "007", "009", "010")) %>%
    #     separate(label, into = c("age_group", "coverage_category"), sep = ":") %>%
    #     mutate(age_group = str_squish(str_replace_all(age_group, "years", "")),
    #            age_group = ifelse(age_group == "65 and over", "65+", age_group),
    #            coverage_category = ifelse(coverage_category == "With health insurance coverage", "coverage", "no coverage"),
    #            label = paste0(age_group, ": ", coverage_category))
    #   
    #   ggplot(insurance, aes(label, percent)) +
    #     geom_chicklet(stat = "identity", width = 0.3, fill = "#63CF89") +
    #     coord_flip() +
    #     labs(y = "% of county's Latinx pop.",
    #          x = "Age x health insurance") +
    #     theme(panel.background = element_rect(fill = "transparent")) +
    #     theme_minimal() +
    #     scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    #     theme(panel.grid.minor =element_blank(),
    #           panel.grid.major = element_blank(),
    #           axis.line.y = element_line(color = "#cacee6", size = 0.8),
    #           axis.ticks.y = element_line(color = "#cacee6", size = 0.8),
    #           axis.text = element_text(family = "Karla", color = "#51535e", size = 14),
    #           axis.title = element_text(family = "Karla", color = "#51535e", size = 15))
    #   
    # })
    
    #----------------------End Tab 2-------------------------------------
    
    #---------------------Begin Tab 2 Alt A-----------------------------
    
    # Header
    output$highlighted_map <- renderPlot({
      
      ia_county_shp %>%
        mutate(input_status = ifelse(NAME == input$county_choice2, "1", "0")) %>%
        ggplot(aes(fill = input_status)) +
        geom_sf(color = "#172B4D") +
        scale_fill_manual(values = c("#172B4D", "#5E72E4")) +
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
    
    output$overview <- renderText({
      
      median_age <- median_age_df()
      median_age_hisp <- median_age_hisp_df()
      hh_income <- hh_income_df()
      hh_income_hisp <- hh_income_hisp_df()
      
      glue("The median age of the county's population in 2019 was {median_age$estimate[median_age$variable_index == '001']} years (± {median_age$moe[median_age$variable_index == '001']}).
           For females, the median was {median_age$estimate[median_age$variable_index == '003']} years (± {median_age$moe[median_age$variable_index == '003']}), while for males, the median was 
           {median_age$estimate[median_age$variable_index == '002']} years (± {median_age$moe[median_age$variable_index == '002']}). For the Latinx population, the median
           age was {median_age_hisp$estimate[median_age_hisp$variable_index == '001']} years (± {median_age_hisp$moe[median_age$variable_index == '001']}). Latino males
           had a median age of {median_age_hisp$estimate[median_age_hisp$variable_index == '002']} years (± {median_age_hisp$moe[median_age$variable_index == '002']}), while the median age for Latinas
           alone was {median_age_hisp$estimate[median_age_hisp$variable_index == '003']} years (± {median_age_hisp$moe[median_age$variable_index == '003']}).
           <br></br>
           The median household income for the county in 2019 was ${format(hh_income$estimate, big.mark = ',')} (± ${format(hh_income$moe, big.mark = ',')}) in 2019-inflation-adjusted dollars.
           For the Latinx population, the median household income in the same year was ${format(hh_income_hisp$estimate, big.mark = ',')} (± ${format(hh_income_hisp$moe, big.mark = ',')}).")
      
      
    })
    
    
    map_var2 <- reactive({if (input$unit2 == "as a %") "percent" else "estimate" })
    map_symbol2 <- reactive ({ if (input$unit2 == "as a %") "%" else ""})
    
    # Interactive leaflet map
    output$map2 <- renderLeaflet({
      
      argon_map <- "https://api.mapbox.com/styles/v1/sullivannicole/ckp33bliz5lh817o0g3xsrnyw/tiles/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic3VsbGl2YW5uaWNvbGUiLCJhIjoiY2prZTRzcnBvMDA1bTNwcGdkM2poamd6cyJ9.-Edd8XaXp1XUm6vUyReerw"
      map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © <a href='https://www.creative-tim.com/product/argon-design-system'>Argon-Style</a>"
      
      pal <- colorNumeric(c("#29066B", "#7D3AC1", "#AF4BCE", "#DB4CB2", "#EA7369", "#F0A58F", "#FCEAE6"), NULL)
      
      # ia_counties_tidy %>%
      ia_metro_tidy %>%
        filter(variable_group == "B03001" & variable_index == '003' & county_name == input$county_choice2) %>%
        rename(var = !!map_var2()) %>%
        leaflet() %>%
        addTiles(urlTemplate = argon_map, attribution = map_attr) %>%
        # addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.9,
                    fillColor = ~pal(var),
                    label = ~paste0(tract_desc, ": ", formatC(var, big.mark = ","), map_symbol2())) %>% # use NAME variable for county
        addLegend(pal = pal, values = ~var, opacity = 0.3)
    })
    
    # Card 1: Demographics--------------------------------------------------------
    
    output$arcplot_origin2 <- renderPlot({
      
      pc_latin_origin <- ia_counties_tidy %>%
        filter(county_name == input$county_choice2 & variable_group == "B06004I" & variable_index != "001") %>%
        mutate(ymax = cumsum(prop),
               ymin = lag(ymax),
               ymin = ifelse(is.na(ymin), 0, ymin),
               label = str_replace_all(label, "Estimate!!Total:!!", "")) %>%
        mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
      
      pc_latin_origin %>%
        ggplot() +
        ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
        coord_fixed() +
        scale_fill_manual(values = c("#5E72E4", "#172B4D", "#63CF89", "#5DCEF0")) +
        scale_color_manual(values = c("#5E72E4", "#172B4D", "#63CF89", "#5DCEF0")) +
        labs(color = "",
             fill = "") +
        arc_ggtheme +
        guides(fill = guide_legend(nrow = 2,byrow = TRUE))
      
    })
    
    
    output$arcplot_origin_text2 <- renderText({ 
      
      pc_latin_origin <- ia_counties_tidy %>%
        filter(county_name == input$county_choice2 & variable_group == "B06004I" & variable_index != "001") %>%
        mutate(label = str_replace_all(label, "Estimate!!Total:!!", ""),
               percent = round(as.numeric(percent)))
      
      born_ia <- pc_latin_origin %>% filter(label == "Born in state of residence")
      born_other_state <- pc_latin_origin %>% filter(label == "Born in other state in the United States")
      native_born_outside <- pc_latin_origin %>% filter(label == "Native; born outside the United States")
      foreign_born <- pc_latin_origin %>% filter(label == "Foreign born")
      
      if (born_ia$percent > 0) born_ia_sentence <- glue("{born_ia$percent}%  of the county's Latinx are Iowa-born. ") else born_ia_sentence <- ""
      if (born_other_state$percent > 0) born_other_state_sentence <- glue("Another {born_other_state$percent}% of the county's Latinx Iowans were born in another state in the US. ") else born_other_state_sentence <- ""
      if (native_born_outside$percent > 0) native_born_outside_sentence <- glue("Of the rest of the Latinx in {input$county_choice2}, {native_born_outside$percent}% are native, born outside the US. ") else native_born_outside_sentence <- ""
      if(foreign_born$percent > 0) foreign_born_sentence <- glue("An estimated {foreign_born$percent}% of the county's Latinx were foreign born. ") else foreign_born_sentence <- ""
      
      glue("{born_ia_sentence}{born_other_state_sentence}{native_born_outside_sentence}{foreign_born_sentence}")
      
    })
    
    output$bar_status <- renderPlot({
      
    # Relationship status
      status <- ia_counties_tidy %>%
        filter(variable_group == "B12002I" & county_name == input$county_choice2 & !variable_index %in% c("001", "002", "008")) %>%
        filter(percent != 0) %>%
        mutate(label = str_replace_all(label, ":", ", "),
               label = str_replace_all(label, "married \\(", "married \n\\("),
               gender = ifelse(substr(label, 1, 4) == "Male", "Male", "Female"),
               label = str_remove_all(label, "Male, |Female, "))
      
      ggplot(status, aes(percent, label, fill = gender)) +
        geom_bar(stat = "identity", width = 0.4, position = position_dodge()) +
        geom_errorbar(aes(xmin = percent-moe_pc, xmax = percent+moe_pc), 
                      width = 0.05, color = "#4f515c", position = position_dodge(0.4)) +
        scale_fill_manual(values = c(hex_green, hex_purple)) +
        labs(y = "", 
             x = glue("% of Latinx pop. in {unique(status$county_name)}"),
             fill = "") +
        theme_minimal() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        project_ggtheme
    
    })
    
    output$lollipop_language <- renderPlotly({
      
      # Language at home
      language_plot <- ia_counties_tidy %>%
        filter(variable_group == "B16006" & county_name == input$county_choice2 & !(variable_index %in% c("001", "003")) & percent > 0) %>%
        mutate(label = str_replace(label, ":", "\n")) %>%
        ggplot(aes(text = glue("{round(percent, 1)}% \n {label}"))) +
        geom_segment(aes(x=label, xend=label, y=0, yend=percent), color=hex_purple, size = 0.8) +
        geom_point(aes(label, percent), color=hex_purple, size=3.5) +
        labs(y = "% of Latinx pop.",
             x = "Language spoken at home \nby English proficiency\n") +
        coord_flip() +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.title = element_text(color = "#51535e"))
      
      ggplotly(language_plot, tooltip = "text") %>%
        layout(font = list(family = "Karla")) %>%
        style(hoverlabel = list(bgcolor = "#172B4D",
                                bordercolor = "#172B4D",
                                font = list(family = "Karla", color = "white")))
      
    })
    
    output$arcplot_heritage2 <- renderPlot({
      
      ia_counties_tidy %>%
        filter(county_name == input$county_choice2 & variable_group == "B03001" & variable_index %in% c("003", "004", "005", "006", "008", "016", "027")) %>%
        mutate(denom = ifelse(variable_index == "003", estimate, NA)) %>%
        fill(denom, .direction = "updown") %>%
        mutate(prop = estimate/denom,
               percent = prop*100) %>%
        filter(percent != 100 & percent != 0) %>%
        mutate(ymax = cumsum(prop),
               ymin = lag(ymax),
               ymin = ifelse(is.na(ymin), 0, ymin),
               label = trimws(str_replace_all(label, "Hispanic or Latino:|:", ""))) %>%
        mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1) %>%
        ggplot() +
        ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
        coord_fixed() +
        scale_fill_manual(values = c("#5E72E4",  "#172B4D","#EC603E", "#EA445B", "#63CF89", "#5DCEF0")) +
        scale_color_manual(values = c("#5E72E4",  "#172B4D","#EC603E", "#EA445B", "#63CF89", "#5DCEF0")) +
        labs(color = "",
             fill = "") +
        arc_ggtheme +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
      
    })
    
    output$arcplot2 <- renderPlot({
      
      pc_latin <- ia_counties_tidy %>%
        filter(county_name == input$county_choice2 & variable_group == "B03003" & variable_index != '001') %>%
        mutate(ymax = cumsum(prop),
               ymin = lag(ymax),
               ymin = ifelse(is.na(ymin), 0, ymin),
               label = as.factor(label)) %>%
        mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
      
      # Arc chart for % of county that's Latino
      pc_latin %>%
        ggplot() +
        ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
        coord_fixed() +
        scale_fill_manual(values = c("#5E72E4", "#172B4D")) +
        scale_color_manual(values = c("#5E72E4", "#172B4D")) +
        labs(color = "",
             fill = "") +
        arc_ggtheme
      
    })
    
    # Card 2: Economics & Workforce--------------------------------------------------------
    output$bar_gender_work <- renderPlot({
      
      # Employment rate by gender
      gender_work <- ia_counties_tidy %>%
        filter(variable_group == "B20005I" & county_name == input$county_choice2 & variable_index %in% c("005", "027", "028", "052", "074", "075") & percent > 0) %>%
        mutate(label = str_remove(label, ", year-round in the past 12 months"),
               label = str_replace_all(label, "s:", "s"),
               label = str_replace_all(label, ":", ", "),
               gender = ifelse(substr(label, 1, 4) == "Male", "Male", "Female"),
               label = str_remove_all(label, "Male, |Female, "),
               label = str_replace_all(label, ",", ", \n"))
      
      ggplot(gender_work, aes(percent, label, fill = gender)) +
        geom_bar(stat = "identity", width = 0.4, position=position_dodge()) +
        geom_errorbar(aes(xmin = percent-moe_pc, xmax = percent+moe_pc), 
                      width = 0.1, color = "#4f515c", position = position_dodge(0.4)) +
        scale_fill_manual(values = c(hex_pink, hex_purple)) +
        labs(y = "", 
             x = glue("% of Latinx pop. in {unique(gender_work$county_name)}")) +
        labs(fill = "") +
        theme_minimal() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        project_ggtheme
      
    })
    
    output$arc_homeownership <- renderPlot({
      
      # Homeownership
      tenure <- ia_counties_tidy %>%
        filter(variable_group == "B25003I" & county_name == input$county_choice2 & variable_index != "001") %>%
        mutate(ymax = cumsum(prop),
               ymin = lag(ymax),
               ymin = ifelse(is.na(ymin), 0, ymin),
               label = as.factor(label)) %>%
        mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
      
      tenure %>%
        ggplot() +
        ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.9, r = 1, start = ymin, end = ymax, fill = label, color = label)) +
        coord_fixed() +
        scale_fill_manual(values = c("#5E72E4", "#172B4D")) +
        scale_color_manual(values = c("#5E72E4", "#172B4D")) +
        labs(color = "",
             fill = "") +
        arc_ggtheme
      
    })
    
    # Lollipop chart of means of transportation to work
    
    output$chicklet_poverty2 <- renderPlot({
      
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
        ggplot(aes(poverty_group, percent)) +
        geom_bar(stat = "identity", width = 0.1, fill = "#5E72E4", color = "#5E72E4") +
        coord_flip() +
        labs(x = "",
             y = "% of Latinx pop. in county") +
        theme_minimal() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        project_ggtheme
      
    })
    
    output$lollipop_transportation2 <- renderPlotly({
      
      # Means of transportation
      means_transportation <- ia_counties_tidy %>%
        filter(county_name == input$county_choice2 & variable_group == "B08105I" & variable_index != "001" & percent > 0) %>%
        ggplot(aes(text = glue("{round(percent)}% \n {label}"))) +
        geom_segment(aes(x=label, xend=label, y=0, yend=percent), color="#EC603E", size = 0.8) +
        geom_point(aes(label, percent), color="#EC603E", size=3.5) +
        labs(y = "% of Latinx pop.",
             x = "") +
        coord_flip() +
        theme_minimal() +
        theme(panel.grid = element_blank())
      
      ggplotly(means_transportation, tooltip = "text") %>%
        layout(font = list(family = "Karla")) %>%
        style(hoverlabel = list(bgcolor = "#172B4D",
                                bordercolor = "#172B4D",
                                font = list(family = "Karla", color = "white")))
      
    })
    
    output$chicklet_insurance2 <- renderPlot({
      
      insurance <- ia_counties_tidy %>%
        filter(county_name == input$county_choice2 & variable_group == "C27001I" & variable_index %in% c("003", "004", "006", "007", "009", "010")) %>%
        separate(label, into = c("age_group", "coverage_category"), sep = ":") %>%
        mutate(age_group = str_squish(str_replace_all(age_group, "years", "")),
               age_group = ifelse(age_group == "65 and over", "65+", age_group),
               coverage_category = ifelse(coverage_category == "With health insurance coverage", "coverage", "no coverage"),
               label = paste0(age_group, ": ", coverage_category))
      
      ggplot(insurance, aes(label, percent)) +
        geom_chicklet(stat = "identity", width = 0.3, fill = "#63CF89") +
        coord_flip() +
        labs(y = "% of county's Latinx pop.",
             x = "Age x health insurance") +
        theme(panel.background = element_rect(fill = "transparent")) +
        theme_minimal() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        project_ggtheme
      
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
      scale_color_manual(values = c(hex_pink, hex_blue_dk)) +
      labs(y = "% of Latinx pop.",
           x = "Educational attainment\n",
           color = "") +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            legend.position = "bottom")
    
    ggplotly(education_plot, tooltip = "text") %>%
      layout(font = list(family = "Karla")) %>%
      style(hoverlabel = list(bgcolor = "#172B4D",
                              bordercolor = "#172B4D",
                              font = list(family = "Karla", color = "white")))
    
    })
    
    output$arc_disciplines <- renderPlot({
      
      # Disciplines in school
      disciplines <- ia_counties_tidy %>%
        filter(variable_group == "C15010I" & county_name == input$county_choice2 & !(variable_index %in% c("001")))  %>%
        mutate(ymax = cumsum(prop),
               ymin = lag(ymax),
               ymin = ifelse(is.na(ymin), 0, ymin),
               label = as.factor(label)) %>%
        mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
      
      disciplines %>%
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
    
    output$bar_computer <- renderPlotly({
      
      # Presence of a computer/type of internet
      internet <- ia_counties_tidy %>%
        filter(variable_group == "B28009I" & county_name == input$county_choice2 & !(variable_index %in% c("001", "002"))) %>%
        mutate(label = str_remove_all(label, "Has a | subscription alone| subscription|With a |With "),
               label = str_replace(label, "Without an", "Without"),
               label = str_to_sentence(str_replace(label, ":", ",\n")),
               text = paste0(label, ": ", round(percent, 1), "%"))
      
      
      internet_bar <- ggplot(internet, aes(percent, label, text = text)) +
        geom_bar(stat = "identity", width = 0.4, fill = hex_orange) +
        geom_errorbar(aes(xmin = percent-moe_pc, xmax = percent+moe_pc), 
                      width = 0.1, color = "#4f515c") +
        labs(y = "", 
             x = glue("% of Latinx pop. in {unique(internet$county_name)}")) +
        labs(fill = "") +
        theme_minimal() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        project_ggtheme
      
      ggplotly(internet_bar, tooltip = "text") %>%
        layout(font = list(family = "Karla")) %>%
        style(hoverlabel = list(bgcolor = "#172B4D",
                                bordercolor = "#172B4D",
                                font = list(family = "Karla", color = "white")))
      
    })
    
    output$arc_enrolled <- renderPlot({
      
      # Enrolled in school
      school_enrollment <- ia_counties_tidy %>%
        filter(variable_group == "B14007I" & county_name == input$county_choice2 & !(variable_index %in% c("001", "002"))) %>%
        mutate(label = str_remove(label, "Enrolled in school:"),
               label = str_replace(label, "Enrolled in college", "College"),
               label = ifelse(substr(label, 1, 2) == "En", "Pre-k through 12th", label)) %>%
        group_by(label) %>%
        summarize(prop = sum(prop)) %>%
        ungroup() %>%
        mutate(ymax = cumsum(prop),
               ymin = lag(ymax),
               ymin = ifelse(is.na(ymin), 0, ymin),
               label = as.factor(label)) %>%
        mutate_at(c("ymin", "ymax"), rescale, to = pi*c(-.5, .5), from = 0:1)
      
      school_enrollment %>%
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
    
    
    #---------------------End Tab 2 Alt A-----------------------
    
    
    #---------------------Begin Tab 3------------------------------------
    
    output$bar_median_age <- renderPlotly({
      
      median_age <- state_county %>%
        filter(variable == "B01002I_001" & NAME %in% input$region_choice) %>%
        mutate(text = paste(NAME, ": ", estimate, " years"))
      
      age_plot <- ggplot(median_age, aes(NAME, estimate, text = text)) +
        geom_bar(stat = "identity", width = 0.1, fill = "#5E72E4") +
        # geom_text(aes(x = NAME, y = estimate), size = 3.5, family = "Karla") +
        labs(x = "", y = "Median age of Latinx pop. (in years)") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.line.x = element_line(color = "#cacee6", size = 0.8))
      
      ggplotly(age_plot, tooltip = "text") %>%
        layout(font = list(family = "Karla")) %>%
        style(hoverlabel = list(bgcolor = "#172B4D",
                                bordercolor = "#172B4D",
                                font = list(family = "Karla", color = "white")))
    })
    
    output$lollipop_income <- renderPlotly({
      
      income <- state_county %>%
        filter(variable == "B19013I_001" & NAME %in% input$region_choice) %>%
        mutate(text = paste0(NAME, ": $", format(estimate, big.mark = ",")))
      
      income_plot <- ggplot(income, aes(text = text)) +
        geom_linerange(aes(x = NAME, ymin = 0, ymax = estimate), color = hex_blue_dk, size = 1)+
        geom_point(aes(x = NAME, y = estimate), color = hex_blue_dk, size = 2.5) +
        labs(x = "",
             y = "Median household income\n\n",
             color = "") +
        coord_flip() +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(color = hex_grey))
      
      ggplotly(income_plot, tooltip = "text") %>%
        layout(font = list(family = "Karla")) %>%
        style(hoverlabel = list(bgcolor = "#172B4D",
                                bordercolor = "#172B4D",
                                font = list(family = "Karla", color = "white")))
    })
    
    #---------------------End Tab 3--------------------------------------
    
    
    #---------------------Begin Tab 4------------------------------------
    
    output$income_disparities <- renderPlotly({
      
      
      income_df <- disparities %>%
        filter(year >= 2021) %>%
        select(year, per_capita_income, per_capita_income_eliminating_disparities) %>%
        mutate(per_capita_income_eliminating_disparities = per_capita_income + 
                 (per_capita_income_eliminating_disparities-per_capita_income)*input$income_knob/100,
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
        style(hoverlabel = list(#bgcolor = hex_purple,
                                bordercolor = "white",
                                font = list(family = "Karla", color = "white")))
      
    })
    
    output$earnings_disparities <- renderPlot({
      
      
      dollars_df <- disparities %>%
        filter(year >= 2021) %>%
        select(year, latinx_dollars_earned, dollars_earned_eliminate_disparities) %>%
        mutate(latinx_dollars_earned = latinx_dollars_earned/1000000000,
               dollars_earned_eliminate_disparities = dollars_earned_eliminate_disparities/1000000000,
               dollars_earned_eliminate_disparities = latinx_dollars_earned + 
                 (dollars_earned_eliminate_disparities-latinx_dollars_earned)*input$earnings_knob/100)
      
      ggplot(dollars_df, aes(year, latinx_dollars_earned)) +
        geom_line(aes(y = latinx_dollars_earned, color = "With disparities \n(current trajectory)"), size = 1.2) +
        geom_line(aes(y = dollars_earned_eliminate_disparities, color = "Without disparities"), size = 1.2) +
        scale_color_manual(values = c(hex_pink, hex_green)) +
        geom_ribbon(aes(ymin = latinx_dollars_earned, ymax = dollars_earned_eliminate_disparities), alpha = 0.2, fill = hex_green) +
        theme(legend.position = "bottom") +
        labs(y = "Latinx dollars earned (in $B)\n",
             color = "") +
        ylim(0.9, 9) +
        # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
        project_ggtheme
      
    })
    
    output$poverty_disparities <- renderPlot({
      
      
      poverty_df <- disparities %>%
        filter(year >= 2021) %>%
        select(year, latinx_people_in_poverty, poverty_with_disparities_eliminated) %>%
        mutate(poverty_with_disparities_eliminated = latinx_people_in_poverty + 
                 (poverty_with_disparities_eliminated-latinx_people_in_poverty)*input$poverty_knob/100)
      
      ggplot(poverty_df, aes(year, latinx_people_in_poverty)) +
        geom_line(aes(y = latinx_people_in_poverty, color = "With disparities \n(current trajectory)"), size = 1.2) +
        geom_line(aes(y = poverty_with_disparities_eliminated, color = "Without disparities"), size = 1.2) +
        scale_color_manual(values = c(hex_pink, hex_green)) +
        geom_ribbon(aes(ymin = latinx_people_in_poverty, ymax = poverty_with_disparities_eliminated), alpha = 0.2, fill = hex_green) +
        theme(legend.position = "bottom") +
        labs(y = "Latinx individuals in poverty\n",
             color = "") +
        ylim(3500, 24000) +
        # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
        project_ggtheme
      
    })
    
    output$homeowners_disparities <- renderPlot({
      
      
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
)