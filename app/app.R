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

# install.packages(c("usethis", "gitcreds", "gh"))
# library(usethis)
# library(gitcreds)
# library(gh)

# usethis::use_git_config(user.name = "sullivannicole", user.email = "nicasull@gmail.com")
# usethis::create_github_token()
# gitcreds::gitcreds_set()
# gh::gh_whoami()
# usethis::git_sitrep()
# credentials::ssh_setup_github()

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
source("tabs/tab_0.R")
source("tabs/tab_1.R")
source("tabs/tab_2.R")

# App
shiny::shinyApp(
  ui = argonDashPage(
    title = "Argon Dashboard Nuestro IA",
    author = "Al Exito Iowa",
    description = "Argon Dash Nuestro IA",
    sidebar = argonSidebar,
    gfonts::use_font("karla", "www/css/karla.css"),
    tags$style(type = "text/css", ".header-container {position:absolute; top:0; bottom:0; right:0; left:0;}
                                    .map-container {position:absolute; right:0; left:0; /*z-index: -1;*/ padding: 40px 20px 40px 20px; background-color: #e9ecef;}
                                    .map-btn-container {position: absolute; z-index: 1; padding-top: 50px}
                                    .navbar-vertical.navbar-expand-md {overflow-y: hidden !important; padding-left: 40px !important; padding-right: 40px !important;} "),
    # navbar = argonNav, 
    # header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        tab_1,
        tab_2,
        tab_0
      )
    ),
    footer = argonFooter
  ),
  server = function(input, output) {

    # Tab 1-----------------------------------------------------------
    
    output$state_total_latinx <- renderText({ 
      
      # % of state that's Latinx
      # ia_state_pc_latinx <- ia_state %>%
        # filter(variable %in% c("B01001I_001", "B01001_001") & year == acs_yr) %>%
        # spread(variable, value = estimate) %>%
        # fill("B01001I_001", .direction = "updown") %>%
        # fill("B01001_001", .direction = "updown") %>%
        # filter(row_number() == 1) %>%
        # mutate(pc_of_state_latinx = B01001I_001/B01001_001*100)
      
      # glue("{round(ia_state_pc_latinx$B01001I_001/1000, 0)}K Latinx in the state of Iowa")})
      
      
      # % of metro that's Latinx
      ia_metro_pc_latinx <- ia_metro_labeled %>%
        filter(variable %in% c("B01001I_001", "B01001_001") & year == acs_yr) %>%
        tibble() %>%
        select(-geometry) %>%
        group_by(variable, variable_group, variable_index) %>%
        summarize(estimate = sum(estimate)) %>%
        ungroup() %>%
        spread(variable, value = estimate) %>%
        tidyr::fill("B01001I_001", .direction = "updown") %>%
        tidyr::fill("B01001_001", .direction = "updown") %>%
        filter(row_number() == 1) %>%
        mutate(pc_of_metro_latinx = B01001I_001/B01001_001*100)
      
      glue("{round(ia_metro_pc_latinx$B01001I_001/1000, 0)}K Latinx in the Iowa 7-county metro")})
    
    output$state_pc_latinx <- renderText({

      # % of state that's Latinx
      ia_state_pc_latinx <- ia_state %>%
        filter(variable %in% c("B01001I_001", "B01001_001") & year == acs_yr) %>%
        spread(variable, value = estimate) %>%
        fill("B01001I_001", .direction = "updown") %>%
        fill("B01001_001", .direction = "updown") %>%
        filter(row_number() == 1) %>%
        mutate(pc_of_state_latinx = B01001I_001/B01001_001*100)

      glue("{round(ia_state_pc_latinx$pc_of_state_latinx)}% of the state pop.")

      })

    output$state_pc_latinx_trend <- renderPlot({
      
      # % of state that's Latinx over time
      # ia_state_pc_latinx_trend <- ia_state %>%
      #   filter(variable %in% c("B01001I_001", "B01001_001")) %>%
      #   group_by(year) %>%
      #   mutate(denom = ifelse(variable == "B01001_001", estimate, NA)) %>%	
      #   arrange(year, variable) %>%
      #   fill(denom, .direction = "down") %>%
      #   ungroup() %>%
      #   mutate(pc_latinx = estimate/denom*100) %>%
      #   filter(pc_latinx != 100)
      # 
      # ia_state_pc_latinx_trend %>%
      #   mutate(year = lubridate::ymd(year, truncated = 2L)) %>%
      # ggplot(aes(year, pc_latinx)) +
      #   geom_line(color = "#63CF89", size = 0.8) +
      #   geom_point(shape = 21, fill = "white", color = "#63CF89", size = 2, stroke = 1.5) +
      #   labs(x = "",
      #        y = "") +
      #   theme_minimal() +
      #   theme(panel.grid = element_blank(),
      #         axis.text.y = element_text(size = 13, family = "Karla"),
      #         axis.text.x = element_blank())
      
      # % of metro that's Latinx over time
      ia_metro_pc_latinx_trend <- ia_metro_labeled %>%
        tibble() %>%
        select(-geometry) %>%
        filter(variable %in% c("B01001I_001", "B01001_001")) %>%
        group_by(year, variable) %>%
        summarize(estimate = sum(estimate)) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(denom = ifelse(variable == "B01001_001", estimate, NA)) %>%
        arrange(year, variable) %>%
        fill(denom, .direction = "down") %>%
        ungroup() %>%
        mutate(pc_latinx = estimate/denom*100) %>%
        filter(pc_latinx != 100)
      
      ia_metro_pc_latinx_trend %>%
        mutate(year = lubridate::ymd(year, truncated = 2L)) %>%
      ggplot(aes(year, pc_latinx)) +
        geom_line(color = "#63CF89", size = 0.8) +
        geom_point(shape = 21, fill = "white", color = "#63CF89", size = 2, stroke = 1.5) +
        labs(x = "",
             y = "") +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.text.y = element_text(size = 13, family = "Karla"),
              axis.text.x = element_blank())
      
    }, height = 120, width = 200)
    
    map_var <- reactive({ if (input$unit == "as a %") "percent" else "estimate"})
    map_symbol <- reactive ({ if (input$unit == "as a %") "%" else ""})
    
    # Interactive leaflet map
    output$map <- renderLeaflet({
      
      argon_map <- "https://api.mapbox.com/styles/v1/sullivannicole/ckp33bliz5lh817o0g3xsrnyw/tiles/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic3VsbGl2YW5uaWNvbGUiLCJhIjoiY2prZTRzcnBvMDA1bTNwcGdkM2poamd6cyJ9.-Edd8XaXp1XUm6vUyReerw"
      map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © <a href='https://www.creative-tim.com/product/argon-design-system'>Argon-Style</a>"
      
    # Violet to orange palette
    # pal <- colorNumeric(c("#1c1745", "#EA445B", "#fcb09d"), NULL)
      
    # Deep navy to green
      # pal <- colorNumeric(c("#010106", "#15255B", "#254285", "#3E6EA3", "#599DC1", "#83C1CE", "#C5DDCE"), NULL)
      
      pal <- colorNumeric(c("#29066B", "#7D3AC1", "#AF4BCE", "#DB4CB2", "#EA7369", "#F0A58F", "#FCEAE6"), NULL)
      
      #Dark blue to white palette
      # pal <- colorNumeric(c("#02152E", "#5E72E4", "#5DCEF0", "white"), NULL)
    
    # ia_data_labeled %>%
    iowa_metro_labeled %>%
      filter(variable_group == "B03001" & variable_index == '003') %>%
      rename(var = !!map_var()) %>%
      leaflet() %>%
      addTiles(urlTemplate = argon_map, attribution = map_attr) %>%
      # addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.9,
                  fillColor = ~pal(var),
                  label = ~paste0(county_name, ": ", formatC(var, big.mark = ","), map_symbol())) %>% # use NAME variable for county
      addLegend(pal = pal, values = ~var, opacity = 1.0)
    })
    
    output$tinymap <- renderPlot({
      
      ia_shp_data %>%
        filter(county_name == input$county_choice_test) %>%
        ggplot() +
        geom_sf(fill = "#5E72E4", color = "#5E72E4") +
        labs(title = glue("{input$county_choice}\n\n")) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#5E72E4"))
      
    }, height = 50, width = 100)
    
    # End Tab 1-----------------------------------------------------------
    
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
    
    output$arcplot_origin <- renderPlot({
      
      pc_latin_origin <- ia_data_labeled %>%
        filter(county_name == input$county_choice & variable_group == "B06004I" & variable_index != "001") %>%
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
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.position = "bottom") +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
      
    })
    
    output$county_chosen_text <- renderText({
      
      trimws(input$county_choice)
      
    })
    
    output$county_chosen_total_latinx <- renderText({
      
      county_chosen_total <- ia_data_labeled %>%
        filter(county_name == input$county_choice & concept == 'HISPANIC OR LATINO ORIGIN' & variable_index == '003')
      
      county_chosen_total$estimate
      
      
    })
    
    output$arcplot_origin_text <- renderText({ 
      
      pc_latin_origin <- ia_data_labeled %>%
        filter(county_name == input$county_choice & variable_group == "B06004I" & variable_index != "001") %>%
        mutate(label = str_replace_all(label, "Estimate!!Total:!!", ""),
               percent = round(as.numeric(percent)))
      
      born_ia <- pc_latin_origin %>% filter(label == "Born in state of residence")
      born_other_state <- pc_latin_origin %>% filter(label == "Born in other state in the United States")
      native_born_outside <- pc_latin_origin %>% filter(label == "Native; born outside the United States")
      foreign_born <- pc_latin_origin %>% filter(label == "Foreign born")
      
      if (born_ia$percent > 0) born_ia_sentence <- glue("{born_ia$percent}%  were born in Iowa. ") else born_ia_sentence <- ""
      if (born_other_state$percent > 0) born_other_state_sentence <- glue("Another {born_other_state$percent}% of the county's Latinx Iowans were born in another state in the US. ") else born_other_state_sentence <- ""
      if (native_born_outside$percent > 0) native_born_outside_sentence <- glue("Of the rest of the Latinx in {input$county_choice}, {native_born_outside$percent}% are native, born outside the US. ") else native_born_outside_sentence <- ""
      if(foreign_born$percent > 0) foreign_born_sentence <- glue("An estimated {foreign_born$percent}% of the county's Latinx were foreign born. ") else foreign_born_sentence <- ""
      
      glue("{born_ia_sentence}{born_other_state_sentence}{native_born_outside_sentence}{foreign_born_sentence}")
      
    })
    
    output$arcplot_heritage <- renderPlot({
      
      ia_data_labeled %>%
        filter(county_name == input$county_choice & variable_group == "B03001" & variable_index %in% c("003", "004", "005", "006", "008", "016", "027")) %>%
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
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.position = "bottom") +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
      
    })
   
    output$arcplot <- renderPlot({
      
      pc_latin <- ia_data_labeled %>%
        filter(county_name == input$county_choice & variable_group == "B03003" & variable_index != '001') %>%
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
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.position = "bottom")
      
    })
    
    # Lollipop chart of means of transportation to work
    
    output$chicklet_poverty <- renderPlot({
      
      ia_data_labeled %>% 
        filter(county_name == input$county_choice & variable_group == "B17020I") %>%
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
        theme(panel.grid.minor =element_blank(),
              panel.grid.major = element_blank(),
              axis.line.y = element_line(color = "#cacee6", size = 0.8),
              axis.ticks.y = element_line(color = "#cacee6", size = 0.8),
              axis.text = element_text(family = "Karla", color = "#51535e"),
              axis.title = element_text(family = "Karla", color = "#51535e"))
      
    })
    
    output$lollipop_transportation <- renderPlotly({
      
      # Means of transportation
      means_transportation <- ia_data_labeled %>%
        filter(county_name == input$county_choice & variable_group == "B08105I" & variable_index != "001" & percent > 0) %>%
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
    
    # argonTable
    output$argonTable <- renderUI({
      
      wrap <- if (input$cardWrap == "Enable") TRUE else FALSE
      
      argonTable(
        cardWrap = wrap,
        headTitles = c(
          "PROJECT",
          "BUDGET",
          "STATUS",
          "USERS",
          "COMPLETION",
          ""
        ),
        argonTableItems(
          argonTableItem("Argon Design System"),
          argonTableItem(dataCell = TRUE, "$2,500 USD"),
          argonTableItem(
            dataCell = TRUE, 
            argonBadge(
              text = "Pending",
              status = "danger"
            )
          ),
          argonTableItem(
            argonAvatar(
              size = "sm",
              src = "https://image.flaticon.com/icons/svg/219/219976.svg"
            )
          ),
          argonTableItem(
            dataCell = TRUE, 
            argonProgress(value = 60, status = "danger")
          ),
          argonTableItem(
            argonButton(
              name = "Click me!",
              status = "warning",
              icon = "atom",
              size = "sm"
            )
          )
        )
      )
    })
    
  }
)