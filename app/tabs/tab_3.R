tab_3 <- argonTabItem(
  tabName = "tab_3",
  
  argonCard(
    title = "Compare",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    argonRow(argonColumn(br(), h1("Regions:")) %>% argonTextColor(color = "primary") %>% argonMargin(orientation = 'l', value = 2),
             argonColumn(width = 10, pickerInput("region_choice", width = "100%", # Make it span the entire column, resizing automatically
                                                 label = "", 
                                                 choices = c( "Dallas","Guthrie", "Jasper", "Madison","Marshall", "Polk", "Warren", "State of Iowa"),
                                                 # When ready to go to whole state, delete line above and un-comment line below:
                                                 # choices = county_names$county_name, 
                                                 selected = c("Marshall", "Jasper", "State of Iowa"),
                                                 multiple = TRUE,
                                                 options = list(create = FALSE,
                                                                style = "btn-danger",
                                                                `live-search` = TRUE)))),
    argonRow(argonColumn(width = 1),
              argonColumn(width = 10, 
                    plotlyOutput("bar_median_age")),
             argonColumn(width = 1)),
    
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10, 
                         plotlyOutput("lollipop_income")),
             argonColumn(width = 1))
  ))


