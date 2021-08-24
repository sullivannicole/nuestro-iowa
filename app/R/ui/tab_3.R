tab_3 <- argonTabItem(
  tabName = "tab_3",
  
  # County selection
  argonRow(argonColumn(br(), h1("County:")) %>% argonTextColor(color = "primary") %>% argonMargin(orientation = 'l', value = 2),
           argonColumn(width = 10, pickerInput("region_choice", width = "100%", # Make it span the entire column, resizing automatically
                                               label = "", 
                                               choices = c( "Dallas","Guthrie", "Jasper", "Madison","Marshall", "Polk", "Warren"),
                                               # When ready to go to whole state, delete line above and un-comment line below:
                                               # choices = county_names$county_name, 
                                               selected = c("Marshall", "Polk"),
                                               multiple = TRUE,
                                               options = list(create = FALSE,
                                                              style = "btn-danger",
                                                              `live-search` = TRUE)))),
  
  argonCard(
    title = "Income",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10, 
                         withLoader(plotlyOutput("time_income"), loader = "loader1")),
             argonColumn(width = 1))
    
  ), # Close Card 1
  
  argonCard(
    title = "Homeownership",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10,
                         withLoader(plotlyOutput("time_homeownership"), loader = "loader1")),
             argonColumn(width = 1))
    
  ), # Close card 2
  # 
  argonCard(
    title = "Employment",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    
    argonRow(div(style = "padding: 20px 0px 20px 50px",
                 h3("Males employed full-time in last 12 months"))),
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10,
                         withLoader(plotlyOutput("time_employment_male"), loader = "loader1")),
             argonColumn(width = 1)),
    
    argonRow(div(style = "padding: 20px 0px 20px 50px",
                 h3("Females employed full-time in last 12 months"))),
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10,
                         withLoader(plotlyOutput("time_employment_female"), loader = "loader1")),
             argonColumn(width = 1)),
    
    
    
  ), # Close card 3
  
  argonCard(
    title = "Poverty",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    
    argonRow(div(style = "padding: 20px 0px 20px 50px",
                 h3("Living below federal poverty level"))),
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10,
                         withLoader(plotlyOutput("time_poverty"), loader = "loader1")),
             argonColumn(width = 1))
    
  ), # Closes card 4
  
  argonCard(
    title = "Educational attainment",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    
    argonRow(div(style = "padding: 20px 0px 20px 50px",
                 h3("Males 25 and over with Bachelor's degree or higher"))),
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10,
                         withLoader(plotlyOutput("bachelors_male_disparities"), loader = "loader1")),
             argonColumn(width = 1)),
    
    argonRow(div(style = "padding: 20px 0px 20px 50px",
                 h3("Females 25 and over with Bachelor's degree or higher"))),
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10,
                         withLoader(plotlyOutput("bachelors_female_disparities"), loader = "loader1")),
             argonColumn(width = 1)),
    
    
    
  ), # Closes card 5
  
  argonCard(
    title = "High school graduation rates",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    
    # argonRow(div(style = "padding: 20px 0px 20px 50px",
    #              h3("Living below federal poverty level"))),
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10,
                         withLoader(plotlyOutput("hs_rates_disparities"), loader = "loader1")),
             argonColumn(width = 1))
    
  ) # Closes card 6
  
)