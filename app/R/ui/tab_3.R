tab_3 <- argonTabItem(
  tabName = "tab_3",
  
  argonCard(
    title = "Income",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    argonRow(argonColumn(br(), h1("County:")) %>% argonTextColor(color = "primary") %>% argonMargin(orientation = 'l', value = 2),
             argonColumn(width = 10, pickerInput("region_choice", width = "100%", # Make it span the entire column, resizing automatically
                                                 label = "", 
                                                 choices = c( "Dallas","Guthrie", "Jasper", "Madison","Marshall", "Polk", "Warren"),
                                                 # When ready to go to whole state, delete line above and un-comment line below:
                                                 # choices = county_names$county_name, 
                                                 selected = c("Marshall", "Jasper"),
                                                 multiple = TRUE,
                                                 options = list(create = FALSE,
                                                                style = "btn-danger",
                                                                `live-search` = TRUE)))),
    
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10, 
                         plotOutput("time_income")),
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
    argonRow(argonColumn(br(), h1("County:")) %>% argonTextColor(color = "primary") %>% argonMargin(orientation = 'l', value = 2),
             argonColumn(width = 10, pickerInput("region_choice2", width = "100%", # Make it span the entire column, resizing automatically
                                                 label = "", 
                                                 choices = c( "Dallas","Guthrie", "Jasper", "Madison","Marshall", "Polk", "Warren"),
                                                 # When ready to go to whole state, delete line above and un-comment line below:
                                                 # choices = county_names$county_name, 
                                                 selected = c("Marshall", "Jasper"),
                                                 multiple = TRUE,
                                                 options = list(create = FALSE,
                                                                style = "btn-danger",
                                                                `live-search` = TRUE)))),
    # argonRow(argonColumn(width = 1),
    #           argonColumn(width = 10, 
    #                 plotlyOutput("bar_median_age")),
    #          argonColumn(width = 1)),
    
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10, 
                         plotOutput("time_homeownership")),
             argonColumn(width = 1))
    
  ), # Close card 2
  
  argonCard(
    title = "Employment",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    argonRow(argonColumn(br(), h1("County:")) %>% argonTextColor(color = "primary") %>% argonMargin(orientation = 'l', value = 2),
             argonColumn(width = 10, pickerInput("region_choice3", width = "100%", # Make it span the entire column, resizing automatically
                                                 label = "", 
                                                 choices = c( "Dallas","Guthrie", "Jasper", "Madison","Marshall", "Polk", "Warren"),
                                                 # When ready to go to whole state, delete line above and un-comment line below:
                                                 # choices = county_names$county_name, 
                                                 selected = c("Marshall", "Jasper"),
                                                 multiple = TRUE,
                                                 options = list(create = FALSE,
                                                                style = "btn-danger",
                                                                `live-search` = TRUE)))),
    # argonRow(argonColumn(width = 1),
    #           argonColumn(width = 10, 
    #                 plotlyOutput("bar_median_age")),
    #          argonColumn(width = 1)),
    
    argonRow(div(style = "padding: 20px 0px 20px 50px",
                 h3("Male employment"))),
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10, 
                         plotOutput("time_employment_male")),
             argonColumn(width = 1)),
    
    argonRow(div(style = "padding: 20px 0px 20px 50px",
                 h3("Female employment"))),
    argonRow(argonColumn(width = 1),
             argonColumn(width = 10, 
                         plotOutput("time_employment_female")),
             argonColumn(width = 1))
    
  ) # Close card 3
  
)


