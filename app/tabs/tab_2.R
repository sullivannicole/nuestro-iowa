tab_2 <- argonTabItem(
  tabName = "tab_2",
  
  # Card 1: Demographics--------------------------------------------------------
  argonCard(
    title = "Birth & migration",
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    width = 12,
    
    # Row 1: Select county to filter entire tab
    argonRow(argonColumn(br(), h1("County:")) %>% argonTextColor(color = "primary") %>% argonMargin(orientation = 'l', value = 2),
              argonColumn(width = 10, pickerInput("county_choice", width = "100%", # Make it span the entire column, resizing automatically
                         label = "", 
                         choices = c( "Dallas","Guthrie", "Jasper", "Madison","Marshall", "Polk", "Warren"),
                         # When ready to go to whole state, delete line above and un-comment line below:
                         # choices = county_names$county_name, 
                         selected = "Dallas",
                         multiple = FALSE,
                         options = list(create = FALSE,
                                        style = "btn-danger",
                                        `live-search` = TRUE)))),
             # argonColumn(plotOutput("tinymap", width = "100%", height = "30%"))),
    
    # Row 2: arcplot showing birthplace/origin
    argonRow(argonColumn(width = 8,
                         plotOutput("arcplot_origin")),
             argonColumn(br(),
                         br(),
                         br(),
                         h3("Birthplace"),
                         div(style = "display: inline-block;", h4("Of the ")), div(style = "display: inline-block", textOutput("county_chosen_total_latinx") %>% h4()),
                         div(style = "display: inline-block", h4(" Latinx residing in ")), div(style = "display: inline-block", textOutput("county_chosen_text") %>% h4()),
                         div(style = "display: inline-block", h4("county,")),
                         textOutput("arcplot_origin_text"))
    ),
    
    # Row 4: arcplot of heritage of county's Latinx
    argonRow(argonColumn(width = 8,
                         plotOutput("arcplot_heritage")),
             argonColumn(br(),
                         br(),
                         br(),
                         br(),
                         h3("Ancestral origin"),
                         "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.")),
    
    # Row 4: arcplot showing % of county that's Latinx
    argonRow(argonColumn(width = 8,
                         plotOutput("arcplot")),
             argonColumn(br(),
                         br(),
                         br(),
                        "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text."))
    
),
# End Card 1--------------------------------------------------------
br(),
br(),

# Card 2: Economics & Workforce--------------------------------------------------------
argonCard(
  title = "Economics & Workforce",
  hover_lift = FALSE,
  shadow = TRUE,
  shadow_size = NULL,
  hover_shadow = FALSE,
  border_level = 0,
  width = 12,
  argonRow(argonColumn(width = 8,
                       plotOutput("chicklet_poverty")),
           argonColumn(br(),
                       br(),
                       br(),
                       br(),
                       h3("Poverty status by age group"),
                       "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.")),
  argonRow(argonColumn(width = 8,
                       plotlyOutput("lollipop_transportation")),
           argonColumn(br(),
                       br(),
                       br(),
                       br(),
                       h3("Means of transportation to work"),
                       "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text."))
),

# Card 3: Education--------------------------------------------------------
argonCard(
  title = "Education",
  hover_lift = FALSE,
  shadow = TRUE,
  shadow_size = NULL,
  hover_shadow = FALSE,
  border_level = 0,
  width = 12
),

# Card 4: Health--------------------------------------------------------
argonCard(
  title = "Health",
  hover_lift = FALSE,
  shadow = TRUE,
  shadow_size = NULL,
  hover_shadow = FALSE,
  border_level = 0,
  width = 12,
  argonRow(argonColumn(width = 8,
                       plotOutput("chicklet_insurance")),
           argonColumn(h3("Health insurance coverage"),
                       "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text."))
)
)


