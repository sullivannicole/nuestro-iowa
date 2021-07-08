tab_2_alt_a <- argonTabItem(
  tabName = "tab_2_alt_a",
  
  # Header
  div(class = "tab-2-header-div",
      
      # Spacer div
      div(style = "height: 150px;"),
      
      # Row 1
      argonRow(width = 12,
               argonColumn(width = 1),
               argonColumn(width = 4,
                           div(style = "height: 45px"),
                           argonRow(argonH1(display = 3, "County") %>% argonTextColor("white")),
                           argonRow(pickerInput("county_choice2",
                                                # width = "100%", # Make it span the entire column, resizing automatically
                                                label = "",
                                                choices = c( "Dallas","Guthrie", "Jasper", "Madison","Marshall", "Polk", "Warren"),
                                                # When ready to go to whole state, delete line above and un-comment line below:
                                                # choices = county_names$county_name,
                                                selected = "Dallas",
                                                multiple = FALSE,
                                                options = list(create = FALSE,
                                                               style = "btn-default",
                                                               `live-search` = TRUE))),
                           br(),
                           div(style = "padding-left: 40px;",
                               argonRow(
                                 div(style = "display: inline-block; color: #172B4D; font-size: 25px; font-weight: bold; margin-right: 7px",
                                     "TOTAL POP: ", " "),
                                 div(style = "display: inline-block;", 
                                     textOutput("region_pop"))),
                               argonRow(div(style = "display: inline-block; color: #172B4D; font-size: 25px; font-weight: bold; margin-right: 7px",
                                            "LATINX POP:"),
                                        div(style = "display: inline-block;", 
                                            textOutput("region_latinx_pop"))),
                               argonRow(div(style = "display: inline-block; color: #172B4D; font-size: 25px; font-weight: bold; margin-right: 7px",
                                            "% LATINX:"),
                                        div(style = "display: inline-block;", 
                                            textOutput("region_pc_latinx")))),
                           br(),
                           br(),
                           br(),
                           br()
               ),
               argonColumn(width = 7,
                           plotOutput("highlighted_map", height = "100%")))
  ),
  
  # Div wrap around cards under header to push down 550px
  
  # # Row 1: Cards
  div(style = "padding: 650px 15px 0px 45px;
               margin-bottom: 125px",
      
      argonRow(argonColumn(width = 8,
                           argonH1("Overview", display = 3),
                           htmlOutput("overview")),
               # tags$ul(
               #   tags$li(textOutput("median_age")),
               #   tags$li(textOutput("median_male_age")),
               #   tags$li(textOutput("median_female_age")))),
               
               argonCard(width = 4,
                         title = "Fast Facts",
                         "Placeholder text. Placeholder text.")
               
      ) # End row
      
  ), # End div wrap around cards at top
  # 
  # # Row 2: Map
  # 
  argonRow(width = 12,
           div(class = "map-btn-container",
               dropdownButton(radioButtons("unit2",
                                           "See Latinx pop. as a:",
                                           c("percent of the tract pop." = "as a %",
                                             "raw number" = "as a raw num"),
                                           selected = "as a raw num"))),
           div(class = "map-container", leafletOutput("map2"))),
  br(),
  br(),
  br(),
  
  
  # Div around everything under map to push items down
  div(style = "padding-top: 480px;",
      
      # Card 1: Demographics--------------------------------------------------------
      argonCard(
        title = "Demographics",
        hover_lift = FALSE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        width = 12,
        
        # Arcplot showing birthplace/origin
        argonRow(argonColumn(width = 8,
                             plotOutput("arcplot_origin2")),
                 argonColumn(div(style = "height: 85px;"), # spacer div
                             h3("Birthplace"),
                             textOutput("arcplot_origin_text2"))
        ),
        
        # Relationship status bar
        argonRow(argonColumn(width = 8,
                             plotOutput("bar_status")),
                 argonColumn(
                   div(style = "height: 85px;"), # spacer div
                   h3("Marital status"),
                   "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.")),
        
        # Language spoken at home lollipop
        argonRow(argonColumn(width = 8,
                             plotlyOutput("lollipop_language")),
                 argonColumn(
                   div(style = "height: 85px;"), # spacer div
                   h3("Language spoken at home"),
                   "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.")),
        
        # Arcplot of heritage of county's Latinx
        argonRow(argonColumn(width = 8,
                             plotOutput("arcplot_heritage2")),
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
        
        # Row 4: arcplot showing % of county that's Latinx - not necessary anymore since it's in header
      #   argonRow(argonColumn(width = 8,
      #                        plotOutput("arcplot2")),
      #            argonColumn(style = "height: 85px;",
      #                        "Placeholder text. Placeholder text.
      #                    Placeholder text. Placeholder text.
      #                    Placeholder text. Placeholder text.
      #                    Placeholder text. Placeholder text.
      #                    Placeholder text. Placeholder text."))
      #   
      ), # End Card 1--------------------------------------------------------
      
      div(style = "height: 60px;"), # spacer div
      
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
                             plotOutput("bar_gender_work")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Work experience by sex"),
                             "Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.")),
        argonRow(argonColumn(width = 8,
                             plotOutput("arc_homeownership")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Homeownership"),
                             "Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.")),
        argonRow(argonColumn(width = 8,
                             plotlyOutput("lollipop_transportation2")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Means of transportation to work"),
                             "Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.
                              Placeholder text. Placeholder text.")),
        argonRow(argonColumn(width = 8,
                             plotOutput("chicklet_poverty2")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Poverty status by age group"),
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
        width = 12,
        argonRow(argonColumn(width = 8,
                             plotlyOutput("lollipop_education")),
                 argonColumn(div(style = "height: 100px;"),
                             h3("Educational Attainment"),
                             "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.")),
        argonRow(argonColumn(width = 8,
                             plotOutput("arc_disciplines")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Disciplines of Bachelor's degrees"),
                             "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.")),
        argonRow(argonColumn(width = 8,
                             plotlyOutput("bar_computer")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Presence of a computer/internet in the home"),
                             "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.")),
        argonRow(argonColumn(width = 8,
                             plotOutput("arc_enrolled")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("School enrollment status"),
                             "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text."))
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
                             plotOutput("chicklet_insurance2")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Health insurance coverage"),
                             "Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text.
                         Placeholder text. Placeholder text."))
      )
  ) # End of div wrap of everything under map
  
)