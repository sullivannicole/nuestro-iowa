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
                   textOutput("bar_status_text"))),
        
        # Language spoken at home lollipop
        argonRow(argonColumn(width = 8,
                             plotlyOutput("lollipop_language")),
                 argonColumn(
                   div(style = "height: 85px;"), # spacer div
                   h3("Language spoken at home"),
                        "English proficiency levels vary amongst
                        the county's Latinx community. While
                        some households speak predominantly English
                        at home, other households communicate wholesale
                        en español. Hover over the graph to see the
                        breakdowns.")),
        
        # Arcplot of heritage of county's Latinx
        argonRow(argonColumn(width = 8,
                             plotOutput("arcplot_heritage2")),
                 argonColumn(br(),
                             h3("Ancestral origin"),
                             textOutput("heritage_text"))),
        
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
                             h3("Employment status by sex"),
                             textOutput("gender_work_text"))),
        argonRow(argonColumn(width = 8,
                             plotOutput("arc_homeownership")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Homeownership"),
                             textOutput("homeownership_text"))),
        argonRow(argonColumn(width = 8,
                             plotlyOutput("lollipop_transportation2")),
                 argonColumn(div(style = "height: 50px;"),
                             h3("Means of transportation to work"),
                             "Having proper means of transportation to work is
                             integral to maintaining employment. Hover over the
                             graph at level to explore the predominant means
                             of transportation to work for the Latinx pop.")),
        argonRow(argonColumn(width = 8,
                             plotOutput("chicklet_poverty2")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Poverty status by age group"),
                             textOutput("poverty_text")))
        
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
                 argonColumn(div(style = "height: 120px;"),
                             h3("Educational Attainment"),
                             "Educational attainment differ amongst Latinas
                             and Latino males in the county. Hover over
                             the graph to learn more.")),
        argonRow(argonColumn(width = 8,
                             plotOutput("arc_disciplines")),
                 argonColumn(div(style = "height: 55px;"),
                             h3("Disciplines of Bachelor's degrees"),
                             textOutput("disciplines_text"))),
        argonRow(argonColumn(width = 8,
                             plotlyOutput("bar_computer")),
                 argonColumn(div(style = "height: 85px;"),
                             h3("Presence of a computer/internet in the home"),
                             "Internet in the home and presence of a computer
                             has become increasingly central to completing
                             assignments (for K-12 family members), as well as
                             excelling at work. Hover over the plot to explore
                             the data.")),
        argonRow(argonColumn(width = 8,
                             plotOutput("arc_enrolled")),
                 argonColumn(div(style = "height: 100px;"),
                             h3("School enrollment status"),
                             textOutput("enrollment_text")))
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
                 argonColumn(div(style = "height: 70px;"),
                             h3("Health insurance coverage"),
                             textOutput("insurance_text"))),
        
        div(style = "height: 75px;") # Spacer div
        
        
        
        
      ),
      
      div(style = "text-align: center;
                    background-color: #191d36;
                    border-radius: 15px;
                    padding: 20px;",
          shinydashboardPlus::accordion(id = "accordion1",
                                        accordionItem(title = "+ Download Data",
                                                      collapsed = T,
                                                      # Input: Choose dataset ----
                                                      argonRow(width = 12,
                                                               column(width = 8,
                                                                      pickerInput("dataset", "Choose data:",
                                                                                  selected = "Health",
                                                                                  choices = c("Demographics", 
                                                                                              "Economics and Workforce", 
                                                                                              "Education", 
                                                                                              "Health"),
                                                                                  width = "100%",
                                                                                  multiple = T)),
                                                               # Button
                                                               column(width = 4,
                                                                      div(style = "height: 30px;"), # Spacer div
                                                                      downloadButton("download_data", "Download data")))))
          ) # End download data div
  ) # End of div wrap of everything under map
  
)