tab_4 <- argonTabItem(
  tabName = "tab_4",
  
  
  # Header
  div(class = "tab-4-header-div",
      
      # Spacer div
      div(style = "height: 150px;"),
      
      # Card row
      div(style = "padding: 0px 50px 0px 50px;",
          
          h1(strong("NUESTRO FUTURE")),
          "Eliminating disparities would bring economic and economic benefits to the Latinx population as well as the rest of the Central Iowa population.
          Toggle the inputs below to test how new programs or policies aimed at education or citizenship within Latinx communities would affect household incomes,
          population earnings, homeownership, and educational attainment." %>% argonTextColor(hex_blue_dk),
          br(),
          br(),
          argonRow(width = 12,
                   
                   argonCard(width = 3,
                             numericInput(inputId = "hs_grads",
                                          label = "Additional high school graduates",
                                          value = 50)),
                   argonCard(width = 3,
                             numericInput(inputId = "hs_to_assoc",
                                          label = "Current high schoolers to associate's degree attainment",
                                          value = 50)),
                   argonCard(width = 3,
                             numericInput(inputId = "assoc_to_bach",
                                          label = "Current associate's holders to Bachelor's degree attainment",
                                          value = 50)),
                   argonCard(width = 3,
                             numericInput(inputId = "hs_to_bach",
                                          label = "Current high schoolers to Bachelor's degree attainment",
                                          value = 1000))
                   
          )),
      
      div(style = "padding-top: 50px; margin: auto; width: 50%; color: white;",
          prettySwitch(
            inputId = "undoc_to_citizens",
            label = "Full citizenship status for undocumented immigrants", 
            status = "primary",
            slim = TRUE
          )),
      
      # Spacers
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      
  ), # End header
  
  div(style = "margin-top: 700px !important; text-align: center;",
      
      # Row 1
      div(class = "tab-4-row-1",
          
          argonRow(
            argonColumn(width = 8,
                        withLoader(plotlyOutput("plot_addtl_dollars", height = 550), loader = "loader1"),
                        h2(strong("EDUCATIONAL ATTAINMENT")),
                        withLoader(plotlyOutput("plot_attainment"), loader = "loader1")),
            argonColumn(width = 4, 
                        div(style = "border-radius: 25px; background: white; padding: 40px 30px 30px 30px !important;",
                            h2(strong("PER CAPITA INCOME")),
                            withLoader(plotlyOutput("plot_per_cap_inc", height = 300), loader = "loader1")))
            
          ) # End row 1
      ) # End tab 4 div
  ) # End div
  
)

# tab_4 <- argonTabItem(
#   tabName = "tab_4",
#   
#   argonCard(title = "Per capita income",
#             hover_lift = FALSE,
#             shadow = TRUE,
#             shadow_size = NULL,
#             hover_shadow = FALSE,
#             border_level = 0,
#             width = 12,
#             
#             # First row
#             argonRow(
#               
#               # Left column
#               argonColumn(
#                 width = 4,
#                 div(style = "height: 15px;"), # spacer div
#                 div(style = "padding: 30px;",
#                   knobInput(
#                   inputId = "income_knob",
#                   label = "",
#                   value = 15,
#                   lineCap = "round",
#                   post = "%",
#                   inputColor = hex_purple,
#                   fgColor = hex_purple,
#                   # bgColor = hex_purple,
#                   cursor = F),
#                 div(style = "text-align: center;",
#                     strong("of income disparities eliminated")))
#               ),
#               
#               # Right column
#               argonColumn(width = 8,
#                           plotlyOutput("income_disparities", height = 500)
#               )
#             ) # End row
#   ), # End card
#   
#   # Second card
#   argonCard(title = "Total Latinx earnings",
#             hover_lift = FALSE,
#             shadow = TRUE,
#             shadow_size = NULL,
#             hover_shadow = FALSE,
#             border_level = 0,
#             width = 12,
#             
#             # First row
#             argonRow(
#               
#               # Left column
#               argonColumn(
#                 width = 4,
#                 div(style = "height: 30px;"), # spacer div
#                 div(style = "padding: 15px;",
#                     knobInput(
#                       inputId = "earnings_knob",
#                       label = "",
#                       value = 15,
#                       lineCap = "round",
#                       post = "%",
#                       inputColor = hex_purple,
#                       fgColor = hex_purple,
#                       # bgColor = hex_purple,
#                       cursor = F),
#                     div(style = "text-align: center;",
#                         strong("of total earnings disparities eliminated")))
#               ),
#               
#               # Right column
#               argonColumn(width = 8,
#                           plotlyOutput("earnings_disparities", height = 500)
#               )
#             ) # End row
#   ), # End card
#   
#   argonCard(title = "Latinx in poverty",
#             hover_lift = FALSE,
#             shadow = TRUE,
#             shadow_size = NULL,
#             hover_shadow = FALSE,
#             border_level = 0,
#             width = 12,
#             
#             # First row
#             argonRow(
#               
#               # Left column
#               argonColumn(
#                 width = 4,
#                 div(style = "height: 30px;"), # spacer div
#                 div(style = "padding: 15px;",
#                     knobInput(
#                       inputId = "poverty_knob",
#                       label = "",
#                       value = 15,
#                       lineCap = "round",
#                       post = "%",
#                       inputColor = hex_purple,
#                       fgColor = hex_purple,
#                       # bgColor = hex_purple,
#                       cursor = F),
#                     div(style = "text-align: center;",
#                         strong("of poverty disparities eliminated")))
#               ),
#               
#               # Right column
#               argonColumn(width = 8,
#                           plotlyOutput("poverty_disparities", height = 500)
#               )
#             ) # End row
#   ), # End card
#   
#   argonCard(title = "Latinx homeowners",
#             hover_lift = FALSE,
#             shadow = TRUE,
#             shadow_size = NULL,
#             hover_shadow = FALSE,
#             border_level = 0,
#             width = 12,
#             
#             # First row
#             argonRow(
#               
#               # Left column
#               argonColumn(
#                 width = 4,
#                 div(style = "height: 30px;"), # spacer div
#                 div(style = "padding: 15px;",
#                     knobInput(
#                       inputId = "homeowners_knob",
#                       label = "",
#                       value = 15,
#                       lineCap = "round",
#                       post = "%",
#                       inputColor = hex_purple,
#                       fgColor = hex_purple,
#                       # bgColor = hex_purple,
#                       cursor = F),
#                     div(style = "text-align: center;",
#                         strong("of homeownership disparities eliminated")))
#               ),
#               
#               # Right column
#               argonColumn(width = 8,
#                           plotlyOutput("homeowners_disparities", height = 500)
#               )
#             ) # End row
#   ), # End card
#   
# ) # End tab