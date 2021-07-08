tab_4 <- argonTabItem(
  tabName = "tab_4",
  
  argonCard(title = "Per capita income",
            hover_lift = FALSE,
            shadow = TRUE,
            shadow_size = NULL,
            hover_shadow = FALSE,
            border_level = 0,
            width = 12,
            
            # First row
            argonRow(
              
              # Left column
              argonColumn(
                width = 4,
                div(style = "height: 15px;"), # spacer div
                div(style = "padding: 30px;",
                  knobInput(
                  inputId = "income_knob",
                  label = "",
                  value = 15,
                  lineCap = "round",
                  post = "%",
                  inputColor = hex_purple,
                  fgColor = hex_purple,
                  # bgColor = hex_purple,
                  cursor = F),
                div(style = "text-align: center;",
                    strong("of income disparities eliminated")))
              ),
              
              # Right column
              argonColumn(width = 8,
                          plotlyOutput("income_disparities", height = 500)
              )
            ) # End row
  ), # End card
  
  # Second card
  argonCard(title = "Total Latinx earnings",
            hover_lift = FALSE,
            shadow = TRUE,
            shadow_size = NULL,
            hover_shadow = FALSE,
            border_level = 0,
            width = 12,
            
            # First row
            argonRow(
              
              # Left column
              argonColumn(
                width = 4,
                div(style = "height: 30px;"), # spacer div
                div(style = "padding: 15px;",
                    knobInput(
                      inputId = "earnings_knob",
                      label = "",
                      value = 15,
                      lineCap = "round",
                      post = "%",
                      inputColor = hex_purple,
                      fgColor = hex_purple,
                      # bgColor = hex_purple,
                      cursor = F),
                    div(style = "text-align: center;",
                        strong("of total earnings disparities eliminated")))
              ),
              
              # Right column
              argonColumn(width = 8,
                          plotOutput("earnings_disparities")
              )
            ) # End row
  ), # End card
  
  argonCard(title = "Latinx in poverty",
            hover_lift = FALSE,
            shadow = TRUE,
            shadow_size = NULL,
            hover_shadow = FALSE,
            border_level = 0,
            width = 12,
            
            # First row
            argonRow(
              
              # Left column
              argonColumn(
                width = 4,
                div(style = "height: 30px;"), # spacer div
                div(style = "padding: 15px;",
                    knobInput(
                      inputId = "poverty_knob",
                      label = "",
                      value = 15,
                      lineCap = "round",
                      post = "%",
                      inputColor = hex_purple,
                      fgColor = hex_purple,
                      # bgColor = hex_purple,
                      cursor = F),
                    div(style = "text-align: center;",
                        strong("of poverty disparities eliminated")))
              ),
              
              # Right column
              argonColumn(width = 8,
                          plotOutput("poverty_disparities")
              )
            ) # End row
  ), # End card
  
  argonCard(title = "Latinx homeowners",
            hover_lift = FALSE,
            shadow = TRUE,
            shadow_size = NULL,
            hover_shadow = FALSE,
            border_level = 0,
            width = 12,
            
            # First row
            argonRow(
              
              # Left column
              argonColumn(
                width = 4,
                div(style = "height: 30px;"), # spacer div
                div(style = "padding: 15px;",
                    knobInput(
                      inputId = "homeowners_knob",
                      label = "",
                      value = 15,
                      lineCap = "round",
                      post = "%",
                      inputColor = hex_purple,
                      fgColor = hex_purple,
                      # bgColor = hex_purple,
                      cursor = F),
                    div(style = "text-align: center;",
                        strong("of homeownership disparities eliminated")))
              ),
              
              # Right column
              argonColumn(width = 8,
                          plotOutput("homeowners_disparities")
              )
            ) # End row
  ), # End card
  
) # End tab