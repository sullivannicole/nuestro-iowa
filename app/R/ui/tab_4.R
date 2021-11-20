tab_4 <- argonTabItem(
  tabName = "tab_4",
  
  argonRow(
    
    argonColumn(width = 3,
                div(style = "height: 100px;"), # spacer div
                HTML("<h1 style = 'font-size: 60px !important; line-height: 50px !important;'>Nuestro choice</h1>"),
                br(),
                "Toggle the numbers below to reflect your program's expected educational and/or citizenship outcomes and see projections for the future (to the right) change.",
                br(),
                br(),
                br(),
                argonH1("Education", display = 4),
                numericInput(inputId = "hs_grads",
                             label = "Additional Latinx high school graduates",
                             min = 0,
                             value = 15),
                numericInput(inputId = "hs_to_assoc",
                             label = "Additional Latinx with associate's degrees (current education: high school)",
                             min = 0,
                             value = 0),
                numericInput(inputId = "assoc_to_bach",
                             label = "Additional Latinx with Bachelor's degrees (current education: associate's)",
                             min = 0,
                             value = 0),
                numericInput(inputId = "hs_to_bach",
                             label = "Additional Latinx with Bachelor's degrees (current education: high school)",
                             min = 0,
                             value = 0),
                
                argonH1("Citizenship", display = 4),
                shinyWidgets::prettySwitch(
                  inputId = "undoc_to_citizens",
                  label = "Full citizenship status for undocumented immigrants",
                  status = "primary",
                  slim = TRUE
                )
    ),
    argonColumn(width = 8,
                div(style = "height: 50px;"), # spacer div
                div(style = "text-align: center; 
                             background-color: white;
                             padding: 50px 50px;",
                    HTML("<h1 style = 'font-size: 60px !important;'>Nuestro future</h1>"),
                    "Based on your choices (at left), in the future, we'd see:",
                    argonRow(argonColumn(width = 6,
                                         withLoader(plotlyOutput("plot_addtl_dollars", height = 200), loader = "loader1"),
                                         div(style = "color: #172B4D; font-size: 50px; font-weight: bold;", 
                                             textOutput("total_addtl_dollars_number")),
                                         "TOTAL ADDITIONAL LATINX EARNINGS OVER THE NEXT 30 YEARS",
                                         
                                         withLoader(plotlyOutput("plot_homeownership", height = 200), loader = "loader1"),
                                         div(style = "color: #172B4D; font-size: 50px; font-weight: bold;", 
                                             textOutput("homeownership_number")),
                                         "LATINX HOMEOWNERSHIP"),
                             
                             argonColumn(width = 6, 
                                         withLoader(plotlyOutput("plot_per_cap_inc", height = 200), loader = "loader1"),
                                         #argonH1("20000", display = 2),
                                         div(style = "color: #172B4D; font-size: 50px; font-weight: bold;", 
                                             textOutput("per_capita_income_number")),
                                         "IN AVERAGE ANNUAL INCOME PER LATINX-IOWAN",
                                         withLoader(plotlyOutput("plot_attainment", height = 200), loader = "loader1"),
                                         div(style = "color: #172B4D; font-size: 50px; font-weight: bold;", 
                                             textOutput("attainment_number")),
                                         "IN LATINX WITH HS ATTAINMENT OR MORE")
                    ),
                    div(style = "height: 100px;") # spacer div at bottom
                ))
  )
)



# Header
# div(class = "tab-4-header-div",
#     
#     # Spacer div
#     div(style = "height: 150px;"),
#     
#     # Card row
#     div(style = "padding: 0px 50px 0px 50px;",
#         
#         h1(strong("NUESTRO FUTURE")),
#         div(style = "color: #172B4d;",
#             "Eliminating disparities would bring economic benefits to the Latinx population as well as the rest of the Central Iowa population.
#         Toggle the inputs below to test how new programs or policies aimed at education or citizenship within Latinx communities would affect household incomes,
#         population earnings, homeownership, and educational attainment."),
#         br(),
#         br(),
#         argonRow(width = 12,
#                  
#                  argonCard(width = 3,
#                            numericInput(inputId = "hs_grads",
#                                         label = "Additional high school graduates",
#                                         value = 50)),
#                  argonCard(width = 3,
#                            numericInput(inputId = "hs_to_assoc",
#                                         label = "Current high schoolers to associate's degree attainment",
#                                         value = 50)),
#                  argonCard(width = 3,
#                            numericInput(inputId = "assoc_to_bach",
#                                         label = "Current associate's holders to Bachelor's degree attainment",
#                                         value = 50)),
#                  argonCard(width = 3,
#                            numericInput(inputId = "hs_to_bach",
#                                         label = "Current high schoolers to Bachelor's degree attainment",
#                                         value = 1000))
#                  
#         )),
#     
#     div(style = "padding-top: 50px; margin: auto; width: 50%; color: white;",
#         shinyWidgets::prettySwitch(
#           inputId = "undoc_to_citizens",
#           label = "Full citizenship status for undocumented immigrants",
#           status = "primary",
#           slim = TRUE
#         )),
#     
#     # Spacers
#     br(),
#     br(),
#     br(),
#     br(),
#     br(),
#     br(),
#     
# ), # End header
# 
# div(style = "margin-top: 700px !important; text-align: center;",
#     
#     # Row 1
#     div(class = "tab-4-row-1",
#         
#         argonRow(
#           argonColumn(width = 8,
#                       withLoader(plotlyOutput("plot_addtl_dollars", height = 550), loader = "loader1")),
#           
#           argonColumn(width = 4,
#                       div(style = "border-radius: 25px; background: white; padding: 40px 30px 30px 30px !important;",
#                           h2(strong("PER CAPITA INCOME")),
#                           withLoader(plotlyOutput("plot_per_cap_inc", height = 200), loader = "loader1"),
#                           
#                           h2(strong("HOMEOWNERSHIP RATES")),
#                           withLoader(plotlyOutput("plot_homeownership", height = 200), loader = "loader1")
#                           
#                       ))
#           
#         ) # End row 1 argonRow
#         
#     ), # End row 1 div
#     
#     div(class = "tab-4-row-2",
#         
#         argonCard(width = 12,
#                   h2(strong("EDUCATIONAL ATTAINMENT")),
#                   withLoader(plotlyOutput("plot_attainment"), loader = "loader1"))
#     )
#     
#     # End row 2 div
#     
# ) # End div
# 
#)