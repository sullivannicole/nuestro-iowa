card1_english <- argonColumn(h4("Latinx Community Highlights"),
                             br(),
                             div(HTML("<li><b>1,544 Latinx businesses</b> generating <b>$159.6M</b> in annual revenue</li>")),
                             br(),
                             div(HTML("<li><b>52K Latinx residents</b> in the metro, which will more than double by 2050 and drive overall metro pop. growth</li>")),
                             br(),
                             div(HTML("<li><b>22K Latinx workers</b> collectively earning <b>$1B in annual income</b>, contributing significantly to the overall central IA economy</li>")),
                             br(),
                             HTML("<li>Latinx community turned around over a decade of pop. decline in Marshall County</li>"))

card1_spanish <- rgonColumn(h4("Aspectos destacados de la comunidad latina"),
                            br(),
                            div(HTML("<li><b>Hay 1,544 negocios latinos que generan <b>$159.6 millones</b> de dólares en ingresos anuales</li>")),
                            br(),
                            div(HTML("<li><b>Hay 52 mil residentes latinos en el área metropolitana, una cifra que se duplicará para el año 2050 e impulsará el crecimiento de la población general de la zona metropolitana.</li>")),
                            br(),
                            div(HTML("<li><b>Hay 22 mil trabajadores latinos que, colectivamente, generan mil millones de dólares en ingresos anuales, lo que representa una contribución importante a la economía de la región central de Iowa.</li>")),
                            br(),
                            HTML("<li>La comunidad latina transformó más de una década de pérdida de población en el condado de Marshall.</li>"))

card2_english <- argonColumn(h4("Disparities (Compared with Whites)"),
                             div(HTML("<li>Average household income is <b>33% lower</b></li>")),
                             div(HTML("<li>Poverty rates are <b>13 percentage points higher</b></li>")),
                             div(HTML("<li>Homeownership rates are <b>24 percentage points lower</b></li>")),
                             div(HTML("<li><b>21 percentage point gap</b> in health insurance coverage for those aged 19-64</li>")),
                             div(HTML("<li><b>25 percentage point gap</b> in Bachelor's degree or higher completion</li>")),
                             div(HTML("<li><b>33 percentage point gap</b> in high school completion</li>")))

card2_spanish <- argonColumn(h4("Disparidades (en comparación con los blancos)"),
                             div(HTML("<li>Los ingresos familiares promedio son <b>33% más bajos</b></li>")),
                             div(HTML("<li>La tasa de pobreza es <b>13 puntos porcentuales más alta</b></li>")),
                             div(HTML("<li>La tasa de propiedad de vivienda es <b>24 puntos porcentuales más baja</b></li>")),
                             div(HTML("<li>Existe una brecha de <b>21 puntos porcentuales</b> en la cobertura médica para las personas de entre 19 y 64 años.</li>")),
                             div(HTML("<li>Existe una brecha de <b>25 puntos porcentuales </b>en la culminación de estudios universitarios a nivel de licenciatura o postgrado</li>")),
                             div(HTML("<li><b>Existe una brecha de <b>33 puntos porcentuales</b> en la culminación de estudios de preparatoria</li>")))

card3_english <- argonColumn(h4("Root causes of disparities"),
                             div(HTML("<li><b>Eliminating education disparities</b> will largely close the income and poverty gap</li>")),
                             # br(),
                             div(HTML("<li><b>Eliminating income and education disparities</b> will mostly close the homeownership gap</li>")),
                             # br(),
                             div(HTML("<li>Latinx households face <b>additional barriers</b> to homeownership</li>")))

card3_spanish <- argonColumn(h4("Causas fundamentales de las disparidades"),
                             div(HTML("<li><b>La eliminación de las disparidades educativas</b> reducirá, en gran medida, las en ingresos y pobreza.</li>")),
                             # br(),
                             div(HTML("<li><b>La eliminación de las disparidades en ingresos y educación</b> reducirá, en gran medida, la brecha en la tasa de propiedad de vivienda.</li>")),
                             # br(),
                             div(HTML("<li>Las familias latinas enfrentan <b>obstáculos adicionales</b> para comprar sus viviendas propias.</li>")))

tab_1_alt_a <- argonTabItem(
  tabName = "tab_1_alt_a",
  br(),
  br(),
  div(class = "tab-1-header-container",
      argonDashHeader(#color = "danger",
                      #gradient = TRUE,
                      #separator = TRUE,
                      #separator_color = 'secondary',
                      br(),
                      # argonRow(argonColumn(width = 5),
                      #          argonColumn(argonButton(name = "Al Exito", src = "www.alexitoiowa.org", status = "primary") %>% argonTextColor(color = "white"))),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      div(style = "margin-right: 40px;",
                          argonRow(width = 12,
                                   argonColumn(width = 1),
                                   argonColumn(argonH1(display = 3, "A deep dive into la riqueza de Latinx-Iowan culture") %>% argonTextColor(color = "white"),
                                               br(),
                                               div("Latinx Iowans represent a youthful, highly entrepeneurial, collective-minded subset of the Iowa population - 
                  an invaluable part of the school system, workforce, and community. Latinx Iowans are also a fast-growing subpopulation, 
                  and our contributions to Central Iowa make us a force to be reckoned with. Scroll on to learn more.") %>% argonTextColor(color = "white")),
                                   # actionButton(inputId = 'jumpToP2', label = 'Jump to Second Tab',
                                   #              onclick ="window.open('http://google.com', '_blank')")),
                                   argonColumn(width = 4, argonImage(src = "latam.svg", floating = TRUE) %>% argonBlur() %>% argonPersp(side = "right") ))),
                      br(),
                      br(),
                      br(),
                      br(),
      )),
  
  # Row 1: info cards
  div(style = "margin-top: 550px !important;",
      argonRow(
        
        # Horizontal Tabset 1
        argonColumn(
          width = 4,
          
          argonTabSet(
            id = "tab-1",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 12,
            iconList = lapply(X = 1:2, FUN = icon, name = "hands-helping"),
            # iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
            argonTab(
              tabName = "Highlights",
              active = FALSE,
              card1_english
            ),
            argonTab(
              tabName = "Positivos",
              active = TRUE,
              card1_spanish
            )
          )
        ),
        
        # Horizontal Tabset 2
        argonColumn(
          width = 4,
          
          argonTabSet(
            id = "tab-2",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 12,
            iconList = lapply(X = 1:2, FUN = icon, name = "users"),
            argonTab(
              tabName = "Disparities",
              active = TRUE,
              card2_english
            ),
            argonTab(
              tabName = "Disparidades",
              active = FALSE,
              card2_spanish
            )
          )
        ),
        
        argonColumn(
          width = 4,
          
          argonTabSet(
            id = "tab-2",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 12,
            iconList = lapply(X = 1:2, FUN = icon, name = "key"),
            argonTab(
              tabName = "Causes of disparities",
              active = FALSE,
              card3_english
            ),
            argonTab(
              tabName = "Causas de disparidades",
              active = TRUE,
              card3_spanish
            )
          )
        )
      )), # End info cards row
  
  # Spacer div,
  div(style = "height: 75px;"),
  
  # Row 2: About (short)
  
  div(class = "tab-1-about-container",
      argonH1("Nuestra Visión", display = 3) %>% argonTextColor("white"),
      div(style = "font-size: 35px; color: #172B4D;", HTML("A welcoming state where everyone thrives.<br>
                                                            Un estado acogedor donde todes prosperen."))),
  # Spacer div,
  div(style = "margin-top: 500px;",
      argonRow(argonColumn(
        argonH1("About", display = 3),
        br(),
        "This dashboard..."))),
  
  div(style = "height: 450px !important;")
  
  
)
# argonCard(title = "Latinx Community Highlights",
#           icon = icon("hands-helping"),#users
#           status = "warning",
#           width = 4,
#           shadow = TRUE,
#           border_level = 2,
#           hover_shadow = TRUE,
#           hover_lift = TRUE,
#           center = TRUE,
#           div(HTML("<li><b>1,544 Latinx businesses</b> generating <b>$159.6M</b> in annual revenue</li>")),
#           # br(),
#           div(HTML("<li><b>52K Latinx residents</b> in the metro, which will more than double by 2050 and drive overall metro pop. growth</li>")),
#           # br(),
#           div(HTML("<li><b>22K Latinx workers</b> collectively earning <b>$1B in annual income</b>, contributing significantly to the overall central IA economy</li>")),
#           # br(),
#           HTML("<li>Latinx community turned around over a decade of pop. decline in Marshall County</li>")
# ),
# argonCard(title = "Disparities (compared with Whites)",
#           icon = icon("users"),
#           width = 4,
#           center = TRUE,
#           shadow = TRUE,
#           border_level = 2,
#           hover_shadow = TRUE,
#           hover_lift = TRUE,
#           div(HTML("<li>Average household income is <b>33% lower</b></li>")),
#           # br(),
#           div(HTML("<li>Poverty rates are <b>13 percentage points higher</b></li>")),
#           # br(),
#           div(HTML("<li>Homeownership rates are <b>24 percentage points lower</b></li>")),
#           # br(),
#           div(HTML("<li><b>21 percentage point gap</b> in health insurance coverage for those aged 19-64</li>")),
#           # br(),
#           div(HTML("<li><b>25 percentage point gap</b> in Bachelor's degree or higher completion</li>")),
#           # br(),
#           div(HTML("<li><b>33 percentage point gap</b> in high school completion</li>"))
# ),
#   
#   argonCard(title = "Root causes of disparities",
#             icon = icon("key"),
#             status = "success",
#             width = 4,
#             center = TRUE,
#             shadow = TRUE,
#             border_level = 2,
#             hover_shadow = TRUE,
#             hover_lift = TRUE,
#             div(HTML("<li><b>Eliminating education disparities</b> will largely close the income and poverty gap</li>")),
#             # br(),
#             div(HTML("<li><b>Eliminating income and education disparities</b> will mostly close the homeownership gap</li>")),
#             # br(),
#             div(HTML("<li>Latinx households face <b>additional barriers</b> to homeownership</li>"))
#   )
#   
# )), # End row 1

# Row 3: Our Team
# div(style = "margin-top: 500px;",
#     argonH1("Nuestro Team", display = 4),
#     h3("Latinx Leaders"),
#     argonRow(
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Maria Alcivar",
#           subtitle = "Researcher in Women's Health/NFP/Immigration",
#           src = "profile.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Alejandro Alfaro-Santiz",
#           subtitle = "Reverend, Trinity Las Americas",
#           src = "alejandro_alfaro-santiz.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Fernando Aveiga",
#           subtitle = "Business, Housing, Arts",
#           src = "profile.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Rob X. Barron",
#           subtitle = "Executive Director, Iowa & Minnesota Campus Compact",
#           src = "rob_barron.png"
#         )
#       )
#     ),
#     # Latinx Leaders Row 2
#     argonRow(
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Nathan Blake",
#           subtitle = "Chief Deputy Iowa Attorney General, Iowa Dept. of Justice",
#           src = "nathan_blake.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Kenia Calderón Cerón",
#           subtitle = "Vice President, GreenState Credit Union",
#           src = "kenia_calderon_ceron.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Miguel Contreras",
#           subtitle = "Art Director, Flock DSM",
#           src = "miguel_contreras.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Ana Coppola",
#           subtitle = "Health Subject Matter Expert",
#           src = "profile.png"
#         )
#       )
#     ),
#     
#     # Latinx Leaders Row 3
#     argonRow(
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Suzanna de Baca",
#           subtitle = "President and Group Publisher, Business Publications Corporation",
#           src = "suzanna_de_baca.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Frank Young Dunn",
#           subtitle = "City Planning Subject Matter Expert",
#           src = "profile.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Alejandro Hernandez",
#           subtitle = "Dean, College of Business and Public Administration, Drake University",
#           src = "alejandro_hernandez.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Junior Ibarra",
#           subtitle = "Entrepeneur/Housing Subject Matter Expert",
#           src = "profile.png"
#         )
#       )
#     ),
#     
#     # Row 4 Latinx Leaders
#     argonRow(
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Alex Jimenez",
#           subtitle = "Event & Client Specialist, Wildflower Boutique",
#           src = "alex_jimenez.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Erica Johnson",
#           subtitle = "Founding Executive Director, Iowa Migrant Movement for Justice",
#           src = "erica_johnson.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Dawn Martinez Oropeza",
#           subtitle = "Executive Director, Al Éxito",
#           src = "dawn_martinez_oropeza.png"
#         )
#       ),
#       argonColumn(
#         width = 3,
#         argonUser(
#           title = "Noemi Mendez",
#           subtitle = "Exectuive Director, Latina Leadership Initiative of Greater Des Moines",
#           src = "noemi_mendez.png"
#         )
#       )
#     )),
# 
# # Consultant Row
# h3("Consultants"),
# argonRow(
#   argonColumn(
#     width = 3,
#     argonUser(
#       title = "Ruby Herrera",
#       subtitle = "Educator, DMP",
#       src = "ruby_herrera.png"
#     )
#   ),
#   argonColumn(
#     width = 3,
#     argonUser(
#       title = "Joel Huting",
#       subtitle = "Co-Founder & Quantitative Lead, Vandegrift Huting Consultants",
#       src = "joel_huting.jpeg"
#     )
#   ),
#   argonColumn(
#     width = 3,
#     argonUser(
#       title = "Darcie Vandegrift",
#       subtitle = "Co-Founder & Qualitative Lead, Vandegrift Huting Consultants",
#       src = "darcie_vandegrift.png"
#     )
#   ),
#   argonColumn(
#     width = 3,
#     argonUser(
#       title = "Nicole Sullivan",
#       subtitle = "Data Scientist, Vandegrift Huting Consultants",
#       src = "nicole_sullivan.png"
#     )
#   )
# ),
# 
# Sponsor partners Row
# h3("Sponsor Partners"),
# argonRow(
#   argonColumn(
#     width = 6,
#     argonUser(
#       title = "Angela Dethlefs-Trettin",
#       subtitle = "Partner",
#       src = "profile.png"
#     )
#   ),
#   argonColumn(
#     width = 6,
#     argonUser(
#       title = "Suzanne Mineck",
#       subtitle = "President and CEO, Mid-Iowa Health Foundation",
#       src = "suzanne_mineck.png"
#     )
#   )
# ),
# 

# Spacer div,
# div(style = "height: 125px;"),
# 
# # Row 4: Our Sponsors
# div(class = "tab-1-sponsors-container",
#     argonH1("Nuestro Sponsors", display = 4) %>% argonTextColor("white"),
#     argonRow(argonColumn(width = 4,
#                          class = "tab-1-sponsors-column",
#                          a(img(src = "placeholder-logo-1.png"), href="https://www.google.com"),
#                          "Sponsor 1"),
#              argonColumn(width = 4,
#                          class = "tab-1-sponsors-column",
#                          a(img(src = "placeholder-logo-1.png"), href="https://www.google.com"),
#                          "Sponsor 2"),
#              argonColumn(width = 4,
#                          class = "tab-1-sponsors-column",
#                          a(img(src = "placeholder-logo-1.png"), href="https://www.google.com"),
#                          "Sponsor 3"
#              ))),
# 
