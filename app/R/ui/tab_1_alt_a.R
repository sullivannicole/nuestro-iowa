tab_1_alt_a <- argonTabItem(
  tabName = "tab_1_alt_a",
  br(),
  br(),
  div(class = "tab-1-header-container",
      argonDashHeader(color = "danger",
                      gradient = TRUE,
                      separator = TRUE,
                      separator_color = 'secondary',
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
                  and their contributions to Central Iowa make them a force to be reckoned with. Scroll on to learn more.") %>% argonTextColor(color = "white")),
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
        argonCard(title = "Significant",
                  icon = icon("users"),
                  status = "warning",
                  width = 4,
                  shadow = TRUE,
                  border_level = 2,
                  hover_shadow = TRUE,
                  hover_lift = TRUE,
                  center = TRUE,
                  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                  Phasellus metus ipsum, gravida auctor arcu id, volutpat volutpat orci. 
                  Pellentesque dapibus dapibus risus. Vivamus elementum eros eget mauris vulputate tempus."
        ),
        argonCard(title = "Entrepeneurial",
                  icon = icon("hands-helping"),
                  width = 4,
                  center = TRUE,
                  shadow = TRUE,
                  border_level = 2,
                  hover_shadow = TRUE,
                  hover_lift = TRUE,
                  "In hac habitasse platea dictumst. Pellentesque vel risus sem. 
                  Etiam vulputate, erat nec ornare laoreet, ante enim hendrerit orci, eget congue mauris turpis in nibh. 
                  Nullam metus felis, hendrerit eget orci eu, suscipit vehicula ipsum. "
        ),
        
        argonCard(title = "Growing",
                  icon = icon("chart-line"),
                  status = "success",
                  width = 4,
                  center = TRUE,
                  shadow = TRUE,
                  border_level = 2,
                  hover_shadow = TRUE,
                  hover_lift = TRUE,
                  "Curabitur fermentum lacus in felis porttitor, vel blandit sapien vestibulum. Morbi felis sapien, rutrum sit amet tortor vitae, consequat lobortis nibh. 
                  Nulla in enim turpis. 
                  Curabitur suscipit nunc lorem, at tempus nulla suscipit semper."
        )
        
      )), # End row 1
  
  # Spacer div,
  div(style = "height: 75px;"),
  
  # Row 2: About (short)
  
  div(class = "tab-1-about-container",
      argonH1("About", display = 3) %>% argonTextColor("white"),
      div(style = "font-size: 35px; color: #172B4D;", "Nuestro Iowa was created with the mission of...
                                    Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                                    Phasellus metus ipsum, gravida auctor arcu id, volutpat volutpat orci. ")),
  
  # Row 3: Our Team
  div(style = "margin-top: 600px;",
      argonH1("Nuestro Team", display = 4),
      argonRow(
        argonColumn(
          width = 3,
          argonUser(
            title = "Dawn Oropeza",
            subtitle = "Executive Director, Al Éxito Iowa",
            src = "dawn_oropeza_square.png"
          )
        ),
        argonColumn(
          width = 3,
          argonUser(
            title = "Alejandro Hernandez",
            subtitle = "Dean of the College of Business, Drake University",
            src = "alejandro_hernandez.jpeg"
          )
        ),
        argonColumn(
          width = 3,
          argonUser(
            title = "Darcie Vandegrift",
            subtitle = "Co-founder, VandegriftHuting Consulting",
            src = "darcie_vandegrift.jpeg"
          )
        ),
        argonColumn(
          width = 3,
          argonUser(
            title = "Joel Huting",
            subtitle = "Co-founder, VandegriftHuting Consulting",
            src = "joel_huting.jpeg"
          )
        )
      )),
  
  # Spacer div,
  div(style = "height: 125px;"),
  
  # Row 4: Our Sponsors
  div(class = "tab-1-sponsors-container",
    argonH1("Nuestro Sponsors", display = 4) %>% argonTextColor("white"),
    argonRow(argonColumn(width = 4,
                         class = "tab-1-sponsors-column",
                         a(img(src = "placeholder-logo-1.png"), href="https://www.google.com"),
                         "Sponsor 1"),
             argonColumn(width = 4,
                         class = "tab-1-sponsors-column",
                         a(img(src = "placeholder-logo-1.png"), href="https://www.google.com"),
                         "Sponsor 2"),
             argonColumn(width = 4,
                         class = "tab-1-sponsors-column",
                         a(img(src = "placeholder-logo-1.png"), href="https://www.google.com"),
                         "Sponsor 3"
                         ))),
  
  div(style = "height: 450px !important;")
  
  
)