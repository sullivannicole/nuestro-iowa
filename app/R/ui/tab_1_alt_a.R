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
      h3("Latinx Leaders"),
      argonRow(
        argonColumn(
          width = 3,
          argonUser(
            title = "Maria Alcivar",
            subtitle = "Researcher in Women's Health/NFP/Immigration",
            src = "profile.png"
          )
        ),
        argonColumn(
          width = 3,
          argonUser(
            title = "Alejandro Alfaro-Santiz",
            subtitle = "Reverend, Trinity Las Americas",
            src = "alejandro_alfaro-santiz.png"
          )
        ),
        argonColumn(
          width = 3,
          argonUser(
            title = "Fernando Aveiga",
            subtitle = "Business, Housing, Arts",
            src = "profile.png"
          )
        ),
        argonColumn(
          width = 3,
          argonUser(
            title = "Rob X. Barron",
            subtitle = "Executive Director, Iowa & Minnesota Campus Compact",
            src = "rob_barron.png"
          )
        )
      ),
  # Latinx Leaders Row 2
  argonRow(
    argonColumn(
      width = 3,
      argonUser(
        title = "Nathan Blake",
        subtitle = "Chief Deputy Iowa Attorney General, Iowa Dept. of Justice",
        src = "nathan_blake.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Kenia Calderón Cerón",
        subtitle = "Vice President, GreenState Credit Union",
        src = "kenia_calderon_ceron.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Miguel Contreras",
        subtitle = "Art Director, Flock DSM",
        src = "miguel_contreras.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Ana Coppola",
        subtitle = "Health Subject Matter Expert",
        src = "profile.png"
      )
    )
  ),
  
  # Latinx Leaders Row 3
  argonRow(
    argonColumn(
      width = 3,
      argonUser(
        title = "Suzanna de Baca",
        subtitle = "President and Group Publisher, Business Publications Corporation",
        src = "suzanna_de_baca.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Frank Young Dunn",
        subtitle = "City Planning Subject Matter Expert",
        src = "profile.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Alejandro Hernandez",
        subtitle = "Dean, College of Business and Public Administration, Drake University",
        src = "alejandro_hernandez.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Junior Ibarra",
        subtitle = "Entrepeneur/Housing Subject Matter Expert",
        src = "profile.png"
      )
    )
  ),
  
  # Row 4 Latinx Leaders
  argonRow(
    argonColumn(
      width = 3,
      argonUser(
        title = "Alex Jimenez",
        subtitle = "Event & Client Specialist, Wildflower Boutique",
        src = "alex_jimenez.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Erica Johnson",
        subtitle = "Founding Executive Director, Iowa Migrant Movement for Justice",
        src = "erica_johnson.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Dawn Martinez Oropeza",
        subtitle = "Executive Director, Al Éxito",
        src = "dawn_martinez_oropeza.png"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Noemi Mendez",
        subtitle = "Exectuive Director, Latina Leadership Initiative of Greater Des Moines",
        src = "noemi_mendez.png"
      )
    )
  )),
  
  # Consultant Row
  h3("Consultants"),
  argonRow(
    argonColumn(
      width = 4,
      argonUser(
        title = "Ruby Herrera",
        subtitle = "Educator, DMP",
        src = "ruby_herrera.png"
      )
    ),
    argonColumn(
      width = 4,
      argonUser(
        title = "Joel Huting",
        subtitle = "Co-Founder & Quantitative Lead, Vandegrift Huting Consultants",
        src = "joel_huting.jpeg"
      )
    ),
    argonColumn(
      width = 4,
      argonUser(
        title = "Darcie Vandegrift",
        subtitle = "Co-Founder & Qualitative Lead, Vandegrift Huting Consultants",
        src = "darcie_vandegrift.png"
      )
    )
  ),
  
  # Sponsor partners Row
  h3("Sponsor Partners"),
  argonRow(
    argonColumn(
      width = 6,
      argonUser(
        title = "Angela Dethlefs-Trettin",
        subtitle = "Partner",
        src = "profile.png"
      )
    ),
    argonColumn(
      width = 6,
      argonUser(
        title = "Suzanne Mineck",
        subtitle = "President and CEO, Mid-Iowa Health Foundation",
        src = "suzanne_mineck.png"
      )
    )
  ),
  
  
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