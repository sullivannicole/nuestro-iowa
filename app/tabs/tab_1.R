tab_1 <- argonTabItem(
  tabName = "tab_1",
  br(),
  br(),
  div(class = "header-container",
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
                  div(style = "margin-right: 40px;",
                     argonRow(width = 12,
                    argonColumn(width = 1),
                    argonColumn(argonH1(display = 3, "A deep dive into life in the Latinx-Iowan community") %>% argonTextColor(color = "white"),
                  argonLead("Latinx Iowans make up a larger percentage of the state's population today than ever before 
            - and that proportion only continues to grow. What's the Iowan home like for the Latinx?") %>% argonTextColor(color = "white")),
                  argonColumn(width = 4, argonImage(src = "latam.svg", floating = TRUE) %>% argonBlur() %>% argonPersp(side = "right") ))),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                 )),
      # style = "margin-left: -75px !important;
      #         margin-top: -100px !important;
      #         margin-right: -97px !important;"),
  
  # Row 1: info cards
  # argonH1("Info Cards", display = 4),
  div(style = "margin-top: 450px !important;",
    br(),
    br(),
      argonRow(
        argonCard(width = 4,
                  shadow = TRUE,
                  border_level = 2,
                  hover_shadow = TRUE,
                  hover_lift = TRUE,
                  center = TRUE,
                  textOutput("state_total_latinx") %>% h1(),
                  # div(style = "display: inline-block;", argonIcon("chart-pie-35", color = "primary")),
                  # div(style = "display: inline-block", h4(" ")),
                  # div(style = "display: inline-block;", textOutput("state_pc_latinx") %>% h4()),
                  br(),
                  argonProgress(value = 8, status = "warning", text = "total metro pop.")
                  ),
        argonCard(width = 4,
                  center = TRUE,
                  shadow = TRUE,
                  border_level = 2,
                  hover_shadow = TRUE,
                  hover_lift = TRUE,
                  h1("+1.3% of the metro pop since 2009"),
                  plotOutput("state_pc_latinx_trend", height = "60%", width = "100%")
        ),
        argonCard(width = 4,
                  center = TRUE,
                  shadow = TRUE,
                  border_level = 2,
                  hover_shadow = TRUE,
                  hover_lift = TRUE,
                  h1("placeholder text"),
                  h4("More placeholder text.")
        )
  )),
  br(),
  br(),
  
  # Row 2: map
    # argonCard(
    #   width = 14,
    #   src = NULL,
    #   icon = icon("map"),
    #   # status = "success",
    #   shadow = TRUE,
    #   border_level = 2,
    #   hover_shadow = TRUE,
    #   title = "Latinx pop. in Iowa",
      argonRow(width = 12,
               div(class = "map-btn-container", dropdownButton(radioButtons("unit", "See Latinx pop. as a:",
                                           c("percent of the county pop." = "as a %",
                                             "raw number" = "as a raw num"), selected = "as a raw num"))), 
                div(class = "map-container", leafletOutput("map"))),
      # argonRow(center = TRUE,
      #          radioButtons("unit", "See Latinx pop. as a:",
      #                       c("percent of the county pop." = "as a %",
      #                         "raw number" = "as a raw num"), selected = "as a raw num")), #),
    br(), br(),
    div(style = "padding-top: 500px;",
      argonCard(
      width = 12,
      title = "Argon Card",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = FALSE,
      argonRow(
        argonColumn(
          width = 6,
          radioButtons(
            "dist", 
            "Distribution type:",
            c("Normal" = "norm",
              "Uniform" = "unif",
              "Log-normal" = "lnorm",
              "Exponential" = "exp")
          )
        ),
        argonColumn(width = 6, plotOutput("plot"))
      )
    )),
  br(),
  
  # info cards
  
  
  # profile cards
  argonH1("User Cards", display = 4),
  argonRow(
    argonColumn(
      width = 3,
      argonUser(
        title = "Ryan Tompson",
        subtitle = "Web Developer",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-1-800x800.jpg"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Romina Hadid",
        subtitle = "Marketing Strategist",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-2-800x800.jpg"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Alexander Smith",
        subtitle = "UI/UX Designer",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-3-800x800.jpg"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "John Doe",
        subtitle = "Founder and CEO",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-4-800x800.jpg"
      )
    )
  ),
  br(), br(),
  
  argonH1("Profile Card", display = 4),
  argonRow(
    argonColumn(
      width = 12,
      argonProfile(
        title = "John",
        subtitle = "Japan, Kagoshima",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-1-800x800.jpg",
        url = "https://www.google.com",
        url_1 = "https://www.google.com",
        url_2 = "https://www.google.com",
        stats = argonProfileStats(
          argonProfileStat(
            value = 22,
            description = "Friends"
          ),
          argonProfileStat(
            value = 10,
            description = "Photos"
          ),
          argonProfileStat(
            value = 89,
            description = "Comments"
          )
        ),
        "An artist of considerable range, Ryan — 
                  the name taken by Melbourne-raised, 
                  Brooklyn-based Nick Murphy — writes, 
                  performs and records all of his own music, 
                  giving it a warm, intimate feel with a solid 
                  groove structure. An artist of considerable 
                  range."
      )
    )
  )
)