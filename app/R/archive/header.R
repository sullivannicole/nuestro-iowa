source("tabs/tab_2.R")

argonHeader <- argonDashHeader(
  gradient = TRUE,
  color = "danger",
  separator = TRUE,
  separator_color = "secondary",
  h1("Take a deep dive into Latinx Iowan life"),
  h3("through the lens of US Census data"),
  br(),
  br(),
  # argonRow(
  #   argonColumn(width = 3,
  #               argonImage(
  #                 url = "https://www.google.com",
  #                 # src = "nuestro_iowa_white.svg",
  #                 floating = FALSE,
  #                 card_mode = FALSE
  #               )),
  #   argonColumn(width = 6),
  #   argonColumn(argonDropNav(
  #   title = "Data", 
  #   src = NULL,
  #   # src = "transparent.svg",
  #   orientation = "left",
  #   argonDropNavTitle(title = "Welcome!"),
  #   argonDropNavItem(
  #     title = "Item 1", 
  #     src = argonTabItem("tab_2"),
  #     icon = argonIcon("single-02")
  #   ),
  #   argonDropNavItem(
  #     title = "Item 2", 
  #     src = NULL, 
  #     icon = argonIcon("settings-gear-65")
  #   ),
  #   argonDropNavDivider(),
  #   argonDropNavItem(
  #     title = "Item 3", 
  #     src = "#", 
  #     icon = argonIcon("calendar-grid-58")
  #   )
  # ))),
  argonRow()
  # argonCard(
  #   title = "",
  #   src = "http://www.google.com",
  #   width = 12,
  #   hover_lift = TRUE,
  #   shadow = TRUE,
  #   shadow_size = NULL,
  #   hover_shadow = FALSE,
  #   border_level = 0,
  #   #icon = argonIcon("atom"),
  #   status = "primary",
  #   background_color = NULL,
  #   gradient = FALSE, 
  #   floating = FALSE
  #   
  # )
)