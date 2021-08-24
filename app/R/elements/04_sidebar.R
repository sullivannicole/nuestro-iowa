argonSidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "danger",
  size = "md",
  side = "left",
  id = "app_sidebar",
  # brand_url = "https://www.alexitoiowa.org/",
  # brand_logo = "nuestro_iowa_white.svg",
  br(),
  br(),
  br(),
  argonImage(src = "nuestro_iowa_white.svg"),
  argonSidebarHeader(title = "Main Menu") %>% argonTextColor(color = "white"),
  argonSidebarMenu(
    argonSidebarItem(
      tabName = "tab_1_alt_a",
      "Nuestro Iowa"
    ),
    # argonSidebarItem(
    #   tabName = "tab_1_alt_a",
    #   "Overview Alt A"
    # ),
    # argonSidebarItem(
    #   tabName = "tab_2",
    #   "County Profiles"
    # ),
    argonSidebarItem(
      tabName = "tab_2_alt_a",
      "Nuestro county"
    ),
    argonSidebarItem(
      tabName = "tab_3",
      "Disparities"
    ),
    argonSidebarItem(
      tabName = "tab_4",
      "Nuestro future"
    )
  ),
  argonSidebarDivider(),
  argonSidebarHeader(title = "Using American Community Survey Data") %>% argonTextColor(color = "white")
)
