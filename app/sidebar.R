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
      "Overview"
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
      "Geo-profiles"
    ),
    argonSidebarItem(
      tabName = "tab_3",
      "Compare"
    ),
    argonSidebarItem(
      tabName = "tabs2",
      "Disparities"
    ),
    argonSidebarItem(
      tabName = "tab_0",
      "About"
    )
  ),
  argonSidebarDivider(),
  argonSidebarHeader(title = "Using 2019 Census Data") %>% argonTextColor(color = "white")
)
