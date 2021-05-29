argonSidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "danger",
  size = "md",
  side = "left",
  id = "my_sidebar",
  # brand_url = "https://www.alexitoiowa.org/",
  # brand_logo = "nuestro_iowa_white.svg",
  br(),
  br(),
  br(),
  argonImage(src = "nuestro_iowa_white.svg"),
  argonSidebarHeader(title = "Main Menu") %>% argonTextColor(color = "white"),
  argonSidebarMenu(
    argonSidebarItem(
      tabName = "tab_1",
      "Overview"
    ),
    argonSidebarItem(
      tabName = "tab_2",
      "County Profiles"
    ),
    argonSidebarItem(
      tabName = "tabs",
      "Compare"
    ),
    argonSidebarItem(
      tabName = "tab_0",
      "About"
    )
  ),
  argonSidebarDivider(),
  argonSidebarHeader(title = "Using 2019 Census Data") %>% argonTextColor(color = "white")
)
