argonNav <- argonDashNavbar(
  img = "nuestro_iowa_white.svg",
  argonDropNav(
    title = "Data", 
    src = "transparent.svg",
    # src = "nuestro_iowa_white.svg",
    orientation = "left",
    argonDropNavTitle(title = "Welcome!"),
    argonDropNavItem(
      title = "Item 1", 
      src = "https://www.google.com", 
      icon = argonIcon("single-02")
    ),
    argonDropNavItem(
      title = "Item 2", 
      src = NULL, 
      icon = argonIcon("settings-gear-65")
    ),
    argonDropNavDivider(),
    argonDropNavItem(
      title = "Item 3", 
      src = "#", 
      icon = argonIcon("calendar-grid-58")
    )
  )
)