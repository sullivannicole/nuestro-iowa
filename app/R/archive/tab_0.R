tab_0 <- argonTabItem(
  tabName = "tab_0",
  br(),
  br(),
  argonCard(
    width = 12,
    title = "About",
    src = NULL,
    hover_lift = TRUE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    # icon = argonIcon("atom"),
    status = "primary",
    background_color = NULL,
    gradient = FALSE, 
    floating = FALSE,
    argonRow(
      
      argonH1("Welcome to the Nuestro Iowa project!", display = 4),
      "'Nuestro Iowa' means 'Our Iowa' in Spanish and refers to the collective Iowan experience - the experience of the Latinx Iowan community, and how that experience is a vibrant and integral part of the broader Iowan community as well. 'Nuestro Iowa' celebrates the richness of Latinx Iowa; it also evokes the sense of a shared Iowa across racial and ethnic experiences and highlights opportunities for improving equity for Iowan hermanos and hermanas that are Latinx."
    ) %>% argonPadding(orientation = 'l', value = 4) %>% argonPadding(orientation = 'r', value = 4)
  ),
  br()
)