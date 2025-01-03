dat <- data.frame(
  x = c(15, 34, 59, 21, 33, 66),
  y = c("Admin A", "Admin B", "Admin C", "Admin C", "Admin B", "Admin A"),
  group = c("Displaced", "Non displaced", "Non displaced", "Displaced", "Displaced", "Non displaced")
)

dat |> 
  bar(
    x = "y",
    y = "x",
    group = "group",
    flip = F,
    add_text = F,
    title = "In Admin A and C, Non-Displaced Persons Face Greater WASH Challenges Than Their Displaced Counterparts",
    subtitle = "% of households not accessing WASH services by admin 1 and status",
    caption = "Source: FAO 2022. No message is a real one. Fake data are used in this example. As a cautiom, no decision should be made based on this plot.",
  ) +
  theme_visualizer_bar() +
  scale_color_visualizer_discrete() +
  scale_fill_visualizer_discrete()


