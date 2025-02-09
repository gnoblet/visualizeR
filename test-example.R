# dat <- data.frame(
#   x = c(15, 34, 59, 21, 33, 66),
#   y = c("Admin A", "Admin B", "Admin C", "Admin C", "Admin B", "Admin A"),
#   group = c("Displaced", "Non displaced", "Non displaced", "Displaced", "Displaced", "Non displaced")
# )


library(visualizeR)

# dat |>
#   bar(
#     x = "y",
#     y = "x",
#     #group = "group",
#     group_title = "Displacement Status",
#     flip = T,
#     add_text = T,
#     title = "In Admin A and C, Non-Displaced Persons Face Greater WASH Challenges Than Their Displaced Counterparts",
#     subtitle = "% of households not accessing WASH services by admin 1 and displacement status",
#     caption = "Source: FAO 2022. No message is a real one. Fake data are used in this example. As a cautiom, no decision should be made based on this plot.",
#   ) +
#   theme_bar(flip = T, add_text = T) +
#   scale_color_visualizer_discrete() +
#   scale_fill_visualizer_discrete()


library(rio)
dat <- import("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/11_SevCatOneNumNestedOneObsPerGroup.csv")

dumbbell(
  df,
  "stat",
  "setting",
  "admin1",
  title = "% of HHs that reported open defecation as sanitation facility",
  group_y_title = "Admin 1",
  group_x_title = "Setting"
)

library(dplyr)
library(data.table)
# dat as a data.table if it4s not
if (!checkmate::test_data_table(dat)) {
  rlang::warn("Converting dat to data.table.")
  data.table::setDT(dat)
}

# in all character columns, tranform empty string to NA
vars_chr <- colnames(dat)[sapply(dat, is.character)]
dat[, (vars_chr) := lapply(.SD, function(x) fifelse(x == "", NA_character_, x)), .SDcols = vars_chr]

# in value, if -1 replace with NA
dat[, value := fifelse(value == -1, NA_real_, value)]

# remove lines where value is NA (in place)
dat <- dat[!is.na(value), ]

dat
  # arrange(value) |>
  # group_by(region) |>
  # mutate(key = forcats::fct_reorder(key, value)) |>



dumbbell(
  dat |> arrange(value) |> tail(50) |> mutate(
    value = value/1000000,
    key = ifelse(key == "Democratic Republic of the Congo", "DRC", key)) |>
    filter(region %in% c("Europe", "Americas")),
  "value",
  "region",
  "key",
  title = "% of HHs that reported open defecation as sanitation facility",
  group_y_title = "Admin 1",
  group_x_title = "Setting", point_size = 3, line_to_y_axis = T

)


bar(
  df =  dat |> arrange(value) |> tail(20) |> mutate(
    value = value/1000000,
  key = ifelse(key == "Democratic Republic of the Congo", "DRC", key)),
  x = "key",
  y = "value",
  group = "region",
  group_title = "Region",
  order = "grouped",
  x_rm_na = T,
  y_rm_na = T,
  group_rm_na = T,
    flip = F,
  title = "Population of Global Regions in Million",
) +
theme_bar(flip = F, axis_text_x_angle = 45) +
scale_color_visualizer_discrete() +
scale_fill_visualizer_discrete()
