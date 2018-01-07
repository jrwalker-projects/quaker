context("runmap")

#set wd to access raw test data in the package
setwd(system.file("extdata",package="quaker"))

df.raw <- readRDS("quakes.rds")
df.clean <- eq_clean_data(df.raw)
df.map <- df.clean %>%
  filter(!is.na(LATITUDE),
         !is.na(LONGITUDE)) #avoid warning
date.map <- eq_map(df.map, annot_col = "DATE")
date.map.pretty <- eq_map(df.map, annot_col = "DATE", pretty = TRUE)

test_that("eq_map", {
  expect_is(date.map, c("leaflet", "htmlwidget")) #leaflet map
  expect_is(date.map[1], "list") #type List
  expect_gt(length(date.map$x), 0) #not empty
  expect_warning(eq_map(df.map, annot_col = "DATE"), regexp = NA) #not expecting any warnings
  expect_error(eq_map(df.map, annot_col = "DATE"), regexp = NA) #not expecting errors
  expect_error(eq_map(df.map, annot_col = "MISTAKE")) #expect error on bad col name
  expect_is(date.map.pretty, c("leaflet", "htmlwidget")) #leaflet map
  expect_is(date.map.pretty[1], "list") #type List
  expect_gt(length(date.map.pretty$x), 0) #not empty
  expect_warning(eq_map(df.map, annot_col = "DATE", pretty = TRUE), regexp = NA) #not expecting any warnings
  expect_error(eq_map(df.map, annot_col = "DATE", pretty = TRUE), regexp = NA) #not expecting errors
  })

df.label <- df.map %>%
  dplyr::mutate(popup_text = eq_create_label(.))
label.map <- eq_map(df.label, annot_col = "popup_text")

test_that("eq_create_label", {
  expect_is(label.map, c("leaflet", "htmlwidget")) #leaflet map
  expect_is(label.map[1], "list") #type List
  expect_gt(length(label.map$x), 0) #not empty
  expect_warning(mutate(df.map, popup_text = eq_create_label(df.map)), regexp = NA) #not expecting any warnings
  expect_error(eq_map(df.label, annot_col = "DATE"), regexp = NA) #not expecting errors
})
