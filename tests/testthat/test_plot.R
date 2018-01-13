context("plot")

#set wd to access raw test data in the package
mywd <- getwd()
setwd(system.file("extdata",package="quaker"))
df.raw <- readRDS("quakes.rds")
#df.raw <- quakes
setwd(mywd)

df.clean <- eq_clean_data(df.raw)
df.China <- df.clean %>%
  filter(COUNTRY == "CHINA")
df.3 <- df.clean %>%
  filter(COUNTRY %in% c("CHINA", "GREECE", "ITALY"))
df.neg <- df.clean %>%
  filter(lubridate::year(DATE) < 0)

tlsNNN <- ggplot2::ggplot(data = df.China, ggplot2::aes(x=DATE, y=COUNTRY, size=EQ_PRIMARY, colour=EQ_PRIMARY, alpha=TOTAL_DEATHS)) +
  geom_timeline() + ggplot2::guides(alpha = FALSE, colour = FALSE)
tlsYYN <- ggplot2::ggplot(data = df.China, ggplot2::aes(x=DATE, y=COUNTRY, size=EQ_PRIMARY, colour=EQ_PRIMARY, alpha=TOTAL_DEATHS)) +
  geom_timeline(xmin = -100, xmax = 1700) + ggplot2::guides(alpha = FALSE, colour = FALSE)
tl3NNN <- ggplot2::ggplot(data = df.3, ggplot2::aes(x=DATE, y=COUNTRY, size=EQ_PRIMARY, colour=EQ_PRIMARY, alpha=TOTAL_DEATHS)) +
  geom_timeline() + ggplot2::guides(alpha = FALSE, colour = FALSE)
tl3YYN <- ggplot2::ggplot(data = df.3, ggplot2::aes(x=DATE, y=COUNTRY, size=EQ_PRIMARY, colour=EQ_PRIMARY, alpha=TOTAL_DEATHS)) +
  geom_timeline(xmin = -100, xmax = 1700) + ggplot2::guides(alpha = FALSE, colour = FALSE)

test_that("geom_timeline", {
  expect_warning(ggplot2::ggplot(data=df.China,
                                 ggplot2::aes(x=DATE, y=COUNTRY, size = EQ_PRIMARY, alpha = TOTAL_DEATHS))+
                   geom_timeline(), regexp = NA) #not expecting warning
  expect_error(ggplot2::ggplot(data=df.China,
                                 ggplot2::aes(x=DATE, y=COUNTRY, size = EQ_PRIMARY, alpha = TOTAL_DEATHS))+
                   geom_timeline(), regexp = NA) #not expecting error
  vdiffr::expect_doppelganger("single y no minmax no labels", tlsNNN, "")
  vdiffr::expect_doppelganger("single y min max no labels", tlsYYN, "")
  vdiffr::expect_doppelganger("multiple y no minmax no labels", tl3NNN, "")
  vdiffr::expect_doppelganger("multiple y min max no labels", tl3YYN, "")
})

tlsNNY <- ggplot2::ggplot(data = df.China, ggplot2::aes(x=DATE, y=COUNTRY, size=TOTAL_DEATHS, colour=EQ_PRIMARY,
                                                        alpha=TOTAL_DEATHS, label=LOCATION_NAME)) +
  geom_timeline() +
  geom_timeline_label(n_max = 3) +
  ggplot2::guides(alpha = FALSE, colour = FALSE)
tlsYYY <- ggplot2::ggplot(data = df.China, ggplot2::aes(x=DATE, y=COUNTRY, size=TOTAL_DEATHS, colour=EQ_PRIMARY,
                                                        alpha=TOTAL_DEATHS, label=LOCATION_NAME)) +
  geom_timeline(xmin = -100, xmax = 1700) +
  geom_timeline_label(xmin = -100, xmax = 1700, n_max = 3) +
  ggplot2::guides(alpha = FALSE, colour = FALSE)

tlmNNY <- ggplot2::ggplot(data = df.3, ggplot2::aes(x=DATE, y=COUNTRY, size=TOTAL_DEATHS, colour=EQ_PRIMARY,
                                                    alpha=TOTAL_DEATHS, label=LOCATION_NAME)) +
  geom_timeline() +
  geom_timeline_label(n_max = 3) +
  ggplot2::guides(alpha = FALSE, colour = FALSE)
tlmYYY <- ggplot2::ggplot(data = df.3, ggplot2::aes(x=DATE, y=COUNTRY, size=TOTAL_DEATHS, colour=EQ_PRIMARY,
                                                    alpha=TOTAL_DEATHS, label=LOCATION_NAME)) +
  geom_timeline(xmin = -100, xmax = 1700) +
  geom_timeline_label(xmin = -100, xmax = 1700, n_max = 3) +
  ggplot2::guides(alpha = FALSE, colour = FALSE)

required_label_aesthetic <- ggplot2::ggplot(data=df.China, ggplot2::aes(x=DATE, y=COUNTRY, size = EQ_PRIMARY, alpha = TOTAL_DEATHS))+
  geom_timeline() + geom_timeline_label()

test_that("geom_timeline_label", {
  expect_error(print(required_label_aesthetic)) #expecting error - label is required aesthetic
  vdiffr::expect_doppelganger("single y no minmax labels", tlsNNY, "")
  vdiffr::expect_doppelganger("single y min max labels", tlsYYY, "")
  vdiffr::expect_doppelganger("multiple y no minmax labels", tlmNNY, "")
  vdiffr::expect_doppelganger("multiple y min max labels", tlmYYY, "")
})
