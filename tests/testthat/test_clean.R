
context("data clean")

#set wd to access raw test data in the package
setwd(system.file("extdata",package="quaker"))
df.raw <- readRDS("quakes.rds")
setwd("..")

test_that("read raw data from inst-extdata", {
    expect_is(df.raw, "tbl_df") #type tibble
    expect_gt(ncol(df.raw), 0) #at least 1 col
    expect_gt(nrow(df.raw), 0) #at least 1 row
    expect_equal(names(df.raw[1]), "I_D") #check a col name
})

df.clean <- eq_clean_data(df.raw)
df.bce <- df.clean %>%
    filter(lubridate::year(df.clean$DATE) < 0)
na.dates <- sum(is.na(df.clean$DATE))

test_that("eq_clean_data", {
    expect_is(df.clean, "tbl_df") #type table_df 'tibble'
    expect_gt(nrow(df.clean), 0) #at least 1 row
    expect_is(df.clean$DATE, "Date") #date column created as type "Date"
    expect_equal(na.dates, 0) #none of the cleaned dates are NA
})
