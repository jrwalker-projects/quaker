# quaker
R package to display &amp; map earthquake data

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis build status](https://travis-ci.org/jrwalker-projects/quaker.svg?branch=master)](https://travis-ci.org/jrwalker-projects/quaker)
[![Coverage status](https://codecov.io/gh/jrwalker-projects/quaker/branch/master/graph/badge.svg)](https://codecov.io/github/jrwalker-projects/quaker?branch=master)
[![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/badges/shields.svg)]()
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg?style=flat)](https://github.com/jrwalker-projects/quaker)


The quaker package can assist in access to the U.S. National Oceanographic and Atmospheric Administration (NOAA) on significant earthquakes around the world. 

The quaker function **eq_clean_data** prepares NOAA website data for visualization and analysis

ggplot2 extentions **geom_timeline** and **geom_timeline_label** allow views of earthquake data over time for one or more countries

![Sample timepline chart for US & Greece with location labels for the largest quakes by magnitude](man/figures/ReadmeTimelineLabels.png)

interactive leaflet maps **eq_map** displays earthquake locations on an interactive map providing additional information as the user selects data points

![Sample map for Mexico earthquakes showing location, magnitude and the total number of deaths for data points](man/figures/ReadmeEqMaps.png)

Unit testing for the new geoms uses the vdiffr library to compare plot output.

To install the quaker package:

	install.packages("devtools")
	devtools::install_github("jrwalker-projects/quaker")