# Filter Day or Night

MoveApps

Github repository: *github.com/movestore/Filter-Day-or-Night*

## Description
This App assigns location records as occurring during day or night, and can then filter the dataset to only daytime or nighttime records. The time of sunset and sunrise can be adapted by a user-specified time interval to adjust the day/night assignments, for example to account for the behavior the animal(s) in the dataset. The resulting dataset with added values is provided as output and can be downloaded as a .csv artefact. 

## Documentation
This App calculates the time of sunrise and sunset for the timestamp and location of each record in your dataset, using the sunriset function in the [maptools](https://cran.r-project.org/web/packages/maptools/index.html) package. Based on these results, it then defines each record as "day" or "night", using an optional user-defined adjustment to the sunrise and sunset times, for example, to account for behavioral patterns of the animal(s) in your dataset or local site conditions. All calculations assume that the input data provide timestamp in UTC and locations in the WGS85 coordinate reference system.

By default, the start and end of day and night are defined by the time of local sunrise and sunset. Optionally, adaptation times can be defined in the settings to adjust the time of sunrise and/or sunset by a fixed number of minutes. This adjustment will affect the reported sunrise and sunset times, as well as the day/night assignment.

You can choose to extract only daytime or nighttime locations from the dataset, or leave the selection empty to retain the full dataset. The time of sunrise and sunset (adjusted according to the adaptation time if specified), a day/night assignment, the year and Julian day are added to the final dataset. 

Note that if the dataset contains records at high latitudes (Arctic/Antarctic) during periods when there are no sunrise and sunset, it is possible that all locations will be retained or deleted, depending on settings.

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
`data_selectedTime_night_day.csv`: csv file containing the original dataset, filtered for daytime or nighttime location if specified, with the calculated time of sunrise, time of sunset, day/night assignment, year and Julian day, based on the selected settings. Times of local sunrise and sunset are reported in the format `yyyy-MM-dd HH:mm:ss` (UTC).

### Parameters
`window`: Optional selection to indicate whether to extract only nighttime (between sunset and sunrise, 'sundownup') or daytime (between sunrise and sunset, 'sunupdown') locations, or to retain the full dataset (no selection).

`upX`: Sunrise adaption time to define how many minutes before (negative values) or after (positive values) sunrise you want to define the transition between night and day. This is 0 by default.

`downX`: Sunset adaption time how many minutes before (negative values) or after (positive values) sunset you want to define the transition between day and night. This is 0 by default.

### Null or error handling:
**Parameter `window`:** The default value (NULL, i.e., no selection of radio button) will return the input data set.

**Parameter `upX`:** The default value here is '0', indicating that no adaption time is necessary. If the value gets too large, the calculations might become difficult to understand.

**Parameter `downX`:** The default value here is '0', indicating that no adaption time is necessary. If the value gets too large, the calculations might become difficult to understand.

**Data:** If there are no day/night locations in the dataset, NULL is returned, likely leading to an error.