# Filter Day or Night

MoveApps

Github repository: *github.com/movestore/Filter-Day-or-Night*

## Description
This App filters the data set to all night or day positions. The locations are given as output and a csv table artefact to download. Sunset and sunrise can be adapted by a selected time interval to define night/day use of your animal(s).

## Documentation
This App extracts all night or day positions of the input data set. For this, the sunriset() function from the maptools() package is used. If there are any locations in Arctic/Antarctic regions in times where there is no sunrise and sunset, all locations are retained or deleted, depending on day/night selection.

It is possible to leave the night/day selection empty. Then all locations are returned with added attributes: time of sunrise, time of sunsest and if it is day or night (daynight) for each location. 

Per default local sunrise and sunset are used to define the start/end of day/night. It is possible to adapt those start/end times by adding/subtracting a fixed number of minutes to/from the times of sunrise/sunset. This also affects the daynight attribute.

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
`data_selectedTime_night_day.csv`: csv-file with Table of all positions that are of low speed (below given threshold) and during night (day) as selected (see Documentation above)

### Parameters 
`window`: Radiobuttons selection to indicate if night locations ('sundownup') or day locations ('sunupdown') shall be selected by the filter.

`upX`: Sunrise adaption time that indicates how many minutes after (or if negative value before) sunrise you want your interval to start/end.

`downX`: Sunset adaption time that indicated how many minutes after (or if negative value before) sunset you want your interval to end/start.

### Null or error handling:
**Parameter `window`:** The default value (NULL, i.e. no selection of radio button) will return the input data set.

**Parameter `upX`:** The default value here is '0', indicating that no adaption time is necessary. If the value gets too large, the calculations might become difficult to understand.

**Parameter `downX`:** The default value here is '0', indicating that no adaption time is necessary. If the value gets too large, the calculations might become difficult to understand.

**Data:** If there are no day/night locations in the data set, NULL is returned, likely leading to an error.