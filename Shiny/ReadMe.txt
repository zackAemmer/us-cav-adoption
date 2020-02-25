########################################################
This R-Shiny interactive tool applies the mode choice model to the matched PUMS/LODES dataset, as detailed in the "Magic Carpet Year 3" final report.

########################################################
Files in this folder that are required to run the tool:

app.R:
The interface and logical programming of the tool.

runUtilityModel.R:
A function to perform the utility calculations for each mode and individual. This is called by app.R.

matched_den.rds:
The set of matched individuals from the PUMS/LODES dataset. This is loaded on tool startup.

puma_shapes.rds:
Contains the PUMA polygons and information for displaying in the interactive map.

state_shapes.rds:
Contains the State polygons and information for displaying in the interactive map.

########################################################
R Libraries that must be installed prior to running the tool:

"shiny"
"leaflet"
"maps"
"mapproj"
"stringr"
"htmltools"

These can be installed by calling install.packages("name_of_package") in the R command line

########################################################
Starting the Tool:

1. Open app.R in Rstudio or other Interactive Development Environment

2. Set the working directory to the same location as the app.R file
	setwd("C:/...your_directory_here/shiny")

3. Select all contents of the app.R file in Rstudio, and run them
	ctrl + a
	ctrl + enter
	(Can alternatively click "Run App" in the top right corner of Rstudio)

4. The tool will then start and load the puma/state shapefiles

########################################################
Operating the Tool:

1. Choose calculation parameters as displayed the left-hand panel of the app screen
	-Base Prices are the initial price for using a service
	-Price per mile is the additional cost per mile of trip to use a service

2. Choose view parameters as displayed in the middle of the app screen
	-State for PUMA evaluation will limit the PUMAs displayed to one state for the sake of speeding up the map generation. This setting does nothing if the Aggregation Level is set to "State"

3. Click Run Model when settings are set as desired
	-Allow ~2min for the utility calculations. The Rstudio console will provide information on which step of the process the tool is currently performing

4. Zoom, drag, and hover over areas of interest to view the map once it is generated
	-Change view parameters to adjust how results are shown
	-Select "All States" under state for PUMA evaluation to see all US PUMAs: this map takes ~5mins to generate and is slower to view

5. Use the "Save Results" button to save the calculated utility values to a file named "utils.csv" in the same folder as the app
	-Running this multiple times will replace any previously saved data unless utils.csv is renamed inbetween saves

########################################################
Notes:

-Changing any view parameters will instantly update the map. This does not perform any new utility calculations; it only changes how they are displayed

-Changing any calculation parameters will not update the map until "Run Model" is clicked again

-Trip density and Accessibility metrics are not available under the "State" aggregation level

-Changing the Service Types parameter only changes which services are shown on the map, it does not recalculate utilities for the implementation of different modes. It is assumed that all modes are implemented for all metrics

########################################################
Developed By:

Zack Aemmer
Tianqi Zou
Don MacKenzie
Sustainable Transportation Lab, University of Washington