# RE-SWOT
This is a visualization tool that employs NLP and Visualization to enable the competitive analysis from app reviews.
It relies on R/Shiny and Tableau Desktop.

# Step 1: Download or clone this repository

# Step 2: Install the required R packages

Open RStudio and run:
install.packages(c("DT", "shiny", "shinydashboard", "shinyjs", "stringr", "udpipe", "quanteda", "plyr", "dplyr", "data.table", "rjson", "httr", "tidytext", "gtools", "V8"))

Optional: For using Google sheets for storing data, you will also need to install "googlesheets" package and uncomment the relevant rows on app.R.

# Step 3: Run a Shiny app and upload review CSV
With app.R open, click on "Run app" to run a local version of the RE-SWOT Shiny app.

Within the Shiny app, you can upload a CSV with review data following the format specified in the app.
After the upload, data is processed and the visualization data is ready for download.

# Step 4: Use the downloaded visualization data on Tableau
After making sure you have Tableau Desktop installed, open the workbook ReSWOT.twb. 
It comes with a sample csv of data loaded, which you can replace with the data generated on Shiny.
To replace the csv on Tableau, follow these steps:
- Go to Data > Add new data source > Text file
- Select the csv generated by Shiny
- Replace the sample data by the new csv
- Note: You might need to adjust the text qualifier for "" so that all columns are correctly detected

# Optional: Host app on the cloud
You can optionally host this Shiny app and Tableau visualization on the cloud so it can be accessed from anywhere. For that, the following infrastructure was used:
- Tableau Server: https://www.tableau.com/products/server
- Google Sheets: Create a spreadsheet using a Google account and connect to it using the library googlesheets (https://github.com/jennybc/googlesheets/tree/master/inst/shiny-examples) 
- Shiny Server: https://www.rstudio.com/products/shiny/shiny-server/

In this case, a connection to Google Sheets needs to be established on Shiny using googlesheets package, and a connection to Tableau needs to be established so that a spreadsheet is used as a data connection. Besides that, the Tableau workbook should be uploaded to Tableau Server with the Google credentials embedded.
Finally, the visualization hosted on Tableau server can be embedded on Shiny by replacing the url in row 38 of app.R.
