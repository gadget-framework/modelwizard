<!---
Rebuild HTML with:
knitr::pandoc('user-guide.md', 'html') ; fs::file_touch('ui.R')
-->

## Overview

At the very top of the page, you will see a set of tabs:

* **Load / save**: Here you can load or save the model data to a spreadsheet
* **Specification**: Here you can configure the overall model structure
* **Data**: Here you can view fill in the data tables required by the structure
* **Gadget3 script**: Here you can get an R script to build a [gadget3](https://gadget-framework.github.io/gadget3/) model using your specification & data
* **SS3/r4ss script**: Here you can get an R script to build a [r4ss](https://github.com/r4ss/r4ss) model using your specification & data

## Starting a model

By default, Modelwizard will start with all fields empty, so you can start a new model from scratch.

If you want to start from the demo model, click the **Load / save** tab and **Load demo data**

If you want to start from a previously saved model, click the **Load / save** tab and upload a file under **Load spreadsheet**

## Filling in a model specification

Firstly, you need to describe your scenario on the **Specification** tab.
Either start here to fill in details or click "Load demo data" on the **Load / save** tab.

### **Area** section

Modelwizard currently only supports single-area models.
Enter a name for the area your model covers here.

### **Time** section

Enter the years your model will cover, and seasonality if desired.

### **Stock** section

Give your stock a name and describe it's physical attributes, to configure the model bins.

### **Commercial & survey fleets** section

For each fleet present in your model, click **Add new** to add a section to the page:

* **identifier**: A short name for the fleet to use in the model
* **Start/end year for fleet**: If the fleet is not present for the entire model period, enter the years for which it is operational
* **Landings in**: The unit the landings data will be provided in the next tab
* **Length distribution** / **Age-Length distribution**: If data is available for either, choose the unit and provide the data on the next tab

### **Abundance indices** section

As with fleets, click **Add new** to add a section to the page.
The options are the same, apart from:

* **Aggregated observations**: If you have observation data not broken down by length/age, choose the unit and provide the data on the next tab

## Providing observation data for the model

The **Data** tab provides a spreadsheet view for each data set required by your model.
Year/step/area/age/length columns will be pre-filled based on the specification, and a value column will be ready to fill in.

If you prefer, you can use Excel to fill in your data. Go back to the **Load / save** tab, click **Save data to spreadsheet**, and open the downloaded spreadsheet in Excel.

Finally, you can skip this step entirely. Instead, generate a model script, and modify the code to load the data from another source.

## Building a gadget3 model

Once all model data is provided, you can visit the **Gadget3 script** tab to download a script that will build and run your model.

Download the spreadsheet, copy-paste the code into a R script and run it.

Visit [demo_output_figures](demo_output_figures.html) to see representative output from running the demo.

At this point you have should have a functional gadget3 model.
However, the results it produces will not be useful without futher tuning.
The next step should be the [gadget3 documentation](https://gadget-framework.github.io/gadget3/articles/) to learn more about how to customise your model.

## Building a SS3 model

Once all model data is provided, you can visit the **SS3/r4ss script** tab to download a script that will build and run your model.

Download the spreadsheet, copy-paste the code into a R script and run it.

Visit [demo_output_figures](demo_output_figures.html) to see representative output from running the demo.

At this point you have should have a functional SS3 model.
However, the results it produces will not be useful without futher tuning.
The next step should be the [SS3 documentation](https://vlab.noaa.gov/web/stock-synthesis) and [r4ss documentation](https://r4ss.github.io/r4ss/) to learn more about how to customise your model.

Also, visit [the Math4Fish website](https://math4fish.ieo.csic.es/proyecto-2-3/) for introductory material on developing an SS3 model.
