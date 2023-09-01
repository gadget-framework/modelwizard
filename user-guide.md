# ModelWizard: A user guide

## Overview

At the very top of the page, you will see a set of tabs:

* **Specification**: Here you can configure the overall model structure, as well as load & save to a spreadsheet
* **Data**: Here you can view fill in the data tables required by the structure
* **Parameters**: Here you can view parameters the model will need, and set min/max/initial values
* **Gadget3 script**: Here you can get an R script to build a [gadget3](https://gadget-framework.github.io/gadget3/) model using your specification & data
* **SS3/r4ss script**: Here you can get an R script to build a [r4ss](https://github.com/r4ss/r4ss) model using your specification & data

## Building a model

1. Fill in inputs on the *Specification* tab. You can have any number of fleets and indices, use the "Add new" buttons to make an extra set available.
2. Once done, either visit the *Data* tab and fill in the tables, or press "Save data to spreadsheet" and fill in the tables using Excel / Libreoffice. Load the spreadsheet again once done.
3. Fill in the table on the *Parameters* tab. The parameters here will vary based on your model.
4. Finally, visit one of the *script* tabs. Download your spreadsheet and copy-paste the script into R.

