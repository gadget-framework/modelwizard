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

## Building a gadget3 model

1. Fill in inputs on the *Specification* tab.

   You can have any number of fleets and indices, use the "Add new" buttons to make an extra set available.

2. Once done, either:

     1. Visit the *Data* tab and fill in the tables from your data sources.
     2. You can also press "Save data to spreadsheet" and fill in the tables using Excel / Libreoffice.
     3. Ignore the data tab, and modify the example script to extract data from an [MFDB](https://gadget-framework.github.io/mfdb/) instance instead of a spreadsheet.

3. Visit the script tab. Download your spreadsheet and copy-paste the script into R.

4. Run the script, which will optimise your model and produce a detailed output page.

   Visit [demo_output_figures](demo_output_figures.html) to see representative output from running the demo.

At this point you have should have a functional gadget3 model.
However, the results it produces will not be useful without futher tuning.

## Building a SS3 model

1. Fill in inputs on the *Specification* tab.

   You can have any number of fleets and indices, use the "Add new" buttons to make an extra set available.

2. Once done, either:

     1. Visit the *Data* tab and fill in the tables from your data sources.
     2. You can also press "Save data to spreadsheet" and fill in the tables using Excel / Libreoffice.
     3. Ignore the data tab, and modify the example script to extract data from an [MFDB](https://gadget-framework.github.io/mfdb/) instance instead of a spreadsheet.

3. Visit the script tab. Download your spreadsheet and copy-paste the script into R.

At this point you should have the beginnings of an SS3 model.
See [the Math4Fish website](https://math4fish.ieo.csic.es/proyecto-2-3/) for introductory material on developing an SS3 model.
