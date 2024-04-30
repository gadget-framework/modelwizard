<!---
Rebuild HTML with:
knitr::pandoc('other-models.md', 'html') ; fs::file_touch('ui.R')
-->

## Other modeling tools

Size-based methods and surplus production models have been widely employed to understand population dynamics in fisheries.
ICES has been developing methods to establish proxy MSY reference points for fish stocks with limited data (categories 3 and 4).

In order to provide open access to the tools and models developed by ICES and other collaborators, a repository has been created housing the following mathematical-statistical models: Length-based indicators (LBI), Length-based Spawning Potential Ratio (LBSPR), and Mean-length Z (MLZ).
These methods are specifically designed to assess the status of fish stocks based on length and biological parameters.
These tools are used to compute proxy MSY reference indicators.

Additionally, the repository includes links to interactive applications (shiny apps) that facilitate model execution and result interpretation as detailed below:

### Length-based indicators (LBI; Froese, 2004; ICES, 2015)

The LBI method assesses stocks based on conservation, sustainability, yield optimization, and maximum sustainable yield (MSY) objectives.
It achieves this by generating a set of indicators derived from the length composition of catches/landings,
utilizing proportion-at-length in catch (single year),
length at first capture in fishery (estimated from length frequency data),
biological parameters (Linf and length-at-maturity).

* **Application**: [https://scott.shinyapps.io/LBIndicator_shiny/](https://scott.shinyapps.io/LBIndicator_shiny/)
* **Datasets, examples, and R scripts**: [https://github.com/ices-tools-dev/ICES_MSY](https://github.com/ices-tools-dev/ICES_MSY)

### Length-based assessment of spawning potential ratio (LBSPR; Hordyk et al., 2015)

LBSPR assess stock status by the Spawning Potential ratio (SPR) defined as the proportion of Spawning Biomass per recruit (SBPR) in an exploited stock with regards to SBPR in an unfished (virgin) stock, utilizing the length composition data from the fishery and estimates of life history parameters.

* **Application**: [http://barefootecologist.com.au/lbspr.html](http://barefootecologist.com.au/lbspr.html)
* **User Guide**:  [https://cran.r-project.org/web/packages/LBSPR/vignettes/LBSPR.html](https://cran.r-project.org/web/packages/LBSPR/vignettes/LBSPR.html)
* **Datasets, examples, and R scripts**: [https://github.com/AdrianHordyk/LBSPR](https://github.com/AdrianHordyk/LBSPR)

### Mean-length Z (MLZ; Gendamke and Hoenig, 2006)

The MLZ method proposed by Gedamke and Hoenig (2006) enables total mortality estimation through the use of the time series of mean length (>full selection) in catch, length of full selection (estimated from catch length frequency data), and growth parameters.
Using a time series of mean length observations, the Gedamke- Hoenig estimator yields period-specific estimates of Z and the corresponding years of change in mortality.

* **Application**:  There is no web application available
* **User Guide**:  [https://cran.r-project.org/web/packages/MLZ/vignettes/MLZ.html](https://cran.r-project.org/web/packages/MLZ/vignettes/MLZ.html)
* **Datasets, examples, and R scripts**: [https://github.com/ices-tools-dev/ICES_MSY](https://github.com/ices-tools-dev/ICES_MSY)

### Stochastic surplus production model in continuous time (SPICT; Pedersen and Berg, 2017)

SPICT explicitly models both biomass and fishing dynamics as stochastic processes in a state-space framework.
It is formulated as a continuous time model to allow a representation of seasonal fishing behavior and incorporation of sub-annual catch and index data.
One of the most important inputs for fitting SPiCT is the catch biomass time series (by weight).
Additionally, SPiCT requires at least a biomass index to calibrate the population biomass and estimate the model parameters.
An important advantage of SPiCT is that it allows the use of multiple biomass indices with different time-series in addition to the catch time series.

* **Application**: [https://cloudfs.hcmr.gr/index.php/s/yRqWk0zTTOCWbZT?path=%2FSPiCT%2FSource%20code](https://cloudfs.hcmr.gr/index.php/s/yRqWk0zTTOCWbZT?path=%2FSPiCT%2FSource%20code)
* **ShinyApp manual**: [https://cloudfs.hcmr.gr/index.php/s/yRqWk0zTTOCWbZT/download?path=%2F&files=shinyApp_manual.pdf](https://cloudfs.hcmr.gr/index.php/s/yRqWk0zTTOCWbZT/download?path=%2F&files=shinyApp_manual.pdf)
* **User Guide**: [https://cloudfs.hcmr.gr/index.php/s/yRqWk0zTTOCWbZT?path=%2FSPiCT%2FSPiCT%20official%20manuals](https://cloudfs.hcmr.gr/index.php/s/yRqWk0zTTOCWbZT?path=%2FSPiCT%2FSPiCT%20official%20manuals)
* **Datasets, examples, and R scripts**: [https://cloudfs.hcmr.gr/index.php/s/yRqWk0zTTOCWbZT?path=%2FSPiCT%2FDemo-test%20data](https://cloudfs.hcmr.gr/index.php/s/yRqWk0zTTOCWbZT?path=%2FSPiCT%2FDemo-test%20data)
* **Alternative Shiny Application**: [https://ffdb.farfish.eu/shiny/spictgui/](https://ffdb.farfish.eu/shiny/spictgui/)

## Implementation Guide for Data-Limited Models 

Reproducible document providing an implementation guide for the four previously mentioned models:
size-based indicators (LBI),
size-based spawning potential ratio (LBSPR),
mean length-based mortality estimators (MLZ),
and stochastic surplus production in continuous time (SPiCT).

* **Guide**: [https://git.csic.es/math4fish/entregable_1_4/-/blob/main/Entregable_1_4.pdf](https://git.csic.es/math4fish/entregable_1_4/-/blob/main/Entregable_1_4.pdf)
