# ModelWizard: A marine model builder for Gadget3 and SS3

## Local installation

The following system dependencies need to be installed:

```
apt install r-recommended liblapack-dev libblas-dev gfortran
# For r4ss
apt install libfontconfig1-dev
# For rsconnect / shinyapps.io
apt install libcurl4-openssl-dev libssl-dev
```

Then R dependencies can be installed with:

```
Rscript install-deps.R
```

To run the app, 

## Branding

Symlink (or create a new) ``branding.R``, e.g:

```
ln -rs branding.IEOinputSA.R branding.R
```

## Shinyapps.io deployment

Once installed locally, you can deploy to your shinyapps account with:

```
rsconnect::setAccountInfo(...)
rsconnect::deployApp('.')
```

## Acknowedgements

This work has been developed in the framework of Math4fish a project funded by European Union -NextGenerationEU. Component 3. Investment 7.
Convenio entre el Ministerio de Agricultura,
Pesca y Alimentación y la Agencia Estatal Consejo Superior de Investigaciones Científicas M.P. -
A través del Instituto Español de Oceanografía -
Para impulsar la investigación pesquera como base para la gestión pesquera sostenible, Eje6.
Math4Fish: Nuevas Herramientas para el Modelado Matemático en el Asesoramiento Científico de Pesquerías Españolas.
