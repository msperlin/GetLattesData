# Package GetLattesData

[![](http://cranlogs.r-pkg.org/badges/GetTDData)](https://cran.r-project.org/web/packages/GetTDData/index.html)

[Lattes](http://lattes.cnpq.br/) is an unique and largest platform for academic curriculumns. There you can find information about the academic work of **all** Brazilian scholars. It includes institution of PhD, current employer, field of work, all publications metadata and more. It is an unique and reliable source of information for bibliometric studies. 

I've been working with Lattes data for some time. Here I present a short list of papers that have used this data.

-  [The Brazilian scientific output published in journals: A study based on a large CV database](http://www.sciencedirect.com/science/article/pii/S1751157716301559)

- [The researchers, the publications and the journals of Finance in Brazil: An analysis based on resumes from the Lattes platform](http://bibliotecadigital.fgv.br/ojs/index.php/rbfin/article/view/47157)    

- [Análise do Perfil dos Acadêmicos e de suas Publicações Científicas em Administração (in Portuguese)](http://www.scielo.br/scielo.php?script=sci_arttext&pid=S1415-65552017000100062)

- Is predatory publishing a real threat? Evidence from a large database study. [Working paper](https://www.dropbox.com/s/9xcuc8z64fntxob/PredatoryPublishing_v2.pdf?dl=0)

Package `GetLattesData` is a wrap up of functions I've been using for accessing the dataset. It's main innovation is the possibility of downloading data directly from Lattes, without any manual work or captcha solving. 

# Installation

The package is available in CRAN:

```
install.packages('GetLattesData')
```
  
You can also install the development version from Github:

```
#install.packages('devtools')
devtools::install_github('msperlin/GetLattesData')
```

# Example of usage 

See [vignette for more examples](https://cran.r-project.org/web/packages/GetTDData/index.html).

```
library(GetLattesData)

# ids from EA-UFRGS
my.ids <- c('K4713546D3', 'K4440252H7', 
            'K4783858A0', 'K4723925J2')

# qualis for the field of management
field.qualis = 'ADMINISTRAÇÃO PÚBLICA E DE EMPRESAS, CIÊNCIAS CONTÁBEIS E TURISMO'

l.out <- gld_get_lattes_data(id.vec = my.ids, field.qualis = field.qualis)

tpublic <- l.out$tpublic
dplyr::glimpse(tpublic)
```
