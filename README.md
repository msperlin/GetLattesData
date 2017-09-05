# Package GetLattesData

[Lattes](http://lattes.cnpq.br/) is the largest and unique platform for academic curriculumns. There you can find information about the academic work of **ALL** Brazilian scholars. It includes institution of PhD, current employer, field of work, all publications metadata and many more. It is an unique and reliable source of information for bibliometric studies. 

I've been working with Lattes data for some time. Here I present a short list of papers that have used this data.

-  [The Brazilian scientific output published in journals: A study based on a large CV database](http://www.sciencedirect.com/science/article/pii/S1751157716301559)
- [The researchers, the publications and the journals of Finance in Brazil: An analysis based on resumes from the Lattes platform](http://bibliotecadigital.fgv.br/ojs/index.php/rbfin/article/view/47157)    
- Predatory publications in the Brazilian academic system: an empirical analysis (Working paper)

Package `GetLattesData` is a wrap up of the functions that I've been using for acessing this dataset. It's main innovation is the possibility of downloading data directly from Lattes, without any kind of manual work. 

# Installation

The package is not yeat in CRAN. In the meanwhile, you can install it using devtools.

```
#install.packages('devtools')
devtools::install_github('msperlin/GetLattesData')
```

# Examples of usage 

```
library(GetLattesData)

# ids from EA-UFRGS
my.ids <- c('K4713546D3', 'K4440252H7', 
            'K4783858A0', 'K4723925J2')

# qualis for the field of management
field.qualis = 'ADMINISTRAÇÃO PÚBLICA E DE EMPRESAS, CIÊNCIAS CONTÁBEIS E TURISMO'

l.out <- gld_get_lattes_data(id.vec = my.ids, field.qualis = field.qualis)

tpublic <- l.out$tpublic
str(tpublic)
```
