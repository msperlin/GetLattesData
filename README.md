<!-- badges: start -->
[![R-CMD-check](https://github.com/msperlin/GetLattesData/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/msperlin/GetLattesData/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/GetLattesData)](https://CRAN.R-project.org/package=GetLattesData)
[![](https://cranlogs.r-pkg.org/badges/GetLattesData)](https://CRAN.R-project.org/package=GetLattesData)
[![License: GPL-2](https://img.shields.io/badge/license-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
<!-- badges: end -->

# Package GetLattesData

[Lattes](https://lattes.cnpq.br/) is the largest platform of academic CVs in Brazil. It holds information about the academic work of **all** Brazilian scholars, including the institutions of their degrees, current employer, field of work, publication metadata, and much more. It is a unique and reliable source of information for bibliometric studies.

Package `GetLattesData` provides a simple API to **read the XML files exported by the Lattes platform** into tidy dataframes, and to enrich publications with **Qualis** (the Brazilian journal ranking) and **SJR** indicators.

Some studies that have used this data:

- Is predatory publishing a real threat? Evidence from a large database study. [Scientometrics](https://link.springer.com/article/10.1007/s11192-018-2750-6)
- The Brazilian scientific output published in journals: A study based on a large CV database. [Journal of Informetrics](https://www.sciencedirect.com/science/article/pii/S1751157716301559)
- The researchers, the publications and the journals of Finance in Brazil: An analysis based on resumes from the Lattes platform. [Brazilian Review of Finance](https://periodicos.fgv.br/rbfin/article/view/47157)
- Análise do Perfil dos Acadêmicos e de suas Publicações Científicas em Administração (in Portuguese). [RAC](https://www.scielo.br/j/rac/a/BHg98Xqc3gQ3sPpXXP3hSzF/?lang=pt)

## Installation

The package is available in CRAN:

```r
install.packages('GetLattesData')
```

You can also install the development version from GitHub:

```r
# install.packages('devtools')
devtools::install_github('msperlin/GetLattesData')
```

`GetLattesData` requires R >= 4.1.0.

## Getting the data

Due to a captcha wall on the Lattes website, `GetLattesData` **no longer downloads CVs automatically**. To use the package you must:

1. Open the researcher's CV on [Lattes](https://lattes.cnpq.br/).
2. Click the **XML** button in the top-right corner of the page.
3. Solve the captcha and save the resulting `.zip` file locally.
4. Read it with the functions below.

## Quick start

Two example CVs are shipped with the package. Replace them with the files you downloaded.

### Read a single CV

`gld_read_zip2()` parses one Lattes `.zip` file and returns a list of tibbles:

```r
library(GetLattesData)

f_in <- system.file('extdata/3262699324398819.zip', package = 'GetLattesData')

my_l <- gld_read_zip2(f_in)
names(my_l)
#>  [1] "info"             "course_bachelors" "course_msc"       "course_phd"      
#>  [5] "pos_doc"          "published_papers" "accepted_papers"  "books"           
#>  [9] "supervisions"     "at_prof"          "projects"         "coauthors"       
#> [13] "conferences"      "awards"
```

### Read several CVs and add Qualis/SJR

`gld_get_lattes_data_from_zip()` reads a vector of `.zip` files, combines the results, and enriches the publications with Qualis and SJR:

```r
f_in <- system.file(
  c('extdata/3262699324398819.zip', 'extdata/8373564643000623.zip'),
  package = 'GetLattesData'
)

l_out <- gld_get_lattes_data_from_zip(
  zip.files    = f_in,
  field.qualis = 'ADMINISTRAÇÃO PÚBLICA E DE EMPRESAS, CIÊNCIAS CONTÁBEIS E TURISMO'
)

names(l_out)
l_out$tpesq                 # researchers
l_out$tpublic.published     # published papers (+ qualis, SJR, H.SJR)
l_out$tpublic.accepted      # accepted papers
```

`field.qualis` must match a Qualis area exactly (see the Qualis table from [CAPES/Sucupira](https://sucupira.capes.gov.br/sucupira/public/index.xhtml)). If you do not need Qualis, omit the argument.

## Output

`gld_read_zip2()` returns a list with the following elements:

| Element | Contents |
| --- | --- |
| `info` | researcher general information |
| `course_bachelors`, `course_msc`, `course_phd` | academic degrees |
| `pos_doc` | post-doctoral training |
| `published_papers`, `accepted_papers` | journal articles |
| `books` | published/organized books |
| `supervisions` | completed supervisions (MSc, PhD, and others) |
| `at_prof` | professional activities / employment |
| `projects` | research and extension projects |
| `coauthors` | co-authors of published papers |
| `conferences` | conference papers |
| `awards` | awards and titles |

Every table includes the researcher's `nome_completo` and the source `id_file`, so results from several CVs can be safely combined.

The older `gld_get_lattes_data_from_zip()` returns the same information with different names (`tpesq`, `tpublic.published`, `tpublic.accepted`, `tsupervisions`, `tbooks`, `tconferences`, `t_atprof`, `tprojects`, `tcoauthors`) and adds `qualis`, `SJR` and `H.SJR` columns to the papers.

## Citation

If you use `GetLattesData` in academic work, please cite the package:

```r
citation('GetLattesData')
```

## License

GPL-2. See [LICENSE](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html).

Bug reports and feature requests are welcome at <https://github.com/msperlin/GetLattesData/issues>.
