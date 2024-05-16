## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(eval=TRUE)

## -----------------------------------------------------------------------------
library(GetLattesData)

# get files from pkg (you can download from other researchers in lattes website)
f.in <- c(system.file('extdata/3262699324398819.zip', package = 'GetLattesData'),
          system.file('extdata/8373564643000623.zip', package = 'GetLattesData'))

# set qualis
field.qualis = 'ADMINISTRAÇÃO PÚBLICA E DE EMPRESAS, CIÊNCIAS CONTÁBEIS E TURISMO'

# get data
l.out <- gld_get_lattes_data_from_zip(f.in,
                                      field.qualis = field.qualis )

f <- "~/Desktop/6915544029415506.zip"
l_out <- gld_read_zip2(f)

## -----------------------------------------------------------------------------
dplyr::glimpse(l_out$conferences)

## -----------------------------------------------------------------------------
tpesq <- l.out$tpesq
str(l.out$tcoauthors)

a = l.out$tcoauthors

## -----------------------------------------------------------------------------
dplyr::glimpse(l.out$tpublic.published)

## -----------------------------------------------------------------------------
tpublic.published <- l.out$tpublic.published

library(ggplot2)

p <- ggplot(tpublic.published, aes(x = qualis)) +
  geom_bar(position = 'identity') + facet_wrap(~name) +
  labs(x = paste0('Qualis: ', field.qualis))
print(p)

## -----------------------------------------------------------------------------
library(dplyr)

my.tab <- tpublic.published %>%
  group_by(name) %>%
  summarise(n.papers = n(),
            max.SJR = max(SJR, na.rm = T),
            mean.SJR = mean(SJR, na.rm = T),
            n.A1.qualis = sum(qualis == 'A1', na.rm = T),
            n.A2.qualis = sum(qualis == 'A2', na.rm = T),
            median.authorship = median(as.numeric(order.aut), na.rm = T ))

knitr::kable(my.tab)

tprojects <- l.out$tprojects
dplyr::glimpse(tprojects)
