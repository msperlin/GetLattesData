## ------------------------------------------------------------------------
library(GetLattesData)
library(ggplot2)

# new PPG (PO-FIN)
my.ids <- c('K4713546D3', 'K4796143H6', 'K4123414D0', 'K4735956T7',
            'K4781462D3', 'K4765099U6', 'K4440252H7', 'K4783858A0',
            'K4769290Y6', 'K4723925J2', 'K4763474Y5', 'K4466733D0', 
            'K4768258E6')

area.qualis = 'ADMINISTRAÇÃO PÚBLICA E DE EMPRESAS, CIÊNCIAS CONTÁBEIS E TURISMO'

l.out <- gld_get_lattes_data(id.vec = my.ids, area.qualis = area.qualis )

tpesq <- l.out$tpesq
tpublic <- l.out$tpublic

p <- ggplot(tpublic, aes(x = qualis)) +
  geom_bar(position = 'identity') + facet_wrap(~NOME.COMPLETO) +
  labs(x = paste0('Qualis: ',area.qualis))
print(p)


