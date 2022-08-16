# Contextualizing RBMFC in the Brazilian strict sense postgraduation

Copyright © 2022 Leonardo Ferreira Fontenelle <leonardof@leonardof.med.br>.

Distributed under the GNU General Public License v3.0; see LICENSE.

This is the analytic code for the equally named manuscript, to be 
submitted to [ABEC Meeting 2022](https://meeting22.abecbrasil.org.br/). 
It downloads open data from CAPES and tabulates which postgraduate 
programs (from which evaluation areas) published how many articles in 
which journals during years 2017-2020.

Some background: Coordenação de Aperfeiçoamento de Pessoal de Nível 
Superior (CAPES) is the governmental organization responsible for 
coordinating "strict sense" postgraduate programs, that is, those 
offering masters' and PhD courses. CAPES groups postgraduate programs 
in "evaluation areas" for the purpose of evaluating, regulating and 
funding the postgraduate programs. A major part of the evaluation 
consists in assessing the postgraduate programs' scholarly output. In 
turn, ranking the journals withing the evaluation areas is a major part 
of assessing the scholarly output.

Running the analytic code without changes will create, in the `data` 
directory, two CSV files for each of two journals. The journal 
motivating the study is _Revista Brasileira de Medicina de Família e 
Comunidade_ (RBMFC for short), ISSN 2179-7994. The comparison journal is 
_Revista de APS_, ISSN 1809-8363.

If you want to run a similar analysis for another journal(s), take hold 
of all its ISSNs and edit these lines of code:

```{r}
# Rev Bras Med Fam Comunidade
data$output[grepl("^\\((2179-7994|1809-5909)\\)", DS_ISSN), 
            ID_VALOR_LISTA := first(ID_VALOR_LISTA)]
# Rev APS
data$output[grepl("^\\((1809-8363|1516-7704)\\)", DS_ISSN), 
            ID_VALOR_LISTA := first(ID_VALOR_LISTA)]
```

and 

```{r}
# 1. Revista Brasileira de Medicina de Família e Comunidade
# 2. Revista de APS
focal_journals <- journals[
  ISSN_1 %in% c("2179-7994", "1809-5909", "1809-8363", "1516-7704"), 
  unique(ID_VALOR_LISTA)
]
```
