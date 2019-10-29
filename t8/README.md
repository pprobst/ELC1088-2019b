# Recipere:
* uma DSL de definição de receitas culinárias compacta.
    * onde "compacta" é um jeito carinhoso de dizer que sou preguiçoso.

### Dependências
* Python 3
* [textX](https://github.com/textX/textX/) - biblioteca/programa em Python que
  auxilia no desenvolvimento de DSLs pequenas; similar ao Xtext.

### Motivação:
- Facilidade. 
    * Por dias ponderei alguma ideia  de DSL que fosse "segura" de ser
  elaborada, ou seja, algo que não me fizesse desistir no meio do caminho por
  conta da complexidade a ser considerada. Receitas são simples de serem
  definidas e estamos acostumados a escrevê-las à mão; o que a DSL _Recipere_ faz
  é simplesmente dar uma definição formal (mas flexível) para a definição de
  receitas, permitindo, assim, a interpretação da mesma computacionalmente
  (neste caso, a partir da linguagem Python).

### Arquivos
- ```recipere.tx``` 
    - arquivo de gramática.
- ```example.recip```
    - exemplo de definição de receita.
- ```recipedot.py```
    - script que interpreta um arquivo de definição de receita e gera o código
      fonte da definição de um grafo escrito na linguagem DOT (GraphViz).

### Como gerar o grafo da receita?
Primeiramente, você deve criar um arquivo similar a ```example.recip```. Seguindo
o template do arquivo de exemplo, dificilmente encontrará problemas.

Atente-se às unidades de medida aceitas pela gramática!

* Unidades de medida de ingredientes:
```
"u" | "ml" | "l" | "g" | "mg" | "kg" | "oz" | "fl oz" | 
"lb" | "teaspoon" | "teaspoons" | "tablespoon" | "tablespoons" | 
"cup" | "cups" | "colher de chá" | "colheres de chá" | 
"colher de sopa" | "colheres de sopa" | "xícara" | "xícaras"
```

* Unidades de tempo:
``` 
"s" | "m" | "h"
```
Onde ```s```, ```m``` e ```h``` são, respectivamente, _segundos_, _minutos_ e _horas_.

Por fim, para gerar o grafo da receita, rode o seguinte comando:
```
$ python recipedot.py nome_do_arquivo.recip
```

O arquivo DOT gerado terá o nome ```nome_do_arquivo.recip.dot```. Este 
arquivo pode ser visualizado com algum programa de visualização de arquivos 
DOT, como o [xdot](https://github.com/jrfonseca/xdot.py).

