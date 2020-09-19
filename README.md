# FutUber

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo**: 06<br>
**Paradigma**: Funcional<br>

## Alunos
|Matrícula | Aluno |
| --          | --                             |
| 16/0124484  |  Heron Rodrigues Sousa         |
| 16/0031982  |  João Pedro Mota Jardim        |
| 15/0132590  |  João Vitor Ramos de Souza     |
| 16/0016428  |  Paulo Victor de Menezes Lopes |

## Sobre 
Trata-se de um programa que lê 2 arquivos (nós e arestas), montando um grafo para livre interação do usuário. Além disso, há uma opção de gerar um png do grafo atual. Os dois arquivos que populam o programa inicialmente foram retirados de um projeto anterior de alguns membros do grupo, representando o mapa de Vicente Pires. https://github.com/projeto-de-algoritmos/Final_PaulUber 

![Grafo de Vicente Pires](https://github.com/UnBParadigmas/2020.1_G6_Funcional_FutUber/blob/master/images/Grafo%20vicente.jpg)

## Screenshots

Listagem de cidades (primeira opção do menu):
![Lista de Cidades](https://github.com/UnBParadigmas/2020.1_G6_Funcional_FutUber/blob/master/images/lista_cidades.png)

Listagem do caminho percorrido e o custo em Djikstra (segunda opção do menu):
![Djikstra](https://github.com/UnBParadigmas/2020.1_G6_Funcional_FutUber/blob/master/images/djikstra.png)

## Instalação 
**Linguagens**: Haskell<br>
**Tecnologias**: GHCI, Graphviz <br>
As instruções para a instalação do Graphviz, tanto para Windows quanto para Linux, se encontram [aqui](https://graphviz.org/download/).

## Uso 

Abra o terminal na raíz do projeto já com a plataforma GHCI e Graphviz instalada e execute os seguintes comandos.

 ```
ghci

:l menu

menu
 ```

Siga as instruções do menu para interagir com o projeto.

## Vídeo
<iframe width="560" height="315" src="https://www.youtube.com/embed/kRwyy-bqGzE" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## Fontes
[Grafos em haskell](https://szakallas.eu/2016/10/15/finding-shortest-paths-in-graphs-in-haskell/) <br>
[Material de grafos](https://github.com/edsomjr/TEP/blob/master/Grafos/slides/SSP-2/SSP-2.pdf) <br>
[Função agrupar estradas](https://stackoverflow.com/questions/12398458/how-to-group-similar-items-in-a-list-using-haskell) <br>
