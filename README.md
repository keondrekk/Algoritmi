# Algoritmi

Vengono definiti come algoritmi una serie di istruzioni ordinate e ben definite che servono a risolvere il problema. Ne esistono di diversi tipi e diverse tipologie:
Andando a studiare gli algoritmo di ordinamento, ne esistono due definiti come bubblesort e quicksort.

## Bubble sort
 Data una lista o un vettore di elementi, ogni volta che due elementi adiacenti sono in ordine sbagliato vengono invertiti. Le iterazioni dell'algoritmo finiscono nel momento in cui il vettore o la lista sono ordinati.

## Quick sort 

Definito come ordinamento rapido, utilizza un elemento casuale di un vettore definito come pivot. Tale elemento farà da spartiacque fra due vettori: quello di sinistra, con elementi dal valore minore dell'elemento pivot, viceversa il vettore avrà valori maggiori. Si confrontano i singoli elementi dei due vettori per vedere se sono effettivamente nella parte giusta. L'iterazione finisce nel momento in cui non si fanno più passaggi da un vettore all'altro.

cont_quick_sort <- 0
quick_sort_eff <- function(v){
cont_quick_sort <<- cont_quick_sort + 1
if (length(v) <= 1){
return (v)
}
pivot <- v[1]
cont_quick_sort <<- cont_quick_sort + 1
sx <- v[v < pivot]
cont_quick_sort <<- cont_quick_sort + length(v)
dx <- v[v > pivot]
cont_quick_sort <<- cont_quick_sort + length(v)
minori <- quick_sort(sx)
maggiori <- quick_sort(dx)
risultato <- c(minori, pivot, maggiori)
cont_quick_sort <<- cont + 1
return (risultato)
}

bubble_sort_eff <- function(v){
conta_op <- 0
scansiona_ancora = TRUE
conta_op <- conta_op + 1 # assegna scansiona_ancora
while(scansiona_ancora == TRUE){
conta_op <- conta_op + 1 # condizione del while
scansiona_ancora = FALSE
conta_op <- conta_op + 1 # assegn scansiona_ancora
for (i in 1:(length(v)-1)){
conta_op <- conta_op + 1
if(v[i] > v[i+1]){
conta_op <- conta_op + 1 # controllo della if
variabile_appoggio <- v[i]
conta_op <- conta_op + 1 # assegn variabile appoggio
v[i] <- v[i+1]
conta_op <- conta_op + 1 # primo scambio
v[i+1] <- variabile_appoggio
conta_op <- conta_op + 1 # secondo scambio
scansiona_ancora = TRUE
conta_op <- conta_op + 1 # assegn scansiona_ancora
}}}
print(paste("Totale operazioni:", conta_op))
return (v)
}
