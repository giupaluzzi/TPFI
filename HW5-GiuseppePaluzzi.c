#include <stdio.h>
#include <stdlib.h>

// 1. Type Safety
int typeSafety(){
    unsigned int x = 1;
    return (*(char*)&x == 1);
    /* Nel tipo int, il byte meno significativo indica il segno, quindi considerando
    unsigned int (solo positivi), il byte meno significativo conterrà 1 nei little endian
    e 0 nei big endian 
    P.S. I Mac con Apple Silicon sono little endian */
}
/*
int main(){
    printf("%d", typeSafety());
    return typeSafety(); 
}
*/


// 2. Array Mutabili
typedef struct T {
    int value;
    int index;
} Tuple;

int compareV (const void *a, const void *b){
    Tuple *pairA = (Tuple *)a;
    Tuple *pairB = (Tuple *)b;
    return pairA->value - pairB->value;
}

int compareI (const void *a, const void *b){
    Tuple *pairA = (Tuple *)a;
    Tuple *pairB = (Tuple *)b;
    return pairA->index - pairB->index;
}

int rmvDups(int* list, int n){
    // Creo una lista di coppie (valore, indice)
    Tuple* pairs = (Tuple*)malloc((n+1)*sizeof(Tuple));
    for (int i = 0; i < n; i++){
        pairs[i].index = i;
        pairs[i].value = list[i];
    }
    // Ordino in base ai valori
    qsort(pairs, n, sizeof(Tuple), compareV);

    // Rimuovo i duplicati
    int l = 0;
    for (int i = 1; i<n+1; i++) {
        if (pairs[i].value != pairs[l].value){
            l++;
            pairs[l].value = pairs[i].value;
            pairs[l].index = pairs[i].index;
        }
        else if (pairs[i].index < pairs[l].index) {
            pairs[l].index = pairs[i].index;
        }
    }

    // Ordino in base agli indici, ripristinando l'ordine originale
    qsort(pairs, l, sizeof(Tuple), compareI);

    for (int i = 0; i < l; i++){
        list[i] = pairs[i].value;
    }
    free(pairs);
    return l;
}

/*
int main() {
    int newLength;
    int list[] = {2,5,3,2,1,3,5,6,8,9,5,7,4,2,1};
    int n = sizeof(list) / sizeof(list[0]);
    newLength = rmvDups(list, n);
    for (int i = 0; i<newLength; i++){
        printf("%d", list[i]);
    }
    return newLength;
}
*/




// 3. Dati Non-Funzionali
typedef struct C {
	struct C * left; // sottoalbero sx
	int n;
    int k;
    int res;
	struct C * right; // sottoalbero dx
} cbinNode;

typedef cbinNode* cbinTree;


// Funzione per creare un nodo
cbinTree mkNode (int n, int k){
    cbinTree node = (cbinTree)malloc(sizeof(cbinNode));
    node->n = n;
    node->k = k;
    node->left = NULL;
    node->right = NULL;
    node->res=0;
    return node;
}

// Funzione per creare l'albero delle chiamate ricorsive
cbinTree cBinInvocation(int n, int k){
    if (k<0){
        return NULL;
    }

    cbinTree T = mkNode(n,k);
    if (n==k || k==0) {
        T->res=1;
        return T;
    } 
    else {
        T->left = cBinInvocation(n-1,k-1);
        T->right = cBinInvocation(n-1,k);
        if (T->left != NULL && T->right != NULL) 
            T->res = (T->left)->res + (T->right)->res;
        else if (T->left != NULL) 
            T->res = (T->left)->res;
        else if (T->right != NULL)
            T->res = (T->right)->res;
    }
    return T;
}

#define MAX_N 1000
#define MAX_K 1000
cbinTree visited[MAX_N][MAX_K];

cbinTree cBinInvocationSharing(int n, int k){
    if (k<0){
        return NULL;
    }

    if (visited[n][k] != NULL) {
        return visited[n][k];
    }

    cbinTree T = mkNode(n,k);
    if (n==k || k==0) {
        T->res=1;
        return T;
    } 
    else {
        T->left = cBinInvocationSharing(n-1,k-1);
        T->right = cBinInvocationSharing(n-1,k);
        if (T->left != NULL && T->right != NULL) 
            T->res = (T->left)->res + (T->right)->res;
        else if (T->left != NULL) 
            T->res = (T->left)->res;
        else if (T->right != NULL)
            T->res = (T->right)->res;
    }
    visited[n][k]=T;
    return T;
}


void printTree(cbinTree T, int depth) {
    if (T == NULL) return;
    printTree(T->right,depth+1);

    for (int i = 0; i < depth; i++) {
        printf("   ");
    }
    printf("(%d, %d, %d)\n", T->n, T->k, T->res);

    printTree(T->left, depth+1);
}

/*
int main() {
    int n = 5;
    int k = 3;
    
    cbinTree tree = cBinInvocationSharing(n, k);
    printTree(tree, 0);

    printf("\n\n");
    for (int i = 0; i < MAX_N; i++) {
        for (int j = 0; j < MAX_K; j++) {
            visited[i][j] = NULL;
        }
    }
    cbinTree tree1 = cBinInvocationSharing(n, k);
    
    printTree(tree1, 0);
    return 0;
}
*/





// 4. Crivello di Eulero
typedef struct Pair {
    int succ;
    int prec;
} Pair;

void printPrimes(Pair *p, int n) {
    // Stampa i numeri primi
    int i=2;
    do{
        printf("%d ",i);
        i+=p[i].succ;
    } while (i<=n);
    printf("\n");
}

Pair *eulerSieve(int n) {
    // Allocazione del vettore di coppie (n+1 elementi)
    Pair *p = malloc((n + 1) * sizeof(Pair));

    // Inizializzazione del vettore
    for (int i = 2; i <= n; i++) {
        p[i].succ = 1;
        p[i].prec = 1;
    }

    // Cancellazione dei non-primi
    int i = 2;
    while (i<=n && p[i].succ != -1){
        for (int j=2*i; j<=n; j+=i){
            if (p[j].prec !=-1){
                p[j-p[j].prec].succ += p[j].succ;                
                p[j+p[j].succ].prec += p[j].prec;
                p[j].prec = -1;
                p[j].succ = -1;
            }
        }
        int a = i;
        while (p[a].succ == -1) 
            a+=1;
        i+=p[a].succ;

    }
    return p;
}
/*
int main() {
  int n = 1000;

  Pair *p = eulerSieve(n);
  printPrimes(p, n);

  free(p);

  return 0;
}
*/