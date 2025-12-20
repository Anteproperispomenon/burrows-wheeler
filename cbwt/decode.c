#include <stdlib.h>
#include "../openbwt/openbwt.h"

/* Assumes the input data is already a manageable size. */
int undo_bwt ( const unsigned char *inputArray , unsigned char *outputArray, int sz, int pidx) {
    int *tempArray;


    tempArray = malloc(sz * sizeof(int));

    /* outputArray = malloc (4 + (sz * sizeof(unsigned char))); */

    /* UnBWT(const unsigned char *T, unsigned char *U, int *A, int n, int pidx); */

    int rslt = UnBWT(inputArray, outputArray, tempArray, sz, pidx);
    
    free(tempArray);

    if (rslt >= 0) {
      return (0);
    }

    else return rslt;


}
