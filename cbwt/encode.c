#include <stdlib.h>
#include "../openbwt/openbwt.h"

/* Assumes the input data is already a manageable size. */
int do_bwt ( const unsigned char *inputArray , unsigned char *outputArray, int sz) {
    int *tempArray;

    int pidx;

    tempArray = malloc(sz * sizeof(int));

    /* outputArray = malloc (4 + (sz * sizeof(unsigned char))); */

    int rslt = BWT(inputArray, (outputArray+4), tempArray, sz);

    free(tempArray);

    if (rslt >= 0) {
      outputArray[0] = (unsigned char)((rslt >>  0) & 0xff),
      outputArray[1] = (unsigned char)((rslt >>  8) & 0xff),
      outputArray[2] = (unsigned char)((rslt >> 16) & 0xff),
      outputArray[3] = (unsigned char)((rslt >> 24) & 0xff);

      return (0);
    }

    else return rslt;


}
