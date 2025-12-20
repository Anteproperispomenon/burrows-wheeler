#ifndef _DECODE_H
#define _DECODE_H 1

int undo_bwt ( const unsigned char *inputArray , unsigned char *outputArray, int sz, int pidx) ;

int undo_bwt_alt (const unsigned char *inputArray, unsigned char *outputArray, int *workArray, int sz, int pidx) ;

#endif
