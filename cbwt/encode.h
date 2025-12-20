#ifndef _ENCODE_H
#define _ENCODE_H 1

int do_bwt ( const unsigned char *inputArray , unsigned char *outputArray, int sz) ;

int do_bwt_alt (const unsigned char *inputArray, unsigned char *outputArray, int *workArray, int sz) ;

#endif

