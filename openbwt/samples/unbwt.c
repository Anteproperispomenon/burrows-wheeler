/*
 * unbwt.c for the OpenBWT project
 * Copyright (c) 2008 zxcb All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include "openbwt.h"


int
main(int argc, const char *argv[]) {
  unsigned char *T;
  int *A;
  FILE *ifp, *ofp;
  int pidx, size, blocksize, err;
  unsigned char temp[4];

  /* Check arguments. */
  if(argc != 3) {
    fprintf(stderr, "usage: %s INFILE OUTFILE\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  /* Open files. */
  if((ifp = fopen(argv[1], "rb")) == NULL) { perror(argv[1]); exit(EXIT_FAILURE); }
  if((ofp = fopen(argv[2], "wb")) == NULL) { perror(argv[2]); exit(EXIT_FAILURE); }

  /* Read blocksize. */
  fread(&(temp[0]), sizeof(unsigned char), 4, ifp);
  blocksize = (temp[0] <<  0) | (temp[1] <<  8) | (temp[2] << 16) | (temp[3] << 24);

  /* Allocate memory. */
  T = malloc(blocksize * sizeof(unsigned char));
  A = malloc(blocksize * sizeof(int));
  if((T == NULL) || (A == NULL)) { perror(NULL); exit(EXIT_FAILURE); }

  while(fread(&(temp[0]), sizeof(unsigned char), 4, ifp) == 4) {
    pidx = (temp[0] <<  0) | (temp[1] <<  8) | (temp[2] << 16) | (temp[3] << 24);
    /* Read BWT data. */
    size = fread(T, sizeof(unsigned char), blocksize, ifp);

    /* Inverse Burrows-Wheeler Transform. */
    err = UnBWT(T, T, A, size, pidx);
    if(err < 0) {
      if(err == -1) { fprintf(stderr, "Invalid arguments.\n"); }
      else { fprintf(stderr, "Cannot allocate memory.\n"); }
      exit(EXIT_FAILURE);
    }

    /* Write data. */
    fwrite(T, sizeof(unsigned char), size, ofp);
  }

  /* Deallocate memory. */
  free(T); free(A);

  /* Close files */
  fclose(ifp);
  fclose(ofp);

  return 0;
}
