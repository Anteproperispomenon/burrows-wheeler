/*
 * bwt.c for the OpenBWT project
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
  int pidx, size, blocksize;
  unsigned char temp[4];

  /* Check arguments. */
  if(argc != 4) {
    fprintf(stderr, "usage: %s NUM INFILE OUTFILE\n", argv[0]);
    fprintf(stderr, "  NUM    set block size to NUM MiB [1..512]\n");
    exit(EXIT_FAILURE);
  }

  /* Set blocksize */
  blocksize = atoi(argv[1]);
  if(blocksize < 1) { blocksize = 1; }
  else if(512 < blocksize) { blocksize = 512; } /* max 512 MiB */
  blocksize <<= 20;

  /* Open files. */
  if((ifp = fopen(argv[2], "rb")) == NULL) { perror(argv[2]); exit(EXIT_FAILURE); }
  if((ofp = fopen(argv[3], "wb")) == NULL) { perror(argv[3]); exit(EXIT_FAILURE); }

  /* Write blocksize. */
  temp[0] = (unsigned char)((blocksize >>  0) & 0xff),
  temp[1] = (unsigned char)((blocksize >>  8) & 0xff),
  temp[2] = (unsigned char)((blocksize >> 16) & 0xff),
  temp[3] = (unsigned char)((blocksize >> 24) & 0xff);
  fwrite(&(temp[0]), sizeof(unsigned char), 4, ofp); /* sotre data in little-endian format. */

  /* Allocate memory. */
  T = malloc(blocksize * sizeof(unsigned char));
  A = malloc(blocksize * sizeof(int));
  if((T == NULL) || (A == NULL)) { perror(NULL); exit(EXIT_FAILURE); }

  while(0 < (size = fread(T, sizeof(unsigned char), blocksize, ifp))) {
    /* Burrows-Wheeler Transform. */
    pidx = BWT(T, T, A, size);
    if(pidx < 0) {
      if(pidx == -1) { fprintf(stderr, "Invalid arguments.\n"); }
      else { fprintf(stderr, "Cannot allocate memory.\n"); }
      exit(EXIT_FAILURE);
    }

    /* Write BWT data. */
    temp[0] = (unsigned char)((pidx >>  0) & 0xff),
    temp[1] = (unsigned char)((pidx >>  8) & 0xff),
    temp[2] = (unsigned char)((pidx >> 16) & 0xff),
    temp[3] = (unsigned char)((pidx >> 24) & 0xff);
    fwrite(&(temp[0]), sizeof(unsigned char), 4, ofp); /* sotre data in little-endian format. */
    fwrite(T, sizeof(unsigned char), size, ofp);
  }

  /* Deallocate memory. */
  free(T); free(A);

  /* Close files. */
  fclose(ifp);
  fclose(ofp);

  return 0;
}
