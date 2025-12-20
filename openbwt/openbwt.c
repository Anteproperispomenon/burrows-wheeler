/*
 * openbwt.c for the OpenBWT project
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

#include <stdlib.h>
#include "openbwt.h"


#define chr(i) (cs == sizeof(int) ? ((const int *)T)[i]:((const unsigned char *)T)[i])

static
void
getCounts(const unsigned char *T, int *C, int n, int k, int cs) {
  int i;
  for(i = 0; i < k; ++i) { C[i] = 0; }
  for(i = 0; i < n; ++i) { ++C[chr(i)]; }
}
static
void
getBuckets(const int *C, int *B, int k, int end) {
  int i, sum = 0;
  if(end) { for(i = 0; i < k; ++i) { sum += C[i]; B[i] = sum; } }
  else { for(i = 0; i < k; ++i) { sum += C[i]; B[i] = sum - C[i]; } }
}

static
void
induceSA(const unsigned char *T, int *SA, int *C, int *B, int n, int k, int cs) {
  int *b, i, j;
  int c0, c1;
  if(C == B) { getCounts(T, C, n, k, cs); }
  getBuckets(C, B, k, 0);
  j = n - 1;
  b = SA + B[c1 = chr(j)];
  *b++ = ((0 < j) && (chr(j - 1) < c1)) ? ~j : j;
  for(i = 0; i < n; ++i) {
    j = SA[i], SA[i] = ~j;
    if(0 < j) {
      --j;
      if((c0 = chr(j)) != c1) { B[c1] = b - SA; b = SA + B[c1 = c0]; }
      *b++ = ((0 < j) && (chr(j - 1) < c1)) ? ~j : j;
    }
  }
  if(C == B) { getCounts(T, C, n, k, cs); }
  getBuckets(C, B, k, 1);
  for(i = n - 1, b = SA + B[c1 = 0]; 0 <= i; --i) {
    if(0 < (j = SA[i])) {
      --j;
      if((c0 = chr(j)) != c1) { B[c1] = b - SA; b = SA + B[c1 = c0]; }
      *--b = ((j == 0) || (chr(j - 1) > c1)) ? ~j : j;
    } else {
      SA[i] = ~j;
    }
  }
}
static
int
computeBWT(const unsigned char *T, int *SA, int *C, int *B, int n, int k, int cs) {
  int *b, i, j, pidx = -1;
  int c0, c1;
  if(C == B) { getCounts(T, C, n, k, cs); }
  getBuckets(C, B, k, 0);
  j = n - 1;
  b = SA + B[c1 = chr(j)];
  *b++ = ((0 < j) && (chr(j - 1) < c1)) ? ~j : j;
  for(i = 0; i < n; ++i) {
    if(0 < (j = SA[i])) {
      --j;
      c0 = chr(j);
      SA[i] = ~c0;
      if(c0 != c1) { B[c1] = b - SA; b = SA + B[c1 = c0]; }
      *b++ = ((0 < j) && (chr(j - 1) < c1)) ? ~j : j;
    } else if(j != 0) {
      SA[i] = ~j;
    }
  }
  if(C == B) { getCounts(T, C, n, k, cs); }
  getBuckets(C, B, k, 1);
  for(i = n - 1, b = SA + B[c1 = 0]; 0 <= i; --i) {
    if(0 < (j = SA[i])) {
      --j;
      c0 = chr(j);
      SA[i] = c0;
      if(c0 != c1) { B[c1] = b - SA; b = SA + B[c1 = c0]; }
      *--b = ((0 < j) && (chr(j - 1) > c1)) ? ~((int)chr(j - 1)) : j;
    } else if(j != 0) {
      SA[i] = ~j;
    } else {
      pidx = i;
    }
  }
  return pidx;
}

static
int
suffixsort(const unsigned char *T, int *SA, int fs, int n, int k, int cs, int depth) {
  int *C, *B, *RA;
  int i, j, c, m, p, q, plen, qlen, name;
  int c0, c1;
  int diff;
  int pidx = 0;

  if(k <= fs) { C = SA + n; B = (k <= (fs - k)) ? C + k : C; }
  else if((C = B = (int *)malloc(k * sizeof(int))) == NULL) { return -2; }
  getCounts(T, C, n, k, cs); getBuckets(C, B, k, 1);
  for(i = 0; i < n; ++i) { SA[i] = 0; }
  for(i = n - 2, c = 0, c1 = chr(n - 1); 0 <= i; --i, c1 = c0) {
    if((c0 = chr(i)) < (c1 + c)) { c = 1; }
    else if(c != 0) { SA[--B[c1]] = i + 1, c = 0; }
  }
  induceSA(T, SA, C, B, n, k, cs);
  if(fs < k) { free(C); }

  for(i = 0, m = 0; i < n; ++i) {
    p = SA[i];
    if((0 < p) && (chr(p - 1) > (c0 = chr(p)))) {
      for(j = p + 1; (j < n) && (c0 == (c1 = chr(j))); ++j) { }
      if((j < n) && (c0 < c1)) { SA[m++] = p; }
    }
  }
  for(i = m; i < n; ++i) { SA[i] = 0; }
  for(i = n - 2, j = n, c = 0, c1 = chr(n - 1); 0 <= i; --i, c1 = c0) {
    if((c0 = chr(i)) < (c1 + c)) { c = 1; }
    else if(c != 0) { SA[m + ((i + 1) >> 1)] = j - i - 1; j = i + 1; c = 0; }
  }
  for(i = 0, name = 0, q = n, qlen = 0; i < m; ++i) {
    p = SA[i], plen = SA[m + (p >> 1)], diff = 1;
    if(plen == qlen) {
      for(j = 0; (j < plen) && (chr(p + j) == chr(q + j)); j++) { }
      if(j == plen) { diff = 0; }
    }
    if(diff != 0) { ++name, q = p, qlen = plen; }
    SA[m + (p >> 1)] = name;
  }

  if(name < m) {
    RA = SA + n + fs - m;
    for(i = n - 1, j = m - 1; m <= i; --i) {
      if(SA[i] != 0) { RA[j--] = SA[i] - 1; }
    }
    if(suffixsort((unsigned char *)RA, SA, fs + n - m * 2, m, name, sizeof(int), depth + 1) != 0) { return -2; }
    for(i = n - 2, j = m - 1, c = 0, c1 = chr(n - 1); 0 <= i; --i, c1 = c0) {
      if((c0 = chr(i)) < (c1 + c)) { c = 1; }
      else if(c != 0) { RA[j--] = i + 1, c = 0; }
    }
    for(i = 0; i < m; ++i) { SA[i] = RA[SA[i]]; }
  }

  if(k <= fs) { C = SA + n; B = (k <= (fs - k)) ? C + k : C; }
  else if((C = B = (int *)malloc(k * sizeof(int))) == NULL) { return -2; }
  getCounts(T, C, n, k, cs); getBuckets(C, B, k, 1);
  for(i = m; i < n; ++i) { SA[i] = 0; }
  for(i = m - 1; 0 <= i; --i) {
    j = SA[i], SA[i] = 0;
    SA[--B[chr(j)]] = j;
  }
  if(depth != 0) { induceSA(T, SA, C, B, n, k, cs); }
  else { pidx = computeBWT(T, SA, C, B, n, k, cs); }
  if(fs < k) { free(C); }

  return pidx;
}

int
BWT(const unsigned char *T, unsigned char *U, int *A, int n) {
  int i, pidx;
  if((T == NULL) || (U == NULL) || (A == NULL) || (n < 0)) { return -1; }
  if(n <= 1) { if(n == 1) { U[0] = T[0]; } return n; }

  pidx = suffixsort(T, A, 0, n, 256, sizeof(unsigned char), 0);
  if(pidx < 0) { return pidx; }
  U[0] = T[n - 1];
  for(i = 0; i < pidx; ++i) { U[i + 1] = (unsigned char)A[i]; }
  for(i += 1; i < n; ++i) { U[i] = (unsigned char)A[i]; }
  pidx += 1;
  return pidx;
}

int
UnBWT(const unsigned char *T, unsigned char *U, int *A, int n, int pidx) {
  unsigned int C[256];
  unsigned char D[256];
  int i, p;
  int c, d, len, half;

  if((T == NULL) || (A == NULL) ||
     (n < 0) || (pidx < 0) || (n < pidx) ||
     ((0 < n) && (pidx == 0))) { return -1; }
  if(n <= 1) { if(n == 1) { U[0] = T[0]; } return 0; }

  if(n <= (1 << 24)) {
    /* mergedTLF, O(n) time, 5n space, n <= 2^24 */
    unsigned int *B = (unsigned int *)A;
    for(c = 0; c < 256; ++c) { C[c] = 0; }
    for(i = 0; i < n; ++i) { B[i] = (C[T[i]]++ << 8) | T[i]; }
    for(c = 0, i = 0; c < 256; ++c) {
      p = C[c];
      C[c] = i;
      i += p;
    }
    for(i = n - 1, p = 0; 0 <= i; --i) {
      p = (B[p] >> 8) + C[U[i] = B[p] & 0xff];
      if(p < pidx) { ++p; }
    }
  } else if(T != U) {
    /* LF, O(n) time, 6n space */
    for(c = 0; c < 256; ++c) { C[c] = 0; }
    for(i = 0; i < n; ++i) { A[i] = C[T[i]]++; }
    for(c = 0, i = 0; c < 256; ++c) {
      p = C[c];
      C[c] = i;
      i += p;
    }
    for(i = n - 1, p = 0; 0 <= i; --i) {
      p = A[p] + C[U[i] = T[p]];
      if(p < pidx) { ++p; }
    }
  } else {
    /* PSI, O(n lg k) time, 5n space */
    for(c = 0; c < 256; ++c) { C[c] = 0; }
    for(i = 0; i < n; ++i) { ++C[T[i]]; }
    for(c = 0, d = 0, i = 0; c < 256; ++c) {
      p = C[c];
      if(0 < p) {
        C[c] = i;
        D[d++] = (unsigned char)c;
        i += p;
      }
    }
    for(i = 0; i < pidx; ++i) { A[C[T[i]]++] = i; }
    for(; i < n; ++i)         { A[C[T[i]]++] = i + 1; }
    for(c = 0; c < d; ++c) { C[c] = C[D[c]]; }
    for(i = 0, p = pidx; i < n; ++i) {
      for(c = 0, len = d, half = len >> 1;
          0 < len;
          len = half, half >>= 1) {
        if(C[c + half] < (unsigned int)p) {
          c += half + 1;
          half -= (len & 1) ^ 1;
        }
      }
      U[i] = D[c];
      p = A[p - 1];
    }
  }
  return 0;
}
