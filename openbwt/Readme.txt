OpenBWT - v1.0

== Introduction ==
This project is an open source library for the Burrows-Wheeler Transformation.
The BWT runs in O(n) worst-case time and 2n + O(1) worst-case extra space,
where n is the length of the input string.

== License ==
OpenBWT is licensed under the MIT license. See the file License.txt for details.

== C Interfaces ==

/*
  Burrows-Wheeler Transform
  @param T[0..n-1] The input string.
  @param U[0..n-1] The output BWTed-string.
  @param A[0..n-1] The temporary array.
  @param n The length of the string.
  @return The primary index if no error occurred, -1 or -2 otherwise.
*/
int
BWT(const unsigned char *T, unsigned char *U, int *A, int n);

/*
  Inverse Burrows-Wheeler Transform
  @param T[0..n-1] The input BWTed-string.
  @param U[0..n-1] The output string.
  @param A[0..n-1] The temporary array.
  @param n The length of the string.
  @param pidx The primary index.
  @return 0 if no error occurred, -1 or -2 otherwise.
*/
int
UnBWT(const unsigned char *T, unsigned char *U, int *A, int n, int pidx);
