BitVector
=========
BitVectors for OCaml with C backend using SIMD instructions for manipulation.

For bit-packing DNA characters and applying fitch and sankoff algorithms for
median and costs of optimal assignments of a phylogeny, this data-structure
could have wider applications in encryption, image manipulation and other areas
where SIMD instructions shine.


bv.h     - Defines width of set for SIMD instructions, value decomposition
           functions and the OCaml interface functions in bv.c
bv.c     - Chooses which header file to load (bv\_sse.h, ...) which tells the
           file what native functions to replace with SIMD instructions.
bv\_\*.c - Implements functions (see list below)
bv\_\*.h - defines what functions are defined in above file


SIMD Instruction Overloading Definitions -

BV\_HAS\_ALIGNLENGTH    - bv\_alignment\_length     - for allocation width
BV\_HAS\_DISTANCE       - bv\_distance              - distance between two vects
BV\_HAS\_ELTCOUNT       - bv\_eltcount              - number of set bits in elt
BV\_HAS\_COMPARE        - bv\_compare               - compare two vects {-1,0,1}
BV\_HAS\_UNION          - bv\_union                 - vector union
BV\_HAS\_INTER          - bv\_inter                 - vector intersection
BV\_HAS\_SATURATION     - bv\_saturation            - # of chars with set bit
BV\_HAS\_PSATURATION    - bv\_poly\_saturation      - # of chars with n set bits


Resources
=========
Some are difficult to find; here is the list that has helped me the most.

Intel Intrinsics Guide (SSE/AVX/MMX)-
    http://software.intel.com/en-us/articles/intel-intrinsics-guide/

Microsoft VS2010 Compiler Intrinsics -
    http://msdn.microsoft.com/en-us/library/26td21ds(v=vs.100).aspx

ARM NEON Intrinsics (in GCC) -
    http://gcc.gnu.org/onlinedocs/gcc/ARM-NEON-Intrinsics.html

AltiVec Instruction Cross-Reference (Apple) -
    https://developer.apple.com/hardwaredrivers/ve/instruction_crossref.html
