BitVector
=========
BitVectors for OCaml with C backend using SIMD instructions for manipulation.

For bit-packing DNA characters and applying fitch and sankoff algorithms for
median and costs of optimal assignments of a phylogeny, this data-structure
could have wider applications in encryption, image manipulation and other areas
where SIMD instructions shine.

SIMD Instruction Overloading Definitions -

    BV_HAS_MALLOC       - bv_malloc           - how to allocate memory
    BV_HAS_ALIGNLENGTH  - bv_alignment_length - how much data to allocate
    BV_HAS_DISTANCE     - bv_distance         - distance between two vects
    BV_HAS_ELTCOUNT     - bv_eltcount         - number of set bits in elt
    BV_HAS_COMPARE      - bv_compare          - compare two vects {-1,0,1}
    BV_HAS_UNION        - bv_union            - vector union
    BV_HAS_INTER        - bv_inter            - vector intersection
    BV_HAS_SATURATION   - bv_saturation       - # of chars with set bit
    BV_HAS_PSATURATION  - bv_poly_saturation  - # of chars with n set bits


Resources
=========

Intel Intrinsics Guide (SSE/AVX/MMX)-
    http://software.intel.com/en-us/articles/intel-intrinsics-guide/

Microsoft VS2010 Compiler Intrinsics -
    http://msdn.microsoft.com/en-us/library/26td21ds(v=vs.100).aspx

ARM NEON Intrinsics (in GCC) -
    http://gcc.gnu.org/onlinedocs/gcc/ARM-NEON-Intrinsics.html

AltiVec Instruction Cross-Reference (Apple) -
    https://developer.apple.com/hardwaredrivers/ve/instruction_crossref.html
