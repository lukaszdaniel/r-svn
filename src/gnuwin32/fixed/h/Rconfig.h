/* Rconfig.h.  Generated automatically */
#ifndef R_RCONFIG_H
#define R_RCONFIG_H

#ifndef R_CONFIG_H

#define HAVE_F77_UNDERSCORE 1
/* all R platforms have this */
#define IEEE_754 1
/* #undef WORDS_BIGENDIAN */
#define R_INLINE inline
/* #undef HAVE_VISIBILITY_ATTRIBUTE */
/* all R platforms have the next two */
#define SUPPORT_UTF8 1
#define SUPPORT_MBCS 1
#define ENABLE_NLS 1
/* #undef HAVE_AQUA */
/* Enable the use of Fortran character lengths,
   e.g. in BLAS.h and Lapack.h */
#ifndef DONT_USE_FC_LEN_T
#define FC_LEN_T size_t
#endif

#define PR18534fixed 1

/* NB: the rest are for the C compiler used to build R:
   they do not necessarily apply to a C++ compiler */
#define SIZEOF_SIZE_T 8
/* #undef HAVE_ALLOCA_H */
/* optional C99 type */
#define HAVE_UINTPTR_T 1
/* C23 (or clang) extension */
#define HAVE_ENUM_BASE_TYPE 1

#endif /* not R_CONFIG_H */

#endif /* not R_RCONFIG_H */
