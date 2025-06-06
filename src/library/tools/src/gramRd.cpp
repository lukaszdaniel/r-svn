/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2025  The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>
#include <CXXR/RContext.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <Defn.h>
#include <Parse.h>
#include <R_ext/RS.h>           /* for R_chk_* allocation */
#include <Rmath.h> /* for imax2(.),..*/
#include "localization.h"

using namespace R;
using namespace CXXR;

/* bison creates a non-static symbol yylloc (and other) in both gramLatex.o
   and gramRd.o, so remap */   

#define yylloc yyllocR
#undef yynerrs /* from Defn.h */
#define yynerrs yynerrsR
#undef yychar /* from Defn.h */
#define yychar yycharR
#undef yylval /* from Defn.h */
#define yylval yylvalR

#define DEBUGVALS 0		/* 1 causes detailed internal state output to R console */	
#define DEBUGMODE 0		/* 1 causes Bison output of parse state, to stdout or stderr */

static bool wCalls = true;
static bool warnDups = false;

#define YYERROR_VERBOSE 1

static void yyerror(const char *);
static int yylex(void);
static int yyparse(void);

#define yyconst const

typedef struct yyltype
{
  int first_line;
  int first_column;
  int first_byte;

  int last_line;
  int last_column;
  int last_byte;
} yyltype;

# define YYLTYPE yyltype
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
	if (N)								\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).first_byte   = YYRHSLOC (Rhs, 1).first_byte;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	  (Current).last_byte    = YYRHSLOC (Rhs, N).last_byte;		\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	  (Current).first_byte   = (Current).last_byte =		\
	    YYRHSLOC (Rhs, 0).last_byte;				\
	}								\
    while (0)

/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

/* Functions used in the parsing process */

static void	GrowList(SEXP, SEXP);
static int	KeywordLookup(const char *);
static SEXP	UserMacroLookup(const char *);
static SEXP	InstallKeywords(void);
static SEXP	NewList(void);
static SEXP     makeSrcref(YYLTYPE *, SEXP);
static int	xxgetc(void);
static int	xxungetc(int);

/* Flags used to mark need for postprocessing in the dynamicFlag attribute */

#define STATIC 0
#define HAS_IFDEF 1
#define HAS_SEXPR 2

/* Internal lexer / parser state variables */

static char const yyunknown[] = "unknown macro"; /* our message, not bison's */


typedef struct ParseState ParseState;
struct ParseState {
    int xxinRString, xxQuoteLine, xxQuoteCol;
    int	xxinEqn;
    int	xxNewlineInString;
    int	xxlineno, xxbyteno, xxcolno;
    int	xxmode, xxitemType, xxbraceDepth;  /* context for lexer */
    int	xxDebugTokens;  /* non-zero causes debug output to R console */
    const char* xxBasename;     /* basename of file for error messages */
    SEXP	Value;
    int	xxinitvalue;
    SEXP	xxMacroList;/* A hashed environment containing all the standard and user-defined macro names */
    SEXP mset; /* Precious mset for protecting parser semantic values */
    ParseState *prevState;
};

static bool busy = false;
static ParseState parseState;

#define PRESERVE_SV(x) R_PreserveInMSet((x), parseState.mset)
#define RELEASE_SV(x)  R_ReleaseFromMSet((x), parseState.mset)

#define RLIKE 1		/* Includes R strings; xxinRString holds the opening quote char, or 0 outside a string */
#define LATEXLIKE 2
#define VERBATIM 3
#define INOPTION 4
#define COMMENTMODE 5   /* only used in deparsing */
#define UNKNOWNMODE 6   /* ditto */

static SEXP     SrcFile;  /* parse_Rd will *always* supply a srcfile */

/* Routines used to build the parse tree */

static SEXP	xxpushMode(int, int, int);
static void	xxpopMode(SEXP);
static SEXP	xxnewlist(SEXP);
static SEXP	xxnewlist2(SEXP, SEXP);
static SEXP	xxnewlist3(SEXP, SEXP, SEXP);
static SEXP	xxnewlist4(SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist5(SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist7(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist8(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist9(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static SEXP	xxlist(SEXP, SEXP);
static SEXP	xxmarkup(SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxmarkup2(SEXP, SEXP, SEXP, int, int, YYLTYPE *);
static SEXP	xxmarkup3(SEXP, SEXP, SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxOptionmarkup(SEXP, SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxtag(SEXP, int, YYLTYPE *);
static void	xxsavevalue(SEXP, YYLTYPE *);
static void	xxWarnNewline(void);
static SEXP	xxnewcommand(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxusermacro(SEXP, SEXP, YYLTYPE *);
static int	mkMarkup(int);
static int      mkIfdef(int);
static int	mkCode(int);
static int	mkText(int);
static int	mkVerb(int);
static int 	mkComment(int);

static SEXP R_RdTagSymbol = NULL;
static SEXP R_RdOptionSymbol = NULL;
static SEXP R_DefinitionSymbol = NULL;
static SEXP R_DynamicFlagSymbol = NULL;
static SEXP R_MacroSymbol = NULL;

#define YYSTYPE		SEXP



# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    END_OF_INPUT = 258,            /* END_OF_INPUT  */
    ERROR = 259,                   /* ERROR  */
    SECTIONHEADER = 260,           /* SECTIONHEADER  */
    RSECTIONHEADER = 261,          /* RSECTIONHEADER  */
    VSECTIONHEADER = 262,          /* VSECTIONHEADER  */
    SECTIONHEADER2 = 263,          /* SECTIONHEADER2  */
    RCODEMACRO = 264,              /* RCODEMACRO  */
    SEXPR = 265,                   /* SEXPR  */
    RDOPTS = 266,                  /* RDOPTS  */
    LATEXMACRO = 267,              /* LATEXMACRO  */
    VERBMACRO = 268,               /* VERBMACRO  */
    OPTMACRO = 269,                /* OPTMACRO  */
    ESCAPE = 270,                  /* ESCAPE  */
    LISTSECTION = 271,             /* LISTSECTION  */
    ITEMIZE = 272,                 /* ITEMIZE  */
    DESCRIPTION = 273,             /* DESCRIPTION  */
    NOITEM = 274,                  /* NOITEM  */
    LATEXMACRO2 = 275,             /* LATEXMACRO2  */
    VERBMACRO2 = 276,              /* VERBMACRO2  */
    VERBLATEX = 277,               /* VERBLATEX  */
    LATEXMACRO3 = 278,             /* LATEXMACRO3  */
    NEWCOMMAND = 279,              /* NEWCOMMAND  */
    USERMACRO = 280,               /* USERMACRO  */
    USERMACRO1 = 281,              /* USERMACRO1  */
    USERMACRO2 = 282,              /* USERMACRO2  */
    USERMACRO3 = 283,              /* USERMACRO3  */
    USERMACRO4 = 284,              /* USERMACRO4  */
    USERMACRO5 = 285,              /* USERMACRO5  */
    USERMACRO6 = 286,              /* USERMACRO6  */
    USERMACRO7 = 287,              /* USERMACRO7  */
    USERMACRO8 = 288,              /* USERMACRO8  */
    USERMACRO9 = 289,              /* USERMACRO9  */
    IFDEF = 290,                   /* IFDEF  */
    ENDIF = 291,                   /* ENDIF  */
    TEXT = 292,                    /* TEXT  */
    RCODE = 293,                   /* RCODE  */
    VERB = 294,                    /* VERB  */
    COMMENT = 295,                 /* COMMENT  */
    UNKNOWN = 296,                 /* UNKNOWN  */
    STARTFILE = 297,               /* STARTFILE  */
    STARTFRAGMENT = 298            /* STARTFRAGMENT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define END_OF_INPUT 258
#define ERROR 259
#define SECTIONHEADER 260
#define RSECTIONHEADER 261
#define VSECTIONHEADER 262
#define SECTIONHEADER2 263
#define RCODEMACRO 264
#define SEXPR 265
#define RDOPTS 266
#define LATEXMACRO 267
#define VERBMACRO 268
#define OPTMACRO 269
#define ESCAPE 270
#define LISTSECTION 271
#define ITEMIZE 272
#define DESCRIPTION 273
#define NOITEM 274
#define LATEXMACRO2 275
#define VERBMACRO2 276
#define VERBLATEX 277
#define LATEXMACRO3 278
#define NEWCOMMAND 279
#define USERMACRO 280
#define USERMACRO1 281
#define USERMACRO2 282
#define USERMACRO3 283
#define USERMACRO4 284
#define USERMACRO5 285
#define USERMACRO6 286
#define USERMACRO7 287
#define USERMACRO8 288
#define USERMACRO9 289
#define IFDEF 290
#define ENDIF 291
#define TEXT 292
#define RCODE 293
#define VERB 294
#define COMMENT 295
#define UNKNOWN 296
#define STARTFILE 297
#define STARTFRAGMENT 298

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;

int yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_END_OF_INPUT = 3,               /* END_OF_INPUT  */
  YYSYMBOL_ERROR = 4,                      /* ERROR  */
  YYSYMBOL_SECTIONHEADER = 5,              /* SECTIONHEADER  */
  YYSYMBOL_RSECTIONHEADER = 6,             /* RSECTIONHEADER  */
  YYSYMBOL_VSECTIONHEADER = 7,             /* VSECTIONHEADER  */
  YYSYMBOL_SECTIONHEADER2 = 8,             /* SECTIONHEADER2  */
  YYSYMBOL_RCODEMACRO = 9,                 /* RCODEMACRO  */
  YYSYMBOL_SEXPR = 10,                     /* SEXPR  */
  YYSYMBOL_RDOPTS = 11,                    /* RDOPTS  */
  YYSYMBOL_LATEXMACRO = 12,                /* LATEXMACRO  */
  YYSYMBOL_VERBMACRO = 13,                 /* VERBMACRO  */
  YYSYMBOL_OPTMACRO = 14,                  /* OPTMACRO  */
  YYSYMBOL_ESCAPE = 15,                    /* ESCAPE  */
  YYSYMBOL_LISTSECTION = 16,               /* LISTSECTION  */
  YYSYMBOL_ITEMIZE = 17,                   /* ITEMIZE  */
  YYSYMBOL_DESCRIPTION = 18,               /* DESCRIPTION  */
  YYSYMBOL_NOITEM = 19,                    /* NOITEM  */
  YYSYMBOL_LATEXMACRO2 = 20,               /* LATEXMACRO2  */
  YYSYMBOL_VERBMACRO2 = 21,                /* VERBMACRO2  */
  YYSYMBOL_VERBLATEX = 22,                 /* VERBLATEX  */
  YYSYMBOL_LATEXMACRO3 = 23,               /* LATEXMACRO3  */
  YYSYMBOL_NEWCOMMAND = 24,                /* NEWCOMMAND  */
  YYSYMBOL_USERMACRO = 25,                 /* USERMACRO  */
  YYSYMBOL_USERMACRO1 = 26,                /* USERMACRO1  */
  YYSYMBOL_USERMACRO2 = 27,                /* USERMACRO2  */
  YYSYMBOL_USERMACRO3 = 28,                /* USERMACRO3  */
  YYSYMBOL_USERMACRO4 = 29,                /* USERMACRO4  */
  YYSYMBOL_USERMACRO5 = 30,                /* USERMACRO5  */
  YYSYMBOL_USERMACRO6 = 31,                /* USERMACRO6  */
  YYSYMBOL_USERMACRO7 = 32,                /* USERMACRO7  */
  YYSYMBOL_USERMACRO8 = 33,                /* USERMACRO8  */
  YYSYMBOL_USERMACRO9 = 34,                /* USERMACRO9  */
  YYSYMBOL_IFDEF = 35,                     /* IFDEF  */
  YYSYMBOL_ENDIF = 36,                     /* ENDIF  */
  YYSYMBOL_TEXT = 37,                      /* TEXT  */
  YYSYMBOL_RCODE = 38,                     /* RCODE  */
  YYSYMBOL_VERB = 39,                      /* VERB  */
  YYSYMBOL_COMMENT = 40,                   /* COMMENT  */
  YYSYMBOL_UNKNOWN = 41,                   /* UNKNOWN  */
  YYSYMBOL_STARTFILE = 42,                 /* STARTFILE  */
  YYSYMBOL_STARTFRAGMENT = 43,             /* STARTFRAGMENT  */
  YYSYMBOL_44_ = 44,                       /* '{'  */
  YYSYMBOL_45_ = 45,                       /* '}'  */
  YYSYMBOL_46_ = 46,                       /* '['  */
  YYSYMBOL_47_ = 47,                       /* ']'  */
  YYSYMBOL_YYACCEPT = 48,                  /* $accept  */
  YYSYMBOL_Init = 49,                      /* Init  */
  YYSYMBOL_RdFragment = 50,                /* RdFragment  */
  YYSYMBOL_RdFile = 51,                    /* RdFile  */
  YYSYMBOL_SectionList = 52,               /* SectionList  */
  YYSYMBOL_Section = 53,                   /* Section  */
  YYSYMBOL_ArgItems = 54,                  /* ArgItems  */
  YYSYMBOL_Item = 55,                      /* Item  */
  YYSYMBOL_Markup = 56,                    /* Markup  */
  YYSYMBOL_UserMacro = 57,                 /* UserMacro  */
  YYSYMBOL_LatexArg = 58,                  /* LatexArg  */
  YYSYMBOL_LatexArg2 = 59,                 /* LatexArg2  */
  YYSYMBOL_Item0Arg = 60,                  /* Item0Arg  */
  YYSYMBOL_Item2Arg = 61,                  /* Item2Arg  */
  YYSYMBOL_RLikeArg = 62,                  /* RLikeArg  */
  YYSYMBOL_RLikeArg2 = 63,                 /* RLikeArg2  */
  YYSYMBOL_VerbatimArg = 64,               /* VerbatimArg  */
  YYSYMBOL_VerbatimArg1 = 65,              /* VerbatimArg1  */
  YYSYMBOL_VerbatimArg2 = 66,              /* VerbatimArg2  */
  YYSYMBOL_IfDefTarget = 67,               /* IfDefTarget  */
  YYSYMBOL_goLatexLike = 68,               /* goLatexLike  */
  YYSYMBOL_goRLike = 69,                   /* goRLike  */
  YYSYMBOL_goRLike2 = 70,                  /* goRLike2  */
  YYSYMBOL_goOption = 71,                  /* goOption  */
  YYSYMBOL_goVerbatim = 72,                /* goVerbatim  */
  YYSYMBOL_goVerbatim1 = 73,               /* goVerbatim1  */
  YYSYMBOL_goVerbatim2 = 74,               /* goVerbatim2  */
  YYSYMBOL_goItem0 = 75,                   /* goItem0  */
  YYSYMBOL_goItem2 = 76,                   /* goItem2  */
  YYSYMBOL_Arg = 77,                       /* Arg  */
  YYSYMBOL_Option = 78                     /* Option  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  33
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   832

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  48
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  31
/* YYNRULES -- Number of rules.  */
#define YYNRULES  89
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  194

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   298


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    46,     2,    47,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    44,     2,    45,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   232,   232,   233,   234,   237,   240,   243,   244,   246,
     247,   248,   249,   250,   251,   252,   253,   254,   255,   256,
     257,   258,   259,   261,   262,   264,   265,   266,   267,   268,
     269,   270,   271,   272,   274,   275,   276,   277,   278,   279,
     280,   281,   282,   283,   284,   285,   286,   287,   288,   289,
     290,   292,   293,   294,   295,   297,   299,   301,   303,   305,
     308,   311,   316,   318,   319,   328,   330,   332,   336,   337,
     339,   341,   345,   346,   348,   351,   353,   355,   357,   359,
     361,   363,   365,   367,   369,   370,   371,   372,   373,   375
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "END_OF_INPUT",
  "ERROR", "SECTIONHEADER", "RSECTIONHEADER", "VSECTIONHEADER",
  "SECTIONHEADER2", "RCODEMACRO", "SEXPR", "RDOPTS", "LATEXMACRO",
  "VERBMACRO", "OPTMACRO", "ESCAPE", "LISTSECTION", "ITEMIZE",
  "DESCRIPTION", "NOITEM", "LATEXMACRO2", "VERBMACRO2", "VERBLATEX",
  "LATEXMACRO3", "NEWCOMMAND", "USERMACRO", "USERMACRO1", "USERMACRO2",
  "USERMACRO3", "USERMACRO4", "USERMACRO5", "USERMACRO6", "USERMACRO7",
  "USERMACRO8", "USERMACRO9", "IFDEF", "ENDIF", "TEXT", "RCODE", "VERB",
  "COMMENT", "UNKNOWN", "STARTFILE", "STARTFRAGMENT", "'{'", "'}'", "'['",
  "']'", "$accept", "Init", "RdFragment", "RdFile", "SectionList",
  "Section", "ArgItems", "Item", "Markup", "UserMacro", "LatexArg",
  "LatexArg2", "Item0Arg", "Item2Arg", "RLikeArg", "RLikeArg2",
  "VerbatimArg", "VerbatimArg1", "VerbatimArg2", "IfDefTarget",
  "goLatexLike", "goRLike", "goRLike2", "goOption", "goVerbatim",
  "goVerbatim1", "goVerbatim2", "goItem0", "goItem2", "Arg", "Option", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-94)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-50)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      28,   -94,   792,   -94,    20,   792,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,    29,   718,   -94,
     -94,    34,   638,   -94,   -94,   -94,   -19,   -94,   -19,   -94,
     -19,   -94,   -30,   -94,   -94,   -19,   -94,   -19,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   792,    -6,   -94,
     -94,   -94,   638,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   265,   556,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   -22,   -94,   638,   -94,     2,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   755,   -94,   -94,
     -94,   -30,   -94,   -94,     1,   -94,   -19,   -94,   -94,     4,
     -94,   -94,   638,   306,   -94,   347,   -94,   -94,   -94,   388,
       7,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   680,
     -94,   -94,     2,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   597,   -94,   224,   -94,   -94,   429,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   470,   -94,   179,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   511,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     4,     0,    75,     0,     0,    75,    76,    79,    75,
      78,    79,    83,    80,    52,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    75,    20,    19,     0,     0,     7,
      21,     0,     0,     1,    22,    12,     0,    11,     0,     9,
       0,    75,     0,    10,    13,     0,    79,     0,    53,    79,
      79,    79,    79,    79,    79,    79,    79,     0,     0,     2,
       8,     3,     0,    76,    78,    75,    79,    78,    47,    82,
      83,    75,    80,    79,    75,    75,    25,    26,    27,    28,
      29,     0,     0,    23,    31,    32,    30,    62,    67,    70,
      14,     0,    77,     0,    17,     0,    66,    51,    71,    54,
      79,    79,    79,    79,    79,    79,    79,     0,    74,    33,
      41,     0,    34,    44,    75,    37,     0,    38,    75,    45,
      75,    75,     0,     0,    85,     0,    24,    64,    63,     0,
       0,    18,    55,    79,    79,    79,    79,    79,    79,     0,
      15,    42,     0,    39,    75,    65,    35,    81,    46,    50,
      75,     0,    87,     0,    84,    69,     0,    89,    56,    79,
      79,    79,    79,    79,    43,    40,     0,    36,     0,    48,
      88,    86,    68,    57,    79,    79,    79,    79,    73,     0,
      58,    79,    79,    79,    72,    79,    79,    79,    59,    79,
      79,    60,    79,    61
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -94,   -94,   -94,   -94,     3,    -2,   -64,   -10,   -94,    22,
      -8,   -43,   -94,    -9,    -4,   -93,   -11,    -5,   -94,    -7,
      10,   -94,   -94,   -31,   -94,   -94,   -94,   -94,   -94,   -17,
     -58
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     4,    31,    27,    28,    29,    82,    83,    84,    85,
      35,    90,   115,    44,    37,    94,    39,    46,   148,    57,
      36,    38,   129,    42,    40,    47,   166,   116,    45,    86,
      95
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      43,    41,   131,    34,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    32,    92,   127,    93,   125,   141,    87,
      33,    88,    81,    89,    30,    81,    60,    30,    96,     1,
      98,   108,    59,   111,    58,    97,   114,    61,    99,   100,
     101,   102,   103,   104,   105,   106,    92,    93,   147,   164,
      30,    91,   109,   142,   157,   113,   144,   112,   151,   110,
     107,   117,   120,   118,     0,   156,   121,   119,   122,     0,
       2,     3,   126,     0,   128,   146,     0,   149,   150,    30,
       0,     0,     0,   130,     0,    58,     0,     0,     0,   132,
     133,   134,   135,   136,   137,   138,     0,     0,     0,   145,
       0,     0,   179,     0,     0,    60,   143,   167,     0,     0,
       0,     0,     0,   109,     0,   126,     0,     0,     0,     0,
       0,     0,   158,   159,   160,   161,   162,   163,    91,    30,
      91,    91,     0,     0,     0,     0,   165,    34,     0,     0,
       0,   126,     0,   109,     0,     0,   126,     0,   173,   174,
     175,   176,   177,     0,     0,     0,     0,     0,   109,     0,
      91,    30,     0,   180,   181,   182,   183,     0,     0,   126,
     185,   186,   187,     0,   188,   189,   190,     0,   191,   192,
      62,   193,   -49,     0,     0,     0,     0,     0,    63,    64,
       0,    65,    66,    67,    68,     0,    69,    70,     0,    71,
      72,    73,    74,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    75,   -49,    76,    77,    78,    79,
      80,     0,     0,    81,   -49,    62,   -49,   170,     0,     0,
       0,     0,     0,    63,    64,     0,    65,    66,    67,    68,
       0,    69,    70,     0,    71,    72,    73,    74,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    75,
       0,    76,    77,    78,    79,    80,   123,     0,    81,   171,
       0,     0,     0,     0,    63,    64,     0,    65,    66,    67,
      68,     0,    69,    70,     0,    71,    72,    73,    74,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      75,     0,    76,    77,    78,    79,    80,    62,     0,    81,
     124,     0,     0,     0,     0,    63,    64,     0,    65,    66,
      67,    68,     0,    69,    70,     0,    71,    72,    73,    74,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    75,     0,    76,    77,    78,    79,    80,   153,     0,
      81,   152,     0,     0,     0,     0,    63,    64,     0,    65,
      66,    67,    68,     0,    69,    70,     0,    71,    72,    73,
      74,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    75,     0,    76,    77,    78,    79,    80,    62,
       0,    81,   154,     0,     0,     0,     0,    63,    64,     0,
      65,    66,    67,    68,     0,    69,    70,     0,    71,    72,
      73,    74,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    75,     0,    76,    77,    78,    79,    80,
      62,     0,    81,   155,     0,     0,     0,     0,    63,    64,
       0,    65,    66,    67,    68,     0,    69,    70,     0,    71,
      72,    73,    74,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    75,     0,    76,    77,    78,    79,
      80,    62,     0,    81,   172,     0,     0,     0,     0,    63,
      64,     0,    65,    66,    67,    68,     0,    69,    70,     0,
      71,    72,    73,    74,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    75,     0,    76,    77,    78,
      79,    80,    62,     0,    81,   178,     0,     0,     0,     0,
      63,    64,     0,    65,    66,    67,    68,     0,    69,    70,
       0,    71,    72,    73,    74,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    75,     0,    76,    77,
      78,    79,    80,     0,     0,    81,   184,    62,     0,    -5,
       0,     0,     0,     0,     0,    63,    64,     0,    65,    66,
      67,    68,     0,    69,    70,     0,    71,    72,    73,    74,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    75,     0,    76,    77,    78,    79,    80,   168,     0,
      81,     0,     0,     0,     0,     0,    63,    64,     0,    65,
      66,    67,    68,     0,    69,    70,     0,    71,    72,    73,
      74,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    75,   169,    76,    77,    78,    79,    80,    62,
       0,    81,     0,     0,     0,     0,     0,    63,    64,     0,
      65,    66,    67,    68,     0,    69,    70,     0,    71,    72,
      73,    74,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    75,     0,    76,    77,    78,    79,    80,
       0,     5,    81,   -16,     0,     6,     7,     8,     9,     0,
      10,    11,     0,     0,     0,     0,    12,     0,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,   -16,    25,     0,     5,
      26,    -6,     0,     6,     7,     8,     9,     0,    10,    11,
       0,     0,     0,     0,    12,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,     0,    25,   139,     0,    26,     0,
       6,     7,     8,     9,     0,    10,    11,     0,     0,     0,
       0,    12,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,   140,    25,     5,     0,    26,     0,     6,     7,     8,
       9,     0,    10,    11,     0,     0,     0,     0,    12,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,     0,    25,
       0,     0,    26
};

static const yytype_int16 yycheck[] =
{
      11,     9,    95,     5,    15,    16,    17,    18,    19,    20,
      21,    22,    23,     3,    44,    37,    46,    81,   111,    36,
       0,    38,    44,    40,     2,    44,    28,     5,    45,     1,
      47,    37,     3,    64,    24,    46,    67,     3,    49,    50,
      51,    52,    53,    54,    55,    56,    44,    46,    44,   142,
      28,    41,    62,   111,    47,    66,   114,    65,   122,    63,
      57,    70,    73,    71,    -1,   129,    74,    72,    75,    -1,
      42,    43,    82,    -1,    91,   118,    -1,   120,   121,    57,
      -1,    -1,    -1,    93,    -1,    75,    -1,    -1,    -1,   100,
     101,   102,   103,   104,   105,   106,    -1,    -1,    -1,   116,
      -1,    -1,   166,    -1,    -1,   107,   114,   150,    -1,    -1,
      -1,    -1,    -1,   123,    -1,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   133,   134,   135,   136,   137,   138,   118,   107,
     120,   121,    -1,    -1,    -1,    -1,   144,   139,    -1,    -1,
      -1,   151,    -1,   153,    -1,    -1,   156,    -1,   159,   160,
     161,   162,   163,    -1,    -1,    -1,    -1,    -1,   168,    -1,
     150,   139,    -1,   174,   175,   176,   177,    -1,    -1,   179,
     181,   182,   183,    -1,   185,   186,   187,    -1,   189,   190,
       1,   192,     3,    -1,    -1,    -1,    -1,    -1,     9,    10,
      -1,    12,    13,    14,    15,    -1,    17,    18,    -1,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    -1,    -1,    44,    45,     1,    47,     3,    -1,    -1,
      -1,    -1,    -1,     9,    10,    -1,    12,    13,    14,    15,
      -1,    17,    18,    -1,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    41,     1,    -1,    44,    45,
      -1,    -1,    -1,    -1,     9,    10,    -1,    12,    13,    14,
      15,    -1,    17,    18,    -1,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,     1,    -1,    44,
      45,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,    13,
      14,    15,    -1,    17,    18,    -1,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    41,     1,    -1,
      44,    45,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,
      13,    14,    15,    -1,    17,    18,    -1,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    41,     1,
      -1,    44,    45,    -1,    -1,    -1,    -1,     9,    10,    -1,
      12,    13,    14,    15,    -1,    17,    18,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    41,
       1,    -1,    44,    45,    -1,    -1,    -1,    -1,     9,    10,
      -1,    12,    13,    14,    15,    -1,    17,    18,    -1,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      41,     1,    -1,    44,    45,    -1,    -1,    -1,    -1,     9,
      10,    -1,    12,    13,    14,    15,    -1,    17,    18,    -1,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    41,     1,    -1,    44,    45,    -1,    -1,    -1,    -1,
       9,    10,    -1,    12,    13,    14,    15,    -1,    17,    18,
      -1,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    -1,    -1,    44,    45,     1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,    13,
      14,    15,    -1,    17,    18,    -1,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    41,     1,    -1,
      44,    -1,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,
      13,    14,    15,    -1,    17,    18,    -1,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     1,
      -1,    44,    -1,    -1,    -1,    -1,    -1,     9,    10,    -1,
      12,    13,    14,    15,    -1,    17,    18,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    41,
      -1,     1,    44,     3,    -1,     5,     6,     7,     8,    -1,
      10,    11,    -1,    -1,    -1,    -1,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    -1,     1,
      40,     3,    -1,     5,     6,     7,     8,    -1,    10,    11,
      -1,    -1,    -1,    -1,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    -1,    37,     1,    -1,    40,    -1,
       5,     6,     7,     8,    -1,    10,    11,    -1,    -1,    -1,
      -1,    16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,     1,    -1,    40,    -1,     5,     6,     7,
       8,    -1,    10,    11,    -1,    -1,    -1,    -1,    16,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    -1,    37,
      -1,    -1,    40
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,    42,    43,    49,     1,     5,     6,     7,     8,
      10,    11,    16,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    37,    40,    51,    52,    53,
      57,    50,    68,     0,    53,    58,    68,    62,    69,    64,
      72,    58,    71,    64,    61,    76,    65,    73,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    67,    68,     3,
      53,     3,     1,     9,    10,    12,    13,    14,    15,    17,
      18,    20,    21,    22,    23,    35,    37,    38,    39,    40,
      41,    44,    54,    55,    56,    57,    77,    77,    77,    77,
      59,    68,    44,    46,    63,    78,    77,    64,    77,    64,
      64,    64,    64,    64,    64,    64,    64,    52,    37,    55,
      62,    71,    58,    64,    71,    60,    75,    61,    58,    65,
      64,    58,    67,     1,    45,    54,    55,    37,    77,    70,
      55,    63,    64,    64,    64,    64,    64,    64,    64,     1,
      36,    63,    78,    58,    78,    77,    59,    44,    66,    59,
      59,    54,    45,     1,    45,    45,    54,    47,    64,    64,
      64,    64,    64,    64,    63,    58,    74,    59,     1,    36,
       3,    45,    45,    64,    64,    64,    64,    64,    45,    54,
      64,    64,    64,    64,    45,    64,    64,    64,    64,    64,
      64,    64,    64,    64
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    48,    49,    49,    49,    50,    51,    52,    52,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    54,    54,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    56,    56,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    57,    57,    57,    57,    57,    57,    57,    57,    57,
      57,    57,    58,    59,    59,    60,    61,    62,    63,    63,
      64,    65,    66,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    77,    77,    77,    77,    78
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     3,     3,     1,     2,     1,     1,     2,     2,
       2,     2,     2,     2,     3,     4,     4,     3,     4,     1,
       1,     1,     2,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     3,     4,     2,     2,     3,
       4,     2,     3,     4,     2,     2,     3,     1,     4,     4,
       3,     3,     1,     2,     3,     4,     5,     6,     7,     9,
      10,    11,     2,     2,     2,     2,     2,     2,     4,     3,
       2,     2,     4,     3,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     3,     2,     4,     3,     4,     3
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  switch (yykind)
    {
    case YYSYMBOL_SECTIONHEADER: /* SECTIONHEADER  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_RSECTIONHEADER: /* RSECTIONHEADER  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_VSECTIONHEADER: /* VSECTIONHEADER  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_SECTIONHEADER2: /* SECTIONHEADER2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_RCODEMACRO: /* RCODEMACRO  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_SEXPR: /* SEXPR  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_LATEXMACRO: /* LATEXMACRO  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_VERBMACRO: /* VERBMACRO  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_OPTMACRO: /* OPTMACRO  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_ESCAPE: /* ESCAPE  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_LISTSECTION: /* LISTSECTION  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_ITEMIZE: /* ITEMIZE  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_DESCRIPTION: /* DESCRIPTION  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_NOITEM: /* NOITEM  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_LATEXMACRO2: /* LATEXMACRO2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_VERBMACRO2: /* VERBMACRO2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_VERBLATEX: /* VERBLATEX  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_LATEXMACRO3: /* LATEXMACRO3  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_NEWCOMMAND: /* NEWCOMMAND  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO: /* USERMACRO  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO1: /* USERMACRO1  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO2: /* USERMACRO2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO3: /* USERMACRO3  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO4: /* USERMACRO4  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO5: /* USERMACRO5  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO6: /* USERMACRO6  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO7: /* USERMACRO7  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO8: /* USERMACRO8  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_USERMACRO9: /* USERMACRO9  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_IFDEF: /* IFDEF  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_ENDIF: /* ENDIF  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_TEXT: /* TEXT  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_RCODE: /* RCODE  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_VERB: /* VERB  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_COMMENT: /* COMMENT  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_UNKNOWN: /* UNKNOWN  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_STARTFILE: /* STARTFILE  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_STARTFRAGMENT: /* STARTFRAGMENT  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_ArgItems: /* ArgItems  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_LatexArg: /* LatexArg  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_RLikeArg2: /* RLikeArg2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_VerbatimArg1: /* VerbatimArg1  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_VerbatimArg2: /* VerbatimArg2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_IfDefTarget: /* IfDefTarget  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goLatexLike: /* goLatexLike  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goRLike: /* goRLike  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goRLike2: /* goRLike2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goOption: /* goOption  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goVerbatim: /* goVerbatim  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goVerbatim1: /* goVerbatim1  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goVerbatim2: /* goVerbatim2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goItem0: /* goItem0  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_goItem2: /* goItem2  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_Option: /* Option  */
            { RELEASE_SV((*yyvaluep)); }
        break;

      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* Init: STARTFILE RdFile END_OF_INPUT  */
                                                        { xxsavevalue(yyvsp[-1], &(yyloc)); RELEASE_SV(yyvsp[-2]); YYACCEPT; }
    break;

  case 3: /* Init: STARTFRAGMENT RdFragment END_OF_INPUT  */
                                                        { xxsavevalue(yyvsp[-1], &(yyloc)); RELEASE_SV(yyvsp[-2]); YYACCEPT; }
    break;

  case 4: /* Init: error  */
                                                        { PRESERVE_SV(parseState.Value = R_NilValue);  YYABORT; }
    break;

  case 5: /* RdFragment: goLatexLike ArgItems  */
                                                { yyval = yyvsp[0]; RELEASE_SV(yyvsp[-1]); }
    break;

  case 6: /* RdFile: SectionList  */
                                                { yyval = yyvsp[0]; }
    break;

  case 7: /* SectionList: Section  */
                                                { yyval = xxnewlist(yyvsp[0]); }
    break;

  case 8: /* SectionList: SectionList Section  */
                                                { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
    break;

  case 9: /* Section: VSECTIONHEADER VerbatimArg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 10: /* Section: RDOPTS VerbatimArg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], HAS_SEXPR, &(yyloc)); }
    break;

  case 11: /* Section: RSECTIONHEADER RLikeArg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 12: /* Section: SECTIONHEADER LatexArg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 13: /* Section: LISTSECTION Item2Arg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 14: /* Section: SECTIONHEADER2 LatexArg LatexArg2  */
                                                  { yyval = xxmarkup2(yyvsp[-2], yyvsp[-1], yyvsp[0], 2, STATIC, &(yyloc)); }
    break;

  case 15: /* Section: IFDEF IfDefTarget SectionList ENDIF  */
                                                    { yyval = xxmarkup2(yyvsp[-3], yyvsp[-2], yyvsp[-1], 2, HAS_IFDEF, &(yyloc)); RELEASE_SV(yyvsp[0]); }
    break;

  case 16: /* Section: IFDEF IfDefTarget SectionList error  */
                                                    { yyval = xxmarkup2(yyvsp[-3], yyvsp[-2], yyvsp[-1], 2, HAS_IFDEF, &(yyloc)); }
    break;

  case 17: /* Section: SEXPR goOption RLikeArg2  */
                                                 { yyval = xxmarkup(yyvsp[-2], yyvsp[0], HAS_SEXPR, &(yyloc)); xxpopMode(yyvsp[-1]); }
    break;

  case 18: /* Section: SEXPR goOption Option RLikeArg2  */
                                                      { yyval = xxOptionmarkup(yyvsp[-3], yyvsp[-1], yyvsp[0], HAS_SEXPR, &(yyloc)); xxpopMode(yyvsp[-2]); }
    break;

  case 19: /* Section: COMMENT  */
                                                { yyval = xxtag(yyvsp[0], COMMENT, &(yyloc)); }
    break;

  case 20: /* Section: TEXT  */
                                                { yyval = xxtag(yyvsp[0], TEXT, &(yyloc)); }
    break;

  case 21: /* Section: UserMacro  */
                                                { yyval = yyvsp[0]; }
    break;

  case 22: /* Section: error Section  */
                                                { yyval = yyvsp[0]; }
    break;

  case 23: /* ArgItems: Item  */
                                                { yyval = xxnewlist(yyvsp[0]); }
    break;

  case 24: /* ArgItems: ArgItems Item  */
                                                { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
    break;

  case 25: /* Item: TEXT  */
                                                { yyval = xxtag(yyvsp[0], TEXT, &(yyloc)); }
    break;

  case 26: /* Item: RCODE  */
                                                { yyval = xxtag(yyvsp[0], RCODE, &(yyloc)); }
    break;

  case 27: /* Item: VERB  */
                                                { yyval = xxtag(yyvsp[0], VERB, &(yyloc)); }
    break;

  case 28: /* Item: COMMENT  */
                                                { yyval = xxtag(yyvsp[0], COMMENT, &(yyloc)); }
    break;

  case 29: /* Item: UNKNOWN  */
                                                { yyval = xxtag(yyvsp[0], UNKNOWN, &(yyloc)); yyerror(yyunknown); }
    break;

  case 30: /* Item: Arg  */
                                                { yyval = xxmarkup(R_NilValue, yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 31: /* Item: Markup  */
                                                { yyval = yyvsp[0]; }
    break;

  case 32: /* Item: UserMacro  */
                                                { yyval = yyvsp[0]; }
    break;

  case 33: /* Item: error Item  */
                                                { yyval = yyvsp[0]; }
    break;

  case 34: /* Markup: LATEXMACRO LatexArg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 35: /* Markup: LATEXMACRO2 LatexArg LatexArg2  */
                                                { yyval = xxmarkup2(yyvsp[-2], yyvsp[-1], yyvsp[0], 2, STATIC, &(yyloc)); }
    break;

  case 36: /* Markup: LATEXMACRO3 LatexArg LatexArg2 LatexArg2  */
                                                         { yyval = xxmarkup3(yyvsp[-3], yyvsp[-2], yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 37: /* Markup: ITEMIZE Item0Arg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 38: /* Markup: DESCRIPTION Item2Arg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 39: /* Markup: OPTMACRO goOption LatexArg  */
                                                { yyval = xxmarkup(yyvsp[-2], yyvsp[0], STATIC, &(yyloc)); xxpopMode(yyvsp[-1]); }
    break;

  case 40: /* Markup: OPTMACRO goOption Option LatexArg  */
                                                     { yyval = xxOptionmarkup(yyvsp[-3], yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); xxpopMode(yyvsp[-2]); }
    break;

  case 41: /* Markup: RCODEMACRO RLikeArg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 42: /* Markup: SEXPR goOption RLikeArg2  */
                                                 { yyval = xxmarkup(yyvsp[-2], yyvsp[0], HAS_SEXPR, &(yyloc)); xxpopMode(yyvsp[-1]); }
    break;

  case 43: /* Markup: SEXPR goOption Option RLikeArg2  */
                                                      { yyval = xxOptionmarkup(yyvsp[-3], yyvsp[-1], yyvsp[0], HAS_SEXPR, &(yyloc)); xxpopMode(yyvsp[-2]); }
    break;

  case 44: /* Markup: VERBMACRO VerbatimArg  */
                                                { yyval = xxmarkup(yyvsp[-1], yyvsp[0], STATIC, &(yyloc)); }
    break;

  case 45: /* Markup: VERBMACRO2 VerbatimArg1  */
                                                { yyval = xxmarkup2(yyvsp[-1], yyvsp[0], R_NilValue, 1, STATIC, &(yyloc)); }
    break;

  case 46: /* Markup: VERBMACRO2 VerbatimArg1 VerbatimArg2  */
                                                      { yyval = xxmarkup2(yyvsp[-2], yyvsp[-1], yyvsp[0], 2, STATIC, &(yyloc)); }
    break;

  case 47: /* Markup: ESCAPE  */
                                                { yyval = xxmarkup(yyvsp[0], R_NilValue, STATIC, &(yyloc)); }
    break;

  case 48: /* Markup: IFDEF IfDefTarget ArgItems ENDIF  */
                                                 { yyval = xxmarkup2(yyvsp[-3], yyvsp[-2], yyvsp[-1], 2, HAS_IFDEF, &(yyloc)); RELEASE_SV(yyvsp[0]); }
    break;

  case 49: /* Markup: IFDEF IfDefTarget ArgItems error  */
                                                 { yyval = xxmarkup2(yyvsp[-3], yyvsp[-2], yyvsp[-1], 2, HAS_IFDEF, &(yyloc)); }
    break;

  case 50: /* Markup: VERBLATEX VerbatimArg LatexArg2  */
                                                  { yyval = xxmarkup2(yyvsp[-2], yyvsp[-1], yyvsp[0], 2, STATIC, &(yyloc)); }
    break;

  case 51: /* UserMacro: NEWCOMMAND VerbatimArg1 VerbatimArg  */
                                                     { yyval = xxnewcommand(yyvsp[-2], yyvsp[-1], yyvsp[0], &(yyloc)); }
    break;

  case 52: /* UserMacro: USERMACRO  */
                                                { yyval = xxusermacro(yyvsp[0], xxnewlist(NULL), &(yyloc)); }
    break;

  case 53: /* UserMacro: USERMACRO1 VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-1], xxnewlist(yyvsp[0]), &(yyloc)); }
    break;

  case 54: /* UserMacro: USERMACRO2 VerbatimArg VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-2], xxnewlist2(yyvsp[-1], yyvsp[0]), &(yyloc)); }
    break;

  case 55: /* UserMacro: USERMACRO3 VerbatimArg VerbatimArg VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-3], xxnewlist3(yyvsp[-2], yyvsp[-1], yyvsp[0]), &(yyloc)); }
    break;

  case 56: /* UserMacro: USERMACRO4 VerbatimArg VerbatimArg VerbatimArg VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-4], xxnewlist4(yyvsp[-3], yyvsp[-2], yyvsp[-1], yyvsp[0]), &(yyloc)); }
    break;

  case 57: /* UserMacro: USERMACRO5 VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-5], xxnewlist5(yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1], yyvsp[0]), &(yyloc)); }
    break;

  case 58: /* UserMacro: USERMACRO6 VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-6], xxnewlist6(yyvsp[-5], yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1], yyvsp[0]), &(yyloc)); }
    break;

  case 59: /* UserMacro: USERMACRO7 VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-8], xxnewlist7(yyvsp[-7], yyvsp[-6], yyvsp[-5], yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1]), &(yyloc)); }
    break;

  case 60: /* UserMacro: USERMACRO8 VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-9], xxnewlist8(yyvsp[-8], yyvsp[-7], yyvsp[-6], yyvsp[-5], yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1]), &(yyloc)); }
    break;

  case 61: /* UserMacro: USERMACRO9 VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg  */
                                                { yyval = xxusermacro(yyvsp[-10], xxnewlist9(yyvsp[-9], yyvsp[-8], yyvsp[-7], yyvsp[-6], yyvsp[-5], yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1]), &(yyloc)); }
    break;

  case 62: /* LatexArg: goLatexLike Arg  */
                                                { xxpopMode(yyvsp[-1]); yyval = yyvsp[0]; }
    break;

  case 63: /* LatexArg2: goLatexLike Arg  */
                                                { xxpopMode(yyvsp[-1]); yyval = yyvsp[0]; }
    break;

  case 64: /* LatexArg2: goLatexLike TEXT  */
                                                { xxpopMode(yyvsp[-1]); yyval = xxnewlist(yyvsp[0]); 
     						  if(wCalls)
    	    					      warning(_("bad markup (extra space?) at %s:%d:%d"), 
    	    					            parseState.xxBasename, (yylsp[0]).first_line, (yylsp[0]).first_column); 
     						  else
    	    					      warningcall(R_NilValue, _("bad markup (extra space?) at %s:%d:%d"), 
    	    					            parseState.xxBasename, (yylsp[0]).first_line, (yylsp[0]).first_column); 
						}
    break;

  case 65: /* Item0Arg: goItem0 Arg  */
                                                { xxpopMode(yyvsp[-1]); yyval = yyvsp[0]; }
    break;

  case 66: /* Item2Arg: goItem2 Arg  */
                                                { xxpopMode(yyvsp[-1]); yyval = yyvsp[0]; }
    break;

  case 67: /* RLikeArg: goRLike Arg  */
                                                { xxpopMode(yyvsp[-1]); yyval = yyvsp[0]; }
    break;

  case 68: /* RLikeArg2: '{' goRLike2 ArgItems '}'  */
                                                { xxpopMode(yyvsp[-2]); yyval = yyvsp[-1]; }
    break;

  case 69: /* RLikeArg2: '{' goRLike2 '}'  */
                                                { xxpopMode(yyvsp[-1]); yyval = xxnewlist(NULL); }
    break;

  case 70: /* VerbatimArg: goVerbatim Arg  */
                                                { xxpopMode(yyvsp[-1]); yyval = yyvsp[0]; }
    break;

  case 71: /* VerbatimArg1: goVerbatim1 Arg  */
                                                { xxpopMode(yyvsp[-1]); yyval = yyvsp[0]; }
    break;

  case 72: /* VerbatimArg2: '{' goVerbatim2 ArgItems '}'  */
                                                { xxpopMode(yyvsp[-2]); yyval = yyvsp[-1]; }
    break;

  case 73: /* VerbatimArg2: '{' goVerbatim2 '}'  */
                                                { xxpopMode(yyvsp[-1]); yyval = xxnewlist(NULL); }
    break;

  case 74: /* IfDefTarget: goLatexLike TEXT  */
                                        { xxpopMode(yyvsp[-1]); yyval = xxnewlist(xxtag(yyvsp[0], TEXT, &(yyloc))); }
    break;

  case 75: /* goLatexLike: %empty  */
                                                { yyval = xxpushMode(LATEXLIKE, UNKNOWN, FALSE); }
    break;

  case 76: /* goRLike: %empty  */
                                                { yyval = xxpushMode(RLIKE, UNKNOWN, FALSE); }
    break;

  case 77: /* goRLike2: %empty  */
                                                { parseState.xxbraceDepth--; yyval = xxpushMode(RLIKE, UNKNOWN, FALSE); parseState.xxbraceDepth++; }
    break;

  case 78: /* goOption: %empty  */
                                                { yyval = xxpushMode(INOPTION, UNKNOWN, FALSE); }
    break;

  case 79: /* goVerbatim: %empty  */
                                                { yyval = xxpushMode(VERBATIM, UNKNOWN, FALSE); }
    break;

  case 80: /* goVerbatim1: %empty  */
                                                { yyval = xxpushMode(VERBATIM, UNKNOWN, TRUE); }
    break;

  case 81: /* goVerbatim2: %empty  */
                                                { parseState.xxbraceDepth--; yyval = xxpushMode(VERBATIM, UNKNOWN, FALSE); parseState.xxbraceDepth++; }
    break;

  case 82: /* goItem0: %empty  */
                                                { yyval = xxpushMode(LATEXLIKE, ESCAPE, FALSE); }
    break;

  case 83: /* goItem2: %empty  */
                                                { yyval = xxpushMode(LATEXLIKE, LATEXMACRO2, FALSE); }
    break;

  case 84: /* Arg: '{' ArgItems '}'  */
                                                { yyval = yyvsp[-1]; }
    break;

  case 85: /* Arg: '{' '}'  */
                                                { yyval = xxnewlist(NULL); }
    break;

  case 86: /* Arg: '{' ArgItems error '}'  */
                                                { yyval = yyvsp[-2]; }
    break;

  case 87: /* Arg: '{' error '}'  */
                                                { yyval = xxnewlist(NULL); }
    break;

  case 88: /* Arg: '{' ArgItems error END_OF_INPUT  */
                                                { yyval = yyvsp[-2]; }
    break;

  case 89: /* Option: '[' Item ']'  */
                                                { yyval = yyvsp[-1]; }
    break;



      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}



static SEXP xxpushMode(int newmode, int newitem, int neweqn)
{
    SEXP ans;

    PRESERVE_SV(ans = allocVector(INTSXP, 7));
    INTEGER(ans)[0] = parseState.xxmode;		/* Lexer mode */
    INTEGER(ans)[1] = parseState.xxitemType;	/* What is \item? */
    INTEGER(ans)[2] = parseState.xxbraceDepth;	/* Brace depth used in RCODE and VERBATIM */
    INTEGER(ans)[3] = parseState.xxinRString;      /* Quote char that started a string */
    INTEGER(ans)[4] = parseState.xxQuoteLine;      /* Where the quote was */
    INTEGER(ans)[5] = parseState.xxQuoteCol;       /*           "         */
    INTEGER(ans)[6] = parseState.xxinEqn;          /* In the first arg to \eqn or \deqn:  no escapes */

#if DEBUGMODE
    Rprintf("xxpushMode(%d, %s) pushes %d, %s, %d\n", newmode, yytname[YYTRANSLATE(newitem)], 
    						parseState.xxmode, yytname[YYTRANSLATE(parseState.xxitemType)], parseState.xxbraceDepth);
#endif
    parseState.xxmode = newmode;
    parseState.xxitemType = newitem;
    parseState.xxbraceDepth = 0;
    parseState.xxinRString = 0;
    parseState.xxinEqn = neweqn;

    return ans;
}

static void xxpopMode(SEXP oldmode) 
{
#if DEBUGVALS
    Rprintf("xxpopMode(%d, %s, %d) replaces %d, %s, %d\n", INTEGER(oldmode)[0], yytname[YYTRANSLATE(INTEGER(oldmode)[1])], INTEGER(oldmode)[2], 
    					parseState.xxmode, yytname[YYTRANSLATE(parseState.xxitemType)], parseState.xxbraceDepth);
#endif
    parseState.xxmode = INTEGER(oldmode)[0];
    parseState.xxitemType = INTEGER(oldmode)[1]; 
    parseState.xxbraceDepth = INTEGER(oldmode)[2];
    parseState.xxinRString = INTEGER(oldmode)[3];
    parseState.xxQuoteLine = INTEGER(oldmode)[4];
    parseState.xxQuoteCol  = INTEGER(oldmode)[5];
    parseState.xxinEqn	= INTEGER(oldmode)[6];

    RELEASE_SV(oldmode);
}

static int getDynamicFlag(SEXP item)
{
    SEXP flag = getAttrib(item, R_DynamicFlagSymbol);
    if (isNull(flag)) return 0;
    else return INTEGER(flag)[0];
}

static void setDynamicFlag(SEXP item, int flag)
{
    if (flag)
	setAttrib(item, R_DynamicFlagSymbol, ScalarInteger(flag));
}

static SEXP xxnewlist(SEXP item)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxnewlist(item=%p)", item);
#endif    
    PRESERVE_SV(ans = NewList());
    if (item) {
    	int flag = getDynamicFlag(item);
	GrowList(ans, item);
    	setDynamicFlag(ans, flag);
	RELEASE_SV(item);
    }
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", ans, length(ans));
#endif
    return ans;
}

static SEXP xxnewlist2(SEXP item1, SEXP item2)
{
    return xxlist(xxnewlist(item1), item2);
}

static SEXP xxnewlist3(SEXP item1, SEXP item2, SEXP item3)
{
    return xxlist(xxnewlist2(item1, item2), item3);
}

static SEXP xxnewlist4(SEXP item1, SEXP item2, SEXP item3, SEXP item4)
{
    return xxlist(xxnewlist3(item1, item2, item3), item4);
}

static SEXP xxnewlist5(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5)
{
    return xxlist(xxnewlist4(item1, item2, item3, item4), item5);
}

static SEXP xxnewlist6(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6)
{
    return xxlist(xxnewlist5(item1, item2, item3, item4, item5), item6);
}

static SEXP xxnewlist7(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7)
{
    return xxlist(xxnewlist6(item1, item2, item3, item4, item5, item6), item7);
}

static SEXP xxnewlist8(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7, SEXP item8)
{
    return xxlist(xxnewlist7(item1, item2, item3, item4, item5, item6, item7), item8);
}

static SEXP xxnewlist9(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7, SEXP item8, SEXP item9)
{
    return xxlist(xxnewlist8(item1, item2, item3, item4, item5, item6, item7, item8), 
                  item9);
}

static SEXP xxlist(SEXP list, SEXP item)
{
    int flag = getDynamicFlag(list) | getDynamicFlag(item);
#if DEBUGVALS
    Rprintf("xxlist(list=%p, item=%p)", list, item);
#endif
    GrowList(list, item);
    RELEASE_SV(item);
    setDynamicFlag(list, flag);
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", list, length(list));
#endif
    return list;
}

static SEXP xxmarkup(SEXP header, SEXP body, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup(header=%p, body=%p)", header, body);    
#endif
    if (isNull(body)) 
        PRESERVE_SV(ans = allocVector(VECSXP, 0));
    else {
        flag |= getDynamicFlag(body);
	PRESERVE_SV(ans = PairToVectorList(CDR(body)));
	RELEASE_SV(body);
    }
    if (isNull(header))
	setAttrib(ans, R_RdTagSymbol, mkString("LIST"));
    else {
	setAttrib(ans, R_RdTagSymbol, header);
	RELEASE_SV(header);
    }
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxnewcommand(SEXP cmd, SEXP name, SEXP defn, YYLTYPE *lloc)
{
    SEXP ans, prev, defnvals, val, thename, thedefn;
    char buffer[128], *defnBuffer = NULL;
    const char *c;
    int maxarg = 0, vlen, len = 0;
#if DEBUGVALS
    Rprintf("xxnewcommand(cmd=%p, name=%p, defn=%p)", cmd, name, defn);
#endif
    thename = CADR(name);
    /* 
     * Multi-line definitions are parsed as multiple
     * VERB items.  The macro handler can only handle
     * one item, so we concatenate everything into
     * one long string.
     */
    defnvals = CDR(defn);
    while (!isNull(defnvals)) {
        val = CAR(defnvals);
        if (TYPEOF(val) == STRSXP) {
            vlen = (int)strlen(CHAR(STRING_ELT(val, 0)));
            defnBuffer = R_Realloc(defnBuffer, len + vlen + 1, char);
            strncpy(defnBuffer + len, CHAR(STRING_ELT(val, 0)), vlen + 1);
            len += vlen;
        }
        defnvals = CDR(defnvals);
    }
    if (len != 0) {
        PROTECT(thedefn = mkString(defnBuffer)); 
        R_Free(defnBuffer);
    } else
    	PROTECT(thedefn = mkString(""));

    if (warnDups) {
	prev = findVar(installTrChar(STRING_ELT(thename, 0)), parseState.xxMacroList);
    	if (prev != R_UnboundValue && !streql(CHAR(STRING_ELT(cmd,0)), "\\renewcommand")) {
	    snprintf(buffer, sizeof(buffer), _("Macro '%s' previously defined."), 
                 CHAR(STRING_ELT(thename, 0)));
            yyerror(buffer);
        }
    }
    for (c = CHAR(STRING_ELT(thedefn, 0)); *c; c++) {
    	if (*c == '#' && isdigit(*(c+1))) 
    	    maxarg = imax2(maxarg, *(c+1) - '0');
    }
    if (maxarg > 4) {
    	snprintf(buffer, sizeof(buffer), "%s", _("At most 4 arguments are allowed for user defined macros."));
	yyerror(buffer);
    }
    PROTECT(ans = ScalarInteger(USERMACRO + maxarg));
    setAttrib(ans, R_RdTagSymbol, cmd);
    setAttrib(ans, R_DefinitionSymbol, thedefn);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    defineVar(installTrChar(STRING_ELT(thename, 0)), ans, parseState.xxMacroList);
    UNPROTECT(2); /* thedefn, ans */

    PRESERVE_SV(ans);
    RELEASE_SV(cmd);
    RELEASE_SV(name);
    RELEASE_SV(defn);
    return ans;
}

#define START_MACRO -2
#define END_MACRO -3

static bool isComment(SEXP elt)
{
    SEXP a = getAttrib(elt, R_RdTagSymbol);
    return (isString(a) && LENGTH(a) == 1 &&
           streql(CHAR(STRING_ELT(a, 0)), "COMMENT"));
}

static SEXP xxusermacro(SEXP macro, SEXP args, YYLTYPE *lloc)
{
    SEXP ans, value, nextarg;
    int len;
    const char *c, *start ;

#if DEBUGVALS
    Rprintf("xxusermacro(macro=%p, args=%p)", macro, args);
#endif
    len = length(args)-1;
    PRESERVE_SV(ans = allocVector(STRSXP, len + 1));
    value = UserMacroLookup(CHAR(STRING_ELT(macro,0)));
    if (TYPEOF(value) == STRSXP)
    	SET_STRING_ELT(ans, 0, STRING_ELT(value, 0));
    else
    	error(_("No macro definition for '%s'."), CHAR(STRING_ELT(macro,0)));
    nextarg=args;
    for (int i = 0; i < len; i++, nextarg = CDR(nextarg)) {
	if (isNull(CDR(CADR(nextarg)))) {
	    /* This happens for an empty argument {} and for invocation
	       of a macro with zero parameters. In that case, the ""
	       element of ans is not needed but does no harm. */
	    SET_STRING_ELT(ans, i+1, mkChar(""));
	    continue;
	}
	if (isNull(CDR(CDR(CADR(nextarg))))) {
	    /* The common case: argument without newline nor comment.
	       (when the length is 1, there can be no comment) */
	    SEXP s = CADR(CADR(nextarg));
	    if (TYPEOF(s) == STRSXP && LENGTH(s) == 1)
		SET_STRING_ELT(ans, i+1, STRING_ELT(s, 0));
	    else
		error("%s", _("internal error: invalid argument to xxusermacro"));
	    continue;
	}

	/* An argument with a newline or comment or both. Exclude comments and
	   concatenate VERBs from different lines (newline characters are
	   in the VERBs already). */
	CXXR::RAllocStack::Scope rscope;
	size_t ilen = 0;
	for (SEXP si = CDR(CADR(nextarg)); si != R_NilValue; si = CDR(si)) {
	    SEXP stri = CAR(si);
	    if (TYPEOF(stri) == STRSXP && LENGTH(stri) == 1) {
		if (!isComment(stri))
		    ilen += LENGTH(STRING_ELT(stri, 0));
	    } else
		error("%s", _("internal error: invalid argument to xxusermacro"));
	}

	char *str = (char *)R_alloc(ilen + 1, sizeof(char));
	size_t offset = 0;
	for (SEXP si = CDR(CADR(nextarg)); si != R_NilValue; si = CDR(si)) {
	    SEXP stri = CAR(si);
	    if (!isComment(stri)) {
		int nc = LENGTH(STRING_ELT(stri, 0));
		if (nc)
		    memcpy(str + offset, CHAR(STRING_ELT(stri, 0)), nc);
		offset += nc;
	    }
	}
	str[offset] = '\0';
	SET_STRING_ELT(ans, i+1, mkCharCE(str, CE_UTF8));
    }
    RELEASE_SV(args);

    /* Now push the expanded macro onto the input stream, in reverse order */
    xxungetc(END_MACRO);
    start = CHAR(STRING_ELT(ans, 0));
    for (c = start + strlen(start); c > start; c--) {
    	if (c > start + 1 && *(c-2) == '#' && isdigit(*(c-1))) {
    	    int which = *(c-1) - '0';
	    if (which >= len + 1)
		/* currently this won't happen, because the parser gets
		   confused whenever there is invalid number of {} arguments
		   to a user macro */
		error(_("Not enough arguments passed to user macro '%s'"),
		        CHAR(STRING_ELT(macro,0)));
    	    const char *arg = CHAR(STRING_ELT(ans, which));
    	    for (size_t ii = strlen(arg); ii > 0; ii--) xxungetc(arg[ii-1]);
    	    c--;
	} else
    	    xxungetc(*(c-1));
    }
    xxungetc(START_MACRO);

    setAttrib(ans, R_RdTagSymbol, mkString("USERMACRO"));
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setAttrib(ans, R_MacroSymbol, macro);
    RELEASE_SV(macro);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);
#endif
    return ans;
}

static SEXP xxOptionmarkup(SEXP header, SEXP option, SEXP body, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxOptionmarkup(header=%p, option=%p, body=%p)", header, option, body);    
#endif
    flag |= getDynamicFlag(body);
    PRESERVE_SV(ans = PairToVectorList(CDR(body)));
    RELEASE_SV(body);
    setAttrib(ans, R_RdTagSymbol, header);
    RELEASE_SV(header);
    flag |= getDynamicFlag(option);
    setAttrib(ans, R_RdOptionSymbol, option);
    RELEASE_SV(option);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);    
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmarkup2(SEXP header, SEXP body1, SEXP body2, int argcount, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup2(header=%p, body1=%p, body2=%p)", header, body1, body2);        
#endif

    PRESERVE_SV(ans = allocVector(VECSXP, argcount));
    if (!isNull(body1)) {
    	int flag1 = getDynamicFlag(body1);
    	SET_VECTOR_ELT(ans, 0, PairToVectorList(CDR(body1)));
	RELEASE_SV(body1);
    	setDynamicFlag(VECTOR_ELT(ans, 0), flag1);
    	flag |= flag1;
    }
    if (!isNull(body2)) {
    	int flag2;
	if (argcount < 2) error("%s", _("internal error: inconsistent argument count"));
	flag2 = getDynamicFlag(body2);
    	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body2)));    
	RELEASE_SV(body2);
    	setDynamicFlag(VECTOR_ELT(ans, 1), flag2);
    	flag |= flag2;
    }
    setAttrib(ans, R_RdTagSymbol, header);
    RELEASE_SV(header);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmarkup3(SEXP header, SEXP body1, SEXP body2, SEXP body3, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup2(header=%p, body1=%p, body2=%p, body3=%p)", header, body1, body2, body3);        
#endif

    PRESERVE_SV(ans = allocVector(VECSXP, 3));
    if (!isNull(body1)) {
    	int flag1 = getDynamicFlag(body1);
    	SET_VECTOR_ELT(ans, 0, PairToVectorList(CDR(body1)));
	RELEASE_SV(body1);
    	setDynamicFlag(VECTOR_ELT(ans, 0), flag1);
    	flag |= flag1;
    }
    if (!isNull(body2)) {
    	int flag2;
	flag2 = getDynamicFlag(body2);
    	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body2)));    
	RELEASE_SV(body2);
    	setDynamicFlag(VECTOR_ELT(ans, 1), flag2);
    	flag |= flag2;
    }
    if (!isNull(body3)) {
    	int flag3;
	flag3 = getDynamicFlag(body3);
    	SET_VECTOR_ELT(ans, 2, PairToVectorList(CDR(body3)));    
	RELEASE_SV(body3);
    	setDynamicFlag(VECTOR_ELT(ans, 2), flag3);
    	flag |= flag3;
    }    
    setAttrib(ans, R_RdTagSymbol, header);
    RELEASE_SV(header);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static void xxsavevalue(SEXP Rd, YYLTYPE *lloc)
{
    int flag = getDynamicFlag(Rd);
    PRESERVE_SV(parseState.Value = PairToVectorList(CDR(Rd)));
    if (!isNull(parseState.Value)) {
    	setAttrib(parseState.Value, R_ClassSymbol, mkString("Rd"));
    	setAttrib(parseState.Value, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    	setDynamicFlag(parseState.Value, flag);
    }
    RELEASE_SV(Rd);
}

static SEXP xxtag(SEXP item, int type, YYLTYPE *lloc)
{
    setAttrib(item, R_RdTagSymbol, mkString(yytname[YYTRANSLATE(type)]));
    setAttrib(item, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    return item;
}

static void xxWarnNewline(void)
{
    if (parseState.xxNewlineInString) {
	if(wCalls)
	    warning(_("newline within quoted string at %s:%d"), 
		    parseState.xxBasename, parseState.xxNewlineInString);
	else
	    warningcall(R_NilValue,
			_("newline within quoted string at %s:%d"), 
			parseState.xxBasename, parseState.xxNewlineInString);
    }
}


/*----------------------------------------------------------------------------*/


static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need arbitrarily large size, since this is how macros are expanded. */

#define PUSH_BACK(c) do {                  \
	if (npush >= pushsize - 1) {             \
	    int *old = pushbase;              \
            pushsize *= 2;                    \
	    pushbase = (int*) malloc(pushsize*sizeof(int));         \
	    if(!pushbase) error(_("unable to allocate buffer for long macro at line %d"), parseState.xxlineno);\
	    memmove(pushbase, old, npush*sizeof(int));        \
	    if(old != pushback) free(old); }	    \
	pushbase[npush++] = (c);                        \
} while(0)



#define PUSHBACK_BUFSIZE 32

static int pushback[PUSHBACK_BUFSIZE];
static int *pushbase;
static unsigned int npush, pushsize;
static int macrolevel;
static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];


static int xxgetc(void)
{
    int c, oldpos;

    do {
    	if(npush) {    	
    	    c = pushbase[--npush]; 
    	    if (c == START_MACRO) {
    	    	macrolevel++;
    	    	if (macrolevel > 1000) 
    	    	    error("%s", _("macros nested too deeply: infinite recursion?"));
    	    } else if (c == END_MACRO) macrolevel--;
    	} else  c = ptr_getc();
    } while (c == START_MACRO || c == END_MACRO);

    if (!macrolevel) {
	oldpos = prevpos;
	prevpos = (prevpos + 1) % PUSHBACK_BUFSIZE;
	prevbytes[prevpos] = parseState.xxbyteno;
	prevlines[prevpos] = parseState.xxlineno;    
	/* We only advance the column for the 1st byte in UTF-8, so handle later bytes specially */
	if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF) {
	    parseState.xxcolno--;   
	    prevcols[prevpos] = prevcols[oldpos];
	} else 
	    prevcols[prevpos] = parseState.xxcolno;

	if (c == EOF) return R_EOF;

	R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
	R_ParseContext[R_ParseContextLast] = (char) c;

	if (c == '\n') {
	    parseState.xxlineno += 1;
	    parseState.xxcolno = 1;
	    parseState.xxbyteno = 1;
	} else {
	    parseState.xxcolno++;
	    parseState.xxbyteno++;
	}

	if (c == '\t') parseState.xxcolno = ((parseState.xxcolno + 6) & ~7) + 1;

	R_ParseContextLine = parseState.xxlineno;
    }
    /* Rprintf("get %c\n", c); */
    return c;
}

static int xxungetc(int c)
{
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    if (c == END_MACRO) macrolevel++;
    if (!macrolevel) {
    	parseState.xxlineno = prevlines[prevpos];
    	parseState.xxbyteno = prevbytes[prevpos];
    	parseState.xxcolno  = prevcols[prevpos];
    	prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;

    	R_ParseContextLine = parseState.xxlineno;

    	R_ParseContext[R_ParseContextLast] = '\0';
    	/* macOS requires us to keep this non-negative */
    	R_ParseContextLast = (R_ParseContextLast + PARSE_CONTEXT_SIZE - 1) 
		% PARSE_CONTEXT_SIZE;
    }
    if (c == START_MACRO) macrolevel--;
    PUSH_BACK(c);
    /* Rprintf("unget %c;", c); */
    return c;
}

static SEXP makeSrcref(YYLTYPE *lloc, SEXP srcfile)
{
    SEXP val;

    PROTECT(val = allocVector(INTSXP, 6));
    INTEGER(val)[0] = lloc->first_line;
    INTEGER(val)[1] = lloc->first_byte;
    INTEGER(val)[2] = lloc->last_line;
    INTEGER(val)[3] = lloc->last_byte;
    INTEGER(val)[4] = lloc->first_column;
    INTEGER(val)[5] = lloc->last_column;
    setAttrib(val, R_SrcfileSymbol, srcfile);
    setAttrib(val, R_ClassSymbol, mkString("srcref"));
    UNPROTECT(1); /* val */
    return val;
}

static SEXP mkString2(const char *s, size_t len)
{
    SEXP t;
    cetype_t enc = CE_UTF8;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, (int) len, enc));
    UNPROTECT(1); /* t */
    return t;
}


/* Stretchy List Structures : Lists are created and grown using a special */
/* dotted pair.  The CAR of the list points to the last cons-cell in the */
/* list and the CDR points to the first.  The list can be extracted from */
/* the pair by taking its CDR, while the CAR gives fast access to the end */
/* of the list. */


/* Create a stretchy-list dotted pair */

static SEXP NewList(void)
{
    SEXP s = CONS(R_NilValue, R_NilValue);
    SETCAR(s, s);
    return s;
}

/* Add a new element at the end of a stretchy list */

static void GrowList(SEXP l, SEXP s)
{
    SEXP tmp = CONS(s, R_NilValue);
    SETCDR(CAR(l), tmp);
    SETCAR(l, tmp);
}

/*--------------------------------------------------------------------------*/

static void InitSymbols(void)
{
    if (!R_RdTagSymbol)
	R_RdTagSymbol = install("Rd_tag");
    if (!R_RdOptionSymbol)
	R_RdOptionSymbol = install("Rd_option");
    if (!R_DefinitionSymbol)
	R_DefinitionSymbol = install("definition");
    if (!R_DynamicFlagSymbol)
	R_DynamicFlagSymbol = install("dynamicFlag");
    if (!R_MacroSymbol)
	R_MacroSymbol = install("macro");
}

static SEXP ParseRd(ParseStatus *status, SEXP srcfile, bool fragment, SEXP macros)
{
    bool keepmacros = (!isLogical(macros) || asLogical(macros));

    InitSymbols();
    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';

    parseState.xxlineno = 1;
    parseState.xxcolno = 1; 
    parseState.xxbyteno = 1;

    SrcFile = srcfile;

    npush = 0;
    pushbase = pushback;
    pushsize = PUSHBACK_BUFSIZE;
    macrolevel = 0;

    parseState.xxmode = LATEXLIKE; 
    parseState.xxitemType = UNKNOWN;
    parseState.xxbraceDepth = 0;
    parseState.xxinRString = 0;
    parseState.xxNewlineInString = 0;
    parseState.xxinEqn = 0;
    if (fragment) parseState.xxinitvalue = STARTFRAGMENT;
    else	  parseState.xxinitvalue = STARTFILE;

    if (!isEnvironment(macros))
	macros = InstallKeywords();

    PROTECT(macros);
    PROTECT(parseState.xxMacroList = R_NewHashedEnv(macros, 0));
    PROTECT(parseState.mset = R_NewPreciousMSet(50));

    parseState.Value = R_NilValue;

    if (yyparse()) *status = PARSE_ERROR;
    else *status = PARSE_OK;

    if (keepmacros && !isNull(parseState.Value))
	setAttrib(parseState.Value, install("macros"), parseState.xxMacroList);

#if DEBUGVALS
    Rprintf("ParseRd result: %p\n", parseState.Value);    
#endif    
    RELEASE_SV(parseState.Value);
    UNPROTECT(3); /* macros, parseState.xxMacroList, parseState.mset */

    if (pushbase != pushback) free(pushbase);

    return parseState.Value;
}

#include <Rconnections.h>
static Rconnection con_parse;

/* need to handle incomplete last line */
static int con_getc(void)
{
    int c;
    static int last=-1000;

    c = Rconn_fgetc(con_parse);
    if (c == EOF && last != '\n') c = '\n';
    return (last = c);
}

static SEXP R_ParseRd(Rconnection con, ParseStatus *status, SEXP srcfile, bool fragment, SEXP macros)
{
    con_parse = con;
    ptr_getc = con_getc;
    return ParseRd(status, srcfile, fragment, macros);
}

/*----------------------------------------------------------------------------
 *
 *  The Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.  
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.  
 *
 */


/* Special Symbols */
/* Section and R code headers */

struct {
    const char *name;
    int token;
}

/* When adding keywords here, make sure all the handlers 
   are also modified:  checkRd, Rd2HTML, Rd2latex, Rd2txt, any other new ones... */

static keywords[] = {
    /* These sections contain Latex-like text */

    { "\\author",  SECTIONHEADER },
    { "\\concept", SECTIONHEADER },
    { "\\description",SECTIONHEADER },
    { "\\details", SECTIONHEADER },
    { "\\docType", SECTIONHEADER },

    { "\\encoding",SECTIONHEADER },
    { "\\format",  SECTIONHEADER },
    { "\\keyword", SECTIONHEADER },
    { "\\note",    SECTIONHEADER },    
    { "\\references", SECTIONHEADER },

    { "\\section", SECTIONHEADER2 },    
    { "\\seealso", SECTIONHEADER },
    { "\\source",  SECTIONHEADER },
    { "\\title",   SECTIONHEADER },

    /* These sections contain R-like text */

    { "\\examples",RSECTIONHEADER },
    { "\\usage",   RSECTIONHEADER },

    /* These sections contain verbatim text */

    { "\\alias",   VSECTIONHEADER }, 
    { "\\name",    VSECTIONHEADER },
    { "\\synopsis",VSECTIONHEADER }, 
    { "\\Rdversion",VSECTIONHEADER },

    /* These macros take no arguments.  One character non-alpha escapes get the
       same token value */

    { "\\cr",      ESCAPE },
    { "\\dots",    ESCAPE },
    { "\\ldots",   ESCAPE },
    { "\\R",       ESCAPE },    
    { "\\tab",     ESCAPE },

    /* These macros take one LaTeX-like argument. */

    { "\\abbr",    LATEXMACRO },
    { "\\acronym", LATEXMACRO },
    { "\\bold",    LATEXMACRO },
    { "\\cite",    LATEXMACRO },
    { "\\command", LATEXMACRO },
    { "\\dfn",     LATEXMACRO },
    { "\\dQuote",  LATEXMACRO },
    { "\\email",   LATEXMACRO },

    { "\\emph",    LATEXMACRO },    
    { "\\file",    LATEXMACRO },
    { "\\pkg",	   LATEXMACRO },
    { "\\sQuote",  LATEXMACRO },

    { "\\strong",  LATEXMACRO },

    { "\\var",     LATEXMACRO },

    /* These are like SECTIONHEADER/LATEXMACRO, but they change the interpretation of \item */

    { "\\arguments",LISTSECTION },
    { "\\value",   LISTSECTION },

    { "\\describe",DESCRIPTION },
    { "\\enumerate",ITEMIZE },
    { "\\itemize", ITEMIZE },

    { "\\item",    NOITEM }, /* will change to UNKNOWN, ESCAPE, or LATEXMACRO2 depending on context */

    /* These macros take two LaTeX-like arguments. */

    { "\\enc",     LATEXMACRO2 },
    { "\\if",      LATEXMACRO2 },
    { "\\method",  LATEXMACRO2 },
    { "\\S3method",LATEXMACRO2 },
    { "\\S4method",LATEXMACRO2 },
    { "\\tabular", LATEXMACRO2 },
    { "\\subsection", LATEXMACRO2 },

    /* This macro takes one verbatim and one LaTeX-like argument. */

    { "\\href",    VERBLATEX },

    /* This macro takes three LaTeX-like arguments. */

    { "\\ifelse",  LATEXMACRO3 },

    /* These macros take one optional bracketed option and always take 
       one LaTeX-like argument */

    { "\\link",    OPTMACRO },
    { "\\linkS4class", OPTMACRO },

    /* These markup macros require an R-like text argument */

    { "\\code",    RCODEMACRO },
    { "\\dontshow",RCODEMACRO },
    { "\\donttest",RCODEMACRO },
    { "\\dontdiff",RCODEMACRO },
    { "\\testonly",RCODEMACRO },

    /* This macro takes one optional bracketed option and one R-like argument */

    { "\\Sexpr",   SEXPR },

    /* This is just like a VSECTIONHEADER, but it needs SEXPR processing */

    { "\\RdOpts",   RDOPTS },

    /* These macros take one verbatim arg and ignore everything except braces */

    { "\\dontrun", VERBMACRO }, /* at least for now */    
    { "\\env",     VERBMACRO },
    { "\\kbd", 	   VERBMACRO },	
    { "\\option",  VERBMACRO },
    { "\\out",     VERBMACRO },
    { "\\preformatted", VERBMACRO },

    { "\\samp",    VERBMACRO },
    { "\\special", RCODEMACRO },
    { "\\url",     VERBMACRO },
    { "\\verb",    VERBMACRO },

    /* These ones take one or two verbatim args */

    { "\\eqn",     VERBMACRO2 },
    { "\\deqn",    VERBMACRO2 },
    { "\\figure",  VERBMACRO2 },

    /* We parse IFDEF/IFNDEF as markup, not as a separate preprocessor step */ 

    { "#ifdef",    IFDEF },
    { "#ifndef",   IFDEF },
    { "#endif",    ENDIF },

    /* These allow user defined macros */
    { "\\newcommand", NEWCOMMAND },
    { "\\renewcommand", NEWCOMMAND },

    { 0,	   0	      }
    /* All other markup macros are rejected. */
};

/* Record the longest # directive here */
#define DIRECTIVE_LEN 7   

static SEXP InstallKeywords(void)
{
    int num;
    SEXP result, name, val;
    num = sizeof(keywords)/sizeof(keywords[0]);
    PROTECT(result = R_NewHashedEnv(R_EmptyEnv, num));
    for (int i = 0; keywords[i].name; i++) {
        name = install(keywords[i].name);
        PROTECT(val = ScalarInteger(keywords[i].token));
    	defineVar(name, val, result);
	UNPROTECT(1); /* val */
    }
    UNPROTECT(1); /* result */
    return result;
}

static int KeywordLookup(const char *s)
{
    SEXP rec = findVar(install(s), parseState.xxMacroList);
    if (rec == R_UnboundValue) return UNKNOWN;
    else return INTEGER(rec)[0];
}

static SEXP UserMacroLookup(const char *s)
{
    SEXP rec = findVar(install(s), parseState.xxMacroList);
    if (rec == R_UnboundValue) error(_("Unable to find macro %s"), s);
    PROTECT(rec);
    SEXP res = getAttrib(rec, R_DefinitionSymbol);
    UNPROTECT(1); /* rec */
    return res;
}

static void yyerror(const char *s)
{
    static const char *const yytname_translations[] =
    {
    /* the left column are strings coming from bison, the right
       column are translations for users.
       The first YYENGLISH from the right column are English to be translated,
       the rest are to be copied literally.  The #if 0 block below allows xgettext
       to see these.
    */    
#define YYENGLISH 17
	"$undefined",	"input", 	
	"SECTIONHEADER","section header",
	"RSECTIONHEADER","section header",
	"VSECTIONHEADER","section header",
	"LISTSECTION",	"section header",

	"LATEXMACRO",	"macro",
	"LATEXMACRO2",  "macro",
	"LATEXMACRO3",  "macro",
	"RCODEMACRO",	"macro",
	"VERBMACRO",    "macro",
	"VERBMACRO2",	"macro",

	"ESCAPE",	"macro",
	"ITEMIZE",	"macro",
	"IFDEF",	"conditional",
	"SECTIONHEADER2","section header",
	"OPTMACRO",	"macro",

	"DESCRIPTION",	"macro",
	"VERB",		"VERBATIM TEXT",
	0,		0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    static char const yyshortunexpected[] = "unexpected %s";
    static char const yylongunexpected[] = "unexpected %s '%s'";
    char *expecting;
    char ParseErrorMsg[PARSE_ERROR_SIZE];
    SEXP filename;
    char ParseErrorFilename[PARSE_ERROR_SIZE];

    xxWarnNewline();	/* post newline warning if necessary */

    /*
    R_ParseError     = yylloc.first_line;
    R_ParseErrorCol  = yylloc.first_column;
    R_ParseErrorFile = SrcFile;
    */

    if (streqln(s, yyunexpected, sizeof yyunexpected -1)) {
	int translated = FALSE;
    	/* Edit the error message */    
    	expecting = (char *) strstr(s + sizeof yyunexpected -1, yyexpecting);
    	if (expecting) *expecting = '\0';
    	for (int i = 0; yytname_translations[i]; i += 2) {
    	    if (streql(s + sizeof yyunexpected - 1, yytname_translations[i])) {
    	    	if (yychar < 256)
    	    	    snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
			     _(yyshortunexpected), 
			     i/2 < YYENGLISH ? _(yytname_translations[i+1])
			     : yytname_translations[i+1]);
    	    	else
    	    	    snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
			     _(yylongunexpected), 
			     i/2 < YYENGLISH ? _(yytname_translations[i+1])
			     : yytname_translations[i+1], 
			     CHAR(STRING_ELT(yylval, 0)));
    	    	translated = TRUE;
    	    	break;
    	    }
    	}
    	if (!translated) {
    	    if (yychar < 256) 
    		snprintf(ParseErrorMsg, PARSE_ERROR_SIZE, _(yyshortunexpected),
			s + sizeof yyunexpected - 1);
    	    else
    	    	snprintf(ParseErrorMsg, PARSE_ERROR_SIZE, _(yylongunexpected),
			 s + sizeof yyunexpected - 1, CHAR(STRING_ELT(yylval, 0)));
	}
    	if (expecting) {
 	    translated = FALSE;
    	    for (int i = 0; yytname_translations[i]; i += 2) {
    	    	if (streql(expecting + sizeof yyexpecting - 1, yytname_translations[i])) {
    	    	    strcat(ParseErrorMsg, _(yyexpecting));
    	    	    strcat(ParseErrorMsg, i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                    : yytname_translations[i+1]);
    	    	    translated = TRUE;
		    break;
		}
	    }
	    if (!translated) {
	    	strcat(ParseErrorMsg, _(yyexpecting));
	    	strcat(ParseErrorMsg, expecting + sizeof yyexpecting - 1);
	    }
	}
    } else if (streqln(s, yyunknown, sizeof yyunknown-1)) {
    	snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
		"%s '%s'", s, CHAR(STRING_ELT(yylval, 0)));
    } else {
    	snprintf(ParseErrorMsg, PARSE_ERROR_SIZE, "%s", s);
    }
    filename = findVar(install("filename"), SrcFile);
    if (isString(filename) && LENGTH(filename))
    	strncpy(ParseErrorFilename, CHAR(STRING_ELT(filename, 0)), PARSE_ERROR_SIZE - 1);
    else
        ParseErrorFilename[0] = '\0';
    if (wCalls) {
	if (yylloc.first_line != yylloc.last_line)
	    warning("%s:%d-%d: %s", 
		    ParseErrorFilename, yylloc.first_line, yylloc.last_line, ParseErrorMsg);
	else
	    warning("%s:%d: %s", 
		    ParseErrorFilename, yylloc.first_line, ParseErrorMsg);
    } else {
	if (yylloc.first_line != yylloc.last_line)
	    warningcall(R_NilValue, "%s:%d-%d: %s", 
		    ParseErrorFilename, yylloc.first_line, yylloc.last_line, ParseErrorMsg);
	else
	    warningcall(R_NilValue, "%s:%d: %s", 
			ParseErrorFilename, yylloc.first_line, ParseErrorMsg);
    }
}

#define TEXT_PUSH(c) do {		    \
	size_t nc = bp - stext;		    \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
	    nstext *= 2;		    \
	    stext = (char*) malloc(nstext); \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), parseState.xxlineno);\
	    memmove(stext, old, nc);        \
	    if(st1) free(st1);		    \
	    st1 = stext;		    \
	    bp = stext+nc; }		    \
	*bp++ = ((char) c);		    \
} while(0)

static void setfirstloc(void)
{
    yylloc.first_line = parseState.xxlineno;
    yylloc.first_column = parseState.xxcolno;
    yylloc.first_byte = parseState.xxbyteno;
}

static void setlastloc(void)
{
    yylloc.last_line = prevlines[prevpos];
    yylloc.last_column = prevcols[prevpos];
    yylloc.last_byte = prevbytes[prevpos];
}

/* Split the input stream into tokens. */
/* This is the lowest of the parsing levels. */

static int token(void)
{
    int c, lookahead;
    int outsideLiteral = parseState.xxmode == LATEXLIKE || parseState.xxmode == INOPTION || parseState.xxbraceDepth == 0;

    if (parseState.xxinitvalue) {
        yylloc.first_line = 0;
        yylloc.first_column = 0;
        yylloc.first_byte = 0;
        yylloc.last_line = 0;
        yylloc.last_column = 0;
        yylloc.last_byte = 0;
	PRESERVE_SV(yylval = mkString(""));
        c = parseState.xxinitvalue;
    	parseState.xxinitvalue = 0;
    	return(c);
    }

    setfirstloc();    
    c = xxgetc();

    switch (c) {
    	case '%': if (!parseState.xxinEqn) return mkComment(c);
    	    break;
	case '\\':
	    if (!parseState.xxinEqn) {
		lookahead = xxungetc(xxgetc());
		if (isalpha(lookahead) && parseState.xxmode != VERBATIM 
		    /* In R strings, only link or var is allowed as markup */
		    && (lookahead == 'l' || lookahead == 'v' || !parseState.xxinRString)) 
		    return mkMarkup(c);
	    }
	    break;
        case R_EOF:
            if (parseState.xxinRString) {
       		xxWarnNewline();
       		error(_("Unexpected end of input (in %c quoted string opened at %s:%d:%d)"), 
 			parseState.xxinRString, parseState.xxBasename, parseState.xxQuoteLine, parseState.xxQuoteCol);
    	    }
    	    return END_OF_INPUT; 
    	case '#':
    	    if (!parseState.xxinEqn && yylloc.first_column == 1) return mkIfdef(c);
    	    break;
    	case LBRACE:
    	    if (!parseState.xxinRString) {
    	    	parseState.xxbraceDepth++;
    	    	if (outsideLiteral) return c;
    	    }
    	    break;
    	case RBRACE:
    	    if (!parseState.xxinRString) {
    	    	parseState.xxbraceDepth--;
    	    	if (outsideLiteral || parseState.xxbraceDepth == 0) return c;
    	    }
    	    break;
    	case '[':
    	case ']':
    	    if (parseState.xxmode == INOPTION ) return c; 
    	    break;
    } 	    

    switch (parseState.xxmode) {
	case RLIKE:     return mkCode(c);
	case INOPTION:
	case LATEXLIKE: return mkText(c);
	case VERBATIM:  return mkVerb(c);
    }

    return ERROR; /* We shouldn't get here. */
}

#define INITBUFSIZE 128

static int mkText(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0, lookahead;

    while(1) {
    	switch (c) {
    	case '\\': 
    	    lookahead = (char) xxgetc();
    	    if (lookahead == LBRACE || lookahead == RBRACE ||
    	        lookahead == '%' || lookahead == '\\') {
    	    	c = lookahead;
    	    	break;
    	    }
    	    xxungetc(lookahead);
    	    if (isalpha(lookahead)) goto stop;
    	case ']':
    	    if (parseState.xxmode == INOPTION) goto stop;
            break;
    	case '%':
    	case LBRACE:
    	case RBRACE:
    	case R_EOF:
    	    goto stop;
    	}
    	TEXT_PUSH(c);
    	if (c == '\n') goto stop;
    	c = xxgetc();
    };
stop:
    if (c != '\n') xxungetc(c); /* newline causes a break, but we keep it */
    PRESERVE_SV(yylval = mkString2(stext, bp - stext));
    if(st1) free(st1);
    return TEXT;
}

static int mkComment(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;

    do TEXT_PUSH(c);
    while ((c = xxgetc()) != '\n' && c != R_EOF);

    xxungetc(c);

    PRESERVE_SV(yylval = mkString2(stext, bp - stext));
    if(st1) free(st1);
    return COMMENT;
}

#define EAT_DASHES(n_var) do {				\
	for (c = xxgetc(); c == '-'; c = xxgetc()) {	\
	    n_var++;					\
	    TEXT_PUSH(c);				\
	}						\
    } while (0)

#define EAT_CHARS_TO_DELIM_OR_EOF(delim) do {	\
	while (c != delim && c != R_EOF) {	\
	    TEXT_PUSH(c);			\
	    c = xxgetc();			\
	}					\
    } while (0)

static int closingRawStringDelim(int c)
{
    switch(c) {
    case '(': return ')';
    case '{': return '}';
    case '[': return ']';
    case '|': return '|';
    default:  return 0;
    }
}

static int mkCode(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;

    /* Avoid double counting initial braces */
    if (c == LBRACE && !parseState.xxinRString) parseState.xxbraceDepth--;
    if (c == RBRACE && !parseState.xxinRString) parseState.xxbraceDepth++; 

    while(1) {
	/* handle a raw string */
	if (parseState.xxinRString == 0 && (c == 'r' || c == 'R')) {
    	    int lookahead = xxgetc();
	    if (lookahead == '"' || lookahead == '\'') {
		TEXT_PUSH(c);
		int quote = lookahead;
		parseState.xxinRString = quote;
    	    	parseState.xxQuoteLine = parseState.xxlineno;
    	    	parseState.xxQuoteCol  = parseState.xxcolno;
		TEXT_PUSH(quote);
		int ndash = 0;
		EAT_DASHES(ndash);
		int delim = closingRawStringDelim(c);
		if (delim != 0) {
		    int done = FALSE;
		    do {
			EAT_CHARS_TO_DELIM_OR_EOF(delim);
			if (c == delim) {
			    TEXT_PUSH(c);
			    int nndash = 0;
			    EAT_DASHES(nndash);
			    if (nndash == ndash && c == quote)
				done = TRUE; // close quote is handled below
			}
			else done = TRUE; // EOF; move on
		    } while (! done);
		}
	    }
	    else xxungetc(lookahead);
	}

	int escaped = 0;
    	if (c == '\\') {
    	    int lookahead = xxgetc();
    	    if (lookahead == '\\' || lookahead == '%') {
    	         c = lookahead;
    	         escaped = 1;
    	    } else xxungetc(lookahead);
    	}
    	if ((!escaped && c == '%') || c == R_EOF) break;
    	if (parseState.xxinRString) {
    	    /* This stuff is messy, because there are two levels of escaping:
    	       The Rd escaping and the R code string escaping. */
    	    if (c == '\\') {
    		int lookahead = xxgetc();
    		if (lookahead == '\\') { /* This must be the 3rd backslash */
    		    lookahead = xxgetc();
    		    if (lookahead == parseState.xxinRString || lookahead == '\\') {	
    	    	    	TEXT_PUSH(c);
    	    	    	c = lookahead;
    	    	    	escaped = 1;
    	    	    } else {
    	    	    	xxungetc(lookahead); /* put back the 4th char */
    	    	    	xxungetc('\\');	     /* and the 3rd */
    	    	    }
    	    	} else if (lookahead == parseState.xxinRString) { /* There could be one or two before this */
    	    	    TEXT_PUSH(c);
    	    	    c = lookahead;
    	    	    escaped = 1;
    	    	} else if (!escaped && (lookahead == 'l' || lookahead == 'v')) { 
    	    	    /* assume \link or \var; this breaks vertical tab, but does anyone ever use that? */
    	    	    xxungetc(lookahead);
    	    	    break;
    	    	} else xxungetc(lookahead);
    	    }
    	    if (!escaped && c == parseState.xxinRString)
    	    	parseState.xxinRString = 0;
    	} else {
    	    if (c == '#') {
    	    	do {
    	    	    int escaped = 0;
    	    	    TEXT_PUSH(c);
    	    	    c = xxgetc();
    	    	    if (c == '\\') {
		        int lookahead = xxgetc();
		        if (lookahead == '\\' || lookahead == '%' || lookahead == LBRACE || lookahead == RBRACE) {
		            c = lookahead;
		            escaped = 1;
		        } else xxungetc(lookahead);
    		    }
    	    	    if (c == LBRACE && !escaped) parseState.xxbraceDepth++;
    	    	    else if (c == RBRACE && !escaped) parseState.xxbraceDepth--;
    	    	} while (c != '\n' && c != R_EOF && parseState.xxbraceDepth > 0);
    	    	if (c == RBRACE && !escaped) parseState.xxbraceDepth++; /* avoid double counting */
    	    }
    	    if (c == '\'' || c == '"' || c == '`') {
    	    	parseState.xxinRString = c;
    	    	parseState.xxQuoteLine = parseState.xxlineno;
    	    	parseState.xxQuoteCol  = parseState.xxcolno;
    	    } else if (c == '\\' && !escaped) {
    	    	int lookahead = xxgetc();
    	    	if (lookahead == LBRACE || lookahead == RBRACE) {
		    c = lookahead;
		} else if (isalpha(lookahead)) {
    	    	    xxungetc(lookahead);
    	    	    c = '\\';
    	    	    break;
    	    	} else {
    	    	    TEXT_PUSH('\\');
    	    	    c = lookahead;
    	    	}
    	    } else if (c == LBRACE) {
    	    	parseState.xxbraceDepth++;
    	    } else if (c == RBRACE) {
    	    	if (parseState.xxbraceDepth == 1) break;
    	    	else parseState.xxbraceDepth--;
    	    } else if (c == R_EOF) break;
    	}
    	TEXT_PUSH(c);
    	if (c == '\n') {
    	    if (parseState.xxinRString && !parseState.xxNewlineInString) 
    	    	parseState.xxNewlineInString = parseState.xxlineno-1;
    	    break;
    	}
    	c = xxgetc();
    }
    if (c != '\n') xxungetc(c);
    PRESERVE_SV(yylval = mkString2(stext, bp - stext));
    if(st1) free(st1);
    return RCODE; 
}

static int mkMarkup(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int retval = 0, attempt = 0;

    TEXT_PUSH(c);
    while (isalnum((c = xxgetc()))) TEXT_PUSH(c);

    while (attempt++ < 2) {
    	/* character escapes are processed as text, not markup */
    	if (bp == stext+1) {
    	    TEXT_PUSH(c);
    	    TEXT_PUSH('\0');
    	    retval = TEXT;
    	    c = xxgetc();
    	    break;
    	} else {
    	    TEXT_PUSH('\0');
    	    retval = KeywordLookup(stext);
    	    if (retval == UNKNOWN && attempt == 1) { /* try again, non-digits only */
    	    	bp--; 				     /* pop the \0 */
    	        while (isdigit(*(bp-1))) {
            	    xxungetc(c);
    	            c = *(--bp);                     /* pop the last letter into c */
            	}
            } else {
            	if (retval == NOITEM) 
    	    	    retval = parseState.xxitemType;
    	    	break;
    	    }
        }
    }
    PRESERVE_SV(yylval = mkString2(stext, bp - stext - 1));
    if(st1) free(st1);
    xxungetc(c);
    return retval;
}

static int mkIfdef(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int retval;

    TEXT_PUSH(c);
    while (isalpha((c = xxgetc())) && bp - stext <= DIRECTIVE_LEN) TEXT_PUSH(c);
    TEXT_PUSH('\0');
    xxungetc(c);

    retval = KeywordLookup(stext);
    PRESERVE_SV(yylval = mkString2(stext, bp - stext - 1));

    switch (retval) {
    case ENDIF:  /* eat chars to the end of the line */
    	do { c = xxgetc(); }
    	while (c != '\n' && c != R_EOF);
    	break;
    case UNKNOWN:
	RELEASE_SV(yylval);
    	bp--; bp--;
    	for (; bp > stext; bp--) 
    	    xxungetc(*bp);
    	switch (parseState.xxmode) {
    	case RLIKE:     
    	    retval = mkCode(*bp);
    	    break;
    	case INOPTION:
    	case LATEXLIKE:
    	    retval = mkText(*bp);
    	    break;
    	case VERBATIM:
    	    retval = mkVerb(*bp);
    	    break;
	}
	break;
    }
    if(st1) free(st1);
    return retval;
}

static int mkVerb(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;

    /* Avoid double counting initial braces */
    if (c == LBRACE) parseState.xxbraceDepth--;
    if (c == RBRACE) parseState.xxbraceDepth++;     

    while(1) {
    	int escaped = 0;
        if (c == '\\') {
            int lookahead = xxgetc();
            if (lookahead == '\\' || lookahead == '%' || lookahead == LBRACE || lookahead == RBRACE) {
		escaped = 1;
		if (parseState.xxinEqn) TEXT_PUSH(c);
		c = lookahead;
	    } else xxungetc(lookahead);
        }
        if (c == R_EOF) break;
        if (!escaped) {
    	    if (c == '%' && !parseState.xxinEqn) break;
	    else if (c == LBRACE) parseState.xxbraceDepth++;
    	    else if (c == RBRACE) {
	    	if (parseState.xxbraceDepth == 1) break;
	    	else parseState.xxbraceDepth--;
	    }
	}
    	TEXT_PUSH(c);
    	if (c == '\n') break;
    	c = xxgetc();
    };
    if (c != '\n') xxungetc(c);
    PRESERVE_SV(yylval = mkString2(stext, bp - stext));
    if(st1) free(st1);
    return VERB;  
}

static int yylex(void)
{
    int tok = token();

    if (parseState.xxDebugTokens) {
        Rprintf("%d:%d: %s", yylloc.first_line, yylloc.first_column, yytname[YYTRANSLATE(tok)]);
    	if (parseState.xxinRString) Rprintf("(in %c%c)", parseState.xxinRString, parseState.xxinRString);
    	if (tok > 255 && tok != END_OF_INPUT) 
    	    Rprintf(": %s", CHAR(STRING_ELT(yylval, 0)));
	Rprintf("\n");
    }
    setlastloc();
    return tok;
}

static void PutState(ParseState *state) {
    state->xxinRString = parseState.xxinRString;
    state->xxQuoteLine = parseState.xxQuoteLine;
    state->xxQuoteCol = parseState.xxQuoteCol;
    state->xxinEqn = parseState.xxinEqn;
    state->xxNewlineInString = parseState.xxNewlineInString;
    state->xxlineno = parseState.xxlineno;
    state->xxbyteno = parseState.xxbyteno;
    state->xxcolno = parseState.xxcolno;
    state->xxmode = parseState.xxmode;
    state->xxitemType = parseState.xxitemType;
    state->xxbraceDepth = parseState.xxbraceDepth;
    state->xxDebugTokens = parseState.xxDebugTokens;
    state->xxBasename = parseState.xxBasename;
    state->Value = parseState.Value;
    state->xxinitvalue = parseState.xxinitvalue;
    state->xxMacroList = parseState.xxMacroList;
    state->prevState = parseState.prevState;
}

static void UseState(ParseState *state) {
    parseState.xxinRString = state->xxinRString;
    parseState.xxQuoteLine = state->xxQuoteLine;
    parseState.xxQuoteCol = state->xxQuoteCol;
    parseState.xxinEqn = state->xxinEqn;
    parseState.xxNewlineInString = state->xxNewlineInString;
    parseState.xxlineno = state->xxlineno;
    parseState.xxbyteno = state->xxbyteno;
    parseState.xxcolno = state->xxcolno;
    parseState.xxmode = state->xxmode;
    parseState.xxitemType = state->xxitemType;
    parseState.xxbraceDepth = state->xxbraceDepth;
    parseState.xxDebugTokens = state->xxDebugTokens;
    parseState.xxBasename = state->xxBasename;
    parseState.Value = state->Value;
    parseState.xxinitvalue = state->xxinitvalue;
    parseState.xxMacroList = state->xxMacroList;
    parseState.prevState = state->prevState;
}

static void PushState(void) {
    if (busy) {
    	ParseState *prev = (ParseState*) malloc(sizeof(ParseState));
	if (prev == NULL) error("%s", _("unable to allocate in PushState"));
    	PutState(prev);
    	parseState.prevState = prev;
    } else 
        parseState.prevState = NULL;  
    busy = true;
}

static void PopState(void) {
    if (parseState.prevState) {
    	ParseState *prev = parseState.prevState;
    	UseState(prev);
    	free(prev);
    } else
    	busy = false;
}

/* "do_parseRd" 

 .External2(C_parseRd,file, srcfile, encoding, verbose, basename, warningCalls, macros, warndups)
 If there is text then that is read and the other arguments are ignored.
*/

SEXP parseRd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = CDR(args);

    SEXP s = R_NilValue, source;
    Rconnection con;
    bool wasopen;
    int ifile;
    ParseStatus status;
    SEXP macros;

#if DEBUGMODE
    yydebug = 1;
#endif 

    R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';

    PushState();

    ifile = asInteger(CAR(args));                       args = CDR(args);

    con = getConnection(ifile);
    wasopen = con->isopen;
    source = CAR(args);					args = CDR(args);
    /* encoding is unused */
    args = CDR(args);
    if(!isLogical(CAR(args)) || LENGTH(CAR(args)) != 1)
    	error(_("invalid '%s' value"), "verbose");
    parseState.xxDebugTokens = asInteger(CAR(args));		args = CDR(args);
    parseState.xxBasename = CHAR(STRING_ELT(CAR(args), 0));	args = CDR(args);
    bool fragment = asBool(CAR(args));				args = CDR(args);
    wCalls = asLogicalNoNA(CAR(args), "warningCalls");		args = CDR(args);
    macros = CAR(args);						args = CDR(args);
    warnDups = asBool(CAR(args));

    if (ifile >= 3) {/* file != "" */
	if(!wasopen) {
	    if(!con->open(con)) error("%s", _("cannot open the connection"));
	}
	if(!con->canread) error("%s", _("cannot read from this connection"));
	/* Set up a context which will close the connection on error */
	try {
	s = R_ParseRd(con, &status, source, fragment, macros);
	} catch (...) {
        if (!wasopen && con->isopen)
            con->close(con);
        throw;
	}
	PopState();
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    else {
      PopState();
      error("%s", _("invalid Rd file"));
    }
    return s;
}

/* "do_deparseRd" 

 .External2(C_deparseRd, element, state)
*/

SEXP deparseRd(SEXP e, SEXP state)
{
    SEXP result;
    int  outlen, *statevals, quoteBraces;
    bool inRComment;
    const char *c;
    char *outbuf, *out, lookahead;
    bool escape;

    if(!isString(e) || LENGTH(e) != 1) 
    	error("%s", _("'deparseRd' only supports deparsing character elements"));
    e = STRING_ELT(e, 0);

    if(!isInteger(state) || LENGTH(state) != 5) error("%s", _("bad state"));

    PushState();

    parseState.xxbraceDepth = INTEGER(state)[0];
    parseState.xxinRString = INTEGER(state)[1];
    parseState.xxmode = INTEGER(state)[2];
    parseState.xxinEqn = INTEGER(state)[3];
    quoteBraces = INTEGER(state)[4];

    if (parseState.xxmode != LATEXLIKE && parseState.xxmode != RLIKE && parseState.xxmode != VERBATIM && parseState.xxmode != COMMENTMODE 
     && parseState.xxmode != INOPTION  && parseState.xxmode != UNKNOWNMODE) {
        PopState();
    	error(_("bad text mode %d in 'deparseRd'"), parseState.xxmode);
    }

    for (c = CHAR(e), outlen=0; *c; c++) {
    	outlen++;
    	/* any special char might be escaped */
    	if (*c == '{' || *c == '}' || *c == '%' || *c == '\\') outlen++;
    }
    out = outbuf = (char*) R_chk_calloc(outlen+1, sizeof(char));
    inRComment = false;
    for (c = CHAR(e); *c; c++) {
    	escape = false;
    	if (parseState.xxmode != UNKNOWNMODE) {
	    switch (*c) {
	    case '\\':
		if (parseState.xxmode == RLIKE && parseState.xxinRString) {
		    lookahead = *(c+1);
		    if (lookahead == '\\' || lookahead == parseState.xxinRString || lookahead == 'l') 
		    	escape = true;
		    break;
		}          /* fall through to % case for non-strings... */    
	    case '%':
		if (parseState.xxmode != COMMENTMODE && !parseState.xxinEqn)
		    escape = true;
		break;
	    case LBRACE:
	    case RBRACE:
		if (quoteBraces || parseState.xxmode == LATEXLIKE)
		    escape = true;
		else if (!parseState.xxinRString && !parseState.xxinEqn && (parseState.xxmode == RLIKE || parseState.xxmode == VERBATIM)) {
		    if (*c == LBRACE) parseState.xxbraceDepth++;
		    else if (parseState.xxbraceDepth <= 0) escape = true;
		    else parseState.xxbraceDepth--;
		}
		break;
	    case '\'':
	    case '"':
	    case '`':
	    	if (parseState.xxmode == RLIKE) {
		    if (parseState.xxinRString) {
			if (parseState.xxinRString == *c) parseState.xxinRString = 0;
		    } else if (!inRComment) parseState.xxinRString = *c;
		}
		break;
	    case '#':
	    	if (parseState.xxmode == RLIKE && !parseState.xxinRString) 
	    	    inRComment = true;
	    	break;
	    case '\n':
	    	inRComment = false;
	    	break;
	    }
	}
    	if (escape)
    	    *out++ = '\\';
    	*out++ = *c;
    }
    *out = '\0';
    PROTECT(result = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, ScalarString(mkChar(outbuf)));
    SET_VECTOR_ELT(result, 1, duplicate(state));
    R_chk_free(outbuf);

    statevals = INTEGER( VECTOR_ELT(result, 1) );
    statevals[0] = parseState.xxbraceDepth;
    statevals[1] = parseState.xxinRString;

    PopState();

    UNPROTECT(1); /* result */
    return result;
}

