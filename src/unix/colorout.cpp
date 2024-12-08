/*
 *  Below code was created based on colorout.c from colorout R package.
 *  Copyright (C) 2024 Lukasz Daniel
 *
 *  You can distribute this file under the terms of
 *  the GNU General Public License (GPL >= 2).
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

// Original copyright disclaimer from the colorout package.
/* This file is part of colorout R package
 **
 ** It is distributed under the GNU General Public License.
 ** See the file ../LICENSE for details.
 **
 ** Authors:
 ** (c) 2011-2014 Jakson Aquino: jalvesaq@gmail.com
 ** (c)      2014 Dominique-Laurent Couturier: dlc48@medschl.cam.ac.uk
 **
 ***************************************************************/
// Source code for the colorout package is available at:
// https://github.com/jalvesaq/colorout

#include <R_ext/Minmax.h>
#include <string>
#include <array>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <R_ext/Visibility.h>
#include <R_ext/RStartup.h> // for otype_t

#define too_small 1e-12
#define hlzero 0

#if CXXR_FALSE
typedef struct pattern {
    char *ptrn;
    char *compiled;
    int cpldsize;
    int matchsize;
    char *color;
    int crsize;
    struct pattern * next;
} pattern_t;

pattern_t *P = NULL;
#endif

#define crnormal "\033[0;38;5;40m"
#define crnumber "\033[0;38;5;214m"
#define crnegnum "\033[0;38;5;209m"
#define crdate "\033[0;38;5;179m"
#define crstring "\033[0;38;5;85m"
#define crconst "\033[0;38;5;35m"
#define crstderr "\033[0;38;5;213m"
#define crwarn "\033[0;1;38;5;1m"
#define crerror "\033[0;48;5;1;38;5;15m"
#define crlogicalT "\033[0;38;5;78m"
#define crlogicalF "\033[0;38;5;203m"
#define crinfinite "\033[0;38;5;39m"
#define crindex "\033[0;38;5;30m"
#define crzero "\033[0;38;5;226m"
namespace
{
constexpr size_t normalsize = std::char_traits<char>::length(crnormal);
constexpr size_t numbersize = std::char_traits<char>::length(crnumber);
constexpr size_t negnumsize = std::char_traits<char>::length(crnegnum);
constexpr size_t datesize = std::char_traits<char>::length(crdate);
constexpr size_t stringsize = std::char_traits<char>::length(crstring);
constexpr size_t constsize = std::char_traits<char>::length(crconst);
constexpr size_t logicalTsize = std::char_traits<char>::length(crlogicalT);
constexpr size_t logicalFsize = std::char_traits<char>::length(crlogicalF);
constexpr size_t infinitesize = std::char_traits<char>::length(crinfinite);
constexpr size_t indexsize = std::char_traits<char>::length(crindex);
constexpr size_t zerosize = std::char_traits<char>::length(crzero);

constexpr size_t maxsize = std::max({logicalTsize, logicalFsize, constsize, infinitesize}) + 6 + normalsize;
std::array<char, maxsize> s_piece;
#define piece s_piece.data()
} // anonymous namespace

static bool isletter(const char b)
{
    return (b >= 'A' && b <= 'Z') || (b >= 'a' && b <= 'z');
}

static bool isword(const char * b, int i, int len)
{
    int is_letter_preceeding = i > 0 ? isletter(b[i-1]) : 0;
    int is_letter_following = isletter(b[i+len]);
    return !is_letter_preceeding && !is_letter_following;
}

static bool iswhitespace(const char b)
{
    /* space or horizontal tab, new line, vertical tab, form feed, carriage return */
    return b == 32 || (b >= 9 && b <= 13);
}

static bool isnumber(const char * b, int i, int len)
{
    if(i > 0 && !iswhitespace(b[i-1]) && b[i-1] != '-')
        return 0;

    int l = len;
    if(l > (i + 5))
        l = i + 5;
    i++;
    while(i < l){
        if(((b[i] > 0 && b[i] < '0') || (b[i] > '9' && b[i] < 'A') || (b[i] > 'Z' && b[i] < 'a') || (b[i] > 'z' && b[i] > 0)) &&
                b[i] != '.' && b[i] != ',' &&
                !(b[i] == 'e' && (i + 2) < len && (b[i+1] == '-' || b[i+1] == '+') && b[i+2] >= '0' && b[i+2] <= '9') &&
                !(b[i-1] == 'e' && (b[i] == '-' || b[i] == '+')))
            break;
        if((b[i] < '0' || b[i] > '9') &&
                b[i] != '.' && b[i] != ',' &&
                !(b[i] == 'e' && (i + 2) < len && (b[i+1] == '-' || b[i+1] == '+') && b[i+2] >= '0' && b[i+2] <= '9') &&
                !(b[i-1] == 'e' && (b[i] == '-' || b[i] == '+')))
            return 0;
        i++;
    }
    return 1;
}


static bool iszero(const char * b, int i, int len)
{
    char *charnum, *stopstring;
    double x;
    int j;
    j = i;
    charnum = (char*)calloc(sizeof(char),len+1);
    while(i<len){
        charnum[i-j] = b[i];
        i++;
    }
    x = strtod(charnum, &stopstring);
    free(charnum);
    if (x < too_small)
        return 1;
    else
        return 0;
}

static bool isindex(const char * b, int i, int len)
{
    // Element of unnamed list
    if(i > 1 && b[i-2] == '[')
        return 0;

    if(b[i] == ','){
        i++;
        if(!((b[i] >= '0' && b[i] <= '9') || b[i] == ' '))
            return 0;
    }
    while(i < len && ((b[i] >= '0' && b[i] <= '9') || b[i] == ' '))
        i++;

    // Vector or matrix index?
    // Also check that the previous index is a digit
    if ((b[i] == ']' && b[i-1] >= '0' && b[i-1] <= '9') || (b[i] == ',' && b[i+1] == ']' && b[i-1] >= '0' && b[i-1] <= '9'))
        return 1;
    else
        return 0;
}



static bool isdate(const char * b, int i, int len)
{
    if((len-i)>9){
        /* YYYYxMMxDD or YYYYxDDxMM */
        if(b[i+4] == b[i+7] && b[i+4] == '-'){
            if(b[i] >= '1' && b[i] <= '9' &&
                    b[i+1] >= '0' && b[i+1] <= '9' &&
                    b[i+2] >= '0' && b[i+2] <= '9' &&
                    b[i+3] >= '0' && b[i+3] <= '9' &&
                    b[i+5] >= '0' && b[i+5] <= '3' &&
                    b[i+6] >= '0' && b[i+6] <= '9' &&
                    b[i+8] >= '0' && b[i+8] <= '3' &&
                    b[i+9] >= '0' && b[i+9] <= '9')
                return 1;
            else
                return 0;
            /* DDxMMxYYYY or MMxDDxYYYY */
        } else if(b[i+2] == b[i+5] && b[i+2] == '-'){
            if(b[i] >= '0' && b[i] <= '3' &&
                    b[i+1] >= '0' && b[i+1] <= '9' &&
                    b[i+3] >= '0' && b[i+3] <= '3' &&
                    b[i+4] >= '0' && b[i+4] <= '9' &&
                    b[i+6] >= '1' && b[i+6] <= '9' &&
                    b[i+7] >= '0' && b[i+7] <= '9' &&
                    b[i+8] >= '0' && b[i+8] <= '9' &&
                    b[i+9] >= '0' && b[i+9] <= '9')
                return 1;
            else
                return 0;
        } else
            return 0;
        /* wrong length */
    } else
        return 0;
}


static bool istime(const char * b, int i, int len)
{
    if((len-i)>7){
        /* HH:MM:SS */
        if(b[i+2] == ':' && b[i+5] == ':'){
            if(b[i] >= '0' && b[i] <= '9' &&
                    b[i+1] >= '0' && b[i+1] <= '9' &&
                    b[i+3] >= '0' && b[i+3] <= '5' &&
                    b[i+4] >= '0' && b[i+4] <= '9' &&
                    b[i+6] >= '0' && b[i+6] <= '5' &&
                    b[i+7] >= '0' && b[i+7] <= '9')
                return 1;
            else
                return 0;
        } else
            return 0;
        /* wrong length */
    } else
        return 0;
}

#if CXXR_FALSE
static int ispattern(const char * b, int i, int len, const pattern_t *p)
{
    int n = 0;
    int j = i;
    if((len-i) >= p->matchsize){
        while(n < p->cpldsize){
            if(p->compiled[n] == b[j]){
                n++;
                j++;
            } else if(p->compiled[n] == '\x02' && b[j] >= p->compiled[n+1] && b[j] <= p->compiled[n+2]){
                n += 3;
                j++;
            } else if(p->compiled[n] == '\x03'){
                n++;
                if(p->compiled[n] == '\x02' && b[j] >= p->compiled[n+1] && b[j] <= p->compiled[n+2]){
                    while(b[j] >= p->compiled[n+1] && b[j] <= p->compiled[n+2])
                        j++;
                    n += 3;
                } else {
                    while(p->compiled[n] == b[j])
                        j++;
                    n++;
                }
            } else {
                break;
            }
        }
    }
    if(n == p->cpldsize && (j - i) >= p->matchsize)
        return (j - i);
    return 0;
}

void colorout_UnsetZero(void)
{
    hlzero = 0;
}

void colorout_SetZero(double *zr)
{
    hlzero = 1;
    too_small = *zr;
}

SEXP colorout_ListPatterns(void)
{
    int n = 0;
    pattern_t *p = P;

    while(p){
        n++;
        /*
        printf("pattern = %s\nmatch size = %d\ncompiled = ", p->ptrn, p->matchsize);
        for(int i = 0; i < strlen(p->compiled); i++)
            if(p->compiled[i] == '\x02')
                printf("%s2\033[0m", crnumber);
            else if (p->compiled[i] == '\x03')
                printf("%s3\033[0m", crnumber);
            else
                printf("%c", p->compiled[i]);
        printf("\ncompiled size = %d\n", p->cpldsize);
        */
        p = p->next;
    }

    SEXP res = PROTECT(allocVector(STRSXP, n));
    SEXP nms = PROTECT(allocVector(STRSXP, n));

    p = P;
    int i = 0;
    while(p){
        SET_STRING_ELT(nms, i, mkChar(p->ptrn));
        SET_STRING_ELT(res, i, mkChar(p->color));
        p = p->next;
        i++;
    }
    setAttrib(res, R_NamesSymbol, nms);
    UNPROTECT(2);
    return res;
}

void colorout_DeletePattern(char **pattern)
{
    pattern_t *p = P;
    pattern_t *prev = NULL;
    pattern_t *next = NULL;
    while(p){
        next = p->next;
        if(strcmp(*pattern, p->ptrn) == 0){
            if(prev)
                prev->next = p->next;
            free(p->ptrn);
            free(p->compiled);
            free(p->color);
            free(p);
            if(p == P)
                P = next;
        } else {
            prev = p;
        }
        p = next;
    }
}

void colorout_AddPattern(char **pattern, char **color)
{
    pattern_t *p = (pattern_t*)calloc(1, sizeof(pattern_t));
    p->ptrn = (char*)malloc(strlen(*pattern)+1);
    strcpy(p->ptrn, *pattern);

    // Compile the pattern for faster comparison
    p->compiled = (char*)calloc(1, strlen(*pattern)+1);
    int i = 0;
    int j = 0;
    int l = strlen(p->ptrn);
    p->matchsize = 0;
    while(i < l){
        if(i > 0 && p->ptrn[i] == '*' && p->ptrn[i-1] != '\\'){
            // Put x03 before the pattern to be repeated
            if(i > 4 && p->ptrn[i-5] == '[' && p->ptrn[i-3] == '-' && p->ptrn[i-1] == ']'){
                p->compiled[j] = p->compiled[j-1];
                p->compiled[j-1] = p->compiled[j-2];
                p->compiled[j-2] = p->compiled[j-3];
                p->compiled[j-3] = '\x03';
                j++;
                i++;
            } else {
                p->compiled[j] = p->compiled[j-1];
                p->compiled[j-1] = '\x03';
                j++;
                i++;
            }
        } else if(i < (l - 4) && p->ptrn[i] == '[' && p->ptrn[i + 2] == '-' && p->ptrn[i + 4] == ']'){
            p->compiled[j] = '\x02';
            p->compiled[j+1] = p->ptrn[i+1];
            p->compiled[j+2] = p->ptrn[i+3];
            i += 5;
            j += 3;
            p->matchsize++;
        } else {
            p->compiled[j] = p->ptrn[i];
            i++;
            j++;
            p->matchsize++;
        }
    }
    p->cpldsize = strlen(p->compiled);

    p->color = (char*)malloc(strlen(*color)+1);
    strcpy(p->color, *color);
    p->crsize = strlen(p->color);

    if(P)
        p->next = P;

    P = p;
}

static int max(const int a, const int b)
{
    if(a > b)
        return a;
    return b;
}

static void alloc_piece(void)
{
    int maxsize;

    maxsize = max(logicalTsize, logicalFsize);
    maxsize = max(maxsize, constsize);
    maxsize = max(maxsize, infinitesize);

    if(piece != NULL)
        free(piece);
    piece = (char*)calloc(1, maxsize + 6 + normalsize);
}

void colorout_SetColors(char **normal, char **number, char **negnum,
        char **datenum, char **string, char **constant, char **stderror,
        char **warn, char **error, char **logicalT, char **logicalF,
        char **infinite, char **index, char **zero, int *verbose)
{
    strncpy(crnormal, normal[0], 63);
    strncpy(crnumber, number[0], 63);
    strncpy(crnegnum, negnum[0], 63);
    strncpy(crdate,   datenum[0], 63);
    strncpy(crstring, string[0], 63);
    strncpy(crconst,  constant[0], 63);
    strncpy(crstderr, stderror[0], 63);
    strncpy(crwarn,   warn[0], 63);
    strncpy(crerror,  error[0], 63);
    strncpy(crlogicalT, logicalT[0], 63);
    strncpy(crlogicalF, logicalF[0], 63);
    strncpy(crinfinite, infinite[0], 63);
    strncpy(crindex, index[0], 63);
    strncpy(crzero,     zero[0], 63);

    normalsize = strlen(crnormal);
    numbersize = strlen(crnumber);
    negnumsize = strlen(crnegnum);
    datesize = strlen(crdate);
    stringsize = strlen(crstring);
    constsize = strlen(crconst);
    logicalTsize = strlen(crlogicalT);
    logicalFsize = strlen(crlogicalF);
    infinitesize = strlen(crinfinite);
    indexsize = strlen(crindex);
    zerosize     = strlen(crzero);

    alloc_piece();

    if(*verbose){
        printf("%snormal\033[0m ", crnormal);
        if(hlzero)
            printf("%sx[x<=-%g]\033[0m %sx[abs(x)<%g]\033[0m %sx[x>=%g]\033[0m ",
                    crnegnum, too_small, crzero, too_small, crnumber, too_small);
        else
            printf("%sx[x<0]\033[0m %sx[x>=0]\033[0m ", crnegnum, crnumber);

        printf("%s19/01/2038 03:14:07\033[0m %s\"string\"\033[0m\n%sNA/NaN/NULL\033[0m %sFALSE\033[0m %sTRUE\033[0m %sInf\033[0m %s[index]\033[0m %sstderror\033[0m %swarn\033[0m %serror\033[0m\n",
                crdate, crstring, crconst, crlogicalF, crlogicalT, crinfinite, crindex, crstderr, crwarn, crerror);
    }
}
#endif

char *colorout_make_bigger(char *ptr, int *len)
{
    char *nnbuf;
    *len = *len + 1024;
    nnbuf = (char*)calloc(1, *len);
    strcpy(nnbuf, ptr);
    free(ptr);
    *len = *len - 64;
    return(nnbuf);
}


/* This function color prints the contents of 'buf', of length 'len' and type 'otype' */
attribute_hidden void colorout_R_WriteConsoleEx(const char *buf, int len, otype_t otype)
{
    /* gnome-terminal extends the background color for the other line
     * if the "\033[0m" is after the newline*/
    bool neednl = false;
    char *bbuf = (char *)malloc((len + 1) * sizeof(char));
    strcpy(bbuf, buf);
    if (buf[len - 1] == '\n')
    {
        neednl = true;
        bbuf[len - 1] = '\0';
        --len;
    }

    /* In CXXR 'otype_t' is an enum and can have four values:
     * 0 = Normal output (should be sent to stdout)
     * 1 = Information
     * 2 = Warning
     * 3 = Error
     * */
    if(otype){
        if (otype == 2 /* WARNING_ */)
        {
            fprintf(stderr, "%s%s\033[0m", crwarn, bbuf);
        }
        else if (otype == 3 /* ERROR_ */)
        {
            fprintf(stderr, "%s%s\033[0m", crerror, bbuf);
        }
        else if (otype == 1 /* INFORMATION_ */)
        {
            fprintf(stderr, "%s%s\033[0m", crstderr, bbuf);
        }

        /* Put the newline back */
        if(neednl)
            fprintf(stderr, "\n");
        fflush(stderr);
        /* other type (i.e., non warning/error(s)). could be numbers, date, aso. */
    } else {
        int l = len + 1024;
        char *newbuf = (char *)calloc(sizeof(char), l);
        l -= 64;
        strcpy(newbuf, crnormal);
        int i = 0;
        int j = normalsize;
        /* for all i smaller than obj length */
#if CXXR_FALSE
        bool haspttrn;
#endif
        while(i < len){
            if(j >= l)
                newbuf = colorout_make_bigger(newbuf, &l);
#if CXXR_FALSE
            /* Custom patterns */
            haspttrn = 0;
            if(P){
                int psz = 0;
                pattern_t *p = P;
                while(p){
                    psz = ispattern(bbuf, i, len, p);
                    if(psz){
                        haspttrn = 1;
                        strcat(newbuf, p->color);
                        j += p->crsize;
                        for(int k = 0; k < psz; k++){
                            newbuf[j] = bbuf[i];
                            i++;
                            j++;
                        }
                        strcat(newbuf, crnormal);
                        j += normalsize;
                    }
                    p = p->next;
                }
                if(haspttrn)
                    continue;
            }
            if(haspttrn)
                continue;
#endif
            if(bbuf[i] == '"'){
                strcat(newbuf, crstring);
                j += stringsize;
                newbuf[j] = bbuf[i];
                i++;
                j++;
                while(i < len && bbuf[i] != '\n'){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                    if(i > 2 && bbuf[i-1] == '"' && bbuf[i-2] != '\\')
                        break;
                }
                strcat(newbuf, crnormal);
                j += normalsize;
            } else if(bbuf[i] == '[' && ((bbuf[i+1] >= '0' && bbuf[i+1] <= '9') || bbuf[i+1] == ',' || bbuf[i+1] == ' ') && isindex(bbuf, i+1, len)){
                strcat(newbuf, crindex);
                j += indexsize;
                newbuf[j] = bbuf[i];
                i++;
                j++;
                while(bbuf[i] != ']'){
                    if(j > l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
                newbuf[j] = bbuf[i];
                i++;
                strcat(newbuf, crnormal);
                j += normalsize + 1;
                /* NULL */
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'U' && bbuf[i+2] == 'L' && bbuf[i+3] == 'L'
                    && isword(bbuf, i, 4)){
                snprintf(piece, maxsize, "%sNULL%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 4;
                j += 4 + normalsize + constsize;
                /* TRUE */
            } else if(bbuf[i] == 'T' && bbuf[i+1] == 'R' && bbuf[i+2] == 'U' && bbuf[i+3] == 'E'
                    && isword(bbuf, i, 4)){
                snprintf(piece, maxsize, "%sTRUE%s", crlogicalT, crnormal);
                strcat(newbuf, piece);
                i += 4;
                j += 4 + normalsize + logicalTsize;
                /* FALSE */
            } else if(bbuf[i] == 'F' && bbuf[i+1] == 'A' && bbuf[i+2] == 'L' && bbuf[i+3] == 'S' && bbuf[i+4] == 'E'
                    && isword(bbuf, i, 5)){
                snprintf(piece, maxsize, "%sFALSE%s", crlogicalF, crnormal);
                strcat(newbuf, piece);
                i += 5;
                j += 5 + normalsize + logicalFsize;
                /* NA */
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'A'
                    && isword(bbuf, i, 2)){
                snprintf(piece, maxsize, "%sNA%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 2;
                j += 2 + normalsize + constsize;
                /* Inf */
            } else if(bbuf[i] == 'I' && bbuf[i+1] == 'n' && bbuf[i+2] == 'f'
                    && !isletter(bbuf[i+3])){
                if(i > 0 && bbuf[i-1] == '-'){
                    newbuf[j-1] = 0;
                    snprintf(piece, maxsize, "%s-Inf%s", crinfinite, crnormal);
                    strcat(newbuf, piece);
                } else {
                    snprintf(piece, maxsize, "%sInf%s", crinfinite, crnormal);
                    strcat(newbuf, piece);
                }
                i += 3;
                j += 3 + normalsize + infinitesize;
                /* NaN */
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'a' && bbuf[i+2] == 'N'
                    && isword(bbuf, i, 3)){
                snprintf(piece, maxsize, "%sNaN%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 3;
                j += 3 + normalsize + constsize;
                /* hexadecimal number */
            } else if(bbuf[i] == '0' && bbuf[i+1] == 'x' && ((bbuf[i+2] >= '0' && bbuf[i+2] <= '9') ||
                        (bbuf[i+2] >= 'a' && bbuf[i+2] <= 'f'))){
                strcat(newbuf, crnumber);
                j += numbersize;
                newbuf[j] = bbuf[i];
                newbuf[j+1] = 'x';
                i += 2;
                j += 2;
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || (bbuf[i] >= 'a' && bbuf[i] <= 'f'))){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
                strcat(newbuf, crnormal);
                j += normalsize;
                /* date YYYYxMMxDD or DDxMMxYYYY */
            } else if(isdate(bbuf, i, len)){
                strcat(newbuf, crdate);
                j += datesize;
                for(int k = 0; k < 10; k++){
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
                /* if time is appended to the date */
                if(bbuf[i] == ' ' && istime(bbuf, i+1, len)){
                    for(int k = 0; k < 9; k++){
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                    }
                }
                strcat(newbuf, crnormal);
                j += normalsize;
                /* time */
            } else if(istime(bbuf, i, len)){
                strcat(newbuf, crdate);
                j += datesize;
                for(int k = 0; k < 8; k++){
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
                strcat(newbuf, crnormal);
                j += normalsize;
                /* positive numbers */
            } else if(bbuf[i] >= '0' && bbuf[i] <= '9' && isnumber(bbuf, i, len)){
                if (hlzero && iszero(bbuf, i, len)){
                    strcat(newbuf, crzero);
                    j += zerosize;
                }else{
                    strcat(newbuf, crnumber);
                    j += numbersize;
                }
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || bbuf[i] == '.')){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                    if(bbuf[i] == 'e' && (i + 2) < len && (bbuf[i+1] == '-' || bbuf[i+1] == '+') && (bbuf[i+2] >= '0' && bbuf[i+2] <= '9')){
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                    }
                }
                strcat(newbuf, crnormal);
                j += normalsize;
                /* negative numbers */
            } else if(bbuf[i] == '-' && (bbuf[i+1] >= '0' && bbuf[i+1] <= '9') && isnumber(bbuf, i+1, len)){
                if (hlzero && iszero(bbuf, i+1, len)){
                    strcat(newbuf, crzero);
                    j += zerosize;
                }else{
                    strcat(newbuf, crnegnum);
                    j += negnumsize;
                }
                newbuf[j] = '-';
                i++;
                j++;
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || bbuf[i] == '.')){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                    if(bbuf[i] == 'e' && (i + 2) < len && (bbuf[i+1] == '-' || bbuf[i+1] == '+') && (bbuf[i+2] >= '0' && bbuf[i+2] <= '9')){
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                    }
                }
                strcat(newbuf, crnormal);
                j += normalsize;
                /* anything else */
            } else {
                newbuf[j] = bbuf[i];
                i++;
                j++;
            }
        }

        if(neednl)
            printf("%s\033[0m\n", newbuf);
        else
            printf("%s\033[0m", newbuf);
        fflush(stdout);
        free(newbuf);
    }
    free(bbuf);
}

#if CXXR_FALSE
void colorout_ColorOutput(void)
{
    if(colorout_initialized)
        return;

    if(colors_initialized == 0){
        if(strcmp(getenv("TERM"), "fbterm") == 0){
            strcpy(crnormal, "\033[1;40}");
            strcpy(crnumber, "\033[1;214}");
            strcpy(crnegnum, "\033[1;209}");
            strcpy(crdate,   "\033[1;179}");
            strcpy(crstring, "\033[1;85}");
            strcpy(crconst,  "\033[1;35}");
            strcpy(crstderr, "\033[1;213}");
            strcpy(crwarn,   "\033[1;1}");
            strcpy(crerror,  "\033[2;1}\033[1;7}");
            strcpy(crlogicalT, "\033[1;78}");
            strcpy(crlogicalF, "\033[1;203}");
            strcpy(crinfinite, "\033[1;39}");
            strcpy(crindex, "\033[1;30}");
            strcpy(crzero,     "\033[1;226}");
        } else {
            strcpy(crnormal, "\033[0;38;5;40m");
            strcpy(crnumber, "\033[0;38;5;214m");
            strcpy(crnegnum, "\033[0;38;5;209m");
            strcpy(crdate,   "\033[0;38;5;179m");
            strcpy(crstring, "\033[0;38;5;85m");
            strcpy(crconst,  "\033[0;38;5;35m");
            strcpy(crstderr, "\033[0;38;5;213m");
            strcpy(crwarn,   "\033[0;1;38;5;1m");
            strcpy(crerror,  "\033[0;48;5;1;38;5;15m");
            strcpy(crlogicalT, "\033[0;38;5;78m");
            strcpy(crlogicalF, "\033[0;38;5;203m");
            strcpy(crinfinite, "\033[0;38;5;39m");
            strcpy(crindex, "\033[0;38;5;30m");
            strcpy(crzero,     "\033[0;38;5;226m");
        }
        normalsize = strlen(crnormal);
        numbersize = strlen(crnumber);
        negnumsize = strlen(crnegnum);
        datesize = strlen(crdate);
        stringsize = strlen(crstring);
        constsize = strlen(crconst);
        logicalTsize = strlen(crlogicalT);
        logicalFsize = strlen(crlogicalF);
        infinitesize = strlen(crinfinite);
        indexsize = strlen(crindex);
        zerosize     = strlen(crzero);
        colors_initialized = 1;

        alloc_piece();
    }

    /* Replace Rstd_WriteConsoleEx() with colorout_R_WriteConsoleEx().
     * See R source code: files src/unix/system.c and src/unix/sys-std.c */
    save_R_Outputfile = R_Outputfile;
    save_R_Consolefile = R_Consolefile;
    save_ptr_R_WriteConsole = ptr_R_WriteConsole;
    save_ptr_R_WriteConsoleEx = ptr_R_WriteConsoleEx;

    R_Outputfile = NULL;
    R_Consolefile = NULL;
    ptr_R_WriteConsole = NULL;
    ptr_R_WriteConsoleEx = colorout_R_WriteConsoleEx;

    colorout_initialized = 1;
}

void colorout_noColorOutput(void)
{
    if(colorout_initialized){
        R_Outputfile = save_R_Outputfile;
        R_Consolefile = save_R_Consolefile;
        ptr_R_WriteConsole = save_ptr_R_WriteConsole;
        ptr_R_WriteConsoleEx = save_ptr_R_WriteConsoleEx;
        colorout_initialized = 0;
    }
}


SEXP colorout_is_enabled(void) {
  return ScalarLogical(colorout_initialized);
}
#endif
