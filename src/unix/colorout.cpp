/*
 *  Below code was created based on colorout.c (1.3-2)
 *  from the colorout R package.
 *  Copyright (C) 2024-2025 Lukasz Daniel
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
// updated to match colorout 1.3-3

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <string_view>
#include <cctype>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/StringVector.hpp>
#include <R_ext/Visibility.h>  // for attribute_hidden
#include <R_ext/RStartup.h> // for otype_t

using namespace CXXR;

#if CXXR_FALSE
#include <Rinterface.h>

#define R_INTERFACE_PTRS 1

static void (*save_ptr_R_WriteConsole)(const char *, int);
static void (*save_ptr_R_WriteConsoleEx)(const char *, int, int);
static void *save_R_Outputfile;
static void *save_R_Consolefile;

static char crnormal[64], crnumber[64], crnegnum[64], crdate[64], crstring[64],
            crconst[64], crstderr[64], crwarn[64], crerror[64],
            crlogicalF[64], crlogicalT[64], crinfinite[64], crindex[64], crzero[64];
static int normalsize, numbersize, negnumsize, datesize, stringsize, constsize,
           logicalTsize, logicalFsize, infinitesize, indexsize, zerosize;
static int colors_initialized = 0;
static int colorout_initialized = 0;
static char *piece;

static double too_small = 1e-12;
static int highlight_zero = 0;

typedef struct pattern {
    char *ptrn;
    char *compiled;
    unsigned int cpldsize;
    unsigned int matchsize;
    char *color;
    unsigned int crsize;
    struct pattern *next;
} pattern_t;

pattern_t *P = NULL;
#endif

#define too_small 1e-12
#define highlight_zero false

#define crnormal   "\033[0;38;5;40m"
#define crnumber   "\033[0;38;5;214m"
#define crnegnum   "\033[0;38;5;209m"
#define crdate     "\033[0;38;5;179m"
#define crstring   "\033[0;38;5;85m"
#define crconst    "\033[0;38;5;35m"
#define crstderr   "\033[0;38;5;213m"
#define crwarn     "\033[0;1;38;5;1m"
#define crerror    "\033[0;48;5;1;38;5;15m"
#define crlogicalT "\033[0;38;5;78m"
#define crlogicalF "\033[0;38;5;203m"
#define crinfinite "\033[0;38;5;39m"
#define crindex    "\033[0;38;5;30m"
#define crzero     "\033[0;38;5;226m"
#define crreset    "\033[0m"

#define time_len 8
#define date_len 10

#define isletter(b) std::isalpha(b)

static inline bool isword(std::string_view b, unsigned int i, unsigned int len) noexcept
{
    if (i + len > b.size()) return false;
    bool is_letter_preceeding = (i > 0) ? isletter(b[i-1]) : false;
    bool is_letter_following = isletter(b[i+len]);
    return !is_letter_preceeding && !is_letter_following;
}

static inline bool isdelim(char b) noexcept
{
    /* not a letter or number */
    return !(std::isalpha(b) || std::isdigit(b));
}

static bool isnumber(std::string_view b, unsigned int i) noexcept
{
    if(i > 0 && !isdelim(b[i-1]))
        return false;

    /* Look 8 bytes ahead */
    unsigned int l = b.size();
    if(l > (i + 8))
        l = i + 8;
    i++;
    while(i < l){
        if (isdelim(b[i]))
            return true;
        if(!std::isdigit(b[i]) && b[i] != '.' && b[i] != ',' &&
                !(b[i] == 'e' && (i + 2) < b.size() && (b[i+1] == '-' || b[i+1] == '+') && std::isdigit(b[i+2])) &&
                !(b[i-1] == 'e' && (b[i] == '-' || b[i] == '+')))
            return false;
        i++;
    }
    return true;
}


static bool is_zero(std::string_view b, unsigned int i)
{
    char *stopstring;
    double x;
    unsigned int j = i;
    char *charnum = (char*)calloc(sizeof(char),b.size()+1);
    while(i<b.size()){
        charnum[i-j] = b[i];
        i++;
    }
    x = strtod(charnum, &stopstring);
    free(charnum);
    return (x < too_small);
}

static bool isindex(std::string_view b, unsigned int i) noexcept
{
    // Element of unnamed list
    if(i > 1 && b[i-2] == '[')
        return false;

    if(b[i] == ','){
        i++;
        if (!(std::isdigit(b[i]) || b[i] == ' '))
            return false;
    }

    while (i < b.size() && (std::isdigit(b[i]) || b[i] == ' '))
        ++i;

    // Vector or matrix index?
    // Also check that the previous index is a digit
    return ((b[i] == ']' && std::isdigit(b[i-1])) || (b[i] == ',' && b[i+1] == ']' && std::isdigit(b[i-1])));
}



static bool isdate(std::string_view b, unsigned int i) noexcept
{
    /* wrong length */
    if (i + date_len > b.size()) return false;

    /* YYYYxMMxDD or YYYYxDDxMM */
    if(b[i+4] == b[i+7] && b[i+4] == '-'){
        return (b[i] >= '1' && b[i] <= '9' &&
                b[i+1] >= '0' && b[i+1] <= '9' &&
                b[i+2] >= '0' && b[i+2] <= '9' &&
                b[i+3] >= '0' && b[i+3] <= '9' &&
                b[i+5] >= '0' && b[i+5] <= '3' &&
                b[i+6] >= '0' && b[i+6] <= '9' &&
                b[i+8] >= '0' && b[i+8] <= '3' &&
                b[i+9] >= '0' && b[i+9] <= '9');
    }

    /* DDxMMxYYYY or MMxDDxYYYY */
    if(b[i+2] == b[i+5] && b[i+2] == '-'){
        return (b[i] >= '0' && b[i] <= '3' &&
                b[i+1] >= '0' && b[i+1] <= '9' &&
                b[i+3] >= '0' && b[i+3] <= '3' &&
                b[i+4] >= '0' && b[i+4] <= '9' &&
                b[i+6] >= '1' && b[i+6] <= '9' &&
                b[i+7] >= '0' && b[i+7] <= '9' &&
                b[i+8] >= '0' && b[i+8] <= '9' &&
                b[i+9] >= '0' && b[i+9] <= '9');
    }

    return false;
}


static bool istime(std::string_view b, unsigned int i) noexcept
{
    /* wrong length */
    if (i + time_len > b.size()) return false;

    /* HH:MM:SS */
    if (b[i + 2] != ':' || b[i + 5] != ':') return false;

    return (b[i] >= '0' && b[i] <= '9' &&
            b[i+1] >= '0' && b[i+1] <= '9' &&
            b[i+3] >= '0' && b[i+3] <= '5' &&
            b[i+4] >= '0' && b[i+4] <= '9' &&
            b[i+6] >= '0' && b[i+6] <= '5' &&
            b[i+7] >= '0' && b[i+7] <= '9');
}

#if CXXR_FALSE
static unsigned int ispattern(const char *b, unsigned int i, const pattern_t *p)
{
    unsigned int n = 0;
    unsigned int j = i;
    if(b.size() >= i+p->matchsize){
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
    highlight_zero = 0;
}

void colorout_SetZero(double *zr)
{
    highlight_zero = 1;
    too_small = *zr;
}

SEXP colorout_ListPatterns(void)
{
    unsigned int n = 0;
    pattern_t *p = P;

    while(p){
        n++;
        /*
        printf("pattern = %s\nmatch size = %d\ncompiled = ", p->ptrn, p->matchsize);
        for(unsigned int i = 0; i < strlen(p->compiled); i++)
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
    GCStackRoot<StringVector> res, nms;
    res = StringVector::create(n);
    nms = StringVector::create(n);

    p = P;
    unsigned int i = 0;
    while(p){
        SET_STRING_ELT(nms, i, mkChar(p->ptrn));
        SET_STRING_ELT(res, i, mkChar(p->color));
        p = p->next;
        i++;
    }
    setAttrib(res, R_NamesSymbol, nms);

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
    unsigned int i = 0;
    unsigned int j = 0;
    unsigned int l = strlen(p->ptrn);
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

static unsigned int max(const unsigned int a, const unsigned int b)
{
    if(a > b)
        return a;
    return b;
}

static void alloc_piece(void)
{
    unsigned int maxsize;

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
        if(highlight_zero)
            printf("%sx[x<=-%g]\033[0m %sx[abs(x)<%g]\033[0m %sx[x>=%g]\033[0m ",
                    crnegnum, too_small, crzero, too_small, crnumber, too_small);
        else
            printf("%sx[x<0]\033[0m %sx[x>=0]\033[0m ", crnegnum, crnumber);

        printf("%s19/01/2038 03:14:07\033[0m %s\"string\"\033[0m\n%sNA/NaN/NULL\033[0m %sFALSE\033[0m %sTRUE\033[0m %sInf\033[0m %s[index]\033[0m %sstderror\033[0m %swarn\033[0m %serror\033[0m\n",
                crdate, crstring, crconst, crlogicalF, crlogicalT, crinfinite, crindex, crstderr, crwarn, crerror);
    }
}

char *colorout_make_bigger(char *ptr, unsigned int *len)
{
    char *nnbuf;
    *len = *len + 1024;
    nnbuf = (char*)calloc(1, *len);
    strcpy(nnbuf, ptr);
    free(ptr);
    *len = *len - 64;
    return(nnbuf);
}
#endif

/* This function color prints the contents of 'buf', of length 'len' and type 'otype' */
attribute_hidden void colorout_R_WriteConsoleEx(const char *buf, int len_, otype_t otype)
{

    if (!buf || len_ <= 0) return;

    /* gnome-terminal extends the background color for the other line
     * if the "\033[0m" is after the newline*/
    bool neednl = (buf[len_ - 1] == '\n');
    unsigned int len = len_ - neednl;
    std::string_view bbuf(buf, len);

    // Handle error/warning/info
    auto print_err = [&](const char *color) {
        // std::fprintf(stderr, "%s%s%s%s", color, std::string(bbuf).c_str(), crreset, neednl ? "\n" : "");
        std::fputs(color, stderr);
        std::fwrite(bbuf.data(), 1, bbuf.size(), stderr);
        std::fputs(crreset, stderr);
        if (neednl) std::fputc('\n', stderr);
        std::fflush(stderr);
    };

    /* In CXXR 'otype_t' is an enum and can have four values:
     * 0 = Normal output (should be sent to stdout)
     * 1 = Information
     * 2 = Warning
     * 3 = Error
     * */
    if (otype == WARNING_)      { print_err(crwarn); return; }
    else if (otype == ERROR_)   { print_err(crerror); return; }
    else if (otype == INFORMATION_) { print_err(crstderr); return; }

    /* other type (i.e., non warning/error(s)). could be numbers, date, aso. */
    if (true) {
        std::string newbuf;
        newbuf.reserve(len + 64); // reserve some extra space
        newbuf += crnormal;

        unsigned int i = 0; // source position
        /* for all i smaller than obj length */
#if CXXR_FALSE
        bool haspttrn;
#endif
        while(i < len){
#if CXXR_FALSE
            /* Custom patterns */
            haspttrn = false;
            if(P){
                unsigned int psz = 0;
                pattern_t *p = P;
                while(p){
                    psz = ispattern(bbuf, i, p);
                    if(psz){
                        haspttrn = true;
                        newbuf += p->color;
                        for(unsigned int k = 0; k < psz; k++){
                            newbuf.push_back(bbuf[i++]);
                        }
                        newbuf += crnormal;
                    }
                    p = p->next;
                }
                if(haspttrn)
                    continue;
            }
            if(haspttrn)
                continue;
#endif
                /* Strings */
            if(bbuf[i] == '"'){
                newbuf += crstring;
                newbuf.push_back(bbuf[i++]);
                while(i < len && bbuf[i] != '\n'){
                    newbuf.push_back(bbuf[i++]);
                    if(i > 2 && bbuf[i-1] == '"' && bbuf[i-2] != '\\')
                        break;
                }
                newbuf += crnormal;
                // Index like [1], [2, 3]
            } else if((i+1 < len) && bbuf[i] == '[' && (std::isdigit(bbuf[i+1]) || bbuf[i+1] == ',' || bbuf[i+1] == ' ') && isindex(bbuf, i+1)){
                newbuf += crindex;
                newbuf.push_back(bbuf[i++]);
                while(bbuf[i] != ']'){
                    newbuf.push_back(bbuf[i++]);
                }
                newbuf.push_back(bbuf[i++]);
                newbuf += crnormal;
                /* NULL */
            } else if((i+3 < len) && (bbuf.compare(i, 4, "NULL") == 0) && isword(bbuf, i, 4)){
                newbuf += crconst; newbuf += "NULL"; newbuf += crnormal; i += 4;
                /* TRUE */
            } else if((i+3 < len) && (bbuf.compare(i, 4, "TRUE") == 0) && isword(bbuf, i, 4)){
                newbuf += crlogicalT; newbuf += "TRUE"; newbuf += crnormal; i += 4;
                /* FALSE */
            } else if((i+4 < len) && (bbuf.compare(i, 5, "FALSE") == 0) && isword(bbuf, i, 5)){
                newbuf += crlogicalF; newbuf += "FALSE"; newbuf += crnormal; i += 5;
                /* NA */
            } else if((i+1 < len) && (bbuf.compare(i, 2, "NA") == 0) && isword(bbuf, i, 2)){
                newbuf += crconst; newbuf += "NA"; newbuf += crnormal; i += 2;
                /* Inf or -Inf */
            } else if((i+3 < len) && (bbuf.compare(i, 3, "Inf") == 0) && !isletter(bbuf[i+3])){
                if(i > 0 && bbuf[i-1] == '-'){
                    newbuf.pop_back(); // remove '-'
                    newbuf += crinfinite; newbuf += "-Inf"; newbuf += crnormal;
                } else {
                    newbuf += crinfinite; newbuf += "Inf"; newbuf += crnormal;
                }
                i += 3;
                /* NaN */
            } else if((i+2 < len) && (bbuf.compare(i, 3, "NaN") == 0) && isword(bbuf, i, 3)){
                newbuf += crconst; newbuf += "NaN"; newbuf += crnormal; i += 3;
                /* hexadecimal number */
            } else if((i+2 < len) && bbuf[i] == '0' && bbuf[i+1] == 'x' && std::isxdigit(bbuf[i+2])){
                newbuf += crnumber;
                newbuf.push_back(bbuf[i++]); // '0'
                newbuf.push_back(bbuf[i++]); // 'x'
                while(i < len && std::isxdigit(bbuf[i])){
                    newbuf.push_back(bbuf[i++]);
                }
                newbuf += crnormal;
                /* date YYYYxMMxDD or DDxMMxYYYY */
            } else if(isdate(bbuf, i)){
                newbuf += crdate;
                for(unsigned int k = 0; k < date_len; k++){
                    newbuf.push_back(bbuf[i++]);
                }
                /* if time is appended to the date */
                if(bbuf[i] == ' ' && istime(bbuf, i+1)){
                    newbuf.push_back(bbuf[i++]); // space
                    for(unsigned int k = 0; k < time_len; k++){
                        newbuf.push_back(bbuf[i++]);
                    }
                }
                newbuf += crnormal;
                /* time */
            } else if(istime(bbuf, i)){
                newbuf += crdate;
                for(unsigned int k = 0; k < time_len; k++){
                    newbuf.push_back(bbuf[i++]);
                }
                newbuf += crnormal;
                /* positive numbers */
            } else if(std::isdigit(bbuf[i]) && isnumber(bbuf, i)){
                if (highlight_zero && is_zero(bbuf, i)){
                    newbuf += crzero;
                }else{
                    newbuf += crnumber;
                }
                while(i < len && (std::isdigit(bbuf[i]) || bbuf[i] == '.')){
                    newbuf.push_back(bbuf[i++]);
                    if(bbuf[i] == 'e' && (i + 2) < len && (bbuf[i+1] == '-' || bbuf[i+1] == '+') && std::isdigit(bbuf[i+2])){
                        newbuf.push_back(bbuf[i++]);
                        newbuf.push_back(bbuf[i++]);
                    }
                }
                newbuf += crnormal;
                /* negative numbers */
            } else if((i+1 < len) && bbuf[i] == '-' && std::isdigit(bbuf[i+1]) && isnumber(bbuf, i+1)){
                if (highlight_zero && is_zero(bbuf, i+1)){
                    newbuf += crzero;
                }else{
                    newbuf += crnegnum;
                }
                newbuf.push_back('-'); i++;
                while(i < len && (std::isdigit(bbuf[i]) || bbuf[i] == '.')){
                    newbuf.push_back(bbuf[i++]);
                    if(bbuf[i] == 'e' && (i + 2) < len && (bbuf[i+1] == '-' || bbuf[i+1] == '+') && std::isdigit(bbuf[i+2])){
                        newbuf.push_back(bbuf[i++]);
                        newbuf.push_back(bbuf[i++]);
                    }
                }
                newbuf += crnormal;
                /* anything else */
            } else {
                newbuf.push_back(bbuf[i++]);
            }
        }

    newbuf += crreset;
    if (neednl) newbuf.push_back('\n');
    std::fwrite(newbuf.data(), 1, newbuf.size(), stdout);
    std::fflush(stdout);
    }
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
