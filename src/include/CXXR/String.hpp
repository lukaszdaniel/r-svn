/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file String.hpp
 * @brief Class CXXR::String and associated C interface.
 */

#ifndef CXXR_STRING_HPP
#define CXXR_STRING_HPP

#include <string>
#include <unordered_map>
#include <CXXR/Allocator.hpp>
#include <CXXR/VectorBase.hpp>
#include <Rinternals.h> // for cetype_t, R_NaString, R_BlankString

namespace R
{
    extern void InitNames();
} // namespace R

namespace CXXR
{
    /** @brief RObject representing a character string.
     *
     * At any one time, at most one String object with a particular
     * text and encoding may exist.
     */
    class String : public VectorBase
    {
    public:
        /** @brief Blank string.
         *
         * @return <tt>const</tt> pointer to the string "".
         */
        static SEXP blank();

        /** @brief 'Not available' string.
         *
         * Note that although the 'not available' string contains the
         * text "NA", it is identified as the 'not available' string
         * by its <em>address</em>, not by its content.  It is
         * entirely in order to create another string with the text
         * "NA", and that string will not be considered 'not
         * available'.
         *
         * @return <tt>const</tt> pointer to the string representing
         *         'not available'.
         */
        static SEXP NA();

        /** @brief Get a pointer to a String object.
         *
         * If no String with the specified text and encoding currently
         * exists, one will be created, and a pointer to it returned.
         * Otherwise a pointer to the existing String will be
         * returned.
         *
         * @param str The text of the required String. (Embedded null
         *          characters are permissible.)
         *
         * @param encoding The encoding of the required String.
         *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted
         *          in this context (checked).  Note that if \a str
         *          contains no non-ASCII characters, then the
         *          encoding is set to CE_NATIVE regardless of the
         *          value of the \a encoding parameter.
         *
         * @return Pointer to a String (preexisting or newly
         * created) representing the specified text in the specified
         * encoding.
         */
        static SEXP obtain(const std::string &str, cetype_t encoding = CE_NATIVE);

        /** @brief used in package utils and graphics
         */
        static bool s_known_to_be_latin1;
        static bool s_known_to_be_utf8;

    // private:
        // The first element of the key is the text, the second
        // element the encoding:
        using key = std::pair<std::string, cetype_t>;

        // Hashing is based simply on the text of the key, not on its
        // encoding:
        class Hasher
        {
        public:
            std::size_t operator()(const key &k) const
            {
                return s_string_hasher(k.first);
            }

        private:
            static std::hash<std::string> s_string_hasher;
        };

        // The cache is implemented as a mapping from keys to pointers
        // to String objects.  Each String simply contains
        // a pointer locating its entry within the cache.
        using map = std::unordered_map<key, SEXP, Hasher, std::equal_to<key>,
                                       CXXR::Allocator<std::pair<const key, SEXP>>>;

        static map s_hash_table; // Global hash of CHARSXPs
        String();
        static SEXP create(const std::string &text, cetype_t encoding, bool isAscii);

        String(const String &) = delete;
        String &operator=(const String &) = delete;

        // Declared private to ensure that String objects are
        // allocated only using 'new':
        ~String();

        // Initialize the static data members:
        static void initialize();
        friend void ::R::InitNames();
    };

    /** @brief Is a std::string entirely ASCII?
     *
     * @param str The string to be examined.
     *
     * @return false if str contains at least one non-ASCII character,
     * otherwise true.  In particular the function returns true for an
     * empty string.
     */
    bool isASCII(const std::string &str);

    // Designed for use with std::accumulate():
    unsigned int stringWidth(unsigned int minwidth, SEXP string);

    // Designed for use with std::accumulate():
    unsigned int stringWidthQuote(unsigned int minwidth, SEXP string);
} // namespace CXXR

namespace R
{
#define known_to_be_latin1 CXXR::String::s_known_to_be_latin1
#define known_to_be_utf8 CXXR::String::s_known_to_be_utf8

    bool Seql(SEXP a, SEXP b);
    bool streql(const char *s, const char *t);
    bool streqln(const char *s, const char *t, size_t n);
    /* Hashing Functions */

    bool (HASHASH)(SEXP x);
    int (HASHVALUE)(SEXP x);
    void (SET_HASHASH)(SEXP x, int v);
    void (SET_HASHVALUE)(SEXP x, int v);

    /** @brief String Hashing
     *
     * This is taken from the second edition of the "Dragon Book" by
     * Aho, Ullman and Sethi.
     *
     * @note This hash function seems to work well enough for symbol tables,
     * and hash tables get saved as part of environments so changing it
     * is a major decision.
     */
    int R_Newhashpjw(const char *s);

    /** @brief Does a rho::String have LATIN1 encoding?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return true iff \a x is marked as having LATIN1 encoding.
     */
    bool (IS_LATIN1)(SEXP x);

    /** @brief Does a CXXR::String have UTF8 encoding?
     *
     * @param x Pointer to a CXXR::String (checked).
     *
     * @return true iff \a x is marked as having UTF8 encoding.
     */
    bool (IS_UTF8)(SEXP x);

    /** @brief Does a CXXR::String have bytecode encoding?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return true iff \a x is marked as having BYTES encoding.
     */
    bool (IS_BYTES)(SEXP x);

    /** @brief Is a CXXR::String pure ASCII?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return true iff \a x contains only ASCII characters.
     */
    bool (IS_ASCII)(SEXP x);

    /** @brief Does a CXXR::String have NATIVE encoding?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return true iff \a x is marked as having NATIVE encoding.
     */
    bool (IS_NATIVE)(SEXP x);

    /** @brief Is the encoding of a CXXR::String known?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return a non-zero value iff \a x is marked as having either
     * LATIN1 encoding or UTF8 encoding.
     */
    int (ENC_KNOWN)(SEXP x);

    bool (IS_CACHED)(SEXP x);

    /** @brief Set LATIN1 encoding.
     *
     * @param x Pointer to a CXXR::String.
     */
    void (SET_LATIN1)(SEXP x);

    /** @brief Set UTF8 encoding.
     *
     * @param x Pointer to a CXXR::String.
     */
    void (SET_UTF8)(SEXP x);

    /** @brief Set BYTES encoding.
     *
     * @param x Pointer to a CXXR::String.
     */
    void (SET_BYTES)(SEXP x);

    /** @brief Set ASCII encoding.
     *
     * @param x Pointer to a CXXR::String.
     */
    void (SET_ASCII)(SEXP x);

    void (SET_CACHED)(SEXP x);

    const char *EncodeChar(SEXP);
    const char *translateChar0(SEXP);
    const char *translateCharFP(SEXP);
    const char *translateCharFP2(SEXP);
    const char *trCharUTF8(SEXP);
} // namespace R

extern "C"
{
    /** @brief NA_STRING as a CHARSXP
     */
    // LibExtern SEXP R_NaString;

    /** @brief "" as a CHARSXP
     */
    // LibExtern SEXP R_BlankString;

    /** @brief Access the content of CXXR::String as a C-style string.
     *
     * @param x \c non-null pointer to a CXXR::String.
     *
     * @return \c const pointer to character 0 of \a x.
     */
    const char *R_CHAR(SEXP x);

    /** @brief Get a pointer to a CXXR::String object.
     *
     * CE_NATIVE encoding is assumed.  If no CXXR::String with the
     * specified text and encoding currently exists, one will be
     * created.  Otherwise a pointer to the existing CXXR::String will
     * be returned.
     *
     * @param name The null-terminated text of the required string.
     *
     * @return Pointer to a string object representing the specified
     *         text.
     */
    SEXP Rf_mkChar(const char *name);

    /** @brief Get a pointer to a CXXR::String object.
     *
     * If no CXXR::String with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer to
     * the existing CXXR::String will be returned.
     *
     * @param str The null-terminated text of the required cached string.
     *
     * @param encoding The encoding of the required String.
     *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted in
     *          this context (checked).
     *
     * @return Pointer to a string object representing the specified
     *         text in the specified encoding.
     */
    SEXP Rf_mkCharCE(const char *str, cetype_t encoding);

    /** @brief Create a CXXR::String object for specified text and
     * encoding.
     *
     * If no CXXR::String with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer to
     * the existing CXXR::String will be returned.
     *
     * @param name The text of the string to be created, possibly
     *          including embedded null characters.  The encoding is
     *          assumed to be CE_NATIVE.
     *
     * @param len The length of the string pointed to by \a text.
     *          Must be nonnegative.  The created string will comprise
     *          the text plus an appended null byte.
     *
     * @param encoding The encoding of the required String.
     *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted in
     *          this context (checked).
     *
     * @return Pointer to the created string.
     */
    SEXP Rf_mkCharLenCE(const char *name, int len, cetype_t encoding);

    /** @brief Create a CXXR::String object for specified text.
     *
     * CE_NATIVE encoding is assumed.  If no CXXR::String with the
     * specified text and encoding currently exists, one will be
     * created.  Otherwise a pointer to the existing CXXR::String will
     * be returned.
     *
     * @param text The text of the string to be created, possibly
     *          including embedded null characters.  The encoding is
     *          assumed to be CE_NATIVE.
     *
     * @param length The length of the string pointed to by \a text.
     *          Must be nonnegative.  The created string will comprise
     *          the text plus an appended null byte.
     *
     * @return Pointer to the created string.
     */
    SEXP Rf_mkCharLen(const char *name, int len);

    /** @brief Convert contents of a CXXR::String to UTF8.
     *
     * @param x Non-null pointer to a CXXR::String.
     *
     * @return The text of \a x rendered in UTF8 encoding.
     *
     * @note The result is held in memory allocated using R_alloc().
     * The calling code must arrange for this memory to be released in
     * due course.
     */
    const char *Rf_translateCharUTF8(SEXP x);
    const char *Rf_translateChar(SEXP);
    cetype_t Rf_getCharCE(SEXP x);
    const char *Rf_reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst);
    SEXP Rf_installChar(SEXP x);
    SEXP Rf_installNoTrChar(SEXP charSXP);
    SEXP Rf_installTrChar(SEXP x);
} // extern "C"

#endif /* CXXR_STRING_HPP */
