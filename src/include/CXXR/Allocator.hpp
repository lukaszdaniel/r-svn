/* The following code is adapted from an example taken from the book
 * "The C++ Standard Library - A Tutorial and Reference"
 * by Nicolai M. Josuttis, Addison-Wesley, 1999
 *
 * (C) Copyright Nicolai M. Josuttis 1999.
 * Permission to copy, use, modify, sell and distribute this software
 * is granted provided this copyright notice appears in all copies.
 * This software is provided "as is" without express or implied
 * warranty, and with no claim as to its suitability for any purpose.
 */

/* Adaptations Copyright Andrew Runnalls 2007, under the same terms as
 * the original copyright above.
 */

/** @file Allocator.hpp
 * @brief STL-compatible allocator front-ending CXXR::MemoryBank.
 */

#ifndef ALLOCATOR_HPP
#define ALLOCATOR_HPP

#include <limits>
#include <CXXR/MemoryBank.hpp>

namespace CXXR
{
    /** @brief STL-compatible allocator front-ending CXXR::MemoryBank.
     *
     * This templated class enables container classes within the C++
     * standard library to allocate their memory via CXXR::MemoryBank.
     * However, its calls to MemoryBank are configured so that they do
     * not give rise to garbage collections: this is to avoid any
     * reentrant calls to the code for C++ standard library
     * containers.
     *
     * The code below is adapted from an example in the book "The C++
     * Standard Library - A Tutorial and Reference" by Nicolai
     * M. Josuttis, Addison-Wesley, 1999.  Also see Item 10 of Meyers'
     * 'Effective STL' for the arcana of STL allocators.
     */
    template <typename T>
    class Allocator
    {
    public:
        // type definitions
        using value_type = T;
        using pointer = T *;
        using const_pointer = const T *;
        using reference = T &;
        using const_reference = const T &;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;

        // rebind allocator to type U
        template <class U>
        struct rebind
        {
            using other = Allocator<U>;
        };

        // return address of values
        pointer address(reference value) const
        {
            return &value;
        }
        const_pointer address(const_reference value) const
        {
            return &value;
        }

        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        Allocator() throw()
        {
        }

        Allocator(const Allocator &) throw()
        {
        }

        template <class U>
        Allocator(const Allocator<U> &) throw()
        {
        }

        ~Allocator() throw()
        {
        }

        // return maximum number of elements that can be allocated
        size_type max_size() const throw()
        {
            return std::numeric_limits<std::size_t>::max() / sizeof(T);
        }

        // allocate but don't initialize num elements of type T
        pointer allocate(size_type num, const void * /*hint*/ = nullptr)
        {
            return static_cast<pointer>(MemoryBank::allocate(num * sizeof(T), false));
        }

        // initialize elements of allocated storage p with value value
        template <class U, class... Args>
        void construct(U *p, Args &&...args)
        {
            ::new ((void *)p) U(std::forward<Args>(args)...);
        }

        // destroy elements of initialized storage p
        void destroy(pointer p)
        {
            // destroy objects by calling their destructor
            p->~T();
        }

        // deallocate storage p of deleted elements
        void deallocate(pointer p, size_type num)
        {
            MemoryBank::deallocate(p, num * sizeof(T));
        }
    };

    // return that all specializations of this allocator are interchangeable
    template <typename T1, typename T2>
    bool operator==(const Allocator<T1> &,
                    const Allocator<T2> &) throw()
    {
        return true;
    }

    template <typename T1, typename T2>
    bool operator!=(const Allocator<T1> &,
                    const Allocator<T2> &) throw()
    {
        return false;
    }
} // namespace CXXR

#endif // ALLOCATOR_HPP
