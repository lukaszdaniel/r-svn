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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file GCNode.hpp
 * @brief Class CXXR::GCNode.
 */

#ifndef GCNODE_HPP
#define GCNODE_HPP

#include <cstddef>
#include <cstdint>
#include <functional>
#include <utility>
#include <limits>
#include <vector>
#include <cassert>

#include <memory>
#include <bitset>
#include <string>
#include <CXXR/config.hpp>
#include <CXXR/RTypes.hpp>
#include <CXXR/SEXPTYPE.hpp>
#include <CXXR/GCManager.hpp>





namespace siv
{
    /// Alias to differentiate between stable IDs and internal ids.
    /// A stable ID allows to access the data through the index vector and is associated with the same object until its erased.
    /// An internal id is simply the current position of the object in the data vector and may change with deletions.
    using ID = uint64_t;

    /// Forward declaration
    template <typename T>
    class Vector;

    /// @brief A standalone struct allowing to access an object without the need to have a reference to the containing Vector.
    /// @tparam T The type of the object
    template <typename T>
    class Ref
    {
    public:
        using element_type = T;

        /// @brief Constructs an invalid reference.
        constexpr Ref() noexcept = default;

        /// @brief Constructs a reference bound to a specific vector and generation token.
        /// @param id Stable ID of the object.
        /// @param generation_id Generation token used to detect stale references.
        /// @param vector Pointer to the owning vector.
        constexpr Ref(ID id,
                      uint32_t generation,
                      Vector<T> *owner) noexcept
            : m_id(id), m_generation(generation), m_owner(owner)
        {
        }

        /// @brief Pointer-like access to the underlying object
        [[nodiscard]] T *operator->() const noexcept
        {
            return &(*m_owner)[m_id];
        }

        /// @brief Dereference operator
        [[nodiscard]] T &operator*() const noexcept
        {
            return (*m_owner)[m_id];
        }

        constexpr void reset() noexcept
        {
            m_owner = nullptr;
            m_id = invalid_id;
            m_generation = 0;
        }

        /// @brief Returns the stable ID of the associated object.
        [[nodiscard]] constexpr ID getID() const noexcept
        {
            return m_id;
        }

        /// @brief Checks whether the reference is still valid.
        /// @return false if uninitialized or if the object has been erased from the vector, true otherwise
        [[nodiscard]] constexpr explicit operator bool() const noexcept
        {
            return isValid();
        }

        [[nodiscard]] constexpr bool isValid() const noexcept
        {
            return m_owner && m_owner->isValid(m_id, m_generation);
        }

        friend constexpr bool operator==(const Ref &, const Ref &) noexcept = default;

    private:
        static constexpr ID invalid_id = std::numeric_limits<ID>::max();

        /// The stable ID of the object.
        ID m_id{invalid_id};
        /// The generation ID of the object at the time of creation. Used to check the generation of the handle.
        uint32_t m_generation{};
        /// A raw pointer to the vector containing the object associated with this handle
        Vector<T> *m_owner{};
    };

    /// @brief A vector that provides stable IDs when adding objects.
    /// These stable IDs will still allow to access their associated objects even after inserting of removing other objects.
    /// This comes at the cost of a small overhead because of an additional indirection.
    ///
    /// @tparam T The type of the objects to be stored in the vector. It has to be movable.
    ///
    template <typename T>
    class Vector
    {
    public:
        using value_type = T;
        using size_type = std::size_t;
        using iterator = typename std::vector<T>::iterator;
        using const_iterator = typename std::vector<T>::const_iterator;

        /// @brief Constructs an empty vector container.
        Vector() = default;

        /// @brief Constructs an empty vector container with reserved capacity.
        explicit Vector(size_type reserve_size)
        {
            reserve(reserve_size);
        }

        /// @brief Constructs a new object in-place and returns its stable ID.
        /// @tparam Args Constructor arguments types
        /// @param args Constructor arguments
        /// @return The ID to retrieve the object
        template <typename... Args>
        [[nodiscard]] ID emplace_back(Args &&...args)
        {
            const ID id = getFreeSlot();
            m_data.emplace_back(std::forward<Args>(args)...);
            return id;
        }

        /// @brief Copies an object into the container and returns its stable ID.
        /// @param object The object to copy
        /// @return The ID to retrieve the object
        [[nodiscard]] ID push_back(const T &object)
        {
            const ID id = getFreeSlot();
            m_data.push_back(object);
            return id;
        }

        /// @brief Moves an object into the container and returns its stable ID.
        /// @param obj The object to move
        /// @return The ID to retrieve the object
        [[nodiscard]] ID push_back(T &&obj)
        {
            const ID id = getFreeSlot();
            m_data.push_back(std::move(obj));
            // m_internal_to_ID.push_back(Metadata{id, 0});
            // m_ID_to_internal[id] = m_data.size() - 1u;
            return id;
        }

        /// @brief Removes the object associated with the provided stable ID.
        /// @param id The ID of the object to remove
        void erase(ID id)
        {
            // Fetch relevant info
            const size_type internal_id = m_ID_to_internal[id];
            const size_type last_internal_id = m_data.size() - 1;
            const ID last_ID = m_internal_to_ID[last_internal_id].m_stable_id;
            // Update validity ID
            ++(m_internal_to_ID[internal_id].m_generation);

            if (internal_id != last_internal_id)
            {
                // Swap the object to delete with the object at the end
                std::swap(m_data[internal_id], m_data[last_internal_id]);
                std::swap(m_internal_to_ID[internal_id], m_internal_to_ID[last_internal_id]);
                std::swap(m_ID_to_internal[id], m_ID_to_internal[last_ID]);
            }

            // Destroy the object
            m_data.pop_back();
        }

        /// @brief Removes all objects matching the given predicate.
        /// @param predicate The predicate used to check an object has to be removed
        template <typename Pred>
        void remove_if(Pred &&predicate)
        {
            for (size_type internal_id = 0; internal_id < m_data.size();)
            {
                if (predicate(m_data[internal_id]))
                {
                    eraseViaInternalID(internal_id);
                }
                else
                {
                    ++internal_id;
                }
            }
        }

        /// @brief Removes the object referenced by the handle from the vector
        /// @param handle The handle referencing the object to remove
        void erase(const Ref<T> &handle)
        {
            // Ensure the handle is from this vector
            assert(handle.m_owner == this);
            // Ensure the object hasn't already been erased
            assert(handle.isValid());
            erase(handle.getID());
        }

        /// @brief Access the object reference by its stable ID.
        /// @param id The object's stable ID
        /// @return A reference to the object
        T &operator[](ID id)
        {
            return m_data[getInternalID(id)];
        }

        /// @brief Access the object reference by its stable ID in const mode.
        /// @param id The object's stable ID
        /// @return A constant reference to the object
        const T &operator[](ID id) const
        {
            return m_data[getInternalID(id)];
        }

        /// @brief Creates a handle that can safely access the object.
        /// @param id The ID of the object
        /// @return A handle to the object
        [[nodiscard]] Ref<T> getRef(ID id)
        {
            /* Ensure the object is valid. If the internal id is greater than the current size
             * it means that it has been swapped and removed. */
            assert(m_ID_to_internal[id] < m_data.size());
            return {id, getGenerationID(id), this};
        }

        /// @brief Returns the object stored at a specific internal id.
        /// @param i The internal id of the object
        /// @return A reference to the object
        T &getDataAt(size_type i)
        {
            return m_data[i];
        }

        /// @brief Checks if the provided object is still valid considering its last known generation ID
        /// @param id The ID of the object
        /// @param generation_id The last known generation ID
        /// @return True if the last known generation ID is equal to the current one
        [[nodiscard]] bool isValid(ID id, uint32_t generation_id) const
        {
            return generation_id == m_internal_to_ID[getInternalID(id)].m_generation;
        }

        /// @brief Returns an iterator to the first active object.
        iterator begin() noexcept
        {
            return m_data.begin();
        }

        /// @brief Returns an iterator to the last active object.
        iterator end() noexcept
        {
            return m_data.end();
        }

        /// @brief Returns a const iterator to the first active object.
        const_iterator begin() const noexcept
        {
            return m_data.begin();
        }

        /// @brief Returns a const iterator to the last active object.
        const_iterator end() const noexcept
        {
            return m_data.end();
        }

        /// @brief Returns the number of active objects in the container.
        [[nodiscard]] size_type size() const noexcept
        {
            return m_data.size();
        }

        /// @brief Tells if the vector is currently full
        [[nodiscard]] bool isFull() const noexcept
        {
            return m_internal_to_ID.size() <= m_data.size();
        }

        /// @brief Tells if the vector is currently empty
        [[nodiscard]] bool empty() const noexcept
        {
            return m_data.empty();
        }

        /// @brief Returns the vector's capacity (i.e. the number of allocated slots in the vector)
        [[nodiscard]] size_type capacity() const noexcept
        {
            return m_data.capacity();
        }

        /// @brief Returns the stable ID of the object stored at a given internal index.
        [[nodiscard]] ID getID(size_type internal_id) const noexcept
        {
            return m_internal_to_ID[internal_id].m_stable_id;
        }

        /// @brief Returns the internal id associated with a stable ID.
        [[nodiscard]] size_type getInternalID(ID id) const noexcept
        {
            return m_ID_to_internal[id];
        }

        const T &getAt(uint64_t id) const
        {
            return m_data[getInternalID(id)];
        }

        /// @brief Pre allocates @p size slots in the vector
        /// @param size The number of slots to allocate in the vector
        void reserve(size_type size)
        {
            m_data.reserve(size);
            m_internal_to_ID.reserve(size);
            m_ID_to_internal.reserve(size);
        }

        /// @brief Reduces storage capacities to fit the current vector sizes.
        void shrink_to_fit()
        {
            m_data.shrink_to_fit();
            m_internal_to_ID.shrink_to_fit();
            m_ID_to_internal.shrink_to_fit();
        }

    private:
        /// @brief Creates a fresh stable ID for a new object.
        ID getFreeSlot()
        {
            const ID id = getFreeID();
            m_ID_to_internal[id] = m_data.size();
            return id;
        }

        ID getFreeID()
        {
            // This means that we have available slots
            if (m_internal_to_ID.size() > m_data.size())
            {
                // Update the validity ID
                ++(m_internal_to_ID[m_data.size()].m_generation);
                return m_internal_to_ID[m_data.size()].m_stable_id;
            }
            // A new slot has to be created
            const ID new_id = m_data.size();
            m_internal_to_ID.push_back({new_id, 0});
            m_ID_to_internal.push_back(new_id);
            return new_id;
        }

        /// @brief Stores metadata used to track object lifetime and generation across reuses.
        struct Metadata
        {
            /// Stable ID, allowing to retrieve the ID of the object from the data vector.
            ID m_stable_id = 0;
            /// An identifier that is changed when the object is erased, used to ensure a handle is still valid.
            uint32_t m_generation = 0;
        };

        /// The vector holding the actual objects.
        std::vector<T> m_data;
        /// The vector holding the associated metadata. It is accessed using the same index as for the data vector.
        std::vector<Metadata> m_internal_to_ID;
        /// The vector that stores the internal data index for each ID.
        std::vector<size_type> m_ID_to_internal;

        /// @brief Removes the object from the vector
        /// @param internal_id The internal id in the data vector of the object to remove
        void eraseViaInternalID(size_type internal_id)
        {
            erase(m_internal_to_ID[internal_id].m_stable_id);
        }

        /// @brief Return the generation ID associated with the provided ID
        /// @param id The stable ID of the object for which to retrieve the generation ID.
        /// @return The generation ID associated with the provided ID.
        [[nodiscard]] uint32_t getGenerationID(ID id) const
        {
            return m_internal_to_ID[getInternalID(id)].m_generation;
        }

        /// @brief Returns metadata for the object associated with a stable ID.
        /// @param id The stable ID of the object for which to retrieve metadata.
        /// @return A reference to the metadata for the specified object.
        Metadata &getMetadataAt(ID id)
        {
            return m_internal_to_ID[getInternalID(id)];
        }
    };
}









#define NAMED_BITS 16
#define GP_BITS 16

/* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */

#ifdef SWITCH_TO_REFCNT
# define REFCNTMAX ((1 << NAMED_BITS) - 1)
#endif

namespace CXXR
{
    /* Flags */

    struct sxpinfo_struct {
        sxpinfo_struct(SEXPTYPE stype = NILSXP): type(stype), scalar(false), obj(false),
            alt(false), gp(0), m_mark(false), debug(false),
            m_memory_traced(false), m_refcnt_enabled(true), m_rstep(false), m_gcgen(0),
            m_ext_allocator(false), m_refcnt(0), m_binding_tag(NILSXP), extra(0)
        {
        }

        void clear()
        {
            type = NILSXP;
            scalar = false;
            obj = false;
            alt = false;
            gp = 0;
            m_mark = false;
            debug = false;
            m_memory_traced = false;
            m_refcnt_enabled = true;
            m_rstep = false;
            m_gcgen = 0;
            m_ext_allocator = false;
            m_refcnt = 0;
            m_binding_tag = NILSXP;
            extra = 0;
        }

        SEXPTYPE type : TYPE_BITS;
        unsigned int scalar : 1;
        unsigned int obj : 1;
        unsigned int alt : 1;
        unsigned int gp : GP_BITS;
        unsigned int m_mark : 1;
        unsigned int debug : 1;
        unsigned int m_memory_traced : 1;  /* functions and memory tracing */
        unsigned int m_refcnt_enabled : 1;  /* used on closures and when REFCNT is defined */
        unsigned int m_rstep : 1;
        unsigned int m_gcgen : 2;  /* old generation number */
        unsigned int m_ext_allocator : 1;  /* was external allocator used? */
        unsigned int m_refcnt : NAMED_BITS;
        SEXPTYPE m_binding_tag : TYPE_BITS; /* used for immediate bindings */
        unsigned int extra : 5; /* unused bits */
    }; /*		    Tot: 64 bits, 1 double */

    /** @brief Base class for objects managed by the garbage collector.
     *
     * Abstract base class for all objects managed by the garbage
     * collector.
     *
     * \par Derived class checklist:
     * Classes derived directly or indirectly from GCNode should do
     * the following to ensure the integrity of the garbage collection
     * scheme:
     * <ol>
     * <li>Explicitly declare a destructor (even if it does nothing)
     * as either protected or (if no further derivation from the class
     * is envisaged) private.  This ensures that objects of classes
     * derived from GCNode can be created only using 'new'.</li>
     *
     * <li>If the derived class contains any pointers or references to
     * other objects derived from GCNode, these should be encapsulated
     * within an object of the templated class GCEdge, and the class
     * should reimplement the methods detachReferents() and visitReferents()
     * appropriately.  (This does not necessarily apply to pointers
     * where other logic ensures that the pointer does not outlive the
     * thing pointed to.)</li>
     * </ol>
     *
     * \par Infant immunity:
     * While a GCNode or an object of a class derived from GCNode is
     * under construction, it is effectively immune from the garbage
     * collector.  Not only does this greatly simplify the coding of
     * the constructors themselves, it also means that in implementing
     * the virtual method visitReferents(), it is not necessary to
     * consider the possibility that the garbage collector will invoke
     * this method for a node whose construction is not yet complete.
     *
     * \par
     * The method expose() is used to end this immunity once
     * construction of an object is complete.  However, there appears
     * to be no clean and general way in C++ of calling expose() \e
     * exactly when construction is complete.  Consequently, a node
     * N's infant immunity will in fact continue until one of the
     * following events occurs:
     * <ul>
     *
     * <li>The method expose() is called explicitly for N, or for a
     * node that refers to N (and so on recursively).</li>
     *
     * <li>N is visited by a \b GCNode::Ager object (as part of write
     * barrier enforcement).  This will happen if a node that is
     * already exposed to the garbage collector is modified so that it
     * refers to N (or a node that refers to N, and so on
     * recursively).</li>
     *
     * <li>A pointer to N (or a node that refers to N, and so on
     * recursively) is specified in the constructor of a GCRoot
     * object, and the optional argument \a expose is set to
     * true.</li>
     *
     * <li>N is designated as the key, value or R finalizer of a weak
     * reference object.</li>
     *
     * <li>N is itself a weak reference object (in which case it is
     * exposed to garbage collection during construction).</li>
     *
     * </ul>
     *
     * \par
     * It is the responsibility of any code that creates an object of
     * a class derived from GCNode to ensure that, under normal
     * operation, the object is in due course exposed to the garbage
     * collector.  However, if exceptions occur, the static member
     * function slaughterInfants() can be used to delete \e all the
     * nodes currently enjoying infant immunity.
     *
     * \par Nested <tt>new</tt>:
     * Consider the following code to create a PairList of two
     * elements, with \c first as the 'car' of the first element and
     * \c second as the 'car' of the second element:
     * \code
     * CXXR::GCStackRoot<CXXR::PairList>
     *   pl2(new CXXR::PairList(first, new CXXR::PairList(second)));
     * \endcode
     * Is this code sound?  You might suppose that there is a risk
     * that the second element of the list will be garbage-collected
     * when \c new is invoked to allocate space for the first element.
     * But this is not so: at this stage, the second element will
     * still enjoy infant immunity.
     *
     * \par
     * However, a potential problem arises from a different quarter.
     * Suppose that the \c new to allocate space for the second
     * element succeeds, but that the \c new to allocate space for the
     * first element fails because of shortage of memory, and throws
     * <tt>std::bad_alloc</tt>.  Then the second element will not be
     * exposed to the garbage collector, and the space it occupies is
     * potentially lost.  An easy workaround is for the handler that
     * catches the exception to invoke slaughterInfants().
     *
     * @note Because this base class is used purely for housekeeping
     * by the garbage collector, and does not contribute to the
     * 'meaning' of an object of a derived class, all of its data
     * members are mutable.
     *
     * @todo The (private) cleanup() method needs to address the
     * possibility that derived classes may have destructors that
     * release some external resource (e.g. a lock).  Maybe a garbage
     * collection without a 'mark' phase would do the trick.
     */
    class GCNode
    {
    public:
        /** @brief Schwarz counter.
         *
         * The Schwarz counter (see for example Stephen C. Dewhurst's
         * book 'C++ Gotchas') is a programming idiom to ensure that a
         * class (including particularly its static members) is
         * initialized before any client of the class requires to use
         * it, and that on program exit the class's static resources
         * are not cleaned up prematurely (e.g. while the class is
         * still in use by another class's static members).  Devices
         * such as this are necessitated by the fact that the standard
         * does not prescribe the order in which objects of file and
         * global scope in different compilation units are
         * initialized: it only specifies that the order of
         * destruction must be the reverse of the order of
         * initialization.
         *
         * This is achieved by the unusual stratagem of including the
         * \e definition of a lightweight data item within this header
         * file.  This data item is of type GCNode::SchwarzCounter, and is
         * declared within an anonymous namespace.  Each file that
         * <tt>\#include</tt>s this header file will therefore include
         * a definition of a SchwarzCounter object, and this definition
         * will precede any data definitions within the enclosing file
         * that depend on class GCNode.  Consequently, the SchwarzCounter
         * object will be constructed before any data objects of the
         * client file.  The constructor of SchwarzCounter is so defined
         * that when the first such object is created, the class GCNode
         * will itself be initialized.
         *
         * Conversely, when the program exits, data items within each
         * client file will have their destructors invoked before the
         * file's SchwarzCounter object has its destructor invoked.  This
         * SchwarzCounter destructor is so defined that only when the last
         * SchwarzCounter object is destroyed is the GCNode class itself
         * cleaned up.
         */
        class SchwarzCounter
        {
        public:
            SchwarzCounter()
            {
                // if (!s_count++)
                //     GCNode::initialize();
            }

            ~SchwarzCounter()
            {
                // if (!--s_count)
                //     GCNode::cleanup();
            }

        private:
            static unsigned int s_count;
        };

        /** @brief Main GCNode Constructor.
         *
         */
        GCNode(SEXPTYPE stype);

        /** @brief Abstract base class for the Visitor design pattern.
         *
         * See Gamma et al 'Design Patterns' Ch. 5 for a description
         * of the Visitor design pattern.
         *
         * The const in the name refers to the fact that the visitor
         * does not modify the node it visits (or modifies only
         * mutable fields).  There is currently no provision for the
         * visitor object itself to be be considered const during a
         * visit.
         */
        struct const_visitor
        {
            virtual ~const_visitor() {}

            /** @brief Perform visit.
             *
             * In the light of what the visitor discovers, it may
             * elect also to visit the referents of \a node, by
             * calling visitReferents().
             *
             * @param node Node to be visited.
             */
            virtual void operator()(const GCNode *node) = 0;
        };

        /** @brief Conduct a visitor to the nodes referred to by this
         * one.
         *
         * The referents of this node are those objects (derived from
         * GCNode) designated by a GCEdge within this object.
         *
         * @param v Pointer to the visitor object.
         *
         * @note If this method is reimplemented in a derived class,
         * the reimplemented version must remember to invoke
         * visitReferents() for the immediate base class of the
         * derived class, to ensure that \e all referents of the
         * object get visited.  It is suggested that implementations
         * set up stack-based pointers to all the referents of a node
         * before visiting any of them; in that case, if the
         * (recursive) visiting pushes the node out of the processor
         * cache, there is no need to fetch it back in.
         */
        virtual void visitReferents(const_visitor *v) const {}

        /** @brief Null out all references from this node to other nodes.
         *
         * The referents of this node are those objects (derived from
         * GCNode) designated by a GCEdge within this object.  This
         * function changes all GCEdges within this object to
         * encapsulate a null pointer.  It is used during the sweep
         * phase of a mark-sweep garbage collection to break up
         * unreachable subgraphs, and in particular to remove
         * reference loops from them.  After the application of this
         * method, the GCNode should be regarded as a 'zombie', kept
         * in existence only so other nodes can detach their
         * references to it cleanly (using decRefCount()).
         *
         * @note If this method is reimplemented in a derived class,
         * the reimplemented version must remember to invoke
         * detachReferents() for the immediate base class of the
         * derived class, to ensure that \e all referents of the
         * object get detached.
         */
        virtual void detachReferents() {}

        /** @brief Allocate memory.
         *
         * Allocates memory for a new object of a class derived from
         * GCNode.
         *
         * @param bytes Number of bytes of memory required.
         *
         * @return Pointer to the allocated memory block.
         *
         * @note This function will often carry out garbage collection
         * of some kind before allocating memory.  However, no
         * garbage collection will be performed if at least one
         * GCInhibitor object is in existence.
         */
        static void *operator new(size_t bytes) HOT_FUNCTION;

        /** @brief Placement new for GCNode.
         */
        static void *operator new(size_t, void *where)
        {
            return where;
        }

        /** @brief Deallocate memory
         *
         * Deallocate memory previously allocated by operator new.
         *
         * @param p Pointer to the allocated memory block.
         *
         * @param bytes Size in bytes of the memory block, as
         * requested when the block was allocated.
         */
        static void operator delete(void *p, size_t bytes);

        /** @brief Decrement the reference count.
         *
         */
        static void decRefCount(const GCNode *node)
        {
            if (node && (node->sxpinfo.m_refcnt > 0 && node->sxpinfo.m_refcnt < REFCNTMAX))
                --(node->sxpinfo.m_refcnt);
        }

        /** @brief Increment the reference count.
         *
         */
        static void incRefCount(const GCNode *node)
        {
            if (node && (node->sxpinfo.m_refcnt < REFCNTMAX))
                ++(node->sxpinfo.m_refcnt);
        }

        // Returns the stored reference count.
        unsigned int getRefCount() const
        {
            return sxpinfo.m_refcnt;
        }

        bool refCountEnabled() const
        {
            return sxpinfo.m_refcnt_enabled;
        }

        void markNotMutable()
        {
            sxpinfo.m_refcnt = REFCNTMAX;
        }

        virtual ~GCNode();

        /** @brief Initiate a garbage collection.
         *
         * @param num_old_gens_to_collect The number of old generations to
         * collect.  Must be strictly smaller than numGenerations().
         */
        static void gc(unsigned int num_old_gens_to_collect);

        /** @brief Number of GCNode objects in existence.
         *
         * @return the number of GCNode objects currently in
         * existence.
         */
        static size_t numNodes() { return s_num_nodes; }

        /** @brief Number of old generations used by garbage collector.
         *
         * @return The number of old generations into which GCNode objects
         * are ranked by the garbage collector.
         */
        static constexpr unsigned int numOldGenerations() { return GCManager::numOldGenerations(); }

        /** @brief Number of all generations.
         *
         * @return The number of all generations that includes
         * old + newly created GCObjects (with generation 0)
         */
        static constexpr unsigned int numGenerations() { return GCManager::numOldGenerations() + 1; }

        unsigned int generation() const { return sxpinfo.m_gcgen; }

        bool isMarked() const { return sxpinfo.m_mark; }

#ifndef EXPEL_OLD_TO_NEW
        static void moveToOldToNew(const GCNode *node);
#endif

    // private:
        /** @brief Visitor class used to impose a minimum generation number.
         *
         * This visitor class is used to ensure that a node and its
         * descendants all have generation numbers that exceed a
         * specified minimum value, and is used in implementing the
         * write barrier in the generational garbage collector.
         */
        class Ager: public const_visitor
        {
        public:
            /**
             * @param min_gen The minimum generation number that the
             * visitor is to apply.
             */
            Ager(unsigned int min_gen)
                : m_mingen(min_gen)
            {
            }

            unsigned int mingen() const { return m_mingen; }

            // Virtual function of const_visitor:
            void operator()(const GCNode *node) override;

        private:
            unsigned int m_mingen;
        };

        /** Visitor class used to mark nodes.
         *
         * This visitor class is used during the mark phase of garbage
         * collection to ensure that a node and its descendants are
         * marked.  However, nodes with generation numbers exceeding a
         * specified level are left unmarked.  It is assumed that no
         * node references a node with a younger generation number.
         */
        class Marker: public const_visitor
        {
        public:
            /**
             * @param max_gen Nodes with a generation number exceeding
             *          this are not to be marked.
             */
            Marker(unsigned int max_gen)
                : m_maxgen(max_gen)
            {
            }

            unsigned int maxgen() const { return m_maxgen; }

            // Virtual function of const_visitor:
            void operator()(const GCNode *node) override;

        private:
            unsigned int m_maxgen;
        };

        /** Marker that counts the number of nodes marked.
         */
        class CountingMarker: public Marker
        {
        public:
            CountingMarker(unsigned int max_gen)
                : Marker(max_gen), m_marks_applied(0)
            {
            }

            unsigned int marksApplied() const
            {
                return m_marks_applied;
            }

            void operator()(const GCNode *node) override;

        private:
            unsigned int m_marks_applied;
        };

        /** Visitor class used to abort the program if old-to-new
         * references are found.
         */
        class OldToNewChecker: public const_visitor
        {
        public:
            /**
             * @param min_gen The minimum generation number that is
             * acceptable in visited nodes.
             */
            OldToNewChecker(unsigned int min_gen)
                : m_mingen(min_gen)
            {
            }

            // Virtual function of const_visitor:
            void operator()(const GCNode *node) override;

        private:
            unsigned int m_mingen;
        };

        // GCNode(const GCNode &) = delete;
        GCNode &operator=(const GCNode &) = delete;

        // Clean up static data at end of run:
        static void cleanup();

        /** @brief Initialize the entire memory subsystem.
         *
         * This method must be called before any GCNodes are created.
         * If called more than once in a single program run, the
         * second and subsequent calls do nothing.
         */
        static void initialize();

        /** @brief Prevent old-to-new references.
         *
         * This is the first stage of garbage collection. It
         * propagates the generation recursively
         * through the node graph.
         *
         * If \a node points to a node of a younger generation than
         * the node in question, then raise it to this node's generation, and
         * propagate this change to the nodes to which \a node refers,
         * and so on recursively.
         *
         * @param max_generation The cut-off generation number for which
         *          age propagation will not occur.
         */
        static void propagateAges(unsigned int max_generation);

        /** @brief Carry out the mark phase of garbage collection.
         *
         * @param max_generation The cut-off generation number for which
         *          marking will not occur.
         */
        static void mark(unsigned int max_generation);

        /** @brief Carry out the sweep phase of garbage collection.
         *
         * @param max_generation The cut-off generation number for which
         *          sweep will not occur.
         */
        static void sweep(unsigned int max_generation);

        std::string gpbits() const
        {
            return std::bitset<GP_BITS>(sxpinfo.gp).to_string();
        }

        // Not implemented.  Declared private to prevent clients
        // allocating arrays of GCNode.
        static void *operator new[](size_t);

        mutable struct sxpinfo_struct sxpinfo;
#define ID_NOT_SET std::numeric_limits<siv::ID>::max()
        mutable siv::ID m_ID{ID_NOT_SET};
        mutable bool m_in_new_space{};
        mutable bool m_in_old_to_new_list{};

        void moveToGeneration(unsigned int generation) const;

        static size_t s_num_nodes; // Number of nodes in existence

        /** @brief The Heap Structure.
         *
         * Nodes for each class/generation combination
         * are arranged in circular doubly-linked lists.  The double linking
         * allows nodes to be removed in constant time; this is used by the
         * collector to move reachable nodes out of free space and into the
         * appropriate generation.  The circularity eliminates the need for
         * end checks.  In addition, each link is anchored at an artificial
         * node, the Peg RObject's in the structure below, which simplifies
         * pointer maintenance.  The circular doubly-linked arrangement is
         * taken from Baker's in-place incremental collector design; see
         * ftp://ftp.netcom.com/pub/hb/hbaker/NoMotionGC.html or the Jones and
         * Lins GC book.  The linked lists are implemented by adding two
         * pointer fields to the RObject structure, which increases its size
         * from 5 to 7 doubles. Other approaches are possible but don't seem
         * worth pursuing for R.
         *
         * There are two options for dealing with old-to-new pointers.  The
         * first option is to make sure they never occur by transferring all
         * referenced younger objects to the generation of the referrer when a
         * reference to a newer object is assigned to an older one.  This is
         * enabled by defining EXPEL_OLD_TO_NEW.  The second alternative is to
         * keep track of all nodes that may contain references to newer nodes
         * and to "age" the nodes they refer to at the beginning of each
         * collection.  This is the default.  The first option is simpler in
         * some ways, but will create more floating garbage and add a bit to
         * the execution time, though the difference is probably marginal on
         * both counts.
         */
// #define EXPEL_OLD_TO_NEW
        static siv::Vector<CXXR::GCNode *> s_Old;
        static unsigned int s_gencount[1 + GCManager::numOldGenerations()];

        /** @brief Next generation table.
        *
        * Look-up table used to advance generation number
        * following a garbage collection.
        */
        static unsigned int s_next_gen[1 + GCManager::numOldGenerations()];
    };
} // namespace CXXR

namespace
{
    // CXXR::SchwarzCounter<CXXR::GCNode> gcnode_schwarz_ctr;
    // CXXR::GCNode::SchwarzCounter gcnode_schwarz_ctr;
}

namespace R
{
    /** @brief Is this node marked?
     *
     * @param x Pointer to \c RObject.
     *
     * @return true iff \a x is marked to prevent GC of this node.  Returns false if \a x
     * is nullptr.
     */
    int (MARK)(SEXP x);
    int (REFCNT)(SEXP x);
    bool (REFCNT_ENABLED)(SEXP x);
    void (DECREMENT_REFCNT)(SEXP x);
    void (INCREMENT_REFCNT)(SEXP x);
    void (DISABLE_REFCNT)(SEXP x);
    void (ENABLE_REFCNT)(SEXP x);
} // namespace R

extern "C"
{
    void (MARK_NOT_MUTABLE)(SEXP x);
} // extern "C"

#endif // GCNODE_HPP
