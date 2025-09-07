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

/** @file GCManager.hpp
 *
 * @brief Class CXXR::GCManager.
 */

#ifndef GCMANAGER_HPP
#define GCMANAGER_HPP

#include <iosfwd>
#include <CXXR/RTypes.hpp>

#define GC_TORTURE

namespace CXXR
{
    /** @brief Class for managing garbage collection.
     *
     * This class only has static members.  A garbage
     * collection can be initiated explicitly by calling
     * GCManager::gc().  Also, GCNode::operator new() may
     * automatically initiate garbage collection.
     */
    class GCManager
    {
    public:
        /** @brief Not for general use.
         *
         * All garbage collection will be inhibited while any object
         * of this type exists.
         *
         * @deprecated This class is provided for use in implementing
         * functions (such as SET_ATTRIB()) in the Rinternals.h
         * interface which would not give rise to any memory
         * allocations as implemented in CR but may do so as
         * implemented in rho.  It is also used within the GCNode
         * class to handle reentrant calls to gclite() and gc().  Its
         * use for other purposes is deprecated: use instead more
         * selective protection against garbage collection such as
         * that provided by class GCStackRoot<T>.
         *
         * @note GC inhibition is implemented as an object type to
         * facilitate reinstatement of garbage collection when an
         * exception is thrown. In CR it is known under R_GCEnabled variable.
         */
        struct GCInhibitor
        {
            GCInhibitor()
            {
                ++s_inhibitor_count;
            }

            ~GCInhibitor()
            {
                --s_inhibitor_count;
            }

            /** @brief Is inhibition currently in effect?
             *
             * @return true iff garbage collection is currently inhibited.
             */
            static bool active()
            {
                return s_inhibitor_count != 0;
            }
        };

        /** @brief Initiate a garbage collection.
         *
         * Note that enableGC() must have been called before this
         * method is used.
         *
         * @param size_needed An indication of the size
         *          wanted in the event that prompted garbage
         *          collection.  If in doubt, set it to 0.
         *
         * @param force_full_collection If this is true, a garbage collection of all
         *          generations of nodes is forced.  Otherwise
         *          GCManager decides for itself how many generations
         *          should be collected.
         */
        static void gc(R_size_t size_needed, bool force_full_collection = false);

        /** @brief Enable mark-sweep garbage collection.
         *
         * No automatic mark-sweep garbage collection of GCNode
         * objects will take place until this method has been called.
         *
         * @param initial_threshold  Initial value for the collection
         *          threshold.  The threshold will never be made less
         *          than this value during the run (or until the
         *          threshold is changed by a subsequent call to
         *          enableGC() ).
         *
         * @param initial_node_threshold  Initial node value for the collection
         *          threshold.  The threshold will never be made less
         *          than this value during the run (or until the
         *          threshold is changed by a subsequent call to
         *          enableGC() ).
         */
        static void enableGC(size_t initial_threshold, size_t initial_node_threshold);

        static bool gcIsRunning()
        {
            return s_gc_is_running;
        }

        /** @brief Number of old generations used by garbage collector.
         *
         * This will be at most 3, sxpinfo allocates 2 bits
         * for the old generation count, so only 0, 1, 2 or 3 (max) is allowed.
         *
         * @return The number of old generations into which GCNode objects
         * are ranked by the garbage collector.
         */
        static constexpr unsigned int numOldGenerations() { return s_num_old_generations; }


        /** @brief Set/unset monitors on mark-sweep garbage collection.
         *
         * @param pre_gc If not a null pointer, this function will be
         *          called just before garbage collection begins,
         *          e.g. to carry out timing.  It must not itself give
         *          rise to a garbage collection.
         *
         * @param post_gc If not a null pointer, this function will be
         *          called just after garbage collection is completed.
         *          It  must not itself give rise to a garbage
         *          collection.
         */
        static void setMonitors(void (*pre_gc)() = nullptr, void (*post_gc)() = nullptr)
        {
            s_pre_gc = pre_gc;
            s_post_gc = post_gc;
        }

        /** @brief Set the output stream for garbage collection reporting.
         *
         * @param os Pointer to the output stream to which reporting
         *          should be directed.  If NULL, suppresses reporting.
         *
         * @return The previous value of the output stream pointer.
         */
        static std::ostream *setReporting(std::ostream *os = nullptr);

        /** @brief Current threshold level for mark-sweep garbage
         * collection.
         *
         * @return The current threshold level.  When GCNode::operator
         * new is on the point of requesting memory from MemoryBank,
         * if it finds that the number of bytes already allocated via
         * MemoryBank is at least as great as this threshold level, it
         * may initiate a mark-sweep garbage collection.
         */
        static size_t memoryThreshold() { return s_threshold; }

        static bool gc_pending() { return s_gc_pending; }

        static bool gc_fail_on_error() { return s_gc_fail_on_error; }
        static void set_gc_fail_on_error(bool status) { s_gc_fail_on_error = status; }

        /** @brief Report error encountered during garbage collection.
         *
         * Report error encountered during garbage collection where for detecting
         * problems it is better to abort, but for debugging (or some production runs,
         * where external validation of results is possible) it may be preferred to
         * continue. Configurable via _R_GC_FAIL_ON_ERROR_. Typically these problems
         * are due to memory corruption.
         *
         * @param msg Error message to be displayed.
         */
        static void gc_error(const char *msg);
        static bool FORCE_GC();
        static void setTortureParameters(int gap, int wait, bool inhibitor);
        static bool gc_inhibit_release();
        static void setInhibitor(bool inhibitor);
        static int gc_force_wait();
        static int gc_force_gap();

    public: // private:
        static constexpr unsigned int s_num_old_generations = 2;
        static const unsigned int s_collect_counts_max[s_num_old_generations];
        static unsigned int s_gen_gc_counts[s_num_old_generations + 1];
        static size_t s_threshold;
        static size_t s_min_threshold;
        static size_t s_max_threshold;
        static size_t s_node_threshold;
        static size_t s_min_node_threshold;
        static size_t s_max_node_threshold;
#define R_VSize CXXR::GCManager::s_threshold
#define orig_R_VSize CXXR::GCManager::s_min_threshold
#define R_MaxVSize CXXR::GCManager::s_max_threshold
#define R_NSize CXXR::GCManager::s_node_threshold
#define orig_R_NSize CXXR::GCManager::s_min_node_threshold
#define R_MaxNSize CXXR::GCManager::s_max_node_threshold
        /* current units for VSize: changes at initialization */
        static unsigned int s_vsfac;
#define vsfac CXXR::GCManager::s_vsfac
        static bool s_gc_fail_on_error;
        static bool s_gc_is_running; // R_in_gc
        static bool s_gc_pending;
        static unsigned int s_gc_count;
#ifdef GC_TORTURE
        /* **** if the user specified a wait before starting to force
        **** collections it might make sense to also wait before starting
        **** to inhibit releases */
        static int s_gc_force_wait;
        static int s_gc_force_gap;
        static bool s_gc_inhibit_release;
#endif

        static void mem_err_heap();
        static size_t R_GetMaxVSize(void);
        static bool R_SetMaxVSize(size_t size);
        static size_t R_GetMaxNSize(void);
        static bool R_SetMaxNSize(size_t size);

        // Callback for CXXR::MemoryBank to cue a garbage collection:
        static size_t cue(size_t bytes_wanted);

        // Detailed control of the garbage collection, in particular
        // choosing how many generations to collect, is carried out
        // here.
        static unsigned int gcGenController(R_size_t size_needed, bool force_full_collection);

        /** @brief Choose how many generations to collect according to a rota.
         *
         * There are three levels of collections.  Level 0 collects only
         * the youngest generation, Level 1 collects the two youngest
         * generations, and Level 2 collects all generations.  This
         * function decides how many old generations to collect according
         * to a rota.  Most collections are Level 0.  However, after every
         * collect_counts_max[0] Level 0 collections, a Level 1 collection
         * will be carried out; similarly after every
         * collect_counts_max[1] Level 1 collections a Level 2 collection
         * will be carried out.
         *
         * @param minlevel (<= 2, not checked) This parameter places a
         *          minimum on the number of old generations to be
         *          collected.  If minlevel is higher than the number of
         *          generations that genRota would have chosen for itself,
         *          the position in the rota is advanced accordingly.
         */
        static unsigned int genRota(unsigned int minlevel);

        static unsigned int s_inhibitor_count; // Number of GCInhibitor
                                               // objects in existence.

        static std::ostream *s_os; // Pointer to output stream for GC
                                   // reporting, or NULL.

        // Callbacks e.g. for timing:
        static void (*s_pre_gc)();
        static void (*s_post_gc)();

        GCManager() = delete;
        GCManager(const GCManager &) = delete;
        GCManager(GCManager &&) = delete;
        GCManager &operator=(const GCManager &) = delete;
        GCManager &operator=(GCManager &&) = delete;
    };
} // namespace CXXR

#endif /* GCMANAGER_HPP */
