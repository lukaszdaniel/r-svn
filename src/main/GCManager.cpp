/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file GCManager.cpp
 *
 * Class GCManager and associated C-callable functions.
 */

#include <algorithm> // for std::fill
#include <CXXR/MemoryBank.hpp>
#include <CXXR/GCManager.hpp>
#include <CXXR/BadObject.hpp>
#include <Rinterface.h> // for R_Suicide()
#include <R_ext/Error.h> // for Rf_error
#include <R_ext/Print.h> // for REprintf
#include <Defn.h> // for R_VSIZE, R_NSIZE

namespace CXXR
{
    unsigned int GCManager::s_gen_gc_counts[s_num_old_generations + 1];

    unsigned int GCManager::s_inhibitor_count = 0;
    bool GCManager::s_gc_fail_on_error = false;
    bool GCManager::s_gc_is_running = false;
    bool GCManager::s_gc_pending = false;
    unsigned int GCManager::s_gc_count = 0;
#ifdef GC_TORTURE
    int GCManager::s_gc_force_wait = 0;
    int GCManager::s_gc_force_gap = 0;
    bool GCManager::s_gc_inhibit_release = false;
#endif
    size_t GCManager::s_threshold = R_VSIZE;
    size_t GCManager::s_min_threshold = 0;
    size_t GCManager::s_max_threshold = R_SIZE_T_MAX;
    size_t GCManager::s_max_used = 0;
    size_t GCManager::s_node_threshold = R_NSIZE;
    size_t GCManager::s_min_node_threshold = 0;
    size_t GCManager::s_max_node_threshold = R_SIZE_T_MAX;
    size_t GCManager::s_max_node_used = 0;

    double GCManager::s_min_free_frac = 0.2;
    double GCManager::s_ngrow_frac = 0.70;
    double GCManager::s_nshrink_frac = 0.30;

    double GCManager::s_vgrow_frac = 0.70;
    double GCManager::s_vshrink_frac = 0.30;

#ifdef SMALL_MEMORY
    /* On machines with only 32M of memory (or on a classic Mac OS port)
       it might be a good idea to use settings like these that are more
       aggressive at keeping memory usage down. */
    double GCManager::s_ngrow_incr_frac = 0.0, GCManager::s_nshrink_incr_frac = 0.2;
    unsigned int GCManager::s_ngrow_incr_min = 50000, GCManager::s_nshrink_incr_min = 0;
    double GCManager::s_vgrow_incr_frac = 0.0, GCManager::s_vshrink_incr_frac = 0.2;
    unsigned int GCManager::s_vgrow_incr_min = 100000, GCManager::s_vshrink_incr_min = 0;
#else
    double GCManager::s_ngrow_incr_frac = 0.2, GCManager::s_nshrink_incr_frac = 0.2;
    unsigned int GCManager::s_ngrow_incr_min = 40000, GCManager::s_nshrink_incr_min = 0;
    double GCManager::s_vgrow_incr_frac = 0.2, GCManager::s_vshrink_incr_frac = 0.2;
    unsigned int GCManager::s_vgrow_incr_min = 80000, GCManager::s_vshrink_incr_min = 0;
#endif

    unsigned int GCManager::s_vsfac = 1;
    std::ostream *GCManager::s_os = nullptr;

    void (*GCManager::s_pre_gc)() = nullptr;
    void (*GCManager::s_post_gc)() = nullptr;

    bool GCManager::FORCE_GC()
    {
#ifdef GC_TORTURE
        if (s_gc_pending)
        {
            return true;
        }
        else if (s_gc_force_wait > 0)
        {
            --s_gc_force_wait;
            if (s_gc_force_wait > 0)
            {
                return false;
            }
            else
            {
                s_gc_force_wait = s_gc_force_gap;
                return true;
            }
        }
        return false;
#else
        return s_gc_pending;
#endif
    }

    void GCManager::setTortureParameters(int gap, int wait, bool inhibitor)
    {
        s_gc_force_gap = gap;
        s_gc_force_wait = wait;
        s_gc_inhibit_release = inhibitor;
    }

    void GCManager::setInhibitor(bool inhibitor)
    {
        s_gc_inhibit_release = inhibitor;
    }

    int GCManager::gc_force_wait()
    {
        return s_gc_force_wait;
    }

    int GCManager::gc_force_gap()
    {
        return s_gc_force_gap;
    }

    bool GCManager::gc_inhibit_release()
    {
        return s_gc_inhibit_release;
    }

    void GCManager::handle_gc_error(const char *msg)
    {
        if (s_gc_fail_on_error)
            R_Suicide(msg);
        else if (s_gc_is_running)
            REprintf("%s", msg);
        else
            Rf_error("%s", msg);
    }

    void GCManager::mem_err_heap()
    {
        if (R_MaxVSize == R_SIZE_T_MAX)
            errorcall(R_NilValue, "%s", _("vector memory exhausted"));
        else {
            double l = R_GetMaxVSize() / Kilo;
            const char *unit = "Kb";

            if (l > Mega) {
                l /= Mega;
                unit = "Gb";
            }
            else if (l > Kilo) {
                l /= Kilo;
                unit = "Mb";
            }
            errorcall(R_NilValue,
                _("vector memory limit of %0.1f %s reached, see mem.maxVSize()"),
                l, unit);
        }
    }

    size_t GCManager::R_GetMaxVSize(void)
    {
        if (R_MaxVSize == R_SIZE_T_MAX) return R_SIZE_T_MAX;
        return R_MaxVSize * vsfac;
    }

    bool GCManager::R_SetMaxVSize(size_t size)
    {
        if (size == R_SIZE_T_MAX) {
            R_MaxVSize = R_SIZE_T_MAX;
            return true;
        }
        if (vsfac == 1) {
            if (size >= R_VSize) {
                R_MaxVSize = size;
                return true;
            }
        }
        else
            if (size / vsfac >= R_VSize) {
                R_MaxVSize = (size + 1) / vsfac;
                return true;
            }
        return false;
    }

    size_t GCManager::R_GetMaxNSize(void)
    {
        return R_MaxNSize;
    }

    bool GCManager::R_SetMaxNSize(size_t size)
    {
        if (size >= R_NSize) {
            R_MaxNSize = size;
            return true;
        }
        return false;
    }

#define VHEAP_FREE() (R_VSize - MemoryBank::doublesAllocated())
    size_t GCManager::cue(size_t bytes_wanted)
    {
        size_t n_doubles = bytes_wanted / sizeof(VECREC);
        GCManager::gc(n_doubles, false); // gc() counts in doubles, not bytes
        if (VHEAP_FREE() < n_doubles)
            mem_err_heap();
        return R_VSize * sizeof(VECREC);
    }
// #undef VHEAP_FREE
    void GCManager::enableGC(size_t initial_threshold, size_t initial_node_threshold)
    {
        // Safeguard against cases where R_VSize or R_NSize have already been set up.
        if (R_VSize != R_VSIZE)
        {
            Rf_warning("R_VSize has been changed before GCManager::enableGC(). Ignoring init value.");
            initial_threshold = R_VSize;
        }
        if (R_NSize != R_NSIZE)
        {
            Rf_warning("R_NSize has been changed before GCManager::enableGC(). Ignoring init value.");
            initial_node_threshold = R_NSize;
        }

        vsfac = sizeof(VECREC);
        R_VSize = (initial_threshold + 1) / vsfac;
        orig_R_VSize = R_VSize;
        if (R_MaxVSize < R_SIZE_T_MAX)
            R_MaxVSize = (R_MaxVSize + 1) / vsfac;

        R_NSize = initial_node_threshold;
        orig_R_NSize = initial_node_threshold;

        for (unsigned int i = 0; i <= s_num_old_generations; ++i)
            s_gen_gc_counts[i] = 0;
        MemoryBank::setGCCuer(cue, R_VSize * sizeof(VECREC));
    }

    unsigned int GCManager::genRota(unsigned int num_old_gens_to_collect)
    {
        static unsigned int s_collect_counts[s_num_old_generations] = { 0 };
        static unsigned int s_level = 0;

        auto reset_counters = [](unsigned int *counters) {
            std::fill(counters, counters + s_num_old_generations, 0);
            };

            // Full reset request: collect everything and reset counters
        if (num_old_gens_to_collect >= s_num_old_generations) {
            reset_counters(s_collect_counts);
            return s_level = num_old_gens_to_collect;
        }

        // Manual override: jump directly to requested level, adjust counters accordingly
        if (s_level != num_old_gens_to_collect) {
            s_level = num_old_gens_to_collect;

            // Reset all lower-level counters when skipping ahead
            for (unsigned int gen = 0; gen < s_level; ++gen)
                s_collect_counts[gen] = 0;

            // Increment the target level counter if within range
            if (s_level < s_num_old_generations)
                ++s_collect_counts[s_level];

            return s_level;
        }

        // Normal escalation through all generations
        for (unsigned int gen = 0; gen < s_num_old_generations; ++gen) {
            ++s_collect_counts[gen];

            // If we just escalated to a higher gen, reset the lower one
            if (gen > 0 && s_collect_counts[gen] < s_collect_counts_max[gen])
                s_collect_counts[gen - 1] = 0;

            // Stop escalating once we're within the limit for this generation
            if (s_collect_counts[gen] <= s_collect_counts_max[gen])
                return s_level = gen;
        }

        // We've exceeded all thresholds → full collection
        reset_counters(s_collect_counts);
        return s_level = s_num_old_generations;
    }

// #define DEBUG_GC
#ifdef DEBUG_GC
    static void DEBUG_GC_SUMMARY(bool full_gc)
    {
        REprintf("\n%s, VSize = %lu", full_gc ? "Full" : "Minor",
            MemoryBank::doublesAllocated());
        unsigned int OldCount = 0;
        for (unsigned int gen = 1; gen < GCNode::numGenerations(); gen++)
            OldCount += GCNode::s_gencount[gen];
        REprintf(": %d", OldCount);
    }
#else
#define DEBUG_GC_SUMMARY(x)
#endif

// #define DEBUG_ADJUST_HEAP
#ifdef DEBUG_ADJUST_HEAP
    static void DEBUG_ADJUST_HEAP_PRINT(double node_occup, double vect_occup)
    {
        REprintf("Node occupancy: %.0f%%\nVector occupancy: %.0f%%\n",
            100.0 * node_occup, 100.0 * vect_occup);
        R_size_t alloc = MemoryBank::doublesAllocated() +
            sizeof(VectorBase) * MemoryBank::blocksAllocated();
        REprintf("Total allocation: %lu\n", alloc);
        REprintf("Ncells %lu\nVcells %lu\n", R_NSize, R_VSize);
    }
#else
#define DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup)
#endif /* DEBUG_ADJUST_HEAP */

    /* Heap Size Adjustment. */
    namespace
    {
        R_size_t applyAggressiveGrow(R_size_t change)
        {
            static R_size_t last_in_use = 0;
            static unsigned int adjust_count = 1;

            if (adjust_count < 50) {
                ++adjust_count;

                /* A decrease in live nodes contributes no predicted growth. */
                R_size_t R_NodesInUse = GCNode::numNodes();
                double growth = R_NodesInUse > last_in_use
                    ? (double)(R_NodesInUse - last_in_use) : 0.0;

                /* Try to achieve an occupancy rate of R_NGrowFrac. */
                double target = ((double)R_NodesInUse + growth) / R_NGrowFrac;
                last_in_use = R_NodesInUse;

                /* Exclude out-of-range conversion, including a rounded-up
                   SIZE_MAX. */
                if (target < (double)R_SIZE_T_MAX) {
                    R_size_t next_nsize = (R_size_t)target;
                    if (next_nsize > R_NSize && next_nsize - R_NSize > change)
                        change = next_nsize - R_NSize;
                }
            }
            return change;
        }
    } // anonymous namespace

    void GCManager::AdjustHeapSize(R_size_t size_needed)
    {
        // Calculate minimum required free nodes and vectors
        R_size_t R_MinNFree = (R_size_t)(orig_R_NSize * R_MinFreeFrac);
        R_size_t R_MinVFree = (R_size_t)(orig_R_VSize * R_MinFreeFrac);

        // Calculate required node and vector sizes
        R_size_t NNeeded = GCNode::numNodes() + R_MinNFree;
        R_size_t VNeeded = MemoryBank::doublesAllocated() + size_needed + R_MinVFree;

        // Calculate occupancy rates
        double node_occup = ((double)NNeeded) / R_NSize;
        double vect_occup = ((double)VNeeded) / R_VSize;

        // Adjust node heap size
        if (node_occup > R_NGrowFrac) {
            R_size_t change = (R_size_t)(R_NGrowIncrMin + R_NGrowIncrFrac * R_NSize);

            /* for early adjustments grow more aggressively */
            change = applyAggressiveGrow(change);

            if (R_NSize <= R_MaxNSize && change <= R_MaxNSize - R_NSize)
                R_NSize += change;
        }
        else if (node_occup < R_NShrinkFrac) {
            R_NSize -= (R_size_t)(R_NShrinkIncrMin + R_NShrinkIncrFrac * R_NSize);

            if (R_NSize < NNeeded)
                R_NSize = (NNeeded < R_MaxNSize) ? NNeeded : R_MaxNSize;

            if (R_NSize < orig_R_NSize)
                R_NSize = orig_R_NSize;
        }

        // Adjust vector heap size
        if (vect_occup > 1.0 && VNeeded < R_MaxVSize)
            R_VSize = VNeeded;

        if (vect_occup > R_VGrowFrac) {
            R_size_t change = (R_size_t)(R_VGrowIncrMin + R_VGrowIncrFrac * R_VSize);

            if (R_MaxVSize - R_VSize >= change)
                R_VSize += change;
        }
        else if (vect_occup < R_VShrinkFrac) {
            R_VSize -= (R_size_t)(R_VShrinkIncrMin + R_VShrinkIncrFrac * R_VSize);

            if (R_VSize < VNeeded)
                R_VSize = VNeeded;

            if (R_VSize < orig_R_VSize)
                R_VSize = orig_R_VSize;
        }

        DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup);
    }

/* Node Sorting.  MemoryBank::defragment() attempts to improve locality of reference
   by rearranging the free list to place nodes on the same place page
   together and order nodes within pages.  This involves a sweep of the
   heap, so it should not be done too often, but doing it at least
   occasionally does seem essential.  Sorting on each full colllection is
   probably sufficient.
*/
#define SORT_NODES
    // former RunGenCollect()
    unsigned int GCManager::gcGenController(R_size_t size_needed, bool force_full_collection)
    {
        static unsigned int level = 0;
        unsigned int gens_collected;

        BadObject::s_firstBadObject.clear();

        /* determine number of generations to collect */
        if (force_full_collection) level = s_num_old_generations;

#ifdef PROTECTCHECK
        level = s_num_old_generations;
#endif

        level = genRota(level);

        bool ok = false;
        while (!ok) {
            ok = true;

            GCNode::gc(level);
            gens_collected = level;

            if (level < s_num_old_generations) {
                if (VHEAP_FREE() < size_needed + R_MinFreeFrac * R_VSize) {
                    ++level;
                    if (VHEAP_FREE() < size_needed)
                        ok = false;
                }
                else level = 0;
            }
            else level = 0;
        } // end of while loop
        s_gen_gc_counts[gens_collected]++;

        if (gens_collected > 0)
        {
            if (gens_collected == s_num_old_generations) {
                /**** do some adjustment for intermediate collections? */
                AdjustHeapSize(size_needed);
            }
            GCNode::DEBUG_CHECK_NODE_COUNTS("after heap adjustment");
        }

#ifdef SORT_NODES
        if (gens_collected == s_num_old_generations)
            MemoryBank::defragment();
#endif

        return gens_collected;
    }

    // former R_gc_internal()
    void GCManager::gc(R_size_t size_needed, bool force_full_collection)
    {
        try
        {
            R_CHECK_THREAD;
            if (GCInhibitor::active() || s_gc_is_running) {
                if (s_gc_is_running)
                    throw std::runtime_error("GCManager::gc(): *** recursive gc invocation\n");

                // if (!force_full_collection &&
                //     VHEAP_FREE() < size_needed + R_MinFreeFrac * R_VSize)
                //     force_full_collection = true;

                if (size_needed > VHEAP_FREE()) {
                    R_size_t expand = size_needed - VHEAP_FREE();
                    if (R_VSize + expand > R_MaxVSize)
                        mem_err_heap();
                    R_VSize += expand;
                }

                s_gc_pending = true;
                return;
            }
            s_gc_pending = false;

            double ncells, vcells, vfrac, nfrac;
            BadObject bad_object;
            unsigned int gens_collected = 0;

#ifdef IMMEDIATE_FINALIZERS
            bool first = true;
            bool ok = false;
            while (!ok) {
                ok = true;
#endif

                ++s_gc_count;

                R_N_maxused = std::max(R_N_maxused, GCNode::numNodes());
                R_V_maxused = std::max(R_V_maxused, MemoryBank::doublesAllocated());

                BEGIN_SUSPEND_INTERRUPTS{
                s_gc_is_running = true;
                if (s_pre_gc) // gc_start_timing();
                    (*s_pre_gc)();
                gens_collected = gcGenController(size_needed, force_full_collection);
                if (s_post_gc) // gc_end_timing();
                    (*s_post_gc)();
                s_gc_is_running = false;
                } END_SUSPEND_INTERRUPTS;

                if (R::R_check_constants > 2 ||
                    (R::R_check_constants > 1 && gens_collected == GCNode::numOldGenerations()))
                    R::R_checkConstants(true);

                if (s_os) {
                    REprintf("Garbage collection %d = %d", s_gc_count, s_gen_gc_counts[0]);
                    for (unsigned int i = 0; i < GCNode::numOldGenerations(); i++)
                        REprintf("+%d", s_gen_gc_counts[i + 1]);
                    REprintf(" (level %d) ... ", gens_collected);
                    DEBUG_GC_SUMMARY(gens_collected == GCNode::numOldGenerations());

                    ncells = GCNode::numNodes();
                    nfrac = (100.0 * ncells) / R_NSize;
                    /* We try to make this consistent with the results returned by gc */
                    ncells = 0.1 * ceil(10 * ncells * sizeof(RObject) / Mega);
                    REprintf("\n%.1f %s of cons cells used (%d%%)\n",
                        ncells, "Mbytes", (int)(nfrac + 0.5));
                    vcells = MemoryBank::doublesAllocated();
                    vfrac = (100.0 * vcells) / R_VSize;
                    vcells = 0.1 * ceil(10 * vcells * vsfac / Mega);
                    REprintf("%.1f %s of vectors used (%d%%)\n",
                        vcells, "Mbytes", (int)(vfrac + 0.5));
                }

                if (!BadObject::s_firstBadObject.isEmpty()) {
                    bad_object = BadObject::s_firstBadObject;
                }

#ifdef IMMEDIATE_FINALIZERS
                if (first) {
                    first = false;
                    /* Run any eligible finalizers.  The return result of
                       RunFinalizers is true if any finalizers are actually run.
                       There is a small chance that running finalizers here may
                       chew up enough memory to make another immediate collection
                       necessary.  If so, we jump back to the beginning and run
                       the collection, but on this second pass we do not run
                       finalizers. */
                    if (RunFinalizers() && (size_needed > VHEAP_FREE()))
                        ok = false;
                }
            } // end of while loop
#endif

            bad_object.printSummary();

            /* sanity check on logical scalar values */
            if (R_TrueValue != NULL && LOGICAL(R_TrueValue)[0] != TRUE) {
                LOGICAL(R_TrueValue)[0] = TRUE;
                throw std::runtime_error(_("internal TRUE value has been modified"));
            }
            if (R_FalseValue != NULL && LOGICAL(R_FalseValue)[0] != FALSE) {
                LOGICAL(R_FalseValue)[0] = FALSE;
                throw std::runtime_error(_("internal FALSE value has been modified"));
            }
            if (R_LogicalNAValue != NULL &&
                LOGICAL(R_LogicalNAValue)[0] != NA_LOGICAL) {
                LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
                throw std::runtime_error(_("internal logical NA value has been modified"));
            }
                /* code */
        }
        catch (const std::runtime_error &e)
        {
            handle_gc_error(e.what());
        }
    }

    std::ostream *GCManager::setReporting(std::ostream *os)
    {
        std::ostream *ans = s_os;
        s_os = os;
        return ans;
    }
} // namespace CXXR
