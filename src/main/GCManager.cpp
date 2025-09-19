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
    size_t GCManager::s_node_threshold = R_NSIZE;
    size_t GCManager::s_min_node_threshold = 0;
    size_t GCManager::s_max_node_threshold = R_SIZE_T_MAX;
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

    void GCManager::gc_error(const char *msg)
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
#undef VHEAP_FREE
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

    std::ostream *GCManager::setReporting(std::ostream *os)
    {
        std::ostream *ans = s_os;
        s_os = os;
        return ans;
    }
} // namespace CXXR
