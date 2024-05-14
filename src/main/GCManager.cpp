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

#include <CXXR/GCManager.hpp>

namespace CXXR
{
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
    std::ostream *GCManager::s_os = nullptr;

    void (*GCManager::s_pre_gc)() = nullptr;
    void (*GCManager::s_post_gc)() = nullptr;

    void GCManager::setTortureParameters(int gap, int wait, bool inhibitor)
    {
        s_gc_force_gap = gap;
        s_gc_force_wait = wait;
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

    void GCManager::setInhibitor(bool inhibitor)
    {
        s_gc_inhibit_release = inhibitor;
    }

    std::ostream *GCManager::setReporting(std::ostream *os)
    {
        std::ostream *ans = s_os;
        s_os = os;
        return ans;
    }
} // namespace CXXR
