/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011-2023   The R Core Team.
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

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <cstdlib>
#include <CXXR/ProtectStack.hpp>
#include <R.h>
#include "parallel.h"
#include "localization.h"

/* Based on example at
   https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getlogicalprocessorinformation?redirectedfrom=MSDN
*/

#ifndef _W64
# include "glpi.h"
#endif
#undef TRUE
#undef FALSE

typedef BOOL 
(WINAPI *LPFN_GLPI_EX)(LOGICAL_PROCESSOR_RELATIONSHIP,
                       PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX, PDWORD);

// Helper function to count set bits in the processor mask.
static DWORD CountSetBits(ULONG_PTR bitMask)
{
    DWORD LSHIFT = sizeof(ULONG_PTR)*8 - 1;
    DWORD bitSetCount = 0;
    ULONG_PTR bitTest = (ULONG_PTR)1 << LSHIFT;    
    DWORD i;

    for (i = 0; i <= LSHIFT; ++i) {
        bitSetCount += ((bitMask & bitTest)?1:0);
        bitTest/=2;
    }
    return bitSetCount;
}

// Detect CPUs using GetLogicaProcessInformationEx, if available.
static bool ncpus_ex(int *ians)
{
    LPFN_GLPI_EX glpi;
    BOOL done = FALSE;
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX buffer = NULL;
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX ptr = NULL;
    DWORD returnLength = 0;
    DWORD logicalProcessorCount = 0;
    DWORD processorCoreCount = 0;
    DWORD byteOffset = 0;
    DWORD ngroups = 0;

    /* 7/Server 2008 R2 */
    glpi = (LPFN_GLPI_EX) 
	GetProcAddress(GetModuleHandle(TEXT("kernel32")),
		       "GetLogicalProcessorInformationEx");
    if (NULL == glpi)
	return false;

    /* count the number of logical processors using RelationGroup, counting
       bits in affinity masks would not work on 32-bit systems */

    while (!done) {
        DWORD rc = glpi(RelationGroup, buffer, &returnLength);
        if (rc == FALSE) {
            if (buffer) {
		free(buffer);
		buffer = NULL;
	    }
            if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
                buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)
		          malloc(returnLength);
                if (!buffer) error("%s", _("memory allocation error"));

            } else
		error(_("in reading processor information, probable cause: %lu"),
		      (unsigned long)GetLastError());
        } else
	    done = TRUE;
    }

    for(byteOffset = 0; byteOffset < returnLength; byteOffset += ptr->Size) {

	ptr = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)
	      ((char *)buffer + byteOffset);

        if (ptr->Relationship == RelationGroup) {
	    ngroups = ptr->Group.ActiveGroupCount;
	    for(DWORD i = 0; i < ngroups; i++)
		logicalProcessorCount += ptr->Group.GroupInfo[i].ActiveProcessorCount;
        }
    }

    /* count the number of physical processors via RelationProcessorCore */

    done = FALSE;
    returnLength = 0;

    while (!done) {
        DWORD rc = glpi(RelationProcessorCore, buffer, &returnLength);
        if (rc == FALSE) {
            if (buffer) {
		free(buffer);
		buffer = NULL;
	    }
            if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
                buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)
		         malloc(returnLength);
                if (!buffer) error("%s", _("memory allocation error"));
            } else
		error(_("in reading processor information, probable cause: %lu"),
		      (unsigned long)GetLastError());
        } else done = TRUE;
    }

    for(byteOffset = 0; byteOffset < returnLength; byteOffset += ptr->Size) {

        ptr = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)
	      ((char *)buffer + byteOffset);

        if (ptr->Relationship == RelationProcessorCore)
	    processorCoreCount++;
    }

    ians[0] = processorCoreCount;
    ians[1] = logicalProcessorCount;
    free(buffer);

    return TRUE;
}

SEXP ncpus(SEXP virtual_)
{
    // int virt = asLogical(virtual_);

    SEXP ans = allocVector(INTSXP, 2);
    PROTECT(ans);
    int *ians = INTEGER(ans);
    for(int i = 1; i < 2; i++) ians[i] = NA_INTEGER;

    if (ncpus_ex(ians)) {
	UNPROTECT(1);
	return ans;
    }

    BOOL done = FALSE;
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION buffer = NULL;
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION ptr = NULL;
    DWORD returnLength = 0;
    DWORD logicalProcessorCount = 0;
    DWORD processorCoreCount = 0;
    DWORD byteOffset = 0;
    /* Reports only processors within the group in which R is running */

    while (!done) {
        DWORD rc = GetLogicalProcessorInformation(buffer, &returnLength);
        if (rc == FALSE) {
            if (buffer) {
		free(buffer);
		buffer = NULL;
	    }
            if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
                buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION) malloc(returnLength);
                if (!buffer) error("%s", _("memory allocation error"));
            } else
		error(_("in reading processor information, probable cause: %lu"),
		      (unsigned long)GetLastError());
        } else done = TRUE;
    }

    ptr = buffer;

    while (byteOffset + sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION) <= 
	   returnLength) {
        switch (ptr->Relationship) {
        case RelationNumaNode:
            // Non-NUMA systems report a single record of this type.
            break;

        case RelationProcessorCore:
            processorCoreCount++;
            // A hyperthreaded core supplies more than one logical processor.
            logicalProcessorCount += CountSetBits(ptr->ProcessorMask);
            break;

        case RelationCache:
            // Cache data is in ptr->Cache, one CACHE_DESCRIPTOR structure for each cache. 
            break;

        default:
            break;
        }

        byteOffset += sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
        ptr++;
    }

    ians[0] = processorCoreCount;
    ians[1] = logicalProcessorCount;
    free(buffer);
    UNPROTECT(1);

    return ans;
}
