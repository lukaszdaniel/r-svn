/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2025  The R Core Team
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/* This module is only compiled if HAVE_WORKING_CAIRO is true */

/* additional entry points used here

    cairo_show_page
    cairo_pdf_surface_create (1.2)
    cairo_ps_surface_create  (1.2)
    cairo_ps_surface_set_eps  (1.6)
    cairo_surface_set_fallback_resolution (1.2)
    cairo_surface_write_to_png
    cairo_svg_surface_create (1.2)
    cairo_svg_surface_restrict_to_version (1.2)

 */

#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>

#ifdef Win32
//#define HAVE_PANGOCAIRO 1
#define HAVE_CAIRO_SVG 1
#define HAVE_CAIRO_PDF 1
#define HAVE_CAIRO_PS 1

# define raise our_raise
# include <Defn.h>
# undef raise
#else
# include <Defn.h>
#endif

#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>
#include <Defn.h>
#include <Fileio.h>		/* R_fopen */

#include "cairoBM.h"

#include "../localization.h"
#undef FALSE
#undef TRUE

using namespace R;

static double RedGamma	 = 1.0;
static double GreenGamma = 1.0;
static double BlueGamma	 = 1.0;

static void cbm_Size(double *left, double *right,
		     double *bottom, double *top,
		     pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    *left = 0.0;
    *right = xd->windowWidth;
    *bottom = xd->windowHeight;
    *top = 0.0;
}

#define NO_X11 1
#include "cairoFns.cpp"

#ifdef Win32
# include "winbitmap.h"
#else
# include "bitmap.h"
#endif
#undef TRUE
#undef FALSE

static bool BM_Open(pDevDesc dd, pX11Desc xd, int width, int height)
{
    char buf[R_PATH_MAX];
    cairo_status_t res;
    if (xd->type == PNG || xd->type == JPEG ||
	xd->type == TIFF || xd->type == BMP ||
        xd->type == PNGdirect) {
	xd->cs = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
					    xd->windowWidth,
					    xd->windowHeight);
        res = cairo_surface_status(xd->cs);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning(_("cairo error '%s'"), cairo_status_to_string(res));
            return false;
        }
        xd->cc = cairo_create(xd->cs);
        res = cairo_status(xd->cc);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning(_("cairo error '%s'"), cairo_status_to_string(res));
            return false;
        }
        cairo_set_operator(xd->cc, CAIRO_OPERATOR_OVER);
        cairo_reset_clip(xd->cc);
        cairo_set_antialias(xd->cc, xd->antialias);
    }
#ifdef HAVE_CAIRO_SVG
    else if(xd->type == SVG) {
        snprintf(buf, R_PATH_MAX, R_CAIRO_FN(xd), xd->npages + 1);
        xd->cs = cairo_svg_surface_create(buf,
                                          (double)xd->windowWidth,
                                          (double)xd->windowHeight);
        res = cairo_surface_status(xd->cs);
        if (res != CAIRO_STATUS_SUCCESS) {
            xd->cs = NULL;
            warning(_("cairo error '%s'"), cairo_status_to_string(res));
            return false;
        }
        if(xd->onefile)
            cairo_svg_surface_restrict_to_version(xd->cs, CAIRO_SVG_VERSION_1_2);
        xd->cc = cairo_create(xd->cs);
        res = cairo_status(xd->cc);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning(_("cairo error '%s'"), cairo_status_to_string(res));
            return false;
        }
        cairo_set_antialias(xd->cc, xd->antialias);
    }
#endif
#ifdef HAVE_CAIRO_PDF
    else if(xd->type == PDF) {
        snprintf(buf, R_PATH_MAX, R_CAIRO_FN(xd), xd->npages + 1);
        xd->cs = cairo_pdf_surface_create(buf,
                                          (double)xd->windowWidth,
                                          (double)xd->windowHeight);
        res = cairo_surface_status(xd->cs);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning(_("cairo error '%s'"), cairo_status_to_string(res));
            return false;
        }
        cairo_surface_set_fallback_resolution(xd->cs, xd->fallback_dpi,
                                              xd->fallback_dpi);
        xd->cc = cairo_create(xd->cs);
        res = cairo_status(xd->cc);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning(_("cairo error '%s'"), cairo_status_to_string(res));
            return false;
        }
        cairo_set_antialias(xd->cc, xd->antialias);
    }
#endif
#ifdef HAVE_CAIRO_PS
    else if(xd->type == PS) {
        snprintf(buf, R_PATH_MAX, R_CAIRO_FN(xd), xd->npages + 1);
        xd->cs = cairo_ps_surface_create(buf,
                                         (double)xd->windowWidth,
                                         (double)xd->windowHeight);
        res = cairo_surface_status(xd->cs);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning(_("cairo error '%s'"), cairo_status_to_string(res));
            return false;
        }
// We already require >= 1.2
#if CAIRO_VERSION_MAJOR > 2 || CAIRO_VERSION_MINOR >= 6
        if(!xd->onefile)
            cairo_ps_surface_set_eps(xd->cs, TRUE);
#endif
        cairo_surface_set_fallback_resolution(xd->cs, xd->fallback_dpi,
                                              xd->fallback_dpi);
        xd->cc = cairo_create(xd->cs);
        res = cairo_status(xd->cc);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning(_("cairo error '%s'"), cairo_status_to_string(res));
            return false;
        }
        cairo_set_antialias(xd->cc, xd->antialias);
    }
#endif
    else
	error("%s", _("unimplemented cairo-based device"));

    CairoInitPatterns(xd);
    CairoInitClipPaths(xd);
    CairoInitMasks(xd);
    CairoInitGroups(xd);
    xd->appending = 0;

    return true;
}


static int stride;

static unsigned int Cbitgp(void *xi, int x, int y)
{
    unsigned int *data = (unsigned int *) xi;
    return data[x*stride+y];
}

static void BM_Close_bitmap(pX11Desc xd)
{
    if (xd->type == PNGdirect) {
	char buf[R_PATH_MAX];
	snprintf(buf, R_PATH_MAX, R_CAIRO_FN(xd), xd->npages);
	cairo_surface_write_to_png(xd->cs, buf);
	return;
    } 

    void *xi = cairo_image_surface_get_data(xd->cs);
    if (!xi) {
	warning(_("'%s' called on non-surface"), "BM_Close_bitmap");
	return;
    }

    stride = cairo_image_surface_get_stride(xd->cs)/4;
    if (xd->type == PNG)
	R_SaveAsPng(xi, xd->windowWidth, xd->windowHeight,
		    Cbitgp, 0, xd->fp, 0, xd->res_dpi);
    else if(xd->type == JPEG)
	R_SaveAsJpeg(xi, xd->windowWidth, xd->windowHeight,
		     Cbitgp, 0, xd->quality, xd->fp, xd->res_dpi);
    else if(xd->type == BMP)
	R_SaveAsBmp(xi, xd->windowWidth, xd->windowHeight,
		    Cbitgp, 0, xd->fp, xd->res_dpi);
    else {
	char buf[R_PATH_MAX];
	snprintf(buf, R_PATH_MAX, xd->filename, xd->npages);
	/* filename in native encoding on Windows */
	R_SaveAsTIFF(xi, xd->windowWidth, xd->windowHeight,
		     Cbitgp, 0, buf, xd->res_dpi,
		     xd->quality);
    }
}

static void BM_NewPage(const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    char buf[R_PATH_MAX];
    cairo_status_t res;

    xd->npages++;
    if (xd->type == PNG || xd->type == JPEG || xd->type == BMP) {
	if (xd->npages > 1) {
	    /* try to preserve the page we do have */
	    BM_Close_bitmap(xd);
	    if (xd->fp) fclose(xd->fp);
	}
	snprintf(buf, R_PATH_MAX, xd->filename, xd->npages);
	/* filename in native encoding on Windows */
	xd->fp = R_fopen(buf, "wb");
	if (!xd->fp)
	    error(_("could not open file '%s'"), buf);
    }
    else if(xd->type == PNGdirect || xd->type == TIFF) {
	if (xd->npages > 1) {
	    xd->npages--;
	    BM_Close_bitmap(xd);
	    xd->npages++;
	}
    }
#ifdef HAVE_CAIRO_SVG
    else if(xd->type == SVG) {
	if (xd->npages > 1 && xd->cs) {
	    cairo_show_page(xd->cc);
	    if(!xd->onefile) {
		cairo_surface_destroy(xd->cs);
		cairo_destroy(xd->cc);
                snprintf(buf, R_PATH_MAX, R_CAIRO_FN(xd), xd->npages);
                xd->cs = cairo_svg_surface_create(buf,
                                                  (double)xd->windowWidth,
                                                  (double)xd->windowHeight);
                res = cairo_surface_status(xd->cs);
                if (res != CAIRO_STATUS_SUCCESS) {
                    xd->cs = NULL;
                    error(_("cairo error '%s'"), cairo_status_to_string(res));
                }
                if(xd->onefile)
                    cairo_svg_surface_restrict_to_version(xd->cs, CAIRO_SVG_VERSION_1_2);
                xd->cc = cairo_create(xd->cs);
                res = cairo_status(xd->cc);
                if (res != CAIRO_STATUS_SUCCESS) {
                    error(_("cairo error '%s'"), cairo_status_to_string(res));
                }
                cairo_set_antialias(xd->cc, xd->antialias);
            }
        }
    }
#endif
#ifdef HAVE_CAIRO_PDF
    else if(xd->type == PDF) {
	if (xd->npages > 1) {
	    cairo_show_page(xd->cc);
	    if(!xd->onefile) {
		cairo_surface_destroy(xd->cs);
		cairo_destroy(xd->cc);
                snprintf(buf, R_PATH_MAX, R_CAIRO_FN(xd), xd->npages);
                xd->cs = cairo_pdf_surface_create(buf,
                                                  (double)xd->windowWidth,
                                                  (double)xd->windowHeight);
                res = cairo_surface_status(xd->cs);
                if (res != CAIRO_STATUS_SUCCESS) {
                    error(_("cairo error '%s'"), cairo_status_to_string(res));
                }
                cairo_surface_set_fallback_resolution(xd->cs, xd->fallback_dpi,
                                                      xd->fallback_dpi);
                xd->cc = cairo_create(xd->cs);
                res = cairo_status(xd->cc);
                if (res != CAIRO_STATUS_SUCCESS) {
                    error(_("cairo error '%s'"), cairo_status_to_string(res));
                }
                cairo_set_antialias(xd->cc, xd->antialias);
            }
	}
    }
#endif
#ifdef HAVE_CAIRO_PS
    else if(xd->type == PS) {
	if (xd->npages > 1) {
	    cairo_show_page(xd->cc);
	    if(!xd->onefile) {
                cairo_surface_destroy(xd->cs);
                cairo_destroy(xd->cc);
                snprintf(buf, R_PATH_MAX, R_CAIRO_FN(xd), xd->npages);
                xd->cs = cairo_ps_surface_create(buf,
                                                 (double)xd->windowWidth,
                                                 (double)xd->windowHeight);
                res = cairo_surface_status(xd->cs);
                if (res != CAIRO_STATUS_SUCCESS) {
                    error(_("cairo error '%s'"), cairo_status_to_string(res));
                }
// We already require >= 1.2
#if CAIRO_VERSION_MAJOR > 2 || CAIRO_VERSION_MINOR >= 6
                if(!xd->onefile)
                    cairo_ps_surface_set_eps(xd->cs, TRUE);
#endif
                cairo_surface_set_fallback_resolution(xd->cs, xd->fallback_dpi,
                                                      xd->fallback_dpi);
                xd->cc = cairo_create(xd->cs);
                res = cairo_status(xd->cc);
                if (res != CAIRO_STATUS_SUCCESS) {
                    error(_("cairo error '%s'"), cairo_status_to_string(res));
                }
                cairo_set_antialias(xd->cc, xd->antialias);
            }
        }
    }
#endif
    else
	error("%s", _("unimplemented cairo-based device"));

    cairo_reset_clip(xd->cc);
    if (xd->type == PNG  || xd->type == TIFF|| xd->type == PNGdirect) {
	/* First clear it */
	cairo_set_operator (xd->cc, CAIRO_OPERATOR_CLEAR);
	cairo_paint (xd->cc);
	cairo_set_operator (xd->cc, CAIRO_OPERATOR_OVER);
	xd->fill = gc->fill;
    } else
	xd->fill = R_OPAQUE(gc->fill) ? gc->fill: xd->canvas;
    CairoColor(xd->fill, xd);
    cairo_new_path(xd->cc);
    cairo_paint(xd->cc);
}


static void BM_Close(pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (xd->npages)
	if (xd->type == PNG || xd->type == JPEG ||
	    xd->type == TIFF || xd->type == BMP || xd->type == PNGdirect)
	    BM_Close_bitmap(xd);
    if (xd->fp) fclose(xd->fp);
    CairoDestroyGroups(xd);
    CairoDestroyMasks(xd);
    CairoDestroyClipPaths(xd);
    CairoDestroyPatterns(xd);
    if (xd->cc) cairo_show_page(xd->cc);
    if (xd->cs) cairo_surface_destroy(xd->cs);
    if (xd->cc) cairo_destroy(xd->cc);
    free(xd);
}



static bool BMDeviceDriver(pDevDesc dd, int kind, SEXP filename,
	       int quality, int width, int height, int ps,
	       int bg, int res, int antialias, const char *family,
	       double dpi, const char *symbolfamily, bool usePUA)
{
    pX11Desc xd;
    int res0 = (res > 0) ? res : 72;
    double dps = ps;

    /* allocate new device description */
    if (!(xd = (pX11Desc) calloc(1, sizeof(X11Desc)))) return false;
    strncpy(xd->filename, R_ExpandFileName(translateCharFP(filename)),
            R_PATH_MAX - 1);
    xd->filename[R_PATH_MAX - 1] = '\0';
#ifdef R_CAIRO_UTF8_FILENAMES
    // not currently in use
    strncpy(xd->filename, R_ExpandFileName(translateChar(filename)),
            R_PATH_MAX - 1);
    xd->filename[R_PATH_MAX - 1] = '\0';
    strncpy(xd->filenameUTF8, R_ExpandFileNameUTF8(translateCharUTF8(filename)),
            R_PATH_MAX - 1);
    xd->filenameUTF8[R_PATH_MAX - 1] = '\0';
#else
    // thow error if cannot be translated.
    strncpy(xd->filename, R_ExpandFileName(translateCharFP(filename)),
            R_PATH_MAX - 1);
    xd->filename[R_PATH_MAX - 1] = '\0';
#endif
    xd->quality = quality;
    xd->windowWidth = width;
    xd->windowHeight = height;
    strncpy(xd->basefontfamily, family, 499);
    xd->basefontfamily[499] = '\0';
    strncpy(xd->symbolfamily, symbolfamily, 499);
    xd->symbolfamily[499] = '\0';
    xd->usePUA = usePUA;
#ifdef HAVE_PANGOCAIRO
    /* Pango's default resolution is 96 dpi */
    dps *= res0/96.0;
#else
    dps *= res0/72.0;
#endif
    xd->pointsize = dps;
    xd->bg = bg;
    xd->res_dpi = res;
    xd->fallback_dpi = dpi;
    switch(antialias){
    case 1: xd->antialias = CAIRO_ANTIALIAS_DEFAULT; break;
    case 2: xd->antialias = CAIRO_ANTIALIAS_NONE; break;
    case 3: xd->antialias = CAIRO_ANTIALIAS_GRAY; break;
    case 4: xd->antialias = CAIRO_ANTIALIAS_SUBPIXEL; break;
    default: xd->antialias = CAIRO_ANTIALIAS_DEFAULT;
    }
    xd->npages = 0;
    xd->col = R_RGB(0, 0, 0);
    xd->fill = xd->canvas = bg;
    xd->type = (X_GTYPE) kind;
    xd->fp = NULL;
    xd->lty = -1;
    xd->lwd = -1;
    xd->lend = (R_GE_lineend) 0;
    xd->ljoin = (R_GE_linejoin) 0;

    if (!BM_Open(dd, xd, width, height)) {
	free(xd);
	return false;
    }
    if (xd->type == SVG || xd->type == PDF || xd->type == PS)
	xd->onefile = (quality != 0);

    /* Set up Data Structures  */
    dd->size = cbm_Size;
    dd->clip = Cairo_Clip;
    dd->rect = Cairo_Rect;
    dd->circle = Cairo_Circle;
    dd->line = Cairo_Line;
    dd->polyline = Cairo_Polyline;
    dd->polygon = Cairo_Polygon;
    dd->path = Cairo_Path;
    dd->raster = Cairo_Raster;
#ifdef HAVE_PANGOCAIRO
    dd->metricInfo = PangoCairo_MetricInfo;
    dd->strWidth = dd->strWidthUTF8 = PangoCairo_StrWidth;
    dd->text = dd->textUTF8 = PangoCairo_Text;
#else
    dd->metricInfo = Cairo_MetricInfo;
    dd->strWidth = dd->strWidthUTF8 = Cairo_StrWidth;
    dd->text = dd->textUTF8 = Cairo_Text;
#endif
    dd->hasTextUTF8 = TRUE;
#if defined(Win32) && !defined(USE_FC)
    dd->wantSymbolUTF8 = (Rboolean) NA_LOGICAL;
#else
    dd->wantSymbolUTF8 = TRUE;
#endif
    dd->useRotatedTextInContour = FALSE;

    dd->haveTransparency = 2;
    dd->haveRaster = 2;
    switch(xd->type) {
    case PDF:
    case SVG:
    case PNG:
    case PNGdirect:
	dd->haveTransparentBg = 3;
	break;
    case PS:
	dd->haveTransparentBg = 2;
	dd->haveRaster = 3; /* ?? */
	break;
    default: /* TIFF, BMP */
	dd->haveTransparency = 1;
    }

    dd->newPage = BM_NewPage;
    dd->close = BM_Close;

    dd->setPattern = Cairo_SetPattern;
    dd->releasePattern = Cairo_ReleasePattern;
    dd->setClipPath = Cairo_SetClipPath;
    dd->releaseClipPath = Cairo_ReleaseClipPath;
    dd->setMask = Cairo_SetMask;
    dd->releaseMask = Cairo_ReleaseMask;
    dd->defineGroup = Cairo_DefineGroup;
    dd->useGroup = Cairo_UseGroup;
    dd->releaseGroup = Cairo_ReleaseGroup;
    dd->stroke = Cairo_Stroke;
    dd->fill = Cairo_Fill;
    dd->fillStroke = Cairo_FillStroke;
    dd->capabilities = Cairo_Capabilities;
    dd->glyph = Cairo_Glyph;

    dd->left = 0;
    dd->right = width;
    dd->top = 0;
    dd->bottom = height;
    /* rescale points to pixels */
    dd->cra[0] = 0.9 * ps * res0/72.0;
    dd->cra[1] = 1.2 * ps * res0/72.0;
    dd->startps = ps;
    xd->fontscale = dps/ps;
    dd->ipr[0] = dd->ipr[1] = 1.0/res0;
    xd->lwdscale = res0/96.0;
    dd->xCharOffset = 0.4900;
    dd->yCharOffset = 0.3333;
    dd->yLineBias = 0.2;
    dd->canClip= TRUE;
    dd->canHAdj = 2;
    dd->canChangeGamma = FALSE;
    dd->startcol = xd->col;
    dd->startfill = xd->fill;
    dd->startlty = LTY_SOLID;
    dd->startfont = 1;
    dd->startgamma = 1;
    dd->displayListOn = FALSE;
    dd->deviceVersion = R_GE_fontVar;

    dd->deviceSpecific = (void *) xd;

    return true;
}

const static struct {
    const char * const name;
    X_GTYPE gtype;
} devtable[] = {
    { "", WINDOW },
    { "", XIMAGE },
    { "png", PNG },
    { "jpeg", JPEG },
    { "svg", SVG },
    { "png", PNGdirect },
    { "cairo_pdf", PDF },
    { "cairo_ps", PS },
    { "tiff", TIFF },
    { "bmp", BMP }
};

/*
   cairo(filename, type, width, height, pointsize, bg, res, antialias, 
         quality, family, dpi, symbolfamily)
*/
#ifdef __cplusplus
extern "C"
#endif
SEXP in_Cairo(SEXP args)
{
    pGEDevDesc gdd;
    SEXP sc;
    const char *family, *symbolfamily;
    int type, quality, width, height, pointsize, bgcolor, res, antialias;
    double dpi;
    SEXP filename;
    CXXR::RAllocStack::Scope rscope;

    args = CDR(args); /* skip entry point name */
    if (!isString(CAR(args)) || LENGTH(CAR(args)) < 1)
	error(_("invalid '%s' argument"), "filename");
    filename = STRING_ELT(CAR(args), 0);
    if (filename == NA_STRING)
	error(_("invalid '%s' argument"), "filename");
    args = CDR(args);
    type = asInteger(CAR(args));
    if(type == NA_INTEGER || type <= 0)
	error(_("invalid '%s' argument"), "type");
    args = CDR(args);
    width = asInteger(CAR(args));
    if(width == NA_INTEGER || width <= 0)
	error(_("invalid '%s' argument"), "width");
    args = CDR(args);
    height = asInteger(CAR(args));
    if(height == NA_INTEGER || height <= 0)
	error(_("invalid '%s' argument"), "height");
    args = CDR(args);
    pointsize = asInteger(CAR(args));
    if(pointsize == NA_INTEGER || pointsize <= 0)
	error(_("invalid '%s' argument"), "pointsize");
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	error(_("invalid '%s' value"), "bg");
    bgcolor = RGBpar(sc, 0);
    args = CDR(args);
    res = asInteger(CAR(args));
    args = CDR(args);
    antialias = asInteger(CAR(args));
    if(antialias == NA_INTEGER)
	error(_("invalid '%s' argument"), "antialias");
    args = CDR(args);
    quality = asInteger(CAR(args));
    if(type == JPEG) {
	if(quality == NA_INTEGER || quality < 0 || quality > 100)
	    error(_("invalid '%s' argument"), "quality");
    } else if(type == TIFF) {
	if(quality == NA_INTEGER)
	    error(_("invalid '%s' argument"), "compression");
    }
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) < 1)
	error(_("invalid '%s' argument"), "family");
    family = translateChar(STRING_ELT(CAR(args), 0));
    args = CDR(args);
    dpi = asReal(CAR(args));
    if(ISNAN(dpi) || dpi <= 0)
	error(_("invalid '%s' argument"), "dpi");
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) < 1)
	error(_("invalid '%s' argument"), "symbolfamily");
    symbolfamily = translateChar(STRING_ELT(CAR(args), 0));
    /* scsymbol forced to have "usePUA" attribute in R code */
    bool usePUA = asBool(getAttrib(CAR(args), install("usePUA")));

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	pDevDesc dev;
	/* Allocate and initialize the device driver data */
	if (!(dev = (pDevDesc) calloc(1, sizeof(DevDesc)))) return 0;
	if (!BMDeviceDriver(dev, devtable[type].gtype, filename, quality,
			    width, height, pointsize,
			    bgcolor, res, antialias, family, dpi,
                            symbolfamily, usePUA)) {
	    free(dev);
	    error(_("unable to start device '%s'"), devtable[type].name);
	}
	gdd = GEcreateDevDesc(dev);
	pX11Desc xd = (pX11Desc) dev->deviceSpecific;
	/* filename in native encoding on Windows */
	GEaddDevice2f(gdd, devtable[type].name, xd->filename);
    } END_SUSPEND_INTERRUPTS;

    return R_NilValue;
}

#ifdef __cplusplus
extern "C"
#endif
SEXP in_CairoVersion(void)
{
    SEXP ans = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkChar(cairo_version_string()));
    UNPROTECT(1);
    return ans;
}

#ifdef __cplusplus
extern "C"
#endif
SEXP in_PangoVersion(void)
{
#ifdef HAVE_PANGOCAIRO
    SEXP ans = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkChar(pango_version_string()));
    UNPROTECT(1);
    return ans;
#else
    return mkString("");
#endif
}
