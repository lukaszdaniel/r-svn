/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2023  The R Core Team
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
#include <config.h>
#endif
#include <Localization.h>

#ifdef Win32
#define USE_MDI 1
#endif

#include <cstdio>
#include "graphapp/ga.h"
#include "graphapp/graphapp.h"
#include "graphapp/stdimg.h"
#include "console.h"
#include "consolestructs.h"
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include "rui.h"
#include "editor.h"
#include <CXXR/String.hpp>
#include <Defn.h>
#include <Rembedded.h>
#include <Rinterface.h>
#include <Fileio.h>
#include <Startup.h>

using namespace R;

#undef gettext
#define gettext GA_gettext

#define MCHECK(a) if (!(a)) {del(c); return NULL;}

// FIXME headers
RECT *RgetMDIsize(void); /* in rui.c */

/* Pointers to currently open editors */
static editor REditors[MAXNEDITORS];
static int neditors  = 0;
static bool fix_editor_up = FALSE;

static EditorData neweditordata(int file)
{
    EditorData p;
    p = (EditorData) malloc(sizeof(struct structEditorData));
    p->file = file;
    p->filename = NULL;
    p->title = (char *) malloc((EDITORMAXTITLE + 1)*sizeof(char));
    p->title[EDITORMAXTITLE] = p->title[0] = '\0';
    return p;
}

void deleditordata(EditorData p)
{
    if (p->stealconsole)
	fix_editor_up = FALSE;
    if (p->filename)
	free(p->filename);
    free(p->title);
    free(p->hmenu);
    free(p->pmenu);
    free(p);
}

static void editor_set_title(editor c, const char *title)
{
    char wtitle[EDITORMAXTITLE+1];
    textbox t = (textbox) getdata(c);
    EditorData p = (EditorData) getdata(t);
    strncpy(wtitle, title, EDITORMAXTITLE);
    wtitle[EDITORMAXTITLE] = '\0';
    strcpy(p->title, wtitle);
    if (strlen(wtitle) + strlen(G_("R Editor")) + 3 < EDITORMAXTITLE) {
	strcat(wtitle, " - ");
	strcat(wtitle, G_("R Editor"));
    }
    settext(c, wtitle);
}

/*** FILE MANAGEMENT FUNCTIONS ***/

/* FIXME: simplify this once UTF-8 is the only supported native encoding. */

static wchar_t* utf8_to_wchar(const char *src)
{
    size_t needed = Rf_utf8towcs(NULL, src, 0);
    wchar_t *res = (wchar_t  *)malloc((needed + 1)*sizeof(wchar_t));
    if (res)
	Rf_utf8towcs(res, src, needed + 1);
    return res;
}
	
static char* utf8_to_native(const char *src)
{
    /* a defensive guess, reEnc2 would throw error if not enough */
    size_t needed = strlen(src) * 4;
    char *res = (char *)malloc(needed + 1);
    if (res)
	reEnc2(src, res, needed + 1, CE_UTF8, CE_NATIVE, 3);
    return res;
}	

static char* native_to_utf8(const char *src)
{
    /* a defensive guess, reEnc2 would throw error if not enough */
    size_t needed = strlen(src) * 4;
    char *res = (char *)malloc(needed + 1);
    if (res)
	reEnc2(src, res, needed + 1, CE_NATIVE, CE_UTF8, 3);
    return res;
}	

static char* wchar_to_utf8(const wchar_t *src)
{
    size_t needed = wcstoutf8(NULL, src, INT_MAX);
    char *res = (char *)malloc(needed + 1);
    if (res)
	wcstoutf8(res, src, needed + 1);
    return res;
}

#define MSGSIZE 512

static void editor_load_file(editor c, const char *name, int enc)
{
    textbox t = (textbox) getdata(c);
    EditorData p = (EditorData) getdata(t);
    FILE *f;
    char *buffer = NULL, tmp[MSGSIZE];
    const char *sname;
    long num = 1, bufsize;

    if(enc == CE_UTF8) {
	wchar_t *wname = utf8_to_wchar(name);
	f = R_wfopen(wname, L"r");
	free(wname);
	sname = utf8_to_native(name);
    } else {
	f = R_fopen(name, "r");
	sname = name;
    }
    if (f == NULL) {
	snprintf(tmp, MSGSIZE, 
		 G_("unable to open file %s for reading"), sname);
	R_ShowMessage(tmp);
	if (enc == CE_UTF8)
	    free((char *)sname); 
	return;
    }
    p->file = TRUE;
    if (p->filename)
	free(p->filename);
    if (enc == CE_UTF8) {
	p->filename = (char *) malloc(strlen(name) + 1);
	strcpy(p->filename, name);
    } else
	p->filename = native_to_utf8(name);

    bufsize = 0;
    while (num > 0) {
	buffer = (char *) realloc(buffer, bufsize + 3000 + 1);
	num = fread(buffer + bufsize, 1, 3000 - 1, f);
	if (num >= 0) {
	    bufsize += num;
	    buffer[bufsize] = '\0';
	}
	else {
	    snprintf(tmp, MSGSIZE,
		     G_("Could not read from file '%s'"), sname);
	    askok(tmp);
	}
    }
    setlimittext(t, 2 * strlen(buffer));
    settext(t, buffer);
    gsetmodified(t, 0);
    free(buffer);
    fclose(f);
    if (enc == CE_UTF8)
	free((char *)sname); 
}

static void editor_save_file(editor c, const char *name, int enc)
{
    textbox t = (textbox) getdata(c);
    FILE *f;
    char buf[MSGSIZE];
    const char *sname;

    if (name == NULL)
	return;
    else {
	if(enc == CE_UTF8) {
	    wchar_t *wname = utf8_to_wchar(name);
	    f = R_wfopen(wname, L"w");
	    free(wname);
	    sname = utf8_to_native(name);
	} else {
	    sname = name;
	    f = R_fopen(sname, "w");
	}
	if (f == NULL) {
	    snprintf(buf, MSGSIZE, G_("Could not save file '%s'"), sname);
	    askok(buf);
	    if (enc == CE_UTF8)
		free((char *)sname);
	    return;
	}
	fprintf(f, "%s", gettext(t));
	fclose(f);
	if (enc == CE_UTF8)
	    free((char *)sname);
    }
}

static void editorsaveas(editor c)
{
    textbox t = (textbox) getdata(c);
    EditorData p = (EditorData) getdata(t);
    wchar_t *wname;

    setuserfilterW(L"R files (*.R)\0*.R\0S files (*.q, *.ssc, *.S)\0*.q;*.ssc;*.S\0All files (*.*)\0*.*\0\0");
    wname = askfilesaveW(G_("Save script as"), "");
    if (wname) {
	char *name = wchar_to_utf8(wname);
	char *q = strchr(name, '.');
	if(!q) {
	    char *tmp = (char *)malloc(strlen(name) + 2 + 1);
	    if (tmp) {
		strcpy(tmp, name);
		strcat(tmp, ".R");
		free(name);
		name = tmp;
	    }
	}
	editor_save_file(c, name, CE_UTF8);
	p->file = TRUE;
	p->filename = name; /* keeps name */
	gsetmodified(t, 0);
	char *sname = utf8_to_native(name);
	editor_set_title(c, sname);
	free(sname);
    }
    show(c);
}

static void menueditorsaveas(control m)
{
    editor c = (editor) getdata(m);
    editorsaveas(c);
}

static void editorsave(editor c)
{
    textbox t = (textbox) getdata(c);
    EditorData p = (EditorData) getdata(t);
    if (p->file) {  /* save existing file without prompt */
	editor_save_file(c, p->filename, CE_UTF8);
	gsetmodified(t, 0);
    }
    /* if new file then prompt for name to save as */
    else editorsaveas(c);
}

static void menueditorsave(control m)
{
    editor c = (editor) getdata(m);
    editorsave(c);
    show(c); /* steal focus back from tool button */
}

/* global console configuration variables, from console.c */
extern char fontname[LF_FACESIZE+4];
extern int fontsty, pointsize;

static void editorprint(control m)
{
    printer lpr;
    font f;
    textbox t = (textbox) getdata(m);
    const char *contents = gettext(t);
    char msg[LF_FACESIZE + 128];
    char *linebuf = NULL;
    int cc, rr, fh, page, linep, i, j, istartline;
    int top, left;

    if (!(lpr = newprinter(0.0, 0.0, ""))) return;
    f = gnewfont(lpr, !streql(fontname, "FixedFont") ? fontname : "Courier New",
		 fontsty, pointsize, 0.0, 1);
    top = devicepixelsy(lpr) / 5;
    left = devicepixelsx(lpr) / 5;
    fh = fontheight(f);
    rr = getheight(lpr) - top;
    cc = getwidth(lpr) - 2*left;

    linep = rr; /* line in printer page */
    page = 1;
    i = 0;
    while ((size_t) i < strlen(contents)) {
	if ( linep + fh >= rr ) { /* new page */
	    if (page > 1) nextpage(lpr);
	    snprintf(msg, LF_FACESIZE + 128, "Page %d", page++);
	    gdrawstr(lpr, f, Black, pt(cc - gstrwidth(lpr, f, msg) - 1, top),
		     msg);
	    linep = top + 2*fh;
	}
	j = 0;
	istartline = i;
	while (contents[i] != '\n' && contents[i] != '\0') {
	    ++i; ++j;
	}
	linebuf = (char *) realloc(linebuf, (j+1)*sizeof(char));
	strncpy(linebuf, &contents[istartline], j);
	linebuf[j] = '\0';
	gdrawstr(lpr, f, Black, pt(left, linep), linebuf);
	linep += fh;
	++i;
    }
    free(linebuf);
    if (f != FixedFont) del(f);
    del(lpr);
}

static void editorconsole(editor c)
{
    show(RConsole);
}

/* Remove global pointer to editor when closing an editor. Fill the
 * gap in the array with the last editor in the list */

static void editorupdateglobals(editor c)
{
    int i = 0;
    if (neditors > 1) {
	while (REditors[i] != c) ++i;
	if (i < neditors - 1)
	    REditors[i] = REditors[neditors-1];
    }
    --neditors;
}

/* Hooks called when editor window is destroyed */

static void editordel(editor c)
{
    editorupdateglobals(c);
}

static void textboxdel(textbox t)
{
    EditorData p = (EditorData) getdata(t);
    deleditordata(p);
}

int editorchecksave(editor c)
{
    textbox t = (textbox) getdata(c);
    EditorData p = (EditorData) getdata(t);
    int save;
    char buf[EDITORMAXTITLE + 100];
    if (ggetmodified(t)) {
	snprintf(buf, EDITORMAXTITLE + 100,
		 "\"%s\" has been modified.  Do you want to save the changes?",
		 (p->title ? p->title : "Untitled"));
	save = askyesnocancel(buf);
	switch (save) {
	case YES:
	    editorsave(c);
	    break;
	case NO:
	    break;
	case CANCEL:
	    return 1;
	}
    }
    return 0;
}

static void editorclose(editor c)
{
    if (!editorchecksave(c))
	del(c);
}

static void menueditorclose(control m)
{
    editor c = (editor) getdata(m);
    editorclose(c);
}

/* Called when exiting Rgui, check if any open editors need saving */

void editorcleanall(void)
{
    for (int i = neditors-1;  i >= 0; --i) {
	if (editorchecksave(REditors[i])) {
	    R_ProcessEvents();  // see R_CleanUp
	    jump_to_toplevel();
	}
	del(REditors[i]);
    }
}

static void editornew(void)
{
    Rgui_Edit("", CE_NATIVE, "", 0);
}

void menueditornew(control m)
{
    editornew();
}

static void editoropen(const char *default_name)
{
    wchar_t *wname;

    textbox t; EditorData p;
    setuserfilterW(L"R files (*.R)\0*.R\0S files (*.q, *.ssc, *.S)\0*.q;*.ssc;*.S\0All files (*.*)\0*.*\0\0");
    wname = askfilenameW(G_("Open script"), default_name); /* returns NULL if open dialog cancelled */
    if (wname) {
	char *name = wchar_to_utf8(wname);
	/* check if file is already open in an editor. If so, close and open again */
	for (int i = 0; i < neditors; ++i) {
	    t = (textbox) getdata(REditors[i]);
	    p = (EditorData) getdata(t);
	    if (p->filename && streql(name, p->filename)) {
		editorclose(REditors[i]);
		break;
	    }
	}
	char *sname = utf8_to_native(name);
	Rgui_Edit(name, CE_UTF8, sname, 0);
	free(name);
	free(sname);
    } else show(RConsole);
}

void menueditoropen(control m)
{
    editor c = (editor) getdata(m);
    /* It really is not clear what is meant here: seems to assume an
       editor window but is called from elsewhere, hopefully with NULL
       (since 2.1.1 patched). */
    if (c) {
	textbox t = (textbox) getdata(c);
	EditorData p = (EditorData) getdata(t);
	if (p->file && p->filename) {
	    /* name of currently-open file, if there is one */
	    char *sname = utf8_to_native(p->filename);
	    editoropen(sname);
	    free(sname);
	    return;
	}
    } 
    editoropen("");
}


/*** EDITING FUNCTIONS ***/

static void editorundo(control m)
{
    textbox t = (textbox) getdata(m);
    undotext(t);
}

static void editorcut(control m)
{
    textbox t = (textbox) getdata(m);
    cuttext(t);
}

static void editorcopy(control m)
{
    textbox t = (textbox) getdata(m);
    copytext(t);
}

static void editorpaste(control m)
{
    textbox t = (textbox) getdata(m);
    /* check whether the widget text limit needs to be increased
     * before doing the paste */
    int pastelen = getpastelength();
    checklimittext(t, pastelen + 1);
    pastetext(t);
}

static void editordelete(control m)
{
    textbox t = (textbox) getdata(m);
    cleartext(t);
}

static void editorselectall(control m)
{
    textbox t = (textbox) getdata(m);
    selecttextex(t, 0, -1);
}

static void editorfind(control m)
{
    textbox t = (textbox) getdata(m);
    finddialog(t);  /* actual find/replace work is in graphapp/dialogs.c */
}

static void editorreplace(control m)
{
    textbox t = (textbox) getdata(m);
    replacedialog(t);
}


/*** FUNCTIONS FOR RUNNING R CODE FROM THE EDITOR ***/

static void editorrunline(textbox t)
{
    int length = getlinelength(t); /* return character num */
    /* Extra space for null and word length in getcurrentline */
    size_t alength = length * MB_CUR_MAX + 1 + sizeof(WORD);
    char *line = (char *) malloc(alength);
    memset(line, 0, alength);
    getcurrentline(t, line, length);
    consolecmd(RConsole, line);
    free(line);
    scrollcaret(t, 1);
    show(t);
}

/* For the moment, send the selected text to the console via the
   clipboard. Wasteful, but currently consolecmd only allows a maximum
   of 8K of text */

static void editorrunselection(textbox t, long start, long end)
{
    copytext(t);
    if (consolecanpaste(RConsole)) {
	consolepaste(RConsole);
	if (gettext(t)[end-1] != '\n')
	    consolenewline(RConsole);
    }
}

static bool busy_running = FALSE;

static void editorrun(textbox t)
{
    if (!busy_running) {
	long start=0, end=0;
	if (CharacterMode != RGui) {
	    R_ShowMessage(G_("No RGui console to paste to"));
	    return;
	}
	busy_running = TRUE;
	textselectionex(t, &start, &end);
	if (start >= end)
	    editorrunline(t);
	else {
	    editorrunselection(t, start, end);
	    selecttextex(t, end, end); /* move insertion point to end of selection after running */
	}
	busy_running = FALSE;
    }
}

static void menueditorrun(control m)
{
    textbox t = (textbox) getdata(m);
    editorrun(t);
}

static void editorrunall(control m)
{
    textbox t = (textbox) getdata(m);
    long start=0, end=0;
    textselectionex(t, &start, &end); /* save current selection state */
    selecttextex(t, 0, -1);
    editorrun(t);
    selecttextex(t, start, end);  /* return to original selection state */
}

/* Grey out menu buttons as appropriate */

static void editormenuact(control m)
{
    long start, end;
    textbox t = (textbox) getdata(m);
    EditorData p = (EditorData) getdata(t);
    textselectionex(t, &start, &end);
    if (start < end) {
	enable(p->mcut);
	enable(p->mcopy);
	enable(p->mdelete);
	enable(p->mpopcut);
	enable(p->mpopcopy);
	enable(p->mpopdelete);
    }
    else {
	disable(p->mcut);
	disable(p->mcopy);
	disable(p->mdelete);
	disable(p->mpopcut);
	disable(p->mpopcopy);
	disable(p->mpopdelete);
    }
    if (modeless_active()){
	disable(p->mfind);
	disable(p->mreplace);
    }
    else {
	enable(p->mfind);
	enable(p->mreplace);
    }
    helpmenuact(p->hmenu);
    pkgmenuact(p->pmenu);
}

static void editorresize(editor c, rect r)
{
    resize((control) getdata(c), r);
    resize(c, r);
}

static void editorcontrolkeydown(textbox t, int key)
{
    switch (key) {
    case F5:
	editorrun(t);
	break;
    }
}

static void editorasciikeydown(textbox t, int key)
{
    /* check whether the text limit is about to be exceeded and
       increase it if necessary.  Ugh - this callback is called after
       the text is inserted, so we make space for two characters */
    checklimittext(t, 2);
}

/* Set keyboard focus to the text editing area when the editor window receives focus */

static void editorfocus(editor c)
{
    textbox t = (textbox) getdata(c);
    show(t);
}

static void editorhelp(void)
{
    char s[4096];

    strcpy(s, G_("R EDITOR\n"));
    strcat(s, "\n");
    strcat(s, G_("A standard text editor for editing and running R code.\n"));
    strcat(s, "\n");
    strcat(s, G_("RUNNING COMMANDS\n"));
    strcat(s, G_("To run a line or section of R code, select the code and either\n"));
    strcat(s, G_("     Press Ctrl-R\n"));
    strcat(s, G_("     Select \"Run line or selection\" from the \"Edit\" menu\n"));
    strcat(s, G_("     Press the \"Run line or selection\" icon on the toolbar\n"));
    strcat(s, G_("This will copy the selected commands to the console and evaluate them.\n"));
    strcat(s, G_("If there is no selection, this will just run the current line and advance\n"));
    strcat(s, G_("the cursor by one line.\n"));

    askok(s);
}


static void menueditorhelp(control m)
{
    editorhelp();
}

static MenuItem EditorPopup[] = {                /* Numbers used below */
    {GN_("Run line or selection"), menueditorrun, 'R', 0}, /* 0 */
    {"-", 0, 0, 0},
    {GN_("Undo"), editorundo, 'Z', 0},                     /* 2 */
    {"-", 0, 0, 0},
    {GN_("Cut"), editorcut, 'X', 0},                       /* 4 */
    {GN_("Copy"), editorcopy, 'C', 0},                     /* 5 */
    {GN_("Paste"), editorpaste, 'V', 0},                   /* 6 */
    {GN_("Delete"), editordelete, 0, 0},                   /* 7 */
    {"-", 0, 0, 0},
    {GN_("Select all"), editorselectall, 'A', 0},          /* 9 */
    LASTMENUITEM
};

static editor neweditor(void)
{
    int x, y, w, h, w0, h0;
    editor c;
    menuitem m;
    textbox t;
    long flags;
    font editorfn = (consolefn ? consolefn : FixedFont);
    EditorData p = neweditordata(0);
    DWORD rand;

    w = (pagercol + 1)*fontwidth(editorfn);
    h = (pagerrow + 1)*fontheight(editorfn) + 1;
#ifdef USE_MDI
    if(ismdi()) {
	RECT *pR = RgetMDIsize();
	w0 = pR->right;
	h0 = pR->bottom;
    } else {
#endif
	w0 = devicewidth(NULL);
	h0 = deviceheight(NULL);
#ifdef USE_MDI
    }
#endif
    x = (w0 - w) / 2; x = x > 20 ? x : 20;
    y = (h0 - h) / 2; y = y > 20 ? y : 20;
    rand = GetTickCount();
    w0 = 0.4*x; h0 = 0.4*y;
    w0 = w0 > 20 ? w0 : 20;
    h0 = h0 > 20 ? h0 : 20;
    x += (rand % w0) - w0/2;
    y += ((rand/w0) % h0) - h0/2;
    flags = StandardWindow | Menubar;
#ifdef USE_MDI
    if (ismdi()) flags |= Document;
#endif
    c = (editor) newwindow("", rect(x, y, w, h), flags);
    t = newrichtextarea(NULL, rect(0, 0, w, h));
    setdata(c, t);
    setdata(t, p);

    gsetcursor(c, ArrowCursor);
    setforeground(c, guiColors[editorfg]);
    setbackground(c, guiColors[editorbg]);
    setbackground(t, guiColors[editorbg]);
    
#ifdef USE_MDI
    if (ismdi() && (RguiMDI & RW_TOOLBAR)) {
	int btsize = 24;
	rect r = rect(2, 2, btsize, btsize);
	control tb, bt;
	addto(c);
	MCHECK(tb = newtoolbar(btsize + 4));
	addto(tb);
	MCHECK(bt = newtoolbutton(open_image, r, menueditoropen));
	MCHECK(addtooltip(bt, G_("Open script")));
	setdata(bt, c);
	r.x += (btsize + 1) ;
	MCHECK(bt = newtoolbutton(save_image, r, menueditorsave));
	MCHECK(addtooltip(bt,  G_("Save script")));
	setdata(bt, c);
	r.x += (btsize + 6);
	MCHECK(bt = newtoolbutton(copy1_image, r, menueditorrun));
	MCHECK(addtooltip(bt, G_("Run line or selection")));
	setdata(bt, t);
	r.x += (btsize + 6);
	MCHECK(bt = newtoolbutton(console_image, r, editorconsole));
	MCHECK(addtooltip(bt, G_("Return focus to Console")));
	r.x += (btsize + 6);
	MCHECK(bt = newtoolbutton(print_image, r, editorprint));
	MCHECK(addtooltip(bt, G_("Print script")));
	setdata(bt, t);
	MCHECK(addtooltip(bt, G_("Print")));
    }
#endif
    addto(c);
    /* Right-click context menu */
    MCHECK(m = gpopup(editormenuact, EditorPopup));
    setdata(m, t);
    setdata(EditorPopup[0].m, t);
    setdata(EditorPopup[2].m, t);
    setdata(p->mpopcut = EditorPopup[4].m, t);
    setdata(p->mpopcopy = EditorPopup[5].m, t);
    setdata(EditorPopup[6].m, t);
    setdata(p->mpopdelete = EditorPopup[7].m, t);
    setdata(EditorPopup[9].m, t);

    addto(c);
    MCHECK(m = newmenubar(editormenuact));
    setdata(m, t);
    MCHECK(newmenu(G_("File")));
    MCHECK(m = newmenuitem(G_("New script"), 'N', menueditornew));
    setdata(m, c);
    MCHECK(m = newmenuitem(G_("Open script..."), 'O', menueditoropen));
    setdata(m, c);
    MCHECK(m = newmenuitem(G_("Save"), 'S', menueditorsave));
    setdata(m, c);
    MCHECK(m = newmenuitem(G_("Save as..."), 0, menueditorsaveas));
    setdata(m, c);
    MCHECK(m = newmenuitem("-", 0, NULL));
    MCHECK(m = newmenuitem(G_("Print..."), 'P', editorprint));
    setdata(m, t);
    MCHECK(m = newmenuitem("-", 0, NULL));
    MCHECK(m = newmenuitem(G_("Close script"), 0, menueditorclose));
    setdata(m, c);
    MCHECK(newmenu(G_("Edit")));
    MCHECK(m = newmenuitem(G_("Undo"), 'Z', editorundo));
    setdata(m, t);
    MCHECK(m = newmenuitem("-", 0, NULL));
    MCHECK(p->mcut = newmenuitem(G_("Cut"), 'X', editorcut));
    setdata(p->mcut, t);
    MCHECK(p->mcopy = newmenuitem(G_("Copy"), 'C', editorcopy));
    setdata(p->mcopy, t);
    MCHECK(m = newmenuitem(G_("Paste"), 'V', editorpaste));
    setdata(m, t);
    MCHECK(p->mdelete = newmenuitem(G_("Delete"), 0, editordelete));
    setdata(p->mdelete, t);
    MCHECK(m = newmenuitem(G_("Select all"), 'A', editorselectall));
    setdata(m, t);
    MCHECK(newmenuitem(G_("Clear console"), 'L', menuclear));
    MCHECK(m = newmenuitem("-", 0, NULL));
    MCHECK(m = newmenuitem(G_("Run line or selection"), 'R', menueditorrun));
    setdata(m, t);
    MCHECK(m = newmenuitem(G_("Run all"), 0, editorrunall));
    setdata(m, t);
    MCHECK(m = newmenuitem("-", 0, NULL));
    MCHECK(p->mfind = newmenuitem(G_("Find..."), 'F', editorfind));
    setdata(p->mfind, t);
    MCHECK(p->mreplace = newmenuitem(G_("Replace..."), 'H', editorreplace));
    setdata(p->mreplace, t);
    MCHECK(m = newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("GUI preferences..."), 0, menuconfig));

    /* Packages menu should go here */
    p->pmenu = (PkgMenuItems) malloc(sizeof(struct structPkgMenuItems));
    RguiPackageMenu(p->pmenu);

#ifdef USE_MDI
    newmdimenu(); /* Create and fill the 'Window' menu */
#endif

    MCHECK(m = newmenu(G_("Help")));
    MCHECK(newmenuitem(G_("Editor"), 0, menueditorhelp));
    MCHECK(newmenuitem("-", 0, NULL));
    p->hmenu = (HelpMenuItems) malloc(sizeof(struct structHelpMenuItems));
    RguiCommonHelp(m, p->hmenu);

    settextfont(t, editorfn);
    setforeground(t, guiColors[editorfg]);    
    setresize(c, editorresize);
    setclose(c, editorclose);
    setdel(c, editordel);
    setdel(t, textboxdel);
    setonfocus(c, editorfocus);
    setkeyaction(t, editorcontrolkeydown);
    setkeydown(t, editorasciikeydown);

    /* Store pointer to new editor in global array */
    REditors[neditors] = c;
    ++neditors;
    return c;
}

/* Change the font used in all running editors */

void editorsetfont(font f)
{
    int i, ismod;
    textbox t;
    for (i = 0; i < neditors; i++) {
	t = (textbox) getdata(REditors[i]);
	ismod = ggetmodified(t);
	/* Don't change the modification flag when changing font  */
	settextfont(t, f);
	gsetmodified(t, ismod);
	show(t);
    }
}

static void eventloop(editor c)
{
    while (fix_editor_up) {
	/* avoid consuming 100% CPU time here */
	R_WaitEvent();
	R_ProcessEvents();
    }
}

/* Open existing file for editing or open blank editor for a new
   file. If calling from fix() or edit(), then don't send events to
   the console until editor is closed.  */

#include <unistd.h>

int Rgui_Edit(const char *filename, int enc, const char *title,
	      int modal)
{
    editor c;
    EditorData p;

    if (neditors == MAXNEDITORS) {
	R_ShowMessage(G_("Maximum number of editors reached"));
	return 1;
    }
    c = neweditor();
    if (!c) {
	R_ShowMessage(G_("Unable to create editor window"));
	return 1;
    }
    if (strlen(filename) > 0) {
	editor_load_file(c, filename, enc);
	editor_set_title(c, title);
    }
    else {
	editor_set_title(c, G_("Untitled"));
    }
    show(c);

    p = (EditorData) getdata((control) getdata(c));
    p->stealconsole = modal;
    if (modal) {
	fix_editor_up = TRUE;
	eventloop(c);
    }
    return 0;
}
