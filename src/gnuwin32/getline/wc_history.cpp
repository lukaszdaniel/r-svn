#include <cstdlib> /* for malloc */
#include <cstdio>
#include <vector>
#include <strings.h>
#include <cwchar>
#include "wc_history.h"

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <Localization.h>
#include <R_ext/Error.h>

static int	HIST_SIZE = 512;
static int      hist_pos = 0, hist_last = 0, gl_beep_on = 1;
static std::vector<wchar_t *> hist_buf;
static int      wgl_init_done = -1;

static void gl_error(const char *msg)
{
    char buf[1001];
    
    wgl_init_done = 1;
    snprintf(buf, 1000, 
	     "%s\nDisabling commands history for this session", msg);
    buf[1000] = '\0';
    R_ShowMessage(buf);
}

static void gl_beep(void)
{
    if (gl_beep_on) MessageBeep(MB_OK);
}

static wchar_t *hist_save(const wchar_t *p)        
/* makes a copy of the string */
{
    wchar_t *s = 0;
    int   len = wcslen(p);
    const wchar_t *nl = wcschr(p, L'\n');

    if (nl) {
        if ((s = (wchar_t *) malloc(len * sizeof(wchar_t)))) {
            wcsncpy(s, p, len-1);
	    s[len-1] = 0;
	}
    } else {
        if ((s = (wchar_t *) malloc((len+1) * sizeof(wchar_t))))
            wcscpy(s, p);
    }
    if (s == 0)
	gl_error(_("*** Error: hist_save() failed on malloc"));
    
    return s;
}

void wgl_hist_init(int size, int beep)
{
    HIST_SIZE = size;
    hist_buf.resize(size);
    if (hist_buf.empty()) {
	gl_error(_("\n*** Error: wgl_hist_init() failed on malloc\n"));
	return;
    }
    hist_buf[0] = (wchar_t*) L"";
    for (int i = 1; i < HIST_SIZE; i++)
	hist_buf[i] = (wchar_t *)0;
    hist_pos = hist_last = 0;
    wgl_init_done = 0;
    gl_beep_on = beep;
}

void wgl_histadd(const wchar_t *buf)
{
    const wchar_t *p = buf;

    if(wgl_init_done > 0) return;
    while (*p == ' ' || *p == '\t' || *p == '\n') p++;
    if (*p) {
	hist_buf[hist_last] = hist_save(buf);
	hist_last = hist_last + 1;
	if(hist_last > HIST_SIZE - 1) {
	    int i, size = HIST_SIZE + 512;
	    hist_buf.resize(size);
	    if (hist_buf.empty()) {
		gl_error(_("*** Error: wgl_histadd() failed on realloc"));
		return;
	    }
	    for(i = HIST_SIZE; i < size; i++)
		hist_buf[i] = (wchar_t *)0;
	    HIST_SIZE = size;
	}
	hist_buf[hist_last] = (wchar_t*) L"";
    }
    hist_pos = hist_last;
}

const wchar_t *wgl_hist_prev(void)
/* loads previous hist entry into input buffer, sticks on first */
{
    const wchar_t *p = 0;
    int   next = hist_pos - 1;

    if(wgl_init_done) return L"";
    if (hist_buf[hist_pos] != 0 && next >= 0) {
        hist_pos = next;
        p = hist_buf[hist_pos];
    } 
    if (p == 0) {
	p = L"";
	gl_beep();
    }
    return p;
}

const wchar_t *wgl_hist_next(void)
/* loads next hist entry into input buffer, clears on last */
{
    const wchar_t *p = 0;

    if(wgl_init_done) return L"";
    if (hist_pos != hist_last) {
        hist_pos = hist_pos+1;
	p = hist_buf[hist_pos];
    } 
    if (p == 0) {
	p = L"";
	gl_beep();
    }
    return p;
}

void wgl_savehistory(const char *file, int size)
{
    if (wgl_init_done || !file || !hist_last) return;
    FILE *fp = fopen(file, "w");
    if (!fp) {
       char msg[256];
       snprintf(msg, 256, _("Unable to open %s"), file);
       R_ShowMessage(msg);
       return;
    }
    int init = hist_last - size;
    init = (init < 0) ? 0 : init;
    for (int i = init; i < hist_last; i++)
       fprintf(fp, "%ls\n", hist_buf[i]);
    fclose(fp); 
}

void wgl_loadhistory(const char *file)
{
    FILE *fp;
    wchar_t buf[1000];

    if (wgl_init_done || !file) return;
    fp = fopen(file, "r");
    if (!fp) return;
    for(;;) {
	if(!fgetws(buf, 1000, fp)) break;
	wgl_histadd(buf);
    }
    fclose(fp); 
}

void wgl_savehistoryW(const wchar_t *file, int size)
{
    if (wgl_init_done || !file || !hist_last) return;
    FILE *fp = _wfopen(file, L"w");
    if (!fp) {
       char msg[256];
       snprintf(msg, 256, _("Unable to open %ls"), file);
       R_ShowMessage(msg);
       return;
    }
    int init = hist_last - size;
    init = (init < 0) ? 0 : init;
    for (int i = init; i < hist_last; i++)
       fprintf(fp, "%ls\n", hist_buf[i]);
    fclose(fp); 
}

void wgl_loadhistoryW(const wchar_t *file)
{
    wchar_t buf[1000];

    if (wgl_init_done || !file) return;
    FILE *fp = _wfopen(file, L"r");
    if (!fp) return;
    for(;;) {
	if(!fgetws(buf, 1000, fp)) break;
	wgl_histadd(buf);
    }
    fclose(fp); 
}
