#ifndef GETLINE_H
#define GETLINE_H

#include <cstddef>

typedef size_t (*gl_strwidth_proc)(const char *);

#ifdef __cplusplus
extern "C" {
#endif

/* read a line of input */
int             getline(const char *prompt, char *buf, int maxlen);
int             getline2(const char *prompt, char **buf);

void            gl_setwidth(int);		/* specify width of screen */
void            gl_histadd(const char *);	/* adds entries to hist */
void		gl_strwidth(gl_strwidth_proc);	/* to bind gl_strlen */
void		gl_loadhistory(const char *);
void		gl_savehistory(const char *, int size);
void            gl_hist_init(int, int);		/* set up history buffer */
char    	*gl_hist_next(void);	/* return ptr to next item */
char    	*gl_hist_prev(void);	/* return ptr to prev item */
void		gl_free(void *ptr);

extern int 	(*gl_in_hook)(char *);
extern int 	(*gl_out_hook)(char *);
extern int	(*gl_tab_hook)(char *, int, size_t *);

#ifdef Win32
extern void (*gl_events_hook)(void);
#endif

#ifdef __cplusplus
} // extern "C" 
#endif

#endif /* GETLINE_H */
