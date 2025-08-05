#include <windows.h>

/* The mingw-runtime startup code has _argc and _argv as visible
   symbols, as do the MS compilers.

   The mingw-w64-crt is different.
*/
// FIXME headers
#ifdef __cplusplus
extern "C"
#endif
void GA_startgraphapp(HINSTANCE Instance, HINSTANCE PrevInstance, int CmdShow);

int PASCAL
WinMain (HINSTANCE Instance, HINSTANCE PrevInstance, LPSTR CmdLine,
	 int CmdShow)
{
    extern void AppMain(int argc, char **argv);

    GA_startgraphapp(Instance, PrevInstance, CmdShow);
#ifdef _W64
    AppMain(__argc, __argv);
#else
    AppMain(_argc, _argv);
#endif
    return 0;
}
