
#include <sys/ioctl.h>

#include "wrapper.h"

int inline_c_0_27845636a9593d6afbe261949c7f56b1c314bfeb(Win_Size * wsPtr_inline_c_0) {
return ( ioctl(0, TIOCGWINSZ, wsPtr_inline_c_0) );
}

