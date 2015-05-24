
#include <sys/ioctl.h>

int inline_c_0_c78c83e00bfa687833ea0d933bb782ead6c5def7(winsize * wsPtr_inline_c_0) {
return ( ioctl(0, TIOCGWINSZ, wsPtr_inline_c_0) );
}

