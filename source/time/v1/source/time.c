#include <sys/time.h>

double get_time_(void)
{
    struct timeval tv;
    gettimeofday(&tv, 0);
    return tv.tv_sec + tv.tv_usec * 1.e-6;
}
