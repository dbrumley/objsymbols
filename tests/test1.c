#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


int foo(int fd, char *buf, int len)
{
  return read(fd, buf, len);
}


int bar(int fd)
{
 char buf[512];
 char buf2[128];
 foo(fd, buf, sizeof(buf));
 if(buf[0] == 'a'){
    strcpy(buf2, buf);
 }
 return 0;
}

int main(int argc, char *argv[])
{
  int fd = open("/tmp/foo", O_RDONLY);
  return bar(fd);
}
