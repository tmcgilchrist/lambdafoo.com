---
layout: post
title: Minimal IRIX Kernel Driver
categories:
- IRIX
---
There are loads of articles and examples about how to develop a simple device
driver for Linux or any of the BSDs, but outside of that there are very few. So
why not have one for IRIX, it's not exaclty the most common OS but there's a
good community around it at [Nekochan](http://www.nekochan.net) and
for anyone curious about Unix its a worthwhile task.

So presented here is a rather short "Hello, world" driver for the IRIX operating
system.

1. Introduction
2. Basic Requirements
3. Kernel Driver
4. Compiling
5. Testing
6. Conclusion

### 1. Introduction ###
This article is for anyone interested in kernel development under IRIX. It
assumes that the reader has had previous exposure to C and is familar with some
form of Unix, hopefully IRIX but Linux or *BSD is sufficient. There are already
loads of similar articles showing how to get started with Linux or BSD drivers,
but the only real resource for IRIX is SGIs monster
[Device Driver](http://techpubs.sgi.com/library/tpl/cgi-bin/browse.cgi?coll=0650&db=bks&cmd=toc&pth=/SGI_Developer/DevDriver_PG)
document. The problem I found with this document is, the depth of the material
presented makes if difficult to know where to start from. So hopefully this
article will provide a gentle introduction to IRIX before you jump into the SGI
documentation.

### 2. Basic Requirements ###
The main requirements for using the driver presented are:
 * root access to a SGI machine running IRIX 6.5
 * MIPSPro Compiler


Unfortunately, these requirements may be a little difficult to fulfill but
[eBay](http://www.ebay.com) is your friend when looking for SGI equipment. All
code was tested on an SGI O2 R10K 250Mhz running IRIX 6.5.27, and compiled with
MIPSPro 7.3.

### 3. Kernel Driver ###
The driver is very simple but provides a starting point for investigation about
IRIX.

{% codeblock lang:c%}

#include <sys/types.h>
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/sysmacros.h>
#include <sys/ddi.h>

/* ====================================================================
 * Module version information, required for loadable modules.
 */

#include <sys/mload.h>
char *sim_mversion = M_VERSION;

/* ====================================================================
 * Device-Related Constants and Structures
 */

int sim_devflag = D_MP;

/* ==================================================================
 *    FUNCTION TABLE OF CONTENTS
 */
void sim_init(void);
int sim_unload(void);
int sim_reg(void);
int sim_unreg(void);
int sim_open(dev_t devp, int oflag, int otyp, struct cred crp);
int sim_close(dev_t dev, int oflag, int otyp, struct cred crp);

/*
 * For Irix6.4 compatability only, do nothing here.
 */
void sim_init(void) {
   printf("sim_init()\n");
}

/*
 * Called by the kernel when the driver is loaded.
 * Here you'd do things like setup per device data and
 * register this driver for the hardware.
 */
int sim_reg(void) {
    printf("sim_reg()\n");
    return 0;
}

/*
 * Unloads the driver.
 */
int sim_unload(void) {
    printf("sim_unload()\n");
    return 0;
}

/*
 * Unregisters the driver.
 */
int sim_unreg(void) {
    printf("sim_unreg()\n");
    return 0;
}

/*
 * Opens the driver.
 */
int sim_open(dev_t devp, int oflag, int otyp, struct cred crp)
{
    printf("sim_open()\n");
    return 0;
}

/*
 * Closes the driver.
 */
int sim_close(dev_t dev, int oflag, int otyp, struct cred crp)
{
    printf("sim_close()\n");
    return 0;
}

{% endcodeblock %}

### 4. Compiling ###
The major area missing from the SGI Driver manual is how to get your code to
compile and load into the kernel. To help solve this problem a makefile has been
provided and the important sections will be covered here.

{% codeblock lang:sh %}
#!smake

CPUBOARD=IP32

{% endcodeblock %}

The first line is pretty self explainatory, run smake, which is a slightly
different version of the standard unix make command. The CPUBOARD indicates
which architecture to compile the driver for. The architecture differs between
machines even if they have the same CPU so to find correct number run.


    hinv |grep IP

which will printout the board number to use. I'm using an R10K in an O2, so my
board number is IP32.

    include /var/sysgen/Makefile.kernloadio

The file /var/sysgen/Makefile.kernloadio is a sample Makefile for kernel
drivers, it sets up all the compiler and linker preferences for us. So far I've
treated this as a black-box and haven't changed anything in it, I assume the SGI
engineers knew what they were doing.

    all: compile
    compile:
        $(CC) $(CFLAGS) $(LDFLAGS) -c simple.c

This is will compile the driver, and should be pretty familiar to any C coder.

    load:
        $(ML) ld -v -c simple.o -p sim -s 13

    clean:
        $(RM) -f simple.o

The load target here will do exactly what it says, load the driver  into the
kernel, and the clean will cleanup the files created. When  running the 'load'
target you may get an error like:

    -- load ---
    /sbin/ml ld -v -c simple.o -p sim_ -s 13
    Error loading module sim_: Major number already in use.
    *** Error code 255
    smake: Error: 1 error

This is simply saying that an existing driver is using the major  device number
that we specified, so keep changing the number until it  doesn't clash. There
must be some sort of logic to assigning major  device numbers but it isn't
terribly important here.

### 5. Testing ###
Being a simple dirver there isn't a whole lot to test. The main thing is that it
can be loaded and unloaded successfully, and that you can see the messages
printed out when this happens. So open a new shell

    tail -f /var/adm/SYSLOG

This will open the main logfile under IRIX so you can see the output of the
driver. Now compile the driver and load the driver, you'll need to be root
for this to work.

    make && make load

Make a note of the id assigned to the driver, as it is required to unload
it. Check that the driver has been loaded by listing all loadable drivers. The
ml command is used for manipulating loadable kernel drivers, check out man ml
for more details. In the other shell you should see something like

    Jan 31 01:34:20 6A:sgi unix: sim_init()
    Jan 31 01:34:20 6A:sgi unix: sim_reg()
    Jan 31 01:34:20 5E:sgi lboot: Module /usr/people/you/code/simple/simple.o dynamically loaded.

To unload the driver use the id assigned to it earlier, eg 5
    ml unld 5

In the other shell you should see

    Jan 31 01:34:20 6A:sgi unix: sim_unreg()
    Jan 31 01:34:20 6A:sgi unix: sim_unload()

### 6. Conclusion ###
So now you should have a working driver for IRIX that can be modified to support
any sort of hardware outlined in the
[SGI Device Driver](http://techpubs.sgi.com/library/tpl/cgi-bin/browse.cgi?coll=0650&db=bks&cmd=toc&pth=/SGI_Developer/DevDriver_PG)
document. There are plenty of more advanced examples of how to write drivers for
PCI cards, SCSI, TCP/IP networking, amongst others. The PCI card examples are
the most relevant if you have an O2 as they have PCI built in, unlike other SGI
machines, and cheap PCI cards are everywhere, so you'll always have good
material to work with.

### Extra ###
Just an additional thing I came across when messing about withloadable
drivers. Most of the examples in the SGI Device Driverdocument are for
non-loadable drivers if you add the following sections you'll be able to treat
them as loadable drivers. A Loadable Driver will fail when attempting to load
with this cryptic error message.

    Error loading module sim_:  Module version string is missing.
    *** Error code 255

This means that the loadable version string is missing from this driver. To fix
add the following code to the driver, and the error should disappear.

     #include <sys>char *pfxmversion = M_VERSION;

where **pfx** is the prefix used in the driver. In this article the prefix would
be **sim_**.

### Downloads ###

The source code is up on github: [Simple IRIX Device Driver](https://github.com/tmcgilchrist/simple_irix_driver)
