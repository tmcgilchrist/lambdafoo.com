---
title: Acer Aspire One with NetBSD 5.0
author: Tim McGilchrist
date: 2009-05-23 00:00
tags: NetBSD
description: Acer Aspire One with NetBSD 5.0
---

Finished installing NetBSD 5.0 on my AspireOne, here's the output from dmesg.

``` shell

    Copyright (c) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008
    The NetBSD Foundation, Inc.  All rights reserved.
    Copyright (c) 1982, 1986, 1989, 1991, 1993
    The Regents of the University of California.  All rights reserved.

    NetBSD 5.0 (GENERIC) #0: Sat May 16 18:00:22 EST 2009
    tim@micro.:/usr/src/sys/arch/i386/compile/GENERIC
    total memory = 1523 MB
    avail memory = 1486 MB
    timecounter: Timecounters tick every 10.000 msec
    RTC BIOS diagnostic error 80&lt;clock_battery&gt;
    timecounter: Timecounter "i8254" frequency 1193182 Hz quality 100
    Acer AOA110 (1)
    mainbus0 (root)
    cpu0 at mainbus0 apid 0: Intel 686-class, 1596MHz, id 0x106c2
    cpu0: Enhanced SpeedStep (1228 mV) 1600 MHz
    cpu0: Enhanced SpeedStep frequencies available (MHz): 1600 1467 1333 1200 1067 933 800
    cpu1 at mainbus0 apid 1: Intel 686-class, 1596MHz, id 0x106c2
    ioapic0 at mainbus0 apid 4: pa 0xfec00000, version 20, 24 pins
    acpi0 at mainbus0: Intel ACPICA 20080321
    acpi0: X/RSDT: OemId &lt;INTEL ,Napa    ,00000001&gt;, AslId &lt;    ,01000013&gt;
    acpi0: SCI interrupting at int 9
    acpi0: fixed-feature power button present
    timecounter: Timecounter "ACPI-Fast" frequency 3579545 Hz quality 1000
    ACPI-Fast 24-bit timer
    acpibut0 at acpi0 (PWRB, PNP0C0C): ACPI Power Button
    acpilid0 at acpi0 (LID0, PNP0C0D): ACPI Lid Switch
    acpibut1 at acpi0 (SLPB, PNP0C0E): ACPI Sleep Button
    acpibat0 at acpi0 (BAT1, PNP0C0A-0): ACPI Battery (Control Method)
    acpiacad0 at acpi0 (ACAD, ACPI0003): ACPI AC Adapter
    hpet0 at acpi0 (HPET, PNP0103)
    hpet0: mem 0xfed00000-0xfed003ff irq 0,8
    timecounter: Timecounter "hpet0" frequency 14318179 Hz quality 2000
    npx1 at acpi0 (FPU, PNP0C04)
    npx1: io 0xf0 irq 13
    npx1: reported by CPUID; using exception 16
    attimer1 at acpi0 (TIMR, PNP0100): AT Timer
    attimer1: io 0x40-0x43,0x50-0x53
    FWHD (INT0800) at acpi0 not configured
    pckbc1 at acpi0 (KBC, PNP0303): kbd port
    pckbc1: io 0x60,0x64 irq 1
    pckbc2 at acpi0 (MOUE, PNP0F13): aux port
    pckbc2: irq 12
    acpiec0 at acpi0 (EC0, PNP0C09): ACPI Embedded Controller
    acpiec0: io 0x62,0x66
    WMID (PNP0C14) at acpi0 not configured
    apm0 at acpi0: Power Management spec V1.2
    pckbd0 at pckbc1 (kbd slot)
    pckbc1: using irq 1 for kbd slot
    wskbd0 at pckbd0: console keyboard
    pms0 at pckbc1 (aux slot)
    pms0: Synaptics touchpad version 7.2
    pms0: Palm detect
    pckbc1: using irq 12 for aux slot
    wsmouse0 at pms0 mux 0
    pci0 at mainbus0 bus 0: configuration mode 1
    pci0: i/o space, memory space enabled, rd/line, rd/mult, wr/inv ok
    pchb0 at pci0 dev 0 function 0
    pchb0: vendor 0x8086 product 0x27ac (rev. 0x03)
    agp0 at pchb0: detected 7932k stolen memory
    agp0: aperture at 0x60000000, size 0x10000000
    vga1 at pci0 dev 2 function 0: vendor 0x8086 product 0x27ae (rev. 0x03)
    wsdisplay0 at vga1 kbdmux 1: console (80x25, vt100 emulation), using wskbd0
    wsmux1: connecting to wsdisplay0
    drm at vga1 not configured
    vendor 0x8086 product 0x27a6 (miscellaneous display, revision 0x03) at pci0 dev 2 function 1 not configured
    azalia0 at pci0 dev 27 function 0: Generic High Definition Audio Controller
    azalia0: interrupting at ioapic0 pin 16
    azalia0: host: 0x8086/0x27d8 (rev. 2), HDA rev. 1.0
    ppb0 at pci0 dev 28 function 0: vendor 0x8086 product 0x27d0 (rev. 0x02)
    pci1 at ppb0 bus 1
    pci1: i/o space, memory space enabled
    ppb1 at pci0 dev 28 function 1: vendor 0x8086 product 0x27d2 (rev. 0x02)
    pci2 at ppb1 bus 2
    pci2: i/o space, memory space enabled
    re0 at pci2 dev 0 function 0: RealTek 8100E/8101E/8102E/8102EL PCIe 10/100BaseTX (rev. 0x02)
    re0: interrupting at ioapic0 pin 17
    re0: Ethernet address 00:1e:68:8a:70:30
    re0: using 256 tx descriptors
    rlphy0 at re0 phy 7: RTL8201L 10/100 media interface, rev. 1
    rlphy0: 10baseT, 10baseT-FDX, 100baseTX, 100baseTX-FDX, auto
    ppb2 at pci0 dev 28 function 2: vendor 0x8086 product 0x27d4 (rev. 0x02)
    pci3 at ppb2 bus 3
    pci3: i/o space, memory space enabled
    ath0 at pci3 dev 0 function 0
    ath0: interrupting at ioapic0 pin 18
    ath0: 11b rates: 1Mbps 2Mbps 5.5Mbps 11Mbps
    ath0: 11g rates: 1Mbps 2Mbps 5.5Mbps 11Mbps 6Mbps 9Mbps 12Mbps 18Mbps 24Mbps 36Mbps 48Mbps 54Mbps
    ath0: mac 14.2 phy 7.0 radio 10.2
    ppb3 at pci0 dev 28 function 3: vendor 0x8086 product 0x27d6 (rev. 0x02)
    pci4 at ppb3 bus 4
    pci4: i/o space, memory space enabled
    vendor 0x197b product 0x2382 (miscellaneous system) at pci4 dev 0 function 0 not configured
    vendor 0x197b product 0x2381 (SD Host Controller system, interface 0x01) at pci4 dev 0 function 2 not configured
    vendor 0x197b product 0x2383 (miscellaneous system) at pci4 dev 0 function 3 not configured
    vendor 0x197b product 0x2384 (miscellaneous system) at pci4 dev 0 function 4 not configured
    uhci0 at pci0 dev 29 function 0: vendor 0x8086 product 0x27c8 (rev. 0x02)
    uhci0: interrupting at ioapic0 pin 16
    usb0 at uhci0: USB revision 1.0
    uhci1 at pci0 dev 29 function 1: vendor 0x8086 product 0x27c9 (rev. 0x02)
    uhci1: interrupting at ioapic0 pin 17
    usb1 at uhci1: USB revision 1.0
    uhci2 at pci0 dev 29 function 2: vendor 0x8086 product 0x27ca (rev. 0x02)
    uhci2: interrupting at ioapic0 pin 18
    usb2 at uhci2: USB revision 1.0
    uhci3 at pci0 dev 29 function 3: vendor 0x8086 product 0x27cb (rev. 0x02)
    uhci3: interrupting at ioapic0 pin 19
    usb3 at uhci3: USB revision 1.0
    ehci0 at pci0 dev 29 function 7: vendor 0x8086 product 0x27cc (rev. 0x02)
    ehci0: interrupting at ioapic0 pin 16
    ehci0: EHCI version 1.0
    ehci0: companion controllers, 2 ports each: uhci0 uhci1 uhci2 uhci3
    usb4 at ehci0: USB revision 2.0
    ppb4 at pci0 dev 30 function 0: vendor 0x8086 product 0x2448 (rev. 0xe2)
    pci5 at ppb4 bus 5
    pci5: i/o space, memory space enabled
    ichlpcib0 at pci0 dev 31 function 0
    ichlpcib0: vendor 0x8086 product 0x27b9 (rev. 0x02)
    timecounter: Timecounter "ichlpcib0" frequency 3579545 Hz quality 1000
    ichlpcib0: 24-bit timer
    ichlpcib0: TCO (watchdog) timer configured.
    piixide0 at pci0 dev 31 function 2
    piixide0: Intel 82801GBM/GHM Serial ATA Controller (ICH7) (rev. 0x02)
    piixide0: bus-master DMA support present
    piixide0: primary channel wired to compatibility mode
    piixide0: primary channel interrupting at ioapic0 pin 14
    atabus0 at piixide0 channel 0
    piixide0: secondary channel wired to compatibility mode
    piixide0: secondary channel interrupting at ioapic0 pin 15
    atabus1 at piixide0 channel 1
    ichsmb0 at pci0 dev 31 function 3: vendor 0x8086 product 0x27da (rev. 0x02)
    ichsmb0: interrupting at ioapic0 pin 17
    iic0 at ichsmb0: I2C bus
    isa0 at ichlpcib0
    pcppi0 at isa0 port 0x61
    midi0 at pcppi0: PC speaker (CPU-intensive output)
    sysbeep0 at pcppi0
    attimer1: attached to pcppi0
    timecounter: Timecounter "clockinterrupt" frequency 100 Hz quality 0
    azalia0: codec[0]: Realtek ALC268 (rev. 1.1), HDA rev. 1.0
    audio0 at azalia0: full duplex, independent
    acpiacad0: AC adapter online.
    wd0 at atabus1 drive 0: &lt;SSDPAMM0008G1EA&gt;
    wd0: drive supports 1-sector PIO transfers, LBA addressing
    wd0: 7695 MB, 15636 cyl, 16 head, 63 sec, 512 bytes/sect x 15761088 sectors
    uhub0 at usb0: vendor 0x8086 UHCI root hub, class 9/0, rev 1.00/1.00, addr 1
    uhub0: 2 ports with 2 removable, self powered
    uhub1 at usb1: vendor 0x8086 UHCI root hub, class 9/0, rev 1.00/1.00, addr 1
    uhub1: 2 ports with 2 removable, self powered
    uhub2 at usb2: vendor 0x8086 UHCI root hub, class 9/0, rev 1.00/1.00, addr 1
    uhub2: 2 ports with 2 removable, self powered
    uhub3 at usb3: vendor 0x8086 UHCI root hub, class 9/0, rev 1.00/1.00, addr 1
    uhub3: 2 ports with 2 removable, self powered
    uhub4 at usb4: vendor 0x8086 EHCI root hub, class 9/0, rev 2.00/1.00, addr 1
    uhub4: 8 ports with 8 removable, self powered
    wd0: 32-bit data port
    wd0: drive supports PIO mode 4, DMA mode 2, Ultra-DMA mode 4 (Ultra/66)
    wd0(piixide0:1:0): using PIO mode 4, Ultra-DMA mode 4 (Ultra/66) (using DMA)
    acpibat0: battery info: SIMPLO, LION, UM08A72 0095
    acpibat0: battery info: SIMPLO, LION, UM08A72 0095
    uvideo0 at uhub4 port 5 configuration 1 interface 0: Sonix Technology Co., Ltd. USB 2.0 Camera, rev 2.00/1.00, addr 2
    video0 at uvideo0: Sonix Technology Co., Ltd. USB 2.0 Camera, rev 2.00/1.00, addr 2
    Kernelized RAIDframe activated
    pad0: outputs: 44100Hz, 16-bit, stereo
    audio1 at pad0: half duplex
    boot device: wd0
    root on wd0a dumps on wd0b
    root file system type: ffs
    wsdisplay0: screen 1 added (80x25, vt100 emulation)
    wsdisplay0: screen 2 added (80x25, vt100 emulation)
    wsdisplay0: screen 3 added (80x25, vt100 emulation)
    wsdisplay0: screen 4 added (80x25, vt100 emulation)

```

I'm writing up some notes on how the install went and special things I needed to
do to get hardware working which I'll be posting sometime over the weekend.
