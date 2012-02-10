---
layout: post
title: Meraki Internals
categories:
- Coding
- Meraki
---

Pristine Meraki Mini.

<a href="http://picasaweb.google.co.uk/timmcgil/MerakiInternals/photo#5172528806907627362"><img src="http://lh4.google.co.uk/timmcgil/R8iDi240R2I/AAAAAAAAAU4/74bJ_LEqkEQ/s400/IMG_0724_1.JPG" /></a>

Removing the back cover. The screws are under the top right and bottom left of
the unit when holding the antenna end up. It was a little tricky to peel back
the sticker as the printed part split away from the glue part.

<a href="http://picasaweb.google.co.uk/timmcgil/MerakiInternals/photo#5172529098965403506"><img src="http://lh4.google.co.uk/timmcgil/R8iDz240R3I/AAAAAAAAAVA/v5oTnvMc0ug/s400/IMG_0725_1.JPG" /></a>

Merak internals revealed. At the top is the antenna connection, under the silver
shielding I assume is the CPU.Above that is an IC marked Samsung K4S561632H,
which upon further googling appears to be an SDRAM chip. Just left of that are
header pins for a serial port.

Pinout for the serial port:
<pre>
      VCC o
LAN   TX  o   Antenna
      RX  o
      GND o
</pre>

The chip closest to the ethernet ports is a Realtek RTL8201CP.

<a href="http://picasaweb.google.co.uk/timmcgil/MerakiInternals/photo#5172530258606573490"><img src="http://lh6.google.co.uk/timmcgil/R8iE3W40R7I/AAAAAAAAAXU/HVcPnkM5CS0/s400/IMG_0729_1.JPG" /></a>

Serial to USB cable attached to the Meraki.

<a href="http://picasaweb.google.co.uk/timmcgil/MerakiInternals/photo#5172528519144818514"><img src="http://lh5.google.co.uk/timmcgil/R8iDSG40R1I/AAAAAAAAAUw/2XZ-vOp0igY/s400/IMG_0730_1.JPG" /></a>

The part number is FTDI TTL-232R-3V3, which is available in Australia through [Glyn Ltd](http://www.glyn.co.nz/index.htm).
Internationally checkout the
[FTDI Online Store](http://apple.clickandbuild.com/cnb/shop/ftdichip?op=catalogue-products-null&amp;prodCategoryID=47&amp;title=Cables%253A+TTL-232R).

Further details available on the
[OpenWRT Meraki](http://wiki.openwrt.org/OpenWrtDocs/Hardware/Meraki/Mini)
page.
