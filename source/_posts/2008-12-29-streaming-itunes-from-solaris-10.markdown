--- 
layout: post
title: Streaming iTunes from Solaris 10
tags: 
- Solaris
status: draft
type: post
published: false
meta: 
  _edit_last: "1"
  _aioseop_title: Streaming iTunes library from Solaris 10
  _aioseop_keywords: solaris itunes, itunes streaming, itunes library solaris,
---
<strong>Problem:</strong> Sharing iTunes library from home server running Solaris.
<strong>Solution:</strong> Firefly Media Server (aka mt-daapd).

Firefly acts as a shared iTunes volume to any iTunes clients. Since all my clients will be Macs this is the perfect solution. I had previously tried to follow <a href="http://blogs.sun.com/constantin/entry/solaris_home_part_4_streaming">these </a> instructions but had no luck.

Coming back with a fresh Solaris 10 and Sun Studio 11 install I had a bit more luck.

<strong>Step 1:</strong> Dependencies available from Blastwave.
<ul>
	<li>libid3tag</li>
	<li>sqlite3</li>
</ul>
<strong>Step 2:</strong> Download Firefly from here. I used the latest nightly svn download.

<strong>Step 3:</strong> Running the configure script.
A couple of pre-requisites before running configure, make sure you have the Sun Studio compilers in your PATH. Otherwise it will try to use the gcc compiler or not find one at all.

<code> export PATH=/opt/SUNWspro/bin:$PATH</code>
<pre><code>
./configure --enable-sqlite3 --with-sqlite3-includes=/opt/csw/include \
 -with-sqlite3-libs=/opt/csw/lib \
--with-gdbm-includes=/opt/csw/include --with-gdbm-libs=/opt/csw/lib \
 LDFLAGS="-L/opt/csw/lib" \
CPPFLAGS="-I/opt/csw/include" --prefix=/opt/local
</code></pre>
Which looks for the sqlite3 and gdbm libraries from my Blastwave install, and will install the final binary under <em>/opt/local</em>. Change this if you want to install elsewhere.

<strong>Step 4:</strong> Compiling
Run a quick make to compile everything,
<pre><code>
gmake
gmake install
</code></pre>
