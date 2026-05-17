+++
title = "What Does a Linux Distribution Mean?"
date = 2018-03-03
[taxonomies]
tags = ["opinion", "linux"]
[extra]
headline = "Blame Debian, basically"
+++

> Disclaimer: This post was translated into English by an AI model. It may contain mistakes or awkward wording.

Recent activity around projects such as Flatpak made me think more deeply about the package-management philosophy behind the major Linux distributions. Today I read [TingPing's post about Debian XChat](https://tingping.github.io/2018/03/02/when-distros-get-it-wrong.html), which prompted me to write down my own thoughts.

<!--more-->

# GNU/Linux Is Not GNU + Linux

What *is* a Linux distribution? The question seems easy to answer: it is a complete operating system based on the Linux kernel, usually bundled with free applications from GNU.

But a Linux distribution contains far more than Linux and GNU. It also contains a huge amount of software from other developers. Without that software, Linux plus GNU alone would not satisfy users' needs. This creates a mismatch between development and distribution: **software developers are not software distributors**. That is rare in the Windows and macOS worlds. I think Debian has a lot to do with this phenomenon, and Debian-style distribution is the focus of this post.

# Debian-style Software Distribution

No operating system is inherently obligated to distribute other people's software, let alone develop a package manager for it. Like Slackware, a system could simply provide prebuilt `tar.gz` archives, like portable applications on Windows, or build scripts. So why does Debian distribute non-system software? I see two answers:

1. Debian treats these programs as Debian's own system features, similar to optional Windows features: once enabled, they become part of the system.
2. Debian really is distributing software for the convenience of users.

My answer is simple. A "Debian developer" is effectively a software packager, but Debian developers carry a heavy responsibility: they must test software carefully, guarantee the stability of the Debian system, and receive bug reports for all packaged software. Bugs are reported to Debian first, and Debian developers either fix them or forward them upstream. This means Debian regards these packages as Debian's additional functionality. Debian's own wording, saying that Debian includes more than 51,000 precompiled packages in a convenient format for installation, fits this view quite well.

Debian is an early distribution. Its package management was convenient for users, so Debian gained more users, more developers, and gradually a huge influence. Later distributions mostly inherited the characteristic of package management, forming a shared GNU/Linux culture.

Some distributions, such as Arch Linux and Fedora, are much closer to upstream. I do not think they still follow Debian's exact view; they package software more in the spirit of providing convenience to users. I will leave that aside here.

# The Awkward Position of Developers

Under the Debian model, where does the original software developer stand? I think they are almost doing unpaid work for Debian, while having little decision-making power over Debian's package, because the software is free software. For example, the `xscreensaver` package in Debian stable was old and had bugs. Some Debian newcomers did not know they should report bugs to Debian, and kept "harassing" the author of `xscreensaver`. The author was angry, added a notice asking users to update, and [even wanted Debian to remove the package](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=819703), but there was not much he could do. This is a typical tragedy under the Debian model.

I used to think Debian was too arrogant: if the original author considered the software stable, why would Debian insist on shipping an older version? Now I think Debian has a point. First, different authors have different standards for stability. Second, some authors introduce new features while fixing bugs, and new features introduce new bugs, so the software may never become "stable enough" in Debian's sense. Debian developers therefore need to backport patches themselves. This strict standard is also what gives Debian its remarkable stability.

# Developers Strike Back

Application software usually does not need to be as stable as the operating system. Fashionable new features may matter more to users. But software developers often cannot distribute directly, because there are too many distributions. How can we fix the mismatch where developers are not distributors, and let developers publish software themselves?

At the moment, the answers seem to be:

1. source code;
2. tarballs;
3. Flatpak;
4. Snap;
5. AppImage;
6. self-packaging or self-hosted repositories.

The first two are the de facto standards, especially source code, but they are inconvenient for users. Given Linux's strict permission model, installing a random tarball never feels quite right. Flatpak and Snap may be the future. Canonical is pushing Snap, which I have not used and will not judge; the open-source community around GNOME has embraced Flatpak. Personally, I think AppImage is the most correct approach.

Flatpak's biggest weakness is that the Flatpak runtime is separated completely from the system, which wastes disk space. Its architecture is also complicated, especially compared with AppImage's simplicity. AppImage's best idea is that it considers the system runtime: by examining many distributions, it finds a lowest common subset, and only bundles dependencies that almost no distribution can be expected to have. That keeps AppImages small and executable on almost any Linux distribution. My only complaint is that AppImage mounts programs under `/tmp` by default; running something huge like LibreOffice can consume quite a bit of memory. Also, AppImage does not seem to support incremental updates, but if your network is fast, that is not a major issue. I think AppImage is closer to a traditional software distribution method, while Flatpak is trying to implement a full App Store. If there were only one distribution in the world, such as Debian, perhaps Flatpak would also care about the system runtime. Just a thought.

The sixth method looks awkward, especially on Debian, because the package manager is supposed to be a system administration tool for installing system add-ons. What does it mean for third parties to use it? But there is no need to be overly fussy here. Since Debian is an open operating system, it is not unreasonable for third parties to provide extra functionality.

# Back to the Beginning

Finally, about TingPing's post: TingPing says Debian packaging XChat is irresponsible. I do not think that claim holds. In my view, Debian developers developed an additional feature called XChat, and that feature does not have much to do with upstream. If it has bugs or security problems, report them directly to Debian.

