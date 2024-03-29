+++
title = "Linux 发行版意味着什么？"
date = 2018-03-03
[taxonomies]
tags = ["opinion", "linux"]
[extra]
headline = "都怪 Debian（指）"
+++

近来 Flatpak 等项目风风火火，让我更深地思索了 Linux 各大发行版的「包管理」的理念。今天看到了 [TingPing 关于 Debian XChat 的博客](https://tingping.github.io/2018/03/02/when-distros-get-it-wrong.html)，促使我写下了自己的思考。
<!--more-->

# GNU/Linux 并不是 GNU + Linux

Linux 发行版**是**什么？这个问题似乎很容易回答，即是基于 Linux 内核的完整操作系统，一般来说还会带上 GNU 出品的自由应用程序。

但是，Linux 发行版远远不止 Linux 和 GNU，还有来自其他开发者的海量软件，没有这些软件，仅凭 Linux 和 GNU，Linux 发行版完全不能满足用户需求。那么现在就有一个开发与分发的错位现象：**软件开发者不是软件分发者**，这在 Windows 和 macOS 的世界里很罕见。我个人认为，这一现象与 Debian 有很大关系，这也正是本文着重讨论的发行版。

# Debian 式软件分发

任何一个操作系统都根本没有必要分发别人的软件，更别说开发一个包管理器了。正如 Slackware 一样，我们完全可以不开发这些东西，而是提供编译好的 tar.gz（像 Windows 下常见的绿色软件一样）或构建脚本。那么，Debian 为什么要分发非系统软件呢？有两个答案：

1. Debian 把这些程序当作 Debian 自己的系统功能，就像 Windows 的可选功能一样，只有选择启用时，系统就包含了这些功能。
2. Debian 确实是为了方便用户而分发软件。

答案我觉得很简单。「Debian 开发者」其实就是给软件打包者，但 Debian 的开发者任务繁重：需要严密测试软件，保证 Debian 系统的稳定性；所有软件的 BUG 不是报告给上游，而是报告给 Debian，由 Debian 开发者修复或报告给上游。这就意味着，Debian 认为这些软件都是 Debian 的附加功能。Debian 官网的说法「Debian 不只是提供一个纯粹的操作系统：它还附带了超过 51000 个软件包，这些预先编译好的软件被包裹成一种良好的格式以便于在您的机器上进行安装。」其实正好印证了我的想法。

Debian 是比较早期的发行版，包管理对用户来说很易用，于是使得 Debian 的用户数越来越多，开发者也越来越多，逐渐形成了巨大的影响力，这影响力使得后来的发行版几乎都有「包管理」的特征，也形成了 GNU/Linux 的共同文化。

现在有一些接近上游的发行版，如 Arch Linux、Fedora 等，我认为已经不再是 Debian 那种想法，而确实是抱着给用户提供方便的想法来提供软件打包的。这里暂且不论。

# 尴尬的软件开发者

那么，在 Debian 模式下，软件的原开发者处于什么位置呢？我觉得有点类似于在给 Debian 免费打工，而原作者其实对 Debian 没有什么决策力（因为是自由软件）。举个例子，Debian 稳定版中的 xscreensaver 包很旧了，有一些 bug，一些 Debian 新手不知道应该给 Debian 报告 bug，不断「骚扰」xscreensaver 的作者，作者很生气，在软件里贴出要求用户更新版本的通知，[甚至想要 Debian 移除 xscreensaver 包](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=819703)，但也没有任何办法。这就是 Debian 模式下的一个典型悲剧。

我过去认为 Debian 过于「自大」，原作者认为软件已经稳定了，但 Debian 却不这么认为，坚持发布旧版软件。我现在认为其实是有道理的：(1)软件作者不同，对稳定性的要求不同；(2)一些作者在修复 bug 的同时会引入新功能，新功能又会引入新 bug，这会导致软件永远不够稳定，所以 Debian 开发者需要自己 backport 补丁。这样严苛的标准，也造就了 Debian 非凡的稳定性。

# 开发者的逆袭

但是，应用软件一般情况下不需要特别稳定，有时候时尚的新功能对用户来说比较重要，但软件开发者却无力帮忙——发行版种类太多了——那么，如何弥补「开发者不是分发者」的错位、怎么让软件开发者自己发布软件呢？

目前来看，答案有以下几种：

1. 源代码
2. tarball
3. Flatpak
4. Snap
5. AppImage
6. 自己打包 / 自建软件源

其中，1、2 是目前的事实标准，尤以 1 为流行，但这对用户来说十分不便，再考虑到 Linux 对权限管得很严，总觉得把 tarball 装在哪都不合适。3、4 可能是未来的方向，Canonical 力推 Snap（没用过，不评价），而开源社区如 GNOME 则选择拥抱 Flatpak。AppImage 是我个人觉得最正确的一种。

Flatpak 最大的缺陷是把 Flatpak 运行时和系统完全割裂开来，这会浪费很多硬盘空间（够了，我知道你们硬盘大，你们说了几百遍了），其次是整个架构很复杂，完全不能和 AppImage 的简单性相比。AppImage 最正确的一点是考虑了系统运行时（通过考察各种发行版得出一个最小公共子集），只打包不是（几乎）所有发行版都有的依赖，这使得 AppImage 大小很小，而且能在几乎所有 Linux 发行版上执行。我对 AppImage 唯一的怨言是它默认把程序挂载到 /tmp 里，执行 LibreOffice 这种巨大的程序会占去不少内存（行了行了，我知道你们内存大，你们也说了几百遍了）。此外，AppImage 似乎无法增量更新，不过你们网速快，这点不是问题。我觉得 AppImage 会是一种比较传统的软件分发方式，而 Flatpak 则是要完整实现一个 App Store。如果世界上只有一个发行版，如 Debian，那么 Flatpak 大概也会考虑系统运行时吧？我就想想。

第 6 种方法看上去很尴尬，尤其在 Debian 里面，因为包管理工具本来是系统管理工具，用来安装附加功能的，你用这个东西算怎么回事呢？我觉得倒大可不必这么锱铢必较，既然 Debian 是开放的操作系统，那么由第三方提供附加功能也没什么大不了的。

# 首尾呼应

最后谈一下 TingPing 的那篇博文。TingPing 说 Debian 给 XChat 打包是不负责任的行为，在我看来这点并不成立，原因就是，Debian 开发者开发一个叫 XChat 的附加功能，和上游其实关系不大。有 Bug 或者安全性问题直接给 Debian 汇报就行。
