+++
title = "Haskell as a non-toy"
date = 2021-06-03
[taxonomies]
tags = ["haskell", "rust", "review"]
[extra]
headline = "Haskell 好用吗？"
math = true
comment = true
+++

最近拿 Haskell 亲身实践了一段时间，这篇文章总结了我的一些心得体会。

<!--more-->

> **Disclosure**: The material in this blog post has not been reviewed, endorsed, or approved of by the Rust Foundation. For more information on the Rust Foundation Trademark Policy, click [here](https://foundation.rust-lang.org/).

## 为什么又是 Haskell？

是的，我之前学过 Haskell，并且对里面的概念也比较熟悉了。Monoid? Monad? Traversable? Foldable? Quantified constraints? Data kinds? Rank N types? Type families?…… 这些名字我不说非常熟悉，至少都碰到并且用过了。其实 Haskell 一直被我用来做一些比较理论性的小试验。（不过 Haskell 一直没有真正的依赖类型，所以有时候我还是会用 Coq。）对很多人来说，学习 Haskell 到这里就足够了，毕竟很多人真的认为 Haskell 就是用来学习的，“不会真有人写 Haskell 吧？”

另一方面，我始终对我手上的工具不甚满意。在这次实践之前，我个人（不含特殊需求）最常用的编程语言有 C++, Python 和 Rust。（Emacs 配置会用到 Emacs Lisp，不过我写得也不多。）

1. C++ 被我用来做和操作系统和硬件关系比较密切的实验，比如内存模型。还用来写一些数据结构（毕竟在 Rust 里写没什么收益，还要加上很多 `unsafe` 标记）。
2. Python 用来写脚本。对我来说，这包括任何复杂的 shell 脚本，还有和网络相关的。另外我还把 Python 当作计算器，还用来快速做出原型并进行概念验证。
3. 除了上面提到的，其他任何需求我都首选 Rust。

所以我的主要工作语言是 Rust 和 Python。对多数人来说，这个组合可能足够好了，但我始终感觉有痒点没有解决。

1. Rust 在各种意义上来讲都过于 “重量级”。要使用第三方 crate？先开一个新项目。不过这一痛点已经有 [cargo™ play](https://boats.gitlab.io/blog/post/2017-12-27-things-explicit-is-not/) 等项目缓解了。最大的痛点是，Rust 代码太 ["noisy"](https://boats.gitlab.io/blog/post/2017-12-27-things-explicit-is-not/): 很明显的东西必须要写出来，写出来就算了，还很难组合。而且 Rust 会给人一种非常大的考虑内存占用、计算开销的心理压力，这常常让我犯了 "perfect is the enemy of good" 的错误。
2. Python 相比之下，怎么写都行，心理负担极小。但是一方面，CPython 的性能和并发让我特别恼火；另一方面，Python 的抽象能力实在不高。

总而言之，学习多种编程语言不会让你变成更好的程序员，而是让你对任何一门语言都不满意。[学习 Python/Haskell 会让你变成更差的程序员](https://lukeplant.me.uk/blog/posts/why-learning-haskell-python-makes-you-a-worse-programmer/)。

于是，我打算重新捡起 Haskell。契机是，偶然的一次机会，我用 Haskell 写了一个 [Telegram 机器人](https://github.com/ksqsf/rustdalaobot)。这个机器人非常简单，最初的想法就是匹配消息内容，然后回复匹配到的内容。当然，根据上面的流程，我首先想到的是使用 Rust 写。这不就巧了，我之前还真用 Rust 写过 bot，体验非常糟糕。于是这次我打算换一个语言体验一番。恰好想到了 Haskell，一搜 Haskell 也真的有 Telegram bot 库，于是就动手了。

整个过程非常舒服，而且写起来很快，第一版只花了 10 分钟。写 Haskell 过程中，我可以自由地使用我的一切知识，而在 Rust 或 Python 中，我常常要思考如何把我的想法 “编译” 成比较低级的语言。比如说，在这个 bot 中我写了一个[简单的文本匹配器](https://github.com/ksqsf/rustdalaobot/blob/991ae4cbd741e996627f83eee7c9453470a58799/app/Main.hs#L19)。每个匹配器就是一个字符串到布尔值的函数。最开始，我按照常规定义了一个新类型，写完之后发现根本就是重复了布尔代数，并且还缺少了 True 和 False 的对应物。之后，我把匹配器表示成高阶函数，代码不仅更短，而且更灵活了（`patternFromWords` 也从 partial 变成 total 的了）。

经过这次比较愉快的经历，我开始认真思考是不是可以比较严肃地使用 Haskell。（也不仅仅是只有这一个经历就改变了我的看法。我对抽象的看法一直在更新，并且越来越重视组合性。）此外，关于 Haskell 本身，(1) 生态谈不上匮乏，想用的东西常常都能找到，比如 Telegram bot 框架 (2) GHC 对并发和并行有很好的支持 (3) 有认识的朋友也很喜爱 Haskell。

于是，我就又一次拾起了 Haskell！

## 并发

我对并发和并行一直都很感兴趣，而且也学习了不少这方面的知识。我非常痛恨 CPython，标准的反面教材。之前用 Python 做 meet in the middle 攻击，计算是 CPU 密集的，同时还会产生大量数据。而 CPython 因为臭名昭著的 GIL，没法直接多线程，于是就使用了 `multiprocessing`……似乎没毛病？有！我最终要把若干 GB 大小的计算结果丢给主进程，结果这里就成了性能瓶颈……（Python 似乎现在有共享内存的设施，但是我真的不想碰，`multiprocessing` 的接口也属实糟糕了点。）最后，被迫改成 Rust，结果是 Rust 单线程就足够快了。（很久以后，我用 OCaml 也实现了一个版本，不过确切的结果我忘了，似乎也是单线程就够用了。）

好巧不巧，我刚拿起 Haskell 的时候，碰到了这样一篇文章，标题是 [Go 性能神话的破灭](https://zhuanlan.zhihu.com/p/368304027)。文中有一个小的测试程序，对比了 Go 和 .NET 的并发性能。于是我就把 Go 代码翻译成了 Haskell。

{{ gist(url="https://gist.github.com/ksqsf/e267b667dbaad03f9486e99a15f07edb") }}

**编码过程**：整个过程还算流畅，最大的难点倒不是实现那些 goroutine，[Concurrent Haskell](https://en.wikipedia.org/wiki/Concurrent_Haskell) 本身完全就是用户态纤程，和 OS 线程是 m:n 关系，和 goroutine 完全一致。最大的难点是……临时阻止 stdout 输出。上面的代码可以看到我定义了一个 `Job` monad，它的 `Bool` 代表是否可以输出。还有一个有趣的事情是，最开始我是想直接用 `ReaderT` 而不定义新类型的，但是出现了很多类型错误，我算不过来，改成这样类型就容易算了。从某种意义上说，Haskell 强制让我把代码写得更清晰了……

写这个程序也让我自我感觉更良好了。因为这个程序里面用了 monad transformer 之类的比较难以理解的东西，而我写的时候基本都是水到渠成，所以我现在可以比较自信地说我已经初学者毕业了！LOL

**实验结果**：我的内存很小，所以 `runs = 10` 时根本跑不完。总之，结果有点差劲。后来我补习了很多 GHC （比如看[这个视频学了一些 GHC GC](https://www.youtube.com/watch?v=vvLDerKtUWE&t=2817s)）和性能侦测工具（比如 [ThreadScope](https://wiki.haskell.org/ThreadScope)）等等的知识，再加上自己的很多乱七八糟的实验，最终才能在比较小的 `runs` 下得到比较快的结果。但是……还是很糟糕。知乎评论区里提到的不到一秒就能跑完真的想都不敢想。

我可以继续掩耳盗铃，说这个文章的例子过于矫揉造作。不过我觉得这不是正确的态度，正确的态度应该是真正地理解为什么 GHC 表现这么差。我暂时没兴趣继续深挖下去，就暂且留着这个坑好了。

Haskell 的用户态纤程就是有栈协程。这个模型可以说是非常符合直觉的。不过我一开始有点困惑为什么 Haskell 本身没用 delimited continuation 来建模这个事情，而是要做一个 primitive，后来才发现原来这个新模型是 [2006 年才提出的](http://lambda-the-ultimate.org/node/1435)，Concurrent Haskell 是 20 世纪的设计了。

总而言之，我认为 Haskell 的并发实现可以用，但不是最好的实现。它没法和 Go、.NET、JVM 竞争，但远远好于 Python。

哦对了，对我来说还有一个好处。Rust 没有信号量，[Haskell 里有 QSem](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-QSem.html)，所以我可以用 Haskell 来学习 [The Little Book of Semaphores](https://greenteapress.com/wp/semaphores/)。

## FFI、内存安全

我接下来考虑到的事情是，如果 Haskell 本身缺库，那么这缺陷完全可以由 FFI 弥补。于是我开始看 Haskell FFI，并且写了一个 [OpenCC 的绑定](https://hackage.haskell.org/package/opencc) 练手。

先简单总结一下 Haskell 对 FFI 的看法。Haskell 是一个 "lazy", "pure" 的温柔乡，而 FFI 是通向邪恶外界世界的大门，所以 FFI 全部在 `IO` 里面。这合理吗？大部分情况下是合理的，但是有的计算人家就是 pure 的呀！没错，的确如此。于是我们碰到了 `unsafePerformIO` 的合理使用之一。（还有一个合理情况是全局的 `IORef`，嗯，记得加上 `NOINLINE` pragma。）

> 因为 `unsafePerformIO` 的存在，`IO a` 不能理解成 `State RealWorld a`。为什么？因为 `RealWorld` 并不是只有类型带 IO 的函数才能访问、修改的。那应该怎么办？可能只能把整个 GHC 都 formalize 进去才行吧。

其次，所有资源是用指针（[`Ptr`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Foreign-Ptr.html)）管理的。然后我们遇到了一个重大问题：资源怎么管理？GC 非常善于回收垃圾内存，但是非常不善于回收非内存资源。比较简单的情况，我们可以使用 `ForeignPtr` 挂一个 finalizer。而 Haskell 本身是怎么管理的呢？使用 bracket idiom。差不多就是 defer 语句。总之，这里我依然觉得 RAII 是更好的解决方案。

而 Haskell 完全不能帮你更好地管理资源！我本人因为编码错误，就遇到了 double free 的问题。于是我需要调试。嗯，调试？？请用 [`Debug.Trace`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)。如果你熟悉 Rust，这相当于 Rust 的 `dbg!()` 宏。为什么不插入 `printf` 或者单步调试呢？首先前者肯定是做不到的，因为 Haskell 是 "non-strict" 语言。后者对我遇到的情况没什么帮助，因为是 GC 时调用 finalizer 才会发生的。

事后反思一下，如果 Haskell 有 linear type，这个错误根本不会出现。所幸 [GHC 9.0 已经有 linear type 可以亲自体验了](https://www.haskell.org/ghc/blog/20210204-ghc-9.0.1-released.html)，非常期待它未来会发挥怎样的作用。[Tweag.io 有很多关于 Haskell linear type 的文章值得一读。](https://www.tweag.io/blog/tags/linear-types/)

我个人把这一节总结成：Rust 已经赢了太多了✌️

## 字符串

字符串可能是个比较小的问题，但仍然是个槽点。

在做 OpenCC 绑定的过程中，不可避免地碰到了 `String`, `ByteString`, `Text` 的问题。Haskell 本身的字符串类型是 `Char` 链表。没错，每个 `Char` 先不论字符有多大，已经自带了 8 字节的指针……于是，不可避免地，我找到了 `ByteString` 和 `Text` 这两个事实标准。（说真的，快把 `String` 改成 UTF-8 字节数组吧……和 Rust 一样……）

`ByteString` 就是一个字符数组的 `ForeignPtr`，而 `Text` 是 UTF-16 （wtf！）编码的。（好消息, [Text 要改成 UTF-8 了](https://discourse.haskell.org/t/hf-tech-proposal-1-utf-8-encoded-text/2499)）。还有，`Text` 只能存 Unicode 字符串，而且没有类似于 Rust 的 lossy 转换函数。`decodeUtf8` 在遇到错误时会抛异常，[Michael Snoyman 就吐槽过这个](https://www.snoyman.com/blog/2020/11/haskell-bad-parts-2/)。

看起来好像改一个类型就行了？问题是，整个标准库都是 `String`。于是每个库都带着自己的 IO 函数，什么 `Data.Text.IO.putStrLn` 啦，`Data.ByteString.Char8.hGetContents` 啦，不一而足。反正你不得不想办法在字符串类型间转换来转换去。另外有趣的是，标准库和这些库里的 lazy, strict 命名约定是反过来的。虽然可以理解，但是我还是想在这里吐槽一句……

总结：Rust✌️

## 异常

上面提到了异常。异常就是 bottom value 嘛，有什么可说的？错了，如果真的是抓不了的那倒还好，问题是 Haskell 里可以抓异常，而且异步异常被用来做很多事情。比如说，取消一个 Haskell 线程就是给这个线程扔一个异常。对这个线程来说，这个异常是异步的。（然后又引入了关于 FFI 的一堆破事。）异常是有用的不假，而且对有栈协程来说可能也是必要的，但是在 Haskell 里这完全被滥用了。Prelude 里一大堆 partial 函数已经成了经久不衰的槽点。

相比之下，Rust 这边则反而比 Haskell 显得更在乎类型一些。Rust 社区对 panic 的看法非常统一：不可恢复的错误。换句话说，你不应该去抓它。Rust 也有机制可以抓，但是总体上是不鼓励的，甚至可以在 Cargo.toml 里面一键修改成想抓也抓不了（panic=abort）。另外，partial 基本都有 `unwrap`, `expect` 等标记，明显而统一。Haskell 没有这样统一的标志，于是你经常需要读文档（说好的类型即文档呢？），不注意的话就会踩到 `decodeUtf8` 这样的坑里。

## 工具链

Haskell 的工具链（ghc, cabal-install, stack）是传统黑点了。不过这里我想说的是，目前来看整体体验还不错，没遇到太多问题。

以前我用 Stack，一直没出事，但是我相当反感电脑上装好几套 LTS，我也根本不想去记 LTS 版本号。开玩笑，我记住 GHC 版本号就不错了好吗。总之，如果硬盘大、记性好，用 stack 一定是相当不错的。

这段时间主要是用 Nix 管理我的 Haskell 工具链。为什么呢？因为 Homebrew 太慢换成 Nix 顺便就管理了。也因为 Cabal 还是会炸。举个例子我装了 nixpkgs 的 ghc 8.10.4，自带了一个 `directory` 包版本比 Hackage 上的版本少了 0.0.0.1（我没骗你哦），导致 Hackage 上很多库都装不上了……我不清楚 Cabal 到底是怎么工作的，翻了一下 cabal 文件也没什么线索，版本号都是兼容的，很是离谱。

Cabal 还有一个非常神秘的问题是不能卸载。Cargo™ 都有 `uninstall` 命令的……

Haskell 这里把很多事情都搞得极为麻烦，包就是包，我不在乎它是源码（cabal）还是二进制（ghc-pkg），你能让我从代码里引用不就行了吗？总之，因为乱七八糟的原因，Nix 还要做很多 hack 才行。不过除去上面这些问题，倒还没遇到什么特别糟糕的事情。

之前非常想用 `nix-shell` 实现 Haskell 脚本，替代 Python，但是始终不能优雅地实现。今天改用 ghcup 后实现了：

```haskell
#!/usr/bin/env runghc
{-# LANGUAGE PackageImports, OverloadedStrings #-}
import "turtle" Turtle
main = echo "it works!"
```

嗯对，现在 ghc 可以轻松找到 cabal 装的库了。在用 nix 的时候因为不想（浪费磁盘，而且实际上也不能）用 cabal 里的包所以做不到的事情，没想到这么轻易就解决了 = =!

## 性能

为什么在乎性能？因为我很在乎性能，不然我也不至于首选 Rust 不是？我最近读了很多关于 Haskell 性能的文章，大部分都是尬吹（比如[这个 SO 问题](https://stackoverflow.com/questions/35027952/why-is-haskell-ghc-so-darn-fast), [这个 wiki 页面](https://wiki.haskell.org/Performance)），少数有实际内容的看起来比较合理（比如[这篇 Haskell 表现最差的](https://pl-rants.net/posts/haskell-vs-go-vs-ocaml-vs/)）。总之，Haskell 并不能神奇地把任何代码都变成最佳形式。

对我来说，Haskell 最神奇的地方是它可以做一些非常高级的优化，比如说 ghc rewrite rules。

```haskell
collatz :: Integer -> Bool
collatz x
    | x == 0 || x == 1 = True
    | odd x            = collatz (3*x+1)
    | otherwise        = collatz (x `quot` 2)
{-# NOINLINE collatz #-}
{-# RULES "collatz" forall x. collatz x = True #-}
```

这个规则说，只要碰到形状是 `collatz x` 这样的代码，都可以替换成一个 `True`。这个例子是我随便造的，它真正的价值在于可以轻易实现类似 `bar (foo x) (foo y) = foo x (bar y z)` 的代数规则。只要自己把规则写上去，GHC 就可以自动判断某处要不要进行重写。

另外，GHC 还有一些神奇的动作：

```haskell
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
import System.IO
import Data.IORef
 
hLines :: Handle -> IO [L.ByteString]
hLines h = L.hGetContents h >>= pure . L.lines
 
main :: IO ()
main = do
  cnt <- newIORef 0
  input <- hLines stdin
  flip mapM_ input $ \line -> do
    L.putStr line
    modifyIORef cnt (+1)
    cur <- readIORef cnt
    if cur > 100
      then L.putStr (input !! (cur - 100))
      else return ()
```

这个程序使用了所谓的惰性 IO，把输入变成行流（`hLines`），如果当前行数是 i > 100，那么会输出 i-100 行，否则就什么都不输出。这个程序看起来需要把整个列表保存在内存里，不然怎么使用 `!!` 呢？但惊人的是，并没有！这个程序的内存占用是恒定的。

总而言之，很多人对 Haskell 性能的评价是 “难以捉摸”。我认为是确切的，它和我之前熟悉的所有语言的性能特征都完全不同。lazy 使得程序的行为和性能都无法 inductively reason。

对于简单的小任务，Haskell 的性能到底如何呢？我写了一个基于 ByteString 的非常 naive 的 `wc` 程序：

```haskell
import qualified Data.ByteString.Char8 as C
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  withFile (head args) ReadMode $ \handle -> do
    contents <- C.hGetContents handle
    let lines = length $ C.lines contents
        words = length $ C.words contents
        bytes = C.length contents
    putStrLn (show lines <> " " <> show words <> " " <> show bytes)
```

可以看到这个 `wc` 是非常 naive 的，这里的写法似乎暗示了整个 `contents` 会被扫描三遍。那么会不会呢？嗯，不知道🤷‍♂️ 但是可以看看它的性能表现。我下载了 UTF-8 格式的四大名著，然后反复拼接得到了一个 500MB 的 txt 文件。上面的程序 O2 编译后，在这个文件上运行了 2.841 秒。那么 macOS 自带的、C 语言写的 `wc` 要多久呢？2.293 秒。仅快了 0.5 秒。这里说「仅」，是因为上面这个程序真的是太 naive 了，花两分钟就能写出来。[如果花更多时间优化，可以更快。](https://chrispenner.ca/posts/wc)（另外不知道是不是 GNU coreutils 在 macOS 下水土不服，它花了 10 多秒才能给出结果。）由于某种我未知的原因，使用 lazy 版本的 bytestring 可以把时间继续缩短到 2.564 秒，只需加 5 个字节（把 `Data.ByteString.Char8` 改成 `Data.ByteString.Lazy.Char8`）。

对我来说这个结果已经非常惊人了。只花一点点时间写一个看上去非常简单的程序，就得到可以接受的性能，天底下竟然真有这种好事！

## 人体工学与工程化

最后想提到的是人体工学。这个词是我从 Rust 社区那边抄过来的，意思就是写代码写得舒服不舒服。对 Haskell 来说，最基本的就是类型。

而我现在对类型有点不爽了。Haskell 里充斥着 monad。Monad 好吗？好啊，它可以分离副作用，把副作用关到笼子里。但是，经常会让代码变得非常糟糕。举个例子，这是用 Haskell 的 Z3 绑定解决四皇后问题的代码。

```haskell
script :: Z3 (Maybe [Integer])
script = do
  q <- traverse mkFreshIntVar ["q1", "q2", "q3", "q4"]
  [_1, _4] <- traverse mkInteger [1, 4]
  assert =<< mkAnd =<< sequence (   [ mkLe _1 qi | qi <- q ]
                                 ++ [ mkLe qi _4 | qi <- q ])
  assert =<< mkDistinct q
  assert =<< mkNot =<< mkOr =<< T.sequence [diagonal (j-i) (q!!i) (q!!j) | i<-[0..3], j<-[i+1..3]]
  fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalInt m) q
  where mkAbs x = do
          _0 <- mkInteger 0
          join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
        diagonal d c c' =
          join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> (mkInteger (fromIntegral d))
```

……你看得清在做什么吗？为什么会这样？最搞笑的是 `join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x` 这里。为什么要写这么多乱七八糟的东西？我当然知道这是在干什么，但是，为什么非要搞成这样……？话说回来，这个库是比较低级的库（对 libz3 的直接封装），如果硬要比，可以和 z3 的 C API 相比，这时候就会发现，Haskell 能让这些代码写到一行里面也不错？

最后回到最开始的移植 Go 代码里。之前提到了我使用了 `StateT`，于是突然间你不管做什么 `IO` 都要加上 `lift` 了。这太丑了……于是人们又开发出了一堆解决方案……这些解决方案可能很聪明，但是说到底都是在 workaround Haskell 本身的限制。真的有必要么？相比之下，OCaml、Rust 等允许副作用的语言里这甚至算不上问题。

> Are you quite sure that all those bells and whistles, all those wonderful facilities of your so-called “powerful” programming languages, belong to the solution set rather than to the problem set?
> 
> -- Edsger W. Dijkstra

但是只要你忍住不乱用类型，所有东西都塞到 IO 里面，顶多用用 Reader 什么的，那其实 Haskell 是非常简单的。[Z.Haskell](https://z.haskell.world) 的作者韩冬就表达过这样的看法。嗯，我对 Haskell 工程化的看法主要就是受他在这个 [Z.Haskell 讲座](https://www.bilibili.com/video/BV1VU4y1h7QG) 的影响。

{{ bilibili(bvid="BV1VU4y1h7QG") }}

## 结论

Haskell 社区一直有这样一个问题：如果 Haskell 真的那么好，那应该早就被所有人用上了。是不是 Haskell 其实并没有那么好？（有很多人对这个问题有自己的看法，不提也罢。）

上面写了那么多，我的态度已经很明显了：Haskell 本身还行，但是周边属实有点欠缺。不过我是乐观的，毕竟是周边问题，只要有人投入都可以解决。（而且经验告诉我们，就算本体有根本问题，只要有足够的投入也可以解决，例如 Python、Javascript。）而且我认为 Haskell 社区是非常 “理性” 的，做任何事都会努力去寻找正确的做法，只要找到了更好的做法，之前的做法就会被抛弃，我非常喜欢这一点。非常奇妙的是，Haskell 社区里很多大的、重要的项目都是单人或者很少人推动的，也许…… Haskell 确实可以提高单人的生产力？（一个猜想，很可能错。）

虽然这篇文章看似在黑，但对我来说，Haskell 极有可能会成为我的首选工具（而不是 Rust），这段时间体验下来，感觉还是非常趁手的。而且我还有很多 idea 需要有足够抽象能力的语言才能实现。但是还是不得不说，Rust 修正了 Haskell 中的很多很多很多设计错误，并且工具链非常好好好，能火起来真的不是偶然，我也会继续用 Rust 写要求高性能的程序，hs、rs 两开花。

## 逸事一则

上面提到的那个 Telegram bot 在运行了几天之后段错误了。嗯……这让我说点什么好？
