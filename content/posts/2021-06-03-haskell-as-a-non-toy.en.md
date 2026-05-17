+++
title = "Haskell as a non-toy"
date = 2021-06-03
[taxonomies]
tags = ["haskell", "rust", "review"]
[extra]
headline = "Is Haskell pleasant to use?"
math = true
comment = true
+++

> Disclaimer: This post was translated into English by an AI model. It may contain mistakes or awkward wording.

I recently spent some time using Haskell in practice. This post summarizes some of my impressions.

<!--more-->

> **Disclosure**: The material in this blog post has not been reviewed, endorsed, or approved of by the Rust Foundation. For more information on the Rust Foundation Trademark Policy, click [here](https://foundation.rust-lang.org/).

## Why Haskell Again?

Yes, I had learned Haskell before, and I was already familiar with many of its concepts. Monoid? Monad? Traversable? Foldable? Quantified constraints? Data kinds? Rank-N types? Type families? I would not say I knew them extremely well, but I had at least encountered and used them. In fact, I had always used Haskell for small theoretical experiments. Since Haskell still does not have real dependent types, I sometimes use Coq instead. For many people, learning Haskell to this point is enough; after all, many really do think Haskell is only for learning. "Surely nobody actually writes Haskell, right?"

On the other hand, I have never been fully satisfied with the tools in my hand. Before this experiment, my most frequently used programming languages, excluding special needs, were C++, Python, and Rust. I also use Emacs Lisp for Emacs configuration, though I do not write much of it.

1. I use C++ for experiments close to operating systems and hardware, such as memory models, and for data structures, because writing some of them in Rust brings little benefit while adding many `unsafe` markers.
2. I use Python for scripts: complex shell scripts, network-related work, calculator-like tasks, and quick prototypes.
3. For anything else, I choose Rust first.

So my main working languages are Rust and Python. For most people this combination may be good enough, but I always felt some itch remained.

1. Rust is "heavyweight" in every sense. Want to use a third-party crate? Create a new project first. Projects such as [cargo™ play](https://boats.gitlab.io/blog/post/2017-12-27-things-explicit-is-not/) alleviate this somewhat. The biggest pain is that Rust code is very ["noisy"](https://boats.gitlab.io/blog/post/2017-12-27-things-explicit-is-not/): obvious things must be written out, and once written they are often hard to compose. Rust also creates strong psychological pressure to consider memory use and computational cost, which often makes me fall into the "perfect is the enemy of good" trap.
2. Python, by contrast, lets you write almost anything with little mental burden. But CPython's performance and concurrency annoy me greatly, and Python's abstraction power is not high.

In short, learning multiple programming languages does not make you a better programmer; it makes you dissatisfied with every language. [Learning Python/Haskell can make you a worse programmer](https://lukeplant.me.uk/blog/posts/why-learning-haskell-python-makes-you-a-worse-programmer/).

So I decided to pick up Haskell again. The trigger was that I once wrote a [Telegram bot](https://github.com/ksqsf/rustdalaobot) in Haskell. The bot was simple: match message content and reply. Following my usual workflow, I first thought of Rust. Coincidentally, I had written a bot in Rust before, and the experience was terrible. This time I wanted to try another language. I thought of Haskell, found that Haskell did have Telegram bot libraries, and started writing.

The whole process was very comfortable and fast. The first version took only ten minutes. While writing Haskell, I could freely use all my knowledge; in Rust or Python I often need to think about how to "compile" my ideas into a lower-level language. For example, in that bot I wrote a [simple text matcher](https://github.com/ksqsf/rustdalaobot/blob/991ae4cbd741e996627f83eee7c9453470a58799/app/Main.hs#L19). Each matcher is a function from strings to booleans. At first I defined a new type in the usual way, but soon found that I was merely reproducing Boolean algebra, and even lacked counterparts to True and False. Later I represented matchers as higher-order functions; the code became shorter and more flexible, and `patternFromWords` went from partial to total.

After this pleasant experience, I began seriously considering whether Haskell could be used seriously. This was not the only experience that changed my view; my view of abstraction has been changing, and I value compositionality more and more. Also, Haskell's ecosystem is not as barren as people say: things I want often exist, such as Telegram bot frameworks; GHC has good support for concurrency and parallelism; and some friends I know also love Haskell.

So I picked up Haskell again.

## Concurrency

I have always been interested in concurrency and parallelism and have studied quite a bit about them. I especially hate CPython, which is a textbook negative example. When I previously used Python for a meet-in-the-middle attack, the computation was CPU-bound and produced a large amount of data. Because of the infamous GIL, CPython cannot use threads directly for such work, so I used `multiprocessing`. Sounds fine? It was not. I eventually needed to send several GB of results to the main process, and that became a performance bottleneck. Python may now have shared-memory facilities, but I really do not want to touch them, and the `multiprocessing` interface is awful. I was forced to rewrite it in Rust, and Rust was fast enough even single-threaded.

When I had just picked up Haskell again, I happened to read an article titled ["The Collapse of the Go Performance Myth"](https://zhuanlan.zhihu.com/p/368304027). It contained a small test program comparing Go and .NET concurrency performance, so I translated the Go code into Haskell.

{{ gist(url="https://gist.github.com/ksqsf/e267b667dbaad03f9486e99a15f07edb") }}

**The coding process** was fairly smooth. The hardest part was not implementing goroutine-like behavior; [Concurrent Haskell](https://en.wikipedia.org/wiki/Concurrent_Haskell) is user-space lightweight threads with an m:n relation to OS threads, much like goroutines. The hardest part was temporarily suppressing stdout output. In the code above I defined a `Job` monad whose `Bool` indicates whether output is allowed. Another interesting point: at first I wanted to use `ReaderT` directly instead of defining a new type, but I ran into many type errors I could not work out. After changing to this form, the types became easier to reason about. In a sense, Haskell forced me to make the code clearer.

Writing this program also made me feel better about myself. It used monad transformers and other things that are often considered hard, but I wrote it naturally. I can now confidently say I have graduated from beginner status. LOL.

**The experimental result** was poor. My memory is small, so `runs = 10` could not finish. Later I learned more about GHC, including learning some GHC GC from [this video](https://www.youtube.com/watch?v=vvLDerKtUWE&t=2817s), and performance tools such as [ThreadScope](https://wiki.haskell.org/ThreadScope), and did many messy experiments. Eventually I could get faster results with smaller `runs`, but the results were still bad. The claim in the Zhihu comments that it could finish in under a second was unimaginable.

I could keep covering my ears and say the article's example was contrived, but I do not think that is the right attitude. The right attitude is to really understand why GHC performs poorly here. I am not interested in digging deeper for now, so I will leave this hole open.

Haskell's user-space threads are stackful coroutines. This model is very intuitive. At first I wondered why Haskell did not model this with delimited continuations and instead made it a primitive, but later I learned that the newer model was only [proposed in 2006](http://lambda-the-ultimate.org/node/1435), while Concurrent Haskell is a design from the twentieth century.

In short, I think Haskell's concurrency implementation is usable, but not the best. It cannot compete with Go, .NET, or the JVM, but it is far better than Python.

One more benefit for me: Rust does not have semaphores, while [Haskell has `QSem`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-QSem.html), so I can use Haskell to study [*The Little Book of Semaphores*](https://greenteapress.com/wp/semaphores/).

## FFI and Memory Safety

Next I considered that if Haskell lacks a library, FFI can compensate. So I started reading about Haskell FFI and wrote an [OpenCC binding](https://hackage.haskell.org/package/opencc) as practice.

To summarize Haskell's view of FFI: Haskell is a gentle land of laziness and purity, while FFI is the gate to the evil outside world, so FFI lives in `IO`. Is this reasonable? In most cases, yes. But some computations are pure. Indeed they are, and this is one reasonable use of `unsafePerformIO`. Another reasonable case is a global `IORef`, with a `NOINLINE` pragma.

> Because `unsafePerformIO` exists, `IO a` cannot simply be understood as `State RealWorld a`. Why? Because `RealWorld` is not accessible and mutable only by functions whose types mention `IO`. What should we do instead? Perhaps formalize all of GHC.

Second, resources are managed through pointers, namely [`Ptr`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Foreign-Ptr.html). Then we encounter a major question: how should resources be managed? GC is good at reclaiming memory, but bad at reclaiming non-memory resources. In simple cases, we can use `ForeignPtr` with a finalizer. Haskell itself uses the bracket idiom, roughly like `defer`. In short, I still think RAII is a better solution.

Haskell does not help you manage resources better. I personally hit a double-free bug due to a coding mistake. Then I needed to debug it. Debug? Use [`Debug.Trace`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html). If you know Rust, this is like the `dbg!()` macro. Why not insert `printf` or use step debugging? The former is not really feasible because Haskell is non-strict; the latter was not helpful because the crash occurred when the GC called the finalizer.

In retrospect, if Haskell had linear types, this bug would not have happened. Fortunately, [GHC 9.0 already has linear types](https://www.haskell.org/ghc/blog/20210204-ghc-9.0.1-released.html), and I look forward to what they will do in the future. [Tweag.io has many articles on Haskell linear types](https://www.tweag.io/blog/tags/linear-types/) worth reading.

My summary of this section is: Rust wins too much here.

## Strings

Strings may be a smaller issue, but still a point of complaint.

While writing the OpenCC binding, I inevitably encountered `String`, `ByteString`, and `Text`. Haskell's built-in string type is a linked list of `Char`. Each `Char`, regardless of the character itself, already carries eight bytes of pointer overhead. So I inevitably found `ByteString` and `Text`, the two de facto standards. Really, please make `String` a UTF-8 byte array like Rust.

`ByteString` is an array of bytes behind a `ForeignPtr`; `Text` is UTF-16. The good news is that [Text is moving to UTF-8](https://discourse.haskell.org/t/hf-tech-proposal-1-utf-8-encoded-text/2499). Also, `Text` can only store Unicode strings and lacks a Rust-like lossy conversion function. `decodeUtf8` throws an exception on invalid input; [Michael Snoyman has complained about this](https://www.snoyman.com/blog/2020/11/haskell-bad-parts-2/).

It sounds like changing a type should be enough. The problem is that the entire standard library uses `String`. Every library therefore brings its own IO functions: `Data.Text.IO.putStrLn`, `Data.ByteString.Char8.hGetContents`, and so on. You constantly have to convert among string types. An amusing detail is that the lazy/strict naming conventions in the standard library and these libraries are reversed. I can understand it, but still want to complain.

Summary: Rust wins.

## Exceptions

Exceptions were mentioned above. Aren't exceptions just bottom values? What is there to say? Wrong. If they were truly uncatchable, perhaps that would be fine, but Haskell exceptions can be caught, and asynchronous exceptions are used for many things. For example, canceling a Haskell thread means throwing an exception to that thread. To the thread, the exception is asynchronous. This also introduces a pile of FFI problems. Exceptions are useful and may be necessary for stackful coroutines, but in Haskell they are abused. The large number of partial functions in Prelude is a long-standing complaint.

By contrast, Rust appears to care more about types here. The Rust community has a unified view of panic: an unrecoverable error. In other words, you should not catch it. Rust has mechanisms to catch it, but overall this is discouraged; you can even set `panic=abort` in `Cargo.toml` so catching is impossible. Partial operations are also clearly and uniformly marked with `unwrap`, `expect`, and so on. Haskell lacks such a unified marker, so you often need to read the documentation, despite the slogan that types are documentation, or you may step into a trap like `decodeUtf8`.

## Tooling

Haskell's tooling, namely `ghc`, `cabal-install`, and `stack`, has traditionally been a sore spot. But here I want to say that the overall experience currently seems fine, and I have not encountered too many problems.

I used Stack before and never had accidents, but I disliked having several LTS sets on my computer, and I did not want to remember LTS version numbers. Remembering GHC versions is already enough. If you have disk space and good memory, Stack is probably excellent.

Recently I have mainly used Nix to manage my Haskell toolchain. Why? Because Homebrew was slow, I switched to Nix, and managed Haskell along the way. Also Cabal still breaks. For example, I installed GHC 8.10.4 from nixpkgs, whose bundled `directory` package was older than the Hackage version by `0.0.0.1`, causing many Hackage packages to fail to install. I do not understand exactly how Cabal works; I looked through the cabal files and found no clue. The version bounds all looked compatible, which was bizarre.

Cabal also has a mysterious problem: it cannot uninstall. Cargo™ has an `uninstall` command.

Haskell makes many things needlessly complicated. A package is a package; I do not care whether it is source code through Cabal or a binary package through `ghc-pkg`. Just let me reference it from code. Because of various reasons, Nix needs many hacks. But aside from these issues, I have not encountered anything especially terrible.

I had long wanted to use `nix-shell` to implement Haskell scripts as a replacement for Python, but could not do it elegantly. Today, after switching to ghcup, I achieved it:

```haskell
#!/usr/bin/env runghc
{-# LANGUAGE PackageImports, OverloadedStrings #-}
import "turtle" Turtle
main = echo "it works!"
```

Now GHC can easily find Cabal-installed libraries. When using Nix, I could not do this because I did not want to, and in practice could not, use Cabal packages. I did not expect it to be solved so easily.

## Performance

Why care about performance? Because I care about performance. Otherwise I would not choose Rust first. I recently read many articles about Haskell performance. Most were awkward praise, such as [this Stack Overflow question](https://stackoverflow.com/questions/35027952/why-is-haskell-ghc-so-darn-fast) and [this wiki page](https://wiki.haskell.org/Performance), while a few with real content seemed more reasonable, such as [this post about where Haskell performs worst](https://pl-rants.net/posts/haskell-vs-go-vs-ocaml-vs/). In short, Haskell cannot magically turn any code into the optimal form.

For me, the most magical part of Haskell is that it can do very advanced optimizations, such as GHC rewrite rules:

```haskell
collatz :: Integer -> Bool
collatz x
    | x == 0 || x == 1 = True
    | odd x            = collatz (3*x+1)
    | otherwise        = collatz (x `quot` 2)
{-# NOINLINE collatz #-}
{-# RULES "collatz" forall x. collatz x = True #-}
```

This rule says that whenever code shaped like `collatz x` is encountered, it can be replaced with `True`. This example is artificial, but the real value is that one can easily implement algebraic rules such as `bar (foo x) (foo y) = foo x (bar y z)`. Once you write the rule, GHC can automatically decide whether to rewrite at a given point.

GHC also does some magical things:

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

This program uses so-called lazy IO, turning input into a stream of lines with `hLines`. If the current line number is `i > 100`, it outputs line `i - 100`; otherwise it outputs nothing. The program looks as though it must keep the whole list in memory, because otherwise how could it use `!!`? Astonishingly, it does not. Its memory usage is constant.

Many people describe Haskell performance as "hard to predict". I think that is accurate. Its performance characteristics are completely different from all the languages I previously knew. Laziness makes program behavior and performance difficult to reason about inductively.

For simple small tasks, how fast is Haskell? I wrote a very naive `wc` based on `ByteString`:

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

This `wc` is extremely naive and appears to scan `contents` three times. Does it? I do not know. But we can look at performance. I downloaded UTF-8 versions of the four great classical Chinese novels and concatenated them into a 500 MB text file. Compiled with `-O2`, the program ran in 2.841 seconds. macOS's C `wc` took 2.293 seconds, only 0.5 seconds faster. I say "only" because this program is truly naive and takes two minutes to write. [With more optimization, it could be faster.](https://chrispenner.ca/posts/wc) For a reason I do not know, using the lazy version of bytestring reduced the time to 2.564 seconds by changing only five bytes in the import.

This result was astonishing to me. With very little time, one can write a simple-looking program and get acceptable performance. Such good things really exist.

## Ergonomics and Engineering

The last thing I want to mention is ergonomics, a word I borrowed from the Rust community. It means how comfortable code is to write. For Haskell, the most basic issue is types.

And I am now somewhat unhappy with the types. Haskell is full of monads. Are monads good? Yes: they separate side effects and put them in a cage. But they often make code ugly. For example, here is code using Haskell's Z3 bindings to solve the four queens problem:

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

Can you see what it is doing? Why does it become like this? The funniest part is `join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x`. Why write so much messy stuff? Of course I know what it does, but why does it have to look like this? On the other hand, this library is a low-level binding, a direct wrapper around `libz3`. If we compare it with Z3's C API, being able to write this code in one line is maybe not bad.

Returning to the earlier Go-port example, I mentioned using `StateT`; suddenly every `IO` operation needs `lift`. That is ugly. People then develop many solutions to this problem. They may be clever, but ultimately they are workarounds for Haskell's limitations. Is this really necessary? In languages that permit side effects, such as OCaml or Rust, this is not even a problem.

> Are you quite sure that all those bells and whistles, all those wonderful facilities of your so-called "powerful" programming languages, belong to the solution set rather than to the problem set?
>
> -- Edsger W. Dijkstra

But if you can resist abusing types, put everything into `IO`, and at most use something like `Reader`, then Haskell is actually very simple. Han Dong, the author of [Z.Haskell](https://z.haskell.world), has expressed a similar view. My view of Haskell engineering is mainly influenced by his [Z.Haskell talk](https://www.bilibili.com/video/BV1VU4y1h7QG).

{{ bilibili(bvid="BV1VU4y1h7QG") }}

## Conclusion

The Haskell community has long faced this question: if Haskell is really so good, should it not already be used by everyone? Is Haskell perhaps not actually that good? Many people have their own views on this question; never mind those here.

After writing all of the above, my attitude should be clear: Haskell itself is fine, but the surrounding ecosystem is indeed somewhat lacking. Still, I am optimistic. After all, ecosystem problems can be solved as long as people invest in them. Experience also tells us that even if the language itself has fundamental problems, enough investment can solve them too; see Python and JavaScript. I also think the Haskell community is very "rational": when doing anything, it tries hard to find the right way. Once a better way is found, the old way is abandoned. I like this very much. It is also fascinating that many large and important projects in the Haskell community are driven by one person or very few people. Maybe Haskell really can improve individual productivity? This is only a guess, and probably wrong.

Although this post may look like a criticism, for me Haskell is very likely to become my first-choice tool instead of Rust. After using it for a while, it feels very handy. I also have many ideas that require a language with sufficient abstraction power. But I still have to say that Rust corrected many, many, many design mistakes in Haskell, and its toolchain is very, very, very good. Its popularity is no accident. I will continue to use Rust for programs with high performance requirements. Let Haskell and Rust both bloom.

## An Anecdote

The Telegram bot mentioned above segfaulted after running for a few days. Well... what am I supposed to say about that?
