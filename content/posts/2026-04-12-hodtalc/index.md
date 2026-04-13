+++
title = "Higher-order Data Types à la Carte"
date = 2026-04-12
[taxonomies]
tags = ["haskell", "dtalc"]
[extra]
headline = "编码命令式程序意外地并不简单"
math = true
comment = true
+++

最近有个小项目需要表达一些 AST，而我期望这个 AST 本身是可扩展的：我可以定义一些语言碎片，然后再按需将它们组合起来。这听起来很适合 DTALC！但很快你就会发现——经典的 DTALC 写法是行不通的。这篇文章记录一下解法。

<!-- more -->

## 经典 DTALC

<aside>
这篇文章的代码 self-contained，可以复制运行尝试。
</aside>

为了能用 `do` 写代码，我们选定基本的 AST 骨架为 `Free` monad：

```haskell
import Control.Monad

data Free f a = Pure a | Impure (f (Free f a))
  deriving Functor

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (Pure a) >>= f = f a
  (Impure a) >>= f = Impure $ fmap (>>= f) a
```

然后是 DTALC 的一些基本样板代码：

```haskell
data (f :+ g) a = Inl (f a) | Inr (g a)
  deriving Functor
class f :< g where
  inj :: f a -> g a
instance f :< f where
  inj = id
instance (Functor f, Functor g) => f :< (f :+ g) where
  inj = Inl
instance {-# OVERLAPPABLE #-} (Functor h, f :< g) => f :< (h :+ g) where
  inj = Inr . inj
```

假设这个语言有 2 个碎片，一个叫 `BaseF` 提供最基本的通用功能，一个叫 `ConcF` 提供并发原语。

```haskell
-- | 表达式
data Expr a where
  Lit :: a -> Expr a

-- | 内存地址
newtype Addr ty = Addr Integer

-- | 扩展语言 ConcF 提供结构化并发原语
data ConcF k where
  Fork :: [k] -> k -> ConcF k
deriving instance Functor ConcF

-- | 基本语言
data BaseF k where
  If    :: Expr Bool -> k -> k -> k
  While :: Expr Bool -> k -> k
deriving instance Functor BaseF
if_ :: (BaseF :< f) => Expr Bool -> Free f () -> Free f () -> Free f ()
if_ c t f = Impure . inj $ If c t f (Pure ())
while_ :: (BaseF :< f) => Expr Bool -> Free f () -> Free f ()
while_ c t = Impure . inj $ While c t (Pure ())

-- | 扩展语言 ConcF 提供结构化并发原语
data ConcF k where
  Fork :: [k] -> k
deriving instance Functor ConcF
fork :: (ConcF :< f) => [Free f ()] -> Free f ()
fork threads = Impure . inj $ Fork threads (Pure ())

-- | 扩展语言 TestF 提供插入任何 IO 的能力
data TestF k where
  AnyIO :: IO () -> k -> TestF k
deriving instance Functor TestF
anyIO :: (TestF :< f) => IO () -> Free f ()
anyIO io = Impure . inj $ AnyIO io (Pure ())
```

类型检查通过，看起来一切正常！我们来写个解释器：

```haskell
import Control.Concurrent

-- | 解释执行类型类，还是 DTALC 样板代码
class (Functor f, Monad m) => Exec f m where
  execAlg :: f (m a) -> m a
instance (Exec f m, Exec g m) => Exec (f :+ g) m where
  execAlg (Inl fa) = execAlg fa
  execAlg (Inr ga) = execAlg ga
exec :: (Exec f m) => Free f a -> m a
exec (Pure x) = pure x
exec (Impure op) = execAlg (fmap exec op)

-- 这里直接把 IO 当作解释器
instance Exec BaseF IO where
  execAlg (If (Lit c) t f k) = (if c then t else f) >> k
  execAlg (If _ _ _ _) = error "暂不考虑"
  execAlg (While (Lit c) t k) =
    if c
    then t >> execAlg (While (Lit c) t k) >> k
    else k

instance Exec ConcF IO where
  execAlg (Fork threads k) = do
    mvars <- replicateM (length threads) newEmptyMVar
    forM (zip threads mvars) $ \(thread, mvar) -> do
      thread
      putMVar mvar ()
    forM_ mvars takeMVar
    k

instance Exec TestF IO where
  execAlg (AnyIO io k) = io >> k
```

看起来还是一切正常！应该成功了吧？来写个小程序测试一下：

```haskell
example :: Free (BaseF :+ ConcF :+ TestF) ()
example = do 
  if_ (Lit True) (anyIO $ putStrLn "true") (anyIO $ putStrLn "false")
  anyIO $ putStrLn "done"
```

```haskell
λ> exec example
true
done
done
```

布兑！为什么 done 输出了两次？

## 到底发生了什么

要解释这个问题，就得先回头看看 Free monad 到底在干什么。

不妨直接把这个程序给展开：

```haskell
example 
= do  {- 直接展开 smart constructor -}
  Impure . inj $ If (Lit True)
                    (Impure . inj $ AnyIO (putStrLn "true") (Pure ()))
                    (Impure . inj $ AnyIO (putStrLn "false") (Pure ()))
                    (Pure ())
  Impure . inj $ AnyIO (putStrLn "done") (Pure ())
= {- 展开 do -}
  ((Impure . inj) $ If (Lit True)
                    (Impure . inj $ AnyIO (putStrLn "true") (Pure ()))
                    (Impure . inj $ AnyIO (putStrLn "false") (Pure ()))
                    (Pure ()))
  >>= \_ -> ((Impure . inj) $ AnyIO (putStrLn "done") (Pure ()))
= {- 展开 bind -}
  (Impure . inj) $ fmap (>>= kont) firstStep
  where firstStep = If (Lit True)
                    (Impure . inj $ AnyIO (putStrLn "true") (Pure ()))
                    (Impure . inj $ AnyIO (putStrLn "false") (Pure ()))
                    (Pure ())
        kont = \_ -> ((Impure . inj) $ AnyIO (putStrLn "done") (Pure ()))
= {- 展开 fmap；注意到 fmap 在所有 k 类型参数上都工作，即 -}
  (Impure . inj) $ If (Lit True)
                    ((Impure . inj $ AnyIO (putStrLn "true") (Pure ())) >>= kont)
                    ((Impure . inj $ AnyIO (putStrLn "false") (Pure ())) >>= kont)
                    ((Pure ()) >>= kont)
  where kont = \_ -> ((Impure . inj) $ AnyIO (putStrLn "done") (Pure ()))
```

破案了，原因是我们把子程序也写成了 `k`，而 Functor 参数 `k` 在 Free monad 的语境下的意义是 continuation，即「下一步」是什么。（这也是为什么一开始就把它叫 `k`。）

换句话说，我们不能把子程序用 `k` 来表示。相反，应该直接写入「代码/AST」本身，即

```haskell
data BaseF k where
  If    :: Expr Bool -> (Free BaseF ()) -> (Free BaseF ()) -> k -> BaseF k
  While :: Expr Bool -> (Free BaseF ()) -> (Free BaseF ()) -> BaseF k
```

但很显然这样做是有问题的：我们当然希望 while 循环里可以用别的扩充了的语句，而不仅仅是 BaseF 的语句。因此，我们必须要引入一个额外的参数用于 open recursion，这里不妨叫 `sig` 表示语句 signature：

```haskell
data BaseF sig k where
  If    :: Expr Bool -> (Free sig ()) -> (Free sig ()) -> k -> BaseF sig k
  While :: Expr Bool -> (Free sig ()) -> (Free sig ()) -> BaseF sig k

data ConcF sig k where
  Fork :: [Free sig ()] -> k -> ConcF sig k

data TestF sig k where
  AnyIO :: IO () -> k -> TestF sig k
```

遗憾的是，这个做法也行不通。至于为什么，留作读者练习。（提示：难点在于最终合成语言，因为你需要把 sig Fix 起来。）

## 高阶 DTALC

由于上述两个办法都走不通，不得不考虑一个更合理的办法。最好是能同时处理 k 和 sig 两个 open recursion。

我们回头重新思考一下 DTALC 到底在做什么：

- 我们用 Functor 表示语句/操作/命令的形状；
- 然后用 Coproduct （上文的 `:+`） 将两个 Functor 组合起来，结果还是 Functor；
- 在 Functor 之间可构成序关系 `:<`，较小的 functor 可以注入大 functor；
- 对任何 Functor 都有相应的 Free monad，这里才是我们实际在用的。

但我们注意到，这个语句/操作/命令/effect里 **必须没有子语句/操作/命令/effect**。换句话说，经典的 DTALC 是一阶的，而无法应对具有「子 effect」的情形。这种 effect 里包 effect 的情况就叫 **高阶 effect**，对应地，这里提到的 dtalc 写法就称作 **高阶 dtalc**。这里 `BaseF` 和 `ConcF` 是含有子命令的（高阶 effect），而 `TestF` 没有，所以 `TestF` 是一阶的。

我们知道一个结果为 a 的计算/程序/AST依然可以用 functor（甚至 monad）来描述，所以可以直接在这个地方做 open recursion：

```haskell
data BaseF f k where
  If    :: Expr Bool -> (f ()) -> (f ()) -> k -> BaseF sig k
  While :: Expr Bool -> (f ()) -> (f ()) -> BaseF sig k
data ConcF f k where
  Fork :: [f ()] -> k -> ConcF f k
data TestF f k where
  AnyIO :: IO () -> k -> TestF f k
```

然后定义对应于 `Free` 的、带有额外 `sig` 参数的高阶 free monad，就叫它 `HFree` 吧：

```haskell
data HFree sig a = HPure a | HImpure (sig (HFree sig) (HFree sig a))
```

这里需要注意 `sig` 本身不是 Functor，它的 kind 是 `* -> * -> *`，而 `HFree sig` 我们希望是个 monad，即 `HFree` 把 `sig` 的两个类型参数都封锁在了里面。

同时我们还想继续保留 `(BaseF :< sig, TestF :< sig)` 这种写法，也就意味着 DTALC 本身必须扩充成高阶的：

```haskell
data (f :+ g) sig a = Inl (f sig a) | Inr (g sig a)
```

`HFree sig` 本身应该是个 monad，但注意， `Functor` 已经无法直接定义出来了：

```haskell
instance Functor (HFree sig) where
  fmap f (HPure a)    = HPure (f a)
  fmap f (HImpure op) = HImpure _hole
--  _hole :: sig (HFree sig) (HFree sig b)
```

这里我们可以尝试强行定义，因为 `HFree sig` 是 Functor，`sig (HFree sig)` 看起来也像个 functor，那当然：

```haskell
instance Functor (sig (HFree sig)) => Functor (HFree sig) where
  fmap f (HPure a)    = HPure (f a)
  fmap f (HImpure op) = HImpure $ fmap (fmap f) op
```

但是这样的问题在于我们不知道 dtalc 组合的 `f :+ g` 到底是什么东西——它本身并不是个 Functor，我们只能讨论 `instance Functor (HFree (f :+ g))`，这会和之前的 `instance Functor (HFree sig)` 重叠。总之，这也是条死路。

但上面的探索提示我们 `sig` 本身具有某种性质，两个 sig 组合起来依然具有这种性质。具体是什么性质呢？考虑 `BaseF f a` 和 `BaseF (f :+ g) b`，我们希望 `BaseF` 命令形状不变，但 `f` 总是可以注入到 `f :+ g` 里。换句话说，第二个参数应该是可以换的。我们不妨把这种 “内层 functor” 可替换的东西叫 `HFunctor`：

```haskell
-- | “高阶” functor
class HFunctor sig where
  hmap :: (forall x. f x -> g x) -> (a -> b) -> sig f a -> sig g b
-- 第一个参数可理解为函子 f 到函子 g 的自然变换
```

`HFunctor` 表明：只要我们可以把 `f` 注入到 `f :+ g` 里，就可以把 `BaseF f` 转换成 `BaseF (f :+ g)`。这里的定义更宽泛一点。

有了它之后就可以定义 functor、applicative 和 monad 了：

```haskell
instance HFunctor sig => Functor (HFree sig) where
  fmap f (HPure a)    = HPure (f a)
  fmap f (HImpure op) = HImpure (hmap id (fmap f) op)

instance HFunctor sig => Applicative (HFree sig) where
  pure  = HPure
  (<*>) = ap

instance HFunctor sig => Monad (HFree sig) where
  HPure a    >>= k = k a
  HImpure op >>= k = HImpure (hmap id (>>= k) 
```

`HFree` 的直觉和 `Free` 相同：`sig` 代表命令的形状，`HFree sig` 代表程序的形状。

现在可以定义 DTALC 的样板了：

```haskell
data (f :+ g) sig a = Inl (f sig a) | Inr (g sig a)
infixr 6 :+

instance (HFunctor f, HFunctor g) => HFunctor (f :+ g) where
  hmap t f (Inl x) = Inl (hmap t f x)
  hmap t f (Inr x) = Inr (hmap t f x)

class (HFunctor f, HFunctor g) => f :< g where
  inj :: f sig e -> g sig e
instance (HFunctor f) => f :< f where
  inj = id
instance (HFunctor f, HFunctor g) => f :< (f :+ g) where
  inj = Inl
instance {-# OVERLAPPABLE #-} (HFunctor h, f :< g) => f :< (h :+ g) where
  inj = Inr . inj
```

然后重写之前的语言，虽然 `HFunctor` 写起来很机械，但还是要手写：

```haskell
data BaseF f k where
  If    :: Expr Bool -> f () -> f () -> k -> BaseF f k
  While :: Expr Bool -> f () -> k -> BaseF f k
instance HFunctor BaseF where
  hmap t f (If c' t' f' n') = If c' (t t') (t f') (f n')
  hmap t f (While c' b' n') = While c' (t b') (f n')

data ConcF f k where
  Fork :: [f ()] -> k -> ConcF f k
instance HFunctor ConcF where
  hmap t f (Fork ts' n') = Fork (t <$> ts') (f n')

data TestF f k where
  AnyIO :: IO () -> k -> TestF f k
instance HFunctor TestF where
  hmap _ f (AnyIO io' n') = AnyIO io' (f n')
```

基本原则是：如果是子程序，就用自然变换 `t`，如果不是，就用普通的 functor 的 `f` 直接转换里面的数据。也是很机械的。

然后就可以发现，之前的 smart constructor 居然可以原样工作：

```haskell
if_ :: (BaseF :< sig) => Expr Bool -> HFree sig () -> HFree sig () -> HFree sig ()
if_ c t f = HImpure . inj $ If c t f (HPure ())
while_ :: (BaseF :< sig) => Expr Bool -> HFree sig () -> HFree sig ()
while_ c t = HImpure . inj $ While c t (HPure ())

fork :: (ConcF :< f) => [HFree f ()] -> HFree f ()
fork threads = HImpure . inj $ Fork threads (HPure ())

anyIO :: (TestF :< f) => IO () -> HFree f ()
anyIO io = HImpure . inj $ AnyIO io (HPure ())
```

但是代数的类型类需要做少量修改：

```haskell
class (HFunctor sig, Monad m) => Exec sig m where
  execAlg :: sig m (m a) -> m a
instance (Exec f m, Exec g m) => Exec (f :+ g) m where
  execAlg (Inl fa) = execAlg fa
  execAlg (Inr ga) = execAlg ga

instance Exec BaseF IO where
  execAlg (If (Lit c) t f k) = pure (if c then t else f) >> k
  execAlg (If _ _ _ _) = error "暂不考虑"
  execAlg (While (Lit c) t k) =
    if c
    then pure t >> execAlg (While (Lit c) t k) >> k
    else k

instance Exec ConcF IO where
  execAlg (Fork threads k) = do
    mvars <- replicateM (length threads) newEmptyMVar
    forM (zip threads mvars) $ \(thread, mvar) -> do
      thread
      putMVar mvar ()
    forM_ mvars takeMVar
    k

instance Exec TestF IO where
  execAlg (AnyIO io k) = io >> k
```

绕了一大圈，于是我们实现了所谓的高阶 dtalc，example 也终于可以正常工作了：

```haskell
λ> exec example
true
done
```

现在可以在命令里任意添加子程序了，而 dtalc 用起来就和一阶的版本一样好用。

## 附录：如何直观地理解（并非）

dtalc 样板代码只需复制粘贴即可，但对好奇的读者来说，上面的讨论中主要诉诸于直觉和类型制导，但依然非常难以理解。这里尝试用 ~~抽象废话~~ 范畴论解释一下，或许还更好理解一点（并不）。

我们已经知道 DTALC 本身是在组合函子，那么高阶 DTALC 肯定也在组合些什么。如果仔细观察一下 `BaseF`：对于任意函子 `f`，`BaseF f` 也是个函子。这就意味着，`BaseF` 本身是个从函子到函子的映射，或者也可以叫做 functor transformer。可以类比 `Free` 把 functor 映成一个 monad（也是函子）。因此，高阶 dtalc 不组合 functor，而是组合 functor transformer。

考虑范畴 \\(Hask\\) 及其自函子范畴。`BaseF` 把自函子映为另一个自函子，即 `BaseF` 是自函子范畴上的自函子。`BaseF` 如果是函子，它必然可以映射函子范畴上的态射（即自然变换），即函子 f 到 g 的自然变换被映为 `BaseF(f)` 到 `BaseF(g)` 的自然变换。这恰好对应于 `HFunctor`：将自然变换 `f ~> g` 映射到 `(a -> b) -> sig f a -> sig g b`。

用交换图表示，函子范畴上有 \\(X\\) 和 \\(Y\\) 范畴，\\(\mathcal{T}\\) 是 functor transformer：

$$\begin{CD}
X @>{\eta}>> Y \\\\
@V{}VV @VV{}V \\\\
\mathcal{T}(X) @>>{\texttt{hmap}(\eta)}> \mathcal{T}(Y)
\end{CD}$$

`HFree` 比较特殊，是将 functor transformer 映为 monad（自然也是 functor）：

$$\begin{CD}
\mathrm{HFree}(\mathcal{T}_1) @>{\texttt{hoist}}>> \mathrm{HFree}(\mathcal{T}_2) \\\\
@V{p.m.}VV @VV{p.m.}V \\\\
\mathcal{T}_1(\mathrm{HFree}(\mathcal{T}_1)) @>>{}> \mathcal{T}_2(\mathrm{HFree}(\mathcal{T}_2))
\end{CD}$$

这直接给出了升级 sig 的函数：

```haskell
hoist :: (sig1 :< sig2) => HFree sig1 a -> HFree sig2 a
hoist (HPure x) = HPure x
hoise (HImpure op) = HImpure $ fmap hoist op
```

<aside>
我也没完全搞明白这块的理论 :-(
</aside>

最后，如果需要工业级的高阶 dtalc，可以参考 [compdata](https://hackage.haskell.org/package/compdata-0.13.1/docs/Data-Comp-Multi.html)。
