+++
title = "Higher-order Data Types à la Carte"
date = 2026-04-12
[taxonomies]
tags = ["haskell", "dtalc"]
[extra]
headline = "Encoding imperative programs is unexpectedly nontrivial"
math = true
comment = true
+++

> Disclaimer: This post was translated into English by an AI model. It may contain mistakes or awkward wording.

I recently needed to represent an AST for a small project, and I wanted the AST itself to be extensible: I could define fragments of a language and then combine them as needed. This sounds very suitable for DTALC. But one soon discovers that the classic DTALC formulation does not work. This post records the solution.

<!-- more -->

## Classic DTALC

<aside>
The code in this post is self-contained and can be copied and run.
</aside>

To write code with `do`, we choose the basic AST skeleton to be the `Free` monad:

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

Then comes the usual DTALC boilerplate:

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

Suppose the language has two fragments. `BaseF` provides basic control flow, while `ConcF` provides concurrency primitives.

```haskell
-- | Expressions
data Expr a where
  Lit :: a -> Expr a

-- | Memory addresses
newtype Addr ty = Addr Integer

-- | Basic language
data BaseF k where
  If    :: Expr Bool -> k -> k -> k
  While :: Expr Bool -> k -> k
deriving instance Functor BaseF
if_ :: (BaseF :< f) => Expr Bool -> Free f () -> Free f () -> Free f ()
if_ c t f = Impure . inj $ If c t f (Pure ())
while_ :: (BaseF :< f) => Expr Bool -> Free f () -> Free f ()
while_ c t = Impure . inj $ While c t (Pure ())

-- | Concurrency fragment
data ConcF k where
  Fork :: [k] -> k
deriving instance Functor ConcF
fork :: (ConcF :< f) => [Free f ()] -> Free f ()
fork threads = Impure . inj $ Fork threads (Pure ())

-- | Test fragment: embed arbitrary IO
data TestF k where
  AnyIO :: IO () -> k -> TestF k
deriving instance Functor TestF
anyIO :: (TestF :< f) => IO () -> Free f ()
anyIO io = Impure . inj $ AnyIO io (Pure ())
```

The type checker accepts this, so everything looks fine. Let us write an interpreter:

```haskell
import Control.Concurrent

class (Functor f, Monad m) => Exec f m where
  execAlg :: f (m a) -> m a
instance (Exec f m, Exec g m) => Exec (f :+ g) m where
  execAlg (Inl fa) = execAlg fa
  execAlg (Inr ga) = execAlg ga
exec :: (Exec f m) => Free f a -> m a
exec (Pure x) = pure x
exec (Impure op) = execAlg (fmap exec op)
```

The individual interpreters look straightforward. But when we test:

```haskell
example :: Free (BaseF :+ ConcF :+ TestF) ()
example = do 
  if_ (Lit True) (anyIO $ putStrLn "true") (anyIO $ putStrLn "false")
  anyIO $ putStrLn "done"
```

we get:

```haskell
λ> exec example
true
done
done
```

Wait, wat?! Why is `done` printed twice?

## What Happened?

To explain this, we must look back at what the `Free` monad is doing. Expanding the program, the important part is that `fmap` acts on every parameter of type `k`:

```haskell
(Impure . inj) $ If (Lit True)
                  ((Impure . inj $ AnyIO (putStrLn "true") (Pure ())) >>= kont)
                  ((Impure . inj $ AnyIO (putStrLn "false") (Pure ())) >>= kont)
                  ((Pure ()) >>= kont)
```

Case closed: we wrote subprograms as `k`, but in the context of a `Free` monad, the functor parameter `k` means a continuation, namely "what comes next". This is why it was called `k` in the first place.

In other words, we cannot represent subprograms with `k`. Instead, we should put the code or AST itself directly into the command:

```haskell
data BaseF k where
  If    :: Expr Bool -> (Free BaseF ()) -> (Free BaseF ()) -> k -> BaseF k
  While :: Expr Bool -> (Free BaseF ()) -> (Free BaseF ()) -> BaseF k
```

But this obviously has a problem: inside a `while` loop, we want to use statements from other extended fragments, not only `BaseF`. Therefore we must introduce an extra parameter for open recursion, call it `sig` for the statement signature:

```haskell
data BaseF sig k where
  If    :: Expr Bool -> (Free sig ()) -> (Free sig ()) -> k -> BaseF sig k
  While :: Expr Bool -> (Free sig ()) -> (Free sig ()) -> BaseF sig k

data ConcF sig k where
  Fork :: [Free sig ()] -> k -> ConcF sig k

data TestF sig k where
  AnyIO :: IO () -> k -> TestF sig k
```

Unfortunately, this approach also does not work. The difficulty lies in the final combined language, where `sig` must be tied into a fixed point.

## Higher-order DTALC

Since the approaches above fail, we need a more reasonable method, preferably one that handles both `k` and `sig`.

Think again about what DTALC is doing:

* we use a `Functor` to represent the shape of statements, operations, commands, or effects;
* we combine two functors with a coproduct, written `:+`, and the result is still a functor;
* we define an order relation `:<` between functors, so a smaller functor can be injected into a larger one;
* every functor gets a corresponding `Free` monad, which is what we actually use.

But we now notice that the statement, command, operation, or effect shape **must not contain sub-statements, sub-commands, or sub-effects**. Classic DTALC is first-order and cannot handle effects with sub-effects. This situation, where effects contain effects, is called a **higher-order effect**, and the corresponding DTALC style is **higher-order DTALC**. `BaseF` and `ConcF` contain subcommands and are higher-order effects, while `TestF` does not, so it is first-order.

We know that a computation, program, or AST returning `a` can itself be described by a functor or even a monad, so we can perform open recursion directly at that point:

```haskell
data BaseF f k where
  If    :: Expr Bool -> (f ()) -> (f ()) -> k -> BaseF f k
  While :: Expr Bool -> (f ()) -> (f ()) -> BaseF f k
data ConcF f k where
  Fork :: [f ()] -> k -> ConcF f k
data TestF f k where
  AnyIO :: IO () -> k -> TestF f k
```

Then define a higher-order free monad, analogous to `Free` but with an extra `sig` parameter. Call it `HFree`:

```haskell
data HFree sig a = HPure a | HImpure (sig (HFree sig) (HFree sig a))
```

Here `sig` is not itself a `Functor`; its kind is `* -> * -> *`. We want `HFree sig` to be a monad, so `HFree` seals both type parameters of `sig`.

We also want to preserve syntax such as `(BaseF :< sig, TestF :< sig)`, which means DTALC itself must be lifted to the higher-order setting:

```haskell
data (f :+ g) sig a = Inl (f sig a) | Inr (g sig a)
```

`HFree sig` itself should be a monad, but notice that `Functor` can no longer be defined directly:

```haskell
instance Functor (HFree sig) where
  fmap f (HPure a)    = HPure (f a)
  fmap f (HImpure op) = HImpure _hole
--  _hole :: sig (HFree sig) (HFree sig b)
```

We can try to force a definition. Since `HFree sig` is a `Functor`, and `sig (HFree sig)` looks like a functor too, surely:

```haskell
instance Functor (sig (HFree sig)) => Functor (HFree sig) where
  fmap f (HPure a)    = HPure (f a)
  fmap f (HImpure op) = HImpure $ fmap (fmap f) op
```

The problem is that we do not know what the DTALC combination `f :+ g` is. It is not itself a `Functor`; we can only discuss `instance Functor (HFree (f :+ g))`, and that overlaps with the earlier `instance Functor (HFree sig)`. In short, this is also a dead end.

But the exploration above suggests that `sig` itself has some property, and that combining two `sig`s preserves this property. What is it?

Consider `BaseF f a` and `BaseF (f :+ g) b`. We want the command shape of `BaseF` to remain unchanged, while the inner functor `f` can always be injected into `f :+ g`. Let us call this replaceability of the "inner functor" `HFunctor`:

```haskell
-- | "Higher-order" functor
class HFunctor sig where
  hmap :: (forall x. f x -> g x) -> (a -> b) -> sig f a -> sig g b
-- The first argument can be understood as a natural transformation
-- from functor f to functor g, so it can also be written that way.
```

`HFunctor` says: as long as we can inject `f` into `f :+ g`, we can convert `BaseF f` into `BaseF (f :+ g)`. The definition here is a little more general than that.

With `HFunctor`, we can define `Functor`, `Applicative`, and `Monad` for `HFree`:

```haskell
instance HFunctor sig => Functor (HFree sig) where
  fmap f (HPure a)    = HPure (f a)
  fmap f (HImpure op) = HImpure (hmap id (fmap f) op)

instance HFunctor sig => Applicative (HFree sig) where
  pure  = HPure
  (<*>) = ap

instance HFunctor sig => Monad (HFree sig) where
  HPure a    >>= k = k a
  HImpure op >>= k = HImpure (hmap id (>>= k) op)
```

The intuition of `HFree` is the same as `Free`: `sig` represents the shape of commands, and `HFree sig` represents the shape of programs.

Now the higher-order DTALC boilerplate is:

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

Then rewrite the language. `HFunctor` instances are mechanical but must be written:

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

The rule is mechanical: if something is a subprogram, use the natural transformation `t`; otherwise use the ordinary mapping function `f` on the contained data.

The original smart constructors now work unchanged:

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

The algebra class needs only small changes:

```haskell
class (HFunctor sig, Monad m) => Exec sig m where
  execAlg :: sig m (m a) -> m a
instance (Exec f m, Exec g m) => Exec (f :+ g) m where
  execAlg (Inl fa) = execAlg fa
  execAlg (Inr ga) = execAlg ga

instance Exec BaseF IO where
  execAlg (If (Lit c) t f k) = pure (if c then t else f) >> k
  execAlg (If _ _ _ _) = error "not considered for now"
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

After this detour, the example finally behaves correctly:

```haskell
λ> exec example
true
done
```

Now commands may freely contain subprograms, while DTALC remains just as pleasant to use as the first-order version.

## Appendix: An Intuitive, or Not So Intuitive, View

The DTALC boilerplate can simply be copied, but curious readers may still want a theoretical picture. The discussion above was guided by intuition and types, but remains difficult. Category theory may or may not make it clearer.

We know that ordinary DTALC combines functors. Higher-order DTALC must therefore combine something else. If we inspect `BaseF`, we see that for any functor `f`, `BaseF f` is also a functor. Thus `BaseF` itself maps functors to functors; it is a functor transformer. This is analogous to `Free`, which maps a functor to a monad, which is also a functor. Therefore higher-order DTALC does not combine functors; it combines functor transformers.

Consider the category \\(Hask\\) and its category of endofunctors. `BaseF` maps an endofunctor to another endofunctor; it is an endofunctor on the endofunctor category. If `BaseF` is a functor, it must map morphisms in the functor category, namely natural transformations. A natural transformation from `f` to `g` is mapped to a natural transformation from `BaseF(f)` to `BaseF(g)`. This is precisely `HFunctor`: it maps a natural transformation `f ~> g` to `(a -> b) -> sig f a -> sig g b`.

As a commutative diagram, if \\(X\\) and \\(Y\\) are objects in the functor category and \\(\mathcal{T}\\) is a functor transformer:

$$\begin{CD}
X @>{\eta}>> Y \\\\
@V{}VV @VV{}V \\\\
\mathcal{T}(X) @>>{\texttt{hmap}(\eta)}> \mathcal{T}(Y)
\end{CD}$$

`HFree` is special: it maps a functor transformer to a monad, which is naturally also a functor:

$$\begin{CD}
\mathrm{HFree}(\mathcal{T}_1) @>{\texttt{hoist}}>> \mathrm{HFree}(\mathcal{T}_2) \\\\
@V{p.m.}VV @VV{p.m.}V \\\\
\mathcal{T}_1(\mathrm{HFree}(\mathcal{T}_1)) @>>{}> \mathcal{T}_2(\mathrm{HFree}(\mathcal{T}_2))
\end{CD}$$

This directly suggests a function for upgrading `sig`:

```haskell
hoist :: (sig1 :< sig2) => HFree sig1 a -> HFree sig2 a
hoist (HPure x) = HPure x
hoise (HImpure op) = HImpure $ fmap hoist op
```

<aside>
I have not fully understood the theory here either :-(
</aside>

Finally, if you need industrial-strength higher-order DTALC, see [compdata](https://hackage.haskell.org/package/compdata-0.13.1/docs/Data-Comp-Multi.html).
