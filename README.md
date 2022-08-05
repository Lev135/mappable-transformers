mappable-transformers
===

This package extends `mtl` type classes, such as `MonadReader`, `MonadWriter`,
`MonadState` and `MonadError`, to make possible change (map) type of
environment, log, state and error respectively.

Examples
===

Errors
---

Assume, we have two stateful computations, that can fail with different errors:
```haskell
foo :: (MonadState SomeState m, MonadError FooErr m) => m Int

bar :: (MonadState SomeState m, MonadError BarErr m) => m Int
```

Everything is fine with common `mtl` classes until we want to compose this
computations in `foobar`:
```haskell
foobar :: (MonadState SomeState m, MonadError _ m) => m Int
foobar = do
  fooRes <- foo
  barRes <- bar
  return $ fooRes + barRes
```
This won't type check whatever we fill in the whole, because `foo` and `bar`
have different error type. So we need to make a sum type to store one of these 
errors:
```haskell
data FooBarErr = FooErr FooErr | BarErr BarErr

foobar :: (MonadState SomeState m, MonadError FooBarErr m) => m Int
foobar = do
  fooRes <- _ foo
  barRes <- _ bar
  return $ fooRes + barRes
```
However, `mtl` does not provide any way to change error type in monad with 
`MonadError` constraint. So we are not able to "lift" `foo`'s errors and there
is no way to fill the wholes above. Using `mtl` only we have to change `foo`
and `bar` function in such a way that they throw `FooBar` exception
(i. e. packing all exceptions from foo/bar in `FooErr`/`BarErr` constructors
respectively everywhere in `foo` and `bar` function and changing constraints
to `MonadError FooBarErr m` everywhere).

This approach is simple, but has several disadvantages:
- We have to change subfunctions (`foo` & `bar` in our example): not only types,
  but also bodies wrapping all errors manually.
- Nothing in subfunctions signatures indicates that it throws only its errors
  (`FooErr`/`BarErr` resp.) so type checker is not able to check it. In addition 
  it can be difficult for people to recognize it from the first sight.

This package provides alternative solution.  
Using `Control.Monad.Except.Mappable.mapError` we can write `foobar` literally
as follows:
```haskell
foobar :: (MonadState SomeState m, MonadError FooBarErr m, _) => m Int
foobar = do
  fooRes <- mapError FooErr foo
  barRes <- mapError BarErr bar
  return $ fooRes + barRes
```
Note the type hole in constraint. It is here, because we haven't listed all 
constraints. In fact we have not listed even all type variables. 
Full type signature will be like (in fact, in this case the first two 
constraints are redundant):
```haskell
foobar :: 
  ( MonadState SomeState m,
    MonadError FooBarErr m,
    MappableError FooErr FooBarErr m' m,
    MappableError BarErr FooBarErr m'' m,
    MonadState SomeState m',
    MonadState SomeState m''
  ) => m Int
```
Looks terrible, isn't it? Fortunately due to strong functional dependencies
all this boilerplate code can be omitted as in the example above.
Moreover, we can omit constraints totally and write as
```haskell
foobar :: _ => m Int
```
leaving constraints' inference for compiler, and it also works fine.

Note, that we can't omit type signature at all, because in that case
compiler will try to use concrete transformer combination instead of classes.
