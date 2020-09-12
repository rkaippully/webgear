# Traits
The application from the previous section is very rudimentary. Let us enhance it a bit.

Instead of always printing the time in UTC, let us accept a query parameter named `local` and respond with the
time in the local time zone when it is set to true.

Obviously, this requires extracting a query parameter from the request. So far, we have not looked at the type of
requests and operations allowed on them. So let us do that first.

## Requests
The request parameter to handler functions has the type `#!hs Linked (req :: [Type]) Request`. This might look confusing
if you are not familiar with type-level programming in Haskell. The annotation `#!hs req :: [Type]` means that this type
parameter is a list of types and not a single type. So `#!hs Linked [Bool, Int] Request` is a valid type, but `#!hs
Linked Char Request` is not.

You must be wondering why we need to "link" a request with a list of types. Well, many attributes such as query
parameters are optional, they may not exist for a given request or may not have the right type. So instead of checking
the presence of an attribute in the request every time we need it, we check it once at the beginning. If this is
successful, we record that fact in this list of types `#!hs req`.

Such attributes are called traits in WebGear terminology. Query parameters, path variables, headers, HTTP methods are
all examples of traits.

So how does this work in practice? Here is the modified application supporting a query parameter named `local`.

```hs
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import Control.Monad.IO.Class   (MonadIO (liftIO))
import Data.Time                (getCurrentTime, getZonedTime)
import Network.Wai.Handler.Warp (run)
import WebGear

getTime :: Handler '[] String
getTime = queryParam @"local" @Bool getTimeHandler

getTimeHandler :: Has (QueryParam "local" Bool) req => Handler req String
getTimeHandler = Kleisli $
  \request -> do
    let local = get (Proxy @(QueryParam "local" Bool)) request
    if local
      then ok200 . show <$> liftIO getZonedTime
      else ok200 . show <$> liftIO getCurrentTime

main :: IO ()
main = run 3000 (toApplication getTime)
```

!!! tip "Testing"
    `curl` is a handy tool to test HTTP APIs:

    ```shell
    curl --get 'http://localhost:3000' --data-urlencode 'local=True'  # Local time
    curl --get 'http://localhost:3000' --data-urlencode 'local=False' # UTC time
    curl --get 'http://localhost:3000'                                # 400 Bad Request
    ```

!!! note "Handler Type"
    Did you notice that the type of `#!hs getTime` changed from `#!hs MonadIO m => Kleisli m a (Response String)` to
    `#!hs Handler '[] String`? It is not really a change because `#!hs Handler` is defined as:

    ```hs
    type Handler req a = Kleisli Router (Linked req Request) (Response a)
    ```

    `#!hs Router` has a `#!hs MonadIO` instance (besides other functionality), so this is not very different from the
    previous type.

## Using Traits
Using traits in your API handlers is easy.

First, when you start processing the request in your handler, we don't know whether any of the traits that we are
interested in is present in the request. So the handler has the type `#!hs Handler '[] a`; the list of traits is empty.

Second, functions such as `#!hs queryParam` verify the presence of a trait and then invoke another handler (`#!hs
getTimeHandler` in the above example). In this case, `#!hs queryParam` invokes `#!hs getTimeHandler` only if the request
has the trait `#!hs QueryParam "local" Bool`. Presence of this trait indicates that the request has a query parameter
named `local` and it can be parsed to a boolean value. The type of `#!hs getTimeHandler` is `#!hs Has (QueryParam
"local" Bool) req => Handler req String` indicating that it is verified already that the request has the trait.

Third, the `#!hs get` function is used to retrieve the value of a trait attribute, in our example, the query parameter
value as a `#!hs Bool`. This function can only be used if you have the appropriate `#!hs Has` constraint for your
handler. This is a type-safe access mechanism for trait attributes.

!!! tip "DataKinds"
    The `#!hs DataKinds` language extension enables promotion of data constructors to type constructors. With this
    extension, we could use some values as types. For example, we already saw types such as `#!hs Linked '[] Request`
    and `#!hs QueryParam "local" Bool`; both `#!hs '[]` - the empty list - and `#!hs "local"` - a string literal - are
    used as types here. This extension helps to define expressive APIs in WebGear. Read more about this extension in
    [GHC documentation](https://downloads.haskell.org/~ghc/8.10.2/docs/html/users_guide/glasgow_exts.html#extension-DataKinds).

## Defining Traits
Most of the time, you can just use traits defined by WebGear as mentioned above and be done with it. But you can easily
define your own traits as well if the traits provided out of the box are insufficient.

All you need to do is define a new data type and have an instance of `#!hs Trait` type class. For example, here is how
you would implement the `#!hs QueryParam` trait:

```hs
{-# LANGUAGE DataKinds, FlexibleInstances, InstanceSigs, MultiParamTypeClasses,
             ScopedTypeVariables, TypeApplications, TypeFamilies #-}

import GHC.TypeLits (KnownSymbol, Symbol)
import WebGear

data QueryParam (name :: Symbol) val

instance (KnownSymbol name, FromHttpApiData val, Monad m)
  => Trait (QueryParam name val) Request m where
  
  -- Trait attribute retrieved on success
  type Attribute (QueryParam name val) Request = val
  -- An error value on failure
  type Absence (QueryParam name val) Request =
    Either ParamNotFound ParamParseError

  toAttribute :: Request -> m (Result (QueryParam name val) Request)
  toAttribute r = return $ do
    case getRequestParam (Proxy @name) r of
      Nothing -> NotFound (Left ParamNotFound)
      Just x  -> case parseQueryParam x of
                   Left e  -> NotFound (Right $ ParamParseError e)
                   Right v -> Found v

getRequestParam :: KnownSymbol name => Proxy name -> Request -> Maybe Text
getRequestParam = ...
```

!!! tip "Symbols"
    If you are not familiar with `#!hs Symbol`, `#!hs KnownSymbol` etc, they introduce type level string literals. All
    string literals at type level are of kind `#!hs Symbol` and they are instances of `#!hs KnownSymbol` type class. You
    can use the `#!hs symbolVal` function to convert them to a `#!hs String` value.

The `#!hs Trait` type class defined two associated types. The type `#!hs Attribute` is the trait attribute value when
the trait is present in the request. The type `#!hs Absence` is used to indicate absence of a trait; this could be some
sort of error value.

The `#!hs toAttribute` function either returns a `#!hs Found` value with an `#!hs Attribute` on success, or a `#!hs
NotFound` value with an `#!hs Absence` on failure.

Now we have a way of defining traits and checking their presence in the request. But there is still a missing piece. Our
handlers accept a parameter of type `#!hs Linked req Request`. How do we construct a value of this type?

## Linking
As already explained, a `#!hs Linked req Request` value is essentially a request with a list of traits - `#!hs req` -
that are known to be present for this request. How does this list get built? Obviously we cannot have a function that
adds arbitrary traits to this list. A trait should be added to this list if and only if the `#!hs toAttribute` function
returns a `#!hs Found` value. Otherwise, all the type-safety guarantees will be void.

This is where linking comes in. These are the functions that let us operate on linked values:

1. `#!hs link :: a -> Linked '[] a` lets us "promote" a regular value to a linked value with no traits proven on
   it. Think of this as a first step in starting to work with linked values.
2. `#!hs unlink :: Linked ts a -> a` is the opposite operation. You use it to extract the value out of a linked value.
3. `#!hs probe :: Trait t a m => Linked ts a -> m (Either (Absence t a) (Linked (t:ts) a))` grows the type level list of
   traits in a linked value. Given a linked value which has a list of traits already established, this function will
   probe for another trait `t` using the `#!hs toAttribute` function. If the trait is found, it will return a `#!hs
   Linked (t:ts) a` value thereby adding the trait to the type level list. If the trait is not present, you get the
   `#!hs Absence t a` indicating an error.

## Summary
In summary, this is how you use traits:

1. Define a data type `#!hs T` for the trait. 
2. Define an instance of `#!hs Trait` type class for this type.
3. In your handlers, add a constraint `#!hs Has T req`, so that you can use `#!hs get (Proxy @T) request` to retrieve the
   trait value.
4. Use `#!hs probe` to check presence of traits and form linked values. These linked requests can be passed as arguments
   to handlers.
