# Middlewares
Middlewares are functions that take a handler as their input and produces a handler as output. They are useful to
enhance behaamviour of handlers somehow.

```hs
type Middleware req req' a' a = Handler req' a' -> Handler req a
```

We already met the `#!hs queryParam` function in the previous section which is a middleware. Here is possible
implementation for it in pseudo-code:

```hs
queryParam :: (KnownSymbol name, FromHttpApiData val)
           => Middleware req (QueryParam name val:req) a a
queryParam handler = Kleisli $ \request -> do
  res <- probe @(QueryParam name val) request
  either sendErrorResponse (runKleisli handler) res
```

As you can see, this just uses `#!hs probe` as mentioned in the previous section in an attempt to prove the presence of
the `#!hs QueryParam` trait. If successful, we call `#!hs handler` with that result and if unsuccessful, use `#!hs
sendErrorResponse` to handle it.

## Types of Middlewares
So what are the kind of things we can do in a middleware?

As seen in the `#!hs queryParam` example, we can modify the request parameter passed to the inner handler. Such
middlewares are called request middlewares and have this type:

```hs
type RequestMiddleware req req' a = Middleware req req' a a
```

Similarly, we have response middlewares which modify the responses sent from the inner handler.

```hs
type ResponseMiddleware req a' a = Middleware req req a' a
```

Of course, we could have a middleware that modifies both the request and response.
